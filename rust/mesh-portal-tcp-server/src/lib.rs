
#[macro_use]
extern crate async_trait;

#[macro_use]
extern crate anyhow;

#[macro_use]
extern crate strum_macros;

use std::convert::{TryFrom, TryInto};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use anyhow::Error;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};
use tokio::net::tcp::{OwnedReadHalf, OwnedWriteHalf};
use tokio::sync::{broadcast, mpsc, Mutex, oneshot};
use tokio::sync::mpsc::error::SendTimeoutError;

use mesh_portal_api_server::{MuxCall, Portal, PortalMuxer, Router, PortalAssignRequestHandler, PortalCall};
use mesh_portal_serde::version::latest::frame::CloseReason;
use mesh_portal_serde::version::latest::log::Log;
use mesh_portal_serde::version::latest::portal::{Exchanger, inlet, outlet};
use mesh_portal_serde::version::latest::resource::Status;
use mesh_portal_serde::version::latest::resource::Code;
use mesh_portal_tcp_common::{FrameReader, FrameWriter, PrimitiveFrameReader, PrimitiveFrameWriter};
use std::sync::atomic::{AtomicU64, Ordering};
use std::future::Future;
use tokio::task::yield_now;
use mesh_portal_serde::version::latest::config::PortalConfig;
use mesh_portal_serde::version::latest::messaging::Message;

#[derive(Clone,strum_macros::Display)]
pub enum Event {
    Status(Status),
    ClientConnected,
    FlavorNegotiation(EventResult<String>),
    Authorization(EventResult<String>),
    ResourceCtrl(EventResult<String>),
    Shutdown,
}

#[derive(Clone)]
pub enum EventResult<E>{
    Ok(E),
    Err(String)
}

pub enum TcpServerCall {
    ListenEvents(oneshot::Sender<broadcast::Receiver<Event>>),
    InjectMessage(Message),
    Shutdown
}

struct Alive {
    pub alive: bool
}

impl Alive {
    pub fn new() -> Self {
        Self {
            alive: true
        }
    }
}

pub struct PortalTcpServer {
    portal_config: PortalConfig,
    port: usize,
    server: Arc<dyn PortalServer>,
    broadcaster_tx: broadcast::Sender<Event>,
    call_tx: mpsc::Sender<TcpServerCall>,
    mux_tx: mpsc::Sender<MuxCall>,
    alive: Arc<Mutex<Alive>>,
    key_seq: AtomicU64,
    request_handler: Arc<dyn PortalAssignRequestHandler>
}

impl PortalTcpServer {

    pub fn new(port: usize, server: Box<dyn PortalServer>) -> mpsc::Sender<TcpServerCall> {
        let (call_tx,mut call_rx) = mpsc::channel(1024 );
        {
            let call_tx = call_tx.clone();
            tokio::task::spawn_blocking(move || {
                let server: Arc<dyn PortalServer> = server.into();
                let (broadcaster_tx, _) = broadcast::channel(32);

                let (mux_tx, mux_rx) = mpsc::channel(1024);
                let router = server.router_factory(mux_tx.clone());

                PortalMuxer::new(mux_tx.clone(), mux_rx, router);

                let server = Self {
                    request_handler: server.portal_request_handler(),
                    key_seq: AtomicU64::new(0),
                    portal_config: Default::default(),
                    port,
                    server,
                    broadcaster_tx,
                    call_tx: call_tx.clone(),
                    mux_tx: mux_tx.clone(),
                    alive: Arc::new(Mutex::new(Alive::new()))
                };

                tokio::spawn(async move {
                    server.broadcaster_tx.send(Event::Status(Status::Unknown)).unwrap_or_default();
                    {
                        let port = server.port.clone();
                        let broadcaster_tx = server.broadcaster_tx.clone();
                        let alive = server.alive.clone();
                        tokio::spawn(async move {
                            yield_now().await;
                            while let Option::Some(call) = call_rx.recv().await {
                                match call {
                                    TcpServerCall::InjectMessage(_) => {}
                                    TcpServerCall::ListenEvents(tx) => {
                                        tx.send(broadcaster_tx.subscribe());
                                    },
                                    TcpServerCall::Shutdown => {
                                        broadcaster_tx.send(Event::Shutdown).unwrap_or_default();
                                        alive.lock().await.alive = false;
                                        match std::net::TcpStream::connect(format!("localhost:{}", port)) {
                                            Ok(_) => {}
                                            Err(_) => {}
                                        }
                                        return;
                                    }
                                }
                            }
                        });
                    }

                    server.start().await;
                });
            });
        }
        call_tx
    }


    async fn start(self) {
            let addr = format!("localhost:{}", self.port);
            match std::net::TcpListener::bind(addr.clone()) {
                Ok(std_listener) => {
                    tokio::time::sleep(Duration::from_secs(0)).await;
                    let listener = TcpListener::from_std(std_listener).unwrap();
                    self.broadcaster_tx.send( Event::Status(Status::Ready) ).unwrap_or_default();
                    tokio::time::sleep(Duration::from_secs(0)).await;
                    while let Ok((stream, _)) = listener.accept().await {
                        {
                            if !self.alive.lock().await.alive.clone() {
                                (self.server.logger())("server reached final shutdown");
                                break;
                            }
                        }
                        self.broadcaster_tx.send( Event::ClientConnected ).unwrap_or_default();
                        (&self).handle(stream).await;
                    }
                    self.broadcaster_tx.send( Event::Status(Status::Done(Code::Ok)) ).unwrap_or_default();
                }
                Err(error) => {
                    let message = format!("FATAL: could not setup TcpListener {}", error);
                    (self.server.logger())(message.as_str());
                    self.broadcaster_tx.send( Event::Status(Status::Panic(message)) ).unwrap_or_default();
                }
            }

    }

    async fn handle( &self, stream: TcpStream ) -> Result<(),Error> {
        let (reader, writer) = stream.into_split();
        let mut reader = PrimitiveFrameReader::new(reader);
        let mut writer = PrimitiveFrameWriter::new(writer);

        let flavor = reader.read_string().await?;

        // first verify flavor matches
        if flavor != self.server.flavor() {
            let message = format!("ERROR: flavor does not match.  expected '{}'", self.server.flavor() );

            writer.write_string(message.clone() ).await?;
            tokio::time::sleep(Duration::from_secs(0)).await;

            self.broadcaster_tx.send( Event::FlavorNegotiation(EventResult::Err(message.clone()))).unwrap_or_default();
            return Err(anyhow!(message));
        } else {
            self.broadcaster_tx.send( Event::FlavorNegotiation(EventResult::Ok(self.server.flavor()))).unwrap_or_default();
        }


        writer.write_string( "Ok".to_string() ).await?;
        yield_now().await;

        match self.server.auth(&mut reader, &mut writer).await
        {
            Ok(user) => {
                self.broadcaster_tx.send( Event::Authorization(EventResult::Ok(user.clone()))).unwrap_or_default();
                tokio::time::sleep(Duration::from_secs(0)).await;
                writer.write_string( "Ok".to_string() ).await?;

                let mut reader : FrameReader<inlet::Frame> = FrameReader::new(reader );
                let mut writer : FrameWriter<outlet::Frame>  = FrameWriter::new(writer );

//                        self.broadcaster_tx.send( Event::Info(EventResult::Ok(info.clone()))).unwrap_or_default();

                        let (outlet_tx,mut outlet_rx) = mpsc::channel(1024);

                        fn logger( log: Log ) {
                            println!("{}", log.to_string() );
                        }

                        let key = self.key_seq.fetch_add(1, Ordering::Relaxed );

                        let portal = Portal::new(key,self.portal_config.clone(), self.request_handler.clone(), outlet_tx, logger );
                        let inlet_tx = portal.inlet_tx.clone();
                        let result = self.mux_tx.send(MuxCall::Add(portal)).await;
                        yield_now().await;
                        match result {
                            Err(err) => {
                                let message = err.to_string();
                                (self.server.logger())(message.as_str());
        //                                self.broadcaster_tx.send( Event::Info(EventResult::Err(message.clone()))).unwrap_or_default();
                            }
                            Ok(_) => {
                                match self.request_handler.default_assign().await {
                                    Ok(assign) => {
                                        let assign = Exchanger::new(assign);

                                        self.broadcaster_tx.send( Event::ResourceCtrl(EventResult::Ok(user.clone()))).unwrap_or_default();
                                        self.mux_tx.send(MuxCall::Assign {assign, portal:key }).await?;
                                        yield_now().await;
                                    }
                                    Err(_) => {
                                    }
                                }
                            }
                        }

                {
                            let logger = self.server.logger();
                                tokio::spawn(async move {
                                    loop {
                                        if let Result::Ok(frame) = reader.read().await {
println!("server TCP READ FRAME: {}",frame.to_string());
                                            let result = inlet_tx.send(frame).await;
                                            yield_now().await;
                                            if result.is_err() {
                                                (logger)("FATAL: cannot send frame to portal inlet_tx");
                                                return;
                                            }
                                        }else {
                                            println!("TCP Reader end...");
                                            break;
                                        }
                                    }
                            });
                        }

                    {
                        let logger = self.server.logger();
                        let task = tokio::task::spawn_blocking(move || {
                            tokio::spawn(async move {
                                while let Option::Some(frame) = outlet_rx.recv().await {
                                    println!("server... SENDING from outlet_rx frame :==:> {}", frame.to_string());
                                    writer.write(frame).await;
                                }
                            });
println!("server: outlet_rx complete.");
                        });
                        task.await?;
                    }
                }
            Err(err) => {
                let message = format!("ERROR: authorization failed: {}", err.to_string());
                (self.server.logger())(message.as_str());
                self.broadcaster_tx.send( Event::Authorization(EventResult::Err(message.clone()))).unwrap_or_default();
                writer.write_string( message ).await?;
            }
        }
        Ok(())

    }
}

pub struct RouterProxy {
    pub server: Arc<dyn PortalServer>
}


#[async_trait]
pub trait PortalServer: Sync+Send {
    fn flavor(&self) -> String;
    async fn auth(&self, reader: &mut PrimitiveFrameReader, writer: &mut PrimitiveFrameWriter) -> Result<String,Error>;
    fn router_factory(&self, mux_tx: tokio::sync::mpsc::Sender<MuxCall> ) -> Box<dyn Router>;
    fn logger(&self) -> fn(message: &str);
    fn portal_request_handler(&self) -> Arc<dyn PortalAssignRequestHandler>;
}

