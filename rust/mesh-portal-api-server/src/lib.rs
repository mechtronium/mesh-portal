#[macro_use]
extern crate anyhow;

#[macro_use]
extern crate async_trait;

use std::collections::HashMap;
use std::future::Future;
use std::prelude::rust_2021::TryInto;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Error;
use futures::future::select_all;
use futures::FutureExt;
use tokio::sync::mpsc::error::{SendError, SendTimeoutError};
use tokio::sync::{mpsc, oneshot};
use uuid::Uuid;

use mesh_portal_api::message;
use mesh_portal_serde::mesh;
use mesh_portal_serde::version::latest;
use mesh_portal_serde::version::latest::entity::response;
use mesh_portal_serde::version::latest::fail;
use mesh_portal_serde::version::latest::frame::CloseReason;
use mesh_portal_serde::version::latest::id::Address;
use mesh_portal_serde::version::latest::log::Log;
use mesh_portal_serde::version::latest::messaging::{Exchange, ExchangeId};
use mesh_portal_serde::version::latest::pattern::AddressKindPattern;
use mesh_portal_serde::version::latest::portal::{inlet, outlet};
use mesh_portal_serde::version::latest::resource::ResourceStub;
use mesh_portal_serde::version::latest::resource::Status;
use mesh_portal_serde::version::v0_0_1::artifact::{Artifact, ArtifactRequest, ArtifactResponse};
use mesh_portal_serde::version::v0_0_1::config::{Assign, Config, ConfigBody, PortalConfig};
use mesh_portal_serde::version::v0_0_1::generic::entity::request::ReqEntity;
use mesh_portal_serde::version::v0_0_1::generic::id::KindParts;
use mesh_portal_serde::version::v0_0_1::generic::payload::Payload;
use mesh_portal_serde::version::v0_0_1::generic::portal::inlet::{AssignRequest, Frame};
use mesh_portal_serde::version::v0_0_1::generic::portal::Exchanger;
use mesh_portal_serde::version::v0_0_1::util::ConvertFrom;
use std::fmt::Debug;

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum PortalStatus {
    None,
    Initializing,
    Ready,
    Panic(String),
}

pub fn log(log: Log) {
    match log {
        Log::Info(message) => {
            println!("{}", message);
        }
        Log::Fatal(message) => {
            eprintln!("{}", message);
        }
        Log::Warn(message) => {
            println!("{}", message);
        }
        Log::Error(message) => {
            eprintln!("{}", message);
        }
    }
}

#[derive(Debug)]
pub struct ExchangePair {
    pub id: ExchangeId,
    pub tx: tokio::sync::oneshot::Sender<inlet::Response>,
}

#[derive(Debug)]
pub enum PortalCall {
    FrameIn(inlet::Frame),
    FrameOut(outlet::Frame),
    Exchange(ExchangePair),
}

#[derive(Debug)]
pub struct Portal {
    key: u64,
    config: PortalConfig,
    request_handler: Arc<dyn PortalRequestHandler>,
    outlet_tx: mpsc::Sender<outlet::Frame>,
    mux_tx: mpsc::Sender<MuxCall>,
    pub log: fn(log: Log),
    pub call_tx: mpsc::Sender<PortalCall>,
    pub mux_rx: mpsc::Receiver<MuxCall>,
}

impl Portal {
    pub fn new(
        key: u64,
        config: PortalConfig,
        request_handler: Arc<dyn PortalRequestHandler>,
        outlet_tx: mpsc::Sender<outlet::Frame>,
        logger: fn(log: Log),
    ) -> Self {
        let (mux_tx, mux_rx) = tokio::sync::mpsc::channel(1024);
        let (call_tx, mut call_rx) = tokio::sync::mpsc::channel(1024);
       // call_tx.try_send(PortalCall::FrameIn(inlet::Frame::Log(Log::Info("Hello".to_string()))));

        {
            let mut exchanges: HashMap<ExchangeId, oneshot::Sender<inlet::Response>> =
                HashMap::new();
            let mux_tx = mux_tx.clone();
            let outlet_tx = outlet_tx.clone();
            let request_handler = request_handler.clone();
            tokio::spawn(async move {
                tokio::time::sleep(Duration::from_millis(1));
println!("!!! Started call_rx recv...." );
                while let Option::Some(command) = call_rx.recv().await {
println!(">>> REcv PortalCall" );
                    match command {
                        PortalCall::FrameIn(frame) => match frame {
                            inlet::Frame::Log(log) => {
                                (logger)(log);
                            }
                            inlet::Frame::Request(request) => {
println!("Received Request from: {}", request.from.to_string());
                                match &request.exchange {
                                    Exchange::Notification => {
                                        for to in &request.to {
                                            let request = mesh::Request::from(
                                                request.clone().into(),
                                                to.clone(),
                                                Exchange::Notification,
                                            );
                                            let result = mux_tx
                                                .send(MuxCall::MessageIn(
                                                    message::Message::Request(request),
                                                ))
                                                .await;
                                            if let Result::Err(_err) = result {
                                                logger(Log::Fatal(
                                                    "FATAL: send timeout error request_tx"
                                                        .to_string(),
                                                ))
                                            }
                                        }
                                    }
                                    Exchange::RequestResponse(exchange_id) => {
println!("Portal: Sending Exchange Request");
                                        if request.to.len() != 1 {
                                            (logger)(Log::Error("exchange requests cannot have more than one recipient".to_string()))
                                        } else {
                                            let to = request
                                                .to
                                                .first()
                                                .expect("expected to identifier")
                                                .clone();
                                            let request = mesh::Request::from(
                                                request.clone().into(),
                                                to,
                                                Exchange::RequestResponse(exchange_id.clone()),
                                            );
                                            let result = mux_tx
                                                .send(MuxCall::MessageIn(
                                                    message::Message::Request(request),
                                                ))
                                                .await;
                                            if let Result::Err(_err) = result {
                                                logger(Log::Fatal(
                                                    "FATAL: frame timeout error request_tx"
                                                        .to_string(),
                                                ));
                                            }
                                        }
                                    }
                                }
                            }
                            inlet::Frame::Response(response) => {
                                match exchanges.remove(&response.exchange) {
                                    None => {
                                        logger(Log::Fatal(format!(
                                            "FATAL: missing request/response exchange id '{}'",
                                            response.exchange
                                        )));
                                    }
                                    Some(tx) => {
                                        let tx = tx;
                                        tx.send(response).expect("ability to send response");
                                    }
                                }
                            }
                            inlet::Frame::Status(status) => {}
                            inlet::Frame::AssignRequest(assign) => {
                                let request_handler = request_handler.clone();
                                let outlet_tx = outlet_tx.clone();
                                tokio::spawn(async move {
                                    async fn process(
                                        assign: Exchanger<AssignRequest>,
                                        request_handler: &Arc<dyn PortalRequestHandler>,
                                        outlet_tx: &mpsc::Sender<outlet::Frame>,
                                    ) -> Result<(), Error> {
                                        let id = assign.id;
                                        let assign = assign.item;
                                        let assign =
                                            request_handler.handle_assign_request(assign).await?;
                                        let assign = Exchanger { id, item: assign };
                                        outlet_tx.send(outlet::Frame::Assign(assign)).await?;
                                        Ok(())
                                    }
                                    match process(assign, &request_handler, &outlet_tx).await {
                                        Ok(_) => {}
                                        Err(err) => {}
                                    }
                                });
                            }
                            inlet::Frame::Artifact(_) => {}
                            inlet::Frame::Config(_) => {}
                            inlet::Frame::Close(_) => {}
                        },
                        PortalCall::Exchange(exchange) => {
                            exchanges.insert(exchange.id, exchange.tx);
                        }
                        PortalCall::FrameOut(frame) => {
                            match outlet_tx
                                .send(frame)
                                .await
                            {
                                Ok(_) => {}
                                Err(err) => {
                                    logger(Log::Fatal(
                                        "FATAL: frame timeout error outlet_tx".to_string(),
                                    ));
                                }
                            }
                        }
                    }
                }
println!("@@@ Terminating");
            });
        }

        Self {
            key,
            config,
            request_handler,
            call_tx,
            outlet_tx,
            log: logger,
            mux_tx,
            mux_rx,
        }
    }

    pub async fn send(&self, frame: outlet::Frame) -> Result<(), Error> {
        self.outlet_tx
            .send(
                frame
            )
            .await?;
        Ok(())
    }

    pub async fn exchange(&self, request: outlet::Request) -> Result<inlet::Response, Error> {
        //        let mut request = request;
        let exchange_id: ExchangeId = Uuid::new_v4().to_string();
        //        let request = request.exchange(Exchange::RequestResponse(exchange_id.clone()));
        let (tx, rx) = tokio::sync::oneshot::channel();
        let exchange = ExchangePair {
            id: exchange_id,
            tx,
        };
        self.call_tx
            .send_timeout(
                PortalCall::Exchange(exchange),
                Duration::from_secs(self.config.frame_timeout.clone()),
            )
            .await?;

        Ok(rx.await?)
    }

    pub fn shutdown(&mut self) {
        self.outlet_tx
            .try_send(outlet::Frame::Close(CloseReason::Done))
            .unwrap_or(());
    }
}

#[async_trait]
pub trait PortalRequestHandler: Send + Sync + Debug {
    async fn default_assign(&self) -> Result<Assign, Error> {
        Err(anyhow!("request handler does not have a default assign"))
    }

    async fn handle_assign_request(&self, request: AssignRequest) -> Result<Assign, Error> {
        Err(anyhow!("request handler does not assign"))
    }

    async fn handle_artifact_request(
        &self,
        request: ArtifactRequest,
    ) -> Result<ArtifactResponse<Artifact>, Error> {
        Err(anyhow!("request handler does not handle artifacts"))
    }

    async fn handle_config_request(
        &self,
        request: ArtifactRequest,
    ) -> Result<ArtifactResponse<Config<ConfigBody>>, Error> {
        Err(anyhow!("request handler does not handle configs"))
    }
}

#[derive(Debug)]
pub struct DefaultPortalRequestHandler {}

impl Default for DefaultPortalRequestHandler {
    fn default() -> Self {
        Self {}
    }
}

#[async_trait]
impl PortalRequestHandler for DefaultPortalRequestHandler {}

#[derive(Debug)]
pub enum MuxCall {
    Add(Portal),
    Assign {
        assign: Exchanger<Assign>,
        portal: u64,
    },
    Remove(Address),
    MessageIn(message::Message),
    MessageOut(message::Message),
    SelectAll(oneshot::Sender<Vec<ResourceStub>>), // for testing only
}

pub trait Router: Send + Sync {
    fn route(&self, message: message::Message);
    fn logger(&self, message: &str) {
        println!("{}", message);
    }
}

pub struct PortalMuxer {
    portals: HashMap<u64, Portal>,
    address_to_portal: HashMap<Address, u64>,
    address_to_assign: HashMap<Address, Assign>,
    router: Box<dyn Router>,
    mux_tx: mpsc::Sender<MuxCall>,
    mux_rx: mpsc::Receiver<MuxCall>,
}

impl PortalMuxer {
    pub fn new(
        mux_tx: mpsc::Sender<MuxCall>,
        mux_rx: mpsc::Receiver<MuxCall>,
        router: Box<dyn Router>,
    ) {
        let mut muxer = Self {
            portals: HashMap::new(),
            address_to_portal: HashMap::new(),
            address_to_assign: HashMap::new(),
            router,
            mux_tx,
            mux_rx,
        };

        tokio::spawn(async move {
            loop {
                let mut ids = vec![];
                let mut futures = vec![];

                for (key, portal) in &mut muxer.portals {
                    futures.push(portal.mux_rx.recv().boxed());
                    ids.push(key.clone());
                }

                futures.push(muxer.mux_rx.recv().boxed());

                let (call, future_index, _) = select_all(futures).await;

                async fn handle(call: MuxCall, muxer: &mut PortalMuxer) -> Result<(), Error> {
                    match call {
                        MuxCall::Add(portal) => {
                            muxer.portals.insert(portal.key.clone(), portal);
                        }
                        MuxCall::Assign { assign, portal } => {
                            muxer
                                .address_to_assign
                                .insert(assign.stub.address.clone(), assign.item.clone());
                            muxer
                                .address_to_portal
                                .insert(assign.stub.address.clone(), portal);
                            let portal = muxer
                                .portals
                                .get(&portal)
                                .ok_or(anyhow!("expected portal"))?;
                            portal.send(outlet::Frame::Assign(assign.clone())).await?;
                            muxer.router.logger(
                                format!(
                                    "INFO: added portal to muxer at address {}",
                                    assign.stub.address.to_string()
                                )
                                .as_str(),
                            );
                        }
                        MuxCall::Remove(address) => {
                            muxer.address_to_assign.remove(&address);
                            if let Option::Some(mut portal) =
                                muxer.address_to_portal.remove(&address)
                            {
                                muxer.router.logger(
                                    format!(
                                        "INFO: removed address {} from portal muxer",
                                        address.to_string()
                                    )
                                    .as_str(),
                                );
                            }
                        }
                        MuxCall::MessageIn(message) => {
println!("MuxCall::MessageIn(message) to: {}",message.to().to_string());
                            muxer.router.route(message);
                        }
                        MuxCall::MessageOut(message) => {
println!("MuxCall::MessageOut(message) to: {}",message.to().to_string());
                            match muxer.portals.get(
                                muxer
                                    .address_to_portal
                                    .get(&message.to())
                                    .ok_or(anyhow!("expected address"))?,
                            ) {
                                Some(portal) => match message {
                                    message::Message::Request(request) => {
                                        portal.call_tx.try_send(PortalCall::FrameOut(
                                            outlet::Frame::Request(request.try_into()?),
                                        ));
                                    }
                                    message::Message::Response(response) => {
                                        portal.call_tx.try_send(PortalCall::FrameOut(
                                            outlet::Frame::Response(response.into()),
                                        ));
                                    }
                                },
                                None => {}
                            }
                        }
                        MuxCall::SelectAll(tx) => {
                            let mut rtn = vec![];
                            for (_, assign) in &muxer.address_to_assign {
                                rtn.push(assign.stub.clone());
                            }
                            tx.send(rtn);
                        }
                    }
                    Ok(())
                }

                match call {
                    None => {
                        if future_index >= ids.len() {
                            // shutdown
                            return;
                        } else {
                            let key = ids.get(future_index).expect("expected key");
                            if let Option::Some(mut portal) = muxer.portals.remove(key) {
                                portal.shutdown();
                            }
                        }
                    }
                    Some(call) => {
                        handle(call, &mut muxer).await;
                    }
                }
            }
        });
    }
}
