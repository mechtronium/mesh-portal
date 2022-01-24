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
use tokio::sync::mpsc::error::{SendError, SendTimeoutError, TryRecvError};
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
use mesh_portal_serde::mesh::{Request, Response};

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
    pub mux_tx: mpsc::Sender<MuxCall>,
    pub inlet_tx: mpsc::Sender<inlet::Frame>,
    pub outlet_tx: mpsc::Sender<outlet::Frame>,
    pub mux_rx: mpsc::Receiver<MuxCall>,
    pub log: fn(log: Log),
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
        let (inlet_tx, mut inlet_rx) = tokio::sync::mpsc::channel(1024);

        {
            let mux_tx = mux_tx.clone();
            tokio::spawn(async move {
                loop {
                    match inlet_rx.recv().await {
                        Some(frame) => {
                            let frame:inlet::Frame = frame;
println!("Server Portal Frame > {}",frame.to_string() );
                            handle(&mux_tx, frame ).await;
                            continue;
                        }
                        None => {
                            break;
                        }
                    }
                    async fn handle( mux_tx: &mpsc::Sender<MuxCall>, frame: inlet::Frame ) {
                        match frame {
                            inlet::Frame::Log(log) => {
                                println!("{}",log.to_string());
                            }
                            inlet::Frame::AssignRequest(assign) => {
                                // we aren't doing this yet
                            }
                            inlet::Frame::Request(request) => {
                                let request = Request{
                                    id: request.id,
                                    to: request.to,
                                    from: request.from,
                                    entity: request.entity,
                                    exchange: request.exchange
                                };
                                mux_tx.send(MuxCall::MessageIn( message::Message::Request(request))).await;

                            }
                            inlet::Frame::Response(response) => {
                                let response = Response{
                                    id: response.id,
                                    to: response.to,
                                    from: response.from,
                                    entity: response.entity,
                                    exchange: response.exchange
                                };
                                mux_tx.send(MuxCall::MessageIn( message::Message::Response(response))).await;
                            }
                            inlet::Frame::Artifact(_) => {
                                // not implemented
                            }
                            inlet::Frame::Config(_) => {
                                // not implemented
                            }
                            inlet::Frame::Status(_) => {
                                // not implemented
                            }
                            inlet::Frame::Close(_) => {
                                // not implemented
                            }
                        }
                    }
                }
            });
        }


        Self {
            key,
            config,
            request_handler,
            inlet_tx,
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

#[derive(Debug,strum_macros::Display)]
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
println!("MuxCall: {}",call.to_string());
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
                            muxer.router.route(message);
                        }
                        MuxCall::MessageOut(message) => {
                            match muxer.portals.get(
                                muxer
                                    .address_to_portal
                                    .get(&message.to())
                                    .ok_or(anyhow!("expected address"))?,
                            ) {
                                Some(portal) => match message {
                                    message::Message::Request(request) => {
                                        portal.outlet_tx.try_send(
                                            outlet::Frame::Request(request.try_into()?),
                                        );
                                    }
                                    message::Message::Response(response) => {
                                        portal.outlet_tx.try_send( outlet::Frame::Response(response.into()), );
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
