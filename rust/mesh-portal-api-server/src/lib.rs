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
use tokio::sync::{broadcast, mpsc, oneshot};

use mesh_portal_serde::version::latest;
use mesh_portal_serde::version::latest::entity::response;
use mesh_portal_serde::version::latest::fail;
use mesh_portal_serde::version::latest::frame::CloseReason;
use mesh_portal_serde::version::latest::id::Address;
use mesh_portal_serde::version::latest::messaging::{Exchange, ExchangeId, Message, Request, RequestExchange, Response};
use mesh_portal_serde::version::latest::pattern::AddressKindPattern;
use mesh_portal_serde::version::latest::portal::{Exchanger, inlet, outlet};
use mesh_portal_serde::version::latest::resource::ResourceStub;
use mesh_portal_serde::version::latest::resource::Status;
use std::fmt::Debug;
use dashmap::DashMap;
use tokio::task::yield_now;
use mesh_portal_serde::version::latest::artifact::{Artifact, ArtifactRequest, ArtifactResponse};
use mesh_portal_serde::version::latest::config::{Assign, Config, ConfigBody, PortalConfig};
use mesh_portal_serde::version::latest::portal::inlet::{AssignRequest, Log};

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum PortalStatus {
    None,
    Initializing,
    Ready,
    Panic(String),
}

pub enum PortalMuxerEvent {
    PortalAdded{ info: PortalInfo, tx: mpsc::Sender<PortalCall>},
    PortalRemoved(PortalInfo),
    ResourceAssigned{ stub: ResourceStub, tx: mpsc::Sender<PortalCall>},
    ResourceRemoved(Address)
}


#[derive(Debug)]
pub enum PortalCall {
    Request(RequestExchange),
    Assign(Assign)
}

#[derive(Debug,Clone)]
pub struct PortalInfo {
    pub portal_key: String
}


#[derive(Debug)]
pub struct Portal {
    pub info: PortalInfo,
    pub config: PortalConfig,
    pub tx: mpsc::Sender<PortalCall>,
    pub log: fn(log: Log),
}

impl Portal {
    pub fn new(
        info: PortalInfo,
        config: PortalConfig,
        outlet_tx: mpsc::Sender<outlet::Frame>,
        mut inlet_rx: mpsc::Receiver<inlet::Frame>,
        mux_tx: mpsc::Sender<MuxCall>,
        request_handler: Arc<dyn PortalAssignRequestHandler>,
        router: Arc<dyn MeshRouter>,
        logger: fn(log: Log),
    ) -> Self {
        let (tx, mut rx) = tokio::sync::mpsc::channel(1024);
        let exchanges = Arc::new( DashMap::new() );
        {
            let info = info.clone();
            let outlet_tx = outlet_tx.clone();
            let exchanges = exchanges.clone();
            let mux_tx = mux_tx.clone();
            tokio::spawn( async move {
                while Some(call) = rx.recv().await {
                    match call {
                        PortalCall::Request(exchange) => {
                            exchanges.insert( exchange.request.id.clone(), exchange.tx );
                            outlet_tx.send(outlet::Frame::Request(exchange.request)).await;
                        }
                        PortalCall::Assign(assign) => {
                            let assign = Exchanger::new(assign);
                            mux_tx.send( MuxCall::Assign{assign, portal_key: info.portal_key.clone() } ).await;
                        }
                    }
                }
            });
        }

        {
            let request_handler = request_handler.clone();
            let info = info.clone();
            let portal_config = config.clone();
            tokio::spawn(async move {
                loop {
                    match inlet_rx.recv().await {
                        Some(frame) => {
                            let frame:inlet::Frame = frame;
                            match frame {
                                inlet::Frame::Log(log) => {
                                    println!("{}",log.to_string());
                                }
                                inlet::Frame::AssignRequest(request) => {
                                    let result = request_handler.handle_assign_request(request.item.clone(), &mux_tx).await;
                                    match result {
                                        Ok(assignment) => {
                                            let assign = request.with(assignment);
                                            mux_tx.send( MuxCall::Assign { assign, portal_key: info.key.clone() }).await;
                                        }
                                        Err(error) => {
                                            println!("{}",error.to_string());
                                        }
                                    }
                                }
                                inlet::Frame::Request(request) => {
                                    let (exchange,mut rx) = RequestExchange::new(request.clone());
                                    router.request( exchange );
                                    tokio::spawn(async move {
                                        match tokio::time::timeout(Duration::from_secs(portal_config.response_timeout ), rx ).await {
                                            Ok(Ok(response)) => {
                                                outlet_tx.send( outlet::Frame::Response(response)).await;
                                            }
                                            _ => {
                                                let response = request.fail("timeout".to_string());
                                                outlet_tx.send( outlet::Frame::Response(response)).await;
                                            }
                                        }
                                    });
                                }
                                inlet::Frame::Response(response) => {
                                    if let Option::Some((_,tx)) = exchanges.remove(&response.response_to) {
                                        tx.send(response);
                                    }  else {
                                        router.response(response);
                                    }
                                }
                                inlet::Frame::Artifact(_) => {
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
                        None => {
                            break;
                        }
                    }
                }
            });
        }


        Self {
            info,
            config,
            log: logger,
            tx,
        }
    }

    pub async fn send(&self, frame: outlet::Frame) -> Result<(), Error> {
println!("SENDING OUTLET FRAME: {}", frame.to_string());
        match self.outlet_tx.send( frame ).await {
            Ok(_) => {
println!("OUTLET FRAME SENT");
            }
            Err(err) => {println!("SendError");}
        }
        Ok(())
    }

    pub fn shutdown(&mut self) {
        self.outlet_tx
            .try_send(outlet::Frame::Close(CloseReason::Done))
            .unwrap_or(());
    }
}

#[async_trait]
pub trait PortalAssignRequestHandler: Send + Sync + Debug {
    async fn default_assign(&self) -> Result<Assign, Error> {
        Err(anyhow!("request handler does not have a default assign"))
    }

    async fn handle_assign_request(&self, request: AssignRequest, mux_tx: &mpsc::Sender<MuxCall> ) -> Result<Assign, Error> {
        Err(anyhow!("request handler does not assign"))
    }

    async fn handle_artifact_request(
        &self,
        request: ArtifactRequest,
    ) -> Result<ArtifactResponse, Error> {
        Err(anyhow!("request handler does not handle artifacts"))
    }

    async fn handle_config_request(
        &self,
        request: ArtifactRequest,
    ) -> Result<ArtifactResponse, Error> {
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
impl PortalAssignRequestHandler for DefaultPortalRequestHandler {}

#[derive(Debug,strum_macros::Display)]
pub enum MuxCall {
    AddPortal(Portal),
    RemovePortal(String),
    Assign {
        assign: Exchanger<Assign>,
        portal_key: String,
    },
    RemoveResource(Address),
    SelectAll(oneshot::Sender<Vec<ResourceStub>>), // for testing only
    GetBroadcaster(oneshot::Sender<broadcast::Receiver<PortalMuxerEvent>>)
}

pub trait MeshRouter: Send + Sync {
    fn request(&self, exchange: RequestExchange );

    fn logger(&self, message: &str) {
        println!("{}", message);
    }
}

pub struct PortalMuxer {
    portals: HashMap<String, Portal>,
    address_to_portal: HashMap<Address, String>,
    address_to_assign: HashMap<Address, Assign>,
    router: Arc<dyn MeshRouter>,
    mux_tx: mpsc::Sender<MuxCall>,
    mux_rx: mpsc::Receiver<MuxCall>,
    broadcast_tx: broadcast::Sender<PortalMuxerEvent>
}

impl PortalMuxer {
    pub fn new(
        mux_tx: mpsc::Sender<MuxCall>,
        mux_rx: mpsc::Receiver<MuxCall>,
        router: Arc<dyn MeshRouter>,
    ) {
        let (broadcast_tx,_) = broadcast::channel(1024);
        let mut muxer = Self {
            portals: HashMap::new(),
            address_to_portal: HashMap::new(),
            address_to_assign: HashMap::new(),
            router,
            mux_tx,
            mux_rx,
            broadcast_tx,
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
                        MuxCall::AddPortal(portal) => {
                            let tx = portal.tx.clone();
                            muxer.portals.insert(portal.info.portal_key.clone(), portal);
                            muxer.broadcast_tx.send( PortalMuxerEvent::PortalAdded { info, tx });
                        }
                        MuxCall::RemovePortal(portal_key) => {
                            if let Some(portal) =  muxer.portals.remove(&portal_key )
                            {
                                muxer.broadcast_tx.send(PortalMuxerEvent::PortalRemoved(portal.info.clone()));
                                let mut remove = vec![];
                                for (address,portal_key) in muxer.address_to_portal {
                                    if portal_key == portal.info.portal_key {
                                        remove.push(address.clone() );
                                        muxer.broadcast_tx.send( PortalMuxerEvent::ResourceRemoved(address));
                                    }
                                }
                                for address in remove {
                                    muxer.address_to_portal.remove(&address);
                                }
                            }
                        }
                        MuxCall::Assign { assign, portal_key } => {
                            muxer
                                .address_to_assign
                                .insert(assign.stub.address.clone(), assign.item.clone());
                            muxer
                                .address_to_portal
                                .insert(assign.stub.address.clone(), portal_key.clone());
                            let portal = muxer
                                .portals
                                .get(&portal_key)
                                .ok_or(anyhow!("expected portal"))?;
                            portal.send(outlet::Frame::Assign(assign.clone())).await?;

                            muxer.router.logger(
                                format!(
                                    "INFO: added portal to muxer at address {}",
                                    assign.stub.address.to_string()
                                )
                                .as_str(),
                            );
                            muxer.broadcast_tx.send( PortalMuxerEvent::ResourceAssigned{ stub:assign.stub.clone(), tx: portal.tx.clone() });
                        }
                        MuxCall::RemoveResource(address) => {
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
                            muxer.broadcast_tx.send( PortalMuxerEvent::ResourceRemoved(address));
                        }
                        /*MuxCall::Request(exchange) => {
                            let portal_key = muxer.address_to_portal.get(&exchange.request.to);

                            match muxer.address_to_portal.get(&exchange.request.to ) {
                                Some(portal_key) => {
                                    match muxer.portals.get(portal_key ) {
                                        Some(portal) => {
                                            portal.tx.send( PortalCall::Request(exchange)).await;
                                        }
                                        None => {
                                            let response = exchange.request.fail(format!("PortalMuxer does not have portal {}",portal_key));
                                            exchange.tx.send(response);
                                        }
                                    }

                                }
                                None => {
                                    let response = exchange.request.fail(format!("PortalMuxer does not have resource '{}'",exchange.request.to.to_string()));
                                    exchange.tx.send(response);
                                }
                            }
                        }*/
                        MuxCall::SelectAll(tx) => {
                            let mut rtn = vec![];
                            for (_, assign) in &muxer.address_to_assign {
                                rtn.push(assign.stub.clone());
                            }
                            tx.send(rtn);
                        }
                        MuxCall::GetBroadcaster(tx) => {
                            tx.send( muxer.broadcast_tx.subscribe() );
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
