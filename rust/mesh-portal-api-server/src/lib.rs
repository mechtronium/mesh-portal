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
use futures::{FutureExt, SinkExt};
use tokio::sync::mpsc::error::{SendError, SendTimeoutError, TryRecvError};
use tokio::sync::{broadcast, mpsc, oneshot};

use mesh_portal::version::latest;
use mesh_portal::version::latest::entity::response;
use mesh_portal::version::latest::fail;
use mesh_portal::version::latest::frame::CloseReason;
use mesh_portal::version::latest::id::Address;
use mesh_portal::version::latest::messaging::{Message, Request,  Response};
use mesh_portal::version::latest::pattern::AddressKindPattern;
use mesh_portal::version::latest::portal::{Exchanger, inlet, outlet};
use mesh_portal::version::latest::resource::ResourceStub;
use mesh_portal::version::latest::resource::Status;
use std::fmt::Debug;
use dashmap::DashMap;
use tokio::task::yield_now;
use mesh_portal::version::latest::artifact::{Artifact, ArtifactRequest, ArtifactResponse};
use mesh_portal::version::latest::config::{Assign, Config, ConfigBody, PortalConfig};
use mesh_portal::version::latest::portal::inlet::{AssignRequest, Log};
use mesh_portal::version::latest::portal::outlet::Frame;

#[derive(Debug,Clone)]
pub struct PortalApi {
    pub info: PortalInfo,
    tx: mpsc::Sender<PortalCall>
}

impl PortalApi {
    pub async fn handle_request( &self, request: Request ) -> Response {
        let (tx,rx) = oneshot::channel();
        self.tx.send( PortalCall::Request { request: request.clone(), tx }).await;
        match tokio::time::timeout(Duration::from_secs(60), rx ).await {
            Ok(Ok(response)) => {
                response
            }
            _ => {
                request.fail("timeout".to_string().as_str() )
            }
        }
    }

    pub fn assign(&self, assign: Assign)  {
        let tx = self.tx.clone();
        tokio::spawn(async move  {
            tx.send( PortalCall::Assign(assign)).await;
        });
    }
}

#[derive(Clone)]
pub enum PortalEvent {
    PortalAdded(PortalApi),
    PortalRemoved(String),
    ResourceAdded(PortalResourceApi),
    ResourceRemoved(Address)
}

enum PortalCall {
  Request{ request: Request, tx: oneshot::Sender<Response>},
  Assign(Assign),
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum PortalStatus {
    None,
    Initializing,
    Ready,
    Panic(String),
}

#[derive(Debug,Clone)]
pub struct PortalInfo {
    pub portal_key: String
}

#[derive(Debug)]
pub struct Portal {
    pub info: PortalInfo,
    pub config: PortalConfig,
    outlet_tx: mpsc::Sender<outlet::Frame>,
    exchanges: Arc<DashMap<String,oneshot::Sender<Response>>>,
    pub log: fn(log: Log),
    tx: mpsc::Sender<PortalCall>,
    broadcast_tx: broadcast::Sender<PortalEvent>
}

impl Portal {
    pub fn new(
        info: PortalInfo,
        config: PortalConfig,
        outlet_tx: mpsc::Sender<outlet::Frame>,
        request_handler: Arc<dyn PortalRequestHandler>,
        broadcast_tx: broadcast::Sender<PortalEvent>,
        logger: fn(log: Log),
    ) -> (Self,mpsc::Sender<inlet::Frame>) {
        let (inlet_tx,mut inlet_rx) = mpsc::channel(1024);
        let exchanges: Arc<DashMap<String,oneshot::Sender<Response>>> = Arc::new( DashMap::new() );
        let (tx,mut rx) = mpsc::channel(1024);
        let portal_api = PortalApi {
          tx: tx.clone(),
          info: info.clone()
        };
        {
            let config = config.clone();
            let exchanges = exchanges.clone();
            let outlet_tx = outlet_tx.clone();
            let portal_api = portal_api.clone();
            let broadcast_tx = broadcast_tx.clone();
            tokio::spawn(async move {
               while let Some(call) = rx.recv().await {
                   match call {
                       PortalCall::Request { request, tx } => {
                           let exchanges = exchanges.clone();
                           let outlet_tx = outlet_tx.clone();
                           tokio::spawn( async move {
                               exchanges.insert( request.id.clone(), tx );
                               outlet_tx.send( outlet::Frame::Request(request.clone()) ).await;
                           });
                       }
                       PortalCall::Assign(assign) => {
                           let portal_api = portal_api.clone();
                           let assign = Exchanger::new(assign);
                           let stub = assign.stub.clone();
                           outlet_tx.send(outlet::Frame::Assign(assign)).await;
                           let resource_api = PortalResourceApi {
                               stub,
                               portal_api: portal_api
                           };
                           broadcast_tx.send( PortalEvent::ResourceAdded(resource_api));
                       }
                   }
               }
            });
        }

        {
            let request_handler = request_handler.clone();
            let info = info.clone();
            let portal_config = config.clone();
            let outlet_tx = outlet_tx.clone();
            let exchanges = exchanges.clone();
            tokio::spawn(async move {
                loop {
                    match inlet_rx.recv().await {
                        Some(frame) => {
                            let frame:inlet::Frame = frame;
                            match frame {
                                inlet::Frame::Log(log) => {
                                    (logger)(log);
                                }
                                inlet::Frame::AssignRequest(request) => {
                                    let result = request_handler.handle_assign_request(request.item.clone() ).await;
                                    match result {
                                        Ok(assignment) => {
                                            let assign = request.with(assignment);
                                            outlet_tx.send( outlet::Frame::Assign(assign) ).await;
                                        }
                                        Err(error) => {
                                            println!("{}",error.to_string());
                                        }
                                    }
                                }
                                inlet::Frame::Request(request) => {
                                    let request_handler = request_handler.clone();
                                    let outlet_tx = outlet_tx.clone();
                                    tokio::spawn(async move {
                                        match tokio::time::timeout(Duration::from_secs(portal_config.response_timeout ), request_handler.route_to_mesh(request.clone()) ).await {
                                            Ok(response) => {
                                                outlet_tx.send( outlet::Frame::Response(response)).await;
                                            }
                                            _ => {
                                                let response = request.fail("timeout".to_string().as_str());
                                                outlet_tx.send( outlet::Frame::Response(response)).await;
                                            }
                                        }
                                    });
                                }
                                inlet::Frame::Response(response) => {
                                    if let Option::Some((_,mut tx)) = exchanges.remove(&response.response_to) {
                                        tx.send(response);
                                    } else {
                                        (logger)(Log::new( "Portal", "response had no listening request" ));
                                    }
                                }
                                inlet::Frame::Artifact(request) => {
                                    let request_handler = request_handler.clone();
                                    let outlet_tx = outlet_tx.clone();
                                    tokio::spawn( async move {
                                        match request_handler.handle_artifact_request(request.item.clone()).await {
                                            Ok(response ) => {
                                                let response = request.with(response);
                                                outlet_tx.send( outlet::Frame::Artifact(response)).await;
                                            }
                                            Err(err) => {
                                                (logger)(Log::new( "Portal", err.to_string().as_str() ));
                                            }
                                        }
                                    });
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


        (Self {
            info,
            config,
            log: logger,
            exchanges,
            outlet_tx,
            broadcast_tx,
            tx
        },inlet_tx)
    }


    pub fn api(&self) -> PortalApi {
        PortalApi {
            info: self.info.clone(),
            tx: self.tx.clone()
        }
    }

    pub async fn handle_request(&self, request: Request ) -> Response {
        let (tx,rx) = oneshot::channel();
        self.exchanges.insert( request.id.clone(), tx );
        self.outlet_tx.send( outlet::Frame::Request(request.clone()) ).await;
        match tokio::time::timeout(Duration::from_secs(self.config.response_timeout ), rx ).await {
            Ok(Ok(response)) => {
                response
            }
            _ => {
                let response = request.fail("timeout".to_string().as_str() );
                response
            }
        }
    }

    pub fn assign(&self, assign: Assign ) {
        let outlet_tx = self.outlet_tx.clone();
        tokio::spawn(async move {
            let assign = Exchanger::new(assign);
            outlet_tx.send(outlet::Frame::Assign(assign)).await;
        });
    }

    pub fn shutdown(&mut self) {
        self.outlet_tx
            .try_send(outlet::Frame::Close(CloseReason::Done))
            .unwrap_or(());
    }
}

#[async_trait]
pub trait PortalRequestHandler: Send + Sync {

    async fn route_to_mesh(&self, request: Request ) -> Response;

    async fn default_assign(&self) -> Result<Assign, Error> {
        Err(anyhow!("request handler does not have a default assign"))
    }

    async fn handle_assign_request(&self, request: AssignRequest ) -> Result<Assign, Error> {
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


#[derive(Debug,Clone)]
pub struct PortalResourceApi {
   portal_api: PortalApi,
   pub stub: ResourceStub
}

impl PortalResourceApi {
   pub async fn handle_request( &self, request: Request ) -> Response {
       self.portal_api.handle_request(request).await
   }
}