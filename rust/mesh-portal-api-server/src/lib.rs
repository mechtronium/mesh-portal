#![allow(warnings)]

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

use dashmap::{DashMap, DashSet};
use mesh_portal::version::latest;
use mesh_portal::version::latest::artifact::{Artifact, ArtifactRequest, ArtifactResponse};
use mesh_portal::version::latest::config::{Document, PointConfig, PortalConfig};
use mesh_portal::version::latest::entity::response;
use mesh_portal::version::latest::fail;
use mesh_portal::version::latest::frame::CloseReason;
use mesh_portal::version::latest::id::{Point, Port};
use mesh_portal::version::latest::log::{RootLogger, SpanLogger};
use mesh_portal::version::latest::messaging::{
    Agent, ProtoRequest, Request, Response, Scope, SysMethod,
};
use mesh_portal::version::latest::particle::Stub;
use mesh_portal::version::latest::payload::Payload;
use mesh_portal::version::latest::portal::inlet::AssignRequest;
use mesh_portal::version::latest::portal::outlet::{Frame, RequestFrame};
use mesh_portal::version::latest::portal::{inlet, outlet, Exchanger};
use mesh_portal::version::latest::sys::{Assign, Sys};
use mesh_portal_versions::error::MsgErr;
use mesh_portal_versions::version::v0_0_1::id::id::{Layer, ToPoint, ToPort};
use mesh_portal_versions::version::v0_0_1::wave::{
    AsyncMessenger, AsyncRequestHandler, MethodKind, ResponseCore, RootInputCtx, AsyncRouter, Wave,
    WaveFrame,
};
use std::fmt::Debug;
use tokio::task::yield_now;


#[derive(Clone)]
pub enum PortalEvent {
    PortalAdded(Point),
    PortalRemoved(Point),
    ParticleAdded(Point),
    ParticleRemoved(Point),
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum PortalStatus {
    None,
    Initializing,
    Ready,
    Panic(String),
}

#[derive(Debug, Clone)]
pub struct PortalInfo {
    pub portal_key: String,
}

pub struct Portal {
    pub info: PortalInfo,
    pub config: PortalConfig,
    outlet_tx: mpsc::Sender<WaveFrame>,
    pub logger: RootLogger,
    broadcast_tx: broadcast::Sender<PortalEvent>,
    point: Point,
    messenger: Arc<dyn AsyncMessenger>,
    assigned: Arc<DashSet<Point>>,
}

impl Portal {
    pub fn new(
        info: PortalInfo,
        config: PortalConfig,
        outlet_tx: mpsc::Sender<WaveFrame>,
        broadcast_tx: broadcast::Sender<PortalEvent>,
        logger: RootLogger,
        point: Point,
        messenger: Arc<dyn AsyncMessenger>,
    ) -> (Self, mpsc::Sender<WaveFrame>) {
        let (inlet_tx, mut inlet_rx) = mpsc::channel(1024);
        let (tx, mut rx) = mpsc::channel(1024);

        {
            let outlet_tx = outlet_tx.clone();
            let logger = logger.point(point.clone());
            let messenger = messenger.clone();
            tokio::spawn(async move {
                loop {
                    let logger = logger.clone();
                    match inlet_rx.recv().await {
                        Some(frame) => {
                            let span = frame.span;
                            match frame.message {
                                Wave::Request(request) => {
                                    match tokio::time::timeout(
                                        Duration::from_secs(config.response_timeout),
                                        messenger.send(request),
                                    )
                                    .await
                                    {
                                        Ok(Ok(response)) => {
                                            let frame = response.to_frame(span);
                                            outlet_tx.send(frame)
                                        }
                                        _ => {
                                            let response =
                                                request.fail("timeout".to_string().as_str());
                                            response
                                        }
                                    }
                                }
                                Wave::Response(response) => {
                                    let logger = logger.for_span_async(span);
                                    messenger.route(Wave::Response(response)).await;
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

        let assigned = Arc::new(DashSet::new());

        (
            Self {
                info,
                config,
                logger,
                outlet_tx,
                broadcast_tx,
                point,
                messenger,
                assigned,
            },
            inlet_tx,
        )
    }

    pub async fn assign(&self, assign: Assign) {
        let point = assign.details.stub.point.clone();
        self.assigned.insert(point.clone());
        let logger = self.logger.point(point.clone());
        let logger = logger.span();
        let stub = assign.details.stub.clone();
        let assign: Sys = assign.into();
        let assign: Payload = assign.into();
        let mut request =
            ProtoRequest::sys(Point::local_portal().clone().to_port(), SysMethod::Assign);
        request.body = assign;
        let request = request.to_request(
            point.clone().to_port().with_layer(Layer::Shell),
            Agent::Anonymous,
            Scope::Full,
        );
        let frame = request.to_frame(logger.current_span());
        self.outlet_tx.send(frame).await;

        self.broadcast_tx
            .send(PortalEvent::ParticleAdded(point));
    }

    async fn send_request(&self, request: Request) -> Response {
        match tokio::time::timeout(
            Duration::from_secs(self.config.response_timeout),
            self.messenger.send(request),
        )
        .await
        {
            Ok(Ok(response)) => response,
            _ => {
                let response = request.fail("timeout".to_string().as_str());
                response
            }
        }
    }

    pub fn shutdown(&mut self) {

    }

    pub fn has_core_port(&self, port: &Port) -> Result<(), ()> {
        if let Layer::Shell = port.layer {
            return Err(());
        }

        let point = port.clone().to_point();
        if self.point.is_parent(&point).is_ok() {
            return Ok(());
        }

        if self.assigned.contains(&point) {
            Ok(())
        }

        Err(())
    }
}

#[async_trait]
impl AsyncRouter for Portal {
    async fn route(&self, wave: Wave) {
        match wave {
            Wave::Request(request) => {
                if self.has_core_port(&request.to).is_err() {
                    self.messenger.route(request.not_found().into() ).await;
                    return;
                }

                // don't allow anyone to say this request came from itself
                if  self.has_core_port(&request.from).is_ok() {
                    self.messenger.route(request.forbidden().into() ).await;
                    return;
                }

                // the portal shell sends Sys messages...
                if request.core.method.kind() == MethodKind::Sys {
                    self.messenger.route(request.forbidden().into() ).await;
                    return;
                }
                let logger = self.logger.point(request.to.clone().to_point());
                let span = logger.span_async().current_span();
                let frame = request.to_frame(span);
                self.outlet_tx.send(frame).await;
            }
            Wave::Response(response) => {
                // Portal only handles requests (it should be expecting the response)
            }
        }
    }
}



#[cfg(test)]
pub mod test {

    #[test]
    pub fn test() {}
}
