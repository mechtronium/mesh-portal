#![allow(warnings)]
#[macro_use]
extern crate async_trait;

#[macro_use]
extern crate anyhow;

use std::collections::HashSet;
use dashmap::{DashMap, DashSet};
use mesh_portal::error::MsgErr;
use mesh_portal::version::latest::id::{Point, Port, Uuid};
use mesh_portal::version::latest::messaging::{Agent, RequestCtx, SysMethod};
use mesh_portal_versions::version::v0_0_1::id::id::{TargetLayer, ToPoint, ToPort};
use mesh_portal_versions::version::v0_0_1::messaging::{
    AsyncInternalRequestHandlers, AsyncMessenger, AsyncMessengerAgent, AsyncRequestHandler,
    AsyncRequestHandlerRelay, AsyncRouter, Request, RequestFrame, RequestHandlerRelay, Requestable,
    Response, ResponseFrame, WaitTime, Wave, WaveFrame,
};
use mesh_portal_versions::version::v0_0_1::quota::Timeouts;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;
use tokio::sync::oneshot;
use mesh_portal::version::latest::entity::response::ResponseCore;
use mesh_portal::version::latest::payload::Payload;
use mesh_portal::version::latest::sys::{Assign, Sys};

pub struct Portal {
    pub port: Port,
    pub assigned: Arc<DashSet<Point>>,
    pub messenger: Arc<dyn AsyncMessenger<RequestFrame, ResponseFrame>>,
    pub handlers: AsyncInternalRequestHandlers<AsyncRequestHandlerRelay>,
    pub auth
}

impl Portal {
    pub async fn new(
        inlet_tx: mpsc::Sender<WaveFrame>,
        outlet_rx: mpsc::Receiver<WaveFrame>,
    ) -> Result<Self, MsgErr> {
        let assigned = Arc::new(DashSet::new());
        let messenger = Arc::new(PortalMessenger::new(inlet_tx, assigned.clone()));

        async fn listen_for_point(
            mut outlet_rx: mpsc::Receiver<WaveFrame>
        ) -> Result<(Point, mpsc::Receiver<WaveFrame>), MsgErr> {
            while let Ok(frame) = tokio::time::timeout(
                Duration::from_secs(Timeouts::default().from(&WaitTime::High)),
                outlet_rx.recv(),
            ).await
            {
                if let Ok(frame) = frame {
                    if let Wave::Request(request) = frame.wave {
                        let point: Point = request
                            .require_method(SysMethod::AssignPort)?
                            .require_body("Point")?;
                        return Ok((point, outlet_rx));
                    } // else do nothing...
                } else {
                    return Err(MsgErr::server_error());
                }
            }
            Err(MsgErr::timeout())
        }

        let (point, mut outlet_rx) = listen_for_point(outlet_rx).await?;
        assigned.insert(point.clone());

        let handlers = AsyncInternalRequestHandlers::new();

       let mut port= point.to_port().with_layer(TargetLayer::Core);

       {
          let handlers = handlers.clone();
          tokio::spawn(async move {
             while let Ok(frame) = outlet_rx.recv().await {}
          });
       }

        Ok(Self {
            port,
           assigned,
           messenger,
            handlers,
        })
    }

    pub fn has_core(&self, point: &Point) -> Result<(), ()> {
        if self.port.point == *point {
            return Ok(());
        }
        if self.assigned.contains_key(point) {
            Ok(())
        } else {
            Err(())
        }
    }
}

pub struct PortalMessenger {
    inlet_tx: mpsc::Sender<WaveFrame>,
    exchanges: Arc<DashMap<Uuid, oneshot::Sender<ResponseFrame>>>,
    assigned: Arc<DashSet<Point>>,
    timeouts: Timeouts,
}

impl PortalMessenger {
    pub fn new(inlet_tx: mpsc::Sender<WaveFrame>, assigned: Arc<DashSet<Point>>) -> Self {
        Self {
            inlet_tx,
            exchanges: Arc::new(DashMap::new()),
            assigned,
            timeouts: Default::default(),
        }
    }
}

impl AsyncMessenger<RequestFrame, ResponseFrame> for PortalMessenger {
    async fn send(&self, request: RequestFrame) -> ResponseFrame {
        if !self.assigned.contains(&request.from().to_point()) {
            return request.forbidden();
        }

        let (tx, rx) = oneshot::channel();
        self.exchanges.insert(request.id(), tx);
        self.inlet_tx.send(request.into()).await;

        if let Ok(frame) =
            tokio::time::timeout(Duration::from_secs(self.timeouts.from(&request)), rx).await
        {
            if let Ok(response) = frame {
                response
            } else {
                request.server_error()
            }
        } else {
            request.timeout()
        }
    }

    fn send_sync(&self, request: RequestFrame) -> ResponseFrame {
        let (tx, rx) = oneshot::channel();
        self.exchanges.insert(request.id(), tx);
        let response = tokio::runtime::Handle::current().block_on(async move {
            let stub = request.as_stub();
            self.inlet_tx.send(request.into()).await;
            if let Ok(result) =
                tokio::time::timeout(Duration::from_secs(self.timeouts.from(&request)), rx).await
            {
                if let Ok(frame) = result {
                    frame
                } else {
                    stub.server_error()
                }
            } else {
                stub.timeout()
            }
        });
        response
    }

    async fn route(&self, wave: Wave) {
    }
}

pub trait PortalCtrlFactory {
   fn create(&self, Assign, AsyncMessengerAgent) -> Result<Box<dyn AsyncRequestHandler>,MsgErr>;
}



#[derive(AsyncRequestHandler)]
pub struct PortalRequestHandler {
    factory: Box<dyn PortalCtrlFactory>,
    handlers: Arc<DashMap<Point,Box<dyn AsyncRequestHandler>>>,
    messenger: Arc<dyn AsyncMessenger<Request,Response>>
}

#[routes_async(self.handlers)]
impl PortalRequestHandler {

   pub fn new( messenger: Arc<dyn AsyncMessenger<Request,Response>>, factory: Box<dyn PortalCtrlFactory>) -> Self {
      Self {
         factory,
         handlers: Arc::new(DashMap::new()),
         messenger
      }
   }

   #[route_async(Sys<Assign>)]
   pub async fn assign( &self, request: RequestCtx<Assign>) -> Result<ResponseCore,MsgErr> {
      let messenger = AsyncMessengerAgent::new(Agent::Anonymous, request.details.stub.point.to_port().with_layer(TargetLayer::Core), self.messenger.clone() );
      self.handlers.insert( request.details.stub.point.clone(), self.factory.create(request.input.clone(),messenger)?);
      Ok(ResponseCore::ok(Payload::Empty))
   }

}
