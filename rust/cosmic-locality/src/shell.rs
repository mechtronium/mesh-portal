use cosmic_nom::Res;
use cosmic_portal_cli::Cli;
use cosmic_portal_cli_exe::CliRelay;
use dashmap::DashMap;
use futures::SinkExt;
use mesh_portal::error::MsgErr;
use mesh_portal::version::latest::entity::response::RespCore;
use mesh_portal::version::latest::id::{Point, Port, Uuid};
use mesh_portal::version::latest::log::{LogSpan, PointLogger, RootLogger};
use mesh_portal::version::latest::messaging::{Agent, ReqShell, RespShell, RootRequestCtx};
use mesh_portal_versions::version::v0_0_1::config::config::bind::RouteSelector;
use mesh_portal_versions::version::v0_0_1::id::id::{Layer, Point, Port, Topic, ToPoint, ToPort, TraversalLayer, Uuid};
use mesh_portal_versions::version::v0_0_1::quota::Timeouts;
use mesh_portal_versions::version::v0_0_1::wave::{AsyncInternalRequestHandlers, AsyncPointRequestHandlers, AsyncRequestHandler, AsyncRequestHandlerRelay, AsyncRouter, AsyncTransmitter, AsyncTransmitterWithAgent, ReqCtx, ReqXtra, Requestable, RespXtra, Wave, WaveXtra, RequestHandler, RespShell, Agent, ReqShell, RespCore};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;
use tokio::sync::mpsc::Sender;
use tokio::sync::oneshot;
use mesh_portal_versions::error::MsgErr;
use mesh_portal_versions::version::v0_0_1::id::Traversal;
use mesh_portal_versions::version::v0_0_1::log::RootLogger;
use crate::star::StarSkel;
use crate::state::ShellState;

#[derive(AsyncRequestHandler)]
pub struct ShellEx {
    skel: StarSkel,
    state: ShellState,
}

impl ShellEx {
    pub fn new(skel: StarSkel, state: ShellState) -> Self {
        Self {
            skel,
            state
        }
    }

}

impl TraversalLayer for ShellEx {
    fn layer(&self) -> &Layer {
        &Layer::Shell
    }

    fn towards_fabric_router(&self) -> &Sender<Traversal<Wave>> {
        &self.skel.towards_fabric_router
    }

    fn towards_core_router(&self) -> &Sender<Traversal<Wave>> {
        &self.skel.towards_core_router
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid,oneshot::Sender<RespShell>>> {
        &self.skel.exchange
    }

    async fn handle(&self, request: ReqShell) {
        todo!()
    }

    async fn outgoing_request(&self, request: Traversal<ReqShell> ) -> Result<(),MsgErr> {
        // check from
        if request.from.point != *request.logger.point()  {
            request.logger.warn(format!("core attempted to send a message with a from Point that was not assigned to it: {}",request.from.point.to_string()));
            let mut response = request.forbidden();
            response.from = Port::new( request.logger.point().clone(), Layer::Shell, Topic::None );
            let response = request.with(response).wrap();
            self.skel.towards_core_router(response).await;
            return Ok(());
        }

        self.state.outgoing_requests.insert(request.id.clone());

        self.skel.towards_core_router(response.wrap()).await;
        Ok(())
    }

    async fn outgoing_response(&self, response: Traversal<RespShell> ) -> Result<(),MsgErr>{
        // check from
        if response.from.point != *response.logger.point()  {
            request.logger.warn(format!("core attempted to send a response with a from Point that was not assigned to it: {}",response.from.point.to_string()));
            return Ok(());
        }

        if self.state.incoming_requests.remove(&response.response_to).is_none() {
            response.logger.warn("Core attempted to send a response to a request id that the Shell does not have a record of");
            return Ok(());
        }

        self.skel.towards_fabric_router.send(response.wrap() ).await;
        Ok(())
    }


    async fn incoming_request(&self, request: Traversal<ReqShell> ) -> Result<(),MsgErr>{
        self.state.incoming_requests.insert(request.id.clone());
        self.skel.towards_core_router.send(request.wrap()).await;
        Ok(())
    }

    async fn incoming_response(&self, response: Traversal<RespShell> ) -> Result<(),MsgErr>{
        if self.state.outgoing_requests.remove(&response.response_to).is_none() {
            response.logger.warn("blocked external response to a request that Shell does not have a record of");
        }
        Ok(())
    }
}

#[routes]
impl ShellEx {
    #[route("<*>")]
    pub async fn any(&self, ctx: ReqCtx<'_, ReqShell>) -> Result<RespCore, MsgErr> {
        let (tx, rx) = oneshot::channel();
        self.exchanges.insert(ctx.input.id.clone(), tx);
        self.core_tx.send((*ctx.input).into()).await;
        if let Ok(frame) = tokio::time::timeout(
            Duration::from_secs(
                self.timeouts
                    .from(self.timeouts.from(&ctx.input.handling.wait)),
            ),
            rx,
        )
        .await
        {
            if let Ok(response) = frame {
                Ok(response.core)
            }
        }
        Ok(ctx.server_error().into())
    }
}
