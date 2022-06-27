use crate::cli::{CliSessionState, CommandExecutor};
use crate::star::{CliSession, StarInternalTransmitter, StarSkel, TopicHandler};
use crate::state::ShellState;
use cosmic_nom::Res;
use cosmic_portal_cli::Cli;
use cosmic_portal_cli_exe::CliRelay;
use dashmap::mapref::one::Ref;
use dashmap::DashMap;
use futures::SinkExt;
use mesh_portal::error::MsgErr;
use mesh_portal::version::latest::entity::response::RespCore;
use mesh_portal::version::latest::id::{Point, Port, Uuid};
use mesh_portal::version::latest::log::{LogSpan, PointLogger, RootLogger};
use mesh_portal::version::latest::messaging::{Agent, ReqShell, RespShell, RootRequestCtx};
use mesh_portal_versions::error::MsgErr;
use mesh_portal_versions::version::v0_0_1::cli::RawCommand;
use mesh_portal_versions::version::v0_0_1::config::config::bind::RouteSelector;
use mesh_portal_versions::version::v0_0_1::id::id::{
    Layer, Point, Port, PortSelector, ToPoint, ToPort, Topic, TraversalLayer, Uuid,
};
use mesh_portal_versions::version::v0_0_1::id::Traversal;
use mesh_portal_versions::version::v0_0_1::log::RootLogger;
use mesh_portal_versions::version::v0_0_1::parse::ScopedVars;
use mesh_portal_versions::version::v0_0_1::quota::Timeouts;
use mesh_portal_versions::version::v0_0_1::wave::{
    Agent, AsyncInternalRequestHandlers, AsyncPointRequestHandlers, AsyncRequestHandler,
    AsyncRequestHandlerRelay, AsyncRouter, AsyncTransmitter, AsyncTransmitterWithAgent, InCtx,
    ReqShell, ReqXtra, RequestHandler, Requestable, RespCore, RespShell, RespXtra, RootInCtx, Wave,
    WaveXtra,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;
use tokio::sync::mpsc::Sender;
use tokio::sync::oneshot;

#[derive(AsyncRequestHandler)]
pub struct ShellEx {
    skel: StarSkel,
    state: ShellState,
}

impl ShellEx {
    pub fn new(skel: StarSkel, state: ShellState) -> Self {
        Self { skel, state }
    }
}

impl TraversalLayer for ShellEx {
    fn layer(&self) -> &Layer {
        &Layer::Shell
    }

    async fn traversal_router(&self, traversal: Traversal<Wave>) {
        self.skel.traversal_router.send(traversal).await;
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, oneshot::Sender<RespShell>>> {
        &self.skel.exchange
    }

    async fn layer_handle(&self, request: ReqShell) {
        let logger = self.skel.logger.point(request.to.point.clone()).span();
        let trasmitter = Arc::new(StarInternalTransmitter::new(
            self.skel.clone(),
            self.layer().clone(),
        ));
        let ctx = RootInCtx::new(request, logger, trasmitter.clone());
        let response: Result<RespShell, MsgErr> = self.handle(ctx).await;
        let wave: Wave = response.into();
        trasmitter.route(wave).await;
    }

    async fn to_fabric_request(&self, request: Traversal<ReqShell>) -> Result<(), MsgErr> {
        // check from
        if request.from.point != *request.logger.point() {
            request.logger.warn(format!("core attempted to send a message with a from Point that was not assigned to it: {}",request.from.point.to_string()));
            let mut response = request.forbidden();
            response.from = Port::new(request.logger.point().clone(), Layer::Shell, Topic::None);
            let response = request.with(response).wrap();
            self.skel.towards_core_router(response).await;
            return Ok(());
        }

        self.state.fabric_requests.insert(request.id.clone());

        self.skel.towards_core_router(response.wrap()).await;
        Ok(())
    }

    async fn to_fabric_response(&self, response: Traversal<RespShell>) -> Result<(), MsgErr> {
        // check from
        if response.from.point != *response.logger.point() {
            request.logger.warn(format!("core attempted to send a response with a from Point that was not assigned to it: {}",response.from.point.to_string()));
            return Ok(());
        }

        if self
            .state
            .core_requests
            .remove(&response.response_to)
            .is_none()
        {
            response.logger.warn("Core attempted to send a response to a request id that the Shell does not have a record of");
            return Ok(());
        }

        self.skel.traversal_router.send(response.wrap()).await;
        Ok(())
    }

    async fn to_core_request(&self, request: Traversal<ReqShell>) -> Result<(), MsgErr> {
        self.state.core_requests.insert(request.id.clone());
        self.skel.towards_core_router.send(request.wrap()).await;
        Ok(())
    }

    async fn to_core_response(&self, response: Traversal<RespShell>) -> Result<(), MsgErr> {
        if self
            .state
            .fabric_requests
            .remove(&response.response_to)
            .is_none()
        {
            response.logger.warn(
                "blocked external response to a request that Shell does not have a record of",
            );
        }
        Ok(())
    }
}

#[routes]
impl ShellEx {
    #[route("Msg<NewCli>")]
    pub async fn new_session(&self, ctx: InCtx<'_, ReqShell>) -> Result<Port, MsgErr> {
        // only allow a cli session to be created by any layer of THIS particle
        if ctx.from.clone().to_point() != ctx.to.clone().to_point() {
            return Err(MsgErr::forbidden());
        }

        let mut session_port = ctx
            .to
            .clone()
            .with_topic(Topic::uuid())
            .with_layer(Layer::Shell);

        let env = ScopedVars::new(ctx.to.clone().to_point());

        let session = CliSession {
            source_selector: ctx.from.clone().into(),
            env,
        };

        self.skel
            .state
            .topic
            .insert(session_port.clone(), Box::new(session));

        Ok(session_port)
    }
}

#[routes_async]
impl CliSession {
    #[route("Msg<Exec>")]
    pub async fn exec(&self, ctx: InCtx<'_, RawCommand>) -> Result<RespCore, MsgErr> {
        if self.state.source != ctx.req().from {
            return Err(MsgErr::forbidden());
        }

        let exec_topic = Topic::uuid();
        let exec_port = self.port.clone().with_topic(exec_topic.clone());
        let mut exec = CommandExecutor::new(
            exec_port,
            self.source_selector.clone(),
            self.state.vars.clone(),
        );

        let result = exec.execute(ctx).await;

        result
    }
}

#[derive(AsyncRequestHandler)]
pub struct CliSession {
    pub source_selector: PortSelector,
    pub env: ScopedVars,
}

impl TopicHandler for CliSession {
    fn source_selector(&self) -> &PortSelector {
        &self.source_selector
    }
}
