use crate::star::StarSkel;
use anyhow::anyhow;
use dashmap::DashMap;
use http::{HeaderMap, StatusCode, Uri};
use mesh_artifact_api::Artifact;
use mesh_portal::version::latest::config::bind::{
    BindConfig, Pipeline, PipelineStep, PipelineStop, StepKind,
};
use mesh_portal::version::latest::entity::request::get::{Get, GetOp};
use mesh_portal::version::latest::entity::request::{Method, Rc, ReqCore};
use mesh_portal::version::latest::entity::response::RespCore;
use mesh_portal::version::latest::id::Point;
use mesh_portal::version::latest::log::{PointLogger, SpanLogger};
use mesh_portal::version::latest::messaging::{Agent, Message, ReqShell, RespShell};
use mesh_portal::version::latest::msg::MsgMethod;
use mesh_portal::version::latest::payload::{Call, CallKind, Substance};
use mesh_portal::version::latest::security::Access;
use mesh_portal_versions::error::MsgErr;
use mesh_portal_versions::version::v0_0_1::config::config::bind::{
    BindConfig, MessageKind, PipelineStepVar, PipelineStopVar,
};
use mesh_portal_versions::version::v0_0_1::id::id::{
    Layer, Point, ToPoint, ToPort, TraversalLayer, Uuid,
};
use mesh_portal_versions::version::v0_0_1::id::ArtifactSubKind;
use mesh_portal_versions::version::v0_0_1::id::Traversal;
use mesh_portal_versions::version::v0_0_1::log::{PointLogger, RootLogger, SpanLogger};
use mesh_portal_versions::version::v0_0_1::parse::model::PipelineVar;
use mesh_portal_versions::version::v0_0_1::parse::{
    Env, MapResolver, MultiVarResolver, PointCtxResolver, RegexCapturesResolver,
};
use mesh_portal_versions::version::v0_0_1::security::Access;
use mesh_portal_versions::version::v0_0_1::selector::selector::PipelineKind;
use mesh_portal_versions::version::v0_0_1::selector::{PayloadBlock, PayloadBlockVar};
use mesh_portal_versions::version::v0_0_1::substance::substance::{Call, CallKind, Substance};
use mesh_portal_versions::version::v0_0_1::sys::ParticleRecord;
use mesh_portal_versions::version::v0_0_1::util::{ToResolved, ValueMatcher};
use mesh_portal_versions::version::v0_0_1::wave::{
    Agent, CmdMethod, Method, ReqCore, ReqShell, Requestable, RespCore, RespShell, Wave,
};
use nom::combinator::map_res;
use regex::{CaptureMatches, Regex};
use starlane_core::artifact::ArtifactRef;
use starlane_core::error::Error;
use starlane_core::message::delivery::Delivery;
use starlane_core::star::core::particle::driver::ResourceCoreDriverApi;
use starlane_core::star::StarSkel;
use std::collections::HashMap;
use std::pin::Pin;
use std::str::FromStr;
use std::sync::Arc;
use tokio::io::AsyncBufReadExt;
use tokio::sync::mpsc::Sender;
use tokio::sync::{mpsc, Mutex};

/// The idea here is to eventually move this funcitionality into it's own crate 'mesh-bindex'
/// this mod basically enforces the bind

#[derive(Clone)]
pub struct FieldState {
    pipe_exes: Arc<DashMap<String, PipeEx>>,
}

impl FieldState {
    pub fn new() -> Self {
        Self {
            pipe_exes: Arc::new(DashMap::new()),
        }
    }
}

#[derive(Clone)]
pub struct FieldEx {
    pub skel: StarSkel,
    pub state: FieldState,
}

fn request_id(request: &ReqShell) -> String {
    format!("{}{}", request.to.to_string(), request.id)
}

fn request_id_from_response(response: &RespShell) -> String {
    format!("{}{}", response.from.to_string(), response.response_to)
}

impl FieldEx {
    pub fn new(skel: StarSkel, state: FieldState) -> Self {
        Self { skel, state }
    }

    async fn handle_action(&self, action: RequestAction) -> anyhow::Result<()> {
        match action.action {
            PipeAction::CoreRequest(mut request) => {
                let request = request.with(Wave::Req(request.payload));
                self.skel.towards_core_router.send(request).await;
            }
            PipeAction::FabricRequest(mut request) => {
                let request = request.with(Wave::Req(request.payload));
                self.skel.traversal_router.send(request).await;
            }
            PipeAction::Respond => {
                let pipex = self.state.pipe_exes.remove(&action.request_id);

                match pipex {
                    None => {
                        error!("no pipeline set for requst_id: {}", action.request_id);
                    }
                    Some((_, mut pipex)) => {
                        self.skel.traversal_router.send(pipex.respond()).await;
                    }
                }
            }
        }
        Ok(())
    }
}

#[async_trait]
impl TraversalLayer for FieldEx {
    fn layer(&self) -> &Layer {
        &Layer::Field
    }

    async fn towards_fabric_router(&self, traversal: Traversal<Wave>) {
        self.skel.traversal_router.send(traversal).await;
    }

    async fn towards_core_router(&self, traversal: Traversal<Wave>) {
        self.skel.towards_core_router.send(traversal).await;
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, tokio::sync::oneshot::Sender<RespShell>>> {
        &self.skel.exchange
    }

    async fn layer_handle(&self, request: ReqShell) {
        // not sure if field will ever handle anything for itself
    }

    async fn to_core_request(&self, mut request: Traversal<ReqShell>) -> Result<(), MsgErr> {
        info!("BindEx: handle_request");
        request.logger.set_span_attr("message-id", &request.id);
        let access = self.skel.registry.access(&request.agent, &request.to).await;

        match access {
            Ok(access) => {
                if !access.permissions().particle.execute {
                    let err_msg = format!(
                        "execute permission required to send requests to {}",
                        request.to.to_string()
                    );
                    request.logger.error(err_msg.as_str());
                    self.skel
                        .fabric
                        .send(Wave::Resp(request.err(err_msg.into())));
                    return Ok(());
                }
            }
            Err(err) => {
                error!("{}", err.to_string())
            }
        }

        let bind = self.skel.machine.artifacts.bind(&request.to).await?;
        let route = bind.select(&request.item)?;

        let regex = route.selector.path.clone();

        let env = {
            let path_regex_capture_resolver =
                RegexCapturesResolver::new(regex, request.item.core.uri.path().to_string())?;
            let mut env = Env::new(request.item.to.clone().to_point());
            env.add_var_resolver(Arc::new(path_regex_capture_resolver));
            env.set_var("self.bundle", bind.bundle()?.to_string().as_str());
            env.set_var("self.bind", bind.point().clone().to_string().as_str());
            env
        };

        let request_id = request_id(&request.item);

        let pipeline = route.block.clone();

        let call = request.to_call()?;
        let logger = request.logger.span();
        let mut pipex = PipeEx::new(request, self.clone(), pipeline, env, logger.clone());
        let action = match pipex.next() {
            Ok(action) => action,
            Err(err) => {
                let err_msg = format!("Binder: pipeline error for call {}", call.to_string());
                logger.error(err_msg.as_str());
                self.skel
                    .traversal_router
                    .send(pipex.fail(500, err_msg.as_str()))
                    .await;
                return Ok(());
            }
        };

        if let PipeAction::Respond = action {
            self.skel.traversal_router.send(pipex.respond()).await;
            return Ok(());
        }

        self.state.pipe_exes.insert(request_id.clone(), pipex);

        let action = RequestAction { request_id, action };

        self.handle_action(action);
        Ok(())
    }

    async fn to_core_response(&self, mut traversal: Traversal<RespShell>) -> Result<(), MsgErr> {
        let request_id = request_id_from_response(&response);
        let mut pipex = self.state.pipe_exes.remove(&request_id);

        if let None = pipex {
            let err_msg = format!(
                "Binder: cannot locate a pipeline executor for processing request: {}",
                response.response_to
            );
            traversal.logger.span().error(err_msg.clone());
            error!("{}", err_msg);
            return Err(err_msg.into());
        }

        let (_, mut pipex) = pipex.expect("pipeline executor");

        let action = pipex.handle_response(response)?;

        if let PipeAction::Respond = action {
            self.skel.traversal_router.send(pipex.respond()).await;
            return Ok(());
        }

        self.state.pipe_exes.insert(request_id.clone(), pipex);

        let action = RequestAction { request_id, action };

        self.handle_action(action);

        Ok(())
    }
}

pub struct PipeEx {
    pub logger: SpanLogger,
    pub traversal: PipeTraversal,
    pub binder: FieldEx,
    pub pipeline: PipelineVar,
    pub env: Env,
}

impl PipeEx {
    pub fn new(
        traversal: Traversal<ReqShell>,
        binder: FieldEx,
        pipeline: PipelineVar,
        env: Env,
        logger: SpanLogger,
    ) -> Self {
        let traversal = PipeTraversal::new(traversal);
        Self {
            traversal: traversal,
            binder,
            pipeline,
            env,
            logger,
        }
    }
}

impl PipeEx {
    pub fn next(&mut self) -> anyhow::Result<PipeAction> {
        match self.pipeline.consume() {
            Some(segment) => {
                self.execute_step(&segment.step)?;
                Ok(self.execute_stop(&segment.stop)?)
            }
            None => Ok(PipeAction::Respond),
        }
    }

    pub fn handle_response(&mut self, response: RespShell) -> anyhow::Result<PipeAction> {
        self.traversal.push(Message::Resp(response));
        self.next()
    }

    fn respond(self) -> Traversal<Wave> {
        self.traversal.respond()
    }

    fn fail(self, status: u16, error: &str) -> Traversal<Wave> {
        self.traversal.fail(status, error)
    }

    fn execute_stop(&mut self, stop: &PipelineStopVar) -> Result<PipeAction, MsgErr> {
        match stop {
            PipelineStopVar::Internal => {
                let request = self.traversal.request();
                Ok(PipeAction::CoreRequest(request))
            }
            PipelineStopVar::Call(call) => {
                let call: Call = call.clone().to_resolved(&self.env)?;
                let (method, path) = match &call.kind {
                    CallKind::Msg(msg) => {
                        let path = msg.path.clone().to_resolved(&self.env)?;
                        (Method::Msg(msg.method.clone()), path)
                    }
                    CallKind::Http(http) => {
                        let path = http.path.clone().to_resolved(&self.env)?;
                        (Method::Http(http.method.clone()), path)
                    }
                };
                let mut core: ReqCore = method.into();
                core.body = self.traversal.body.clone();
                core.headers = self.traversal.headers.clone();
                core.uri = Uri::from_str(path.as_str())?;
                let request = self.traversal.initial.clone().with(ReqShell::new(
                    core,
                    self.traversal.to(),
                    call.point,
                ));
                Ok(PipeAction::FabricRequest(request))
            }
            PipelineStopVar::Respond => Ok(PipeAction::Respond),
            PipelineStopVar::Point(point) => {
                let uri = self.traversal.uri.clone();
                let point: Point = point.clone().to_resolved(&self.env)?;
                let method = Method::Cmd(CmdMethod::Read);
                let mut core = method.into();
                core.uri = uri;

                let request = self.traversal.initial.clone().with(ReqShell::new(
                    core,
                    self.traversal.to(),
                    point,
                ));
                Ok(PipeAction::FabricRequest(request))
            }
        }
    }

    fn execute_step(&self, step: &PipelineStepVar) -> Result<(), MsgErr> {
        match &step.entry {
            StepKind::Request => {
                for block in &step.blocks {
                    self.execute_block(block)?;
                }
            }
            StepKind::Response => {}
        }
        Ok(())
    }

    fn execute_block(&self, block: &PayloadBlockVar) -> Result<(), MsgErr> {
        let block: PayloadBlock = block.clone().to_resolved(&self.env)?;
        match block {
            PayloadBlock::RequestPattern(pattern) => {
                pattern.is_match(&self.traversal.body)?;
            }
            PayloadBlock::ResponsePattern(pattern) => {
                pattern.is_match(&self.traversal.body)?;
            }
        }
        Ok(())
    }
}

pub struct PipeTraversal {
    pub initial: Traversal<ReqShell>,
    pub method: Method,
    pub body: Substance,
    pub uri: Uri,
    pub headers: HeaderMap,
    pub status: StatusCode,
}

impl PipeTraversal {
    pub fn new(initial_request: Traversal<ReqShell>) -> Self {
        Self {
            method: initial_request.core.method.clone(),
            body: initial_request.item.core.body.clone(),
            uri: initial_request.item.core.uri.clone(),
            headers: initial_request.item.core.headers.clone(),
            initial: initial_request,
            status: StatusCode::from_u16(200).unwrap(),
        }
    }

    pub fn request_core(&self) -> ReqCore {
        ReqCore {
            headers: self.headers.clone(),
            method: self.method.clone(),
            uri: self.uri.clone(),
            body: self.body.clone(),
        }
    }

    pub fn to(&self) -> Point {
        self.initial.to.clone().to_point()
    }

    pub fn from(&self) -> Point {
        self.initial.from.clone().to_point()
    }

    pub fn request(&self) -> Traversal<ReqShell> {
        self.initial
            .clone()
            .with(ReqShell::new(self.request_core(), self.from(), self.to()))
    }

    pub fn response_core(&self) -> RespCore {
        RespCore {
            headers: self.headers.clone(),
            body: self.body.clone(),
            status: self.status.clone(),
        }
    }

    pub fn response(&self) -> RespShell {
        RespShell::new(
            self.response_core(),
            self.to().to_port(),
            self.from().to_port(),
            self.initial.id.clone(),
        )
    }

    pub fn push(&mut self, message: Message) {
        match message {
            Message::Req(request) => {
                self.method = request.core.method;
                self.uri = request.core.uri;
                self.headers = request.core.headers;
                self.body = request.core.body;
            }
            Message::Resp(response) => {
                self.headers = response.core.headers;
                self.body = response.core.body;
                self.status = response.core.status;
            }
        }
    }

    pub fn respond(self) -> Traversal<Wave> {
        let core = self.response_core();
        let response = self.initial.core(core);
        self.initial.with(Wave::Resp(response))
    }

    pub fn fail(self, status: u16, error: &str) -> Traversal<Wave> {
        let response = self.initial.fail(status, error);
        self.initial.with(Wave::Resp(response))
    }
}

pub trait RegistryApi: Send + Sync {
    async fn access(&self, to: &Agent, on: &Point) -> anyhow::Result<Access>;
    async fn locate(&self, particle: &Point) -> anyhow::Result<ParticleRecord>;
}

struct RequestAction {
    pub request_id: String,
    pub action: PipeAction,
}

pub enum PipeAction {
    CoreRequest(Traversal<ReqShell>),
    FabricRequest(Traversal<ReqShell>),
    Respond,
}
