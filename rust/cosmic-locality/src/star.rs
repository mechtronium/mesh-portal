use crate::cli::{CliSessionState, CommandExecutor};
use crate::driver::Drivers;
use crate::field::{FieldEx, RegistryApi};
use crate::machine::MachineSkel;
use crate::portal::{PortalInlet, PortalShell};
use crate::router::StarRouter;
use crate::shell::ShellEx;
use crate::state::{
    DriverState, FieldState, ParticleStates, PortalInletState, PortalShellState, ShellState,
};
use cosmic_lanes::HyperWave;
use cosmic_locality::driver::Drivers;
use dashmap::mapref::one::Ref;
use dashmap::DashMap;
use mesh_portal_versions::error::MsgErr;
use mesh_portal_versions::version::v0_0_1::cli::RawCommand;
use mesh_portal_versions::version::v0_0_1::id::id::{
    Layer, Point, Port, PortSelector, RouteSeg, ToPoint, ToPort, Topic, TraversalLayer, Uuid,
};
use mesh_portal_versions::version::v0_0_1::id::{StarKey, StarSub};
use mesh_portal_versions::version::v0_0_1::id::{Traversal, TraversalDirection};
use mesh_portal_versions::version::v0_0_1::log::PointLogger;
use mesh_portal_versions::version::v0_0_1::parse::ScopedVars;
use mesh_portal_versions::version::v0_0_1::quota::Timeouts;
use mesh_portal_versions::version::v0_0_1::substance::substance::Substance;
use mesh_portal_versions::version::v0_0_1::sys::{Assign, Sys};
use mesh_portal_versions::version::v0_0_1::util::ValueMatcher;
use mesh_portal_versions::version::v0_0_1::wave::{
    Agent, AsyncRequestHandler, AsyncTransmitter, AsyncTransmitterWithAgent, ReqCtx, ReqShell,
    Requestable, RespCore, RespShell, RootReqCtx, Router, Transporter, Wave,
};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::oneshot::error::RecvError;
use tokio::sync::{mpsc, oneshot};
use tokio::time::error::Elapsed;

#[derive(Clone)]
pub struct StarState {
    pub field: Arc<DashMap<Point, FieldState>>,
    pub shell: Arc<DashMap<Point, ShellState>>,
    pub driver: Arc<DashMap<Point, DriverState>>,
    pub portal_inlet: Arc<DashMap<Point, PortalInletState>>,
    pub portal_shell: Arc<DashMap<Point, PortalShellState>>,
    pub topic: Arc<DashMap<Port, Box<dyn TopicHandler>>>,
}

impl StarState {
    pub fn new() -> Self {
        Self {
            field: Arc::new(DashMap::new()),
            shell: Arc::new(DashMap::new()),
            driver: Arc::new(DashMap::new()),
            portal_inlet: Arc::new(DashMap::new()),
            portal_shell: Arc::new(DashMap::new()),
            topic: Arc::new(DashMap::new()),
        }
    }

    pub fn find_topic(
        &self,
        port: &Port,
        source: &Port,
    ) -> Option<Result<&Box<dyn TopicHandler>, MsgErr>> {
        match self.topic.get(port) {
            None => None,
            Some(topic) => {
                let topic = topic.value();
                if topic.source_selector().is_match(source).is_ok() {
                    Some(Ok(topic))
                } else {
                    Some(Err(MsgErr::forbidden()))
                }
            }
        }
    }

    pub fn find_field(&self, point: &Point) -> FieldState {
        match self.field.get(point) {
            None => {
                let rtn = FieldState::new();
                self.field.insert(point.clone(), rtn.clone());
                rtn
            }
            Some((_, rtn)) => rtn,
        }
    }

    pub fn find_shell(&self, point: &Point) -> ShellState {
        match self.shell.get(point) {
            None => {
                let rtn = ShellState::new();
                self.shell.insert(point.clone(), rtn.clone());
                rtn
            }
            Some((_, rtn)) => rtn,
        }
    }

    pub fn find_portal_inlet(&self, point: &Point) -> PortalInletState {
        match self.portal_inlet.get(point) {
            None => {
                let rtn = PortalInletState::new();
                self.portal_inlet.insert(point.clone(), rtn.clone());
                rtn
            }
            Some((_, rtn)) => rtn,
        }
    }

    pub fn find_portal_shell(&self, point: &Point) -> PortalShellState {
        match self.portal_shell.get(point) {
            None => {
                let rtn = PortalShellState::new();
                self.portal_shell.insert(point.clone(), rtn.clone());
                rtn
            }
            Some((_, rtn)) => rtn,
        }
    }

    pub fn find_driver(&self, point: &Point) -> DriverState {
        match self.driver.get(point) {
            None => {
                let rtn = DriverState::None;
                self.driver.insert(point.clone(), rtn.clone());
                rtn
            }
            Some((_, rtn)) => rtn,
        }
    }
}

#[derive(Clone)]
pub struct StarSkel {
    pub key: StarKey,
    pub kind: StarSub,
    pub logger: PointLogger,
    pub registry: Arc<dyn RegistryApi>,
    pub surface: mpsc::Sender<Wave>,
    pub traversal_router: mpsc::Sender<Traversal<Wave>>,
    pub fabric: mpsc::Sender<Wave>,
    pub machine: MachineSkel,
    pub exchange: Arc<DashMap<Uuid, oneshot::Sender<RespShell>>>,
    pub state: StarState,
}

impl StarSkel {
    pub fn point(&self) -> &Point {
        &self.logger.point
    }
}

pub enum StarCall {
    HyperWave(HyperWave),
    Traverse(Traversal<Wave>),
}

pub struct StarTx {
    surface: mpsc::Sender<HyperWave>,
    traversal_router: mpsc::Sender<Traversal<Wave>>,
    call_rx: mpsc::Receiver<StarCall>,
}

impl StarTx {
    pub fn new() -> Self {
        let (surface_tx, mut surface_rx) = mpsc::channel(1024);
        let (traversal_router_tx, mut traversal_router_rx) = mpsc::channel(1024);

        let (call_tx, call_rx) = mpsc::channel(1024);

        {
            let call_tx = call_tx.clone();
            tokio::spawn(async move {
                while let Some(wave) = surface_rx.recv().await {
                    call_tx.send(StarCall::HyperWave(wave)).await;
                }
            });
        }

        {
            let call_tx = call_tx.clone();
            tokio::spawn(async move {
                while let Some(traversal) = traversal_router_rx.recv().await {
                    call_tx.send(StarCall::Traverse(traversal)).await;
                }
            });
        }

        Self {
            surface: surface_tx,
            traversal_router: traversal_router_tx,
            call_rx,
        }
    }
}

#[derive(Clone, AsyncRequestHandler)]
pub struct Star {
    skel: StarSkel,
    call_rx: mpsc::Receiver<StarCall>,
    drivers: Drivers,
    transmitter: AsyncTransmitterWithAgent,
}

impl Star {
    pub fn new(skel: StarSkel, mut call_rx: mpsc::Receiver<StarCall>, drivers: Drivers) {
        let transmitter = AsyncTransmitterWithAgent::new(
            Agent::Point(skel.point().clone()),
            skel.point().clone().to_port().with_layer(Layer::Shell),
            Arc::new(StarInternalTransmitter::new(skel.clone())),
        );
        let star = Self {
            skel,
            call_rx,
            drivers,
            transmitter,
        };
        star.start();
    }

    fn start(mut self) {
        tokio::spawn(async move {
            while let Some(call) = self.call_rx.recv().await {
                match call {
                    StarCall::HyperWave(wave) => {
                        self.hyperwave(wave);
                    }
                    StarCall::Traverse(traversal) => {
                        self.traverse(traversal);
                    }
                }
            }
        });
    }

    fn hyperwave(&self, wave: HyperWave) {
        // right now we don't do anything with HyperWave, but maybe in the future...
        let wave = wave.wave;

        // first check if this wave was intended for the star itself
        if *wave.to() == *self.skel.point() && wave.is_req() {
            let req = wave.unwrap_req();
            let stub = req.as_stub();
            let ctx = RootReqCtx::new(req, self.skel.logger.span(), self.transmitter.clone());
            match self.handle(ctx).await {
                Ok(resp) => {
                    self.transmitter.route(Wave::Resp(resp)).await;
                    return;
                }
                Err(err) => {
                    self.transmitter.route(Wave::Resp(stub.err(err))).await;
                    return;
                }
            }
        }

        // okay if it wasn't for the star, then on to regular routing...
        // hyperwaves are delivered to the Surface of the star therefor it will be deposited in the Field
        let transmitter = StarInternalTransmitter::new(self.skel.clone(), Layer::Field);
        transmitter.route(wave).await;
    }

    async fn traverse(&self, traversal: Traversal<Wave>) {
        let next = traversal.next();
        match next {
            None => match traversal.dir {
                TraversalDirection::Fabric => {
                    self.skel.fabric.send(traversal.payload);
                }
                TraversalDirection::Core => {
                    self.skel
                        .logger
                        .warn("should not have traversed a wave all the way to the core in Star");
                }
            },
            Some(next) => {
                if traversal.is_req()
                    && next == traversal.to().layer
                    && self.skel.state.topic.contains_key(traversal.to())
                {
                    let topic = self.skel.state.find_topic(traversal.to(), traversal.from());
                    match topic {
                        None => {
                            // send some sort of Not_found
                            //                            req.not_found()
                        }
                        Some(result) => {
                            match result {
                                Ok(topic_handler) => {
                                    let req = traversal.unwrap_req().payload;
                                    let ctx = RootReqCtx::new(
                                        req,
                                        self.skel.logger.span(),
                                        self.transmitter.clone(),
                                    );

                                    topic_handler.handle(ctx).await;
                                }
                                Err(err) => {
                                    // some some 'forbidden' error message sending towards_core...
                                }
                            }
                        }
                    }
                } else {
                    match next {
                        Layer::PortalInlet => {
                            let inlet = PortalInlet::new(
                                self.skel.clone(),
                                self.skel.state.find_portal_inlet(&traversal.location),
                            );
                            inlet.traverse(traversal).await;
                        }
                        Layer::Field => {
                            let field = FieldEx::new(
                                self.skel.clone(),
                                self.skel.state.find_field(traversal.payload.to()).field,
                            );
                            field.traverse(traversal).await;
                        }
                        Layer::Shell => {
                            let shell = ShellEx::new(
                                self.skel.clone(),
                                self.skel.state.find_shell(traversal.payload.to()).shell,
                            );
                            shell.traverse(traversal).await;
                        }
                        Layer::Driver => {
                            self.drivers.traverse(traversal).await;
                        }
                        Layer::PortalShell => {
                            let portal = PortalShell::new(
                                self.skel.clone(),
                                self.skel
                                    .state
                                    .find_portal_shell(traversal.payload.to())
                                    .portal_shell(),
                            );
                            portal.traverse(traversal).await;
                        }
                        _ => {
                            self.skel.logger.warn("attempt to traverse wave in the inner layers which the Star does not manage");
                        }
                    }
                }
            }
        }
    }

    async fn to_fabric(&self, wave: Wave) {
        let skel = self.skel.clone();
        tokio::spawn(async move {
            skel.fabric.send(wave).await;
        });
    }
}

#[routes]
impl Star {
    #[route("Sys<Assign>")]
    pub async fn assign(&self, ctx: ReqCtx<'_, Sys>) -> Result<RespCore, MsgErr> {
        self.drivers.assign(ctx).await
    }
}

#[derive(Clone)]
pub struct StarInternalTransmitter {
    pub skel: StarSkel,
    pub layer: Layer,
}

impl StarInternalTransmitter {
    pub fn new(skel: StarSkel, layer: Layer) -> Self {
        Self { skel, layer }
    }

    pub fn with(self, layer: Layer) -> Self {
        Self::new(self.skel, layer)
    }
}

impl AsyncTransmitter for StarInternalTransmitter {
    async fn send(&self, request: ReqShell) -> RespShell {
        let (tx, rx) = oneshot::channel();
        self.skel.exchange.insert(request.id.clone(), tx);
        let stub = request.as_stub();
        self.route(Wave::Req(request)).await;
        match tokio::time::timeout(
            Duration::from_secs(
                self.skel
                    .machine
                    .timeouts
                    .from(request.handling.wait.clone()),
            ),
            rx,
        )
        .await
        {
            Ok(Ok(resp)) => resp,
            Ok(Err(err)) => stub.server_error(),
            Err(err) => stub.timeout(),
        }
    }

    async fn route(&self, wave: Wave) {
        let record = self
            .skel
            .registry
            .locate(&wave.to().clone().to_point())
            .await?;
        let location = record.location.clone().ok_or()?;
        let plan = record.details.stub.kind.wave_traversal_plan();
        let logger = skel.logger.point(wave.to().clone().to_point());
        let logger = logger.span();
        let mut traversal = Traversal::new(
            wave,
            record,
            location,
            plan.stack.first().cloned().unwrap(),
            logger,
            // default to Fabric direction
            TraversalDirection::Fabric,
        );

        // if the to is not even in this star, traverse towards fabric
        if location != *skel.point() {
            // already set to travel towards Fabric
            self.skel.traversal_router.send(traversal).await;
        }
        // if the to and from points are the same traverse in the direction of the target layer
        else if wave.to().point == wave.from().point {
            if wave.to().layer == wave.from().layer {
                self.skel
                    .logger
                    .warn("attempt to send wave to same point & layer");
            } else if wave.from() < wave.to() {
                // traveling from Fabric to Core (we need to reverse order of the traversal)
                traversal.reverse();
                self.skel.traversal_router.send(traveral).await;
            } else {
                // traveling from Core to Fabric... already set that way no action needed.
                self.skel.traversal_router.send(traveral).await;
            }
        } else {
            // finally we handle the most common case where the points are not the same
            if traversal.to().layer > self.layer {
                // the destination layer is greater than the insertion layer therefor we are traveling towards the Core
                traversal.reverse();
                self.skel.traversal_router.send(traveral).await;
            } else {
                // the destination layer is less than the insertion layer therefore we are traveling towards the Fabric (no action needed)
                self.skel.traversal_router.send(traveral).await;
            }
        }

        match self.skel.registry.locate(wave.to()).await {
            Ok(record) => match record.location.ok_or() {
                Ok(location) => {
                    let plan = record.details.stub.kind.wave_traversal_plan();
                    let logger = skel.logger.point(wave.to().clone().to_point());
                    let logger = logger.span();
                    let traversal = Traversal::new(
                        wave,
                        record,
                        location,
                        plan.stack.first().cloned().unwrap(),
                        logger,
                    );

                    if traversal.to().point == traversal.from().point {
                        if traversal.from().layer < traversal.to().layer {
                            self.skel.towards_core_router.send(traversal).await;
                        } else if traversal.from().layer > traversal.to().layer {
                            self.skel.traversal_router.send(traversal).await;
                        } else {
                            self.skel
                                .logger
                                .warn("attempt to route a wave to itself on the same layer");
                            return;
                        }
                    } else {
                        // sense it's not sending to itself it must be routed all the way to the surface of the star
                        self.skel.traversal_router.send(traversal).await;
                    }
                }
                Err(err) => {
                    self.skel.logger.warn(format!(
                        "point does not have a location: {}",
                        wave.to().to_string()
                    ));
                }
            },
            Err(err) => {
                self.skel.logger.warn(format!(
                    "could not find location for point: {}",
                    wave.to().to_string()
                ));
            }
        }
    }
}

pub trait TopicHandler: Send + Sync + AsyncRequestHandler + Serialize + Deserialize {
    fn source_selector(&self) -> &PortSelector;
}
