use crate::driver::Drivers;
use crate::field::{FieldEx, RegistryApi};
use crate::machine::MachineSkel;
use crate::portal::PortalInlet;
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
use mesh_portal_versions::version::v0_0_1::id::id::{
    Layer, Point, Port, RouteSeg, ToPoint, ToPort, TraversalLayer, Uuid,
};
use mesh_portal_versions::version::v0_0_1::id::StarKey;
use mesh_portal_versions::version::v0_0_1::id::Traversal;
use mesh_portal_versions::version::v0_0_1::log::PointLogger;
use mesh_portal_versions::version::v0_0_1::quota::Timeouts;
use mesh_portal_versions::version::v0_0_1::substance::substance::Substance;
use mesh_portal_versions::version::v0_0_1::sys::{Assign, Sys};
use mesh_portal_versions::version::v0_0_1::wave::{
    Agent, AsyncRequestHandler, AsyncTransmitter, AsyncTransmitterWithAgent, ReqCtx, ReqShell,
    Requestable, RespCore, RespShell, RootReqCtx, Router, Transporter, Wave,
};
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
}

impl StarState {
    pub fn new() -> Self {
        Self {
            field: Arc::new(DashMap::new()),
            shell: Arc::new(DashMap::new()),
            driver: Arc::new(DashMap::new()),
            portal_inlet: Arc::new(DashMap::new()),
            portal_shell: Arc::new(DashMap::new()),
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
    pub logger: PointLogger,
    pub key: StarKey,
    pub registry: Arc<dyn RegistryApi>,
    pub surface: mpsc::Sender<Wave>,
    pub towards_fabric_router: mpsc::Sender<Traversal<Wave>>,
    pub towards_core_router: mpsc::Sender<Traversal<Wave>>,
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
    TowardsFabric(Traversal<Wave>),
    TowardsCore(Traversal<Wave>),
}

pub struct StarTx {
    surface: mpsc::Sender<HyperWave>,
    towards_fabric_router: mpsc::Sender<Traversal<Wave>>,
    towards_core_router: mpsc::Sender<Traversal<Wave>>,
    call_rx: mpsc::Receiver<StarCall>,
}

impl StarTx {
    pub fn new() -> Self {
        let (surface_tx, mut surface_rx) = mpsc::channel(1024);
        let (towards_fabric_router_tx, mut towards_fabric_router_rx) = mpsc::channel(1024);
        let (towards_core_router_tx, mut towards_core_router_rx) = mpsc::channel(1024);

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
                while let Some(traversal) = towards_fabric_router_rx.recv().await {
                    call_tx.send(StarCall::TowardsFabric(traversal)).await;
                }
            });
        }

        {
            let call_tx = call_tx.clone();
            tokio::spawn(async move {
                while let Some(traversal) = towards_core_router_rx.recv().await {
                    call_tx.send(StarCall::TowardsCore(traversal)).await;
                }
            });
        }

        Self {
            surface: surface_tx,
            towards_fabric_router: towards_fabric_router_tx,
            towards_core_router: towards_core_router_tx,
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
            Arc::new(StarTransmitter::new(skel.clone())),
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
                        self.receive_hyperwave(wave);
                    }
                    StarCall::TowardsFabric(traversal) => {
                        self.towards_fabric(traversal);
                    }
                    StarCall::TowardsCore(traversal) => {
                        self.towards_core(traversal);
                    }
                }
            }
        });
    }

    fn receive_hyperwave(&self, wave: HyperWave) {
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

        let skel = self.skel.clone();
        tokio::spawn(async move {
            async fn route(skel: StarSkel, wave: Wave) -> anyhow::Result<()> {
                let record = skel.registry.locate(&wave.to().clone().to_point()).await?;
                if let RouteSeg::Fabric(route) = &record.location.ok_or()?.route {
                    match StarKey::from_str(route.as_str()) {
                        Ok(key) => {
                            if key == skel.key {
                                let locality = record.location.clone().ok_or()?;
                                let plan = record.details.stub.kind.wave_traversal_plan();
                                let logger = skel.logger.point(wave.to().clone().to_point());
                                let logger = logger.span();
                                let traversal = Traversal::new(
                                    wave,
                                    record,
                                    locality,
                                    plan.stack.first().cloned().unwrap(),
                                    logger,
                                );
                                skel.towards_core_router.send(traversal).await;
                            } else {
                                // routed to this star, but not the appropriate location
                                if wave.is_req() {
                                    let req = wave.unwrap_req();
                                    let resp = req.not_found();
                                    skel.fabric.send(resp.into() ).await;
                                }
                            }
                        }
                        Err(err) => skel.logger.warn(format!(
                            "unexpected Fabric Route in Point '{}' (expecting a valid StarKey)",
                            wave.to().to_string()
                        )),
                    }
                } else {
                    skel.logger.warn(format!(
                        "unexpected Point '{}' (expecting a valid Fabric StarKey Route)",
                        wave.to().to_string()
                    ))
                }
                Ok(())
            }
            route(skel, wave).await;
        });
    }

    async fn towards_fabric(&self, traversal: Traversal<Wave>) {
        let next = traversal.next_towards_fabric();
        match next {
            None => {
                self.skel.fabric.send(traversal.payload).await;
            }
            Some(next) => match next {
                Layer::PortalInlet => {
                    let inlet = PortalInlet::new(
                        self.skel.clone(),
                        self.skel.state.find_portal_inlet(&traversal.locality),
                    );
                    inlet.towards_fabric(traversal).await;
                }
                Layer::Field => {
                    let field = FieldEx::new(
                        self.skel.clone(),
                        self.skel.state.find_field(traversal.payload.to()).field,
                    );
                    field.towards_fabric(traversal).await;
                }
                Layer::Shell => {
                    let shell = ShellEx::new(
                        self.skel.clone(),
                        self.skel.state.find_shell(traversal.payload.to()).shell,
                    );
                    shell.towards_fabric(traversal).await;
                }
                Layer::Driver => {
                    self.drivers.towards_fabric(traversal).await;
                }
                Layer::PortalShell => {
                    let portal = PortalShell::new(
                        self.skel.clone(),
                        self.skel
                            .state
                            .find_portal_shell(traversal.payload.to())
                            .portal_shell(),
                    );
                    portal.towards_fabric(traversal).await;
                }
                _ => {
                    self.skel.logger.warn("attempt to traverse wave in the inner layers which the Star does not manage");
                }
            },
        }
    }

    async fn towards_core(&self, traversal: Traversal<Wave>) {
        let next = traversal.next_towards_core();
        match next {
            None => {
                self.skel.core.send(traversal.payload).await;
            }
            Some(next) => match next {
                Layer::PortalInlet => {
                    let inlet = PortalInlet::new(
                        self.skel.clone(),
                        self.skel.state.find_portal_inlet(&traversal.locality),
                    );
                    inlet.towards_core(traversal).await;
                }
                Layer::Field => {
                    let field = FieldEx::new(
                        self.skel.clone(),
                        self.skel.state.find_field(traversal.payload.to()).field,
                    );
                    field.towards_core(traversal).await;
                }
                Layer::Shell => {
                    let shell = ShellEx::new(
                        self.skel.clone(),
                        self.skel.state.find_shell(traversal.payload.to()).shell,
                    );
                    shell.towards_core(traversal).await;
                }
                Layer::Driver => {
                    self.drivers.towards_core(traversal).await;
                }
                Layer::PortalShell => {
                    let portal = PortalShell::new(
                        self.skel.clone(),
                        self.skel
                            .state
                            .find_portal_shell(traversal.payload.to())
                            .portal_shell(),
                    );
                    portal.towards_core(traversal).await;
                }
                _ => {
                    self.skel.logger.warn("attempt to traverse wave in the inner layers which the Star does not manage");
                }
            },
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
        if let Sys::Assign(assign) = ctx.input {
            self.drivers.assign(assign).await?;
            // terrible hack to clone it but I need to add ok() and err() autos to ctx...
            let req = ctx.request().clone();
            Ok(req.core.ok(Substance::Empty))
        } else {
            Err(MsgErr::bad_request())
        }
    }
}

pub struct StarTransmitter {
    pub skel: StarSkel,
}

impl StarTransmitter {
    pub fn new(skel: StarSkel) -> Self {
        Self { skel }
    }
}

impl AsyncTransmitter for StarTransmitter {
    async fn send(&self, request: ReqShell) -> RespShell {
        let (tx, rx) = oneshot::channel();
        self.skel.exchange.insert(request.id.clone(), tx);
        let stub = request.as_stub();
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
        match self.skel.registry.locate(wave.to()).await {
            Ok(record) => match record.location.ok_or() {
                Ok(location) => {
                    if location == *self.skel.point() {
                        self.skel.surface.send(wave).await;
                    } else {
                        self.skel.fabric.send(wave).await;
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
