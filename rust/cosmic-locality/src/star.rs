use std::ops::{Deref, DerefMut};
use std::str::FromStr;
use std::sync::Arc;
use dashmap::DashMap;
use dashmap::mapref::one::Ref;
use tokio::sync::{mpsc, oneshot};
use cosmic_lanes::HyperWave;
use cosmic_locality::driver::Drivers;
use mesh_portal_versions::error::MsgErr;
use mesh_portal_versions::version::v0_0_1::id::id::{Layer, Point, RouteSeg, ToPoint, TraversalLayer, Uuid};
use mesh_portal_versions::version::v0_0_1::id::StarKey;
use mesh_portal_versions::version::v0_0_1::log::PointLogger;
use mesh_portal_versions::version::v0_0_1::substance::substance::Substance;
use mesh_portal_versions::version::v0_0_1::sys::{Assign, Sys};
use mesh_portal_versions::version::v0_0_1::wave::{AsyncRequestHandler, AsyncTransmitter, ReqCtx, ReqShell, RespCore, RespShell, RootReqCtx, Router, Transporter, Wave};
use crate::driver::Drivers;
use crate::field::{FieldEx, RegistryApi};
use crate::router::StarRouter;
use crate::shell::ShellEx;
use crate::state::{DriverState, FieldState, ParticleStates, ShellState};
use mesh_portal_versions::version::v0_0_1::id::Traversal;
use crate::machine::MachineSkel;
use crate::portal::PortalInlet;

#[derive(Clone)]
pub struct StarState {
    pub states: Arc<DashMap<Point, ParticleStates>>,
}

impl StarState {
    pub fn new() -> Self {
        Self {
            states: Arc::new(DashMap::new())
        }
    }

    pub fn find<P:ToPoint+Clone>(&self, point: &P ) -> ParticleStates {
        let point = point.clone().to_point();
        match self.states.get(&point) {
            None => {
                let particle = ParticleStates::new();
                self.states.insert( point, particle.clone() );
                particle
            }
            Some(particle) => {
                particle.value().clone()
            }
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
    pub exchange: Arc<DashMap<Uuid,oneshot::Sender<RespShell>>>,
    pub state: StarState
}

pub enum StarCall{
    HyperWave(HyperWave),
    TowardsFabric(Traversal<Wave>),
    TowardsCore(Traversal<Wave>)
}

pub struct StarTx {
    surface: mpsc::Sender<HyperWave>,
    towards_fabric_router: mpsc::Sender<Traversal<Wave>>,
    towards_core_router: mpsc::Sender<Traversal<Wave>>,
    call_rx: mpsc::Receiver<StarCall>
}

impl StarTx {
    pub fn new() -> Self {
        let (surface_tx,mut surface_rx) = mpsc::channel(1024);
        let (towards_fabric_router_tx,mut towards_fabric_router_rx) = mpsc::channel(1024);
        let (towards_core_router_tx,mut towards_core_router_rx) = mpsc::channel(1024);

        let (call_tx,call_rx) = mpsc::channel(1024);

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
            call_rx
        }
    }
}



#[derive(Clone)]
pub struct Star {
    skel: StarSkel,
    call_rx: mpsc::Receiver<StarCall>,
    drivers: Drivers,
}


impl Star {

    pub fn new(skel: StarSkel, mut call_rx: mpsc::Receiver<StarCall>, drivers: Drivers ) {
        let star = Self {
            skel,
            call_rx,
            drivers,
        };
        star.start();
    }

    fn start(mut self) {
        tokio::spawn( async move {
            while let Some(call) = self.call_rx.recv().await {
                match call {
                    StarCall::HyperWave(wave) => {
                        self.wave(wave);
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

    fn wave( &self, wave: HyperWave )  {
        // right now we don't do anything with HyperWave, but maybe in the future...
        let wave = wave.wave;
        let skel = self.skel.clone();
        tokio::spawn(async move {
            async fn route( skel: StarSkel, wave: Wave ) -> anyhow::Result<()> {
                let record = skel.registry.locate(&wave.to().clone().to_point()).await?;
                if let RouteSeg::Fabric(route) = &record.location.ok_or()?.route {
                    match StarKey::from_str(route.as_str() ) {
                        Ok(key) => {
                            if key == skel.key {
                                let plan= record.details.stub.kind.wave_traversal_plan();
                                let traversal = Traversal::new(wave, record, plan.stack.first().cloned().unwrap() );
                                skel.towards_core_router.send(traversal).await;
                            } else {
                                skel.fabric.send(wave).await;
                            }
                        }
                        Err(err) => {
                            skel.logger.warn(format!("unexpected Fabric Route in Point '{}' (expecting a valid StarKey)",wave.to().to_string()))
                        }
                    }
                } else {
                    skel.logger.warn(format!("unexpected Point '{}' (expecting a valid Fabric StarKey Route)",wave.to().to_string()))
                }
                Ok(())
            }
            route(skel,wave).await;
        });
    }

    async fn towards_fabric( &self, traversal: Traversal<Wave>) {
        let next = traversal.next_towards_fabric();
        match next {
            None => {
                self.skel.fabric.send(traversal.payload).await;
            },
            Some(next) =>  {
                match next {
                    Layer::PortalInlet => {
                        let inlet = PortalInlet::new( self.skel.clone(), self.skel.state.find(traversal.payload.to() ).portal_inlet(&traversal.locality) );
                        inlet.towards_fabric(traversal).await;
                    }
                    Layer::Field => {
                        let field = FieldEx::new( self.skel.clone(), self.skel.state.find(traversal.payload.to() ).field );
                        field.towards_fabric(traversal).await;
                    },
                    Layer::Shell => {
                        let shell = ShellEx::new(self.skel.clone(), self.skel.state.find(traversal.payload.to() ).shell);
                        shell.towards_fabric(traversal).await;
                    }
                    Layer::Driver => {
                        drivers.towards_fabric(traversal).await;
                    }
                    Layer::PortalShell => {
                        let portal = PortalShell::new( self.skel.clone(), self.skel.state.find(traversal.payload.to() ).portal_shell());
                        portal.towards_fabric(traversal).await;
                    }
                    _ => {
                       self.skel.logger.warn("attempt to traverse wave in the inner layers which the Star does not manage");
                    }
                }
            }
        }

    }

    async fn towards_core( &self, traversal: Traversal<Wave>) {

    }

    async fn to_fabric( &self, wave: Wave ) {

    }

}

#[routes]
impl Star {

    #[route("Sys<Assign>")]
    pub async fn assign(&self, ctx: ReqCtx<'_, Sys>) -> Result<RespCore, MsgErr> {
        if let Sys::Assign(assign) = ctx.input {
            self.drivers.assign(assign).await?;
            let states = ParticleStates {
                field: FieldState::new() ,
                shell: ShellState::new(),
                driver: DriverState::new(),
                portal_inlet: None,
                tunnel: None
            };
            self.state.states.insert( assign.details.stub.point.clone(), states );
            // terrible hack to clone it but I need to add ok() and err() autos to ctx...
            let req = ctx.request().clone();
            Ok(req.core.ok(Substance::Empty))
        } else {
            Err(MsgErr::bad_request())
        }
    }

    #[route("Sys<Transport>")]
    pub async fn transport(&self, ctx: ReqCtx<'_, Wave>) -> Result<RespCore, MsgErr> {
    }

}

