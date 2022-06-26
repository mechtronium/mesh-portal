use crate::field::RegistryApi;
use crate::machine::MachineSkel;
use crate::star::StarSkel;
use crate::state::DriverState;
use cosmic_locality::state::DriverState;
use dashmap::DashMap;
use mesh_portal_versions::error::MsgErr;
use mesh_portal_versions::version::v0_0_1::id::id::{Kind, Layer, ToPoint, TraversalLayer, Uuid};
use mesh_portal_versions::version::v0_0_1::id::{StarKey, Traversal};
use mesh_portal_versions::version::v0_0_1::log::PointLogger;
use mesh_portal_versions::version::v0_0_1::particle::particle::Status;
use mesh_portal_versions::version::v0_0_1::substance::substance::Substance;
use mesh_portal_versions::version::v0_0_1::sys::{Assign, Sys};
use mesh_portal_versions::version::v0_0_1::wave::{
    AsyncRequestHandler, ReqCtx, ReqShell, RespCore, RespShell, Wave,
};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc::Sender;
use tokio::sync::{mpsc, oneshot};

#[derive(AsyncRequestHandler)]
pub struct Drivers {
    pub skel: StarSkel,
    pub drivers: HashMap<Kind, Arc<dyn Driver>>,
}

impl Drivers {
    pub fn new(skel: StarSkel, drivers: HashMap<Kind, Arc<dyn Driver>>) -> Self {
        Self { skel, drivers }
    }

    pub fn init(&self) -> Result<(), MsgErr> {
        let mut errs = vec![];
        for driver in self.drivers.values() {
            if driver.status() != DriverStatus::Ready
                && driver.status() != DriverStatus::Initializing
            {
                driver.lifecycle(DriverLifecycleEvent::Init);
            }

            if driver.status() != DriverStatus::Ready {
                errs.push(MsgErr::server_error());
            }
        }

        if !errs.is_empty() {
            // need to fold these errors into one
            Err(MsgErr::server_error())
        } else {
            Ok(())
        }
    }
}

impl Drivers {
    pub async fn assign(&self, ctx: ReqCtx<'_, Sys>) -> Result<RespCore, MsgErr> {
        if let Sys::Assign(assign) = &ctx.input {
            match self.drivers.get(&assign.details.stub.kind) {
                None => Err(format!(
                    "do not have driver for Kind: <{}>",
                    assign.details.stub.kind.to_string()
                )
                .into()),
                Some(driver) => {
                    let state = tokio::time::timeout(
                        Duration::from_secs(self.skel.machine.timeouts.high),
                        driver.assign(assign),
                    )
                    .await??;
                    self.skel
                        .state
                        .driver
                        .insert(ctx.request().to.clone().to_point(), state);
                    Ok(ctx.request().core.ok(Substance::Empty))
                }
            }
        } else {
            Err(MsgErr::bad_request())
        }
    }
}

impl TraversalLayer for Drivers {
    fn layer(&self) -> &Layer {
        &Layer::Driver
    }

    async fn towards_fabric_router(&self, traversal: Traversal<Wave>) {
        self.skel.traversal_router.send(traversal).await;
    }

    async fn towards_core_router(&self, traversal: Traversal<Wave>) {
        match self.drivers.get(&traversal.record.details.stub.kind) {
            None => {
                traversal.logger.warn(format!(
                    "star does not have a driver for Kind <{}>",
                    traversal.record.details.stub.kind.to_string()
                ));
            }
            Some(driver) => {
                driver.towards_core_router(traversal).await;
            }
        }
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, tokio::sync::oneshot::Sender<RespShell>>> {
        &self.skel.exchange
    }

    async fn layer_handle(&self, request: ReqShell) {}
}

pub struct DriversBuilder {
    pub factories: HashMap<Kind, Box<dyn DriverFactory>>,
    pub logger: Option<PointLogger>,
}

impl DriversBuilder {
    pub fn add(&mut self, factory: Box<dyn DriverFactory>) {
        self.factories.insert(factory.kind(), factory);
    }

    pub fn logger(&mut self, logger: PointLogger) {
        self.logger.replace(logger);
    }

    pub fn build(self) -> Result<Drivers, MsgErr> {
        if self.logger.is_none() {
            return Err("expected point logger to be set".into());
        }
        let mut drivers = HashMap::new();
        for factory in self.factories.values() {
            drivers.insert(factory.kind(), factory.create(skel));
        }
        Ok(Drivers::new(drivers, self.logger.unwrap()))
    }
}

pub trait DriverFactory {
    fn kind(&self) -> Kind;
    fn create(&self, skel: StarSkel) -> Arc<dyn Driver>;
}

pub trait Driver: TraversalLayer + AsyncRequestHandler {
    fn skel(&self) -> StarSkel;

    fn layer(&self) -> &Layer {
        &Layer::Driver
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, oneshot::Sender<RespShell>>> {
        &self.skel().exchange
    }

    fn lifecycle(&self, event: DriverLifecycleEvent) {}

    fn status(&self) -> DriverStatus;

    async fn assign(&self, assign: &Assign) -> Result<DriverState, MsgErr> {
        Ok(DriverState::None)
    }

    async fn towards_core_router(&self, traversal: Traversal<Wave>) {
        // here's where we actually handle the Wave...
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum DriverLifecycleEvent {
    Init,
    Shutdown,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum DriverStatus {
    Started,
    Initializing,
    Ready,
    Unavailable,
}

#[derive(Clone)]
pub struct DriverSkel {
    pub star: StarKey,
    pub logger: PointLogger,
    pub registry: Arc<dyn RegistryApi>,
    pub surface: mpsc::Sender<Wave>,
    pub towards_fabric_router: mpsc::Sender<Traversal<Wave>>,
    pub towards_core_router: mpsc::Sender<Traversal<Wave>>,
    pub fabric: mpsc::Sender<Wave>,
    pub machine: MachineSkel,
    pub exchange: Arc<DashMap<Uuid, oneshot::Sender<RespShell>>>,
}

impl From<StarSkel> for DriverSkel {
    fn from(skel: StarSkel) -> Self {
        Self {
            star: skel.key,
            logger: skel.logger.push("driver").unwrap(),
            registry: skel.registry,
            surface: skel.surface,
            towards_fabric_router: skel.traversal_router,
            towards_core_router: skel.towards_core_router,
            fabric: skel.fabric,
            machine: skel.machine,
            exchange: skel.exchange,
        }
    }
}
