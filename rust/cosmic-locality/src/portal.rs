use crate::guest::GuestSkel;
use crate::host::HostSkel;
use crate::star::StarSkel;
use crate::state::{PortalInletState, PortalShellState, TunnelState};
use cosmic_lanes::HyperwayInterchange;
use dashmap::DashMap;
use mesh_portal_versions::version::v0_0_1::id::id::{Layer, TraversalLayer, Uuid};
use mesh_portal_versions::version::v0_0_1::id::Traversal;
use mesh_portal_versions::version::v0_0_1::wave::{ReqShell, RespShell, Wave};
use std::sync::Arc;
use tokio::sync::mpsc;
use tokio::sync::oneshot::Sender;

/// the portal endpoint that is within the Mesh
pub struct PortalInlet {
    pub skel: StarSkel,
    pub state: PortalInletState,
}

impl PortalInlet {
    pub fn new(skel: StarSkel, state: PortalInletState) -> Self {
        Self { skel, state }
    }
}

impl TraversalLayer for PortalInlet {
    fn layer(&self) -> &Layer {
        todo!()
    }

    async fn towards_fabric_router(&self, traversal: Traversal<Wave>) {
        todo!()
    }

    async fn towards_core_router(&self, traversal: Traversal<Wave>) {
        todo!()
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, Sender<RespShell>>> {
        todo!()
    }

    async fn layer_handle(&self, request: ReqShell) {
        todo!()
    }
}

/// the portal endpoint that is outside of the Mesh
pub struct PortalOutlet {}

impl TraversalLayer for PortalOutlet {
    fn layer(&self) -> &Layer {
        todo!()
    }

    async fn towards_fabric_router(&self, traversal: Traversal<Wave>) {
        todo!()
    }

    async fn towards_core_router(&self, traversal: Traversal<Wave>) {
        todo!()
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, Sender<RespShell>>> {
        todo!()
    }

    async fn layer_handle(&self, request: ReqShell) {
        todo!()
    }
}

/// The mid-portion of the Portal [Between Inlet & Outlet]
pub struct TunnelOutlet {
    state: TunnelState,
    pub skel: StarSkel,
    towards_core: Arc<HyperwayInterchange>,
}

impl TunnelOutlet {
    pub fn new(skel: StarSkel, towards_core: Arc<HyperwayInterchange>, state: TunnelState) -> Self {
        Self {
            skel,
            towards_core,
            state,
        }
    }
}

impl TraversalLayer for TunnelOutlet {
    fn layer(&self) -> &Layer {
        todo!()
    }

    async fn towards_fabric_router(&self, traversal: Traversal<Wave>) {
        todo!()
    }

    async fn towards_core_router(&self, traversal: Traversal<Wave>) {
        todo!()
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, Sender<RespShell>>> {
        todo!()
    }

    async fn layer_handle(&self, request: ReqShell) {
        todo!()
    }
}

pub struct TunnelInlet {
    skel: HostSkel,
    state: TunnelState,
}

impl TunnelInlet {
    pub fn new(skel: HostSkel, state: TunnelState) -> Self {
        Self { skel, state }
    }
}

impl TraversalLayer for TunnelInlet {
    fn layer(&self) -> &Layer {
        todo!()
    }

    async fn towards_fabric_router(&self, traversal: Traversal<Wave>) {
        todo!()
    }

    async fn towards_core_router(&self, traversal: Traversal<Wave>) {
        todo!()
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, Sender<RespShell>>> {
        todo!()
    }

    async fn layer_handle(&self, request: ReqShell) {
        todo!()
    }
}

pub struct PortalOutlet {
    pub skel: GuestSkel,
    pub state: PortalOutletState,
}

impl PortalOutlet {
    pub fn new(skel: StarSkel, state: PortalOutletState) -> Self {
        Self { skel, state }
    }
}

impl TraversalLayer for PortalOutlet {
    fn layer(&self) -> &Layer {
        todo!()
    }

    async fn towards_fabric_router(&self, traversal: Traversal<Wave>) {
        todo!()
    }

    async fn towards_core_router(&self, traversal: Traversal<Wave>) {
        todo!()
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, Sender<RespShell>>> {
        todo!()
    }

    async fn layer_handle(&self, request: ReqShell) {
        todo!()
    }
}

pub struct PortalShell {
    pub skel: StarSkel,
    pub state: PortalShellState,
}

impl PortalShell {
    pub fn new(skel: StarSkel, state: PortalShellState) -> Self {
        Self { skel, state }
    }
}

impl TraversalLayer for PortalShell {
    fn layer(&self) -> &Layer {
        &Layer::PortalShell
    }

    async fn towards_fabric_router(&self, traversal: Traversal<Wave>) {
        self.skel.traversal_router.send(traversal).await;
    }

    async fn towards_core_router(&self, traversal: Traversal<Wave>) {
        // intercept Assignments
        self.skel.towards_core_router.send(traversal).await;
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, Sender<RespShell>>> {
        &self.skel.exchange
    }

    async fn layer_handle(&self, request: ReqShell) {
        // this is where
    }
}
