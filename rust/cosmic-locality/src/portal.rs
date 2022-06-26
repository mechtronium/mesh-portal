use std::sync::Arc;
use tokio::sync::mpsc;
use cosmic_lanes::HyperwayInterchange;
use mesh_portal_versions::version::v0_0_1::id::id::{Layer, TraversalLayer};
use mesh_portal_versions::version::v0_0_1::id::Traversal;
use mesh_portal_versions::version::v0_0_1::wave::Wave;
use crate::host::HostSkel;
use crate::star::StarSkel;
use crate::state::{PortalInletState, TunnelState};

/// the portal endpoint that is within the Mesh
pub struct PortalInlet {
    pub skel: StarSkel,
    pub state: PortalInletState
}

impl PortalInlet {
    pub fn new( skel: StarSkel, state: PortalInletState ) -> Self {
        Self {
            skel,
            state
        }
    }
}

impl TraversalLayer for PortalInlet{
    async fn towards_core(&self, mut traversal: Traversal<Wave>) {
        traversal.layer = Layer::PortalInlet;
        self.skel.towards_core_router.send(traversal).await;
    }

    async fn towards_fabric(&self, mut traversal: Traversal<Wave>) {
        traversal.layer = Layer::PortalInlet;
        self.skel.towards_fabric_router.send(traversal).await;
    }
}


/// the portal endpoint that is outside of the Mesh
pub struct PortalOutlet{
}

impl TraversalLayer for PortalOutlet {
    async fn towards_core(&self, mut traversal: Traversal<Wave>) {
        traversal.layer = Layer::PortalOutlet;
        self.skel.towards_core_router.send(traversal).await;
    }

    async fn towards_fabric(&self, mut traversal: Traversal<Wave>) {
        traversal.layer = Layer::PortalOutlet;
        self.skel.towards_fabric_router.send(traversal).await;
    }
}


/// The mid-portion of the Portal [Between Inlet & Outlet]
pub struct TunnelOutlet {
    state: TunnelState,
    pub skel: StarSkel,
    towards_core: Arc<HyperwayInterchange>
}

impl TunnelOutlet {
    pub fn new(skel: StarSkel, towards_core: Arc<HyperwayInterchange>, state: TunnelState ) -> Self {
        Self {
            skel,
            towards_core,
            state
        }
    }
}

impl TraversalLayer for TunnelOutlet {
    async fn towards_core(&self, mut traversal: Traversal<Wave>) {
        traversal.layer = Layer::Tunnel;
        self.towards_core.outbound(traversal.payload).await;
    }

    async fn towards_fabric(&self, mut traversal: Traversal<Wave>) {
        traversal.layer = Layer::Tunnel;
        self.skel.towards_fabric_router.send(traversal).await;
    }
}


pub struct TunnelInlet {
    skel: HostSkel,
    state: TunnelState,
}

impl TunnelInlet {
    pub fn new(skel: HostSkel, state: TunnelState ) -> Self {
        Self {
            skel,
            state
        }
    }
}


impl TraversalLayer for TunnelInlet{
    async fn towards_core(&self, mut traversal: Traversal<Wave>) {
        traversal.layer = Layer::Tunnel;
        self.skel.towards_core.send(traversal)
    }

    async fn towards_fabric(&self, mut traversal: Traversal<Wave>) {
        traversal.layer = Layer::Tunnel;
        self.skel.fabric.outbound(traversal.wave);
    }
}




pub struct PortalOutlet {
    pub skel: GuestSkel,
    pub state: PortalOutletState
}

impl PortalOutlet {
    pub fn new( skel: StarSkel, state: PortalOutletState ) -> Self {
        Self {
            skel,
            state
        }
    }
}

impl TraversalLayer for PortalOutlet{
    async fn towards_core(&self, mut traversal: Traversal<Wave>) {
        traversal.layer = Layer::PortalOutlet;
        self.skel.towards_core_router.send(traversal).await;
    }

    async fn towards_fabric(&self, mut traversal: Traversal<Wave>) {
        traversal.layer = Layer::PortalOutlet;
        self.skel.towards_fabric_router.send(traversal).await;
    }
}

