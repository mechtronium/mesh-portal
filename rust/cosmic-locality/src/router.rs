use std::sync::Arc;
use tokio::sync::mpsc;
use mesh_portal_versions::version::v0_0_1::id::id::{Layer, TraversalPlan};
use mesh_portal_versions::version::v0_0_1::wave::{AsyncTransmitter, Router, Wave};
use mesh_portal_versions::version::v0_0_1::id::Traversal;

pub struct StarRouter {
    pub layer: Layer,
}

impl StarRouter {

    pub fn new(layer: Layer, routers: Vec<mpsc::Sender<Traversal>>, fabric_transmitter: Arc<dyn AsyncTransmitter>, core_transmitter: Arc<dyn AsyncTransmitter>, surface: Arc<dyn Router> ) -> Self {
        Self {
            layer,
            fabric_transmitter,
            core_transmitter,
            routers,
            surface,
        }
    }

    pub async fn to_fabric(&self, wave: Wave ) {
        self.fabric_transmitter.route(wave).await;
    }

    pub async fn to_surface(&self, wave: Wave ) {
        self.surface.route(wave).await;
    }


    pub async fn route_towards_fabric(&self, traversal: Traversal)
    {
       let next = traversal.kind.wave_traversal_plan().towards_fabric(&self.layer);
       match next {
           None => self.fabric_transmitter.route(traversal.payload).await,
           Some(next) => self.routers.get(next).as_ref().unwrap().send(traversal).await
       }
    }

    pub async fn route_towards_core(&self, traversal: Traversal)
    {
        let next = traversal.kind.wave_traversal_plan().towards_core(&self.layer);
        match next {
            None => self.core_transmitter.route(traversal.payload).await,
            Some(next) => self.routers.get(next).as_ref().unwrap().send(traversal).await
        }
    }
}

