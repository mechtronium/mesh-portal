use dashmap::DashMap;
use futures::future::select_all;
use futures::FutureExt;
use mesh_portal_versions::version::v0_0_1::id::id::{Point, ToPoint};
use mesh_portal_versions::version::v0_0_1::wave::Wave;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::mpsc;

#[macro_use]
extern crate async_trait;

pub struct Hyperway {
    pub end_point: Point,
    outbound: OutboundLanes,
    inbound: InboundLanes,
}

pub struct HyperwayOut {
    pub end_point: Point,
    outbound: OutboundLanes,
}

pub struct HyperwayIn {
    pub end_point: Point,
    inbound: InboundLanes,
}

impl HyperwayOut {
    pub async fn outbound(&self, wave: Wave) {
        self.outbound.send(wave).await;
    }
}

impl HyperwayIn {
    pub async fn inbound(&mut self) -> Option<Wave> {
        self.inbound.receive().await
    }

    pub async fn inbound_into_call(&mut self) -> Option<HyperwayCall> {
        let wave = self.inbound().await;
        match wave {
            None => None,
            Some(wave) => {
                let hyperwave = HyperWave {
                    from: self.end_point.clone(),
                    wave,
                };

                Some(HyperwayCall::Wave(hyperwave))
            }
        }
    }
}

impl Hyperway {
    pub fn split(self) -> (HyperwayIn, HyperwayOut) {
        (
            HyperwayIn {
                end_point: self.end_point.clone(),
                inbound: self.inbound,
            },
            HyperwayOut {
                end_point: self.end_point.clone(),
                outbound: self.outbound,
            },
        )
    }
}

pub enum HyperwayCall {
    Wave(HyperWave),
    Add(HyperwayIn),
    Remove(Point),
}

/// doesn't do much now, but the eventual idea is to have it handle multiple lanes
/// and send to them based on priority
pub struct OutboundLanes {
    pub tx: mpsc::Sender<Wave>,
}

impl OutboundLanes {
    async fn send(&self, wave: Wave) {
        self.tx.send(wave).await;
    }
}

/// doesn't do much now, but the eventual idea is to have it handle multiple lanes
/// and draw from them based on priority
pub struct InboundLanes {
    pub rx: mpsc::Receiver<Wave>,
}

impl InboundLanes {
    async fn receive(&mut self) -> Option<Wave> {
        self.rx.recv().await
    }
}

pub struct HyperWave {
    pub from: Point,
    pub wave: Wave,
}

pub struct HyperwayInterchange {
    hyperways: Arc<DashMap<Point, HyperwayOut>>,
    call_tx: mpsc::Sender<HyperwayCall>,
}

impl HyperwayInterchange {
    pub fn new(router: Box<dyn HyperRouter>) -> Self {
        let (call_tx, mut call_rx) = mpsc::channel(1024);
        let hyperways: Arc<DashMap<Point, HyperwayOut>> = Arc::new(DashMap::new());

        {
            let hyperway_outs = hyperways.clone();
            tokio::spawn(async move {
                let mut hyperway_ins :HashMap<Point,HyperwayIn>= HashMap::new();
                loop {
                    let mut rx = vec![];
                    let mut index_to_point = HashMap::new();

                    for (index, (point,hyperway)) in hyperway_ins.iter_mut().enumerate() {
                        index_to_point.insert(index, point.clone());
                        rx.push(hyperway.inbound_into_call().boxed())
                    }

                    rx.push(call_rx.recv().boxed());

                    let (result, index, _) = select_all(rx).await;

                    match result {
                        Some(HyperwayCall::Add(hyperway_in)) => {
                            hyperway_ins.insert(hyperway_in.end_point.clone(), hyperway_in);
                        }
                        Some(HyperwayCall::Remove(hyperway)) => {
                            hyperway_ins.remove(&hyperway);
                            hyperway_outs.remove(&hyperway);
                        }
                        Some(HyperwayCall::Wave(wave)) => {
                            router.route(wave).await;
                        }
                        None => {
                            match index_to_point.get(&index) {
                                Some(hyperway) => {
                                    hyperway_ins.remove(hyperway);
                                    hyperway_outs.remove(hyperway);
                                }
                                None => {
                                    // this means call_rx returned None... we are done here.
                                    break;
                                }
                            }
                        }
                    }
                }
            });
        }

        Self { hyperways, call_tx }
    }

    pub fn add(&mut self, hyperway: Hyperway) {
        let (hyperway_in, hyperway_out) = hyperway.split();
        self.hyperways
            .insert(hyperway_out.end_point.clone(), hyperway_out);
        let call_tx = self.call_tx.clone();
        tokio::spawn(async move {
            call_tx.send(HyperwayCall::Add(hyperway_in)).await;
        });
    }

    pub fn remove(&mut self, hyperway: Point) {
        self.hyperways.remove(&hyperway);
        let call_tx = self.call_tx.clone();
        tokio::spawn(async move {
            call_tx.send(HyperwayCall::Remove(hyperway)).await;
        });
    }

    pub async fn outbound(&self, wave: Wave) {
        let point = wave.to().clone().to_point();
        match self.hyperways.get(&point) {
            None => {
                // do nothing
            }
            Some(hyperway) => {
                hyperway.value().outbound(wave).await;
            }
        }
    }
}

#[async_trait]
pub trait HyperRouter: Send + Sync {
    async fn route(&self, wave: HyperWave);
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
