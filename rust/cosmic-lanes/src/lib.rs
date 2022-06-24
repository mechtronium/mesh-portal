#![allow(warnings)]

use dashmap::DashMap;
use futures::future::select_all;
use futures::FutureExt;
use mesh_portal_versions::version::v0_0_1::id::id::{Point, ToPoint, ToPort, Version};
use mesh_portal_versions::version::v0_0_1::wave::{Agent, Method, ReqShell, Requestable, RespShell, SysMethod, Wave};
use std::collections::HashMap;
use std::future::Future;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use tokio::sync::mpsc;
use mesh_portal_versions::error::MsgErr;
use mesh_portal_versions::version::v0_0_1::command::request::create::{PointFactory, PointFactoryU128, PointSegTemplate};
use mesh_portal_versions::version::v0_0_1::frame::frame::PrimitiveFrame;
use mesh_portal_versions::version::v0_0_1::log::{PointLogger, RootLogger};
use mesh_portal_versions::version::v0_0_1::substance::substance::{Substance, SubstanceKind, Token};
use mesh_portal_versions::version::v0_0_1::sys::Sys;
use mesh_portal_versions::version::v0_0_1::util::uuid;

#[macro_use]
extern crate async_trait;

pub struct Hyperway {
    pub agent: Agent,
    pub logger: PointLogger,
    pub end_point: Point,
    outbound: OutboundLanes,
    inbound: InboundLanes,
}

#[derive(Clone)]
pub struct HyperwayStub {
    pub agent: Agent,
    pub logger: PointLogger,
    pub end_point: Point
}

pub struct HyperwayOut {
    pub agent: Agent,
    pub end_point: Point,
    outbound: OutboundLanes,
    logger: PointLogger
}

pub struct HyperwayIn {
    pub agent: Agent,
    pub end_point: Point,
    inbound: InboundLanes,
    logger: PointLogger
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
                logger: self.logger.push("inbound").unwrap(),
                agent: self.agent.clone()
            },
            HyperwayOut {
                end_point: self.end_point.clone(),
                outbound: self.outbound,
                logger: self.logger.push("outbound").unwrap(),
                agent: self.agent.clone()
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
    logger: PointLogger,
    authenticator: Box<dyn HyperAuthenticator>
}

impl HyperwayInterchange {
    pub fn new(router: Box<dyn HyperRouter>, authenticator: Box<dyn HyperAuthenticator>, logger: PointLogger ) -> Self {
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

        Self { hyperways, call_tx, authenticator, logger }
    }

    pub fn point(&self) -> &Point {
        &self.logger.point
    }

    async fn auth(&mut self, req: ReqShell) -> Result<(mpsc::Sender<Wave>, mpsc::Receiver<Wave>),RespShell> {
        // first make sure we are talking to the local lane connector
        if req.to.clone().to_point() != Point::local_hypergate() {
            let mut resp = req.bad_request();
            resp.from = Point::local_hypergate().to_port();
            return Err(resp);
        }

        // if this is not a request to Authenticate then it is a bad request
        if Method::Sys(SysMethod::ConnectReq) != req.core.method  {
            return Err(req.bad_request());
        }

        let stub = req.as_stub();
        match self.authenticator.auth(req).await {
            Ok(hyperway) => {
                let (inbound_tx,inbound_rx) = mpsc::channel(1024);
                let (outbound_tx,outbound_rx) = mpsc::channel(1024);
                let hyperway = Hyperway {
                    agent: hyperway.agent,
                    logger: hyperway.logger,
                    end_point: hyperway.end_point,
                    inbound: InboundLanes {rx:inbound_rx},
                    outbound: OutboundLanes {tx:outbound_tx},
                };
                self.add(hyperway);
                Ok((inbound_tx,outbound_rx))
            }
            Err(err) => {
                Err(stub.err(err))
            }
        }
    }

    pub fn add(&self, hyperway: Hyperway) {
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
               self.logger.error(format!("attempt to send wave from '{}' to hyperway '{}' which is not present in this HyperwayInterchange", wave.from().to_string(), wave.to().to_string()) );
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


#[async_trait]
pub trait HyperAuthenticator {
    async fn auth( &mut self, req: ReqShell ) -> Result<HyperwayStub,MsgErr>;
}

pub struct AnonHyperAuthenticator{
    pub logger: RootLogger,
    pub lane_point_factory: Box<dyn PointFactory>,
}

impl AnonHyperAuthenticator{
    pub fn new(lane_point_factory: Box<dyn PointFactory>, logger: RootLogger ) -> Self {
        Self {
            logger,
            lane_point_factory,
        }
    }
}

#[async_trait]
impl HyperAuthenticator for AnonHyperAuthenticator{
    async fn auth(&mut self, req: ReqShell) -> Result<HyperwayStub, MsgErr> {

        if let Substance::Sys(Sys::ConnectReq(auth_req)) = &req.core.body {
            if let Some(end_point) = &auth_req.end_point {
                let end_point = end_point.clone();
                let lane_point = self.lane_point_factory.create()?;
                let logger = self.logger.point(lane_point);
                return Ok(HyperwayStub {
                    agent: Agent::Anonymous,
                    logger,
                    end_point
                })
            } else {
                return Err(MsgErr::bad_request())
            }
        } else {
            // must be a LaneAuth
            return Err(MsgErr::bad_request())
        }
    }
}

pub struct AnonHyperAuthenticatorAssignEndPoint {
   pub logger: RootLogger,
   pub lane_point_factory: Box<dyn PointFactory>,
   pub end_point_factory: Box<dyn PointFactory>
}

impl AnonHyperAuthenticatorAssignEndPoint {
    pub fn new(lane_point_factory: Box<dyn PointFactory>, end_point_factory: Box<dyn PointFactory>, logger: RootLogger ) -> Self {
        Self {
            logger,
            lane_point_factory,
            end_point_factory
        }
    }
}

#[async_trait]
impl HyperAuthenticator for AnonHyperAuthenticatorAssignEndPoint {
    async fn auth(&mut self, req: ReqShell) -> Result<HyperwayStub, MsgErr> {

       if let Substance::Sys(Sys::ConnectReq(auth_req)) = &req.core.body {
           if let None = auth_req.end_point {
               let end_point = self.end_point_factory.create()?;
               let lane_point = self.lane_point_factory.create()?;
               let logger = self.logger.point(lane_point);
               return Ok(HyperwayStub {
                   agent: Agent::Anonymous,
                   logger,
                   end_point
               })
           } else {
               return Err(MsgErr::bad_request())
           }
       } else {
           // must be a LaneAuth
           return Err(MsgErr::bad_request())
       }
    }
}


pub struct TokensFromHeavenHyperAuthenticatorAssignEndPoint {
    pub logger: RootLogger,
    pub tokens: Arc<DashMap<Token,HyperwayStub>>
}

impl TokensFromHeavenHyperAuthenticatorAssignEndPoint {
    pub fn new( tokens: Arc<DashMap<Token,HyperwayStub>>, logger: RootLogger ) -> Self {
        Self {
            logger,
            tokens
        }
    }
}

#[async_trait]
impl HyperAuthenticator for TokensFromHeavenHyperAuthenticatorAssignEndPoint {
    async fn auth(&mut self, req: ReqShell) -> Result<HyperwayStub, MsgErr> {
        if let Substance::Sys(Sys::ConnectReq(auth_req)) = &req.core.body {
            if let None = auth_req.end_point {
                match &*auth_req.auth {
                    Substance::Token(token) => {
                        if let Some((_,stub)) = self.tokens.remove(token)  {
                            return Ok(stub)
                        }
                        else {
                            return Err(MsgErr::forbidden())
                        }
                    }
                    _ => { return Err(MsgErr::bad_request()); }
                }
            } else {
                return Err(MsgErr::bad_request())
            }
        } else {
            // must be a LaneAuth
            return Err(MsgErr::bad_request())
        }
    }
}

pub struct TokenDispensingHyperwayInterchange {
    pub agent: Agent,
    pub logger: PointLogger,
    pub tokens: Arc<DashMap<Token,HyperwayStub>>,
    pub lane_point_factory: Box<dyn PointFactory>,
    pub end_point_factory: Box<dyn PointFactory>,
    pub interchange: HyperwayInterchange,
}

impl TokenDispensingHyperwayInterchange {

    pub fn new(agent: Agent, router: Box<dyn HyperRouter>, lane_point_factory: Box<dyn PointFactory>, end_point_factory: Box<dyn PointFactory>, logger: PointLogger ) -> Self {
        let tokens = Arc::new(DashMap::new());
        let authenticator = Box::new(TokensFromHeavenHyperAuthenticatorAssignEndPoint::new(tokens.clone(),logger.logger.clone()));
        let interchange = HyperwayInterchange::new( router, authenticator, logger.clone() );
        Self {
            agent,
            tokens,
            logger,
            lane_point_factory,
            end_point_factory,
            interchange
        }
    }

    pub fn dispense( &mut self ) -> Result<(Token, HyperwayStub),MsgErr> {
        let token = Token::new_uuid();
        let end_point = self.end_point_factory.create()?;
        let lane_point = self.lane_point_factory.create()?;
        let logger = self.logger.point(lane_point);
        let stub = HyperwayStub {
            agent: self.agent.clone(),
            logger,
            end_point
        };
        self.tokens.insert(token.clone(),stub.clone());
        Ok((token,stub))
    }
}

impl Deref for TokenDispensingHyperwayInterchange {
    type Target = HyperwayInterchange;

    fn deref(&self) -> &Self::Target {
        &self.interchange
    }
}

impl DerefMut for TokenDispensingHyperwayInterchange {
    fn deref_mut(&mut self) -> &mut Self::Target {
        & mut self.interchange
    }
}


#[async_trait]
pub trait HyperGate {
  async fn unlock(&self, version: semver::Version ) -> Result<Box<dyn OnRamp>, String>;
}

#[async_trait]
pub trait OnRamp {
  async fn enter(&self, request: ReqShell ) -> Result<(mpsc::Sender<Wave>, mpsc::Receiver<Wave>),RespShell>;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
