use crate::error::MsgErr;
use crate::version::v0_0_1::command::request::{Method, RequestCore};
use crate::version::v0_0_1::entity::response::ResponseCore;
use crate::version::v0_0_1::id::id::{Point, Topic};
use crate::version::v0_0_1::messaging::messaging::{
    Agent, Message, RequestCtx, MessageIn, MessageOut, Request, Response,
    RootRequestCtx,
};
use crate::version::v0_0_1::parse::model::MethodScopeSelector;
use crate::version::v0_0_1::security::Access;
use crate::version::v0_0_1::util::ValueMatcher;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock};
use crate::version::v0_0_1::config::config::bind::RouteSelector;

pub trait Router: Send + Sync {
    fn route(&self, message: Message);
}

pub struct AsyncMessengerRelay {
    pub proxy: Arc<dyn AsyncMessenger>
}

#[async_trait]
impl AsyncMessenger for AsyncMessengerRelay {
    async fn send(&self, request: Request) -> Response{
        self.proxy.send(request).await
    }
}


pub struct SyncMessengerRelay {
    pub proxy: Arc<dyn SyncMessenger>
}

impl SyncMessenger for SyncMessengerRelay {
    fn send(&self, request: Request) -> Response{
        self.proxy.send(request)
    }
}


#[async_trait]
pub trait AsyncMessenger: Send + Sync {
    async fn send(&self, request: Request) -> Response;
}

pub trait SyncMessenger: Send + Sync {
    fn send(&self, request: Request) -> Response;
}

pub trait AccessProvider: Send + Sync {
    fn access(&self, to: &Agent, on: &Point) -> Result<Access, MsgErr>;
}

pub struct AllAccessProvider();

impl AccessProvider for AllAccessProvider {
    fn access(&self, _: &Agent, _: &Point) -> Result<Access, MsgErr> {
        Ok(Access::SuperOwner)
    }
}

#[async_trait]
pub trait Global: Send + Sync {
    async fn handle(&self, request: Request) -> Response;
}

pub struct InternalPipeline<H>
{
    pub selector: RouteSelector,
    pub handler: H,
}

impl<H> InternalPipeline<H>
{
    pub fn new(selector: RouteSelector, mut handler: H) -> Self {
        Self {
            selector,
            handler,
        }
    }
}

pub trait RequestHandler {
    fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr>;
}

#[async_trait]
pub trait AsyncRequestHandler: Sync+Send  {
    async fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr>;
}

pub struct RequestHandlerRelay {
   pub relay: Box<dyn RequestHandler>
}

impl RequestHandler for RequestHandlerRelay {
    fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr> {
        self.relay.handle(ctx)
    }
}

pub struct AsyncRequestHandlerRelay {
    pub relay: Box<dyn AsyncRequestHandler>
}

#[async_trait]
impl AsyncRequestHandler for AsyncRequestHandlerRelay {
    async fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr> {
        self.relay.handle(ctx).await
    }
}

pub struct InternalRequestHandler<H> {
    pipelines: Vec<InternalPipeline<H>>,
}

impl<H> InternalRequestHandler<H> {
    pub fn new() -> Self {
        Self {
            pipelines: vec![],
        }
    }

    pub fn add(&mut self, selector: RouteSelector, handler: H) {
        let pipeline = InternalPipeline { selector, handler };
        self.pipelines.push(pipeline);
    }

    pub fn select(&self, request: &Request ) -> Result<&H,()> {
        for pipeline in self.pipelines.iter() {
            if pipeline.selector.is_match(&request).is_ok() {
                return Ok(&pipeline.handler);
            }
        }
        Err(())
    }
}

impl InternalRequestHandler<RequestHandlerRelay> {
    pub fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr> {
        for pipeline in self.pipelines.iter() {
            if pipeline.selector.is_match(&ctx.request).is_ok() {
                let handler = &pipeline.handler;
                return handler.handle(ctx);
            }
        }
        Ok(ResponseCore::not_found())
    }
}

impl InternalRequestHandler<AsyncRequestHandlerRelay> {
    pub async fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr> {
        for pipeline in self.pipelines.iter() {
            if pipeline.selector.is_match(&ctx.request).is_ok() {
                return pipeline.handler.handle(ctx).await;
            }
        }
        Ok(ResponseCore::not_found())
    }
}



