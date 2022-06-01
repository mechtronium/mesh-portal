use crate::error::MsgErr;
use crate::version::v0_0_1::command::request::{Method, RequestCore};
use crate::version::v0_0_1::entity::response::ResponseCore;
use crate::version::v0_0_1::id::id::{Point, Topic};
use crate::version::v0_0_1::messaging::messaging::{
    Agent, Message, MessageCtx, MessageIn, MessageOut, Request, Response, RootMessageCtx,
};
use crate::version::v0_0_1::parse::model::MethodScopeSelector;
use crate::version::v0_0_1::security::Access;
use crate::version::v0_0_1::util::ValueMatcher;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

pub trait Router: Send + Sync {
    fn route(&self, message: Message);
}

#[async_trait]
pub trait AsyncMessenger: Send + Sync {
    async fn send(&self, request: Request) -> Response;
}

pub struct AsyncMessengerProxy {
    pub messenger: Box<dyn AsyncMessenger>,
}

#[async_trait]
impl AsyncMessenger for AsyncMessengerProxy {
    async fn send(&self, request: Request) -> Response {
        self.messenger.send(request).await
    }
}

pub trait Messenger: Send + Sync {
    fn send(&self, request: Request) -> Response;
}

pub struct MessengerProxy {
    pub messenger: Box<dyn Messenger>,
}

#[async_trait]
impl Messenger for MessengerProxy {
    fn send(&self, request: Request) -> Response {
        self.messenger.send(request)
    }
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

#[derive(Clone)]
pub enum HandlerSelector {
    Topic(Topic),
    Request(MethodScopeSelector),
}

pub struct HandlerPair<E, F>
where
    F: FnMut(MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr>,
{
    pub selector: HandlerSelector,
    pub handler: F,
    pub messenger: PhantomData<E>,
}

impl<E, F> HandlerPair<E, F>
where
    F: FnMut(MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr>,
{
    fn new<I, O>(selector: HandlerSelector, mut f: F) -> Self
    where
        I: TryFrom<Request, Error = MsgErr>,
        O: Into<ResponseCore>,
    {
        let messenger = Default::default();
        Self {
            selector,
            handler: f,
            messenger,
        }
    }
}


impl HandlerSelector {
    pub fn is_match(&self, request: &Request) -> bool {
        match self {
            HandlerSelector::Topic(topic) => request.to.topic == *topic,
            HandlerSelector::Request(selector) => selector.is_match(request).is_ok(),
        }
    }
}

pub trait Handler<I, O, E> {
    fn handle(&mut self, ctx: MessageCtx<I, E>) -> Result<O, MsgErr>;
}

impl<I, O, E, F> Handler<I, O, E> for F
where
    F: FnMut(MessageCtx<I, E>) -> Result<O, MsgErr> + Copy,
{
    fn handle(&mut self, ctx: MessageCtx<I, E>) -> Result<O, MsgErr> {
        self(ctx)
    }
}

pub trait HandlerMatch {
    fn is_match(&self, message: MessageIn) -> Result<(), ()>;
}

pub struct Handlers<E, F>
where
    F: FnMut(MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr> + Copy,
{
    handlers: Vec<HandlerPair<E, F>>,
    messenger: PhantomData<E>,
}

impl<E, F> Handlers<E, F>
where
    F: FnMut(MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr> + Copy,
{
    pub fn new() -> Self {
        Handlers {
            handlers: vec![],
            messenger: Default::default(),
        }
    }

    fn map_selector<I, S>(mut s: S) -> impl FnMut(RootMessageCtx<Request, E>) -> Result<(), ()>
    where
        S: FnMut(MessageCtx<'_, I, E>) -> Result<(), ()> + Copy,
        I: TryFrom<Request, Error = MsgErr>,
    {
        move |ctx| {
            let mut root: RootMessageCtx<I, E> = ctx.transform_input().map_err(|e| ())?;
            let ctx = root.push();
            s(ctx)
        }
    }

    fn map_handler<I, O, F2>(
        mut f: F2,
    ) -> impl FnMut(RootMessageCtx<Request, E>) -> Result<Response, MsgErr>
    where
        I: TryFrom<Request, Error = MsgErr>,
        O: Into<Response>,
        F2: FnMut(MessageCtx<'_, I, E>) -> Result<O, MsgErr>,
    {
        move |ctx| {
            let mut root: RootMessageCtx<I, E> = ctx.transform_input()?;
            let ctx = root.push();
            f(ctx).map(|o| o.into())
        }
    }

    pub fn add(&mut self, handler: HandlerPair<E, F>) {
        self.handlers.push(handler);
    }

    pub fn remove(&mut self, topic: &Topic) {
        self.handlers.retain(|h| {
            if let HandlerSelector::Topic(t) = &h.selector {
                *t != *topic
            } else {
                true
            }
        });
    }

    pub fn handle(&self, mut ctx: RootMessageCtx<Request, E>) -> ResponseCore {
        for handler in self.handlers.iter() {
            if handler.selector.is_match(&ctx.request) {
                let ctx = ctx.push();
                match (handler.handler.clone())(ctx) {
                    Ok(response) => {
                        return response;
                    }
                    Err(err) => {
                        return ResponseCore::fail(err.to_string().as_str());
                    }
                }
            }
        }
        ResponseCore::fail("could not match request to handler")
    }
}

/*
#[async_trait]
pub trait ArtifactApi: Send+Sync {
    fn get_artifact<A>(&self, artifact: &Point) -> Result<ArtifactItem<CachedConfig<A>>>;
}

 */
