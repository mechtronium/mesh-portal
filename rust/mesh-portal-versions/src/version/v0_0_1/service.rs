use crate::error::MsgErr;
use crate::version::v0_0_1::command::request::{Method, RequestCore};
use crate::version::v0_0_1::entity::response::ResponseCore;
use crate::version::v0_0_1::id::id::{Point, Topic};
use crate::version::v0_0_1::messaging::messaging::{
    Agent, Message, MessageCtx, MessageIn, MessageOut, Request, RequestCtx, Response, ResponseCtx,
    RootMessageCtx,
};
use crate::version::v0_0_1::parse::model::MethodScopeSelector;
use crate::version::v0_0_1::security::Access;
use crate::version::v0_0_1::util::ValueMatcher;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::sync::RwLock;

pub trait Router: Send + Sync {
    fn route(&self, message: Message);
}

#[async_trait]
pub trait AsyncMessenger<R>: Send + Sync {
    async fn send(&self, request: RequestCtx<R>) -> ResponseCtx<Response>;
}

pub struct AsyncMessengerProxy<R>
where
    R: Send + Sync,
{
    pub messenger: Box<dyn AsyncMessenger<R>>,
}

#[async_trait]
impl<R> AsyncMessenger<R> for AsyncMessengerProxy<R>
where
    R: Send + Sync,
{
    async fn send(&self, request: RequestCtx<R>) -> ResponseCtx<Response> {
        self.messenger.send(request).await
    }
}

pub trait Messenger<R>: Send + Sync {
    fn send(&self, request: RequestCtx<R>) -> Response;
}

pub struct MessengerProxy<R> {
    pub messenger: Box<dyn Messenger<R>>,
}

#[async_trait]
impl<R> Messenger<R> for MessengerProxy<R> {
    fn send(&self, request: RequestCtx<R>) -> Response {
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
pub enum IntPipelineSelector {
    Topic(Topic),
    Request(MethodScopeSelector),
}

pub struct HandlerPair<E, H>
where
    H: Handler<E>,
{
    pub selector: IntPipelineSelector,
    pub handler: H,
    pub messenger: PhantomData<E>,
}

impl<E, H> HandlerPair<E, H>
where
    H: Handler<E>,
{
    pub fn new(selector: IntPipelineSelector, mut handler: H) -> Self {
        let messenger = Default::default();
        Self {
            selector,
            handler,
            messenger,
        }
    }
}

impl IntPipelineSelector {
    pub fn is_match(&self, request: &Request) -> bool {
        match self {
            IntPipelineSelector::Topic(topic) => request.to.topic == *topic,
            IntPipelineSelector::Request(selector) => selector.is_match(request).is_ok(),
        }
    }
}

pub trait Handler<E> {
    fn handle(&self, ctx: MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr>;
}

impl<E, F> Handler<E> for F
where
    F: Fn(MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr> + Copy,
{
    fn handle(&self, ctx: MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr> {
        self(ctx)
    }
}

pub trait HandlerMatch {
    fn is_match(&self, message: MessageIn) -> Result<(), ()>;
}

pub struct Handlers<E, H>
where
    H: Handler<E>,
{
    handlers: RwLock<Vec<HandlerPair<E, H>>>,
    messenger: PhantomData<E>,
}

impl<E, H> Handlers<E, H>
where
    E: Sized,
    H: Handler<E>,
{
    pub fn new() -> Self {
        Handlers {
            handlers: RwLock::new(vec![]),
            messenger: Default::default(),
        }
    }

    pub fn add(&mut self, handler: HandlerPair<E, H>) -> Result<(), MsgErr> {
        let mut write = self.handlers.write()?;
        write.push(handler);
        Ok(())
    }

    pub fn remove(&self, topic: &Topic) -> Result<(), MsgErr> {
        let mut write = self.handlers.write()?;
        write.retain(|h| {
            if let IntPipelineSelector::Topic(t) = &h.selector {
                *t != *topic
            } else {
                true
            }
        });
        Ok(())
    }

    pub fn handle(&self, ctx: RootMessageCtx<Request, E>) -> ResponseCore {
        fn unlock<E2, H2>(
            handlers: &Handlers<E2, H2>,
            ctx: RootMessageCtx<Request, E2>,
        ) -> Result<ResponseCore, MsgErr>
        where
            E2: Sized,
            H2: Handler<E2>,
        {
            let read = handlers.handlers.read()?;
            for pair in read.iter() {
                if pair.selector.is_match(&ctx.request) {
                    let ctx = ctx.push();
                    let handler = &pair.handler;
                    match handler.handle(ctx) {
                        Ok(response) => return Ok(response),
                        Err(err) => {
                            return Ok(ResponseCore::fail(err.to_string().as_str()));
                        }
                    }
                }
            }
            Ok(ResponseCore::fail("could not match request to handler"))
        }

        match unlock(self, ctx) {
            Ok(response) => response,
            Err(err) => ResponseCore::err(err),
        }
    }
}

/*
#[async_trait]
pub trait ArtifactApi: Send+Sync {
    fn get_artifact<A>(&self, artifact: &Point) -> Result<ArtifactItem<CachedConfig<A>>>;
}

 */

/*
pub trait IntHandler<E> {
    fn handle(&self, ctx: MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr>;
}

impl<E,F,S> IntHandler<E> for F
    where
        F: Fn(&S,MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr> + Copy,
{
    fn handle( &self, ctx: MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr> {
        self(ctx)
    }
}

 */

pub struct IntPipeline<E> {
    pub selector: IntPipelineSelector,
    pub handler: Box<dyn IntHandler<E>>,
}

pub trait IntHandler<E> {
    fn handle(&self, ctx: MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr>;
}

impl<E,F> IntHandler<E> for F
where
    F: Fn(MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr> + Copy,
{
    fn handle(&self, ctx: MessageCtx<Request, E>) -> Result<ResponseCore, MsgErr> {
        self(ctx)
    }
}

pub struct IntRouter<E> {
    pipelines: Vec<IntPipeline<E>>,
    messenger: PhantomData<E>,
}

impl<E> IntRouter<E> {
    pub fn new() -> Self {
        Self {
            pipelines: vec![],
            messenger: PhantomData::default(),
        }
    }

    pub fn add(
        &mut self,
        selector: IntPipelineSelector,
        handler: Box<dyn IntHandler<E>>,
    ) {
        let pipeline = IntPipeline {
            selector,
            handler
        };
        self.pipelines.push(pipeline);
    }

    pub fn handle(&self, ctx: RootMessageCtx<Request, E>) -> Result<ResponseCore, MsgErr> {
        for pipeline in self.pipelines.iter() {
            if pipeline.selector.is_match(&ctx.request) {
                let ctx = ctx.push();
                let handler= &pipeline.handler;
                return handler.handle(ctx);
            }
        }
        Ok(ResponseCore::fail("not found"))
    }
}




#[cfg(test)]
pub mod test {
    use crate::error::MsgErr;
    use crate::version::v0_0_1::entity::response::ResponseCore;
    use crate::version::v0_0_1::id::id::Topic;
    use crate::version::v0_0_1::messaging::messaging::{MessageCtx, Request};
    use crate::version::v0_0_1::service::{IntPipelineSelector, Handlers, IntPipeline, IntRouter};

    pub struct Obj {
        pub router: IntRouter<String>,
    }

    #[select("Msg<NewSession> -[ Text ]->")]
    pub fn something() {

    }

    impl Obj {
        pub fn new() -> Self {
            let mut router = IntRouter::new();
            let rtn = Self {
                router: IntRouter::new(),
            };
            router.add(
                IntPipelineSelector::Topic(Topic::Named("blah".to_string())),
                Box::new(do_something),
            );
            rtn
        }
    }

    pub fn do_something(ctx: MessageCtx<Request, String>) -> Result<ResponseCore, MsgErr> {
        Ok(ResponseCore::fail("Test failure"))
    }

    #[test]
    pub fn test() {
        let mut router:IntRouter<String> = IntRouter::new();
//        router.pipelines.push(IntPipeline)
    }
}
