#![allow(warnings)]

mod scratch;

use core::option::Option;
use core::option::Option::None;
use core::result::Result::{Err, Ok};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc};
use tokio::sync::RwLock;
use mesh_portal::error::MsgErr;
use mesh_portal::version::latest::cli::{RawCommand, Transfer};
use mesh_portal::version::latest::entity::request::create::{CreateOp, Fulfillment, KindTemplate, Set};
use mesh_portal::version::latest::entity::request::{Method, Rc, RequestCore};
use mesh_portal::version::latest::entity::request::get::Get;
use mesh_portal::version::latest::entity::request::select::Select;
use mesh_portal::version::latest::entity::response::ResponseCore;
use mesh_portal::version::latest::id::{Point, TargetLayer, Topic, Uuid};
use mesh_portal::version::latest::messaging::{Request, RequestCtx, Response, RootRequestCtx};
use mesh_portal::version::latest::particle::Stub;
use mesh_portal::version::latest::payload::{Payload, PayloadType};
use mesh_portal::version::latest::id::Port;
use mesh_portal::version::latest::util::uuid;
use mesh_portal_versions::version::v0_0_1::id::id::{ToPoint, ToPort};
use mesh_portal_versions::version::v0_0_1::messaging::{AsyncMessenger, AsyncRequestHandler, AsyncMessengerRelay, AsyncRequestHandlerRelay, InternalPipeline, InternalRequestHandlers, RequestHandler, RequestHandlerRelay, Router, SyncMessenger, SyncMessengerRelay};
use mesh_portal_versions::version::v0_0_1::msg::MsgMethod;
use mesh_portal_versions::version::v0_0_1::parse::{command, command_line, Env};
use mesh_portal_versions::version::v0_0_1::parse::error::result;
use mesh_portal_versions::version::v0_0_1::parse::model::MethodScopeSelector;
use cosmic_nom::new_span;
use mesh_portal::version::latest::command::request::MethodPattern;
use mesh_portal::version::latest::config::bind::RouteSelector;
use mesh_portal_versions::version::v0_0_1::command::Command;
use mesh_portal_versions::version::v0_0_1::util::{ToResolved, ValuePattern};

#[macro_use]
extern crate cosmic_macros;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate async_trait;

#[derive(AsyncRequestHandler)]
pub struct CliRelay {
  pub port: Port,
  pub messenger: AsyncMessengerRelay,
  pub handlers: RwLock<InternalRequestHandlers<AsyncRequestHandlerRelay>>
}

#[routes_async(self.handlers.read().await)]
impl CliRelay {
    fn new(port: Port, messenger: AsyncMessengerRelay) -> Self {

        let handlers = RwLock::new(InternalRequestHandlers::new());

        let rtn = Self {
            port,
            messenger,
            handlers,
        };

        rtn
    }

    fn filter(&self, request: &Request) -> bool {
        request.from.layer == TargetLayer::Core
    }

    #[route("Msg<NewSession>")]
    pub async fn new_session(&self, ctx: RequestCtx<'_,Request>) -> Result<Port, MsgErr> {
        if !self.filter(ctx.request()) {
            return Err(MsgErr::new(403, "forbidden"));
        }

        let mut session_port = self.port.clone().with_topic(Topic::uuid());
        let mut source = ctx.request().from.clone();

        let messenger = self.messenger.clone().with_port(session_port.clone());

        let session = CliSession {
            port: session_port.clone(),
            relay: self.port.clone(),
            env: Env::new(self.port.clone().to_point()),
            source,
            messenger
        };

        let selector = RouteSelector::any().with_topic(session_port.topic.clone());
        {
            let mut write = self.handlers.write().await;
            write.add(selector, AsyncRequestHandlerRelay::new(Box::new(session)));
        }

        Ok(session_port)
    }

    #[route("Msg<EndSession>")]
    pub async fn end_session(&self, ctx: RequestCtx<'_,Request>) -> Result<ResponseCore, MsgErr> {
        if !self.filter(ctx.request()) {
            return Err(MsgErr::new(403, "forbidden"));
        }

        let mut write  = self.handlers.write().await;
        write.remove_topic(Some(ValuePattern::Pattern(ctx.to.topic.clone())));
        Ok(ResponseCore::ok(Payload::Empty))
    }
}


#[derive(AsyncRequestHandler)]
pub struct CliSession {
    pub relay: Port,
    pub port: Port,
    pub env: Env,
    pub messenger: AsyncMessengerRelay,
    // will only handle requests from THIS port
    pub source: Port
}

#[routes_async]
impl CliSession {

    pub fn new( port: Port, relay: Port, messenger: AsyncMessengerRelay, source: Port ) -> CliSession {
        let messenger = messenger.with_port( port.clone() );
        let env = Env::new(port.clone().to_point() );
        Self {
            port,
            relay,
            env,
            messenger,
            source,
        }
    }

    pub fn filter( &self, request: &Request ) -> bool {
        request.from == self.source
    }

    #[route("Msg<ExecCommand>")]
    pub async fn exec( &self, ctx: RequestCtx<'_,RawCommand> ) -> Result<ResponseCore,MsgErr> {

        if !self.filter(ctx.request()) {
            return Err(MsgErr::forbidden());
        }

        let exec_topic = Topic::uuid();
        let exec_port = self.port.clone().with_topic(exec_topic.clone());
        let messenger = self.messenger.clone().with_topic(exec_topic);
        let mut exec = CommandExecutor::new( exec_port, self.source.clone(), messenger, self.env.clone() );

        let result = exec.execute(ctx).await;

        result
    }

}

#[derive(AsyncRequestHandler)]
pub struct CommandExecutor {
    messenger: AsyncMessengerRelay,
    port: Port,
    source: Port,
    env: Env
}

#[routes_async]
impl CommandExecutor {
    pub fn new(
        port: Port,
        source: Port,
        messenger: AsyncMessengerRelay,
        env: Env
    ) -> Self {

        Self {
            messenger,
            port,
            source,
            env
        }
    }

    pub async fn execute( &self, raw: RequestCtx<'_,RawCommand> ) -> Result<ResponseCore,MsgErr> {
        let command = result(command_line(new_span(raw.line.as_str())))?;
        let mut env = self.env.clone();
        for transfer in &raw.transfers {
            env.set_file(transfer.id.clone(),transfer.content.clone())
        }
        let command: Command = command.to_resolved(&self.env)?;
        let request: RequestCore = command.into();
        let request = Request::new(request,self.port.clone(), Point::registry().to_port() );
        Ok(self.messenger.send(request).await.core)
    }

}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
