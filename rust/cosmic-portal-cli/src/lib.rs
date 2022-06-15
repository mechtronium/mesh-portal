#![allow(warnings)]

use std::sync::Arc;
use mesh_portal::error::MsgErr;
use mesh_portal::version::latest::cli::{CommandTemplate, RawCommand, Transfer};
use mesh_portal::version::latest::entity::request::RequestCore;
use mesh_portal::version::latest::entity::response::ResponseCore;
use mesh_portal::version::latest::id::{Point, Port, Topic};
use mesh_portal::version::latest::messaging::{ProtoRequest, Request, Response};
use mesh_portal::version::latest::msg::MsgMethod;
use mesh_portal_versions::version::v0_0_1::id::id::{Layer, ToPort};
use mesh_portal_versions::version::v0_0_1::wave::{AsyncMessengerAgent, RequestHandler, SyncMessenger, SyncMessengerRelay};

#[macro_use]
extern crate cosmic_macros;

#[macro_use]
extern crate async_trait;

pub struct Cli{
    messenger: AsyncMessengerAgent
}

impl Cli{

    pub fn new( messenger: AsyncMessengerAgent ) -> Self {
        Self {
            messenger
        }
    }

    pub async fn session(&self) -> Result<CliSession<'_>,MsgErr> {
        let to = self.messenger.from.with_layer(Layer::Shell).with_topic(Topic::CLI);
        let request = ProtoRequest::msg( to, MsgMethod::new( "NewSession").unwrap() );
        let response = self.messenger.send( request ).await;
        if response.core.is_ok() {
            let session:Port = response.core.body.try_into()?;
           Ok(CliSession::new(self,session.clone(), self.messenger.clone().with_from(response.to.with_layer(Layer::Core).with_topic(session.topic))))
        } else {
            Err("could not create cli".into())
        }
    }
}

#[derive(Clone)]
pub struct CliSession<'a> {
    pub cli: &'a Cli,
    pub to: Port,
    pub messenger: AsyncMessengerAgent
}

impl <'a> CliSession<'a> {

    pub fn new( cli: &'a Cli, to: Port, messenger: AsyncMessengerAgent ) -> Self {
        Self {
            cli,
            to,
            messenger
        }
    }

    pub async fn exec<R:ToString>(&self, raw: R) -> Response {
        self.exec_with_transfers(raw,vec![]).await
    }

    pub async fn exec_with_transfers<R>(&self, raw: R, transfers: Vec<Transfer>) -> Response
        where
            R: ToString,
    {
        let raw= RawCommand {
            line: raw.to_string(),
            transfers,
        };
        let mut request: ProtoRequest = ProtoRequest::msg(self.to.clone(),MsgMethod::new("Exec").unwrap(),  );
        request.core.body = raw.into();
        self.messenger.send(request).await
    }

    pub fn template<R:ToString>( &self, raw: R) -> Result<CommandTemplate,MsgErr> {
        unimplemented!()
    }
}

impl <'a> Drop for CliSession<'a> {
    fn drop(&mut self) {
        let request = ProtoRequest::msg(self.to.with_topic(Topic::CLI), MsgMethod::new("DropSession").unwrap() );
        self.messenger.send_sync(request);
    }
}





#[cfg(test)]
pub mod test {
    use mesh_portal::error::MsgErr;
    use mesh_portal::version::latest::entity::request::RequestCore;
    use mesh_portal::version::latest::entity::response::ResponseCore;
    use mesh_portal::version::latest::messaging::{Request, RootRequestCtx};
    use mesh_portal::version::latest::payload::Payload;
    use std::marker::PhantomData;
    use std::sync::{Arc, RwLock};
    use mesh_portal_versions::version::v0_0_1::wave::{AsyncRequestHandler, InputCtx, RequestHandler, RequestHandlerRelay};


    #[test]
    pub fn test() {
        //let mut obj: Obj = Obj::new();
        //        router.pipelines.push(IntPipeline)
    }


}
