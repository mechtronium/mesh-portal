#![allow(warnings)]

use std::sync::Arc;
use mesh_portal::error::MsgErr;
use mesh_portal::version::latest::cli::{CommandTemplate, RawCommand, Transfer};
use mesh_portal::version::latest::entity::request::ReqCore;
use mesh_portal::version::latest::entity::response::RespCore;
use mesh_portal::version::latest::id::{Point, Port, Topic};
use mesh_portal::version::latest::messaging::{ReqProto, ReqShell, RespShell};
use mesh_portal::version::latest::msg::MsgMethod;
use mesh_portal_versions::version::v0_0_1::id::id::{Layer, ToPort};
use mesh_portal_versions::version::v0_0_1::wave::{AsyncTransmitterWithAgent, RequestHandler, SyncTransmitter, SyncTransmitRelay};

#[macro_use]
extern crate cosmic_macros;

#[macro_use]
extern crate async_trait;

pub struct Cli{
    tx: AsyncTransmitterWithAgent
}

impl Cli{

    pub fn new(messenger: AsyncTransmitterWithAgent) -> Self {
        Self {
            tx: messenger
        }
    }

    pub async fn session(&self) -> Result<CliSession<'_>,MsgErr> {
        let to = self.tx.from.with_layer(Layer::Shell).with_topic(Topic::CLI);
        let request = ReqProto::msg(to, MsgMethod::new( "NewSession").unwrap() );
        let response = self.tx.send( request ).await?;
        if response.core.is_ok() {
            let session:Port = response.core.body.try_into()?;
           Ok(CliSession::new(self,session.clone(), self.tx.clone().with_from(response.to.with_layer(Layer::Core).with_topic(session.topic))))
        } else {
            Err("could not create cli".into())
        }
    }
}

#[derive(Clone)]
pub struct CliSession<'a> {
    pub cli: &'a Cli,
    pub to: Port,
    pub transmitter: AsyncTransmitterWithAgent
}

impl <'a> CliSession<'a> {

    pub fn new(cli: &'a Cli, to: Port, messenger: AsyncTransmitterWithAgent) -> Self {
        Self {
            cli,
            to,
            transmitter: messenger
        }
    }

    pub async fn exec<R:ToString>(&self, raw: R) -> Result<RespShell,MsgErr> {
        self.exec_with_transfers(raw,vec![]).await
    }

    pub async fn exec_with_transfers<R>(&self, raw: R, transfers: Vec<Transfer>) -> Result<RespShell,MsgErr>
        where
            R: ToString,
    {
        let raw= RawCommand {
            line: raw.to_string(),
            transfers,
        };
        let mut request: ReqProto = ReqProto::msg(self.to.clone(), MsgMethod::new("Exec").unwrap(),  );
        request.body(raw.into())?;
        self.transmitter.send(request.clone()).await
    }

    pub fn template<R:ToString>( &self, raw: R) -> Result<CommandTemplate,MsgErr> {
        unimplemented!()
    }
}

impl <'a> Drop for CliSession<'a> {
    fn drop(&mut self) {
        let request = ReqProto::msg(self.to.with_topic(Topic::CLI), MsgMethod::new("DropSession").unwrap() );
        self.transmitter.send_sync(request);
    }
}





#[cfg(test)]
pub mod test {
    use mesh_portal::error::MsgErr;
    use mesh_portal::version::latest::entity::request::ReqCore;
    use mesh_portal::version::latest::entity::response::RespCore;
    use mesh_portal::version::latest::messaging::{ReqShell, RootRequestCtx};
    use mesh_portal::version::latest::payload::Payload;
    use std::marker::PhantomData;
    use std::sync::{Arc, RwLock};
    use mesh_portal_versions::version::v0_0_1::wave::{AsyncRequestHandler, ReqCtx, RequestHandler, RequestHandlerRelay};


    #[test]
    pub fn test() {
        //let mut obj: Obj = Obj::new();
        //        router.pipelines.push(IntPipeline)
    }


}
