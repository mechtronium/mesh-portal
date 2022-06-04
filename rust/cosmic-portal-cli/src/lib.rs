#![allow(warnings)]

use mesh_portal::version::latest::cli::{RawCommand, Transfer};
use mesh_portal::version::latest::entity::request::RequestCore;
use mesh_portal::version::latest::entity::response::ResponseCore;
use mesh_portal::version::latest::id::{Point, Port};
use mesh_portal::version::latest::messaging::{Request, Response};
use mesh_portal_versions::version::v0_0_1::id::id::ToPort;
use mesh_portal_versions::version::v0_0_1::messaging::{RequestHandler, SyncMessenger, SyncMessengerRelay};

#[macro_use]
extern crate cosmic_macros;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate async_trait;


trait CliClient {
    fn send(&self, raw : RawCommand) -> Response;

    fn send_with_transfers<C>(&self, raw: C, transfers: Vec<Transfer>) -> Response
    where
        C: ToString,
    {
        let raw= RawCommand {
            line: raw.to_string(),
            transfers,
        };
        self.send(raw)
    }
}

pub struct CliClientMessenger {
    pub port: Port,
    pub messenger: SyncMessengerRelay
}

impl CliClient for CliClientMessenger {
    fn send(&self, raw: RawCommand) -> Response{
        let request: RequestCore = raw.into();
        let request = Request::new(request, self.port.clone(), Point::registry().to_port() );
        self.messenger.send(request)
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
    use mesh_portal_versions::version::v0_0_1::messaging::{AsyncRequestHandler, RequestCtx, RequestHandler, RequestHandlerRelay};


    #[test]
    pub fn test() {
        //let mut obj: Obj = Obj::new();
        //        router.pipelines.push(IntPipeline)
    }


}
