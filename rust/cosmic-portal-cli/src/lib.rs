#![allow(warnings)]

use mesh_portal::version::latest::cli::{RawCommand, Transfer};
use mesh_portal::version::latest::messaging::Response;
use mesh_portal_versions::version::v0_0_1::messaging::RequestHandler;

#[macro_use]
extern crate cosmic_macros;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate async_trait;


trait CliClient {
    fn send(&self, command_line: RawCommand) -> Response;
    //        let request = MsgRequest::new("RawCommand")?.with_body(line.into());

    fn line<C>(&self, content: C, transfers: Vec<Transfer>) -> Response
    where
        C: ToString,
    {
        let line = RawCommand {
            line: content.to_string(),
            transfers,
        };
        self.send(line)
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
        let mut obj: Obj = Obj::new();
        //        router.pipelines.push(IntPipeline)
    }


}
