#![allow(warnings)]
mod scratch;

use mesh_portal::version::latest::cli::{RawCommand, Transfer};
use mesh_portal::version::latest::messaging::Response;
use mesh_portal::version::latest::service::RequestHandler;

#[macro_use]
extern crate cosmic_macros;

#[macro_use]
extern crate lazy_static;

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
    use mesh_portal::version::latest::messaging::{MessageCtx, Request, RootMessageCtx};
    use mesh_portal::version::latest::service::RequestHandler;
    use mesh_portal::version::latest::payload::Payload;
    use std::marker::PhantomData;

    //#[derive(RequestHandler)]
    pub struct Obj<E> {
        pub phantom: PhantomData<E>,
    }

    //#[routes]
    impl<E> Obj<E> {
        pub fn new() -> Self {
            let rtn = Self {
                phantom: Default::default(),
            };
            rtn
        }

     //   #[route("Msg<NewSession>")]
        fn something(&self, ctx: MessageCtx<RequestCore, String>) -> Result<ResponseCore, MsgErr> {
            Ok(ctx.ok(Payload::Empty))
        }

      //  #[route("Http<Get>/users")]
        fn user(&self, ctx: MessageCtx<RequestCore, String>) -> Result<ResponseCore, MsgErr> {
            Ok(ctx.ok(Payload::Empty))
        }
    }

    #[test]
    pub fn test() {
        let mut obj: Obj<String> = Obj::new();
        //        router.pipelines.push(IntPipeline)
    }


}
