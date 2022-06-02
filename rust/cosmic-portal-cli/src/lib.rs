use mesh_portal::version::latest::cli::{RawCommand, Transfer};
use mesh_portal::version::latest::messaging::Response;

#[macro_use]
extern crate cosmic_macros;

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
    use mesh_portal::version::latest::messaging::{MessageCtx, Request};
    use mesh_portal::version::latest::payload::Payload;
    use std::marker::PhantomData;

    pub struct Obj<E> {
        pub phantom: PhantomData<E>,
    }

    impl<E> Obj<E> {
        pub fn new() -> Self {
            let rtn = Self {
                phantom: Default::default(),
            };
            rtn
        }

        #[route("Msg<NewSession> -[ Text ]->")]
        pub fn something(&self, ctx: MessageCtx<RequestCore, String>) -> Result<ResponseCore, MsgErr> {
            Ok(ctx.ok(Payload::Empty))
        }
    }

    pub fn do_something(ctx: MessageCtx<Request, String>) -> Result<ResponseCore, MsgErr> {
        Ok(ResponseCore::fail("Test failure"))
    }

    #[test]
    pub fn test() {
        let mut obj: Obj<String> = Obj::new();
        //        router.pipelines.push(IntPipeline)
    }
}
