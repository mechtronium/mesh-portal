use mesh_portal::error::MsgErr;
use mesh_portal::version::latest::entity::request::RequestCore;
use mesh_portal::version::latest::entity::response::ResponseCore;
use mesh_portal::version::latest::messaging::MessageCtx;
use mesh_portal::version::latest::payload::Payload;
use crate::test::Obj;

impl<E> mesh_portal_versions::version::v0_0_1::messaging::RequestHandler<E> for Obj<E> {
    fn handle(
        &self,
        ctx: mesh_portal::version::latest::messaging::RootMessageCtx<
            mesh_portal::version::latest::messaging::Request,
            E,
        >,
    ) -> Result<ResponseCore, MsgErr> {
        if __Obj_something__.is_match(&ctx.request.core).is_ok() {
            return self._something(ctx);
        }
        if __Obj_user__.is_match(&ctx.request.core).is_ok() {
            return self._user(ctx);
        }
        Ok(ResponseCore::not_found())
    }
}

impl<E> Obj<E> {
    fn _something(
        &self,
        mut ctx: mesh_portal::version::latest::messaging::RootMessageCtx<
            mesh_portal::version::latest::messaging::Request,
            String,
        >,
    ) -> Result<mesh_portal::version::latest::entity::response::ResponseCore, MsgErr> {
        let mut ctx: mesh_portal::version::latest::messaging::RootMessageCtx<RequestCore, String> =
            ctx.transform_input()?;
        let ctx = ctx.push();
        fn inner(ctx: MessageCtx<RequestCore, String>) -> Result<ResponseCore, MsgErr> {
            Ok(ctx.ok(Payload::Empty))
        }
        match inner(ctx) {
            Ok(rtn) => Ok(mesh_portal::version::latest::entity::response::ResponseCore::from(rtn)),
            Err(err) => Err(err),
        }
    }
    fn _user(
        &self,
        mut ctx: mesh_portal::version::latest::messaging::RootMessageCtx<
            mesh_portal::version::latest::messaging::Request,
            String,
        >,
    ) -> Result<mesh_portal::version::latest::entity::response::ResponseCore, MsgErr> {
        let mut ctx: mesh_portal::version::latest::messaging::RootMessageCtx<RequestCore, String> =
            ctx.transform_input()?;
        let ctx = ctx.push();
        fn inner(ctx: MessageCtx<RequestCore, String>) -> Result<ResponseCore, MsgErr> {
            Ok(ctx.ok(Payload::Empty))
        }
        match inner(ctx) {
            Ok(rtn) => Ok(mesh_portal::version::latest::entity::response::ResponseCore::from(rtn)),
            Err(err) => Err(err),
        }
    }
}

lazy_static! {
    static ref __Obj_something__: &'static mesh_portal::version::latest::config::bind::RouteSelector =
        & mesh_portal::version::latest::parse::route_attribute("#[route(\"Msg<NewSession>\")]")
            .unwrap();
    static ref __Obj_user__: &'static mesh_portal::version::latest::config::bind::RouteSelector =
        & mesh_portal::version::latest::parse::route_attribute("#[route(\"Http<Get>/users\")]")
            .unwrap();
}
