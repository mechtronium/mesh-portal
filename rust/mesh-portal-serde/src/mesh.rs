use crate::version::latest::entity::request::ReqEntity;
use crate::version::latest::id::Identifier;

pub type Request = generic::Request<ReqEntity,Identifier>;
pub type Response = generic::Response<Identifier>;

pub mod generic {
    use std::convert::TryInto;
    use serde::{Serialize,Deserialize};

    use crate::version::latest::portal::{inlet, outlet};
    use crate::version::latest::id::Identifier;
    use crate::version::latest::messaging::{ExchangeId, Exchange};
    use crate::version::latest::entity::request::ReqEntity;
    use crate::version::latest::entity::response;
    use crate::version::latest::{portal, entity};

    #[derive(Clone,Serialize,Deserialize)]
    pub struct Request<ENTITY,ID>{
        pub to: ID,
        pub from: ID,
        pub entity: ENTITY,
        pub exchange: Exchange
    }

    impl<ENTITY,ID> Request<ENTITY,ID> {
        pub fn new(to: ID, from: ID, entity: ENTITY, exchange: Exchange) -> Self {
            Request {
                to,
                from,
                entity,
                exchange
            }
        }
    }

    impl <ID> Request<ReqEntity,ID>{
        pub fn from(request: inlet::Request, from: ID, to: ID, exchange: Exchange) -> Self {
            Self {
                to,
                from,
                entity: request.entity,
                exchange
            }
        }
    }

    impl Into<inlet::exchange::Request> for Request<ReqEntity,Identifier> {
        fn into(self) -> inlet::exchange::Request {
            inlet::exchange::Request {
                to: vec![self.to.into()],
                entity: self.entity,
                exchange: self.exchange
            }
        }
    }

    impl Into<portal::outlet::exchange::Request> for Request<ReqEntity,Identifier> {
        fn into(self) ->  portal::outlet::exchange::Request{
            portal::outlet::exchange::Request{
                from: self.from,
                entity: self.entity,
                exchange: self.exchange
            }
        }
    }

    #[derive(Clone,Serialize,Deserialize)]
    pub struct Response<ID>{
        pub to: ID,
        pub from: ID,
        pub exchange: ExchangeId,
        pub entity: response::RespEntity
    }

    impl <ID> Response<ID>  {
        pub fn from(response: outlet::Response, from: ID, to: ID) -> Self {
            Self {
                to,
                from,
                exchange: response.exchange,
                entity: response.entity
            }
        }
    }

    /*
    impl Into<inlet::Response> for Response {
        fn into(self) -> inlet::Response {
            inlet::Response {
                to: self.to,
                exchange: self.exchange,
                entity: self.entity
            }
        }
    }

     */

    impl Into<outlet::Response> for Response<Identifier> {
        fn into(self) -> outlet::Response {
            outlet::Response {
                from: self.from,
                exchange: self.exchange,
                entity: self.entity
            }
        }
    }
}