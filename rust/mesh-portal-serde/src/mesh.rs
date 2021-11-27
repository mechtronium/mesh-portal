use crate::version::latest;
use crate::version::latest::entity::request::ReqEntity;
use crate::version::latest::id::Identifier;

pub type Request = generic::Request<ReqEntity, Identifier>;
pub type Response = generic::Response<Identifier>;

pub mod generic {
    use serde::{Deserialize, Serialize};
    use std::convert::{TryFrom, TryInto};

    use crate::version::latest;
    use crate::version::latest::entity::request::{ReqEntity, Rc, Msg, Http};
    use crate::version::latest::entity::response;
    use crate::version::latest::generic;
    use crate::version::latest::id::Identifier;
    use crate::version::latest::messaging::{Exchange, ExchangeId};
    use crate::version::latest::payload::PayloadDelivery;
    use crate::version::latest::portal::{inlet, outlet};
    use crate::version::latest::{entity, portal};
    use crate::version::v0_0_1::util::{unique_id, ConvertFrom, Convert};

    #[derive(Clone, Serialize, Deserialize)]
    pub struct Request<ENTITY, ID> {
        pub id: String,
        pub to: ID,
        pub from: ID,
        pub entity: ENTITY,
        pub exchange: Exchange,
    }


use crate::version::v0_0_1;

    impl TryInto<latest::portal::outlet::Request>
        for Request<latest::entity::request::ReqEntity, latest::id::Identifier>

    {
        type Error = crate::error::Error;

        fn try_into(self) -> Result<portal::outlet::Request, Self::Error> {
            Ok(generic::portal::outlet::Request {
                from: self.from.try_into()?,
                entity: ConvertFrom::convert_from(self.entity)?,
                exchange: self.exchange,
            })
        }
    }

    impl<ENTITY, ID> Request<ENTITY, ID> {
        pub fn new(to: ID, from: ID, entity: ENTITY, exchange: Exchange) -> Self {
            Request {
                id: unique_id(),
                to,
                from,
                entity,
                exchange,
            }
        }
    }

    impl<ID> Request<ReqEntity, ID> {
        pub fn from(request: inlet::Request, from: ID, to: ID, exchange: Exchange) -> Self {
            Self {
                id: request.id,
                to,
                from,
                entity: request.entity,
                exchange,
            }
        }
    }

    /*
    impl <IDENTIFIER,PAYLOAD> Request<latest::generic::entity::request::ReqEntity<PAYLOAD>,IDENTIFIER> {
        fn inlet_requests(self) -> Vec<generic::portal::inlet::Request<IDENTIFIER, PAYLOAD>> {

            generic::portal::inlet::Request {
                id: self.id,
                to: self.to,
                entity: self.entity,
                exchange: self.exchange
            }
        }
    }

     */

    #[derive(Clone, Serialize, Deserialize)]
    pub struct Response<ID> {
        pub id: String,
        pub to: ID,
        pub from: ID,
        pub exchange: ExchangeId,
        pub entity: response::RespEntity,
    }

    impl<ID> Response<ID> {
        pub fn from(response: outlet::Response, from: ID, to: ID) -> Self {
            Self {
                id: response.id,
                to,
                from,
                exchange: response.exchange,
                entity: response.entity,
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
                id: self.id,
                from: self.from,
                exchange: self.exchange,
                entity: self.entity,
            }
        }
    }
}
