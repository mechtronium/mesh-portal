use crate::version::latest;
use crate::version::latest::entity::request::ReqEntity;
use crate::version::latest::id::Address;

pub type Request = generic::Request<ReqEntity>;
pub type Response = generic::Response;

pub mod generic {
    use serde::{Deserialize, Serialize};
    use std::convert::{TryFrom, TryInto};

    use crate::version::latest;
    use crate::version::latest::entity::request::{ReqEntity, Rc, Msg, Http};
    use crate::version::latest::entity::response;
    use crate::version::latest::generic;
    use crate::version::latest::messaging::{Exchange, ExchangeId};
    use crate::version::latest::payload::PayloadDelivery;
    use crate::version::latest::portal::{inlet, outlet};
    use crate::version::latest::{entity, portal};
    use crate::version::v0_0_1::util::{unique_id, ConvertFrom, Convert};

    #[derive(Clone, Serialize, Deserialize)]
    pub struct Request<Entity> {
        pub id: String,
        pub to: Address,
        pub from: Address,
        pub entity: Entity,
        pub exchange: Exchange,
    }


use crate::version::v0_0_1;
    use crate::version::latest::id::Address;

    impl TryInto<latest::portal::outlet::Request>
        for Request<latest::entity::request::ReqEntity>

    {
        type Error = crate::error::Error;

        fn try_into(self) -> Result<portal::outlet::Request, Self::Error> {
            Ok(generic::portal::outlet::Request {
                from: self.from,
                //entity: ConvertFrom::convert_from(self.entity)?,
                entity: self.entity,
                exchange: self.exchange,
            })
        }
    }

    impl<Entity> Request<Entity> {
        pub fn new(to: Address, from: Address, entity: Entity, exchange: Exchange) -> Self {
            Request {
                id: unique_id(),
                to,
                from,
                entity,
                exchange,
            }
        }
    }

    impl Request<ReqEntity> {
        pub fn from(request: inlet::Request, from: Address, to: Address, exchange: Exchange) -> Self {
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
    pub struct Response {
        pub id: String,
        pub to: Address,
        pub from: Address,
        pub exchange: ExchangeId,
        pub entity: response::RespEntity,
    }

    impl Response {
        pub fn from(response: outlet::Response, from: Address, to: Address ) -> Self {
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

    impl Into<outlet::Response> for Response {
        fn into(self) -> outlet::Response {
            outlet::Response {
                id: self.id,
                to: self.to,
                from: self.from,
                exchange: self.exchange,
                entity: self.entity,
            }
        }
    }
}
