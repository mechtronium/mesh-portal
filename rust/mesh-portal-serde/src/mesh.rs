use crate::version::latest;
use crate::version::latest::entity::request::ReqEntity;
use crate::version::latest::entity::response::RespEntity;
use crate::version::latest::fail;
use crate::version::latest::id::Address;

pub type Request = generic::Request;
pub type Response = generic::Response;

pub mod generic {
    use serde::{Deserialize, Serialize};
    use std::convert::{TryFrom, TryInto};

    use crate::version::latest;
    use crate::version::latest::entity::request::{Http, Msg, Rc, ReqEntity};
    use crate::version::latest::entity::response;
    use crate::version::latest::messaging::{Exchange, ExchangeId};
    use crate::version::latest::payload::PayloadDelivery;
    use crate::version::latest::portal::{inlet, outlet};
    use crate::version::latest::{entity, portal};
    use crate::version::latest::{fail, generic};
    use crate::version::v0_0_1::util::{unique_id, Convert, ConvertFrom};

    #[derive(Clone, Serialize, Deserialize)]
    pub struct Request {
        pub id: String,
        pub to: Address,
        pub from: Address,
        pub entity: ReqEntity,
        pub exchange: Exchange,
    }

    use crate::error::Error;
    use crate::version::latest::entity::response::RespEntity;
    use crate::version::latest::id::Address;
    use crate::version::v0_0_1;

    impl TryInto<portal::outlet::Request> for Request {
        type Error = crate::error::Error;

        fn try_into(self) -> Result<portal::outlet::Request, Self::Error> {
            Ok(portal::outlet::Request {
                from: self.from,
                to: self.to,
                //entity: ConvertFrom::convert_from(self.entity)?,
                entity: self.entity,
                exchange: self.exchange,
            })
        }
    }

    impl Request {
        pub fn new(to: Address, from: Address, entity: ReqEntity, exchange: Exchange) -> Self {
            Request {
                id: unique_id(),
                to,
                from,
                entity,
                exchange,
            }
        }
    }

    impl Request {
        pub fn from(
            request: inlet::Request,
            to: Address,
            exchange: Exchange,
        ) -> Self {
            Self {
                id: request.id,
                to,
                from:request.from,
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
    pub struct Response{
        pub id: String,
        pub to: Address,
        pub from: Address,
        pub exchange: ExchangeId,
        pub entity: RespEntity
    }

    /*
    impl<Payload> Response<Payload> {
        pub fn from(response: outlet::Response, from: Address, to: Address) -> Result<Self, Error> {
            Ok(Self {
                id: response.id,
                to,
                from,
                exchange: response.exchange,
                entity: match response.entity {
                    RespEntity::Ok(payload) => RespEntity::Ok(payload.convert()?),
                    RespEntity::Fail(fail) => RespEntity::Fail(fail),
                },
            })
        }
    }

     */

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
            outlet::Response{
                id: self.id,
                to: self.to,
                from: self.from,
                entity: self.entity,
                exchange: self.exchange
            }
        }
    }

    /*
    impl<FromPayload> Response<FromPayload> {
        fn convert<ToPayload>(self) -> Result<generic::portal::outlet::Response<ToPayload>, Error>
        where
            ToPayload: TryFrom<FromPayload, Error = Error>,
        {
            Ok(generic::portal::outlet::Response {
                id: self.id,
                to: self.to,
                from: self.from,
                exchange: self.exchange,
                entity: match self.entity {
                    RespEntity::Ok(payload) => RespEntity::Ok(payload.convert()?),
                    RespEntity::Fail(fail) => RespEntity::Fail(fail),
                },
            })
        }
    }

     */
}
