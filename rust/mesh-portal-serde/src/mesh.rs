use crate::version::latest::entity::request::Entity;

pub type Request = generic::Request<Entity>;
pub type Response = generic::Response;

pub mod generic {
    use anyhow::Error;
    use std::convert::TryInto;

    use crate::version::latest::portal::{inlet, outlet};
    use crate::version::latest::id::Identifier;
    use crate::version::latest::messaging::{ExchangeId, Exchange};
    use crate::version::latest::entity::request::Entity;
    use crate::version::latest::entity::response;

    #[derive(Clone)]
    pub struct Request<ENTITY> {
        pub to: Identifier,
        pub from: Identifier,
        pub entity: ENTITY,
        pub exchange: Exchange
    }

    impl<ENTITY> Request<ENTITY> {
        pub fn new(to: Identifier, from: Identifier, entity: ENTITY, exchange: Exchange) -> Self {
            Request {
                to,
                from,
                entity,
                exchange
            }
        }
    }


    impl Request<Entity> {
        pub fn from(request: inlet::Request, from: Identifier, to: Identifier, exchange: Exchange) -> Self {
            Self {
                to,
                from,
                entity: request.entity,
                exchange
            }
        }
    }

    impl Into<inlet::exchange::Request> for Request<Entity> {
        fn into(self) -> inlet::exchange::Request {
            inlet::exchange::Request {
                to: vec![self.to],
                entity: self.entity,
                exchange: self.exchange
            }
        }
    }


    #[derive(Clone)]
    pub struct Response {
        pub to: Identifier,
        pub from: Identifier,
        pub exchange: ExchangeId,
        pub entity: response::Entity
    }

    impl Response {
        pub fn from(response: outlet::Response, from: Identifier, to: Identifier) -> Self {
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

    impl Into<outlet::Response> for Response {
        fn into(self) -> outlet::Response {
            outlet::Response {
                from: self.from,
                exchange: self.exchange,
                entity: self.entity
            }
        }
    }
}