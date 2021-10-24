

pub mod inlet {
    use crate::version::latest::operation::Operation;
    use crate::mesh::generic;

    pub type Request = generic::Request<Operation>;
    pub type Response = generic::Response;
}

pub mod outlet {
    use crate::version::latest::operation::ExtOperation;
    use crate::mesh::generic;

    pub type Request = generic::Request<ExtOperation>;
    pub type Response = generic::Response;
}


pub mod generic {
    use anyhow::Error;
    use std::convert::TryInto;

    use crate::version::latest::portal::{inlet, outlet};
    use crate::version::latest::delivery::ResponseEntity;
    use crate::version::latest::id::Identifier;
    use crate::version::latest::messaging::{ExchangeId, Exchange};
    use crate::version::latest::operation::{ExtOperation, Operation};

    #[derive(Clone)]
    pub struct Request<OPERATION> {
        pub to: Identifier,
        pub from: Identifier,
        pub operation: OPERATION,
        pub kind: Exchange,
    }

    impl<OPERATION> Request<OPERATION> {
        pub fn new(to: Identifier, from: Identifier, operation: OPERATION) -> Self {
            Request {
                to,
                from,
                operation,
                kind: Exchange::None
            }
        }
    }

    impl TryInto<Request<ExtOperation>> for Request<Operation> {
        type Error = Error;

        fn try_into(self) -> Result<Request<ExtOperation>, Self::Error> {
            match self.operation {
                Operation::Rc(_) => {
                    Err(anyhow!("cannot turn a ResourceOperation into an ExtOperation"))
                }
                Operation::Msg(ext) => {
                    Ok(Request {
                        to: self.to,
                        from: self.from,
                        operation: ext,
                        kind: self.kind
                    })
                }
            }
        }
    }

    impl Request<Operation> {
        pub fn from(request: inlet::Request, from: Identifier, to: Identifier) -> Self {
            Self {
                to,
                from,
                operation: request.entity,
                kind: request.exchange
            }
        }
    }

    impl Into<inlet::Request> for Request<Operation> {
        fn into(self) -> inlet::Request {
            inlet::Request {
                to: vec![self.to],
                entity: self.operation,
                exchange: self.kind
            }
        }
    }

    impl Into<outlet::Request> for Request<ExtOperation> {
        fn into(self) -> outlet::Request {
            outlet::Request {
                from: self.from,
                entity: self.operation,
                exchange: self.kind
            }
        }
    }

    #[derive(Clone)]
    pub struct Response {
        pub to: Identifier,
        pub from: Identifier,
        pub exchange_id: ExchangeId,
        pub signal: ResponseEntity
    }

    impl Response {
        pub fn from(response: outlet::Response, from: Identifier, to: Identifier) -> Self {
            Self {
                to,
                from,
                exchange_id: response.exchange,
                signal: response.entity
            }
        }
    }

    impl Into<inlet::Response> for Response {
        fn into(self) -> inlet::Response {
            inlet::Response {
                to: self.to,
                exchange: self.exchange_id,
                entity: self.signal
            }
        }
    }

    impl Into<outlet::Response> for Response {
        fn into(self) -> outlet::Response {
            outlet::Response {
                from: self.from,
                exchange: self.exchange_id,
                entity: self.signal
            }
        }
    }
}