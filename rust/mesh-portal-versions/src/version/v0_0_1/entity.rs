
    use serde::{Deserialize, Serialize};
    use crate::version::v0_0_1::util::ValueMatcher;

    #[derive(
        Debug, Clone, Serialize, Deserialize, strum_macros::Display, strum_macros::EnumString,Eq,PartialEq
    )]
    pub enum MethodKind {
        Cmd,
        Msg,
        Http,
    }

    impl ValueMatcher<MethodKind> for MethodKind {
        fn is_match(&self, x: &MethodKind) -> Result<(), ()> {
            if self == x {
                Ok(())
            } else {
                Err(())
            }
        }
    }

    pub mod response {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::cmd::request::RequestCore;
        use crate::version::v0_0_1::fail;
        use crate::version::v0_0_1::fail::Fail;
        use crate::version::v0_0_1::id::id::{GenericKind, Meta, Point};
        use crate::version::v0_0_1::messaging::messaging::Response;
        use crate::version::v0_0_1::payload::payload::{Errors, Payload, Primitive};
        use crate::version::v0_0_1::util::uuid;
        use http::response::Parts;
        use http::{HeaderMap, StatusCode};
        use serde::{Deserialize, Serialize};
        use std::sync::Arc;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct ResponseCore {
            #[serde(with = "http_serde::header_map")]
            pub headers: HeaderMap,

            #[serde(with = "http_serde::status_code")]
            pub status: StatusCode,

            pub body: Payload,
        }

        impl ResponseCore {
            pub fn ok_html(html: &str) -> Self {
                let bin = Arc::new(html.to_string().into_bytes());
                ResponseCore::ok(Payload::Bin(bin))
            }

            pub fn new() -> Self {
                ResponseCore {
                    headers: HeaderMap::new(),
                    status: StatusCode::from_u16(200u16).unwrap(),
                    body: Payload::Empty,
                }
            }

            pub fn ok(body: Payload) -> Self {
                Self {
                    headers: HeaderMap::new(),
                    status: StatusCode::from_u16(200u16).unwrap(),
                    body,
                }
            }

            pub fn server_error() -> Self {
                Self {
                    headers: HeaderMap::new(),
                    status: StatusCode::from_u16(500u16).unwrap(),
                    body: Payload::Empty,
                }
            }

            pub fn fail(message: &str) -> Self {
                let errors = Errors::default(message.clone());
                Self {
                    headers: HeaderMap::new(),
                    status: StatusCode::from_u16(500u16).unwrap(),
                    body: Payload::Errors(errors),
                }
            }

            pub fn with_new_payload(self, payload: Payload) -> Self {
                Self {
                    headers: self.headers,
                    status: self.status,
                    body: payload,
                }
            }

            pub fn is_ok(&self) -> bool {
                return self.status.is_success();
            }

            pub fn into_response(self, from: Point, to: Point, response_to: String) -> Response {
                Response {
                    id: uuid(),
                    from,
                    to,
                    core: self,
                    response_to,
                }
            }
        }

        impl TryInto<http::response::Builder> for ResponseCore {
            type Error = MsgErr;

            fn try_into(self) -> Result<http::response::Builder, Self::Error> {
                let mut builder = http::response::Builder::new();

                for (name, value) in self.headers {
                    match name {
                        Some(name) => {
                            builder =
                                builder.header(name.as_str(), value.to_str()?.to_string().as_str());
                        }
                        None => {}
                    }
                }

                Ok(builder.status(self.status))
            }
        }

        impl TryInto<http::Response<Bin>> for ResponseCore {
            type Error = MsgErr;

            fn try_into(self) -> Result<http::Response<Bin>, Self::Error> {
                let mut builder = http::response::Builder::new();

                for (name, value) in self.headers {
                    match name {
                        Some(name) => {
                            builder =
                                builder.header(name.as_str(), value.to_str()?.to_string().as_str());
                        }
                        None => {}
                    }
                }

                let response = builder.status(self.status).body(self.body.to_bin()?)?;
                Ok(response)
            }
        }
    }

