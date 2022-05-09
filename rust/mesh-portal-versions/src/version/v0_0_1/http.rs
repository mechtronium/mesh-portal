use crate::version::v0_0_1::entity::entity::request::{Method, RequestCore};
use crate::version::v0_0_1::entity::entity::response::ResponseCore;
use crate::version::v0_0_1::id::id::Meta;
use crate::version::v0_0_1::payload::payload::{Errors, Payload, Primitive};
use http::{HeaderMap, StatusCode, Uri};
use serde::{Deserialize, Serialize};
use crate::error::MsgErr;

pub type HttpMethod = http::Method;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HttpRequest {

    #[serde(with = "http_serde::method")]
    pub method: HttpMethod,

    #[serde(with = "http_serde::header_map")]
    pub headers: HeaderMap,

    #[serde(with = "http_serde::uri")]
    pub uri: Uri,
    pub body: Payload,
}

impl HttpRequest {
    pub fn ok(&self, payload: Payload) -> ResponseCore {
        ResponseCore {
            headers: Default::default(),
            status: StatusCode::from_u16(200u16).unwrap(),
            body: payload,
        }
    }

    pub fn fail(&self, error: &str) -> ResponseCore {
        let errors = Errors::default(error);
        ResponseCore {
            headers: Default::default(),
            status: StatusCode::from_u16(500u16).unwrap(),
            body: Payload::Errors(errors),
        }
    }
}

impl TryFrom<RequestCore> for HttpRequest {
    type Error = MsgErr;

    fn try_from(core: RequestCore) -> Result<Self, Self::Error> {
        if let Method::Http(method) = core.method {
            Ok(Self {
                method,
                headers: core.headers,
                uri: core.uri,
                body: core.body,
            })
        } else {
            Err("expected Http".into())
        }
    }
}
