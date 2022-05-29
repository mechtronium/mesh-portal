use std::ops::Deref;
use crate::error::MsgErr;
use crate::version::v0_0_1::command::request::{Method, RequestCore};
use crate::version::v0_0_1::entity::response::ResponseCore;
use crate::version::v0_0_1::id::id::Meta;
use crate::version::v0_0_1::payload::payload::{Errors, Payload, Primitive};
use http::{HeaderMap, StatusCode, Uri};
use nom::combinator::all_consuming;
use serde::{Deserialize, Serialize};
use crate::version::v0_0_1::parse::camel_case;
use crate::version::v0_0_1::parse::error::result;
use crate::version::v0_0_1::span::new_span;

#[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq,Hash)]
pub struct MsgMethod {
    string: String
}

impl MsgMethod {
    pub fn new<S:ToString>( string: S) -> Result<Self,MsgErr> {
        let tmp = string.to_string();
        let string = result(all_consuming(camel_case)(new_span(tmp.as_str())))?.to_string();
        Ok(Self {
            string
        })
    }
}

impl TryFrom<String> for MsgMethod {
    type Error = MsgErr;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl TryFrom<&str> for MsgMethod {
    type Error = MsgErr;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}


impl Deref for MsgMethod {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl Default for MsgMethod {
    fn default() -> Self {
        Self {
            string: "Def".to_string()
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MsgRequest {
    pub method: MsgMethod,

    #[serde(with = "http_serde::header_map")]
    pub headers: HeaderMap,

    #[serde(with = "http_serde::uri")]
    pub uri: Uri,
    pub body: Payload,
}

impl Default for MsgRequest {
    fn default() -> Self {
        Self {
            method: Default::default(),
            headers: Default::default(),
            uri: Default::default(),
            body: Default::default()
        }
    }
}

impl MsgRequest {

    pub fn new<M>(method: M) -> Result<Self,MsgErr> where M: TryInto<MsgMethod,Error=MsgErr>{
        Ok(MsgRequest {
            method: method.try_into()?,
            ..Default::default()
        })
    }

    pub fn with_body(mut self, body:Payload) -> Self {
        self.body = body;
        self
    }

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

impl TryFrom<RequestCore> for MsgRequest {
    type Error = MsgErr;

    fn try_from(core: RequestCore) -> Result<Self, Self::Error> {
        if let Method::Msg(action) = core.method {
            Ok(Self {
                method: action,
                headers: core.headers,
                uri: core.uri,
                body: core.body,
            })
        } else {
            Err("expected Msg".into())
        }
    }
}
