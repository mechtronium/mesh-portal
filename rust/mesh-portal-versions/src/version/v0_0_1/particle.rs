pub mod particle {
    use std::collections::HashMap;
    use std::str::FromStr;

    use nom::branch::alt;
    use nom::bytes::complete::{is_a, tag};
    use nom::character::complete::{alpha1, digit1};
    use nom::combinator::{not, recognize};
    use nom::error::{ErrorKind, ParseError, VerboseError};
    use nom::sequence::{delimited, tuple};
    use nom::CompareResult::Incomplete;
    use nom::Parser;
    use nom_supreme::error::ErrorTree;
    use nom_supreme::{parse_from_str, ParserExt};
    use serde::{Deserialize, Serialize};

    use crate::error::MsgErr;
    use crate::version::v0_0_1::id::id::{GenericKind, GenericKindBase, Point, PointKind};
    use crate::version::v0_0_1::parse::{Res};
    use crate::version::v0_0_1::payload::payload::{Payload, PayloadMap};
    use crate::version::v0_0_1::parse::parse_alpha1_str;
    use crate::version::v0_0_1::span::OwnedSpan;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct StatusUpdate {
        pub from: Point,
        pub status: Status,
    }

    #[derive(
        Debug,
        Clone,
        Serialize,
        Deserialize,
        Eq,
        PartialEq,
        strum_macros::Display,
        strum_macros::EnumString,
    )]
    pub enum Status {
        Unknown,      // initial status or when we status cannot be determined
        Pending,      // resource is now registered but not assigned to a host
        Assigning,    // resource is being assigned to at least one host
        Initializing, // assigned to a host and undergoing custom initialization...This resource can send requests but not receive requests.
        Ready,        // ready to take requests
        Paused, // can not receive requests (probably because it is waiting for some other resource to make updates)...
        Resuming, // like Initializing but triggered after a pause is lifted, the resource may be doing something before it is ready to accept requests again.
        Panic,    // something is wrong... all requests are blocked and responses are cancelled.
        Done, // this resource had a life span and has now completed succesfully it can no longer receive requests.
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum Code {
        Ok,
        Error(i32),
    }

    impl ToString for Code {
        fn to_string(&self) -> String {
            match self {
                Code::Ok => "Ok".to_string(),
                Code::Error(code) => {
                    format!("Err({})", code)
                }
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Progress {
        pub step: u16,
        pub total: u16,
    }

    impl ToString for Progress {
        fn to_string(&self) -> String {
            format!("{}/{}", self.step, self.total)
        }
    }

    pub fn ok_code(input: OwnedSpan) -> Res<OwnedSpan, Code> {
        tag("Ok")(input).map(|(next, code)| (next, Code::Ok))
    }

    pub fn error_code(input: OwnedSpan) -> Res<OwnedSpan, Code> {
        let (next, err_code) = delimited(tag("Err("), digit1, tag(")"))(input.clone())?;
        Ok((
            next,
            Code::Error(match err_code.parse() {
                Ok(i) => i,
                Err(err) => {
                    return Err(nom::Err::Error(ErrorTree::from_error_kind(
                        input,
                        ErrorKind::Tag,
                    )))
                }
            }),
        ))
    }

    pub fn code(input: OwnedSpan) -> Res<OwnedSpan, Code> {
        alt((error_code, ok_code))(input)
    }

    pub fn status(input: OwnedSpan) -> Res<OwnedSpan, Status> {
        parse_alpha1_str(input)
    }

    pub type Properties = HashMap<String, Property>;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Property {
        pub key: String,
        pub value: String,
        pub locked: bool,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Archetype {
        pub kind: GenericKind,
        pub properties: Properties,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Stub {
        pub point: Point,
        pub kind: GenericKind,
        pub properties: Properties,
        pub status: Status,
    }

    impl Stub {
        pub fn point_and_kind(self) -> PointKind {
            PointKind {
                point: self.point,
                kind: self.kind,
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Particle {
        pub stub: Stub,
        pub state: Box<Payload>,
    }
}
