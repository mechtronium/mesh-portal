use alloc::string::String;
use chrono::{DateTime, Utc};
use nom_locate::LocatedSpan;

use nom::lib::std::collections::HashMap;
use nom::{Offset, Slice};

pub mod command;
pub mod config;
pub mod entity;
pub mod frame;
pub mod http;
pub mod id;
pub mod log;
pub mod messaging;
pub mod msg;
pub mod parse;
pub mod particle;
pub mod payload;
pub mod portal;
pub mod security;
pub mod selector;
pub mod span;
pub mod wrap;

use serde::{Deserialize, Serialize};

use crate::error::MsgErr;
use crate::version::v0_0_1::bin::Bin;

pub type State = HashMap<String, Bin>;

extern "C" {
    pub fn mesh_portal_uuid() -> String;
    pub fn mesh_portal_timestamp() -> DateTime<Utc>;
}

pub mod artifact {
    use crate::version::v0_0_1::bin::Bin;
    use crate::version::v0_0_1::id::id::Point;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Artifact {
        pub point: Point,
        pub bin: Bin,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ArtifactRequest {
        pub point: Point,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ArtifactResponse {
        pub to: Point,
        pub payload: Bin,
    }
}

pub type Port = String;

pub mod path {
    use crate::error::MsgErr;
    use crate::version::v0_0_1::parse::consume_path;
    use crate::version::v0_0_1::span::new_span;
    use alloc::format;
    use alloc::string::{String, ToString};
    use alloc::vec::Vec;
    use serde::{Deserialize, Serialize};
    use std::str::FromStr;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct Path {
        string: String,
    }

    impl Path {
        fn new(string: &str) -> Self {
            Path {
                string: string.to_string(),
            }
        }

        pub fn make_absolute(string: &str) -> Result<Self, MsgErr> {
            if string.starts_with("/") {
                Path::from_str(string)
            } else {
                Path::from_str(format!("/{}", string).as_str())
            }
        }

        pub fn bin(&self) -> Result<Vec<u8>, MsgErr> {
            let bin = bincode::serialize(self)?;
            Ok(bin)
        }

        pub fn is_absolute(&self) -> bool {
            self.string.starts_with("/")
        }

        pub fn cat(&self, path: &Path) -> Result<Self, MsgErr> {
            if self.string.ends_with("/") {
                Path::from_str(format!("{}{}", self.string.as_str(), path.string.as_str()).as_str())
            } else {
                Path::from_str(
                    format!("{}/{}", self.string.as_str(), path.string.as_str()).as_str(),
                )
            }
        }

        pub fn parent(&self) -> Option<Path> {
            let s = self.to_string();
            let parent = std::path::Path::new(s.as_str()).parent();
            match parent {
                None => Option::None,
                Some(path) => match path.to_str() {
                    None => Option::None,
                    Some(some) => match Self::from_str(some) {
                        Ok(parent) => Option::Some(parent),
                        Err(error) => {
                            eprintln!("{}", error.to_string());
                            Option::None
                        }
                    },
                },
            }
        }

        pub fn last_segment(&self) -> Option<String> {
            let split = self.string.split("/");
            match split.last() {
                None => Option::None,
                Some(last) => Option::Some(last.to_string()),
            }
        }

        pub fn to_relative(&self) -> String {
            let mut rtn = self.string.clone();
            rtn.remove(0);
            rtn
        }
    }

    impl FromStr for Path {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_, path) = consume_path(new_span(s))?;
            Ok(Self {
                string: path.to_string(),
            })
        }
    }

    impl ToString for Path {
        fn to_string(&self) -> String {
            self.string.clone()
        }
    }
}

pub mod bin {
    use std::collections::HashMap;
    use std::sync::Arc;

    use serde::{Deserialize, Serialize};

    pub type Bin = Arc<Vec<u8>>;
}

pub mod util {
    use alloc::format;
    use alloc::string::{String, ToString};
    use chrono::{DateTime, Utc};
    use serde::{Deserialize, Serialize};

    use crate::error::MsgErr;
    use crate::version::v0_0_1::http::HttpMethod;
    use crate::version::v0_0_1::{mesh_portal_timestamp, mesh_portal_uuid };

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum MethodPattern {
        Any,
        None,
        Pattern(#[serde(with = "http_serde::method")] HttpMethod),
    }

    impl MethodPattern {
        pub fn is_match(&self, x: &HttpMethod) -> Result<(), ()> {
            match self {
                Self::Any => Ok(()),
                Self::Pattern(exact) => exact.is_match(x),
                Self::None => Err(()),
            }
        }

        pub fn is_match_opt(&self, x: Option<&HttpMethod>) -> Result<(), ()> {
            match self {
                Self::Any => Ok(()),
                Self::Pattern(exact) => match x {
                    None => Err(()),
                    Some(x) => self.is_match(x),
                },
                Self::None => Err(()),
            }
        }
    }

    impl ToString for MethodPattern {
        fn to_string(&self) -> String {
            match self {
                Self::Any => "*".to_string(),
                Self::None => "!".to_string(),
                Self::Pattern(pattern) => pattern.to_string(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum ValuePattern<T> {
        Any,
        None,
        Pattern(T),
    }

    impl<T> ValuePattern<T>
    where
        T: ToString,
    {
        pub fn stringify(self) -> ValuePattern<String> {
            match self {
                ValuePattern::Any => ValuePattern::Any,
                ValuePattern::None => ValuePattern::None,
                ValuePattern::Pattern(t) => ValuePattern::Pattern(t.to_string()),
            }
        }
    }

    impl<T> ValuePattern<T> {
        pub fn modify<X, F>(self, mut f: F) -> Result<ValuePattern<X>, MsgErr>
        where
            F: FnMut(T) -> Result<X, MsgErr>,
        {
            Ok(match self {
                ValuePattern::Any => ValuePattern::Any,
                ValuePattern::None => ValuePattern::None,
                ValuePattern::Pattern(from) => ValuePattern::Pattern(f(from)?),
            })
        }

        pub fn wrap<X>(self, x: X) -> ValuePattern<X> {
            match self {
                ValuePattern::Any => ValuePattern::Any,
                ValuePattern::None => ValuePattern::None,
                ValuePattern::Pattern(_) => ValuePattern::Pattern(x),
            }
        }

        pub fn is_match<X>(&self, x: &X) -> Result<(), ()>
        where
            T: ValueMatcher<X>,
        {
            match self {
                ValuePattern::Any => Ok(()),
                ValuePattern::Pattern(exact) => exact.is_match(x),
                ValuePattern::None => Err(()),
            }
        }

        pub fn is_match_opt<X>(&self, x: Option<&X>) -> Result<(), ()>
        where
            T: ValueMatcher<X>,
        {
            match self {
                ValuePattern::Any => Ok(()),
                ValuePattern::Pattern(exact) => match x {
                    None => Err(()),
                    Some(x) => self.is_match(x),
                },
                ValuePattern::None => Err(()),
            }
        }
    }

    impl<V: ToString> ToString for ValuePattern<V> {
        fn to_string(&self) -> String {
            match self {
                ValuePattern::Any => "*".to_string(),
                ValuePattern::None => "!".to_string(),
                ValuePattern::Pattern(pattern) => pattern.to_string(),
            }
        }
    }

    pub trait ValueMatcher<X> {
        fn is_match(&self, x: &X) -> Result<(), ()>;
    }

    pub struct RegexMatcher {
        pub pattern: String,
    }

    impl ToString for RegexMatcher {
        fn to_string(&self) -> String {
            self.pattern.clone()
        }
    }

    impl RegexMatcher {
        pub fn new(string: String) -> Self {
            Self { pattern: string }
        }
    }

    impl ValueMatcher<String> for RegexMatcher {
        fn is_match(&self, x: &String) -> Result<(), ()> {
            let matches = x.matches(x);
            if matches.count() > 0 {
                Ok(())
            } else {
                Err(())
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct StringMatcher {
        pub pattern: String,
    }

    impl ToString for StringMatcher {
        fn to_string(&self) -> String {
            self.pattern.clone()
        }
    }

    impl StringMatcher {
        pub fn new(string: String) -> Self {
            Self { pattern: string }
        }
    }

    impl ValueMatcher<String> for StringMatcher {
        fn is_match(&self, x: &String) -> Result<(), ()> {
            if self.pattern == *x {
                Ok(())
            } else {
                Err(())
            }
        }
    }

    pub trait Convert<A> {
        fn convert(self) -> Result<A, MsgErr>;
    }

    pub trait ConvertFrom<A>
    where
        Self: Sized,
    {
        fn convert_from(a: A) -> Result<Self, MsgErr>;
    }

    /*
    impl<A: Convert<B>, B> ConvertFrom<B> for A where A: Sized {
        fn convert_from(a: A) -> Result<Self, Error> {
            a.convert()
        }
    }
     */

    pub fn uuid() -> String {
        unsafe { mesh_portal_uuid() }
    }

    pub fn timestamp() -> DateTime<Utc>{
        unsafe { mesh_portal_timestamp() }
    }
}

pub mod fail {
    use alloc::string::String;
    use serde::{Deserialize, Serialize};

    use crate::error::MsgErr;
    use crate::version::v0_0_1::id::id::Specific;

    pub mod mesh {
        use alloc::string::String;
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Error(String),
        }
    }

    pub mod portal {
        use alloc::string::String;
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::fail::{http, msg, resource};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Error(String),
            Resource(resource::Fail),
            Msg(msg::Fail),
            Http(http::Error),
        }
    }

    pub mod http {
        use alloc::string::String;
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Error {
            pub message: String,
        }
    }

    pub mod resource {
        use alloc::string::String;
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::fail::{
            Bad, BadCoercion, BadRequest, Conditional, Messaging, NotFound,
        };
        use crate::version::v0_0_1::id::id::Point;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Create(Create),
            Update(Update),
            Select(Select),
            BadRequest(BadRequest),
            Conditional(Conditional),
            Messaging(Messaging),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Create {
            AddressAlreadyInUse(String),
            WrongParentResourceType { expected: String, found: String },
            CannotUpdateArchetype,
            InvalidProperty { expected: String, found: String },
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Update {
            Immutable,
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Select {
            WrongAddress { required: Point, found: Point },
            BadSelectRouting { required: String, found: String },
            BadCoercion(BadCoercion),
        }
    }

    pub mod msg {
        use alloc::string::String;
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::fail::{BadRequest, Conditional};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Error(String),
            BadRequest(BadRequest),
            Conditional(Conditional),
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum BadRequest {
        NotFound(NotFound),
        Bad(Bad),
        Illegal(Illegal),
        Wrong(Wrong),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BadCoercion {
        pub from: String,
        pub into: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Conditional {
        Timeout(Timeout),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Timeout {
        pub waited: i32,
        pub message: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum NotFound {
        Any,
        ResourceType(String),
        Kind(String),
        Specific(String),
        Address(String),
        Key(String),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Bad {
        ResourceType(String),
        Kind(String),
        Specific(String),
        Address(String),
        Key(String),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Identifier {
        ResourceType,
        Kind,
        Specific,
        Address,
        Key,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Illegal {
        Immutable,
        EmptyToFieldOnMessage,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Wrong {
        pub received: String,
        pub expected: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Messaging {
        RequestReplyExchangesRequireOneAndOnlyOneRecipient,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Fail {
        Mesh(mesh::Fail),
        Resource(resource::Fail),
        Portal(portal::Fail),
        Error(String),
    }

    impl ToString for Fail {
        fn to_string(&self) -> String {
            "Fail".to_string()
        }
    }

    /*    impl Into<MsgErr> for Fail {
           fn into(self) -> MsgErr {
               MsgErr {
                   status: 500,
                   message: "Fail".to_string(),
               }
           }
       }

    */
}
