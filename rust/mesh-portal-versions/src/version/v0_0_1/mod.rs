use nom_locate::LocatedSpan;
use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;
pub mod log;
pub mod messaging;
pub mod security;
pub mod config;
pub mod parse;
mod id;
mod selector;
mod frame;
mod payload;
mod command;
mod msg;
mod entity;
mod particle;


use serde::{Deserialize, Serialize};

use crate::error::MsgErr;
use crate::version::v0_0_1::bin::Bin;

pub type State = HashMap<String, Bin>;
pub type Span<'a> = LocatedSpan<&'a str, SpanExtra>;
pub type SpanExtra = Rc<String>;

pub fn span(s: &str) -> Span {
    Span::new_extra(s, Rc::new(s.to_string()))
}

extern "C" {
    pub fn mesh_portal_unique_id() -> String;
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
    use crate::version::v0_0_1::{span, Span};
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
            let (_, path) = consume_path(span(s))?;
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

pub mod portal {
    use crate::version::v0_0_1::util::unique_id;
    use serde::{Deserialize, Serialize};
    use std::ops::Deref;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Exchanger<T> {
        pub id: String,
        pub item: T,
    }

    impl<T> Exchanger<T> {
        pub fn new(item: T) -> Self {
            Exchanger {
                id: unique_id(),
                item,
            }
        }

        pub fn with<X>(self, item: X) -> Exchanger<X> {
            Exchanger { id: self.id, item }
        }
    }

    impl<T> Deref for Exchanger<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.item
        }
    }

    pub mod inlet {
        use std::convert::TryFrom;
        use std::ops::Deref;

        use crate::error::MsgErr;
        use crate::version::v0_0_1::artifact::ArtifactRequest;
        use crate::version::v0_0_1::frame::frame::{CloseReason, PrimitiveFrame};
        use crate::version::v0_0_1::id::id::{GenericKind, GenericKindBase, Point};
        use crate::version::v0_0_1::messaging::messaging::{Request, Response};
        use crate::version::v0_0_1::particle::particle::StatusUpdate;
        use crate::version::v0_0_1::payload::payload::Payload;
        use crate::version::v0_0_1::portal;
        use crate::version::v0_0_1::portal::Exchanger;
        use crate::version::v0_0_1::selector::selector::TksPattern;
        use crate::version::v0_0_1::util::unique_id;
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Log {
            pub src: String,
            pub message: String,
        }

        impl Log {
            pub fn new(src: &str, message: &str) -> Self {
                Self {
                    src: src.to_string(),
                    message: message.to_string(),
                }
            }
        }

        impl ToString for Log {
            fn to_string(&self) -> String {
                format!("{}: {}", self.src, self.message)
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum Frame {
            Log(Log),
            AssignRequest(Exchanger<AssignRequest>),
            Request(Request),
            Response(Response),
            Artifact(Exchanger<ArtifactRequest>), // portal inlet will cache and return artifact
            Status(StatusUpdate),
            Close(CloseReason),
        }

        impl Frame {
            pub fn from(&self) -> Option<Point> {
                match self {
                    Frame::Log(_) => Option::None,
                    Frame::Request(request) => Option::Some(request.from.clone()),
                    Frame::Response(response) => {
                        // Response will need a from field for it to work within Ports
                        Option::None
                    }
                    Frame::Artifact(artifact) => Option::None,
                    Frame::Status(status) => Option::Some(status.from.clone()),
                    Frame::Close(_) => Option::None,
                    Frame::AssignRequest(_) => Option::None,
                }
            }
        }

        #[derive(
            Debug, Clone, Serialize, Deserialize, strum_macros::Display, strum_macros::EnumString,
        )]
        pub enum AssignRequest {
            Control,
        }

        impl TryInto<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                Ok(PrimitiveFrame {
                    data: bincode::serialize(&self)?,
                })
            }
        }

        impl TryFrom<PrimitiveFrame> for portal::inlet::Frame {
            type Error = MsgErr;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }

    pub mod outlet {
        use std::convert::TryFrom;

        use crate::error::MsgErr;
        use crate::version::v0_0_1::artifact::{Artifact, ArtifactResponse};
        use crate::version::v0_0_1::config::config::{Assign, Config, ConfigBody};
        use crate::version::v0_0_1::frame::frame::{CloseReason, PrimitiveFrame};
        use crate::version::v0_0_1::id::id::{GenericKind, GenericKindBase, Point};
        use crate::version::v0_0_1::messaging::messaging::{Request, Response};
        use crate::version::v0_0_1::payload::payload::Payload;
        use crate::version::v0_0_1::portal::Exchanger;
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum Frame {
            Init,
            Assign(Exchanger<Assign>),
            Request(Request),
            Response(Response),
            Artifact(Exchanger<ArtifactResponse>),
            Close(CloseReason),
        }

        impl Frame {
            pub fn to(&self) -> Option<Point> {
                match self {
                    Frame::Assign(assign) => Option::Some(assign.stub.point.clone()),
                    Frame::Request(request) => Option::Some(request.to.clone()),
                    Frame::Response(response) => Option::Some(response.to.clone()),
                    Frame::Artifact(artifact) => Option::Some(artifact.to.clone()),
                    Frame::Close(_) => Option::None,
                    Frame::Init => Option::None,
                }
            }
        }

        impl TryInto<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                Ok(PrimitiveFrame {
                    data: bincode::serialize(&self)?,
                })
            }
        }

        impl TryFrom<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }

    pub mod initin {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::artifact::{Artifact, ArtifactRequest, ArtifactResponse};
        use crate::version::v0_0_1::frame::frame::PrimitiveFrame;
        use crate::version::v0_0_1::portal::Exchanger;
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct PortalAuth {
            pub user: String,
            pub portal_key: Option<String>,
        }

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum Frame {
            Flavor(String),
            Auth(PortalAuth),
            Artifact(Exchanger<ArtifactRequest>),
            Ok,
            Ready,
        }

        impl TryInto<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                Ok(PrimitiveFrame {
                    data: bincode::serialize(&self)?,
                })
            }
        }

        impl TryFrom<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }
    pub mod initout {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::artifact::{Artifact, ArtifactResponse};
        use crate::version::v0_0_1::frame::frame::PrimitiveFrame;
        use crate::version::v0_0_1::portal::Exchanger;
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum Frame {
            Ok,
            Artifact(Exchanger<ArtifactResponse>),
        }
        impl TryInto<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                Ok(PrimitiveFrame {
                    data: bincode::serialize(&self)?,
                })
            }
        }

        impl TryFrom<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }
}

pub mod util {
    use serde::{Deserialize, Serialize};

    use crate::error::MsgErr;
    use crate::version::v0_0_1::mesh_portal_unique_id;
    use crate::version::v0_0_1::payload::payload::HttpMethod;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum MethodPattern {
        Any,
        None,
        Pattern(#[serde(with = "http_serde::method")] HttpMethod),
    }

    impl MethodPattern {
        pub fn is_match(&self, x: &HttpMethod) -> Result<(), MsgErr> {
            match self {
                Self::Any => Ok(()),
                Self::Pattern(exact) => exact.is_match(x),
                Self::None => Err("None pattern".into()),
            }
        }

        pub fn is_match_opt(&self, x: Option<&HttpMethod>) -> Result<(), MsgErr> {
            match self {
                Self::Any => Ok(()),
                Self::Pattern(exact) => match x {
                    None => Err("option none".into()),
                    Some(x) => self.is_match(x),
                },
                Self::None => Err("None pattern".into()),
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

    impl<T> ValuePattern<T> {
        pub fn is_match<X>(&self, x: &X) -> Result<(), MsgErr>
        where
            T: ValueMatcher<X>,
        {
            match self {
                ValuePattern::Any => Ok(()),
                ValuePattern::Pattern(exact) => exact.is_match(x),
                ValuePattern::None => Err("None pattern".into()),
            }
        }

        pub fn is_match_opt<X>(&self, x: Option<&X>) -> Result<(), MsgErr>
        where
            T: ValueMatcher<X>,
        {
            match self {
                ValuePattern::Any => Ok(()),
                ValuePattern::Pattern(exact) => match x {
                    None => Err("option none".into()),
                    Some(x) => self.is_match(x),
                },
                ValuePattern::None => Err("None pattern".into()),
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
        fn is_match(&self, x: &X) -> Result<(), MsgErr>;
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
        fn is_match(&self, x: &String) -> Result<(), MsgErr> {
            let matches = x.matches(x);
            if matches.count() > 0 {
                Ok(())
            } else {
                Err(format!("could not match pattern '{}' in '{}'", self.pattern, x).into())
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
        fn is_match(&self, x: &String) -> Result<(), MsgErr> {
            if self.pattern == *x {
                Ok(())
            } else {
                Err(format!("expecting pattern: '{}' found: '{}'", self.pattern, x).into())
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

    pub fn unique_id() -> String {
        //        Uuid::new_v4().to_string()
        unsafe { mesh_portal_unique_id() }
    }
}

pub mod fail {
    use serde::{Deserialize, Serialize};

    use crate::error::MsgErr;
    use crate::version::v0_0_1::id::id::Specific;

    pub mod mesh {
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Error(String),
        }
    }

    pub mod portal {
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
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Error {
            pub message: String,
        }
    }

    pub mod resource {
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





#[cfg(test)]
pub mod test {
    use http::Uri;
    use std::collections::HashMap;
    use std::str::FromStr;

    use nom::combinator::{all_consuming, recognize};

    use crate::error::MsgErr;
    use crate::version::v0_0_1::config::config::bind::parse::{
        bind, final_bind, http_pipeline, pipeline, pipeline_step, pipeline_stop,
        select_http_pipelines,
    };
    use crate::version::v0_0_1::config::config::bind::{PipelinesSubScope, ProtoBind};
    use crate::version::v0_0_1::entity::entity::request::{Action, RequestCore};
    use crate::version::v0_0_1::id::id::{Point, PointSeg, PointSubst, RouteSeg};
    use crate::version::v0_0_1::parse::error::{find, result};
    use crate::version::v0_0_1::parse::{access_grant, access_grant_kind, base_point_segment, camel_case, capture_point, child_perms, consume_point, consume_point_subst, create, file_point_capture_segment, MapResolver, particle_perms, permissions, permissions_mask, point, point_route_segment, point_subst, point_template, publish, rec_skewer, Res, skewer_chars, ToResolved, var, var_subst, version_point_segment};
    use crate::version::v0_0_1::payload::payload::{HttpMethod, Payload};
    use crate::version::v0_0_1::security::{
        ChildPerms, ParticlePerms, Permissions, PermissionsMask, PermissionsMaskKind,
    };
    use crate::version::v0_0_1::selector::selector::parse::point_selector;
    use crate::version::v0_0_1::selector::selector::parse::version;
    use crate::version::v0_0_1::selector::selector::{
        http_method, http_method_pattern, http_pattern, http_pattern_scoped, PointKindHierarchy,
        PointSelector, upload_step,
    };
    use crate::version::v0_0_1::util::ValueMatcher;
    use crate::version::v0_0_1::{span, Span};
    use nom::error::VerboseError;
    use nom::{Err, Offset};
    use nom_supreme::error::{BaseErrorKind, ErrorTree, StackContext};
    use nom_supreme::final_parser::ExtractContext;
    use regex::Regex;

    #[test]
    pub fn test_point_kind_pattern_matching() -> Result<(), MsgErr> {
        let pattern = PointSelector::from_str("**")?;
        let point = PointKindHierarchy::from_str("localhost<Space>:mechtron<Mechtron>")?;
        assert!(pattern.matches(&point));
        Ok(())
    }

    #[test]
    pub fn test_query_root() -> Result<(), MsgErr> {
        let inclusive_pattern = PointSelector::from_str("localhost+:**")?;
        let exclusive_pattern = PointSelector::from_str("localhost:**")?;
        println!(
            "inclusive: '{}'",
            inclusive_pattern.query_root().to_string()
        );
        println!(
            "exclusive : '{}'",
            exclusive_pattern.query_root().to_string()
        );
        Ok(())
    }

    #[test]
    pub fn test_inclusive() -> Result<(), MsgErr> {
        // if a hop is 'inclusive' then this will match to true.  We do this for cases like:
        // localhost+:**   // Here we want everything under localhost INCLUDING localhost to be matched
        let inclusive_pattern = PointSelector::from_str("localhost+:**")?;
        let exclusive_pattern = PointSelector::from_str("localhost:**")?;
        let point1 = PointKindHierarchy::from_str("localhost<Space>")?;
        let point2 = PointKindHierarchy::from_str("localhost<Space>:mechtron<Mechtron>")?;

        assert!(!exclusive_pattern.matches(&point1));
        assert!(exclusive_pattern.matches(&point2));
        assert!(inclusive_pattern.matches(&point1));
        assert!(inclusive_pattern.matches(&point2));
        Ok(())
    }

    #[test]
    pub fn test_inclusive2() -> Result<(), MsgErr> {
        let inclusive_pattern = PointSelector::from_str("localhost+:app+:**")?;
        let exclusive_pattern = PointSelector::from_str("localhost:app:**")?;
        let point1 = PointKindHierarchy::from_str("localhost<Space>")?;
        let point2 = PointKindHierarchy::from_str("localhost<Space>:app<App>")?;
        let point3 = PointKindHierarchy::from_str("localhost<Space>:app<App>:mechtron<Mechtron>")?;

        assert!(!exclusive_pattern.matches(&point1));
        assert!(!exclusive_pattern.matches(&point2));
        assert!(exclusive_pattern.matches(&point3));
        assert!(inclusive_pattern.matches(&point1));
        assert!(inclusive_pattern.matches(&point2));
        assert!(inclusive_pattern.matches(&point3));
        Ok(())
    }

    #[test]
    pub fn test_some_matches() -> Result<(), MsgErr> {
        let users = PointSelector::from_str("**<User>")?;
        let point1 = PointKindHierarchy::from_str("localhost<Space>")?;
        let point2 = PointKindHierarchy::from_str("localhost<Space>:app<App>")?;
        let point3 = PointKindHierarchy::from_str("localhost<Space>:app<App>:mechtron<Mechtron>")?;
        let point4 = PointKindHierarchy::from_str("localhost<Space>:app<App>:users<UserBase>")?;
        let point5 =
            PointKindHierarchy::from_str("localhost<Space>:app<App>:users<UserBase>:scott<User>")?;
        let point6 = PointKindHierarchy::from_str("localhost<Space>:users<UserBase>")?;
        let point7 =
            PointKindHierarchy::from_str("localhost<Space>:users<UserBase>:superuser<User>")?;

        assert!(!users.matches(&point1));
        assert!(!users.matches(&point2));
        assert!(!users.matches(&point3));
        assert!(!users.matches(&point4));
        assert!(users.matches(&point5));
        assert!(!users.matches(&point6));
        assert!(users.matches(&point7));
        Ok(())
    }

    #[test]
    pub fn test_a_match() -> Result<(), MsgErr> {
        let pattern = PointSelector::from_str("localhost:app+:**")?;
        let point = PointKindHierarchy::from_str("localhost<Space>:app<App>")?;

        assert!(pattern.matches(&point));
        Ok(())
    }

    #[test]
    pub fn test_skewer_chars() -> Result<(), MsgErr> {
        match all_consuming(rec_skewer)(span("317")) {
            Ok(ok) => {
                return Err("should not have parsed 317".into());
            }
            Err(_) => {}
        }
        /*
        assert_eq!( rec_skewer("hello1"), Ok(("","hello1")) );
        assert_eq!( all_consuming(rec_skewer)("hello-kitty"), Ok(("","hello-kitty")) );
        assert_eq!( all_consuming(rec_skewer)("hello-kitty123"), Ok(("","hello-kitty123")) );
        assert_eq!( rec_skewer("hello-kitty.1.2.3"), Ok((".1.2.3","hello-kitty")) );
        assert_eq!( rec_skewer("skewer-takes-no-Caps"), Ok(("Caps","skewer-takes-no-")) );
         */
        Ok(())
    }

    #[test]
    pub fn test_point() -> Result<(), MsgErr> {
        assert_eq!(
            (span(""), RouteSeg::Local),
            all_consuming(point_route_segment)(span(""))?
        );

        all_consuming(point)(span("[root]"))?;
        all_consuming(point)(span("hello:kitty"))?;
        all_consuming(point)(span("hello.com:kitty"))?;
        all_consuming(point)(span("hello:kitty:/file.txt"))?;
        all_consuming(point)(span("hello.com:kitty:/file.txt"))?;
        all_consuming(point)(span("hello.com:kitty:/"))?;
        //all_consuming(point)("hello.com:kitty:/greater-glory/file.txt")?;
        all_consuming(point)(span("hello.com:kitty:base"))?;

        all_consuming(version)(span("1.0.0"))?;
        let (next, version) = all_consuming(version_point_segment)(span(":1.2.3"))?;
        println!("next: '{}' segment: '{}'", next, version.to_string());
        all_consuming(point)(span("hello.com:bundle:1.2.3"))?;
        let (next, addy) = all_consuming(point)(span("hello.com:bundle:1.2.3:/"))?;
        println!("{}", addy.last_segment().unwrap().to_string());
        let (next, addy) = all_consuming(point)(span("hello.com:bundle:1.2.3"))?;
        //       let (next, addy) = all_consuming(point)("hello.com:bundle:1.2.3:/some/file.txt")?;
        let (next, addy) =
            all_consuming(point)(span("hello.com:bundle:1.2.3:/greater-glory/file.txt"))?;
        println!("{}", addy.to_string());
        println!("{}", addy.parent().unwrap().to_string());
        println!("{}", addy.last_segment().unwrap().to_string());

        Ok(())
    }

    #[test]
    pub fn test_point_template() -> Result<(), MsgErr> {
        all_consuming(point_template)(span("hello:kitty"))?;
        all_consuming(point_template)(span("hello:kitty-%"))?;
        all_consuming(point_template)(span("hello:kitty:bob:/some-%-time"))?;
        Ok(())
    }

    #[test]
    pub fn test_create() -> Result<(), MsgErr> {
        all_consuming(create)(span("hello:kitty<App>"))?;
        all_consuming(create)(span(
            "hello:kitty<App>{ +config='some:config:1.0.0:/blah.conf' }",
        ))?;
        Ok(())
    }

    #[test]
    pub fn test_publish() -> Result<(), MsgErr> {
        let (_, block) = all_consuming(upload_step)(span("^[ bundle.zip ]->"))?;
        assert_eq!("bundle.zip", block.name.as_str());
        all_consuming(publish)(span("^[ bundle.zip ]-> space.org:hello:1.0.0"))?;
        Ok(())
    }

    #[test]
    pub fn test_pipeline_stop() -> Result<(), MsgErr> {
        pipeline_stop(span("apps:my-app^Http<Get>/*"))?;
        Ok(())
    }

    #[test]
    pub fn test_pipeline() -> Result<(), MsgErr> {
        pipeline(span("-> apps:my-app^Http<Get>/users/$1 => &"))?;
        pipeline(span("-> apps:bundle:1.0.0:/html/$1 => &"))?;
        Ok(())
    }

    #[test]
    pub fn test_capture_point() -> Result<(), MsgErr> {
        file_point_capture_segment(span("$1"))?;

        let point = all_consuming(capture_point)(span("apps:bundle:1.0.0:/html/$1"))?.1;
        let regex = Regex::new("/(.*)")?;
        let point = point.to_point(regex.captures("/index.html").unwrap())?;
        Ok(())
    }

    #[test]
    pub fn test_point_kind_pattern() -> Result<(), MsgErr> {
        all_consuming(point_selector)(span("*"))?;
        all_consuming(point_selector)(span("space"))?;
        all_consuming(point_selector)(span("space:base"))?;
        all_consuming(point_selector)(span("space:my-files:/"))?;
        all_consuming(point_selector)(span("space:my-files:/file.txt"))?;
        all_consuming(point_selector)(span("space:my-files:/dir/file.txt"))?;
        all_consuming(point_selector)(span("space<Space>:base"))?;
        all_consuming(point_selector)(span("**:*<Base>"))?;
        all_consuming(point_selector)(span("space<Space>:base<Base>"))?;
        all_consuming(point_selector)(span("space:base:blah"))?;
        all_consuming(point_selector)(span("space:base:*"))?;
        all_consuming(point_selector)(span("space<Space>:**<Base>"))?;
        all_consuming(point_selector)(span("space:series:1.0.0:/some/file.txt"))?;
        all_consuming(point_selector)(span("space+"))?;
        all_consuming(point_selector)(span("space+:**"))?;
        Ok(())
    }

    #[cfg(test)]
    pub mod bind {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::config::config::bind::parse::{bind, final_bind};
        use crate::version::v0_0_1::config::config::bind::ProtoBind;
        use crate::version::v0_0_1::Span;
    }

    #[test]
    pub fn test_http_pattern() -> Result<(), MsgErr> {
        let any_pattern = all_consuming(http_pattern)(span("Http<*>"))?.1;
        let get_any_pattern = all_consuming(http_pattern)(span("Http<Get>"))?.1;
        let get_some_pattern = all_consuming(http_pattern)(span("Http<Get>^/some$"))?.1;
        let get_some_star_pattern = all_consuming(http_pattern)(span("Http<Get>^/some/*"))?.1;
        let get_some_capture_pattern = all_consuming(http_pattern)(span("Http<Get>^/some/(.*)"))?.1;

        let get_some = RequestCore {
            action: Action::Http(HttpMethod::GET),
            headers: Default::default(),
            uri: Uri::from_static("/some"),
            body: Payload::Empty,
        };

        let get_some_plus = RequestCore {
            action: Action::Http(HttpMethod::GET),
            headers: Default::default(),
            uri: Uri::from_static("/some/plus"),
            body: Payload::Empty,
        };

        let post_some = RequestCore {
            action: Action::Http(HttpMethod::POST),
            headers: Default::default(),
            uri: Uri::from_static("/some"),
            body: Payload::Empty,
        };

        assert!(any_pattern.is_match(&get_some).is_ok());
        assert!(any_pattern.is_match(&post_some).is_ok());
        assert!(get_any_pattern.is_match(&get_some).is_ok());
        assert!(get_any_pattern.is_match(&get_some_plus).is_ok());
        assert!(get_some_pattern.is_match(&get_some).is_ok());
        assert!(get_some_pattern.is_match(&post_some).is_err());
        assert!(get_some_pattern.is_match(&get_some_plus).is_err());
        assert!(get_some_star_pattern.is_match(&get_some_plus).is_ok());
        assert!(get_some_capture_pattern.is_match(&get_some_plus).is_ok());

        Ok(())
    }

    #[test]
    pub fn test_selector() -> Result<(), MsgErr> {
        let get_some = RequestCore {
            action: Action::Http(HttpMethod::GET),
            headers: Default::default(),
            uri: Uri::from_static("/some"),
            body: Payload::Empty,
        };

        let get_some_plus = RequestCore {
            action: Action::Http(HttpMethod::GET),
            headers: Default::default(),
            uri: Uri::from_static("/some/plus"),
            body: Payload::Empty,
        };

        let post_some = RequestCore {
            action: Action::Http(HttpMethod::POST),
            headers: Default::default(),
            uri: Uri::from_static("/some"),
            body: Payload::Empty,
        };

        let get_selector = all_consuming(http_pipeline)(span("<Get> -> {{}};"))?.1;
        let get_some_selector = all_consuming(http_pipeline)(span("<Get>^/some -> {{}} => &;"))?.1;
        let post_some_selector =
            all_consuming(http_pipeline)(span("<Post>^/some -> {{}} => &;"))?.1;

        assert!(get_selector.is_match(&get_some).is_ok());
        assert!(get_some_selector.is_match(&get_some).is_ok());
        assert!(post_some_selector.is_match(&post_some).is_ok());
        Ok(())
    }

    #[test]
    pub fn test_scope() -> Result<(), MsgErr> {
        let get_some = RequestCore {
            action: Action::Http(HttpMethod::GET),
            headers: Default::default(),
            uri: Uri::from_static("/some"),
            body: Payload::Empty,
        };

        let get_some_plus = RequestCore {
            action: Action::Http(HttpMethod::GET),
            headers: Default::default(),
            uri: Uri::from_static("/some/plus"),
            body: Payload::Empty,
        };

        let post_some = RequestCore {
            action: Action::Http(HttpMethod::POST),
            headers: Default::default(),
            uri: Uri::from_static("/some"),
            body: Payload::Empty,
        };

        let delete_some = RequestCore {
            action: Action::Http(HttpMethod::DELETE),
            headers: Default::default(),
            uri: Uri::from_static("/some"),
            body: Payload::Empty,
        };

        let section = all_consuming(select_http_pipelines)(span(
            r#"Http {
      <Get>^/(.*) -> {{}} => &;
      <Get>^/some -> {{ }} => &;
      <Post>^/some -> {{ }} => &;
    }"#,
        ))?
        .1;

        if let PipelinesSubScope::Http(scope) = section {
            assert!(scope.find_match(&get_some).is_ok());
            assert!(scope.find_match(&get_some).is_ok());
            assert!(scope.find_match(&post_some).is_ok());
            assert!(scope.find_match(&delete_some).is_err());
        } else {
            return Err("expected Http Section".into());
        }

        Ok(())
    }

    #[test]
    pub fn test_http_method() -> Result<(), MsgErr> {
        http_method(span("Get"))?;
        assert!(http_method(span("Bad")).is_err());
        Ok(())
    }

    #[test]
    pub fn test_http_method_pattern() -> Result<(), MsgErr> {
        http_method_pattern(span("*"))?;
        http_method_pattern(span("Get"))?;
        assert!(http_method_pattern(span("Bad")).is_err());
        /*        match http_method_pattern("Bad") {
                   Ok(_) => {}
                   Err(err) => {
                       err.context
                   }
               }

        */
        Ok(())
    }

    #[test]
    pub fn test_permission_block() -> Result<(), MsgErr> {
        let span = span("rWx-");
        let perm = particle_perms(span)?;
        let (span, block) = perm;
        println!("offset : {}", span.location_offset());

        assert_eq!(
            block,
            ParticlePerms {
                read: false,
                write: true,
                execute: false
            }
        );
        Ok(())
    }

    #[test]
    pub fn test_permissions() -> Result<(), MsgErr> {
        let span = span("cSd-RwX");
        let result = permissions(span)?;
        let (span, permissions) = result;
        println!("offset : {}", span.location_offset());

        assert_eq!(
            permissions,
            crate::version::v0_0_1::security::Permissions {
                child: ChildPerms {
                    create: false,
                    select: true,
                    delete: false
                },
                particle: ParticlePerms {
                    read: true,
                    write: false,
                    execute: true
                }
            }
        );
        Ok(())
    }

    #[test]
    pub fn test_permissions_mask() -> Result<(), MsgErr> {
        let span = span("&cSd-RwX");
        let result = permissions_mask(span)?;
        let (span, mask) = result;
        let permissions = crate::version::v0_0_1::security::Permissions {
            child: ChildPerms {
                create: false,
                select: true,
                delete: false,
            },
            particle: ParticlePerms {
                read: true,
                write: false,
                execute: true,
            },
        };
        let mask2 = PermissionsMask {
            kind: PermissionsMaskKind::And,
            permissions,
        };
        assert_eq!(mask, mask2);
        Ok(())
    }

    #[test]
    pub fn test_root_pattern_match() -> Result<(), MsgErr> {
        let pattern = PointSelector::from_str("+:**")?;
        assert!(pattern.matches_root());
        Ok(())
    }

    #[test]
    pub fn test_perms() -> Result<(), MsgErr> {
        let span = span("Csd-rxy");
        println!("{}", result(permissions(span)).err().unwrap());
        Ok(())
    }

    #[test]
    pub fn test_perm_mask() -> Result<(), MsgErr> {
        let span = span("Csd-rxy");
        println!("{}", result(permissions_mask(span)).err().unwrap());
        Ok(())
    }

    #[test]
    pub fn test_subst() -> Result<(), MsgErr> {
        let val = span("&cSd-RwX");
        let var = span("$(env.some-var)");
        let val = result(var_subst(permissions_mask)(val))?;
        let var = result(var_subst(permissions_mask)(var))?;

        let mut map = MapResolver::new();
        map.insert("env.some-var", "+CSD-RWX");

        let val = val.to_resolved(&map)?;
        let var = var.to_resolved(&map)?;

        println!("val {}", val.to_string());
        println!("var {}", var.to_string());

        Ok(())
    }

    #[test]
    pub fn test_access_grant_kind() -> Result<(), MsgErr> {
        let s = span("perm &rCsd-rwx");
        let result = result(access_grant_kind(s))?;

        Ok(())
    }

    #[test]
    pub fn test_access_grant() -> Result<(), MsgErr> {
        let s = span("perm &rCsd-rwx xon localhost:app");
        let result = result(access_grant(s))?;

        Ok(())
    }

    #[test]
    pub fn test_point_subst() -> Result<(), MsgErr> {
        let s = span("$(some):other:$(user):uppy:..:/dir/file.txt");

        let result = result(point_subst(s));

        match result {
            Ok(point) => {
                println!("{}", point.to_string());
                let mut map = MapResolver::new();
                map.insert("some", "yesterday");
                map.insert("user", "scott");
                match point.to_resolved(&map) {
                    Ok(point) => {
                        println!("{}", point.to_string())
                    }
                    Err(err) => {
                        println!("{}", err.to_string())
                    }
                }
            }
            Err(err) => {
                println!("{}", err)
            }
        }

        Ok(())
    }
    #[test]
    pub fn test_var() -> Result<(), MsgErr> {
        let s = span("$(users)");

        let var = result(var(s))?;

        println!("{}", var.to_string());

        Ok(())
    }

    #[test]
    pub fn test_subst2() -> Result<(), MsgErr> {
        let s = span("$(users)");

        let (next, val) = var_subst(point)(s).unwrap();
        println!("err {}", val.to_string());

        Ok(())
    }
    #[test]
    pub fn test_point_normalize() -> Result<(), MsgErr> {
        let addr = ".::yesterday:other:..:scott:uppy:/hello/../dir/files.txt";
        let point = result(point(span(addr)))?;
        let point = consume_point(addr)?;
        let norm = point.clone().normalize()?;
        assert_eq!(
            "yesterday:scott:uppy:/dir/files.txt".to_string(),
            norm.to_string()
        );
        Ok(())
    }

    #[test]
    pub fn test_error_output() -> Result<(), MsgErr> {
        consume_point("spAce:base").err().unwrap().print();
        consume_point("3space:base").err().unwrap().print();
        consume_point("space..com:base").err().unwrap().print();
        consume_point("space:ba?se").err().unwrap().print();
        consume_point("space.com:-base").err().unwrap().print();
        consume_point("space.com:-base:7.7.0")
            .err()
            .unwrap()
            .print();
        consume_point("space.com:base:3.7?").err().unwrap().print();

        consume_point("space.com:base:3.7.0:/unh*ppy-file")
            .err()
            .unwrap()
            .print();
        consume_point("space.com:base:3.7.0:/unHappy-d?r/")
            .err()
            .unwrap()
            .print();
        consume_point("space.com:base:3.7.0:/all-good/under-thunder.txt ")
            .err()
            .unwrap()
            .print();

        consume_point_subst("space.com:base:3.7.0:following-version-must-be-fileroot")
            .err()
            .unwrap()
            .print();
        // make sure artifact bundle itself works (when eof is reached)
        assert!(consume_point("space.com:base:3.7.0").is_ok());
        Ok(())
    }


    #[test]
    pub fn point_subst_err() -> Result<(), MsgErr> {
        consume_point_subst("spAce:base").err().unwrap().print();
        consume_point_subst("3space:base").err().unwrap().print();
        consume_point_subst("space..com:base").err().unwrap().print();
        consume_point_subst("space:ba?se").err().unwrap().print();
        consume_point_subst("space.com:-base").err().unwrap().print();
        consume_point_subst("space.com:-base:7.7.0")
            .err()
            .unwrap()
            .print();
        consume_point_subst("space.com:base:3.7?").err().unwrap().print();

        consume_point_subst("space.com:base:3.7.0:/unh*ppy-file")
            .err()
            .unwrap()
            .print();
        consume_point_subst("space.com:base:3.7.0:/unHappy-d?r/")
            .err()
            .unwrap()
            .print();

        consume_point_subst("space.com:base:3.7.0:/all-good/under-thunder.txt ")
            .err()
            .unwrap()
            .print();

        consume_point_subst("space.com:base:3.7.0:following-version-must-be-fileroot")
            .err()
            .unwrap()
            .print();

        consume_point_subst("space.com:base:3.7.0:$(following-version-must-be-fileroot)")
            .err()
            .unwrap()
            .print();

        consume_point_subst("space.com:$$(base)")
            .err()
            .unwrap()
            .print();

        assert!(consume_point_subst("space.com:base:3.7.0:/$(this-should-work)").is_ok());

        // make sure artifact bundle itself works (when eof is reached)
        assert!(consume_point_subst("space.com:base:3.7.0").is_ok());
        assert!(consume_point_subst("space.com:base").is_ok());
//        assert!(consume_point_subst("space.com:$(var)").is_ok());
        Ok(())
    }
}




