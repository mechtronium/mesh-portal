use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;
use nom_locate::LocatedSpan;

use serde::{Deserialize, Serialize};

use crate::error::MsgErr;
use crate::version::v0_0_1::bin::Bin;

pub type State = HashMap<String, Bin>;
pub type Span<'a> = LocatedSpan<&'a str>;

extern "C" {
    pub fn mesh_portal_unique_id() -> String;
}

pub mod artifact {
    use crate::version::v0_0_1::bin::Bin;
    use crate::version::v0_0_1::id::Address;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Artifact {
        pub address: Address,
        pub bin: Bin,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ArtifactRequest {
        pub address: Address,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ArtifactResponse {
        pub to: Address,
        pub payload: Bin,
    }
}

pub type Port = String;

pub mod id {
    use nom::bytes::complete::tag;
    use nom::combinator::{all_consuming, opt};
    use nom::sequence::{delimited, tuple};
    use regex::Captures;
    use std::collections::HashMap;
    use std::convert::TryInto;
    use std::fmt::Formatter;
    use std::ops::Deref;
    use std::str::FromStr;

    use semver::SemVerError;
    use serde::de::Visitor;
    use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

    use crate::error::MsgErr;
    use crate::version::v0_0_1::parse::{address, camel_case, consume_address, Res};
    use crate::version::v0_0_1::pattern::parse::{address_and_kind, resource_type, specific};
    use crate::version::v0_0_1::pattern::{AddressKindPattern, Pattern, SpecificPattern, VersionReq};

    pub type ResourceType = String;
    pub type ResourceKind = KindParts;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct AddressAndKind {
        pub address: Address,
        pub kind: ResourceKind,
    }

    impl AddressAndKind {
        pub fn new(address: Address, kind: ResourceKind) -> Self {
            Self { address, kind }
        }
    }

    impl ToString for AddressAndKind {
        fn to_string(&self) -> String {
            format!("{}<{}>", self.address.to_string(), self.kind.to_string())
        }
    }

    impl FromStr for AddressAndKind {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let address_and_kind = match all_consuming(address_and_kind)(s) {
                Ok((_, address_and_kind)) => address_and_kind,
                Err(err) => {
                    return Err("could not parse AddressAndKind".into());
                }
            };
            Ok(address_and_kind)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct AddressAndType {
        pub address: Address,
        pub resource_type: ResourceType,
    }

    pub type Meta = HashMap<String, String>;
    pub type PayloadClaim = String;
    pub type HostKey = String;

    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    pub struct Version {
        pub version: semver::Version,
    }

    impl Deref for Version {
        type Target = semver::Version;

        fn deref(&self) -> &Self::Target {
            &self.version
        }
    }

    impl Serialize for Version {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str(self.version.to_string().as_str())
        }
    }

    struct VersionVisitor;

    impl<'de> Visitor<'de> for VersionVisitor {
        type Value = Version;

        fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
            formatter.write_str("SemVer version")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            match Version::from_str(v) {
                Ok(version) => Ok(version),
                Err(error) => {
                    //Err(de::Error::custom(error.to_string() ))
                    Err(de::Error::invalid_type(de::Unexpected::Str(v), &self))
                }
            }
        }
    }

    impl<'de> Deserialize<'de> for Version {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_str(VersionVisitor)
        }
    }

    impl ToString for Version {
        fn to_string(&self) -> String {
            self.version.to_string()
        }
    }

    impl TryInto<semver::Version> for Version {
        type Error = MsgErr;

        fn try_into(self) -> Result<semver::Version, Self::Error> {
            Ok(self.version)
        }
    }

    impl FromStr for Version {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let version = semver::Version::from_str(s)?;
            Ok(Self { version })
        }
    }

    /// Stands for "Type, Kind, Specific"
    pub trait Tks {
        fn resource_type(&self) -> ResourceType;
        fn kind_to_string(&self) -> Option<String>;
        fn specific(&self) -> Option<Specific>;
        fn matches(&self, tks: &dyn Tks) -> bool;
    }

    #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, Hash)]
    pub struct Specific {
        pub vendor: String,
        pub product: String,
        pub variant: String,
        pub version: Version,
    }

    impl ToString for Specific {
        fn to_string(&self) -> String {
            format!(
                "{}:{}:{}:{}",
                self.vendor,
                self.product,
                self.variant,
                self.version.to_string()
            )
        }
    }

    impl FromStr for Specific {
        type Err = ();

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            todo!()
        }
    }

    impl TryInto<SpecificPattern> for Specific {
        type Error = MsgErr;

        fn try_into(self) -> Result<SpecificPattern, Self::Error> {
            Ok(SpecificPattern {
                vendor: Pattern::Exact(self.vendor),
                product: Pattern::Exact(self.product),
                variant: Pattern::Exact(self.variant),
                version: VersionReq::from_str(self.version.to_string().as_str())?,
            })
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum RouteSegment {
        Local,
        Domain(String),
        Tag(String),
        Mesh(String),
    }

    impl ToString for RouteSegment {
        fn to_string(&self) -> String {
            match self {
                RouteSegment::Local => ".".to_string(),
                RouteSegment::Domain(domain) => domain.clone(),
                RouteSegment::Tag(tag) => {
                    format!("[{}]", tag)
                }
                RouteSegment::Mesh(mesh) => {
                    format!("<<{}>>", mesh)
                }
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum AddressSegment {
        Root,
        Space(String),
        Base(String),
        FilesystemRootDir,
        Dir(String),
        File(String),
        Version(Version),
    }

    impl AddressSegment {
        pub fn apply_captures(self, captures: &Captures) -> Result<Self, MsgErr> {
            match self {
                AddressSegment::Root => Ok(AddressSegment::Root),
                AddressSegment::Space(replacement) => {
                    let mut dst = String::new();
                    captures.expand(replacement.as_str(), &mut dst);
                    Ok(AddressSegment::Space(dst))
                }
                AddressSegment::Base(replacement) => {
                    let mut dst = String::new();
                    captures.expand(replacement.as_str(), &mut dst);
                    Ok(AddressSegment::Base(dst))
                }
                AddressSegment::FilesystemRootDir => Ok(AddressSegment::FilesystemRootDir),
                AddressSegment::Dir(replacement) => {
                    let mut dst = String::new();
                    captures.expand(replacement.as_str(), &mut dst);
                    Ok(AddressSegment::Dir(dst))
                }
                AddressSegment::File(replacement) => {
                    let mut dst = String::new();
                    captures.expand(replacement.as_str(), &mut dst);
                    Ok(AddressSegment::File(dst))
                }
                AddressSegment::Version(version) => Ok(AddressSegment::Version(version)),
            }
        }

        pub fn is_version(&self) -> bool {
            match self {
                AddressSegment::Version(_) => true,
                _ => false,
            }
        }

        pub fn is_filepath(&self) -> bool {
            match self {
                AddressSegment::Dir(_) => true,
                AddressSegment::FilesystemRootDir => true,
                AddressSegment::File(_) => true,
                _ => false,
            }
        }

        pub fn is_file(&self) -> bool {
            match self {
                AddressSegment::File(_) => true,
                _ => false,
            }
        }

        pub fn is_dir(&self) -> bool {
            match self {
                AddressSegment::Dir(_) => true,
                AddressSegment::FilesystemRootDir => true,
                _ => false,
            }
        }

        pub fn terminating_delim(&self) -> &str {
            match self {
                AddressSegment::Space(_) => ":",
                AddressSegment::Base(_) => ":",
                AddressSegment::Dir(_) => "",
                AddressSegment::File(_) => "",
                AddressSegment::Version(_) => ":",
                AddressSegment::FilesystemRootDir => "",
                AddressSegment::Root => "",
            }
        }

        pub fn is_filesystem_ref(&self) -> bool {
            match self {
                AddressSegment::Space(_) => false,
                AddressSegment::Base(_) => false,
                AddressSegment::Dir(_) => true,
                AddressSegment::File(_) => true,
                AddressSegment::Version(_) => false,
                AddressSegment::FilesystemRootDir => true,
                AddressSegment::Root => false,
            }
        }
    }

    impl ToString for AddressSegment {
        fn to_string(&self) -> String {
            match self {
                AddressSegment::Space(space) => space.clone(),
                AddressSegment::Base(base) => base.clone(),
                AddressSegment::Dir(dir) => dir.clone(),
                AddressSegment::File(file) => file.clone(),
                AddressSegment::Version(version) => version.to_string(),
                AddressSegment::FilesystemRootDir => "/".to_string(),
                AddressSegment::Root => "".to_string(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct Address {
        pub route: RouteSegment,
        pub segments: Vec<AddressSegment>,
    }

    impl Address {
        pub fn to_bundle(self) -> Result<Address, MsgErr> {
            if self.segments.is_empty() {
                return Err("Address does not contain a bundle".into());
            }

            if let Some(AddressSegment::Version(_)) = self.segments.last() {
                return Ok(self);
            }

            return self.parent().expect("expected parent").to_bundle();
        }

        pub fn to_safe_filename(&self) -> String {
            self.to_string()
        }

        pub fn is_artifact_bundle_part(&self) -> bool {
            for segment in &self.segments {
                if segment.is_version() {
                    return true;
                }
            }
            return false;
        }

        pub fn is_artifact(&self) -> bool {
            if let Option::Some(segment) = self.last_segment() {
                if self.is_artifact_bundle_part() && segment.is_file() {
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }

        pub fn is_artifact_bundle(&self) -> bool {
            if let Option::Some(segment) = self.last_segment() {
                segment.is_version()
            } else {
                false
            }
        }

        pub fn push(&self, segment: String) -> Result<Self, MsgErr> {
            if self.segments.is_empty() {
                Self::from_str(segment.as_str())
            } else {
                let last = self.last_segment().expect("expected last segment");
                let address = match last {
                    AddressSegment::Root => segment,
                    AddressSegment::Space(_) => {
                        format!("{}:{}", self.to_string(), segment)
                    }
                    AddressSegment::Base(_) => {
                        format!("{}:{}", self.to_string(), segment)
                    }
                    AddressSegment::FilesystemRootDir => {
                        format!("{}{}", self.to_string(), segment)
                    }
                    AddressSegment::Dir(_) => {
                        format!("{}{}", self.to_string(), segment)
                    }
                    AddressSegment::Version(_) => {
                        if segment != "/" {
                            return Err(
                                "Root filesystem artifact dir required after version".into()
                            );
                        }
                        format!("{}:/", self.to_string())
                    }
                    AddressSegment::File(_) => return Err("cannot append to a file".into()),
                };
                Self::from_str(address.as_str())
            }
        }

        pub fn push_file(&self, segment: String) -> Result<Self, MsgErr> {
            Self::from_str(format!("{}{}", self.to_string(), segment).as_str())
        }

        pub fn push_segment(&self, segment: AddressSegment) -> Self {
            let mut address = self.clone();
            address.segments.push(segment);
            address
        }

        pub fn last_segment(&self) -> Option<AddressSegment> {
            self.segments.last().cloned()
        }

        pub fn filepath(&self) -> Option<String> {
            let mut path = String::new();
            for segment in &self.segments {
                match segment {
                    AddressSegment::FilesystemRootDir => {
                        path.push_str("/");
                    }
                    AddressSegment::Dir(dir) => {
                        path.push_str(dir.as_str());
                    }
                    AddressSegment::File(file) => {
                        path.push_str(file.as_str());
                    }
                    _ => {}
                }
            }
            if path.is_empty() {
                None
            } else {
                Some(path)
            }
        }

        pub fn is_filesystem_ref(&self) -> bool {
            if let Option::Some(last_segment) = self.last_segment() {
                last_segment.is_filesystem_ref()
            } else {
                false
            }
        }
    }

    impl FromStr for Address {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(consume_address(s)?.1)
        }
    }

    impl Into<String> for Address {
        fn into(self) -> String {
            self.to_string()
        }
    }

    impl TryInto<AddressKindPattern> for Address {
        type Error = MsgErr;

        fn try_into(self) -> Result<AddressKindPattern, Self::Error> {
            Ok(AddressKindPattern::from_str(self.to_string().as_str())?)
        }
    }

    impl Address {
        pub fn to_full_string(&self) -> String {
            match self.route {
                RouteSegment::Local => {
                    let mut rtn = String::new();
                    rtn.push_str(".::");
                    if self.segments.is_empty() {
                        rtn.push_str("ROOT");
                    } else {
                        for (i, segment) in self.segments.iter().enumerate() {
                            rtn.push_str(segment.to_string().as_str());
                            if i != self.segments.len() - 1 {
                                rtn.push_str(segment.terminating_delim());
                            }
                        }
                    }
                    rtn.to_string()
                }
                _ => self.to_string()
            }
        }

    }


    impl ToString for Address {
        fn to_string(&self) -> String {
            let mut rtn = String::new();

            match &self.route {
                RouteSegment::Local => {}
                RouteSegment::Domain(domain) => {
                    rtn.push_str(format!("{}::", domain).as_str());
                }
                RouteSegment::Tag(tag) => {
                    rtn.push_str(format!("[{}]::", tag).as_str());
                }
                RouteSegment::Mesh(mesh) => {
                    rtn.push_str(format!("<<{}>>::", mesh).as_str());
                }
            }

            if self.segments.is_empty() {
                "ROOT".to_string()
            } else {
                for (i, segment) in self.segments.iter().enumerate() {
                    rtn.push_str(segment.to_string().as_str());
                    if i != self.segments.len() - 1 {
                        rtn.push_str(segment.terminating_delim());
                    }
                }
                rtn.to_string()
            }
        }
    }

    impl Address {
        pub fn parent(&self) -> Option<Address> {
            if self.segments.is_empty() {
                return Option::None;
            }
            let mut segments = self.segments.clone();
            segments.remove(segments.len() - 1);
            Option::Some(Self {
                route: self.route.clone(),
                segments,
            })
        }

        pub fn parse(input: &str) -> Res<&str, Self> {
            address(input)
        }

        pub fn root() -> Self {
            Self {
                route: RouteSegment::Local,
                segments: vec![],
            }
        }

        pub fn root_with_route(route: RouteSegment) -> Self {
            Self {
                route,
                segments: vec![],
            }
        }

        pub fn is_root(&self) -> bool {
            self.segments.is_empty()
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct CaptureAddress {
        pub route: RouteSegment,
        pub segments: Vec<AddressSegment>,
    }

    impl CaptureAddress {
        pub fn to_address(self, captures: Captures) -> Result<Address, MsgErr> {
            let mut segments = vec![];
            for segment in self.segments {
                segments.push(segment.apply_captures(&captures)?)
            }
            let address = Address {
                route: self.route,
                segments,
            };

            // to make sure all the regex captures are removed...
            let address = Address::from_str(address.to_string().as_str())?;
            Ok(address)
        }
    }

    impl ToString for CaptureAddress {
        fn to_string(&self) -> String {
            let mut rtn = String::new();

            match &self.route {
                RouteSegment::Local => {}
                RouteSegment::Domain(domain) => {
                    rtn.push_str(format!("{}::", domain).as_str());
                }
                RouteSegment::Tag(tag) => {
                    rtn.push_str(format!("[{}]::", tag).as_str());
                }
                RouteSegment::Mesh(mesh) => {
                    rtn.push_str(format!("<<{}>>::", mesh).as_str());
                }
            }

            if self.segments.is_empty() {
                "[root]".to_string()
            } else {
                for (i, segment) in self.segments.iter().enumerate() {
                    rtn.push_str(segment.to_string().as_str());
                    if i != self.segments.len() - 1 {
                        rtn.push_str(segment.terminating_delim());
                    }
                }
                rtn.to_string()
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct KindParts {
        pub resource_type: ResourceType,
        pub kind: Option<String>,
        pub specific: Option<Specific>,
    }

    impl ToString for KindParts {
        fn to_string(&self) -> String {
            if self.kind.is_some() && self.specific.is_some() {
                format!(
                    "{}<{}<{}>>",
                    self.resource_type.to_string(),
                    self.kind.as_ref().expect("kind"),
                    self.specific.as_ref().expect("specific").to_string()
                )
            } else if self.kind.is_some() {
                format!(
                    "{}<{}>",
                    self.resource_type.to_string(),
                    self.kind.as_ref().expect("kind")
                )
            } else {
                self.resource_type.to_string()
            }
        }
    }

    impl FromStr for KindParts {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_, kind) = all_consuming(tuple((
                resource_type,
                opt(delimited(
                    tag("<"),
                    tuple((camel_case, opt(delimited(tag("<"), specific, tag(">"))))),
                    tag(">"),
                )),
            )))(s)
            .map(|(next, (resource_type, rest))| {
                let mut rtn = KindParts {
                    resource_type,
                    kind: Option::None,
                    specific: Option::None,
                };

                match rest {
                    Some((kind, specific)) => {
                        rtn.kind = Option::Some(kind.to_string());
                        match specific {
                            Some(specific) => {
                                rtn.specific = Option::Some(specific);
                            }
                            None => {}
                        }
                    }
                    None => {}
                }

                (next, rtn)
            })?;
            Ok(kind)
        }
    }

    impl KindParts {
        pub fn new(
            resource_type: ResourceType,
            kind: Option<String>,
            specific: Option<Specific>,
        ) -> Self {
            Self {
                resource_type,
                kind,
                specific,
            }
        }
    }

    impl Tks for KindParts {
        fn resource_type(&self) -> ResourceType {
            self.resource_type.clone()
        }

        fn kind_to_string(&self) -> Option<String> {
            self.kind.clone()
        }

        fn specific(&self) -> Option<Specific> {
            self.specific.clone()
        }

        fn matches(&self, tks: &dyn Tks) -> bool {
            self.resource_type == tks.resource_type()
                && self.kind == tks.kind_to_string()
                && self.specific == tks.specific()
        }
    }
}

pub mod path {
    use crate::error::MsgErr;
    use crate::version::v0_0_1::parse::consume_path;
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
            let (_, path) = consume_path(s)?;
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

pub mod pattern {
    use std::convert::TryInto;
    use std::fmt::Formatter;
    use std::ops::Deref;
    use std::str::FromStr;

    use semver::ReqParseError;
    use serde::de::Visitor;
    use serde::{de, Deserializer, Serializer};

    use crate::error::MsgErr;

    use crate::version::v0_0_1::entity::request::{Action, Rc, RcCommandType, RequestCore};
    use crate::version::v0_0_1::id::{
        Address, AddressSegment, ResourceKind, ResourceType, RouteSegment, Specific, Tks, Version,
    };
    use crate::version::v0_0_1::parse::{address, camel_case, camel_case_to_string_matcher, capture_address, capture_path, consume_address_kind_path, file_chars, path, path_regex, Res };
    use crate::version::v0_0_1::pattern::parse::{address_kind_pattern, pattern, value_pattern};
    use crate::version::v0_0_1::pattern::specific::{
        ProductPattern, VariantPattern, VendorPattern,
    };
    use crate::version::v0_0_1::payload::{Call, CallKind, CallWithConfig, HttpCall, HttpMethod, HttpMethodType, ListPattern, MapPattern, MsgCall, Payload, PayloadFormat, PayloadPattern, PayloadTypePattern, Primitive, PrimitiveType, Range};
    use crate::version::v0_0_1::util::{MethodPattern, StringMatcher, ValueMatcher, ValuePattern};
    use crate::{Deserialize, Serialize};
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0, one_of};
    use nom::combinator::{all_consuming, fail, opt, recognize};
    use nom::error::{context, ErrorKind, ParseError, VerboseError};
    use nom::multi::separated_list0;
    use nom::sequence::{delimited, preceded, terminated, tuple};
    use nom::{AsChar, IResult, InputTakeAtPosition, Parser, InputLength, InputTake, Compare};
    use nom_supreme::error::ErrorTree;
    use nom_supreme::parser_ext::FromStrParser;
    use nom_supreme::{parse_from_str, ParserExt};
    use regex::Regex;
    use std::collections::HashMap;
    use crate::version::v0_0_1::config::bind::parse::{many_until0, select_block, SelectBlock};

    #[derive(Debug,Clone,Serialize,Deserialize,)]
    pub struct TksPattern {
        pub resource_type: ResourceTypePattern,
        pub kind: KindPattern,
        pub specific: ValuePattern<SpecificPattern>,
    }

    impl TksPattern {
        pub fn new(
            resource_type: ResourceTypePattern,
            kind: KindPattern,
            specific: ValuePattern<SpecificPattern>,
        ) -> Self {
            Self {
                resource_type,
                kind,
                specific,
            }
        }

        pub fn matches(&self, kind: &ResourceKind) -> bool
        where
            ResourceKind: Eq + PartialEq,
        {
            self.resource_type.matches(&kind.resource_type())
                && self.kind.matches(kind)
                && self.specific.is_match_opt(kind.specific().as_ref()).is_ok()
        }
    }

    impl ToString for TksPattern {
        fn to_string(&self) -> String {
            format!("{}<{}<{}>>", self.resource_type.to_string(), self.kind.to_string(), self.specific.to_string() )
        }
    }

    impl TksPattern {
        pub fn any() -> Self {
            Self {
                resource_type: ResourceTypePattern::Any,
                kind: KindPattern::Any,
                specific: ValuePattern::Any,
            }
        }
    }

    pub type KindPattern = Pattern<ResourceKind>;
    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct AddressKindPattern {
        pub hops: Vec<Hop>,
    }

    impl FromStr for AddressKindPattern {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_, rtn) = all_consuming(address_kind_pattern)(s)?;
            Ok(rtn)
        }
    }

    impl AddressKindPattern {
        fn consume(&self) -> Option<AddressKindPattern> {
            if self.hops.is_empty() {
                Option::None
            } else {
                let mut hops = self.hops.clone();
                hops.remove(0);
                Option::Some(AddressKindPattern { hops })
            }
        }

        pub fn matches_root(&self) -> bool {

           if self.hops.is_empty() {
             true
           } else if self.hops.len() == 1 {
               let hop = self.hops.first().unwrap();
               if SegmentPattern::InclusiveAny == hop.segment || SegmentPattern::InclusiveRecursive == hop.segment {
                   let resource_kind = ResourceKind::new( "Root".to_string(), None, None );
                   hop.tks.matches(&resource_kind)
               } else {
                   false
               }
           }
           else {
               false
           }
        }

        pub fn is_root(&self) -> bool {
            self.hops.is_empty()
        }

        pub fn is_final(&self) -> bool {
            self.hops.len() == 1
        }

        pub fn query_root(&self) -> Address {
            let mut segments = vec![];
            for hop in &self.hops {
                if let SegmentPattern::Exact(exact) = &hop.segment {
                    if hop.inclusive {
                        break;
                    }
                    match exact {
                        ExactSegment::Address(seg) => {
                            segments.push(seg.clone());
                        }
                        ExactSegment::Version(version) => {
                            segments.push(AddressSegment::Version(version.clone()));
                        }
                    }
                } else {
                    break;
                }
            }

            Address {
                route: RouteSegment::Local,
                segments,
            }
        }

        pub fn sub_select_hops(&self) -> Vec<Hop> {
            let mut hops = self.hops.clone();
            let query_root_segments = self.query_root().segments.len();
            for _ in 0..query_root_segments {
                hops.remove(0);
            }
            hops
        }

        pub fn matches(&self, address_kind_path: &AddressKindPath) -> bool
        where
            ResourceType: Clone,
            ResourceKind: Clone,
        {

            if address_kind_path.is_root() && self.is_root() {
                return true;
            }

            if address_kind_path.segments.is_empty() || self.hops.is_empty() {
                return false;
            }

            let hop = self.hops.first().expect("hop");
            let seg = address_kind_path.segments.first().expect("segment");

            /*
            if address_kind_path.segments.len() < self.hops.len() {
                // if a hop is 'inclusive' then this will match to true.  We do this for cases like:
                // localhost+:**   // Here we want everything under localhost INCLUDING localhost to be matched
println!("hop: {}", hop.to_string());
println!("seg: {}", seg.to_string());
                if hop.inclusive && hop.matches(&seg) {
                    return true;
                } else {
                    return false;
                }
            }

             */

            if address_kind_path.is_final() && self.is_final() {
                // this is the final hop & segment if they match, everything matches!
                hop.matches(seg)
            } else if address_kind_path.is_root() {
                false
            } else if self.is_root() {
                false
            } else if address_kind_path.is_final() {
                // we still have hops that haven't been matched and we are all out of path... but we have a weird rule
                // if a hop is 'inclusive' then this will match to true.  We do this for cases like:
                // localhost+:**   // Here we want everything under localhost INCLUDING localhost to be matched
                if hop.inclusive && hop.matches(&seg) {
                    true
                } else {
                    false
                }
            }
            // special logic is applied to recursives **
            else if hop.segment.is_recursive() && self.hops.len() >= 2 {
                // a Recursive is similar to an Any in that it will match anything, however,
                // it is not consumed until the NEXT segment matches...
                let next_hop = self.hops.get(1).expect("next<Hop>");
                if next_hop.matches(seg) {
                    // since the next hop after the recursive matches, we consume the recursive and continue hopping
                    // this allows us to make matches like:
                    // space.org:**:users ~ space.org:many:silly:dirs:users
                    self.consume()
                        .expect("AddressTksPattern")
                        .matches(&address_kind_path.consume().expect("AddressKindPath"))
                } else {
                    // the NEXT hop does not match, therefore we do NOT consume() the current hop
                    self.matches(&address_kind_path.consume().expect("AddressKindPath"))
                }
            } else if hop.segment.is_recursive() && address_kind_path.is_final() {
               hop.matches( address_kind_path.segments.last().expect("segment"))
            } else if hop.segment.is_recursive() {
                hop.matches( address_kind_path.segments.last().expect("segment")) && self.matches( &address_kind_path.consume().expect("address_kind_path") )
            } else if hop.matches(seg) {
                // in a normal match situation, we consume the hop and move to the next one
                self.consume()
                    .expect("AddressTksPattern")
                    .matches(&address_kind_path.consume().expect("AddressKindPath"))
            } else {
                false
            }
        }
    }

    impl ToString for AddressKindPattern {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            for (index,hop) in self.hops.iter().enumerate() {
                rtn.push_str(hop.to_string().as_str() );
                if index < self.hops.len()-1 {
                    rtn.push_str(":");
                }
            }
            rtn
        }
    }

    #[derive(Debug,Clone,Eq,PartialEq,Hash)]
    pub struct VersionReq {
        pub version: semver::VersionReq,
    }

    impl Deref for VersionReq {
        type Target = semver::VersionReq;

        fn deref(&self) -> &Self::Target {
            &self.version
        }
    }

    impl Serialize for VersionReq {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str(self.version.to_string().as_str())
        }
    }

    impl<'de> Deserialize<'de> for VersionReq {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_str(VersionReqVisitor)
        }
    }

    struct VersionReqVisitor;

    impl<'de> Visitor<'de> for VersionReqVisitor {
        type Value = VersionReq;

        fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
            formatter.write_str("SemVer version requirement")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            match VersionReq::from_str(v) {
                Ok(version) => Ok(version),
                Err(error) => {
                    //Err(de::Error::custom(error.to_string() ))
                    Err(de::Error::invalid_type(de::Unexpected::Str(v), &self))
                }
            }
        }
    }

    impl ToString for VersionReq {
        fn to_string(&self) -> String {
            self.version.to_string()
        }
    }

    impl TryInto<semver::VersionReq> for VersionReq {
        type Error = MsgErr;

        fn try_into(self) -> Result<semver::VersionReq, Self::Error> {
            Ok(self.version)
        }
    }

    impl FromStr for VersionReq {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let version = semver::VersionReq::from_str(s)?;
            Ok(Self { version })
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq)]
    pub enum SegmentPattern {
        InclusiveAny,       // +:*  // includes Root if it's the first segment
        InclusiveRecursive, // +:** // includes Root if its the first segment
        Any,       // *
        Recursive, // **
        Exact(ExactSegment),
        Version(VersionReq),
    }

    impl SegmentPattern {
        pub fn is_exact(&self) -> bool {
            match self {
                SegmentPattern::Exact(_) => true,
                _ => false,
            }
        }

        pub fn matches(&self, segment: &AddressSegment) -> bool {
            match self {
                SegmentPattern::InclusiveAny => true,
                SegmentPattern::InclusiveRecursive => true,
                SegmentPattern::Any => true,
                SegmentPattern::Recursive => true,
                SegmentPattern::Exact(exact) => match exact {
                    ExactSegment::Address(pattern) => *pattern == *segment,
                    ExactSegment::Version(a) => {
                        if let AddressSegment::Version(b) = segment {
                            *a == *b
                        } else {
                            false
                        }
                    }
                },
                SegmentPattern::Version(req) => {
                    if let AddressSegment::Version(b) = segment {
                        req.matches(b)
                    } else {
                        false
                    }
                }
            }
        }

        pub fn is_recursive(&self) -> bool {
            match self {
                SegmentPattern::InclusiveAny=> false,
                SegmentPattern::InclusiveRecursive => true,
                SegmentPattern::Any => false,
                SegmentPattern::Recursive => true,
                SegmentPattern::Exact(_) => false,
                SegmentPattern::Version(_) => false,
            }
        }
    }

    impl ToString for SegmentPattern {
        fn to_string(&self) -> String {
            match self {
                SegmentPattern::InclusiveAny => "+:*".to_string(),
                SegmentPattern::InclusiveRecursive=> "+:**".to_string(),
                SegmentPattern::Any => "*".to_string(),
                SegmentPattern::Recursive => "**".to_string(),
                SegmentPattern::Exact(exact) => exact.to_string(),
                SegmentPattern::Version(version) => version.to_string()
            }
        }
    }

    pub type KeySegment = String;

    #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
    pub enum ExactSegment {
        Address(AddressSegment),
        Version(Version),
    }

    impl ExactSegment {
        pub fn matches(&self, segment: &AddressSegment) -> bool {
            match self {
                ExactSegment::Address(s) => *s == *segment,
                ExactSegment::Version(a) => {
                    if let AddressSegment::Version(b) = segment {
                        *a == *b
                    } else {
                        false
                    }
                }
            }
        }
    }

    impl ToString for ExactSegment {
        fn to_string(&self) -> String {
            match self {
                ExactSegment::Address(address) => address.to_string(),
                ExactSegment::Version(version) => version.to_string()
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SpecificPattern {
        pub vendor: VendorPattern,
        pub product: ProductPattern,
        pub variant: VariantPattern,
        pub version: VersionReq,
    }

    impl ValueMatcher<Specific> for SpecificPattern {
        fn is_match(&self, specific: &Specific) -> Result<(), MsgErr> {
            if self.vendor.matches(&specific.vendor)
                && self.product.matches(&specific.product)
                && self.variant.matches(&specific.variant)
                && self.version.matches(&specific.version)
            {
                Ok(())
            } else {
                Err("Specific does not match pattern".into())
            }
        }
    }

    impl ToString for SpecificPattern {
        fn to_string(&self) -> String {
            format!(
                "{}:{}:{}:({})",
                self.vendor.to_string(),
                self.product.to_string(),
                self.variant.to_string(),
                self.version.to_string()
            )
        }
    }
    pub mod specific {
        use std::ops::Deref;
        use std::str::FromStr;

        use crate::error::MsgErr;
        use crate::version::v0_0_1::pattern::Pattern;

        pub struct VersionReq {
            pub req: semver::VersionReq,
        }

        impl Deref for VersionReq {
            type Target = semver::VersionReq;

            fn deref(&self) -> &Self::Target {
                &self.req
            }
        }

        impl FromStr for VersionReq {
            type Err = MsgErr;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(VersionReq {
                    req: semver::VersionReq::from_str(s)?,
                })
            }
        }

        pub type VendorPattern = Pattern<String>;
        pub type ProductPattern = Pattern<String>;
        pub type VariantPattern = Pattern<String>;
        pub type VersionPattern = Pattern<VersionReq>;
    }

    pub mod parse {
        use std::convert::{TryFrom, TryInto};
        use std::str::FromStr;

        use nom::branch::alt;
        use nom::bytes::complete::tag;
        use nom::character::complete::{alpha1, digit1};
        use nom::combinator::{all_consuming, opt, recognize};
        use nom::error::{context, ContextError, ParseError, VerboseError};
        use nom::multi::{many0, many1};
        use nom::sequence::{delimited, preceded, terminated, tuple};
        use nom::{Compare, InputIter, InputLength, InputTake, Parser, UnspecializedInput};
        use nom::{Err, IResult};

        use crate::error::MsgErr;
        use crate::version::v0_0_1::id::{
            AddressAndKind, AddressSegment, KindParts, ResourceKind, ResourceType, Specific,
            Version,
        };
        use crate::version::v0_0_1::parse::{
            address, address_segment_chars, camel_case, domain_chars, file_chars, skewer_chars,
            version_address_segment, version_chars, version_req_chars, Res,
        };
        use crate::version::v0_0_1::pattern::specific::{
            ProductPattern, VariantPattern, VendorPattern,
        };
        use crate::version::v0_0_1::pattern::{
            AddressKindPattern, ExactSegment, Hop, KindPattern, Pattern, ResourceTypePattern,
            SegmentPattern, SpecificPattern, TksPattern, VersionReq,
        };
        use crate::version::v0_0_1::util::ValuePattern;
        use nom_supreme::error::ErrorTree;
        use nom_supreme::{parse_from_str, ParserExt};

        fn inclusive_any_segment(input: &str) -> Res<&str, SegmentPattern> {
            tag("+:*")(input).map(|(next, _)| (next, SegmentPattern::InclusiveAny))
        }

        fn inclusive_recursive_segment(input: &str) -> Res<&str, SegmentPattern> {
            tag("+:**")(input).map(|(next, _)| (next, SegmentPattern::InclusiveRecursive))
        }

        fn any_segment(input: &str) -> Res<&str, SegmentPattern> {
            tag("*")(input).map(|(next, _)| (next, SegmentPattern::Any))
        }

        fn recursive_segment(input: &str) -> Res<&str, SegmentPattern> {
            tag("**")(input).map(|(next, _)| (next, SegmentPattern::Recursive))
        }

        fn exact_space_segment(input: &str) -> Res<&str, SegmentPattern> {
            address_segment_chars(input).map(|(next, segment)| {
                (
                    next,
                    SegmentPattern::Exact(ExactSegment::Address(AddressSegment::Space(
                        segment.to_string(),
                    ))),
                )
            })
        }

        fn exact_base_segment(input: &str) -> Res<&str, SegmentPattern> {
            address_segment_chars(input).map(|(next, segment)| {
                (
                    next,
                    SegmentPattern::Exact(ExactSegment::Address(AddressSegment::Base(
                        segment.to_string(),
                    ))),
                )
            })
        }

        fn exact_file_segment(input: &str) -> Res<&str, SegmentPattern> {
            file_chars(input).map(|(next, segment)| {
                (
                    next,
                    SegmentPattern::Exact(ExactSegment::Address(AddressSegment::File(
                        segment.to_string(),
                    ))),
                )
            })
        }

        fn exact_dir_segment(input: &str) -> Res<&str, SegmentPattern> {
            file_chars(input).map(|(next, segment)| {
                (
                    next,
                    SegmentPattern::Exact(ExactSegment::Address(AddressSegment::Dir(
                        segment.to_string(),
                    ))),
                )
            })
        }

        fn exact_version_segment(input: &str) -> Res<&str, SegmentPattern> {
            let (next, version): (&str, Version) = parse_from_str(version_chars).parse(input)?;

            Ok((next, SegmentPattern::Exact(ExactSegment::Version(version))))
        }

        fn version_req_segment(input: &str) -> Res<&str, SegmentPattern> {
            delimited(tag("("), version_req, tag(")"))(input)
                .map(|(next, version_req)| (next, SegmentPattern::Version(version_req)))
        }

        fn space_segment(input: &str) -> Res<&str, SegmentPattern> {
            alt((inclusive_recursive_segment,inclusive_any_segment,recursive_segment, any_segment, exact_space_segment))(input)
        }

        fn base_segment(input: &str) -> Res<&str, SegmentPattern> {
            alt((recursive_segment, any_segment, exact_base_segment))(input)
        }

        fn file_segment(input: &str) -> Res<&str, SegmentPattern> {
            alt((recursive_segment, any_segment, exact_file_segment))(input)
        }

        fn dir_segment(input: &str) -> Res<&str, SegmentPattern> {
            terminated(
                alt((recursive_segment, any_segment, exact_dir_segment)),
                tag("/"),
            )(input)
        }

        fn version_segment(input: &str) -> Res<&str, SegmentPattern> {
            alt((
                recursive_segment,
                any_segment,
                exact_version_segment,
                version_req_segment,
            ))(input)
        }

        pub fn pattern<'r, O, E: ParseError<&'r str>, V>(
            mut value: V,
        ) -> impl FnMut(&'r str) -> IResult<&str, Pattern<O>, E>
        where
            V: Parser<&'r str, O, E>,
        {
            move |input: &str| {
                let x: Res<&str, &str> = tag("*")(input);
                match x {
                    Ok((next, _)) => Ok((next, Pattern::Any)),
                    Err(_) => {
                        let (next, p) = value.parse(input)?;
                        let pattern = Pattern::Exact(p);
                        Ok((next, pattern))
                    }
                }
            }
        }

        /*
        pub fn context<I: Clone, E: ContextError<I>, F, O>(
            context: &'static str,
            mut f: F,
        ) -> impl FnMut(I) -> IResult<I, O, E>
            where
                F: Parser<I, O, E>,
        {
            move |i: I| match f.parse(i.clone()) {
                Ok(o) => Ok(o),
                Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
                Err(Err::Error(e)) => Err(Err::Error(E::add_context(i, context, e))),
                Err(Err::Failure(e)) => Err(Err::Failure(E::add_context(i, context, e))),
            }
        }

         */
        pub fn value_pattern<I: Clone, O, E: ParseError<I>, F>(
            mut f: F,
        ) -> impl FnMut(I) -> IResult<I, ValuePattern<O>, E>
        where
            I: InputLength + InputTake + Compare<&'static str>,
            F: Parser<I, O, E>,
            E: nom::error::ContextError<I>,
        {
            move |input: I| match tag::<&'static str, I, E>("*")(input.clone()) {
                Ok((next, _)) => Ok((next, ValuePattern::Any)),
                Err(err) => f.parse(input.clone()).map( |(next,res)|(next, ValuePattern::Pattern(res))),
            }
        }
        /*
        pub fn value_pattern<E,F,O>(
            mut f: F
        ) -> impl Fn(&str) -> IResult<&str, ValuePattern<O>, E>
        where F: Parser<&'static str,O,E>, E: ContextError<&'static str> {
            move |input: &str| match tag::<&str,&'static str,ErrorTree<&'static str>>("*")(input) {
                Ok((next, _)) => Ok((next, ValuePattern::Any)),
                Err(err) => {
                    match f.parse(input.clone()) {
                        Ok((input,output)) => {Ok((input,ValuePattern::Pattern(output)))}
                        Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
                        Err(Err::Error(e)) => Err(Err::Error(E::add_context(input.clone(), "value_pattern", e))),
                        Err(Err::Failure(e)) => Err(Err::Failure(E::add_context(input.clone(), "value_pattern", e))),
                    }
                }
            }
        }

         */

        /*
        pub fn value_pattern<P>(
            parse: fn(input: &str) -> Res<&str, P>,
        ) -> impl Fn(&str) -> Res<&str, ValuePattern<P>> {
            move |input: &str| match tag::<&str, &str, VerboseError<&str>>("*")(input) {
                Ok((next, _)) => Ok((next, ValuePattern::Any)),
                Err(_) => {
                    let (next, p) = parse(input)?;
                    let pattern = ValuePattern::Pattern(p);
                    Ok((next, pattern))
                }
            }
        }
         */

        fn version_req(input: &str) -> Res<&str, VersionReq> {
            parse_from_str(version_req_chars).parse(input)
        }

        fn rec_domain(input: &str) -> Res<&str, &str> {
            recognize(tuple((
                many1(terminated(skewer_chars, tag("."))),
                skewer_chars,
            )))(input)
        }

        // can be a hostname or domain name
        fn space(input: &str) -> Res<&str, &str> {
            recognize(alt((skewer_chars, rec_domain)))(input)
        }

        pub fn specific_pattern(input: &str) -> Res<&str, SpecificPattern> {
            tuple((
                pattern(rec_domain),
                tag(":"),
                pattern(skewer_chars),
                tag(":"),
                pattern(skewer_chars),
                tag(":"),
                delimited(tag("("), version_req, tag(")")),
            ))(input)
            .map(|(next, (vendor, _, product, _, variant, _, version))| {
                let vendor: Pattern<&str> = vendor;
                let product: Pattern<&str> = product;
                let variant: Pattern<&str> = variant;

                let vendor: VendorPattern = vendor.into();
                let product: ProductPattern = product.into();
                let variant: VariantPattern = variant.into();

                let specific = SpecificPattern {
                    vendor,
                    product,
                    variant,
                    version,
                };
                (next, specific)
            })
        }

        fn kind_parts(input: &str) -> Res<&str, KindParts> {
            tuple((
                resource_type,
                opt(delimited(
                    tag("<"),
                    tuple((camel_case, opt(delimited(tag("<"), specific, tag(">"))))),
                    tag(">"),
                )),
            ))(input)
            .map(|(next, (resource_type, more))| {
                let mut parts = KindParts {
                    resource_type,
                    kind: None,
                    specific: None,
                };

                match more {
                    Some((kind, specific)) => {
                        parts.kind = Option::Some(kind.to_string());
                        parts.specific = specific;
                    }
                    None => {}
                }

                (next, parts)
            })
        }

        fn rec_kind(input: &str) -> Res<&str, &str> {
            recognize(kind_parts)(input)
        }

        pub fn kind(input: &str) -> Res<&str, ResourceKind> {
            parse_from_str(rec_kind)
                .parse(input)
                .map(|(next, kind)| (next, kind))
        }

        pub fn delim_kind(input: &str) -> Res<&str, ResourceKind> {
            delimited(tag("<"), kind, tag(">"))(input)
        }

        pub fn consume_kind(input: &str) -> Result<ResourceKind, MsgErr> {
            let (_, kind_parts) = all_consuming(kind_parts)(input)?;

            Ok(kind_parts.try_into()?)
        }

        pub fn kind_pattern(input: &str) -> Res<&str, KindPattern> {
            pattern(kind)(input).map(|(next, kind)| (next, kind))
        }

        pub fn resource_type(input: &str) -> Res<&str, ResourceType> {
            parse_from_str(camel_case).parse(input)
        }

        pub fn resource_type_pattern(input: &str) -> Res<&str, ResourceTypePattern> {
            pattern(resource_type)(input)
        }

        pub fn tks(input: &str) -> Res<&str, TksPattern> {
            delimited(
                tag("<"),
                tuple((
                    resource_type_pattern,
                    opt(delimited(
                        tag("<"),
                        tuple((
                            kind_pattern,
                            opt(delimited(
                                tag("<"),
                                value_pattern(specific_pattern),
                                tag(">"),
                            )),
                        )),
                        tag(">"),
                    )),
                )),
                tag(">"),
            )(input)
            .map(|(next, (resource_type, kind_and_specific))| {
                let (kind, specific) = match kind_and_specific {
                    None => (Pattern::Any, ValuePattern::Any),
                    Some((kind, specific)) => (
                        kind,
                        match specific {
                            None => ValuePattern::Any,
                            Some(specific) => specific,
                        },
                    ),
                };

                let tks = TksPattern {
                    resource_type,
                    kind,
                    specific,
                };

                (next, tks)
            })
        }

        fn space_hop(input: &str) -> Res<&str, Hop> {
            tuple((space_segment, opt(tks), opt(tag("+"))))(input).map(|(next, (segment, tks,inclusive))| {
                let tks = match tks {
                    None => TksPattern::any(),
                    Some(tks) => tks,
                };
                let inclusive = inclusive.is_some();
                (next, Hop { inclusive, segment, tks })
            })
        }

        fn base_hop(input: &str) -> Res<&str, Hop> {
            tuple((base_segment, opt(tks), opt(tag("+"))))(input).map(|(next, (segment, tks, inclusive))| {
                let tks = match tks {
                    None => TksPattern::any(),
                    Some(tks) => tks,
                };
                let inclusive = inclusive.is_some();
                (next, Hop { inclusive, segment, tks })
            })
        }

        fn file_hop(input: &str) -> Res<&str, Hop> {
            tuple((file_segment,opt(tag("+"))))(input).map(|(next, (segment,inclusive))| {
                let tks = TksPattern {
                    resource_type: Pattern::Exact("File".to_string()),
                    kind: Pattern::Any,
                    specific: ValuePattern::Any,
                };
                let inclusive = inclusive.is_some();
                (next, Hop { inclusive, segment, tks })
            })
        }

        fn dir_hop(input: &str) -> Res<&str, Hop> {
            tuple((dir_segment,opt(tag("+"))))(input).map(|(next, (segment,inclusive))| {
                let tks = TksPattern::any();
                let inclusive = inclusive.is_some();
                (next, Hop { inclusive, segment, tks })
            })
        }

        fn version_hop(input: &str) -> Res<&str, Hop> {
            tuple((version_segment, opt(tks), opt(tag("+"))))(input).map(|(next, (segment, tks, inclusive ))| {
                let tks = match tks {
                    None => TksPattern::any(),
                    Some(tks) => tks,
                };
                let inclusive = inclusive.is_some();
                (next, Hop { inclusive, segment, tks })
            })
        }

        pub fn address_kind_pattern(input: &str) -> Res<&str, AddressKindPattern> {
            context(
                "address_kind_pattern",
                tuple((
                    space_hop,
                    many0(preceded(tag(":"), base_hop)),
                    opt(preceded(tag(":"), version_hop)),
                    opt(preceded(tag(":/"), tuple((many0(dir_hop), opt(file_hop))))),
                )),
            )(input)
            .map(
                |(next, (space_hop, base_hops, version_hop, filesystem_hops))| {
                    let mut hops = vec![];
                    hops.push(space_hop);
                    for base_hop in base_hops {
                        hops.push(base_hop);
                    }
                    if let Option::Some(version_hop) = version_hop {
                        hops.push(version_hop);
                    }
                    if let Some((dir_hops, file_hop)) = filesystem_hops {
                        // first push the filesystem root
                        hops.push(Hop {
                            inclusive: false,
                            segment: SegmentPattern::Exact(ExactSegment::Address(
                                AddressSegment::FilesystemRootDir,
                            )),
                            tks: TksPattern {
                                resource_type: Pattern::Exact("Dir".to_string()),
                                kind: Pattern::Any,
                                specific: ValuePattern::Any,
                            },
                        });
                        for dir_hop in dir_hops {
                            hops.push(dir_hop);
                        }
                        if let Some(file_hop) = file_hop {
                            hops.push(file_hop);
                        }
                    }

                    let rtn = AddressKindPattern { hops };

                    (next, rtn)
                },
            )
        }

        pub fn address_and_kind(input: &str) -> Res<&str, AddressAndKind> {
            tuple((address, kind))(input)
                .map(|(next, (address, kind))| (next, AddressAndKind { address, kind }))
        }

        pub fn version(input: &str) -> Res<&str, Version> {
            parse_from_str(version_chars).parse(input)
        }

        pub fn specific(input: &str) -> Res<&str, Specific> {
            tuple((
                domain_chars,
                tag(":"),
                skewer_chars,
                tag(":"),
                skewer_chars,
                tag(":"),
                version,
            ))(input)
            .map(|(next, (vendor, _, product, _, variant, _, version))| {
                let specific = Specific {
                    vendor: vendor.to_string(),
                    product: product.to_string(),
                    variant: variant.to_string(),
                    version,
                };
                (next, specific)
            })
        }
    }

    pub fn skewer<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !((char_item.is_alpha() && char_item.is_lowercase())
                        || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn skewer_or_snake<T>(i: T) -> Res<T, T>
        where
            T: InputTakeAtPosition + nom::InputLength,
            <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-') &&
                !(char_item == '_')
                    && !((char_item.is_alpha() && char_item.is_lowercase())
                    || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }


    fn not_quote<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                (char_item == '"')
            },
            ErrorKind::AlphaNumeric,
        )
    }

    fn filename<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-') && !(char_item.is_alpha() || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub struct LabeledPrimitiveTypeDef {
        pub label: String,
        pub def: PrimitiveTypeDef,
    }

    pub struct PrimitiveTypeDef {
        pub primitive: PrimitiveType,
        pub format: Option<PayloadFormat>,
        pub verifier: Option<CallWithConfig>,
    }

    #[derive(Debug, Clone, strum_macros::Display, strum_macros::EnumString, Eq, PartialEq)]
    pub enum Format {
        #[strum(serialize = "json")]
        Json,
        #[strum(serialize = "image")]
        Image,
    }

    pub enum EntityPattern {
        Rc(RcPattern),
        Msg(MsgPattern),
        Http(HttpPattern),
    }

    impl ValueMatcher<RequestCore> for EntityPattern {
        fn is_match(&self, core: &RequestCore) -> Result<(), MsgErr> {
            match &core.action {
                Action::Rc(found) => {
                    if let EntityPattern::Rc(pattern) = self {
                        pattern.is_match(core)
                    } else {
                        Err(format!(
                            "Entity pattern mismatch. expected: '{}' found: '{}'",
                            self.to_string(),
                            found.to_string()
                        )
                        .into())
                    }
                }
                Action::Msg(found) => {
                    if let EntityPattern::Msg(pattern) = self {
                        pattern.is_match(core)
                    } else {
                        Err(format!(
                            "Entity pattern mismatch. expected: '{}' found: '{}'",
                            self.to_string(),
                            found.to_string()
                        )
                        .into())
                    }
                }
                Action::Http(found) => {
                    if let EntityPattern::Http(pattern) = self {
                        pattern.is_match(core)
                    } else {
                        Err(format!(
                            "Entity pattern mismatch. expected: '{}' found: '{}'",
                            self.to_string(),
                            found.to_string()
                        )
                        .into())
                    }
                }
            }
        }
    }

    impl ToString for EntityPattern {
        fn to_string(&self) -> String {
            match self {
                EntityPattern::Rc(rc) => rc.to_string(),
                EntityPattern::Msg(msg) => msg.to_string(),
                EntityPattern::Http(http) => http.to_string(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RcPattern {
        pub command: Pattern<RcCommandType>,
    }

    impl ValueMatcher<RequestCore> for RcPattern {
        fn is_match(&self, core: &RequestCore) -> Result<(), MsgErr> {
            if let Action::Rc(rc) = &core.action {
                if self.command.matches(&rc.get_type()) {
                    Ok(())
                } else {
                    Err("no match".into())
                }
            } else {
                Err("no match".into())
            }
        }
    }

    impl ToString for RcPattern {
        fn to_string(&self) -> String {
            format!("Rc<{}>", self.command.to_string())
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MsgPattern {
        pub action: ValuePattern<StringMatcher>,
        pub path_regex: String,
    }

    impl ToString for MsgPattern {
        fn to_string(&self) -> String {
            format!("Msg<{}>{}", self.action.to_string(), self.path_regex)
        }
    }

    impl ValueMatcher<RequestCore> for MsgPattern {
        fn is_match(&self, core: &RequestCore) -> Result<(), MsgErr> {
            if let Action::Msg(action) = &core.action {
                self.action.is_match(action)?;
                let matches = core.uri.path().matches(&self.path_regex);
                if matches.count() > 0 {
                    Ok(())
                } else {
                    Err(format!(
                        "Could not match Msg path: '{}' with: '{}'",
                        core.uri, self.path_regex
                    )
                    .into())
                }
            } else {
                Err("not a Msg Action".into())
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HttpPattern {
        pub method: MethodPattern,
        pub path_regex: String,
    }

    impl ToString for HttpPattern {
        fn to_string(&self) -> String {
            format!("Http<{}>{}", self.method.to_string(), self.path_regex)
        }
    }

    impl ValueMatcher<RequestCore> for HttpPattern {
        fn is_match(&self, found: &RequestCore) -> Result<(), MsgErr> {
            if let Action::Http(method) = &found.action {
                self.method.is_match(&method)?;

                let regex = Regex::new(self.path_regex.as_str())?;

                if regex.is_match(found.uri.to_string().as_str() ) {
                    Ok(())
                } else {
                    Err(format!(
                        "Could not match Msg path: '{}' with: '{}'",
                        found.uri, self.path_regex
                    )
                    .into())
                }
            } else {
                Err("action does not match".into())
            }
        }
    }

    pub fn primitive(input: &str) -> Res<&str, PrimitiveType> {
        parse_from_str(alpha1).parse(input)
    }

    pub fn format(input: &str) -> Res<&str, PayloadFormat> {
        parse_from_str(alpha1).parse(input)
    }

    pub fn primitive_def(input: &str) -> Res<&str, PrimitiveTypeDef> {
        tuple((
            primitive,
            opt(preceded(tag("~"), opt(format))),
            opt(preceded(tag("~"), call_with_config)),
        ))(input)
        .map(|(next, (primitive, format, verifier))| {
            (
                next,
                PrimitiveTypeDef {
                    primitive,
                    format: match format {
                        Some(Some(format)) => Some(format),
                        _ => Option::None,
                    },
                    verifier,
                },
            )
        })
    }

    pub fn consume_primitive_def(input: &str) -> Res<&str, PrimitiveTypeDef> {
        all_consuming(primitive_def)(input)
    }

    pub fn call_with_config(input: &str) -> Res<&str, CallWithConfig> {
        tuple((call, opt(preceded(tag("+"), Address::parse))))(input)
            .map(|(next, (call, config))| (next, CallWithConfig { call, config }))
    }

    pub fn rc_command(input: &str) -> Res<&str, RcCommandType> {
        parse_from_str(alpha1).parse(input)
    }

    /*
    pub fn rc_call_kind(input: &str) -> Res<&str, CallKind> {
        delimited(tag("Rc<"), rc_command, tag(">"))(input)
            .map(|(next, rc_command)| (next, CallKind::Rc(rc_command)))
    }

     */

    pub fn msg_call(input: &str) -> Res<&str, CallKind> {
        tuple((
            delimited(tag("Msg<"), alphanumeric1, tag(">")),
            opt(recognize(capture_path)),
        ))(input)
        .map(|(next, (action, path))| {
            let path = match path {
                None => "/",
                Some(path) => path,
            };
            (
                next,
                CallKind::Msg(MsgCall::new(action.to_string(), path.to_string())),
            )
        })
    }

    pub fn http_call(input: &str) -> Res<&str, CallKind> {
        tuple((
            delimited(tag("Http<"), http_method, tag(">")),
            capture_path,
        ))(input)
        .map(|(next, (method, path))| {
            (
                next,
                CallKind::Http(HttpCall::new(method, path.to_string())),
            )
        })
    }

    pub fn call_kind(input: &str) -> Res<&str, CallKind> {
        alt((msg_call, http_call))(input)
    }

    pub fn call(input: &str) -> Res<&str, Call> {
        tuple((capture_address, preceded(tag("^"), call_kind)))(input)
            .map(|(next, (address, kind))| (next, Call { address, kind }))
    }

    pub fn consume_call(input: &str) -> Res<&str, Call> {
        all_consuming(call)(input)
    }

    pub fn labeled_primitive_def(input: &str) -> Res<&str, LabeledPrimitiveTypeDef> {
        tuple((skewer, delimited(tag("<"), primitive_def, tag(">"))))(input).map(
            |(next, (label, primitive_def))| {
                let labeled_def = LabeledPrimitiveTypeDef {
                    label: label.to_string(),
                    def: primitive_def,
                };
                (next, labeled_def)
            },
        )
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum Block {
        Upload(UploadBlock),
        RequestPattern(PatternBlock),
        ResponsePattern(PatternBlock),
        CreatePayload(Payload),
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct UploadBlock {
        pub name: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct CreateBlock {
        pub payload: Payload,
    }

    pub type PatternBlock = ValuePattern<PayloadPattern>;

    pub fn digit_range(input: &str) -> Res<&str, Range> {
        tuple((digit1, tag("-"), digit1))(input).map(|(next, (min, _, max))| {
            let min: usize = usize::from_str(min).expect("usize");
            let max: usize = usize::from_str(max).expect("usize");
            let range = Range::MinMax { min, max };

            (next, range)
        })
    }

    pub fn exact_range(input: &str) -> Res<&str, Range> {
        digit1(input).map(|(next, exact)| {
            (
                next,
                Range::Exact(
                    usize::from_str(exact)
                        .expect("expect to be able to change digit string into usize"),
                ),
            )
        })
    }

    pub fn range(input: &str) -> Res<&str, Range> {
        delimited(
            multispace0,
            opt(alt((digit_range, exact_range))),
            multispace0,
        )(input)
        .map(|(next, range)| {
            let range = match range {
                Some(range) => range,
                None => Range::Any,
            };
            (next, range)
        })
    }

    pub fn primitive_data_struct(input: &str) -> Res<&str, PayloadTypePattern> {
        context( "selector",primitive)(input).map(|(next, primitive)| (next, PayloadTypePattern::Primitive(primitive)))
    }

    pub fn array_data_struct(input: &str) -> Res<&str, PayloadTypePattern> {
        context("selector",tuple((primitive, context("array",delimited(tag("["), range, tag("]"))))))(input).map(
            |(next, (primitive, range))| {
                (
                    next,
                    PayloadTypePattern::List(ListPattern { primitive, range }),
                )
            },
        )
    }

    pub fn map_entry_pattern_any(input: &str) -> Res<&str, ValuePattern<MapEntryPattern>> {
        delimited(multispace0, tag("*"), multispace0)(input)
            .map(|(next, _)| (next, ValuePattern::Any))
    }

    pub fn map_entry_pattern(input: &str) -> Res<&str, MapEntryPattern> {
        tuple((skewer, opt(delimited(tag("<"), payload_pattern, tag(">")))))(input).map(
            |(next, (key_con, payload_con))| {
                let payload_con = match payload_con {
                    None => ValuePattern::Any,
                    Some(payload_con) => payload_con,
                };

                let map_entry_con = MapEntryPattern {
                    key: key_con.to_string(),
                    payload: payload_con,
                };
                (next, map_entry_con)
            },
        )
    }

    pub fn map_entry_patterns(input: &str) -> Res<&str, Vec<MapEntryPattern>> {
        separated_list0(
            delimited(multispace0, tag(","), multispace0),
            map_entry_pattern,
        )(input)
    }

    pub fn consume_map_entry_pattern(input: &str) -> Res<&str, MapEntryPattern> {
        all_consuming(map_entry_pattern)(input)
    }

    pub fn required_map_entry_pattern(input: &str) -> Res<&str, Vec<MapEntryPattern>> {
        delimited(tag("["), map_entry_patterns, tag("]"))(input)
            .map(|(next, params)| (next, params))
    }

    pub fn allowed_map_entry_pattern(input: &str) -> Res<&str, ValuePattern<PayloadPattern>> {
        payload_pattern(input).map(|(next, con)| (next, con))
    }

    //  [ required1<Bin>, required2<Text> ] *<Bin>
    pub fn map_pattern_params(input: &str) -> Res<&str, MapPattern> {
        tuple((
            opt(map_entry_patterns),
            multispace0,
            opt(allowed_map_entry_pattern),
        ))(input)
        .map(|(next, (required, _, allowed))| {
            let mut required_map = HashMap::new();
            match required {
                Option::Some(required) => {
                    for require in required {
                        required_map.insert(require.key, require.payload);
                    }
                }
                Option::None => {}
            }

            let allowed = match allowed {
                Some(allowed) => allowed,
                None => ValuePattern::None,
            };

            let con = MapPattern::new(required_map, allowed);

            (next, con)
        })
    }

    enum MapConParam {
        Required(Vec<ValuePattern<MapEntryPattern>>),
        Allowed(ValuePattern<PayloadPattern>),
    }

    // EXAMPLE:
    //  Map { [ required1<Bin>, required2<Text> ] *<Bin> }
    pub fn map_pattern(input: &str) -> Res<&str, MapPattern> {
        tuple((
            delimited(multispace0, tag("Map"), multispace0),
            opt(delimited(
                tag("{"),
                delimited(multispace0, map_pattern_params, multispace0),
                tag("}"),
            )),
        ))(input)
        .map(|(next, (_, entries))| {
            let mut entries = entries;
            let con = match entries {
                None => MapPattern::any(),
                Some(con) => con,
            };

            (next, con)
        })
    }

    /*
    fn value_pattern<I,O,E>(input: I) -> IResult<I, ValuePattern<O>, E>
        where
            I: InputTake + Clone + Compare<I>+ InputTakeAtPosition, <I as InputTakeAtPosition>::Item: AsChar + Clone,
            E: ParseError<I>

    {
        alt((tag("*"),multispace0))(input).map( |(next,tag):(&str,&str)|{
            let rtn = match tag{
                "*" => ValuePattern::Any,
                _ => ValuePattern::None
            };
            (next,rtn)
        })
    }*/

    /*
        pub fn value_pattern<V>(
            input: &str,
            parser: fn(&str) -> Res<&str, V>,
        ) -> Res<&str, ValuePattern<V>> {
            let result = context( "value_pattern",parser)(input);
            let result = match result {
                Ok((next, v)) => {
                    return Ok((next, ValuePattern::Pattern(v)));
                }
                Err(err) => {
    println!("ERRROR!");
                    IResult::Err(err)
                }
            };
            let pattern_result = context("value_pattern",alt((nom_supreme::tag::complete::tag("*"), nom_supreme::tag::complete::tag("!"))))(input);
            match pattern_result {
                Ok((next,tag)) => {
                    match tag {
                        "*" => Ok((next,ValuePattern::Any)),
                        "!" => Ok((next,ValuePattern::None)),
                        _ => {
                            return result;
                        }
                    }
                }
                Err(err) => {
                    Err(Err::Error(E::add_context(i, "value_pattern", err)))
                }
            }
        }

         */

    /*
    pub fn value_pattern_wrapper<'a,'b,V,F>( mut parser: F ) -> impl FnMut(&'a str) -> Res<&'b str,ValuePattern<V>>
      where F: 'a+ Parser<&'a str,ValuePattern<V>,VerboseError<&'b str>>
    {
        move |input: &str| {
            parser.parse(input)
        }
    }

     */

    /*
    pub fn value_pattern_wrapper<I:Clone, O, E: ParseError<I>, F>(
        mut first: F,
    ) -> impl FnMut(I) -> IResult<I, ValuePattern<O>, E>

        where
            F: Parser<I, ValuePattern<O>, E>,
            I: InputTake + Clone + Compare<I>+ InputTakeAtPosition, <I as InputTakeAtPosition>::Item: AsChar + Clone
    {
        move |input: I| {
            //let result1 = value_pattern(input.clone());
            first.parse(input)//.or(result1)
        }
    }

     */

    /*
    pub fn value_pattern_wrapper<O>(
        mut parser: F,
    ) -> impl FnMut(&str) -> Res<&str,ValuePattern<O>>
        where
            F: Parser<Res<&str,ValyePattern<O>>>,
    {
        move |input: &str| {
            match parser.parse(input ).or( ) {
                Ok((i, out)) => {
                    Ok((i, ValuePattern::Pattern(out)))
                }
                Err(e) => {
                    value_pattern::<O>(input)
                }
            }
        }
    }

     */

    pub fn value_constrained_map_pattern(input: &str) -> Res<&str, ValuePattern<MapPattern>> {
        value_pattern(map_pattern)(input)
    }

    pub fn msg_action(input: &str) -> Res<&str, ValuePattern<StringMatcher>> {
        value_pattern(camel_case_to_string_matcher)(input)
    }

    pub fn msg_pattern_scoped(input: &str) -> Res<&str, MsgPattern> {
        tuple((delimited(tag("<"), msg_action, tag(">")), opt(path_regex)))(input).map(
            |(next, (action, path_regex))| {
                let path_regex = match path_regex {
                    None => "*".to_string(),
                    Some(path_regex) => path_regex.to_string(),
                };
                let rtn = MsgPattern {
                    action,
                    path_regex: path_regex.to_string(),
                };
                (next, rtn)
            },
        )
    }

    pub fn msg_pattern(input: &str) -> Res<&str, MsgPattern> {
        tuple((
            tag("Msg"),
            delimited(tag("<"), msg_action, tag(">")),
            opt(path_regex),
        ))(input)
        .map(|(next, (_, action, path_regex))| {
            let path_regex = match path_regex {
                None => "*".to_string(),
                Some(path_regex) => path_regex.to_string(),
            };
            let rtn = MsgPattern {
                action,
                path_regex: path_regex.to_string(),
            };
            (next, rtn)
        })
    }

    pub fn http_method(input: &str) -> Res<&str, HttpMethod> {
        context("http_method", parse_from_str(camel_case )).parse(input).map( |(next,method):(&str,HttpMethodType)| {
            (next,method.to_method())
        })
    }

    pub fn http_method_pattern(input: &str) -> Res<&str, MethodPattern> {
        context("@http_method_pattern",method_pattern(http_method))(input)
    }

    pub fn method_pattern<I: Clone, E: ParseError<I>, F>(
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, MethodPattern, E>
        where
            I: InputLength + InputTake + Compare<&'static str>,
            F: Parser<I, HttpMethod, E>,
            E: nom::error::ContextError<I>,
    {
        move |input: I| match tag::<&'static str, I, E>("*")(input.clone()) {
            Ok((next, _)) => Ok((next, MethodPattern::Any)),
            Err(err) => f.parse(input.clone()).map( |(next,res)|(next, MethodPattern::Pattern(res))),
        }
    }

    pub fn http_pattern_scoped(input: &str) -> Res<&str, HttpPattern> {
        tuple((
            delimited(context("angle_bracket_open",tag("<")), http_method_pattern, context("angle_bracket_close",tag(">"))),
            opt(path_regex),
        ))(input)
        .map(|(next, (method, path_regex))| {
            let path_regex = match path_regex {
                None => "*".to_string(),
                Some(path_regex) => path_regex.to_string(),
            };
            let rtn = HttpPattern {
                method,
                path_regex: path_regex.to_string(),
            };
            (next, rtn)
        })
    }

    pub fn http_pattern(input: &str) -> Res<&str, HttpPattern> {
        tuple((
            tag("Http"),
            delimited(tag("<"), http_method_pattern, tag(">")),
            opt(path_regex),
        ))(input)
        .map(|(next, (_, method, path_regex))| {
            let path_regex = match path_regex {
                None => "*".to_string(),
                Some(path_regex) => path_regex.to_string(),
            };
            let rtn = HttpPattern {
                method,
                path_regex: path_regex.to_string(),
            };
            (next, rtn)
        })
    }

    pub fn rc_command_type(input: &str) -> Res<&str, RcCommandType> {
        parse_from_str(alpha1).parse(input)
    }

    pub fn rc_pattern_scoped(input: &str) -> Res<&str, RcPattern> {
        pattern(delimited(tag("<"), rc_command_type, tag(">")))(input)
            .map(|(next, command)| (next, RcPattern { command }))
    }

    pub fn rc_pattern(input: &str) -> Res<&str, RcPattern> {
        tuple((tag("Rc"), delimited(tag("<"), rc_pattern_scoped, tag(">"))))(input)
            .map(|(next, (_, pattern))| (next, pattern))
    }

    pub fn map_pattern_payload_structure(input: &str) -> Res<&str, PayloadTypePattern> {
        map_pattern(input).map(|(next, con)| (next, PayloadTypePattern::Map(Box::new(con))))
    }

    pub fn payload_structure(input: &str) -> Res<&str, PayloadTypePattern> {
        alt((
            array_data_struct,
            primitive_data_struct,
            map_pattern_payload_structure,
        ))(input)
    }

    pub fn msg_entity_pattern(input: &str) -> Res<&str, EntityPattern> {
        msg_pattern(input).map(|(next, pattern)| (next, EntityPattern::Msg(pattern)))
    }
    pub fn http_entity_pattern(input: &str) -> Res<&str, EntityPattern> {
        http_pattern(input).map(|(next, pattern)| (next, EntityPattern::Http(pattern)))
    }

    pub fn rc_entity_pattern(input: &str) -> Res<&str, EntityPattern> {
        rc_pattern(input).map(|(next, pattern)| (next, EntityPattern::Rc(pattern)))
    }

    pub fn entity_pattern(input: &str) -> Res<&str, EntityPattern> {
        alt((msg_entity_pattern, http_entity_pattern, rc_entity_pattern))(input)
    }

    pub fn payload_structure_with_validation(input: &str) -> Res<&str, PayloadPattern> {
        tuple((
            context("selector",payload_structure),
            opt(preceded(tag("~"), opt(format))),
            opt(preceded(tag("~"), call_with_config)),
        ))(input)
        .map(|(next, (data, format, verifier))| {
            (
                next,
                PayloadPattern {
                    structure: data,
                    format: match format {
                        Some(Some(format)) => Some(format),
                        _ => Option::None,
                    },
                    validator: verifier,
                },
            )
        })
    }

    pub fn consume_payload_structure(input: &str) -> Res<&str, PayloadTypePattern> {
        all_consuming(payload_structure)(input)
    }

    pub fn consume_data_struct_def(input: &str) -> Res<&str, PayloadPattern> {
        all_consuming(payload_structure_with_validation)(input)
    }

    pub fn payload_pattern_any(input: &str) -> Res<&str, ValuePattern<PayloadPattern>> {
        tag("*")(input).map(|(next, _)| (next, ValuePattern::Any))
    }

    pub fn payload_pattern(input: &str) -> Res<&str, ValuePattern<PayloadPattern>> {
        context("@payload-pattern", value_pattern(payload_structure_with_validation))(input)
            .map(|(next, payload_pattern)| (next, payload_pattern))
    }

    /*
    pub fn payload_patterns(input: &str) -> Res<&str, ValuePattern<PayloadPattern>> {
        alt((tag("*"), recognize(payload_structure_with_validation)))(input).map(|(next, data)| {
            let data = match data {
                "*" => ValuePattern::Any,
                exact => ValuePattern::Pattern(
                    payload_structure_with_validation(input)
                        .expect("recognize already passed this...")
                        .1,
                ),
            };
            (next, data)
        })
    }

     */

    pub fn payload_filter_block_empty(input: &str) -> Res<&str, PatternBlock> {
        multispace0(input).map(|(next, _)| (input, PatternBlock::None))
    }

    pub fn payload_filter_block_any(input: &str) -> Res<&str, PatternBlock> {
        let (next, _) = delimited(multispace0, context("selector",tag("*")), multispace0)(input)?;

        Ok((next, PatternBlock::Any))
    }

    pub fn payload_filter_block_def(input: &str) -> Res<&str, PatternBlock> {

        payload_structure_with_validation(input)
            .map(|(next, pattern)| (next, PatternBlock::Pattern(pattern)))
    }

    fn insert_block_pattern(input: &str) -> Res<&str, UploadBlock> {
        delimited(multispace0, filename, multispace0)(input).map(|(next, filename)| {
            (
                next,
                UploadBlock {
                    name: filename.to_string(),
                },
            )
        })
    }

    pub fn text_payload_block(input: &str) -> Res<&str, Block> {
        delimited(
            tag("+["),
            tuple((
                multispace0,
                delimited(tag("\""), not_quote, tag("\"")),
                multispace0,
            )),
            tag("]"),
        )(input)
        .map(|(next, (_, text, _))| {
            (
                next,
                Block::CreatePayload(Payload::Primitive(Primitive::Text(text.to_string()))),
            )
        })
    }

    pub fn upload_payload_block(input: &str) -> Res<&str, Block> {
        delimited(
            tag("^["),
            tuple((multispace0, file_chars, multispace0)),
            tag("]"),
        )(input)
        .map(|(next, (_, filename, _))| {
            (
                next,
                Block::Upload(UploadBlock {
                    name: filename.to_string(),
                }),
            )
        })
    }

    pub fn upload_step(input: &str) -> Res<&str, UploadBlock> {
        terminated(upload_payload_block, tag("->"))(input).map(|(next, block)| {
            if let Block::Upload(block) = block {
                (next, block)
            } else {
                panic!("it should have been an UploadBlock!");
            }
        })
    }

    pub fn request_payload_filter_block(input: &str) -> Res<&str, Block> {
        context("request-payload-filter-block",
        terminated(
            tuple((
                multispace0,
                alt((payload_filter_block_any, payload_filter_block_def, payload_filter_block_empty, fail)),
                multispace0,
            )),
            tag("]"),
        ))(input)
        .map(|(next, (_, block, _))| (next, Block::RequestPattern(block)))
    }

    pub fn response_payload_filter_block(input: &str) -> Res<&str, Block> {
        context("response-payload-filter-block",
                terminated(
                    tuple((
                        multispace0,
                        alt((payload_filter_block_any, payload_filter_block_def, payload_filter_block_empty, fail)),
                        multispace0,
                    )),
                    tag("]"),
                ))(input)
            .map(|(next, (_, block, _))| (next, Block::ResponsePattern(block)))
    }

    pub fn pipeline_step_block(input: &str) -> Res<&str, Block> {
        let request = request_payload_filter_block as for<'r> fn(&'r str) -> Result<(&'r str, Block), nom::Err<ErrorTree<&'r str>>>;
        let response = response_payload_filter_block as for<'r> fn(&'r str) -> Result<(&'r str, Block), nom::Err<ErrorTree<&'r str>>>;
        let upload = upload_payload_block as for<'r> fn(&'r str) -> Result<(&'r str, Block), nom::Err<ErrorTree<&'r str>>>;
        context( "pipeline-step-block", select_block(vec!(
            SelectBlock(tag("-["), request),
            SelectBlock(tag("=["), response),
            SelectBlock(tag("^["),upload),
        )))(input)
    }

    pub fn consume_pipeline_block(input: &str) -> Res<&str, Block> {
        all_consuming(pipeline_step_block)(input)
    }

    #[derive(Clone, Eq, PartialEq)]
    pub struct MapEntryPattern {
        pub key: String,
        pub payload: ValuePattern<PayloadPattern>,
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct Hop {
        pub inclusive: bool,
        pub segment: SegmentPattern,
        pub tks: TksPattern,
    }



    impl Hop {
        pub fn matches(&self, address_kind_segment: &AddressKindSegment) -> bool {
            self.segment.matches(&address_kind_segment.address_segment) && self.tks.matches(&address_kind_segment.kind)
        }
    }

    impl ToString for Hop {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            rtn.push_str( self.segment.to_string().as_str() );

            if let Pattern::Exact(resource_type) = &self.tks.resource_type {
                rtn.push_str(format!("<{}", resource_type.to_string()).as_str());
                if let Pattern::Exact(kind) = &self.tks.kind {
                    rtn.push_str(format!("<{}", kind.to_string()).as_str());
                    if let ValuePattern::Pattern(specific) = &self.tks.specific {
                        rtn.push_str(format!("<{}", specific.to_string()).as_str());
                        rtn.push_str(">");
                    }
                    rtn.push_str(">");
                }
                rtn.push_str(">");
            }

            if self.inclusive {
                rtn.push_str("+");
            }

            rtn
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub enum Pattern<P> {
        Any,
        Exact(P),
    }

    impl<P> Pattern<P>
    where
        P: Eq + PartialEq,
    {

        pub fn is_any(&self) -> bool {
            match self {
                Pattern::Any => true,
                Pattern::Exact(_) => false
            }
        }


        pub fn matches(&self, t: &P) -> bool {
            match self {
                Self::Any => true,
                Self::Exact(p) => *p == *t,
            }
        }
        pub fn matches_opt(&self, other: Option<&P>) -> bool {
            match self {
                Self::Any => true,
                Self::Exact(exact) => {
                    if let Option::Some(other) = other {
                        *exact == *other
                    } else {
                        false
                    }
                }
            }
        }

        pub fn convert<To>(self) -> Result<Pattern<To>, MsgErr>
        where
            P: TryInto<To, Error =MsgErr> + Eq + PartialEq,
        {
            Ok(match self {
                Pattern::Any => Pattern::Any,
                Pattern::Exact(exact) => Pattern::Exact(exact.try_into()?),
            })
        }
    }

    /*
    impl <From,Into> Pattern<From> where From: TryInto<Into>{

        fn convert(self) -> Result<Pattern<Into>, Error> {
            Ok( match self {
                Pattern::Any => {Pattern::Any}
                Pattern::Exact(from) => {
                    Pattern::Exact(from.try_into()?)
                }
            })
        }
    }

     */

    impl Into<Pattern<String>> for Pattern<&str> {
        fn into(self) -> Pattern<String> {
            match self {
                Pattern::Any => Pattern::Any,
                Pattern::Exact(f) => Pattern::Exact(f.to_string()),
            }
        }
    }

    impl<P> ToString for Pattern<P>
    where
        P: ToString,
    {
        fn to_string(&self) -> String {
            match self {
                Pattern::Any => "*".to_string(),
                Pattern::Exact(exact) => exact.to_string(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum EmptyPattern<P> {
        Any,
        Pattern(P),
    }

    impl<P> EmptyPattern<P>
    where
        P: Eq + PartialEq,
    {
        pub fn matches(&self, t: &P) -> bool {
            match self {
                Self::Any => true,
                Self::Pattern(p) => *p == *t,
            }
        }
        pub fn matches_opt(&self, other: Option<&P>) -> bool {
            match self {
                Self::Any => true,
                Self::Pattern(exact) => {
                    if let Option::Some(other) = other {
                        *exact == *other
                    } else {
                        false
                    }
                }
            }
        }

        pub fn convert<To>(self) -> Result<EmptyPattern<To>, MsgErr>
        where
            P: TryInto<To, Error =MsgErr> + Eq + PartialEq,
        {
            Ok(match self {
                EmptyPattern::Any => EmptyPattern::Any,
                EmptyPattern::Pattern(exact) => EmptyPattern::Pattern(exact.try_into()?),
            })
        }
    }

    impl Into<EmptyPattern<String>> for EmptyPattern<&str> {
        fn into(self) -> EmptyPattern<String> {
            match self {
                EmptyPattern::Any => EmptyPattern::Any,
                EmptyPattern::Pattern(f) => EmptyPattern::Pattern(f.to_string()),
            }
        }
    }

    impl<P> ToString for EmptyPattern<P>
    where
        P: ToString,
    {
        fn to_string(&self) -> String {
            match self {
                EmptyPattern::Any => "".to_string(),
                EmptyPattern::Pattern(exact) => exact.to_string(),
            }
        }
    }

    pub type ResourceTypePattern = Pattern<ResourceType>;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct AddressKindPath {
        pub route: RouteSegment,
        pub segments: Vec<AddressKindSegment>,
    }

    impl AddressKindPath {
        pub fn new(route: RouteSegment, segments: Vec<AddressKindSegment>) -> Self {
            Self { route, segments }
        }

        pub fn push(&self, segment: AddressKindSegment) -> AddressKindPath
        where
            ResourceKind: Clone,
            ResourceType: Clone,
        {
            if let AddressSegment::Root = segment.address_segment {
                println!("pushing ROOT");
            }
            let mut segments = self.segments.clone();
            segments.push(segment);
            Self {
                route: self.route.clone(),
                segments,
            }
        }

        pub fn consume(&self) -> Option<AddressKindPath> {
            if self.segments.len() <= 1 {
                return Option::None;
            }
            let mut segments = self.segments.clone();
            segments.remove(0);
            Option::Some(AddressKindPath {
                route: self.route.clone(),
                segments,
            })
        }

        pub fn is_root(&self) -> bool {
            self.segments.is_empty()
        }

        pub fn is_final(&self) -> bool {
            self.segments.len() == 1
        }
    }

    impl ToString for AddressKindPath {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            match &self.route {
                RouteSegment::Local => {}
                route => {
                    rtn.push_str(route.to_string().as_str());
                    rtn.push_str("::");
                }
            }

            for (index, segment) in self.segments.iter().enumerate() {
                rtn.push_str(segment.to_string().as_str());
                if index < self.segments.len()-1 {
                    rtn.push_str(segment.address_segment.terminating_delim());
                }
            }

            rtn
        }
    }

    #[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
    pub struct AddressKindSegment {
        pub address_segment: AddressSegment,
        pub kind: ResourceKind,
    }

    impl ToString for AddressKindSegment {
        fn to_string(&self) -> String {
            format!(
                "{}<{}>",
                self.address_segment.to_string(),
                self.kind.to_string()
            )
        }
    }

    impl FromStr for AddressKindPath {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(consume_address_kind_path(s)?)
        }
    }
}

pub mod messaging {
    use std::collections::HashMap;
    use std::convert::TryInto;

    use serde::{Deserialize, Serialize};
    use http::StatusCode;

    use crate::error::{MsgErr, StatusErr};
    use crate::version::v0_0_1::entity::request::RequestCore;
    use crate::version::v0_0_1::entity::response::ResponseCore;
    use crate::version::v0_0_1::id::Address;
    use crate::version::v0_0_1::pattern::{AddressKindPath, AddressKindPattern};
    use crate::version::v0_0_1::payload::{Errors, MsgCall, Payload, Primitive};
    use crate::version::v0_0_1::security::{Access, AccessGrant, EnumeratedAccess, Permissions, Privilege, EnumeratedPrivileges, Privileges};
    use crate::version::v0_0_1::util::unique_id;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Request {
        pub id: String,
        pub agent: Agent,
        pub session: Option<Session>,
        pub scope: Scope,
        pub handling: Handling,
        pub from: Address,
        pub to: Address,
        pub core: RequestCore,
    }

    impl Request {

        pub fn result<E:StatusErr>(self, result: Result<ResponseCore, E> ) -> Response {
            match result {
                Ok(core) => {
                    Response {
                        id: unique_id(),
                        to: self.from,
                        from: self.to,
                        core,
                        response_to: self.id
                    }
                }
                Err(err) => {
                    let core = self.core.err(err);
                    Response {
                        id: unique_id(),
                        to: self.from,
                        from: self.to,
                        core,
                        response_to: self.id
                    }
                }
            }
        }

        pub fn payload_result<E: StatusErr>(self, result: Result<Payload, E> ) -> Response {
            match result {
                Ok(payload) => {
                    self.ok_payload(payload)
                }
                Err(err) => {
                    let core = self.core.err(err);
                    Response {
                        id: unique_id(),
                        to: self.from,
                        from: self.to,
                        core,
                        response_to: self.id
                    }
                }
            }
        }

        pub fn to_call(&self) -> MsgCall {
            MsgCall{
                path: self.core.uri.to_string(),
                action: self.core.action.to_string()
            }
        }
    }

    impl Request {
        pub fn new(core: RequestCore, from: Address, to: Address) -> Self {
            Self {
                id: unique_id(),
                agent: Agent::Anonymous,
                session: Option::None,
                scope: Scope::Full,
                handling: Default::default(),
                from,
                to,
                core,
            }
        }

        /*
        pub fn result<E>(self, result: Result<ResponseCore,E> ) -> Response where E: ToString {
            match result {
                Ok(core) => {
                    Response {
                        id: unique_id(),
                        to: self.from,
                        from: self.to,
                        core,
                        response_to: self.id
                    }
                }
                Err(err) => {
                    self.fail(err.to_string().as_str())
                }
            }
        }

        pub fn payload_result<E>(self, result: Result<Payload,E> ) -> Response where E: ToString {
            match result {
                Ok(payload) => {
                    self.ok_payload(payload)
                }
                Err(err) => {
                    self.fail(err.to_string().as_str())
                }
            }
        }

         */

        pub fn ok(self) -> Response {
            let core = ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(200u16 ).unwrap(),
                body: Payload::Empty,
            };
            let response = Response {
                id: unique_id(),
                from: self.to,
                to: self.from,
                core,
                response_to: self.id,
            };
            response
        }

        pub fn ok_payload(self, payload: Payload) -> Response {
            let core = ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(200u16).unwrap(),
                body: payload
            };
            let response = Response {
                id: unique_id(),
                from: self.to,
                to: self.from,
                core,
                response_to: self.id,
            };
            response
        }

        pub fn fail(self, error: &str) -> Response {
            let core = ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(500u16).unwrap(),
                body: Payload::Primitive(Primitive::Errors(Errors::default(
                    error.to_string().as_str(),
                ))),
            };
            let response = Response {
                id: unique_id(),
                from: self.to,
                to: self.from,
                core,
                response_to: self.id,
            };
            response
        }

        pub fn not_found(self) -> Response {
            let core = ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(404u16).unwrap(),
                body: Payload::Empty,
            };
            let response = Response {
                id: unique_id(),
                from: self.to,
                to: self.from,
                core,
                response_to: self.id,
            };
            response
        }


        pub fn status(self, status: u16 ) -> Response {
            fn process(request: &Request, status: u16) -> Result<Response,http::status::InvalidStatusCode> {
                let core = ResponseCore {
                    headers: Default::default(),
                    status: StatusCode::from_u16(status)?,
                    body: Payload::Empty
                };
                let response = Response {
                    id: unique_id(),
                    from: request.to.clone(),
                    to: request.from.clone(),
                    core,
                    response_to: request.id.clone(),
                };
                Ok(response)
            }
            match process(&self, status) {
                Ok(response) => response,
                Err(err) => self.fail(format!("bad status: {}", status).as_str() )
            }
        }
    }

    pub struct RequestBuilder {
        pub to: Option<Address>,
        pub from: Option<Address>,
        pub core: Option<RequestCore>,
        pub agent: Agent,
        pub session: Option<Session>,
        pub scope: Scope,
        pub handling: Handling,
    }

    impl RequestBuilder {
        pub fn new() -> Self {
            Self {
                ..Default::default()
            }
        }

        pub fn to( mut self, address: Address ) -> Self {
            self.to = Some(address);
            self
        }

        pub fn from( mut self, address: Address ) -> Self {
            self.from = Some(address);
            self
        }

        pub fn core( mut self, core: RequestCore ) -> Self {
            self.core = Some(core);
            self
        }

        pub fn agent( mut self, agent: Agent ) -> Self {
            self.agent = agent;
            self
        }

        pub fn session( mut self, session: Session ) -> Self {
            self.session = Some(session);
            self
        }

        pub fn scope( mut self, scope: Scope) -> Self {
            self.scope = scope;
            self
        }

        pub fn handling( mut self, handling: Handling ) -> Self {
            self.handling = handling;
            self
        }

        pub fn build(self) -> Result<Request,MsgErr> {
            Ok(Request {
                id: unique_id(),
                to: self.to.ok_or("RequestBuilder: 'to' must be set")?,
                from: self.from.ok_or("RequestBuilder: 'from' must be set")?,
                core: self.core.ok_or("RequestBuilder: 'core' must be set")?,
                agent: self.agent,
                session: self.session,
                scope: self.scope,
                handling: self.handling
            })
        }

    }

    impl Default for RequestBuilder {
        fn default() -> Self {
            Self {
                to: None,
                from: None,
                core: None,
                agent: Default::default(),
                session: None,
                scope: Default::default(),
                handling: Default::default()
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ProtoRequest {
        pub id: String,
        pub to: Option<Address>,
        pub core: Option<RequestCore>,
    }

    impl ProtoRequest {
        pub fn new() -> Self {
            Self {
                id: unique_id(),
                to: Option::None,
                core: Option::None,
            }
        }

        pub fn validate(&self) -> Result<(), MsgErr> {
            self.to.as_ref().ok_or("request.to must be set")?;
            Ok(())
        }

        pub fn to(&mut self, to: Address) {
            self.to = Option::Some(to);
        }

        pub fn core(&mut self, core: RequestCore) {
            self.core = Option::Some(core);
        }

        pub fn into_request(self, from: Address, agent: Agent, session: Option<Session>, scope: Scope) -> Result<Request, MsgErr> {
            self.validate()?;
            let core = self
                .core
                .or(Option::Some(Default::default()))
                .expect("expected RequestCore");
            let request = Request {
                id: self.id,
                from,
                to: self.to.expect("expected to address"),
                core,
                agent,
                session,
                handling: Default::default(),
                scope
            };
            Ok(request)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Response {
        pub id: String,
        pub from: Address,
        pub to: Address,
        pub core: ResponseCore,
        pub response_to: String,
    }

    impl Response {
        pub fn new(core: ResponseCore, from: Address, to: Address, response_to: String) -> Self {
            Self {
                id: unique_id(),
                to,
                from,
                core,
                response_to,
            }
        }

        pub fn ok_or(self) -> Result<Self, MsgErr> {
            if self.core.status.is_success() {
                Ok(self)
            } else {
                if let Payload::Primitive(Primitive::Text(error)) = self.core.body {
                    Err(error.into())
                } else {
                    Err(format!("error code: {}", self.core.status).into())
                }
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Message {
        Request(Request),
        Response(Response),
    }

    impl Message {
        pub fn payload(&self) -> Payload {
            match self {
                Message::Request(request) => request.core.body.clone(),
                Message::Response(response) => response.core.body.clone(),
            }
        }

        pub fn to(&self) -> Address {
            match self {
                Message::Request(request) => request.to.clone(),
                Message::Response(response) => response.to.clone(),
            }
        }
    }

    impl From<Request> for Message {
        fn from(request: Request) -> Self {
            Self::Request(request)
        }
    }

    impl From<Response> for Message {
        fn from(response: Response) -> Self {
            Self::Response(response)
        }
    }


    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum RequestTransform {
        Request(RequestCore),
        Response(ResponseCore)
    }

    pub enum ResponseKindExpected {
        None,
        Synch, // requestor will wait for response
        Async(Payload) // The payload
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Agent {
        Anonymous,
        Authenticated(AuthedAgent)
    }

    impl Default for Agent {
        fn default() -> Self {
            Self::Anonymous
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct AuthedAgent {
        pub owner: Address,
        pub executor: Address
    }

    impl AuthedAgent {
        pub fn new(address: Address) -> Self {
            Self {
                owner: address.clone(),
                executor: address
            }
        }
    }

    impl TryInto<AuthedAgent> for Agent {
        type Error = MsgErr;

        fn try_into(self) -> Result<AuthedAgent, Self::Error> {
            match self {
                Agent::Anonymous => Err(MsgErr::new(401, "Authorization required")),
                Agent::Authenticated(auth) => Ok(auth)
            }
        }
    }

    impl Into<Agent> for AuthedAgent {
        fn into(self) -> Agent {
            Agent::Authenticated(self)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Session {
       pub id: String,
       pub attributes: HashMap<String,String>
    }

    impl Session {
        pub fn get_preferred_username(&self) -> Option<String> {
            self.attributes.get(&"preferred_username".to_string()).cloned()
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Scope {
        Full,
        None,
        Grants(Vec<ScopeGrant>),
    }

    impl Scope {
        pub fn mask( &self, on: &AddressKindPath ) -> Access {
            match self {
                Scope::Full => {
                    access.clone()
                }
                Scope::None => {
                    Access::none()
                }
                Scope::Grants(grants) => {
                    let mut access  = access.clone();
                    let mut privileges = EnumeratedPrivileges::none();
                    let mut permissions = Permissions::full();
                    for grant in grants {
                       if grant.on.matches(on) {
                           match &grant.aspect {
                               ScopeGrantAspect::Perm(and) => permissions.and(and),
                               ScopeGrantAspect::Priv(and) =>  privileges.insert(and.clone())
                           }
                       }
                   }
                }
            }
        }
    }

    impl Default for Scope {
        fn default() -> Self {
            Self::None
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ScopeGrant {
        pub on: AddressKindPattern,
        pub kind: ScopeGrantKind,
        pub aspect: ScopeGrantAspect
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ScopeGrantKind{
        Or,
        And
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ScopeGrantAspect {
        Perm(Permissions),
        Priv(Privilege)
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RequestAccess {
        pub permissions: Permissions,
        pub privileges:  Privileges,
    }


    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Roles{
        Full,
        None,
        Enumerated(Vec<String>)
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Handling {
        kind: HandlingKind,
        priority: Priority,
        retries: Retries,
        timeout: Timeout
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum HandlingKind {
        Durable,  // Mesh will guarantee delivery eventually once Request call has returned
        Queued,   // Slower but more reliable delivery, message can be lost if a star crashes, etc
        Immediate // Message should never touch a filesystem, it will be in memory for its entire journey for immediate processing
    }

    impl Default for Handling {
        fn default() -> Self {
            Self {
                kind: HandlingKind::Queued,
                priority: Default::default(),
                retries: Default::default(),
                timeout: Default::default()
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Timeout {
        Never,
        Max,
        Medium,
        Min
    }

    impl Default for Timeout {
        fn default() -> Self {
            Timeout::Medium
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Retries {
        None,
        Max,
        Medium,
        Min
    }

    impl Default for Retries{
        fn default() -> Self {
            Retries::None
        }
    }




    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Priority{
        High,
        Med,
        Low
    }

    impl Default for Priority {
        fn default() -> Self {
            Self::Med
        }
    }


    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Karma{
        Super,
        High,
        Med,
        Low,
        None
    }

    impl Default for Karma{
        fn default() -> Self {
            Self::High
        }
    }

}

pub mod security {
    use std::collections::HashSet;
    use std::ops::{Deref, DerefMut};
    use std::str::FromStr;
    use nom::combinator::all_consuming;
    use crate::error::MsgErr;
    use crate::version::v0_0_1::id::Address;
    use crate::version::v0_0_1::pattern::AddressKindPattern;
    use serde::{Serialize,Deserialize};
    use crate::version::v0_0_1::messaging::ScopeGrant;
    use crate::version::v0_0_1::parse::permissions_mask;
    use crate::version::v0_0_1::Span;

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub enum Access {
        // bool is true if Super is also Owner
        Super(bool),
        Owner,
        Enumerated(EnumeratedAccess)
    }

    impl Access {

        pub fn has_super(&self) -> bool {
            match self {
                Access::Super(_) => true,
                _=> false,
            }
        }

        pub fn has_owner(&self) -> bool {
            match self {
                Access::Owner => true,
                Access::Super(owner)=> owner.clone(),
                _ => false,
            }
        }


        pub fn has_full(&self) -> bool {
            match self {
                Access::Super(_) => true,
                Access::Owner => true,
                Access::Enumerated(_) => false
            }
        }

        pub fn none() -> Self {
            Self::Enumerated(EnumeratedAccess::none())
        }

        pub fn permissions(&self) -> Permissions {
            match self {
                Access::Super(_) => {
                    Permissions::full()
                },
                Access::Owner => {
                    Permissions::full()
                },
                Access::Enumerated(enumerated) => {
                    enumerated.permissions.clone()
                }
            }
        }

        pub fn check_privilege(&self, privilege: &str) -> Result<(),MsgErr> {
            match self {
                Access::Super(_)=> Ok(()),
                Access::Owner=> Ok(()),
                Access::Enumerated(enumerated) => {
                    match enumerated.privileges.contains(privilege) {
                        true => Ok(()),
                        false => Err(format!("'{}'",privilege).into())
                    }
                }
            }
        }

    }


    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub enum Privileges{
        Full,
        Enumerated(EnumeratedPrivileges)
    }

    impl Privileges {
        pub fn has( &self, privilege: &str ) -> Result<(),()>{
            match self {
                Privileges::Full => Ok(()),
                Privileges::Enumerated(privileges) => privileges.has(privilege)
            }
        }

        pub fn none() -> Self {
            Self{ set: HashSet::new() }
        }

        pub fn or( mut self, other: &Self ) -> Self  {
            match self {
                Privileges::Full => {
                    self
                }
                Privileges::Enumerated(privileges) => {
                    match other {
                        Privileges::Full => {
                            Priviledges::Full
                        }
                        Privileges::Enumerated(other) => {
                            Privileges::Enumerated(privileges.or(other))
                        }
                    }
                }
            }
        }

        pub fn and( self, other: &Self ) -> Privileges {
            match other {
                Privileges::Full => {
                    self
                },
                Privileges::Enumerated(other) => {
                    match self {
                        Privileges::Full => {
                            other.clone()
                        }
                        Privileges::Enumerated(enumerated_self) => {
                            Privileges::Enumerated(enumerated_self.and(other))
                        }
                    }
                }
            }
        }

        pub fn add( &mut self, privilege: &str ) {
            self.set.insert(privilege.to_string() );
        }

    }



    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct EnumeratedPrivileges {
       set: HashSet<String>
    }

    impl EnumeratedPrivileges {
        pub fn new() -> Self {
            Self{ set: HashSet::new() }
        }

        pub fn none() -> Self {
            Self{ set: HashSet::new() }
        }

        pub fn or( mut self, other: &Self ) -> Self {
            for p in other.set.iter() {
                if !self.contains(p) {
                    self.insert(p.clone());
                }
            }
            self
        }

        pub fn and( mut self, other: &Self ) -> Self {
            self.retain( |p| other.contains(p) );
            self
        }

        pub fn add( &mut self, privilege: &str ) {
            self.set.insert(privilege.to_string() );
        }

        pub fn has( &self, privilege: &str ) -> Result<(),()>{
            let privilege = privilege.to_string();
            if self.set.contains(&privilege) {
                Ok(())
            } else {
                Err(())
            }
        }
    }



    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
    pub enum Privilege {
        Full,
        Single(String)
    }

    impl ToString for Privilege {
        fn to_string(&self) -> String {
            match self {
                Privilege::Full => "*".to_string(),
                Privilege::Single(name) => name.clone()
            }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct EnumeratedAccess {
        pub permissions: Permissions,
        pub privileges: Privileges
    }

    impl EnumeratedAccess {

        pub fn mask(&mut self, scope_grant: &ScopeGrant ) {

        }


        pub fn full() -> Self {
            Self{
                permissions: Permissions::full(),
                privileges: Privileges::Full
            }
        }

        pub fn none() -> Self {
            Self {
                permissions: Permissions::none(),
                privileges: Privileges::none(),
            }
        }

        pub fn and( &mut self, access: &Self ) {
            self.permissions.and(&access.permissions);
            self.privileges = self.privileges.and(&access.privileges);
        }

        pub fn clear_privs(&mut self) {
            self.privileges.clear();
        }

        pub fn add(&mut self, grant: &AccessGrant ) {
            match &grant.kind {
                AccessGrantKind::Super => {
                    // we can't mask Super with Enumerated... it does nothing
                }
                AccessGrantKind::Privilege(prv) => {
                    self.privileges.insert(prv.clone());
                }
                AccessGrantKind::PermissionsMask(mask) => {
                    match mask.kind {
                        PermissionsMaskKind::Or => self.permissions.or(&mask.permissions),
                        PermissionsMaskKind::And => self.permissions.and(&mask.permissions)
                    }
                }
            }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq)]
    pub struct PermissionsMask {
        pub kind: PermissionsMaskKind,
        pub permissions: Permissions
    }

    impl FromStr for PermissionsMask {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let span = Span::new(s);
            Ok(all_consuming(permissions_mask)(span)?.1)
        }
    }

    impl ToString for PermissionsMask {
        fn to_string(&self) -> String {
            match self.kind{
                PermissionsMaskKind::Or => format!("+{}", self.permissions.to_string()),
                PermissionsMaskKind::And => format!("&{}", self.permissions.to_string())
            }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq)]
    pub struct Permissions {
        pub child: ChildPerms,
        pub particle: ParticlePerms
    }

    impl Permissions {
        pub fn full() -> Self {
            Self {
                child: ChildPerms::full(),
                particle: ParticlePerms::full()
            }
        }

        pub fn none() -> Self {
            Self {
                child: ChildPerms::none(),
                particle: ParticlePerms::none()
            }
        }

        pub fn or( &mut self, permissions: &Permissions ) {
            self.child.or(&permissions.child);
            self.particle.or(&permissions.particle);
        }

        pub fn and( &mut self, permissions: &Permissions ) {
            self.child.and(&permissions.child);
            self.particle.and(&permissions.particle);
        }
    }

    impl ToString for Permissions {
        fn to_string(&self) -> String {
            format!("{}-{}", self.child.to_string(), self.particle.to_string() )
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq)]
    pub struct ChildPerms{
        pub create: bool,
        pub select: bool,
        pub delete: bool,
    }

    impl ChildPerms{
        pub fn full()->Self {
            Self {
                create: true,
                select: true,
                delete: true
            }
        }

        pub fn none()->Self {
            Self {
                create: false,
                select: false,
                delete:false
            }
        }

        pub fn or(&mut self, block: &ChildPerms) {
            self.create |= block.create;
            self.select |= block.select;
            self.delete |= block.delete;
        }

        pub fn and(&mut self, block: &ChildPerms) {
            self.create &= block.create;
            self.select &= block.select;
            self.delete &= block.delete;
        }
    }

    impl ToString for ChildPerms{
        fn to_string(&self) -> String {
            let mut rtn = String::new();

            if self.create {
                rtn.push_str("C");
            } else {
                rtn.push_str("c");
            }

            if self.select {
                rtn.push_str("S");
            } else {
                rtn.push_str("s");
            }

            if self.delete {
                rtn.push_str("D");
            } else {
                rtn.push_str("d");
            }

            rtn
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq)]
    pub struct ParticlePerms {
        pub read: bool,
        pub write: bool,
        pub execute: bool,
    }

    impl ParticlePerms {
        pub fn full()->Self {
            Self {
                read: true,
                write: true,
                execute: true
            }
        }

        pub fn none()->Self {
            Self {
                read: false,
                write: false,
                execute:false
            }
        }

        pub fn or(&mut self, block: &ParticlePerms) {
            self.read |= block.read;
            self.write |= block.write;
            self.execute |= block.execute;
        }

        pub fn and(&mut self, block: &ParticlePerms) {
            self.read &= block.read;
            self.write &= block.write;
            self.execute &= block.execute;
        }
    }

    impl FromStr for ParticlePerms {
        type Err = ();

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            todo!()
        }
    }

    impl ToString for ParticlePerms {
        fn to_string(&self) -> String {
            let mut rtn = String::new();

            if self.read {
                rtn.push_str("R");
            } else {
                rtn.push_str("r");
            }

            if self.write {
                rtn.push_str("W");
            } else {
                rtn.push_str("w");
            }

            if self.execute {
                rtn.push_str("X");
            } else {
                rtn.push_str("x");
            }

            rtn
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq)]
    pub enum PermissionsMaskKind {
        Or,
        And
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct AccessGrant {
        pub kind: AccessGrantKind,
        pub on_point: AddressKindPattern,
        pub to_point: AddressKindPattern,
        pub by_particle: Address,
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub enum AccessGrantKind {
        Super,
        Privilege(Privilege),
        PermissionsMask(PermissionsMask),
    }

    impl ToString for AccessGrantKind {
        fn to_string(&self) -> String {
            match self {
                AccessGrantKind::Super => "super".to_string(),
                AccessGrantKind::Privilege(_) => "priv".to_string(),
                AccessGrantKind::PermissionsMask(_) => "perm".to_string()
            }
        }
    }
}



pub mod frame {
    use std::convert::TryInto;

    use serde::{Deserialize, Serialize};

    use crate::error::MsgErr;

    pub struct PrimitiveFrame {
        pub data: Vec<u8>,
    }

    impl PrimitiveFrame {
        pub fn size(&self) -> u32 {
            self.data.len() as u32
        }
    }

    impl From<Vec<u8>> for PrimitiveFrame {
        fn from(value: Vec<u8>) -> Self {
            Self { data: value }
        }
    }

    impl From<String> for PrimitiveFrame {
        fn from(value: String) -> Self {
            let bytes = value.as_bytes();
            Self {
                data: bytes.to_vec(),
            }
        }
    }

    impl TryInto<String> for PrimitiveFrame {
        type Error = MsgErr;

        fn try_into(self) -> Result<String, Self::Error> {
            Ok(String::from_utf8(self.data)?)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
    pub enum CloseReason {
        Done,
        Error(String),
    }
}

pub mod bin {
    use std::collections::HashMap;
    use std::sync::Arc;

    use serde::{Deserialize, Serialize};

    pub type Bin = Arc<Vec<u8>>;
}

pub mod payload {
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;
    use std::ops::{Deref, DerefMut};

    use crate::error::MsgErr;
    use crate::version::v0_0_1::bin::Bin;
    use crate::version::v0_0_1::entity::request::{Action, Rc, RcCommandType, RequestCore};
    use crate::version::v0_0_1::id::{
        Address, CaptureAddress, Meta, PayloadClaim, ResourceKind, ResourceType,
    };
    use crate::version::v0_0_1::pattern::TksPattern;
    use crate::version::v0_0_1::resource::{Resource, ResourceStub, Status};
    use crate::version::v0_0_1::util::{ValueMatcher, ValuePattern};
    use std::str::FromStr;
    use std::sync::Arc;
    use http::{Method, Uri};

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
    pub enum Payload {
        Empty,
        Primitive(Primitive),
        List(PrimitiveList),
        Map(PayloadMap),
    }

    impl Payload {

        pub fn to_text(self) -> Result<String, MsgErr> {
            if let Payload::Primitive(Primitive::Text(text)) = self {
                Ok(text)
            } else {
                Err("not a 'Text' payload".into())
            }
        }


        pub fn is_some(&self) -> bool {
            if let Self::Empty = self {
                false
            } else {
                true
            }
        }

        pub fn from_bin(bin: Bin) -> Self {
            Self::Primitive(Primitive::Bin(bin))
        }

        pub fn payload_type(&self) -> PayloadType {
            match self {
                Payload::Empty => PayloadType::Empty,
                Payload::Primitive(primitive) => PayloadType::Primitive,
                Payload::List(list) => PayloadType::List,
                Payload::Map(map) => PayloadType::Map,
            }
        }

        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            match self {
                Payload::Empty => Ok(Arc::new(vec![])),
                Payload::Primitive(primitive) => primitive.to_bin(),
                Payload::List(list) => list.to_bin(),
                Payload::Map(map) => map.to_bin(),
            }
        }
    }

    impl TryInto<HashMap<String,Payload>> for Payload {
        type Error = MsgErr;

        fn try_into(self) -> Result<HashMap<String,Payload>, Self::Error> {
            match self {
                Payload::Map(map) => Ok(map.map),
                _ => Err("Payload type must a Map".into()),
            }
        }
    }

    impl TryInto<String> for Payload {
        type Error = MsgErr;

        fn try_into(self) -> Result<String, Self::Error> {
            match self {
                Payload::Primitive(Primitive::Text(text)) => Ok(text),
                Payload::Primitive(Primitive::Bin(bin)) => {
                    Ok(String::from_utf8(bin.to_vec())?)
                },
                _ => Err("Payload type must an Text".into()),
            }
        }
    }


    impl TryInto<Address> for Payload {
        type Error = MsgErr;

        fn try_into(self) -> Result<Address, Self::Error> {
            match self {
                Payload::Primitive(Primitive::Address(address)) => Ok(address),
                _ => Err("Payload type must an Address".into()),
            }
        }
    }

    impl TryInto<Primitive> for Payload {
        type Error = MsgErr;

        fn try_into(self) -> Result<Primitive, Self::Error> {
            match self {
                Payload::Primitive(primitive) => Ok(primitive),
                _ => Err("Payload type must be Primitive".into()),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct PayloadMap {
        pub map: HashMap<String, Payload>,
    }

    impl Into<Payload> for PayloadMap {
        fn into(self) -> Payload {
            Payload::Map(self)
        }
    }

    impl Deref for PayloadMap {
        type Target = HashMap<String, Payload>;

        fn deref(&self) -> &Self::Target {
            &self.map
        }
    }

    impl DerefMut for PayloadMap {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.map
        }
    }

    impl Default for PayloadMap {
        fn default() -> Self {
            Self {
                map: Default::default(),
            }
        }
    }
    /*
    impl <ToKind,FromKind> TryInto<ToKind> for PayloadMap<FromKind> {
        type Error = Error;

        fn try_into(self) -> Result<ToKind, Self::Error> {
            let mut map = HashMap::new();
            for (k,v) in self.map {
                map.insert( k, v.try_into()? );
            }
            Ok(Self{map})
        }
    }

     */

    impl PayloadMap {
        /*
        pub fn new(constraints: MapConstraints<KEY,ADDRESS,IDENTIFIER,KIND> ) -> Self {
            Self{
        //        constraints,
                map: HashMap::new()
            }
        }

         */
        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            Ok(Arc::new(bincode::serialize(&self)?))
        }

        pub fn new() -> Self {
            Self {
                map: HashMap::new(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Errors {
        map: HashMap<String, String>,
    }

    impl Errors {
        pub fn empty() -> Self {
            Self {
                map: HashMap::new(),
            }
        }

        pub fn default(message: &str) -> Self {
            let mut map = HashMap::new();
            map.insert("default".to_string(), message.to_string());
            Self { map }
        }
    }

    impl ToString for Errors {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            for (index, (_, value)) in self.iter().enumerate() {
                rtn.push_str(value.as_str());
                if index == self.len() - 1 {
                    rtn.push_str("\n");
                }
            }
            rtn
        }
    }

    impl Deref for Errors {
        type Target = HashMap<String, String>;

        fn deref(&self) -> &Self::Target {
            &self.map
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
    pub enum Primitive {
        Text(String),
        Address(Address),
        Stub(ResourceStub),
        Meta(Meta),
        Bin(Bin),
        Boolean(bool),
        Int(i64),
        Status(Status),
        Resource(Resource),
        Errors(Errors),
    }

    impl Primitive {
        pub fn primitive_type(&self) -> PrimitiveType {
            match self {
                Primitive::Text(_) => PrimitiveType::Text,
                Primitive::Address(_) => PrimitiveType::Address,
                Primitive::Stub(_) => PrimitiveType::Stub,
                Primitive::Meta(_) => PrimitiveType::Meta,
                Primitive::Bin(_) => PrimitiveType::Bin,
                Primitive::Boolean(_) => PrimitiveType::Boolean,
                Primitive::Int(_) => PrimitiveType::Int,
                Primitive::Status(_) => PrimitiveType::Status,
                Primitive::Resource(_) => PrimitiveType::Resource,
                Primitive::Errors(_) => PrimitiveType::Errors,
            }
        }

        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            match self {
                Primitive::Text(text) => {
                    let text = text.into_bytes();
                    Ok(Arc::new(text))
                }
                Primitive::Address(address) => {
                    let address = address.to_string().into_bytes();
                    Ok(Arc::new(address))
                }
                Primitive::Stub(stub) => Ok(Arc::new(bincode::serialize(&stub)?)),
                Primitive::Meta(meta) => Ok(Arc::new(bincode::serialize(&meta)?)),
                Primitive::Bin(bin) => Ok(bin),
                Primitive::Boolean(flag) => Ok(Arc::new(flag.to_string().into_bytes())),
                Primitive::Int(int) => Ok(Arc::new(int.to_string().into_bytes())),
                Primitive::Status(status) => {
                    let status = status.to_string().into_bytes();
                    Ok(Arc::new(status))
                }
                Primitive::Resource(resource) => Ok(Arc::new(bincode::serialize(&resource)?)),
                Primitive::Errors(errors) => Ok(Arc::new(bincode::serialize(&errors)?)),
            }
        }
    }

    impl TryInto<Bin> for Primitive {
        type Error = MsgErr;

        fn try_into(self) -> Result<Bin, Self::Error> {
            match self {
                Primitive::Bin(bin) => Ok(bin),
                _ => Err("Primitive must be of type Bin".into()),
            }
        }
    }

    impl TryInto<Address> for Primitive {
        type Error = MsgErr;

        fn try_into(self) -> Result<Address, Self::Error> {
            match self {
                Primitive::Address(address) => Ok(address),
                _ => Err("Primitive must be of type Address".into()),
            }
        }
    }


    impl TryInto<String> for Primitive {
        type Error = MsgErr;

        fn try_into(self) -> Result<String, Self::Error> {
            match self {
                Primitive::Text(text) => Ok(text),
                _ => Err("Primitive must be of type Text".into()),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct PrimitiveList {
        pub primitive_type: PrimitiveType,
        pub list: Vec<Primitive>,
    }

    impl ToString for PrimitiveList {
        fn to_string(&self) -> String {
            format!("{}[]", self.primitive_type.to_string())
        }
    }

    impl PrimitiveList {
        pub fn new(primitive_type: PrimitiveType) -> Self {
            Self {
                primitive_type,
                list: vec![],
            }
        }
        pub fn validate(&self) -> Result<(), MsgErr> {
            for primitive in &self.list {
                if primitive.primitive_type() != self.primitive_type {
                    return Err(format!(
                        "PrimitiveList type mismatch expected: {} received: {}",
                        self.primitive_type.to_string(),
                        primitive.primitive_type().to_string()
                    )
                    .into());
                }
            }
            Ok(())
        }

        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            Ok(Arc::new(bincode::serialize(&self)?))
        }
    }

    impl Deref for PrimitiveList {
        type Target = Vec<Primitive>;

        fn deref(&self) -> &Self::Target {
            &self.list
        }
    }

    impl DerefMut for PrimitiveList {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.list
        }
    }
    #[derive(Debug, Clone, strum_macros::Display, Eq, PartialEq, Hash, Serialize, Deserialize)]
    pub enum PrimitiveType {
        Address,
        Text,
        Boolean,
        Code,
        Int,
        Meta,
        Bin,
        Stub,
        Status,
        Resource,
        Errors,
    }

    impl FromStr for PrimitiveType {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(match s {
                "Address" => Self::Address,
                "Text" => Self::Text,
                "Boolean" => Self::Boolean,
                "Code" => Self::Code,
                "Int" => Self::Int,
                "Meta" => Self::Meta,
                "Bin" => Self::Bin,
                "Stub" => Self::Stub,
                "Status" => Self::Status,
                "Resource" => Self::Resource,
                what => return Err(format!("unrecognized PrimitiveType: {}", what).into()),
            })
        }
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
    pub enum PayloadType {
        Empty,
        Primitive,
        List,
        Map,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct ListPattern {
        pub primitive: PrimitiveType,
        pub range: Range,
    }

    impl ListPattern {
        pub fn is_match(&self, list: &PrimitiveList) -> Result<(), MsgErr> {
            for i in &list.list {
                if self.primitive != i.primitive_type() {
                    return Err(format!(
                        "Primitive List expected: {} found: {}",
                        self.primitive.to_string(),
                        i.primitive_type().to_string()
                    )
                    .into());
                }
            }

            Ok(())
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum Range {
        MinMax { min: usize, max: usize },
        Exact(usize),
        Any,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum PayloadTypePattern {
        Empty,
        Primitive(PrimitiveType),
        List(ListPattern),
        Map(Box<MapPattern>),
    }

    impl PayloadTypePattern {
        pub fn is_match(&self, payload: &Payload) -> Result<(), MsgErr> {
            match self {
                PayloadTypePattern::Empty => {
                    if payload.payload_type() == PayloadType::Empty {
                        Ok(())
                    } else {
                        Err(format!(
                            "Payload expected: Empty found: {}",
                            payload.payload_type().to_string()
                        )
                        .into())
                    }
                }
                PayloadTypePattern::Primitive(expected) => {
                    if let Payload::Primitive(found) = payload {
                        if *expected == found.primitive_type() {
                            Ok(())
                        } else {
                            Err(format!(
                                "Payload Primitive expected: {} found: {}",
                                expected.to_string(),
                                found.primitive_type().to_string()
                            )
                            .into())
                        }
                    } else {
                        Err(format!(
                            "Payload expected: {} found: {}",
                            expected.to_string(),
                            payload.payload_type().to_string()
                        )
                        .into())
                    }
                }
                PayloadTypePattern::List(expected) => {
                    if let Payload::List(found) = payload {
                        expected.is_match(found)
                    } else {
                        Err(format!(
                            "Payload expected: List found: {}",
                            payload.payload_type().to_string()
                        )
                        .into())
                    }
                }
                PayloadTypePattern::Map(expected) => {
                    if let Payload::Map(found) = payload {
                        expected.is_match(found)
                    } else {
                        Err(format!(
                            "Payload expected: {} found: {}",
                            expected.to_string(),
                            payload.payload_type().to_string()
                        )
                        .into())
                    }
                }
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct PayloadPattern {
        pub structure: PayloadTypePattern,
        pub format: Option<PayloadFormat>,
        pub validator: Option<CallWithConfig>,
    }

    impl ValueMatcher<Payload> for PayloadPattern {
        fn is_match(&self, payload: &Payload) -> Result<(), MsgErr> {
            self.structure.is_match(&payload)?;

            // more matching to come... not sure exactly how to match Format and Validation...
            Ok(())
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct CallWithConfig {
        pub call: Call,
        pub config: Option<Address>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Call {
        pub address: CaptureAddress,
        pub kind: CallKind,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum CallKind {
        Msg(MsgCall),
        Http(HttpCall),
    }

    impl CallKind {
        pub fn core_with_body(self, body: Payload) -> Result<RequestCore, MsgErr> {
            Ok(match self {
                CallKind::Msg(msg) => RequestCore {
                    headers: Default::default(),
                    action: Action::Msg(msg.action),
                    uri: Uri::from_str(msg.path.as_str())?,
                    body,
                },
                CallKind::Http(http) => RequestCore {
                    headers: Default::default(),
                    action: Action::Http(http.method),
                    uri: Uri::from_str(http.path.as_str())?,
                    body,
                },
            })
        }
    }

    impl ToString for Call {
        fn to_string(&self) -> String {
            format!("{}^{}", self.address.to_string(), self.kind.to_string())
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
    pub struct MsgCall {
        pub path: String,
        pub action: String,
    }

    impl MsgCall {
        pub fn new(action: String, path: String) -> Self {
            Self { action, path }
        }
    }

    impl ToString for MsgCall {
        fn to_string(&self) -> String {
            format!("Msg<{}>{}", self.action, self.path)
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
    pub struct HttpCall {
        pub path: String,

        #[serde(with = "http_serde::method")]
        pub method: HttpMethod,
    }

    impl HttpCall {
        pub fn new(method: HttpMethod, path: String) -> Self {
            Self { method, path }
        }
    }

    impl ToString for HttpCall {
        fn to_string(&self) -> String {
            format!("Http<{}>{}", self.method.to_string(), self.path)
        }
    }

    #[derive(
        Debug,
        Clone,
        Eq,
        PartialEq,
        Serialize,
        Deserialize,
        Hash,
        strum_macros::Display,
        strum_macros::EnumString,
    )]
    pub enum HttpMethodType {
        Get,
        Post,
        Put,
        Delete,
        Patch,
        Head,
        Connect,
        Options,
        Trace,
    }

    impl HttpMethodType {
        pub fn to_method(self) -> HttpMethod {
            match self {
                HttpMethodType::Get => HttpMethod::GET,
                HttpMethodType::Post => HttpMethod::POST,
                HttpMethodType::Put => HttpMethod::PUT,
                HttpMethodType::Delete => HttpMethod::DELETE,
                HttpMethodType::Patch => HttpMethod::PATCH,
                HttpMethodType::Head => HttpMethod::HEAD,
                HttpMethodType::Connect => HttpMethod::CONNECT,
                HttpMethodType::Options => HttpMethod::OPTIONS,
                HttpMethodType::Trace => HttpMethod::TRACE
            }
        }
    }

    pub type HttpMethod = Method;

    /*
    impl FromStr for HttpMethod {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let input = s.to_uppercase();
            match input.as_str() {
                "GET" => Ok(HttpMethod::GET),
                "POST" => Ok(HttpMethod::POST),
                "PUT" => Ok(HttpMethod::Put),
                "DELETE" => Ok(HttpMethod::DELETE),
                "PATCH" => Ok(HttpMethod::Patch),
                "HEAD" => Ok(HttpMethod::Head),
                "CONNECT" => Ok(HttpMethod::Connect),
                "OPTIONS" => Ok(HttpMethod::Options),
                "TRACE"=>  Ok(HttpMethod::Trace),
                what => Err(format!("unrecognized http method.  found: {}", what ).into())
            }
        }
    }

     */

    impl ValueMatcher<HttpMethod> for HttpMethod {
        fn is_match(&self, found: &HttpMethod) -> Result<(), crate::error::MsgErr> {
            if *self == *found {
                Ok(())
            } else {
                Err(format!(
                    "Http Method mismatch. expected: '{}', found: '{}'",
                    self.to_string(),
                    found.to_string()
                )
                .into())
            }
        }
    }

    impl ToString for CallKind {
        fn to_string(&self) -> String {
            match self {
                CallKind::Msg(msg) => msg.to_string(),
                CallKind::Http(http) => http.to_string(),
            }
        }
    }

    #[derive(
        Debug,
        Clone,
        Eq,
        PartialEq,
        strum_macros::Display,
        strum_macros::EnumString,
        Serialize,
        Deserialize,
    )]
    pub enum PayloadFormat {
        #[strum(serialize = "json")]
        Json,
        #[strum(serialize = "image")]
        Image,
    }

    impl PrimitiveType {
        pub fn is_match(&self, primitive: &Primitive) -> Result<(), MsgErr> {
            match primitive {
                Primitive::Text(_) => {
                    if *self == Self::Text {
                        Ok(())
                    } else {
                        Err("expected Text primitive".into())
                    }
                }
                Primitive::Address(_) => {
                    if *self == Self::Address {
                        Ok(())
                    } else {
                        Err("expected Address primitive".into())
                    }
                }
                Primitive::Stub(_) => {
                    if *self == Self::Stub {
                        Ok(())
                    } else {
                        Err("expected Stub primitive".into())
                    }
                }
                Primitive::Meta(_) => {
                    if *self == Self::Meta {
                        Ok(())
                    } else {
                        Err("expected Meta primitive".into())
                    }
                }
                Primitive::Bin(_) => {
                    if *self == Self::Bin {
                        Ok(())
                    } else {
                        Err("expected Bin primitive".into())
                    }
                }
                Primitive::Boolean(_) => {
                    if *self == Self::Boolean {
                        Ok(())
                    } else {
                        Err("expected Boolean primitive".into())
                    }
                }
                Primitive::Int(_) => {
                    if *self == Self::Int {
                        Ok(())
                    } else {
                        Err("expected Int primitive".into())
                    }
                }
                Primitive::Status(_) => {
                    if *self == Self::Status {
                        Ok(())
                    } else {
                        Err("expected Status primitive".into())
                    }
                }
                Primitive::Resource(_) => {
                    if *self == Self::Resource {
                        Ok(())
                    } else {
                        Err("expected Resource primitive".into())
                    }
                }
                Primitive::Errors(errors) => {
                    if *self == Self::Errors {
                        Ok(())
                    } else {
                        Err("expected Errors primitive".into())
                    }
                }
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct MapPattern {
        pub required: HashMap<String, ValuePattern<PayloadPattern>>,
        pub allowed: ValuePattern<PayloadPattern>,
    }

    impl Default for MapPattern {
        fn default() -> Self {
            MapPattern {
                required: Default::default(),
                allowed: ValuePattern::Any,
            }
        }
    }

    impl ToString for MapPattern {
        fn to_string(&self) -> String {
            "Map?".to_string()
        }
    }

    impl MapPattern {
        pub fn new(
            required: HashMap<String, ValuePattern<PayloadPattern>>,
            allowed: ValuePattern<PayloadPattern>,
        ) -> Self {
            MapPattern { required, allowed }
        }

        pub fn empty() -> Self {
            Self {
                required: HashMap::new(),
                allowed: ValuePattern::None,
            }
        }

        pub fn any() -> Self {
            Self {
                required: HashMap::new(),
                allowed: ValuePattern::Any,
            }
        }

        pub fn is_match(&self, map: &PayloadMap) -> Result<(), MsgErr> {
            // if Any keys are allowed then skip
            for (key, payload) in &map.map {
                if !self.required.contains_key(key) {
                    match &self.allowed {
                        ValuePattern::Any => {}
                        ValuePattern::None => {
                            return Err(format!(
                                "key: '{}' not required or allowed by Map constraints",
                                key
                            )
                            .into());
                        }
                        ValuePattern::Pattern(pattern) => {
                            pattern.is_match(payload)?;
                        }
                    }
                }
            }

            // now make sure all required are present and meet constraints
            for (key, constraint) in &self.required {
                if !map.contains_key(key) {
                    return Err(format!(
                        "missing required key : '{}' defined in Map constraints",
                        key
                    )
                    .into());
                }
                constraint.is_match(
                    &map.get(key)
                        .expect("expected map element after testing for it"),
                )?;
            }

            Ok(())
        }
    }

    /*
    impl<FromResourceType,FromKind,FromPayload,FromTksPattern, ToResourceType,ToKind,ToPayload,ToTksPattern> ConvertFrom<Valuepattern<PayloadPattern<FromResourceType,FromKind,FromPayload,FromTksPattern>>>
    for ValuePattern<PayloadPattern<FromResourceType,FromKind,FromPayload,FromTksPattern>>
        where
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            FromTksPattern: TryInto<ToTksPattern, Error = Error> + Clone,
            FromPayload: TryInto<ToPayload, Error = Error> + Clone,
            FromResourceType: TryInto<ToResourceType, Error = Error> + Clone,
            ToKind: Clone,

    {

        fn convert_from(
            a: HashMap<String, ValuePattern<PayloadPattern<FromKind>>>
        ) -> Result<Self, Error>
            where
                Self: Sized,
        {
            let mut rtn = HashMap::new();
            for (k,v) in a {
                rtn.insert( k, ConvertFrom::convert_from(v)?);
            }
            Ok(rtn)
        }
    }

     */

    /*
    impl <KEY,ADDRESS,IDENTIFIER,KIND> ValuePattern<Payload<KEY,ADDRESS,IDENTIFIER,KIND>> for PayloadType<KEY,ADDRESS,IDENTIFIER,KIND> {
        fn is_match(&self, payload: &Payload<KEY,ADDRESS,IDENTIFIER,KIND>) -> Result<(), Error> {
            match **self {
                PayloadType::Empty => {
                    if let Payload::Empty = payload {
                        Ok(())
                    } else {
                        Err(format!("Payload expected: '{}' found: Empty",self.to_string()).into())
                    }
                }
                PayloadType::Primitive(expected) => {
                    if let Payload::Primitive(found)= payload {
                        expected.is_match(found)
                    } else {
                        Err(format!("Payload expected: '{}' found: '{}'",self.to_string(), payload.to_string()).into())
                    }
                }
                PayloadType::List(expected) => {
                    if let Payload::List(found)= payload {
                        expected.is_match(&found.primitive_type )
                    } else {
                        Err(format!("Payload expected: '{}' found: '{}'",self.to_string(), payload.to_string()).into())
                    }
                }
                PayloadType::Map(expected) => {
                    if let Payload::Map(found)= payload {
                        expected.is_match(&found.primitive_type )
                    } else {
                        Err(format!("Payload expected: '{}' found: '{}'",self.to_string(), payload.to_string()).into())
                    }
                }
            }
        }
    }

     */

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PayloadRef<PAYLOAD_CLAIM, PAYLOAD_PATTERN> {
        pub claim: PAYLOAD_CLAIM,
        pub pattern: PAYLOAD_PATTERN,
    }

    /*
    impl<FromPayloadClaim, FromPayloadPattern> PayloadRef<FromPayloadClaim, FromPayloadPattern> {
        pub fn convert<ToPayloadClaim, ToPayloadPattern>(
            self,
        ) -> Result<PayloadRef<ToPayloadClaim, ToPayloadPattern>, Error>
        where
            ToPayloadClaim: TryFrom<FromPayloadClaim, Error = Error>,
            ToPayloadPattern: TryFrom<FromPayloadPattern, Error = Error>,
        {
            Ok(Self {
                claim: self.claim.try_into()?,
                pattern: self.pattern.try_into()?,
            })
        }
    }

     */

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum PayloadDelivery<PAYLOAD, PAYLOAD_REF> {
        Payload(PAYLOAD),
        Ref(PAYLOAD_REF),
    }

    /*
    impl<FromPayload, FromPayloadRef> PayloadDelivery<FromPayload, FromPayloadRef> {
        pub fn convert<ToPayload, ToPayloadRef>(
            self,
        ) -> Result<PayloadDelivery<ToPayload, ToPayloadRef>, Error>
        where
            ToPayload: TryFrom<FromPayload,Error=Error>,
            ToPayloadRef: TryFrom<FromPayloadRef,Error=Error>,
        {
            match self {
                PayloadDelivery::Payload(payload) => Ok(payload.try_into()?),
                PayloadDelivery::Ref(payload_ref) => {
                    Ok(payload_ref.try_into()?)
                }
            }
        }
    }
     */
}

pub mod command {
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Command {
        pub cli: CliId,
        pub payload: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum CommandStatus {
        Running,
        Exit(i32),
    }

    pub type CliId = String;
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CommandEvent {
        pub cli: CliId,
        pub line: Option<String>,
        pub status: CommandStatus,
    }

    pub mod common {
        use std::collections::HashMap;
        use std::convert::{TryFrom, TryInto};
        use std::ops::{Deref, DerefMut};

        use serde::{Deserialize, Serialize};

        use crate::error::MsgErr;
        use crate::version::v0_0_1::payload::{Payload, PayloadMap};

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum StateSrc {
            Stateless,
            StatefulDirect(Payload),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum PropertyMod {
            Set {
                key: String,
                value: String,
                lock: bool,
            },
            UnSet(String),
        }

        impl PropertyMod {
            pub fn set_or<E>( &self, err: E) -> Result<String,E> {
                match self {
                    Self::Set { key, value, lock } => {
                        Ok(value.clone())
                    },
                    Self::UnSet(_) => {
                        Err(err)
                    }
                }
            }

            pub fn opt(&self) -> Option<String> {
                match self {
                    Self::Set { key, value, lock } => {
                        Some(value.clone())
                    },
                    Self::UnSet(_) => {
                        None
                    }
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct SetProperties {
            pub map: HashMap<String, PropertyMod>,
        }

        impl Default for SetProperties {
            fn default() -> Self {
                Self {
                    map: Default::default(),
                }
            }
        }

        impl SetProperties {
            pub fn new() -> Self {
                Self {
                    map: HashMap::new(),
                }
            }

            pub fn append(&mut self, properties: SetProperties) {
                for (_, property) in properties.map.into_iter() {
                    self.push(property);
                }
            }

            pub fn push(&mut self, property: PropertyMod) {
                match &property {
                    PropertyMod::Set { key, value, lock } => {
                        self.map.insert(key.clone(), property);
                    }
                    PropertyMod::UnSet(key) => {
                        self.map.insert(key.clone(), property);
                    }
                }
            }
        }

        impl Deref for SetProperties {
            type Target = HashMap<String, PropertyMod>;

            fn deref(&self) -> &Self::Target {
                &self.map
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum SetLabel {
            Set(String),
            SetValue { key: String, value: String },
            Unset(String),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct SetRegistry {
            pub labels: Vec<SetLabel>,
        }

        impl Deref for SetRegistry {
            type Target = Vec<SetLabel>;

            fn deref(&self) -> &Self::Target {
                &self.labels
            }
        }

        impl DerefMut for SetRegistry {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.labels
            }
        }

        impl Default for SetRegistry {
            fn default() -> Self {
                Self {
                    labels: Default::default(),
                }
            }
        }
    }
}

pub mod msg {
    use crate::error::MsgErr;
    use crate::version::v0_0_1::entity::request::{Action, RequestCore};
    use crate::version::v0_0_1::entity::response::ResponseCore;
    use crate::version::v0_0_1::id::Meta;
    use crate::version::v0_0_1::payload::{Errors, Payload, Primitive};
    use serde::{Deserialize, Serialize};
    use http::{HeaderMap, StatusCode, Uri};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MsgRequest {
        pub action: String,

        #[serde(with = "http_serde::header_map")]
        pub headers: HeaderMap,

        #[serde(with = "http_serde::uri")]
        pub uri: Uri,
        pub body: Payload,
    }

    impl MsgRequest {
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
                body: Payload::Primitive(Primitive::Errors(errors)),
            }
        }
    }

    impl TryFrom<RequestCore> for MsgRequest {
        type Error = MsgErr;

        fn try_from(core: RequestCore) -> Result<Self, Self::Error> {
            if let Action::Msg(action) = core.action {
                Ok(Self {
                    action,
                    headers: core.headers,
                    uri: core.uri,
                    body: core.body,
                })
            } else {
                Err("expected Msg".into())
            }
        }
    }
}

pub mod config {
    use std::collections::HashMap;
    use std::ops::Deref;

    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::config::bind::BindConfig;
    use crate::version::v0_0_1::id::{Address, ResourceKind};
    use crate::version::v0_0_1::resource;
    use crate::version::v0_0_1::resource::ResourceStub;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum PortalKind {
        Mechtron,
        Portal,
    }

    impl ToString for PortalKind {
        fn to_string(&self) -> String {
            match self {
                PortalKind::Mechtron => "Mechtron".to_string(),
                PortalKind::Portal => "Portal".to_string(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Info {
        pub stub: ResourceStub,
        pub kind: PortalKind,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PortalConfig {
        pub max_payload_size: u32,
        pub init_timeout: u64,
        pub frame_timeout: u64,
        pub response_timeout: u64,
    }

    impl Default for PortalConfig {
        fn default() -> Self {
            Self {
                max_payload_size: 128 * 1024,
                init_timeout: 30,
                frame_timeout: 5,
                response_timeout: 15,
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Assign {
        pub config: Config<ResourceConfigBody>,
        pub stub: resource::ResourceStub,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Config<Body> {
        pub address: Address,
        pub body: Body,
    }

    impl<Body> Deref for Config<Body> {
        type Target = Body;

        fn deref(&self) -> &Self::Target {
            &self.body
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ConfigBody {
        Bind(BindConfig),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ResourceConfigBody {
        Control,
        Named(String),
    }

    pub mod bind {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::entity::request::{Rc, RequestCore};
        use crate::version::v0_0_1::entity::EntityType;
        use crate::version::v0_0_1::id::CaptureAddress;
        use crate::version::v0_0_1::pattern::{
            Block, EntityPattern, HttpPattern, MsgPattern, RcPattern,
        };
        use crate::version::v0_0_1::payload::Call;
        use crate::version::v0_0_1::payload::PayloadType::Primitive;
        use crate::version::v0_0_1::payload::{Payload, PayloadPattern};
        use crate::version::v0_0_1::util::{ValueMatcher, ValuePattern};
        use serde::{Deserialize, Serialize};
        use std::convert::TryInto;

        pub struct ProtoBind {
            pub sections: Vec<PipelinesSubScope>,
        }

        impl TryInto<BindConfig> for ProtoBind {
            type Error = MsgErr;

            fn try_into(self) -> Result<BindConfig, Self::Error> {
                let mut opt_msg = Option::None;
                let mut opt_http = Option::None;
                let mut opt_rc = Option::None;

                for section in self.sections {
                    match section {
                        PipelinesSubScope::Msg(msg) => {
                            if opt_msg.is_some() {
                                return Err("multiple Msg sections not allowed.".into());
                            }
                            opt_msg = Some(msg);
                        }
                        PipelinesSubScope::Http(http) => {
                            if opt_http.is_some() {
                                return Err("multiple Http sections not allowed.".into());
                            }
                            opt_http = Some(http);
                        }
                        PipelinesSubScope::Rc(rc) => {
                            if opt_rc.is_some() {
                                return Err("multiple Rc sections not allowed.".into());
                            }
                            opt_rc = Some(rc);
                        }
                    }
                }
                let mut bind: BindConfig = Default::default();
                if let Option::Some(msg) = opt_msg {
                    bind.msg = msg;
                }
                if let Option::Some(http) = opt_http {
                    bind.http = http;
                }
                if let Option::Some(rc) = opt_rc {
                    bind.rc = rc;
                }
                Ok(bind)
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct BindConfig {
            pub msg: ConfigScope<EntityType, Selector<MsgPattern>>,
            pub http: ConfigScope<EntityType, Selector<HttpPattern>>,
            pub rc: ConfigScope<EntityType, Selector<RcPattern>>,
        }

        impl Default for BindConfig {
            fn default() -> Self {
                Self {
                    msg: ConfigScope::new(EntityType::Msg, vec![]),
                    http: ConfigScope::new(EntityType::Http, vec![]),
                    rc: ConfigScope::new(EntityType::Rc, vec![]),
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct ConfigScope<T, E> {
            pub scope_type: T,
            pub elements: Vec<E>,
        }

        impl<T, E> ConfigScope<T, E> {
            pub fn new(scope_type: T, elements: Vec<E>) -> Self {
                Self {
                    scope_type,
                    elements,
                }
            }
        }

        impl<T> ConfigScope<T, Selector<HttpPattern>> {
            pub fn find_match(&self, m: &RequestCore) -> Result<Selector<HttpPattern>, MsgErr> {
                for e in &self.elements {
                    if e.is_match(m).is_ok() {
                        return Ok(e.clone());
                    }
                }
                Err("no match".into())
            }
        }

        impl<T> ConfigScope<T, Selector<MsgPattern>> {
            pub fn find_match(&self, m: &RequestCore) -> Result<Selector<MsgPattern>, MsgErr> {
                for e in &self.elements {
                    if e.is_match(m).is_ok() {
                        return Ok(e.clone());
                    }
                }
                Err("no match".into())
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct Pipeline {
            pub segments: Vec<PipelineSegment>,
        }

        impl Pipeline {
            pub fn new() -> Self {
                Self { segments: vec![] }
            }

            pub fn consume(&mut self) -> Option<PipelineSegment> {
                if self.segments.is_empty() {
                    Option::None
                } else {
                    Option::Some(self.segments.remove(0))
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct PipelineStep {
            pub kind: StepKind,
            pub blocks: Vec<Block>,
        }

        impl PipelineStep {
            pub fn new(kind: StepKind) -> Self {
                Self {
                    kind,
                    blocks: vec![],
                }
            }
        }

        /*
        #[derive(Debug,Clone,Eq,PartialEq)]
        pub struct CreateBlock{
            pub payload: Payload
        }

         */

        pub type PatternBlock = ValuePattern<PayloadPattern>;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub enum PipelineStop {
            Internal,
            Call(Call),
            Respond,
            CaptureAddress(CaptureAddress),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Selector<P> {
            pub pattern: P,
            pub pipeline: Pipeline,
        }

        impl<P> Selector<P> {
            pub fn new(pattern: P, pipeline: Pipeline) -> Self {
                Selector { pattern, pipeline }
            }
        }

        impl Selector<EntityPattern> {
            pub fn is_match(&self, m: &RequestCore) -> Result<(), MsgErr> {
                self.pattern.is_match(m)
            }
        }

        impl Selector<MsgPattern> {
            pub fn is_match(&self, m: &RequestCore) -> Result<(), MsgErr> {
                self.pattern.is_match(m)
            }
        }

        impl Selector<RcPattern> {
            pub fn is_match(&self, m: &RequestCore) -> Result<(), MsgErr> {
                self.pattern.is_match(m)
            }
        }

        impl Selector<HttpPattern> {
            pub fn is_match(&self, m: &RequestCore) -> Result<(), MsgErr> {
                self.pattern.is_match(m)
            }
        }

        pub enum Whitelist {
            Any,
            None,
            Enumerated(Vec<CallPattern>),
        }

        pub enum CallPattern {
            Any,
            Call,
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct PipelineSegment {
            pub step: PipelineStep,
            pub stop: PipelineStop,
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub enum StepKind {
            Request,
            Response,
        }

        pub enum PipelinesSubScope {
            Msg(ConfigScope<EntityType, Selector<MsgPattern>>),
            Http(ConfigScope<EntityType, Selector<HttpPattern>>),
            Rc(ConfigScope<EntityType, Selector<RcPattern>>),
        }

        pub enum ScopeType {
            Bind,
            Msg,
            Http,
            Rc,
        }

        pub mod parse {
            use std::borrow::Borrow;
            use std::ops::{Deref, DerefMut};
            use crate::version::v0_0_1::config::bind::{
                Pipeline, PipelineSegment, PipelineStep, PipelineStop, PipelinesSubScope,
                ProtoBind, ConfigScope, Selector, StepKind,
            };
            use crate::version::v0_0_1::entity::EntityType;
            use crate::version::v0_0_1::parse::{capture_address, Res};
            use crate::version::v0_0_1::pattern::{
                call, entity_pattern, http_pattern, http_pattern_scoped, msg_pattern_scoped,
                pipeline_step_block, rc_pattern_scoped, EntityPattern, HttpPattern, MsgPattern,
                RcPattern,
            };
            use nom::branch::{alt, Alt};
            use nom::bytes::complete::tag;
            use nom::character::complete::multispace0;
            use nom::combinator::{all_consuming, fail, not, opt, peek, success};
            use nom::error::{context, ContextError, ErrorKind, ParseError};
            use nom::multi::{many0, many1};
            use nom::sequence::{delimited, preceded, tuple};
            use nom::{AsChar, Compare, IResult, InputIter, InputLength, InputTake, Parser, UnspecializedInput, InputTakeAtPosition, FindToken, Err};
            use nom_supreme::error::{ErrorTree, StackContext};
            use nom_supreme::final_parser::final_parser;
            use nom_supreme::ParserExt;
            use crate::error::MsgErr;

            pub fn final_bind(input: &str) -> Result<ProtoBind,String>
            {
                final_parser(delimited( multispace0,bind,multispace0 ) )(input).map_err( |err| final_bind_error(err) )
            }

            fn final_bind_error(error: ErrorTree<&str>) -> String {
                match find_error_stack(&error) {
                    Ok(stack) => {
println!("initial stack: {}",stack.to_string());
                        match stack.normalize() {
                            Ok(normalized) => {
                                let seg = normalized.final_segment();
                                let hierarchy = normalized.hierarchy();
println!("hierarch: {}", hierarchy);
                                match final_bind_error_message(normalized) {
                                    Ok(error) => {
                                        format_error( seg.location,hierarchy, error)
                                    }
                                    Err(error) => {
                                        format_error( seg.location,hierarchy, format!("internal Bind parser error: {} ",error))
                                    }
                                }
                            }
                            Err(_) => {
                                return "internal Bind parser error: could not unwrap error stack".to_string()
                            }
                        }
                    }
                    Err(_) => {
                        return "internal Bind parser error: could not find context stack".to_string()
                    }
                }
            }

            fn final_bind_error_message( stack: ErrorStack<&str> ) -> Result<String,String> {
                let hierarchy = stack.hierarchy();
println!("HIERARCHY {}", hierarchy);
println!("CONTEXT   {}", stack.final_segment().context);
println!("STACK {}", stack.to_string() );

                match hierarchy.as_str() {
                        "Bind.Pipelines" =>match stack.final_segment().context {
                            "multi_select_scope"=>{return Ok("expecting 'Rc', 'Msg' or 'Http' (pipelines selectors) or '}' (close Pipelines scope)".to_string());},
                            "selector"=>{return Ok("expecting 'Pipelines' (Pipelines selector)".to_string());},
                            "scope:open"=>{return Ok("expecting '{' (Pipelines open scope)".to_string());},
                            _ => {}
                        }
                        "Bind.Pipelines.Http.Pipeline.Step" => match stack.final_segment().context {
                            "!select-block" => return Ok("expected '->' (forward request) or '-[' (RequestPayloadFilter)".to_string()),
                            _ => {}
                        }
                    _ => {}
                }

                if hierarchy.ends_with(".Stop") {
                    return Ok("expected valid pipeline stop \"{{ }}\" (Core) or valid Address".to_string());
                }

                match stack.final_segment().context {
                    "http_method" => Ok("expecting valid HttpMethod: 'Get', 'Post', 'Put', 'Delete', etc... (HttpMethod)".to_string()),
                    "@http_method_pattern" => Ok("expecting '*' or valid HttpMethod: 'Get', 'Post', 'Put', 'Delete', etc... (HttpMethodPattern)".to_string()),
                    "angle_bracket_open" => Ok("expecting: '<' (open angle bracket)".to_string()),
                    "angle_bracket_close" => Ok("expecting: '>' (close angle bracket)".to_string()),
                    "!pipeline-step:exit" => Ok("pipeline step exit expecting: '->' (forward request) or '=>' (forward response)".to_string()),
                    "@payload-pattern" => Ok("expecting '*' or valid Payload: 'Bin', 'Text', 'Stub', 'Address', etc... (Payload)".to_string()),
                    "request-payload-filter-block" => Ok("expecting '*' or valid Payload: 'Bin', 'Text', 'Stub', 'Address', etc... (PayloadPattern)".to_string()),
                    "response-payload-filter-block" => Ok("expecting '*' or valid Payload: 'Bin', 'Text', 'Stub', 'Address', etc... (PayloadPattern)".to_string()),
                    "multi_select_scope" => {
                        match hierarchy.as_str() {
                            "Bind.Pipelines" =>Ok("expecting 'Rc', 'Msg' or 'Http' (pipelines selectors) or '}' (close Pipelines scope)".to_string()),
                            what => Err(format!("unrecognized hierarchy/context combo: {}/{}", hierarchy, what))
                        }
                    }
                    what => Err(format!("unrecognized parse error context: '{}'",what))
                }


            }

            pub fn format_error( input: &str, hierarchy: String, message: String ) -> String {
               format!("{}\n\n{}: {}", extract_problem_line(input),hierarchy,message )
            }

            pub fn find_error_stack<'a>(error: &'a ErrorTree<&str>) -> Result<ErrorStack<&'a str>,()> {
println!("find_error_stack");
                let mut segs = find_error_segments(error)?;
println!("Segs count: {}", segs.len() );
                segs.reverse();
                let stack = ErrorStack::new(segs)?;
                Ok(stack)
            }

            pub fn find_error_segments<'a>(error: &'a ErrorTree<&str>) -> Result<Vec<ErrorSegment<&'a str>>,()> {
                match error {
                    ErrorTree::Base { location, kind } => {
                        println!("found error kind: {} location: {}", kind.to_string(), location);
                        Err(())
                    }
                    ErrorTree::Stack { base, contexts } => {
                        //let mut stack = to_error_stack(contexts);

                        if let StackContext::Context(context) = first_context(contexts)? {
                            if "select" == context {
                                let mut stack = find_select_error(base.as_ref())?;
                                stack.append(&mut to_error_segments(contexts));
                                return Ok(stack);
                            } else if context.starts_with("!") {
                                return Ok(to_error_segments(contexts))
                            }
                        }

                        {
                            diagnose_contexts(contexts);
                            println!("finding error segments: ");
                            let mut stack = match find_error_segments(base.as_ref()) {
                                Ok(stack) => stack,
                                Err(_) => vec![]
                            };
                            stack.append(&mut to_error_segments(contexts));
                            Ok(stack)
                        }
                    }
                    ErrorTree::Alt(alts) => {
                        println!("alts");
                        Ok(vec!(find_selected_alt_branch(alts)?))
                    }
                }
            }


            // if the context terminates with selector then do NOT choose it (meaning the selector did not match)
            pub fn find_selected_alt_branch<'a>(alts: &'a Vec<ErrorTree<&str>>) -> Result<ErrorSegment<&'a str>,()> {
                for alt in alts {
                    match alt {
                        ErrorTree::Base { location, kind } => {}
                        ErrorTree::Stack { base, contexts } => {
                            if !contexts.is_empty() {
                                let (location,context) = contexts.first().expect("first context");
                                if let StackContext::Context(context) = context {
                                    // if context IS selector that is an indication that parsing
                                    // failed or errored as the alt was being selected, meaning, this is not the right branch
                                    if *context != "selector" {
                                        return Ok(ErrorSegment {
                                            location: *location,
                                            context:  *context
                                        });
                                    }
                                }
                            }
                       }
                        ErrorTree::Alt(alts) => {
                            return find_selected_alt_branch(alts);
                        }
                    }
                }
                Err(())
            }


            pub fn find_select_error<'a>(error: &'a ErrorTree<&str>) -> Result<Vec<ErrorSegment<&'a str>>,()> {
                println!("find_select_error...");
                match error {
                    ErrorTree::Base { location, kind } => {
                        println!("found error kind: {}", kind.to_string() );
                        Err(())
                    }
                    ErrorTree::Stack { base, contexts } => {
                        diagnose_contexts(contexts);
                        println!("find_Select_error::Stack...");

                        if let StackContext::Context(context) = first_context(contexts)?  {

                            if "selector" == context {
                                // if there is an error in the actual selector then this alt is aborted
                                diagnose_contexts(contexts);
                                println!("abort...");
                                return Err(());
                            } else if context.starts_with("!") {
                                return Ok(to_error_segments(contexts))
                            }
                        }
                        {
                            match base.as_ref() {
                                ErrorTree::Base {..} => {
                                    Ok(to_error_segments(contexts))
                                }
                                _ => {
                                    let mut stack = find_error_segments(base)?;
                                    stack.append( & mut to_error_segments(contexts) );
                                    Ok(stack)
                                }
                            }
                        }
                    }
                    ErrorTree::Alt(alts) => {
                        Ok(vec![find_selected_alt_branch(alts)?])
                    }
                }
            }

            #[derive(Clone)]
            pub struct ErrorStack<I> where I: Clone{
                pub list: Vec<ErrorSegment<I>>
            }

            impl <I> ErrorStack<I> where I:Clone {
                pub fn new(mut list: Vec<ErrorSegment<I>>)->Result<Self,()> {
                    if list.is_empty() {
                        return Err(());
                    } else {
                        Ok(Self { list })
                    }
                }

                pub fn normalize(&self) -> Result<Self,()> {
                    let mut list = vec![];
                    for seg in self.list.iter() {
                        list.push(seg.clone() );
                        // @ indicates the context is a wrapper meaning
                        // all its children will be wrapped in that error message
                        if seg.context.starts_with("@") {
                            break;
                        }
                    }
                    Self::new(list)
                }

                pub fn hierarchy(&self) -> String {
                    let mut list = vec![];
                    for seg in self.list.iter() {
                        if !seg.context.is_empty() {
                            let c = seg.context.chars().next().expect("expected first character");
                            if c.is_alpha() && c.is_uppercase() {
                                list.push(seg.context)
                            }
                        }
                    }
                    let mut rtn = String::new();
                    for (index, seg) in list.iter().enumerate() {
                        rtn.push_str(seg);
                        if index < list.len()-1 {
                            rtn.push_str(".")
                        }
                    }
                    rtn
                }
            }


            impl <'a> ErrorStack<&'a str> {
                pub fn final_segment(&self) -> ErrorSegment<&'a str> {
                    match self.list.last()  {
                        None => {
                            ErrorSegment{
                                context: "?",
                                location: "?"
                            }
                        }
                        Some(seg) => {seg.clone()}
                    }
                }
            }

            impl <I> Deref for ErrorStack<I> where I: Clone {
                type Target = Vec<ErrorSegment<I>>;

                fn deref(&self) -> &Self::Target {
                    &self.list
                }
            }

            impl <I> DerefMut for ErrorStack<I> where I: Clone {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.list
                }
            }

            impl <I> ToString for ErrorStack<I> where I: Clone{
                fn to_string(&self) -> String {
                    let mut rtn = String::new();
                    for (index,seg) in self.list.iter().enumerate() {
                       rtn.push_str(seg.context);
                       if index < self.list.len()-1 {
                           rtn.push_str(".")
                       }
                    }
                    rtn
                }
            }



            #[derive(Clone)]
            pub struct ErrorSegment<I> where I: Clone{
                pub context: &'static str,
                pub location: I
            }

            fn to_error_segments<I>(contexts: &Vec<(I, StackContext)> ) -> Vec<ErrorSegment<I>> where I: Clone{
                let mut rtn = vec![];
                for (location,context) in contexts {
                    if let StackContext::Context(context) = context {
                        let seg = ErrorSegment {
                            location: location.clone(),
                            context
                        };
                        rtn.push(seg);
                    }
                }
                rtn
            }



            fn first_context(contexts: &Vec<(&str, StackContext)>) -> Result<StackContext,()> {
                if contexts.is_empty() {
                    Err(())
                } else {
                    Ok(contexts.first().cloned().unwrap().1)
                }
            }

            fn second_context(contexts: &Vec<(&str, StackContext)>) -> Result<StackContext,()> {
                if contexts.len() < 2 {
                    Err(())
                } else {
                    Ok(contexts.get(1).cloned().unwrap().1)
                }
            }

            fn diagnose_contexts(contexts: &Vec<(&str, StackContext)>) {
                for context in contexts.iter().rev() {
                    let c = match context.1 {
                        StackContext::Kind(_) => {"?"}
                        StackContext::Context(ctx) => {ctx}
                    };
                    print!("{}.",c);
                }
                println!();
            }



            fn extract_problem_line( input: &str ) -> String {
                if input.trim().is_empty() {
                    return "Problem: \"\"".to_string();
                }

                if input.len() < 80 {
                    return format!("Problem: \"{}\"",input.trim_end().lines().next().expect("line"));
                }

                format!("Problem: \"{}\"",input[0..80].trim_end().lines().next().expect("line"))
            }

            fn contexts_to_error( mut contexts: Vec<(&str,StackContext)>) -> String {

                if contexts.is_empty() {
                    return "internal parsing error: could not create usable error message because of missing context".to_string();
                }

                let (input, context) = contexts.remove(0);

                fn parent( contexts: &Vec<(&str,StackContext)>) -> String {
                    match contexts.first() {
                        None => {"?"}
                        Some((_,context)) => {
                            match context {
                                StackContext::Kind(_) => {"!"}
                                StackContext::Context(context) => {context}
                            }
                        }
                    }.to_string()
                }

                let message = match context {
                    StackContext::Kind(kind) => {
                         format!(": parse error: '{}'",kind.description())
                    }
                    StackContext::Context(context) => {
                        if context.starts_with("!") {
                            context[1..].to_string()
                        } else if context == "scope:expect-selector" {
                            format!( "expected '{}' (scope selector)", parent(&contexts) )
                        } else {
                            match context {
                                what => {
                                    format!("internal parsing error: no parse error handler for context '{}'", what)
                                }
                            }
                        }
                    }
                };

                contexts.reverse();
                let mut hierarchy = String::new();
                let len = contexts.len();
                for (index, (_,context)) in contexts.into_iter().enumerate() {
                    match context {
                        StackContext::Kind(kind) => {
                            return format!("{}\n\ninternal parsing error: unexpected kind error when processing context hierarchy: '{}'", extract_problem_line(input),kind.description() );
                        }
                        StackContext::Context(context) => {
                            hierarchy.push_str(context);
                            if index < len - 1 {
                                hierarchy.push_str(".");
                            }
                        }
                    }
                }

                format!("{}\n\n{}: {}", extract_problem_line(input), hierarchy, message )
            }

            pub fn bind(input: &str) -> Res<&str, ProtoBind> {
                select_scope("Bind", select_pipelines)(input).map(|(next, sections)| {
                    let bind = ProtoBind { sections };

                    (next, bind)
                })
            }



            pub fn many_until0<I, O, O2, E, F,U>(mut f: F, mut until: U) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
                where
                    I: Clone + InputLength+std::fmt::Display,
                    F: Parser<I, O, E>,
                    U: Parser<I, O2, E>,
                    E: ParseError<I>,
            {
                move |mut i: I| {
                    let mut acc = vec![];
                    loop {
                        let len = i.input_len();

                        match until.parse(i.clone()) {
                            Ok(_) => {
                                return Ok((i, acc));
                            }
                            Err(e2) => {
                                // ignore
                            }
                        }

                        match f.parse(i.clone()) {
                            Err(nom::Err::Error(e)) => {
                                return Err(nom::Err::Error(e));
                            },
                            Err(e) => return Err(e),
                            Ok((i1, o)) => {
                                // infinite loop check: the parser must always consume
                                if i1.input_len() == len {
                                    return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Many0)));
                                }
                                i = i1;
                                acc.push(o);
                            }
                        }
                    }
                }
            }


            pub fn select0<I, O, O2, E, F,U>(mut f: F, mut until: U) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
                where
                    I: Clone + InputLength+std::fmt::Display,
                    F: Parser<I, O, E>,
                    U: Parser<I, O2, E>,
                    E: ParseError<I>+ContextError<I>,
            {
                context("select",many_until0(f,context("scope:close",until)) )
            }

            /// successfully parse at least one branch or fail
            /// does not return errors for any branches in a fail
            pub fn alt_fail<I: Clone, O, E: ParseError<I>, List: Alt<I, O, E>>(
                mut l: List,
            ) -> impl FnMut(I) -> IResult<I, O, E> {
                move |i: I| {
                    match l.choice(i.clone()) {
                        Ok(ok) => {Ok(ok)}
                        Err(_) => {
                            let e = E::from_error_kind(i,ErrorKind::Alt );
                            Err(Err::Failure(e))
                        }
                    }
                }
            }


            pub struct SelectBlock<S,F>(pub S,pub F);

            pub fn select_block<I, E, S,F, O, O2>(
                mut select_blocks: Vec<SelectBlock<S, F>>,
            ) -> impl FnMut(I) -> IResult<I, O, E>
                where
                    I: Clone+InputTake+InputLength+InputIter+ Compare<&'static str>+InputTakeAtPosition+ToString,
                    <I as InputIter>::Item: AsChar,
                    <I as InputIter>::Item: Clone,
                    <I as InputTakeAtPosition>::Item: AsChar + Clone,
                    F: Parser<I, O, E>,
                    S: Parser<I, O2, E>,
                    E: ParseError<I> + ContextError<I>,
            {
                let parser = move | i: I | {

                    for mut select_block in select_blocks.iter_mut() {
                        match select_block.0.parse(i.clone())
                        {
                            Ok((i,_)) => {
                                return select_block.1.parse(i.clone());
                            }
                            Err(_) => {}
                        }
                    }

                    // if a block hasn't been selected yet then we have an error

                    let e = E::from_error_kind(i.clone(),ErrorKind::Tag);
                    let e = E::add_context(i, "!select-block", e );
                    return Err(Err::Failure(e))
                };

                parser
            }

            pub fn multi_select_scope<I, E, S,F, O, O2>(
                mut selectors: S,
                mut f: F,
            ) -> impl FnMut(I) -> IResult<I, O, E>
                where
                    I: Clone+InputTake+InputLength+InputIter+ Compare<&'static str>+InputTakeAtPosition+ToString,
                    <I as InputIter>::Item: AsChar,
                    <I as InputIter>::Item: Clone,
                    <I as InputTakeAtPosition>::Item: AsChar + Clone,
                    F: Parser<I, O, E>,
                    S: Parser<I, O2, E>,
                    E: ParseError<I> + ContextError<I>,
            {
                let parser = move | i: I | {

                    // first remove any whitespace
                    let (i,_) = multispace0(i.clone())?;

                    // next if we hit the until condition return Error (instead of Failure)
                    match not(tag("}"))(i.clone()) {
                        Ok(_) => {
                            // do nothing
                        }
                        Err(err) => {
                            return Err(err);
                        }
                    }

                    match selectors.parse(i.clone()) {
                        Ok(_) => {
                            f.parse(i)
                        },
                        Err(e) => {
                            match e {
                                Err::Incomplete(_) => {Err(e)}
                                Err::Error(e2) => {
                                    Err(Err::Failure(E::add_context(i,"multi_select_scope", e2)))
//                                    Err(Err::Failure(e2))
                                }
                                Err::Failure(e2) => {
                                    Err(Err::Failure(E::add_context(i.clone(),"multi_select_scope", e2)))
                                }
                            }
                        },
                    }
                };

                parser
            }

            fn select_scope<I, E, F, O>(
                selection: &'static str,
                mut f: F,
            ) -> impl FnMut(I) -> IResult<I, O, E>
            where
                I: Clone+InputTake+InputLength+InputIter+ Compare<&'static str>+InputTakeAtPosition,
                <I as InputIter>::Item: AsChar,
                <I as InputIter>::Item: Clone,
                <I as InputTakeAtPosition>::Item: AsChar + Clone,
                F: Parser<I, O, E>,
                E: ParseError<I> + ContextError<I>,
            {
                let parser = move |i: I| {
                    let (next,_) = context("selector",tag(selection))(i)?;
                    let (next,_) = multispace0(next)?;
                    let (next,_) = context( "scope:open", tag("{"), )(next)?;
                    let (next,_) = multispace0(next)?;
                    let (next, rtn ) = f.parse(next)?;
                    let (next,_) = multispace0(next)?;
                    let (next,_) = context( "scope:close", tag("}"), )(next)?;
                    Ok( (next, rtn))
                };

                context(selection, parser )
            }

            fn scope<I,E, F, O, O2, O3, Open, Close>(
                mut open: Open,
                mut f: F,
                mut close: Close,
            ) -> impl FnMut(I) -> IResult<I, O, E>
                where
                    I: Clone+InputTake+InputLength+InputIter+ Compare<&'static str>+InputTakeAtPosition,
                    <I as InputIter>::Item: AsChar,
                    <I as InputIter>::Item: Clone,
                    <I as InputTakeAtPosition>::Item: AsChar + Clone,
                    F: Parser<I, O, E>,
                    Open: Parser<I, O2, E>,
                    Close: Parser<I, O3, E>,
                    E: ParseError<I> + ContextError<I>,
            {
                move |i: I| {

                    let (next,_) = open.parse(i)?;
                    let (next,_) = multispace0(next)?;
                    let (next, rtn ) = f.parse(next)?;
                    let (next,_) = multispace0(next)?;
                    let (next,_) = close.parse(next)?;
                    Ok( (next, rtn))
                }
            }


            fn whitespace_until<I,E, F, O>(
                mut f: F,
            ) -> impl FnMut(I) -> IResult<I, O, E>
                where
                    I: Clone+InputTake+InputLength+InputIter+ Compare<&'static str>+InputTakeAtPosition,
                    <I as InputIter>::Item: AsChar,
                    <I as InputIter>::Item: Clone,
                    <I as InputTakeAtPosition>::Item: AsChar + Clone,
                    F: Parser<I, O, E>,
                    E: ParseError<I> + ContextError<I>,
            {
                move |i: I| {
                    let (next,_) = multispace0(i)?;
                    let (next,rtn) = f.parse(next)?;
                    Ok( (next, rtn))
                }
            }

            fn padded_curly_open<I>(input:I) -> Res<I,I>
                where
                    I: Clone+InputTake+InputLength+InputIter+ Compare<&'static str>+InputTakeAtPosition,
                    <I as InputIter>::Item: AsChar,
                    <I as InputIter>::Item: Clone,
                    <I as InputTakeAtPosition>::Item: AsChar + Clone,
            {
                delimited(multispace0,context("!expected: '{'",tag("{")),multispace0)(input)
            }

            fn padded_curly_close<I>(input:I) -> Res<I,I>
                where
                    I: Clone+InputTake+InputLength+InputIter+ Compare<&'static str>+InputTakeAtPosition,
                    <I as InputIter>::Item: AsChar,
                    <I as InputIter>::Item: Clone,
                    <I as InputTakeAtPosition>::Item: AsChar + Clone,
            {
                preceded(multispace0,context("!expected: '}'",tag("}")))(input)
            }


            pub fn select_pipelines(input: &str) -> Res<&str, Vec<PipelinesSubScope>> {
                select_scope("Pipelines", select0(pipelines, whitespace_until(tag("}"))))(input)
            }

            pub fn pipelines(input: &str) -> Res<&str, PipelinesSubScope> {
                multi_select_scope(
                    alt((tag("Rc"),tag("Msg"),tag("Http"))),
                    alt((
                        select_msg_pipelines,
                        select_http_pipelines,
                        select_rc_pipelines,
                )))(input)
            }

            pub fn select_msg_pipelines(input: &str) -> Res<&str, PipelinesSubScope> {
                select_scope("Msg", msg_selectors )(input).map(|(next, selectors)| {
                    (
                        next,
                        PipelinesSubScope::Msg(ConfigScope::new(EntityType::Msg, selectors)),
                    )
                })
            }

            pub fn select_http_pipelines(input: &str) -> Res<&str, PipelinesSubScope> {
                select_scope("Http", http_pipelines)(input).map(|(next, selectors)| {
                    (
                        next,
                        PipelinesSubScope::Http(ConfigScope::new(EntityType::Http, selectors)),
                    )
                })
            }

            pub fn select_rc_pipelines(input: &str) -> Res<&str, PipelinesSubScope> {
                select_scope("Rc", rc_selectors)(input).map(|(next, selectors)| {
                    (
                        next,
                        PipelinesSubScope::Rc(ConfigScope::new(EntityType::Rc, selectors)),
                    )
                })
            }

            pub fn pipeline_step(input: &str) -> Res<&str, PipelineStep> {
                context( "Step",tuple((
                    select0(pipeline_step_block, alt((context("selector",tag("->")), context("selector",tag("=>")))) ),
                    context(
                        "!pipeline-step:exit",
                        alt((tag("->"), tag("=>"), fail )),
                    ),
                )))(input)
                .map(|(next, (blocks, kind))| {
                    let kind = match kind {
                        "->" => StepKind::Request,
                        "=>" => StepKind::Response,
                        _ => panic!("nom parse rules should have selected -> or =>"),
                    };
                    (next, PipelineStep { kind, blocks })
                })
            }

            pub fn core_pipeline_stop(input: &str) -> Res<&str, PipelineStop> {
                context("Core", delimited(
                    tag("{{"),
                    delimited(multispace0, opt(tag("*")), multispace0),
                    tag("}}"),
                ))(input)
                .map(|(next, _)| (next, PipelineStop::Internal))
            }

            pub fn return_pipeline_stop(input: &str) -> Res<&str, PipelineStop> {
                tag("&")(input).map(|(next, _)| (next, PipelineStop::Return))
            }

            pub fn call_pipeline_stop(input: &str) -> Res<&str, PipelineStop> {
                context("Call",call)(input).map(|(next, call)| (next, PipelineStop::Call(call)))
            }

            pub fn address_pipeline_stop(input: &str) -> Res<&str, PipelineStop> {
                context("Address",capture_address)(input)
                    .map(|(next, address)| (next, PipelineStop::CaptureAddress(address)))
            }

            pub fn pipeline_stop(input: &str) -> Res<&str, PipelineStop> {
                context("Stop", alt((
                    core_pipeline_stop,
                    return_pipeline_stop,
                    call_pipeline_stop,
                    address_pipeline_stop,
                )))(input)
            }

            pub fn consume_pipeline_step(input: &str) -> Res<&str, PipelineStep> {
                all_consuming(pipeline_step)(input)
            }

            pub fn consume_pipeline_stop(input: &str) -> Res<&str, PipelineStop> {
                all_consuming(pipeline_stop)(input)
            }

            pub fn pipeline_segment(input: &str) -> Res<&str, PipelineSegment> {
                tuple((
                    multispace0,
                    pipeline_step,
                    multispace0,
                    pipeline_stop,
                    multispace0,
                ))(input)
                .map(|(next, (_, step, _, stop, _))| (next, PipelineSegment { step, stop }))
            }

            pub fn pipeline(input: &str) -> Res<&str, Pipeline> {
                context("Pipeline",many_until0(pipeline_segment, tuple((multispace0,tag(";")))))(input).map(|(next, segments)| (next, Pipeline { segments }))
            }

            pub fn consume_pipeline(input: &str) -> Res<&str, Pipeline> {
                all_consuming(pipeline)(input)
            }

            pub fn entity_selectors(input: &str) -> Res<&str, Vec<Selector<EntityPattern>>> {
                many0(delimited(multispace0, entity_selector, multispace0))(input)
            }

            pub fn msg_selectors(input: &str) -> Res<&str, Vec<Selector<MsgPattern>>> {
                select0(delimited(multispace0, msg_selector, multispace0),padded_curly_close)(input)
            }

            pub fn http_pipelines(input: &str) -> Res<&str, Vec<Selector<HttpPattern>>> {
                select0(delimited(multispace0, http_pipeline, multispace0), padded_curly_close)(input)
            }

            pub fn rc_selectors(input: &str) -> Res<&str, Vec<Selector<RcPattern>>> {
                select0(delimited(multispace0, rc_selector, multispace0), padded_curly_close)(input)
            }

            pub fn entity_selector(input: &str) -> Res<&str, Selector<EntityPattern>> {
                tuple((entity_pattern, multispace0, pipeline, tag(";")))(input).map(
                    |(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)),
                )
            }

            pub fn msg_selector(input: &str) -> Res<&str, Selector<MsgPattern>> {
                tuple((msg_pattern_scoped, multispace0, pipeline, tag(";")))(input).map(
                    |(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)),
                )
            }

            pub fn final_http_pipeline(input: &str) -> Result<Selector<HttpPattern>, MsgErr> {
                final_parser(http_pipeline)(input).map_err( |e:ErrorTree<&str>| {
                    e.to_string().into()
                })
            }

            pub fn http_pipeline(input: &str) -> Res<&str, Selector<HttpPattern>> {
                tuple((http_pattern_scoped, multispace0, pipeline, tag(";")))(input).map(
                    |(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)),
                )
            }

            pub fn rc_selector(input: &str) -> Res<&str, Selector<RcPattern>> {
                tuple((rc_pattern_scoped, multispace0, pipeline, tag(";")))(input).map(
                    |(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)),
                )
            }

            pub fn consume_selector(input: &str) -> Res<&str, Selector<EntityPattern>> {
                all_consuming(entity_selector)(input)
            }
        }
    }
}

pub mod entity {

    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum EntityType {
        Rc,
        Msg,
        Http,
    }


    pub mod request {
        use http::{HeaderMap, Request, StatusCode, Uri};
        use http::status::InvalidStatusCode;
        use crate::error::{MsgErr, StatusErr};
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::entity::request::create::Create;
        use crate::version::v0_0_1::entity::request::get::Get;
        use crate::version::v0_0_1::entity::request::query::Query;
        use crate::version::v0_0_1::entity::request::select::Select;
        use crate::version::v0_0_1::entity::request::set::Set;
        use crate::version::v0_0_1::entity::request::update::Update;
        use crate::version::v0_0_1::entity::response::ResponseCore;
        use crate::version::v0_0_1::fail;
        use crate::version::v0_0_1::fail::{BadRequest, Fail, NotFound};
        use crate::version::v0_0_1::id::{Address, Meta, PayloadClaim, ResourceKind, ResourceType};
        use crate::version::v0_0_1::pattern::TksPattern;
        use crate::version::v0_0_1::payload::{Errors, HttpMethod, Payload, Primitive};
        use crate::version::v0_0_1::util::ValueMatcher;
        use serde::{Deserialize, Serialize};


        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Action {
            Rc(Rc),

            Http(#[serde(with = "http_serde::method")]HttpMethod),
            Msg(String),
        }

        impl Into<RequestCore> for Action {
            fn into(self) -> RequestCore {
                RequestCore {
                    headers: Default::default(),
                    action: self,
                    uri: Uri::from_static("/"),
                    body: Payload::Empty,
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct RequestCore {
            #[serde(with = "http_serde::header_map")]
            pub headers: HeaderMap,
            pub action: Action,
            #[serde(with = "http_serde::uri")]
            pub uri: Uri,
            pub body: Payload,
        }

        impl From<http::Request<Bin>> for RequestCore {
            fn from(request: Request<Bin>) -> Self {
                Self {
                    headers: request.headers().clone(),
                    action: Action::Http(request.method().clone()),
                    uri: request.uri().clone(),
                    body: Payload::Primitive(Primitive::Bin(request.body().clone()))
                }
            }
        }

        impl TryInto<http::Request<Bin>> for RequestCore {
            type Error = MsgErr;

            fn try_into(self) -> Result<http::Request<Bin>, MsgErr>{
                let mut builder = http::Request::builder();
                for (name,value) in self.headers {
                    match name {
                        Some(name) => {
                            builder = builder.header(name.as_str(),value.to_str()?.to_string().as_str() );
                        }
                        None => {}
                    }
                }
                match self.action {
                    Action::Http(method) => {
                        builder = builder.method(method).uri(self.uri);
                        Ok(builder.body( self.body.to_bin()? )?)
                    }
                    _ => {
                        Err("cannot convert to http response".into())
                    }
                }

            }
        }


        impl Default for RequestCore {
            fn default() -> Self {
                Self {
                    headers: Default::default(),
                    action: Action::Msg("Default".to_string()),
                    uri: Uri::from_static("/"),
                    body: Payload::Empty,
                }
            }
        }

        impl RequestCore {
            pub fn with_new_payload(self, payload: Payload) -> Self {
                Self {
                    headers: self.headers,
                    uri: self.uri,
                    action: self.action,
                    body: payload,
                }
            }

            pub fn not_found(&self) -> ResponseCore {
                ResponseCore {
                    headers: Default::default(),
                    status: StatusCode::from_u16(404u16).unwrap(),
                    body: Payload::Empty,
                }
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
                    body: Payload::Primitive(Primitive::Errors(errors)),
                }
            }

            pub fn err<E: StatusErr>(&self, error: E) -> ResponseCore {
                let errors = Errors::default(error.message().as_str());
                let status = match StatusCode::from_u16(error.status()) {
                    Ok(status) => status,
                    Err(_) => StatusCode::from_u16(500u16).unwrap()
                };
 println!("----->   returning STATUS of {}",status.as_str());
                ResponseCore {
                    headers: Default::default(),
                    status,
                    body: Payload::Primitive(Primitive::Errors(errors)),
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Rc {
            Create(Create),
            Select(Select),
            Update(Update),
            Query(Query),
            Get(Get),
            Set(Set),
        }

        impl Rc {
            pub fn get_type(&self) -> RcCommandType {
                match self {
                    Rc::Create(_) => RcCommandType::Create,
                    Rc::Select(_) => RcCommandType::Select,
                    Rc::Update(_) => RcCommandType::Update,
                    Rc::Query(_) => RcCommandType::Query,
                    Rc::Get(_) => RcCommandType::Get,
                    Rc::Set(_) => RcCommandType::Set,
                }
            }
        }

        /*
        impl Rc {
            pub fn command_handler(&self, request_to: &Address) -> Result<Address,Error> {
                match self {
                    Rc::Create(create) => { Ok(create.template.address.parent.clone()) }
                    Rc::Select(select) => { Ok(select.pattern.query_root()) }
                    Rc::Update(_) => {request_to.clone()}
                    Rc::Query(_) => { request_to.clone()}
                    Rc::GET(_) => {request_to.parent().as_ref().ok_or("expected parent for get request").clone()}
                    Rc::Set(_) => {request_to.parent().as_ref().ok_or("expected parent for set request").clone()}
                }
            }
        }

         */

        #[derive(
            Debug,
            Clone,
            Eq,
            PartialEq,
            strum_macros::Display,
            strum_macros::EnumString,
            Serialize,
            Deserialize,
        )]
        pub enum RcCommandType {
            Create,
            Select,
            Update,
            Query,
            Get,
            Set,
        }

        impl ValueMatcher<Rc> for Rc {
            fn is_match(&self, x: &Rc) -> Result<(), crate::error::MsgErr> {
                if self.get_type() == x.get_type() {
                    Ok(())
                } else {
                    Err(format!(
                        "Rc command expected: '{}' found: '{}'",
                        self.to_string(),
                        x.to_string()
                    )
                    .into())
                }
            }
        }

        impl ToString for Rc {
            fn to_string(&self) -> String {
                format!("Rc<{}>", self.get_type().to_string())
            }
        }

        pub mod set {
            use crate::version::v0_0_1::command::common::SetProperties;
            use crate::version::v0_0_1::id::Address;
            use serde::{Deserialize, Serialize};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Set {
                pub address: Address,
                pub properties: SetProperties,
            }
        }

        pub mod get {
            use crate::version::v0_0_1::command::common::SetProperties;
            use crate::version::v0_0_1::id::Address;
            use serde::{Deserialize, Serialize};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Get {
                pub address: Address,
                pub op: GetOp,
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum GetOp {
                State,
                Properties(Vec<String>),
            }
        }

        pub mod create {
            use std::convert::TryInto;

            use serde::{Deserialize, Serialize};

            use crate::error::MsgErr;
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::command::common::{SetProperties, SetRegistry, StateSrc};
            use crate::version::v0_0_1::id::{Address, AddressSegment, HostKey, ResourceKind};
            use crate::version::v0_0_1::pattern::SpecificPattern;
            use crate::version::v0_0_1::payload::{Payload, Primitive};
            use crate::version::v0_0_1::util::ConvertFrom;

            pub enum AddressTemplateSegment {
                AddressSegment(AddressSegment),
                Wildcard(String),
            }

            impl AddressTemplateSegment {
                pub fn is_wildcard(&self) -> bool {
                    match self {
                        AddressTemplateSegment::AddressSegment(_) => false,
                        AddressTemplateSegment::Wildcard(_) => true,
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Template {
                pub address: AddressTemplate,
                pub kind: KindTemplate,
            }

            impl Template {
                pub fn new(address: AddressTemplate, kind: KindTemplate) -> Self {
                    Self { address, kind }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct KindTemplate {
                pub resource_type: String,
                pub kind: Option<String>,
                pub specific: Option<SpecificPattern>,
            }

            impl TryInto<ResourceKind> for KindTemplate {
                type Error = MsgErr;

                fn try_into(self) -> Result<ResourceKind, Self::Error> {
                    if self.specific.is_some() {
                        return Err("cannot create a ResourceKind from a specific pattern when using KindTemplate".into());
                    }
                    Ok(ResourceKind {
                        resource_type: self.resource_type,
                        kind: self.kind,
                        specific: None,
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Require {
                File(String),
                Auth(String)
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Fulfillment {
                File { name: String, content: Bin },
                Complete,
            }

            pub struct CreateOp {
                pub template: Template,
                pub properties: SetProperties,
                pub strategy: Strategy,
                pub registry: SetRegistry,
                pub state: StateSrc,
                pub requirements: Vec<Require>,
            }

            impl CreateOp {
                pub fn fulfillment(mut self, bin: Bin) -> Create {
                    Create {
                        template: self.template,
                        state: StateSrc::StatefulDirect(Payload::Primitive(Primitive::Bin(bin))),
                        properties: self.properties,
                        strategy: self.strategy,
                        registry: self.registry,
                    }
                }
            }

            impl Into<Create> for CreateOp {
                fn into(self) -> Create {
                    Create {
                        template: self.template,
                        state: self.state,
                        properties: self.properties,
                        strategy: self.strategy,
                        registry: self.registry,
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Create {
                pub template: Template,
                pub state: StateSrc,
                pub properties: SetProperties,
                pub strategy: Strategy,
                pub registry: SetRegistry,
            }

            impl Create {
                pub fn new(template: Template) -> Self {
                    Self {
                        template,
                        state: StateSrc::Stateless,
                        properties: Default::default(),
                        strategy: Strategy::Create,
                        registry: Default::default(),
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
            pub enum Strategy {
                Create,
                Apply,
                Ensure,
                HostedBy(HostKey),
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct AddressTemplate {
                pub parent: Address,
                pub child_segment_template: AddressSegmentTemplate,
            }

            #[derive(Debug, Clone, strum_macros::Display, Serialize, Deserialize)]
            pub enum AddressSegmentTemplate {
                Exact(String),
                Pattern(String), // must have a '%'
            }
        }

        pub mod select {
            use std::collections::{HashMap, HashSet};
            use std::convert::{TryFrom, TryInto};
            use std::marker::PhantomData;

            use serde::{Deserialize, Serialize};

            use crate::error::MsgErr;
            use crate::version::v0_0_1::fail::{BadCoercion, Fail};
            use crate::version::v0_0_1::id::Address;
            use crate::version::v0_0_1::pattern::{AddressKindPath, AddressKindPattern, Hop};
            use crate::version::v0_0_1::payload::{
                MapPattern, Primitive, PrimitiveList, PrimitiveType,
            };
            use crate::version::v0_0_1::resource::ResourceStub;
            use crate::version::v0_0_1::util::ConvertFrom;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum SelectIntoPayload {
                Stubs,
                Addresses,
            }

            impl SelectIntoPayload {
                pub fn to_primitive(
                    &self,
                    stubs: Vec<ResourceStub>,
                ) -> Result<PrimitiveList, MsgErr> {
                    match self {
                        SelectIntoPayload::Stubs => {
                            let stubs: Vec<Primitive> = stubs
                                .into_iter()
                                .map(|stub| Primitive::Stub(stub))
                                .collect();
                            let stubs = PrimitiveList {
                                primitive_type: PrimitiveType::Stub,
                                list: stubs,
                            };
                            Ok(stubs)
                        }
                        SelectIntoPayload::Addresses => {
                            let addresses: Vec<Primitive> = stubs
                                .into_iter()
                                .map(|stub| Primitive::Address(stub.address))
                                .collect();
                            let stubs = PrimitiveList {
                                primitive_type: PrimitiveType::Address,
                                list: addresses,
                            };
                            Ok(stubs)
                        }
                    }

                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Select {
                pub pattern: AddressKindPattern,
                pub properties: PropertiesPattern,
                pub into_payload: SelectIntoPayload,
                pub kind: SelectKind,
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum SelectKind {
                Initial,
                SubSelect {
                    address: Address,
                    hops: Vec<Hop>,
                    address_kind_path: AddressKindPath,
                },
            }

            impl Select {
                pub fn sub_select(
                    self,
                    address: Address,
                    hops: Vec<Hop>,
                    address_kind_path: AddressKindPath,
                ) -> SubSelect {
                    SubSelect {
                        address,
                        pattern: self.pattern,
                        properties: self.properties,
                        into_payload: self.into_payload,
                        hops,
                        address_kind_path,
                    }
                }
            }

            impl TryInto<SubSelect> for Select {
                type Error = MsgErr;

                fn try_into(self) -> Result<SubSelect, Self::Error> {
                    if let SelectKind::SubSelect {
                        address,
                        hops,
                        address_kind_path,
                    } = self.kind
                    {
                        Ok(SubSelect {
                            address,
                            pattern: self.pattern,
                            properties: self.properties,
                            into_payload: self.into_payload,
                            hops: hops,
                            address_kind_path,
                        })
                    } else {
                        Err("Not of kind SubSelector".into())
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct SubSelect {
                pub address: Address,
                pub pattern: AddressKindPattern,
                pub properties: PropertiesPattern,
                pub into_payload: SelectIntoPayload,
                pub hops: Vec<Hop>,
                pub address_kind_path: AddressKindPath,
            }

            impl Into<Select> for SubSelect {
                fn into(self) -> Select {
                    Select {
                        pattern: self.pattern,
                        properties: self.properties,
                        into_payload: self.into_payload,
                        kind: SelectKind::SubSelect {
                            address: self.address,
                            hops: self.hops,
                            address_kind_path: self.address_kind_path,
                        },
                    }
                }
            }

            impl SubSelect {
                pub fn sub_select(
                    &self,
                    address: Address,
                    hops: Vec<Hop>,
                    address_kind_path: AddressKindPath,
                ) -> SubSelect {
                    SubSelect {
                        address,
                        pattern: self.pattern.clone(),
                        properties: self.properties.clone(),
                        into_payload: self.into_payload.clone(),
                        hops,
                        address_kind_path,
                    }
                }
            }

            impl Select {
                fn new(pattern: AddressKindPattern) -> Self {
                    Self {
                        pattern,
                        properties: Default::default(),
                        into_payload: SelectIntoPayload::Stubs,
                        kind: SelectKind::Initial,
                    }
                }
            }

            pub type PropertiesPattern = MapPattern;
        }

        pub mod update {
            use std::convert::TryInto;

            use serde::{Deserialize, Serialize};

            use crate::error::MsgErr;
            use crate::version::v0_0_1::command::common::SetProperties;
            use crate::version::v0_0_1::id::Address;
            use crate::version::v0_0_1::payload::Payload;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Update {
                pub payload: Payload
            }
        }

        pub mod query {
            use std::convert::TryInto;

            use serde::{Deserialize, Serialize};

            use crate::error::MsgErr;
            use crate::version::v0_0_1::entity::request::Rc;
            use crate::version::v0_0_1::pattern::AddressKindPath;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Query {
                AddressKindPath,
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum QueryResult {
                AddressKindPath(AddressKindPath),
            }

            impl TryInto<AddressKindPath> for QueryResult {
                type Error = MsgErr;

                fn try_into(self) -> Result<AddressKindPath, MsgErr> {
                    match self {
                        QueryResult::AddressKindPath(address_kind_path) => Ok(address_kind_path),
                    }
                }
            }

            impl ToString for QueryResult {
                fn to_string(&self) -> String {
                    match self {
                        QueryResult::AddressKindPath(address_kind_path) => {
                            address_kind_path.to_string()
                        }
                    }
                }
            }

            impl Into<Rc> for Query {
                fn into(self) -> Rc {
                    Rc::Query(self)
                }
            }
        }
    }

    pub mod response {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::entity::request::RequestCore;
        use crate::version::v0_0_1::fail;
        use crate::version::v0_0_1::fail::Fail;
        use crate::version::v0_0_1::id::{Address, Meta, ResourceKind};
        use crate::version::v0_0_1::messaging::Response;
        use crate::version::v0_0_1::payload::{Errors, Payload, Primitive};
        use crate::version::v0_0_1::util::unique_id;
        use serde::{Deserialize, Serialize};
        use std::sync::Arc;
        use http::{HeaderMap, StatusCode};
        use http::response::Parts;
        use crate::version::v0_0_1::bin::Bin;

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
                ResponseCore::ok(Payload::Primitive(Primitive::Bin(bin)))
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
                    body: Payload::Primitive(Primitive::Errors(errors)),
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
                return self.status.is_success()
            }

            pub fn into_response(
                self,
                from: Address,
                to: Address,
                response_to: String,
            ) -> Response {
                Response {
                    id: unique_id(),
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

                for (name,value) in self.headers {
                    match name {
                        Some(name) => {
                            builder = builder.header(name.as_str(),value.to_str()?.to_string().as_str() );
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

                for (name,value) in self.headers {
                    match name {
                        Some(name) => {
                            builder = builder.header(name.as_str(),value.to_str()?.to_string().as_str() );
                        }
                        None => {}
                    }
                }

                let response = builder.status(self.status).body(self.body.to_bin()?)?;
                Ok(response)
            }
        }
    }
}

pub mod resource {
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
    use crate::version::v0_0_1::id::{Address, AddressAndKind, ResourceKind, ResourceType};
    use crate::version::v0_0_1::parse::{address, Res};
    use crate::version::v0_0_1::payload::{Payload, PayloadMap};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct StatusUpdate {
        pub from: Address,
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
        Panic, // something is wrong... all requests are blocked and responses are cancelled.
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

    pub fn ok_code(input: &str) -> Res<&str, Code> {
        tag("Ok")(input).map(|(next, code)| (next, Code::Ok))
    }

    pub fn error_code(input: &str) -> Res<&str, Code> {
        let (next, err_code) = delimited(tag("Err("), digit1, tag(")"))(input)?;
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

    pub fn code(input: &str) -> Res<&str, Code> {
        alt((error_code, ok_code))(input)
    }

    pub fn status(input: &str) -> Res<&str, Status> {
        parse_from_str(alpha1).parse(input)
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
        pub kind: ResourceKind,
        pub properties: Properties,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct ResourceStub {
        pub address: Address,
        pub kind: ResourceKind,
        pub properties: Properties,
        pub status: Status,
    }

    impl ResourceStub {
        pub fn address_and_kind(self) -> AddressAndKind {
            AddressAndKind {
                address: self.address,
                kind: self.kind,
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Resource {
        pub stub: ResourceStub,
        pub state: Box<Payload>,
    }
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
        use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
        use crate::version::v0_0_1::id::{Address, ResourceKind, ResourceType};
        use crate::version::v0_0_1::messaging::{Request, Response};
        use crate::version::v0_0_1::pattern::TksPattern;
        use crate::version::v0_0_1::payload::Payload;
        use crate::version::v0_0_1::portal;
        use crate::version::v0_0_1::portal::Exchanger;
        use crate::version::v0_0_1::resource::StatusUpdate;
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
            pub fn from(&self) -> Option<Address> {
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
        use crate::version::v0_0_1::config::{Assign, Config, ConfigBody};
        use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
        use crate::version::v0_0_1::id::{Address, ResourceKind, ResourceType};
        use crate::version::v0_0_1::messaging::{Request, Response};
        use crate::version::v0_0_1::payload::Payload;
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
            pub fn to(&self) -> Option<Address> {
                match self {
                    Frame::Assign(assign) => Option::Some(assign.stub.address.clone()),
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
        use crate::version::v0_0_1::frame::PrimitiveFrame;
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
        use crate::version::v0_0_1::frame::PrimitiveFrame;
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
    use crate::version::v0_0_1::payload::HttpMethod;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum MethodPattern {
        Any,
        None,
        Pattern(#[serde(with = "http_serde::method")]HttpMethod),
    }

    impl MethodPattern{
        pub fn is_match(&self, x: &HttpMethod) -> Result<(), MsgErr>
        {
            match self {
                Self::Any => Ok(()),
                Self::Pattern(exact) => exact.is_match(x),
                Self::None => Err("None pattern".into()),
            }
        }

        pub fn is_match_opt(&self, x: Option<&HttpMethod>) -> Result<(), MsgErr>
        {
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

    #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
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
    use crate::version::v0_0_1::id::Specific;

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
        use crate::version::v0_0_1::id::Address;

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
            WrongAddress { required: Address, found: Address },
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

pub mod parse {
    use std::str::FromStr;

    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::bytes::complete::{is_a, is_not};
    use nom::character::complete::{alpha0, alpha1, alphanumeric1, char, digit1, multispace0, multispace1, space0, space1};
    use nom::combinator::{all_consuming, not, opt, recognize, value};
    use nom::error::{context, ErrorKind, ParseError, VerboseError};
    use nom::multi::{many0, separated_list0, separated_list1};
    use nom::sequence::{delimited, preceded, terminated, tuple};
    use nom::{AsChar, IResult, InputTakeAtPosition};
    use nom_supreme::parse_from_str;

    use crate::error::MsgErr;
    use crate::version::v0_0_1::command::common::{PropertyMod, SetProperties, StateSrc};
    use crate::version::v0_0_1::config::bind::parse::pipeline_step;
    use crate::version::v0_0_1::entity::request::create::{
        AddressSegmentTemplate, AddressTemplate, AddressTemplateSegment, Create, CreateOp,
        KindTemplate, Require, Strategy, Template,
    };
    use crate::version::v0_0_1::entity::request::get::{Get, GetOp};
    use crate::version::v0_0_1::entity::request::select::{Select, SelectIntoPayload, SelectKind};
    use crate::version::v0_0_1::entity::request::set::Set;
    use crate::version::v0_0_1::id::{
        Address, AddressSegment, CaptureAddress, RouteSegment, Version,
    };
    use crate::version::v0_0_1::pattern::parse::{
        address_kind_pattern, delim_kind, kind, resource_type, specific, specific_pattern, version,
    };
    use crate::version::v0_0_1::pattern::{
        skewer, upload_step, AddressKindPath, AddressKindSegment,
    };
    use crate::version::v0_0_1::util::StringMatcher;
    use nom::bytes::complete::take;
    use nom_supreme::error::ErrorTree;
    use crate::version::v0_0_1::security::{ChildPerms, ParticlePerms, Permissions, PermissionsMask, PermissionsMaskKind};
    use crate::version::v0_0_1::Span;

    pub struct Parser {}

    impl Parser {
        pub fn address(input: &str) -> Res<&str, Address> {
            address(input)
        }

        pub fn consume_address(input: &str) -> Result<Address, MsgErr> {
            let (_, address) = all_consuming(address)(input)?;
            Ok(address)
        }
    }

    pub type Res<I, O> = IResult<I, O, ErrorTree<I>>;

    fn any_resource_path_segment<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !(char_item == '/')
                    && !(char_item == '_')
                    && !(char_item.is_alpha() || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    fn mesh_route_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !(char_item == '/')
                    && !(char_item == '_')
                    && !(char_item == ':')
                    && !(char_item == '(')
                    && !(char_item == ')')
                    && !(char_item.is_alpha() || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn local_route_segment(input: &str) -> Res<&str, RouteSegment> {
        alt((recognize(tag(".::")),recognize(not(other_route_segment))))(input)
            .map(|(next, _)| (next, RouteSegment::Local))
    }

    pub fn domain_route_segment(input: &str) -> Res<&str, RouteSegment> {
        terminated(domain_chars, tag("::"))(input)
            .map(|(next, domain)| (next, RouteSegment::Domain(domain.to_string())))
    }

    pub fn tag_route_segment(input: &str) -> Res<&str, RouteSegment> {
        terminated(delimited(tag("["), skewer_chars, tag("]")), tag("::"))(input)
            .map(|(next, tag)| (next, RouteSegment::Tag(tag.to_string())))
    }

    pub fn mesh_route_segment(input: &str) -> Res<&str, RouteSegment> {
        terminated(delimited(tag("<<"), mesh_route_chars, tag(">>")), tag("::"))(input)
            .map(|(next, tag)| (next, RouteSegment::Tag(tag.to_string())))
    }

    pub fn other_route_segment(input: &str) -> Res<&str, RouteSegment> {
        alt((
            tag_route_segment,
            domain_route_segment,
            mesh_route_segment,
        ))(input)
    }


    pub fn route_segment(input: &str) -> Res<&str, RouteSegment> {
        alt((
            other_route_segment,
            local_route_segment,
        ))(input)
    }

    pub fn space_address_segment(input: &str) -> Res<&str, AddressSegment> {
        space_chars(input).map(|(next, space)| (next, AddressSegment::Space(space.to_string())))
    }

    pub fn base_address_segment(input: &str) -> Res<&str, AddressSegment> {
        preceded(tag(":"), rec_skewer)(input)
            .map(|(next, base)| (next, AddressSegment::Base(base.to_string())))
    }

    pub fn filesystem_address_segment(input: &str) -> Res<&str, AddressSegment> {
        alt((dir_address_segment, file_address_segment))(input)
    }

    pub fn dir_address_segment(input: &str) -> Res<&str, AddressSegment> {
        context("dir_address_segment", terminated(file_chars, tag("/")))(input)
            .map(|(next, dir)| (next, AddressSegment::Dir(format!("{}/", dir))))
    }

    pub fn root_dir_address_segment(input: &str) -> Res<&str, AddressSegment> {
        tag(":/")(input).map(|(next, _)| (next, AddressSegment::FilesystemRootDir))
    }

    pub fn file_address_segment(input: &str) -> Res<&str, AddressSegment> {
        context("file_address_segment", file_chars)(input)
            .map(|(next, filename)| (next, AddressSegment::File(filename.to_string())))
    }

    pub fn version_address_segment(input: &str) -> Res<&str, AddressSegment> {
        preceded(tag(":"), version)(input)
            .map(|(next, version)| (next, AddressSegment::Version(version)))
    }

    pub fn root_address(input: &str) -> Res<&str, Address> {
        tuple((route_segment, tag("ROOT")))(input).map(|(next, (route, _))| {
            let address = Address {
                route,
                segments: vec![],
            };
            (next, address)
        })
    }
    pub fn regular_address(input: &str) -> Res<&str, Address> {
        tuple((
            tuple((route_segment, space_address_segment)),
            many0(base_address_segment),
            opt(version_address_segment),
            opt(root_dir_address_segment),
            many0(filesystem_address_segment),
        ))(input)
        .map(
            |(next, ((hub, space), mut bases, version, root, mut files))| {
                let mut segments = vec![];
                segments.push(space);
                segments.append(&mut bases);
                match version {
                    None => {}
                    Some(version) => {
                        segments.push(version);
                    }
                }

                if let Option::Some(root) = root {
                    segments.push(root);
                    segments.append(&mut files);
                }

                let address = Address {
                    route: hub,
                    segments,
                };

                (next, address)
            },
        )
    }

    pub fn address(input: &str) -> Res<&str, Address> {
        alt((root_address, regular_address))(input)
    }

    pub fn consume_address(input: &str) -> Res<&str, Address> {
        all_consuming(address)(input)
    }

    pub fn capture_address(input: &str) -> Res<&str, CaptureAddress> {
        context( "Address", tuple((
            tuple((route_segment, alt((root_address_capture_segment,space_address_capture_segment)))),
            many0(base_address_capture_segment),
            opt(version_address_segment),
            opt(root_dir_address_segment),
            many0(filesystem_address_capture_segment),
        )))(input)
        .map(
            |(next, ((hub, space), mut bases, version, root, mut files))| {
                let mut segments = vec![];
                segments.push(space);
                segments.append(&mut bases);
                match version {
                    None => {}
                    Some(version) => {
                        segments.push(version);
                    }
                }

                if let Option::Some(root) = root {
                    segments.push(root);
                    segments.append(&mut files);
                }

                let address = CaptureAddress {
                    route: hub,
                    segments,
                };

                (next, address)
            },
        )
    }

    pub fn root_address_capture_segment(input: &str) -> Res<&str, AddressSegment> {
        tag("ROOT")(input)
            .map(|(next, space)| (next, AddressSegment::Root))
    }


    pub fn space_address_capture_segment(input: &str) -> Res<&str, AddressSegment> {
        space_chars_plus_capture(input)
            .map(|(next, space)| (next, AddressSegment::Space(space.to_string())))
    }

    pub fn base_address_capture_segment(input: &str) -> Res<&str, AddressSegment> {
        preceded(tag(":"), rec_skewer_capture)(input)
            .map(|(next, base)| (next, AddressSegment::Base(base.to_string())))
    }

    pub fn filesystem_address_capture_segment(input: &str) -> Res<&str, AddressSegment> {
        alt((dir_address_capture_segment, file_address_capture_segment))(input)
    }

    pub fn dir_address_capture_segment(input: &str) -> Res<&str, AddressSegment> {
        context(
            "dir_address_capture_segment",
            terminated(file_chars_plus_capture, tag("/")),
        )(input)
        .map(|(next, dir)| (next, AddressSegment::Dir(format!("{}/", dir))))
    }

    pub fn file_address_capture_segment(input: &str) -> Res<&str, AddressSegment> {
        context("file_address_capture_segment", file_chars_plus_capture)(input)
            .map(|(next, filename)| (next, AddressSegment::File(filename.to_string())))
    }

    pub fn space_address_kind_segment(input: &str) -> Res<&str, AddressKindSegment> {
        tuple((space_address_segment, delim_kind))(input).map(|(next, (address_segment, kind))| {
            (
                next,
                AddressKindSegment {
                    address_segment,
                    kind,
                },
            )
        })
    }

    pub fn base_address_kind_segment(input: &str) -> Res<&str, AddressKindSegment> {
        tuple((base_address_segment, delim_kind))(input).map(|(next, (address_segment, kind))| {
            (
                next,
                AddressKindSegment {
                    address_segment,
                    kind,
                },
            )
        })
    }

    pub fn filepath_address_kind_segment(input: &str) -> Res<&str, AddressKindSegment> {
        alt((file_address_kind_segment, dir_address_kind_segment))(input)
    }
    pub fn dir_address_kind_segment(input: &str) -> Res<&str, AddressKindSegment> {
        tuple((dir_address_segment, delim_kind))(input).map(|(next, (address_segment, kind))| {
            (
                next,
                AddressKindSegment {
                    address_segment,
                    kind,
                },
            )
        })
    }

    pub fn file_address_kind_segment(input: &str) -> Res<&str, AddressKindSegment> {
        tuple((file_address_segment, delim_kind))(input).map(|(next, (address_segment, kind))| {
            (
                next,
                AddressKindSegment {
                    address_segment,
                    kind,
                },
            )
        })
    }

    pub fn version_address_kind_segment(input: &str) -> Res<&str, AddressKindSegment> {
        tuple((version_address_segment, delim_kind))(input).map(
            |(next, (address_segment, kind))| {
                (
                    next,
                    AddressKindSegment {
                        address_segment,
                        kind,
                    },
                )
            },
        )
    }

    pub fn consume_address_kind_path(input: &str) -> Result<AddressKindPath, MsgErr> {
        let (_, rtn) = all_consuming(address_kind_path)(input)?;
        Ok(rtn)
    }

    pub fn address_kind_path(input: &str) -> Res<&str, AddressKindPath> {
        tuple((
            tuple((route_segment, space_address_kind_segment)),
            many0(base_address_kind_segment),
            opt(version_address_kind_segment),
            many0(file_address_kind_segment),
        ))(input)
        .map(|(next, ((hub, space), mut bases, version, mut files))| {
            let mut segments = vec![];
            segments.push(space);
            segments.append(&mut bases);
            match version {
                None => {}
                Some(version) => {
                    segments.push(version);
                }
            }
            segments.append(&mut files);

            let address = AddressKindPath::new(hub, segments);

            (next, address)
        })
    }

    pub fn asterisk<T, E: nom::error::ParseError<T>>(input: T) -> IResult<T, T, E>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        input.split_at_position_complete(|item| item.as_char() != '*')
    }

    pub fn upper<T, E: nom::error::ParseError<T>>(input: T) -> IResult<T, T, E>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        input.split_at_position_complete(|item| {
            let char_item = item.as_char();

            !char_item.is_uppercase()
        })
    }

    /*    fn any_resource_path_segment<T>(i: T) -> Res<T, T>
           where
               T: InputTakeAtPosition+nom::InputLength,
               <T as InputTakeAtPosition>::Item: AsChar,
       {
           i.split_at_position1_complete(
               |item| {
                   let char_item = item.as_char();
                   !(char_item == '-')
                       && !(char_item == '.')
                       && !(char_item == '/')
                       && !(char_item == '_')
                       && !(char_item.is_alpha() || char_item.is_dec_digit())
               },
               ErrorKind::AlphaNumeric,
           )
       }

    */

    pub fn in_double_quotes<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                char_item == '\"'
            },
            ErrorKind::AlphaNumeric,
        )
    }
    pub fn skewer_dot<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !((char_item.is_alpha() && char_item.is_lowercase())
                        || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn domain<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !((char_item.is_alpha() && char_item.is_lowercase())
                        || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn address_segment_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !(char_item.is_alpha() || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn version_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                char_item != '.'
                    && char_item != '-'
                    && !char_item.is_digit(10)
                    && !(char_item.is_alpha() && char_item.is_lowercase())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn version_req_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '>')
                    && !(char_item == '<')
                    && !(char_item == '^')
                    && !(char_item == '=')
                    && !((char_item.is_alpha() && char_item.is_lowercase())
                        || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn lowercase1<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item.is_alpha() && char_item.is_lowercase())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn rec_skewer(input: &str) -> Res<&str, &str> {
        recognize(tuple((lowercase1, opt(skewer_chars))))(input)
    }

    pub fn rec_skewer_capture(input: &str) -> Res<&str, &str> {
        recognize(tuple((lowercase1, opt(skewer_chars_plus_capture))))(input)
    }

    pub fn skewer_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                char_item != '-'
                    && !char_item.is_digit(10)
                    && !(char_item.is_alpha() && char_item.is_lowercase())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn skewer_chars_plus_capture<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                char_item != '-'
                    && char_item != '$'
                    && !char_item.is_digit(10)
                    && !(char_item.is_alpha() && char_item.is_lowercase())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn skewer_chars_template<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                char_item != '-'
                    && char_item.as_char() != '%'
                    && !char_item.is_digit(10)
                    && !(char_item.is_alpha() && char_item.is_lowercase())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn space_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !((char_item.is_alpha() && char_item.is_lowercase())
                        || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn space_chars_plus_capture<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !(char_item == '$')
                    && !((char_item.is_alpha() && char_item.is_lowercase())
                        || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn domain_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !((char_item.is_alpha() && char_item.is_lowercase())
                        || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn filepath_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !(char_item == '/')
                    && !(char_item == ':')
                    && !(char_item == '_')
                    && !(char_item.is_alpha() || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn file_chars_plus_capture<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !(char_item == '_')
                    && !(char_item == '$')
                    && !(char_item.is_alpha() || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn file_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !(char_item == '_')
                    && !(char_item.is_alpha() || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn file_chars_template<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition + nom::InputLength,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !(char_item == '_')
                    && !(char_item == '%')
                    && !(char_item.is_alpha() || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn not_space(input: &str) -> Res<&str, &str> {
        is_not(" \n\r\t")(input)
    }

    pub fn path(input: &str) -> Res<&str, &str> {
        recognize(tuple((tag("/"), opt(filepath_chars))))(input)
    }

    pub fn capture_path(input: &str) -> Res<&str, &str> {
        recognize(tuple((tag("/"), opt(file_chars_plus_capture))))(input)
    }

    pub fn consume_path(input: &str) -> Res<&str, &str> {
        all_consuming(path)(input)
    }

    pub fn path_regex(input: &str) -> Res<&str, &str> {
        recognize(opt(not_space))(input)
    }
    pub fn camel_case(input: &str) -> Res<&str, &str> {
        recognize(tuple((is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alpha0)))(input)
        //recognize(alpha1)(input)
    }


    pub fn camel_case_to_string_matcher(input: &str) -> Res<&str, StringMatcher> {
        camel_case(input).map(|(next, camel)| (next, StringMatcher::new(camel.to_string())))
    }

    fn parse_version_major_minor_patch(input: &str) -> Res<&str, (&str, &str, &str)> {
        context(
            "version_major_minor_patch",
            tuple((
                terminated(digit1, tag(".")),
                terminated(digit1, tag(".")),
                terminated(digit1, not(digit1)),
            )),
        )(input)
    }

    pub fn parse_version(input: &str) -> Res<&str, ((&str, &str, &str), Option<&str>)> {
        tuple((
            parse_version_major_minor_patch,
            opt(preceded(tag("-"), skewer_chars)),
        ))(input)
    }

    pub fn rec_version(input: &str) -> Res<&str, &str> {
        recognize(parse_version)(input)
    }

    pub fn base_address_segment_wildcard(input: &str) -> Res<&str, AddressTemplateSegment> {
        preceded(
            tag(":"),
            recognize(tuple((many0(skewer), tag("%"), many0(skewer)))),
        )(input)
        .map(|(next, base)| (next, AddressTemplateSegment::Wildcard(base.to_string())))
    }

    pub fn base_address_segment_template(input: &str) -> Res<&str, AddressTemplateSegment> {
        preceded(tag(":"), rec_skewer)(input).map(|(next, base)| {
            (
                next,
                AddressTemplateSegment::AddressSegment(AddressSegment::Base(base.to_string())),
            )
        })
    }

    pub fn filepath_address_segment_wildcard(input: &str) -> Res<&str, AddressTemplateSegment> {
        recognize(tuple((
            many0(filepath_chars),
            tag("%"),
            many0(filepath_chars),
        )))(input)
        .map(|(next, base)| (next, AddressTemplateSegment::Wildcard(base.to_string())))
    }

    pub fn filepath_address_segment_template(input: &str) -> Res<&str, AddressTemplateSegment> {
        filesystem_address_segment(input)
            .map(|(next, segment)| (next, AddressTemplateSegment::AddressSegment(segment)))
    }

    pub fn address_template(input: &str) -> Res<&str, AddressTemplate> {
        let (next, ((hub, space), mut bases, version, root, mut files)) = tuple((
            tuple((route_segment, space_address_segment)),
            many0(alt((
                base_address_segment_wildcard,
                base_address_segment_template,
            ))),
            opt(version_address_segment),
            opt(root_dir_address_segment),
            many0(alt((
                filepath_address_segment_wildcard,
                filepath_address_segment_template,
            ))),
        ))(input)?;

        let mut base_wildcard = false;
        for (index, segment) in bases.iter().enumerate() {
            if segment.is_wildcard() {
                if index != bases.len() - 1 {
                    return Err(nom::Err::Error(ErrorTree::from_error_kind(
                        input,
                        ErrorKind::Tag,
                    )));
                } else {
                    base_wildcard = true;
                }
            }
        }

        if base_wildcard && version.is_some() {
            return Err(nom::Err::Error(ErrorTree::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }

        if base_wildcard && root.is_some() {
            return Err(nom::Err::Error(ErrorTree::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }

        let mut files_wildcard = false;
        for (index, segment) in files.iter().enumerate() {
            if segment.is_wildcard() {
                if index != files.len() - 1 {
                    return Err(nom::Err::Error(ErrorTree::from_error_kind(
                        input,
                        ErrorKind::Tag,
                    )));
                } else {
                    files_wildcard = true;
                }
            }
        }

        let mut space_last = false;
        let last = if !files.is_empty() {
            match files.remove(files.len() - 1) {
                AddressTemplateSegment::AddressSegment(exact) => {
                    AddressSegmentTemplate::Exact(exact.to_string())
                }
                AddressTemplateSegment::Wildcard(pattern) => {
                    AddressSegmentTemplate::Pattern(pattern)
                }
            }
        } else if root.is_some() {
            AddressSegmentTemplate::Exact("/".to_string())
        } else if let Option::Some(version) = &version {
            AddressSegmentTemplate::Exact(version.to_string())
        } else if !bases.is_empty() {
            match bases.remove(bases.len() - 1) {
                AddressTemplateSegment::AddressSegment(exact) => {
                    AddressSegmentTemplate::Exact(exact.to_string())
                }
                AddressTemplateSegment::Wildcard(pattern) => {
                    AddressSegmentTemplate::Pattern(pattern)
                }
            }
        } else {
            space_last = true;
            AddressSegmentTemplate::Exact(space.to_string())
        };

        let mut bases: Vec<AddressSegment> = bases
            .into_iter()
            .map(|b| match b {
                AddressTemplateSegment::AddressSegment(seg) => seg,
                AddressTemplateSegment::Wildcard(_) => {
                    panic!("should have filtered wildcards already!")
                }
            })
            .collect();

        let mut files: Vec<AddressSegment> = files
            .into_iter()
            .map(|b| match b {
                AddressTemplateSegment::AddressSegment(seg) => seg,
                AddressTemplateSegment::Wildcard(_) => {
                    panic!("should have filtered wildcards already!")
                }
            })
            .collect();

        let mut segments = vec![];

        if !space_last {
            segments.push(space);
        }

        segments.append(&mut bases);

        match version {
            None => {}
            Some(version) => {
                segments.push(version);
            }
        }

        if let Option::Some(root) = root {
            segments.push(root);
            segments.append(&mut files);
        }

        let address = Address {
            route: hub,
            segments,
        };

        let address_template = AddressTemplate {
            parent: address,
            child_segment_template: last,
        };

        Ok((next, address_template))
    }

    pub fn kind_template(input: &str) -> Res<&str, KindTemplate> {
        tuple((
            resource_type,
            opt(delimited(
                tag("<"),
                tuple((
                    camel_case,
                    opt(delimited(tag("<"), specific_pattern, tag(">"))),
                )),
                tag(">"),
            )),
        ))(input)
        .map(|(next, (resource_type, more))| {
            let mut parts = KindTemplate {
                resource_type,
                kind: None,
                specific: None,
            };

            match more {
                Some((kind, specific)) => {
                    parts.kind = Option::Some(kind.to_string());
                    parts.specific = specific;
                }
                None => {}
            }

            (next, parts)
        })
    }

    pub fn template(input: &str) -> Res<&str, Template> {
        tuple((
            address_template,
            delimited(tag("<"), kind_template, tag(">")),
        ))(input)
        .map(|(next, (address, kind))| (next, Template { address, kind }))
    }

    pub fn set_property_mod(input: &str) -> Res<&str, PropertyMod> {
        tuple((tag("+"), skewer_dot, tag("="), property_value))(input).map(
            |(next, (_, key, _, value))| {
                (
                    next,
                    PropertyMod::Set {
                        key: key.to_string(),
                        value: value.to_string(),
                        lock: false,
                    },
                )
            },
        )
    }

    pub fn set_property_mod_lock(input: &str) -> Res<&str, PropertyMod> {
        tuple((tag("+@"), skewer_dot, tag("="), property_value))(input).map(
            |(next, (_, key, _, value))| {
                (
                    next,
                    PropertyMod::Set {
                        key: key.to_string(),
                        value: value.to_string(),
                        lock: true,
                    },
                )
            },
        )
    }

    pub fn property_value_not_space_or_comma(input: &str) -> Res<&str, &str> {
        is_not(" \n\r\t,")(input)
    }

    pub fn property_value_single_quotes(input: &str) -> Res<&str, &str> {
        delimited(tag("'"), is_not("'"), tag("'"))(input)
    }

    pub fn property_value_double_quotes(input: &str) -> Res<&str, &str> {
        delimited(tag("\""), is_not("\""), tag("\""))(input)
    }

    pub fn property_value(input: &str) -> Res<&str, &str> {
        alt((
            property_value_single_quotes,
            property_value_double_quotes,
            property_value_not_space_or_comma,
        ))(input)
    }

    pub fn unset_property_mod(input: &str) -> Res<&str, PropertyMod> {
        tuple((tag("!"), skewer_dot))(input)
            .map(|(next, (_, name))| (next, PropertyMod::UnSet(name.to_string())))
    }

    pub fn property_mod(input: &str) -> Res<&str, PropertyMod> {
        alt((set_property_mod, unset_property_mod))(input)
    }

    pub fn set_properties(input: &str) -> Res<&str, SetProperties> {
        separated_list0(tag(","), tuple((multispace0, property_mod, multispace0)))(input).map(
            |(next, properties)| {
                let mut set_properties = SetProperties::new();
                for (_, property, _) in properties {
                    set_properties.push(property);
                }
                (next, set_properties)
            },
        )
    }

    pub fn get_properties(input: &str) -> Res<&str, Vec<String>> {
        separated_list0(tag(","), tuple((multispace0, skewer, multispace0)))(input).map(
            |(next, keys)| {
                let keys: Vec<String> = keys.iter().map(|(_, key, _)| key.to_string()).collect();
                (next, keys)
            },
        )
    }

    pub fn create(input: &str) -> Res<&str, Create> {
        tuple((template, opt(delimited(tag("{"), set_properties, tag("}")))))(input).map(
            |(next, (template, properties))| {
                let properties = match properties {
                    Some(properties) => properties,
                    None => SetProperties::new(),
                };
                let create = Create {
                    template,
                    state: StateSrc::Stateless,
                    properties,
                    strategy: Strategy::Create,
                    registry: Default::default(),
                };
                (next, create)
            },
        )
    }

    pub fn set(input: &str) -> Res<&str, Set> {
        tuple((address, delimited(tag("{"), set_properties, tag("}"))))(input).map(
            |(next, (address, properties))| {
                let set = Set {
                    address,
                    properties,
                };
                (next, set)
            },
        )
    }

    pub fn get(input: &str) -> Res<&str, Get> {
        tuple((address, opt(delimited(tag("{"), get_properties, tag("}")))))(input).map(
            |(next, (address, keys))| {
                let op = match keys {
                    None => GetOp::State,
                    Some(keys) => GetOp::Properties(keys),
                };
                let get = Get { address, op };

                (next, get)
            },
        )
    }

    pub fn select(input: &str) -> Res<&str, Select> {
        address_kind_pattern(input).map(|(next, address_kind_pattern)| {
            let select = Select {
                pattern: address_kind_pattern,
                properties: Default::default(),
                into_payload: SelectIntoPayload::Stubs,
                kind: SelectKind::Initial,
            };
            (next, select)
        })
    }

    pub fn publish(input: &str) -> Res<&str, CreateOp> {
        let (next, (upload, _, address)) = tuple((upload_step, space1, address))(input)?;

        let parent = match address.parent() {
            None => {
                return Err(nom::Err::Error(ErrorTree::from_error_kind(
                    input,
                    ErrorKind::Tag,
                )));
            }
            Some(parent) => parent,
        };

        let last = match address.last_segment() {
            None => {
                return Err(nom::Err::Error(ErrorTree::from_error_kind(
                    input,
                    ErrorKind::Tag,
                )));
            }
            Some(last) => last,
        };

        let template = Template {
            address: AddressTemplate {
                parent,
                child_segment_template: AddressSegmentTemplate::Exact(last.to_string()),
            },
            kind: KindTemplate {
                resource_type: "ArtifactBundle".to_string(),
                kind: None,
                specific: None,
            },
        };

        let create = CreateOp {
            template,
            state: StateSrc::Stateless,
            properties: Default::default(),
            strategy: Strategy::Create,
            registry: Default::default(),
            requirements: vec![Require::File(upload.name)],
        };

        Ok((next, create))
    }

    pub fn permissions_mask(input: Span) -> Res<Span, PermissionsMask> {
        tuple((alt((value(PermissionsMaskKind::Or,char('+')),value(PermissionsMaskKind::And,char('&')))),permissions))(input).map( |(next,(kind,permissions))| {
            let mask = PermissionsMask {
                kind,
                permissions
            };

            (next,mask)
        })
    }


    pub fn permissions(input: Span) -> Res<Span, Permissions> {
        tuple((child_perms, tag("-"), particle_perms))(input).map( |(next,(child,_,particle))| {
            let permissions = Permissions {
                child,
                particle
            };
            (next,permissions)
        })
    }

    pub fn child_perms(input: Span) -> Res<Span, ChildPerms> {
        tuple( (alt( (value(false,char('c')),value(true,char('C')))),
                alt( (value(false,char('s')),value(true,char('S')))),
                alt( (value(false,char('d')),value(true,char('D'))))))(input).map( |(next,(create, select, delete))|{
            let block = ChildPerms{
                create,
                select,
                delete,
            };
            (next,block)
        })
    }

    pub fn particle_perms(input: Span) -> Res<Span, ParticlePerms> {
        tuple( (alt( (value(false,char('r')),value(true,char('R')))),
                   alt( (value(false,char('w')),value(true,char('W')))),
                   alt( (value(false,char('x')),value(true,char('X'))))))(input).map( |(next,(read,write,execute))|{
          let block = ParticlePerms {
              read,
              write,
              execute,
          };
            (next,block)
        })
    }

}

pub mod log {
    use serde::{Serialize,Deserialize};
    use crate::version::v0_0_1::id::Address;

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub enum Level {
        Trace,
        Debug,
        Info,
        Warn,
        Error
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct Log {
        pub point: Address,
        pub message: String,
        pub level: Level
    }

    impl ToString for Log {
        fn to_string(&self) -> String {
            format!("{} {}", self.point.to_string(), self.message )
        }
    }

    impl Log {
        pub fn trace( point: Address, message: &str ) -> Self {
            Self {
                level: Level::Trace,
                point,
                message: message.to_string()
            }
        }
        pub fn debug( point: Address, message: &str ) -> Self {
            Self {
                level: Level::Debug,
                point,
                message: message.to_string()
            }
        }
        pub fn info( point: Address, message: &str ) -> Self {
            Self {
                level: Level::Info,
                point,
                message: message.to_string()
            }
        }
        pub fn warn( point: Address, message: &str ) -> Self {
            Self {
                level: Level::Warn,
                point,
                message: message.to_string()
            }
        }
        pub fn error( point: Address, message: &str ) -> Self {
            Self {
                level: Level::Error,
                point,
                message: message.to_string()
            }
        }
    }

    pub trait ParticleLogger {
        fn log(&self, log: Log );
    }
}

#[cfg(test)]
pub mod test {
    use std::str::FromStr;
    use http::Uri;

    use nom::combinator::{all_consuming, recognize};

    use crate::error::MsgErr;
    use crate::version::v0_0_1::config::bind::parse::{bind, final_bind, select_http_pipelines, http_pipeline, pipeline, pipeline_step, pipeline_stop};
    use crate::version::v0_0_1::config::bind::{PipelinesSubScope, ProtoBind};
    use crate::version::v0_0_1::entity::request::{Action, RequestCore};
    use crate::version::v0_0_1::id::{Address, AddressSegment, RouteSegment};
    use crate::version::v0_0_1::parse::{address, address_template, base_address_segment, camel_case, capture_address, create, file_address_capture_segment, publish, rec_skewer, route_segment, skewer_chars, version_address_segment, Res, particle_perms, permissions, permissions_mask};
    use crate::version::v0_0_1::pattern::parse::address_kind_pattern;
    use crate::version::v0_0_1::pattern::parse::version;
    use crate::version::v0_0_1::pattern::{AddressKindPath, AddressKindPattern, http_method, http_method_pattern, http_pattern, http_pattern_scoped, upload_step};
    use crate::version::v0_0_1::payload::{HttpMethod, Payload};
    use crate::version::v0_0_1::util::ValueMatcher;
    use nom::error::VerboseError;
    use nom::{Err, Offset};
    use nom_supreme::error::ErrorTree;
    use nom_supreme::final_parser::ExtractContext;
    use regex::Regex;
    use crate::version::v0_0_1::security::{ChildPerms, ParticlePerms, Permissions, PermissionsMask, PermissionsMaskKind};
    use crate::version::v0_0_1::Span;

    #[test]
    pub fn test_address_kind_pattern_matching() -> Result<(), MsgErr> {
        let pattern = AddressKindPattern::from_str("**")?;
        let address = AddressKindPath::from_str("localhost<Space>:mechtron<Mechtron>")?;
        assert!(pattern.matches(&address));
        Ok(())
    }

    #[test]
    pub fn test_query_root() -> Result<(), MsgErr> {
        let inclusive_pattern = AddressKindPattern::from_str("localhost+:**")?;
        let exclusive_pattern = AddressKindPattern::from_str("localhost:**")?;
        println!("inclusive: '{}'", inclusive_pattern.query_root().to_string());
        println!("exclusive : '{}'", exclusive_pattern.query_root().to_string());
        Ok(())
    }


    #[test]
    pub fn test_inclusive() -> Result<(), MsgErr> {
        // if a hop is 'inclusive' then this will match to true.  We do this for cases like:
        // localhost+:**   // Here we want everything under localhost INCLUDING localhost to be matched
        let inclusive_pattern = AddressKindPattern::from_str("localhost+:**")?;
        let exclusive_pattern = AddressKindPattern::from_str("localhost:**")?;
        let address1 = AddressKindPath::from_str("localhost<Space>")?;
        let address2 = AddressKindPath::from_str("localhost<Space>:mechtron<Mechtron>")?;

        assert!(!exclusive_pattern.matches(&address1));
        assert!(exclusive_pattern.matches(&address2));
        assert!(inclusive_pattern.matches(&address1));
        assert!(inclusive_pattern.matches(&address2));
        Ok(())
    }


    #[test]
    pub fn test_inclusive2() -> Result<(), MsgErr> {
        let inclusive_pattern = AddressKindPattern::from_str("localhost+:app+:**")?;
        let exclusive_pattern = AddressKindPattern::from_str("localhost:app:**")?;
        let address1 = AddressKindPath::from_str("localhost<Space>")?;
        let address2 = AddressKindPath::from_str("localhost<Space>:app<App>")?;
        let address3 = AddressKindPath::from_str("localhost<Space>:app<App>:mechtron<Mechtron>")?;

        assert!(!exclusive_pattern.matches(&address1));
        assert!(!exclusive_pattern.matches(&address2));
        assert!(exclusive_pattern.matches(&address3));
        assert!(inclusive_pattern.matches(&address1));
        assert!(inclusive_pattern.matches(&address2));
        assert!(inclusive_pattern.matches(&address3));
        Ok(())
    }



    #[test]
    pub fn test_some_matches() -> Result<(), MsgErr> {
        let users = AddressKindPattern::from_str("**<User>")?;
        let address1 = AddressKindPath::from_str("localhost<Space>")?;
        let address2 = AddressKindPath::from_str("localhost<Space>:app<App>")?;
        let address3 = AddressKindPath::from_str("localhost<Space>:app<App>:mechtron<Mechtron>")?;
        let address4 = AddressKindPath::from_str("localhost<Space>:app<App>:users<UserBase>")?;
        let address5 = AddressKindPath::from_str("localhost<Space>:app<App>:users<UserBase>:scott<User>")?;
        let address6 = AddressKindPath::from_str("localhost<Space>:users<UserBase>")?;
        let address7 = AddressKindPath::from_str("localhost<Space>:users<UserBase>:superuser<User>")?;

        assert!(!users.matches(&address1));
        assert!(!users.matches(&address2));
        assert!(!users.matches(&address3));
        assert!(!users.matches(&address4));
        assert!(users.matches(&address5));
        assert!(!users.matches(&address6));
        assert!(users.matches(&address7));
        Ok(())
    }


    #[test]
    pub fn test_a_match() -> Result<(), MsgErr> {
        let pattern = AddressKindPattern::from_str("localhost:app+:**")?;
        let address = AddressKindPath::from_str("localhost<Space>:app<App>")?;

        assert!(pattern.matches(&address));
        Ok(())
    }

    #[test]
    pub fn test_skewer_chars() -> Result<(), MsgErr> {
        match all_consuming(rec_skewer)("317") {
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
    pub fn test_address() -> Result<(), MsgErr> {
        assert_eq!(
            ("", RouteSegment::Local),
            all_consuming(route_segment)("")?
        );

        all_consuming(address)("[root]")?;
        all_consuming(address)("hello:kitty")?;
        all_consuming(address)("hello.com:kitty")?;
        all_consuming(address)("hello:kitty:/file.txt")?;
        all_consuming(address)("hello.com:kitty:/file.txt")?;
        all_consuming(address)("hello.com:kitty:/")?;
        //all_consuming(address)("hello.com:kitty:/greater-glory/file.txt")?;
        all_consuming(address)("hello.com:kitty:base")?;

        all_consuming(version)("1.0.0")?;
        let (next, version) = all_consuming(version_address_segment)(":1.2.3")?;
        println!("next: '{}' segment: '{}'", next, version.to_string());
        all_consuming(address)("hello.com:bundle:1.2.3")?;
        let (next, addy) = all_consuming(address)("hello.com:bundle:1.2.3:/")?;
        println!("{}", addy.last_segment().unwrap().to_string());
        let (next, addy) = all_consuming(address)("hello.com:bundle:1.2.3")?;
        //       let (next, addy) = all_consuming(address)("hello.com:bundle:1.2.3:/some/file.txt")?;
        let (next, addy) =
            all_consuming(address)("hello.com:bundle:1.2.3:/greater-glory/file.txt")?;
        println!("{}", addy.to_string());
        println!("{}", addy.parent().unwrap().to_string());
        println!("{}", addy.last_segment().unwrap().to_string());

        Ok(())
    }

    #[test]
    pub fn test_address_template() -> Result<(), MsgErr> {
        all_consuming(address_template)("hello:kitty")?;
        all_consuming(address_template)("hello:kitty-%")?;
        all_consuming(address_template)("hello:kitty:bob:/some-%-time")?;
        Ok(())
    }

    #[test]
    pub fn test_create() -> Result<(), MsgErr> {
        all_consuming(create)("hello:kitty<App>")?;
        all_consuming(create)("hello:kitty<App>{ +config='some:config:1.0.0:/blah.conf' }")?;
        Ok(())
    }

    #[test]
    pub fn test_publish() -> Result<(), MsgErr> {
        let (_, block) = all_consuming(upload_step)("^[ bundle.zip ]->")?;
        assert_eq!("bundle.zip", block.name.as_str());
        all_consuming(publish)("^[ bundle.zip ]-> space.org:hello:1.0.0")?;
        Ok(())
    }

    #[test]
    pub fn test_pipeline_stop() -> Result<(), MsgErr> {
        pipeline_stop("apps:my-app^Http<Get>/*")?;
        Ok(())
    }

    #[test]
    pub fn test_pipeline() -> Result<(), MsgErr> {
        pipeline("-> apps:my-app^Http<Get>/users/$1 => &")?;
        pipeline("-> apps:bundle:1.0.0:/html/$1 => &")?;
        Ok(())
    }

    #[test]
    pub fn test_capture_address() -> Result<(), MsgErr> {
        file_address_capture_segment("$1")?;

        let address = all_consuming(capture_address)("apps:bundle:1.0.0:/html/$1")?.1;
        let regex = Regex::new("/(.*)")?;
        let address = address.to_address(regex.captures("/index.html").unwrap())?;
        Ok(())
    }

    #[test]
    pub fn test_address_kind_pattern() -> Result<(), MsgErr> {
        all_consuming(address_kind_pattern)("*")?;
        all_consuming(address_kind_pattern)("space")?;
        all_consuming(address_kind_pattern)("space:base")?;
        all_consuming(address_kind_pattern)("space:my-files:/")?;
        all_consuming(address_kind_pattern)("space:my-files:/file.txt")?;
        all_consuming(address_kind_pattern)("space:my-files:/dir/file.txt")?;
        all_consuming(address_kind_pattern)("space<Space>:base")?;
        all_consuming(address_kind_pattern)("**:*<Base>")?;
        all_consuming(address_kind_pattern)("space<Space>:base<Base>")?;
        all_consuming(address_kind_pattern)("space:base:blah")?;
        all_consuming(address_kind_pattern)("space:base:*")?;
        all_consuming(address_kind_pattern)("space<Space>:**<Base>")?;
        all_consuming(address_kind_pattern)("space:series:1.0.0:/some/file.txt")?;
        all_consuming(address_kind_pattern)("space+")?;
        all_consuming(address_kind_pattern)("space+:**")?;
        Ok(())
    }
    #[cfg(test)]
    pub mod bind{
        use crate::error::MsgErr;
        use crate::version::v0_0_1::config::bind::parse::{bind, final_bind};
        use crate::version::v0_0_1::config::bind::ProtoBind;

        #[test]
        pub fn test_expect_pipelines_scope_selector() -> Result<(), MsgErr> {
            match final_bind(
                r#"Bind{Msg{}}"#,
            ) {
                Ok(_) => {
                    panic!("should not be ok")
                }
                Err(err) => {
assert_eq!(r#"Problem: "Msg{}}"

Bind.Pipelines: expecting 'Pipelines' (Pipelines selector)"#, err );
                    Ok(())
                }
            }
        }

        #[test]
        pub fn test_expect_pipelines_scope_selector_open() -> Result<(), MsgErr> {
            match final_bind(
                r#"Bind{Pipelines}"#,
            ) {
                Ok(_) => {
                    panic!("should not be ok")
                }
                Err(err) => {
                    assert_eq!(r#"Problem: "}"

Bind.Pipelines: expecting '{' (Pipelines open scope)"#, err);
                    Ok(())
                }
            }
        }

        #[test]
        pub fn test_expect_pipelines_scope_selector_empty() -> Result<(), MsgErr> {
            match final_bind(r#"Bind{Pipelines{ }}"#) {
                Ok(_) => {
                    Ok(())
                }
                Err(err) => {
                    println!("{}", err);
                    panic!("{}", err)
                }
            }
        }


        #[test]
        pub fn test_expect_pipelines_scope_selector_enumeration() -> Result<(), MsgErr> {
            match final_bind(
                r#"Bind{Pipelines{Blah{ } }}"#,
            ) {
                Ok(_) => {
                    panic!("should not be ok")
                }
                Err(err) => {
                    let expect = r#"Problem: "Blah{ } }}"

Bind.Pipelines: expecting 'Rc', 'Msg' or 'Http' (pipelines selectors) or '}' (close Pipelines scope)"#;
                    assert_eq!(err, expect);
                    Ok(())
                }
            }
        }

        #[test]
        pub fn test_expect_pipelines_scope_selector_msg() -> Result<(), MsgErr> {
            match final_bind(
                r#"Bind{ Pipelines{ Msg{ } }}"#,
            ) {
                Ok(_) => {
                    Ok(())
                }
                Err(err) => {
                    panic!("{}", err);
                }
            }
        }

        #[test]
        pub fn test_expect_http_method_pattern() -> Result<(), MsgErr> {
            match final_bind(
                r#"Bind{
    Pipelines{
        Http {
            <Bad> -> something;
        }
    }
}"#,
            ) {
                Ok(_) => {
                    Ok(())
                }
                Err(err) => {
                    let expected = r#"Problem: "Bad> -> something;"

Bind.Pipelines.Http: expecting '*' or valid HttpMethod: 'Get', 'Post', 'Put', 'Delete', etc... (HttpMethodPattern)"#;
                    assert_eq!(err, expected);
                    Ok(())
                }
            }
        }


        #[test]
        pub fn test_good() -> Result<(), MsgErr> {
            final_bind(
      r#"
            Bind{
                Pipelines{
                    Http {
                        <Get> -> something;
                        <Post>/(.*) -[ Bin ]-> another:what => &;
                    }
                }
            }"#)?;
            Ok(())
        }


        #[test]
        pub fn test_open_angle_bracket() -> Result<(), MsgErr> {
            match final_bind(
                r#"
            Bind{
                Pipelines{
                    Http {
                        Get -> something;
                    }
                }
            }"#) {
                Ok(_) => {
                    panic!("expected failure")
                }
                Err(err) => {
                    assert_eq!(r#"Problem: "Get -> something;"

Bind.Pipelines: expecting: '<' (open angle bracket)"#,err);
                    Ok(())
                }
            }
        }

        #[test]
        pub fn test_close_angle_bracket() -> Result<(), MsgErr> {
            match final_bind(
                r#"
            Bind{
                Pipelines{
                    Http {
                        <Get -> something;
                    }
                }
            }"#) {
                Ok(_) => {
                    panic!("expected failure")
                }
                Err(err) => {
                    println!("{}",err);
                    assert_eq!(r#"Problem: " -> something;"

Bind.Pipelines: expecting: '>' (close angle bracket)"#,err);
                    Ok(())
                }
            }
        }


        #[test]
        pub fn test_missing_pipeline_stop() -> Result<(), MsgErr> {
            match final_bind(
                r#"
            Bind{
                Pipelines{
                    Http {
                        <Get> -> ;
                    }
                }
            }"#) {
                Ok(_) => {
                    panic!("expected failure")
                }
                Err(err) => {
                    println!("{}",err);
                    assert_eq!(r#"Problem: ";"

Bind.Pipelines.Stop: expected valid pipeline stop "{{ }}" (Core) or valid Address"#,err);
                    Ok(())
                }
            }
        }
        #[test]
        pub fn test_invalid_address() -> Result<(), MsgErr> {
            match final_bind(
                r#"
            Bind{
                Pipelines{
                    Http {
                        <Get> -> Bad;
                    }
                }
            }"#) {
                Ok(_) => {
                    panic!("expected failure")
                }
                Err(err) => {
                    println!("{}",err);
                    assert_eq!(r#"Problem: "> something;"

Bind.Pipelines.Http.Pipeline.Step: expected '->' (forward request) or '-[' (RequestPayloadFilter)"#, err);
                    Ok(())
                }
            }
        }

        #[test]
        pub fn test_missing_enter_pipeline_step() -> Result<(), MsgErr> {
            match final_bind(
                r#"
            Bind{
                Pipelines{
                    Http {
                        <Get> > something;
                    }
                }
            }"#) {
                Ok(_) => {
                    panic!("expected failure")
                }
                Err(err) => {
println!("{}",err);
assert_eq!(r#"Problem: "> something;"

Bind.Pipelines.Http.Pipeline.Step: expected '->' (forward request) or '-[' (RequestPayloadFilter)"#, err);
                    Ok(())
                }
            }
        }

        #[test]
        pub fn test_missing_enter_pipeline_step_block() -> Result<(), MsgErr> {
            match final_bind(
                r#"
            Bind{
                Pipelines{
                    Http {
                        <Get> -[ Bad ]-> something;
                    }
                }
            }"#) {
                Ok(_) => {
                    panic!("expected failure")
                }
                Err(err) => {
                    println!("{}",err);
                    Ok(())
                }
            }
        }


        #[test]
        pub fn test_bind() -> Result<(), MsgErr> {
            final_bind(
                r#"

        Bind {
           Pipelines {
               Msg {
                   <Tick> -> {{}};

                   <Ping> -> {{  }} => &;

                   <Signup> -[ Map{username<Text>,password<Text>} ]-> strip:passsword:mechtron^Msg<Strip>/blah -[ Map{username<Text>} ]-> {{*}} =[ Text ]=> &;

                   <DoWhateverYouWant> -[ * ]-> {{ * }} =[ * ]=> &;
               }

               Http {
                  <Get>/(.*) -> space:1.0.0:/html/$1 => &;

                  <Get>/some -> {{ }} => &;

                  <Post>/some -> {{ }} => &;

                  <Get> -> mechtron.io:site-starter:1.0.0:/html/error.html => &;
                }
           }
        }   "#,
            )?;

            Ok(())
        }
    }

    #[test]
    pub fn test_http_pattern() -> Result<(), MsgErr> {
        let any_pattern = all_consuming(http_pattern)("Http<*>")?.1;
        let get_any_pattern = all_consuming(http_pattern)("Http<Get>")?.1;
        let get_some_pattern = all_consuming(http_pattern)("Http<Get>^/some$")?.1;
        let get_some_star_pattern = all_consuming(http_pattern)("Http<Get>^/some/*")?.1;
        let get_some_capture_pattern = all_consuming(http_pattern)("Http<Get>^/some/(.*)")?.1;

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

        assert_eq!(any_pattern.is_match(&get_some), Ok(()));
        assert_eq!(any_pattern.is_match(&post_some), Ok(()));
        assert_eq!(get_any_pattern.is_match(&get_some), Ok(()));
        assert_eq!(get_any_pattern.is_match(&get_some_plus), Ok(()));
        assert_eq!(get_some_pattern.is_match(&get_some), Ok(()));
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
            uri: Uri::from_static( "/some"),
            body: Payload::Empty,
        };

        let get_some_plus = RequestCore {
            action: Action::Http(HttpMethod::GET),
            headers: Default::default(),
            uri: Uri::from_static( "/some/plus" ),
            body: Payload::Empty,
        };

        let post_some = RequestCore {
            action: Action::Http(HttpMethod::POST),
            headers: Default::default(),
            uri: Uri::from_static( "/some"),
            body: Payload::Empty,
        };

        let get_selector = all_consuming(http_pipeline)("<Get> -> {{}};")?.1;
        let get_some_selector = all_consuming(http_pipeline)("<Get>^/some -> {{}} => &;")?.1;
        let post_some_selector = all_consuming(http_pipeline)("<Post>^/some -> {{}} => &;")?.1;

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
            uri: Uri::from_static( "/some" ),
            body: Payload::Empty,
        };

        let get_some_plus = RequestCore {
            action: Action::Http(HttpMethod::GET),
            headers: Default::default(),
            uri: Uri::from_static( "/some/plus" ),
            body: Payload::Empty,
        };

        let post_some = RequestCore {
            action: Action::Http(HttpMethod::POST),
            headers: Default::default(),
            uri: Uri::from_static( "/some" ),
            body: Payload::Empty,
        };

        let delete_some = RequestCore {
            action: Action::Http(HttpMethod::DELETE),
            headers: Default::default(),
            uri: Uri::from_static( "/some" ),
            body: Payload::Empty,
        };

        let section = all_consuming(select_http_pipelines)(
            r#"Http {
          <Get>^/(.*) -> {{}} => &;
          <Get>^/some -> {{ }} => &;
          <Post>^/some -> {{ }} => &;
        }"#,
        )?
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
        http_method("Get")?;
        assert!(http_method("Bad").is_err());
        Ok(())
    }

    #[test]
    pub fn test_http_method_pattern() -> Result<(), MsgErr> {
        http_method_pattern("*")?;
        http_method_pattern("Get")?;
        assert!(http_method_pattern("Bad").is_err());
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
       let span = Span::new("rWx-");
       let perm = particle_perms(span)?;
        let (span,block) = perm;
        println!("offset : {}",span.location_offset());

        assert_eq!(block, ParticlePerms {read:false,write:true,execute:false});
       Ok(())
    }

    #[test]
    pub fn test_permissions() -> Result<(), MsgErr> {
        let span = Span::new("cSd-RwX");
        let result = permissions(span)?;
        let (span,permissions) = result;
        println!("offset : {}",span.location_offset());

        assert_eq!(permissions, crate::version::v0_0_1::security::Permissions {child: ChildPerms{create:false,select:true,delete:false}, particle: ParticlePerms {read:true,write:false,execute:true}});
        Ok(())
    }

    #[test]
    pub fn test_permissions_mask() -> Result<(), MsgErr> {
        let span = Span::new("&cSd-RwX");
        let result = permissions_mask(span)?;
        let (span,mask) = result;
        let permissions = crate::version::v0_0_1::security::Permissions {child: ChildPerms{create:false,select:true,delete:false}, particle: ParticlePerms {read:true,write:false,execute:true}};
        let mask2 = PermissionsMask {
            kind: PermissionsMaskKind::And,
            permissions
        };
        assert_eq!(mask, mask2);
        Ok(())
    }

    #[test]
    pub fn test_root_pattern_match() -> Result<(),MsgErr>{
        let pattern = AddressKindPattern::from_str("+:**")?;
        assert!(pattern.matches_root());
        Ok(())
    }
}

