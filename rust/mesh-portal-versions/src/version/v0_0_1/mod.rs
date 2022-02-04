use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::error::Error;
use crate::version::v0_0_1::bin::Bin;

pub type State = HashMap<String, Bin>;

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
        pub from: Address,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ArtifactResponse<B> {
        pub to: Address,
        pub payload: B,
    }
}

pub type Port = String;

pub mod id {
    use std::collections::HashMap;
    use std::convert::TryInto;
    use std::fmt::Formatter;
    use std::ops::Deref;
    use std::str::FromStr;
    use nom::bytes::complete::tag;
    use nom::combinator::{all_consuming, opt};
    use nom::sequence::{delimited, tuple};

    use semver::SemVerError;
    use serde::de::Visitor;
    use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

    use crate::error::Error;
    use crate::version::v0_0_1::parse::{address, camel_case, consume_address, Res};
    use crate::version::v0_0_1::pattern::parse::{address_and_kind, resource_type, specific};
    use crate::version::v0_0_1::pattern::{Pattern, SpecificPattern, VersionReq};

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
            format!("{}<{}>", self.address.to_string(), self.kind.to_string() )
        }
    }

    impl  FromStr for AddressAndKind{
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let address_and_kind = match all_consuming(address_and_kind)(s) {
                Ok((_,address_and_kind)) => address_and_kind,
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
        type Error = Error;

        fn try_into(self) -> Result<semver::Version, Self::Error> {
            Ok(self.version)
        }
    }

    impl FromStr for Version {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let version = semver::Version::from_str(s)?;
            Ok(Self { version })
        }
    }

    /// Stands for "Type, Kind, Specific"
    pub trait Tks
    {
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
        type Error = Error;

        fn try_into(self) -> Result<SpecificPattern,Self::Error> {
            Ok(SpecificPattern{
                vendor: Pattern::Exact(self.vendor),
                product: Pattern::Exact(self.product),
                variant: Pattern::Exact(self.variant),
                version: VersionReq::from_str( self.version.to_string().as_str() )?
            })
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum RouteSegment {
        Resource,
        Domain(String),
        Tag(String),
        Mesh(String),
    }

    impl ToString for RouteSegment {
        fn to_string(&self) -> String {
            match self {
                RouteSegment::Resource => "".to_string(),
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

        pub fn is_version(&self) -> bool {
            match self {
                AddressSegment::Version(_) => true,
                _ => false
            }
        }

        pub fn is_filepath(&self) -> bool {
            match self {
                AddressSegment::Dir(_) => true,
                AddressSegment::FilesystemRootDir => true,
                AddressSegment::File(_) => true,
                _ => false
            }
        }

        pub fn is_file(&self) -> bool {
            match self {
                AddressSegment::File(_) => true,
                _ => false
            }
        }

        pub fn is_dir(&self) -> bool {
            match self {
                AddressSegment::Dir(_) => true,
                AddressSegment::FilesystemRootDir => true,
                _ => false
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
                AddressSegment::Root => ""
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
                AddressSegment::Root => false
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
                AddressSegment::Root => "".to_string()
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct Address {
        pub route: RouteSegment,
        pub segments: Vec<AddressSegment>,
    }

    impl Address {

        pub fn is_artifact_bundle_part(&self) -> bool {
            for segment in &self.segments {
                if segment.is_version() {
                    return true;
                }
            }
            return false;
        }

        pub fn is_artifact(&self) -> bool {
            if let Option::Some(segment) =  self.last_segment() {
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
            if let Option::Some(segment) =  self.last_segment() {
                segment.is_version()
            } else {
                false
            }
        }

        pub fn push(&self, segment: String) -> Result<Self, Error> {
            if self.segments.is_empty() {
                Self::from_str(segment.as_str())
            } else {
                let last = self.last_segment().expect("expected last segment");
                let address = match last {
                    AddressSegment::Root => {
                        segment
                    }
                    AddressSegment::Space(_) => {
                        format!("{}:{}", self.to_string(), segment)
                    }
                    AddressSegment::Base(_) => {
                        format!("{}:{}", self.to_string(), segment)
                    }
                    AddressSegment::FilesystemRootDir => {
                        format!("{}{}",self.to_string(),segment)
                    }
                    AddressSegment::Dir(_) => {
                        format!("{}{}",self.to_string(),segment)
                    }
                    AddressSegment::Version(_) => {
                        if segment != "/" {
                            return Err("Root filesystem artifact dir required after version".into());
                        }
                        format!("{}:/",self.to_string())
                    }
                    AddressSegment::File(_) => {
                        return Err("cannot append to a file".into())
                    }
                };
                Self::from_str(address.as_str())
            }
        }

        pub fn push_file(&self, segment: String) -> Result<Self, Error> {
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

        pub fn is_filesystem_ref(&self) -> bool {
            if let Option::Some(last_segment) = self.last_segment() {
                last_segment.is_filesystem_ref()
            } else {
                false
            }
        }
    }

    impl FromStr for Address {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(consume_address(s)?.1)
        }
    }

    impl Into<String> for Address {
        fn into(self) -> String {
            self.to_string()
        }
    }

    impl ToString for Address {
        fn to_string(&self) -> String {
            let mut rtn = String::new();

            match &self.route {
                RouteSegment::Resource => {}
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
            }
            else {
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
            segments.remove(segments.len()-1);
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
                route: RouteSegment::Resource,
                segments: vec![],
            }
        }

        pub fn root_with_route( route: RouteSegment) -> Self {
            Self {
                route,
                segments: vec![],
            }
        }

        pub fn is_root(&self) -> bool {
            self.segments.is_empty()
        }
    }


    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct KindParts {
        pub resource_type: ResourceType,
        pub kind: Option<String>,
        pub specific: Option<Specific>,
    }

    impl ToString for KindParts
    {
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
        type Err = Error;

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

    impl Tks for KindParts
    {
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
    use crate::error::Error;
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

        pub fn make_absolute(string: &str) -> Result<Self, Error> {
            if string.starts_with("/") {
                Path::from_str(string)
            } else {
                Path::from_str(format!("/{}", string).as_str())
            }
        }

        pub fn bin(&self) -> Result<Vec<u8>, Error> {
            let bin = bincode::serialize(self)?;
            Ok(bin)
        }

        pub fn is_absolute(&self) -> bool {
            self.string.starts_with("/")
        }

        pub fn cat(&self, path: &Path) -> Result<Self, Error> {
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
        type Err = Error;

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

    use crate::error::Error;

    use crate::version::v0_0_1::id::{Address, AddressSegment, ResourceKind, ResourceType, RouteSegment, Specific, Tks, Version};
    use crate::version::v0_0_1::parse::{address, camel_case_to_string, consume_address_kind_path, file_chars, path, path_regex, Res};
    use crate::version::v0_0_1::pattern::parse::{address_kind_pattern, pattern};
    use crate::version::v0_0_1::pattern::specific::{
        ProductPattern, VariantPattern, VendorPattern,
    };
    use crate::version::v0_0_1::payload::{Call, CallKind, CallWithConfig, HttpCall, HttpMethod, ListPattern, MapPattern, MsgCall, Payload, PayloadFormat, PayloadPattern, PayloadTypePattern, Primitive, PrimitiveType, Range};
    use crate::version::v0_0_1::util::{StringMatcher, ValueMatcher, ValuePattern};
    use crate::{Deserialize, Serialize};
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0};
    use nom::combinator::{all_consuming, opt, recognize};
    use nom::error::{ErrorKind, VerboseError};
    use nom::multi::separated_list0;
    use nom::sequence::{delimited, preceded, terminated, tuple};
    use nom::{AsChar, InputTakeAtPosition, Parser};
    use nom_supreme::{parse_from_str, ParserExt};
    use std::collections::HashMap;
    use nom_supreme::parser_ext::FromStrParser;
    use crate::version::v0_0_1::entity::request::{Http, Msg, RcCommandType, ReqEntity};


    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TksPattern {
        pub resource_type: ResourceTypePattern,
        pub kind: KindPattern,
        pub specific: ValuePattern<SpecificPattern>,
    }


    impl TksPattern
    {
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
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct AddressKindPattern {
        pub hops: Vec<Hop>,
    }

    impl FromStr for AddressKindPattern {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_,rtn) = all_consuming(address_kind_pattern)(s)?;
            Ok(rtn)
        }
    }

    impl AddressKindPattern {
        fn consume(&self) -> Option<AddressKindPattern>

        {
            if self.hops.len() <= 1 {
                Option::None
            } else {
                let mut hops = self.hops.clone();
                hops.remove(0);
                Option::Some(AddressKindPattern { hops })
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
                route: RouteSegment::Resource,
                segments,
            }
        }

        pub fn sub_select_hops(&self) -> Vec<Hop>
        {
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
            if address_kind_path.segments.len() < self.hops.len() {
                return false;
            }

            if address_kind_path.is_root() && self.is_root() {
                return true;
            }

            if address_kind_path.segments.is_empty() || self.hops.is_empty() {
                return false;
            }

            let hop = self.hops.first().expect("hop");
            let seg = address_kind_path.segments.first().expect("segment");


            if address_kind_path.is_final() && self.is_final() {
                // this is the final hop & segment if they match, everything matches!
                hop.matches(seg)
            } else if address_kind_path.is_root() {
                false
            } else if self.is_root() {
                false
            } else if address_kind_path.is_final() {
                // we still have hops that haven't been matched and we are all out of path
                false
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
            } else if hop.matches(seg) {
println!("seg {}", seg.to_string() );
println!("self.hops.len() {}", self.hops.len() );
println!("address_kind_path.len() {}", address_kind_path.segments.len() );
println!("address_kind_path.to_string() {}", address_kind_path.to_string() );
                // in a normal match situation, we consume the hop and move to the next one
                self.consume()
                    .expect("AddressTksPattern")
                    .matches(&address_kind_path.consume().expect("AddressKindPath"))
            } else {
                false
            }
        }
    }

    #[derive(Debug, Clone)]
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
        type Error = Error;

        fn try_into(self) -> Result<semver::VersionReq, Self::Error> {
            Ok(self.version)
        }
    }

    impl FromStr for VersionReq {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let version = semver::VersionReq::from_str(s)?;
            Ok(Self { version })
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum SegmentPattern {
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
                SegmentPattern::Any => true,
                SegmentPattern::Recursive => true,
                SegmentPattern::Exact(exact) => match exact {
                    ExactSegment::Address(pattern) => *pattern == *segment,
                    ExactSegment::Version(a) => {
                        if let AddressSegment::Version(b) = segment {
                            *a == *b
                        } else
                        {
                            false
                        }
                    }
                },
                SegmentPattern::Version(req) => {
                    if let AddressSegment::Version(b) = segment {
                        req.matches(b)
                    } else
                    {
                        false
                    }
                }
            }
        }

        pub fn is_recursive(&self) -> bool {
            match self {
                SegmentPattern::Any => false,
                SegmentPattern::Recursive => true,
                SegmentPattern::Exact(_) => false,
                SegmentPattern::Version(_) => false,
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

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SpecificPattern {
        pub vendor: VendorPattern,
        pub product: ProductPattern,
        pub variant: VariantPattern,
        pub version: VersionReq,
    }

    impl ValueMatcher<Specific> for SpecificPattern {
        fn is_match(&self, specific: &Specific) -> Result<(), Error> {
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

        use crate::error::Error;
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
            type Err = Error;

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
        use nom::error::{context, ParseError, VerboseError};
        use nom::multi::{many1, many0};
        use nom::sequence::{delimited, preceded, terminated, tuple};
        use nom::Parser;
        use nom::{Err, IResult};

        use crate::error::Error;
        use crate::version::v0_0_1::id::{AddressAndKind, AddressSegment, ResourceKind, KindParts, ResourceType, Specific, Version};
        use crate::version::v0_0_1::parse::{address_segment_chars, camel_case, domain_chars, skewer_chars, version_req_chars, Res, version_address_segment, version_chars, address, file_chars};
        use crate::version::v0_0_1::pattern::specific::{
            ProductPattern, VariantPattern, VendorPattern,
        };
        use crate::version::v0_0_1::pattern::{AddressKindPattern, ExactSegment, Hop, KindPattern, Pattern, ResourceTypePattern, SegmentPattern, SpecificPattern, TksPattern, VersionReq};
        use crate::version::v0_0_1::util::ValuePattern;
        use nom_supreme::{parse_from_str,ParserExt};

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

            let (next, version): (&str,Version)  = parse_from_str(version_chars).parse(input)?;

            Ok((next, SegmentPattern::Exact(ExactSegment::Version(version))))
        }

        fn version_req_segment(input: &str) -> Res<&str, SegmentPattern> {
            delimited( tag("("), version_req, tag(")"))(input).map( |(next,version_req)| {
                (next, SegmentPattern::Version(version_req) )
            } )
        }



        fn space_segment(input: &str) -> Res<&str, SegmentPattern> {
            alt((recursive_segment, any_segment, exact_space_segment))(input)
        }

        fn base_segment(input: &str) -> Res<&str, SegmentPattern> {
            alt((recursive_segment, any_segment, exact_base_segment))(input)
        }

        fn file_segment(input: &str) -> Res<&str, SegmentPattern> {
            alt((recursive_segment, any_segment, exact_file_segment))(input)
        }

        fn dir_segment(input: &str) -> Res<&str, SegmentPattern> {
            terminated(alt((recursive_segment, any_segment, exact_dir_segment)),tag("/"))(input)
        }


        fn version_segment(input: &str) -> Res<&str, SegmentPattern> {
            alt((recursive_segment, any_segment, exact_version_segment, version_req_segment))(input)
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

        fn value_pattern<P>(
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

        pub fn kind(input: &str) -> Res<&str, ResourceKind>
        {
            parse_from_str(rec_kind)
                .parse(input)
                .map(|(next, kind)| (next, kind))
        }

        pub fn delim_kind(input: &str) -> Res<&str, ResourceKind>
        {
            delimited(tag("<"), kind, tag(">"))(input)
        }

        pub fn consume_kind(input: &str) -> Result<ResourceKind, Error>
        {
            let (_, kind_parts) = all_consuming(kind_parts)(input)?;

            Ok(kind_parts.try_into()?)
        }

        pub fn kind_pattern(input: &str) -> Res<&str, KindPattern>
        {
            pattern(kind)(input).map(|(next, kind)| (next, kind))
        }

        pub fn resource_type(input: &str) -> Res<&str, ResourceType> {
            parse_from_str(camel_case).parse(input)
        }

        pub fn resource_type_pattern(
            input: &str,
        ) -> Res<&str, ResourceTypePattern> {
            pattern(resource_type)(input)
        }

        pub fn tks(
            input: &str,
        ) -> Res<&str, TksPattern> {
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

        fn space_hop(
            input: &str,
        ) -> Res<&str, Hop> {
            tuple((space_segment, opt(tks)))(input).map(|(next, (segment, tks))| {
                let tks = match tks {
                    None => TksPattern::any(),
                    Some(tks) => tks,
                };
                (next, Hop { segment, tks })
            })
        }

        fn base_hop(
            input: &str,
        ) -> Res<&str, Hop> {
            tuple((base_segment, opt(tks)))(input).map(|(next, (segment, tks))| {
                let tks = match tks {
                    None => TksPattern::any(),
                    Some(tks) => tks,
                };
                (next, Hop { segment, tks })
            })
        }


        fn file_hop(
            input: &str,
        ) -> Res<&str, Hop> {
            file_segment(input).map(|(next, segment)| {
                let tks = TksPattern {
                    resource_type: Pattern::Exact("File".to_string()),
                    kind: Pattern::Any,
                    specific: ValuePattern::Any
                };
                (next, Hop { segment, tks })
            })
        }


        fn dir_hop(
            input: &str,
        ) -> Res<&str, Hop> {
            dir_segment(input).map(|(next, segment)| {
                let tks = TksPattern::any();
                (next, Hop { segment, tks })
            })
        }


        fn version_hop(
            input: &str,
        ) -> Res<&str, Hop> {
            tuple((version_segment, opt(tks)))(input).map(|(next, (segment, tks))| {
                let tks = match tks {
                    None => TksPattern::any(),
                    Some(tks) => tks,
                };
                (next, Hop { segment, tks })
            })
        }


        pub fn address_kind_pattern(
            input: &str,
        ) -> Res<&str, AddressKindPattern> {
            context( "address_kind_pattern", tuple( (space_hop,many0(preceded(tag(":"),base_hop) ),opt(preceded( tag(":"), version_hop)),opt(preceded(tag(":/"),tuple((many0(dir_hop),opt(file_hop))))))))(input).map( |(next, (space_hop, base_hops, version_hop, filesystem_hops) )| {
                let mut hops = vec![];
                hops.push(space_hop);
                for base_hop in base_hops {
                    hops.push(base_hop);
                }
                if let Option::Some(version_hop) = version_hop {
                    hops.push( version_hop );
                }
                if let Some((dir_hops,file_hop)) = filesystem_hops{
                    // first push the filesystem root
                    hops.push( Hop {
                        segment: SegmentPattern::Exact(ExactSegment::Address(AddressSegment::FilesystemRootDir)),
                        tks: TksPattern {
                            resource_type: Pattern::Exact("Dir".to_string()),
                            kind: Pattern::Any,
                            specific: ValuePattern::Any
                        }
                    });
                    for dir_hop in dir_hops {
                        hops.push(dir_hop );
                    }
                    if let Some(file_hop) = file_hop {
                        hops.push(file_hop );
                    }
                }

                let rtn = AddressKindPattern {
                    hops
                };

                (next,rtn)
            } )
        }

        pub fn address_and_kind(input: &str) -> Res<&str, AddressAndKind>  {
            tuple((address,kind))(input).map( |(next,(address,kind))| {
                (next,AddressAndKind {
                    address,
                    kind,
                })
            } )
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
        T: InputTakeAtPosition,
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

    fn not_quote<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
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
        T: InputTakeAtPosition,
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

    impl ValueMatcher<ReqEntity> for EntityPattern {
        fn is_match(&self, entity: &ReqEntity) -> Result<(), Error> {
            match entity {
                ReqEntity::Rc(found) => {
                    if let EntityPattern::Rc(pattern) = self {
                        if pattern.command.matches(&found.command.get_type()) {
                            Ok(())
                        } else {
                            Err("no match".into())
                        }
                    } else {
                        Err(format!(
                            "Entity pattern mismatch. expected: '{}' found: '{}'",
                            self.to_string(),
                            found.to_string()
                        )
                        .into())
                    }
                }
                ReqEntity::Msg(found) => {
                    if let EntityPattern::Msg(pattern) = self {
                        pattern.is_match(found)
                    } else {
                        Err(format!(
                            "Entity pattern mismatch. expected: '{}' found: '{}'",
                            self.to_string(),
                            found.to_string()
                        )
                        .into())
                    }
                }
                ReqEntity::Http(found) => {
                    if let EntityPattern::Http(pattern) = self {
                        pattern.is_match(found)
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

    impl ValueMatcher<Msg> for MsgPattern {
        fn is_match(&self, found: &Msg) -> Result<(), Error> {
            self.action.is_match(&found.action)?;
            let matches = found.path.matches(&self.path_regex);
            if matches.count() > 0 {
                Ok(())
            } else {
                Err(format!(
                    "Could not match Msg path: '{}' with: '{}'",
                    found.path, self.path_regex
                )
                .into())
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HttpPattern {
        pub method: ValuePattern<HttpMethod>,
        pub path_regex: String,
    }

    impl ToString for HttpPattern {
        fn to_string(&self) -> String {
            format!("Http<{}>{}", self.method.to_string(), self.path_regex)
        }
    }

    impl ValueMatcher<Http> for HttpPattern {
        fn is_match(&self, found: &Http) -> Result<(), Error> {
            self.method.is_match(&found.method)?;

            let matches = found.path.matches(&self.path_regex);
            if matches.count() > 0 {
                Ok(())
            } else {
                Err(format!(
                    "Could not match Msg path: '{}' with: '{}'",
                    found.path, self.path_regex
                )
                .into())
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

    pub fn rc_call_kind(input: &str) -> Res<&str, CallKind> {
        delimited(tag("Rc<"), rc_command, tag(">"))(input)
            .map(|(next, rc_command)| (next, CallKind::Rc(rc_command)))
    }

    pub fn msg_call(input: &str) -> Res<&str, CallKind> {
        tuple((
            delimited(tag("Msg<"), alphanumeric1, tag(">")),
            opt(recognize(path)),
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
            delimited(tag("Http<"), parse_from_str(alphanumeric1), tag(">")),
            path,
        ))(input)
        .map(|(next, (method, path))| {
            (
                next,
                CallKind::Http(HttpCall::new(method, path.to_string())),
            )
        })
    }

    pub fn call_kind(input: &str) -> Res<&str, CallKind> {
        alt((rc_call_kind, msg_call, http_call))(input)
    }

    pub fn call(input: &str) -> Res<&str, Call> {
        tuple((address, preceded(tag("^"), call_kind)))(input)
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
        Payload(Payload),
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
        primitive(input).map(|(next, primitive)| (next, PayloadTypePattern::Primitive(primitive)))
    }

    pub fn array_data_struct(input: &str) -> Res<&str, PayloadTypePattern> {
        tuple((primitive, delimited(tag("["), range, tag("]"))))(input).map(
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

    pub fn value_pattern<V>(
        input: &str,
        parser: fn(&str) -> Res<&str, V>,
    ) -> Res<&str, ValuePattern<V>> {
        let result = parser(input);
        match result {
            Ok((next, v)) => {
                return Ok((next, ValuePattern::Pattern(v)));
            }
            Err(error) => {
                // do nothing
            }
        }

        alt((tag("*"), multispace0))(input).map(|(next, tag)| {
            let rtn = match tag {
                "*" => ValuePattern::Any,
                _ => ValuePattern::None,
            };
            (next, rtn)
        })
    }

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
        value_pattern(input, map_pattern)
    }

    pub fn msg_action(input: &str) -> Res<&str, ValuePattern<StringMatcher>> {
        value_pattern(input, camel_case_to_string)
    }

    pub fn msg_pattern_scoped(input: &str) -> Res<&str, MsgPattern> {
        tuple((msg_action, opt(path_regex)))(input).map(|(next, (action, path_regex))| {
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
        parse_from_str(alpha1).parse(input)
    }

    pub fn http_method_pattern(input: &str) -> Res<&str, ValuePattern<HttpMethod>> {
        value_pattern(input, http_method)
    }

    pub fn http_pattern_scoped(input: &str) -> Res<&str, HttpPattern> {
        tuple((http_method_pattern, opt(path_regex)))(input).map(|(next, (method, path_regex))| {
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
        pattern(rc_command_type)(input).map(|(next, command)| (next, RcPattern { command }))
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
            payload_structure,
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
        value_pattern(input, payload_structure_with_validation)
            .map(|(next, payload_pattern)| (next, payload_pattern))
    }

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

    pub fn pattern_block_empty(input: &str) -> Res<&str, PatternBlock> {
        multispace0(input).map(|(next, _)| (input, PatternBlock::None))
    }

    pub fn pattern_block_any(input: &str) -> Res<&str, PatternBlock> {
        let (next, _) = delimited(multispace0, tag("*"), multispace0)(input)?;

        Ok((next, PatternBlock::Any))
    }

    pub fn pattern_block_def(input: &str) -> Res<&str, PatternBlock> {
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
                Block::Payload(Payload::Primitive(Primitive::Text(text.to_string()))),
            )
        })
    }

    pub fn upload_pattern_block(input: &str) -> Res<&str, Block> {
        delimited(
            tag("^["),
            tuple((multispace0, file_chars, multispace0)),
            tag("]"),
        )(input)
        .map(|(next, (_, filename,_))| {
            (
                next,
                Block::Upload(UploadBlock {
                    name: filename.to_string(),
                }),
            )
        })
    }

    pub fn upload_step(input: &str) -> Res<&str, UploadBlock> {
        terminated(
            upload_pattern_block,
            tag("->"))
        (input)
            .map(|(next, block)| {
                if let Block::Upload(block) = block {
                    (
                        next,
                        block
                    )
                } else {
                    panic!("it should have been an UploadBlock!");
                }
            })
    }


    pub fn request_pattern_block(input: &str) -> Res<&str, Block> {
        delimited(
            tag("-["),
            tuple((
                multispace0,
                alt((pattern_block_any, pattern_block_def, pattern_block_empty)),
                multispace0,
            )),
            tag("]"),
        )(input)
        .map(|(next, (_, block, _))| (next, Block::RequestPattern(block)))
    }

    pub fn response_pattern_block(input: &str) -> Res<&str, Block> {
        delimited(
            tag("=["),
            tuple((
                multispace0,
                alt((pattern_block_any, pattern_block_def, pattern_block_empty)),
                multispace0,
            )),
            tag("]"),
        )(input)
        .map(|(next, (_, block, _))| (next, Block::ResponsePattern(block)))
    }

    pub fn pipeline_block(input: &str) -> Res<&str, Block> {
        alt((request_pattern_block, response_pattern_block, upload_pattern_block))(input)
    }

    pub fn consume_pipeline_block(input: &str) -> Res<&str, Block> {
        all_consuming(pipeline_block)(input)
    }

    #[derive(Clone, Eq, PartialEq)]
    pub struct MapEntryPattern {
        pub key: String,
        pub payload: ValuePattern<PayloadPattern>,
    }





    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Hop{
        pub segment: SegmentPattern,
        pub tks: TksPattern,
    }

    impl Hop {
        pub fn matches(&self, address_kind_segment: &AddressKindSegment) -> bool {
            self.segment.matches(&address_kind_segment.address_segment)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Pattern<P> {
        Any,
        Exact(P),
    }

    impl<P> Pattern<P>
        where
            P: Eq + PartialEq,
    {
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

        pub fn convert<To>(self) -> Result<Pattern<To>, Error>
            where
                P: TryInto<To, Error = Error> + Eq + PartialEq,
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

        pub fn convert<To>(self) -> Result<EmptyPattern<To>, Error>
            where
                P: TryInto<To, Error = Error> + Eq + PartialEq,
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
            Self {
                route,
                segments,
            }
        }

        pub fn push(
            &self,
            segment: AddressKindSegment,
        ) -> AddressKindPath
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

        pub fn consume(&self) -> Option<AddressKindPath>
        {
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

    impl ToString for AddressKindPath
    {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            match &self.route {
                RouteSegment::Resource => {}
                route => {
                    rtn.push_str(route.to_string().as_str());
                    rtn.push_str("::");
                }
            }

            for (index, segment) in self.segments.iter().enumerate() {
                rtn.push_str(segment.to_string().as_str());
                if index < self.segments.len() {
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

    impl FromStr for AddressKindPath
    {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(consume_address_kind_path(s)?)
        }
    }
}

pub mod messaging {
    use std::convert::TryInto;

    use serde::{Deserialize, Serialize};

    use crate::error::Error;
    use crate::version::v0_0_1::entity::request::ReqEntity;
    use crate::version::v0_0_1::entity::response::RespEntity;
    use crate::version::v0_0_1::id::Address;
    use crate::version::v0_0_1::util::unique_id;

    pub type ExchangeId = String;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ExchangeType {
        Notification,
        RequestResponse,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Exchange {
        Notification,
        RequestResponse(ExchangeId),
    }

    impl Exchange {
        pub fn requires_response(&self) -> bool {
            match self {
                Exchange::Notification => false,
                Exchange::RequestResponse(_) => true,
            }
        }
    }

    impl TryInto<ExchangeId> for Exchange {
        type Error = Error;

        fn try_into(self) -> Result<ExchangeId, Self::Error> {
            match self {
                Exchange::Notification => Err(
                    "Exchange Notification cannot be converted into a RequestResponse Exchange"
                        .into(),
                ),
                Exchange::RequestResponse(id) => Ok(id),
            }
        }
    }

    impl Into<ExchangeType> for Exchange {
        fn into(self) -> ExchangeType {
            match self {
                Exchange::Notification => ExchangeType::Notification,
                Exchange::RequestResponse(_) => ExchangeType::RequestResponse,
            }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct Request {
        pub id: String,
        pub from: Address,
        pub to: Address,
        pub entity: ReqEntity
    }

    impl Request {
        pub fn new( entity: ReqEntity, from: Address, to: Address ) -> Self {
            Self {
                id: unique_id(),
                from,
                to,
                entity
            }
        }

        pub fn respond( self, entity: RespEntity ) -> ProtoResponse {
            ProtoResponse::new( self.from, entity, self.id)
        }
    }

    #[derive(Debug,Clone)]
    pub struct ProtoRequest {
        pub id: String,
        pub to: Option<Address>,
        pub entity: Option<ReqEntity>
    }

    impl ProtoRequest {
        pub fn new() -> Self {
            Self {
                id: unique_id(),
                to: Option::None,
                entity: Option::None
            }
        }

        pub fn to( &mut self, to: Address ) {
            self.to = Option::Some(to);
        }

        pub fn entity( &mut self, entity: ReqEntity ) {
            self.entity = Option::Some(entity);
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct Response {
        pub id: String,
        pub from: Address,
        pub to: Address,
        pub entity: RespEntity,
        pub response_to: String
    }

    #[derive(Debug,Clone)]
    pub struct ProtoResponse {
        pub to: Address,
        pub entity: RespEntity,
        pub response_to: String
    }

    impl ProtoResponse{
        pub fn new(  to: Address, entity: RespEntity, response_to: String ) -> Self {
            Self {
                to,
                entity,
                response_to
            }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub enum Message {
        Request(Request),
        Response(Response)
    }

    impl Message{
        pub fn to(&self) -> Address {
            match self {
                Message::Request(request) => {
                    request.to.clone()
                }
                Message::Response(response) => {
                    response.to.clone()
                }
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

}

pub mod log {
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Log {
        Warn(String),
        Info(String),
        Error(String),
        Fatal(String),
    }

    impl ToString for Log {
        fn to_string(&self) -> String {
            match self {
                Log::Warn(message) => {
                    format!("WARN: {}", message)
                }
                Log::Info(message) => {
                    format!("INFO: {}", message)
                }
                Log::Error(message) => {
                    format!("ERROR: {}", message)
                }
                Log::Fatal(message) => {
                    format!("FATAL: {}", message)
                }
            }
        }
    }
}

pub mod frame {
    use std::convert::TryInto;

    use serde::{Deserialize, Serialize};

    use crate::error::Error;

    pub struct PrimitiveFrame {
        pub data: Vec<u8>,
    }

    impl PrimitiveFrame {
        pub fn size(&self) -> u32 {
            self.data.len() as u32
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
        type Error = Error;

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
    use std::collections::HashMap;
    use std::ops::{Deref, DerefMut};
    use serde::{Deserialize, Serialize};

    use crate::error::Error;
    use crate::version::v0_0_1::bin::Bin;
    use crate::version::v0_0_1::id::{Address, ResourceKind, Meta, PayloadClaim, ResourceType};
    use crate::version::v0_0_1::pattern::TksPattern;
    use std::str::FromStr;
    use crate::version::v0_0_1::entity::request::RcCommandType;
    use crate::version::v0_0_1::resource::{Resource, ResourceStub, Status};
    use crate::version::v0_0_1::util::{ValueMatcher, ValuePattern};


    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
    pub enum Payload {
        Empty,
        Primitive(Primitive),
        List(PrimitiveList),
        Map(PayloadMap),
    }

    impl Payload {
        pub fn payload_type(&self) -> PayloadType {
            match self {
                Payload::Empty => PayloadType::Empty,
                Payload::Primitive(primitive) => PayloadType::Primitive,
                Payload::List(list) => PayloadType::List,
                Payload::Map(map) => PayloadType::Map,
            }
        }
    }

    impl TryInto<Primitive> for Payload {
        type Error = Error;

        fn try_into(self) -> Result<Primitive, Self::Error> {
            match self {
                Payload::Primitive(primitive) => {
                    Ok(primitive)
                }
                _ => { Err("Payload type must be Primitive".into())}
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

        pub fn new() -> Self {
            Self {
                map: HashMap::new(),
            }
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
            }
        }
    }

    impl TryInto<Bin> for Primitive {
        type Error = Error;

        fn try_into(self) -> Result<Bin, Self::Error> {
            match self {
                Primitive::Bin(bin) => {
                    Ok(bin)
                }
                _ => {
                    Err("Primitive must be of type Bin".into())
                }
            }
        }
    }

    impl TryInto<String> for Primitive {
        type Error = Error;

        fn try_into(self) -> Result<String, Self::Error> {
            match self {
                Primitive::Text(text) => {
                    Ok(text)
                }
                _ => {
                    Err("Primitive must be of type Text".into())
                }
            }
        }
    }


    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct PrimitiveList {
        pub primitive_type: PrimitiveType,
        pub list: Vec<Primitive>,
    }


    impl ToString for PrimitiveList
    {
        fn to_string(&self) -> String {
            format!("{}[]", self.primitive_type.to_string())
        }
    }

    impl PrimitiveList
    {
        pub fn new(primitive_type: PrimitiveType) -> Self {
            Self {
                primitive_type,
                list: vec![],
            }
        }
        pub fn validate(&self) -> Result<(), Error> {
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
    }

    impl Deref for PrimitiveList
    {
        type Target = Vec<Primitive>;

        fn deref(&self) -> &Self::Target {
            &self.list
        }
    }

    impl DerefMut for PrimitiveList
    {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.list
        }
    }
   #[derive(
        Debug,
        Clone,
        strum_macros::Display,
        Eq,
        PartialEq,
        Hash,
        Serialize,
        Deserialize,
    )]
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
    }

    impl FromStr for PrimitiveType {
        type Err = Error;

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
                what => return Err(format!("unrecognized PrimitiveType: {}",what).into())
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
        pub fn is_match(&self, list: &PrimitiveList) -> Result<(), Error> {
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
        pub fn is_match(&self, payload: &Payload) -> Result<(), Error> {
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
        fn is_match(&self, payload: &Payload) -> Result<(), Error> {
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
        pub address: Address,
        pub kind: CallKind,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum CallKind {
        Rc(RcCommandType),
        Msg(MsgCall),
        Http(HttpCall),
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
    pub enum HttpMethod {
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

    impl ValueMatcher<HttpMethod> for HttpMethod {
        fn is_match(&self, found: &HttpMethod) -> Result<(), crate::error::Error> {
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
                CallKind::Rc(command) => {
                    format!("Rc<{}>", command.to_string())
                }
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
        pub fn is_match(
            &self,
            primitive: &Primitive,
        ) -> Result<(), Error>
        {
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

        pub fn is_match(&self, map: &PayloadMap) -> Result<(), Error> {
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
        use std::convert::{TryFrom, TryInto};
        use std::ops::{Deref, DerefMut};

        use serde::{Deserialize, Serialize};

        use crate::error::Error;
        use crate::version::v0_0_1::payload::{Payload, PayloadMap};

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum StateSrc {
            Stateless,
            StatefulDirect(Payload),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum PropertyMod {
            Set{name:String, value: String },
            UnSet(String)
        }


        pub type SetProperties = Vec<PropertyMod>;

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

pub mod http {
    use std::collections::HashMap;

    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::Bin;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HttpRequest {
        pub headers: HashMap<String, String>,
        pub path: String,
        pub body: Option<Bin>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HttpResponse {
        pub headers: HashMap<String, String>,
        pub code: usize,
        pub body: Option<Bin>,
    }

    impl HttpResponse {
        pub fn server_side_error() -> Self {
            Self {
                headers: Default::default(),
                code: 500,
                body: None,
            }
        }
    }
}

pub mod config {
    use std::collections::HashMap;

    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::config::bind::BindConfig;
    use crate::version::v0_0_1::config::mechtron::MechtronConfig;
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

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ConfigBody {
        Bind(BindConfig),
        Mechtron(MechtronConfig),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ResourceConfigBody {
        Control,
        Mechtron(MechtronConfig),
    }

    pub mod mechtron {
        use crate::version::v0_0_1::id::Address;
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct MechtronConfig {
            pub wasm: Address,
            pub kind: String,
        }
    }

    pub mod bind {
        use crate::error::Error;
        use crate::version::v0_0_1::entity::EntityType;
        use crate::version::v0_0_1::pattern::{Block, HttpPattern, MsgPattern, RcPattern};
        use crate::version::v0_0_1::payload::Call;
        use crate::version::v0_0_1::payload::{Payload, PayloadPattern};
        use crate::version::v0_0_1::util::ValuePattern;
        use serde::{Deserialize, Serialize};
        use std::convert::TryInto;
        use crate::version::v0_0_1::entity::request::ReqEntity;

        pub struct ProtoBind {
            pub sections: Vec<Section>,
        }

        impl TryInto<BindConfig> for ProtoBind {
            type Error = Error;

            fn try_into(self) -> Result<BindConfig, Self::Error> {
                let mut opt_msg = Option::None;
                let mut opt_http = Option::None;
                let mut opt_rc = Option::None;

                for section in self.sections {
                    match section {
                        Section::Msg(msg) => {
                            if opt_msg.is_some() {
                                return Err("multiple Msg sections not allowed.".into());
                            }
                            opt_msg = Some(msg);
                        }
                        Section::Http(http) => {
                            if opt_http.is_some() {
                                return Err("multiple Http sections not allowed.".into());
                            }
                            opt_http = Some(http);
                        }
                        Section::Rc(rc) => {
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
            pub msg: Scope<EntityType, Selector<MsgPattern>>,
            pub http: Scope<EntityType, Selector<HttpPattern>>,
            pub rc: Scope<EntityType, Selector<RcPattern>>,
        }

        impl Default for BindConfig {
            fn default() -> Self {
                Self {
                    msg: Scope::new(EntityType::Msg, vec![]),
                    http: Scope::new(EntityType::Http, vec![]),
                    rc: Scope::new(EntityType::Rc, vec![]),
                }
            }
        }

        impl BindConfig {
            pub fn select(&self, entity: ReqEntity) {
                match entity {
                    ReqEntity::Rc(_) => {}
                    ReqEntity::Msg(_) => {}
                    ReqEntity::Http(_) => {}
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Scope<T, E> {
            pub scope_type: T,
            pub elements: Vec<E>,
        }

        impl<T, E> Scope<T, E> {
            pub fn new(scope_type: T, elements: Vec<E>) -> Self {
                Self {
                    scope_type,
                    elements,
                }
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
            Return,
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

        pub enum Section {
            Msg(Scope<EntityType, Selector<MsgPattern>>),
            Http(Scope<EntityType, Selector<HttpPattern>>),
            Rc(Scope<EntityType, Selector<RcPattern>>),
        }

        pub enum ScopeType {
            Bind,
            Msg,
            Http,
            Rc,
        }

        pub mod parse {
            use crate::version::v0_0_1::config::bind::{
                Pipeline, PipelineSegment, PipelineStep, PipelineStop, ProtoBind, Scope, Section,
                Selector, StepKind,
            };
            use crate::version::v0_0_1::entity::EntityType;
            use crate::version::v0_0_1::parse::Res;
            use crate::version::v0_0_1::pattern::{
                call, entity_pattern, http_pattern_scoped, msg_pattern_scoped, pipeline_block,
                rc_pattern_scoped, EntityPattern, HttpPattern, MsgPattern, RcPattern,
            };
            use nom::branch::alt;
            use nom::bytes::complete::tag;
            use nom::character::complete::multispace0;
            use nom::combinator::{all_consuming, opt};
            use nom::multi::{many0, many1};
            use nom::sequence::{delimited, tuple};

            pub fn bind(input: &str) -> Res<&str, ProtoBind> {
                delimited(
                    multispace0,
                    tuple((
                        tag("Bind"),
                        multispace0,
                        delimited(
                            tag("{"),
                            delimited(multispace0, sections, multispace0),
                            tag("}"),
                        ),
                    )),
                    multispace0,
                )(input)
                .map(|(next, (_, _, sections))| {
                    let bind = ProtoBind { sections };

                    (next, bind)
                })
            }

            pub fn sections(input: &str) -> Res<&str, Vec<Section>> {
                delimited(
                    multispace0,
                    many0(delimited(multispace0, section, multispace0)),
                    multispace0,
                )(input)
            }

            pub fn section(input: &str) -> Res<&str, Section> {
                alt((msg_section, http_section, rc_section))(input)
            }

            pub fn msg_section(input: &str) -> Res<&str, Section> {
                tuple((
                    tag("Msg"),
                    multispace0,
                    delimited(
                        tag("{"),
                        delimited(multispace0, msg_selectors, multispace0),
                        tag("}"),
                    ),
                ))(input)
                .map(|(next, (_, _, selectors))| {
                    (next, Section::Msg(Scope::new(EntityType::Msg, selectors)))
                })
            }

            pub fn http_section(input: &str) -> Res<&str, Section> {
                tuple((
                    tag("Http"),
                    multispace0,
                    delimited(
                        tag("{"),
                        delimited(multispace0, http_selectors, multispace0),
                        tag("}"),
                    ),
                ))(input)
                .map(|(next, (_, _, selectors))| {
                    (next, Section::Http(Scope::new(EntityType::Http, selectors)))
                })
            }

            pub fn rc_section(input: &str) -> Res<&str, Section> {
                tuple((
                    tag("Rc"),
                    multispace0,
                    delimited(
                        tag("{"),
                        delimited(multispace0, rc_selectors, multispace0),
                        tag("}"),
                    ),
                ))(input)
                .map(|(next, (_, _, selectors))| {
                    (next, Section::Rc(Scope::new(EntityType::Rc, selectors)))
                })
            }

            pub fn pipeline_step(input: &str) -> Res<&str, PipelineStep> {
                tuple((many0(pipeline_block), alt((tag("->"), tag("=>")))))(input).map(
                    |(next, (blocks, kind))| {
                        let kind = match kind {
                            "->" => StepKind::Request,
                            "=>" => StepKind::Response,
                            _ => panic!("nom parse rules should have selected -> or =>"),
                        };
                        (next, PipelineStep { kind, blocks })
                    },
                )
            }

            pub fn inner_pipeline_stop(input: &str) -> Res<&str, PipelineStop> {
                delimited(
                    tag("{{"),
                    delimited(multispace0, opt(tag("*")), multispace0),
                    tag("}}"),
                )(input)
                .map(|(next, _)| (next, PipelineStop::Internal))
            }

            pub fn return_pipeline_stop(input: &str) -> Res<&str, PipelineStop> {
                tag("&")(input).map(|(next, _)| (next, PipelineStop::Return))
            }

            pub fn call_pipeline_stop(input: &str) -> Res<&str, PipelineStop> {
                call(input).map(|(next, call)| (next, PipelineStop::Call(call)))
            }

            pub fn pipeline_stop(input: &str) -> Res<&str, PipelineStop> {
                alt((
                    inner_pipeline_stop,
                    return_pipeline_stop,
                    call_pipeline_stop,
                ))(input)
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
                many1(pipeline_segment)(input).map(|(next, segments)| (next, Pipeline { segments }))
            }

            pub fn consume_pipeline(input: &str) -> Res<&str, Pipeline> {
                all_consuming(pipeline)(input)
            }

            pub fn entity_selectors(input: &str) -> Res<&str, Vec<Selector<EntityPattern>>> {
                many0(delimited(multispace0, entity_selector, multispace0))(input)
            }

            pub fn msg_selectors(input: &str) -> Res<&str, Vec<Selector<MsgPattern>>> {
                many0(delimited(multispace0, msg_selector, multispace0))(input)
            }

            pub fn http_selectors(input: &str) -> Res<&str, Vec<Selector<HttpPattern>>> {
                many0(delimited(multispace0, http_selector, multispace0))(input)
            }

            pub fn rc_selectors(input: &str) -> Res<&str, Vec<Selector<RcPattern>>> {
                many0(delimited(multispace0, rc_selector, multispace0))(input)
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

            pub fn http_selector(input: &str) -> Res<&str, Selector<HttpPattern>> {
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
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::entity::request::create::Create;
        use crate::version::v0_0_1::entity::request::query::Query;
        use crate::version::v0_0_1::entity::request::select::Select;
        use crate::version::v0_0_1::entity::request::update::Update;
        use crate::version::v0_0_1::entity::response::RespEntity;
        use crate::version::v0_0_1::fail::Fail;
        use crate::version::v0_0_1::id::{Address, ResourceKind, Meta, PayloadClaim, ResourceType};
        use crate::version::v0_0_1::pattern::TksPattern;
        use crate::version::v0_0_1::payload::{HttpMethod, Payload};
        use crate::version::v0_0_1::util::ValueMatcher;
        use serde::{Serialize,Deserialize};


        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum ReqEntity {
            Rc(Rc),
            Msg(Msg),
            Http(Http),
        }

        impl ReqEntity {
            pub fn ok(&self, payload: Payload) -> RespEntity {
                RespEntity::Ok(payload)
            }

            pub fn fail(&self, fail: Fail) -> RespEntity {
                RespEntity::Fail(fail)
            }
        }


        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Rc{
            pub command: RcCommand,
            pub payload: Payload,
        }

        impl Rc {
            pub fn empty_payload(command: RcCommand) ->Self {
                Self{ command, payload: Payload::Empty }
            }

            pub fn with_payload(
                command: RcCommand,
                payload: Payload,
            ) -> Self {
                Self { command, payload }
            }

            pub fn new(command: RcCommand) -> Self {
                Self {
                    command,
                    payload: Payload::Empty,
                }
            }
        }


        #[derive(Debug, Clone, strum_macros::Display, Serialize, Deserialize)]
        pub enum RcCommand {
            Create(Create),
            Select(Select),
            Update(Update),
            Query(Query),
            Get,
        }


        impl RcCommand {
            pub fn get_type(&self) -> RcCommandType {
                match self {
                    RcCommand::Create(_) => RcCommandType::Create,
                    RcCommand::Select(_) => RcCommandType::Select,
                    RcCommand::Update(_) => RcCommandType::Update,
                    RcCommand::Query(_) => RcCommandType::Query,
                    RcCommand::Get => RcCommandType::Get,
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
        pub enum RcCommandType {
            Create,
            Select,
            Update,
            Query,
            Get,
        }

        impl ValueMatcher<RcCommand>
        for RcCommand
        {
            fn is_match(
                &self,
                x: &RcCommand,
            ) -> Result<(), crate::error::Error> {
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
                format!("Rc<{}>", self.command.to_string())
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Msg {
            pub action: String,
            pub path: String,
            pub payload: Payload,
        }

        impl ToString for Msg {
            fn to_string(&self) -> String {
                format!("Msg<{}>{}", self.action, self.path)
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Http {
            pub headers: Meta,
            pub method: HttpMethod,
            pub path: String,
            pub body: Payload,
        }

        impl ToString for Http {
            fn to_string(&self) -> String {
                format!("Http<{}>{}", self.method.to_string(), self.path)
            }
        }
        pub mod create {
            use std::convert::TryInto;

            use serde::{Deserialize, Serialize};

            use crate::error::Error;
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::command::common::{SetProperties, SetRegistry, StateSrc};
            use crate::version::v0_0_1::id::{Address, AddressSegment, HostKey, ResourceKind};
            use crate::version::v0_0_1::pattern::SpecificPattern;
            use crate::version::v0_0_1::payload::{Payload, Primitive};
            use crate::version::v0_0_1::util::ConvertFrom;

            pub enum AddressTemplateSegment {
                AddressSegment(AddressSegment),
                Wildcard(String)
            }

            impl AddressTemplateSegment {
                pub fn is_wildcard(&self) -> bool {
                    match self {
                        AddressTemplateSegment::AddressSegment(_) => {false}
                        AddressTemplateSegment::Wildcard(_) => {true}
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
                type Error = Error;

                fn try_into(self) -> Result<ResourceKind, Self::Error> {
                    if self.specific.is_some() {
                        return Err("cannot create a ResourceKind from a specific pattern when using KindTemplate".into());
                    }
                    Ok(ResourceKind {
                        resource_type: self.resource_type,
                        kind: self.kind,
                        specific: None
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Require {
                File(String)
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Fulfillment {
                File{ name: String, content: Bin },
                Complete
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
                pub fn fulfillment( mut self, bin: Bin) -> Create {
                    Create {
                        template: self.template,
                        state: StateSrc::StatefulDirect(Payload::Primitive(Primitive::Bin(bin))),
                        properties: self.properties,
                        strategy: self.strategy,
                        registry: self.registry
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
                        registry: self.registry
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
            pub enum Strategy{
                Create,
                Apply,
                Ensure,
                HostedBy(HostKey)
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct AddressTemplate {
                pub parent: Address,
                pub child_segment_template: AddressSegmentTemplate,
            }

            #[derive(Debug, Clone, strum_macros::Display, Serialize, Deserialize)]
            pub enum AddressSegmentTemplate {
                Exact(String),
                Pattern(String) // must have a '%'
            }
        }

        pub mod select {
            use std::collections::{HashMap, HashSet};
            use std::convert::{TryFrom, TryInto};
            use std::marker::PhantomData;

            use serde::{Deserialize, Serialize};

            use crate::error::Error;
            use crate::version::v0_0_1::fail::{BadCoercion, Fail};
            use crate::version::v0_0_1::id::Address;
            use crate::version::v0_0_1::pattern::{AddressKindPath, AddressKindPattern, Hop};
            use crate::version::v0_0_1::payload::{MapPattern, Primitive, PrimitiveList, PrimitiveType};
            use crate::version::v0_0_1::resource::ResourceStub;
            use crate::version::v0_0_1::util::ConvertFrom;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum SelectIntoPayload {
                Stubs,
            }

            impl SelectIntoPayload {
                pub fn to_primitive(
                    &self,
                    stubs: Vec<ResourceStub>,
                ) -> Result<PrimitiveList, Error> {
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
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Select {
                pub pattern: AddressKindPattern,
                pub properties: PropertiesPattern,
                pub into_payload: SelectIntoPayload,
                pub kind: SelectKind
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

            impl TryInto<SubSelect> for Select{
                type Error = Error;

                fn try_into(self) -> Result<SubSelect, Self::Error> {
                    if let SelectKind::SubSelect { address, hops, address_kind_path } = self.kind {
                        Ok(SubSelect {
                            address,
                            pattern: self.pattern,
                            properties: self.properties,
                            into_payload: self.into_payload,
                            hops: hops,
                            address_kind_path
                        } )
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
                ) -> SubSelect
                {
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

            use crate::error::Error;
            use crate::version::v0_0_1::command::common::SetProperties;
            use crate::version::v0_0_1::id::Address;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Update {
                pub address: Address,
                pub properties: SetProperties,
            }

        }

        pub mod query {
            use std::convert::TryInto;

            use serde::{Deserialize, Serialize};

            use crate::error::Error;
            use crate::version::v0_0_1::entity::request::RcCommand;
            use crate::version::v0_0_1::pattern::AddressKindPath;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Query {
                AddressKindPath,
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum QueryResult {
                AddressKindPath(AddressKindPath),
            }

            impl TryInto<AddressKindPath>
            for QueryResult
            {
                type Error = Error;

                fn try_into(self) -> Result<AddressKindPath, Error> {
                    match self {
                        QueryResult::AddressKindPath(address_kind_path) => {
                            Ok(address_kind_path)
                        }
                    }
                }
            }

            impl ToString for QueryResult
            {
                fn to_string(&self) -> String {
                    match self {
                        QueryResult::AddressKindPath(address_kind_path) => {
                            address_kind_path.to_string()
                        }
                    }
                }
            }

            impl Into<RcCommand> for Query {
                fn into(self) -> RcCommand {
                    RcCommand::Query(self)
                }
            }
        }

        pub mod get {
            pub struct Get {}
        }
    }

    pub mod response {
        use crate::error::Error;
        use crate::version::v0_0_1::fail;
        use crate::version::v0_0_1::id::{Address, ResourceKind};
        use crate::version::v0_0_1::payload::Payload;
        use serde::{Serialize,Deserialize};

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum RespEntity {
            Ok(Payload),
            Fail(fail::Fail),
        }

        impl RespEntity {
            pub fn ok_or(self) -> Result<Payload, Error> {
                match self {
                    Self::Ok(payload) => Result::Ok(payload),
                    Self::Fail(fail) => Result::Err(fail.into()),
                }
            }
        }

    }
}

pub mod resource {
    use std::str::FromStr;

    use nom::branch::alt;
    use nom::bytes::complete::{is_a, tag};
    use nom::character::complete::{alpha1, digit1};
    use nom::combinator::{not, recognize};
    use nom::error::{ErrorKind, ParseError, VerboseError};
    use nom::sequence::{delimited, tuple};
    use nom::CompareResult::Incomplete;
    use serde::{Deserialize, Serialize};

    use crate::error::Error;
    use crate::version::v0_0_1::id::{Address, AddressAndKind, ResourceKind, ResourceType};
    use crate::version::v0_0_1::parse::{address, Res};
    use crate::version::v0_0_1::payload::{Payload, PayloadMap};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct StatusUpdate {
        pub from: Address,
        pub status: Status,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum Status {
        Unknown,                // initial status or when we status cannot be determined
        Pending,                // resource is now registered but not assigned to a host
        Assigning,              // resource is being assigned to at least one host
        Initializing(Progress), // assigned to a host and undergoing custom initialization...This resource can send requests but not receive requests.  The String gives a progress indication like 2/10 (step 2 of 10) or 7/? when the number of steps are not known.
        Ready,                  // ready to take requests
        Paused(Address), // can not receive requests (probably because it is waiting for some other resource to make updates)... String should be some form of meaningful identifier of which resource Paused this resource
        Resuming(Progress), // like Initializing but triggered after a pause is lifted, the resource may be doing something before it is ready to accept requests again.  String is a progress indication just like Initializing.
        Panic(String), // something is wrong... all requests are blocked and responses are cancelled. String is a hopefully  meaningful message describing why the Resource has Panic
        Done(Code), // this resource had a life span and has now completed succesfully it can no longer receive requests. String is a hopefully meaningful or useful Status message that is returned
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

    impl ToString for Status {
        fn to_string(&self) -> String {
            match self {
                Status::Unknown => "Unknown".to_string(),
                Status::Pending => "Pending".to_string(),
                Status::Assigning => "Assigning".to_string(),
                Status::Initializing(progress) => format!("Initializing({})", progress.to_string()),
                Status::Ready => "Ready".to_string(),
                Status::Paused(address) => format!("Paused({})", address.to_string()),
                Status::Resuming(progress) => format!("Resuming({})", progress.to_string()),
                Status::Panic(message) => format!("Panic('{}')", message),
                Status::Done(code) => format!("Done({})", code.to_string()),
            }
        }
    }

    pub fn delim_progress(input: &str) -> Result<Progress, Error> {
        let (_, (step, _, total)) =
            delimited(tag("("), tuple((digit1, tag("/"), digit1)), tag(")"))(input)?;
        let step = step.parse()?;
        let total = total.parse()?;
        Ok(Progress { step, total })
    }

    pub fn delim_address(input: &str) -> Result<Address, Error> {
        let (_, address) = delimited(tag("("), address, tag(")"))(input)?;
        Ok(address)
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
                    return Err(nom::Err::Error(VerboseError::from_error_kind(
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

    pub fn delim_code(input: &str) -> Result<Code, Error> {
        let (_, code) = delimited(tag("("), code, tag(")"))(input)?;
        Ok(code)
    }

    pub fn delim_panic(input: &str) -> Result<String, Error> {
        let (_, panic) = delimited(tag("("), recognize(not(is_a(")"))), tag(")"))(input)?;
        Ok(panic.to_string())
    }

    pub fn status(input: &str) -> Result<Status, Error> {
        let (next, status) = alpha1(input)?;
        Ok(match status {
            "Unknown" => Status::Unknown,
            "Pending" => Status::Pending,
            "Assigning" => Status::Assigning,
            "Initializing" => Status::Initializing(delim_progress(next)?),
            "Ready" => Status::Ready,
            "Paused" => Status::Paused(delim_address(next)?),
            "Resuming" => Status::Resuming(delim_progress(next)?),
            "Panic" => Status::Panic(delim_panic(next)?),
            "Done" => Status::Done(delim_code(next)?),
            what => {
                return Err(format!("unknown status {}", what).into());
            }
        })
    }

    impl FromStr for Status {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(status(s)?)
        }
    }


    pub type Properties = PayloadMap;

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
                kind: self.kind
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
    use std::ops::Deref;
    use crate::version::v0_0_1::util::unique_id;
    use serde::{Serialize,Deserialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Exchanger<T>{
        pub id: String,
        pub item: T
    }

    impl <T> Exchanger<T> {
        pub fn new( item: T ) -> Self {
            Exchanger{
                id: unique_id(),
                item
            }
        }

        pub fn with<X>(self, item: X ) -> Exchanger<X>{
            Exchanger{
                id: self.id,
                item
            }
        }
    }

    impl <T> Deref for Exchanger<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.item
        }
    }

    pub mod inlet {
        use std::convert::TryFrom;
        use std::ops::Deref;

        use crate::error::Error;
        use crate::version::v0_0_1::artifact::ArtifactRequest;
        use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
        use crate::version::v0_0_1::id::{Address, ResourceKind, ResourceType};
        use crate::version::v0_0_1::log::Log;
        use crate::version::v0_0_1::messaging::{Request, Response};
        use crate::version::v0_0_1::pattern::TksPattern;
        use crate::version::v0_0_1::payload::Payload;
        use crate::version::v0_0_1::portal;
        use crate::version::v0_0_1::portal::Exchanger;
        use crate::version::v0_0_1::resource::StatusUpdate;
        use crate::version::v0_0_1::util::unique_id;
        use serde::{Serialize,Deserialize};


        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum Frame {
            Log(Log),
            AssignRequest(Exchanger<AssignRequest>),
            Request(Request),
            Response(Response),
            Artifact(Exchanger<ArtifactRequest>), // portal inlet will cache and return artifact
            Config(Exchanger<ArtifactRequest>), // portal inlet will cache, parse and return artifact config
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
                    Frame::Artifact(artifact) => Option::Some(artifact.from.clone()),
                    Frame::Config(config) => Option::Some(config.from.clone()),
                    Frame::Status(status) => Option::Some(status.from.clone()),
                    Frame::Close(_) => Option::None,
                    Frame::AssignRequest(_) => Option::None
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display, strum_macros::EnumString)]
        pub enum AssignRequest {
            Control
        }

        impl TryInto<PrimitiveFrame>
        for Frame
        {
            type Error = Error;

            fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                Ok(PrimitiveFrame {
                    data: bincode::serialize(&self)?,
                })
            }
        }

        impl TryFrom<PrimitiveFrame> for portal::inlet::Frame {
            type Error = Error;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }

    pub mod outlet {
        use std::convert::TryFrom;

        use crate::error::Error;
        use crate::version::v0_0_1::artifact::{Artifact, ArtifactResponse};
        use crate::version::v0_0_1::config::{Assign, Config, ConfigBody};
        use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
        use crate::version::v0_0_1::id::{Address, ResourceKind, ResourceType};
        use crate::version::v0_0_1::messaging::{Request, Response};
        use crate::version::v0_0_1::payload::Payload;
        use crate::version::v0_0_1::portal::Exchanger;
        use serde::{Serialize,Deserialize};

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum Frame {
            Assign(Exchanger<Assign>),
            Request(Request),
            Response(Response),
            Artifact(Exchanger<ArtifactResponse<Artifact>>),
            Config(Exchanger<ArtifactResponse<Config<ConfigBody>>>),
            Close(CloseReason),
        }

        impl Frame {
            pub fn to(&self) -> Option<Address> {
                match self {
                    Frame::Assign(assign) => Option::Some(assign.stub.address.clone()),
                    Frame::Request(request) => Option::Some(request.to.clone()),
                    Frame::Response(response) => Option::Some(response.to.clone()),
                    Frame::Artifact(artifact) => Option::Some(artifact.to.clone()),
                    Frame::Config(config) => Option::Some(config.to.clone()),
                    Frame::Close(_) => Option::None,
                }
            }
        }


        impl TryInto<PrimitiveFrame>
        for Frame
        {
            type Error = Error;

            fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                Ok(PrimitiveFrame {
                    data: bincode::serialize(&self)?,
                })
            }
        }



        impl TryFrom<PrimitiveFrame> for Frame {
            type Error = Error;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }
}


pub mod util {
    use serde::{Deserialize, Serialize};
    use uuid::Uuid;

    use crate::error::Error;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum ValuePattern<T> {
        Any,
        None,
        Pattern(T),
    }

    impl<T> ValuePattern<T> {
        pub fn is_match<X>(&self, x: &X) -> Result<(), Error>
        where
            T: ValueMatcher<X>,
        {
            match self {
                ValuePattern::Any => Ok(()),
                ValuePattern::Pattern(exact) => exact.is_match(x),
                ValuePattern::None => Err("None pattern".into()),
            }
        }

        pub fn is_match_opt<X>(&self, x: Option<&X>) -> Result<(), Error>
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
        fn is_match(&self, x: &X) -> Result<(), Error>;
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
        fn is_match(&self, x: &String) -> Result<(), Error> {
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
        fn is_match(&self, x: &String) -> Result<(), Error> {
            if self.pattern == *x {
                Ok(())
            } else {
                Err(format!("expecting pattern: '{}' found: '{}'", self.pattern, x).into())
            }
        }
    }

    pub trait Convert<A> {
        fn convert(self) -> Result<A, Error>;
    }

    pub trait ConvertFrom<A>
    where
        Self: Sized,
    {
        fn convert_from(a: A) -> Result<Self, Error>;
    }

    /*
    impl<A: Convert<B>, B> ConvertFrom<B> for A where A: Sized {
        fn convert_from(a: A) -> Result<Self, Error> {
            a.convert()
        }
    }
     */

    pub fn unique_id() -> String {
        Uuid::new_v4().to_string()
    }
}

pub mod fail {
    use serde::{Deserialize, Serialize};

    use crate::error::Error;
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

    pub mod http {
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Error {
            pub code: u32,
            pub message: String,
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
        Error(String)
    }

    impl ToString for Fail {
        fn to_string(&self) -> String {
            "Fail".to_string()
        }
    }

    impl Into<Error> for Fail {
        fn into(self) -> Error {
            Error {
                message: "Fail".to_string(),
            }
        }
    }
}

pub mod parse {
    use std::str::FromStr;

    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::bytes::complete::{is_a, is_not};
    use nom::character::complete::{alpha0, alphanumeric1, digit1, alpha1, multispace1,multispace0, space1,space0};
    use nom::combinator::{all_consuming, not, opt, recognize};
    use nom::error::{context, ErrorKind, VerboseError, ParseError};
    use nom::multi::{many0, separated_list1};
    use nom::sequence::{delimited, preceded, terminated, tuple};
    use nom::{AsChar, IResult, InputTakeAtPosition};
    use nom_supreme::parse_from_str;

    use crate::error::Error;
    use crate::version::v0_0_1::id::{Address, AddressSegment, RouteSegment, Version};
    use crate::version::v0_0_1::pattern::parse::{address_kind_pattern, delim_kind, kind, resource_type, specific, specific_pattern, version};
    use nom::bytes::complete::take;
    use crate::version::v0_0_1::entity::request::create::{Create, AddressSegmentTemplate, AddressTemplate, KindTemplate, Template, Strategy, AddressTemplateSegment, CreateOp, Require};
    use crate::version::v0_0_1::command::common::{PropertyMod, SetProperties, StateSrc};
    use crate::version::v0_0_1::config::bind::parse::pipeline_step;
    use crate::version::v0_0_1::entity::request::select::{Select, SelectIntoPayload, SelectKind};
    use crate::version::v0_0_1::pattern::{AddressKindPath, AddressKindSegment, skewer, upload_step};
    use crate::version::v0_0_1::util::StringMatcher;

    pub struct Parser {}

    impl Parser {
        pub fn address(input: &str) -> Res<&str, Address> {
            address(input)
        }

        pub fn consume_address(input: &str) -> Result<Address, Error> {
            let (_, address) = all_consuming(address)(input)?;
            Ok(address)
        }
    }

    pub type Res<I, O> = IResult<I, O, VerboseError<I>>;

    fn any_resource_path_segment<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
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
        T: InputTakeAtPosition,
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

    pub fn resource_route_segment(input: &str) -> Res<&str, RouteSegment> {
        not(alt((domain_route_segment, tag_route_segment)))(input)
            .map(|(next, _)| (next, RouteSegment::Resource))
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

    pub fn route_segment(input: &str) -> Res<&str, RouteSegment> {
        alt((
            tag_route_segment,
            domain_route_segment,
            mesh_route_segment,
            resource_route_segment,
        ))(input)
    }

    pub fn space_address_segment(input: &str) -> Res<&str, AddressSegment> {
        space_chars(input).map(|(next, space)| (next, AddressSegment::Space(space.to_string())))
    }

    pub fn base_address_segment(input: &str) -> Res<&str, AddressSegment> {
        preceded(tag(":"),rec_skewer)(input).map(|(next, base)| (next, AddressSegment::Base(base.to_string())))
    }


    pub fn filesystem_address_segment(input: &str) -> Res<&str, AddressSegment> {
        alt((dir_address_segment,file_address_segment ))(input)
    }

    pub fn dir_address_segment(input: &str) -> Res<&str, AddressSegment> {
        context("dir_address_segment",terminated(file_chars, tag("/")))(input)
            .map(|(next, dir)| (next, AddressSegment::Dir(format!("{}/",dir))))
    }

    pub fn root_dir_address_segment(input: &str) -> Res<&str, AddressSegment> {
        tag(":/")(input).map( |(next,_)| {
            (next,AddressSegment::FilesystemRootDir)
        })
    }

    pub fn file_address_segment(input: &str) -> Res<&str, AddressSegment> {
        context("file_address_segment", file_chars)(input).map(|(next, filename)| (next, AddressSegment::File(filename.to_string())))
    }

    pub fn version_address_segment(input: &str) -> Res<&str, AddressSegment> {
        preceded( tag(":"),version)(input).map(|(next, version)| (next, AddressSegment::Version(version)))
    }

    pub fn root_address(input: &str) -> Res<&str, Address> {
        tuple( (route_segment, tag("[root]")))(input).map( |(next,(route,_))| {
            let address = Address {
                route,
                segments: vec![]
            };
            (next, address)
        } )
    }
    pub fn regular_address(input: &str) -> Res<&str, Address> {
                 tuple((
                     tuple((route_segment, space_address_segment)),
                     many0(base_address_segment),
                     opt(version_address_segment),
                     opt( root_dir_address_segment ),
                     many0(filesystem_address_segment),
                 ))(input)
            .map(|(next, ((hub, space), mut bases, version, root, mut files))| {
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
            })
    }

    pub fn address(input: &str) -> Res<&str, Address> {
        alt((root_address,regular_address) )(input)
    }

    pub fn consume_address(input: &str) -> Res<&str, Address> {
        all_consuming(address)(input)
    }

    pub fn space_address_kind_segment(
        input: &str,
    ) -> Res<&str, AddressKindSegment>
    {
        tuple((space_address_segment, delim_kind))(input).map(
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

    pub fn base_address_kind_segment(
        input: &str,
    ) -> Res<&str, AddressKindSegment>
    {
        tuple((base_address_segment, delim_kind))(input).map(
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

    pub fn filepath_address_kind_segment(
        input: &str,
    ) -> Res<&str, AddressKindSegment>
    {
        alt((
            file_address_kind_segment,
            dir_address_kind_segment,
        ))(input)
    }
    pub fn dir_address_kind_segment(
        input: &str,
    ) -> Res<&str, AddressKindSegment>
    {
        tuple((dir_address_segment, delim_kind))(input).map(
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

    pub fn file_address_kind_segment(
        input: &str,
    ) -> Res<&str, AddressKindSegment>
    {
        tuple((file_address_segment, delim_kind))(input).map(
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

    pub fn version_address_kind_segment(
        input: &str,
    ) -> Res<&str, AddressKindSegment>
    {
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

    pub fn consume_address_kind_path(
        input: &str,
    ) -> Result<AddressKindPath, Error>
    {
        let (_, rtn) = all_consuming(address_kind_path)(input)?;
        Ok(rtn)
    }

    pub fn address_kind_path(
        input: &str,
    ) -> Res<&str, AddressKindPath>
    {
        tuple((
            tuple((
                route_segment,
                space_address_kind_segment
            )),
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
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        input.split_at_position_complete(|item| item.as_char() != '*')
    }

    pub fn upper<T, E: nom::error::ParseError<T>>(input: T) -> IResult<T, T, E>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        input.split_at_position_complete(|item| {
            let char_item = item.as_char();

            !char_item.is_uppercase()
        })
    }

    /*    fn any_resource_path_segment<T>(i: T) -> Res<T, T>
           where
               T: InputTakeAtPosition,
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
        T: InputTakeAtPosition,
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

    pub fn domain<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
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
        T: InputTakeAtPosition,
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
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                char_item != '.' &&
                char_item != '-' &&
                    !char_item.is_digit(10) &&
                    !(char_item.is_alpha() && char_item.is_lowercase())
            },
            ErrorKind::AlphaNumeric,
        )
    }


    pub fn version_req_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
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
            T: InputTakeAtPosition,
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

    pub fn rec_skewer(input: &str)->Res<&str,&str> {
            recognize(tuple((
                lowercase1,
                opt(skewer_chars),
            )))(input)
    }

    pub fn skewer_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                char_item != '-' &&
                    !char_item.is_digit(10) &&
                    !(char_item.is_alpha() && char_item.is_lowercase())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn skewer_chars_template<T>(i: T) -> Res<T, T>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                char_item != '-' &&
                    char_item.as_char() != '%' &&
                    !char_item.is_digit(10) &&
                    !(char_item.is_alpha() && char_item.is_lowercase())
            },
            ErrorKind::AlphaNumeric,
        )
    }

    pub fn space_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
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

    pub fn domain_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
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
        T: InputTakeAtPosition,
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

    pub fn file_chars<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
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
            T: InputTakeAtPosition,
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

    pub fn consume_path(input: &str) -> Res<&str, &str> {
        all_consuming(path)(input)
    }

    pub fn path_regex(input: &str) -> Res<&str, &str> {
        recognize(tuple((tag("/"), opt(not_space))))(input)
    }
    pub fn camel_case(input: &str) -> Res<&str, &str> {
        recognize(tuple((
            is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"),
            alpha0,
        )))(input)
        //recognize(alpha1)(input)
    }

    pub fn camel_case_to_string(input: &str) -> Res<&str, StringMatcher> {
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
        preceded(tag(":"),recognize(tuple((many0(skewer),tag("%"),many0(skewer)))))(input).map(|(next, base)| (next, AddressTemplateSegment::Wildcard(base.to_string())))
    }

    pub fn base_address_segment_template(input: &str) -> Res<&str, AddressTemplateSegment> {
        preceded(tag(":"),rec_skewer)(input).map(|(next, base)| (next, AddressTemplateSegment::AddressSegment(AddressSegment::Base(base.to_string()))))
    }

    pub fn filepath_address_segment_wildcard(input: &str) -> Res<&str, AddressTemplateSegment> {
        recognize(tuple((many0(filepath_chars),tag("%"),many0(filepath_chars))))(input).map(|(next, base)| (next, AddressTemplateSegment::Wildcard(base.to_string())))
    }

    pub fn filepath_address_segment_template(input: &str) -> Res<&str, AddressTemplateSegment> {
        filesystem_address_segment(input).map(|(next, segment)| (next, AddressTemplateSegment::AddressSegment(segment)))
    }

    pub fn address_template(input: &str) -> Res<&str, AddressTemplate> {
        let (next, ((hub, space), mut bases, version, root, mut files)) = tuple((
            tuple((route_segment, space_address_segment)),
            many0(alt((base_address_segment_wildcard,base_address_segment_template))),
            opt(version_address_segment),
            opt( root_dir_address_segment ),
            many0(alt( (filepath_address_segment_wildcard,filepath_address_segment_template))),
        ))(input)?;

        let mut base_wildcard = false;
        for (index,segment) in bases.iter().enumerate() {
            if segment.is_wildcard() {
                if  index != bases.len()-1 {
                    return Err(nom::Err::Error(VerboseError::from_error_kind( input, ErrorKind::Tag, )))
                } else {
                    base_wildcard = true;
                }
            }
        }

        if base_wildcard && version.is_some() {
            return Err(nom::Err::Error(VerboseError::from_error_kind( input, ErrorKind::Tag, )))
        }

        if base_wildcard && root.is_some() {
            return Err(nom::Err::Error(VerboseError::from_error_kind( input, ErrorKind::Tag, )))
        }

        let mut files_wildcard = false;
        for (index,segment) in files.iter().enumerate() {
            if segment.is_wildcard() {
                if  index != files.len()-1 {
                    return Err(nom::Err::Error(VerboseError::from_error_kind( input, ErrorKind::Tag, )))
                } else {
                    files_wildcard = true;
                }
            }
        }

        let mut space_last = false;
        let last = if !files.is_empty()  {
            match files.remove(files.len()-1 ) {
                AddressTemplateSegment::AddressSegment(exact) => {AddressSegmentTemplate::Exact(exact.to_string())}
                AddressTemplateSegment::Wildcard(pattern) => {AddressSegmentTemplate::Pattern(pattern)}
            }
        } else if root.is_some() {
            AddressSegmentTemplate::Exact("/".to_string())
        } else if let Option::Some(version) = &version  {
            AddressSegmentTemplate::Exact(version.to_string())
        } else if !bases.is_empty() {
            match bases.remove(bases.len()-1 ) {
                AddressTemplateSegment::AddressSegment(exact) => {AddressSegmentTemplate::Exact(exact.to_string())}
                AddressTemplateSegment::Wildcard(pattern) => {AddressSegmentTemplate::Pattern(pattern)}
            }
        } else {
            space_last = true;
            AddressSegmentTemplate::Exact(space.to_string())
        };

        let mut bases:Vec<AddressSegment> = bases.into_iter().map( |b| match b {
            AddressTemplateSegment::AddressSegment(seg) => {seg}
            AddressTemplateSegment::Wildcard(_) => {panic!("should have filtered wildcards already!")}
        } ).collect();

        let mut files:Vec<AddressSegment> = files.into_iter().map( |b| match b {
            AddressTemplateSegment::AddressSegment(seg) => {seg}
            AddressTemplateSegment::Wildcard(_) => {panic!("should have filtered wildcards already!")}
        } ).collect();


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
                    child_segment_template: last
                };

                Ok((next, address_template))
    }

    pub fn kind_template(input: &str) -> Res<&str, KindTemplate> {
        tuple((
            resource_type,
            opt(delimited(
                tag("<"),
                tuple((camel_case, opt(delimited(tag("<"), specific_pattern, tag(">"))))),
                tag(">"),
            )),
        ))(input)
            .map(|(next, (resource_type, more))| {
                let mut parts = KindTemplate{
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
        tuple((address_template,delimited(tag("<"),kind_template, tag(">"))))(input).map( |(next, (address,kind ))| {
            (next, Template {
                address,
                kind
            })
        } )
    }

    pub fn set_property_mod(input: &str) -> Res<&str, PropertyMod> {
        tuple((tag("+"),skewer,tag("="),property_value))(input).map( |(next,(_,name,_,value))| {
            (next,
            PropertyMod::Set{name: name.to_string(),value: value.to_string()})
        })
    }

    pub fn property_value_not_space(input: &str) -> Res<&str,&str> {
        not_space(input)
    }

    pub fn property_value_single_quotes(input: &str) -> Res<&str,&str> {
        delimited(tag("'"),is_not("'"), tag("'"))(input)
    }

    pub fn property_value_double_quotes(input: &str) -> Res<&str,&str> {
        delimited(tag("\""),is_not("\""), tag("\""))(input)
    }

    pub fn property_value(input: &str) -> Res<&str,&str> {
        alt( (property_value_single_quotes,property_value_double_quotes,property_value_not_space) )(input)
    }

    pub fn unset_property_mod(input: &str) -> Res<&str, PropertyMod> {
        tuple((tag("!"),skewer))(input).map( |(next,(_,name))| {
            (next, PropertyMod::UnSet(name.to_string()))
        })
    }

    pub fn property_mod(input: &str) -> Res<&str, PropertyMod> {
        alt( (set_property_mod,unset_property_mod) )(input)
    }

    pub fn set_properties(input: &str) -> Res<&str, SetProperties> {
        many0(tuple((multispace0,property_mod,multispace1)))(input).map( |(next, properties)| {
            let properties = properties.into_iter().map( |(_,p,_)|p ).collect();
            (next, properties)
        } )
    }

    pub fn create(input: &str) -> Res<&str, Create> {
        tuple((template,opt(delimited(tag("{"),set_properties, tag("}")))))(input).map( |(next, (template, properties))| {
            let properties = match properties {
                Some( properties ) => properties,
                None => SetProperties::new()
            };
            let create = Create {
                template,
                state: StateSrc::Stateless,
                properties,
                strategy: Strategy::Create,
                registry: Default::default()
            };
            (next, create)
        } )
    }

    pub fn select(input: &str) -> Res<&str, Select> {
        address_kind_pattern(input).map( |(next, address_kind_pattern)| {
            let select = Select {
                pattern: address_kind_pattern,
                properties: Default::default(),
                into_payload: SelectIntoPayload::Stubs,
                kind: SelectKind::Initial
            };
            (next,select)
        } )
    }

    pub fn publish(input: &str) -> Res<&str, CreateOp> {
        let (next, (upload,_,address)) = tuple( (upload_step,space1,address) )(input)?;

        let parent = match address.parent() {
            None => {
                return Err(nom::Err::Error(VerboseError::from_error_kind( input, ErrorKind::Tag, )));
            }
            Some(parent) => {parent}
        };

        let last = match address.last_segment() {
            None => {
                return Err(nom::Err::Error(VerboseError::from_error_kind( input, ErrorKind::Tag, )));
            }
            Some(last) => {last}
        };

        let template = Template {
            address: AddressTemplate{
                parent,
                child_segment_template: AddressSegmentTemplate::Exact(last.to_string())
            },
            kind: KindTemplate {
                resource_type: "ArtifactBundle".to_string(),
                kind: None,
                specific: None
            }
        };

        let create = CreateOp {
            template,
            state: StateSrc::Stateless,
            properties: Default::default(),
            strategy: Strategy::Create,
            registry: Default::default(),
            requirements: vec![Require::File(upload.name)]
        };

        Ok((next,create))
    }


}


#[cfg(test)]
pub mod test {
    use std::str::FromStr;

    use nom::combinator::{all_consuming, recognize};

    use crate::error::Error;
    use crate::version::v0_0_1::id::{AddressSegment, RouteSegment};
    use crate::version::v0_0_1::parse::{address, camel_case, route_segment, version_address_segment, skewer_chars, base_address_segment, rec_skewer, address_template, create, publish};
    use nom::Err;
    use nom::error::VerboseError;
    use crate::version::v0_0_1::config::bind::parse::{pipeline, pipeline_step};
    use crate::version::v0_0_1::pattern::parse::version;
    use crate::version::v0_0_1::pattern::parse::address_kind_pattern;
    use crate::version::v0_0_1::pattern::upload_step;

    #[test]
    pub fn test_skewer_chars() -> Result<(),Error> {
        match all_consuming(rec_skewer)("317"){
            Ok(ok) => {
                return Err("should not have parsed 317".into());
            }
            Err(_) => {}
        }
        assert_eq!( rec_skewer("hello1"), Ok(("","hello1")) );
        assert_eq!( all_consuming(rec_skewer)("hello-kitty"), Ok(("","hello-kitty")) );
        assert_eq!( all_consuming(rec_skewer)("hello-kitty123"), Ok(("","hello-kitty123")) );
        assert_eq!( rec_skewer("hello-kitty.1.2.3"), Ok((".1.2.3","hello-kitty")) );
        assert_eq!( rec_skewer("skewer-takes-no-Caps"), Ok(("Caps","skewer-takes-no-")) );
        Ok(())
    }


   #[test]
    pub fn test_address () -> Result<(),Error> {
        assert_eq!(("",RouteSegment::Resource),all_consuming(route_segment)("")?);

         all_consuming(address)("[root]")?;
         all_consuming(address)("hello:kitty")?;
         all_consuming(address)("hello.com:kitty")?;
         all_consuming(address)("hello:kitty:/file.txt")?;
         all_consuming(address)("hello.com:kitty:/file.txt")?;
         all_consuming(address)("hello.com:kitty:/")?;
         //all_consuming(address)("hello.com:kitty:/greater-glory/file.txt")?;
         all_consuming(address)("hello.com:kitty:base")?;

       all_consuming(version)("1.0.0")?;
        let (next,version) = all_consuming(version_address_segment)(":1.2.3")?;
        println!("next: '{}' segment: '{}'", next, version.to_string() );
        all_consuming(address)("hello.com:bundle:1.2.3")?;
        let (next, addy) = all_consuming(address)("hello.com:bundle:1.2.3:/")?;
println!("{}", addy.last_segment().unwrap().to_string() );
       let (next, addy) = all_consuming(address)("hello.com:bundle:1.2.3")?;
//       let (next, addy) = all_consuming(address)("hello.com:bundle:1.2.3:/some/file.txt")?;
       let (next, addy) = all_consuming(address)("hello.com:bundle:1.2.3:/greater-glory/file.txt")?;
println!("{}", addy.to_string() );
println!("{}", addy.parent().unwrap().to_string() );
println!("{}", addy.last_segment().unwrap().to_string() );

       Ok(())
    }

    #[test]
    pub fn test_address_template () -> Result<(),Error> {
        all_consuming(address_template)("hello:kitty")?;
        all_consuming(address_template)("hello:kitty-%")?;
        all_consuming(address_template)("hello:kitty:bob:/some-%-time")?;
        Ok(())
    }


    #[test]
    pub fn test_create() -> Result<(),Error> {
        all_consuming(create)("hello:kitty<App>")?;
        all_consuming(create)("hello:kitty<App>{ +config='some:config:1.0.0:/blah.conf' }")?;
        Ok(())
    }

    #[test]
    pub fn test_pipeline() -> Result<(),Error> {
        let (_,block) = all_consuming(upload_step )("^[ bundle.zip ]->")?;
        assert_eq!( "bundle.zip", block.name.as_str() );
        all_consuming(publish)("^[ bundle.zip ]-> space.org:hello:1.0.0")?;
        Ok(())
    }

    #[test]
    pub fn test_address_kind_pattern() -> Result<(),Error> {
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
        Ok(())
    }


}
