use nom_locate::LocatedSpan;
use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

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
    use crate::version::v0_0_1::id::Point;
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
    use crate::version::v0_0_1::parse::{BruteResolver, camel_case, consume_point_subst, NoResolver, point_subst, point_segment, Res, Resolver, route_segment,  Subst, ToResolved};
    use crate::version::v0_0_1::selector::parse::{
        generic_kind_base, kind, point_and_kind, specific,
    };
    use crate::version::v0_0_1::selector::{Pattern, PointSelector, SpecificSelector, VersionReq};
    use crate::version::v0_0_1::Span;

    pub type GenericKindBase = String;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct PointKind {
        pub point: Point,
        pub kind: GenericKind,
    }

    impl PointKind {
        pub fn new(point: Point, kind: GenericKind) -> Self {
            Self { point, kind }
        }
    }

    impl ToString for PointKind {
        fn to_string(&self) -> String {
            format!("{}<{}>", self.point.to_string(), self.kind.to_string())
        }
    }

    impl FromStr for PointKind {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let point_and_kind = match all_consuming(point_and_kind)(Span::new(s)) {
                Ok((_, point_and_kind)) => point_and_kind,
                Err(err) => {
                    return Err("could not parse PointKind".into());
                }
            };
            Ok(point_and_kind)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct AddressAndType {
        pub point: Point,
        pub resource_type: GenericKindBase,
    }

    pub type Meta = HashMap<String, String>;
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
        fn resource_type(&self) -> GenericKindBase;
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

    impl TryInto<SpecificSelector> for Specific {
        type Error = MsgErr;

        fn try_into(self) -> Result<SpecificSelector, Self::Error> {
            Ok(SpecificSelector {
                vendor: Pattern::Exact(self.vendor),
                product: Pattern::Exact(self.product),
                variant: Pattern::Exact(self.variant),
                version: VersionReq::from_str(self.version.to_string().as_str())?,
            })
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum RouteSeg {
        Local,
        Domain(String),
        Tag(String),
        Mesh(String),
    }

    impl FromStr for RouteSeg {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let s = Span::new(s);
            Ok(all_consuming(route_segment)(s)?.1)
        }
    }

    impl ToString for RouteSeg {
        fn to_string(&self) -> String {
            match self {
                RouteSeg::Local => ".".to_string(),
                RouteSeg::Domain(domain) => domain.clone(),
                RouteSeg::Tag(tag) => {
                    format!("[{}]", tag)
                }
                RouteSeg::Mesh(mesh) => {
                    format!("<<{}>>", mesh)
                }
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum PointSegKind {
        Root,
        Space,
        Base,
        FilesystemRootDir,
        Dir,
        File,
        Version,
        PopSeg,
        PopDir
    }

    impl PointSegKind {
        pub fn terminating_delim(&self) -> &str {
            match self {
                Self::Space => ":",
                Self::Base => ":",
                Self::Dir => "",
                Self::File => "",
                Self::Version => ":",
                Self::FilesystemRootDir => "",
                Self::Root => "",
                Self::PopSeg => ":",
                Self::PopDir => "/",
            }
        }

        pub fn is_normalized(&self) -> bool {
            match self {
                Self::PopSeg => false,
                Self::PopDir => false,
                _ => true
            }
        }
    }


    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum PointSeg {
        Root,
        Space(String),
        Base(String),
        FilesystemRootDir,
        Dir(String),
        File(String),
        Version(Version),
        PopSeg,
        PopDir
    }

    impl PointSeg {
        pub fn kind(&self) -> PointSegKind {
            match self {
                PointSeg::Root => PointSegKind::Root,
                PointSeg::Space(_) => PointSegKind::Space,
                PointSeg::Base(_) => PointSegKind::Base,
                PointSeg::FilesystemRootDir => PointSegKind::FilesystemRootDir,
                PointSeg::Dir(_) => PointSegKind::Dir,
                PointSeg::File(_) => PointSegKind::File,
                PointSeg::Version(_) => PointSegKind::Version,
                PointSeg::PopSeg => PointSegKind::PopSeg,
                PointSeg::PopDir => PointSegKind::PopDir
            }
        }

        pub fn is_normalized(&self) -> bool {
            self.kind().is_normalized()
        }
    }

    // It's not a good idea to do from_str on a PointSeg since they are actually different based on their position in the Point
    impl FromStr for PointSeg {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let s = Span::new(s);
            Ok(all_consuming(point_segment )(s)?.1)
        }
    }

    impl PointSeg {
        pub fn apply_captures(self, captures: &Captures) -> Result<Self, MsgErr> {
            match self {
                PointSeg::Root => Ok(PointSeg::Root),
                PointSeg::Space(replacement) => {
                    let mut dst = String::new();
                    captures.expand(replacement.as_str(), &mut dst);
                    Ok(PointSeg::Space(dst))
                }
                PointSeg::Base(replacement) => {
                    let mut dst = String::new();
                    captures.expand(replacement.as_str(), &mut dst);
                    Ok(PointSeg::Base(dst))
                }
                PointSeg::FilesystemRootDir => Ok(PointSeg::FilesystemRootDir),
                PointSeg::Dir(replacement) => {
                    let mut dst = String::new();
                    captures.expand(replacement.as_str(), &mut dst);
                    Ok(PointSeg::Dir(dst))
                }
                PointSeg::File(replacement) => {
                    let mut dst = String::new();
                    captures.expand(replacement.as_str(), &mut dst);
                    Ok(PointSeg::File(dst))
                }
                PointSeg::Version(version) => Ok(PointSeg::Version(version)),
                PointSeg::PopSeg => {unimplemented!()}
                PointSeg::PopDir => {unimplemented!()}
            }
        }


        pub fn is_version(&self) -> bool {
            match self {
                PointSeg::Version(_) => true,
                _ => false,
            }
        }

        pub fn is_filepath(&self) -> bool {
            match self {
                PointSeg::Dir(_) => true,
                PointSeg::FilesystemRootDir => true,
                PointSeg::File(_) => true,
                _ => false,
            }
        }

        pub fn is_file(&self) -> bool {
            match self {
                PointSeg::File(_) => true,
                _ => false,
            }
        }

        pub fn is_dir(&self) -> bool {
            match self {
                PointSeg::Dir(_) => true,
                PointSeg::FilesystemRootDir => true,
                _ => false,
            }
        }

        pub fn terminating_delim(&self) -> &str {
            self.kind().terminating_delim()
        }

        pub fn is_filesystem_ref(&self) -> bool {
            match self {
                PointSeg::Space(_) => false,
                PointSeg::Base(_) => false,
                PointSeg::Dir(_) => true,
                PointSeg::File(_) => true,
                PointSeg::Version(_) => false,
                PointSeg::FilesystemRootDir => true,
                PointSeg::Root => false,
                PointSeg::PopSeg => false,
                PointSeg::PopDir => true
            }
        }
    }

    impl ToString for PointSeg {
        fn to_string(&self) -> String {
            match self {
                PointSeg::Space(space) => space.clone(),
                PointSeg::Base(base) => base.clone(),
                PointSeg::Dir(dir) => dir.clone(),
                PointSeg::File(file) => file.clone(),
                PointSeg::Version(version) => version.to_string(),
                PointSeg::FilesystemRootDir => "/".to_string(),
                PointSeg::Root => "".to_string(),
                PointSeg::PopSeg => "..".to_string(),
                PointSeg::PopDir => "..".to_string()
            }
        }
    }

    pub type Point = PointDef<RouteSeg, PointSeg>;
    pub type PointSubst = PointDef<Subst<RouteSeg>, Subst<PointSeg>>;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct PointDef<Route, Seg> {
        pub route: Route,
        pub segments: Vec<Seg>,
    }

    impl BruteResolver<Point> for PointSubst {}

    impl ToResolved<Point> for PointSubst {
        fn to_resolved(self, resolver: &dyn Resolver) -> Result<Point, MsgErr> {
            let mut point = String::new();
            point.push_str(self.route.to_resolved_str(resolver)?.as_str() );
            point.push_str("::");

            for (index,segment) in self.segments.iter().enumerate() {
                point.push_str( segment.to_resolved_str(resolver)?.as_str() );

                if index < self.segments.len()-1 {
                    point.push_str(":" );
                }
            }
println!("created: {}", point);
            let point = Point::from_str(point.as_str())?;
            let point = point.normalize()?;
println!("normalized : {}", point.to_string());
            Ok(point)
        }
    }

    impl Point {

        pub fn normalize(self) -> Result<Point,MsgErr> {
            if self.is_normalized() {
                return Ok(self);
            }

            if !self.segments.first().expect("expected first segment").is_normalized() {
                return Err(format!("absolute point paths cannot begin with '..' (reference parent segment) because there is no working point segment: '{}'",self.to_string()).into())
            }

            let mut segments = self.segments.clone();
            for seg in self.segments {
                match seg.is_normalized() {
                    true => segments.push(seg),
                    false => {
                        if segments.pop().is_none() {
                            return Err(format!("'..' too many pop segments directives: out of parents: '{}'",self.to_string()).into());
                        }
                    }
                }

            }
            Ok(Point {
                route: self.route,
                segments
            })
        }

        pub fn is_normalized(&self) -> bool {
            for seg in &self.segments {
                if !seg.is_normalized() {
                    return false;
                }
            }
            true
        }

        pub fn to_bundle(self) -> Result<Point, MsgErr> {
            if self.segments.is_empty() {
                return Err("Address does not contain a bundle".into());
            }

            if let Some(PointSeg::Version(_)) = self.segments.last() {
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
                let point = match last {
                    PointSeg::Root => segment,
                    PointSeg::Space(_) => {
                        format!("{}:{}", self.to_string(), segment)
                    }
                    PointSeg::Base(_) => {
                        format!("{}:{}", self.to_string(), segment)
                    }
                    PointSeg::FilesystemRootDir => {
                        format!("{}{}", self.to_string(), segment)
                    }
                    PointSeg::Dir(_) => {
                        format!("{}{}", self.to_string(), segment)
                    }
                    PointSeg::Version(_) => {
                        if segment != "/" {
                            return Err(
                                "Root filesystem artifact dir required after version".into()
                            );
                        }
                        format!("{}:/", self.to_string())
                    }
                    PointSeg::File(_) => return Err("cannot append to a file".into()),
                };
                Self::from_str(point.as_str())
            }
        }

        pub fn push_file(&self, segment: String) -> Result<Self, MsgErr> {
            Self::from_str(format!("{}{}", self.to_string(), segment).as_str())
        }

        pub fn push_segment(&self, segment: PointSeg) -> Self {
            let mut point = self.clone();
            point.segments.push(segment);
            point
        }

        pub fn last_segment(&self) -> Option<PointSeg> {
            self.segments.last().cloned()
        }

        pub fn filepath(&self) -> Option<String> {
            let mut path = String::new();
            for segment in &self.segments {
                match segment {
                    PointSeg::FilesystemRootDir => {
                        path.push_str("/");
                    }
                    PointSeg::Dir(dir) => {
                        path.push_str(dir.as_str());
                    }
                    PointSeg::File(file) => {
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

    impl FromStr for Point {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let point = consume_point_subst(Span::new(s))?.1;
            let point: Point = point.brute_resolve()?;
            Ok(point)
        }
    }

    impl Into<String> for Point {
        fn into(self) -> String {
            self.to_string()
        }
    }

    impl TryInto<PointSelector> for Point {
        type Error = MsgErr;

        fn try_into(self) -> Result<PointSelector, Self::Error> {
            Ok(PointSelector::from_str(self.to_string().as_str())?)
        }
    }

    impl Point {
        pub fn to_full_string(&self) -> String {
            match self.route {
                RouteSeg::Local => {
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
                _ => self.to_string(),
            }
        }
    }

    impl ToString for Point {
        fn to_string(&self) -> String {
            let mut rtn = String::new();

            match &self.route {
                RouteSeg::Local => {}
                RouteSeg::Domain(domain) => {
                    rtn.push_str(format!("{}::", domain).as_str());
                }
                RouteSeg::Tag(tag) => {
                    rtn.push_str(format!("[{}]::", tag).as_str());
                }
                RouteSeg::Mesh(mesh) => {
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

    impl Point {
        pub fn parent(&self) -> Option<Point> {
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


        pub fn root() -> Self {
            Self {
                route: RouteSeg::Local,
                segments: vec![],
            }
        }

        pub fn root_with_route(route: RouteSeg) -> Self {
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
        pub route: RouteSeg,
        pub segments: Vec<PointSeg>,
    }

    impl CaptureAddress {
        pub fn to_point(self, captures: Captures) -> Result<Point, MsgErr> {
            let mut segments = vec![];
            for segment in self.segments {
                segments.push(segment.apply_captures(&captures)?)
            }
            let point = Point {
                route: self.route,
                segments,
            };

            // to make sure all the regex captures are removed...
            let point = Point::from_str(point.to_string().as_str())?;
            Ok(point)
        }
    }

    impl ToString for CaptureAddress {
        fn to_string(&self) -> String {
            let mut rtn = String::new();

            match &self.route {
                RouteSeg::Local => {}
                RouteSeg::Domain(domain) => {
                    rtn.push_str(format!("{}::", domain).as_str());
                }
                RouteSeg::Tag(tag) => {
                    rtn.push_str(format!("[{}]::", tag).as_str());
                }
                RouteSeg::Mesh(mesh) => {
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
    pub struct GenericKind {
        pub resource_type: GenericKindBase,
        pub kind: Option<String>,
        pub specific: Option<Specific>,
    }

    impl ToString for GenericKind {
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

    impl FromStr for GenericKind {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_, kind) = all_consuming(kind)(Span::new(s))?;

            Ok(kind)
        }
    }

    impl GenericKind {
        pub fn new(
            resource_type: GenericKindBase,
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

    impl Tks for GenericKind {
        fn resource_type(&self) -> GenericKindBase {
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
    use crate::version::v0_0_1::Span;
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
            let (_, path) = consume_path(Span::new(s))?;
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

pub mod selector {
    use std::convert::TryInto;
    use std::fmt::Formatter;
    use std::ops::Deref;
    use std::str::FromStr;

    use semver::ReqParseError;
    use serde::de::Visitor;
    use serde::{de, Deserializer, Serializer};

    use crate::error::MsgErr;

    use crate::version::v0_0_1::config::bind::parse::{many_until0, select_block, SelectBlock};
    use crate::version::v0_0_1::entity::request::{Action, Rc, RcCommandType, RequestCore};
    use crate::version::v0_0_1::id::{
        GenericKind, GenericKindBase, Point, PointSeg, RouteSeg, Specific, Tks, Version,
    };
    use crate::version::v0_0_1::parse::{camel_case, camel_case_to_string_matcher, capture_path, capture_point, consume_hierarchy, file_chars, path, path_regex, point, point_subst, Res};
    use crate::version::v0_0_1::payload::{
        Call, CallKind, CallWithConfig, HttpCall, HttpMethod, HttpMethodType, ListPattern,
        MapPattern, MsgCall, Payload, PayloadFormat, PayloadPattern, PayloadType,
        PayloadTypePattern, Primitive, PrimitiveType, Range,
    };
    use crate::version::v0_0_1::selector::parse::{pattern, point_selector, value_pattern};
    use crate::version::v0_0_1::selector::specific::{
        ProductSelector, VariantSelector, VendorSelector,
    };
    use crate::version::v0_0_1::util::{MethodPattern, StringMatcher, ValueMatcher, ValuePattern};
    use crate::version::v0_0_1::Span;
    use crate::{Deserialize, Serialize};
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0, one_of};
    use nom::combinator::{all_consuming, fail, opt, recognize};
    use nom::error::{context, ErrorKind, FromExternalError, ParseError, VerboseError};
    use nom::multi::separated_list0;
    use nom::sequence::{delimited, preceded, terminated, tuple};
    use nom::{AsChar, Compare, IResult, InputLength, InputTake, InputTakeAtPosition, Parser};
    use nom_locate::LocatedSpan;
    use nom_supreme::error::ErrorTree;
    use nom_supreme::parser_ext::FromStrParser;
    use nom_supreme::{parse_from_str, ParserExt};
    use regex::Regex;
    use std::collections::HashMap;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TksPattern {
        pub resource_type: GenericKindiBaseSelector,
        pub kind: GenericKindSelector,
        pub specific: ValuePattern<SpecificSelector>,
    }

    impl TksPattern {
        pub fn new(
            resource_type: GenericKindiBaseSelector,
            kind: GenericKindSelector,
            specific: ValuePattern<SpecificSelector>,
        ) -> Self {
            Self {
                resource_type,
                kind,
                specific,
            }
        }

        pub fn matches(&self, kind: &GenericKind) -> bool
        where
            GenericKind: Eq + PartialEq,
        {
            self.resource_type.matches(&kind.resource_type())
                && self.kind.matches(kind)
                && self.specific.is_match_opt(kind.specific().as_ref()).is_ok()
        }
    }

    impl ToString for TksPattern {
        fn to_string(&self) -> String {
            format!(
                "{}<{}<{}>>",
                self.resource_type.to_string(),
                self.kind.to_string(),
                self.specific.to_string()
            )
        }
    }

    impl TksPattern {
        pub fn any() -> Self {
            Self {
                resource_type: GenericKindiBaseSelector::Any,
                kind: GenericKindSelector::Any,
                specific: ValuePattern::Any,
            }
        }
    }

    pub type GenericKindSelector = Pattern<GenericKind>;
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PointSelector {
        pub hops: Vec<Hop>,
    }

    impl FromStr for PointSelector {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_, rtn) = all_consuming(point_selector)(Span::new(s))?;
            Ok(rtn)
        }
    }

    impl PointSelector {
        fn consume(&self) -> Option<PointSelector> {
            if self.hops.is_empty() {
                Option::None
            } else {
                let mut hops = self.hops.clone();
                hops.remove(0);
                Option::Some(PointSelector { hops })
            }
        }

        pub fn matches_root(&self) -> bool {
            if self.hops.is_empty() {
                true
            } else if self.hops.len() == 1 {
                let hop = self.hops.first().unwrap();
                if PointSegSelector::InclusiveAny == hop.segment
                    || PointSegSelector::InclusiveRecursive == hop.segment
                {
                    let resource_kind = GenericKind::new("Root".to_string(), None, None);
                    hop.tks.matches(&resource_kind)
                } else {
                    false
                }
            } else {
                false
            }
        }

        pub fn is_root(&self) -> bool {
            self.hops.is_empty()
        }

        pub fn is_final(&self) -> bool {
            self.hops.len() == 1
        }

        pub fn query_root(&self) -> Point {
            let mut segments = vec![];
            for hop in &self.hops {
                if let PointSegSelector::Exact(exact) = &hop.segment {
                    if hop.inclusive {
                        break;
                    }
                    match exact {
                        ExactPointSeg::PointSeg(seg) => {
                            segments.push(seg.clone());
                        }
                        ExactPointSeg::Version(version) => {
                            segments.push(PointSeg::Version(version.clone()));
                        }
                    }
                } else {
                    break;
                }
            }

            Point {
                route: RouteSeg::Local,
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

        pub fn matches(&self, hierarchy: &PointKindHierarchy) -> bool
        where
            GenericKindBase: Clone,
            GenericKind: Clone,
        {
            if hierarchy.is_root() && self.is_root() {
                return true;
            }

            if hierarchy.segments.is_empty() || self.hops.is_empty() {
                return false;
            }

            let hop = self.hops.first().expect("hop");
            let seg = hierarchy.segments.first().expect("segment");

            /*
                        if hierarchy.segments.len() < self.hops.len() {
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

            if hierarchy.is_final() && self.is_final() {
                // this is the final hop & segment if they match, everything matches!
                hop.matches(seg)
            } else if hierarchy.is_root() {
                false
            } else if self.is_root() {
                false
            } else if hierarchy.is_final() {
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
                        .expect("PointSelector")
                        .matches(&hierarchy.consume().expect("AddressKindPath"))
                } else {
                    // the NEXT hop does not match, therefore we do NOT consume() the current hop
                    self.matches(&hierarchy.consume().expect("AddressKindPath"))
                }
            } else if hop.segment.is_recursive() && hierarchy.is_final() {
                hop.matches(hierarchy.segments.last().expect("segment"))
            } else if hop.segment.is_recursive() {
                hop.matches(hierarchy.segments.last().expect("segment"))
                    && self.matches(&hierarchy.consume().expect("hierarchy"))
            } else if hop.matches(seg) {
                // in a normal match situation, we consume the hop and move to the next one
                self.consume()
                    .expect("AddressTksPattern")
                    .matches(&hierarchy.consume().expect("AddressKindPath"))
            } else {
                false
            }
        }
    }

    impl ToString for PointSelector {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            for (index, hop) in self.hops.iter().enumerate() {
                rtn.push_str(hop.to_string().as_str());
                if index < self.hops.len() - 1 {
                    rtn.push_str(":");
                }
            }
            rtn
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum PointSegSelector {
        InclusiveAny,       // +:*  // includes Root if it's the first segment
        InclusiveRecursive, // +:** // includes Root if its the first segment
        Any,                // *
        Recursive,          // **
        Exact(ExactPointSeg),
        Version(VersionReq),
    }

    impl PointSegSelector {
        pub fn is_exact(&self) -> bool {
            match self {
                PointSegSelector::Exact(_) => true,
                _ => false,
            }
        }

        pub fn matches(&self, segment: &PointSeg) -> bool {
            match self {
                PointSegSelector::InclusiveAny => true,
                PointSegSelector::InclusiveRecursive => true,
                PointSegSelector::Any => true,
                PointSegSelector::Recursive => true,
                PointSegSelector::Exact(exact) => match exact {
                    ExactPointSeg::PointSeg(pattern) => *pattern == *segment,
                    ExactPointSeg::Version(a) => {
                        if let PointSeg::Version(b) = segment {
                            *a == *b
                        } else {
                            false
                        }
                    }
                },
                PointSegSelector::Version(req) => {
                    if let PointSeg::Version(b) = segment {
                        req.matches(b)
                    } else {
                        false
                    }
                }
            }
        }

        pub fn is_recursive(&self) -> bool {
            match self {
                PointSegSelector::InclusiveAny => false,
                PointSegSelector::InclusiveRecursive => true,
                PointSegSelector::Any => false,
                PointSegSelector::Recursive => true,
                PointSegSelector::Exact(_) => false,
                PointSegSelector::Version(_) => false,
            }
        }
    }

    impl ToString for PointSegSelector {
        fn to_string(&self) -> String {
            match self {
                PointSegSelector::InclusiveAny => "+:*".to_string(),
                PointSegSelector::InclusiveRecursive => "+:**".to_string(),
                PointSegSelector::Any => "*".to_string(),
                PointSegSelector::Recursive => "**".to_string(),
                PointSegSelector::Exact(exact) => exact.to_string(),
                PointSegSelector::Version(version) => version.to_string(),
            }
        }
    }

    pub type KeySegment = String;

    #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
    pub enum ExactPointSeg {
        PointSeg(PointSeg),
        Version(Version),
    }

    impl ExactPointSeg {
        pub fn matches(&self, segment: &PointSeg) -> bool {
            match self {
                ExactPointSeg::PointSeg(s) => *s == *segment,
                ExactPointSeg::Version(a) => {
                    if let PointSeg::Version(b) = segment {
                        *a == *b
                    } else {
                        false
                    }
                }
            }
        }
    }

    impl ToString for ExactPointSeg {
        fn to_string(&self) -> String {
            match self {
                ExactPointSeg::PointSeg(point) => point.to_string(),
                ExactPointSeg::Version(version) => version.to_string(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SpecificSelector {
        pub vendor: VendorSelector,
        pub product: ProductSelector,
        pub variant: VariantSelector,
        pub version: VersionReq,
    }

    impl ValueMatcher<Specific> for SpecificSelector {
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

    impl ToString for SpecificSelector {
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
        use crate::version::v0_0_1::selector::Pattern;

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

        pub type VendorSelector = Pattern<String>;
        pub type ProductSelector = Pattern<String>;
        pub type VariantSelector = Pattern<String>;
        pub type VersionPattern = Pattern<VersionReq>;
    }

    pub mod parse {
        use std::convert::{TryFrom, TryInto};
        use std::str::FromStr;

        use nom::branch::alt;
        use nom::bytes::complete::tag;
        use nom::character::complete::{alpha1, digit1};
        use nom::combinator::{all_consuming, opt, recognize};
        use nom::error::{context, ContextError, ErrorKind, ParseError, VerboseError};
        use nom::multi::{many0, many1};
        use nom::sequence::{delimited, preceded, terminated, tuple};
        use nom::{Compare, InputIter, InputLength, InputTake, Parser, UnspecializedInput};
        use nom::{Err, IResult};

        use crate::error::MsgErr;
        use crate::version::v0_0_1::id::{
            GenericKind, GenericKindBase, PointKind, PointSeg, Specific, Version,
        };
        use crate::version::v0_0_1::parse::{camel_case, domain_chars, file_chars, point_subst, point_segment_chars, skewer_chars, version_chars, version_point_segment, version_req_chars, Res, point};
        use crate::version::v0_0_1::selector::specific::{
            ProductSelector, VariantSelector, VendorSelector,
        };
        use crate::version::v0_0_1::selector::{
            ExactPointSeg, GenericKindSelector, GenericKindiBaseSelector, Hop, Pattern,
            PointSegSelector, PointSelector, SpecificSelector, TksPattern, VersionReq,
        };
        use crate::version::v0_0_1::util::ValuePattern;
        use crate::version::v0_0_1::Span;
        use nom_supreme::error::ErrorTree;
        use nom_supreme::{parse_from_str, ParserExt};

        fn inclusive_any_segment(input: Span) -> Res<Span, PointSegSelector> {
            tag("+:*")(input).map(|(next, _)| (next, PointSegSelector::InclusiveAny))
        }

        fn inclusive_recursive_segment(input: Span) -> Res<Span, PointSegSelector> {
            tag("+:**")(input).map(|(next, _)| (next, PointSegSelector::InclusiveRecursive))
        }

        fn any_segment(input: Span) -> Res<Span, PointSegSelector> {
            tag("*")(input).map(|(next, _)| (next, PointSegSelector::Any))
        }

        fn recursive_segment(input: Span) -> Res<Span, PointSegSelector> {
            tag("**")(input).map(|(next, _)| (next, PointSegSelector::Recursive))
        }

        fn exact_space_segment(input: Span) -> Res<Span, PointSegSelector> {
            point_segment_chars(input).map(|(next, segment)| {
                (
                    next,
                    PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::Space(
                        segment.to_string(),
                    ))),
                )
            })
        }

        fn exact_base_segment(input: Span) -> Res<Span, PointSegSelector> {
            point_segment_chars(input).map(|(next, segment)| {
                (
                    next,
                    PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::Base(
                        segment.to_string(),
                    ))),
                )
            })
        }

        fn exact_file_segment(input: Span) -> Res<Span, PointSegSelector> {
            file_chars(input).map(|(next, segment)| {
                (
                    next,
                    PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::File(
                        segment.to_string(),
                    ))),
                )
            })
        }

        fn exact_dir_segment(input: Span) -> Res<Span, PointSegSelector> {
            file_chars(input).map(|(next, segment)| {
                (
                    next,
                    PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::Dir(
                        segment.to_string(),
                    ))),
                )
            })
        }

        pub fn parse_version_chars_str<O: FromStr>(input: Span) -> Res<Span, O> {
            let (next, rtn) = recognize(version_chars)(input)?;
            match O::from_str(rtn.to_string().as_str()) {
                Ok(rtn) => Ok((next, rtn)),
                Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
                    next,
                    ErrorKind::Fail,
                ))),
            }
        }

        fn exact_version_segment(input: Span) -> Res<Span, PointSegSelector> {
            version_req(input)
                .map(|(next, version_req)| (next, PointSegSelector::Version(version_req)))
        }

        fn version_req_segment(input: Span) -> Res<Span, PointSegSelector> {
            delimited(tag("("), version_req, tag(")"))(input)
                .map(|(next, version_req)| (next, PointSegSelector::Version(version_req)))
        }

        fn space_segment(input: Span) -> Res<Span, PointSegSelector> {
            alt((
                inclusive_recursive_segment,
                inclusive_any_segment,
                recursive_segment,
                any_segment,
                exact_space_segment,
            ))(input)
        }

        fn base_segment(input: Span) -> Res<Span, PointSegSelector> {
            alt((recursive_segment, any_segment, exact_base_segment))(input)
        }

        fn file_segment(input: Span) -> Res<Span, PointSegSelector> {
            alt((recursive_segment, any_segment, exact_file_segment))(input)
        }

        fn dir_segment(input: Span) -> Res<Span, PointSegSelector> {
            terminated(
                alt((recursive_segment, any_segment, exact_dir_segment)),
                tag("/"),
            )(input)
        }

        fn version_segment(input: Span) -> Res<Span, PointSegSelector> {
            alt((
                recursive_segment,
                any_segment,
                exact_version_segment,
                version_req_segment,
            ))(input)
        }

        /*
        pub fn pattern<'r, O, E: ParseError<&'r str>, V>(
            mut value: V,
        ) -> impl FnMut(&'r str) -> IResult<&str, Pattern<O>, E>
        where
            V: Parser<&'r str, O, E>,
        {
            move |input: &str| {
                let x: Res<Span, Span> = tag("*")(input);
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

         */

        pub fn pattern<'r, O, E: ParseError<Span<'r>>, V>(
            mut value: V,
        ) -> impl FnMut(Span<'r>) -> IResult<Span<'r>, Pattern<O>, E>
        where
            V: Parser<Span<'r>, O, E>,
        {
            move |input: Span| {
                let x: Res<Span, Span> = tag("*")(input);
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
                Err(err) => f
                    .parse(input.clone())
                    .map(|(next, res)| (next, ValuePattern::Pattern(res))),
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
            parse: fn(input: Span) -> Res<Span, P>,
        ) -> impl Fn(&str) -> Res<Span, ValuePattern<P>> {
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

        fn version_req(input: Span) -> Res<Span, VersionReq> {
            let str_input = *input.fragment();
            let rtn: IResult<&str, VersionReq, ErrorTree<&str>> =
                parse_from_str(version_req_chars).parse(str_input);

            match rtn {
                Ok((next, version_req)) => Ok((Span::new(next), version_req)),
                Err(err) => {
                    let tree = Err::Error(ErrorTree::from_error_kind(input, ErrorKind::Fail));
                    Err(tree)
                }
            }
        }

        fn rec_domain(input: Span) -> Res<Span, Span> {
            recognize(tuple((
                many1(terminated(skewer_chars, tag("."))),
                skewer_chars,
            )))(input)
        }

        // can be a hostname or domain name
        fn space(input: Span) -> Res<Span, Span> {
            recognize(alt((skewer_chars, rec_domain)))(input)
        }

        pub fn specific_selector(input: Span) -> Res<Span, SpecificSelector> {
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
                let vendor: Pattern<Span> = vendor;
                let product: Pattern<Span> = product;
                let variant: Pattern<Span> = variant;

                let vendor: VendorSelector = vendor.into();
                let product: ProductSelector = product.into();
                let variant: VariantSelector = variant.into();

                let specific = SpecificSelector {
                    vendor,
                    product,
                    variant,
                    version,
                };
                (next, specific)
            })
        }

        fn kind_parts(input: Span) -> Res<Span, GenericKind> {
            tuple((
                generic_kind_base,
                opt(delimited(
                    tag("<"),
                    tuple((camel_case, opt(delimited(tag("<"), specific, tag(">"))))),
                    tag(">"),
                )),
            ))(input)
            .map(|(next, (resource_type, more))| {
                let mut parts = GenericKind {
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

        fn rec_kind(input: Span) -> Res<Span, Span> {
            recognize(kind_parts)(input)
        }

        pub fn kind(input: Span) -> Res<Span, GenericKind> {
            tuple((
                generic_kind_base,
                opt(delimited(
                    tag("<"),
                    tuple((camel_case, opt(delimited(tag("<"), specific, tag(">"))))),
                    tag(">"),
                )),
            ))(input)
            .map(|(next, (resource_type, rest))| {
                let mut rtn = GenericKind {
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
            })
        }

        pub fn delim_kind(input: Span) -> Res<Span, GenericKind> {
            delimited(tag("<"), kind, tag(">"))(input)
        }

        pub fn consume_kind(input: Span) -> Result<GenericKind, MsgErr> {
            let (_, kind_parts) = all_consuming(kind_parts)(input)?;

            Ok(kind_parts.try_into()?)
        }

        pub fn generic_kind_selector(input: Span) -> Res<Span, GenericKindSelector> {
            pattern(kind)(input).map(|(next, kind)| (next, kind))
        }

        pub fn generic_kind_base(input: Span) -> Res<Span, GenericKindBase> {
            camel_case(input).map(|(next, resource_type)| (next, resource_type.to_string()))
        }

        pub fn generic_kind_base_selector(input: Span) -> Res<Span, GenericKindiBaseSelector> {
            pattern(generic_kind_base)(input)
        }

        pub fn tks(input: Span) -> Res<Span, TksPattern> {
            delimited(
                tag("<"),
                tuple((
                    generic_kind_base_selector,
                    opt(delimited(
                        tag("<"),
                        tuple((
                            generic_kind_selector,
                            opt(delimited(
                                tag("<"),
                                value_pattern(specific_selector),
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

        fn space_hop(input: Span) -> Res<Span, Hop> {
            tuple((space_segment, opt(tks), opt(tag("+"))))(input).map(
                |(next, (segment, tks, inclusive))| {
                    let tks = match tks {
                        None => TksPattern::any(),
                        Some(tks) => tks,
                    };
                    let inclusive = inclusive.is_some();
                    (
                        next,
                        Hop {
                            inclusive,
                            segment,
                            tks,
                        },
                    )
                },
            )
        }

        fn base_hop(input: Span) -> Res<Span, Hop> {
            tuple((base_segment, opt(tks), opt(tag("+"))))(input).map(
                |(next, (segment, tks, inclusive))| {
                    let tks = match tks {
                        None => TksPattern::any(),
                        Some(tks) => tks,
                    };
                    let inclusive = inclusive.is_some();
                    (
                        next,
                        Hop {
                            inclusive,
                            segment,
                            tks,
                        },
                    )
                },
            )
        }

        fn file_hop(input: Span) -> Res<Span, Hop> {
            tuple((file_segment, opt(tag("+"))))(input).map(|(next, (segment, inclusive))| {
                let tks = TksPattern {
                    resource_type: Pattern::Exact("File".to_string()),
                    kind: Pattern::Any,
                    specific: ValuePattern::Any,
                };
                let inclusive = inclusive.is_some();
                (
                    next,
                    Hop {
                        inclusive,
                        segment,
                        tks,
                    },
                )
            })
        }

        fn dir_hop(input: Span) -> Res<Span, Hop> {
            tuple((dir_segment, opt(tag("+"))))(input).map(|(next, (segment, inclusive))| {
                let tks = TksPattern::any();
                let inclusive = inclusive.is_some();
                (
                    next,
                    Hop {
                        inclusive,
                        segment,
                        tks,
                    },
                )
            })
        }

        fn version_hop(input: Span) -> Res<Span, Hop> {
            tuple((version_segment, opt(tks), opt(tag("+"))))(input).map(
                |(next, (segment, tks, inclusive))| {
                    let tks = match tks {
                        None => TksPattern::any(),
                        Some(tks) => tks,
                    };
                    let inclusive = inclusive.is_some();
                    (
                        next,
                        Hop {
                            inclusive,
                            segment,
                            tks,
                        },
                    )
                },
            )
        }

        pub fn point_selector(input: Span) -> Res<Span, PointSelector> {
            context(
                "point_kind_pattern",
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
                            segment: PointSegSelector::Exact(ExactPointSeg::PointSeg(
                                PointSeg::FilesystemRootDir,
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

                    let rtn = PointSelector { hops };

                    (next, rtn)
                },
            )
        }

        pub fn point_and_kind(input: Span) -> Res<Span, PointKind> {
            tuple((point, kind))(input)
                .map(|(next, (point, kind))| (next, PointKind { point, kind }))
        }

        /*
        fn version_req(input: Span) -> Res<Span, VersionReq> {
            let str_input = *input.fragment();
            let rtn:IResult<&str,VersionReq,ErrorTree<&str>> = parse_from_str(version_req_chars).parse(str_input);

            match rtn {
                Ok((next,version_req)) => {
                    Ok((Span::new(next), version_req))
                }
                Err(err) => {
                    let tree = Err::Error(ErrorTree::from_error_kind(input, ErrorKind::Fail));
                    Err(tree)
                }
            }
        }

         */

        pub fn version(input: Span) -> Res<Span, Version> {
            let str_input = *input.fragment();
            let rtn: IResult<&str, semver::Version, ErrorTree<&str>> =
                parse_from_str(version_chars).parse(str_input);

            match rtn {
                Ok((next, version)) => Ok((Span::new(next), Version { version })),
                Err(err) => {
                    let tree = Err::Error(ErrorTree::from_error_kind(input, ErrorKind::Fail));
                    Err(tree)
                }
            }
        }

        pub fn specific(input: Span) -> Res<Span, Specific> {
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
                !(char_item == '-')
                    && !(char_item == '_')
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
        pub def: PayloadTypeDef,
    }

    pub struct PayloadTypeDef {
        pub primitive: PayloadType,
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

                if regex.is_match(found.uri.to_string().as_str()) {
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

    /*    pub fn primitive(input: Span) -> Res<Span, PrimitiveType> {
           let (next,primitive_type) = recognize(alpha1)(input)?;
           match PrimitiveType::from_str( primitive_type.to_string().as_str() ) {
               Ok(primitive_type) => {
                   Ok((next,primitive_type))
               }
               Err(err) => {
                   Err(nom::Err::Error(ErrorTree::from_error_kind(next, ErrorKind::Fail )))
               }
           }
       }

    */

    pub fn format(input: Span) -> Res<Span, PayloadFormat> {
        let (next, format) = recognize(alpha1)(input)?;
        match PayloadFormat::from_str(format.to_string().as_str()) {
            Ok(format) => Ok((next, format)),
            Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
                next,
                ErrorKind::Fail,
            ))),
        }
    }

    /*
    pub fn str_parse<'a, Output, Error>(
        mut recognizer: impl Parser<Span<'a>, Span<'a>, Error>,
        input: Span,
    ) -> impl Parser<Span, Output, Error>
        where
            Output: FromStr,
            Error: nom::error::FromExternalError<Span<'a>, Output::Err>, nom::Err<ErrorTree<Span<'a>>>: From<nom::Err<Error>>,
    {
        let (next, recognize) = recognizer.parse(input)?;

        match Output::from_str( recognize.to_string().as_str() ) {
            Ok(output) => {
                Ok((next, output))
            }
            Err(err) => {
                Err(nom::Err::Error(ErrorTree::from_error_kind(next, ErrorKind::Fail )))
            }
        }
    }

     */

    pub fn primitive_def(input: Span) -> Res<Span, PayloadTypeDef> {
        tuple((
            payload,
            opt(preceded(tag("~"), opt(format))),
            opt(preceded(tag("~"), call_with_config)),
        ))(input)
        .map(|(next, (primitive, format, verifier))| {
            (
                next,
                PayloadTypeDef {
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

    pub fn payload(input: Span) -> Res<Span, PayloadType> {
        parse_camel_case_str(input)
    }

    pub fn consume_primitive_def(input: Span) -> Res<Span, PayloadTypeDef> {
        all_consuming(primitive_def)(input)
    }

    pub fn call_with_config(input: Span) -> Res<Span, CallWithConfig> {
        tuple((call, opt(preceded(tag("+"), point))))(input)
            .map(|(next, (call, config))| (next, CallWithConfig { call, config }))
    }

    pub fn parse_alpha1_str<O: FromStr>(input: Span) -> Res<Span, O> {
        let (next, rtn) = recognize(alpha1)(input)?;
        match O::from_str(rtn.to_string().as_str()) {
            Ok(rtn) => Ok((next, rtn)),
            Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
                next,
                ErrorKind::Fail,
            ))),
        }
    }

    pub fn rc_command(input: Span) -> Res<Span, RcCommandType> {
        parse_alpha1_str(input)
    }

    /*
    pub fn rc_call_kind(input: Span) -> Res<Span, CallKind> {
        delimited(tag("Rc<"), rc_command, tag(">"))(input)
            .map(|(next, rc_command)| (next, CallKind::Rc(rc_command)))
    }

     */

    pub fn msg_call(input: Span) -> Res<Span, CallKind> {
        tuple((
            delimited(tag("Msg<"), alphanumeric1, tag(">")),
            opt(recognize(capture_path)),
        ))(input)
        .map(|(next, (action, path))| {
            let path = match path {
                None => Span::new("/"),
                Some(path) => path,
            };
            (
                next,
                CallKind::Msg(MsgCall::new(action.to_string(), path.to_string())),
            )
        })
    }

    pub fn http_call(input: Span) -> Res<Span, CallKind> {
        tuple((delimited(tag("Http<"), http_method, tag(">")), capture_path))(input).map(
            |(next, (method, path))| {
                (
                    next,
                    CallKind::Http(HttpCall::new(method, path.to_string())),
                )
            },
        )
    }

    pub fn call_kind(input: Span) -> Res<Span, CallKind> {
        alt((msg_call, http_call))(input)
    }

    pub fn call(input: Span) -> Res<Span, Call> {
        tuple((capture_point, preceded(tag("^"), call_kind)))(input)
            .map(|(next, (point, kind))| (next, Call { point, kind }))
    }

    pub fn consume_call(input: Span) -> Res<Span, Call> {
        all_consuming(call)(input)
    }

    pub fn labeled_primitive_def(input: Span) -> Res<Span, LabeledPrimitiveTypeDef> {
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

    pub fn digit_range(input: Span) -> Res<Span, Range> {
        tuple((digit1, tag("-"), digit1))(input).map(|(next, (min, _, max))| {
            let min: usize = usize::from_str(min.to_string().as_str()).expect("usize");
            let max: usize = usize::from_str(max.to_string().as_str()).expect("usize");
            let range = Range::MinMax { min, max };

            (next, range)
        })
    }

    pub fn exact_range(input: Span) -> Res<Span, Range> {
        digit1(input).map(|(next, exact)| {
            (
                next,
                Range::Exact(
                    usize::from_str(exact.to_string().as_str())
                        .expect("expect to be able to change digit string into usize"),
                ),
            )
        })
    }

    pub fn range(input: Span) -> Res<Span, Range> {
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

    pub fn primitive_data_struct(input: Span) -> Res<Span, PayloadTypePattern> {
        context("selector", payload)(input)
            .map(|(next, primitive)| (next, PayloadTypePattern::Primitive(primitive)))
    }

    pub fn array_data_struct(input: Span) -> Res<Span, PayloadTypePattern> {
        context(
            "selector",
            tuple((
                payload,
                context("array", delimited(tag("["), range, tag("]"))),
            )),
        )(input)
        .map(|(next, (primitive, range))| {
            (
                next,
                PayloadTypePattern::List(ListPattern { primitive, range }),
            )
        })
    }

    pub fn map_entry_pattern_any(input: Span) -> Res<Span, ValuePattern<MapEntryPattern>> {
        delimited(multispace0, tag("*"), multispace0)(input)
            .map(|(next, _)| (next, ValuePattern::Any))
    }

    pub fn map_entry_pattern(input: Span) -> Res<Span, MapEntryPattern> {
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

    pub fn map_entry_patterns(input: Span) -> Res<Span, Vec<MapEntryPattern>> {
        separated_list0(
            delimited(multispace0, tag(","), multispace0),
            map_entry_pattern,
        )(input)
    }

    pub fn consume_map_entry_pattern(input: Span) -> Res<Span, MapEntryPattern> {
        all_consuming(map_entry_pattern)(input)
    }

    pub fn required_map_entry_pattern(input: Span) -> Res<Span, Vec<MapEntryPattern>> {
        delimited(tag("["), map_entry_patterns, tag("]"))(input)
            .map(|(next, params)| (next, params))
    }

    pub fn allowed_map_entry_pattern(input: Span) -> Res<Span, ValuePattern<PayloadPattern>> {
        payload_pattern(input).map(|(next, con)| (next, con))
    }

    //  [ required1<Bin>, required2<Text> ] *<Bin>
    pub fn map_pattern_params(input: Span) -> Res<Span, MapPattern> {
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
    pub fn map_pattern(input: Span) -> Res<Span, MapPattern> {
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
            parser: fn(&str) -> Res<Span, V>,
        ) -> Res<Span, ValuePattern<V>> {
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
    ) -> impl FnMut(&str) -> Res<Span,ValuePattern<O>>
        where
            F: Parser<Res<Span,ValyePattern<O>>>,
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

    pub fn value_constrained_map_pattern(input: Span) -> Res<Span, ValuePattern<MapPattern>> {
        value_pattern(map_pattern)(input)
    }

    pub fn msg_action(input: Span) -> Res<Span, ValuePattern<StringMatcher>> {
        value_pattern(camel_case_to_string_matcher)(input)
    }

    pub fn msg_pattern_scoped(input: Span) -> Res<Span, MsgPattern> {
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

    pub fn msg_pattern(input: Span) -> Res<Span, MsgPattern> {
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

    pub fn parse_camel_case_str<O: FromStr>(input: Span) -> Res<Span, O> {
        let (next, rtn) = recognize(camel_case)(input)?;
        match O::from_str(rtn.to_string().as_str()) {
            Ok(rtn) => Ok((next, rtn)),
            Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
                next,
                ErrorKind::Fail,
            ))),
        }
    }

    pub fn http_method(input: Span) -> Res<Span, HttpMethod> {
        context("http_method", parse_camel_case_str)
            .parse(input)
            .map(|(next, method): (Span, HttpMethodType)| (next, method.to_method()))
    }

    pub fn http_method_pattern(input: Span) -> Res<Span, MethodPattern> {
        context("@http_method_pattern", method_pattern(http_method))(input)
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
            Err(err) => f
                .parse(input.clone())
                .map(|(next, res)| (next, MethodPattern::Pattern(res))),
        }
    }

    pub fn http_pattern_scoped(input: Span) -> Res<Span, HttpPattern> {
        tuple((
            delimited(
                context("angle_bracket_open", tag("<")),
                http_method_pattern,
                context("angle_bracket_close", tag(">")),
            ),
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

    pub fn http_pattern(input: Span) -> Res<Span, HttpPattern> {
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

    pub fn rc_command_type(input: Span) -> Res<Span, RcCommandType> {
        parse_alpha1_str(input)
    }

    pub fn rc_pattern_scoped(input: Span) -> Res<Span, RcPattern> {
        pattern(delimited(tag("<"), rc_command_type, tag(">")))(input)
            .map(|(next, command)| (next, RcPattern { command }))
    }

    pub fn rc_pattern(input: Span) -> Res<Span, RcPattern> {
        tuple((tag("Rc"), delimited(tag("<"), rc_pattern_scoped, tag(">"))))(input)
            .map(|(next, (_, pattern))| (next, pattern))
    }

    pub fn map_pattern_payload_structure(input: Span) -> Res<Span, PayloadTypePattern> {
        map_pattern(input).map(|(next, con)| (next, PayloadTypePattern::Map(Box::new(con))))
    }

    pub fn payload_structure(input: Span) -> Res<Span, PayloadTypePattern> {
        alt((
            array_data_struct,
            primitive_data_struct,
            map_pattern_payload_structure,
        ))(input)
    }

    pub fn msg_entity_pattern(input: Span) -> Res<Span, EntityPattern> {
        msg_pattern(input).map(|(next, pattern)| (next, EntityPattern::Msg(pattern)))
    }
    pub fn http_entity_pattern(input: Span) -> Res<Span, EntityPattern> {
        http_pattern(input).map(|(next, pattern)| (next, EntityPattern::Http(pattern)))
    }

    pub fn rc_entity_pattern(input: Span) -> Res<Span, EntityPattern> {
        rc_pattern(input).map(|(next, pattern)| (next, EntityPattern::Rc(pattern)))
    }

    pub fn entity_pattern(input: Span) -> Res<Span, EntityPattern> {
        alt((msg_entity_pattern, http_entity_pattern, rc_entity_pattern))(input)
    }

    pub fn payload_structure_with_validation(input: Span) -> Res<Span, PayloadPattern> {
        tuple((
            context("selector", payload_structure),
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

    pub fn consume_payload_structure(input: Span) -> Res<Span, PayloadTypePattern> {
        all_consuming(payload_structure)(input)
    }

    pub fn consume_data_struct_def(input: Span) -> Res<Span, PayloadPattern> {
        all_consuming(payload_structure_with_validation)(input)
    }

    pub fn payload_pattern_any(input: Span) -> Res<Span, ValuePattern<PayloadPattern>> {
        tag("*")(input).map(|(next, _)| (next, ValuePattern::Any))
    }

    pub fn payload_pattern(input: Span) -> Res<Span, ValuePattern<PayloadPattern>> {
        context(
            "@payload-pattern",
            value_pattern(payload_structure_with_validation),
        )(input)
        .map(|(next, payload_pattern)| (next, payload_pattern))
    }

    /*
    pub fn payload_patterns(input: Span) -> Res<Span, ValuePattern<PayloadPattern>> {
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

    pub fn payload_filter_block_empty(input: Span) -> Res<Span, PatternBlock> {
        multispace0(input).map(|(next, _)| (input, PatternBlock::None))
    }

    pub fn payload_filter_block_any(input: Span) -> Res<Span, PatternBlock> {
        let (next, _) = delimited(multispace0, context("selector", tag("*")), multispace0)(input)?;

        Ok((next, PatternBlock::Any))
    }

    pub fn payload_filter_block_def(input: Span) -> Res<Span, PatternBlock> {
        payload_structure_with_validation(input)
            .map(|(next, pattern)| (next, PatternBlock::Pattern(pattern)))
    }

    fn insert_block_pattern(input: Span) -> Res<Span, UploadBlock> {
        delimited(multispace0, filename, multispace0)(input).map(|(next, filename)| {
            (
                next,
                UploadBlock {
                    name: filename.to_string(),
                },
            )
        })
    }

    pub fn text_payload_block(input: Span) -> Res<Span, Block> {
        delimited(
            tag("+["),
            tuple((
                multispace0,
                delimited(tag("\""), not_quote, tag("\"")),
                multispace0,
            )),
            tag("]"),
        )(input)
        .map(|(next, (_, text, _))| (next, Block::CreatePayload(Payload::Text(text.to_string()))))
    }

    pub fn upload_payload_block(input: Span) -> Res<Span, Block> {
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

    pub fn upload_step(input: Span) -> Res<Span, UploadBlock> {
        terminated(upload_payload_block, tag("->"))(input).map(|(next, block)| {
            if let Block::Upload(block) = block {
                (next, block)
            } else {
                panic!("it should have been an UploadBlock!");
            }
        })
    }

    pub fn request_payload_filter_block(input: Span) -> Res<Span, Block> {
        context(
            "request-payload-filter-block",
            terminated(
                tuple((
                    multispace0,
                    alt((
                        payload_filter_block_any,
                        payload_filter_block_def,
                        payload_filter_block_empty,
                        fail,
                    )),
                    multispace0,
                )),
                tag("]"),
            ),
        )(input)
        .map(|(next, (_, block, _))| (next, Block::RequestPattern(block)))
    }

    pub fn response_payload_filter_block(input: Span) -> Res<Span, Block> {
        context(
            "response-payload-filter-block",
            terminated(
                tuple((
                    multispace0,
                    alt((
                        payload_filter_block_any,
                        payload_filter_block_def,
                        payload_filter_block_empty,
                        fail,
                    )),
                    multispace0,
                )),
                tag("]"),
            ),
        )(input)
        .map(|(next, (_, block, _))| (next, Block::ResponsePattern(block)))
    }

    pub fn pipeline_step_block(input: Span) -> Res<Span, Block> {
        let request = request_payload_filter_block
            as for<'r> fn(Span<'r>) -> Result<(Span<'r>, Block), nom::Err<ErrorTree<Span<'r>>>>;
        let response = response_payload_filter_block
            as for<'r> fn(Span<'r>) -> Result<(Span<'r>, Block), nom::Err<ErrorTree<Span<'r>>>>;
        let upload = upload_payload_block
            as for<'r> fn(Span<'r>) -> Result<(Span<'r>, Block), nom::Err<ErrorTree<Span<'r>>>>;
        context(
            "pipeline-step-block",
            select_block(vec![
                SelectBlock(tag("-["), request),
                SelectBlock(tag("=["), response),
                SelectBlock(tag("^["), upload),
            ]),
        )(input)
    }

    pub fn consume_pipeline_block(input: Span) -> Res<Span, Block> {
        all_consuming(pipeline_step_block)(input)
    }

    #[derive(Clone, Eq, PartialEq)]
    pub struct MapEntryPattern {
        pub key: String,
        pub payload: ValuePattern<PayloadPattern>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Hop {
        pub inclusive: bool,
        pub segment: PointSegSelector,
        pub tks: TksPattern,
    }

    impl Hop {
        pub fn matches(&self, point_kind_segment: &PointKindSeg) -> bool {
            self.segment.matches(&point_kind_segment.segment)
                && self.tks.matches(&point_kind_segment.kind)
        }
    }

    impl ToString for Hop {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            rtn.push_str(self.segment.to_string().as_str());

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

    #[derive(Debug, Clone, Serialize, Deserialize)]
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
                Pattern::Exact(_) => false,
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
            P: TryInto<To, Error = MsgErr> + Eq + PartialEq,
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

    impl<'a> Into<Pattern<String>> for Pattern<Span<'a>> {
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
            P: TryInto<To, Error = MsgErr> + Eq + PartialEq,
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

    pub type GenericKindiBaseSelector = Pattern<GenericKindBase>;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct PointKindHierarchy {
        pub route: RouteSeg,
        pub segments: Vec<PointKindSeg>,
    }

    impl PointKindHierarchy {
        pub fn new(route: RouteSeg, segments: Vec<PointKindSeg>) -> Self {
            Self { route, segments }
        }

        pub fn push(&self, segment: PointKindSeg) -> PointKindHierarchy
        where
            GenericKind: Clone,
            GenericKindBase: Clone,
        {
            if let PointSeg::Root = segment.segment {
                println!("pushing ROOT");
            }
            let mut segments = self.segments.clone();
            segments.push(segment);
            Self {
                route: self.route.clone(),
                segments,
            }
        }

        pub fn consume(&self) -> Option<PointKindHierarchy> {
            if self.segments.len() <= 1 {
                return Option::None;
            }
            let mut segments = self.segments.clone();
            segments.remove(0);
            Option::Some(PointKindHierarchy {
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

    impl ToString for PointKindHierarchy {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            match &self.route {
                RouteSeg::Local => {}
                route => {
                    rtn.push_str(route.to_string().as_str());
                    rtn.push_str("::");
                }
            }

            for (index, segment) in self.segments.iter().enumerate() {
                rtn.push_str(segment.to_string().as_str());
                if index < self.segments.len() - 1 {
                    rtn.push_str(segment.segment.terminating_delim());
                }
            }

            rtn
        }
    }

    #[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
    pub struct PointKindSeg {
        pub segment: PointSeg,
        pub kind: GenericKind,
    }

    impl ToString for PointKindSeg {
        fn to_string(&self) -> String {
            format!("{}<{}>", self.segment.to_string(), self.kind.to_string())
        }
    }

    impl FromStr for PointKindHierarchy {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(consume_hierarchy(Span::new(s))?)
        }
    }
}

pub mod messaging {
    use std::collections::HashMap;
    use std::convert::TryInto;

    use http::StatusCode;
    use serde::{Deserialize, Serialize};

    use crate::error::{MsgErr, StatusErr};
    use crate::version::v0_0_1::entity::request::RequestCore;
    use crate::version::v0_0_1::entity::response::ResponseCore;
    use crate::version::v0_0_1::id::Point;
    use crate::version::v0_0_1::payload::{Errors, MsgCall, Payload, Primitive};
    use crate::version::v0_0_1::security::{
        Access, AccessGrant, EnumeratedAccess, EnumeratedPrivileges, Permissions, Privilege,
        Privileges,
    };
    use crate::version::v0_0_1::selector::{PointKindHierarchy, PointSelector};
    use crate::version::v0_0_1::util::unique_id;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Request {
        pub id: String,
        pub agent: Agent,
        pub session: Option<Session>,
        pub scope: Scope,
        pub handling: Handling,
        pub from: Point,
        pub to: Point,
        pub core: RequestCore,
    }

    impl Request {
        pub fn result<E: StatusErr>(self, result: Result<ResponseCore, E>) -> Response {
            match result {
                Ok(core) => Response {
                    id: unique_id(),
                    to: self.from,
                    from: self.to,
                    core,
                    response_to: self.id,
                },
                Err(err) => {
                    let core = self.core.err(err);
                    Response {
                        id: unique_id(),
                        to: self.from,
                        from: self.to,
                        core,
                        response_to: self.id,
                    }
                }
            }
        }

        pub fn payload_result<E: StatusErr>(self, result: Result<Payload, E>) -> Response {
            match result {
                Ok(payload) => self.ok_payload(payload),
                Err(err) => {
                    let core = self.core.err(err);
                    Response {
                        id: unique_id(),
                        to: self.from,
                        from: self.to,
                        core,
                        response_to: self.id,
                    }
                }
            }
        }

        pub fn to_call(&self) -> MsgCall {
            MsgCall {
                path: self.core.uri.to_string(),
                action: self.core.action.to_string(),
            }
        }
    }

    impl Request {
        pub fn new(core: RequestCore, from: Point, to: Point) -> Self {
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
                status: StatusCode::from_u16(200u16).unwrap(),
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
                body: payload,
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
                body: Payload::Errors(Errors::default(error.to_string().as_str())),
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

        pub fn status(self, status: u16) -> Response {
            fn process(
                request: &Request,
                status: u16,
            ) -> Result<Response, http::status::InvalidStatusCode> {
                let core = ResponseCore {
                    headers: Default::default(),
                    status: StatusCode::from_u16(status)?,
                    body: Payload::Empty,
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
                Err(err) => self.fail(format!("bad status: {}", status).as_str()),
            }
        }
    }

    pub struct RequestBuilder {
        pub to: Option<Point>,
        pub from: Option<Point>,
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

        pub fn to(mut self, point: Point) -> Self {
            self.to = Some(point);
            self
        }

        pub fn from(mut self, point: Point) -> Self {
            self.from = Some(point);
            self
        }

        pub fn core(mut self, core: RequestCore) -> Self {
            self.core = Some(core);
            self
        }

        pub fn agent(mut self, agent: Agent) -> Self {
            self.agent = agent;
            self
        }

        pub fn session(mut self, session: Session) -> Self {
            self.session = Some(session);
            self
        }

        pub fn scope(mut self, scope: Scope) -> Self {
            self.scope = scope;
            self
        }

        pub fn handling(mut self, handling: Handling) -> Self {
            self.handling = handling;
            self
        }

        pub fn build(self) -> Result<Request, MsgErr> {
            Ok(Request {
                id: unique_id(),
                to: self.to.ok_or("RequestBuilder: 'to' must be set")?,
                from: self.from.ok_or("RequestBuilder: 'from' must be set")?,
                core: self.core.ok_or("RequestBuilder: 'core' must be set")?,
                agent: self.agent,
                session: self.session,
                scope: self.scope,
                handling: self.handling,
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
                handling: Default::default(),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ProtoRequest {
        pub id: String,
        pub to: Option<Point>,
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

        pub fn to(&mut self, to: Point) {
            self.to = Option::Some(to);
        }

        pub fn core(&mut self, core: RequestCore) {
            self.core = Option::Some(core);
        }

        pub fn into_request(
            self,
            from: Point,
            agent: Agent,
            session: Option<Session>,
            scope: Scope,
        ) -> Result<Request, MsgErr> {
            self.validate()?;
            let core = self
                .core
                .or(Option::Some(Default::default()))
                .expect("expected RequestCore");
            let request = Request {
                id: self.id,
                from,
                to: self.to.expect("expected to point"),
                core,
                agent,
                session,
                handling: Default::default(),
                scope,
            };
            Ok(request)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Response {
        pub id: String,
        pub from: Point,
        pub to: Point,
        pub core: ResponseCore,
        pub response_to: String,
    }

    impl Response {
        pub fn new(core: ResponseCore, from: Point, to: Point, response_to: String) -> Self {
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
                if let Payload::Text(error) = self.core.body {
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

        pub fn to(&self) -> Point {
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
        Response(ResponseCore),
    }

    pub enum ResponseKindExpected {
        None,
        Synch,          // requestor will wait for response
        Async(Payload), // The payload
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Agent {
        Anonymous,
        Authenticated(AuthedAgent),
    }

    impl Default for Agent {
        fn default() -> Self {
            Self::Anonymous
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct AuthedAgent {
        pub owner: Point,
        pub executor: Point,
    }

    impl AuthedAgent {
        pub fn new(point: Point) -> Self {
            Self {
                owner: point.clone(),
                executor: point,
            }
        }
    }

    impl TryInto<AuthedAgent> for Agent {
        type Error = MsgErr;

        fn try_into(self) -> Result<AuthedAgent, Self::Error> {
            match self {
                Agent::Anonymous => Err(MsgErr::new(401, "Authorization required")),
                Agent::Authenticated(auth) => Ok(auth),
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
        pub attributes: HashMap<String, String>,
    }

    impl Session {
        pub fn get_preferred_username(&self) -> Option<String> {
            self.attributes
                .get(&"preferred_username".to_string())
                .cloned()
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Scope {
        Full,
        None,
        Grants(Vec<ScopeGrant>),
    }

    impl Scope {
        /*
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

         */
    }

    impl Default for Scope {
        fn default() -> Self {
            Self::None
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ScopeGrant {
        pub on: PointSelector,
        pub kind: ScopeGrantKind,
        pub aspect: ScopeGrantAspect,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ScopeGrantKind {
        Or,
        And,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ScopeGrantAspect {
        Perm(Permissions),
        Priv(Privilege),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RequestAccess {
        pub permissions: Permissions,
        pub privileges: Privileges,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Roles {
        Full,
        None,
        Enumerated(Vec<String>),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Handling {
        kind: HandlingKind,
        priority: Priority,
        retries: Retries,
        timeout: Timeout,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum HandlingKind {
        Durable,   // Mesh will guarantee delivery eventually once Request call has returned
        Queued,    // Slower but more reliable delivery, message can be lost if a star crashes, etc
        Immediate, // Message should never touch a filesystem, it will be in memory for its entire journey for immediate processing
    }

    impl Default for Handling {
        fn default() -> Self {
            Self {
                kind: HandlingKind::Queued,
                priority: Default::default(),
                retries: Default::default(),
                timeout: Default::default(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Timeout {
        Never,
        Max,
        Medium,
        Min,
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
        Min,
    }

    impl Default for Retries {
        fn default() -> Self {
            Retries::None
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Priority {
        High,
        Med,
        Low,
    }

    impl Default for Priority {
        fn default() -> Self {
            Self::Med
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Karma {
        Super,
        High,
        Med,
        Low,
        None,
    }

    impl Default for Karma {
        fn default() -> Self {
            Self::High
        }
    }
}

pub mod security {
    use crate::error::MsgErr;
    use crate::version::v0_0_1::id::Point;
    use crate::version::v0_0_1::messaging::ScopeGrant;
    use crate::version::v0_0_1::parse::error::{just_msg, result};
    use crate::version::v0_0_1::parse::{
        particle_perms, permissions_mask, privilege, MapResolver, Resolver, Subst, ToResolved,
    };
    use crate::version::v0_0_1::selector::PointSelector;
    use crate::version::v0_0_1::Span;
    use nom::combinator::all_consuming;
    use nom_supreme::parser_ext::MapRes;
    use serde::{Deserialize, Serialize};
    use std::collections::{HashMap, HashSet};
    use std::ops::{Deref, DerefMut};
    use std::str::FromStr;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Access {
        // bool is true if Super is also Owner
        Super(bool),
        Owner,
        Enumerated(EnumeratedAccess),
    }

    impl Access {
        pub fn has_super(&self) -> bool {
            match self {
                Access::Super(_) => true,
                _ => false,
            }
        }

        pub fn has_owner(&self) -> bool {
            match self {
                Access::Owner => true,
                Access::Super(owner) => owner.clone(),
                _ => false,
            }
        }

        pub fn has_full(&self) -> bool {
            match self {
                Access::Super(_) => true,
                Access::Owner => true,
                Access::Enumerated(_) => false,
            }
        }

        pub fn none() -> Self {
            Self::Enumerated(EnumeratedAccess::none())
        }

        pub fn permissions(&self) -> Permissions {
            match self {
                Access::Super(_) => Permissions::full(),
                Access::Owner => Permissions::full(),
                Access::Enumerated(enumerated) => enumerated.permissions.clone(),
            }
        }

        pub fn check_privilege(&self, privilege: &str) -> Result<(), MsgErr> {
            match self {
                Access::Super(_) => Ok(()),
                Access::Owner => Ok(()),
                Access::Enumerated(enumerated) => {
                    match enumerated.privileges.has(privilege).is_ok() {
                        true => Ok(()),
                        false => Err(format!("'{}'", privilege).into()),
                    }
                }
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Privileges {
        Full,
        Enumerated(EnumeratedPrivileges),
    }

    impl Privileges {
        pub fn has(&self, privilege: &str) -> Result<(), ()> {
            match self {
                Privileges::Full => Ok(()),
                Privileges::Enumerated(privileges) => privileges.has(privilege),
            }
        }

        pub fn none() -> Self {
            Self::Enumerated(EnumeratedPrivileges::none())
        }

        pub fn or(mut self, other: &Self) -> Self {
            match self {
                Privileges::Full => self,
                Privileges::Enumerated(privileges) => match other {
                    Privileges::Full => Privileges::Full,
                    Privileges::Enumerated(other) => Privileges::Enumerated(privileges.or(other)),
                },
            }
        }

        pub fn and(self, other: &Self) -> Privileges {
            match other {
                Privileges::Full => self,
                Privileges::Enumerated(enumerated_other) => match self {
                    Privileges::Full => other.clone(),
                    Privileges::Enumerated(enumerated_self) => {
                        Privileges::Enumerated(enumerated_self.and(enumerated_other))
                    }
                },
            }
        }

        pub fn add(&mut self, privilege: &str) {
            match self {
                Self::Full => {}
                Self::Enumerated(privileges) => privileges.add(privilege),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct EnumeratedPrivileges {
        set: HashSet<String>,
    }

    impl EnumeratedPrivileges {
        pub fn new() -> Self {
            Self {
                set: HashSet::new(),
            }
        }

        pub fn none() -> Self {
            Self {
                set: HashSet::new(),
            }
        }

        pub fn or(mut self, other: &Self) -> Self {
            for p in other.set.iter() {
                if self.has(p).is_err() {
                    self.add(p.as_str());
                }
            }
            self
        }

        pub fn and(mut self, other: &Self) -> Self {
            self.set.retain(|p| other.has(p).is_ok());
            self
        }

        pub fn add(&mut self, privilege: &str) {
            self.set.insert(privilege.to_string());
        }

        pub fn has(&self, privilege: &str) -> Result<(), ()> {
            let privilege = privilege.to_string();
            if self.set.contains(&privilege) {
                Ok(())
            } else {
                Err(())
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum Privilege {
        Full,
        Single(String),
    }

    impl ToString for Privilege {
        fn to_string(&self) -> String {
            match self {
                Privilege::Full => "*".to_string(),
                Privilege::Single(name) => name.clone(),
            }
        }
    }

    impl FromStr for Privilege {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let span = Span::new(s);
            Ok(result(all_consuming(privilege)(span))?)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct EnumeratedAccess {
        pub permissions: Permissions,
        pub privileges: Privileges,
    }

    impl EnumeratedAccess {
        pub fn mask(&mut self, scope_grant: &ScopeGrant) {}

        pub fn full() -> Self {
            Self {
                permissions: Permissions::full(),
                privileges: Privileges::Full,
            }
        }

        pub fn none() -> Self {
            Self {
                permissions: Permissions::none(),
                privileges: Privileges::none(),
            }
        }

        pub fn and(&mut self, access: &Self) {
            self.permissions.and(&access.permissions);
            self.privileges = self.privileges.clone().and(&access.privileges);
        }

        pub fn clear_privs(&mut self) {
            self.privileges = Privileges::none()
        }

        pub fn add(&mut self, grant: &AccessGrant) {
            match &grant.kind {
                AccessGrantKindDef::Super => {
                    // we can't mask Super with Enumerated... it does nothing
                }
                AccessGrantKindDef::Privilege(prv) => match prv {
                    Privilege::Full => {
                        self.privileges = Privileges::Full;
                    }
                    Privilege::Single(prv) => {
                        self.privileges.add(prv.as_str());
                    }
                },
                AccessGrantKindDef::PermissionsMask(mask) => match mask.kind {
                    PermissionsMaskKind::Or => self.permissions.or(&mask.permissions),
                    PermissionsMaskKind::And => self.permissions.and(&mask.permissions),
                },
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct PermissionsMask {
        pub kind: PermissionsMaskKind,
        pub permissions: Permissions,
    }

    impl FromStr for PermissionsMask {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let s = Span::new(s);
            just_msg(all_consuming(permissions_mask)(s))
        }
    }

    impl ToString for PermissionsMask {
        fn to_string(&self) -> String {
            match self.kind {
                PermissionsMaskKind::Or => format!("+{}", self.permissions.to_string()),
                PermissionsMaskKind::And => format!("&{}", self.permissions.to_string()),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Permissions {
        pub child: ChildPerms,
        pub particle: ParticlePerms,
    }

    impl Permissions {
        pub fn full() -> Self {
            Self {
                child: ChildPerms::full(),
                particle: ParticlePerms::full(),
            }
        }

        pub fn none() -> Self {
            Self {
                child: ChildPerms::none(),
                particle: ParticlePerms::none(),
            }
        }

        pub fn or(&mut self, permissions: &Permissions) {
            self.child.or(&permissions.child);
            self.particle.or(&permissions.particle);
        }

        pub fn and(&mut self, permissions: &Permissions) {
            self.child.and(&permissions.child);
            self.particle.and(&permissions.particle);
        }
    }

    impl ToString for Permissions {
        fn to_string(&self) -> String {
            format!("{}-{}", self.child.to_string(), self.particle.to_string())
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct ChildPerms {
        pub create: bool,
        pub select: bool,
        pub delete: bool,
    }

    impl ChildPerms {
        pub fn full() -> Self {
            Self {
                create: true,
                select: true,
                delete: true,
            }
        }

        pub fn none() -> Self {
            Self {
                create: false,
                select: false,
                delete: false,
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

    impl ToString for ChildPerms {
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

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct ParticlePerms {
        pub read: bool,
        pub write: bool,
        pub execute: bool,
    }

    impl ParticlePerms {
        pub fn full() -> Self {
            Self {
                read: true,
                write: true,
                execute: true,
            }
        }

        pub fn none() -> Self {
            Self {
                read: false,
                write: false,
                execute: false,
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
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let s = Span::new(s);
            just_msg(all_consuming(particle_perms)(s))
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

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum PermissionsMaskKind {
        Or,
        And,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct AccessGrantDef<Priv, PermMask, PointSelector, Point> {
        pub kind: AccessGrantKindDef<Priv, PermMask>,
        pub on_point: PointSelector,
        pub to_point: PointSelector,
        pub by_particle: Point,
    }

    impl ToResolved<AccessGrant> for AccessGrantSubst {
        fn to_resolved(self, resolver: &dyn Resolver) -> Result<AccessGrant, MsgErr> {
            Ok(AccessGrant {
                kind: self.kind.to_resolved(resolver)?,
                on_point: self.on_point.to_resolved(resolver)?,
                to_point: self.to_point.to_resolved(resolver)?,
                by_particle: self.by_particle.to_resolved(resolver)?,
            })
        }
    }

    impl AccessGrantSubst {
        pub fn with_by(self, by_particle: Point) -> Result<AccessGrant, MsgErr> {
            let map = MapResolver::new();
            Ok(AccessGrant {
                kind: self.kind.to_resolved(&map)?,
                on_point: self.on_point.to_resolved(&map)?,
                to_point: self.to_point.to_resolved(&map)?,
                by_particle: by_particle,
            })
        }
    }

    pub type AccessGrant = AccessGrantDef<Privilege, PermissionsMask, PointSelector, Point>;
    pub type AccessGrantKind = AccessGrantKindDef<Privilege, PermissionsMask>;
    pub type AccessGrantKindSubst = AccessGrantKindDef<Subst<Privilege>, Subst<PermissionsMask>>;
    pub type AccessGrantSubst = AccessGrantDef<
        Subst<Privilege>,
        Subst<PermissionsMask>,
        Subst<PointSelector>,
        Subst<Point>,
    >;

    impl ToResolved<AccessGrantKind> for AccessGrantKindSubst {
        fn to_resolved(self, resolver: &dyn Resolver) -> Result<AccessGrantKind, MsgErr> {
            match self {
                AccessGrantKindSubst::Super => Ok(AccessGrantKind::Super),
                AccessGrantKindSubst::Privilege(privilege) => {
                    Ok(AccessGrantKind::Privilege(privilege.to_resolved(resolver)?))
                }
                AccessGrantKindSubst::PermissionsMask(perms) => {
                    Ok(AccessGrantKind::PermissionsMask(perms.to_resolved(resolver)?))
                }
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum AccessGrantKindDef<Priv, PermMask> {
        Super,
        Privilege(Priv),
        PermissionsMask(PermMask),
    }

    impl<Priv, PermMask> ToString for AccessGrantKindDef<Priv, PermMask> {
        fn to_string(&self) -> String {
            match self {
                AccessGrantKindDef::Super => "super".to_string(),
                AccessGrantKindDef::Privilege(_) => "priv".to_string(),
                AccessGrantKindDef::PermissionsMask(_) => "perm".to_string(),
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
    use crate::version::v0_0_1::id::{CaptureAddress, GenericKind, GenericKindBase, Meta, Point};
    use crate::version::v0_0_1::particle::{Particle, Status, Stub};
    use crate::version::v0_0_1::selector::TksPattern;
    use crate::version::v0_0_1::util::{ValueMatcher, ValuePattern};
    use http::{Method, Uri};
    use std::str::FromStr;
    use std::sync::Arc;

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
        List,
        Map,
        Point,
        Text,
        Boolean,
        Int,
        Meta,
        Bin,
        Stub,
        Status,
        Particle,
        Errors,
        Json,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
    pub enum Payload {
        Empty,
        List(PayloadList),
        Map(PayloadMap),
        Point(Point),
        Text(String),
        Stub(Stub),
        Meta(Meta),
        Bin(Bin),
        Boolean(bool),
        Int(i64),
        Status(Status),
        Particle(Particle),
        Errors(Errors),
        Json(serde_json::Value),
    }

    impl Payload {
        pub fn to_text(self) -> Result<String, MsgErr> {
            if let Payload::Text(text) = self {
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
            Self::Bin(bin)
        }

        pub fn payload_type(&self) -> PayloadType {
            match self {
                Payload::Empty => PayloadType::Empty,
                Payload::List(list) => PayloadType::List,
                Payload::Map(map) => PayloadType::Map,
                Payload::Point(_) => PayloadType::Point,
                Payload::Text(_) => PayloadType::Text,
                Payload::Stub(_) => PayloadType::Stub,
                Payload::Meta(_) => PayloadType::Meta,
                Payload::Bin(_) => PayloadType::Bin,
                Payload::Boolean(_) => PayloadType::Boolean,
                Payload::Int(_) => PayloadType::Int,
                Payload::Status(_) => PayloadType::Status,
                Payload::Particle(_) => PayloadType::Particle,
                Payload::Errors(_) => PayloadType::Errors,
                Payload::Json(_) => PayloadType::Json,
            }
        }

        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            match self {
                Payload::Empty => Ok(Arc::new(vec![])),
                Payload::List(list) => list.to_bin(),
                Payload::Map(map) => map.to_bin(),
                _ => Err("not supported".into()),
            }
        }
    }

    impl TryInto<HashMap<String, Payload>> for Payload {
        type Error = MsgErr;

        fn try_into(self) -> Result<HashMap<String, Payload>, Self::Error> {
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
                Payload::Text(text) => Ok(text),
                Payload::Bin(bin) => Ok(String::from_utf8(bin.to_vec())?),
                _ => Err("Payload type must an Text".into()),
            }
        }
    }

    impl TryInto<Point> for Payload {
        type Error = MsgErr;

        fn try_into(self) -> Result<Point, Self::Error> {
            match self {
                Payload::Point(point) => Ok(point),
                _ => Err("Payload type must an Address".into()),
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
        Point(Point),
        Stub(Stub),
        Meta(Meta),
        Bin(Bin),
        Boolean(bool),
        Int(i64),
        Status(Status),
        Particle(Particle),
        Errors(Errors),
    }

    impl Primitive {
        pub fn primitive_type(&self) -> PrimitiveType {
            match self {
                Primitive::Text(_) => PrimitiveType::Text,
                Primitive::Point(_) => PrimitiveType::Address,
                Primitive::Stub(_) => PrimitiveType::Stub,
                Primitive::Meta(_) => PrimitiveType::Meta,
                Primitive::Bin(_) => PrimitiveType::Bin,
                Primitive::Boolean(_) => PrimitiveType::Boolean,
                Primitive::Int(_) => PrimitiveType::Int,
                Primitive::Status(_) => PrimitiveType::Status,
                Primitive::Particle(_) => PrimitiveType::Resource,
                Primitive::Errors(_) => PrimitiveType::Errors,
            }
        }

        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            match self {
                Primitive::Text(text) => {
                    let text = text.into_bytes();
                    Ok(Arc::new(text))
                }
                Primitive::Point(point) => {
                    let point = point.to_string().into_bytes();
                    Ok(Arc::new(point))
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
                Primitive::Particle(resource) => Ok(Arc::new(bincode::serialize(&resource)?)),
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

    impl TryInto<Point> for Primitive {
        type Error = MsgErr;

        fn try_into(self) -> Result<Point, Self::Error> {
            match self {
                Primitive::Point(point) => Ok(point),
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
    pub struct PayloadList {
        pub list: Vec<Box<Payload>>,
    }

    impl ToString for PayloadList {
        fn to_string(&self) -> String {
            "[]".to_string()
        }
    }

    impl PayloadList {
        pub fn new() -> Self {
            Self { list: vec![] }
        }
        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            Ok(Arc::new(bincode::serialize(&self)?))
        }
    }

    impl Deref for PayloadList {
        type Target = Vec<Box<Payload>>;

        fn deref(&self) -> &Self::Target {
            &self.list
        }
    }

    impl DerefMut for PayloadList {
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

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct ListPattern {
        pub primitive: PayloadType,
        pub range: Range,
    }

    impl ListPattern {
        pub fn is_match(&self, list: &PayloadList) -> Result<(), MsgErr> {
            /*
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

             */
            unimplemented!()
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
        Primitive(PayloadType),
        List(ListPattern),
        Map(Box<MapPattern>),
    }

    impl PayloadTypePattern {
        pub fn is_match(&self, payload: &Payload) -> Result<(), MsgErr> {
            unimplemented!();
            /*
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

             */
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
        pub config: Option<Point>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Call {
        pub point: CaptureAddress,
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
            format!("{}^{}", self.point.to_string(), self.kind.to_string())
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
                HttpMethodType::Trace => HttpMethod::TRACE,
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
                Primitive::Point(_) => {
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
                Primitive::Particle(_) => {
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
            pub fn set_or<E>(&self, err: E) -> Result<String, E> {
                match self {
                    Self::Set { key, value, lock } => Ok(value.clone()),
                    Self::UnSet(_) => Err(err),
                }
            }

            pub fn opt(&self) -> Option<String> {
                match self {
                    Self::Set { key, value, lock } => Some(value.clone()),
                    Self::UnSet(_) => None,
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
    use http::{HeaderMap, StatusCode, Uri};
    use serde::{Deserialize, Serialize};

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
                body: Payload::Errors(errors),
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
    use crate::version::v0_0_1::id::{GenericKind, Point};
    use crate::version::v0_0_1::particle;
    use crate::version::v0_0_1::particle::Stub;

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
        pub stub: Stub,
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
        pub stub: particle::Stub,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Config<Body> {
        pub point: Point,
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
        use crate::version::v0_0_1::payload::Call;
        use crate::version::v0_0_1::payload::{Payload, PayloadPattern};
        use crate::version::v0_0_1::selector::{
            Block, EntityPattern, HttpPattern, MsgPattern, RcPattern,
        };
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
            use crate::error::MsgErr;
            use crate::version::v0_0_1::config::bind::{
                ConfigScope, Pipeline, PipelineSegment, PipelineStep, PipelineStop,
                PipelinesSubScope, ProtoBind, Selector, StepKind,
            };
            use crate::version::v0_0_1::entity::EntityType;
            use crate::version::v0_0_1::parse::{capture_point, Res};
            use crate::version::v0_0_1::selector::{
                call, entity_pattern, http_pattern, http_pattern_scoped, msg_pattern_scoped,
                pipeline_step_block, rc_pattern_scoped, EntityPattern, HttpPattern, MsgPattern,
                RcPattern,
            };
            use crate::version::v0_0_1::Span;
            use nom::branch::{alt, Alt};
            use nom::bytes::complete::tag;
            use nom::character::complete::multispace0;
            use nom::combinator::{all_consuming, fail, not, opt, peek, success};
            use nom::error::{context, ContextError, ErrorKind, ParseError};
            use nom::multi::{many0, many1};
            use nom::sequence::{delimited, preceded, tuple};
            use nom::{
                AsChar, Compare, Err, FindToken, IResult, InputIter, InputLength, InputTake,
                InputTakeAtPosition, Parser, UnspecializedInput,
            };
            use nom_supreme::error::{ErrorTree, StackContext};
            use nom_supreme::final_parser::final_parser;
            use nom_supreme::ParserExt;
            use std::borrow::Borrow;
            use std::ops::{Deref, DerefMut};

            pub fn final_bind(input: Span) -> Result<ProtoBind, String> {
                final_parser(delimited(multispace0, bind, multispace0))(input)
                    .map_err(|err| final_bind_error(err))
            }

            fn final_bind_error(error: ErrorTree<Span>) -> String {
                unimplemented!()
            }

            #[derive(Clone)]
            pub struct ErrorStack<I>
            where
                I: Clone,
            {
                pub list: Vec<ErrorSegment<I>>,
            }

            impl<I> ErrorStack<I>
            where
                I: Clone,
            {
                pub fn new(mut list: Vec<ErrorSegment<I>>) -> Result<Self, ()> {
                    if list.is_empty() {
                        return Err(());
                    } else {
                        Ok(Self { list })
                    }
                }

                pub fn normalize(&self) -> Result<Self, ()> {
                    let mut list = vec![];
                    for seg in self.list.iter() {
                        list.push(seg.clone());
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
                            let c = seg
                                .context
                                .chars()
                                .next()
                                .expect("expected first character");
                            if c.is_alpha() && c.is_uppercase() {
                                list.push(seg.context)
                            }
                        }
                    }
                    let mut rtn = String::new();
                    for (index, seg) in list.iter().enumerate() {
                        rtn.push_str(seg);
                        if index < list.len() - 1 {
                            rtn.push_str(".")
                        }
                    }
                    rtn
                }
            }

            impl<'a> ErrorStack<&'a str> {
                pub fn final_segment(&self) -> ErrorSegment<&'a str> {
                    match self.list.last() {
                        None => ErrorSegment {
                            context: "?",
                            location: "?",
                        },
                        Some(seg) => seg.clone(),
                    }
                }
            }

            impl<I> Deref for ErrorStack<I>
            where
                I: Clone,
            {
                type Target = Vec<ErrorSegment<I>>;

                fn deref(&self) -> &Self::Target {
                    &self.list
                }
            }

            impl<I> DerefMut for ErrorStack<I>
            where
                I: Clone,
            {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.list
                }
            }

            impl<I> ToString for ErrorStack<I>
            where
                I: Clone,
            {
                fn to_string(&self) -> String {
                    let mut rtn = String::new();
                    for (index, seg) in self.list.iter().enumerate() {
                        rtn.push_str(seg.context);
                        if index < self.list.len() - 1 {
                            rtn.push_str(".")
                        }
                    }
                    rtn
                }
            }

            #[derive(Clone)]
            pub struct ErrorSegment<I>
            where
                I: Clone,
            {
                pub context: &'static str,
                pub location: I,
            }

            fn to_error_segments<I>(contexts: &Vec<(I, StackContext)>) -> Vec<ErrorSegment<I>>
            where
                I: Clone,
            {
                let mut rtn = vec![];
                for (location, context) in contexts {
                    if let StackContext::Context(context) = context {
                        let seg = ErrorSegment {
                            location: location.clone(),
                            context,
                        };
                        rtn.push(seg);
                    }
                }
                rtn
            }

            fn first_context(contexts: &Vec<(&str, StackContext)>) -> Result<StackContext, ()> {
                if contexts.is_empty() {
                    Err(())
                } else {
                    Ok(contexts.first().cloned().unwrap().1)
                }
            }

            fn second_context(contexts: &Vec<(&str, StackContext)>) -> Result<StackContext, ()> {
                if contexts.len() < 2 {
                    Err(())
                } else {
                    Ok(contexts.get(1).cloned().unwrap().1)
                }
            }

            fn diagnose_contexts(contexts: &Vec<(&str, StackContext)>) {
                for context in contexts.iter().rev() {
                    let c = match context.1 {
                        StackContext::Kind(_) => "?",
                        StackContext::Context(ctx) => ctx,
                    };
                    print!("{}.", c);
                }
                println!();
            }

            fn extract_problem_line(input: Span) -> String {
                if input.trim().is_empty() {
                    return "Problem: \"\"".to_string();
                }

                if input.len() < 80 {
                    return format!(
                        "Problem: \"{}\"",
                        input.trim_end().lines().next().expect("line")
                    );
                }

                format!(
                    "Problem: \"{}\"",
                    input[0..80].trim_end().lines().next().expect("line")
                )
            }

            fn contexts_to_error(mut contexts: Vec<(&str, StackContext)>) -> String {
                if contexts.is_empty() {
                    return "internal parsing error: could not create usable error message because of missing context".to_string();
                }

                let (input, context) = contexts.remove(0);

                fn parent(contexts: &Vec<(&str, StackContext)>) -> String {
                    match contexts.first() {
                        None => "?",
                        Some((_, context)) => match context {
                            StackContext::Kind(_) => "!",
                            StackContext::Context(context) => context,
                        },
                    }
                    .to_string()
                }

                let message = match context {
                    StackContext::Kind(kind) => {
                        format!(": parse error: '{}'", kind.description())
                    }
                    StackContext::Context(context) => {
                        if context.starts_with("!") {
                            context[1..].to_string()
                        } else if context == "scope:expect-selector" {
                            format!("expected '{}' (scope selector)", parent(&contexts))
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
                for (index, (_, context)) in contexts.into_iter().enumerate() {
                    match context {
                        StackContext::Kind(kind) => {
                            return format!("{}\n\ninternal parsing error: unexpected kind error when processing context hierarchy: '{}'", extract_problem_line(Span::new(input)),kind.description() );
                        }
                        StackContext::Context(context) => {
                            hierarchy.push_str(context);
                            if index < len - 1 {
                                hierarchy.push_str(".");
                            }
                        }
                    }
                }

                format!(
                    "{}\n\n{}: {}",
                    extract_problem_line(Span::new(input)),
                    hierarchy,
                    message
                )
            }

            pub fn bind(input: Span) -> Res<Span, ProtoBind> {
                select_scope("Bind", select_pipelines)(input).map(|(next, sections)| {
                    let bind = ProtoBind { sections };

                    (next, bind)
                })
            }

            pub fn many_until0<I, O, O2, E, F, U>(
                mut f: F,
                mut until: U,
            ) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
            where
                I: Clone + InputLength + std::fmt::Display,
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
                            }
                            Err(e) => return Err(e),
                            Ok((i1, o)) => {
                                // infinite loop check: the parser must always consume
                                if i1.input_len() == len {
                                    return Err(nom::Err::Error(E::from_error_kind(
                                        i,
                                        ErrorKind::Many0,
                                    )));
                                }
                                i = i1;
                                acc.push(o);
                            }
                        }
                    }
                }
            }

            pub fn select0<I, O, O2, E, F, U>(
                mut f: F,
                mut until: U,
            ) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
            where
                I: Clone + InputLength + std::fmt::Display,
                F: Parser<I, O, E>,
                U: Parser<I, O2, E>,
                E: ParseError<I> + ContextError<I>,
            {
                context("select", many_until0(f, context("scope:close", until)))
            }

            /// successfully parse at least one branch or fail
            /// does not return errors for any branches in a fail
            pub fn alt_fail<I: Clone, O, E: ParseError<I>, List: Alt<I, O, E>>(
                mut l: List,
            ) -> impl FnMut(I) -> IResult<I, O, E> {
                move |i: I| match l.choice(i.clone()) {
                    Ok(ok) => Ok(ok),
                    Err(_) => {
                        let e = E::from_error_kind(i, ErrorKind::Alt);
                        Err(Err::Failure(e))
                    }
                }
            }

            pub struct SelectBlock<S, F>(pub S, pub F);

            pub fn select_block<I, E, S, F, O, O2>(
                mut select_blocks: Vec<SelectBlock<S, F>>,
            ) -> impl FnMut(I) -> IResult<I, O, E>
            where
                I: Clone
                    + InputTake
                    + InputLength
                    + InputIter
                    + Compare<&'static str>
                    + InputTakeAtPosition
                    + ToString,
                <I as InputIter>::Item: AsChar,
                <I as InputIter>::Item: Clone,
                <I as InputTakeAtPosition>::Item: AsChar + Clone,
                F: Parser<I, O, E>,
                S: Parser<I, O2, E>,
                E: ParseError<I> + ContextError<I>,
            {
                let parser = move |i: I| {
                    for mut select_block in select_blocks.iter_mut() {
                        match select_block.0.parse(i.clone()) {
                            Ok((i, _)) => {
                                return select_block.1.parse(i.clone());
                            }
                            Err(_) => {}
                        }
                    }

                    // if a block hasn't been selected yet then we have an error

                    let e = E::from_error_kind(i.clone(), ErrorKind::Tag);
                    let e = E::add_context(i, "!select-block", e);
                    return Err(Err::Failure(e));
                };

                parser
            }

            pub fn multi_select_scope<I, E, S, F, O, O2>(
                mut selectors: S,
                mut f: F,
            ) -> impl FnMut(I) -> IResult<I, O, E>
            where
                I: Clone
                    + InputTake
                    + InputLength
                    + InputIter
                    + Compare<&'static str>
                    + InputTakeAtPosition
                    + ToString,
                <I as InputIter>::Item: AsChar,
                <I as InputIter>::Item: Clone,
                <I as InputTakeAtPosition>::Item: AsChar + Clone,
                F: Parser<I, O, E>,
                S: Parser<I, O2, E>,
                E: ParseError<I> + ContextError<I>,
            {
                let parser = move |i: I| {
                    // first remove any whitespace
                    let (i, _) = multispace0(i.clone())?;

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
                        Ok(_) => f.parse(i),
                        Err(e) => {
                            match e {
                                Err::Incomplete(_) => Err(e),
                                Err::Error(e2) => {
                                    Err(Err::Failure(E::add_context(i, "multi_select_scope", e2)))
                                    //                                    Err(Err::Failure(e2))
                                }
                                Err::Failure(e2) => Err(Err::Failure(E::add_context(
                                    i.clone(),
                                    "multi_select_scope",
                                    e2,
                                ))),
                            }
                        }
                    }
                };

                parser
            }

            fn select_scope<I, E, F, O>(
                selection: &'static str,
                mut f: F,
            ) -> impl FnMut(I) -> IResult<I, O, E>
            where
                I: Clone
                    + InputTake
                    + InputLength
                    + InputIter
                    + Compare<&'static str>
                    + InputTakeAtPosition,
                <I as InputIter>::Item: AsChar,
                <I as InputIter>::Item: Clone,
                <I as InputTakeAtPosition>::Item: AsChar + Clone,
                F: Parser<I, O, E>,
                E: ParseError<I> + ContextError<I>,
            {
                let parser = move |i: I| {
                    let (next, _) = context("selector", tag(selection))(i)?;
                    let (next, _) = multispace0(next)?;
                    let (next, _) = context("scope:open", tag("{"))(next)?;
                    let (next, _) = multispace0(next)?;
                    let (next, rtn) = f.parse(next)?;
                    let (next, _) = multispace0(next)?;
                    let (next, _) = context("scope:close", tag("}"))(next)?;
                    Ok((next, rtn))
                };

                context(selection, parser)
            }

            fn scope<I, E, F, O, O2, O3, Open, Close>(
                mut open: Open,
                mut f: F,
                mut close: Close,
            ) -> impl FnMut(I) -> IResult<I, O, E>
            where
                I: Clone
                    + InputTake
                    + InputLength
                    + InputIter
                    + Compare<&'static str>
                    + InputTakeAtPosition,
                <I as InputIter>::Item: AsChar,
                <I as InputIter>::Item: Clone,
                <I as InputTakeAtPosition>::Item: AsChar + Clone,
                F: Parser<I, O, E>,
                Open: Parser<I, O2, E>,
                Close: Parser<I, O3, E>,
                E: ParseError<I> + ContextError<I>,
            {
                move |i: I| {
                    let (next, _) = open.parse(i)?;
                    let (next, _) = multispace0(next)?;
                    let (next, rtn) = f.parse(next)?;
                    let (next, _) = multispace0(next)?;
                    let (next, _) = close.parse(next)?;
                    Ok((next, rtn))
                }
            }

            fn whitespace_until<I, E, F, O>(mut f: F) -> impl FnMut(I) -> IResult<I, O, E>
            where
                I: Clone
                    + InputTake
                    + InputLength
                    + InputIter
                    + Compare<&'static str>
                    + InputTakeAtPosition,
                <I as InputIter>::Item: AsChar,
                <I as InputIter>::Item: Clone,
                <I as InputTakeAtPosition>::Item: AsChar + Clone,
                F: Parser<I, O, E>,
                E: ParseError<I> + ContextError<I>,
            {
                move |i: I| {
                    let (next, _) = multispace0(i)?;
                    let (next, rtn) = f.parse(next)?;
                    Ok((next, rtn))
                }
            }

            fn padded_curly_open<I>(input: I) -> Res<I, I>
            where
                I: Clone
                    + InputTake
                    + InputLength
                    + InputIter
                    + Compare<&'static str>
                    + InputTakeAtPosition,
                <I as InputIter>::Item: AsChar,
                <I as InputIter>::Item: Clone,
                <I as InputTakeAtPosition>::Item: AsChar + Clone,
            {
                delimited(
                    multispace0,
                    context("!expected: '{'", tag("{")),
                    multispace0,
                )(input)
            }

            fn padded_curly_close<I>(input: I) -> Res<I, I>
            where
                I: Clone
                    + InputTake
                    + InputLength
                    + InputIter
                    + Compare<&'static str>
                    + InputTakeAtPosition,
                <I as InputIter>::Item: AsChar,
                <I as InputIter>::Item: Clone,
                <I as InputTakeAtPosition>::Item: AsChar + Clone,
            {
                preceded(multispace0, context("!expected: '}'", tag("}")))(input)
            }

            pub fn select_pipelines(input: Span) -> Res<Span, Vec<PipelinesSubScope>> {
                select_scope("Pipelines", select0(pipelines, whitespace_until(tag("}"))))(input)
            }

            pub fn pipelines(input: Span) -> Res<Span, PipelinesSubScope> {
                multi_select_scope(
                    alt((tag("Rc"), tag("Msg"), tag("Http"))),
                    alt((
                        select_msg_pipelines,
                        select_http_pipelines,
                        select_rc_pipelines,
                    )),
                )(input)
            }

            pub fn select_msg_pipelines(input: Span) -> Res<Span, PipelinesSubScope> {
                select_scope("Msg", msg_selectors)(input).map(|(next, selectors)| {
                    (
                        next,
                        PipelinesSubScope::Msg(ConfigScope::new(EntityType::Msg, selectors)),
                    )
                })
            }

            pub fn select_http_pipelines(input: Span) -> Res<Span, PipelinesSubScope> {
                select_scope("Http", http_pipelines)(input).map(|(next, selectors)| {
                    (
                        next,
                        PipelinesSubScope::Http(ConfigScope::new(EntityType::Http, selectors)),
                    )
                })
            }

            pub fn select_rc_pipelines(input: Span) -> Res<Span, PipelinesSubScope> {
                select_scope("Rc", rc_selectors)(input).map(|(next, selectors)| {
                    (
                        next,
                        PipelinesSubScope::Rc(ConfigScope::new(EntityType::Rc, selectors)),
                    )
                })
            }

            pub fn pipeline_step(input: Span) -> Res<Span, PipelineStep> {
                context(
                    "Step",
                    tuple((
                        select0(
                            pipeline_step_block,
                            alt((
                                context("selector", tag("->")),
                                context("selector", tag("=>")),
                            )),
                        ),
                        context("!pipeline-step:exit", alt((tag("->"), tag("=>"), fail))),
                    )),
                )(input)
                .map(|(next, (blocks, kind))| {
                    let kind = match kind.to_string().as_str() {
                        "->" => StepKind::Request,
                        "=>" => StepKind::Response,
                        _ => panic!("nom parse rules should have selected -> or =>"),
                    };
                    (next, PipelineStep { kind, blocks })
                })
            }

            pub fn core_pipeline_stop(input: Span) -> Res<Span, PipelineStop> {
                context(
                    "Core",
                    delimited(
                        tag("{{"),
                        delimited(multispace0, opt(tag("*")), multispace0),
                        tag("}}"),
                    ),
                )(input)
                .map(|(next, _)| (next, PipelineStop::Internal))
            }

            pub fn return_pipeline_stop(input: Span) -> Res<Span, PipelineStop> {
                tag("&")(input).map(|(next, _)| (next, PipelineStop::Respond))
            }

            pub fn call_pipeline_stop(input: Span) -> Res<Span, PipelineStop> {
                context("Call", call)(input).map(|(next, call)| (next, PipelineStop::Call(call)))
            }

            pub fn point_pipeline_stop(input: Span) -> Res<Span, PipelineStop> {
                context("Address", capture_point)(input)
                    .map(|(next, point)| (next, PipelineStop::CaptureAddress(point)))
            }

            pub fn pipeline_stop(input: Span) -> Res<Span, PipelineStop> {
                context(
                    "Stop",
                    alt((
                        core_pipeline_stop,
                        return_pipeline_stop,
                        call_pipeline_stop,
                        point_pipeline_stop,
                    )),
                )(input)
            }

            pub fn consume_pipeline_step(input: Span) -> Res<Span, PipelineStep> {
                all_consuming(pipeline_step)(input)
            }

            pub fn consume_pipeline_stop(input: Span) -> Res<Span, PipelineStop> {
                all_consuming(pipeline_stop)(input)
            }

            pub fn pipeline_segment(input: Span) -> Res<Span, PipelineSegment> {
                tuple((
                    multispace0,
                    pipeline_step,
                    multispace0,
                    pipeline_stop,
                    multispace0,
                ))(input)
                .map(|(next, (_, step, _, stop, _))| (next, PipelineSegment { step, stop }))
            }

            pub fn pipeline(input: Span) -> Res<Span, Pipeline> {
                context(
                    "Pipeline",
                    many_until0(pipeline_segment, tuple((multispace0, tag(";")))),
                )(input)
                .map(|(next, segments)| (next, Pipeline { segments }))
            }

            pub fn consume_pipeline(input: Span) -> Res<Span, Pipeline> {
                all_consuming(pipeline)(input)
            }

            pub fn entity_selectors(input: Span) -> Res<Span, Vec<Selector<EntityPattern>>> {
                many0(delimited(multispace0, entity_selector, multispace0))(input)
            }

            pub fn msg_selectors(input: Span) -> Res<Span, Vec<Selector<MsgPattern>>> {
                select0(
                    delimited(multispace0, msg_selector, multispace0),
                    padded_curly_close,
                )(input)
            }

            pub fn http_pipelines(input: Span) -> Res<Span, Vec<Selector<HttpPattern>>> {
                select0(
                    delimited(multispace0, http_pipeline, multispace0),
                    padded_curly_close,
                )(input)
            }

            pub fn rc_selectors(input: Span) -> Res<Span, Vec<Selector<RcPattern>>> {
                select0(
                    delimited(multispace0, rc_selector, multispace0),
                    padded_curly_close,
                )(input)
            }

            pub fn entity_selector(input: Span) -> Res<Span, Selector<EntityPattern>> {
                tuple((entity_pattern, multispace0, pipeline, tag(";")))(input).map(
                    |(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)),
                )
            }

            pub fn msg_selector(input: Span) -> Res<Span, Selector<MsgPattern>> {
                tuple((msg_pattern_scoped, multispace0, pipeline, tag(";")))(input).map(
                    |(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)),
                )
            }

            pub fn http_pipeline(input: Span) -> Res<Span, Selector<HttpPattern>> {
                tuple((http_pattern_scoped, multispace0, pipeline, tag(";")))(input).map(
                    |(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)),
                )
            }

            pub fn rc_selector(input: Span) -> Res<Span, Selector<RcPattern>> {
                tuple((rc_pattern_scoped, multispace0, pipeline, tag(";")))(input).map(
                    |(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)),
                )
            }

            pub fn consume_selector(input: Span) -> Res<Span, Selector<EntityPattern>> {
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
        use crate::version::v0_0_1::id::{GenericKind, GenericKindBase, Meta, Point};
        use crate::version::v0_0_1::payload::{Errors, HttpMethod, Payload, Primitive};
        use crate::version::v0_0_1::selector::TksPattern;
        use crate::version::v0_0_1::util::ValueMatcher;
        use http::status::InvalidStatusCode;
        use http::{HeaderMap, Request, StatusCode, Uri};
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Action {
            Rc(Rc),
            Http(#[serde(with = "http_serde::method")] HttpMethod),
            Msg(String),
        }

        impl ToString for Action {
            fn to_string(&self) -> String {
                match self {
                    Action::Rc(_) => "Rc".to_string(),
                    Action::Http(method) => method.to_string(),
                    Action::Msg(msg) => msg.to_string(),
                }
            }
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
                    body: Payload::Bin(request.body().clone()),
                }
            }
        }

        impl TryInto<http::Request<Bin>> for RequestCore {
            type Error = MsgErr;

            fn try_into(self) -> Result<http::Request<Bin>, MsgErr> {
                let mut builder = http::Request::builder();
                for (name, value) in self.headers {
                    match name {
                        Some(name) => {
                            builder =
                                builder.header(name.as_str(), value.to_str()?.to_string().as_str());
                        }
                        None => {}
                    }
                }
                match self.action {
                    Action::Http(method) => {
                        builder = builder.method(method).uri(self.uri);
                        Ok(builder.body(self.body.to_bin()?)?)
                    }
                    _ => Err("cannot convert to http response".into()),
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
                    body: Payload::Errors(errors),
                }
            }

            pub fn err<E: StatusErr>(&self, error: E) -> ResponseCore {
                let errors = Errors::default(error.message().as_str());
                let status = match StatusCode::from_u16(error.status()) {
                    Ok(status) => status,
                    Err(_) => StatusCode::from_u16(500u16).unwrap(),
                };
                println!("----->   returning STATUS of {}", status.as_str());
                ResponseCore {
                    headers: Default::default(),
                    status,
                    body: Payload::Errors(errors),
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
                    Rc::Create(create) => { Ok(create.template.point.parent.clone()) }
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
            use crate::version::v0_0_1::id::Point;
            use serde::{Deserialize, Serialize};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Set {
                pub point: Point,
                pub properties: SetProperties,
            }
        }

        pub mod get {
            use crate::version::v0_0_1::command::common::SetProperties;
            use crate::version::v0_0_1::id::Point;
            use serde::{Deserialize, Serialize};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Get {
                pub point: Point,
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
            use crate::version::v0_0_1::id::{GenericKind, HostKey, Point, PointSeg};
            use crate::version::v0_0_1::payload::{Payload, Primitive};
            use crate::version::v0_0_1::selector::SpecificSelector;
            use crate::version::v0_0_1::util::ConvertFrom;

            pub enum PointTemplateSeg {
                ExactSeg(PointSeg),
                Wildcard(String),
            }

            impl PointTemplateSeg {
                pub fn is_wildcard(&self) -> bool {
                    match self {
                        PointTemplateSeg::ExactSeg(_) => false,
                        PointTemplateSeg::Wildcard(_) => true,
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Template {
                pub point: PointTemplate,
                pub kind: KindTemplate,
            }

            impl Template {
                pub fn new(point: PointTemplate, kind: KindTemplate) -> Self {
                    Self { point, kind }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct KindTemplate {
                pub resource_type: String,
                pub kind: Option<String>,
                pub specific: Option<SpecificSelector>,
            }

            impl TryInto<GenericKind> for KindTemplate {
                type Error = MsgErr;

                fn try_into(self) -> Result<GenericKind, Self::Error> {
                    if self.specific.is_some() {
                        return Err("cannot create a ResourceKind from a specific pattern when using KindTemplate".into());
                    }
                    Ok(GenericKind {
                        resource_type: self.resource_type,
                        kind: self.kind,
                        specific: None,
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Require {
                File(String),
                Auth(String),
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
                        state: StateSrc::StatefulDirect(Payload::Bin(bin)),
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
            pub struct PointTemplate {
                pub parent: Point,
                pub child_segment_template: PointSegFactory,
            }

            #[derive(Debug, Clone, strum_macros::Display, Serialize, Deserialize)]
            pub enum PointSegFactory {
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
            use crate::version::v0_0_1::id::Point;
            use crate::version::v0_0_1::particle::Stub;
            use crate::version::v0_0_1::payload::{
                MapPattern, Payload, PayloadList, Primitive, PrimitiveType,
            };
            use crate::version::v0_0_1::selector::{Hop, PointKindHierarchy, PointSelector};
            use crate::version::v0_0_1::util::ConvertFrom;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum SelectIntoPayload {
                Stubs,
                Addresses,
            }

            impl SelectIntoPayload {
                pub fn to_primitive(&self, stubs: Vec<Stub>) -> Result<PayloadList, MsgErr> {
                    match self {
                        SelectIntoPayload::Stubs => {
                            let stubs: Vec<Box<Payload>> = stubs
                                .into_iter()
                                .map(|stub| Box::new(Payload::Stub(stub)))
                                .collect();
                            let stubs = PayloadList { list: stubs };
                            Ok(stubs)
                        }
                        SelectIntoPayload::Addresses => {
                            let pointes: Vec<Box<Payload>> = stubs
                                .into_iter()
                                .map(|stub| Box::new(Payload::Point(stub.point)))
                                .collect();
                            let stubs = PayloadList { list: pointes };
                            Ok(stubs)
                        }
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Select {
                pub pattern: PointSelector,
                pub properties: PropertiesPattern,
                pub into_payload: SelectIntoPayload,
                pub kind: SelectKind,
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum SelectKind {
                Initial,
                SubSelect {
                    point: Point,
                    hops: Vec<Hop>,
                    hierarchy: PointKindHierarchy,
                },
            }

            impl Select {
                pub fn sub_select(
                    self,
                    point: Point,
                    hops: Vec<Hop>,
                    hierarchy: PointKindHierarchy,
                ) -> SubSelect {
                    SubSelect {
                        point,
                        pattern: self.pattern,
                        properties: self.properties,
                        into_payload: self.into_payload,
                        hops,
                        hierarchy,
                    }
                }
            }

            impl TryInto<SubSelect> for Select {
                type Error = MsgErr;

                fn try_into(self) -> Result<SubSelect, Self::Error> {
                    if let SelectKind::SubSelect {
                        point,
                        hops,
                        hierarchy,
                    } = self.kind
                    {
                        Ok(SubSelect {
                            point,
                            pattern: self.pattern,
                            properties: self.properties,
                            into_payload: self.into_payload,
                            hops: hops,
                            hierarchy,
                        })
                    } else {
                        Err("Not of kind SubSelector".into())
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct SubSelect {
                pub point: Point,
                pub pattern: PointSelector,
                pub properties: PropertiesPattern,
                pub into_payload: SelectIntoPayload,
                pub hops: Vec<Hop>,
                pub hierarchy: PointKindHierarchy,
            }

            impl Into<Select> for SubSelect {
                fn into(self) -> Select {
                    Select {
                        pattern: self.pattern,
                        properties: self.properties,
                        into_payload: self.into_payload,
                        kind: SelectKind::SubSelect {
                            point: self.point,
                            hops: self.hops,
                            hierarchy: self.hierarchy,
                        },
                    }
                }
            }

            impl SubSelect {
                pub fn sub_select(
                    &self,
                    point: Point,
                    hops: Vec<Hop>,
                    hierarchy: PointKindHierarchy,
                ) -> SubSelect {
                    SubSelect {
                        point,
                        pattern: self.pattern.clone(),
                        properties: self.properties.clone(),
                        into_payload: self.into_payload.clone(),
                        hops,
                        hierarchy,
                    }
                }
            }

            impl Select {
                fn new(pattern: PointSelector) -> Self {
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
            use crate::version::v0_0_1::id::Point;
            use crate::version::v0_0_1::payload::Payload;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Update {
                pub payload: Payload,
            }
        }

        pub mod query {
            use std::convert::TryInto;

            use serde::{Deserialize, Serialize};

            use crate::error::MsgErr;
            use crate::version::v0_0_1::entity::request::Rc;
            use crate::version::v0_0_1::selector::PointKindHierarchy;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Query {
                PointKindHierarchy,
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum QueryResult {
                PointKindHierarchy(PointKindHierarchy),
            }

            impl TryInto<PointKindHierarchy> for QueryResult {
                type Error = MsgErr;

                fn try_into(self) -> Result<PointKindHierarchy, MsgErr> {
                    match self {
                        QueryResult::PointKindHierarchy(hierarchy) => Ok(hierarchy),
                    }
                }
            }

            impl ToString for QueryResult {
                fn to_string(&self) -> String {
                    match self {
                        QueryResult::PointKindHierarchy(hierarchy) => hierarchy.to_string(),
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
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::entity::request::RequestCore;
        use crate::version::v0_0_1::fail;
        use crate::version::v0_0_1::fail::Fail;
        use crate::version::v0_0_1::id::{GenericKind, Meta, Point};
        use crate::version::v0_0_1::messaging::Response;
        use crate::version::v0_0_1::payload::{Errors, Payload, Primitive};
        use crate::version::v0_0_1::util::unique_id;
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
}

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
    use crate::version::v0_0_1::id::{GenericKind, GenericKindBase, Point, PointKind};
    use crate::version::v0_0_1::parse::{point_subst, Res};
    use crate::version::v0_0_1::payload::{Payload, PayloadMap};
    use crate::version::v0_0_1::selector::parse_alpha1_str;
    use crate::version::v0_0_1::Span;

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

    pub fn ok_code(input: Span) -> Res<Span, Code> {
        tag("Ok")(input).map(|(next, code)| (next, Code::Ok))
    }

    pub fn error_code(input: Span) -> Res<Span, Code> {
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

    pub fn code(input: Span) -> Res<Span, Code> {
        alt((error_code, ok_code))(input)
    }

    pub fn status(input: Span) -> Res<Span, Status> {
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
        use crate::version::v0_0_1::id::{GenericKind, GenericKindBase, Point};
        use crate::version::v0_0_1::messaging::{Request, Response};
        use crate::version::v0_0_1::particle::StatusUpdate;
        use crate::version::v0_0_1::payload::Payload;
        use crate::version::v0_0_1::portal;
        use crate::version::v0_0_1::portal::Exchanger;
        use crate::version::v0_0_1::selector::TksPattern;
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
        use crate::version::v0_0_1::config::{Assign, Config, ConfigBody};
        use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
        use crate::version::v0_0_1::id::{GenericKind, GenericKindBase, Point};
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
        use crate::version::v0_0_1::id::Point;

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

pub mod parse {
    use core::fmt;
    use std::collections::HashMap;
    use std::fmt::Formatter;
    use std::ops::{Deref, RangeFrom};
    use std::str::FromStr;

    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::bytes::complete::{is_a, is_not};
    use nom::character::complete::{
        alpha0, alpha1, alphanumeric1, char, digit1, multispace0, multispace1, space0, space1,
    };
    use nom::combinator::{all_consuming, fail, not, opt, peek, recognize, value, verify};
    use nom::error::{context, ContextError, ErrorKind, ParseError, VerboseError};
    use nom::multi::{many0, separated_list0, separated_list1};
    use nom::sequence::{delimited, pair, preceded, terminated, tuple};
    use nom::{
        AsChar, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Slice,
        UnspecializedInput,
    };
    use nom_supreme::parse_from_str;

    use crate::error::MsgErr;
    use crate::version::v0_0_1::command::common::{PropertyMod, SetProperties, StateSrc};
    use crate::version::v0_0_1::config::bind::parse::pipeline_step;
    use crate::version::v0_0_1::entity::request::create::{
        Create, CreateOp, KindTemplate, PointSegFactory, PointTemplate, PointTemplateSeg, Require,
        Strategy, Template,
    };
    use crate::version::v0_0_1::entity::request::get::{Get, GetOp};
    use crate::version::v0_0_1::entity::request::select::{Select, SelectIntoPayload, SelectKind};
    use crate::version::v0_0_1::entity::request::set::Set;
    use crate::version::v0_0_1::id::{CaptureAddress, Point, PointSeg, PointSegKind, PointSubst, RouteSeg, Version};
    use crate::version::v0_0_1::parse::error::{first_context, result};
    use crate::version::v0_0_1::security::{
        AccessGrant, AccessGrantKindDef, AccessGrantKindSubst, AccessGrantSubst, ChildPerms,
        ParticlePerms, Permissions, PermissionsMask, PermissionsMaskKind, Privilege,
    };
    use crate::version::v0_0_1::selector::parse::{
        delim_kind, generic_kind_base, kind, point_selector, specific, specific_selector, version,
    };
    use crate::version::v0_0_1::selector::{
        skewer, upload_step, PointKindHierarchy, PointKindSeg, PointSelector,
    };
    use crate::version::v0_0_1::util::StringMatcher;
    use crate::version::v0_0_1::Span;
    use nom::bytes::complete::take;
    use nom_locate::LocatedSpan;
    use nom_supreme::error::ErrorTree;
    use serde::{Deserialize, Serialize};

/*
pub struct Parser {}

impl Parser {
    pub fn point(input: Span) -> Res<Span, Point> {
        point_subst(input)
    }

    pub fn consume_point(input: Span) -> Result<Point, MsgErr> {
        let (_, point) = all_consuming(point_subst)(input)?;
        Ok(point)
    }
}
 */

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

pub fn local_route_segment(input: Span) -> Res<Span, RouteSeg> {
    alt((recognize(tag(".::")), recognize(not(other_route_segment))))(input)
        .map(|(next, _)| (next, RouteSeg::Local))
}

pub fn domain_route_segment(input: Span) -> Res<Span, RouteSeg> {
    terminated(domain_chars, tag("::"))(input)
        .map(|(next, domain)| (next, RouteSeg::Domain(domain.to_string())))
}

pub fn tag_route_segment(input: Span) -> Res<Span, RouteSeg> {
    terminated(delimited(tag("["), skewer_chars, tag("]")), tag("::"))(input)
        .map(|(next, tag)| (next, RouteSeg::Tag(tag.to_string())))
}

pub fn mesh_route_segment(input: Span) -> Res<Span, RouteSeg> {
    terminated(delimited(tag("<<"), mesh_route_chars, tag(">>")), tag("::"))(input)
        .map(|(next, tag)| (next, RouteSeg::Tag(tag.to_string())))
}

pub fn other_route_segment(input: Span) -> Res<Span, RouteSeg> {
    alt((tag_route_segment, domain_route_segment, mesh_route_segment))(input)
}

pub fn route_segment(input: Span) -> Res<Span, RouteSeg> {
    alt((other_route_segment, local_route_segment))(input)
}

pub fn point_segment(input: Span) -> Res<Span, PointSeg> {
    alt((base_point_segment,space_point_segment,version_point_segment,filesystem_point_segment,file_point_segment))(input)
}


pub fn space_point_segment(input: Span) -> Res<Span, PointSeg> {
    space_chars(input).map(|(next, space)| (next, PointSeg::Space(space.to_string())))
}

pub fn pop_seg(input: Span) -> Res<Span, PointSeg> {
    preceded(tag(":"), tuple((tag(".."), eos)))(input)
        .map(|(next, base)| (next, PointSeg::PopSeg))
}

pub fn pop_dir(input: Span) -> Res<Span, PointSeg> {
    tuple((tag(".."), eos))(input)
        .map(|(next, base)| (next, PointSeg::PopDir))
}

pub fn base_point_segment(input: Span) -> Res<Span, PointSeg> {
    alt((pop_seg,base_point_segment_normalized))(input)
}

pub fn base_point_segment_normalized(input: Span) -> Res<Span, PointSeg> {
    preceded(tag(":"), rec_skewer)(input)
        .map(|(next, base)| (next, PointSeg::Base(base.to_string())))
}

pub fn filesystem_point_segment(input: Span) -> Res<Span, PointSeg> {
    alt((dir_point_segment, file_point_segment))(input)
}

pub fn dir_point_segment(input: Span) -> Res<Span, PointSeg> {
    context("dir_point_segment", terminated(file_chars, tag("/")))(input)
        .map(|(next, dir)| (next, PointSeg::Dir(format!("{}/", dir))))
}

pub fn root_dir_point_segment(input: Span) -> Res<Span, PointSeg> {
    tag(":/")(input).map(|(next, _)| (next, PointSeg::FilesystemRootDir))
}

pub fn file_point_segment(input: Span) -> Res<Span, PointSeg> {
    alt( (pop_dir,file_point_segment_normalized) )(input)
}

pub fn file_point_segment_normalized(input: Span) -> Res<Span, PointSeg> {
    context("file_point_segment", file_chars)(input)
        .map(|(next, filename)| (next, PointSeg::File(filename.to_string())))
}

pub fn version_point_segment(input: Span) -> Res<Span, PointSeg> {
    preceded(tag(":"), version)(input).map(|(next, version)| (next, PointSeg::Version(version)))
}

pub fn root_point_subst(input: Span) -> Res<Span, PointSubst> {
    tuple((var_subst(route_segment), tag("ROOT")))(input).map(|(next, (route, _))| {
        let point = PointSubst {
            route,
            segments: vec![],
        };
        (next, point)
    })
}
pub fn point_subst_non_root(input: Span) -> Res<Span, PointSubst> {

    tuple((
        tuple((var_subst(route_segment), first_seg_subst(space_point_segment))),
        many0(var_subst(base_point_segment)),
        opt(var_subst(version_point_segment)),
        opt(no_subst(root_dir_point_segment)),
        many0(var_subst(filesystem_point_segment)),
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

            let point = PointSubst {
                route: hub,
                segments,
            };

            (next, point)
        },
    )
}


pub fn consume_point(input: &str) -> Result<Point,MsgErr> {
    let span = Span::new(input);
    let point = all_consuming(point)(span)?.1;
    Ok(point)
}

pub fn point(input: Span) -> Res<Span, Point> {
    let (next,point) = point_subst(input.clone())?;
    match point.brute_resolve() {
        Ok(point) => Ok((next,point)),
        Err(err) => {
            let e = ErrorTree::from_error_kind(input.clone(), ErrorKind::Fail );
            let e = ErrorTree::add_context(input, "point-subst-brute-force", e);
            return Err(nom::Err::Failure(e));
        }
    }
}


pub fn point_subst(input: Span) -> Res<Span, PointSubst> {
    alt((root_point_subst, point_subst_non_root))(input)
}

pub fn consume_point_subst(input: Span) -> Res<Span, PointSubst> {
    all_consuming(point_subst)(input)
}

pub fn capture_point(input: Span) -> Res<Span, CaptureAddress> {
    context(
        "Address",
        tuple((
            tuple((
                route_segment,
                alt((root_point_capture_segment, space_point_capture_segment)),
            )),
            many0(base_point_capture_segment),
            opt(version_point_segment),
            opt(root_dir_point_segment),
            many0(filesystem_point_capture_segment),
        )),
    )(input)
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

            let point = CaptureAddress {
                route: hub,
                segments,
            };

            (next, point)
        },
    )
}

pub fn root_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    tag("ROOT")(input).map(|(next, space)| (next, PointSeg::Root))
}

pub fn space_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    space_chars_plus_capture(input)
        .map(|(next, space)| (next, PointSeg::Space(space.to_string())))
}

pub fn base_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    preceded(tag(":"), rec_skewer_capture)(input)
        .map(|(next, base)| (next, PointSeg::Base(base.to_string())))
}

pub fn filesystem_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    alt((dir_point_capture_segment, file_point_capture_segment))(input)
}

pub fn dir_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    context(
        "dir_point_capture_segment",
        terminated(file_chars_plus_capture, tag("/")),
    )(input)
    .map(|(next, dir)| (next, PointSeg::Dir(format!("{}/", dir))))
}

pub fn file_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    context("file_point_capture_segment", file_chars_plus_capture)(input)
        .map(|(next, filename)| (next, PointSeg::File(filename.to_string())))
}

pub fn space_point_kind_segment(input: Span) -> Res<Span, PointKindSeg> {
    tuple((space_point_segment, delim_kind))(input).map(|(next, (point_segment, kind))| {
        (
            next,
            PointKindSeg {
                segment: point_segment,
                kind,
            },
        )
    })
}

pub fn base_point_kind_segment(input: Span) -> Res<Span, PointKindSeg> {
    tuple((base_point_segment, delim_kind))(input).map(|(next, (point_segment, kind))| {
        (
            next,
            PointKindSeg {
                segment: point_segment,
                kind,
            },
        )
    })
}

pub fn filepath_point_kind_segment(input: Span) -> Res<Span, PointKindSeg> {
    alt((file_point_kind_segment, dir_point_kind_segment))(input)
}
pub fn dir_point_kind_segment(input: Span) -> Res<Span, PointKindSeg> {
    tuple((dir_point_segment, delim_kind))(input).map(|(next, (point_segment, kind))| {
        (
            next,
            PointKindSeg {
                segment: point_segment,
                kind,
            },
        )
    })
}

pub fn file_point_kind_segment(input: Span) -> Res<Span, PointKindSeg> {
    tuple((file_point_segment, delim_kind))(input).map(|(next, (point_segment, kind))| {
        (
            next,
            PointKindSeg {
                segment: point_segment,
                kind,
            },
        )
    })
}

pub fn version_point_kind_segment(input: Span) -> Res<Span, PointKindSeg> {
    tuple((version_point_segment, delim_kind))(input).map(|(next, (point_segment, kind))| {
        (
            next,
            PointKindSeg {
                segment: point_segment,
                kind,
            },
        )
    })
}

pub fn consume_hierarchy(input: Span) -> Result<PointKindHierarchy, MsgErr> {
    let (_, rtn) = all_consuming(point_kind_hierarchy)(input)?;
    Ok(rtn)
}

pub fn point_kind_hierarchy(input: Span) -> Res<Span, PointKindHierarchy> {
    tuple((
        tuple((route_segment, space_point_kind_segment)),
        many0(base_point_kind_segment),
        opt(version_point_kind_segment),
        many0(file_point_kind_segment),
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

        let point = PointKindHierarchy::new(hub, segments);

        (next, point)
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

pub fn skewer_colon<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == ':')
                && !((char_item.is_alpha() && char_item.is_lowercase())
                    || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn skewer_dot<I,E>(i: I) -> IResult<I,I,E>
where
    I: InputTakeAtPosition + nom::InputLength,
    <I as InputTakeAtPosition>::Item: AsChar,
    E: nom::error::ContextError<I>+nom::error::ParseError<I>,
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

pub fn point_segment_chars<T>(i: T) -> Res<T, T>
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

pub fn rec_skewer(input: Span) -> Res<Span, Span> {
    recognize(tuple((lowercase1, opt(skewer_chars))))(input)
}

pub fn rec_skewer_capture(input: Span) -> Res<Span, Span> {
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

pub fn not_space(input: Span) -> Res<Span, Span> {
    is_not(" \n\r\t")(input)
}

pub fn path(input: Span) -> Res<Span, Span> {
    recognize(tuple((tag("/"), opt(filepath_chars))))(input)
}

pub fn capture_path(input: Span) -> Res<Span, Span> {
    recognize(tuple((tag("/"), opt(file_chars_plus_capture))))(input)
}

pub fn consume_path(input: Span) -> Res<Span, Span> {
    all_consuming(path)(input)
}

pub fn path_regex(input: Span) -> Res<Span, Span> {
    recognize(opt(not_space))(input)
}
pub fn camel_case(input: Span) -> Res<Span, Span> {
    recognize(tuple((is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alpha0)))(input)
    //recognize(alpha1)(input)
}

pub fn camel_case_to_string_matcher(input: Span) -> Res<Span, StringMatcher> {
    camel_case(input).map(|(next, camel)| (next, StringMatcher::new(camel.to_string())))
}

fn parse_version_major_minor_patch(input: Span) -> Res<Span, (Span, Span, Span)> {
    context(
        "version_major_minor_patch",
        tuple((
            terminated(digit1, tag(".")),
            terminated(digit1, tag(".")),
            terminated(digit1, not(digit1)),
        )),
    )(input)
}

pub fn parse_version(input: Span) -> Res<Span, ((Span, Span, Span), Option<Span>)> {
    tuple((
        parse_version_major_minor_patch,
        opt(preceded(tag("-"), skewer_chars)),
    ))(input)
}

pub fn rec_version(input: Span) -> Res<Span, Span> {
    recognize(parse_version)(input)
}

pub fn base_point_segment_wildcard(input: Span) -> Res<Span, PointTemplateSeg> {
    preceded(
        tag(":"),
        recognize(tuple((many0(skewer), tag("%"), many0(skewer)))),
    )(input)
    .map(|(next, base)| (next, PointTemplateSeg::Wildcard(base.to_string())))
}

pub fn base_point_segment_template(input: Span) -> Res<Span, PointTemplateSeg> {
    preceded(tag(":"), rec_skewer)(input).map(|(next, base)| {
        (
            next,
            PointTemplateSeg::ExactSeg(PointSeg::Base(base.to_string())),
        )
    })
}

pub fn filepath_point_segment_wildcard(input: Span) -> Res<Span, PointTemplateSeg> {
    recognize(tuple((
        many0(filepath_chars),
        tag("%"),
        many0(filepath_chars),
    )))(input)
    .map(|(next, base)| (next, PointTemplateSeg::Wildcard(base.to_string())))
}

pub fn filepath_point_segment_template(input: Span) -> Res<Span, PointTemplateSeg> {
    filesystem_point_segment(input)
        .map(|(next, segment)| (next, PointTemplateSeg::ExactSeg(segment)))
}

pub fn point_template(input: Span) -> Res<Span, PointTemplate> {
    let (next, ((hub, space), mut bases, version, root, mut files)) = tuple((
        tuple((route_segment, space_point_segment)),
        many0(alt((
            base_point_segment_wildcard,
            base_point_segment_template,
        ))),
        opt(version_point_segment),
        opt(root_dir_point_segment),
        many0(alt((
            filepath_point_segment_wildcard,
            filepath_point_segment_template,
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
            PointTemplateSeg::ExactSeg(exact) => PointSegFactory::Exact(exact.to_string()),
            PointTemplateSeg::Wildcard(pattern) => PointSegFactory::Pattern(pattern),
        }
    } else if root.is_some() {
        PointSegFactory::Exact("/".to_string())
    } else if let Option::Some(version) = &version {
        PointSegFactory::Exact(version.to_string())
    } else if !bases.is_empty() {
        match bases.remove(bases.len() - 1) {
            PointTemplateSeg::ExactSeg(exact) => PointSegFactory::Exact(exact.to_string()),
            PointTemplateSeg::Wildcard(pattern) => PointSegFactory::Pattern(pattern),
        }
    } else {
        space_last = true;
        PointSegFactory::Exact(space.to_string())
    };

    let mut bases: Vec<PointSeg> = bases
        .into_iter()
        .map(|b| match b {
            PointTemplateSeg::ExactSeg(seg) => seg,
            PointTemplateSeg::Wildcard(_) => {
                panic!("should have filtered wildcards already!")
            }
        })
        .collect();

    let mut files: Vec<PointSeg> = files
        .into_iter()
        .map(|b| match b {
            PointTemplateSeg::ExactSeg(seg) => seg,
            PointTemplateSeg::Wildcard(_) => {
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

    let point = Point {
        route: hub,
        segments,
    };

    let point_template = PointTemplate {
        parent: point,
        child_segment_template: last,
    };

    Ok((next, point_template))
}

pub fn kind_template(input: Span) -> Res<Span, KindTemplate> {
    tuple((
        generic_kind_base,
        opt(delimited(
            tag("<"),
            tuple((
                camel_case,
                opt(delimited(tag("<"), specific_selector, tag(">"))),
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

pub fn template(input: Span) -> Res<Span, Template> {
    tuple((point_template, delimited(tag("<"), kind_template, tag(">"))))(input)
        .map(|(next, (point, kind))| (next, Template { point, kind }))
}

pub fn set_property_mod(input: Span) -> Res<Span, PropertyMod> {
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

pub fn set_property_mod_lock(input: Span) -> Res<Span, PropertyMod> {
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

pub fn property_value_not_space_or_comma(input: Span) -> Res<Span, Span> {
    is_not(" \n\r\t,")(input)
}

pub fn property_value_single_quotes(input: Span) -> Res<Span, Span> {
    delimited(tag("'"), is_not("'"), tag("'"))(input)
}

pub fn property_value_double_quotes(input: Span) -> Res<Span, Span> {
    delimited(tag("\""), is_not("\""), tag("\""))(input)
}

pub fn property_value(input: Span) -> Res<Span, Span> {
    alt((
        property_value_single_quotes,
        property_value_double_quotes,
        property_value_not_space_or_comma,
    ))(input)
}

pub fn unset_property_mod(input: Span) -> Res<Span, PropertyMod> {
    tuple((tag("!"), skewer_dot))(input)
        .map(|(next, (_, name))| (next, PropertyMod::UnSet(name.to_string())))
}

pub fn property_mod(input: Span) -> Res<Span, PropertyMod> {
    alt((set_property_mod, unset_property_mod))(input)
}

pub fn set_properties(input: Span) -> Res<Span, SetProperties> {
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

pub fn get_properties(input: Span) -> Res<Span, Vec<String>> {
    separated_list0(tag(","), tuple((multispace0, skewer, multispace0)))(input).map(
        |(next, keys)| {
            let keys: Vec<String> = keys.iter().map(|(_, key, _)| key.to_string()).collect();
            (next, keys)
        },
    )
}

pub fn create(input: Span) -> Res<Span, Create> {
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

pub fn set(input: Span) -> Res<Span, Set> {
    tuple((point, delimited(tag("{"), set_properties, tag("}"))))(input).map(
        |(next, (point, properties))| {
            let set = Set { point, properties };
            (next, set)
        },
    )
}

pub fn get(input: Span) -> Res<Span, Get> {
    tuple((point, opt(delimited(tag("{"), get_properties, tag("}")))))(input).map(
        |(next, (point, keys))| {
            let op = match keys {
                None => GetOp::State,
                Some(keys) => GetOp::Properties(keys),
            };
            let get = Get { point, op };

            (next, get)
        },
    )
}

pub fn select(input: Span) -> Res<Span, Select> {
    point_selector(input).map(|(next, point_kind_pattern)| {
        let select = Select {
            pattern: point_kind_pattern,
            properties: Default::default(),
            into_payload: SelectIntoPayload::Stubs,
            kind: SelectKind::Initial,
        };
        (next, select)
    })
}

pub fn publish(input: Span) -> Res<Span, CreateOp> {
    let (next, (upload, _, point)) = tuple((upload_step, space1, point))(input)?;

    let parent = match point.parent() {
        None => {
            return Err(nom::Err::Error(ErrorTree::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }
        Some(parent) => parent,
    };

    let last = match point.last_segment() {
        None => {
            return Err(nom::Err::Error(ErrorTree::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }
        Some(last) => last,
    };

    let template = Template {
        point: PointTemplate {
            parent,
            child_segment_template: PointSegFactory::Exact(last.to_string()),
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Symbol {
    Var(String),
    Ctx(Ctx)
}

impl Symbol {

    pub fn named(name: String) -> Self{
        Self::Var(name)
    }

    pub fn ctx(ctx: Ctx) -> Self{
        Self::Ctx(ctx)
    }
}



#[derive(Debug,Clone,Serialize,Deserialize)]
pub enum Ctx{
    WorkingPoint,
    WorkingPointOneAbove
}

impl ToString for Ctx{
    fn to_string(&self) -> String {
        match self {
            Ctx::WorkingPoint => ".".to_string(),
            Ctx::WorkingPointOneAbove => "..".to_string()
        }
    }
}

pub trait Resolver {
    fn string(&self, symbol: &Symbol ) -> Result<String, MsgErr> {
        match symbol {
            Symbol::Var(var) => self.val(var.as_str() ),
            Symbol::Ctx(ctx) => self.ctx(ctx)
        }
    }

    fn val(&self, id: &str) -> Result<String, MsgErr> {
        Err(format!("variable '{}' not found", id).into())
    }
    fn ctx(&self, ctx: &Ctx) -> Result<String, MsgErr> {
        Err(format!("context operation '{}' not available here", ctx.to_string()).into())
    }
}

pub struct NoResolver;

impl NoResolver{
    pub fn new() -> Self {
        Self{}
    }
}

impl Resolver for NoResolver{
    fn val(&self, id: &str) -> Result<String, MsgErr> {
        Err(format!("unexpected variable '{}'. variables not expected here",id).into())
    }
    fn ctx(&self, ctx: &Ctx) -> Result<String, MsgErr> {
        match ctx {
            Ctx::WorkingPoint => {
                Err("context operators '.' (reference to WorkingPoint) not expected here".into())
            }
            Ctx::WorkingPointOneAbove => {
                Err("context operator '..' (reference to WorkingPointAboveOne) not expected here".into())
            }
        }
    }
}


pub struct MapResolver {
    pub working_point: Point,
    pub map: HashMap<String, String>,
}

impl MapResolver {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            working_point: Point::root()
        }
    }

    pub fn insert(&mut self, key: &str, value: &str) {
        self.map.insert(key.to_string(), value.to_string());
    }
}

impl Resolver for MapResolver {
    fn val(&self, id: &str) -> Result<String, MsgErr> {
        self.map
            .get(id)
            .cloned()
            .ok_or(format!("variable not found: '{}'", id).into())
    }

    fn ctx(&self, ctx: &Ctx) -> Result<String, MsgErr> {
        match ctx {
            Ctx::WorkingPoint => Ok(self.working_point.to_string()),
            Ctx::WorkingPointOneAbove => Ok(self.working_point.parent().ok_or("already at the root point cannot pop any further.")?.to_string())
        }
    }
}

pub trait ToResolved<Resolved> where Self: Sized {
    fn to_resolved(self, resolver: &dyn Resolver) -> Result<Resolved, MsgErr>;
    fn to_resolved_str(self, resolver: &dyn Resolver) -> Result<String, MsgErr> {
        Err("string representation not supported".into())
    }
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Subst<Resolved>
where
    Resolved: Clone + FromStr<Err = MsgErr>
{
    Symbol(Symbol),
    Resolved(Resolved),
}

pub trait BruteResolver<Resolved> where Self: Sized+ToResolved<Resolved>
{
    fn brute_resolve(self) -> Result<Resolved,MsgErr> {
        let resolver = NoResolver::new();
        Ok(self.to_resolved(&resolver)?)
    }
}



impl<Resolved> ToResolved<Resolved> for Subst<Resolved>
where
    Resolved: Clone + FromStr<Err = MsgErr>+ToString
{
    fn to_resolved(self, resolver: &dyn Resolver) -> Result<Resolved, MsgErr> {
        match self {
            Subst::Symbol(symbol) => {
                let raw = resolver.string(&symbol)?;
                Resolved::from_str(raw.as_str())
            }
            Subst::Resolved(val) => Ok(val.clone()),
        }
    }

    fn to_resolved_str(self, resolver: &dyn Resolver) -> Result<String, MsgErr> {
        match &self {
            Subst::Symbol(var) => Ok(resolver.string(var)?),
            Subst::Resolved(val) => Ok(val.to_string()),
        }
    }
}

/*
pub fn subst<'a,I,O,F>(
    mut f: F,
) -> impl FnMut(I) -> Res<I,Subst<O>>
    where F: 'static+nom::Parser<String, O, ErrorTree<String>>+Clone,O: Clone, I: Clone+ToString
{
    move |input:I| {
        let result = variable(Span::new(input.to_string().as_str()) );

        match result {
            Ok((next, variable)) => {
                Ok((next, Subst::Variable { variable, /*parser: Box::new(f.clone())*/ }))
            }
            Err(err) => {
                match first_context(err) {
                    Ok((context, err)) => {
                        // this means that a $ was encountered, but the variable was invalid...
                        // we don't want to confuse with additional info from the parser
                        if context == "variable" {
                            return Err(err);
                        }
                    }
                    Err(_) => {
                        // looks like the error was something unrelated to variables... let's
                        // give the next parser a chance
                    }
                }
                match f.parse(input.to_string()) {
                    Ok((next, v)) => {
                        Ok((Span::new(next.as_str()), Subst::Value(v)))
                    }
                    Err(err) => {
                        Err(nom::Err::Error(ErrorTree::Alt(vec!())))
                    }
                }
            }
        }
    }
}
 */


pub fn no_subst<I: Clone, O, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Subst<O>, E>
    where
        I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
        <I as InputTakeAtPosition>::Item: AsChar,
        F: nom::Parser<I, O, E>,
        E: nom::error::ContextError<I>,
        O: Clone + FromStr<Err = MsgErr>,
{
    move |input: I| { f.parse(input).map(|(next, val)| (next, Subst::Resolved(val))) }
}


pub fn var_subst<I: Clone, O, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Subst<O>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    F: nom::Parser<I, O, E>,
    E: nom::error::ContextError<I>,
    O: Clone + FromStr<Err = MsgErr>,
{
    move |input: I| match var(input.clone()) {
        Ok((next, v)) => Ok((next, Subst::Symbol(v))),
        Err(err) => f
            .parse(input.clone())
            .map(|(next, val)| (next, Subst::Resolved(val))),
    }
}


pub fn first_seg_subst<I: Clone, O, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Subst<O>, E>
    where
        I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
        <I as InputTakeAtPosition>::Item: AsChar+Clone,
        F: nom::Parser<I, O, E>,
        E: nom::error::ContextError<I>,
        O: Clone + FromStr<Err = MsgErr>,
{
    move |input: I| match alt((ctx, var))(input.clone()) {
        Ok((next, v)) => Ok((next, Subst::Symbol(v))),
        Err(err) => f
            .parse(input.clone())
            .map(|(next, val)| (next, Subst::Resolved(val))),
    }
}

// end of segment
pub fn eos<I,E>(input: I) -> IResult<I, (), E>
where
    I: ToString
        + Clone
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar+Clone,
    E: nom::error::ContextError<I>+nom::error::ParseError<I>,

{
    peek(alt((tag("/"), tag(":"), space1)))(input).map(|(next,_)|{
        (next,())
    })
}

pub fn var<I>(input: I) -> Res<I, Symbol>
where
    I: ToString
        + Clone
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    preceded(
        tag("$"),
        context("variable", delimited(tag("("), variable_name, tag(")"))),
    )(input)
    .map(|(next, name)| (next, Symbol::named(name.to_string())))
}

pub fn ctx<I>(input: I) -> Res<I, Symbol>
    where
        I: ToString
        + Clone
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + InputTakeAtPosition,
        <I as InputTakeAtPosition>::Item: AsChar+Clone,
{
    alt((
        value(
            Ctx::WorkingPointOneAbove,
            tuple((tag(".."), eos)),
        ),
        value(
            Ctx::WorkingPoint,
            tuple((tag("."), eos)),
        ),
    ))(input).map(|(next, ctx)| (next, Symbol::ctx(ctx)))
}

pub fn variable_name<I,E>(input: I) -> IResult<I, I,E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    E: nom::error::ContextError<I>+nom::error::ParseError<I>
{
    skewer_dot(input).map(|(next, name)| (next, name))
}

pub fn access_grant(input: Span) -> Res<Span, AccessGrantSubst> {
    tuple((
        access_grant_kind,
        context(
            "access_grant:on",
            pair(
                delimited(multispace1, tag("on"), multispace1),
                var_subst(point_selector),
            ),
        ),
        context(
            "access_grant:to",
            pair(
                delimited(multispace1, tag("to"), multispace1),
                var_subst(point_selector),
            ),
        ),
    ))(input)
    .map(|(next, (kind, (_, on_point), (_, to_point)))| {
        let by_particle = Subst::Symbol(Symbol::named("by-particle".to_string()));
        (
            next,
            AccessGrantSubst {
                kind,
                on_point,
                to_point,
                by_particle,
            },
        )
    })
}

pub fn access_grant_kind(input: Span) -> Res<Span, AccessGrantKindSubst> {
    tuple((
        context(
            "access_grant_kind",
            peek(alt((
                tuple((tag("perm"), space1)),
                tuple((tag("priv"), space1)),
            ))),
        ),
        alt((access_grant_kind_perm, access_grant_kind_priv)),
    ))(input)
    .map(|(next, (_, kind))| (next, kind))
}

pub fn access_grant_kind_priv(input: Span) -> Res<Span, AccessGrantKindSubst> {
    tuple((
        tag("priv"),
        context("access_grant:priv", tuple((space1, var_subst(privilege)))),
    ))(input)
    .map(|(next, (_, (_, privilege)))| (next, AccessGrantKindDef::Privilege(privilege)))
}

pub fn access_grant_kind_perm(input: Span) -> Res<Span, AccessGrantKindSubst> {
    tuple((
        tag("perm"),
        context(
            "access_grant:perm",
            tuple((space1, var_subst(permissions_mask))),
        ),
    ))(input)
    .map(|(next, (_, (_, perms)))| (next, AccessGrantKindDef::PermissionsMask(perms)))
}

pub fn privilege(input: Span) -> Res<Span, Privilege> {
    context("privilege", alt((tag("*"), skewer_colon)))(input).map(|(next, prv)| {
        let prv = match prv.to_string().as_str() {
            "*" => Privilege::Full,
            prv => Privilege::Single(prv.to_string()),
        };
        (next, prv)
    })
}

pub fn permissions_mask(input: Span) -> Res<Span, PermissionsMask> {
    context(
        "permissions_mask",
        tuple((
            alt((
                value(PermissionsMaskKind::Or, char('+')),
                value(PermissionsMaskKind::And, char('&')),
            )),
            permissions,
        )),
    )(input)
    .map(|(next, (kind, permissions))| {
        let mask = PermissionsMask { kind, permissions };

        (next, mask)
    })
}

pub fn permissions(input: Span) -> Res<Span, Permissions> {
    context(
        "permissions",
        tuple((child_perms, tag("-"), particle_perms)),
    )(input)
    .map(|(next, (child, _, particle))| {
        let permissions = Permissions { child, particle };
        (next, permissions)
    })
}

pub fn child_perms(input: Span) -> Res<Span, ChildPerms> {
    context(
        "child_perms",
        alt((
            tuple((
                alt((value(false, char('c')), value(true, char('C')))),
                alt((value(false, char('s')), value(true, char('S')))),
                alt((value(false, char('d')), value(true, char('D')))),
            )),
            fail,
        )),
    )(input)
    .map(|(next, (create, select, delete))| {
        let block = ChildPerms {
            create,
            select,
            delete,
        };
        (next, block)
    })
}

pub fn particle_perms(input: Span) -> Res<Span, ParticlePerms> {
    context(
        "particle_perms",
        tuple((
            alt((value(false, char('r')), value(true, char('R')))),
            alt((value(false, char('w')), value(true, char('W')))),
            alt((value(false, char('x')), value(true, char('X')))),
        )),
    )(input)
    .map(|(next, (read, write, execute))| {
        let block = ParticlePerms {
            read,
            write,
            execute,
        };
        (next, block)
    })
}

/*
pub fn grant<I>(input: I) -> Res<I,AccessGrant> where I:Clone+InputIter+InputLength+InputTake{

}

 */

pub mod error {
    use crate::version::v0_0_1::Span;
    use nom::Err;
    use nom_supreme::error::{ErrorTree, StackContext};

    pub fn result<R>(result: Result<(Span, R), Err<ErrorTree<Span>>>) -> Result<R, String> {
        match result {
            Ok((_, e)) => Ok(e),
            Err(err) => match find(&err) {
                Ok((message, span)) => Err(format!(
                    "parse error (line {}, column {}): {}",
                    span.location_line(),
                    span.get_column(),
                    message
                )),
                Err(err) => Err(format!(
                    "parsing error: could not find matching error message"
                )),
            },
        }
    }

    pub fn just_msg<R, E: From<String>>(
        result: Result<(Span, R), Err<ErrorTree<Span>>>,
    ) -> Result<R, E> {
        match result {
            Ok((_, e)) => Ok(e),
            Err(err) => match find(&err) {
                Ok((message, _)) => Err(E::from(message)),
                Err(err) => Err(E::from(format!(
                    "parsing error: could not find matching error message"
                ))),
            },
        }
    }

    fn message(context: &str) -> Result<String, ()> {
        let message =match context {
          "variable" => "variable name must be alphanumeric lowercase, dashes and dots.  Variables are preceded by the '$' operator and must be sorounded by parenthesis $(env.valid-variable-name)",
          "child_perms" => "expecting child permissions form csd (Create, Select, Delete) uppercase indicates set permission (CSD==full permission, csd==no permission)",
          "particle_perms" => "expecting particle permissions form rwx (Read, Write, Execute) uppercase indicates set permission (RWX==full permission, rwx==no permission)",
          "permissions" => "expecting permissions form 'csd-rwx' (Create,Select,Delete)-(Read,Write,Execute) uppercase indicates set permission (CSD-RWX==full permission, csd-rwx==no permission)",
          "permissions_mask" => "expecting permissions mask symbol '+' for 'Or' mask and '&' for 'And' mask. Example:  &csd-RwX removes ----R-X from current permission",
          "privilege" => "privilege name must be '*' for 'full' privileges or an alphanumeric lowercase, dashes and colons i.e. 'props:email:read'",
          "access_grant:perm" => "expecting permissions mask symbol '+' for 'Or' mask and '&' for 'And' mask. Example:  &csd-RwX removes ----R-X from current permission",
          "access_grant:priv" => "privilege name must be '*' for 'full' privileges or an alphanumeric lowercase, dashes and colons i.e. 'props:email:read'",
          "access_grant:on" => "expecting grant 'on' i.e.: 'grant perm +cSd+RwX on localhost:app:** to localhost:app:users:**<User>'",
          "access_grant:to" => "expecting grant 'to' i.e.: 'grant perm +cSd+RwX on localhost:app:** to localhost:app:users:**<User>'",
           "point-subst-brute-force" => "not expecting variables or working point context '.'/'..' in this point",
          "access_grant_kind" => {

              "expecting access grant kind ['super','perm','priv']"
          },
          what => {
              println!("don't have a message for context '{}'",what);
              return Err(());
          }
      };
        Ok(message.to_string())
    }

    pub fn find<I: Clone>(err: &Err<ErrorTree<I>>) -> Result<(String, I), ()> {
        match err {
            Err::Incomplete(_) => Err(()),
            Err::Error(err) => find_tree(err),
            Err::Failure(err) => find_tree(err),
        }
    }

    pub fn find_tree<I: Clone>(err: &ErrorTree<I>) -> Result<(String, I), ()> {
        match err {
            ErrorTree::Stack { base, contexts } => {
                let (span, context) = contexts.first().unwrap();
                match context {
                    StackContext::Context(context) => {
                        println!("Context: {}", context);
                        match message(*context) {
                            Ok(message) => Ok((message, span.clone())),
                            Err(_) => Err(()),
                        }
                    }
                    _ => Err(()),
                }
            }
            ErrorTree::Base { .. } => Err(()),
            ErrorTree::Alt(alts) => {
                for alt in alts {
                    match find_tree(alt) {
                        Ok(r) => {
                            return Ok(r);
                        }
                        Err(_) => {}
                    }
                }

                Err(())
            }
        }
    }

    pub fn first_context<I>(
        orig: Err<ErrorTree<I>>,
    ) -> Result<(String, Err<ErrorTree<I>>), ()> {
        match &orig {
            Err::Error(err) => match err {
                ErrorTree::Stack { base, contexts } => {
                    let (_, context) = contexts.first().unwrap();
                    match context {
                        StackContext::Context(context) => Ok((context.to_string(), orig)),
                        _ => Err(()),
                    }
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
}
}

pub mod log {
use crate::version::v0_0_1::id::Point;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Level {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Log {
    pub point: Point,
    pub message: String,
    pub level: Level,
}

impl ToString for Log {
    fn to_string(&self) -> String {
        format!("{} {}", self.point.to_string(), self.message)
    }
}

impl Log {
    pub fn trace(point: Point, message: &str) -> Self {
        Self {
            level: Level::Trace,
            point,
            message: message.to_string(),
        }
    }
    pub fn debug(point: Point, message: &str) -> Self {
        Self {
            level: Level::Debug,
            point,
            message: message.to_string(),
        }
    }
    pub fn info(point: Point, message: &str) -> Self {
        Self {
            level: Level::Info,
            point,
            message: message.to_string(),
        }
    }
    pub fn warn(point: Point, message: &str) -> Self {
        Self {
            level: Level::Warn,
            point,
            message: message.to_string(),
        }
    }
    pub fn error(point: Point, message: &str) -> Self {
        Self {
            level: Level::Error,
            point,
            message: message.to_string(),
        }
    }
}

pub trait ParticleLogger {
    fn log(&self, log: Log);
}
}

#[cfg(test)]
pub mod test {
use http::Uri;
use std::collections::HashMap;
use std::str::FromStr;

use nom::combinator::{all_consuming, recognize};

use crate::error::MsgErr;
use crate::version::v0_0_1::config::bind::parse::{
    bind, final_bind, http_pipeline, pipeline, pipeline_step, pipeline_stop,
    select_http_pipelines,
};
use crate::version::v0_0_1::config::bind::{PipelinesSubScope, ProtoBind};
use crate::version::v0_0_1::entity::request::{Action, RequestCore};
use crate::version::v0_0_1::id::{Point, PointSeg, RouteSeg};
use crate::version::v0_0_1::parse::error::{find, result};
use crate::version::v0_0_1::parse::{
    access_grant, access_grant_kind, base_point_segment, camel_case, capture_point,
    child_perms, create, file_point_capture_segment, particle_perms, permissions,
    permissions_mask, point_subst, point_template, publish, rec_skewer, route_segment, skewer_chars,
    var_subst, version_point_segment, MapResolver, Res, ToResolved,
};
use crate::version::v0_0_1::payload::{HttpMethod, Payload};
use crate::version::v0_0_1::security::{
    ChildPerms, ParticlePerms, Permissions, PermissionsMask, PermissionsMaskKind,
};
use crate::version::v0_0_1::selector::parse::point_selector;
use crate::version::v0_0_1::selector::parse::version;
use crate::version::v0_0_1::selector::{
    http_method, http_method_pattern, http_pattern, http_pattern_scoped, upload_step,
    PointKindHierarchy, PointSelector,
};
use crate::version::v0_0_1::util::ValueMatcher;
use crate::version::v0_0_1::Span;
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
    match all_consuming(rec_skewer)(Span::new("317")) {
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
        (Span::new(""), RouteSeg::Local),
        all_consuming(route_segment)(Span::new(""))?
    );

    all_consuming(point_subst)(Span::new("[root]"))?;
    all_consuming(point_subst)(Span::new("hello:kitty"))?;
    all_consuming(point_subst)(Span::new("hello.com:kitty"))?;
    all_consuming(point_subst)(Span::new("hello:kitty:/file.txt"))?;
    all_consuming(point_subst)(Span::new("hello.com:kitty:/file.txt"))?;
    all_consuming(point_subst)(Span::new("hello.com:kitty:/"))?;
    //all_consuming(point)("hello.com:kitty:/greater-glory/file.txt")?;
    all_consuming(point_subst)(Span::new("hello.com:kitty:base"))?;

    all_consuming(version)(Span::new("1.0.0"))?;
    let (next, version) = all_consuming(version_point_segment)(Span::new(":1.2.3"))?;
    println!("next: '{}' segment: '{}'", next, version.to_string());
    all_consuming(point_subst)(Span::new("hello.com:bundle:1.2.3"))?;
    let (next, addy) = all_consuming(point_subst)(Span::new("hello.com:bundle:1.2.3:/"))?;
    println!("{}", addy.last_segment().unwrap().to_string());
    let (next, addy) = all_consuming(point_subst)(Span::new("hello.com:bundle:1.2.3"))?;
    //       let (next, addy) = all_consuming(point)("hello.com:bundle:1.2.3:/some/file.txt")?;
    let (next, addy) =
        all_consuming(point_subst)(Span::new("hello.com:bundle:1.2.3:/greater-glory/file.txt"))?;
    println!("{}", addy.to_string());
    println!("{}", addy.parent().unwrap().to_string());
    println!("{}", addy.last_segment().unwrap().to_string());

    Ok(())
}

#[test]
pub fn test_point_template() -> Result<(), MsgErr> {
    all_consuming(point_template)(Span::new("hello:kitty"))?;
    all_consuming(point_template)(Span::new("hello:kitty-%"))?;
    all_consuming(point_template)(Span::new("hello:kitty:bob:/some-%-time"))?;
    Ok(())
}

#[test]
pub fn test_create() -> Result<(), MsgErr> {
    all_consuming(create)(Span::new("hello:kitty<App>"))?;
    all_consuming(create)(Span::new(
        "hello:kitty<App>{ +config='some:config:1.0.0:/blah.conf' }",
    ))?;
    Ok(())
}

#[test]
pub fn test_publish() -> Result<(), MsgErr> {
    let (_, block) = all_consuming(upload_step)(Span::new("^[ bundle.zip ]->"))?;
    assert_eq!("bundle.zip", block.name.as_str());
    all_consuming(publish)(Span::new("^[ bundle.zip ]-> space.org:hello:1.0.0"))?;
    Ok(())
}

#[test]
pub fn test_pipeline_stop() -> Result<(), MsgErr> {
    pipeline_stop(Span::new("apps:my-app^Http<Get>/*"))?;
    Ok(())
}

#[test]
pub fn test_pipeline() -> Result<(), MsgErr> {
    pipeline(Span::new("-> apps:my-app^Http<Get>/users/$1 => &"))?;
    pipeline(Span::new("-> apps:bundle:1.0.0:/html/$1 => &"))?;
    Ok(())
}

#[test]
pub fn test_capture_point() -> Result<(), MsgErr> {
    file_point_capture_segment(Span::new("$1"))?;

    let point = all_consuming(capture_point)(Span::new("apps:bundle:1.0.0:/html/$1"))?.1;
    let regex = Regex::new("/(.*)")?;
    let point = point.to_point(regex.captures("/index.html").unwrap())?;
    Ok(())
}

#[test]
pub fn test_point_kind_pattern() -> Result<(), MsgErr> {
    all_consuming(point_selector)(Span::new("*"))?;
    all_consuming(point_selector)(Span::new("space"))?;
    all_consuming(point_selector)(Span::new("space:base"))?;
    all_consuming(point_selector)(Span::new("space:my-files:/"))?;
    all_consuming(point_selector)(Span::new("space:my-files:/file.txt"))?;
    all_consuming(point_selector)(Span::new("space:my-files:/dir/file.txt"))?;
    all_consuming(point_selector)(Span::new("space<Space>:base"))?;
    all_consuming(point_selector)(Span::new("**:*<Base>"))?;
    all_consuming(point_selector)(Span::new("space<Space>:base<Base>"))?;
    all_consuming(point_selector)(Span::new("space:base:blah"))?;
    all_consuming(point_selector)(Span::new("space:base:*"))?;
    all_consuming(point_selector)(Span::new("space<Space>:**<Base>"))?;
    all_consuming(point_selector)(Span::new("space:series:1.0.0:/some/file.txt"))?;
    all_consuming(point_selector)(Span::new("space+"))?;
    all_consuming(point_selector)(Span::new("space+:**"))?;
    Ok(())
}

#[cfg(test)]
pub mod bind {
    use crate::error::MsgErr;
    use crate::version::v0_0_1::config::bind::parse::{bind, final_bind};
    use crate::version::v0_0_1::config::bind::ProtoBind;
    use crate::version::v0_0_1::Span;
}

#[test]
pub fn test_http_pattern() -> Result<(), MsgErr> {
    let any_pattern = all_consuming(http_pattern)(Span::new("Http<*>"))?.1;
    let get_any_pattern = all_consuming(http_pattern)(Span::new("Http<Get>"))?.1;
    let get_some_pattern = all_consuming(http_pattern)(Span::new("Http<Get>^/some$"))?.1;
    let get_some_star_pattern = all_consuming(http_pattern)(Span::new("Http<Get>^/some/*"))?.1;
    let get_some_capture_pattern =
        all_consuming(http_pattern)(Span::new("Http<Get>^/some/(.*)"))?.1;

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

    let get_selector = all_consuming(http_pipeline)(Span::new("<Get> -> {{}};"))?.1;
    let get_some_selector =
        all_consuming(http_pipeline)(Span::new("<Get>^/some -> {{}} => &;"))?.1;
    let post_some_selector =
        all_consuming(http_pipeline)(Span::new("<Post>^/some -> {{}} => &;"))?.1;

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

    let section = all_consuming(select_http_pipelines)(Span::new(
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
    http_method(Span::new("Get"))?;
    assert!(http_method(Span::new("Bad")).is_err());
    Ok(())
}

#[test]
pub fn test_http_method_pattern() -> Result<(), MsgErr> {
    http_method_pattern(Span::new("*"))?;
    http_method_pattern(Span::new("Get"))?;
    assert!(http_method_pattern(Span::new("Bad")).is_err());
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
    let span = Span::new("cSd-RwX");
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
    let span = Span::new("&cSd-RwX");
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
    let span = Span::new("Csd-rxy");
    println!("{}", result(permissions(span)).err().unwrap());
    Ok(())
}

#[test]
pub fn test_perm_mask() -> Result<(), MsgErr> {
    let span = Span::new("Csd-rxy");
    println!("{}", result(permissions_mask(span)).err().unwrap());
    Ok(())
}

#[test]
pub fn test_subst() -> Result<(), MsgErr> {
    let val = Span::new("&cSd-RwX");
    let var = Span::new("$(env.some-var)");
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
    let s = Span::new("perm &rCsd-rwx");
    let result = result(access_grant_kind(s))?;

    Ok(())
}

#[test]
pub fn test_access_grant() -> Result<(), MsgErr> {
    let s = Span::new("perm &rCsd-rwx xon localhost:app");
    let result = result(access_grant(s))?;

    Ok(())
}
}
