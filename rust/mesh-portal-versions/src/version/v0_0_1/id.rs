pub mod id {
    use nom::bytes::complete::tag;
    use nom::combinator::{all_consuming, opt};
    use nom::sequence::{delimited, tuple};
    use regex::Captures;
    use std::collections::HashMap;
    use std::convert::TryInto;
    use std::fmt::Formatter;
    use std::ops::{Deref, Range};
    use std::str::FromStr;
    use std::sync::Arc;

    use semver::SemVerError;
    use serde::de::Visitor;
    use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

    use crate::error::{MsgErr, ParseErrs};
    use crate::version::v0_0_1::id::id::PointSegCtx::Working;
    use crate::version::v0_0_1::parse::{camel_case, consume_point, consume_point_ctx, Ctx, CtxResolver, CtxSubst, kind, point_and_kind, point_route_segment, Res, VarResolver, VarResolverErr, VarSubst};

    use crate::version::v0_0_1::parse::error::result;
    use crate::version::v0_0_1::selector::selector::{
        Pattern, PointSelector, SpecificSelector, VersionReq,
    };
    use crate::version::v0_0_1::span::{new_span, SpanExtra};
    use crate::version::v0_0_1::wrap::Span;

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
            let point_and_kind: PointKind = result(all_consuming(point_and_kind)(new_span(s)))?;
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

    impl RouteSeg {
        pub fn is_local(&self) -> bool {
            match self {
                RouteSeg::Local => true,
                _ => false,
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum RouteSegVar {
        Local,
        Domain(String),
        Tag(String),
        Mesh(String),
        Var(Variable)
    }

    impl Into<RouteSegVar> for RouteSeg {
        fn into(self) -> RouteSegVar {
            match self {
                RouteSeg::Local => RouteSegVar::Local,
                RouteSeg::Domain(domain) => RouteSegVar::Domain(domain),
                RouteSeg::Tag(tag) => RouteSegVar::Tag(tag),
                RouteSeg::Mesh(mesh) => RouteSegVar::Mesh(mesh)
            }
        }
    }

    impl FromStr for RouteSeg {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let s = new_span(s);
            Ok(all_consuming(point_route_segment)(s)?.1)
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
        Pop,
        Working,
        Var,
    }

    impl PointSegKind {
        pub fn preceding_delim(&self, post_fileroot: bool) -> &'static str {
            match self {
                Self::Space => "",
                Self::Base => ":",
                Self::Dir => "",
                Self::File => "",
                Self::Version => ":",
                Self::FilesystemRootDir => ":",
                Self::Root => "",
                Self::Pop => match post_fileroot {
                    true => "",
                    false => ":",
                },
                Self::Working => match post_fileroot {
                    true => "",
                    false => ":",
                },
                PointSegKind::Var => match post_fileroot {
                    true => "",
                    false => ":"
                }
            }
        }

        pub fn is_normalized(&self) -> bool {
            match self {
                Self::Pop => false,
                Self::Working => false,
                Self::Var => false,
                _ => true,
            }
        }

        pub fn is_version(&self) -> bool {
            match self {
                Self::Version => true,
                _ => false,
            }
        }

        pub fn is_file(&self) -> bool {
            match self {
                Self::File => true,
                _ => false,
            }
        }

        pub fn is_dir(&self) -> bool {
            match self {
                Self::Dir => true,
                _ => false,
            }
        }


        pub fn is_filesystem_seg(&self) -> bool {
            match self {
                PointSegKind::Root => false,
                PointSegKind::Space => false,
                PointSegKind::Base => false,
                PointSegKind::FilesystemRootDir => true,
                PointSegKind::Dir => true,
                PointSegKind::File => true,
                PointSegKind::Version => false,
                PointSegKind::Pop => true,
                PointSegKind::Working => true,
                PointSegKind::Var => true
            }
        }

        pub fn is_mesh_seg(&self) -> bool {
            match self {
                PointSegKind::Root => true,
                PointSegKind::Space => true,
                PointSegKind::Base => true,
                PointSegKind::FilesystemRootDir => false,
                PointSegKind::Dir => false,
                PointSegKind::File => false,
                PointSegKind::Version => true,
                PointSegKind::Pop => true,
                PointSegKind::Working => true,
                PointSegKind::Var => true
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct Variable {
        pub range: Range<usize>,
        pub name: String,
        pub extra: SpanExtra
    }

    impl Variable {
        pub fn new( name: String, range: Range<usize>, extra: SpanExtra ) -> Self {
            Self {
               name,
               range,
               extra,
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum PointSegCtx {
        Root,
        Space(String),
        Base(String),
        FilesystemRootDir,
        Dir(String),
        File(String),
        Version(Version),
        Working,
        Pop,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum PointSegVar{
        Root,
        Space(String),
        Base(String),
        FilesystemRootDir,
        Dir(String),
        File(String),
        Version(Version),
        Working,
        Pop,
        Var(Variable),
    }

    impl ToString for PointSegVar {
        fn to_string(&self) -> String {
            match self {
                PointSegVar::Root => "".to_string(),
                PointSegVar::Space(space) => space.clone(),
                PointSegVar::Base(base) => base.clone(),
                PointSegVar::FilesystemRootDir => "/".to_string(),
                PointSegVar::Dir(dir) => dir.clone(),
                PointSegVar::File(file) => file.clone(),
                PointSegVar::Version(version) => version.to_string(),
                PointSegVar::Working => ".".to_string(),
                PointSegVar::Pop => "..".to_string(),
                PointSegVar::Var(var) => format!("${{{}}}",var.name )
            }
        }
    }

    impl PointSegVar{
        pub fn kind(&self) -> PointSegKind {
            match self {
                Self::Root => PointSegKind::Root,
                Self::Space(_) => PointSegKind::Space,
                Self::Base(_) => PointSegKind::Base,
                Self::FilesystemRootDir => PointSegKind::FilesystemRootDir,
                Self::Dir(_) => PointSegKind::Dir,
                Self::File(_) => PointSegKind::File,
                Self::Version(_) => PointSegKind::Version,
                Self::Pop => PointSegKind::Pop,
                Self::Working => PointSegKind::Working,
                Self::Var(_) => PointSegKind::Var
            }
        }

        pub fn is_normalized(&self) -> bool {
            self.kind().is_normalized()
        }

        pub fn is_filesystem_seg(&self) -> bool {
            self.kind().is_filesystem_seg()
        }
    }

    impl Into<PointSegVar> for PointSegCtx {
        fn into(self) -> PointSegVar {
            match self {
                PointSegCtx::Root => PointSegVar::Root,
                PointSegCtx::Space(space) => PointSegVar::Space(space),
                PointSegCtx::Base(base) => PointSegVar::Base(base),
                PointSegCtx::FilesystemRootDir => PointSegVar::FilesystemRootDir,
                PointSegCtx::Dir(dir) => PointSegVar::Dir(dir),
                PointSegCtx::File(file) => PointSegVar::File(file),
                PointSegCtx::Version(version) => PointSegVar::Version(version),
                PointSegCtx::Working => PointSegVar::Working,
                PointSegCtx::Pop => PointSegVar::Pop
            }
        }
    }

    impl TryInto<PointSeg> for PointSegVar {
        type Error = MsgErr;

        fn try_into(self) -> Result<PointSeg, Self::Error> {
            match self {
                PointSegVar::Root => Ok(PointSeg::Root),
                PointSegVar::Space(space) => Ok(PointSeg::Space(space)),
                PointSegVar::Base(base) => Ok(PointSeg::Base(base)),
                PointSegVar::FilesystemRootDir => Ok(PointSeg::FilesystemRootDir),
                PointSegVar::Dir(dir) => Ok(PointSeg::Dir(dir)),
                PointSegVar::File(file) => Ok(PointSeg::File(file)),
                PointSegVar::Version(version) => Ok(PointSeg::Version(version)),
                PointSegVar::Working => {
                    Err("cannot convert working directory operator '.' into a point segment without context".into())
                }
                PointSegVar::Pop => {
                    Err("cannot convert pop working directory operator '..' into a point segment without context".into())
                }
                PointSegVar::Var(var) => {
                    Err(format!("cannot resolve var '{}'",var.name).into())
                }
            }
        }
    }



    impl TryInto<PointSeg> for PointSegCtx {
        type Error = MsgErr;

        fn try_into(self) -> Result<PointSeg, Self::Error> {
            match self {
                PointSegCtx::Root => Ok(PointSeg::Root),
                PointSegCtx::Space(space) => Ok(PointSeg::Space(space)),
                PointSegCtx::Base(base) => Ok(PointSeg::Base(base)),
                PointSegCtx::FilesystemRootDir => Ok(PointSeg::FilesystemRootDir),
                PointSegCtx::Dir(dir) => Ok(PointSeg::Dir(dir)),
                PointSegCtx::File(file) => Ok(PointSeg::File(file)),
                PointSegCtx::Version(version) => Ok(PointSeg::Version(version)),
                PointSegCtx::Working => {
                    Err("cannot convert working directory operator '.' into a point segment without context".into())
                }
                PointSegCtx::Pop => {
                    Err("cannot convert pop working directory operator '..' into a point segment without context".into())
                }
            }
        }
    }

    impl PointSegCtx {
        pub fn kind(&self) -> PointSegKind {
            match self {
                Self::Root => PointSegKind::Root,
                Self::Space(_) => PointSegKind::Space,
                Self::Base(_) => PointSegKind::Base,
                Self::FilesystemRootDir => PointSegKind::FilesystemRootDir,
                Self::Dir(_) => PointSegKind::Dir,
                Self::File(_) => PointSegKind::File,
                Self::Version(_) => PointSegKind::Version,
                Self::Pop => PointSegKind::Pop,
                Self::Working => PointSegKind::Working,
            }
        }

        pub fn is_normalized(&self) -> bool {
            self.kind().is_normalized()
        }

        pub fn is_filesystem_seg(&self) -> bool {
            self.kind().is_filesystem_seg()
        }
    }

    pub trait PointSegment {

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
    }

    impl PointSegment for PointSeg {

    }

    impl PointSegment for PointSegCtx {

    }

    impl PointSegment for PointSegVar {

    }


    impl Into<PointSegCtx> for PointSeg {
            fn into(self) -> PointSegCtx {
                match self {
                    PointSeg::Root => PointSegCtx::Root,
                    PointSeg::Space(space) => PointSegCtx::Space(space),
                    PointSeg::Base(base) => PointSegCtx::Base(base),
                    PointSeg::FilesystemRootDir => PointSegCtx::FilesystemRootDir,
                    PointSeg::Dir(dir) => PointSegCtx::Dir(dir),
                    PointSeg::File(file) => PointSegCtx::File(file),
                    PointSeg::Version(version) => PointSegCtx::Version(version),
                }
            }
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
            }
        }

        pub fn is_file(&self) -> bool {
            self.kind().is_file()
        }

        pub fn is_normalized(&self) -> bool {
            self.kind().is_normalized()
        }

        pub fn is_version(&self) -> bool {
            self.kind().is_version()
        }

        pub fn is_filesystem_seg(&self) -> bool {
            self.kind().is_filesystem_seg()
        }
        pub fn preceding_delim(&self, post_fileroot: bool) -> &str {
            self.kind().preceding_delim(post_fileroot)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum PointSegDelim {
        Empty,
        Mesh,
        File,
    }

    impl ToString for PointSegDelim {
        fn to_string(&self) -> String {
            match self {
                PointSegDelim::Empty => "".to_string(),
                PointSegDelim::Mesh => ":".to_string(),
                PointSegDelim::File => "/".to_string(),
            }
        }
    }

    pub type PointSegPair = PointSegPairDef<PointSeg>;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PointSegPairDef<Seg> {
        pub delim: PointSegDelim,
        pub seg: Seg,
    }

    impl<Seg> PointSegPairDef<Seg> {
        pub fn new(delim: PointSegDelim, seg: Seg) -> Self {
            Self { delim, seg }
        }
    }

    impl<Seg> ToString for PointSegPairDef<Seg>
    where
        Seg: ToString,
    {
        fn to_string(&self) -> String {
            format!("{}{}", self.delim.to_string(), self.seg.to_string())
        }
    }

    /*
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

        pub fn preceding_delim(&self, filesystem: bool) -> &'static str {
            self.kind().preceding_delim(filesystem)
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
                PointSeg::Pop => false,
            }
        }
    }

     */

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
            }
        }
    }

    pub type Point = PointDef<RouteSeg, PointSeg>;
    pub type PointCtx = PointDef<RouteSeg, PointSegCtx>;
    pub type PointVar = PointDef<RouteSegVar, PointSegVar>;

    impl VarSubst<PointCtx> for PointVar {
        fn resolve_vars(self, resolver: &dyn VarResolver) -> Result<PointCtx, MsgErr> {
            let mut rtn = String::new();
            let mut after_fs = false;
            let mut errs = vec![];
            for (index,segment) in self.segments.iter().enumerate()  {
                if let PointSegVar::Var(var) = segment {
                    match resolver.val(var.name.as_str()) {
                        Ok(val) => {
                            if index > 0 {
                                if after_fs {
                                    rtn.push_str("/");
                                } else {
                                    rtn.push_str(":");
                                }
                            }
                            rtn.push_str(val.as_str());
                        }
                        Err(err) => {
                            errs.push(ParseErrs::from_range(format!("variable could not be resolved '{}'",var.name).as_str(), "Not Found", var.range.clone(), var.extra.clone() ));
                        }
                    }
                } else if PointSegVar::FilesystemRootDir == *segment {
                   after_fs = true;
                   rtn.push_str(":/");
                } else {
                    if index > 0 {
                        if after_fs {
                            rtn.push_str("/");
                        } else {
                            rtn.push_str(":");
                        }
                        rtn.push_str(segment.to_string().as_str() );
                    }
                }
            }
            if self.is_dir() {
                rtn.push_str("/");
            }

            if !errs.is_empty() {
                let errs = ParseErrs::fold(errs);
                return Err(errs.into());
            }

            consume_point_ctx(rtn.as_str())
        }
    }

    impl CtxSubst<Point> for PointCtx {
        fn resolve_ctx(self, resolver: &dyn CtxResolver) -> Result<Point, MsgErr> {
            if self.segments.is_empty() {
                return Ok(Point {
                    route: self.route,
                    segments: vec![],
                });
            }

            let mut segments = vec![];

            let mut old = self;
            let first_segment = old.segments.remove(0);
            let point = match first_segment {
                PointSegCtx::Working => resolver.ctx(&Ctx::WorkingPoint)?,
                PointSegCtx::Pop => resolver.ctx(&Ctx::WorkingPointPop)?,
                PointSegCtx::Root => "".to_string(),
                PointSegCtx::Space(space) => space,
                PointSegCtx::Base(base) => base,
                PointSegCtx::FilesystemRootDir => "/".to_string(),
                PointSegCtx::Dir(dir) => dir,
                PointSegCtx::File(file) => file,
                PointSegCtx::Version(version) => version.to_string(),
            };

            let mut point = consume_point(point.as_str())?;

            let mut filesystem_count = 0;
            for segment in old.segments {
                match segment {
                    PointSegCtx::Working => {}
                    PointSegCtx::Pop => {
                        let segment = segments.pop();
                        if let Option::Some(PointSeg::FilesystemRootDir) = segment {
                            filesystem_count = filesystem_count - 1;
                        }
                    }
                    PointSegCtx::FilesystemRootDir => {
                        filesystem_count = filesystem_count + 1;
                        segments.push(PointSeg::FilesystemRootDir);
                    }

                    PointSegCtx::Root => {
                        //segments.push(PointSeg::Root)
                    }
                    PointSegCtx::Space(space) => segments.push(PointSeg::Space(space)),
                    PointSegCtx::Base(base) => segments.push(PointSeg::Base(base)),
                    PointSegCtx::Dir(dir) => segments.push(PointSeg::Dir(dir)),
                    PointSegCtx::File(file) => segments.push(PointSeg::File(file)),
                    PointSegCtx::Version(version) => segments.push(PointSeg::Version(version)),
                }
            }

            let mut point_builder = String::new();
            point_builder.push_str(point.to_string().as_str());
            let mut post_filesystem = point.has_filesystem();
            for segment in segments {
                point_builder.push_str(segment.kind().preceding_delim(post_filesystem));
                point_builder.push_str(segment.to_string().as_str());
                if segment.kind() == PointSegKind::FilesystemRootDir {
                    post_filesystem = true;
                }
            }

            let point = consume_point(point_builder.as_str())?;

            Ok(point)
        }
    }

    impl TryInto<Point> for PointCtx {
        type Error = MsgErr;

        fn try_into(self) -> Result<Point, Self::Error> {
            let mut rtn = vec![];
            for segment in self.segments {
                rtn.push(segment.try_into()?);
            }
            Ok(Point {
                route: self.route,
                segments: rtn,
            })
        }
    }

    impl TryFrom<String> for Point {
        type Error = MsgErr;

        fn try_from(value: String) -> Result<Self, Self::Error> {
            consume_point(value.as_str())
        }
    }

    impl TryFrom<&str> for Point {
        type Error = MsgErr;

        fn try_from(value: &str) -> Result<Self, Self::Error> {
            consume_point(value)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct PointDef<Route, Seg> {
        pub route: Route,
        pub segments: Vec<Seg>,
    }

    impl Point {
        pub fn normalize(self) -> Result<Point, MsgErr> {
            if self.is_normalized() {
                return Ok(self);
            }

            if !self
                .segments
                .first()
                .expect("expected first segment")
                .is_normalized()
            {
                return Err(format!("absolute point paths cannot begin with '..' (reference parent segment) because there is no working point segment: '{}'",self.to_string()).into());
            }

            let mut segments = vec![];
            for seg in &self.segments {
                match seg.is_normalized() {
                    true => segments.push(seg.clone()),
                    false => {
                        if segments.pop().is_none() {
                            return Err(format!(
                                "'..' too many pop segments directives: out of parents: '{}'",
                                self.to_string()
                            )
                            .into());
                        }
                    }
                }
            }
            Ok(Point {
                route: self.route,
                segments,
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

        pub fn has_filesystem(&self) -> bool {
            for segment in &self.segments {
                match segment {
                    PointSeg::FilesystemRootDir => {
                        return true;
                    }
                    _ => {}
                }
            }
            false
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

        pub fn push_segment(&self, segment: PointSeg) -> Result<Self, MsgErr> {
            if (self.has_filesystem() && segment.is_filesystem_seg())
                || segment.kind().is_mesh_seg()
            {
                let mut point = self.clone();
                point.segments.push(segment);
                Ok(point)
            } else {
                if self.has_filesystem() {
                    Err("cannot push a Mesh segment onto a point after the FileSystemRoot segment has been pushed".into())
                } else {
                    Err("cannot push a FileSystem segment onto a point until after the FileSystemRoot segment has been pushed".into())
                }
            }
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
                last_segment.is_filesystem_seg()
            } else {
                false
            }
        }
    }

    impl FromStr for Point {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            consume_point(s)
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
        pub fn to_string_impl(&self, show_route: bool) -> String {
            let mut rtn = String::new();

            if show_route {
                rtn.push_str(self.route.to_string().as_str());
            }

            let mut post_fileroot = false;

            if self.segments.is_empty() {
                "ROOT".to_string()
            } else {
                for (i, segment) in self.segments.iter().enumerate() {
                    if let PointSeg::FilesystemRootDir = segment {
                        post_fileroot = true;
                    }
                    if i > 0 {
                        rtn.push_str(segment.kind().preceding_delim(post_fileroot));
                    }
                    rtn.push_str(segment.to_string().as_str());
                }
                rtn.to_string()
            }
        }
    }

    impl ToString for Point {
        fn to_string(&self) -> String {
            self.to_string_impl(!self.route.is_local())
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

    impl PointVar {
        pub fn is_dir(&self) -> bool {
            self.segments.last().unwrap_or(&PointSegVar::Root).kind().is_dir()
        }
    }

    impl PointCtx {
        pub fn is_dir(&self) -> bool {
            self.segments.last().unwrap_or(&PointSegCtx::Root).kind().is_dir()
        }
    }


    /*
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
                        unimplemented!()
                        //                        rtn.push_str(segment.preceding_delim());
                    }
                }
                rtn.to_string()
            }
        }
    }

     */

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct GenericKind {
        pub kind: GenericKindBase,
        pub sub_kind: Option<String>,
        pub specific: Option<Specific>,
    }

    impl ToString for GenericKind {
        fn to_string(&self) -> String {
            if self.sub_kind.is_some() && self.specific.is_some() {
                format!(
                    "{}<{}<{}>>",
                    self.kind.to_string(),
                    self.sub_kind.as_ref().expect("kind"),
                    self.specific.as_ref().expect("specific").to_string()
                )
            } else if self.sub_kind.is_some() {
                format!(
                    "{}<{}>",
                    self.kind.to_string(),
                    self.sub_kind.as_ref().expect("kind")
                )
            } else {
                self.kind.to_string()
            }
        }
    }

    impl FromStr for GenericKind {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_, kind) = all_consuming(kind)(new_span(s))?;

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
                kind: resource_type,
                sub_kind: kind,
                specific,
            }
        }
    }

    impl Tks for GenericKind {
        fn resource_type(&self) -> GenericKindBase {
            self.kind.clone()
        }

        fn kind_to_string(&self) -> Option<String> {
            self.sub_kind.clone()
        }

        fn specific(&self) -> Option<Specific> {
            self.specific.clone()
        }

        fn matches(&self, tks: &dyn Tks) -> bool {
            self.kind == tks.resource_type()
                && self.sub_kind == tks.kind_to_string()
                && self.specific == tks.specific()
        }
    }
}
