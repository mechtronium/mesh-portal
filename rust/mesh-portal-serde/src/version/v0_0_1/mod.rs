use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::version::v0_0_1::bin::Bin;

pub type State = HashMap<String, Bin>;

pub type ArtifactRef = String;
pub type Artifact = Arc<Vec<u8>>;
pub type Port = String;

pub mod id {
    use std::collections::HashMap;
    use std::str::FromStr;

    use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

    use crate::error::Error;
    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::parse::{address, consume_address, Res};
    use semver::SemVerError;
    use serde::de::Visitor;
    use std::convert::TryInto;
    use std::fmt::Formatter;
    use std::ops::Deref;

    pub type ResourceType = String;
    pub type Kind = generic::id::GenericKind<ResourceType>;
    pub type AddressAndKind = generic::id::AddressAndKind<Kind>;
    pub type AddressAndType = generic::id::AddressAndType<ResourceType>;
    pub type Meta = HashMap<String, String>;
    pub type PayloadClaim = String;

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
    pub trait Tks<ResourceType>
    where
        ResourceType: Eq + PartialEq,
    {
        fn resource_type(&self) -> ResourceType;
        fn kind_to_string(&self) -> Option<String>;
        fn specific(&self) -> Option<Specific>;
        fn matches(&self, tks: &dyn Tks<ResourceType>) -> bool;
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

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum RouteSegment {
        Resource,
        Domain(String),
        Tag(String),
        Mesh(String)
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
        Space(String),
        Base(String),
        Dir(String),
        File(String),
        Version(Version)
    }

    impl AddressSegment {
        pub fn terminating_delim(&self) -> &str {
            match self {
                AddressSegment::Space(_) => ":",
                AddressSegment::Base(_) => ":",
                AddressSegment::Dir(_) => "/",
                AddressSegment::File(_) => "",
                AddressSegment::Version(_) => ":",
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
                AddressSegment::Version(version) => {version.to_string()}
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct Address {
        pub route: RouteSegment,
        pub segments: Vec<AddressSegment>,
    }

    impl Address {
        pub fn push( &self, segment: String ) -> Result<Self,Error> {
            Self::from_str(format!("{}:{}",self.to_string(),segment).as_str())
        }

        pub fn push_file( &self, segment: String ) -> Result<Self,Error> {
            Self::from_str(format!("{}{}",self.to_string(),segment).as_str())
        }

        pub fn push_segment( &self, segment: AddressSegment ) -> Self {
            let mut address = self.clone();
            address.segments.push(segment);
            address
        }

        pub fn last_segment(&self) -> Option<AddressSegment> {
            self.segments.last().cloned()
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

            for (i, segment) in self.segments.iter().enumerate() {
                rtn.push_str(segment.to_string().as_str());
                if i != self.segments.len() - 1 {
                    rtn.push_str(segment.terminating_delim());
                }
            }
            rtn.to_string()
        }
    }

    impl Address {
        pub fn parent(&self) -> Option<Address> {
            if self.segments.is_empty() {
                return Option::None;
            }
            let mut segments = self.segments.clone();
            segments.remove(segments.len());
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

        pub fn is_root(&self) -> bool {
            self.segments.is_empty()
        }
    }
}

pub mod pattern {
    use crate::error::Error;
    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Kind, ResourceType, Specific, AddressSegment};
    use crate::version::v0_0_1::pattern::specific::{
        ProductPattern, VariantPattern, VendorPattern,
    };
    use crate::version::v0_0_1::util::ValueMatcher;
    use crate::{Deserialize, Serialize};
    use semver::ReqParseError;
    use serde::de::Visitor;
    use serde::{de, Deserializer, Serializer};
    use std::convert::TryInto;
    use std::fmt::Formatter;
    use std::ops::Deref;
    use std::str::FromStr;

    pub type TksPattern = generic::pattern::TksPattern<ResourceType, Kind>;
    pub type KindPattern = generic::pattern::KindPattern<Kind>;
    pub type AddressKindPattern = generic::pattern::AddressKindPattern<ResourceType,Kind>;

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
    }

    impl SegmentPattern {
        pub fn is_exact(&self) -> bool {
            match self {
                SegmentPattern::Exact(_) => {
                    true
                }
                _ => {
                    false
                }
            }
        }

        pub fn matches(&self, segment: &AddressSegment ) -> bool {
            match self {
                SegmentPattern::Any => true,
                SegmentPattern::Recursive => true,
                SegmentPattern::Exact(exact) => match exact {
                    ExactSegment::Address(pattern) => *pattern == *segment,
                },
            }
        }

        pub fn is_recursive(&self) -> bool {
            match self {
                SegmentPattern::Any => false,
                SegmentPattern::Recursive => true,
                SegmentPattern::Exact(_) => false,
            }
        }
    }

    pub type KeySegment = String;

    #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
    pub enum ExactSegment {
        Address(AddressSegment),
    }

    impl ExactSegment {
        pub fn matches(&self, segment: &AddressSegment) -> bool {
            match self {
                ExactSegment::Address(s) => *s == *segment,
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
        use crate::error::Error;
        use crate::version::v0_0_1::generic::pattern::Pattern;
        use std::ops::Deref;
        use std::str::FromStr;

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

        use crate::version::v0_0_1::generic::pattern::{Hop, KindPattern, Pattern, ResourceTypePattern, TksPattern, EmptyPattern};
        use crate::version::v0_0_1::parse::{
            address_segment_chars, camel_case, domain_chars, skewer_chars, version_req_chars, Res,
        };
        use crate::version::v0_0_1::pattern::specific::{
            ProductPattern, VariantPattern, VendorPattern,
        };
        use crate::version::v0_0_1::pattern::{
            ExactSegment, SegmentPattern, SpecificPattern, VersionReq,
        };
        use crate::version::v0_0_1::util::ValuePattern;
        use nom::branch::alt;
        use nom::bytes::complete::tag;
        use nom::character::complete::{alpha1, digit1};
        use nom::combinator::{opt, recognize, all_consuming};
        use nom::error::{ParseError, VerboseError};
        use nom::multi::many1;
        use nom::sequence::{delimited, terminated, tuple};
        use nom::Parser;
        use nom::{Err, IResult};
        use nom_supreme::{parse_from_str, ParserExt};
        use std::str::FromStr;
        use crate::version::v0_0_1::id::AddressSegment;
        use crate::error::Error;

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
                    SegmentPattern::Exact(ExactSegment::Address(AddressSegment::Space(segment.to_string()))),
                )
            })
        }


        fn exact_base_segment(input: &str) -> Res<&str, SegmentPattern> {
            address_segment_chars(input).map(|(next, segment)| {
                (
                    next,
                    SegmentPattern::Exact(ExactSegment::Address(AddressSegment::Base(segment.to_string()))),
                )
            })
        }

        fn exact_file_segment(input: &str) -> Res<&str, SegmentPattern> {
            address_segment_chars(input).map(|(next, segment)| {
                (
                    next,
                    SegmentPattern::Exact(ExactSegment::Address(AddressSegment::File(segment.to_string()))),
                )
            })
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

        fn specific(input: &str) -> Res<&str, SpecificPattern> {
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

        fn rec_kind(input: &str) -> Res<&str, &str> {
            recognize(tuple((camel_case,opt(delimited(tag("<"),tuple((camel_case,opt(delimited(tag("<"), specific, tag(">"))))),tag(">"))))))(input)
        }

        pub fn kind<Kind: FromStr>(input: &str) -> Res<&str, Kind> {
            parse_from_str(rec_kind)
                .parse(input)
                .map(|(next, kind)| (next, kind))
        }

        pub fn delim_kind<Kind: FromStr>(input: &str) -> Res<&str, Kind> {
            delimited(tag("<"),kind,tag(">"))(input)
        }

        pub fn consume_kind<Kind: FromStr>(input: &str) -> Result<Kind,Error> {
            let (_,kind) = all_consuming(kind )(input)?;
            Ok(kind)
        }


        pub fn kind_pattern<Kind: FromStr>(input: &str) -> Res<&str, KindPattern<Kind>> {
            pattern(kind)(input).map(|(next, kind)| (next, kind))
        }

        pub fn resource_type<ResourceType: FromStr>(input: &str) -> Res<&str, ResourceType> {
            parse_from_str(camel_case).parse(input)
        }

        pub fn resource_type_pattern<ResourceType: FromStr>(
            input: &str,
        ) -> Res<&str, ResourceTypePattern<ResourceType>> {
            pattern(resource_type)(input)
        }

        pub fn tks<ResourceType: FromStr, Kind: FromStr>(
            input: &str,
        ) -> Res<&str, TksPattern<ResourceType, Kind>> {
            delimited(
                tag("<"),
                tuple((
                    resource_type_pattern,
                    opt(delimited(
                        tag("<"),
                        tuple((
                            kind_pattern,
                            opt(delimited(tag("<"), value_pattern(specific), tag(">"))),
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

        fn space_hop<ResourceType: FromStr, Kind: FromStr>(
            input: &str,
        ) -> Res<&str, Hop<ResourceType, Kind>> {
            tuple((space_segment, opt(tks)))(input).map(|(next, (segment, tks))| {
                let tks = match tks {
                    None => TksPattern::any(),
                    Some(tks) => tks,
                };
                (next, Hop { segment, tks })
            })
        }

        fn base_hop<ResourceType: FromStr, Kind: FromStr>(
            input: &str,
        ) -> Res<&str, Hop<ResourceType, Kind>> {
            tuple((base_segment, opt(tks)))(input).map(|(next, (segment, tks))| {
                let tks = match tks {
                    None => TksPattern::any(),
                    Some(tks) => tks,
                };
                (next, Hop { segment, tks })
            })
        }

        fn file_hop<ResourceType: FromStr, Kind: FromStr>(
            input: &str,
        ) -> Res<&str, Hop<ResourceType, Kind>> {
            tuple((file_segment, opt(tks)))(input).map(|(next, (segment, tks))| {
                let tks = match tks {
                    None => TksPattern::any(),
                    Some(tks) => tks,
                };
                (next, Hop { segment, tks })
            })
        }



        #[cfg(test)]
        pub mod test {

            use crate::error::Error;
            use crate::version::v0_0_1::generic::pattern::{Pattern, TksPattern};
            use crate::version::v0_0_1::pattern::parse::{hop, segment, specific, tks, space_hop};
            use crate::version::v0_0_1::pattern::{
                ExactSegment, Pattern, SegmentPattern, SpecificPattern, VersionReq,
            };
            use nom::combinator::all_consuming;
            use std::str::FromStr;
            use crate::version::v0_0_1::id::AddressSegment;
            use crate::version::latest::util::ValuePattern;

            #[test]
            pub fn test_segs() -> Result<(), Error> {
                assert!(segment("*")? == ("", SegmentPattern::Any));
                assert!(segment("**")? == ("", SegmentPattern::Recursive));
                assert!(
                    segment("hello")?
                        == (
                            "",
                            SegmentPattern::Exact(ExactSegment::Address(AddressSegment::Base("hello".to_string())))
                        )
                );
                Ok(())
            }

            #[test]
            pub fn test_specific() -> Result<(), Error> {
                let (_, x) = specific("mysql.org:mysql:innodb:(7.0.1)'")?;
                println!("specific: '{}'", x.to_string());
                let (_, x) = specific("mysql.org:mysql:innodb:(>=7.0.1, <8.0.0)")?;
                println!("specific: '{}'", x.to_string());
                let (_, x) = specific("mysql.org:*:innodb:(>=7.0.1, <8.0.0)")?;
                println!("specific: '{}'", x.to_string());

                Ok(())
            }

            #[test]
            pub fn test_tks() -> Result<(), Error> {
                let tks_pattern = TksPattern {
                    resource_type: Pattern::Exact(CamelCase::new("App")),
                    kind: Pattern::Any,
                    specific: ValuePattern::Any,
                };

                assert!(tks("<App>")? == ("", tks_pattern));

                let tks_pattern = TksPattern {
                    resource_type: Pattern::Exact(CamelCase::new("Database")),
                    kind: Pattern::Exact(CamelCase::new("Relational")),
                    specific: ValuePattern::Any,
                };

                assert!(tks("<Database<Relational>>")? == ("", tks_pattern));

                let tks_pattern = TksPattern {
                    resource_type: Pattern::Exact(CamelCase::new("Database")),
                    kind: Pattern::Exact(CamelCase::new("Relational")),
                    specific: ValuePattern::Pattern(SpecificPattern {
                        vendor: Pattern::Exact(DomainCase::new("mysql.org")),
                        product: Pattern::Exact(SkewerCase::new("mysql")),
                        variant: Pattern::Exact(SkewerCase::new("innodb")),
                        version: VersionReq::from_str("^7.0.1")?,
                    }),
                };

                assert!(
                    tks("<Database<Relational<mysql.org:mysql:innodb:(^7.0.1)>>>")?
                        == ("", tks_pattern)
                );

                Ok(())
            }

            #[test]
            pub fn test_hop() -> Result<(), Error> {
                space_hop("*<Database<Relational<mysql.org:mysql:innodb:(^7.0.1)>>>")?;
                space_hop("**<Database<Relational<mysql.org:mysql:innodb:(^7.0.1)>>>")?;
                space_hop("space.org:<Database<Relational<mysql.org:mysql:innodb:(^7.0.1)>>>")?;
                space_hop("space.org:something<Database<Relational<mysql.org:mysql:innodb:(^7.0.1)>>>")?;
                space_hop("space.org:no-type")?;
                space_hop("space.org:no-type:**")?;
                space_hop("space.org:app:users:*:tenant:**")?;
                space_hop("space.org:app:users:*:tenant:**<Mechtron>")?;
                space_hop("space.org:something:**<*<*<mysql.org:mysql:innodb:(^7.0.1)>>>")?;
                space_hop("space.org:something<*>")?;

                Ok(())
            }
        }
    }
}

pub mod messaging {
    use crate::error::Error;
    use serde::{Deserialize, Serialize};
    use std::convert::TryInto;

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
    use serde::{Deserialize, Serialize};

    use crate::error::Error;
    use crate::version::v0_0_1::bin::Bin;
    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Address, Kind, PayloadClaim, ResourceType};

    use crate::version::v0_0_1::pattern::TksPattern;

    pub type Primitive = generic::payload::Primitive<Kind>;
    pub type Payload = generic::payload::Payload<Kind>;
    pub type PayloadType = generic::payload::PayloadType;
    pub type PayloadRef = generic::payload::PayloadRef<PayloadClaim, PayloadPattern>;
    pub type PayloadDelivery = generic::payload::PayloadDelivery<Payload, PayloadRef>;
    pub type Call = generic::payload::Call;
    pub type CallWithConfig = generic::payload::CallWithConfig;
    pub type MapPattern = generic::payload::MapPattern;
    pub type PayloadTypePattern =
        generic::payload::PayloadTypePattern;
    pub type PayloadPattern = generic::payload::PayloadPattern;
    pub type ListPattern = generic::payload::ListPattern;
    pub type PayloadMap = generic::payload::PayloadMap<Kind>;
    pub type RcCommand = generic::resource::command::RcCommand<ResourceType, Kind>;

    #[derive(
        Debug,
        Clone,
        strum_macros::Display,
        strum_macros::EnumString,
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

    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Address, Kind};
    use crate::version::v0_0_1::ArtifactRef;

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

    pub type Info = generic::config::Info<Kind>;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Config {
        pub max_payload_size: u32,
        pub init_timeout: u64,
        pub frame_timeout: u64,
        pub response_timeout: u64,
        pub bind: BindConfig,
    }

    impl Config {
        pub fn with_bind_config(bind: BindConfig) -> Self {
            Self {
                max_payload_size: 128 * 1024,
                init_timeout: 30,
                frame_timeout: 5,
                response_timeout: 15,
                bind,
            }
        }
    }

    impl Default for Config {
        fn default() -> Self {
            Self {
                max_payload_size: 128 * 1024,
                init_timeout: 30,
                frame_timeout: 5,
                response_timeout: 15,
                bind: Default::default(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SchemaRef {
        pub schema: String,
        pub artifact: Option<ArtifactRef>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BindConfig {
        pub ports: HashMap<String, PortConfig>,
    }

    impl Default for BindConfig {
        fn default() -> Self {
            Self {
                ports: Default::default(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PortConfig {
        pub payload: PayloadConfig,
        pub response: EntityConfig,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum EntityConfig {
        Empty,
        Resource(ResourceConfig),
        Payload(PayloadConfig),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ResourceConfig {
        None,
        Resource,
        Resources,
        State,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum PayloadConfig {
        Text,
        Bin(SchemaRef),
        Bins(HashMap<String, SchemaRef>),
    }
}

pub mod entity {

    #[derive(Debug, Clone)]
    pub enum EntityType {
        Rc,
        Msg,
        Http,
    }

    pub mod request {
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Kind, PayloadClaim, ResourceType};
        use crate::version::v0_0_1::pattern::TksPattern;
        use crate::version::v0_0_1::payload::Payload;

        pub type ReqEntity = generic::entity::request::ReqEntity<ResourceType, Payload>;
        pub type Rc = generic::entity::request::Rc<ResourceType, Kind>;
        pub type Msg = generic::entity::request::Msg<Payload>;
        pub type Http = generic::entity::request::Http<Payload>;
    }

    pub mod response {
        use crate::version::v0_0_1::id::{Address, Kind};
        use crate::version::v0_0_1::payload::Payload;
        use crate::version::v0_0_1::{fail, generic};

        pub type RespEntity = generic::entity::response::RespEntity<Payload>;
    }
}

pub mod resource {
    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Address, Kind, ResourceType};

    #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display, Eq, PartialEq)]
    pub enum Status {
        Unknown,              // initial status or when we status cannot be determined
        Pending,              // resource is now registered but not assigned to a host
        Assigning,            // resource is being assigned to at least one host
        Initializing(String), // assigned to a host and undergoing custom initialization...This resource can send requests but not receive requests.  The String gives a progress indication like 2/10 (step 2 of 10) or 7/? when the number of steps are not known.
        Ready,                // ready to take requests
        Paused(String), // can not receive requests (probably because it is waiting for some other resource to make updates)... String should be some form of meaningful identifier of which resource Paused this resource
        Resuming(String), // like Initializing but triggered after a pause is lifted, the resource may be doing something before it is ready to accept requests again.  String is a progress indication just like Initializing.
        Panic(String), // something is wrong... all requests are blocked and responses are cancelled. String is a hopefully  meaningful message describing why the Resource has Panic
        Done(String), // this resource had a life span and has now completed succesfully it can no longer receive requests. String is a hopefully meaningful or useful Status message that is returned
    }

    pub type Archetype = generic::resource::Archetype<Address>;
    pub type ResourceStub = generic::resource::ResourceStub<Kind>;
}

pub mod portal {
    pub mod inlet {
        use std::convert::TryFrom;

        use crate::error::Error;
        use crate::version::v0_0_1::frame::PrimitiveFrame;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Kind, ResourceType};
        use crate::version::v0_0_1::pattern::TksPattern;
        use crate::version::v0_0_1::payload::Payload;

        pub type ReqEntity = generic::entity::request::ReqEntity<ResourceType, Kind>;
        pub type Request = generic::portal::inlet::Request<ReqEntity>;
        pub type Response = generic::portal::inlet::Response<Payload>;
        pub type Frame = generic::portal::inlet::Frame<ReqEntity, Payload>;

        impl TryFrom<PrimitiveFrame> for generic::portal::inlet::Frame<ReqEntity, Payload> {
            type Error = Error;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }

    }

    pub mod outlet {
        use std::convert::TryFrom;

        use crate::error::Error;
        use crate::version::v0_0_1::frame::PrimitiveFrame;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Kind, ResourceType};
        use crate::version::v0_0_1::payload::Payload;
        use crate::version::latest::entity::request::ReqEntity;

        pub type Request = generic::portal::outlet::Request<ReqEntity>;
        pub type Response = generic::portal::outlet::Response<Payload>;
        pub type Frame = generic::portal::outlet::Frame<Kind, Payload,ReqEntity>;

        impl TryFrom<PrimitiveFrame> for generic::portal::outlet::Frame<Kind, Payload,ReqEntity> {
            type Error = Error;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }
}

pub mod generic {
    use std::collections::HashMap;
    use std::convert::From;
    use std::convert::TryInto;
    use std::fmt::Debug;
    use std::hash::Hash;
    use std::str::FromStr;
    use std::sync::Arc;

    use serde::{Deserialize, Serialize};

    pub mod id {
        use std::convert::{TryFrom, TryInto};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::error::Error;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Specific, Tks};
        use crate::version::v0_0_1::util::Convert;
        use crate::version::v0_0_1::pattern::parse::{consume_kind, resource_type};
        use nom::combinator::{all_consuming, opt};
        use nom::sequence::{tuple, delimited};
        use crate::version::v0_0_1::parse::camel_case;
        use nom::bytes::complete::tag;
        use crate::version::v0_0_1::generic::id::parse::specific;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
        pub struct GenericKind<ResourceType> {
            pub resource_type: ResourceType,
            pub kind: Option<String>,
            pub specific: Option<Specific>,
        }

        impl <ResourceType> FromStr for GenericKind<ResourceType> {
            type Err = Error;

            fn from_str(s: &str) -> Result<Self, Self::Err> {

                let (_,kind) = all_consuming(tuple((resource_type,opt(delimited(tag("<"),tuple((camel_case,opt(delimited(tag("<"), specific, tag(">"))))),tag(">"))))))(input).map( |(next,(resource_type, rest ))| {

                    let mut rtn = GenericKind {
                        resource_type,
                        kind: Option::None,
                        specific: Option::None
                    };

                    match rest {
                        Some((kind,specific)) => {
                            rtn.kind = Option::Some(kind.to_string());
                            match specific {
                                Some(specific) => {
                                    rtn.specific = Option::Some(specific);
                                },
                                None => {}
                            }
                        }
                        None => { }
                    }

                    (next,rtn)
                } );
                Ok(kind)
            }
        }

        impl <ResourceType> GenericKind<ResourceType> {
            pub fn new( resource_type: ResourceType, kind: Option<String>, specific: Option<Specific>) -> Self {
                Self {
                    resource_type,
                    kind,
                    specific
                }
            }
        }

        impl<ResourceType> Tks<ResourceType> for GenericKind<ResourceType>
        where
            ResourceType: Eq + PartialEq + Clone,
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

            fn matches(&self, tks: &dyn Tks<ResourceType>) -> bool {
                self.resource_type == tks.resource_type()
                    && self.kind == tks.kind_to_string()
                    && self.specific == tks.specific()
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
        pub struct AddressAndKind<KIND> {
            pub address: Address,
            pub kind: KIND,
        }

        impl<KIND> AddressAndKind<KIND> {
            pub fn new(address: Address, kind: KIND) -> Self {
                Self { address, kind }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
        pub struct AddressAndType<ResourceType> {
            pub address: Address,
            pub resource_type: ResourceType,
        }

        pub mod parse {
            use crate::version::v0_0_1::id::{Specific, Version};
            use crate::version::v0_0_1::parse::{domain_chars, skewer_chars, version_chars, Res};
            use nom::bytes::complete::{is_a, tag};
            use nom::sequence::{delimited, tuple};
            use nom::Parser;
            use nom_supreme::{parse_from_str, ParserExt};

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
                    delimited(tag("("), version, tag(")")),
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
    }

    pub mod config {
        use std::convert::{TryFrom, TryInto};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::error::Error;
        use crate::version::v0_0_1::config::{Config, PortalKind};
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::resource::Archetype;
        use crate::version::v0_0_1::id::Address;
        use crate::version::v0_0_1::ArtifactRef;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Info<KIND> {
            pub address: Address,
            pub owner: String,
            pub parent: Address,
            pub archetype: Archetype<KIND>,
            pub config: Config,
            pub ext_config: Option<ArtifactRef>,
            pub kind: PortalKind,
        }

        impl<FromKind> Info<FromKind> {
            pub fn convert<ToKind>(self) -> Result<Info<ToKind>, Error>
            where
                ToKind: TryFrom<FromKind, Error = Error>,
            {
                Ok(Info {
                    address: self.address.try_into()?,
                    owner: self.owner,
                    parent: self.parent.try_into()?,
                    archetype: self.archetype.convert()?,
                    config: self.config,
                    ext_config: self.ext_config,
                    kind: self.kind,
                })
            }
        }
    }

    pub mod entity {
        pub mod request {
            use std::convert::{TryFrom, TryInto};
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::__private::fmt::Debug;
            use serde::{Deserialize, Serialize};

            use crate::error::Error;
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::generic;
            use crate::version::v0_0_1::generic::entity::response::RespEntity;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::generic::payload::{HttpMethod, Payload};
            use crate::version::v0_0_1::generic::resource::command::RcCommand;
            use crate::version::v0_0_1::id::Meta;
            use crate::version::v0_0_1::util::{Convert, ConvertFrom};
            use crate::version::v0_0_1::{http, State};
            use crate::version::latest::fail::Fail;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum ReqEntity<ResourceType, Kind> {
                Rc(Rc<ResourceType, Kind>),
                Msg(Msg<Kind>),
                Http(Http<Kind>),
            }

            impl<ResourceType, Payload> ReqEntity<ResourceType, Payload> {
                pub fn ok(&self, payload: Payload) -> RespEntity<Payload> {
                    RespEntity::Ok(payload)
                }

                pub fn fail(&self, fail: Fail) -> RespEntity<Payload> {
                    RespEntity::Fail(fail)
                }
            }

            impl<
                    FromResourceType,
                    FromKind,
                    ToResourceType,
                    ToKind,
                > ConvertFrom<ReqEntity<FromResourceType, FromKind>>
                for ReqEntity<ToResourceType, ToKind>
            where
                FromKind: TryInto<ToKind, Error = Error>+Clone+Eq+PartialEq,
                FromResourceType: TryInto<ToResourceType, Error = Error>+Clone+Eq+PartialEq,
                Payload<FromKind>: TryInto<Payload<ToKind>,Error=Error>
            {
                fn convert_from(
                    a: ReqEntity<FromResourceType, FromKind>,
                ) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    match a {
                        ReqEntity::Rc(rc) => Ok(ReqEntity::Rc(ConvertFrom::convert_from(rc)?)),
                        ReqEntity::Msg(msg) => Ok(ReqEntity::Msg(ConvertFrom::convert_from(msg)?)),
                        ReqEntity::Http(http) => {
                            Ok(ReqEntity::Http(ConvertFrom::convert_from(http)?))
                        }
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Rc<ResourceType, Kind> {
                pub command: RcCommand<ResourceType, Kind>,
                pub payload: Payload<Kind>,
            }

            impl<ResourceType, Kind> Rc<ResourceType, Kind> {
                pub fn with_payload(
                    command: RcCommand<ResourceType, Kind>,
                    payload: Payload<Kind>,
                ) -> Self {
                    Self { command, payload }
                }

                pub fn new(
                    command: RcCommand<ResourceType, Kind>
                ) -> Self {
                    Self { command, payload: Payload::Empty}
                }
            }

            impl<ResourceType, Kind> ToString for Rc<ResourceType, Kind> {
                fn to_string(&self) -> String {
                    format!("Rc<{}>", self.command.to_string())
                }
            }

            impl<
                    FromResourceType,
                    FromKind,
                    ToResourceType,
                    ToKind,
                > ConvertFrom<Rc<FromResourceType, FromKind>>
                for Rc<ToResourceType, ToKind>
            where
                FromResourceType: TryInto<ToResourceType, Error = Error>+Clone+Eq+PartialEq,
                FromKind: TryInto<ToKind, Error = Error>+Clone+Eq+PartialEq,
                Payload<FromKind>: TryInto<Payload<ToKind>,Error=Error>
            {
                fn convert_from(
                    a: Rc<FromResourceType, FromKind>,
                ) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    Ok(Rc {
                        command: a.command.convert()?,
                        payload: a.payload.convert()?,
                    })
                }
            }

            impl<FromResourceType, FromKind > Rc<FromResourceType, FromKind> {
                pub fn convert<ToResourceType, ToKind>(
                    self,
                ) -> Result<Rc<ToResourceType, ToKind>, Error>
                where
                    FromResourceType: TryInto<ToResourceType, Error = Error>+Clone+Eq+PartialEq,
                    FromKind: TryInto<ToKind, Error = Error>+Clone+Eq+PartialEq,
                    Payload<FromKind>: TryInto<Payload<ToKind>,Error=Error>
                {
                    Ok(Rc {
                        command: self.command.convert()?,
                        payload: self.payload.convert()?,
                    })
                }
            }

            impl<FromResourceType, FromKind >
                RcCommand<FromResourceType, FromKind>
            {
                pub fn convert<ToResourceType, ToKind>(
                    self,
                ) -> Result<RcCommand<ToResourceType, ToKind>, Error>
                where
                    FromResourceType: TryInto<ToResourceType, Error = Error>+Clone+Eq+PartialEq,
                    FromKind: TryInto<ToKind, Error = Error>+Clone+Eq+PartialEq,
                    Payload<FromKind>: TryInto<Payload<ToKind>,Error=Error>
                {
                    match self {
                        RcCommand::Create(create) => {
                            Ok(RcCommand::Create(create.convert()?))
                        }
                        RcCommand::Select(select) => {
                            Ok(RcCommand::Select(select.convert()?))
                        }
                        RcCommand::Update(update) => {
                            Ok(RcCommand::Update(update.convert()?))
                        }
                        RcCommand::Query(query) => {
                            Ok(RcCommand::Query(query))
                        }
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Msg<Kind> {
                pub action: String,
                pub path: String,
                pub payload: Payload<Kind>,
            }

            impl<PAYLOAD> ToString for Msg<PAYLOAD> {
                fn to_string(&self) -> String {
                    format!("Msg<{}>{}", self.action, self.path)
                }
            }

            impl<FromKind, ToKind> ConvertFrom<Msg<FromKind>> for Msg<ToKind>
            where
                FromKind: TryInto<ToKind, Error = Error>,
            {
                fn convert_from(a: Msg<FromKind>) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    Ok(Msg {
                        action: a.action,
                        path: a.path,
                        payload: a.payload.convert()?,
                    })
                }
            }

            impl<FromKind> Msg<FromKind> {
                pub fn convert<ToKind>(self) -> Result<Msg<ToKind>, Error>
                where
                    FromKind: TryInto<ToKind,Error=Error>,
                {
                    Ok(Msg {
                        action: self.action,
                        path: self.path,
                        payload: self.payload.convert()?,
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Http<Kind> {
                pub headers: Meta,
                pub method: HttpMethod,
                pub path: String,
                pub body: Payload<Kind>,
            }

            impl<PAYLOAD> ToString for Http<PAYLOAD> {
                fn to_string(&self) -> String {
                    format!("Http<{}>{}", self.method.to_string(), self.path)
                }
            }

            impl<FromKind, ToKind> ConvertFrom<Http<FromKind>> for Http<ToKind>
            where
                FromKind: TryInto<ToKind, Error = Error>,
            {
                fn convert_from(a: Http<FromKind>) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    Ok(Http {
                        headers: a.headers,
                        method: a.method,
                        path: a.path,
                        body: a.body.convert()?,
                    })
                }
            }

            impl<FromKind> Http<FromKind> {
                pub fn convert<ToKind>(self) -> Result<Http<ToKind>, Error>
                where
                    FromKind: TryInto<ToKind,Error=Error>,
                {
                    Ok(Http {
                        headers: self.headers,
                        method: self.method,
                        path: self.path,
                        body: self.body.convert()?,
                    })
                }
            }
        }

        pub mod response {
            use std::convert::{TryFrom, TryInto};
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::error::Error;
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::fail::portal::Fail;
            use crate::version::v0_0_1::generic::payload::Payload;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::util::ConvertFrom;
            use crate::version::v0_0_1::fail;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum RespEntity<PAYLOAD> {
                Ok(PAYLOAD),
                Fail(fail::Fail),
            }

            impl<FromPayload, ToPayload> ConvertFrom<RespEntity<FromPayload>>
                for RespEntity<ToPayload>
            where
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(
                    a: RespEntity<FromPayload>,
                ) -> Result<Self, crate::error::Error>
                where
                    Self: Sized,
                {
                    match a {
                        RespEntity::Ok(payload) => Ok(RespEntity::Ok(payload.try_into()?)),
                        RespEntity::Fail(fail) => Ok(RespEntity::Fail(fail)),
                    }
                }
            }

            impl<FromPayload> RespEntity<FromPayload> {
                pub fn convert<ToPayload>(self) -> Result<RespEntity<FromPayload>, Error>
                where
                    ToPayload: TryFrom<FromPayload, Error = Error>,
                {
                    match self {
                        RespEntity::Ok(ok) => Ok(RespEntity::Ok(ok.try_into()?)),
                        RespEntity::Fail(fail) => Ok(RespEntity::Fail(fail)),
                    }
                }
            }
        }
    }

    pub mod resource {
        use std::collections::{HashMap, HashSet};
        use std::convert::{TryFrom, TryInto};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::error::Error;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::id::AddressAndKind;
        use crate::version::v0_0_1::generic::payload::Primitive;
        use crate::version::v0_0_1::generic::payload::{
            HttpCall, MapPattern, MsgCall, Payload, PayloadType,
        };
        use crate::version::v0_0_1::id::Address;
        use crate::version::v0_0_1::util::ConvertFrom;
        use crate::version::v0_0_1::State;
        use crate::version::v0_0_1::generic::payload::PayloadMap;
        use crate::version::v0_0_1::resource::Status;

        pub type Properties<Kind> = PayloadMap<Kind>;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct Archetype<KIND> {
            pub kind: KIND,
            pub properties: Properties<KIND>
        }

        impl<FromKind, ToKind> ConvertFrom<Archetype<FromKind>> for Archetype<ToKind>
        where
            FromKind: TryInto<ToKind, Error = Error>,
        {
            fn convert_from(a: Archetype<FromKind>) -> Result<Self, Error>
            where
                Self: Sized,
            {
                Ok(Archetype {
                    kind: a.kind.try_into()?,
                    properties: a.properties.convert()?,
                })
            }
        }

        impl<FromKind> Archetype<FromKind> {
            pub fn convert<ToKind>(self) -> Result<Archetype<ToKind>, Error>
            where
                FromKind: TryInto<ToKind, Error = Error>,
            {
                Ok(Archetype {
                    kind: self.kind.try_into()?,
                    properties: self.properties.convert()?
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct ResourceStub<KIND> {
            pub address: Address,
            pub kind: KIND,
            pub properties: Properties<KIND>,
            pub status: Status
        }

        impl<FromKind> ResourceStub<FromKind> {
            pub fn convert<ToKind>(self) -> Result<ResourceStub<ToKind>, Error>
            where
                FromKind: TryInto<ToKind, Error = Error>,
            {
                Ok(ResourceStub {
                    address: self.address.try_into()?,
                    kind: self.kind.try_into()?,
                    properties: self.properties.convert()?,
                    status: self.status
                })
            }
        }

        impl<FromKind, ToKind> ConvertFrom<ResourceStub<FromKind>> for ResourceStub<ToKind>
        where
            FromKind: TryInto<ToKind, Error = Error>,
        {
            fn convert_from(a: ResourceStub<FromKind>) -> Result<Self, Error>
            where
                Self: Sized,
            {
                Ok(ResourceStub {
                    address: a.address.try_into()?,
                    kind: a.kind.try_into()?,
                    properties:a.properties.convert()?,
                    status: a.status
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct Resource<KIND> {
            pub stub: ResourceStub<KIND>,
            pub state: Box<Payload<KIND>>,
        }

        impl<FromKind> Resource<FromKind> {
            pub fn convert<ToKind>(self) -> Result<Resource<ToKind>, Error>
            where
                FromKind: TryInto<ToKind, Error = Error>,
            {
                Ok(Resource {
                    stub: self.stub.convert()?,
                    state: Box::new(self.state.convert()?),
                })
            }
        }

        impl<FromKind, ToKind> ConvertFrom<Resource<FromKind>> for Resource<ToKind>
        where
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            ToKind: Clone,
        {
            fn convert_from(a: Resource<FromKind>) -> Result<Self, Error>
            where
                Self: Sized,
            {
                Ok(Resource {
                    stub: ConvertFrom::convert_from(a.stub)?,
                    state: Box::new(ConvertFrom::convert_from(*a.state)?),
                })
            }
        }

        pub mod command {
            use crate::version::v0_0_1::generic::resource::command::create::Create;
            use crate::version::v0_0_1::generic::resource::command::select::Select;
            use crate::version::v0_0_1::generic::resource::command::update::Update;
            use crate::version::v0_0_1::util::ValueMatcher;
            use serde::{Deserialize, Serialize};
            use crate::version::v0_0_1::generic::resource::command::query::Query;

            #[derive(Debug, Clone, strum_macros::Display, Serialize, Deserialize)]
            pub enum RcCommand<ResourceType, Kind> {
                Create(Create<Kind>),
                Select(Select<ResourceType, Kind>),
                Update(Update<Kind>),
                Query(Query)
            }

            impl<ResourceType, Kind> RcCommand<ResourceType, Kind> {
                pub fn get_type(&self) -> RcCommandType {
                    match self {
                        RcCommand::Create(_) => RcCommandType::Create,
                        RcCommand::Select(_) => RcCommandType::Select,
                        RcCommand::Update(_) => RcCommandType::Update,
                        RcCommand::Query(_) => RcCommandType::Query,
                    }
                }
            }

            #[derive(Debug,Clone,Eq, PartialEq, strum_macros::Display, strum_macros::EnumString,Serialize,Deserialize)]
            pub enum RcCommandType {
                Create,
                Select,
                Update,
                Query
            }

            impl<ResourceType, KIND >
                ValueMatcher<RcCommand<ResourceType, KIND>>
                for RcCommand<ResourceType, KIND>
            {
                fn is_match(
                    &self,
                    x: &RcCommand<ResourceType, KIND>,
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

            pub mod common {
                use crate::error::Error;
                use crate::version::latest::generic::payload::Payload;
                use crate::version::v0_0_1::generic::payload::PayloadMap;
                use serde::{Deserialize, Serialize};
                use std::convert::{TryFrom, TryInto};
                use std::ops::{Deref, DerefMut};

                #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
                pub enum StateSrc<Kind> {
                    Stateless,
                    StatefulDirect(Payload<Kind>),
                }

                impl<FromKind> StateSrc<FromKind> {
                    pub fn convert<ToKind>(self) -> Result<StateSrc<ToKind>, Error>
                    where
                        FromKind: TryInto<ToKind, Error = Error>,
                    {
                        Ok(match self {
                            StateSrc::Stateless => StateSrc::Stateless,
                            StateSrc::StatefulDirect(payload) => {
                                StateSrc::StatefulDirect(payload.convert()?)
                            }
                        })
                    }
                }

                pub type SetProperties<KIND> = PayloadMap<KIND>;

                #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
                pub enum SetLabel{
                    Set(String),
                    SetValue{ key: String, value: String },
                    Unset(String)
                }

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct SetRegistry {
                    pub labels: Vec<SetLabel>
                }

                impl Deref for SetRegistry {
                    type Target = Vec<SetLabel>;

                    fn deref(&self) -> &Self::Target {
                        &self.labels
                    }
                }

                impl DerefMut for SetRegistry {
                    fn deref_mut(&mut self) -> &mut Self::Target {
                        & mut self.labels
                    }
                }

                impl Default for SetRegistry {
                    fn default() -> Self {
                        Self{
                            labels:Default::default()
                        }
                    }
                }
            }

            pub mod create {
                use crate::error::Error;
                use crate::version::latest::generic::payload::Payload;
                use crate::version::v0_0_1::generic::payload::PayloadMap;
                use crate::version::v0_0_1::generic::resource::command::common::{SetProperties, StateSrc, SetRegistry};
                use crate::version::v0_0_1::id::Address;
                use crate::version::v0_0_1::util::ConvertFrom;
                use serde::{Deserialize, Serialize};
                use std::convert::TryInto;
                use crate::version::v0_0_1::pattern::SpecificPattern;

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct Template {
                   pub address: AddressTemplate,
                   pub kind: KindTemplate
                }

                impl Template {
                    pub fn new( address: AddressTemplate, kind: KindTemplate ) -> Self {
                        Self {
                            address,
                            kind
                        }
                    }
                }

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct KindTemplate {
                    pub resource_type: String,
                    pub kind: Option<String>,
                    pub specific: Option<SpecificPattern>,
                }

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct Create<Kind> {
                    pub template: Template,
                    pub state: StateSrc<Kind>,
                    pub properties: SetProperties<Kind>,
                    pub strategy: Strategy,
                    pub registry: SetRegistry
                }

                impl<Kind> Create<Kind> {
                    pub fn new( template: Template ) -> Self {
                        Self {
                            template,
                            state: StateSrc::Stateless,
                            properties: Default::default(),
                            strategy: Strategy::Create,
                            registry: Default::default()
                        }
                    }
                }

                impl<FromKind> Create<FromKind> {
                    pub fn convert<ToKind>(
                        self,
                    ) -> Result<Create<ToKind>, Error>
                    where
                        FromKind: TryInto<ToKind, Error = Error>+Clone,
                        Payload<FromKind>: TryInto<Payload<ToKind>,Error=Error>
                    {
                        Ok(Create {
                            template: self.template,
                            state: self.state.convert()?,
                            properties: self.properties.convert()?,
                            strategy: self.strategy,
                            registry: self.registry
                        })
                    }
                }


                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub enum Strategy{
                    Create,
                    CreateOrUpdate,
                    Ensure
                }

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct AddressTemplate {
                    pub parent: Address,
                    pub child_segment_template: AddressSegmentTemplate,
                }

                #[derive(Debug, Clone, strum_macros::Display, Serialize, Deserialize)]
                pub enum AddressSegmentTemplate {
                    // right now only exact is supported
                    Exact(String),
                }
            }

            pub mod select {
                use crate::error::Error;
                use crate::version::v0_0_1::generic::pattern::AddressKindPattern;
                use crate::version::v0_0_1::generic::payload::{MapPattern, PrimitiveList};
                use crate::version::v0_0_1::util::ConvertFrom;
                use serde::{Deserialize, Serialize};
                use std::collections::{HashMap, HashSet};
                use std::convert::{TryFrom, TryInto};
                use crate::version::v0_0_1::generic::payload::Payload;
                use crate::version::v0_0_1::fail::{Fail, BadCoercion};
                use crate::version::v0_0_1::generic::resource::ResourceStub;
                use crate::version::v0_0_1::payload::PrimitiveType;
                use crate::version::v0_0_1::generic::payload::Primitive;
                use std::marker::PhantomData;
                use crate::version::latest::generic::pattern::{Hop, AddressKindPath};
                use crate::version::latest::fail;
                use crate::version::latest::id::Address;

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub enum SelectIntoPayload  {
                    Stubs
                }

                impl SelectIntoPayload {
                    pub fn to_primitive<Kind>(&self, stubs: Vec<ResourceStub<Kind>>) -> Result<PrimitiveList<Kind>, Fail>
                    {
                        let stubs: Vec<Primitive<Kind>> = stubs.into_iter().map( |stub| Primitive::Stub(stub) ).collect();
                        let stubs = PrimitiveList{
                            primitive_type: PrimitiveType::Stub,
                            list: stubs
                        };
                        Ok(stubs)
                    }
                }


                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct Select<ResourceType, Kind> {
                    pub pattern: AddressKindPattern<ResourceType, Kind>,
                    pub properties: PropertiesPattern,
                    pub into_payload: SelectIntoPayload,
                    pub kind: SelectionKind<ResourceType,Kind>
                }

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub enum SelectionKind<ResourceType,Kind>{
                    Initial,
                    SubSelector{ address: Address, hops: Vec<Hop<ResourceType,Kind>>, address_kind_path: AddressKindPath<Kind> }
                }

                impl <ResourceType,Kind> TryInto<SubSelector<ResourceType,Kind>> for Select<ResourceType,Kind> {
                    type Error = Error;

                    fn try_into(self) -> Result<SubSelector<ResourceType,Kind>, Self::Error> {
                        match self.kind {
                            SelectionKind::Initial => {
                                Err("cannot convert an initial Select into a SubSelect".into())
                            }
                            SelectionKind::SubSelector { address,hops, address_kind_path: address_kind_path } => {
                                Ok(SubSelector{
                                    address,
                                    pattern: self.pattern,
                                    properties: self.properties,
                                    into_payload: self.into_payload,
                                    hops,
                                    address_kind_path
                                })
                            }
                        }
                    }
                }

                impl <ResourceType,Kind> Select<ResourceType,Kind> {

                    pub fn sub_select(self, address:Address, hops: Vec<Hop<ResourceType,Kind>>, address_kind_path: AddressKindPath<Kind>) -> SubSelector<ResourceType, Kind> {

                                SubSelector{
                                    address,
                                    pattern: self.pattern,
                                    properties: self.properties,
                                    into_payload: self.into_payload,
                                    hops,
                                    address_kind_path
                                }

                    }
                }

                #[derive(Debug,Clone,Serialize,Deserialize)]
                pub struct SubSelector<ResourceType,Kind> {
                    pub address: Address,
                    pub pattern: AddressKindPattern<ResourceType, Kind>,
                    pub properties: PropertiesPattern,
                    pub into_payload: SelectIntoPayload,
                    pub hops: Vec<Hop<ResourceType,Kind>>,
                    pub address_kind_path: AddressKindPath<Kind>
                }

                impl <ResourceType,Kind> Into<Select<ResourceType,Kind>> for SubSelector<ResourceType,Kind> {
                    fn into(self) -> Select<ResourceType, Kind> {
                        Select {
                            pattern: self.pattern,
                            properties: self.properties,
                            into_payload: self.into_payload,
                            kind: SelectionKind::SubSelector {
                                address: self.address,
                                hops: self.hops,
                                address_kind_path: self.address_kind_path
                            }
                        }
                    }
                }

                impl <ResourceType,Kind> SubSelector<ResourceType,Kind> {

                    pub fn sub_select(self, address:Address, hops: Vec<Hop<ResourceType,Kind>>, address_tks_path: AddressKindPath<Kind>) -> SubSelector<ResourceType, Kind> {

                        SubSelector{
                            address,
                            pattern: self.pattern,
                            properties: self.properties,
                            into_payload: self.into_payload,
                            hops,
                            address_kind_path: address_tks_path
                        }
                    }
                }


                impl<ResourceType, Kind > Select<ResourceType, Kind>
                {
                    fn new(pattern:AddressKindPattern<ResourceType, Kind>) -> Self {
                        Self{
                            pattern,
                            properties: Default::default(),
                            into_payload: SelectIntoPayload::Stubs,
                            kind: SelectionKind::Initial
                        }
                    }
                }

                impl<FromResourceType, FromKind >
                SelectionKind<FromResourceType, FromKind>
                {
                    pub fn convert<ToResourceType, ToKind>(
                        self,
                    ) -> Result<SelectionKind<ToResourceType, ToKind>, Error>
                        where
                            FromResourceType: TryInto<ToResourceType, Error = Error>+Clone+Eq+PartialEq,
                            FromKind: TryInto<ToKind, Error = Error>+Clone+Eq+PartialEq,
                            Payload<FromKind>: TryInto<Payload<ToKind>,Error=Error>
                    {
                        Ok(match self {
                            SelectionKind::Initial => {
                                SelectionKind::Initial
                            }
                            SelectionKind::SubSelector { address, hops, address_kind_path: address_tks_path } => {
                                let mut converted_hops = vec![];
                                for hop in hops {
                                    converted_hops.push(hop.convert()?);
                                }
                                SelectionKind::SubSelector {
                                    address,
                                    hops: converted_hops,
                                    address_kind_path: address_tks_path.convert()?
                                }
                            }
                        })
                    }
                }

                impl<FromResourceType, FromKind >
                    Select<FromResourceType, FromKind>
                {
                    pub fn convert<ToResourceType, ToKind>(
                        self,
                    ) -> Result<Select<ToResourceType, ToKind>, Error>
                    where
                        FromResourceType: TryInto<ToResourceType, Error = Error>+Clone+Eq+PartialEq,
                        FromKind: TryInto<ToKind, Error = Error>+Clone+Eq+PartialEq,
                        Payload<FromKind>: TryInto<Payload<ToKind>,Error=Error>
                    {
                        Ok(Select {
                            pattern: self.pattern.convert()?,
                            properties: self.properties,
                            into_payload: self.into_payload,
                            kind: self.kind.convert()?
                        })
                    }
                }

                pub type PropertiesPattern = MapPattern;
            }

            pub mod update {
                use crate::error::Error;
                use crate::version::v0_0_1::generic::payload::PayloadMap;
                use crate::version::v0_0_1::generic::resource::command::common::SetProperties;
                use crate::version::v0_0_1::id::Address;
                use serde::{Deserialize, Serialize};
                use std::convert::TryInto;

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct Update<KIND> {
                    pub address: Address,
                    pub properties: SetProperties<KIND>,
                }

                impl<FromKind> Update<FromKind> {
                    pub fn convert<ToKind>(self) -> Result<Update<ToKind>, Error>
                    where
                        FromKind: TryInto<ToKind, Error = Error>,
                    {
                        Ok(Update {
                            address: self.address,
                            properties: self.properties.convert()?,
                        })
                    }
                }
            }

            pub mod query {
                use serde::{Serialize,Deserialize};
                use crate::version::latest::generic::pattern::AddressKindPath;
                use crate::version::latest::generic::payload::RcCommand;

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub enum Query {
                    AddressKindPath
                }

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub enum QueryResult<Kind>{
                    AddressKindPath(AddressKindPath<Kind>)
                }

                impl <ResourceType,Kind> Into<RcCommand<ResourceType,Kind>> for Query {
                    fn into(self)-> RcCommand<ResourceType,Kind> {
                        RcCommand::Query(self)
                    }
                }

            }
        }
    }

    pub mod portal {
        pub mod inlet {
            use std::convert::TryFrom;
            use std::convert::TryInto;
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::error::Error;
            use crate::version::v0_0_1::command::Command;
            use crate::version::v0_0_1::fail;
            use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
            use crate::version::v0_0_1::generic::entity::request;
            use crate::version::v0_0_1::generic::entity::request::ReqEntity;
            use crate::version::v0_0_1::generic::entity::response;
            use crate::version::v0_0_1::id::{Address, Kind, ResourceType};
            use crate::version::v0_0_1::log::Log;
            use crate::version::v0_0_1::messaging::Exchange;
            use crate::version::v0_0_1::messaging::ExchangeId;
            use crate::version::v0_0_1::resource::Status;
            use crate::version::v0_0_1::util::{unique_id, ConvertFrom};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<ReqEntity> {
                pub id: String,
                pub to: Vec<Address>,
                pub entity: ReqEntity,
                pub exchange: Exchange,
            }

            impl<ReqEntity> Request<ReqEntity> {
                pub fn new(entity: ReqEntity) -> Self {
                    Self {
                        id: unique_id(),
                        to: vec![],
                        entity,
                        exchange: Exchange::Notification,
                    }
                }
            }

            impl<ReqEntity> Request<ReqEntity> {
                pub fn exchange(self, exchange: Exchange) -> Request<ReqEntity> {
                    Request {
                        id: self.id,
                        to: self.to,
                        entity: self.entity,
                        exchange,
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Response<Payload> {
                pub id: String,
                pub to: Address,
                pub exchange: ExchangeId,
                pub entity: response::RespEntity<Payload>,
            }

            impl<FromPayload, ToPayload> ConvertFrom<Response<FromPayload>> for Response<ToPayload>
            where
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(a: Response<FromPayload>) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    Ok(Response {
                        id: a.id,
                        to: a.to.try_into()?,
                        entity: ConvertFrom::convert_from(a.entity)?,
                        exchange: a.exchange,
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
            pub enum Frame<ReqEntity, Payload> {
                Log(Log),
                Command(Command),
                Request(Request<ReqEntity>),
                Response(Response<Payload>),
                Status(Status),
                Close(CloseReason),
            }

            /*
            impl<'de,ReqEntity: Deserialize<'de>, Payload: Deserialize<'de>> TryFrom<PrimitiveFrame>
            for Frame<ReqEntity, Payload>
            {
                type Error = Error;

                fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                    let data:&'de[u8] = value.data.as_slice();
                    let frame = bincode::deserialize(data )?;
                    Ok(frame)
                }
            }

             */

            impl<ReqEntity: Serialize, Payload: Serialize> TryInto<PrimitiveFrame>
                for Frame<ReqEntity, Payload>
            {
                type Error = Error;

                fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                    Ok(PrimitiveFrame {
                        data: bincode::serialize(&self)?,
                    })
                }
            }
        }

        pub mod outlet {
            use std::convert::TryFrom;
            use std::convert::TryInto;
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::error::Error;
            use crate::version::v0_0_1::command::CommandEvent;
            use crate::version::v0_0_1::fail;
            use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
            use crate::version::v0_0_1::generic;
            use crate::version::v0_0_1::generic::config::Info;
            use crate::version::v0_0_1::generic::entity::request;
            use crate::version::v0_0_1::generic::entity::response;
            use crate::version::v0_0_1::generic::entity::response::RespEntity;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::generic::portal::inlet;
            use crate::version::v0_0_1::id::{Address, Kind, ResourceType};
            use crate::version::v0_0_1::messaging::{Exchange, ExchangeId};
            use crate::version::v0_0_1::util::{unique_id, ConvertFrom};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<ReqEntity> {
                pub from: Address,
                pub entity: ReqEntity,
                pub exchange: Exchange,
            }

            impl<ReqEntity> Request<ReqEntity> {
                pub fn exchange(self, exchange: Exchange) -> Request<ReqEntity> {
                    Self {
                        from: self.from,
                        entity: self.entity,
                        exchange,
                    }
                }

                pub fn ensure_notification(&self) -> Result<(), Error> {
                    match self.exchange {
                        Exchange::Notification => Ok(()),
                        Exchange::RequestResponse(_) => Err(
                            "expected Notification Exchange but found RequestResponse Exchange"
                                .into(),
                        ),
                    }
                }

                pub fn ok<PAYLOAD>(
                    self,
                    payload: PAYLOAD,
                ) -> Result<inlet::Response<PAYLOAD>, Error> {
                    Ok(inlet::Response {
                        id: unique_id(),
                        to: self.from,
                        exchange: self.exchange.try_into()?,
                        entity: RespEntity::Ok(payload),
                    })
                }

                pub fn fail<PAYLOAD>(
                    self,
                    fail: fail::Fail,
                ) -> Result<inlet::Response<PAYLOAD>, Error> {
                    Ok(inlet::Response {
                        id: unique_id(),
                        to: self.from,
                        exchange: self.exchange.try_into()?,
                        entity: RespEntity::Fail(fail),
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Response<PAYLOAD> {
                pub id: String,
                pub to: Address,
                pub from: Address,
                pub entity: response::RespEntity<PAYLOAD>,
                pub exchange: ExchangeId,
            }

            impl<PAYLOAD> Response<PAYLOAD> {
                pub fn new(
                    to: Address,
                    from: Address,
                    entity: response::RespEntity<PAYLOAD>,
                    exchange: ExchangeId,
                ) -> Self {
                    Self {
                        id: unique_id(),
                        to,
                        from,
                        entity,
                        exchange,
                    }
                }
            }

            impl<FromPayload> Response<FromPayload> {
                pub fn convert<ToAddress, ToPayload>(self) -> Result<Response<ToPayload>, Error>
                where
                    ToPayload: TryFrom<FromPayload, Error = Error>,
                {
                    Ok(Response {
                        id: self.id,
                        to: self.to.try_into()?,
                        from: self.from.try_into()?,
                        entity: ConvertFrom::convert_from(self.entity)?,
                        exchange: self.exchange,
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
            pub enum Frame<KIND, PAYLOAD,ReqEntity> {
                Create(Info<KIND>),
                CommandEvent(CommandEvent),
                Request(Request<ReqEntity>),
                Response(Response<PAYLOAD>),
                Close(CloseReason),
            }

            /*
            impl<FromKey, FromAddress, FromIdentifier, FromKind, FromPayload>
                Frame<FromKey, FromAddress, FromKind, FromPayload>
            {
                pub fn convert<ToKey, ToAddress, ToKind, ToPayload>(
                    self,
                ) -> Result<Frame<ToKey, ToAddress, ToKind, ToPayload>, Error>
                where
                    ToKey: TryFrom<FromKey, Error = Error>,
                    ToAddress: TryFrom<FromAddress, Error = Error>,
                    ToKind: TryFrom<FromKind, Error = Error>,
                    ToPayload: ConvertFrom<FromPayload>,
                {
                    match self {
                        Frame::Init(init) => Ok(Frame::Init(init.convert()?)),
                        Frame::CommandEvent(event) => Ok(Frame::CommandEvent(event)),
                        Frame::Request(request) => Ok(Frame::Request(request.convert()?)),
                        Frame::Response(response) => Ok(Frame::Response(response.convert()?)),
                        Frame::BinParcel(parcel) => Ok(Frame::BinParcel(parcel)),
                        Frame::Close(close) => Ok(Frame::Close(close)),
                    }
                }
            }
             */

            impl<KIND: Serialize, PAYLOAD: Serialize, ReqEntity:Serialize> TryInto<PrimitiveFrame> for Frame<KIND, PAYLOAD, ReqEntity> {
                type Error = Error;

                fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                    Ok(PrimitiveFrame {
                        data: bincode::serialize(&self)?,
                    })
                }
            }

            /*
            impl<
                KEY: Serialize,
                ADDRESS: Serialize,
                IDENTIFIER: Serialize,
                KIND: Serialize,
                PAYLOAD: Serialize,
            > TryFrom<PrimitiveFrame> for Frame<KEY, ADDRESS, IDENTIFIER, KIND, PAYLOAD>
            {
                type Error = Error;

                fn try_from(frame: PrimitiveFrame) -> Result<PrimitiveFrame, Self::Error> {
                    Ok(
                        bincode::deserialize(frame.data.as_slice())?
                     )
                }
            }

             */
        }
    }

    pub mod payload {
        use std::collections::HashMap;
        use std::convert::{TryFrom, TryInto};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::marker::PhantomData;
        use std::ops::{Deref, DerefMut};
        use std::path::Path;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::error::Error;
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::resource::{command, Resource, ResourceStub};
        use crate::version::v0_0_1::id::{Address, Meta};
        use crate::version::v0_0_1::payload::PrimitiveType;
        use crate::version::v0_0_1::resource::Status;
        use crate::version::v0_0_1::util::{Convert, ConvertFrom, ValueMatcher, ValuePattern};
        use crate::version::v0_0_1::{http, State};
        use crate::version::v0_0_1::generic::resource::command::RcCommandType;

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

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct ListPattern {
            pub primitive: PrimitiveType,
            pub range: Range,
        }

        impl ListPattern {
            pub fn is_match<KIND>(&self, list: &PrimitiveList<KIND>) -> Result<(), Error> {
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

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub enum Range {
            MinMax { min: usize, max: usize },
            Exact(usize),
            Any,
        }

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub enum PayloadTypePattern {
            Empty,
            Primitive(PrimitiveType),
            List(ListPattern),
            Map(Box<MapPattern>),
        }

        impl PayloadTypePattern {
            pub fn is_match<Kind>(&self, payload: &Payload<Kind>) -> Result<(), Error> {
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

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct PayloadPattern {
            pub structure: PayloadTypePattern,
            pub format: Option<PayloadFormat>,
            pub validator: Option<CallWithConfig>,
        }


        impl<Kind> ValueMatcher<Payload<Kind>>
            for PayloadPattern
        {
            fn is_match(&self, payload: &Payload<Kind>) -> Result<(), Error> {
                self.structure.is_match(&payload)?;

                // more matching to come... not sure exactly how to match Format and Validation...
                Ok(())
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct CallWithConfig {
            pub call: Call,
            pub config: Option<Address>,
        }

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct Call {
            pub address: Address,
            pub kind: CallKind
        }

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub enum CallKind {
            Rc(RcCommandType),
            Msg(MsgCall),
            Http(HttpCall),
        }

        impl ToString for Call{
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

        impl ToString for CallKind{
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
            pub fn is_match<KIND>(
                &self,
                primitive: &generic::payload::Primitive<KIND>,
            ) -> Result<(), Error>
            where
                KIND: Clone,
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

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct MapPattern {
            pub required:
                HashMap<String, ValuePattern<PayloadPattern>>,
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
                required: HashMap<
                    String,
                    ValuePattern<PayloadPattern>,
                >,
                allowed: ValuePattern<PayloadPattern>,
            ) -> Self {
                MapPattern {
                    required,
                    allowed,
                }
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

            pub fn is_match<Kind>(&self, map: &PayloadMap<Kind>) -> Result<(), Error> {
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

        impl<PAYLOAD, PAYLOAD_REF> Convert<PAYLOAD> for PayloadDelivery<PAYLOAD, PAYLOAD_REF> {
            fn convert(self) -> Result<PAYLOAD, crate::error::Error> {
                match self {
                    PayloadDelivery::Payload(payload) => Ok(payload),
                    PayloadDelivery::Ref(_) => {
                        Err("PayloadDelivery was a reference to a payload".into())
                    }
                }
            }
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

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
        pub enum Payload<KIND> {
            Empty,
            Primitive(Primitive<KIND>),
            List(PrimitiveList<KIND>),
            Map(PayloadMap<KIND>),
        }

        impl<KIND> Payload<KIND> {
            pub fn payload_type(&self) -> PayloadType {
                match self {
                    Payload::Empty => PayloadType::Empty,
                    Payload::Primitive(primitive) => PayloadType::Primitive,
                    Payload::List(list) => PayloadType::List,
                    Payload::Map(map) => PayloadType::Map,
                }
            }
        }

        impl<FromKind> Payload<FromKind> {
            pub fn convert<ToKind>(self) -> Result<Payload<ToKind>, Error>
            where
                FromKind: TryInto<ToKind, Error = Error>,
            {
                Ok(match self {
                    Payload::Empty => Payload::Empty,
                    Payload::Primitive(primitive) => Payload::Primitive(primitive.convert()?),
                    Payload::List(list) => Payload::List(list.convert()?),
                    Payload::Map(map) => Payload::Map(map.convert()?),
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct PayloadMap<KIND> {
            pub map: HashMap<String, Payload<KIND>>,
        }

        impl<Kind> Into<Payload<Kind>> for PayloadMap<Kind> {
            fn into(self) -> Payload<Kind> {
                Payload::Map(self)
            }
        }


        impl<FromKind> PayloadMap<FromKind> {
            pub fn convert<ToKind>(self) -> Result<PayloadMap<ToKind>, Error>
            where
                FromKind: TryInto<ToKind, Error = Error>,
            {
                let mut map = HashMap::new();
                for (k, v) in self.map {
                    map.insert(k, v.convert()?);
                }
                Ok(PayloadMap { map })
            }
        }

        impl <Kind> Deref for PayloadMap<Kind> {
            type Target = HashMap<String,Payload<Kind>>;

            fn deref(&self) -> &Self::Target {
                &self.map
            }
        }

        impl <Kind> DerefMut for PayloadMap<Kind> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.map
            }
        }

        impl <Kind> Default for PayloadMap<Kind> {

            fn default() -> Self {
                Self {
                    map: Default::default()
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

        impl<KIND> PayloadMap<KIND> {
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

       impl<FromKind, ToKind> ConvertFrom<Payload<FromKind>> for Payload<ToKind>
        where
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            ToKind: Clone,
        {
            fn convert_from(a: Payload<FromKind>) -> Result<Self, Error>
            where
                Self: Sized,
            {
                match a {
                    Payload::Empty => Ok(Payload::Empty),
                    Payload::Primitive(primitive) => {
                        Ok(Payload::Primitive(ConvertFrom::convert_from(primitive)?))
                    }
                    Payload::List(list) => {
                        let mut rtn: PrimitiveList<ToKind> =
                            PrimitiveList::new(list.primitive_type);
                        for p in list.list {
                            rtn.push(ConvertFrom::convert_from(p)?);
                        }
                        Ok(Payload::List(rtn))
                    }
                    Payload::Map(map) => {
                        //let mut rtn: PayloadMap<ToKey,ToAddress,ToIdentifier,ToKind> = PayloadMap::new(ConvertFrom::convert_from(map.constraints )? );
                        let mut rtn: PayloadMap<ToKind> = PayloadMap::new();

                        for (key, payload) in map.map {
                            rtn.insert(key, ConvertFrom::convert_from(payload)?);
                        }

                        Ok(Payload::Map(rtn))
                    }
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq,strum_macros::Display)]
        pub enum Primitive<KIND> {
            Text(String),
            Address(Address),
            Stub(ResourceStub<KIND>),
            Meta(Meta),
            Bin(Bin),
            Boolean(bool),
            Int(i64),
            Status(Status),
            Resource(Resource<KIND>),
        }

        impl<FromKind> Primitive<FromKind> {
            fn convert<ToKind>(self) -> Result<Primitive<ToKind>, Error>
            where
                FromKind: TryInto<ToKind, Error = Error>,
            {
                Ok(match self {
                    Primitive::Text(value) => Primitive::Text(value),
                    Primitive::Address(value) => Primitive::Address(value),
                    Primitive::Stub(value) => Primitive::Stub(value.convert()?),
                    Primitive::Meta(value) => Primitive::Meta(value),
                    Primitive::Bin(value) => Primitive::Bin(value),
                    Primitive::Boolean(value) => Primitive::Boolean(value),
                    Primitive::Int(value) => Primitive::Int(value),
                    Primitive::Status(value) => Primitive::Status(value),
                    Primitive::Resource(value) => Primitive::Resource(value.convert()?),
                })
            }
        }

        impl<KIND> Primitive<KIND> {
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

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct PrimitiveList<KIND> {
            pub primitive_type: PrimitiveType,
            pub list: Vec<Primitive<KIND>>,
        }



        impl<FromKind> PrimitiveList<FromKind> {
            fn convert<ToKind>(self) -> Result<PrimitiveList<ToKind>, Error>
            where
                FromKind: TryInto<ToKind, Error = Error>,
            {
                let mut list = vec![];
                for p in self.list {
                    list.push(p.convert()?);
                }
                Ok(PrimitiveList {
                    primitive_type: self.primitive_type,
                    list,
                })
            }
        }

        impl<KIND> ToString for PrimitiveList<KIND>
        where
            KIND: Clone,
        {
            fn to_string(&self) -> String {
                format!("{}[]", self.primitive_type.to_string())
            }
        }

        impl<KIND> PrimitiveList<KIND>
        where
            KIND: Clone,
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

        impl<KIND> Deref for PrimitiveList<KIND>
        where
            KIND: Clone,
        {
            type Target = Vec<Primitive<KIND>>;

            fn deref(&self) -> &Self::Target {
                &self.list
            }
        }

        impl<KIND> DerefMut for PrimitiveList<KIND>
        where
            KIND: Clone,
        {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.list
            }
        }

        impl<FromKind, ToKind> ConvertFrom<Primitive<FromKind>> for Primitive<ToKind>
        where
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            ToKind: Clone,
        {
            fn convert_from(a: Primitive<FromKind>) -> Result<Self, Error>
            where
                Self: Sized,
            {
                match a {
                    Primitive::Text(text) => Ok(Primitive::Text(text)),
                    Primitive::Address(address) => Ok(Primitive::Address(address.try_into()?)),
                    Primitive::Stub(stub) => Ok(Primitive::Stub(ConvertFrom::convert_from(stub)?)),
                    Primitive::Meta(meta) => Ok(Primitive::Meta(meta)),
                    Primitive::Bin(bin) => Ok(Primitive::Bin(bin)),
                    Primitive::Boolean(boolean) => Ok(Primitive::Boolean(boolean)),
                    Primitive::Int(int) => Ok(Primitive::Int(int)),
                    Primitive::Status(status) => Ok(Primitive::Status(status)),
                    Primitive::Resource(resource) => {
                        Ok(Primitive::Resource(ConvertFrom::convert_from(resource)?))
                    }
                }
            }
        }
    }

    pub mod pattern {
        use crate::error::Error;
        use crate::version::v0_0_1::id::{AddressSegment, Tks, RouteSegment};
        use crate::version::v0_0_1::pattern::{SegmentPattern, SpecificPattern, ExactSegment};
        use crate::version::v0_0_1::util::{ValueMatcher, ValuePattern};
        use serde::{Deserialize, Serialize};
        use std::convert::TryInto;
        use crate::version::v0_0_1::id::Address;
        use crate::version::latest::generic::payload::Payload;
        use crate::version::latest::fail::Fail;
        use std::str::FromStr;
        use nom::combinator::all_consuming;
        use crate::version::v0_0_1::parse::{address_kind_path, consume_address_kind_path};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct AddressKindPattern<ResourceType, Kind> {
            pub hops: Vec<Hop<ResourceType, Kind>>,
        }

        impl<ResourceType, Kind> AddressKindPattern<ResourceType, Kind> {
            fn consume(&self) -> Option<AddressKindPattern<ResourceType, Kind>>
            where
                ResourceType: Clone,
                Kind: Clone,
            {
                if self.hops.len() <= 1 {
                    Option::None
                } else {
                    let mut hops = self.hops.clone();
                    hops.remove(0);
                    Option::Some(AddressKindPattern { hops })
                }
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
                                segments.push(seg.clone() );
                            }
                        }
                    } else {
                        break;
                    }
                }

                Address {
                    route: RouteSegment::Resource,
                    segments
                }
            }

            pub fn sub_select_hops(&self) -> Vec<Hop<ResourceType,Kind>> where ResourceType:Clone, Kind:Clone{

                let mut hops= self.hops.clone();
                let query_root_segments = self.query_root().segments.len();
                for _ in 0..query_root_segments {
                    hops.remove(0);
                }
                hops
            }


            pub fn matches(&self, address_kind_path: &AddressKindPath<Kind>) -> bool
            where
                ResourceType: Clone,
                Kind: Clone,
            {
                if address_kind_path.segments.len() < self.hops.len() {
                    return false;
                }

                if address_kind_path.segments.is_empty() || self.hops.is_empty() {
                    return false;
                }

                let hop = self.hops.first().expect("hop");
                let seg = address_kind_path.segments.first().expect("segment");

                if address_kind_path.is_final() && self.is_final() {
                    // this is the final hop & segment if they match, everything matches!
                    hop.matches(seg)
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
                    // in a normal match situation, we consume the hop and move to the next one
                    self.consume()
                        .expect("AddressTksPattern")
                        .matches(&address_kind_path.consume().expect("AddressKindPath"))
                } else {
                    false
                }
            }
        }

        impl<FromResourceType, FromKind> AddressKindPattern<FromResourceType, FromKind> {
            pub fn convert<ToResourceType, ToKind>(
                self,
            ) -> Result<AddressKindPattern<ToResourceType, ToKind>, Error>
            where
                FromResourceType: TryInto<ToResourceType, Error = Error> + Eq + PartialEq,
                FromKind: TryInto<ToKind, Error = Error> + Eq + PartialEq,
            {
                let mut hops = vec![];
                for hop in self.hops {
                    hops.push(hop.convert()?);
                }
                Ok(AddressKindPattern { hops })
            }
        }

        pub type KindPattern<Kind> = Pattern<Kind>;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Hop<ResourceType, Kind> {
            pub segment: SegmentPattern,
            pub tks: TksPattern<ResourceType, Kind>,
        }

        impl<FromResourceType, FromKind> Hop<FromResourceType, FromKind> {
            pub fn convert<ToResourceType, ToKind>(
                self,
            ) -> Result<Hop<ToResourceType, ToKind>, Error>
            where
                FromResourceType: TryInto<ToResourceType, Error = Error> + Eq + PartialEq,
                FromKind: TryInto<ToKind, Error = Error> + Eq + PartialEq,
            {
                Ok(Hop {
                    segment: self.segment,
                    tks: self.tks.convert()?,
                })
            }
        }

        impl<ResourceType, Kind> Hop<ResourceType, Kind> {
            pub fn matches(&self, address_kind_segment: &AddressKindSegment<Kind>) -> bool {
                self.segment
                    .matches(&address_kind_segment.address_segment)
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










        pub type ResourceTypePattern<ResourceType> = Pattern<ResourceType>;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct TksPattern<ResourceType, Kind> {
            pub resource_type: ResourceTypePattern<ResourceType>,
            pub kind: KindPattern<Kind>,
            pub specific: ValuePattern<SpecificPattern>,
        }




        impl<FromResourceType, FromKind> TksPattern<FromResourceType, FromKind> {
            pub fn convert<ToResourceType, ToKind>(
                self,
            ) -> Result<TksPattern<ToResourceType, ToKind>, Error>
            where
                FromResourceType: TryInto<ToResourceType, Error = Error> + Eq + PartialEq,
                FromKind: TryInto<ToKind, Error = Error> + Eq + PartialEq,
            {
                Ok(TksPattern {
                    resource_type: self.resource_type.convert()?,
                    kind: self.kind.convert()?,
                    specific: self.specific,
                })
            }
        }

        impl<ResourceType, Kind> TksPattern<ResourceType, Kind>
        where
            Kind: Tks<ResourceType>,
            ResourceType: Eq + PartialEq,
        {
            pub fn new(
                resource_type: ResourceTypePattern<ResourceType>,
                kind: KindPattern<Kind>,
                specific: ValuePattern<SpecificPattern>,
            ) -> Self {
                Self {
                    resource_type,
                    kind,
                    specific,
                }
            }

            pub fn matches(&self, kind: &Kind) -> bool
            where
                Kind: Eq + PartialEq,
            {
                self.resource_type.matches(&kind.resource_type())
                    && self.kind.matches(kind)
                    && self.specific.is_match_opt(kind.specific().as_ref()).is_ok()
            }
        }

        impl<ResourceType, Kind> TksPattern<ResourceType, Kind> {
            pub fn any() -> Self {
                Self {
                    resource_type: ResourceTypePattern::Any,
                    kind: KindPattern::Any,
                    specific: ValuePattern::Any,
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct AddressKindPath<Kind> {
            pub route: RouteSegment,
            pub segments: Vec<AddressKindSegment<Kind>>,
        }

        impl<Kind> AddressKindPath<Kind> {

            pub fn push(&self, segment: AddressKindSegment<Kind> ) -> AddressKindPath<Kind> where Kind: Clone {
                let mut segments = self.segments.clone();
                segments.push(segment);
                Self{
                    route: self.route.clone(),
                    segments
                }
            }

            pub fn consume(&self) -> Option<AddressKindPath<Kind>>
            where
                Kind: Clone,
            {
                if self.segments.len() <= 1 {
                    return Option::None;
                }
                let mut segments = self.segments.clone();
                segments.remove(0);
                Option::Some(AddressKindPath {
                    route: self.route.clone(),
                    segments })
            }

            pub fn is_final(&self) -> bool {
                self.segments.len() == 1
            }
        }

        impl<Kind:FromStr> FromStr for AddressKindPath<Kind> {
            type Err = Error;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(consume_address_kind_path(s)?)
            }
        }

        impl<Kind:ToString> ToString for AddressKindPath<Kind>{
            fn to_string(&self) -> String {
                let mut rtn = String::new();
                match &self.route {
                    RouteSegment::Resource => {}
                    route => {
                        rtn.push_str(r.to_string().as_str());
                        rtn.push_str("::" );
                    }
                }

                for (index,segment) in self.segments.iter().enumerate() {
                    rtn.push_str( segment.to_string().as_str() );
                    if index < self.segments.len() {
                        rtn.push_str( segment.address_segment.terminating_delim() );
                    }
                }

                rtn
            }
        }

        impl<FromKind>
        AddressKindPath<FromKind>
        {
            pub fn convert<ToKind>(
                self,
            ) -> Result<AddressKindPath<ToKind>, Error>
                where
                    FromKind: TryInto<ToKind, Error = Error>+Clone+Eq+PartialEq,
            {
                let mut segments = vec![];
                for segment in self.segments {
                    segments.push( segment.convert()? );
                }
                Ok(AddressKindPath {
                    route: self.route,
                  segments
                })
            }
        }
        #[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
        pub struct AddressKindSegment<Kind> {
            pub address_segment: AddressSegment,
            pub kind: Kind,
        }

        impl <FromKind> AddressKindSegment<FromKind> {
            pub fn convert<ToKind>(
                self,
            ) -> Result<AddressKindSegment<ToKind>, Error>
                where
                    FromKind: TryInto<ToKind, Error = Error>+Clone+Eq+PartialEq,
            {
                Ok(AddressKindSegment {
                    address_segment: self.address_segment,
                    kind: self.kind.try_into()?
                })
            }
        }

        impl <Kind: ToString> ToString for AddressKindSegment<Kind> {
            fn to_string(&self) -> String {
                format!("{}<{}>", self.address_segment.to_string(), self.kind.to_string() )
            }
        }

        impl <Kind> FromStr for AddressKindPath<Kind> where Kind: FromStr<Err=Error> {
            type Err = Error;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                let (_,rtn) = all_consuming(address_kind_path)(s)?;
                Ok(rtn)
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

    #[derive(Debug, Eq, PartialEq)]
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

    use crate::version::v0_0_1::id::Specific;

    pub mod mesh {
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Error(String)
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

        use crate::version::v0_0_1::fail::{Bad, BadRequest, Conditional, Messaging, NotFound, BadCoercion};
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
            InvalidProperty{ expected: String, found: String }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Update {
            Immutable,
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Select {
            WrongAddress{required:Address,found:Address},
            BadSelectRouting{required:String,found:String},
            BadCoercion(BadCoercion)
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
        pub into: String
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
    }
}

pub mod parse {
    use nom::combinator::{all_consuming, not, opt, recognize};
    use nom::error::{context, ErrorKind, VerboseError};
    use nom::multi::{many0, separated_list1};
    use nom::{AsChar, IResult, InputTakeAtPosition};

    use crate::version::latest::util::StringMatcher;
    use crate::version::v0_0_1::id::{Address, AddressSegment, RouteSegment, Version};
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::bytes::complete::{is_a, is_not};
    use nom::character::complete::{alpha0, digit1, alphanumeric1};
    use nom::sequence::{delimited, preceded, terminated, tuple};
    use nom_supreme::parse_from_str;
    use crate::version::v0_0_1::generic::id::parse::version;
    use crate::version::latest::generic::pattern::{AddressKindSegment, AddressKindPath};
    use std::str::FromStr;
    use crate::error::Error;
    use crate::version::v0_0_1::pattern::parse::{kind, delim_kind};

    pub struct Parser {}

    impl Parser {
        pub fn address(input: &str) -> Res<&str, Address> {
            address(input)
        }

        pub fn consume_address(input: &str) -> Result<Address,Error> {
            let (_,address) = all_consuming(address)(input)?;
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


    pub fn hub_segment(input: &str) -> Res<&str, RouteSegment> {
        alt((tag_route_segment, domain_route_segment, mesh_route_segment,resource_route_segment))(input)
    }

    pub fn space_address_segment(input: &str) -> Res<&str, AddressSegment> {
        space_chars(input).map(|(next, space)| (next, AddressSegment::Space(space.to_string())))
    }

    pub fn base_address_segment(input: &str) -> Res<&str, AddressSegment> {
        skewer_chars(input).map(|(next, base)| (next, AddressSegment::Base(base.to_string())))
    }

    pub fn filepath_address_segment(input: &str) -> Res<&str, AddressSegment> {
        alt( (file_address_segment,dir_address_segment) )(input)
    }
    pub fn dir_address_segment(input: &str) -> Res<&str, AddressSegment> {
        terminated(filepath_chars,tag("/"))(input).map(|(next, name)| (next, AddressSegment::Dir(name.to_string())))
    }

    pub fn file_address_segment(input: &str) -> Res<&str, AddressSegment> {
        file_chars(input)
            .map(|(next, filename)| (next, AddressSegment::File(filename.to_string())))
    }

    pub fn version_address_segment(input: &str) -> Res<&str, AddressSegment> {
       version(input)
            .map(|(next, version)| (next, AddressSegment::Version(version)))
    }


    pub fn address(input: &str) -> Res<&str, Address> {
        tuple((
            tuple((hub_segment, space_address_segment)),
            many0(base_address_segment),
            opt( version_address_segment ),
            many0(filepath_address_segment ),
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

            let address = Address { route: hub, segments };

            (next, address)
        })
    }




    pub fn consume_address(input: &str) -> Res<&str, Address> {
        all_consuming(address)(input)
    }



    pub fn space_address_kind_segment<Kind>(input: &str) -> Res<&str, AddressKindSegment<Kind>> {
        tuple((space_address_segment,delim_kind))(input).map(|(next, (address_segment,kind))| (next, AddressKindSegment{ address_segment, kind}))
    }

    pub fn base_address_kind_segment<Kind>(input: &str) -> Res<&str, AddressKindSegment<Kind>> {
        tuple((base_address_segment,delim_kind))(input).map(|(next, (address_segment,kind))| (next, AddressKindSegment{ address_segment, kind}))
    }

    pub fn filepath_address_kind_segment<Kind>(input: &str) -> Res<&str, AddressKindSegment<Kind>> {
        alt( (file_address_kind_segment,dir_address_kind_segment) )(input)
    }
    pub fn dir_address_kind_segment<Kind>(input: &str) -> Res<&str, AddressKindSegment<Kind>> {
        tuple((dir_address_segment,delim_kind))(input).map(|(next, (address_segment,kind))| (next, AddressKindSegment{ address_segment, kind}))
    }

    pub fn file_address_kind_segment<Kind>(input: &str) -> Res<&str, AddressKindSegment<Kind>> {
        tuple((file_address_segment,delim_kind))(input).map(|(next, (address_segment,kind))| (next, AddressKindSegment{ address_segment, kind}))
    }

    pub fn version_address_kind_segment<Kind>(input: &str) -> Res<&str, AddressKindSegment<Kind>> {
        tuple((version_address_segment,delim_kind))(input).map(|(next, (address_segment,kind))| (next, AddressKindSegment{ address_segment, kind}))
    }

    pub fn consume_address_kind_path<Kind>(input: &str) -> Result<AddressKindPath<Kind>,Error> {
        let (_,rtn ) = all_consuming(address_kind_path)(input)?;
        Ok(rtn)
    }

    pub fn address_kind_path<Kind>(input: &str) -> Res<&str, AddressKindPath<Kind>> {
        tuple((
            tuple((hub_segment, space_address_kind_segment)),
            many0(base_address_kind_segment),
            opt( version_address_kind_segment ),
            many0(file_address_kind_segment ),
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

                let address = AddressKindPath { route: hub, segments };

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
                !(char_item == '-')
                    && !((char_item.is_alpha() && char_item.is_lowercase())
                        || char_item.is_dec_digit())
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



    pub fn skewer_chars<T>(i: T) -> Res<T, T>
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

}
