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

    use serde::{Deserialize, Serialize};

    use crate::error::Error;
    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::parse::{address, consume_address, Res};
    use semver::Version;

    pub type ResourceType = String;
    pub type Kind = generic::id::GenericKind<ResourceType>;
    pub type AddressAndKind = generic::id::AddressAndKind<Kind>;
    pub type AddressAndType = generic::id::AddressAndType<ResourceType>;
    pub type Meta = HashMap<String, String>;
    pub type PayloadClaim = String;



    #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
    pub struct Specific{
        pub vendor: String,
        pub product: String,
        pub variant: String,
        pub version: Version,
    }

    impl ToString for Specific{
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

    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
    pub enum HubSegment{
        Local,
        Domain(String),
        Tag(String)
    }

    impl ToString for HubSegment {
        fn to_string(&self) -> String {
           match self {
               HubSegment::Local => {
                   "".to_string()
               }
               HubSegment::Domain(domain) => {
                   domain.clone()
               }
               HubSegment::Tag(tag) => {
                   format!("[{}]",tag)
               }
           }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
    pub enum AddressSegment {
        Space(String),
        Base(String),
        Dir,
        File(String)
    }

    impl AddressSegment {
        pub fn terminating_delim(&self) -> &str {
           match self {
               AddressSegment::Space(_) => {
                   ":"
               }
               AddressSegment::Base(_) => {
                   ":"
               }
               AddressSegment::Dir => {
                   ""
               }
               AddressSegment::File(_) => {
                   ""
               }
           }
        }

        pub fn as_str(&self) -> &str {
            match self {
                AddressSegment::Space(space) => {
                    space.as_str()
                }
                AddressSegment::Base(base) => {
                    base.as_str()
                }
                AddressSegment::Dir => {
                    "/"
                }
                AddressSegment::File(file) => {
                    file.as_str()
                }
            }
        }
    }

    impl ToString for AddressSegment {
        fn to_string(&self) -> String {
            match self {
                AddressSegment::Space(space) => {
                    space.clone()
                }
                AddressSegment::Base(base) => {
                    base.clone()
                }
                AddressSegment::Dir => {
                    "/".to_string()
                }
                AddressSegment::File(file) => {
                    file.clone()
                }
            }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
    pub struct Address {
        pub hub: HubSegment,
        pub segments: Vec<AddressSegment>
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

            match &self.hub {
                HubSegment::Local => {}
                HubSegment::Domain(domain) => {
                    rtn.push_str(format!("{}::",domain).as_str());
                }
                HubSegment::Tag(tag) => {
                    rtn.push_str(format!("[{}]::",tag).as_str());
                }
            }

            for (i, segment) in self.segments.iter().enumerate() {
                rtn.push_str( segment.as_str() );
                if i != self.segments.len()-1 {
                    rtn.push_str(segment.terminating_delim() );
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
            segments.remove( segments.len() );
            Option::Some( Self {
                hub: self.hub.clone(),
                segments
            })
        }

        pub fn parse( input: &str ) -> Res<&str,Self> {
            address(input)
        }

        pub fn root() -> Self {
            Self{
                hub: HubSegment::Local,
                segments: vec![]
            }
        }

        pub fn is_root(&self)->bool {
            self.segments.is_empty()
        }
    }
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum IdentifierKind {
        Key,
        Address,
    }
}

pub mod pattern {
    use semver::VersionReq;
    use std::ops::Deref;
    use std::str::FromStr;
    use crate::version::v0_0_1::pattern::specific::{VendorPattern, ProductPattern, VariantPattern};
    use crate::version::v0_0_1::id::Specific;

    #[derive(Eq, PartialEq)]
    pub struct AddressTksPattern {
        pub hops: Vec<Hop>,
    }

    impl AddressTksPattern {
        pub fn consume(&self) -> Option<AddressTksPattern> {
            if self.hops.len() <= 1 {
                Option::None
            } else {
                let mut hops = self.hops.clone();
                hops.remove(0);
                Option::Some(AddressTksPattern { hops });
            }
        }

        pub fn is_final(&self) -> bool {
            self.hops.len() == 1
        }

        pub fn matches(&self, address_tks_path: &AddressTksPath) -> bool {
            if address_tks_path.segments.len() < self.hops.len() {
                return false;
            }

            if address_tks_path.segments.is_empty() || self.hops.is_empty() {
                return false;
            }

            let hop = self.hops.first().expect("hop");
            let seg = address_tks_path.segments.first().expect("segment");

            if address_tks_path.is_final() && self.is_final() {
                // this is the final hop & segment if they match, everything matches!
                hop.matches(seg)
            } else if address_tks_path.is_final() {
                // we still have hops that haven't been matched and we are all out of path
                false
            }
            // special logic is applied to recursives **
            else if hop.segment.is_recursive() && self.hops.len() >= 2 {
                // a Recursive is similar to an Any in that it will match anything, however,
                // let's see if the NEXT hop will match the segment
                let next_hop = self.hops.get(1).expect("next<Hop>");
                if next_hop.matches(seg) {
                    // since the next hop after the recursive matches, we consume the recursive and continue hopping
                    // this allows us to make matches like:
                    // space.org:**:users ~ space.org:many:silly:dirs:users
                    self.consume()
                        .expect("AddressTksPattern")
                        .matches(&address_tks_path.consume().expect("AddressTksPath"))
                } else {
                    // the NEXT hop does not match, therefore we do NOT consume() the current hop
                    self.matches(&address_tks_path.consume().expect("AddressTksPath"))
                }
            } else if hop.matches(seg) {
                // in a normal match situation, we consume the hop and move to the next one
                self.consume()
                    .expect("AddressTksPattern")
                    .matches(&address_tks_path.consume().expect("AddressTksPath"))
            } else {
                false
            }
        }
    }

    #[derive(Clone, Eq, PartialEq)]
    pub enum SegmentPattern {
        Any,       // *
        Recursive, // **
        Exact(ExactSegment),
    }

    impl SegmentPattern {
        pub fn matches(&self, segment: &String) -> bool {
            match self {
                SegmentPattern::Any => true,
                SegmentPattern::Recursive => true,
                SegmentPattern::Exact(exact) => match exact {
                    ExactSegment::Address(pattern) => *pattern == *segment,
                },
            }
            false
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
    pub type AddressSegment = String;

    #[derive(Clone, Eq, PartialEq)]
    pub enum ExactSegment {
        Address(AddressSegment),
    }

    impl ExactSegment {
        pub fn matches(&self, segment: &AddressSegment) -> bool {
            match self {
                ExactSegment::Address(s) => *s == *segment,
            }
            false
        }
    }

    #[derive(Clone, Eq, PartialEq)]
    pub struct Hop {
        pub segment: SegmentPattern,
        pub tks: TksPattern,
    }

    impl Hop {
        pub fn matches(&self, address_tks_segment: &AddressTksSegment) -> bool {
            self.segment.matches(&address_tks_segment.address_segment)
        }
    }

    #[derive(Eq, PartialEq)]
    pub enum Pattern<P> {
        Any,
        Exact(P),
    }

    impl<P> Pattern<P> {
        pub fn matches(&self, t: &P) -> bool {
            match self {
                Self::Any => true,
                Self::Exact(p) => *p == t,
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
    }

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

    pub type ResourceTypePattern = Pattern<String>;
    pub type KindPattern = Pattern<String>;

    pub mod specific {
        use semver::VersionReq;
        use std::ops::Deref;
        use std::str::FromStr;
        use crate::pattern::Pattern;
        use crate::error::Error;
        use crate::parse::SkewerCase;
        use crate::version::v0_0_1::pattern::Pattern;

        pub struct Version {
            pub req: VersionReq,
        }

        impl Deref for Version {
            type Target = VersionReq;

            fn deref(&self) -> &Self::Target {
                &self.req
            }
        }

        impl FromStr for Version {
            type Err = Error;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(Version {
                    req: VersionReq::from_str(s)?,
                })
            }
        }

        pub type VendorPattern = Pattern<String>;
        pub type ProductPattern = Pattern<String>;
        pub type VariantPattern = Pattern<String>;
        pub type VersionPattern = Pattern<Version>;
    }

    #[derive(Eq, PartialEq)]
    pub struct SpecificPattern {
        pub vendor: VendorPattern,
        pub product: ProductPattern,
        pub variant: VariantPattern,
        pub version: VersionReq,
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

    #[derive(Eq, PartialEq)]
    pub struct TksPattern {
        pub resource_type: ResourceTypePattern,
        pub kind: KindPattern,
        pub specific: Pattern<SpecificPattern>,
    }

    impl TksPattern {
        pub fn new(
            resource_type: ResourceTypePattern,
            kind: KindPattern,
            specific: Pattern<SpecificPattern>,
        ) -> Self {
            Self {
                resource_type,
                kind,
                specific,
            }
        }

        pub fn matches(&self, tks: &Tks) -> bool {
            self.resource_type.matches(&tks.resource_type)
                && self.kind.matches_opt(tks.kind.as_ref())
                && self.specific.matches_opt(tks.kind.specific())
        }
    }

    impl TksPattern {
        pub fn any() -> Self {
            Self {
                resource_type: ResourceTypePattern::Any,
                kind: KindPattern::Any,
                specific: Pattern::Any,
            }
        }
    }

    #[derive(Eq, PartialEq)]
    pub struct AddressTksPath<Kind> {
        pub segments: Vec<AddressTksSegment<Kind>>,
    }

    impl <Kind> AddressTksPath<Kind> {
        pub fn consume(&self) -> Option<AddressTksPath<Kind>> {
            if self.segments.len() <= 1 {
                Option::None
            }
            let mut segments = self.segments.clone();
            segments.remove(0);
            Option::Some(AddressTksPath { segments })
        }

        pub fn is_final(&self) -> bool {
            self.segments.len() == 1
        }
    }

    #[derive(Eq, PartialEq)]
    pub struct AddressTksSegment<Kind> {
        pub address_segment: AddressSegment,
        pub tks: Kind,
    }

    pub mod parse {

        use nom::branch::alt;
        use nom::bytes::complete::tag;
        use nom::character::complete::{alpha1, digit1};
        use nom::combinator::{opt, recognize};
        use nom::error::VerboseError;
        use nom::sequence::{delimited, tuple, terminated};
        use nom::IResult;
        use nom::Parser;
        use nom_supreme::{parse_from_str, ParserExt};
        use semver::VersionReq;
        use crate::pattern::{SegmentPattern, ExactSegment, Pattern, SpecificPattern, KindPattern, ResourceTypePattern, TksPattern, Hop};
        use mesh_portal_parse::parse::{Res, skewer};
        use crate::version::v0_0_1::parse::{Res, skewer, version_req_chars};
        use crate::version::v0_0_1::pattern::{SegmentPattern, ExactSegment, Pattern, SpecificPattern, TksPattern, Hop, KindPattern, ResourceTypePattern};
        use nom::multi::many1;

        fn any_segment(input: &str) -> Res<&str, SegmentPattern> {
            tag("*")(input).map(|(next, _)| (next, SegmentPattern::Any))
        }

        fn recursive_segment(input: &str) -> Res<&str, SegmentPattern> {
            tag("**")(input).map(|(next, _)| (next, SegmentPattern::Recursive))
        }

        fn exact_segment(input: &str) -> Res<&str, SegmentPattern> {
            any_resource_path_segment(input).map(|(next, segment)| {
                (
                    next,
                    SegmentPattern::Exact(ExactSegment::Address(segment.to_string())),
                )
            })
        }

        fn segment(input: &str) -> Res<&str, SegmentPattern> {
            alt((recursive_segment, any_segment, exact_segment))(input)
        }

        fn pattern<P>(
            parse: fn(input: &str) -> Res<&str, P>,
        ) -> impl Fn(&str) -> Res<&str, Pattern<P>> {
            move |input: &str| match tag::<&str, &str, VerboseError<&str>>("*")(input) {
                Ok((next, _)) => Ok((next, Pattern::Any)),
                Err(_) => {
                    let (next, p) = parse(input)?;
                    let pattern = Pattern::Exact(p);
                    Ok((next, pattern))
                }
            }
        }

        fn version_req(input: &str) -> Res<&str, VersionReq> {
            parse_from_str(version_req_chars).parse(input)
        }

        fn domain( input: &str) -> Res<&str,&str> {
            recognize( tuple((many1(terminated(skewer,tag('.'))),skewer)))(input)
        }

        // can be a hostname or domain name
        fn space( input: &str) -> Res<&str,&str> {
            recognize( alt((skewer,domain)))(input)
        }


        fn specific(input: &str) -> Res<&str, SpecificPattern> {
            tuple((
                pattern(domain),
                tag(":"),
                pattern(skewer),
                tag(":"),
                pattern(skewer),
                tag(":"),
                delimited(tag("("), version_req, tag(")")),
            ))(input)
                .map(|(next, (vendor, _, product, _, variant, _, version))| {
                    let specific = SpecificPattern {
                        vendor,
                        product,
                        variant,
                        version,
                    };
                    (next, specific)
                })
        }

        fn kind(input: &str) -> Res<&str, KindPattern> {
            pattern(camel)(input).map(|(next, kind)| (next, kind))
        }

        fn resource_type(input: &str) -> Res<&str, ResourceTypePattern> {
            pattern(camel)(input).map(|(next, resource_type)| (next, resource_type))
        }

        fn tks(input: &str) -> Res<&str, TksPattern> {
            delimited(
                tag("<"),
                tuple((
                    resource_type,
                    opt(delimited(
                        tag("<"),
                        tuple((kind, opt(delimited(tag("<"), pattern(specific), tag(">"))))),
                        tag(">"),
                    )),
                )),
                tag(">"),
            )(input)
                .map(|(next, (resource_type, kind_and_specific))| {
                    let (kind, specific) = match kind_and_specific {
                        None => (Pattern::Any, Pattern::Any),
                        Some((kind, specific)) => (
                            kind,
                            match specific {
                                None => Pattern::Any,
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

        fn hop(input: &str) -> Res<&str, Hop> {
            tuple((segment, opt(tks)))(input).map(|(next, (segment, tks))| {
                let tks = match tks {
                    None => TksPattern::any(),
                    Some(tks) => tks,
                };
                (next, Hop { segment, tks })
            })
        }

        #[cfg(test)]
        pub mod test {

            use nom::combinator::all_consuming;
            use semver::VersionReq;
            use std::str::FromStr;
            use crate::version::v0_0_1::pattern::parse::{segment, hop, specific, tks};
            use crate::error::Error;
            use crate::version::v0_0_1::pattern::{SegmentPattern, ExactSegment, TksPattern, Pattern, SpecificPattern};

            #[test]
            pub fn test_segs() -> Result<(), Error> {
                assert!(segment("*")? == ("", SegmentPattern::Any));
                assert!(segment("**")? == ("", SegmentPattern::Recursive));
                assert!(
                    segment("hello")?
                        == (
                        "",
                        SegmentPattern::Exact(ExactSegment::Address("hello".to_string()))
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
                    specific: Pattern::Any,
                };

                assert!(tks("<App>")? == ("", tks_pattern));

                let tks_pattern = TksPattern {
                    resource_type: Pattern::Exact(CamelCase::new("Database")),
                    kind: Pattern::Exact(CamelCase::new("Relational")),
                    specific: Pattern::Any,
                };

                assert!(tks("<Database<Relational>>")? == ("", tks_pattern));

                let tks_pattern = TksPattern {
                    resource_type: Pattern::Exact(CamelCase::new("Database")),
                    kind: Pattern::Exact(CamelCase::new("Relational")),
                    specific: Pattern::Exact(SpecificPattern {
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
                hop("*<Database<Relational<mysql.org:mysql:innodb:(^7.0.1)>>>")?;
                hop("**<Database<Relational<mysql.org:mysql:innodb:(^7.0.1)>>>")?;
                hop("space.org:<Database<Relational<mysql.org:mysql:innodb:(^7.0.1)>>>")?;
                hop("space.org:something<Database<Relational<mysql.org:mysql:innodb:(^7.0.1)>>>")?;
                hop("space.org:no-type")?;
                hop("space.org:no-type:**")?;
                hop("space.org:app:users:*:tenant:**")?;
                hop("space.org:app:users:*:tenant:**<Mechtron>")?;
                hop("space.org:something:**<*<*<mysql.org:mysql:innodb:(^7.0.1)>>>")?;
                hop("space.org:something<*>")?;

                Ok(())
            }
        }
    }

}

pub mod messaging {
    use serde::{Deserialize, Serialize};
    use std::convert::TryInto;
    use crate::error::Error;

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
                Exchange::Notification => {
                    Err("Exchange Notification cannot be converted into a RequestResponse Exchange".into())
                }
                Exchange::RequestResponse(id) => {
                    Ok(id)
                }
            }
        }
    }

    impl Into<ExchangeType> for Exchange {
        fn into(self) -> ExchangeType {
            match self {
                Exchange::Notification => {
                    ExchangeType::Notification
                }
                Exchange::RequestResponse(_) => {
                    ExchangeType::RequestResponse
                }
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
    use crate::version::v0_0_1::id::{
        Address, Kind, PayloadClaim,
    };

    pub type Primitive = generic::payload::Primitive<Kind>;
    pub type Payload = generic::payload::Payload<Kind>;
    pub type PayloadType = generic::payload::PayloadType;
    pub type PayloadRef = generic::payload::PayloadRef<PayloadClaim, PayloadPattern>;
    pub type PayloadDelivery = generic::payload::PayloadDelivery<Payload, PayloadRef>;
    pub type Call = generic::payload::Call;
    pub type CallWithConfig = generic::payload::CallWithConfig;
    pub type MapPattern = generic::payload::MapPattern<Kind>;
    pub type PayloadTypePattern = generic::payload::PayloadTypePattern<Kind>;
    pub type PayloadPattern = generic::payload::PayloadPattern<Kind>;
    pub type ListPattern = generic::payload::ListPattern;
    pub type PayloadMap = generic::payload::PayloadMap<Kind>;
    pub type RcCommand = generic::payload::RcCommand;

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

    use crate::version::v0_0_1::ArtifactRef;
    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Address,  Kind};

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
        Http
    }

    pub mod request {
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{
            Address,  Kind, PayloadClaim, ResourceType,
        };
        use crate::version::v0_0_1::payload::Payload;

        pub type ReqEntity = generic::entity::request::ReqEntity<Payload>;
        pub type Rc = generic::entity::request::Rc<Payload>;
        pub type Msg = generic::entity::request::Msg<Payload>;
        pub type Http = generic::entity::request::Http<Payload>;
    }

    pub mod response {
        use crate::version::v0_0_1::{fail, generic};
        use crate::version::v0_0_1::id::{Address,  Kind};
        use crate::version::v0_0_1::payload::Payload;

        pub type RespEntity = generic::entity::response::RespEntity<Payload, fail::Fail>;
    }
}

pub mod resource {
    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Address,  Kind, ResourceType};

    #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display, Eq,PartialEq)]
    pub enum Status {
        Unknown,               // initial status or when we status cannot be determined
        Pending,               // resource is now registered but not assigned to a host
        Assigning,             // resource is being assigned to at least one host
        Initializing(String),  // assigned to a host and undergoing custom initialization...This resource can send requests but not receive requests.  The String gives a progress indication like 2/10 (step 2 of 10) or 7/? when the number of steps are not known.
        Ready,                 // ready to take requests
        Paused(String),        // can not receive requests (probably because it is waiting for some other resource to make updates)... String should be some form of meaningful identifier of which resource Paused this resource
        Resuming(String),      // like Initializing but triggered after a pause is lifted, the resource may be doing something before it is ready to accept requests again.  String is a progress indication just like Initializing.
        Panic(String),         // something is wrong... all requests are blocked and responses are cancelled. String is a hopefully  meaningful message describing why the Resource has Panic
        Done(String),          // this resource had a life span and has now completed succesfully it can no longer receive requests. String is a hopefully meaningful or useful Status message that is returned
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
        use crate::version::v0_0_1::id::{Address,  Kind, ResourceType};
        use crate::version::v0_0_1::payload::Payload;

        pub type Request = generic::portal::inlet::Request<Payload>;
        pub type Response = generic::portal::inlet::Response<Payload>;
        pub type Frame = generic::portal::inlet::Frame<Payload>;

        impl TryFrom<PrimitiveFrame> for generic::portal::inlet::Frame<Payload> {
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
        use crate::version::v0_0_1::id::{Address,  Kind, ResourceType};
        use crate::version::v0_0_1::payload::Payload;

        pub type Request = generic::portal::outlet::Request<Payload>;
        pub type Response = generic::portal::outlet::Response<Payload>;
        pub type Frame = generic::portal::outlet::Frame<Kind, Payload>;

        impl TryFrom<PrimitiveFrame>
            for generic::portal::outlet::Frame<Kind, Payload>
        {
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
        use crate::version::v0_0_1::util::Convert;
        use crate::version::v0_0_1::id::{Address, Specific};

        #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
        pub struct GenericKind<ResourceType>{
            pub resource_type: ResourceType,
            pub kind: Option<String>,
            pub specific: Option<Specific>
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
            use crate::version::v0_0_1::parse::{Res, skewer, version_chars};
            use crate::version::v0_0_1::id::Specific;
            use nom::sequence::{tuple, delimited};
            use nom::bytes::complete::{tag, is_a};
            use semver::Version;
            use nom_supreme::{parse_from_str, parser_ext::ParserExt};

            pub fn version(input: &str ) -> Res<&str, Version> {
                parse_from_str(version_chars).parse(input)
            }

            fn specific(input: &str) -> Res<&str, Specific> {
                tuple((
                    domain,
                    tag(":"),
                    skewer,
                    tag(":"),
                    skewer,
                    tag(":"),
                    delimited(tag("("), version, tag(")")),
                ))(input)
                    .map(|(next, (vendor, _, product, _, variant, _, version))| {
                        let specific = Specific{
                            vendor,
                            product,
                            variant,
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
        use crate::version::v0_0_1::ArtifactRef;
        use crate::version::v0_0_1::config::{Config, PortalKind};
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::resource::Archetype;
        use crate::version::v0_0_1::id::Address;

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


        impl<FromKind> Info<FromKind>
        {
            pub fn convert<ToKind>(
                self,
            ) -> Result<Info<ToKind>, Error>
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

            use serde::{Deserialize, Serialize};
            use serde::__private::fmt::Debug;

            use crate::error::Error;
            use crate::version::v0_0_1::{http, State};
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::generic;
            use crate::version::v0_0_1::generic::payload::{HttpMethod, Payload};
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::generic::payload::RcCommand;
            use crate::version::v0_0_1::id::Meta;
            use crate::version::v0_0_1::util::{Convert, ConvertFrom};
            use crate::version::v0_0_1::generic::entity::response::RespEntity;
            use crate::version::v0_0_1::generic::resource::command::RcCommand;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum ReqEntity<PAYLOAD> {
                Rc(Rc<PAYLOAD>),
                Msg(Msg<PAYLOAD>),
                Http(Http<PAYLOAD>),
            }

            impl <PAYLOAD> ReqEntity<PAYLOAD> {
                pub fn ok<FAIL>(&self, payload: PAYLOAD) -> RespEntity<PAYLOAD,FAIL> {
                    RespEntity::Ok(payload)
                }

                pub fn fail<FAIL>(&self, fail: FAIL ) -> RespEntity<PAYLOAD,FAIL>{
                    RespEntity::Fail(fail)
                }
            }

            impl<FromPayload, ToPayload> ConvertFrom<ReqEntity<FromPayload>> for ReqEntity<ToPayload>
            where
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(a: ReqEntity<FromPayload>) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    match a {
                        ReqEntity::Rc(rc) => Ok(ReqEntity::Rc(ConvertFrom::convert_from(rc)?)),
                        ReqEntity::Msg(msg) => Ok(ReqEntity::Msg(ConvertFrom::convert_from(msg)?)),
                        ReqEntity::Http(http) => Ok(ReqEntity::Http(ConvertFrom::convert_from(http)?)),
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Rc<PAYLOAD> {
                pub command: RcCommand,
                pub payload: PAYLOAD,
            }

            impl <PAYLOAD> Rc<PAYLOAD> {
                pub fn new( command: RcCommand, payload: PAYLOAD ) -> Self {
                    Self{
                        command,
                        payload
                    }
                }
            }

            impl <PAYLOAD> ToString for Rc<PAYLOAD> {
                fn to_string(&self) -> String {
                    format!("Rc<{}>",self.command.to_string())
                }
            }

            impl<FromPayload, ToPayload> ConvertFrom<Rc<FromPayload>> for Rc<ToPayload>
            where
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(a: Rc<FromPayload>) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    Ok(Rc {
                        command: a.command,
                        payload: a.payload.try_into()?,
                    })
                }
            }

            impl<FromPayload> Rc<FromPayload> {
                pub fn convert<ToPayload>(self) -> Result<Rc<ToPayload>, Error>
                where
                    ToPayload: ConvertFrom<FromPayload>,
                {
                    Ok(Rc {
                        command: self.command,
                        payload: ConvertFrom::convert_from(self.payload)?,
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Msg<PAYLOAD> {
                pub action: String,
                pub path: String,
                pub payload: PAYLOAD,
            }

            impl <PAYLOAD> ToString for Msg<PAYLOAD> {
                fn to_string(&self) -> String {
                    format!("Msg<{}>{}",self.action,self.path)
                }
            }


            impl<FromPayload, ToPayload> ConvertFrom<Msg<FromPayload>> for Msg<ToPayload>
            where
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(a: Msg<FromPayload>) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    Ok(Msg {
                        action: a.action,
                        path: a.path,
                        payload: a.payload.try_into()?,
                    })
                }
            }

            impl<FromPayload> Msg<FromPayload> {
                pub fn convert<ToPayload>(self) -> Result<Msg<ToPayload>, Error>
                where
                    ToPayload: ConvertFrom<FromPayload>,
                {
                    Ok(Msg {
                        action: self.action,
                        path: self.path,
                        payload: ConvertFrom::convert_from(self.payload)?,
                    })
                }
            }



            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Http<PAYLOAD> {
                pub headers: Meta,
                pub method: HttpMethod,
                pub path: String,
                pub body: PAYLOAD,
            }

            impl <PAYLOAD> ToString for Http<PAYLOAD> {
                fn to_string(&self) -> String {
                    format!("Http<{}>{}",self.method.to_string(),self.path)
                }
            }

            impl<FromPayload, ToPayload> ConvertFrom<Http<FromPayload>> for Http<ToPayload>
                where
                    FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(a: Http<FromPayload>) -> Result<Self, Error>
                    where
                        Self: Sized,
                {
                    Ok(Http{
                        headers: a.headers,
                        method: a.method,
                        path: a.path,
                        body: a.body.try_into()?,
                    })
                }
            }

            impl<FromPayload> Http<FromPayload> {
                pub fn convert<ToPayload>(self) -> Result<Http<ToPayload>, Error>
                    where
                        ToPayload: ConvertFrom<FromPayload>,
                {
                    Ok(Http{
                        headers: self.headers,
                        method: self.method,
                        path: self.path,
                        body: ConvertFrom::convert_from(self.body)?,
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

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum RespEntity<PAYLOAD, FAIL> {
                Ok(PAYLOAD),
                Fail(FAIL),
            }

            impl<FromPayload, ToPayload, FAIL> ConvertFrom<RespEntity<FromPayload, FAIL>>
                for RespEntity<ToPayload, FAIL>
            where
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(
                    a: RespEntity<FromPayload, FAIL>,
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

            impl<FromPayload, FAIL> RespEntity<FromPayload, FAIL> {
                pub fn convert<ToPayload>(self) -> Result<RespEntity<FromPayload, FAIL>, Error>
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
        use crate::version::v0_0_1::generic::id::{AddressAndKind};
        use crate::version::v0_0_1::generic::payload::{MapPattern, Payload, PayloadType};
        use crate::version::v0_0_1::generic::payload::Primitive;
        use crate::version::v0_0_1::State;
        use crate::version::v0_0_1::util::ConvertFrom;
        use crate::version::v0_0_1::id::Address;

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct Archetype<KIND> {
            pub kind: KIND,
            pub config_src: Option<Address>,
        }

        impl<FromKind, ToKind> ConvertFrom<Archetype<FromKind>>
            for Archetype<ToKind>
        where
            FromKind: TryInto<ToKind, Error = Error>,
        {
            fn convert_from(a: Archetype<FromKind>) -> Result<Self, Error>
            where
                Self: Sized,
            {
                Ok(Archetype {
                    kind: a.kind.try_into()?,
                    config_src: match a.config_src {
                        None => None,
                        Some(some) => Some(some.try_into()?),
                    },
                })
            }
        }

        impl<FromKind> Archetype<FromKind>
        {
            pub fn convert<ToKind>(self) -> Result<Archetype<ToKind>, Error>
            where
                ToKind: TryFrom<FromKind, Error = Error>,
            {
                let config_src = match self.config_src {
                    None => None,
                    Some(config_src) => Some(config_src.try_into()?),
                };
                Ok(Archetype {
                    kind: self.kind.try_into()?,
                    config_src,
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct ResourceStub<KIND> {
            pub address: Address,
            pub archetype: Archetype<KIND>,
        }

        impl<FromKind> ResourceStub<FromKind> {
            pub fn convert<ToAddress, ToKind>(
                self,
            ) -> Result<ResourceStub<ToKind>, Error>
            where
                ToKind: TryFrom<FromKind, Error = Error>,
            {
                Ok(ResourceStub {
                    address: self.address.try_into()?,
                    archetype: self.archetype.convert()?,
                })
            }
        }

        impl<FromKind, ToKind>
            ConvertFrom<ResourceStub<FromKind>>
            for ResourceStub<ToKind>
        where
            FromKind: TryInto<ToKind, Error = Error>,
        {
            fn convert_from(a: ResourceStub<FromKind>) -> Result<Self, Error>
            where
                Self: Sized,
            {
                Ok(ResourceStub {
                    address: a.address.try_into()?,
                    archetype: ConvertFrom::convert_from(a.archetype)?,
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct Resource<KIND>
        {
            pub stub: ResourceStub<KIND>,
            pub state: Box<Payload<KIND>>,
        }

        impl<
                FromKind,
                ToKind,
            > ConvertFrom<Resource<FromKind>>
            for Resource<ToKind>
        where
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            ToKind: Clone,
        {
            fn convert_from(
                a: Resource<FromKind>,
            ) -> Result<Self, Error>
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
            use serde::{Serialize,Deserialize};
            use crate::version::v0_0_1::util::ValueMatcher;
            use crate::version::v0_0_1::generic::resource::command::create::Create;
            use crate::version::v0_0_1::generic::resource::command::select::Select;
            use crate::version::v0_0_1::generic::resource::command::update::Update;

            #[derive(Debug,Clone,strum_macros::Display,strum_macros::EnumString,Serialize,Deserialize)]
            pub enum RcCommand<KIND,PAYLOAD,TksPattern>{
                Create(Create<KIND,PAYLOAD,TksPattern>),
                Select(Select<TksPattern>),
                Update(Update<KIND,TksPattern>)
            }

            impl <KIND,PAYLOAD,TksPattern> ValueMatcher<RcCommand<KIND,PAYLOAD,TksPattern>> for RcCommand<KIND,PAYLOAD,TksPattern> {
                fn is_match(&self, x: &RcCommand<KIND,PAYLOAD,TksPattern>) -> Result<(), crate::error::Error> {
                    if *self == *x {
                        Ok(())
                    } else {
                        Err(format!("Rc command expected: '{}' found: '{}'", self.to_string(),x.to_string()).into())
                    }
                }
            }



            pub mod common {
                use serde::{Serialize,Deserialize};
                use crate::version::v0_0_1::generic::payload::PayloadMap;

                #[derive(Debug,Clone,Serialize,Deserialize,strum_macros::Display)]
                pub enum StateSrc<PAYLOAD> {
                    Stateless,
                    StatefulDirect(PAYLOAD)
                }

                pub type SetProperties<KIND> = PayloadMap<KIND>;
            }

            pub mod create {
                use serde::{Serialize,Deserialize};
                use crate::version::v0_0_1::generic::payload::PayloadMap;
                use crate::version::v0_0_1::generic::resource::command::common::{StateSrc, SetProperties};
                use crate::version::v0_0_1::id::Address;

                #[derive(Debug,Clone,Serialize,Deserialize)]
                pub struct Create<KIND,PAYLOAD,TksPattern>{
                    pub address_template: AddressTemplate,
                    pub state: StateSrc<PAYLOAD>,
                    pub properties: SetProperties<KIND>,
                    pub tks_template: TksPattern,
                }

                #[derive(Debug,Clone,Serialize,Deserialize)]
                pub struct AddressTemplate {
                    pub parent: Address,
                    pub child_segment_template: AddressSegmentTemplate
                }

                #[derive(Debug,Clone,strum_macros::Display,Serialize,Deserialize)]
                pub enum AddressSegmentTemplate {
                    // right now only exact is supported
                    Exact(String)
                }
            }

            pub mod select {
                use serde::{Serialize,Deserialize};
                use std::collections::{HashSet, HashMap};
                use crate::version::v0_0_1::generic::payload::MapPattern;
                use crate::version::v0_0_1::pattern::AddressTksPattern;

                #[derive(Debug,Clone,Serialize,Deserialize)]
                pub struct Select<TksPattern> {
                    pub address_pattern: AddressTksPattern,
                    pub properties: PropertiesPattern<TksPattern>
                }

                pub type PropertiesPattern<TksPattern> = MapPattern<TksPattern>;

            }

            pub mod update {
                use serde::{Serialize,Deserialize};
                use crate::version::v0_0_1::generic::payload::PayloadMap;
                use crate::version::v0_0_1::generic::resource::command::common::SetProperties;
                use crate::version::v0_0_1::id::Address;

                #[derive(Debug,Clone,Serialize,Deserialize)]
                pub struct Update<KIND,TksPattern> {
                    pub address: Address,
                    pub properties: SetProperties<KIND>
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
            use crate::version::v0_0_1::util::{ConvertFrom, unique_id};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<PAYLOAD> {
                pub id: String,
                pub to: Vec<Address>,
                pub entity: request::ReqEntity<PAYLOAD>,
                pub exchange: Exchange
            }

            impl<PAYLOAD> Request<PAYLOAD> {
                pub fn new(entity: request::ReqEntity<PAYLOAD>) -> Self {
                    Self {
                        id: unique_id(),
                        to: vec![],
                        entity,
                        exchange: Exchange::Notification
                    }
                }
            }

            impl<PAYLOAD> Request<PAYLOAD> {
                pub fn exchange(
                    self,
                    exchange: Exchange,
                ) -> Request<PAYLOAD> {
                    Request {
                        id: self.id,
                        to: self.to,
                        entity: self.entity,
                        exchange,
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Response<PAYLOAD> {
                pub id: String,
                pub to: Address,
                pub exchange: ExchangeId,
                pub entity: response::RespEntity<PAYLOAD, fail::portal::Fail>,
            }

            impl<FromPayload, ToPayload>
                ConvertFrom<Response<FromPayload>>
                for Response<ToPayload>
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
            pub enum Frame<PAYLOAD> {
                Log(Log),
                Command(Command),
                Request(Request<PAYLOAD>),
                Response(Response<PAYLOAD>),
                Status(Status),
                Close(CloseReason),
            }

            impl<PAYLOAD: Serialize> TryInto<PrimitiveFrame>
                for Frame<PAYLOAD>
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
            use crate::version::v0_0_1::generic;
            use crate::version::v0_0_1::command::CommandEvent;
            use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
            use crate::version::v0_0_1::generic::config::Info;
            use crate::version::v0_0_1::generic::entity::request;
            use crate::version::v0_0_1::generic::entity::response;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::generic::portal::inlet;
            use crate::version::v0_0_1::id::{Address, Kind, ResourceType};
            use crate::version::v0_0_1::messaging::{Exchange, ExchangeId};
            use crate::version::v0_0_1::util::{ConvertFrom, unique_id};
            use crate::version::v0_0_1::fail;
            use crate::version::v0_0_1::generic::entity::response::RespEntity;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<PAYLOAD> {
                pub from: Address,
                pub entity: request::ReqEntity<PAYLOAD>,
                pub exchange: Exchange
            }

            impl<PAYLOAD> Request<PAYLOAD> {
                pub fn exchange(
                    self,
                    exchange: Exchange,
                ) -> Request<PAYLOAD> {
                    Self {
                        from: self.from,
                        entity: self.entity,
                        exchange,
                    }
                }

                pub fn ensure_notification(&self) -> Result<(),Error> {
                    match self.exchange {
                        Exchange::Notification => {
                            Ok(())
                        }
                        Exchange::RequestResponse(_) => {
                            Err("expected Notification Exchange but found RequestResponse Exchange".into())
                        }
                    }
                }

                pub fn ok( self, payload: PAYLOAD ) -> Result<inlet::Response<PAYLOAD>,Error> {
                    Ok(inlet::Response {
                        id: unique_id(),
                        to: self.from,
                        exchange: self.exchange.try_into()?,
                        entity: RespEntity::Ok(payload)
                    })
                }

                pub fn fail( self, fail: fail::portal::Fail ) -> Result<inlet::Response<PAYLOAD>,Error> {
                    Ok(inlet::Response {
                        id: unique_id(),
                        to: self.from,
                        exchange: self.exchange.try_into()?,
                        entity: RespEntity::Fail(fail)
                    })
                }
            }



            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Response<PAYLOAD> {
                pub id: String,
                pub to: Address,
                pub from: Address,
                pub entity: response::RespEntity<PAYLOAD, fail::Fail>,
                pub exchange: ExchangeId
            }

            impl <PAYLOAD> Response<PAYLOAD> {
                pub fn new(to: Address, from: Address, entity: response::RespEntity<PAYLOAD, fail::Fail>, exchange: ExchangeId ) -> Self {
                    Self {
                        id: unique_id(),
                        to,
                        from,
                        entity,
                        exchange
                    }
                }
            }

            impl<FromPayload> Response<FromPayload> {
                pub fn convert<ToAddress, ToPayload>(
                    self,
                ) -> Result<Response<ToPayload>, Error>
                where
                    ToPayload: TryFrom<FromPayload, Error = Error>,
                {
                    Ok(Response {
                        id: self.id,
                        to: self.to.try_into()?,
                        from: self.from.try_into()?,
                        entity: ConvertFrom::convert_from(self.entity)?,
                        exchange: self.exchange
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
            pub enum Frame<KIND, PAYLOAD> {
                Create(Info<KIND>),
                CommandEvent(CommandEvent),
                Request(Request<PAYLOAD>),
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

            impl<
                    KIND: Serialize,
                    PAYLOAD: Serialize,
                > TryInto<PrimitiveFrame> for Frame<KIND, PAYLOAD>
            {
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
        use crate::version::v0_0_1::{http, State};
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::resource::{Resource, ResourceStub};
        use crate::version::v0_0_1::payload::PrimitiveType;
        use crate::version::v0_0_1::resource::Status;
        use crate::version::v0_0_1::util::{Convert, ConvertFrom, ValueMatcher, ValuePattern};
        use crate::version::v0_0_1::id::{Meta, Address};

        #[derive(
            Debug, Clone, Serialize, Deserialize, Eq, PartialEq,strum_macros::Display,strum_macros::EnumString
        )]
        pub enum PayloadType
        {
            Empty,
            Primitive,
            List,
            Map,
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct ListPattern {
            pub primitive: PrimitiveType,
            pub range: Range,
        }

        impl ListPattern {
            pub fn is_match<KIND>( &self, list: &PrimitiveList<KIND> ) -> Result<(),Error> {
                for i in &list.list {
                    if self.primitive != i.primitive_type()  {
                        return Err(format!("Primitive List expected: {} found: {}",self.primitive.to_string(),i.primitive_type().to_string()).into());
                    }
                }

                Ok(())
            }
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub enum Range {
            MinMax { min: usize, max: usize },
            Exact(usize),
            Any
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub enum PayloadTypePattern<KIND> {
            Empty,
            Primitive(PrimitiveType),
            List(ListPattern),
            Map(Box<MapPattern<KIND>>)
        }

        impl <KIND> PayloadTypePattern<KIND> {
            pub fn is_match( &self, payload: &Payload<KIND> ) -> Result<(),Error> {
                match self {
                    PayloadTypePattern::Empty => {
                        if payload.payload_type() == PayloadType::Empty {
                            Ok(())
                        } else {

                            Err(format!("Payload expected: Empty found: {}",payload.payload_type().to_string()).into())
                        }
                    }
                    PayloadTypePattern::Primitive(expected ) => {
                        if let Payload::Primitive(found) = payload {
                            if *expected == found.primitive_type() {
                                Ok(())
                            } else {
                                Err(format!("Payload Primitive expected: {} found: {}", expected.to_string(), found.primitive_type().to_string()).into())
                            }
                        } else {
                            Err(format!("Payload expected: {} found: {}", expected.to_string(), payload.payload_type().to_string()).into())
                        }
                    }
                    PayloadTypePattern::List(expected) => {
                        if let Payload::List(found) = payload {
                            expected.is_match(found )
                        } else {
                            Err(format!("Payload expected: List found: {}", payload.payload_type().to_string()).into())
                        }
                    }
                    PayloadTypePattern::Map(expected) => {
                        if let Payload::Map(found) = payload {
                            expected.is_match(found)
                        } else {
                            Err(format!("Payload expected: {} found: {}",expected.to_string(), payload.payload_type().to_string()).into())
                        }
                    }
                }

            }
        }
        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct PayloadPattern<KIND> {
            pub structure: PayloadTypePattern<KIND>,
            pub format: Option<PayloadFormat>,
            pub validator: Option<CallWithConfig>,
        }


        impl<
            FromKind,
            ToKind,
        > ConvertFrom<Option<PayloadPattern<FromKind>>>
        for Option<PayloadPattern<ToKind>>
            where
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKind: Clone,
                PayloadTypePattern<ToKind>: ConvertFrom<PayloadTypePattern<FromKind>>,
                PayloadPattern<ToKind>: ConvertFrom<PayloadPattern<FromKind>>

        {
            fn convert_from(
                a: Option<PayloadPattern<FromKind>>,
            ) -> Result<Self, Error>
                where
                    Self: Sized,
            {
                let rtn = match a{
                    None => None,
                    Some(rtn) => {
                        Option::Some(ConvertFrom::convert_from(rtn)?)
                    }
                };
                Ok(rtn)
            }
        }

        impl<
            FromKind,
            ToKind,
        > ConvertFrom<PayloadPattern<FromKind>>
        for PayloadPattern<ToKind>
            where
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKind: Clone,
        {
            fn convert_from(
                a: PayloadPattern<FromKind>,
            ) -> Result<Self, Error>
                where
                    Self: Sized,
            {
                Ok(Self {
                    structure: ConvertFrom::convert_from(a.structure)?,
                    format: a.format,
                    validator: ConvertFrom::convert_from(a.validator)?
                })
            }
        }

        impl<
            FromKind,
            ToKind,
        > ConvertFrom<PayloadTypePattern<FromKind>>
        for PayloadTypePattern<ToKind>
            where
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKind: Clone,

        {
            fn convert_from(
                a: PayloadTypePattern<FromKind>,
            ) -> Result<Self, Error>
                where
                    Self: Sized,
            {
                let rtn = match a {
                    PayloadTypePattern::Empty => {
                        PayloadTypePattern::Empty
                    }
                    PayloadTypePattern::Primitive(primitive) => {
                        PayloadTypePattern::Primitive(primitive)
                    }
                    PayloadTypePattern::List(list) => {
                        PayloadTypePattern::List(list)
                    }
                    PayloadTypePattern::Map(map) => {
                        PayloadTypePattern::Map(Box::new(ConvertFrom::convert_from(*map)?))
                    }
                };

                Ok(rtn)
            }
        }


        impl<KIND> ValueMatcher<Payload<KIND>> for  PayloadPattern<KIND> {
            fn is_match(&self, payload: &Payload<KIND>) -> Result<(), Error> {
                self.structure.is_match(&payload)?;

                // more validation to come...
                Ok(())
            }
        }
        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct CallWithConfig {
            pub call: Call,
            pub config: Option<Address>
        }


        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct Call {
            pub address: Address,
            pub kind: CallKind
        }

        impl ToString for Call {
            fn to_string(&self) -> String {
                format!("{}^{}",self.address.to_string(), self.kind.to_string())
            }
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub enum CallKind{
            Rc(RcCommand),
            Msg(MsgCall),
            Http(HttpCall)
        }


        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct MsgCall {
            pub path: String,
            pub action: String
        }

        impl MsgCall {
            pub fn new( action: String, path: String ) -> Self {
                Self{
                    action,
                    path
                }
            }
        }

        impl ToString for MsgCall {
            fn to_string(&self) -> String {
                format!("Msg<{}>{}",self.action,self.path)
            }
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct HttpCall {
            pub path: String,
            pub method: HttpMethod
        }

        impl HttpCall {
            pub fn new( method: HttpMethod, path: String) -> Self {
                Self{
                    method,
                    path
                }
            }
        }


        impl ToString for HttpCall {
            fn to_string(&self) -> String {
                format!("Http<{}>{}",self.method.to_string(),self.path)
            }
        }


        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize,strum_macros::Display,strum_macros::EnumString)]
        pub enum HttpMethod {
            Get,
            Post,
            Put,
            Delete,
            Patch,
            Head,
            Connect,
            Options,
            Trace
        }

        impl ValueMatcher<HttpMethod> for HttpMethod {
            fn is_match(&self, found: &HttpMethod) -> Result<(), crate::error::Error> {
                if *self == *found {
                    Ok(())
                } else {
                    Err(format!("Http Method mismatch. expected: '{}', found: '{}'", self.to_string(), found.to_string()).into())
                }
            }
        }


        impl ToString for CallKind {
            fn to_string(&self) -> String {
                match self {
                    CallKind::Rc(command) => {
                        format!("Rc<{}>",command.to_string())
                    }
                    CallKind::Msg(msg) => {
                        msg.to_string()
                    }
                    CallKind::Http(http) => {
                        http.to_string()
                    }

                }
            }
        }



        #[derive(Debug,Clone,Eq,PartialEq,strum_macros::Display,strum_macros::EnumString,Serialize,Deserialize)]
        pub enum PayloadFormat {
            #[strum(serialize = "json")]
            Json,
            #[strum(serialize = "image")]
            Image,
        }


        impl PrimitiveType {
            pub fn is_match<KIND>( &self, primitive: &generic::payload::Primitive<KIND>) -> Result<(),Error>
                where KIND: Clone
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
                        if *self == Self::Address{
                            Ok(())
                        } else {
                            Err("expected Address primitive".into())
                        }
                    }
                    Primitive::Stub(_) => {
                        if *self == Self::Stub{
                            Ok(())
                        } else {
                            Err("expected Stub primitive".into())
                        }
                    }
                    Primitive::Meta(_) => {
                        if *self == Self::Meta{
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
                        if *self == Self::Boolean{
                            Ok(())
                        } else {
                            Err("expected Boolean primitive".into())
                        }
                    }
                    Primitive::Int(_) => {
                        if *self == Self::Int{
                            Ok(())
                        } else {
                            Err("expected Int primitive".into())
                        }
                    }
                    Primitive::Status(_) => {
                        if *self == Self::Status{
                            Ok(())
                        } else {
                            Err("expected Status primitive".into())
                        }
                    }
                    Primitive::Resource(_) => {
                        if *self == Self::Resource{
                            Ok(())
                        } else {
                            Err("expected Resource primitive".into())
                        }
                    }
                }
            }
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize,)]
        pub struct MapPattern<KIND> {
            kind_phantom: PhantomData<KIND>,
            pub required: HashMap<String, ValuePattern<PayloadPattern<KIND>>>,
            pub allowed: ValuePattern<PayloadPattern<KIND>>
        }

        impl <KIND> Default for MapPattern<KIND> {
            fn default() -> Self {
                MapPattern {
                    kind_phantom: Default::default(),
                    required: Default::default(),
                    allowed: ValuePattern::Any
                }
            }
        }

        impl <KIND> ToString for MapPattern<KIND> {
            fn to_string(&self) -> String {
                "Map?".to_string()
            }
        }

        impl <KIND> MapPattern<KIND> {
            pub fn new(required: HashMap<String, ValuePattern<PayloadPattern<KIND>>>, allowed: ValuePattern<PayloadPattern<KIND>>) -> Self {
                MapPattern {
                    kind_phantom: Default::default(),
                    required,
                    allowed
                }
            }
        }



        impl <KIND> MapPattern<KIND> {
            pub fn empty() -> Self {
                Self {
                    kind_phantom: Default::default(),
                    required: HashMap::new(),
                    allowed: ValuePattern::None
                }
            }

            pub fn any() -> Self {
                Self {
                    kind_phantom: Default::default(),
                    required: HashMap::new(),
                    allowed: ValuePattern::Any
                }
            }


            pub fn is_match( &self, map:&PayloadMap<KIND> ) -> Result<(),Error>
            {

                // if Any keys are allowed then skip
                    for (key, payload) in &map.map {
                        if !self.required.contains_key(key)
                        {
                            match &self.allowed {
                                ValuePattern::Any => { }
                                ValuePattern::None => {
                                    return Err(format!("key: '{}' not required or allowed by Map constraints", key).into());
                                }
                                ValuePattern::Pattern(pattern) => {
                                    pattern.is_match(payload)?;
                                }
                            }
                        }
                    }

                // now make sure all required are present and meet constraints
                for (key,constraint) in &self.required
                {
                    if !map.contains_key(key) {
                        return Err(format!("missing required key : '{}' defined in Map constraints", key).into())
                    }
                    constraint.is_match(&map.get(key).expect("expected map element after testing for it"))?;
                }

                Ok(())
            }
        }
        impl<
            FromKind,
            ToKind,
        > ConvertFrom<MapPattern< FromKind>>
        for MapPattern<ToKind>
            where
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKind: Clone,
        {
            fn convert_from(
                a: MapPattern<FromKind>,
            ) -> Result<Self, Error>
                where
                    Self: Sized,
            {

                    let mut required = HashMap::new();
                    for (k,v) in a.required {
                        required.insert( k, ConvertFrom::convert_from(v)?);
                    }

                    Ok(MapPattern::new(required, ConvertFrom::convert_from(a.allowed)?))
            }
        }


        impl<
            FromKind,
            ToKind,
        > ConvertFrom<ValuePattern<PayloadPattern<FromKind>>>
        for ValuePattern<PayloadPattern<ToKind>>
            where
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKind: Clone,
        {
            fn convert_from(
                a: ValuePattern<PayloadPattern<FromKind>>
            ) -> Result<Self, Error>
                where Self: Sized,
            {


                    Ok(match a {
                        ValuePattern::Any => {
                            ValuePattern::Any
                        }
                        ValuePattern::None => {
                            ValuePattern::None
                        }
                        ValuePattern::Pattern(pattern) => {
                            ValuePattern::Pattern(ConvertFrom::convert_from(pattern)?)
                        }
                    })

            }
        }


        impl<
            FromKind,
            ToKind,
        > ConvertFrom<HashMap<String, ValuePattern<PayloadPattern<FromKind>>>>
        for HashMap<String, ValuePattern<PayloadPattern<ToKind>>>
            where
                FromAddress: TryInto<ToAddress, Error = Error> + Clone,
                FromKind: TryInto<ToKind, Error = Error> + Clone,
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

        #[derive(Debug,Clone,Serialize,Deserialize)]
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

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display )]
        pub enum Payload<KIND>
        {
            Empty,
            Primitive(Primitive<KIND>),
            List(PrimitiveList<KIND>),
            Map(PayloadMap<KIND>),
        }

        impl<ADDRESS, KIND> Payload<ADDRESS, KIND>
        {
            pub fn payload_type(&self) -> PayloadType {
                match self {
                    Payload::Empty => {
                        PayloadType::Empty
                    }
                    Payload::Primitive(primitive) => {
                       PayloadType::Primitive
                    }
                    Payload::List(list) => {
                        PayloadType::List
                    }
                    Payload::Map(map) => {
                       PayloadType::Map
                    }
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct PayloadMap<KIND>
        {
            pub map: HashMap<String,Payload<KIND>>
        }

        impl <KIND> PayloadMap<KIND>
        {
            /*
            pub fn new(constraints: MapConstraints<KEY,ADDRESS,IDENTIFIER,KIND> ) -> Self {
                Self{
            //        constraints,
                    map: HashMap::new()
                }
            }

             */

            pub fn new() -> Self {
                Self{
                    map: HashMap::new()
                }
            }
        }

        impl <KIND> Deref for PayloadMap<KIND>
        {
            type Target = HashMap<String,Payload<KIND>>;

            fn deref(&self) -> &Self::Target {
                &self.map
            }
        }

        impl <KIND> DerefMut for PayloadMap<KIND>
        {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.map
            }
        }


        impl<
                FromKind,
                ToKind,
            > ConvertFrom<Payload<FromKind>>
            for Payload< ToKind>
        where
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            ToKind: Clone,
        {
            fn convert_from(
                a: Payload< FromKind>,
            ) -> Result<Self, Error>
            where
                Self: Sized,
            {
                match a {
                    Payload::Empty => Ok(Payload::Empty),
                    Payload::Primitive(primitive) => Ok(Payload::Primitive(ConvertFrom::convert_from(primitive)?)),
                    Payload::List(list) => {
                        let mut rtn: PrimitiveList<ToKind> = PrimitiveList::new(list.primitive_type);
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

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub enum Primitive<KIND>
        {
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

        impl <KIND> Primitive<KIND>
        {

           pub fn primitive_type(&self) -> PrimitiveType  {
               match self {
                   Primitive::Text(_) => {
                       PrimitiveType::Text
                   }
                   Primitive::Address(_) => {
                       PrimitiveType::Address
                   }
                   Primitive::Stub(_) => {
                       PrimitiveType::Stub
                   }
                   Primitive::Meta(_) => {
                       PrimitiveType::Meta
                   }
                   Primitive::Bin(_) => {
                       PrimitiveType::Bin
                   }
                   Primitive::Boolean(_) => {
                       PrimitiveType::Boolean
                   }
                   Primitive::Int(_) => {
                       PrimitiveType::Int
                   }
                   Primitive::Status(_) => {
                       PrimitiveType::Status
                   }
                   Primitive::Resource(_) => {
                       PrimitiveType::Resource
                   }
               }
           }

        }

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct PrimitiveList<KIND>
        {
           pub primitive_type: PrimitiveType,
           pub list: Vec<Primitive<KIND>>
        }

        impl <KIND>ToString for PrimitiveList<KIND>
            where  KIND: Clone
        {
            fn to_string(&self) -> String {
                format!("{}[]",self.primitive_type.to_string())
            }
        }

        impl <KIND> PrimitiveList<KIND>
            where KIND: Clone
        {
            pub fn new(primitive_type: PrimitiveType) -> Self {
                Self {
                    primitive_type,
                    list: vec![]
                }
            }
            pub fn validate( &self ) -> Result<(),Error> {
                for primitive in &self.list {
                    if primitive.primitive_type() != self.primitive_type {
                        return Err(format!("PrimitiveList type mismatch expected: {} received: {}", self.primitive_type.to_string(), primitive.primitive_type().to_string() ).into());
                    }
                }
                Ok(())
            }
        }

        impl <KIND> Deref for PrimitiveList<KIND>
            where KIND: Clone
        {
            type Target = Vec<Primitive<KIND>>;

            fn deref(&self) -> &Self::Target {
                &self.list
            }
        }

        impl <KIND> DerefMut for PrimitiveList<KIND>
            where KIND: Clone
        {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.list
            }
        }


        impl< FromKind,
                ToKind,
            > ConvertFrom<Primitive<FromKind>>
            for Primitive< ToKind>
        where
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            ToKind: Clone,
        {
            fn convert_from(
                a: Primitive<FromKind>,
            ) -> Result<Self, Error>
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
}

pub mod util {
    use serde::{Deserialize, Serialize};
    use uuid::Uuid;

    use crate::error::Error;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum ValuePattern<T> {
        Any,
        None,
        Pattern(T)
    }

    impl <T> ValuePattern<T> {
        pub fn is_match<X>( &self, x: &X ) -> Result<(),Error>
            where T: ValueMatcher<X>
        {
            match self {
                ValuePattern::Any => {
                    Ok(())
                }
                ValuePattern::Pattern(exact) => {
                    exact.is_match(x)
                }
                ValuePattern::None => {
                    Err("None pattern".into())
                }
            }
        }
    }

    impl <V:ToString> ToString for ValuePattern<V> {
        fn to_string(&self) -> String {
            match self {
                ValuePattern::Any => "*".to_string(),
                ValuePattern::None => "!".to_string(),
                ValuePattern::Pattern(pattern) => {
                    pattern.to_string()
                }
            }
        }
    }


    pub trait ValueMatcher<X> {
        fn is_match(&self, x: &X) -> Result<(),Error>;
    }

    pub struct RegexMatcher {
        pub pattern: String
    }

    impl ToString for RegexMatcher {
        fn to_string(&self) -> String {
            self.pattern.clone()
        }
    }

    impl RegexMatcher {
        pub fn new(string: String) -> Self {
            Self {
                pattern: string
            }
        }
    }

    impl ValueMatcher<String> for RegexMatcher {
        fn is_match(&self, x: &String) -> Result<(), Error> {
            let matches = x.matches(x);
            if matches.count() > 0 {
                Ok(())
            } else {
                Err(format!("could not match pattern '{}' in '{}'",self.pattern, x).into())
            }
        }
    }



    #[derive(Debug,Eq,PartialEq)]
    pub struct StringMatcher {
        pub pattern: String
    }

    impl ToString for StringMatcher {
        fn to_string(&self) -> String {
            self.pattern.clone()
        }
    }

    impl StringMatcher {
        pub fn new(string: String) -> Self {
            Self {
                pattern: string
            }
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

    pub trait ConvertFrom<A> where Self:Sized {
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
            Error(String),
            QueueOverflow,
        }
    }

    pub mod portal {
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::fail::{http, port, resource};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Error(String),
            Resource(resource::Fail),
            Port(port::Fail),
            Http(http::Error),
        }
    }

    pub mod resource {
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::fail::{Bad, BadRequest, Conditional, Messaging, NotFound};
        use crate::version::v0_0_1::id::Address;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Create(Create),
            Update(Update),
            BadRequest(BadRequest),
            Conditional(Conditional),
            Messaging(Messaging),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Create {
            AddressAlreadyInUse(String),
            WrongParentResourceType{ expected: String, found: String },
            CannotUpdateArchetype
}

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Update {
            Immutable,
        }
    }

    pub mod port {
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
    use nom::{AsChar, InputTakeAtPosition, IResult};
    use nom::combinator::{all_consuming, recognize, opt, not};
    use nom::error::{ErrorKind, VerboseError, context};
    use nom::multi::{separated_list1, many0};

    use crate::version::v0_0_1::id::{Address, HubSegment, AddressSegment};
    use nom::bytes::complete::{is_not, is_a};
    use nom::sequence::{tuple, terminated, preceded, delimited};
    use nom::bytes::complete::tag;
    use nom::character::complete::{alpha0, digit1};
    use crate::version::latest::util::StringMatcher;
    use nom::branch::alt;

    pub struct Parser {
    }

    impl Parser {
        pub fn address( input: &str ) -> Res<&str,Address> {
            address(input)
        }

        pub fn consume_address( input: &str ) -> Res<&str,Address> {
            consume_address(input)
        }
    }

    pub type Res<I,O>=IResult<I,O, VerboseError<I>>;

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

    pub fn local_hub_segment(input: &str) -> Res<&str, HubSegment> {
        not(alt((domain_hub_segment,tag_hub_segment)))(input).map( |(next,_)| {
            (next,HubSegment::Local)
        })
    }

    pub fn domain_hub_segment(input: &str) -> Res<&str, HubSegment> {
        terminated( domain_chars, tag("::") )(input).map( |(next,domain)| {
            (next, HubSegment::Domain(domain.to_string()))
        } )
    }

    pub fn tag_hub_segment(input: &str) -> Res<&str, HubSegment> {
        terminated( delimited(tag("["),skewer,tag("]")), tag("::") )(input).map( |(next,tag)| {
            (next, HubSegment::Tag(tag.to_string()))
        } )
    }

    pub fn hub_segment(input: &str) -> Res<&str, HubSegment> {
        alt((tag_hub_segment, domain_hub_segment,local_hub_segment) )(input)
    }

    pub fn space_address_segment(input: &str) -> Res<&str, AddressSegment> {
        space_chars(input).map( |(next,space)| {
            (next, AddressSegment::Space(space.to_string()))
        } )
    }

    pub fn base_address_segment(input: &str) -> Res<&str, AddressSegment> {
        skewer(input).map( |(next,base)| {
            (next, AddressSegment::Base(base.to_string()))
        } )
    }

    pub fn dir_address_segment(input: &str) -> Res<&str, AddressSegment> {
        tag("/")(input).map( |(next,_)| {
            (next, AddressSegment::Dir )
        } )
    }

    pub fn file_address_segment(input: &str) -> Res<&str, AddressSegment> {
        filepath_chars(input).map( |(next,filename)| {
            (next, AddressSegment::File(filename.to_string()))
        } )
    }

    pub fn address(input: &str) -> Res<&str, Address> {
        tuple((tuple((hub_segment, space_address_segment)), many0(base_address_segment),many0(file_address_segment)))(input).
            map(|(next,((hub,space)),mut bases,mut files)|{
                let mut segments = vec![];
                segments.push( space );
                segments.append(&mut bases);
                segments.append(&mut files );

                let address = Address {
                    hub,
                    segments
                };

                (next, address)
            })
    }

    pub fn consume_address(input: &str) -> Res<&str,Address> {
        all_consuming(address)(input)
    }


    pub fn asterisk<T, E: nom::error::ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
    {
        input.split_at_position_complete(|item| item.as_char() != '*' )
    }


    pub fn upper<T, E: nom::error::ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
    {
        input.split_at_position_complete(|item| { let char_item = item.as_char();

            !char_item.is_uppercase()

        } )
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

    fn in_double_quotes<T>(i: T) -> Res<T, T>
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


    fn domain<T>(i: T) -> Res<T, T>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-') &&
                    !(char_item == '.')
                    && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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
                    && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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
                !(char_item == '-') &&
                !(char_item == '>') &&
                !(char_item == '<') &&
                !(char_item == '^') &&
                !(char_item == '=') &&
                !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
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
                    && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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
                !(char_item == '-') &&
                    !(char_item == '.') &&
                    !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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
                !(char_item == '-') &&
                !(char_item == '.') &&
                !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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

    pub fn not_space(input: &str)->Res<&str,&str> {
        is_not(" \n\r\t")(input)
    }



    pub fn path(input: &str ) -> Res<&str,&str> {
        recognize(tuple((tag("/"), opt(filepath_chars))) )(input)
    }

    pub fn consume_path(input: &str ) -> Res<&str,&str> {
        all_consuming(path)(input)
    }

    pub fn path_regex(input: &str ) -> Res<&str,&str> {
        recognize(tuple((tag("/"), opt(not_space))) )(input)
    }
    pub fn camel_case( input: &str ) -> Res<&str,&str> {
        recognize(tuple((is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"), alpha0)) )(input)
        //recognize(alpha1)(input)
    }

    pub fn camel_case_to_string( input: &str ) -> Res<&str,StringMatcher> {
        camel_case(input).map(|(next,camel)| { (next,StringMatcher::new(camel.to_string()))}  )
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

    pub fn parse_version(input: &str) -> Res<&str, ((&str,&str,&str), Option<&str>)> {
        tuple((parse_version_major_minor_patch, opt(preceded(tag("-"), skewer))))(input)
    }


    pub fn rec_version(input: &str) -> Res<&str, &str> {
        recognize(parse_version)(input)
    }



}
