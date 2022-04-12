pub mod selector {
    use std::convert::TryInto;
    use std::fmt::Formatter;
    use std::ops::Deref;
    use std::str::FromStr;

    use semver::ReqParseError;
    use serde::de::Visitor;
    use serde::{de, Deserializer, Serializer};

    use crate::error::MsgErr;

    use crate::version::v0_0_1::config::config::bind::parse::{many_until0, select_block, SelectBlock};
    use crate::version::v0_0_1::entity::entity::request::{Action, Rc, RcCommandType, RequestCore};
    use crate::version::v0_0_1::id::id::{
        GenericKind, GenericKindBase, Point, PointSeg, RouteSeg, Specific, Tks, Version,
    };
    use crate::version::v0_0_1::parse::{
        camel_case, camel_case_to_string_matcher, capture_path, capture_point, consume_hierarchy,
        file_chars, path, path_regex, point, point_subst, Res,
    };
    use crate::version::v0_0_1::payload::payload::{
        Call, CallKind, CallWithConfig, HttpCall, HttpMethod, HttpMethodType, ListPattern,
        MapPattern, MsgCall, Payload, PayloadFormat, PayloadPattern, PayloadType,
        PayloadTypePattern, Primitive, PrimitiveType, Range,
    };
    use crate::version::v0_0_1::selector::selector::parse::{pattern, point_selector, value_pattern};
    use crate::version::v0_0_1::selector::selector::specific::{
        ProductSelector, VariantSelector, VendorSelector,
    };
    use crate::version::v0_0_1::util::{MethodPattern, StringMatcher, ValueMatcher, ValuePattern};
    use crate::version::v0_0_1::{span, Span};
    use crate::{Deserialize, Serialize};
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0, one_of};
    use nom::combinator::{all_consuming, fail, opt, recognize};
    use nom::error::{context, ErrorKind, FromExternalError, ParseError, VerboseError};
    use nom::multi::separated_list0;
    use nom::sequence::{delimited, preceded, terminated, tuple};
    use nom::{AsChar, Compare, InputLength, InputTake, InputTakeAtPosition, IResult, Parser};
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
            let (_, rtn) = all_consuming(point_selector)(span(s))?;
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
        use crate::version::v0_0_1::selector::selector::Pattern;

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
        use crate::version::v0_0_1::id::id::{
            GenericKind, GenericKindBase, PointKind, PointSeg, Specific, Version,
        };
        use crate::version::v0_0_1::parse::{
            camel_case, domain_chars, file_chars, point, point_segment_chars, point_subst,
            Res, skewer_chars, version_chars, version_point_segment, version_req_chars,
        };
        use crate::version::v0_0_1::selector::selector::specific::{
            ProductSelector, VariantSelector, VendorSelector,
        };
        use crate::version::v0_0_1::selector::selector::{
            ExactPointSeg, GenericKindiBaseSelector, GenericKindSelector, Hop, Pattern,
            PointSegSelector, PointSelector, SpecificSelector, TksPattern, VersionReq,
        };
        use crate::version::v0_0_1::util::ValuePattern;
        use crate::version::v0_0_1::{span, Span};
        use nom_supreme::error::ErrorTree;
        use nom_supreme::{parse_from_str, ParserExt};

        fn inclusive_any_segment(input: Span) -> Res<Span, PointSegSelector> {
            alt((tag("+*"),tag("ROOT+*")))(input).map(|(next, _)| (next, PointSegSelector::InclusiveAny))
        }

        fn inclusive_recursive_segment(input: Span) -> Res<Span, PointSegSelector> {
            alt((tag("+**"),tag("ROOT+**")))(input).map(|(next, _)| (next, PointSegSelector::InclusiveRecursive))
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
                let x: Res<Span, Span> = tag("*")(input.clone());
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
                Ok((next, version_req)) => Ok((span(next), version_req)),
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
                    Ok((span(next), version_req))
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
                Ok((next, version)) => Ok((span(next), Version { version })),
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
                None => span("/"),
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
        multispace0(input.clone()).map(|(next, _)| (input, PatternBlock::None))
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

            let mut post_fileroot = false;
            for (index, segment) in self.segments.iter().enumerate() {
                if let PointSeg::FilesystemRootDir = segment.segment {
                    post_fileroot = true;
                }
                rtn.push_str(segment.segment.preceding_delim(post_fileroot));
                rtn.push_str(segment.to_string().as_str());
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
            Ok(consume_hierarchy(span(s))?)
        }
    }
}
