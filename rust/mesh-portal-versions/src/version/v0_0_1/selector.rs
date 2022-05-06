use crate::version::v0_0_1::payload::payload::{Payload, PayloadPattern, PayloadPatternDef};
use crate::version::v0_0_1::util::ValuePattern;
use serde::{Deserialize, Serialize};
use crate::error::MsgErr;
use crate::version::v0_0_1::id::id::{Point, PointCtx};
use crate::version::v0_0_1::parse::{CtxResolver, CtxSubst};

pub mod selector {
    use std::convert::TryInto;
    use std::fmt::Formatter;
    use std::ops::Deref;
    use std::str::FromStr;

    use semver::ReqParseError;
    use serde::de::Visitor;
    use serde::{de, Deserializer, Serializer};

    use crate::error::MsgErr;

    use crate::version::v0_0_1::entity::entity::request::{Action, Rc, RcCommandType, RequestCore};
    use crate::version::v0_0_1::id::id::{GenericKind, GenericKindBase, Point, PointCtx, PointSeg, RouteSeg, Specific, Tks, Version};
    use crate::version::v0_0_1::parse::{camel_case, camel_case_to_string_matcher, consume_hierarchy, file_chars, path, path_regex, point, point_segment_selector, point_selector, Res};
    use crate::version::v0_0_1::payload::payload::{Call, CallKind, CallWithConfig, CallWithConfigDef, HttpCall, HttpMethod, HttpMethodType, ListPattern, MapPattern, MsgCall, NumRange, Payload, PayloadFormat, PayloadPattern, PayloadPatternDef, PayloadType, PayloadTypePatternDef, Primitive, PrimitiveType};
    use crate::version::v0_0_1::selector::selector::specific::{
        ProductSelector, VariantSelector, VendorSelector,
    };
    use crate::version::v0_0_1::util::{MethodPattern, StringMatcher, ValueMatcher, ValuePattern};
    use crate::version::v0_0_1::parse;
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
    use crate::version::v0_0_1::parse::error::result;
    use crate::version::v0_0_1::span::{new_span};
    use crate::version::v0_0_1::wrap::Span;

    pub type KindPattern=KindPatternDef<GenericKindSelector,GenericSubKindSelector,SpecificSelector>;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct KindPatternDef<GenericKindSelector,GenericSubKindSelector,SpecificSelector> {
        pub kind: GenericKindSelector,
        pub sub_kind: GenericSubKindSelector,
        pub specific: ValuePattern<SpecificSelector>,
    }

    impl KindPattern {
        pub fn new(
            resource_type: GenericKindSelector,
            kind: GenericSubKindSelector,
            specific: ValuePattern<SpecificSelector>,
        ) -> Self {
            Self {
                kind: resource_type,
                sub_kind: kind,
                specific,
            }
        }

        pub fn matches(&self, kind: &GenericKind) -> bool
        where
            GenericKind: Eq + PartialEq,
        {
            self.kind.matches(&kind.resource_type())
                && self.sub_kind.matches(kind)
                && self.specific.is_match_opt(kind.specific().as_ref()).is_ok()
        }
    }

    impl ToString for KindPattern {
        fn to_string(&self) -> String {
            format!(
                "{}<{}<{}>>",
                self.kind.to_string(),
                self.sub_kind.to_string(),
                self.specific.to_string()
            )
        }
    }

    impl KindPattern {
        pub fn any() -> Self {
            Self {
                kind: GenericKindSelector::Any,
                sub_kind: GenericSubKindSelector::Any,
                specific: ValuePattern::Any,
            }
        }
    }


    pub type GenericSubKindSelector = Pattern<GenericKind>;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PointSelectorDef<Hop> {
        pub hops: Vec<Hop>,
    }

    pub type PointSelector = PointSelectorDef<Hop>;

    impl FromStr for PointSelector {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_, rtn) = all_consuming(point_selector)(new_span(s))?;
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
                    hop.kind.matches(&resource_kind)
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

    impl FromStr for PointSegSelector {
        type Err = MsgErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            result(all_consuming(point_segment_selector)(new_span(s)))
        }
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
    pub type SpecificSelector = SpecificSelectorDef<VendorSelector,ProductSelector,VariantSelector,VersionReq>;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SpecificSelectorDef<VendorSelector,ProductSelector,VariantSelector,VersionReq> {
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

    pub struct LabeledPrimitiveTypeDef<Pnt> {
        pub label: String,
        pub def: PayloadTypeDef<Pnt>,
    }

    pub struct PayloadTypeDef<Pnt> {
        pub primitive: PayloadType,
        pub format: Option<PayloadFormat>,
        pub verifier: Option<CallWithConfigDef<Pnt>>,
    }

    #[derive(Debug, Clone, strum_macros::Display, strum_macros::EnumString, Eq, PartialEq)]
    pub enum Format {
        #[strum(serialize = "json")]
        Json,
        #[strum(serialize = "image")]
        Image,
    }

    #[derive(Debug, Clone, strum_macros::Display, strum_macros::EnumString, Eq, PartialEq)]
    pub enum PipelineKind {
        Rc,
        Msg,
        Http
    }



    pub struct ParsedPipelineBlock {

    }

    pub enum PipelineSelector {
        Rc(RcPipelineSelector),
        Msg(MsgPipelineSelector),
        Http(HttpPipelineSelector),
    }

    impl ValueMatcher<RequestCore> for PipelineSelector {
        fn is_match(&self, core: &RequestCore) -> Result<(), MsgErr> {
            match &core.action {
                Action::Rc(found) => {
                    if let PipelineSelector::Rc(pattern) = self {
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
                    if let PipelineSelector::Msg(pattern) = self {
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
                    if let PipelineSelector::Http(pattern) = self {
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

    impl ToString for PipelineSelector {
        fn to_string(&self) -> String {
            match self {
                PipelineSelector::Rc(rc) => rc.to_string(),
                PipelineSelector::Msg(msg) => msg.to_string(),
                PipelineSelector::Http(http) => http.to_string(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RcPipelineSelector {
        pub command: Pattern<RcCommandType>,
    }

    impl ValueMatcher<RequestCore> for RcPipelineSelector {
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

    impl ToString for RcPipelineSelector {
        fn to_string(&self) -> String {
            format!("Rc<{}>", self.command.to_string())
        }
    }



    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MsgPipelineSelector {
        pub action: ValuePattern<StringMatcher>,
        pub path_regex: String,
    }

    impl ToString for MsgPipelineSelector {
        fn to_string(&self) -> String {
            format!("Msg<{}>{}", self.action.to_string(), self.path_regex)
        }
    }

    impl ValueMatcher<RequestCore> for MsgPipelineSelector {
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
    pub struct HttpPipelineSelector {
        pub method: MethodPattern,
        pub path_regex: String,
    }

    impl ToString for HttpPipelineSelector {
        fn to_string(&self) -> String {
            format!("Http<{}>{}", self.method.to_string(), self.path_regex)
        }
    }

    impl ValueMatcher<RequestCore> for HttpPipelineSelector {
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

    /*
    pub fn rc_call_kind(input: Span) -> Res<Span, CallKind> {
        delimited(tag("Rc<"), rc_command, tag(">"))(input)
            .map(|(next, rc_command)| (next, CallKind::Rc(rc_command)))
    }

     */

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

    pub type MapEntryPatternCtx = MapEntryPatternDef<PointCtx>;
    pub type MapEntryPattern = MapEntryPatternDef<Point>;

    #[derive(Clone)]
    pub struct MapEntryPatternDef<Pnt> {
        pub key: String,
        pub payload: ValuePattern<PayloadPatternDef<Pnt>>,
    }

    /*
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Hop {
        pub inclusive: bool,
        pub segment: PointSegSelector,
        pub tks: TksPattern,
    }

     */

    pub type Hop = HopDef<PointSegSelector, KindPattern>;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HopDef<Segment, KindPattern> {
        pub inclusive: bool,
        pub segment: Segment,
        pub kind: KindPattern,
    }


    impl Hop {
        pub fn matches(&self, point_kind_segment: &PointKindSeg) -> bool {
            self.segment.matches(&point_kind_segment.segment)
                && self.kind.matches(&point_kind_segment.kind)
        }
    }

    impl ToString for Hop {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            rtn.push_str(self.segment.to_string().as_str());

            if let Pattern::Exact(resource_type) = &self.kind.kind {
                rtn.push_str(format!("<{}", resource_type.to_string()).as_str());
                if let Pattern::Exact(kind) = &self.kind.sub_kind {
                    rtn.push_str(format!("<{}", kind.to_string()).as_str());
                    if let ValuePattern::Pattern(specific) = &self.kind.specific {
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

    impl <I:ToString> Pattern<I> {
        pub fn to_string_version(self) -> Pattern<String> {
            match self {
                Pattern::Any => Pattern::Any,
                Pattern::Exact(exact) => Pattern::Exact(exact.to_string())
            }
        }
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

    pub type GenericKindSelector = Pattern<GenericKindBase>;

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
            Ok(consume_hierarchy(new_span(s))?)
        }
    }
}

pub type PayloadBlock = PayloadBlockDef<Point>;
pub type PayloadBlockCtx = PayloadBlockDef<PointCtx>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PayloadBlockDef<Pnt> {
    RequestPattern(PatternBlockDef<Pnt>),
    ResponsePattern(PatternBlockDef<Pnt>),
}
impl CtxSubst<PayloadBlock> for PayloadBlockCtx{
    fn resolve_ctx(self, resolver: &dyn CtxResolver) -> Result<PayloadBlock, MsgErr> {
        match self {
            PayloadBlockCtx::RequestPattern(pattern) => {
                Ok(PayloadBlock::RequestPattern(pattern.resolve_ctx(resolver)?))
            }
            PayloadBlockCtx::ResponsePattern(pattern) => {
                Ok(PayloadBlock::ResponsePattern(pattern.resolve_ctx(resolver)?))
            }
        }
    }
}

    #[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UploadBlock {
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateBlock {
    pub payload: Payload,
}

pub type PatternBlock = PatternBlockDef<Point>;
pub type PatternBlockCtx = PatternBlockDef<PointCtx>;
pub type PatternBlockDef<Pnt> = ValuePattern<PayloadPatternDef<Pnt>>;


impl CtxSubst<PatternBlock> for PatternBlockCtx{
    fn resolve_ctx(self, resolver: &dyn CtxResolver) -> Result<PatternBlock, MsgErr> {
        match self {
            PatternBlockCtx::Any => Ok(PatternBlock::Any),
            PatternBlockCtx::None => Ok(PatternBlock::None),
            PatternBlockCtx::Pattern(pattern) => {
                Ok(PatternBlock::Pattern(pattern.resolve_ctx(resolver)?))
            }
        }
    }
}
