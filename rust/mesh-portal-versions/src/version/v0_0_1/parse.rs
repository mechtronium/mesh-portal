use ariadne::{Label, Report, ReportKind};
use core::fmt;
use std::collections::HashMap;
use std::fmt::Formatter;
use std::ops::{Deref, Range, RangeFrom, RangeTo};
use std::rc::Rc;
use std::str::FromStr;
use std::sync::Arc;

use nom::branch::alt;
use nom::bytes::complete::{escaped, is_a, is_not};
use nom::bytes::complete::{tag, take_till, take_until, take_until1, take_while};
use nom::character::complete::{
    alpha0, alpha1, alphanumeric0, alphanumeric1, anychar, char, digit0, digit1, line_ending,
    multispace0, multispace1, newline, one_of, satisfy, space0, space1,
};
use nom::combinator::{
    all_consuming, cut, eof, fail, not, opt, peek, recognize, success, value, verify,
};
use nom::error::{context, ContextError, Error, ErrorKind, ParseError, VerboseError};
use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{
    AsChar, Compare, CompareResult, FindToken, IResult, InputIter, InputLength, InputTake,
    InputTakeAtPosition, Offset, Parser, Slice, UnspecializedInput,
};
use nom_supreme::parse_from_str;

use crate::error::{MsgErr, ParseErrs};
use crate::version::v0_0_1::command::command::common::{PropertyMod, SetProperties, StateSrc};
use crate::version::v0_0_1::config::config::bind::Pipeline;
use crate::version::v0_0_1::config::config::bind::{
    BindConfig, ConfigScope, MessageKind, PipelineStep, PipelineStop, PipelinesSubScope, Selector,
};
use crate::version::v0_0_1::config::config::{bind, Config, PointConfig};
use crate::version::v0_0_1::entity::entity::request::create::{
    Create, CreateOp, KindTemplate, PointSegFactory, PointTemplate, PointTemplateSeg, Require,
    Strategy, Template,
};
use crate::version::v0_0_1::entity::entity::request::get::{Get, GetOp};
use crate::version::v0_0_1::entity::entity::request::select::{
    Select, SelectIntoPayload, SelectKind,
};
use crate::version::v0_0_1::entity::entity::request::set::Set;
use crate::version::v0_0_1::entity::entity::request::RcCommandType;
use crate::version::v0_0_1::entity::entity::EntityKind;
use crate::version::v0_0_1::id::id::{
    CaptureAddress, Point, PointSeg, PointSegDelim, PointSegKind, PointSegPairDef,
    PointSegPairSubst, PointSegSubst, PointSubst, RouteSeg, Version,
};
use crate::version::v0_0_1::parse::error::{first_context, result};
use crate::version::v0_0_1::parse::model::{ActionScope, BindScope, BindScopeKind, Block, BlockKind, DelimitedBlockKind, LexBlock, LexParentScope, LexPipeline, LexPipelineScope, LexPipelineSegment, LexRootScope, LexScope, LexScopeSelector, LexScopeSelectorAndFilters, NestedBlockKind, PipelineScope, PipelineScopeDef, PipelineSegment, RootScopeSelector, Scope, ScopeFilterDef, ScopeFiltersDef, ScopeSelectorAndFilters, ScopeSelectorAndFiltersDef, ScopeSelectorDef, Spanned, TerminatedBlockKind, TextType};
use crate::version::v0_0_1::parse::parse::{
    delim_kind, generic_kind_base, pattern, point_selector, specific_selector, value_pattern,
    version,
};
use crate::version::v0_0_1::payload::payload::{
    Call, CallKind, CallWithConfig, HttpCall, HttpMethod, HttpMethodType, ListPattern, MapPattern,
    MsgCall, NumRange, Payload, PayloadFormat, PayloadPattern, PayloadType, PayloadTypePattern,
};
use crate::version::v0_0_1::security::{
    AccessGrant, AccessGrantKindDef, AccessGrantKindSubst, AccessGrantSubst, ChildPerms,
    ParticlePerms, Permissions, PermissionsMask, PermissionsMaskKind, Privilege,
};
use crate::version::v0_0_1::selector::selector::{
    HttpPipelineSelector, LabeledPrimitiveTypeDef, MapEntryPattern, MsgPipelineSelector,
    PayloadTypeDef, PipelineSelector, PointKindHierarchy, PointKindSeg, PointSelector,
    RcPipelineSelector,
};
use crate::version::v0_0_1::selector::{selector, PatternBlock, PayloadBlock, UploadBlock};
use crate::version::v0_0_1::util::{MethodPattern, StringMatcher, ValuePattern};
use crate::version::v0_0_1::{create_span, Span};
use nom::bytes::complete::take;
use nom::character::is_space;
use nom_locate::LocatedSpan;
use nom_supreme::error::ErrorTree;
use nom_supreme::final_parser::ExtractContext;
use regex::internal::Input;
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
    alt((recognize(tag(".")), recognize(not(other_route_segment))))(input)
        .map(|(next, _)| (next, RouteSeg::Local))
}

pub fn domain_route_segment(input: Span) -> Res<Span, RouteSeg> {
    domain_chars(input).map(|(next, domain)| (next, RouteSeg::Domain(domain.to_string())))
}

pub fn tag_route_segment(input: Span) -> Res<Span, RouteSeg> {
    delimited(tag("["), skewer_chars, tag("]"))(input)
        .map(|(next, tag)| (next, RouteSeg::Tag(tag.to_string())))
}

pub fn mesh_route_segment(input: Span) -> Res<Span, RouteSeg> {
    delimited(tag("<<"), mesh_route_chars, tag(">>"))(input)
        .map(|(next, tag)| (next, RouteSeg::Tag(tag.to_string())))
}

pub fn other_route_segment(input: Span) -> Res<Span, RouteSeg> {
    alt((tag_route_segment, domain_route_segment, mesh_route_segment))(input)
}

pub fn point_route_segment(input: Span) -> Res<Span, RouteSeg> {
    alt((local_route_segment, other_route_segment))(input)
}

pub fn point_segment(input: Span) -> Res<Span, PointSeg> {
    alt((
        base_point_segment,
        space_point_segment,
        version_point_segment,
        filesystem_point_segment,
        file_point_segment,
    ))(input)
}

pub fn mesh_eos(input: Span) -> Res<Span, Span> {
    peek(alt((tag(":"), eop)))(input)
}

pub fn fs_trailing(input: Span) -> Res<Span, Span> {
    peek(pair(
        recognize(tag(":")),
        context("point:version:root_not_trailing", cut(tag("/"))),
    ))(input)
    .map(|(next, (rtn, _))| (next, rtn))
}

// version end of segment
pub fn ver_eos(input: Span) -> Res<Span, Span> {
    peek(alt((fs_trailing, tag(":/"), eop)))(input)
}

// end of point
pub fn eop(input: Span) -> Res<Span, Span> {
    peek(alt((
        eof,
        multispace1,
        tag("\""),
        tag("'"),
        tag("]"),
        tag(")"),
        tag("}"),
        tag("^"),
        tag("["),
        tag("("),
        tag("{"),
    )))(input)
}

pub fn space_no_dupe_dots(input: Span) -> Res<Span, ()> {
    context(
        "point:space_segment:dot_dupes",
        peek(cut(not(take_until("..")))),
    )(input)
    .map(|(next, _)| (next, ()))
}

pub fn space_point_segment(input: Span) -> Res<Span, PointSeg> {
    context(
        "point:space_segment",
        cut(pair(
            recognize(tuple((
                context("point:space_segment_leading", peek(alpha1)),
                space_no_dupe_dots,
                space_chars,
            ))),
            mesh_eos,
        )),
    )(input)
    .map(|(next, (space, x))| (next, PointSeg::Space(space.to_string())))
}

pub fn base_point_segment(input: Span) -> Res<Span, PointSeg> {
    preceded(
        peek(lowercase1),
        context("point:base_segment", cut(pair(rec_skewer, mesh_eos))),
    )(input)
    .map(|(next, (base, _))| (next, PointSeg::Base(base.to_string())))
}

pub fn version_point_segment(input: Span) -> Res<Span, PointSeg> {
    preceded(
        peek(digit0),
        context("point:version_segment", cut(tuple((version, ver_eos)))),
    )(input)
    .map(|(next, (version, _))| (next, PointSeg::Version(version)))
}

pub fn dir_pop(input: Span) -> Res<Span, PointSeg> {
    context("point:dir_pop", tuple((tag(".."), opt(tag("/")))))(input)
        .map(|(next, _)| (next, PointSeg::Pop))
}

pub fn filesystem_point_segment(input: Span) -> Res<Span, PointSeg> {
    tuple((
        peek(not(eop)),
        context(
            "point:file_or_directory",
            cut(alt((dir_pop, dir_point_segment, file_point_segment))),
        ),
    ))(input)
    .map(|(next, (_, seg))| (next, seg))
}

pub fn dir_point_segment(input: Span) -> Res<Span, PointSeg> {
    context("point:dir_segment", terminated(file_chars, tag("/")))(input)
        .map(|(next, dir)| (next, PointSeg::Dir(format!("{}/", dir))))
}

pub fn root_dir_point_segment(input: Span) -> Res<Span, PointSeg> {
    context("point:root_filesystem_segment", tag(":/"))(input)
        .map(|(next, _)| (next, PointSeg::FilesystemRootDir))
}

pub fn file_point_segment(input: Span) -> Res<Span, PointSeg> {
    context("point:file_segment", file_chars)(input)
        .map(|(next, filename)| (next, PointSeg::File(filename.to_string())))
}

pub fn point_subst(input: Span) -> Res<Span, PointSubst> {
    context(
        "point",
        tuple((alt((root_point_subst, point_non_root_subst)), peek(eop))),
    )(input)
    .map(|(next, (point, _))| (next, point))
}

pub fn point(input: Span) -> Res<Span, Point> {
    context(
        "point",
        tuple((alt((root_point, point_non_root)), peek(eop))),
    )(input)
    .map(|(next, (point, _))| (next, point))
}

pub fn root_point(input: Span) -> Res<Span, Point> {
    tuple((opt(terminated(point_route_segment, tag("::"))), tag("ROOT")))(input).map(
        |(next, (route, _))| {
            let route = route.unwrap_or(RouteSeg::Local);
            let point = Point {
                route,
                segments: vec![],
            };
            (next, point)
        },
    )
}

pub fn root_point_subst(input: Span) -> Res<Span, PointSubst> {
    context(
        "root_point_subst",
        tuple((
            opt(terminated(var_subst(point_route_segment), tag("::"))),
            tag("ROOT"),
        )),
    )(input)
    .map(|(next, (route, _))| {
        let route = route.unwrap_or(Subst::Resolved(RouteSeg::Local));
        let point = PointSubst {
            route,
            segments: vec![],
        };
        (next, point)
    })
}

pub fn point_non_root(input: Span) -> Res<Span, Point> {
    context(
        "point_non_root",
        tuple((
            context(
                "point_route",
                opt(terminated(point_route_segment, tag("::"))),
            ),
            space_point_segment,
            many0(tuple((
                seg_delim,
                peek(context("point:bad_leading", cut(alt((lowercase1, digit1))))),
                base_point_segment,
            ))),
            opt(mesh_seg(version_point_segment)),
            opt(tuple((
                root_dir_point_segment,
                many0(filesystem_point_segment),
                eop,
            ))),
            eop,
        )),
    )(input)
    .map(
        |(next, (route, space, mut bases, version, filesystem, _))| {
            let route = route.unwrap_or(RouteSeg::Local);
            let mut segments = vec![];
            let mut bases = bases.into_iter().map(|(_, _, seg)| seg).collect();
            segments.push(space);
            segments.append(&mut bases);
            match version {
                None => {}
                Some(version) => {
                    segments.push(version);
                }
            }

            if let Option::Some((fsroot, mut files, _)) = filesystem {
                segments.push(fsroot);
                segments.append(&mut files);
            }

            let point = Point { route, segments };

            (next, point)
        },
    )
}

pub fn point_non_root_subst(input: Span) -> Res<Span, PointSubst> {
    context(
        "point_non_root_subst",
        tuple((
            context(
                "point_route",
                opt(terminated(var_subst(point_route_segment), tag("::"))),
            ),
            first_seg_subst(space_point_segment),
            many0(tuple((
                tag(":"),
                peek(context(
                    "point:bad_leading",
                    cut(alt((tag("$"), lowercase1, digit1))),
                )),
                var_subst(base_point_segment),
            ))),
            opt(preceded(tag(":"), var_subst(version_point_segment))),
            opt(tuple((
                no_subst(root_dir_point_segment),
                many0(var_subst(filesystem_point_segment)),
                eop,
            ))),
            eop,
        )),
    )(input)
    .map(
        |(next, (route, space, mut bases, version, filesystem, _))| {
            let space = PointSegPairSubst::new(PointSegDelim::Empty, space);
            let route = route.unwrap_or(Subst::Resolved(RouteSeg::Local));
            let mut segments = vec![];
            let mut bases = bases
                .into_iter()
                .map(|(_, _, seg)| PointSegPairSubst::new(PointSegDelim::Mesh, seg))
                .collect();
            segments.push(space);
            segments.append(&mut bases);
            match version {
                None => {}
                Some(version) => {
                    let version = PointSegPairDef::new(PointSegDelim::Mesh, version);
                    segments.push(version);
                }
            }

            if let Option::Some((fsroot, mut files, _)) = filesystem {
                let fsroot = PointSegPairSubst::new(PointSegDelim::Mesh, fsroot);
                let mut files: Vec<PointSegPairSubst> = files
                    .into_iter()
                    .map(|f| PointSegPairDef::new(PointSegDelim::File, f))
                    .collect();
                segments.push(fsroot);
                segments.append(&mut files);
            }

            let point = PointSubst { route, segments };

            (next, point)
        },
    )
}

/*
pub fn point_non_root_subst(input: Span) -> Res<Span, PointSubst> {
    context(
        "point_subst",
        tuple((
            context(
                "point_subst_route",
                opt(terminated(var_subst(point_route_segment), tag("::"))),
            ),
            first_seg_subst(space_point_segment),
            many0(subst_seg(base_point_segment)),
            opt(subst_seg(version_point_segment)),
            opt(no_subst(root_dir_point_segment)),
            many0(subst_seg(filesystem_point_segment)),
        )),
    )(input)
    .map(
        |(next, (route, space, mut bases, version, fsroot, mut files))| {
            let route = match route {
                None => Subst::Resolved(RouteSeg::Local),
                Some(route) => route,
            };
            let space = PointSegPairSubst::new(PointSegDelim::Empty, space);
            let mut segments = vec![];
            segments.push(space);
            segments.append(&mut bases);
            match version {
                None => {}
                Some(version) => {
                    segments.push(version);
                }
            }

            if let Option::Some(root) = fsroot {
                segments.push(PointSegPairSubst::new(PointSegDelim::Mesh, root));
                segments.append(&mut files);
            }

            let point = PointSubst { route, segments };

            (next, point)
        },
    )
}
*/

pub fn consume_point(input: &str) -> Result<Point, MsgErr> {
    let span = create_span(input);
    let point = result(context("consume", all_consuming(point))(span))?;
    Ok(point)
}

pub fn point_old(input: Span) -> Res<Span, Point> {
    let (next, point) = point_subst(input.clone())?;
    match point.brute_resolve() {
        Ok(point) => Ok((next, point)),
        Err(err) => {
            let e = ErrorTree::from_error_kind(input.clone(), ErrorKind::Fail);
            let e = ErrorTree::add_context(input, "point-subst-brute-force", e);
            return Err(nom::Err::Failure(e));
        }
    }
}

pub fn consume_point_subst(input: &str) -> Result<PointSubst, MsgErr> {
    let span = create_span(input);
    let point = result(context("consume", all_consuming(point_subst))(span))?;
    Ok(point)
}

pub fn capture_point(input: Span) -> Res<Span, CaptureAddress> {
    context(
        "Address",
        tuple((
            tuple((
                point_route_segment,
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
    space_chars_plus_capture(input).map(|(next, space)| (next, PointSeg::Space(space.to_string())))
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
        tuple((point_route_segment, space_point_kind_segment)),
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
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn skewer_dot<I, E>(i: I) -> IResult<I, I, E>
where
    I: InputTakeAtPosition + nom::InputLength,
    <I as InputTakeAtPosition>::Item: AsChar,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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
    recognize(tuple((lowercase1, opt(skewer))))(input)
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
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn rough_filepath_chars_plus_capture(i: Span) -> Res<Span, Span> {
    context(
        "capture-path",
        recognize(pair(
            tag("/"),
            cut(many1(alt((
                recognize(filepath_chars),
                recognize(rough_cap_subst),
            )))),
        )),
    )(i)
}

pub fn rough_cap_subst(input: Span) -> Res<Span, Span> {
    delimited(
        tag("$("),
        recognize(pair(
            alpha1,
            many0(alt((
                alpha1,
                digit1,
                recognize(any_block),
                recognize(regex),
            ))),
        )),
        tag(")"),
    )(input)
}

pub fn regex<T>(i: T) -> Res<T, T>
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
    recognize(tuple((is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alphanumeric0)))(input)
    //recognize(alpha1)(input)
}

pub fn skewer_case(input: Span) -> Res<Span, Span> {
    rec_skewer(input)
    //recognize(alpha1)(input)
}

pub fn single_lowercase<T, Input, Error: ParseError<Input>>(
    arr: T,
) -> impl Fn(Input) -> IResult<Input, Input, Error>
where
    Input: InputTakeAtPosition,
    T: FindToken<<Input as InputTakeAtPosition>::Item>,
{
    move |i: Input| {
        let e: ErrorKind = ErrorKind::IsA;
        i.split_at_position1_complete(|c| !arr.find_token(c), e)
    }
}

pub fn single_lowerscase(input: Span) -> Res<Span, Span> {
    is_a("abcdefghijklmnopqrstuvwxyz")(input)
}
pub fn single_digit(input: Span) -> Res<Span, Span> {
    is_a("abcdefghijklmnopqrstuvwxyz")(input)
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
        tuple((point_route_segment, space_point_segment)),
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
    ))(input.clone())?;

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
                    input.clone(),
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
    let (next, (upload, _, point)) = tuple((upload_step, space1, point))(input.clone())?;

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
                input.clone(),
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
    Ctx(Ctx),
}

impl ToString for Symbol {
    fn to_string(&self) -> String {
        match self {
            Symbol::Var(var) => format!("$({})", var),
            Symbol::Ctx(ctx) => ctx.to_string(),
        }
    }
}

impl Symbol {
    pub fn named(name: String) -> Self {
        Self::Var(name)
    }

    pub fn ctx(ctx: Ctx) -> Self {
        Self::Ctx(ctx)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Ctx {
    RelativePoint,
    RelativePointPop,
}

impl ToString for Ctx {
    fn to_string(&self) -> String {
        match self {
            Ctx::RelativePoint => ".".to_string(),
            Ctx::RelativePointPop => "..".to_string(),
        }
    }
}

pub trait Resolver {
    fn string(&self, symbol: &Symbol) -> Result<String, MsgErr> {
        match symbol {
            Symbol::Var(var) => self.val(var.as_str()),
            Symbol::Ctx(ctx) => self.ctx(ctx),
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

impl NoResolver {
    pub fn new() -> Self {
        Self {}
    }
}

impl Resolver for NoResolver {
    fn val(&self, id: &str) -> Result<String, MsgErr> {
        Err(format!("unexpected variable '{}'. variables not expected here", id).into())
    }
    fn ctx(&self, ctx: &Ctx) -> Result<String, MsgErr> {
        match ctx {
            Ctx::RelativePoint => {
                Err("context operators '.' (reference to WorkingPoint) not expected here".into())
            }
            Ctx::RelativePointPop => Err(
                "context operator '..' (reference to WorkingPointAboveOne) not expected here"
                    .into(),
            ),
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
            working_point: Point::root(),
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
            Ctx::RelativePoint => Ok(self.working_point.to_string()),
            Ctx::RelativePointPop => Ok(self
                .working_point
                .parent()
                .ok_or("already at the root point cannot pop any further.")?
                .to_string()),
        }
    }
}

pub trait ToResolved<Resolved>
where
    Self: Sized,
{
    fn to_resolved(self, resolver: &dyn Resolver) -> Result<Resolved, MsgErr>;
    fn to_resolved_str(self, resolver: &dyn Resolver) -> Result<String, MsgErr> {
        Err("string representation not supported".into())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CapSubst<Resolved>
where
    Resolved: Clone,
{
    Symbol(Symbol),
    Resolved(Resolved),
}

impl<Resolved> CapSubst<Resolved>
where
    Resolved: Clone,
{
    pub fn resolved(&self) -> Result<Resolved, ()> {
        match self {
            Self::Symbol(_) => Err(()),
            Self::Resolved(resolved) => Ok(resolved.clone()),
        }
    }

    pub fn check_resolved(&self) -> Result<(), ()> {
        match self {
            Self::Symbol(_) => Err(()),
            Self::Resolved(_) => Ok(()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Subst<Resolved>
where
    Resolved: Clone,
{
    Symbol(Symbol),
    Resolved(Resolved),
}

impl<Resolved> Subst<Resolved>
where
    Resolved: Clone,
{
    pub fn resolved(&self) -> Result<Resolved, ()> {
        match self {
            Self::Symbol(_) => Err(()),
            Self::Resolved(resolved) => Ok(resolved.clone()),
        }
    }

    pub fn check_resolved(&self) -> Result<(), ()> {
        match self {
            Self::Symbol(_) => Err(()),
            Self::Resolved(_) => Ok(()),
        }
    }
}

impl<Resolved> ToString for Subst<Resolved>
where
    Resolved: ToString + Clone + FromStr<Err = MsgErr>,
{
    fn to_string(&self) -> String {
        match self {
            Subst::Symbol(symbol) => symbol.to_string(),
            Subst::Resolved(resolved) => resolved.to_string(),
        }
    }
}

pub trait BruteResolver<Resolved>
where
    Self: Sized + ToResolved<Resolved>,
{
    fn brute_resolve(self) -> Result<Resolved, MsgErr> {
        let resolver = NoResolver::new();
        Ok(self.to_resolved(&resolver)?)
    }
}

impl<Resolved> ToResolved<Resolved> for Subst<Resolved>
where
    Resolved: Clone + FromStr<Err = MsgErr> + ToString,
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
        let result = variable(span(input.to_string().as_str()) );

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
                        Ok((span(next.as_str()), Subst::Value(v)))
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
    move |input: I| {
        f.parse(input)
            .map(|(next, val)| (next, Subst::Resolved(val)))
    }
}

pub fn diagnose<I: Clone, O, E: ParseError<I>, F>(
    tag: &'static str,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    F: nom::Parser<I, O, E>,
    E: nom::error::ContextError<I>,
    O: Clone,
{
    move |input: I| {
        let (next, i) = f.parse(input)?;
        Ok((next, i))
    }
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

pub fn cap_subst<I: Clone, O, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, CapSubst<O>, E>
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
        Ok((next, v)) => Ok((next, CapSubst::Symbol(v))),
        Err(err) => f
            .parse(input.clone())
            .map(|(next, val)| (next, CapSubst::Resolved(val))),
    }
}

pub fn pop<I: Clone, E: ParseError<I>, F>(mut f: F) -> impl FnMut(I) -> IResult<I, PointSeg, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    F: nom::Parser<I, PointSeg, E>,
    E: nom::error::ContextError<I>,
{
    move |input: I| match tag::<&str, I, E>("..")(input.clone()) {
        Ok((next, v)) => Ok((next, PointSeg::Pop)),
        Err(err) => f.parse(input.clone()),
    }
}

pub fn mesh_seg<I: Clone, E: ParseError<I>, F>(mut f: F) -> impl FnMut(I) -> IResult<I, PointSeg, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: nom::Parser<I, PointSeg, E> + Clone,
    E: nom::error::ContextError<I>,
{
    move |input: I| {
        tuple((seg_delim, pop(f.clone())))(input).map(|(next, (delim, seg))| (next, seg))
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
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
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
pub fn seg_delim<I, E>(input: I) -> IResult<I, PointSegDelim, E>
where
    I: ToString
        + Clone
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    alt((
        value(PointSegDelim::File, tag("/")),
        value(PointSegDelim::Mesh, tag(":")),
    ))(input)
    .map(|(next, delim)| (next, delim))
}

// end of segment
pub fn eos<I, E>(input: I) -> IResult<I, (), E>
where
    I: ToString
        + Clone
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    peek(alt((tag("/"), tag(":"), space1)))(input).map(|(next, _)| (next, ()))
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
        context(
            "variable",
            cut(delimited(
                context("variable:open", tag("(")),
                variable_name,
                tag(")"),
            )),
        ),
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
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    alt((
        value(Ctx::RelativePointPop, tuple((tag(".."), eos))),
        value(Ctx::RelativePoint, tuple((tag("."), eos))),
    ))(input)
    .map(|(next, ctx)| (next, Symbol::ctx(ctx)))
}

pub fn variable_name<I, E>(input: I) -> IResult<I, I, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
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
pub fn ispan<'a, I: Clone, O, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Spanned<I, O>, E>
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
    move |input: I| {
        let (next, element) = f.parse(input.clone())?;
        Ok((next, Spanned::new(element, input.clone())))
    }
}

pub fn span<'a, O, F>(mut f: F) -> impl FnMut(Span<'a>) -> Res<Span, Spanned<Span<'a>, O>>
where
    F: nom::Parser<Span<'a>, O, ErrorTree<Span<'a>>>,
    O: Clone,
{
    move |input: Span| {
        let (next, element) = f.parse(input.clone())?;
        Ok((
            next.clone(),
            Spanned::new(element, input.slice(0..(input.len() - next.len()))),
        ))
    }
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

pub fn none<I, O, E>(input: I) -> IResult<I, Option<O>, E> {
    Ok((input, None))
}

pub fn nospace<I, E>(input: I) -> IResult<I, I, E>
where
    I: InputLength + InputTake + Clone,
{
    Ok((input.clone(), input.take(0)))
}

pub fn some<'a, I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Option<O>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
    F: nom::Parser<I, O, E> + Clone,
{
    move |input: I| {
        f.clone()
            .parse(input)
            .map(|(next, output)| (next, Some(output)))
    }
}

pub fn lex_block_alt<'a, I, E>(kinds: Vec<BlockKind>) -> impl FnMut(I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    move |input: I| {
        for kind in &kinds {
            let result = lex_block(kind.clone())(input.clone());
            match &result {
                Ok((next, block)) => return result,
                Err(err) => {
                    match err {
                        nom::Err::Incomplete(Needed) => return result,
                        nom::Err::Error(e) => {
                            // let's hope for another match...
                        }
                        nom::Err::Failure(e) => return result,
                    }
                }
            }
        }

        Err(nom::Err::Failure(E::from_error_kind(
            input.clone(),
            ErrorKind::Alt,
        )))
    }
}

pub fn lex_block<'a, I, E>(kind: BlockKind) -> impl FnMut(I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    move |input: I| match kind {
        BlockKind::Nested(kind) => lex_nested_block(kind).parse(input),
        BlockKind::Terminated(kind) => lex_terminated_block(kind).parse(input),
        BlockKind::Delimited(kind) => lex_delimited_block(kind).parse(input),
        BlockKind::Partial => {
            eprintln!("parser should not be seeking partial block kinds...");
            Err(nom::Err::Failure(E::from_error_kind(
                input,
                ErrorKind::IsNot,
            )))
        }
    }
}

pub fn lex_terminated_block<'a, I, E>(
    kind: TerminatedBlockKind,
) -> impl FnMut(I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    move |input: I| {
        terminated(
            recognize(many0(satisfy(|c| c != kind.as_char()))),
            tag(kind.tag()),
        )(input)
        .map(|(next, content)| {
            let block = LexBlock {
                kind: BlockKind::Terminated(kind),
                content,
                data: (),
            };

            (next, block)
        })
    }
}

/// rough block simply makes sure that the opening and closing symbols match
/// it accounts for multiple embedded blocks of the same kind but NOT of differing kinds
pub fn lex_nested_block<'a, I, E>(
    kind: NestedBlockKind,
) -> impl FnMut(I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    move |input: I| {
        let (next, content) = context(
            kind.context(),
            delimited(
                context(kind.open_context(), tag(kind.open())),
                recognize(many0(alt((recognize(lex_nested_block(kind.clone())),recognize(tuple((
                    not(peek(tag(kind.close()))),
                    alt((recognize(pair(tag("\\"), anychar)), recognize(anychar))),
                ))))))),
                context(kind.close_context(), cut(tag(kind.close()))),
            ),
        )(input)?;
        let block = Block::parse(BlockKind::Nested(kind), content);
        Ok((next, block))
    }
}

pub fn nested_block_content(kind: NestedBlockKind) -> impl FnMut(Span) -> Res<Span, Span> {
    move |input: Span| {
        nested_block(kind)(input).map(|(next, block)| {
            (next, block.content)
        })
    }
}

pub fn nested_block(kind: NestedBlockKind) -> impl FnMut(Span) -> Res<Span, Block<Span, ()>> {
    move |input: Span| {
        let (next, content) = context(
            kind.context(),
            delimited(
                context(kind.open_context(), tag(kind.open())),
                recognize(many0(tuple((
                    not(peek(tag(kind.close()))),
                    context(
                        kind.unpaired_closing_scope(),
                        cut(peek(expected_block_terminator_or_non_terminator(
                            kind.clone(),
                        ))),
                    ),
                    alt((
                        recognize(pair(peek(block_open), any_block)),
                        recognize(anychar),
                    )),
                )))),
                context(kind.close_context(), cut(tag(kind.close()))),
            ),
        )(input)?;
        let block = Block::parse(BlockKind::Nested(kind), content);
        Ok((next, block))
    }
}

pub fn lex_delimited_block<'a, I, E>(
    kind: DelimitedBlockKind,
) -> impl FnMut(I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    move |input: I| {
        let (next, content) = context(
            kind.context(),
            delimited(
                context(kind.context(), tag(kind.delim())),
                recognize(many0(tuple((
                    not(peek(tag(kind.delim()))),
                    alt((recognize(pair(tag("\\"), anychar)), recognize(anychar))),
                )))),
                context(kind.missing_close_context(), cut(tag(kind.delim()))),
            ),
        )(input)?;
        let block = Block::parse(BlockKind::Delimited(kind), content);
        Ok((next, block))
    }
}

fn block_open(input: Span) -> Res<Span, NestedBlockKind> {
    alt((
        value(NestedBlockKind::Curly, tag(NestedBlockKind::Curly.open())),
        value(NestedBlockKind::Angle, tag(NestedBlockKind::Angle.open())),
        value(NestedBlockKind::Parens, tag(NestedBlockKind::Parens.open())),
        value(NestedBlockKind::Square, tag(NestedBlockKind::Square.open())),
    ))(input)
}

fn any_lex_block<'a, I, E>(input: I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    alt((
        lex_nested_block(NestedBlockKind::Curly),
        lex_nested_block(NestedBlockKind::Angle),
        lex_nested_block(NestedBlockKind::Parens),
        lex_nested_block(NestedBlockKind::Square),
        lex_delimited_block(DelimitedBlockKind::DoubleQuotes),
        lex_delimited_block(DelimitedBlockKind::SingleQuotes),
    ))(input)
}

fn any_block(input: Span) -> Res<Span, LexBlock<Span>> {
    alt((
        nested_block(NestedBlockKind::Curly),
        nested_block(NestedBlockKind::Angle),
        nested_block(NestedBlockKind::Parens),
        nested_block(NestedBlockKind::Square),
        lex_delimited_block(DelimitedBlockKind::DoubleQuotes),
        lex_delimited_block(DelimitedBlockKind::SingleQuotes),
    ))(input)
}

pub fn expected_block_terminator_or_non_terminator<I>(
    expect: NestedBlockKind,
) -> impl FnMut(I) -> Res<I, ()>
where
    I: InputIter + InputLength + Slice<RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar,
    I: Clone,
{
    move |input: I| -> Res<I, ()> {
        verify(anychar, move |c| {
            if NestedBlockKind::is_block_terminator(*c) {
                *c == expect.close_as_char()
            } else {
                true
            }
        })(input)
        .map(|(next, _)| (next, ()))
    }
}

/*
pub fn lex_hierarchy_scope<'a>(
    scope: LexScope<Span<'a>>,
    max_depth: usize,
) -> Result<LexHierarchyScope<'a>, MsgErr> {
    let mut errs = vec![];
    let scope = lex_child_scopes(scope)?;
    let mut children = vec![];

    for child in scope.block {
        if max_depth <= 0 {
            let mut builder = Report::build(ReportKind::Error, (), 0);
            let report = builder
                .with_message("exceeded max depth hierarchy for nested scopes")
                .with_label(
                    Label::new(
                        child.block.content.location_offset()
                            ..child.block.content.location_offset() + child.block.content.len(),
                    )
                    .with_message("Nest Limit Exceeded"),
                )
                .finish();
            return Err(ParseErrs::new(report, child.block.content.extra.clone()).into());
        }
        match lex_hierarchy_scope(child, max_depth - 1) {
            Ok(child) => {
                children.push(child);
            }
            Err(err) => errs.push(err),
        }
    }

    Ok(LexHierarchyScope::new(scope.selector.clone(), children))
}*/

pub fn lex_child_scopes<'a>(parent: LexScope<Span<'a>>) -> Result<LexParentScope<'a>, MsgErr> {
    if parent.selector.selector.children.is_some() {
        let (_, child_selector) = all_consuming(lex_scope_selector)(
            parent
                .selector
                .selector
                .children
                .as_ref()
                .expect("child names...")
                .clone(),
        )?;

        let child = LexScope::new(
            ScopeSelectorAndFiltersDef::new(child_selector.into(), parent.selector.filters),
            parent.block,
        );

        Ok(LexParentScope {
            selector: LexScopeSelectorAndFilters::new(
                parent.selector.selector.clone(),
                ScopeFiltersDef::empty(),
            ),
            pipeline_step: None,
            block: vec![child],
        })
    } else {
        let scopes = lex_scopes(parent.block.content)?;

        Ok(LexParentScope {
            selector: parent.selector.into(),
            pipeline_step: parent.pipeline_step,
            block: scopes,
        })
    }
}

pub fn lex_scope(input: Span) -> Res<Span, LexScope<Span>> {
    context(
        "scope",
        tuple((
            peek(alt((alpha1, tag("<")))),
            lex_scope_selector_and_filters,
            multispace1,
            lex_scope_pipeline_step_and_block,
        )),
    )(input)
    .map(|(next, (_, selector, _, (pipeline_step, block)))| {
        let scope = LexScope {
            selector,
            pipeline_step,
            block,
        };
        (next, scope)
    })
}

pub fn lex_scoped_block_kind(input: Span) -> Res<Span, BlockKind> {
    alt((
        value(
            BlockKind::Nested(NestedBlockKind::Curly),
            recognize(tuple((
                multispace0,
                rough_pipeline_step,
                multispace0,
                lex_block(BlockKind::Nested(NestedBlockKind::Curly)),
            ))),
        ),
        value(
            BlockKind::Terminated(TerminatedBlockKind::Semicolon),
            recognize(pair(
                rough_pipeline_step,
                lex_block(BlockKind::Terminated(TerminatedBlockKind::Semicolon)),
            )),
        ),
    ))(input)
}

pub fn lex_scope_pipeline_step_and_block(input: Span) -> Res<Span, (Option<Span>, LexBlock<Span>)> {
    let (_, block_kind) = peek(lex_scoped_block_kind)(input.clone())?;
    match block_kind {
        BlockKind::Nested(_) => tuple((
            rough_pipeline_step,
            multispace1,
            lex_block(BlockKind::Nested(NestedBlockKind::Curly))),
        )(input)
        .map(|(next, (step, _, block))| (next, (Some(step), block))),
        BlockKind::Terminated(_) => {
            lex_block(BlockKind::Terminated(TerminatedBlockKind::Semicolon))(input)
                .map(|(next, block)| (next, (None, block)))
        }
        _ => unimplemented!(),
    }
}

pub fn lex_sub_scope_selectors_and_filters_and_block(input: Span) -> Res<Span, LexBlock<Span>> {
    recognize(pair(
        nested_block_content(NestedBlockKind::Angle),
        tuple((
            opt(scope_filters),
            multispace0,
            opt(rough_pipeline_step),
            multispace0,
            lex_block_alt(vec![
                BlockKind::Nested(NestedBlockKind::Curly),
                BlockKind::Terminated(TerminatedBlockKind::Semicolon),
            ]),
        )),
    ))(input)
    .map(|(next, content)| {
        (
            next,
            LexBlock {
                kind: BlockKind::Partial,
                content,
                data: (),
            },
        )
    })
}

pub fn root_scope(input: Span) -> Res<Span, LexRootScope<Span>> {
    context(
        "root-scope",
        tuple((
            root_scope_selector,
            multispace0,
            context("root-scope:block", cut(peek(tag("{")))),
            context("root-scope:block",cut(lex_nested_block(NestedBlockKind::Curly))),
        )),
    )(input)
    .map(|(next, (selector, _, _, block))| {
        let scope = LexRootScope::new(selector, block);
        (next, scope)
    })
}

pub fn lex_scopes(input: Span) -> Result<Vec<LexScope<Span>>, MsgErr> {

    if input.len() == 0 {
        return Ok(vec![]);
    }

    if wrapper(input.clone(), all_consuming(multispace1)).is_ok() {
        return Ok(vec![]);
    }

    result(context(
        "parsed-scopes",
        all_consuming(many0(delimited(multispace0, context("scope",pair(peek(not(alt((tag("}"),eof)))),cut(lex_scope))), multispace0))),
    )(input).map( |(next,scopes)| {
        let scopes: Vec<LexScope<Span>> = scopes.into_iter().map(|scope|scope.1).collect();
        (next,scopes)
    } ))
}

/*
pub fn sub_scope_selector(input: Span) -> Res<Span, ScopeSelector<Span>> {
    alt((sub_scope_selector_expanded, sub_scope_selector_collapsed))
}




pub fn lex_scope_selector_no_filters(
    input: Span,
) -> Res<Span, ParsedScopeSelectorAndFilters<Span>> {
    context("parsed-scope-selector-no-filters", lex_scope_selector)(input)
        .map(|(next, selector)| (next, ParsedScopeSelectorAndFilters::new(selector, vec![])))
}

 */

pub fn next_selector(input: Span) -> Res<Span, (Span, Option<Span>)> {
    match wrapper(
        input.clone(),
        pair(
            peek(tag("<")),
            tuple((
                tag("<"),
                pair(
                    context("scope-selector", cut(alphanumeric1)),
                    opt(recognize(nested_block(NestedBlockKind::Angle))),
                ),
                tag(">"),
            )),
        ),
    )
    .map(|(next, (_, (_, (name, children), _)))| (next, (name, children)))
    {
        Ok((next, (name, children))) => return Ok((next, (name, children))),
        Err(_) => {}
    }
    pair(
        context("scope-selector", cut(alphanumeric1)),
        opt(recognize(nested_block(NestedBlockKind::Angle))),
    )(input)
}

pub fn lex_scope_selector_and_filters(
    input: Span,
) -> Res<Span, ScopeSelectorAndFiltersDef<LexScopeSelector<Span>, Span>> {
    context(
        "parsed-scope-selector-and-filter",
        pair(lex_scope_selector, scope_filters),
    )(input)
    .map(|(next, (selector, filters))| (next, ScopeSelectorAndFiltersDef::new(selector, filters)))
}

pub fn lex_scope_selector(input: Span) -> Res<Span, LexScopeSelector<Span>> {
    context("parsed-scope-selector", next_selector)(input)
        .map(|(next, (name, children))| (next, LexScopeSelector::new(name, children)))
}

pub fn wrapper<I, O, F>(input: I, mut f: F) -> Res<I, O>
where
    F: FnMut(I) -> Res<I, O>,
{
    f.parse(input)
}

pub fn parse_inner_block<I, E, F>(
    kind: NestedBlockKind,
    mut f: &F,
) -> impl FnMut(I) -> IResult<I, I, E> + '_
where
    I: Clone,
    &'static str: FindToken<<I as InputTakeAtPosition>::Item>,

    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Copy,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
    F: Fn(char) -> bool,
    F: Clone,
{
    move |input: I| {
        let (next, rtn) = alt((
            delimited(
                tag(kind.open()),
                recognize(many1(alt((
                    recognize(any_lex_block),
                    recognize(verify(anychar, move |c| {
                        f(*c) && *c != kind.close_as_char()
                    })),
                )))),
                tag(kind.close()),
            ),
            recognize(many1(verify(anychar, move |c| {
                f(*c) && *c != kind.close_as_char()
            }))),
        ))(input)?;
        Ok((next, rtn))
    }
}

pub fn parse_include_blocks<I, O2, E, F>(
    kind: NestedBlockKind,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, I, E>
where
    I: Clone,
    &'static str: FindToken<<I as InputTakeAtPosition>::Item>,

    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
    F: FnMut(I) -> IResult<I, O2, E>,
    F: Clone,
    <I as InputIter>::Item: std::marker::Copy,
{
    move |input: I| {
        recognize(many0(alt((
            recognize(any_lex_block),
            recognize(verify(anychar, move |c| *c != kind.close_as_char())),
        ))))(input)
    }
}

pub fn scope_filters<'a>(input: Span) -> Res<Span, ScopeFiltersDef<Span>> {
    tuple((
        opt(rough_filepath_chars_plus_capture),
        pair(opt(scope_filter), many0(preceded(tag("-"), scope_filter))),
    ))(input)
    .map(|(next, (path, (first, mut many_filters)))| {
        let mut filters = vec![];
        match first {
            None => {}
            Some(first) => {
                filters.push(first);
            }
        }
        filters.append(&mut many_filters);
        let filters = ScopeFiltersDef { path, filters };
        (next, filters)
    })
}

pub fn scope_filter(input: Span) -> Res<Span, ScopeFilterDef<Span>> {
    delimited(
        tag("("),
        context(
            "scope-filter",
            cut(tuple((
                context("filter-name", cut(scope_name)),
                opt(context(
                    "filter-arguments",
                    preceded(
                        multispace1,
                        parse_include_blocks(NestedBlockKind::Parens, args),
                    ),
                )),
            ))),
        ),
        tag(")"),
    )(input)
    .map(|(next, (name, args))| {
        let filter = ScopeFilterDef { name, args };
        (next, filter)
    })
}

pub fn scope_name(input: Span) -> Res<Span, Span> {
    recognize(pair(skewer_case, peek(alt((eof, multispace1, tag(")"))))))(input)
}

pub fn root_scope_selector<'a>(
    input: Span,
) -> Res<Span, RootScopeSelector<Span, Spanned<Span, Version>>> {
    context(
        "root-scope-selector",
        cut(preceded(
            multispace0,
            pair(context("root-scope-selector:name",cut(root_scope_selector_name)), context("root-scope-selector:version",cut(scope_version))),
        )),
    )(input)
    .map(|(next, (name, version))| (next, RootScopeSelector { version, name }))
}

pub fn scope_version(input: Span) -> Res<Span, Spanned<Span, Version>> {
    context(
        "scope-selector-version",
        tuple((
            tag("(version="),
            span(version),
            context("scope-selector-version-closing-tag", tag(")")),
        )),
    )(input)
    .map(|((next, (_, version, _ )))| (next, version))
}

/*
pub fn mytag<O>( tag: &str ) -> impl Fn(Span) -> Res<Span,O>
{
    move |i: Span| {
        let tag_len = tag.input_len();
        let t = tag.clone();
        let res: IResult<_, _, Error> = match i.compare(t) {
            CompareResult::Ok => Ok(i.take_split(tag_len)),
            _ => {
                let e: ErrorKind = ErrorKind::Tag;
                Err(Err::Error(Error::from_error_kind(i, e)))
            }
        };
        res
    }
}

 */

pub fn scope_selector_name(input: Span) -> Res<Span, Span> {
    context(
        "scope-selector-name",
        delimited(
            (context(
                "scope-selector-name:expect-alphanumeric-leading",
                cut(peek(alpha1)),
            )),
            alphanumeric1,
            context(
                "scope-selector-name:expect-termination",
                cut(peek(alt((
                    multispace1,
                    tag("{"),
                    tag("("),
                    tag("<"),
                    tag(">"),
                )))),
            ),
        ),
    )(input)
    .map(|(next, name)| (next, name))
}

pub fn root_scope_selector_name(input: Span) -> Res<Span, Span> {
    context(
        "root-scope-selector-name",
        pair((peek(alpha1)), alphanumeric1),
    )(input)
    .map(|(next, (_, name))| (next, name))
}

pub fn lex_root_scope(span: Span) -> Result<LexRootScope<Span>, MsgErr> {
    let root_scope = result(delimited(multispace0, root_scope, multispace0)(span))?;
    Ok(root_scope)
}

pub mod model {
    use crate::error::{MsgErr, ParseErrs};
    use crate::version::v0_0_1::config::config::bind::{Pipeline, PipelineStep, PipelineStop};
    use crate::version::v0_0_1::id::id::Version;
    use crate::version::v0_0_1::parse::error::result;
    use crate::version::v0_0_1::parse::{lex_child_scopes, pipeline, pipeline_step, pipeline_stop, Res};
    use crate::version::v0_0_1::Span;
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;
    use std::ops::{Deref, DerefMut};
    use std::str::FromStr;

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct ScopeSelectorAndFiltersDef<S, I> {
        pub selector: S,
        pub filters: ScopeFiltersDef<I>,
    }

    impl<S, I> ScopeSelectorAndFiltersDef<S, I> {
        pub fn new(selector: S, filters: ScopeFiltersDef<I>) -> Self {
            Self { selector, filters }
        }
    }

    pub enum ParsePhase {
        Root,
        SubScopes,
    }

    #[derive(Clone)]
    pub struct Spanned<I, E>
    where
        E: Clone,
        I: ToString,
    {
        pub span: I,
        pub element: E,
    }

    impl<I, E> Spanned<I, E>
    where
        E: Clone,
        I: ToString,
    {
        pub fn new(element: E, span: I) -> Spanned<I, E> {
            Self { span, element }
        }
    }

    impl<I, E> Spanned<I, E>
    where
        E: Clone + ToString,
        I: ToString,
    {
        pub fn len(&self) -> usize {
            self.element.to_string().len()
        }
    }

    impl<I, E> ToString for Spanned<I, E>
    where
        E: Clone + ToString,
        I: ToString,
    {
        fn to_string(&self) -> String {
            self.element.to_string()
        }
    }

    impl<I, E> Deref for Spanned<I, E>
    where
        E: Clone,
        I: ToString,
    {
        type Target = E;

        fn deref(&self) -> &Self::Target {
            &self.element
        }
    }

    impl<I, E> DerefMut for Spanned<I, E>
    where
        E: Clone,
        I: ToString,
    {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.element
        }
    }

    #[derive(Clone, Eq, PartialEq, Hash)]
    pub struct RootScopeSelector<I, V> {
        pub name: I,
        pub version: V,
    }

    impl<I, V> RootScopeSelector<I, V> {
        pub fn new(name: I, version: V) -> Self {
            RootScopeSelector { name, version }
        }
    }

    impl<I: ToString, V: ToString> RootScopeSelector<I, V> {
        pub fn to_concrete(self) -> Result<RootScopeSelector<String, Version>, MsgErr> {
            Ok(RootScopeSelector {
                name: self.name.to_string(),
                version: Version::from_str(self.version.to_string().as_str())?,
            })
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct ScopeSelectorDef<I> {
        pub name: I,
        pub filters: ScopeFiltersDef<I>,
    }

    impl<I> ScopeSelectorDef<I> {
        pub fn new(name: I, filters: ScopeFiltersDef<I>) -> Self {
            Self { name, filters }
        }
    }

    #[derive(Clone)]
    pub struct LexScopeSelector<I> {
        pub name: I,
        pub filters: ScopeFiltersDef<I>,
        pub children: Option<I>,
    }

    impl<I: ToString> LexScopeSelector<I> {
        pub fn new(name: I, children: Option<I>) -> Self {
            Self {
                name,
                filters: ScopeFiltersDef::empty(),
                children,
            }
        }
    }

    impl<I> LexScopeSelector<I> {
        pub fn has_children(&self) -> bool {
            self.children.is_some()
        }
    }

    impl<I: ToString> ScopeSelectorDef<I> {
        fn to_scope_selector(self) -> ScopeSelector {
            ScopeSelector {
                name: self.name.to_string(),
                filters: self.filters.to_scope_filters(),
            }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct ScopeFiltersDef<I> {
        pub path: Option<I>,
        pub filters: Vec<ScopeFilterDef<I>>,
    }

    impl<I: ToString> ScopeFiltersDef<I> {
        pub fn to_scope_filters(self) -> ScopeFilters {
            ScopeFilters {
                path: match self.path {
                    None => None,
                    Some(path) => Some(path.to_string()),
                },
                filters: self
                    .filters
                    .into_iter()
                    .map(|f| f.to_scope_filter())
                    .collect(),
            }
        }

        pub fn is_empty(&self) -> bool {
            self.path.is_none() && self.filters.is_empty()
        }

        pub fn len(&self) -> usize {
            self.filters.len()
        }

        pub fn empty() -> Self {
            Self {
                path: None,
                filters: vec![],
            }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct ScopeFilterDef<I> {
        pub name: I,
        pub args: Option<I>,
    }

    impl<I: ToString> ScopeFilterDef<I> {
        pub fn to_scope_filter(self) -> ScopeFilter {
            ScopeFilter {
                name: self.name.to_string(),
                args: match self.args {
                    None => None,
                    Some(args) => Some(args.to_string()),
                },
            }
        }
    }

    pub type ScopeFilter = ScopeFilterDef<String>;
    pub type ScopeFilters = ScopeFiltersDef<String>;
    pub type LexBlock<I> = Block<I, ()>;
    pub type LexRootScope<I> = Scope<RootScopeSelector<I, Spanned<I, Version>>, Block<I, ()>, I>;
    pub type LexScope<I> =
        Scope<ScopeSelectorAndFiltersDef<LexScopeSelector<I>, I>, Block<I, ()>, I>;
    pub type LexParentScope<'a> =
        Scope<LexScopeSelectorAndFilters<Span<'a>>, Vec<LexScope<Span<'a>>>, Span<'a>>;
    pub type LexPipelineSegment<I> = PipelineSegmentDef<I, Option<I>>;
    pub type LexPipeline<I> = PipelineDef<LexPipelineSegment<I>>;
    pub type LexPipelineScope<I> = PipelineScopeDef<I, LexPipeline<I>>;
    pub type PipelineSegment = PipelineSegmentDef<PipelineStep, PipelineStop>;
    pub type PipelineScope = PipelineScopeDef<String, Vec<MessageScope>>;
    pub type MessageScope = PipelineScopeDef<String, Vec<ActionScope>>;
    pub type ActionScope = PipelineScopeDef<String, Pipeline>;
    pub type ScopeSelector = ScopeSelectorDef<String>;
    pub type ScopeSelectorAndFilters = ScopeSelectorAndFiltersDef<ScopeSelector, String>;
    pub type LexScopeSelectorAndFilters<I> = ScopeSelectorAndFiltersDef<LexScopeSelector<I>, I>;
    //    pub type Pipeline = Vec<PipelineSegment>;


    impl <'a> TryFrom<LexParentScope<'a>> for PipelineScope {
        type Error = MsgErr;

        fn try_from(scope: LexParentScope) -> Result<Self, Self::Error> {
            let mut errs = vec![];
            let mut message_scopes = vec![];
            for message_scope in scope.block {
                match lex_child_scopes(message_scope) {
                    Ok(message_scope) => {
                        let mut block = vec![];
                        for action_scope in message_scope.block {
                            match result(pipeline(action_scope.block.content)) {
                                Ok(pipeline) => {
                                    block.push(ActionScope {
                                        selector: action_scope.selector.to_scope_selector(),
                                        block: pipeline
                                    })
                                }
                                Err(err) => {
                                    errs.push(err);
                                }
                            }
                        }
                        message_scopes.push(MessageScope {
                            selector: message_scope.selector.to_scope_selector(),
                            block
                        })
                    }
                    Err(err) => {
                        errs.push(err);
                    }
                }
            }
            if errs.is_empty() {
                Ok(PipelineScope{
                    selector: scope.selector.to_scope_selector(),
                    block: message_scopes
                })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }

    impl<I: ToString> LexScopeSelectorAndFilters<I> {
        pub fn to_scope_selector(self) -> ScopeSelectorAndFilters {
            ScopeSelectorAndFilters {
                selector: self.selector.to_scope_selector(),
                filters: self.filters.to_scope_filters(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PipelineSegmentDef<Step, Stop> {
        pub step: Step,
        pub stop: Stop,
    }

    #[derive(Clone)]
    pub struct PipelineDef<S> {
        pub segments: Vec<S>,
    }

    impl<S> Deref for PipelineDef<S> {
        type Target = Vec<S>;

        fn deref(&self) -> &Self::Target {
            &self.segments
        }
    }

    impl<S> DerefMut for PipelineDef<S> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.segments
        }
    }

    impl<'a> LexPipeline<Span<'a>> {
        pub fn to_pipeline(self) -> Result<Pipeline, MsgErr> {
            let mut pipeline = Pipeline::new();
            let mut errs = vec![];
            for segment in self.segments {
                match segment.to_pipeline_segment() {
                    Ok(segment) => {
                        pipeline.segments.push(segment);
                    }
                    Err(err) => {
                        errs.push(err);
                    }
                }
            }

            if errs.is_empty() {
                Ok(pipeline)
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }

    impl<'a> LexPipelineSegment<Span<'a>> {
        pub fn to_pipeline_segment(self) -> Result<PipelineSegment, MsgErr> {
            let mut errs = vec![];

            let step = match result(pipeline_step(self.step.clone())) {
                Ok(step) => Some(step),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };

            let stop = match &self.stop {
                Some(stop) => match result(pipeline_stop(stop.clone())) {
                    Ok(stop) => Some(stop),
                    Err(err) => {
                        errs.push(err);
                        None
                    }
                },
                None => {
                    errs.push(ParseErrs::new(
                        "expecting Pipeline Stop to follow Pipeline Step",
                        "Needs a following Pipeline Stop",
                        self.step.clone(),
                    ));
                    None
                }
            };

            if step.is_some() && stop.is_some() && errs.is_empty() {
                let step = step.expect("step");
                let stop = stop.expect("step");
                Ok(PipelineSegment { step, stop })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub enum BindScope {
        PipelineScope(PipelineScope),
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct PipelineScopeDef<I, P> {
        pub selector: ScopeSelectorAndFiltersDef<ScopeSelectorDef<I>, I>,
        pub block: P,
    }

    impl<I: ToString> LexScopeSelector<I> {
        pub fn to_scope_selector(self) -> ScopeSelector {
            ScopeSelector {
                name: self.name.to_string(),
                filters: self.filters.to_scope_filters(),
            }
        }
    }

    #[derive(Clone)]
    pub enum BindScopeKind {
        Pipelines,
    }

    #[derive(Clone)]
    pub enum BuiltInFilter {
        Auth,
        NoAuth,
    }

    #[derive(Clone)]
    pub struct Scope<S, B, P>
    where
        S: Clone,
    {
        pub selector: S,
        pub pipeline_step: Option<P>,
        pub block: B,
    }

    impl<S, B, P> Scope<S, B, P>
    where
        S: Clone,
    {
        pub fn new(selector: S, block: B) -> Self {
            Self {
                selector,
                block,
                pipeline_step: None,
            }
        }

        pub fn new_with_pipeline_step(selector: S, block: B, pipeline_step: Option<P>) -> Self {
            Self {
                selector,
                block,
                pipeline_step,
            }
        }
    }

    impl<S, FromBlock, P> Scope<S, FromBlock, P>
    where
        S: Clone,
    {
        pub fn upgrade<ToBlock>(self, block: ToBlock) -> Scope<S, ToBlock, P> {
            Scope {
                selector: self.selector,
                block,
                pipeline_step: self.pipeline_step,
            }
        }
    }

    #[derive(Clone)]
    pub struct Block<I, D> {
        pub kind: BlockKind,
        pub content: I,
        pub data: D,
    }

    impl<I> Block<I, ()> {
        pub fn parse(kind: BlockKind, content: I) -> Block<I, ()> {
            Block {
                kind,
                content,
                data: (),
            }
        }
    }

    #[derive(Debug, Copy, Clone, strum_macros::Display, Eq, PartialEq)]
    pub enum BlockKind {
        Nested(NestedBlockKind),
        Terminated(TerminatedBlockKind),
        Delimited(DelimitedBlockKind),
        Partial,
    }

    #[derive(Debug, Copy, Clone, strum_macros::Display, Eq, PartialEq)]
    pub enum TerminatedBlockKind {
        Semicolon,
    }

    impl TerminatedBlockKind {
        pub fn tag(&self) -> &'static str {
            match self {
                TerminatedBlockKind::Semicolon => ";",
            }
        }

        pub fn as_char(&self) -> char {
            match self {
                TerminatedBlockKind::Semicolon => ';',
            }
        }
    }

    #[derive(
        Debug, Copy, Clone, strum_macros::Display, strum_macros::EnumString, Eq, PartialEq,
    )]
    pub enum DelimitedBlockKind {
        SingleQuotes,
        DoubleQuotes,
    }

    impl DelimitedBlockKind {
        pub fn delim(&self) -> &'static str {
            match self {
                DelimitedBlockKind::SingleQuotes => "'",
                DelimitedBlockKind::DoubleQuotes => "\"",
            }
        }

        pub fn escaped(&self) -> &'static str {
            match self {
                DelimitedBlockKind::SingleQuotes => "\'",
                DelimitedBlockKind::DoubleQuotes => "\"",
            }
        }

        pub fn context(&self) -> &'static str {
            match self {
                DelimitedBlockKind::SingleQuotes => "single:quotes:block",
                DelimitedBlockKind::DoubleQuotes => "double:quotes:block",
            }
        }

        pub fn missing_close_context(&self) -> &'static str {
            match self {
                DelimitedBlockKind::SingleQuotes => "single:quotes:block:missing-close",
                DelimitedBlockKind::DoubleQuotes => "double:quotes:block:missing-close",
            }
        }
    }

    #[derive(
        Debug, Copy, Clone, strum_macros::Display, strum_macros::EnumString, Eq, PartialEq,
    )]
    pub enum NestedBlockKind {
        Curly,
        Parens,
        Square,
        Angle,
    }

    impl NestedBlockKind {
        pub fn is_block_terminator(c: char) -> bool {
            match c {
                '}' => true,
                ')' => true,
                ']' => true,
                '>' => true,
                _ => false,
            }
        }

        pub fn error_message(span: &Span, context: &str) -> Result<&'static str, ()> {
            if Self::Curly.open_context() == context {
                Ok("expecting '{' (open scope block)")
            } else if Self::Parens.open_context() == context {
                Ok("expecting '(' (open scope block)")
            } else if Self::Angle.open_context() == context {
                Ok("expecting '<' (open scope block)")
            } else if Self::Square.open_context() == context {
                Ok("expecting '[' (open scope block)")
            } else if Self::Curly.close_context() == context {
                Ok("expecting '}' (close scope block)")
            } else if Self::Parens.close_context() == context {
                Ok("expecting ')' (close scope block)")
            } else if Self::Angle.close_context() == context {
                Ok("expecting '>' (close scope block)")
            } else if Self::Square.close_context() == context {
                Ok("expecting ']' (close scope block)")
            } else if Self::Curly.unpaired_closing_scope() == context {
                Ok("closing scope without an opening scope")
            } else if Self::Parens.unpaired_closing_scope() == context {
                Ok("closing scope without an opening scope")
            } else if Self::Angle.unpaired_closing_scope() == context {
                Ok("closing scope without an opening scope")
            } else if Self::Square.unpaired_closing_scope() == context {
                Ok("closing scope without an opening scope")
            } else {
                Err(())
            }
        }

        pub fn context(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "block:{}",
                NestedBlockKind::Parens => "block:()",
                NestedBlockKind::Square => "block:[]",
                NestedBlockKind::Angle => "block:<>",
            }
        }

        pub fn open_context(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "block:open:{",
                NestedBlockKind::Parens => "block:open:(",
                NestedBlockKind::Square => "block:open:[",
                NestedBlockKind::Angle => "block:open:<",
            }
        }

        pub fn close_context(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "block:close:}",
                NestedBlockKind::Parens => "block:close:)",
                NestedBlockKind::Square => "block:close:]",
                NestedBlockKind::Angle => "block:close:>",
            }
        }

        pub fn unpaired_closing_scope(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "block:close-before-open:}",
                NestedBlockKind::Parens => "block:close-before-open:)",
                NestedBlockKind::Square => "block:close-before-open:]",
                NestedBlockKind::Angle => "block:close-before-open:>",
            }
        }

        pub fn open(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "{",
                NestedBlockKind::Parens => "(",
                NestedBlockKind::Square => "[",
                NestedBlockKind::Angle => "<",
            }
        }

        pub fn close(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "}",
                NestedBlockKind::Parens => ")",
                NestedBlockKind::Square => "]",
                NestedBlockKind::Angle => ">",
            }
        }

        pub fn open_as_char(&self) -> char {
            match self {
                NestedBlockKind::Curly => '{',
                NestedBlockKind::Parens => '(',
                NestedBlockKind::Square => '[',
                NestedBlockKind::Angle => '<',
            }
        }

        pub fn close_as_char(&self) -> char {
            match self {
                NestedBlockKind::Curly => '}',
                NestedBlockKind::Parens => ')',
                NestedBlockKind::Square => ']',
                NestedBlockKind::Angle => '>',
            }
        }
    }

    pub enum TextType<I> {
        Comment(I),
        NoComment(I),
    }

    impl<I: ToString> ToString for TextType<I> {
        fn to_string(&self) -> String {
            match self {
                TextType::Comment(i) => i.to_string(),
                TextType::NoComment(i) => i.to_string(),
            }
        }
    }
}

pub mod error {
    use crate::error::{MsgErr, ParseErrs};
    use crate::version::v0_0_1::parse::model::NestedBlockKind;
    use crate::version::v0_0_1::Span;
    use ariadne::Report;
    use ariadne::{Label, ReportKind, Source};
    use nom::{Err, Slice};
    use nom_supreme::error::{BaseErrorKind, ErrorTree, StackContext};

    pub fn result<R>(result: Result<(Span, R), Err<ErrorTree<Span>>>) -> Result<R, MsgErr> {
        match result {
            Ok((_, e)) => Ok(e),
            Err(err) => Err(find_parse_err(&err)),
        }
    }

    /*
    pub fn just_msg<R, E: From<String>>(
        result: Result<(Span, R), Err<ErrorTree<Span>>>,
    ) -> Result<R, E> {
        match result {
            Ok((_, e)) => Ok(e),
            Err(err) => match find(&err) {
                Ok((message, _)) => Err(E::from(message)),
                Err(err) => Err(E::from(err)),
            },
        }
    }

     */

    fn create_err_report(context: &str, loc: Span) -> MsgErr {
        let mut builder = Report::build(ReportKind::Error, (), 23);

        match NestedBlockKind::error_message(&loc, context) {
            Ok(message) => {
                let builder = builder.with_message(message).with_label(
                    Label::new(loc.location_offset()..loc.location_offset()).with_message(message),
                );
                return ParseErrs::from_report(builder.finish(), loc.extra).into();
            }
            Err(_) => {}
        }

        let builder = match context {
            "capture-path" => {
                builder.with_message("Invalid capture path. Legal characters are filesystem characters plus captures $(var=.*) i.e. /users/$(user=.*)").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal capture path"))

            }
                "point" => {
                    builder.with_message("Invalid Point").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Point"))
                },

            "parsed-scopes" => { builder.with_message("expecting a properly formed scope").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("not a scope"))},
            "scope" => { builder.with_message("expecting a properly formed scope").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("not a scope"))},
            "root-scope:block" => { builder.with_message("expecting root scope block {}").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Scope Block"))},
            "pipeline:stop:expecting" =>{ builder.with_message("expecting a pipeline stop: point, call, or return ('&')").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Pipeline Stop"))},
            "pipeline:step" =>{ builder.with_message("expecting a pipeline step ('->', '=>', '-[ Bin ]->', etc...)").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Pipeline Step"))},
            "pipeline:step:entry" =>{ builder.with_message("expecting a pipeline step entry ('-' or '=') to form a pipeline step i.e. '->' or '=>'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Pipeline Entry"))},
            "pipeline:step:exit" =>{ builder.with_message("expecting a pipeline step exit i.e. '->' or '=>'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Pipeline Exit"))},
            "pipeline:step:payload" =>{ builder.with_message("Invalid payload filter").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("invalid payload filter"))},
            "scope:expect-space-after-pipeline-step" =>{ builder.with_message("expecting a space after selection pipeline step (->)").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Space"))},
            "scope-selector-name:expect-alphanumeric-leading" => { builder.with_message("expecting a valid scope selector name starting with an alphabetic character").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Alpha Char"))},
            "scope-selector-name:expect-termination" => { builder.with_message("expecting scope selector to be followed by a space, a filter declaration: '(filter)->' or a sub scope selector: '<SubScope> or subscope terminator '>' '").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Bad Scope Selector Termination"))},
                "scope-selector-version-closing-tag" =>{ builder.with_message("expecting a closing parenthesis for the root version declaration (no spaces allowed) -> i.e. Bind(version=1.0.0)->").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("missing closing parenthesis"))}
                "scope-selector-version-missing-kazing"=> { builder.with_message("The version declaration needs a little style.  Try adding a '->' to it.  Make sure there are no spaces between the parenthesis and the -> i.e. Bind(version=1.0.0)->").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("missing stylish arrow"))}
                "scope-selector-version" => { builder.with_message("Root config selector requires a version declaration with NO SPACES between the name and the version filter example: Bind(version=1.0.0)->").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("bad version declaration"))}
                "scope-selector-name" => { builder.with_message("Expecting an alphanumeric scope selector name. example: Pipeline").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("expecting scope selector"))}
                "root-scope-selector-name" => { builder.with_message("Expecting an alphanumeric root scope selector name and version. example: Bind(version=1.0.0)->").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("expecting scope selector"))}
                "consume" => { builder.with_message("Expected to be able to consume the entire String")}
                "point:space_segment:dot_dupes" => { builder.with_message("Space Segment cannot have consecutive dots i.e. '..'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Consecutive dots not allowed"))}
                "point:version:root_not_trailing" =>{ builder.with_message("Root filesystem is the only segment allowed to follow a bundle version i.e. 'space:base:2.0.0-version:/dir/somefile.txt'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Only root file segment ':/' allowed here"))}
                "point:space_segment_leading" => {builder.with_message("The leading character of a Space segment must be a lowercase letter").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Leading Character"))}
                "point:space_segment" => {builder.with_message("A Point Space Segment must be all lowercase, alphanumeric with dashes and dots.  It follows Host and Domain name rules i.e. 'localhost', 'mechtron.io'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Space Segment"))}
                "point:bad_leading" => {builder.with_message("The leading character must be a lowercase letter (for Base Segments) or a digit (for Version Segments)").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Leading Character"))}
                "point:base_segment" => {builder.with_message("A Point Base Segment must be 'skewer-case': all lowercase alphanumeric with dashes. The leading character must be a letter.").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Base Segment Character"))}
                "point:dir_pop" => {builder.with_message("A Point Directory Pop '..'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Something is Wrong"))}
                "point:dir_segment" => {builder.with_message("A Point Dir Segment follows legal filesystem characters and must end in a '/'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:root_filesystem_segment" => {builder.with_message("Root FileSystem ':/'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:file_segment" => {builder.with_message("A Point File Segment follows legal filesystem characters").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:file_or_directory"=> {builder.with_message("A Point File Segment (Files & Directories) follows legal filesystem characters").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:version_segment" => {builder.with_message("A Version Segment allows all legal SemVer characters").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "filter-name" => {builder.with_message("Filter name must be skewer case with leading character").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid filter name"))}

                "parsed-scope-selector-kazing" => {builder.with_message("Selector needs some style with the '->' operator either right after the Selector i.e.: 'Pipeline ->' or as part of the filter declaration i.e. 'Pipeline(auth)->'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Missing or Invalid Kazing Operator( -> )"))}
            "variable" => {
                    builder.with_message("variable name must be alphanumeric lowercase, dashes and dots.  Variables are preceded by the '$' operator and must be sorounded by parenthesis $(env.valid-variable-name)")
                },
                "child_perms" => {
                    builder.with_message("expecting child permissions form csd (Create, Select, Delete) uppercase indicates set permission (CSD==full permission, csd==no permission)")
                },
                "particle_perms" => {
                    builder.with_message("expecting particle permissions form rwx (Read, Write, Execute) uppercase indicates set permission (RWX==full permission, rwx==no permission)")
                },
                "permissions" => {
                    builder.with_message("expecting permissions form 'csd-rwx' (Create,Select,Delete)-(Read,Write,Execute) uppercase indicates set permission (CSD-RWX==full permission, csd-rwx==no permission)")
                }
                "permissions_mask" => {
                    builder.with_message("expecting permissions mask symbol '+' for 'Or' mask and '&' for 'And' mask. Example:  &csd-RwX removes ----R-X from current permission")
                }
                "privilege" => {
                    builder.with_message("privilege name must be '*' for 'full' privileges or an alphanumeric lowercase, dashes and colons i.e. 'props:email:read'")
                },
                "access_grant:perm" => {
                    builder.with_message("expecting permissions mask symbol '+' for 'Or' mask and '&' for 'And' mask. Example:  &csd-RwX removes ----R-X from current permission")
                },
                "access_grant:priv" => {
                    builder.with_message("privilege name must be '*' for 'full' privileges or an alphanumeric lowercase, dashes and colons i.e. 'props:email:read'")
                },
                "access_grant:on" => {
                    builder.with_message("expecting grant 'on' i.e.: 'grant perm +cSd+RwX on localhost:app:** to localhost:app:users:**<User>'")
                },
                "access_grant:to" => {
                    builder.with_message("expecting grant 'to' i.e.: 'grant perm +cSd+RwX on localhost:app:** to localhost:app:users:**<User>'")
                },
                "point-subst-brute-force" => {
                    builder.with_message("not expecting variables or working point context '.'/'..' in this point")
                },
                "access_grant_kind" => {
                    builder.with_message("expecting access grant kind ['super','perm','priv']")
                },

                what => {
                    builder.with_message(format!("internal parser error: cannot determine an error message for parse context: {}",what))
                }
            };

        //            let source = String::from_utf8(loc.get_line_beginning().to_vec() ).unwrap_or("could not parse utf8 of original source".to_string() );
        ParseErrs::from_report(builder.finish(), loc.extra).into()
    }

    pub fn find_parse_err(err: &Err<ErrorTree<Span>>) -> MsgErr {
        match err {
            Err::Incomplete(_) => "internal parser error: Incomplete".into(),
            Err::Error(err) => find_tree(err),
            Err::Failure(err) => find_tree(err),
        }
    }

    pub enum ErrFind {
        Context(String),
        Message(String),
    }

    pub fn find_tree(err: &ErrorTree<Span>) -> MsgErr {
        match err {
            ErrorTree::Stack { base, contexts } => {
                let (span, context) = contexts.first().unwrap();
                match context {
                        StackContext::Context(context) => {
                            create_err_report(*context, span.clone())
                        }
                        _ => "internal parser error: could not find a parse context in order to generate a useful error message".into()
                    }
            }
            ErrorTree::Base { location, kind } => create_err_report("eof", location.clone()),
            ErrorTree::Alt(alts) => {
                for alt in alts {
                    return find_tree(alt);
                }

                "internal parser error: ErrorTree::Alt could not find a suitable context error in the various alts".into()
            }
        }
    }

    pub fn first_context<I>(orig: Err<ErrorTree<I>>) -> Result<(String, Err<ErrorTree<I>>), ()> {
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
        camel_case, diagnose, domain_chars, file_chars, point, point_segment_chars, point_subst,
        rec_version, skewer_chars, version_chars, version_point_segment, version_req_chars, Res,
    };
    use crate::version::v0_0_1::selector::selector::specific::{
        ProductSelector, VariantSelector, VendorSelector,
    };
    use crate::version::v0_0_1::selector::selector::{
        ExactPointSeg, GenericKindSelector, GenericKindiBaseSelector, Hop, Pattern,
        PointSegSelector, PointSelector, SpecificSelector, TksPattern, VersionReq,
    };
    use crate::version::v0_0_1::util::ValuePattern;
    use crate::version::v0_0_1::{create_span, Span};
    use nom_supreme::error::ErrorTree;
    use nom_supreme::{parse_from_str, ParserExt};

    fn inclusive_any_segment(input: Span) -> Res<Span, PointSegSelector> {
        alt((tag("+*"), tag("ROOT+*")))(input)
            .map(|(next, _)| (next, PointSegSelector::InclusiveAny))
    }

    fn inclusive_recursive_segment(input: Span) -> Res<Span, PointSegSelector> {
        alt((tag("+**"), tag("ROOT+**")))(input)
            .map(|(next, _)| (next, PointSegSelector::InclusiveRecursive))
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
        version_req(input).map(|(next, version_req)| (next, PointSegSelector::Version(version_req)))
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

    pub fn version_req(input: Span) -> Res<Span, VersionReq> {
        let (next, version) = version_req_chars(input.clone())?;
        let str_input = *version.fragment();
        let rtn = semver::VersionReq::parse(str_input);

        match rtn {
            Ok(version) => Ok((next, VersionReq { version })),
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
        tuple((point, kind))(input).map(|(next, (point, kind))| (next, PointKind { point, kind }))
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
        let (next, version) = rec_version(input.clone())?;
        let str_input = *version.fragment();
        let rtn = semver::Version::parse(str_input);

        match rtn {
            Ok(version) => Ok((next, Version { version })),
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

pub fn args<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '"')
                && !(char_item == '_')
                && !(char_item == '{')
                && !(char_item == '}')
                && !(char_item == '(')
                && !(char_item == ')')
                && !(char_item == '[')
                && !(char_item == ']')
                && !(char_item == ' ')
                && !(char_item == '\n')
                && !(char_item == '\t')
                && !(char_item == '\r')
                && !(char_item == '\'')
                && !((char_item.is_alphanumeric()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
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
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
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
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn not_quote<T>(i: T) -> Res<T, T>
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

pub fn filename<T>(i: T) -> Res<T, T>
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

pub fn msg_call(input: Span) -> Res<Span, CallKind> {
    tuple((
        delimited(tag("Msg<"), alphanumeric1, tag(">")),
        opt(recognize(capture_path)),
    ))(input)
    .map(|(next, (action, path))| {
        let path = match path {
            None => create_span("/"),
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

pub fn digit_range(input: Span) -> Res<Span, NumRange> {
    tuple((digit1, tag("-"), digit1))(input).map(|(next, (min, _, max))| {
        let min: usize = usize::from_str(min.to_string().as_str()).expect("usize");
        let max: usize = usize::from_str(max.to_string().as_str()).expect("usize");
        let range = NumRange::MinMax { min, max };

        (next, range)
    })
}

pub fn exact_range(input: Span) -> Res<Span, NumRange> {
    digit1(input).map(|(next, exact)| {
        (
            next,
            NumRange::Exact(
                usize::from_str(exact.to_string().as_str())
                    .expect("expect to be able to change digit string into usize"),
            ),
        )
    })
}

pub fn range(input: Span) -> Res<Span, NumRange> {
    delimited(
        multispace0,
        opt(alt((digit_range, exact_range))),
        multispace0,
    )(input)
    .map(|(next, range)| {
        let range = match range {
            Some(range) => range,
            None => NumRange::Any,
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
    delimited(multispace0, tag("*"), multispace0)(input).map(|(next, _)| (next, ValuePattern::Any))
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
    delimited(tag("["), map_entry_patterns, tag("]"))(input).map(|(next, params)| (next, params))
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

pub fn value_constrained_map_pattern(input: Span) -> Res<Span, ValuePattern<MapPattern>> {
    value_pattern(map_pattern)(input)
}

pub fn msg_action(input: Span) -> Res<Span, ValuePattern<StringMatcher>> {
    value_pattern(camel_case_to_string_matcher)(input)
}

pub fn msg_pattern_scoped(input: Span) -> Res<Span, MsgPipelineSelector> {
    tuple((delimited(tag("<"), msg_action, tag(">")), opt(path_regex)))(input).map(
        |(next, (action, path_regex))| {
            let path_regex = match path_regex {
                None => "*".to_string(),
                Some(path_regex) => path_regex.to_string(),
            };
            let rtn = MsgPipelineSelector {
                action,
                path_regex: path_regex.to_string(),
            };
            (next, rtn)
        },
    )
}

pub fn msg_pattern(input: Span) -> Res<Span, MsgPipelineSelector> {
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
        let rtn = MsgPipelineSelector {
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

pub fn http_pattern_scoped(input: Span) -> Res<Span, HttpPipelineSelector> {
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
        let rtn = HttpPipelineSelector {
            method,
            path_regex: path_regex.to_string(),
        };
        (next, rtn)
    })
}

pub fn http_pattern(input: Span) -> Res<Span, HttpPipelineSelector> {
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
        let rtn = HttpPipelineSelector {
            method,
            path_regex: path_regex.to_string(),
        };
        (next, rtn)
    })
}

pub fn rc_command_type(input: Span) -> Res<Span, RcCommandType> {
    parse_alpha1_str(input)
}

pub fn rc_pattern_scoped(input: Span) -> Res<Span, RcPipelineSelector> {
    pattern(delimited(tag("<"), rc_command_type, tag(">")))(input)
        .map(|(next, command)| (next, RcPipelineSelector { command }))
}

pub fn rc_pattern(input: Span) -> Res<Span, RcPipelineSelector> {
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

pub fn msg_entity_pattern(input: Span) -> Res<Span, PipelineSelector> {
    msg_pattern(input).map(|(next, pattern)| (next, PipelineSelector::Msg(pattern)))
}

pub fn http_entity_pattern(input: Span) -> Res<Span, PipelineSelector> {
    http_pattern(input).map(|(next, pattern)| (next, PipelineSelector::Http(pattern)))
}

pub fn rc_entity_pattern(input: Span) -> Res<Span, PipelineSelector> {
    rc_pattern(input).map(|(next, pattern)| (next, PipelineSelector::Rc(pattern)))
}

pub fn entity_pattern(input: Span) -> Res<Span, PipelineSelector> {
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

/*
pub fn text_payload_block(input: Span) -> Res<Span, PayloadBlock> {
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
            PayloadBlock::CreatePayload(Payload::Text(text.to_string())),
        )
    })
}*/

pub fn upload_payload_block(input: Span) -> Res<Span, UploadBlock> {
    delimited(multispace0, file_chars, multispace0)(input).map(|(next, filename)| {
        (
            next,
            UploadBlock {
                name: filename.to_string(),
            },
        )
    })
}

pub fn upload_step(input: Span) -> Res<Span, UploadBlock> {
    delimited(tag("^["), upload_payload_block, tag("->"))(input)
}

pub fn request_payload_filter_block(input: Span) -> Res<Span, PayloadBlock> {
    tuple((
        multispace0,
        alt((
            payload_filter_block_any,
            payload_filter_block_def,
            payload_filter_block_empty,
        )),
        multispace0,
    ))(input)
    .map(|(next, (_, block, _))| (next, PayloadBlock::RequestPattern(block)))
}

pub fn response_payload_filter_block(input: Span) -> Res<Span, PayloadBlock> {
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
    .map(|(next, (_, block, _))| (next, PayloadBlock::ResponsePattern(block)))
}

pub fn rough_pipeline_step(input: Span) -> Res<Span, Span> {
    recognize(tuple((
        many0(preceded(alt((tag("-"), tag("="), tag("+"))), any_lex_block)),
        alt((tag("->"), tag("=>"))),
    )))(input)
}

pub fn consume_pipeline_block(input: Span) -> Res<Span, PayloadBlock> {
    all_consuming(request_payload_filter_block)(input)
}

/*
pub fn remove_comments_from_span( span: Span )-> Res<Span,Span> {
    let (next,no_comments) = remove_comments(span.clone())?;
    let new = LocatedSpan::new_extra(no_comments.as_str(), span.extra.clone() );
    Ok((next,new))
}
 */

pub fn strip_comments<I>(input: I) -> Res<I, String>
where
    I: InputTakeAtPosition + nom::InputLength + Clone + ToString,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    many0(alt((no_comment, comment)))(input).map(|(next, texts)| {
        let mut rtn = String::new();
        for t in texts {
            match t {
                TextType::NoComment(span) => {
                    rtn.push_str(span.to_string().as_str());
                }
                TextType::Comment(span) => {
                    for i in 0..span.input_len() {
                        // replace with whitespace
                        rtn.push_str(" ");
                    }
                }
            }
        }

        // create with the new string, but use old string as reference
        //let span = LocatedSpan::new_extra(rtn.as_str(), input.extra.clone() );
        (next, rtn)
    })
}

/*
pub fn strip(input: Span) -> Result<Span, MsgErr>
{
    let (_, stripped) = strip_comments(input.clone())?;
    let span = LocatedSpan::new_extra(stripped.as_str().clone(), Arc::new(input.to_string()));
    Ok(span)
}

 */

pub fn no_comment<T>(i: T) -> Res<T, TextType<T>>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item == '#'
        },
        ErrorKind::AlphaNumeric,
    )
    .map(|(next, comment)| (next, TextType::NoComment(comment)))
}

pub fn comment<T>(i: T) -> Res<T, TextType<T>>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item == '\n'
        },
        ErrorKind::AlphaNumeric,
    )
    .map(|(next, comment)| (next, TextType::Comment(comment)))
}

pub fn config(src: &str) -> Result<Config, MsgErr> {
    let (next, stripped) = strip_comments(src)?;
    let span = LocatedSpan::new_extra(stripped.as_str(), Arc::new(src.to_string()));
    let lex_root_scope = lex_root_scope(span.clone())?;
    let root_scope_selector = lex_root_scope.selector.clone().to_concrete()?;
    if root_scope_selector.name.as_str() == "Bind" {
        if root_scope_selector.version == Version::from_str("1.0.0")? {
            let bind = bind_config(lex_root_scope.block.content.clone())?;
            return Ok(Config::Bind(bind));
        } else {
            let message = format!(
                "ConfigParser does not know how to process a Bind at version '{}'",
                root_scope_selector.version.to_string()
            );
            let mut builder = Report::build(ReportKind::Error, (), 0);
            let report = builder
                .with_message(message)
                .with_label(
                    Label::new(
                        lex_root_scope.selector.version.span.location_offset()
                            ..lex_root_scope.selector.version.span.location_offset()
                                + lex_root_scope.selector.version.span.len(),
                    )
                    .with_message("Unsupported Bind Config Version"),
                )
                .finish();
            Err(ParseErrs::from_report(report, lex_root_scope.block.content.extra.clone()).into())
        }
    } else {
        let message = format!(
            "ConfigParser does not know how to process a '{}'",
            lex_root_scope.selector.name,
        );
        let mut builder = Report::build(ReportKind::Error, (), 0);
        let report = builder
            .with_message(message)
            .with_label(
                Label::new(
                    lex_root_scope.selector.name.location_offset()
                        ..lex_root_scope.selector.name.location_offset()
                            + lex_root_scope.selector.name.len(),
                )
                .with_message("Unrecognized Config Kind"),
            )
            .finish();
        Err(ParseErrs::from_report(report, lex_root_scope.block.content.extra.clone()).into())
    }
}

fn bind_config(input: Span) -> Result<BindConfig, MsgErr> {
    let lex_scopes = lex_scopes(input)?;
    let mut scopes = vec![];
    let mut errors = vec![];

    for lex_scope in lex_scopes {
        match sematic_bind_scope(lex_scope) {
            Ok(scope) => {
                scopes.push(scope);
            }
            Err(err) => errors.push(err),
        }
    }

    if !errors.is_empty() {
        let errors = ParseErrs::fold(errors);
        return Err(errors.into());
    }

    let mut config = BindConfig::new(scopes);
    Ok(config)
}

fn sematic_bind_scope(scope: LexScope<Span>) -> Result<BindScope, MsgErr> {
    let selector_name = scope.selector.selector.name.to_string();
    match selector_name.as_str() {
        "Pipeline" => {
            let scope = lex_child_scopes(scope)?;
            let scope = PipelineScope::try_from(scope)?;
            Ok(BindScope::PipelineScope(scope))
        }
        what => {
            let mut builder = Report::build(ReportKind::Error, (), 0);
            let report = builder
                .with_message(format!(
                    "Unrecognized BindConfig selector: '{}'",
                    scope.selector.selector.name
                ))
                .with_label(
                    Label::new(
                        scope.selector.selector.name.location_offset()
                            ..scope.selector.selector.name.location_offset()
                                + scope.selector.selector.name.len(),
                    )
                    .with_message("Unrecognized Selector"),
                )
                .finish();
            Err(ParseErrs::from_report(report, scope.block.content.extra.clone()).into())
        }
    }
}

fn parse_bind_pipelines_scope<'a>(input: Span) -> Result<Spanned<Span, BindScopeKind>, ParseErrs> {
    unimplemented!()
    /*
    let (next, lex_scopes) = lex_scopes(input.clone())?;
    let mut errs = vec![];
    for lex_scope in lex_scopes {
        match lex_scope.selector.name.to_string().as_str() {
            "Msg" => {}
            "Http" => {}
            "Rc" => {}
            what => {
                let mut builder = Report::build(ReportKind::Error, (), 0);
                let report = builder
                    .with_message(format!("Unrecognized Pipeline scope: '{}'", what))
                    .with_label(
                        Label::new(input.location_offset()..input.location_offset())
                            .with_message("Unrecognized Selector"),
                    )
                    .finish();
                errs.push(ParseErrs::new(report, input.extra.clone()));
            }
        }
    }

    if !errs.is_empty() {
        Err(ParseErrs::fold(errs))
    } else {
        Ok(ElemSpan::new(BindBlock::Pipelines, input.clone()))
    }

     */
}

pub fn nospace0(input: Span) -> Res<Span, Span> {
    recognize(many0(satisfy(|c| !c.is_whitespace())))(input)
}

pub fn nospace1(input: Span) -> Res<Span, Span> {
    recognize(pair(
        satisfy(|c| !c.is_whitespace()),
        many0(satisfy(|c| !c.is_whitespace())),
    ))(input)
}

pub fn no_space_with_blocks(input: Span) -> Res<Span, Span> {
    recognize(many1(alt((recognize(any_block), nospace1))))(input)
}

pub fn lex_pipeline(input: Span) -> Res<Span, LexPipeline<Span>> {
    many1(lex_pipeline_segment)(input).map(|(next, segments)| {
        let pipeline = LexPipeline { segments };
        (next, pipeline)
    })
}

pub fn lex_pipeline_segment(input: Span) -> Res<Span, LexPipelineSegment<Span>> {
    tuple((
        multispace0,
        no_space_with_blocks,
        multispace1,
        opt(no_space_with_blocks),
    ))(input)
    .map(|(next, (_, step, _, stop))| {
        let segment = LexPipelineSegment { step, stop };

        (next, segment)
    })
}

#[cfg(test)]
pub mod test {
    use crate::error::{MsgErr, ParseErrs};
    use crate::version::v0_0_1::config::config::Config;
    use crate::version::v0_0_1::parse::error::result;
    use crate::version::v0_0_1::parse::model::{BlockKind, DelimitedBlockKind, LexScope, NestedBlockKind, TerminatedBlockKind};
    use crate::version::v0_0_1::parse::parse::version;
    use crate::version::v0_0_1::parse::{args, bind_config, comment, config, expected_block_terminator_or_non_terminator, lex_block, lex_child_scopes, lex_nested_block, lex_scope, lex_scope_pipeline_step_and_block, lex_scope_selector, lex_scope_selector_and_filters, lex_scopes, mesh_eos, nested_block, nested_block_content, next_selector, no_comment, parse_include_blocks, parse_inner_block, pipeline, pipeline_segment, pipeline_step, pipeline_stop, point, point_subst, rec_version, root_scope, root_scope_selector, rough_filepath_chars_plus_capture, scope_filter, scope_filters, skewer_case, space_chars, space_no_dupe_dots, space_point_segment, strip_comments, wrapper};
    use crate::version::v0_0_1::{create_span, Span};
    use nom::bytes::complete::{escaped, tag};
    use nom::character::complete::{alpha1, anychar, multispace0};
    use nom::character::is_alphanumeric;
    use nom::combinator::{all_consuming, opt, peek, recognize};
    use nom::error::context;
    use nom::multi::many0;
    use nom::sequence::{delimited, pair, tuple};
    use nom::IResult;
    use nom_locate::LocatedSpan;
    use nom_supreme::error::ErrorTree;
    use std::rc::Rc;
    use std::sync::Arc;

    #[test]
    pub fn test_lex_block() -> Result<(), MsgErr> {
        let esc = result(escaped(anychar, '\\', anychar)(create_span("\\}")))?;
        //println!("esc: {}", esc);
        log(result(all_consuming(lex_block(BlockKind::Nested(
            NestedBlockKind::Curly,
        )))(create_span("{}"))))?;
        log(result(all_consuming(lex_block(BlockKind::Nested(
            NestedBlockKind::Curly,
        )))(create_span("{x}"))))?;
        log(result(all_consuming(lex_block(BlockKind::Nested(
            NestedBlockKind::Curly,
        )))(create_span("{\\}}"))))?;
        log(result(all_consuming(lex_block(BlockKind::Delimited(
            DelimitedBlockKind::SingleQuotes,
        )))(create_span("'hello'"))))?;
        log(result(all_consuming(lex_block(BlockKind::Delimited(
            DelimitedBlockKind::SingleQuotes,
        )))(create_span("'ain\\'t it cool?'"))))?;

        //assert!(log(result(all_consuming(lex_block( BlockKind::Nested(NestedBlockKind::Curly)))(create_span("{ }}")))).is_err());
        Ok(())
    }

    #[test]
    pub fn test_bind_config() -> Result<(), MsgErr> {
        let bind_config_str = r#"Bind(version=1.0.0)  { Pipeline<Rc> -> { <Create> -> localhost:app => &; } }
        "#;

        if let Config::Bind(bind)= log(config(bind_config_str))? {
            assert_eq!(bind.pipelines().len(),1);
            let mut pipelines = bind.pipelines();
            let pipeline_scope = pipelines.pop().unwrap();
            assert_eq!(pipeline_scope.selector.selector.name.as_str(), "Pipeline");
            let message_scope = pipeline_scope.block.first().unwrap();
            assert_eq!(message_scope.selector.selector.name.as_str(), "Rc");
            let action_scope = message_scope.block.first().unwrap();
            assert_eq!(action_scope.selector.selector.name.as_str(), "Create");


        } else {
            assert!(false);
        }
        let bind_config_str = r#"Bind(version=1.0.0)  {
           Pipeline<Rc<Create>> -> localhost:app => &;
        }"#;

        if let Config::Bind(bind)= log(config(bind_config_str))? {
            assert_eq!(bind.pipelines().len(),1);
            let mut pipelines = bind.pipelines();
            let pipeline_scope = pipelines.pop().unwrap();
            assert_eq!(pipeline_scope.selector.selector.name.as_str(), "Pipeline");
            let message_scope = pipeline_scope.block.first().unwrap();
            assert_eq!(message_scope.selector.selector.name.as_str(), "Rc");
            let action_scope = message_scope.block.first().unwrap();
            assert_eq!(action_scope.selector.selector.name.as_str(), "Create");


        } else {
            assert!(false);
        }
        let bind_config_str = r#"  Bind(version=1.0.0) {
           Pipeline<Rc> -> {
             Create ; Bok;
               }
        }

        "#;
        assert!(log(config(bind_config_str)).is_err());
     //   assert!(log(config(bind_config_str)).is_err());
        Ok(())
    }

    #[test]
    pub fn test_pipeline_segment() -> Result<(), MsgErr> {
        log(result(pipeline_segment(create_span("-> localhost"))))?;
        assert!(log(result(pipeline_segment(create_span("->")))).is_err());
        assert!(log(result(pipeline_segment(create_span("localhost")))).is_err());
        Ok(())
    }

    #[test]
    pub fn test_pipeline_stop() -> Result<(), MsgErr> {
        log(result(space_chars(create_span("localhost"))))?;
        log(result(space_no_dupe_dots(create_span("localhost"))))?;

        log(result(mesh_eos(create_span(""))))?;
        log(result(mesh_eos(create_span(":"))))?;

        log(result(recognize(tuple((
            context("point:space_segment_leading", peek(alpha1)),
            space_no_dupe_dots,
            space_chars,
        )))(create_span("localhost"))))?;
        log(result(space_point_segment(create_span("localhost.com"))))?;

        log(result(point_subst(create_span("mechtron.io:app:hello"))))?;
        log(result(pipeline_stop(create_span("localhost:app:hello"))))?;
        Ok(())
    }

    #[test]
    pub fn test_pipeline() -> Result<(), MsgErr> {
        log(result(pipeline(create_span("-> localhost => &"))))?;
        Ok(())
    }

    #[test]
    pub fn test_pipeline_step() -> Result<(), MsgErr> {
        log(result(pipeline_step(create_span("->"))))?;
        log(result(pipeline_step(create_span("-[ Text ]->"))))?;
        log(result(pipeline_step(create_span("-[ Text ]=>"))))?;
        log(result(pipeline_step(create_span("=[ Text ]=>"))))?;

        assert!(log(result(pipeline_step(create_span("=")))).is_err());
        assert!(log(result(pipeline_step(create_span("-[ Bin ]=")))).is_err());
        assert!(log(result(pipeline_step(create_span("[ Bin ]=>")))).is_err());
        Ok(())
    }

    #[test]
    pub fn test_rough_bind_config() -> Result<(), MsgErr> {
        let unknown_config_kind = r#"
Unknown(version=1.0.0)-> # test unknown config kind
{
    Pipelines {
    }
}"#;
        let unsupported_bind_version = r#"
Bind(version=100.0.0)-> # test unsupported version
{
    Pipelines {
    }
}"#;
        let multiple_unknown_sub_selectors = r#"
Bind(version=1.0.0)->
{
    Whatever -> { # Someone doesn't care what sub selectors he creates
    }

    Dude(filter $(value))->{}  # he doesn't care one bit!

}"#;

        let now_we_got_rows_to_parse = r#"
Bind(version=1.0.0)->
{
    Pipelines(auth)-> {
       Http {
          <$(method=.*)>/users/$(user=.*)/$(path=.*)-> localhost:app:users:$(user)^Http<$(method)>/$(path) => &;
          <Get>/logout -> localhost:app:mechtrons:logout-handler => &;
       }
    }

    Pipelines-> {
       Msg<FullStop> -> localhost:apps:
       * -> localhost:app:bad-page => &;
    }


}"#;
        log(config(unknown_config_kind));
        log(config(unsupported_bind_version));
        log(config(multiple_unknown_sub_selectors));
        log(config(now_we_got_rows_to_parse));

        Ok(())
    }

    #[test]
    pub fn test_remove_comments() -> Result<(), MsgErr> {
        let bind_str = r#"
# this is a test of comments
Bind(version=1.0.0)->
{
  # let's see if it works a couple of spaces in.
  Pipeline(auth)-> {  # and if it works on teh same line as something we wan to keep

  }

  # looky!  I deliberatly put an error here (space between the filter and the kazing -> )
  # My hope is that we will get a an appropriate error message WITH COMMENTS INTACT
  Pipeline(noauth)-> # look!  I made a boo boo
  {
     # nothign to see here
  }
}"#;

        match config(bind_str) {
            Ok(_) => {}
            Err(err) => {
                err.print();
            }
        }

        Ok(())
    }

    #[test]
    pub fn test_version() -> Result<(), MsgErr> {
        rec_version(create_span("1.0.0"))?;
        rec_version(create_span("1.0.0-alpha"))?;
        version(create_span("1.0.0-alpha"))?;

        Ok(())
    }
    #[test]
    pub fn test_rough_block() -> Result<(), MsgErr> {
        result(all_consuming(lex_nested_block(NestedBlockKind::Curly))(
            create_span("{  }"),
        ))?;
        result(all_consuming(lex_nested_block(NestedBlockKind::Curly))(
            create_span("{ {} }"),
        ))?;
        assert!(
            result(all_consuming(lex_nested_block(NestedBlockKind::Curly))(
                create_span("{ } }")
            ))
            .is_err()
        );
        // this is allowed by rough_block
        result(all_consuming(lex_nested_block(NestedBlockKind::Curly))(
            create_span("{ ] }"),
        ))?;

        result(lex_nested_block(NestedBlockKind::Curly)(create_span(
            r#"x blah


Hello my friend


        }"#,
        )))
        .err()
        .unwrap()
        .print();

        result(lex_nested_block(NestedBlockKind::Curly)(create_span(
            r#"{

Hello my friend


        "#,
        )))
        .err()
        .unwrap()
        .print();
        Ok(())
    }

    #[test]
    pub fn test_block() -> Result<(), MsgErr> {
        log(result(lex_nested_block(NestedBlockKind::Curly)(create_span("{ <Get> -> localhost; }    "))))?;
        if true {
            return Ok(());
        }
        all_consuming(nested_block(NestedBlockKind::Curly))(create_span("{  }"))?;
        all_consuming(nested_block(NestedBlockKind::Curly))(create_span("{ {} }"))?;
        log(result(nested_block(NestedBlockKind::Curly)(create_span("{ [] }"))))?;
        assert!(
            expected_block_terminator_or_non_terminator(NestedBlockKind::Curly)(create_span("}"))
                .is_ok()
        );
        assert!(
            expected_block_terminator_or_non_terminator(NestedBlockKind::Curly)(create_span("]"))
                .is_err()
        );
        assert!(
            expected_block_terminator_or_non_terminator(NestedBlockKind::Square)(create_span("x"))
                .is_ok()
        );
        assert!(nested_block(NestedBlockKind::Curly)(create_span("{ ] }")).is_err());
        result(nested_block(NestedBlockKind::Curly)(create_span(
            r#"{



        ]


        }"#,
        )))
        .err()
        .unwrap()
        .print();
        Ok(())
    }

    #[test]
    pub fn test_root_scope_selector() -> Result<(), MsgErr> {
        assert!(
            (result(root_scope_selector(create_span(
                r#"

            Bind(version=1.0.0)->"#,
            )))
            .is_ok())
        );

        assert!(
            (result(root_scope_selector(create_span(
                r#"

            Bind(version=1.0.0-alpha)->"#,
            )))
            .is_ok())
        );

        result(root_scope_selector(create_span(
            r#"

            Bind(version=1.0.0) ->"#,
        )))
        .err()
        .unwrap()
        .print();

        result(root_scope_selector(create_span(
            r#"

        Bind   x"#,
        )))
        .err()
        .unwrap()
        .print();

        result(root_scope_selector(create_span(
            r#"

        (Bind(version=3.2.0)   "#,
        )))
        .err()
        .unwrap()
        .print();

        Ok(())
    }

    #[test]
    pub fn test_root_scope() -> Result<(), MsgErr> {
        assert!(
            (result(root_scope(create_span(
                r#"
            Bind(version=1.0.0)->{


            }"#,
            )))
            .is_ok())
        );

        assert!(
            (result(root_scope(create_span(
                r#"
            Bind(version=1.0.0)->    {
               Pipes {
               }
            }"#,
            )))
            .is_ok())
        );

        result(root_scope(create_span(
            r#"
    Bind(version=1.0.0)->{
               Pipes {

            }   "#,
        )))
        .err()
        .unwrap()
        .print();

        Ok(())
    }

    #[test]
    pub fn test_scope_filter() -> Result<(), MsgErr> {
        result(scope_filter(create_span("(auth)")))?;
        result(scope_filter(create_span("(auth )")))?;
        result(scope_filter(create_span("(auth hello)")))?;
        result(scope_filter(create_span("(auth +hello)")))?;
        result(scope_filters(create_span("(auth +hello)->")))?;
        result(scope_filters(create_span("(auth +hello)-(filter2)->")))?;
        result(scope_filters(create_span("(3auth +hello)-(filter2)->")))
            .err()
            .unwrap()
            .print();
        result(scope_filters(create_span("(a?th +hello)-(filter2)->")))
            .err()
            .unwrap()
            .print();
        result(scope_filters(create_span("(auth +hello)-(filter2) {}")))
            .err()
            .unwrap()
            .print();

        assert!(skewer_case(create_span("3x")).is_err());

        Ok(())
    }
    #[test]
    pub fn test_next_selector() {
        assert_eq!(
            "Http",
            next_selector(create_span("Http"))
                .unwrap()
                .1
                 .0
                .to_string()
                .as_str()
        );
        assert_eq!(
            "Http",
            next_selector(create_span("<Http>"))
                .unwrap()
                .1
                 .0
                .to_string()
                .as_str()
        );
        assert_eq!(
            "Http",
            next_selector(create_span("Http<Msg>"))
                .unwrap()
                .1
                 .0
                .to_string()
                .as_str()
        );
        assert_eq!(
            "Http",
            next_selector(create_span("<Http<Msg>>"))
                .unwrap()
                .1
                 .0
                .to_string()
                .as_str()
        );
    }
    #[test]
    pub fn test_lex_scope2() -> Result<(), MsgErr> {
        /*        let scope = log(result(lex_scopes(create_span(
                   "  Get -> {}\n\nPut -> {}   ",
               ))))?;

        */
        log(result(many0(delimited(
            multispace0,
            lex_scope,
            multispace0,
        ))(create_span(""))))?;
        log(result(rough_filepath_chars_plus_capture(create_span(
            "/root/$(subst)",
        ))))?;
        log(result(rough_filepath_chars_plus_capture(create_span(
            "/users/$(user=.*)",
        ))))?;

        Ok(())
    }

    #[test]
    pub fn test_lex_scope() -> Result<(), MsgErr> {
        let pipes = log(result(lex_scope(create_span("Pipes -> {}")))).unwrap();

        let pipes = log(result(lex_scope(create_span("Pipes {}"))))?;

        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Pipes");
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.block.content.len(), 0);
        assert!(pipes.selector.filters.is_empty());
        assert_eq!(pipes.pipeline_step, None);

        let pipes = log(result(lex_scope(create_span("Pipes {}"))))?;

        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Pipes");
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.block.content.len(), 0);
        assert_eq!(pipes.selector.filters.len(), 0);
        assert_eq!(pipes.pipeline_step, None);

        let pipes = log(result(lex_scope(create_span("Pipes -> 12345;"))))?;
        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Pipes");
        assert_eq!(pipes.block.content.to_string().as_str(), "12345");
        assert_eq!(
            pipes.block.kind,
            BlockKind::Terminated(TerminatedBlockKind::Semicolon)
        );
        assert_eq!(pipes.selector.filters.len(), 0);
        assert_eq!(pipes.pipeline_step.unwrap().to_string().as_str(), "->");
        let pipes = log(result(lex_scope(create_span(
            //This time adding a space before the 12345... there should be one space in the content, not two
            r#"Pipes ->  12345;"#,
        ))))?;

        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Pipes");
        assert_eq!(pipes.block.content.to_string().as_str(), " 12345");
        assert_eq!(
            pipes.block.kind,
            BlockKind::Terminated(TerminatedBlockKind::Semicolon)
        );
        assert_eq!(pipes.selector.filters.len(), 0);
        assert_eq!(pipes.pipeline_step.unwrap().to_string().as_str(), "->");

        let pipes = log(result(lex_scope(create_span("Pipes(auth) {}"))))?;

        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Pipes");
        assert_eq!(pipes.block.content.len(), 0);
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.selector.filters.len(), 1);
        assert_eq!(pipes.pipeline_step, None);

        let pipes = log(result(lex_scope(create_span("Pipeline<Msg>  {}"))))?;

        assert_eq!(
            pipes.selector.selector.name.to_string().as_str(),
            "Pipeline"
        );
        assert_eq!(
            Some(
                pipes
                    .selector
                    .selector
                    .children
                    .as_ref()
                    .unwrap()
                    .to_string()
                    .as_str()
            ),
            Some("<Msg>")
        );

        assert_eq!(pipes.block.content.to_string().as_str(), "");
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.selector.filters.len(), 0);
        assert_eq!(pipes.pipeline_step, None);

        let pipes = log(result(lex_scope(create_span(
            "Pipeline<Http>(noauth) -> {zoink!{}}",
        ))))?;
        assert_eq!(
            pipes.selector.selector.name.to_string().as_str(),
            "Pipeline"
        );
        assert_eq!(
            Some(
                pipes
                    .selector
                    .selector
                    .children
                    .as_ref()
                    .unwrap()
                    .to_string()
                    .as_str()
            ),
            Some("<Http>")
        );
        assert_eq!(pipes.block.content.to_string().as_str(), "zoink!{}");
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.selector.filters.len(), 1);
        //        assert_eq!(Some(pipes.pipeline_step.unwrap().to_string().as_str()),Some("->") );

        let msg = "Hello my future friend";
        let parseme = format!("<Http<Get>> -> {};", msg);
        let pipes = log(result(lex_scope(create_span(parseme.as_str()))))?;

        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Http");
        assert_eq!(pipes.block.content.to_string().as_str(), msg);
        assert_eq!(
            pipes.block.kind,
            BlockKind::Terminated(TerminatedBlockKind::Semicolon)
        );
        assert_eq!(pipes.selector.filters.len(), 0);
        assert_eq!(pipes.pipeline_step.unwrap().to_string().as_str(), "->");

        let pipes = log(result(lex_scope(create_span(
            "Pipeline<Http<Get>>/users/ -[Text ]-> {}",
        ))))?;
        assert_eq!(
            pipes.selector.selector.name.to_string().as_str(),
            "Pipeline"
        );
        assert_eq!(
            Some(
                pipes
                    .selector
                    .selector
                    .children
                    .as_ref()
                    .unwrap()
                    .to_string()
                    .as_str()
            ),
            Some("<Http<Get>>")
        );
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.selector.filters.len(), 0);
        assert_eq!(
            pipes.pipeline_step.as_ref().unwrap().to_string().as_str(),
            "-[Text ]->"
        );

        let pipes = log(result(lex_scope(create_span(
            "Pipeline<Http<Get>>/users/(auth) -[Text ]-> {}",
        ))))?;
        assert_eq!(
            pipes.selector.selector.name.to_string().as_str(),
            "Pipeline"
        );
        assert_eq!(
            Some(
                pipes
                    .selector
                    .selector
                    .children
                    .as_ref()
                    .unwrap()
                    .to_string()
                    .as_str()
            ),
            Some("<Http<Get>>")
        );
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.selector.filters.len(), 1);
        assert_eq!(
            pipes.pipeline_step.as_ref().unwrap().to_string().as_str(),
            "-[Text ]->"
        );

        let pipes = log(result(lex_scope(create_span(
            "Pipeline<Http<Get>>/users/(auth)-(blah xyz) -[Text ]-> {}",
        ))))?;
        assert_eq!(
            pipes.selector.selector.name.to_string().as_str(),
            "Pipeline"
        );
        assert_eq!(
            Some(
                pipes
                    .selector
                    .selector
                    .children
                    .as_ref()
                    .unwrap()
                    .to_string()
                    .as_str()
            ),
            Some("<Http<Get>>")
        );
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.selector.filters.len(), 2);
        assert_eq!(
            pipes.pipeline_step.as_ref().unwrap().to_string().as_str(),
            "-[Text ]->"
        );

        let (next, stripped) = strip_comments(
            r#"Pipeline<Http>/users/$(auth)(blah xyz) -[Text]-> {

            Get -> {}
            <Put>(superuser) -> localhost:app => &;
            Post/users/scott -> localhost:app^Msg<SuperScott> => &;

        }"#,
        )?;
        let span = LocatedSpan::new_extra(stripped.as_str(), Arc::new(stripped.to_string()));
        let pipes = log(result(lex_scope(span)))?;

        /*
        assert_eq!(
            pipes.selector.selector.name.to_string().as_str(),
            "Pipeline"
        );
        assert_eq!(
            Some(
                pipes
                    .selector
                    .selector
                    .children
                    .as_ref()
                    .unwrap()
                    .to_string()
                    .as_str()
            ),
            Some("<Http>")
        );
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.selector.filters.len(), 1);
        assert_eq!(
            pipes.pipeline_step.as_ref().unwrap().to_string().as_str(),
            "-[Text]->"
        );
         */

        /*
        fn follow(parent: LexScope<Span>, mut indent: String) -> Result<(), MsgErr> {
            let parent = lex_child_scopes(parent)?;
            println!("{}scope: {}", indent, parent.selector.selector.name);
            println!(
                "{}has_children: {}",
                indent,
                parent.selector.selector.has_children()
            );
            if let Some(children) = parent.selector.selector.children.clone() {
                println!("{}childx: {}", indent, children.to_string());
            }
            indent.push_str("\t");
            for child in parent.block {
                follow(child.clone(), indent.clone())?;
            }
            Ok(())
        }*/

        //        log(follow(pipes, "".to_string()))?;

        /* let scope = result(lex_scope(create_span(
            "Pipeline<Http<Get>>(path /users/$(user=.*)) -> {}",
        )))?;

        */

        Ok(())
    }

    pub fn test_nesting_bind() {
        let pipes = log(result(lex_scope(create_span(
            r#"


            Pipeline<Http>/auth/.*(auth) -> {

                   <Get>/auth/more ->

            }"#,
        ))))
        .unwrap();
    }

    #[test]
    pub fn test_root_and_subscope_phases() -> Result<(), MsgErr> {
        let config = r#"
Bind(version=1.2.3)-> {
   Pipeline -> {
   }

   Pipeline(auth)-> {
   }
}

        "#;

        let root = result(root_scope(create_span(config)))?;

        log(lex_scopes(root.block.content.clone()));
        let sub_scopes = lex_scopes(root.block.content.clone())?;

        assert_eq!(sub_scopes.len(), 2);

        Ok(())
    }

    fn log<R>(result: Result<R, MsgErr>) -> Result<R, MsgErr> {
        match result {
            Ok(r) => Ok(r),
            Err(err) => {
                err.print();
                Err(err)
            }
        }
    }
}

pub fn pipeline_step(input: Span) -> Res<Span, PipelineStep> {
    context(
        "pipeline:step",
        tuple((
            alt((
                value(MessageKind::Request, tag("-")),
                value(MessageKind::Response, tag("=")),
            )),
            opt(pair(
                delimited(
                    tag("["),
                    context("pipeline:step:exit", cut(request_payload_filter_block)),
                    tag("]"),
                ),
                context(
                    "pipeline:step:payload",
                    cut(alt((
                        value(MessageKind::Request, tag("-")),
                        value(MessageKind::Response, tag("=")),
                    ))),
                ),
            )),
            context("pipeline:step:exit", cut(tag(">"))),
        )),
    )(input)
    .map(|(next, ( entry, block_and_exit, _))| {
        let mut blocks = vec![];
        let exit = match block_and_exit {
            None => entry.clone(),
            Some((block, exit)) => {
                blocks.push(block);
                exit
            }
        };

        (
            next,
            PipelineStep {
                entry,
                exit,
                blocks,
            },
        )
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
    context("pipeline:stop:point", point_subst)(input)
        .map(|(next, point)| (next, PipelineStop::PointSubst(point)))
}

pub fn pipeline_stop(input: Span) -> Res<Span, PipelineStop> {
    context(
        "Stop",
        pair(
            context(
                "pipeline:stop:expecting",
                cut(peek(alt((alpha1, tag("&"))))),
            ),
            alt((
                core_pipeline_stop,
                return_pipeline_stop,
                call_pipeline_stop,
                point_pipeline_stop,
            )),
        ),
    )(input)
    .map(|(next, (_, pipeline_stop))| (next, pipeline_stop))
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
        "pipeline",
        many0(delimited(multispace0, pipeline_segment, multispace0)),
    )(input)
    .map(|(next, segments)| (next, Pipeline { segments }))
}

pub fn consume_pipeline(input: Span) -> Res<Span, Pipeline> {
    all_consuming(pipeline)(input)
}

pub fn entity_selectors(input: Span) -> Res<Span, Vec<Selector<PipelineSelector>>> {
    many0(delimited(multispace0, entity_selector, multispace0))(input)
}

pub fn entity_selector(input: Span) -> Res<Span, Selector<PipelineSelector>> {
    tuple((entity_pattern, multispace0, pipeline, tag(";")))(input)
        .map(|(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)))
}

pub fn msg_selector(input: Span) -> Res<Span, Selector<MsgPipelineSelector>> {
    tuple((msg_pattern_scoped, multispace0, pipeline, tag(";")))(input)
        .map(|(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)))
}

pub fn http_pipeline(input: Span) -> Res<Span, Selector<HttpPipelineSelector>> {
    tuple((http_pattern_scoped, multispace0, pipeline, tag(";")))(input)
        .map(|(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)))
}

pub fn rc_selector(input: Span) -> Res<Span, Selector<RcPipelineSelector>> {
    tuple((rc_pattern_scoped, multispace0, pipeline, tag(";")))(input)
        .map(|(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)))
}

pub fn consume_selector(input: Span) -> Res<Span, Selector<PipelineSelector>> {
    all_consuming(entity_selector)(input)
}
