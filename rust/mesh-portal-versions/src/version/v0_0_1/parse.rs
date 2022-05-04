use core::fmt;
use std::collections::HashMap;
use std::fmt::Formatter;
use std::ops::{Deref, Range, RangeFrom, RangeTo};
use std::rc::Rc;
use std::str::FromStr;

use nom::bytes::complete::{escaped, is_a, is_not};
use nom::bytes::complete::{tag, take_till, take_until, take_until1, take_while};
use nom::character::complete::{
    alpha0, alphanumeric0, alphanumeric1, anychar, char, digit0, line_ending, multispace0,
    multispace1, newline, one_of, satisfy, space0, space1,
};
use nom::combinator::{cut, eof, fail, not, peek, recognize, success, value, verify};

use crate::error::{MsgErr, ParseErrs};
use crate::version::v0_0_1::command::command::common::{PropertyMod, SetProperties, StateSrc};
use crate::version::v0_0_1::entity::entity::request::create::{
    Create, CreateOp, KindTemplate, PointSegFactory, PointTemplate, PointTemplateSeg, Require,
    Strategy, Template,
};
use crate::version::v0_0_1::entity::entity::request::get::{Get, GetOp};
use crate::version::v0_0_1::entity::entity::request::select::{
    Select, SelectIntoPayload, SelectKind,
};
use crate::version::v0_0_1::entity::entity::request::set::Set;
use crate::version::v0_0_1::id::id::{Point, PointCtx, PointSegCtx, PointSegDelim, PointSegVar, PointVar, RouteSeg, RouteSegVar, Version};
use crate::version::v0_0_1::security::{
    AccessGrantKind, AccessGrantKindDef, ChildPerms, ParticlePerms, Permissions, PermissionsMask,
    PermissionsMaskKind, Privilege,
};
use crate::version::v0_0_1::selector::selector::{
    MapEntryPatternCtx, PointKindHierarchy, PointKindSeg,
};
use crate::version::v0_0_1::util::{MethodPattern, StringMatcher, ValuePattern};
use nom::bytes::complete::take;
use nom::character::is_space;
use nom_supreme::final_parser::ExtractContext;
use regex::internal::Input;
use regex::Regex;
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

pub fn local_route_segment(input: OwnedSpan) -> Res<OwnedSpan, RouteSeg> {
    alt((recognize(tag(".")), recognize(not(other_route_segment))))(input)
        .map(|(next, _)| (next, RouteSeg::Local))
}

pub fn domain_route_segment(input: OwnedSpan) -> Res<OwnedSpan, RouteSeg> {
    domain_chars(input).map(|(next, domain)| (next, RouteSeg::Domain(domain.to_string())))
}

pub fn tag_route_segment(input: OwnedSpan) -> Res<OwnedSpan, RouteSeg> {
    delimited(tag("["), skewer_chars, tag("]"))(input)
        .map(|(next, tag)| (next, RouteSeg::Tag(tag.to_string())))
}

pub fn mesh_route_segment(input: OwnedSpan) -> Res<OwnedSpan, RouteSeg> {
    delimited(tag("<<"), mesh_route_chars, tag(">>"))(input)
        .map(|(next, tag)| (next, RouteSeg::Tag(tag.to_string())))
}

pub fn other_route_segment(input: OwnedSpan) -> Res<OwnedSpan, RouteSeg> {
    alt((tag_route_segment, domain_route_segment, mesh_route_segment))(input)
}

pub fn point_route_segment(input: OwnedSpan) -> Res<OwnedSpan, RouteSeg> {
    alt((local_route_segment, other_route_segment))(input)
}

/*
pub fn point_segment(input: Span) -> Res<Span, PointSegCtx> {
    alt((
        base_point_segment,
        space_point_segment,
        version_point_segment,
        filesystem_point_segment,
        file_point_segment,
    ))(input)
}

 */

pub fn mesh_eos(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    peek(alt((tag(":"), eop)))(input)
}

pub fn fs_trailing(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    peek(pair(
        recognize(tag(":")),
        context("point:version:root_not_trailing", cut(tag("/"))),
    ))(input)
    .map(|(next, (rtn, _))| (next, rtn))
}

// version end of segment
pub fn ver_eos(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    peek(alt((fs_trailing, tag(":/"), eop)))(input)
}

// end of point
pub fn eop(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
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

pub fn space_no_dupe_dots(input: OwnedSpan) -> Res<OwnedSpan, ()> {
    context(
        "point:space_segment:dot_dupes",
        peek(cut(not(take_until("..")))),
    )(input)
    .map(|(next, _)| (next, ()))
}

pub fn space_point_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSeg> {
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

pub fn base_point_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSeg> {
    preceded(
        peek(lowercase1),
        context("point:base_segment", cut(pair(rec_skewer, mesh_eos))),
    )(input)
    .map(|(next, (base, _))| (next, PointSeg::Base(base.to_string())))
}

pub fn version_point_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSeg> {
    preceded(
        peek(digit0),
        context("point:version_segment", cut(tuple((version, ver_eos)))),
    )(input)
    .map(|(next, (version, _))| (next, PointSeg::Version(version)))
}

pub fn dir_pop(input: OwnedSpan) -> Res<OwnedSpan, PointSegCtx> {
    context("point:dir_pop", tuple((tag(".."), opt(tag("/")))))(input)
        .map(|(next, _)| (next, PointSegCtx::Pop))
}

pub fn filesystem_point_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSeg> {
    tuple((
        peek(not(eop)),
        context(
            "point:file_or_directory",
            cut(alt((dir_point_segment, file_point_segment))),
        ),
    ))(input)
    .map(|(next, (_, seg))| (next, seg))
}

pub fn dir_point_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSeg> {
    context("point:dir_segment", terminated(file_chars, tag("/")))(input)
        .map(|(next, dir)| (next, PointSeg::Dir(format!("{}/", dir))))
}

pub fn root_dir_point_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSeg> {
    context("point:root_filesystem_segment", tag(":/"))(input)
        .map(|(next, _)| (next, PointSeg::FilesystemRootDir))
}

pub fn root_dir_point_segment_ctx(input: OwnedSpan) -> Res<OwnedSpan, PointSegCtx> {
    context("point:root_filesystem_segment", tag(":/"))(input)
        .map(|(next, _)| (next, PointSegCtx::FilesystemRootDir))
}

pub fn root_dir_point_segment_var(input: OwnedSpan) -> Res<OwnedSpan, PointSegVar> {
    context("point:root_filesystem_segment", tag(":/"))(input)
        .map(|(next, _)| (next, PointSegVar::FilesystemRootDir))
}


pub fn file_point_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSeg> {
    context("point:file_segment", file_chars)(input)
        .map(|(next, filename)| (next, PointSeg::File(filename.to_string())))
}

pub fn point(input: OwnedSpan) -> Res<OwnedSpan, Point> {
    let (next, point) = point_ctx(input.clone())?;

    match point.try_into() {
        Ok(point) => Ok((next, point)),
        Err(err) => {
            let span = input.slice(0..(input.len() - next.len()));
            let err = ErrorTree::from_error_kind(span.clone(), ErrorKind::Tag);
            let err = ErrorTree::add_context(span.clone(), "point", err);
            Err(nom::Err::Error(err))
        }
    }
}

pub fn point_ctx(input: OwnedSpan) -> Res<OwnedSpan, PointCtx> {
    context(
        "point",
        tuple((alt((root_point, point_non_root)), peek(eop))),
    )(input.clone())
    .map(|(next, (point, _))| (next, point))
}

pub fn point_var(input: OwnedSpan) -> Res<OwnedSpan, PointVar> {
    context(
        "point",
        tuple((alt((root_point_var, point_non_root_var)), peek(eop))),
    )(input.clone())
        .map(|(next, (point, _))| (next, point))
}



/*
pub fn var<O,F,P>(mut f: F ) -> impl FnMut(OwnedSpan) -> Res<OwnedSpan,Var<O,P>> where F: Parser<OwnedSpan,O,ErrorTree<OwnedSpan>>, P: VarParser<O> {
    move | input: OwnedSpan | {
        let result = recognize(pair(peek(tag("$")),context("var",cut(delimited(tag("${"), skewer_case, tag("}") )))))(input.clone());
        match result {
            Ok((next,var)) => {
                Ok( (next, Var::Var{ name: var.to_string(), parser: f }) )
            }
            Err(err) => {
                match &err {
                    Err::Incomplete(_) => {
                        Err(err)
                    }
                    Err::Failure(_) => {
                        Err(err)
                    }
                    // in this case the peek failed which means it is not a variable declaration
                    Err::Error(_) => {
                        let input = to_owned_span(&input);
                        f.parse(input)
                    }
                }
            }
        }


        let input = to_owned_span(&input);

        input.parse(input)
    }
}

 */

pub fn var_seg <F>(mut f: F ) -> impl FnMut(OwnedSpan) -> Res<OwnedSpan,PointSegVar> where F: Parser<OwnedSpan,PointSegCtx,ErrorTree<OwnedSpan>>{

    move | input: OwnedSpan | {
        let result = recognize(pair(peek(tag("$")),context("var",cut(delimited(tag("${"), skewer_case, tag("}") )))))(input.clone());
        match result {
            Ok((next,var)) => {
                Ok( (next, PointSegVar::Var(var.to_string())) )
            }
            Err(err) => {
                Err(err)
            }
        }
    }
}

pub fn var_route<'a,F>(mut f: F ) -> impl FnMut(OwnedSpan) -> Res<OwnedSpan,RouteSegVar> where F: Parser<OwnedSpan,RouteSeg,ErrorTree<OwnedSpan>>{
    move | input: OwnedSpan | {
        let result = recognize(pair(peek(tag("$")),context("var",cut(delimited(tag("${"), skewer_case, tag("}") )))))(input.clone());
        match result {
            Ok((next,var)) => {
                Ok( (next, RouteSegVar::Var(var.to_string())) )
            }
            Err(err) => {
                Err(err)
            }
        }
    }
}
pub fn root_point_var(input: OwnedSpan) -> Res<OwnedSpan, PointVar> {
    tuple((opt(terminated(var_route(point_route_segment), tag("::"))), tag("ROOT")))(input).map(
        |(next, (route, _))| {
            let route = route.unwrap_or(RouteSegVar::Local);
            let point = PointVar {
                route,
                segments: vec![],
            };
            (next, point)
        },
    )
}

pub fn root_point(input: OwnedSpan) -> Res<OwnedSpan, PointCtx> {
    tuple((opt(terminated(point_route_segment, tag("::"))), tag("ROOT")))(input).map(
        |(next, (route, _))| {
            let route = route.unwrap_or(RouteSeg::Local);
            let point = PointCtx {
                route,
                segments: vec![],
            };
            (next, point)
        },
    )
}

pub fn point_non_root_var(input: OwnedSpan) -> Res<OwnedSpan, PointVar> {


    context(
        "point_non_root",
        tuple((
            context(
                "point_route",
                opt(terminated(var_route(point_route_segment), tag("::"))),
            ),
            var_seg(ctx_seg(space_point_segment)),
            many0(tuple((
                seg_delim,
                peek(context("point:bad_leading", cut(alt((tag("$"),tag("."),lowercase1, digit1))))),
                var_seg(mesh_seg(base_point_segment)),
            ))),
            opt(var_seg(mesh_seg(version_point_segment))),
            opt(tuple((
                root_dir_point_segment_var,
                many0(var_seg(pop(filesystem_point_segment))),
                eop,
            ))),
            eop,
        )),
    )(input)
        .map(
            |(next, (route, space, mut bases, version, filesystem, _))| {
                let route = route.unwrap_or(RouteSegVar::Local);
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

                let point = PointVar{ route, segments };

                (next, point)
            },
        )
}


pub fn point_non_root(input: OwnedSpan) -> Res<OwnedSpan, PointCtx> {
    context(
        "point_non_root",
        tuple((
            context(
                "point_route",
                opt(terminated(point_route_segment, tag("::"))),
            ),
            ctx_seg(space_point_segment),
            many0(tuple((
                seg_delim,
                peek(context("point:bad_leading", cut(alt((tag("."),lowercase1, digit1))))),
                pop(base_point_segment),
            ))),
            opt(mesh_seg(version_point_segment)),
            opt(tuple((
                root_dir_point_segment_ctx,
                many0(pop(filesystem_point_segment)),
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

            let point = PointCtx { route, segments };

            (next, point)
        },
    )
}

pub fn consume_point(input: &str) -> Result<Point, MsgErr> {
    let span = new_span(input);
    let point = result(context("consume", all_consuming(point))(span))?;
    Ok(point)
}

/*
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

 */

/*pub fn capture_point(input: Span) -> Res<Span, CaptureAddress> {
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
 */

pub fn space_point_kind_segment(input: OwnedSpan) -> Res<OwnedSpan, PointKindSeg> {
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

pub fn base_point_kind_segment(input: OwnedSpan) -> Res<OwnedSpan, PointKindSeg> {
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

pub fn filepath_point_kind_segment(input: OwnedSpan) -> Res<OwnedSpan, PointKindSeg> {
    alt((file_point_kind_segment, dir_point_kind_segment))(input)
}
pub fn dir_point_kind_segment(input: OwnedSpan) -> Res<OwnedSpan, PointKindSeg> {
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

pub fn file_point_kind_segment(input: OwnedSpan) -> Res<OwnedSpan, PointKindSeg> {
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

pub fn version_point_kind_segment(input: OwnedSpan) -> Res<OwnedSpan, PointKindSeg> {
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

pub fn consume_hierarchy(input: OwnedSpan) -> Result<PointKindHierarchy, MsgErr> {
    let (_, rtn) = all_consuming(point_kind_hierarchy)(input)?;
    Ok(rtn)
}

pub fn point_kind_hierarchy(input: OwnedSpan) -> Res<OwnedSpan, PointKindHierarchy> {
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

pub fn rec_skewer(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(tuple((lowercase1, opt(skewer))))(input)
}

pub fn rec_skewer_capture(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
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

pub fn path_regex(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    let (next, regex_span) = context("regex", recognize(pair(tag("/"), nospace0)))(input.clone())?;

    let regex_string = regex_span.to_string();
    match Regex::new(regex_string.as_str()) {
        Ok(regex) => Ok((next, regex_span)),
        Err(err) => {
            println!("regex error {}", err.to_string());
            return Err(nom::Err::Error(ErrorTree::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }
    }
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

pub fn not_space(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    is_not(" \n\r\t")(input)
}

pub fn path(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(tuple((tag("/"), opt(filepath_chars))))(input)
}

pub fn capture_path(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(tuple((tag("/"), opt(file_chars_plus_capture))))(input)
}

pub fn consume_path(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    all_consuming(path)(input)
}

pub fn camel_case(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(tuple((is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alphanumeric0)))(input)
    //recognize(alpha1)(input)
}

pub fn skewer_case(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
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

pub fn single_lowerscase(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    is_a("abcdefghijklmnopqrstuvwxyz")(input)
}
pub fn single_digit(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    is_a("abcdefghijklmnopqrstuvwxyz")(input)
}

pub fn camel_case_to_string_matcher(input: OwnedSpan) -> Res<OwnedSpan, StringMatcher> {
    camel_case(input).map(|(next, camel)| (next, StringMatcher::new(camel.to_string())))
}

fn parse_version_major_minor_patch(input: OwnedSpan) -> Res<OwnedSpan, (OwnedSpan, OwnedSpan, OwnedSpan)> {
    context(
        "version_major_minor_patch",
        tuple((
            terminated(digit1, tag(".")),
            terminated(digit1, tag(".")),
            terminated(digit1, not(digit1)),
        )),
    )(input)
}

pub fn parse_version(input: OwnedSpan) -> Res<OwnedSpan, ((OwnedSpan, OwnedSpan, OwnedSpan), Option<OwnedSpan>)> {
    tuple((
        parse_version_major_minor_patch,
        opt(preceded(tag("-"), skewer_chars)),
    ))(input)
}

pub fn rec_version(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(parse_version)(input)
}

pub fn base_point_segment_wildcard(input: OwnedSpan) -> Res<OwnedSpan, PointTemplateSeg> {
    preceded(
        tag(":"),
        recognize(tuple((many0(skewer), tag("%"), many0(skewer)))),
    )(input)
    .map(|(next, base)| (next, PointTemplateSeg::Wildcard(base.to_string())))
}

pub fn base_point_segment_template(input: OwnedSpan) -> Res<OwnedSpan, PointTemplateSeg> {
    preceded(tag(":"), rec_skewer)(input).map(|(next, base)| {
        (
            next,
            PointTemplateSeg::ExactSeg(PointSeg::Base(base.to_string())),
        )
    })
}

pub fn filepath_point_segment_wildcard(input: OwnedSpan) -> Res<OwnedSpan, PointTemplateSeg> {
    recognize(tuple((
        many0(filepath_chars),
        tag("%"),
        many0(filepath_chars),
    )))(input)
    .map(|(next, base)| (next, PointTemplateSeg::Wildcard(base.to_string())))
}

pub fn filepath_point_segment_template(input: OwnedSpan) -> Res<OwnedSpan, PointTemplateSeg> {
    filesystem_point_segment(input)
        .map(|(next, segment)| (next, PointTemplateSeg::ExactSeg(segment)))
}

pub fn point_template(input: OwnedSpan) -> Res<OwnedSpan, PointTemplate> {
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

pub fn kind_template(input: OwnedSpan) -> Res<OwnedSpan, KindTemplate> {
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

pub fn template(input: OwnedSpan) -> Res<OwnedSpan, Template> {
    tuple((point_template, delimited(tag("<"), kind_template, tag(">"))))(input)
        .map(|(next, (point, kind))| (next, Template { point, kind }))
}

pub fn set_property_mod(input: OwnedSpan) -> Res<OwnedSpan, PropertyMod> {
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

pub fn set_property_mod_lock(input: OwnedSpan) -> Res<OwnedSpan, PropertyMod> {
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

pub fn property_value_not_space_or_comma(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    is_not(" \n\r\t,")(input)
}

pub fn property_value_single_quotes(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    delimited(tag("'"), is_not("'"), tag("'"))(input)
}

pub fn property_value_double_quotes(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    delimited(tag("\""), is_not("\""), tag("\""))(input)
}

pub fn property_value(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    alt((
        property_value_single_quotes,
        property_value_double_quotes,
        property_value_not_space_or_comma,
    ))(input)
}

pub fn unset_property_mod(input: OwnedSpan) -> Res<OwnedSpan, PropertyMod> {
    tuple((tag("!"), skewer_dot))(input)
        .map(|(next, (_, name))| (next, PropertyMod::UnSet(name.to_string())))
}

pub fn property_mod(input: OwnedSpan) -> Res<OwnedSpan, PropertyMod> {
    alt((set_property_mod, unset_property_mod))(input)
}

pub fn set_properties(input: OwnedSpan) -> Res<OwnedSpan, SetProperties> {
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

pub fn get_properties(input: OwnedSpan) -> Res<OwnedSpan, Vec<String>> {
    separated_list0(tag(","), tuple((multispace0, skewer, multispace0)))(input).map(
        |(next, keys)| {
            let keys: Vec<String> = keys.iter().map(|(_, key, _)| key.to_string()).collect();
            (next, keys)
        },
    )
}

pub fn create(input: OwnedSpan) -> Res<OwnedSpan, Create> {
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

pub fn set(input: OwnedSpan) -> Res<OwnedSpan, Set> {
    tuple((point, delimited(tag("{"), set_properties, tag("}"))))(input).map(
        |(next, (point, properties))| {
            let set = Set { point, properties };
            (next, set)
        },
    )
}

pub fn get(input: OwnedSpan) -> Res<OwnedSpan, Get> {
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

pub fn select(input: OwnedSpan) -> Res<OwnedSpan, Select> {
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

pub fn publish(input: OwnedSpan) -> Res<OwnedSpan, CreateOp> {
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
pub enum Ctx {
    WorkingPoint,
    WorkingPointPop,
    PointFromRoot,
}

impl ToString for Ctx {
    fn to_string(&self) -> String {
        match self {
            Ctx::WorkingPoint => ".".to_string(),
            Ctx::WorkingPointPop => "..".to_string(),
            Ctx::PointFromRoot => "...".to_string(),
        }
    }
}

pub trait CtxResolver {
    fn ctx(&self, ctx: &Ctx) -> Result<String, MsgErr>;
}


pub enum VarResolverErr {
    NotFound
}

pub trait VarResolver {
    fn val(&self, var: &str) -> Result<String, VarResolverErr> {
        Err(VarResolverErr::NotFound)
    }
}

pub struct NoResolver;

impl NoResolver {
    pub fn new() -> Self {
        Self {}
    }
}

impl VarResolver for NoResolver {
}

#[derive(Clone)]
pub struct MapResolver {
    pub map: HashMap<String, String>,
}

impl MapResolver {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: &str, value: &str) {
        self.map.insert(key.to_string(), value.to_string());
    }
}

impl VarResolver for MapResolver {
    fn val(&self, id: &str) -> Result<String, VarResolverErr> {
        self.map
            .get(id)
            .cloned()
            .ok_or(VarResolverErr::NotFound )
    }


}

pub struct MockCtxResolver {
    pub point: Point
}

impl CtxResolver for MockCtxResolver {
    fn ctx(&self, ctx: &Ctx) -> Result<String, MsgErr> {
        match ctx {
            Ctx::WorkingPoint => Ok(self.point.to_string()),
            Ctx::WorkingPointPop => Ok(self
                .point
                .parent()
                .ok_or("already at the root point cannot pop any further.")?
                .to_string()),
            Ctx::PointFromRoot => {
                Err("PointFromRoot operator '...' not available in this resolver".into())
            }
        }
    }
}

pub trait VarSubst<R>
where
    Self: Sized,
{
    fn resolve_vars(self, resolver: &dyn VarResolver) -> Result<R, MsgErr>;
}

pub trait CtxSubst<R>
where
    Self: Sized,
{
    fn resolve_ctx(self, resolver: &dyn CtxResolver) -> Result<R, MsgErr>;
}

/*
pub trait BruteResolver<Resolved>
where
    Self: Sized + ToResolved<Resolved>,
{
    fn brute_resolve(self) -> Result<Resolved, MsgErr> {
        let resolver = NoResolver::new().wrap();
        Ok(self.to_resolved(&resolver)?)
    }
}

 */

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

pub trait SubstParser<T: Sized> {
    fn parse_string(&self, string: String) -> Result<T, MsgErr> {
        let span = new_span(string.as_str());
        let output = result(self.parse_span(span))?;
        Ok(output)
    }

    fn parse_span<'a>(&self, input: OwnedSpan) -> Res<OwnedSpan, T>;
}



pub fn ctx_seg<I: Clone, E: ParseError<I>, F>(mut f: F) -> impl FnMut(I) -> IResult<I, PointSegCtx, E>
    where
        I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
        <I as InputTakeAtPosition>::Item: AsChar + Clone,
        F: nom::Parser<I, PointSeg, E>,
        E: nom::error::ContextError<I>,
{
    move |input: I| match pair(tag::<&str, I, E>(".."), eos)(input.clone()) {
        Ok((next, v)) => Ok((next, PointSegCtx::Pop)),
        Err(err) => {
            match pair(tag::<&str, I, E>("."), eos)(input.clone()) {
                Ok((next, _)) => Ok((next,PointSegCtx::Working)),
                Err(err) => {
                    match f.parse(input) {
                        Ok((next,seg)) => Ok((next,seg.into())),
                        Err(err) => {
                            Err(err)
                        }
                    }
                }
            }
        },
    }
}

pub fn working<I: Clone, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, PointSegCtx, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: nom::Parser<I, PointSeg, E>,
    E: nom::error::ContextError<I>,
{
    move |input: I| match pair(tag::<&str, I, E>("."), eos)(input.clone()) {
        Ok((next, v)) => Ok((next, PointSegCtx::Pop)),
        Err(err) => match f.parse(input.clone()) {
            Ok((next, seg)) => Ok((next, seg.into())),
            Err(err) => Err(err),
        },
    }
}

pub fn pop<I: Clone, E: ParseError<I>, F>(mut f: F) -> impl FnMut(I) -> IResult<I, PointSegCtx, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: nom::Parser<I, PointSeg, E>,
    E: nom::error::ContextError<I>,
{
    move |input: I| match pair(tag::<&str, I, E>(".."), eos)(input.clone()) {
        Ok((next, v)) => Ok((next, PointSegCtx::Pop)),
        Err(err) => match f.parse(input.clone()) {
            Ok((next, seg)) => Ok((next, seg.into())),
            Err(err) => Err(err),
        },
    }
}

pub fn mesh_seg<I: Clone, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, PointSegCtx, E>
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
    peek(alt((tag("/"), tag(":"), space1,eof)))(input).map(|(next, _)| (next, ()))
}

/*
pub fn var<O>(f: impl Fn(Span) -> Res<Span,O>+'static+Clone) -> impl FnMut(Span) -> Res<Span, Symbol<O>>
{
    unimplemented!()
    /*
    move |input: Span| {
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
        .map(|(next, name)| {
            (
                next,
                Symbol::named(name.to_string(), f.clone() ),
            )
        })
    }

     */
}

 */

/*jjj
pub fn ctx<O>(input: Span) -> Res<Span, Symbol<O>>

{
    alt((
        value(Ctx::RelativePointPop, tuple((tag(".."), eos))),
        value(Ctx::RelativePoint, tuple((tag("."), eos))),
    ))(input)
        .map(|(next, ctx)| (next, Symbol::ctx(ctx)))
}

 */

/*
pub fn ctx<I, O, F, E>(input: I) -> Res<I, Symbol<O>>
where
    I: ToString
        + Clone
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: nom::Parser<I, O, E> + Clone,
    E: nom::error::ContextError<I>,
{
    alt((
        value(Ctx::RelativePointPop, tuple((tag(".."), eos))),
        value(Ctx::RelativePoint, tuple((tag("."), eos))),
    ))(input)
    .map(|(next, ctx)| (next, Symbol::ctx(ctx)))
}

 */

pub fn variable_name(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(pair(lowercase1, opt(skewer_dot)))(input).map(|(next, name)| (next, name))
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


pub fn sub<'a, O, F>(mut f: F) -> impl FnMut(OwnedSpan) -> Res<OwnedSpan, Spanned<OwnedSpan, O>>
where
    F: nom::Parser<OwnedSpan, O, ErrorTree<OwnedSpan>>,
    O: Clone,
{
    move |input: OwnedSpan| {
        let (next, element) = f.parse(input.clone())?;
        Ok((
            next.clone(),
            Spanned::new(element, input.slice(0..(input.len() - next.len()))),
        ))
    }
}

pub fn access_grant_kind(input: OwnedSpan) -> Res<OwnedSpan, AccessGrantKind> {
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

pub fn access_grant_kind_priv(input: OwnedSpan) -> Res<OwnedSpan, AccessGrantKind> {
    tuple((
        tag("priv"),
        context("access_grant:priv", tuple((space1, privilege))),
    ))(input)
    .map(|(next, (_, (_, privilege)))| (next, AccessGrantKindDef::Privilege(privilege)))
}

pub fn access_grant_kind_perm(input: OwnedSpan) -> Res<OwnedSpan, AccessGrantKind> {
    tuple((
        tag("perm"),
        context("access_grant:perm", tuple((space1, permissions_mask))),
    ))(input)
    .map(|(next, (_, (_, perms)))| (next, AccessGrantKindDef::PermissionsMask(perms)))
}

pub fn privilege(input: OwnedSpan) -> Res<OwnedSpan, Privilege> {
    context("privilege", alt((tag("*"), skewer_colon)))(input).map(|(next, prv)| {
        let prv = match prv.to_string().as_str() {
            "*" => Privilege::Full,
            prv => Privilege::Single(prv.to_string()),
        };
        (next, prv)
    })
}

pub fn permissions_mask(input: OwnedSpan) -> Res<OwnedSpan, PermissionsMask> {
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

pub fn permissions(input: OwnedSpan) -> Res<OwnedSpan, Permissions> {
    context(
        "permissions",
        tuple((child_perms, tag("-"), particle_perms)),
    )(input)
    .map(|(next, (child, _, particle))| {
        let permissions = Permissions { child, particle };
        (next, permissions)
    })
}

pub fn child_perms(input: OwnedSpan) -> Res<OwnedSpan, ChildPerms> {
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

pub fn particle_perms(input: OwnedSpan) -> Res<OwnedSpan, ParticlePerms> {
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
                recognize(many0(alt((
                    recognize(lex_nested_block(kind.clone())),
                    recognize(tuple((
                        not(peek(tag(kind.close()))),
                        alt((recognize(pair(tag("\\"), anychar)), recognize(anychar))),
                    ))),
                )))),
                context(kind.close_context(), cut(tag(kind.close()))),
            ),
        )(input)?;
        let block = Block::parse(BlockKind::Nested(kind), content);
        Ok((next, block))
    }
}

pub fn nested_block_content(kind: NestedBlockKind) -> impl FnMut(OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    move |input: OwnedSpan| nested_block(kind)(input).map(|(next, block)| (next, block.content))
}

pub fn nested_block(kind: NestedBlockKind) -> impl FnMut(OwnedSpan) -> Res<OwnedSpan, Block<OwnedSpan, ()>> {
    move |input: OwnedSpan| {
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

fn block_open(input: OwnedSpan) -> Res<OwnedSpan, NestedBlockKind> {
    alt((
        value(NestedBlockKind::Curly, tag(NestedBlockKind::Curly.open())),
        value(NestedBlockKind::Angle, tag(NestedBlockKind::Angle.open())),
        value(NestedBlockKind::Parens, tag(NestedBlockKind::Parens.open())),
        value(NestedBlockKind::Square, tag(NestedBlockKind::Square.open())),
    ))(input)
}

fn any_soround_lex_block<'a, I, E>(input: I) -> IResult<I, LexBlock<I>, E>
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

fn any_block(input: OwnedSpan) -> Res<OwnedSpan, LexBlock<OwnedSpan>> {
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

pub fn lex_child_scopes<'a>(parent: LexScope<OwnedSpan>) -> Result<LexParentScope<'a>, MsgErr> {
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

pub fn lex_scope(input: OwnedSpan) -> Res<OwnedSpan, LexScope<OwnedSpan>> {
    context(
        "scope",
        tuple((
            peek(alt((tag("*"), alpha1, tag("<")))),
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

pub fn lex_scoped_block_kind(input: OwnedSpan) -> Res<OwnedSpan, BlockKind> {
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

pub fn lex_scope_pipeline_step_and_block(input: OwnedSpan) -> Res<OwnedSpan, (Option<OwnedSpan>, LexBlock<OwnedSpan>)> {
    let (_, block_kind) = peek(lex_scoped_block_kind)(input.clone())?;
    match block_kind {
        BlockKind::Nested(_) => tuple((
            rough_pipeline_step,
            multispace1,
            lex_block(BlockKind::Nested(NestedBlockKind::Curly)),
        ))(input)
        .map(|(next, (step, _, block))| (next, (Some(step), block))),
        BlockKind::Terminated(_) => {
            lex_block(BlockKind::Terminated(TerminatedBlockKind::Semicolon))(input)
                .map(|(next, block)| (next, (None, block)))
        }
        _ => unimplemented!(),
    }
}

pub fn lex_sub_scope_selectors_and_filters_and_block(input: OwnedSpan) -> Res<OwnedSpan, LexBlock<OwnedSpan>> {
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

pub fn root_scope(input: OwnedSpan) -> Res<OwnedSpan, LexRootScope<OwnedSpan>> {
    context(
        "root-scope",
        tuple((
            root_scope_selector,
            multispace0,
            context("root-scope:block", cut(peek(tag("{")))),
            context(
                "root-scope:block",
                cut(lex_nested_block(NestedBlockKind::Curly)),
            ),
        )),
    )(input)
    .map(|(next, (selector, _, _, block))| {
        let scope = LexRootScope::new(selector, block);
        (next, scope)
    })
}

pub fn lex_scopes(input: OwnedSpan) -> Result<Vec<LexScope<OwnedSpan>>, MsgErr> {
    if input.len() == 0 {
        return Ok(vec![]);
    }

    if wrapper(input.clone(), all_consuming(multispace1)).is_ok() {
        return Ok(vec![]);
    }

    result(
        context(
            "parsed-scopes",
            all_consuming(many0(delimited(
                multispace0,
                context(
                    "scope",
                    pair(peek(not(alt((tag("}"), eof)))), cut(lex_scope)),
                ),
                multispace0,
            ))),
        )(input)
        .map(|(next, scopes)| {
            let scopes: Vec<LexScope<OwnedSpan>> = scopes.into_iter().map(|scope| scope.1).collect();
            (next, scopes)
        }),
    )
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

pub fn next_selector(input: OwnedSpan) -> Res<OwnedSpan, (OwnedSpan, Option<OwnedSpan>)> {
    match wrapper(
        input.clone(),
        pair(
            peek(tag("<")),
            tuple((
                tag("<"),
                pair(
                    context("scope-selector", alt((alphanumeric1, tag("*")))),
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
        context("scope-selector", cut(alt((alphanumeric1, tag("*"))))),
        opt(recognize(nested_block(NestedBlockKind::Angle))),
    )(input)
}

pub fn lex_scope_selector_and_filters(
    input: OwnedSpan,
) -> Res<OwnedSpan, ScopeSelectorAndFiltersDef<LexScopeSelector<OwnedSpan>, OwnedSpan>> {
    context(
        "parsed-scope-selector-and-filter",
        pair(lex_scope_selector, scope_filters),
    )(input)
    .map(|(next, (selector, filters))| (next, ScopeSelectorAndFiltersDef::new(selector, filters)))
}

pub fn lex_scope_selector(input: OwnedSpan) -> Res<OwnedSpan, LexScopeSelector<OwnedSpan>> {
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
                    recognize(any_soround_lex_block),
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
            recognize(any_soround_lex_block),
            recognize(verify(anychar, move |c| *c != kind.close_as_char())),
        ))))(input)
    }
}

pub fn scope_filters<'a>(input: OwnedSpan) -> Res<OwnedSpan, ScopeFiltersDef<OwnedSpan>> {
    tuple((
        pair(opt(scope_filter), many0(preceded(tag("-"), scope_filter))),
        opt(recognize(pair(
            peek(tag("/")),
            context("regex", cut(path_regex)),
        ))),
    ))(input)
    .map(|(next, ((first, mut many_filters), path))| {
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

pub fn scope_filter(input: OwnedSpan) -> Res<OwnedSpan, ScopeFilterDef<OwnedSpan>> {
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

pub fn scope_name(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(pair(skewer_case, peek(alt((eof, multispace1, tag(")"))))))(input)
}

pub fn root_scope_selector<'a>(
    input: OwnedSpan,
) -> Res<OwnedSpan, RootScopeSelector<OwnedSpan, Spanned<OwnedSpan, Version>>> {
    context(
        "root-scope-selector",
        cut(preceded(
            multispace0,
            pair(
                context("root-scope-selector:name", cut(root_scope_selector_name)),
                context("root-scope-selector:version", cut(scope_version)),
            ),
        )),
    )(input)
    .map(|(next, (name, version))| (next, RootScopeSelector { version, name }))
}

pub fn scope_version(input: OwnedSpan) -> Res<OwnedSpan, Spanned<OwnedSpan, Version>> {
    context(
        "scope-selector-version",
        tuple((
            tag("(version="),
            sub(version),
            context("scope-selector-version-closing-tag", tag(")")),
        )),
    )(input)
    .map(|((next, (_, version, _)))| (next, version))
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

pub fn scope_selector_name(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
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

pub fn root_scope_selector_name(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    context(
        "root-scope-selector-name",
        pair((peek(alpha1)), alphanumeric1),
    )(input)
    .map(|(next, (_, name))| (next, name))
}

pub fn lex_root_scope(span: OwnedSpan) -> Result<LexRootScope<OwnedSpan>, MsgErr> {
    let root_scope = result(delimited(multispace0, root_scope, multispace0)(span))?;
    Ok(root_scope)
}

pub mod model {
    use crate::error::{MsgErr, ParseErrs};
    use crate::version::v0_0_1::config::config::bind::{
        BindConfig, MessageKind, PipelineStepCtx, PipelineStepDef, PipelineStopCtx,
        PipelineStopDef, Selector,
    };
    use crate::version::v0_0_1::entity::entity::EntityKind;
    use crate::version::v0_0_1::id::id::{Point, PointCtx, Version};
    use crate::version::v0_0_1::parse::error::result;
    use crate::version::v0_0_1::parse::{camel_case, CtxResolver, CtxSubst, lex_child_scopes, pipeline, PipelineStepCtxParser, PipelineStopCtxParser, SubstParser, value_pattern, var_pipeline, VarResolver, VarSubst};
    use crate::version::v0_0_1::util::{MethodPattern, StringMatcher, ValuePattern};
    use bincode::Options;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alphanumeric1, multispace0, multispace1, satisfy};
    use nom::combinator::{cut, fail, not, peek, recognize, value};
    use regex::Regex;
    use serde::de::Visitor;
    use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
    use std::collections::HashMap;
    use std::fmt::{Formatter, Write};
    use std::marker::PhantomData;
    use std::ops::{Deref, DerefMut};
    use std::str::FromStr;
    use crate::version::v0_0_1::span::{new_span, OwnedSpan, SpanHistory, SpanData, SpanRevisionKind, SuperSpanBuilder, SuperSpan, SpanErrKind};

    #[derive(Clone)]
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

    #[derive(Clone)]
    pub struct ScopeSelectorDef<N, I> {
        pub name: N,
        pub filters: ScopeFiltersDef<I>,
    }

    impl<N, I> ScopeSelectorDef<N, I> {
        pub fn new(name: N, filters: ScopeFiltersDef<I>) -> Self {
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

    impl<N: ToString, I: ToString> ScopeSelectorDef<N, I> {
        fn to_scope_selector(self) -> ScopeSelector {
            ScopeSelector {
                name: self.name.to_string(),
                filters: self.filters.to_scope_filters(),
            }
        }
    }

    #[derive(Clone)]
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

    #[derive(Clone)]
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

    pub type RegexStr = String;
    pub type ScopeFilter = ScopeFilterDef<String>;
    pub type ScopeFilters = ScopeFiltersDef<String>;
    pub type LexBlock<I> = Block<I, ()>;
    pub type LexRootScope<I> = Scope<RootScopeSelector<I, Spanned<I, Version>>, Block<I, ()>, I>;
    pub type LexScope<I> =
        Scope<ScopeSelectorAndFiltersDef<LexScopeSelector<I>, I>, Block<I, ()>, I>;
    pub type LexParentScope<'a> =
        Scope<LexScopeSelectorAndFilters<OwnedSpan>, Vec<LexScope<OwnedSpan>>, OwnedSpan>;

    pub type VarPipelineSegment = VarPipelineSegmentDef<
        Subst<PipelineStepCtx,OwnedSpan, PipelineStepCtxParser>,
        Option<Subst<PipelineStopCtx,OwnedSpan, PipelineStopCtxParser>>,
    >;
    pub type VarPipeline = PipelineDef<VarPipelineSegment>;
    pub type LexPipelineScope<I> = PipelineScopeDef<I, VarPipeline>;
    pub type PipelineSegmentCtx = PipelineSegmentDef<PointCtx>;

    impl CtxSubst<PipelineSegment> for PipelineSegmentCtx{
        fn resolve_ctx(self, resolver: &dyn CtxResolver) -> Result<PipelineSegment, MsgErr> {
            let mut errs = vec![];
            let step = match self.step.resolve_ctx(resolver) {
                Ok(step) => Some(step),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };
            let stop = match self.stop.resolve_ctx(resolver) {
                Ok(stop) => Some(stop),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };
            if errs.is_empty() {
                Ok(PipelineSegment {
                    step: step.expect("step"),
                    stop: stop.expect("stop")
                })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }


    pub type PipelineSegment = PipelineSegmentDef<Point>;
    pub type RequestScope = PipelineScopeDef<String, Vec<MessageScope>>;
    pub type MessageScope = ScopeDef<ValuePatternScopeSelectorAndFilters, Vec<MethodScope>>;
    pub type MethodScope = ScopeDef<ValuePatternScopeSelectorAndFilters, VarPipeline>;
    pub type ScopeSelector = ScopeSelectorDef<String, String>;
    pub type ValuePatternScopeSelector = ScopeSelectorDef<ValuePattern<String>, String>;
    pub type ScopeSelectorAndFilters = ScopeSelectorAndFiltersDef<ScopeSelector, String>;
    pub type ValuePatternScopeSelectorAndFilters =
        ScopeSelectorAndFiltersDef<ValuePatternScopeSelector, String>;
    pub type LexScopeSelectorAndFilters<I> = ScopeSelectorAndFiltersDef<LexScopeSelector<I>, I>;
    //    pub type Pipeline = Vec<PipelineSegment>;

    impl<'a> TryFrom<LexParentScope<'a>> for RequestScope {
        type Error = MsgErr;

        fn try_from(scope: LexParentScope) -> Result<Self, Self::Error> {
            let mut errs = vec![];
            let mut message_scopes = vec![];
            for message_scope in scope.block {
                match lex_child_scopes(message_scope) {
                    Ok(message_scope) => {
                        let mut block = vec![];
                        for method_scope in message_scope.block {
                            match method_scope.selector.to_value_pattern_scope_selector() {
                                Ok(selector) => {
                                    match result(var_pipeline(method_scope.block.content)) {
                                        Ok(pipeline) => block.push(MethodScope {
                                            selector,
                                            block: pipeline,
                                        }),
                                        Err(err) => {
                                            errs.push(err);
                                        }
                                    }
                                }
                                Err(err) => {
                                    errs.push(err);
                                }
                            }
                        }
                        match message_scope.selector.to_value_pattern_scope_selector() {
                            Ok(selector) => message_scopes.push(MessageScope { selector, block }),
                            Err(err) => {
                                errs.push(err);
                            }
                        };
                    }
                    Err(err) => {
                        errs.push(err);
                    }
                }
            }
            if errs.is_empty() {
                Ok(RequestScope {
                    selector: scope.selector.to_scope_selector(),
                    block: message_scopes,
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

    impl<'a> LexScopeSelectorAndFilters<OwnedSpan> {
        pub fn to_value_pattern_scope_selector(
            self,
        ) -> Result<ValuePatternScopeSelectorAndFilters, MsgErr> {
            Ok(ValuePatternScopeSelectorAndFilters {
                selector: self.selector.to_value_pattern_scope_selector()?,
                filters: self.filters.to_scope_filters(),
            })
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct VarPipelineSegmentDef<Step, Stop> {
        pub step: Step,
        pub stop: Stop,
    }


    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PipelineSegmentDef<Pnt> {
        pub step: PipelineStepDef<Pnt>,
        pub stop: PipelineStopDef<Pnt>,
    }

    pub type Pipeline = PipelineDef<PipelineSegment>;
    pub type PipelineCtx = PipelineDef<PipelineSegmentCtx>;


    impl CtxSubst<Pipeline> for PipelineCtx {
        fn resolve_ctx(self, resolver: &dyn CtxResolver) -> Result<Pipeline, MsgErr> {
            let mut errs = vec![];
            let mut segments = vec![];
            for segment in self.segments {
                match segment.resolve_ctx(resolver) {
                    Ok(segment) => segments.push(segment),
                    Err(err) => errs.push(err)
                }
            }
            if errs.is_empty() {
                Ok( Pipeline { segments })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub struct PipelineDef<S> {
        pub segments: Vec<S>,
    }

    impl <S> PipelineDef<S> {
        pub fn new() -> Self {
            Self {
                segments: vec![]
            }
        }
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

    impl VarSubst<PipelineCtx> for VarPipeline {
        fn resolve_vars(self, resolver: &dyn VarResolver) -> Result<PipelineCtx, MsgErr> {
            let mut pipeline = PipelineCtx::new();
            let mut errs = vec![];
            for segment in self.segments {
                match segment.resolve_vars(resolver) {
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

    impl VarSubst<PipelineSegmentCtx> for VarPipelineSegment {
        fn resolve_vars(self, resolver: &dyn VarResolver) -> Result<PipelineSegmentCtx, MsgErr> {
            let mut errs = vec![];

            if self.stop.is_none() {
                errs.push(ParseErrs::from_owned_span(
                    "expecting Pipeline Stop to follow Pipeline Step",
                    "Needs a following Pipeline Stop",
                    self.step.span(),
                ));
            }

            let step = match self.step.resolve(resolver) {
                Ok(step) => Some(step),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };

            let stop = match self.stop {
                Some(stop) => match stop.resolve(resolver) {
                    Ok(stop) => Some(stop),
                    Err(err) => {
                        errs.push(err);
                        None
                    }
                },
                None => None,
            };

            if step.is_some() && stop.is_some() && errs.is_empty() {
                let step = step.expect("step");
                let stop = stop.expect("stop");
                Ok(PipelineSegmentCtx { step, stop })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }

    #[derive(Clone)]
    pub enum BindScope {
        RequestScope(RequestScope),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ScopeDef<S, B> {
        pub selector: S,
        pub block: B,
    }

    #[derive(Clone)]
    pub struct PipelineScopeDef<I, P> {
        pub selector: ScopeSelectorAndFiltersDef<ScopeSelectorDef<I, I>, I>,
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

    impl<'a> LexScopeSelector<OwnedSpan> {
        pub fn to_value_pattern_scope_selector(self) -> Result<ValuePatternScopeSelector, MsgErr> {
            Ok(ValuePatternScopeSelector {
                name: result(value_pattern(camel_case)(self.name))?.stringify(),
                filters: self.filters.to_scope_filters(),
            })
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

        pub fn error_message(span: &OwnedSpan, context: &str) -> Result<&'static str, ()> {
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

    #[derive(Debug,Clone,Serialize,Deserialize)]
    pub enum Chunk<I> {
        Var(I),
        Text(I),
    }

    impl <I> Chunk<I> {
        pub fn span(&self) -> &I {
            match self {
                Chunk::Var(var) => var,
                Chunk::Text(text) => text
            }
        }
    }


    impl<I:ToString> Chunk<I> {
        pub fn len(&self) -> usize {
            match self {
                Chunk::Var(var) => {
                    // account for ${}
                    var.to_string().len() + 3
                }
                Chunk::Text(text) => text.to_string().len(),
            }
        }
    }

    #[derive(Clone)]
    pub enum Var<O,P> where P:VarParser<O>{
        Val(O),
        Var{ name: String, parser: P }
    }

    pub trait VarParser<O> {
        fn parse( input: OwnedSpan ) -> Result<O,MsgErr>;
    }


    #[derive(Clone)]
    pub struct Subst<R,I,P>
    where
        P: SubstParser<R> + Clone,
    {
        pub chunks: Vec<Chunk<I>>,
        pub parser: P,
        pub span: OwnedSpan,
        pub phantom: PhantomData<R>,
    }



    impl<R,I,P> ToString for Subst<R,I,P>
    where
        P: SubstParser<R> + Clone, I: ToString
    {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            for chunk in &self.chunks {
                match chunk {
                    Chunk::Var(var) => {
                        rtn.push_str(format!("${{{}}}", var.to_string()).as_str());
                    }
                    Chunk::Text(text) => {
                        rtn.push_str(text.to_string().as_str());
                    }
                }
            }
            rtn
        }
    }

    impl<R,I: Clone, P> Subst<R,I,P>
    where
        P: SubstParser<R> + Clone,
    {
        pub fn span(&self) -> OwnedSpan {
            self.span.clone()
        }
    }

    /*
    impl<R, I: ToString, P> VarSubst<R> for Subst<R, I, P>
    where
        P: SubstParser<R> + Clone,
    {
        fn resolve_vars(self, resolver: &dyn VarResolver) -> Result<R, MsgErr> {
            let string = self.resolve(resolver)?;
            self.parser.parse_string(string)
        }
    }

     */

    impl<'a,R,P> Subst<R,OwnedSpan,P>
        where
            P: SubstParser<R> + Clone
    {
        fn resolve(&self, resolver: &dyn VarResolver) -> Result<R, MsgErr> {
            let mut span_builder = SuperSpanBuilder::new(self.span());
            for chunk in &self.chunks {
                let mut history_builder = span_builder.original(&chunk.span());
                match chunk {
                    Chunk::Var(var) => {
                        match resolver.val( var.to_string().as_str() ) {
                            Ok(val) => {
                                history_builder.revise(SpanRevisionKind::VarSubst(var.to_string()), val.as_str() );
                            }
                            Err(err) => {
                                history_builder.err(SpanErrKind::VarNotFound(var.to_string()));
                            }
                        }
                    }
                    Chunk::Text(text) => {
                        // nothing since it's already been pushed in the original and is not revised
                    }
                }
            }
            let super_span = span_builder.build();

            if super_span.errs.is_empty() {
                result(self.parser.parse_span(super_span.span.clone() ))
            } else {
                Err(ParseErrs::fold(super_span.errs).into())
            }


        }
    }

}

pub mod error {
    use crate::error::{MsgErr, ParseErrs};
    use crate::version::v0_0_1::parse::model::NestedBlockKind;
    use crate::version::v0_0_1::parse::nospace1;
    use crate::version::v0_0_1::span::OwnedSpan;
    use ariadne::Report;
    use ariadne::{Label, ReportKind, Source};
    use nom::{Err, Slice};
    use nom_supreme::error::{BaseErrorKind, ErrorTree, StackContext};
    use regex::{Error, Regex};

    pub fn result<R>(result: Result<(OwnedSpan, R), Err<ErrorTree<OwnedSpan>>>) -> Result<R, MsgErr> {
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

    fn create_err_report(context: &str, loc: OwnedSpan) -> MsgErr {
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

            "regex" => {
                        let span = result(nospace1(loc.clone()));
                        match span {
                            Ok(span) => {
                                match Regex::new(loc.to_string().as_str()) {
                                    Ok(_) => {
                                        builder.with_message("internal parse error: regex error in this expression")
                                    }
                                    Err(err) => {
                                        match err {
                                            Error::Syntax(syntax) => {
                                                builder.with_message(format!("Regex Syntax Error: '{}'",syntax)).with_label(Label::new(span.location_offset()..span.location_offset()+span.len()).with_message("regex syntax error"))
                                            }
                                            Error::CompiledTooBig(size) => {
                                                builder.with_message("Regex compiled too big").with_label(Label::new(span.location_offset()..span.location_offset()+span.len()).with_message("regex compiled too big"))
                                            }
                                            Error::__Nonexhaustive => {
                                                builder.with_message("Regex is nonexhaustive").with_label(Label::new(span.location_offset()..span.location_offset()+span.len()).with_message("non-exhaustive regex"))
                                            }
                                        }
                                    }
                                }
                            }
                    Err(_) => {
                        builder.with_message("internal parse error: could not identify regex")
                    }
                }
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
                    builder.with_message("variable name must be alphanumeric lowercase, dashes and dots.  Variables are preceded by the '$' operator and must be sorounded by curly brackets ${env.valid-variable-name}")
                },
            "variable:close" => {
                builder.with_message("variable name must be alphanumeric lowercase, dashes and dots.  Variables are preceded by the '$' operator and must be sorounded by curly brackets with no spaces ${env.valid-variable-name}").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Bad Variable Substitution"))
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

    pub fn find_parse_err(err: &Err<ErrorTree<OwnedSpan>>) -> MsgErr {
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

    pub fn find_tree(err: &ErrorTree<OwnedSpan>) -> MsgErr {
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

use ariadne::{Label, Report, ReportKind};
use std::convert::{TryFrom, TryInto};
use std::marker::PhantomData;
use std::sync::Arc;

use nom::branch::alt;
use nom::character::complete::{alpha1, digit1};
use nom::combinator::{all_consuming, opt};
use nom::error::{context, ContextError, ErrorKind, ParseError, VerboseError};
use nom::multi::{many0, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{
    AsChar, Compare, FindToken, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset,
    Parser, Slice, UnspecializedInput,
};
use nom::{Err, IResult};
use nom_locate::LocatedSpan;

use crate::version::v0_0_1::config::config::bind::{
    BindConfig, MessageKind, Pipeline, PipelineStep, PipelineStepCtx, PipelineStop,
    PipelineStopCtx, Selector,
};
use crate::version::v0_0_1::config::config::Document;
use crate::version::v0_0_1::entity::entity::request::RcCommandType;
use crate::version::v0_0_1::id::id::{GenericKind, GenericKindBase, PointKind, PointSeg, Specific};
use crate::version::v0_0_1::parse::error::result;
use crate::version::v0_0_1::parse::model::{BindScope, BindScopeKind, Block, BlockKind, Chunk, DelimitedBlockKind, LexBlock, LexParentScope, LexRootScope, LexScope, LexScopeSelector, LexScopeSelectorAndFilters, NestedBlockKind, PipelineCtx, PipelineSegment, PipelineSegmentCtx, RequestScope, RootScopeSelector, ScopeFilterDef, ScopeFiltersDef, ScopeSelectorAndFiltersDef, Spanned, Subst, TerminatedBlockKind, TextType, Var, VarParser, VarPipeline, VarPipelineSegment};
use crate::version::v0_0_1::payload::payload::{Call, CallCtx, CallKind, CallWithConfig, CallWithConfigCtx, HttpCall, HttpMethod, HttpMethodType, ListPattern, MapPattern, MapPatternCtx, MsgCall, NumRange, PayloadFormat, PayloadPattern, PayloadPatternCtx, PayloadType, PayloadTypePatternCtx, PayloadTypePatternDef};
use crate::version::v0_0_1::selector::selector::specific::{
    ProductSelector, VariantSelector, VendorSelector,
};
use crate::version::v0_0_1::selector::selector::{
    ExactPointSeg, GenericKindSelector, GenericSubKindSelector, Hop, HttpPipelineSelector,
    KindPattern, LabeledPrimitiveTypeDef, MapEntryPattern, MsgPipelineSelector, Pattern,
    PayloadTypeDef, PipelineSelector, PointSegSelector, PointSelector, RcPipelineSelector,
    SpecificSelector, VersionReq,
};
use crate::version::v0_0_1::selector::{
    PatternBlock, PatternBlockCtx, PayloadBlock, PayloadBlockCtx, UploadBlock,
};
use nom_supreme::error::ErrorTree;
use nom_supreme::{parse_from_str, ParserExt};
use crate::version::v0_0_1::span::{new_span, OwnedSpan, SpanHistory, span_with_extra};

fn inclusive_any_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    alt((tag("+*"), tag("ROOT+*")))(input).map(|(next, _)| (next, PointSegSelector::InclusiveAny))
}

fn inclusive_recursive_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    alt((tag("+**"), tag("ROOT+**")))(input)
        .map(|(next, _)| (next, PointSegSelector::InclusiveRecursive))
}

fn any_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    tag("*")(input).map(|(next, _)| (next, PointSegSelector::Any))
}

fn recursive_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    tag("**")(input).map(|(next, _)| (next, PointSegSelector::Recursive))
}

fn exact_space_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    point_segment_chars(input).map(|(next, segment)| {
        (
            next,
            PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::Space(
                segment.to_string(),
            ))),
        )
    })
}

fn exact_base_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    point_segment_chars(input).map(|(next, segment)| {
        (
            next,
            PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::Base(segment.to_string()))),
        )
    })
}

fn exact_file_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    file_chars(input).map(|(next, segment)| {
        (
            next,
            PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::File(segment.to_string()))),
        )
    })
}

fn exact_dir_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    file_chars(input).map(|(next, segment)| {
        (
            next,
            PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::Dir(segment.to_string()))),
        )
    })
}

pub fn parse_version_chars_str<O: FromStr>(input: OwnedSpan) -> Res<OwnedSpan, O> {
    let (next, rtn) = recognize(version_chars)(input)?;
    match O::from_str(rtn.to_string().as_str()) {
        Ok(rtn) => Ok((next, rtn)),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            next,
            ErrorKind::Fail,
        ))),
    }
}

fn exact_version_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    version_req(input).map(|(next, version_req)| (next, PointSegSelector::Version(version_req)))
}

fn version_req_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    delimited(tag("("), version_req, tag(")"))(input)
        .map(|(next, version_req)| (next, PointSegSelector::Version(version_req)))
}

pub fn point_segment_selector(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    alt((
        inclusive_recursive_segment,
        inclusive_any_segment,
        recursive_segment,
        any_segment,
        exact_space_segment,
    ))(input)
}

fn base_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    alt((recursive_segment, any_segment, exact_base_segment))(input)
}

fn file_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    alt((recursive_segment, any_segment, exact_file_segment))(input)
}

fn dir_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    terminated(
        alt((recursive_segment, any_segment, exact_dir_segment)),
        tag("/"),
    )(input)
}

fn dir_segment_meat(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
    alt((recursive_segment, any_segment, exact_dir_segment))(input)
}

fn version_segment(input: OwnedSpan) -> Res<OwnedSpan, PointSegSelector> {
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

pub fn pattern<O, E: ParseError<OwnedSpan>, V>(
    mut value: V,
) -> impl FnMut(OwnedSpan) -> IResult<OwnedSpan, Pattern<O>, E>
where
    V: Parser<OwnedSpan, O, E>,
{
    move |input: OwnedSpan| {
        let x: Res<OwnedSpan, OwnedSpan> = tag("*")(input.clone());
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

pub fn version_req(input: OwnedSpan) -> Res<OwnedSpan, VersionReq> {
    let (next, version) = version_req_chars(input.clone())?;
    let version = version.to_string();
    let str_input = version.as_str();
    let rtn = semver::VersionReq::parse(str_input);

    match rtn {
        Ok(version) => Ok((next, VersionReq { version })),
        Err(err) => {
            let tree = Err::Error(ErrorTree::from_error_kind(input, ErrorKind::Fail));
            Err(tree)
        }
    }
}

fn rec_domain(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(tuple((
        many1(terminated(skewer_chars, tag("."))),
        skewer_chars,
    )))(input)
}

// can be a hostname or domain name
fn space(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(alt((skewer_chars, rec_domain)))(input)
}

pub fn specific_selector(input: OwnedSpan) -> Res<OwnedSpan, SpecificSelector> {
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
        let vendor: Pattern<OwnedSpan> = vendor;
        let product: Pattern<OwnedSpan> = product;
        let variant: Pattern<OwnedSpan> = variant;

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

pub fn rec_domain_pattern(input: OwnedSpan) -> Res<OwnedSpan, Pattern<OwnedSpan>> {
    pattern(rec_domain)(input)
}
pub fn rec_skewer_pattern(input: OwnedSpan) -> Res<OwnedSpan, Pattern<OwnedSpan>> {
    pattern(skewer_chars)(input)
}

pub fn specific_version_req(input: OwnedSpan) -> Res<OwnedSpan, VersionReq> {
    delimited(tag("("), version_req, tag(")"))(input)
}

#[derive(Clone)]
pub struct SkewerPatternParser();
impl SubstParser<Pattern<String>> for SkewerPatternParser {
    fn parse_span<'a>(&self, span: OwnedSpan) -> Res<OwnedSpan, Pattern<String>> {
        let (next, pattern) = rec_skewer_pattern(span)?;
        let pattern = pattern.into();
        Ok((next, pattern))
    }
}

#[derive(Clone)]
pub struct DomainPatternParser();
impl SubstParser<Pattern<String>> for DomainPatternParser {
    fn parse_span<'a>(&self, span: OwnedSpan) -> Res<OwnedSpan, Pattern<String>> {
        let (next, pattern) = rec_domain_pattern(span)?;
        let pattern = pattern.into();
        Ok((next, pattern))
    }
}

fn kind_parts(input: OwnedSpan) -> Res<OwnedSpan, GenericKind> {
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
            kind: resource_type,
            sub_kind: None,
            specific: None,
        };

        match more {
            Some((kind, specific)) => {
                parts.sub_kind = Option::Some(kind.to_string());
                parts.specific = specific;
            }
            None => {}
        }

        (next, parts)
    })
}

fn rec_kind(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(kind_parts)(input)
}

pub fn kind(input: OwnedSpan) -> Res<OwnedSpan, GenericKind> {
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
            kind: resource_type,
            sub_kind: Option::None,
            specific: Option::None,
        };

        match rest {
            Some((kind, specific)) => {
                rtn.sub_kind = Option::Some(kind.to_string());
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

pub fn delim_kind(input: OwnedSpan) -> Res<OwnedSpan, GenericKind> {
    delimited(tag("<"), kind, tag(">"))(input)
}

pub fn consume_kind(input: OwnedSpan) -> Result<GenericKind, MsgErr> {
    let (_, kind_parts) = all_consuming(kind_parts)(input)?;

    Ok(kind_parts.try_into()?)
}

pub fn generic_kind_selector(input: OwnedSpan) -> Res<OwnedSpan, GenericSubKindSelector> {
    pattern(kind)(input).map(|(next, kind)| (next, kind))
}

pub fn generic_kind_base(input: OwnedSpan) -> Res<OwnedSpan, GenericKindBase> {
    camel_case(input).map(|(next, resource_type)| (next, resource_type.to_string()))
}

pub fn generic_kind_base_selector(input: OwnedSpan) -> Res<OwnedSpan, GenericKindSelector> {
    pattern(generic_kind_base)(input)
}

pub fn kind_pattern(input: OwnedSpan) -> Res<OwnedSpan, KindPattern> {
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

        let tks = KindPattern {
            kind: resource_type,
            sub_kind: kind,
            specific,
        };

        (next, tks)
    })
}

fn space_hop(input: OwnedSpan) -> Res<OwnedSpan, Hop> {
    tuple((point_segment_selector, opt(kind_pattern), opt(tag("+"))))(input).map(
        |(next, (segment, kind, inclusive))| {
            let kind = match kind {
                None => KindPattern::any(),
                Some(tks) => tks,
            };
            let inclusive = inclusive.is_some();
            (
                next,
                Hop {
                    inclusive,
                    segment,
                    kind,
                },
            )
        },
    )
}

fn base_hop(input: OwnedSpan) -> Res<OwnedSpan, Hop> {
    tuple((base_segment, opt(kind_pattern), opt(tag("+"))))(input).map(
        |(next, (segment, tks, inclusive))| {
            let tks = match tks {
                None => KindPattern::any(),
                Some(tks) => tks,
            };
            let inclusive = inclusive.is_some();
            (
                next,
                Hop {
                    inclusive,
                    segment,
                    kind: tks,
                },
            )
        },
    )
}

fn file_hop(input: OwnedSpan) -> Res<OwnedSpan, Hop> {
    tuple((file_segment, opt(tag("+"))))(input).map(|(next, (segment, inclusive))| {
        let tks = KindPattern {
            kind: Pattern::Exact("File".to_string()),
            sub_kind: Pattern::Any,
            specific: ValuePattern::Any,
        };
        let inclusive = inclusive.is_some();
        (
            next,
            Hop {
                inclusive,
                segment,
                kind: tks,
            },
        )
    })
}

fn dir_hop(input: OwnedSpan) -> Res<OwnedSpan, Hop> {
    tuple((dir_segment, opt(tag("+"))))(input).map(|(next, (segment, inclusive))| {
        let tks = KindPattern::any();
        let inclusive = inclusive.is_some();
        (
            next,
            Hop {
                inclusive,
                segment,
                kind: tks,
            },
        )
    })
}

fn version_hop(input: OwnedSpan) -> Res<OwnedSpan, Hop> {
    tuple((version_segment, opt(kind_pattern), opt(tag("+"))))(input).map(
        |(next, (segment, tks, inclusive))| {
            let tks = match tks {
                None => KindPattern::any(),
                Some(tks) => tks,
            };
            let inclusive = inclusive.is_some();
            (
                next,
                Hop {
                    inclusive,
                    segment,
                    kind: tks,
                },
            )
        },
    )
}

pub fn point_selector(input: OwnedSpan) -> Res<OwnedSpan, PointSelector> {
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
                    kind: KindPattern {
                        kind: Pattern::Exact("Dir".to_string()),
                        sub_kind: Pattern::Any,
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

pub fn point_and_kind(input: OwnedSpan) -> Res<OwnedSpan, PointKind> {
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

pub fn version(input: OwnedSpan) -> Res<OwnedSpan, Version> {
    let (next, version) = rec_version(input.clone())?;
    let version = version.to_string();
    let str_input = version.as_str();
    let rtn = semver::Version::parse(str_input);

    match rtn {
        Ok(version) => Ok((next, Version { version })),
        Err(err) => {
            let tree = Err::Error(ErrorTree::from_error_kind(input, ErrorKind::Fail));
            Err(tree)
        }
    }
}

pub fn specific(input: OwnedSpan) -> Res<OwnedSpan, Specific> {
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
//}

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

pub fn primitive_def(input: OwnedSpan) -> Res<OwnedSpan, PayloadTypeDef<PointCtx>> {
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

pub fn payload(input: OwnedSpan) -> Res<OwnedSpan, PayloadType> {
    parse_camel_case_str(input)
}

pub fn consume_primitive_def(input: OwnedSpan) -> Res<OwnedSpan, PayloadTypeDef<PointCtx>> {
    all_consuming(primitive_def)(input)
}

pub fn call_with_config(input: OwnedSpan) -> Res<OwnedSpan, CallWithConfigCtx> {
    tuple((call, opt(preceded(tag("+"), point_ctx))))(input)
        .map(|(next, (call, config))| (next, CallWithConfigCtx { call, config }))
}

pub fn parse_alpha1_str<O: FromStr>(input: OwnedSpan) -> Res<OwnedSpan, O> {
    let (next, rtn) = recognize(alpha1)(input)?;
    match O::from_str(rtn.to_string().as_str()) {
        Ok(rtn) => Ok((next, rtn)),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            next,
            ErrorKind::Fail,
        ))),
    }
}

pub fn rc_command(input: OwnedSpan) -> Res<OwnedSpan, RcCommandType> {
    parse_alpha1_str(input)
}

pub fn msg_call(input: OwnedSpan) -> Res<OwnedSpan, CallKind> {
    tuple((
        delimited(tag("Msg<"), alphanumeric1, tag(">")),
        opt(recognize(capture_path)),
    ))(input)
    .map(|(next, (action, path))| {
        let path = match path {
            None => new_span("/"),
            Some(path) => path,
        };
        (
            next,
            CallKind::Msg(MsgCall::new(action.to_string(), path.to_string())),
        )
    })
}

pub fn http_call(input: OwnedSpan) -> Res<OwnedSpan, CallKind> {
    tuple((delimited(tag("Http<"), http_method, tag(">")), capture_path))(input).map(
        |(next, (method, path))| {
            (
                next,
                CallKind::Http(HttpCall::new(method, path.to_string())),
            )
        },
    )
}

pub fn call_kind(input: OwnedSpan) -> Res<OwnedSpan, CallKind> {
    alt((msg_call, http_call))(input)
}

pub fn call(input: OwnedSpan) -> Res<OwnedSpan, CallCtx> {
    tuple((point_ctx, preceded(tag("^"), call_kind)))(input)
        .map(|(next, (point, kind))| (next, CallCtx { point, kind }))
}

pub fn consume_call(input: OwnedSpan) -> Res<OwnedSpan, CallCtx> {
    all_consuming(call)(input)
}

pub fn labeled_primitive_def(input: OwnedSpan) -> Res<OwnedSpan, LabeledPrimitiveTypeDef<PointCtx>> {
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

pub fn digit_range(input: OwnedSpan) -> Res<OwnedSpan, NumRange> {
    tuple((digit1, tag("-"), digit1))(input).map(|(next, (min, _, max))| {
        let min: usize = usize::from_str(min.to_string().as_str()).expect("usize");
        let max: usize = usize::from_str(max.to_string().as_str()).expect("usize");
        let range = NumRange::MinMax { min, max };

        (next, range)
    })
}

pub fn exact_range(input: OwnedSpan) -> Res<OwnedSpan, NumRange> {
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

pub fn range(input: OwnedSpan) -> Res<OwnedSpan, NumRange> {
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

pub fn primitive_data_struct(input: OwnedSpan) -> Res<OwnedSpan, PayloadTypePatternDef<PointCtx>> {
    context("selector", payload)(input)
        .map(|(next, primitive)| (next, PayloadTypePatternDef::Primitive(primitive)))
}

pub fn array_data_struct(input: OwnedSpan) -> Res<OwnedSpan, PayloadTypePatternDef<PointCtx>> {
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
            PayloadTypePatternDef::List(ListPattern { primitive, range }),
        )
    })
}

pub fn map_entry_pattern_any(input: OwnedSpan) -> Res<OwnedSpan, ValuePattern<MapEntryPatternCtx>> {
    delimited(multispace0, tag("*"), multispace0)(input).map(|(next, _)| (next, ValuePattern::Any))
}

pub fn map_entry_pattern(input: OwnedSpan) -> Res<OwnedSpan, MapEntryPatternCtx> {
    tuple((skewer, opt(delimited(tag("<"), payload_pattern, tag(">")))))(input).map(
        |(next, (key_con, payload_con))| {
            let payload_con = match payload_con {
                None => ValuePattern::Any,
                Some(payload_con) => payload_con,
            };

            let map_entry_con = MapEntryPatternCtx {
                key: key_con.to_string(),
                payload: payload_con,
            };
            (next, map_entry_con)
        },
    )
}

pub fn map_entry_patterns(input: OwnedSpan) -> Res<OwnedSpan, Vec<MapEntryPatternCtx>> {
    separated_list0(
        delimited(multispace0, tag(","), multispace0),
        map_entry_pattern,
    )(input)
}

pub fn consume_map_entry_pattern(input: OwnedSpan) -> Res<OwnedSpan, MapEntryPatternCtx> {
    all_consuming(map_entry_pattern)(input)
}

pub fn required_map_entry_pattern(input: OwnedSpan) -> Res<OwnedSpan, Vec<MapEntryPatternCtx>> {
    delimited(tag("["), map_entry_patterns, tag("]"))(input).map(|(next, params)| (next, params))
}

pub fn allowed_map_entry_pattern(input: OwnedSpan) -> Res<OwnedSpan, ValuePattern<PayloadPatternCtx>> {
    payload_pattern(input).map(|(next, con)| (next, con))
}

//  [ required1<Bin>, required2<Text> ] *<Bin>
pub fn map_pattern_params(input: OwnedSpan) -> Res<OwnedSpan, MapPatternCtx> {
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

        let con = MapPatternCtx::new(required_map, allowed);

        (next, con)
    })
}

pub fn format(input: OwnedSpan) -> Res<OwnedSpan, PayloadFormat> {
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
pub fn map_pattern(input: OwnedSpan) -> Res<OwnedSpan, MapPatternCtx> {
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
            None => MapPatternCtx::any(),
            Some(con) => con,
        };

        (next, con)
    })
}

pub fn value_constrained_map_pattern(input: OwnedSpan) -> Res<OwnedSpan, ValuePattern<MapPatternCtx>> {
    value_pattern(map_pattern)(input)
}

pub fn msg_action(input: OwnedSpan) -> Res<OwnedSpan, ValuePattern<StringMatcher>> {
    value_pattern(camel_case_to_string_matcher)(input)
}

pub fn msg_pattern_scoped(input: OwnedSpan) -> Res<OwnedSpan, MsgPipelineSelector> {
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

pub fn msg_pattern(input: OwnedSpan) -> Res<OwnedSpan, MsgPipelineSelector> {
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

pub fn parse_camel_case_str<O: FromStr>(input: OwnedSpan) -> Res<OwnedSpan, O> {
    let (next, rtn) = recognize(camel_case)(input)?;
    match O::from_str(rtn.to_string().as_str()) {
        Ok(rtn) => Ok((next, rtn)),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            next,
            ErrorKind::Fail,
        ))),
    }
}

pub fn http_method(input: OwnedSpan) -> Res<OwnedSpan, HttpMethod> {
    context("http_method", parse_camel_case_str)
        .parse(input)
        .map(|(next, method): (OwnedSpan, HttpMethodType)| (next, method.to_method()))
}

pub fn http_method_pattern(input: OwnedSpan) -> Res<OwnedSpan, MethodPattern> {
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

pub fn http_pattern_scoped(input: OwnedSpan) -> Res<OwnedSpan, HttpPipelineSelector> {
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

pub fn http_pattern(input: OwnedSpan) -> Res<OwnedSpan, HttpPipelineSelector> {
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

pub fn rc_command_type(input: OwnedSpan) -> Res<OwnedSpan, RcCommandType> {
    parse_alpha1_str(input)
}

pub fn rc_pattern_scoped(input: OwnedSpan) -> Res<OwnedSpan, RcPipelineSelector> {
    pattern(delimited(tag("<"), rc_command_type, tag(">")))(input)
        .map(|(next, command)| (next, RcPipelineSelector { command }))
}

pub fn rc_pattern(input: OwnedSpan) -> Res<OwnedSpan, RcPipelineSelector> {
    tuple((tag("Rc"), delimited(tag("<"), rc_pattern_scoped, tag(">"))))(input)
        .map(|(next, (_, pattern))| (next, pattern))
}

pub fn map_pattern_payload_structure(input: OwnedSpan) -> Res<OwnedSpan, PayloadTypePatternDef<PointCtx>> {
    map_pattern(input).map(|(next, con)| (next, PayloadTypePatternDef::Map(Box::new(con))))
}

pub fn payload_structure(input: OwnedSpan) -> Res<OwnedSpan, PayloadTypePatternDef<PointCtx>> {
    alt((
        array_data_struct,
        primitive_data_struct,
        map_pattern_payload_structure,
    ))(input)
}

pub fn msg_entity_pattern(input: OwnedSpan) -> Res<OwnedSpan, PipelineSelector> {
    msg_pattern(input).map(|(next, pattern)| (next, PipelineSelector::Msg(pattern)))
}

pub fn http_entity_pattern(input: OwnedSpan) -> Res<OwnedSpan, PipelineSelector> {
    http_pattern(input).map(|(next, pattern)| (next, PipelineSelector::Http(pattern)))
}

pub fn rc_entity_pattern(input: OwnedSpan) -> Res<OwnedSpan, PipelineSelector> {
    rc_pattern(input).map(|(next, pattern)| (next, PipelineSelector::Rc(pattern)))
}

pub fn entity_pattern(input: OwnedSpan) -> Res<OwnedSpan, PipelineSelector> {
    alt((msg_entity_pattern, http_entity_pattern, rc_entity_pattern))(input)
}

pub fn payload_structure_with_validation(input: OwnedSpan) -> Res<OwnedSpan, PayloadPatternCtx> {
    tuple((
        context("selector", payload_structure),
        opt(preceded(tag("~"), opt(format))),
        opt(preceded(tag("~"), call_with_config)),
    ))(input)
    .map(|(next, (data, format, verifier))| {
        (
            next,
            PayloadPatternCtx {
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

pub fn consume_payload_structure(input: OwnedSpan) -> Res<OwnedSpan, PayloadTypePatternCtx> {
    all_consuming(payload_structure)(input)
}

pub fn consume_data_struct_def(input: OwnedSpan) -> Res<OwnedSpan, PayloadPatternCtx> {
    all_consuming(payload_structure_with_validation)(input)
}

pub fn payload_pattern_any(input: OwnedSpan) -> Res<OwnedSpan, ValuePattern<PayloadPatternCtx>> {
    tag("*")(input).map(|(next, _)| (next, ValuePattern::Any))
}

pub fn payload_pattern(input: OwnedSpan) -> Res<OwnedSpan, ValuePattern<PayloadPatternCtx>> {
    context(
        "@payload-pattern",
        value_pattern(payload_structure_with_validation),
    )(input)
    .map(|(next, payload_pattern)| (next, payload_pattern))
}

pub fn payload_filter_block_empty(input: OwnedSpan) -> Res<OwnedSpan, PatternBlockCtx> {
    multispace0(input.clone()).map(|(next, _)| (input, PatternBlockCtx::None))
}

pub fn payload_filter_block_any(input: OwnedSpan) -> Res<OwnedSpan, PatternBlockCtx> {
    let (next, _) = delimited(multispace0, context("selector", tag("*")), multispace0)(input)?;

    Ok((next, PatternBlockCtx::Any))
}

pub fn payload_filter_block_def(input: OwnedSpan) -> Res<OwnedSpan, PatternBlockCtx> {
    payload_structure_with_validation(input)
        .map(|(next, pattern)| (next, PatternBlockCtx::Pattern(pattern)))
}

fn insert_block_pattern(input: OwnedSpan) -> Res<OwnedSpan, UploadBlock> {
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

pub fn upload_payload_block(input: OwnedSpan) -> Res<OwnedSpan, UploadBlock> {
    delimited(multispace0, file_chars, multispace0)(input).map(|(next, filename)| {
        (
            next,
            UploadBlock {
                name: filename.to_string(),
            },
        )
    })
}

pub fn upload_step(input: OwnedSpan) -> Res<OwnedSpan, UploadBlock> {
    delimited(tag("^["), upload_payload_block, tag("->"))(input)
}

pub fn request_payload_filter_block(input: OwnedSpan) -> Res<OwnedSpan, PayloadBlockCtx> {
    tuple((
        multispace0,
        alt((
            payload_filter_block_any,
            payload_filter_block_def,
            payload_filter_block_empty,
        )),
        multispace0,
    ))(input)
    .map(|(next, (_, block, _))| (next, PayloadBlockCtx::RequestPattern(block)))
}

pub fn response_payload_filter_block(input: OwnedSpan) -> Res<OwnedSpan, PayloadBlockCtx> {
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
    .map(|(next, (_, block, _))| (next, PayloadBlockCtx::ResponsePattern(block)))
}

pub fn rough_pipeline_step(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(tuple((
        many0(preceded(
            alt((tag("-"), tag("="), tag("+"))),
            any_soround_lex_block,
        )),
        alt((tag("->"), tag("=>"))),
    )))(input)
}

pub fn consume_pipeline_block(input: OwnedSpan) -> Res<OwnedSpan, PayloadBlockCtx> {
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

pub fn config(src: &str) -> Result<Document, MsgErr> {
    let (next, stripped) = strip_comments(src)?;
    let span = span_with_extra(stripped.as_str(), Arc::new(src.to_string()));
    let lex_root_scope = lex_root_scope(span.clone())?;
    let root_scope_selector = lex_root_scope.selector.clone().to_concrete()?;
    if root_scope_selector.name.as_str() == "Bind" {
        if root_scope_selector.version == Version::from_str("1.0.0")? {
            let bind = bind_config(lex_root_scope.block.content.clone())?;
            return Ok(Document::BindConfig(bind));
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

fn bind_config(input: OwnedSpan) -> Result<BindConfig, MsgErr> {
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

fn sematic_bind_scope(scope: LexScope<OwnedSpan>) -> Result<BindScope, MsgErr> {
    let selector_name = scope.selector.selector.name.to_string();
    match selector_name.as_str() {
        "Pipeline" => {
            let scope = lex_child_scopes(scope)?;
            let scope = RequestScope::try_from(scope)?;
            Ok(BindScope::RequestScope(scope))
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

fn parse_bind_pipelines_scope<'a>(input: OwnedSpan) -> Result<Spanned<OwnedSpan, BindScopeKind>, ParseErrs> {
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

pub fn nospace0(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(many0(satisfy(|c| !c.is_whitespace())))(input)
}

pub fn nospace1(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(pair(
        satisfy(|c| !c.is_whitespace()),
        many0(satisfy(|c| !c.is_whitespace())),
    ))(input)
}

pub fn no_space_with_blocks(input: OwnedSpan) -> Res<OwnedSpan, OwnedSpan> {
    recognize(many1(alt((recognize(any_block), nospace1))))(input)
}

pub fn var_pipeline(input: OwnedSpan) -> Res<OwnedSpan, VarPipeline> {
    many1(var_pipeline_segment)(input).map(|(next, segments)| {
        let pipeline = VarPipeline { segments };
        (next, pipeline)
    })
}

pub fn var_pipeline_segment(input: OwnedSpan) -> Res<OwnedSpan, VarPipelineSegment> {
    tuple((
        multispace0,
        subst(PipelineStepCtxParser()),
        multispace1,
        opt(subst(PipelineStopCtxParser())),
    ))(input)
    .map(|(next, (_, step, _, stop))| {
        let step = step.into();
        let stop = match stop {
            None => None,
            Some(stop) => {Some(stop.into())}
        };
        let segment = VarPipelineSegment { step, stop };

        (next, segment)
    })
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct PipelineStepCtxParser();
impl SubstParser<PipelineStepCtx> for PipelineStepCtxParser {
    fn parse_span<'a>(&self, input: OwnedSpan) -> Res<OwnedSpan, PipelineStepCtx> {
        pipeline_step_ctx(input)
    }
}

pub fn pipeline_step_ctx(input: OwnedSpan) -> Res<OwnedSpan, PipelineStepCtx> {
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
    .map(|(next, (entry, block_and_exit, _))| {
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
            PipelineStepCtx {
                entry,
                exit,
                blocks,
            },
        )
    })
}

pub fn core_pipeline_stop(input: OwnedSpan) -> Res<OwnedSpan, PipelineStopCtx> {
    context(
        "Core",
        delimited(
            tag("{{"),
            delimited(multispace0, opt(tag("*")), multispace0),
            tag("}}"),
        ),
    )(input)
    .map(|(next, _)| (next, PipelineStopCtx::Internal))
}

pub fn return_pipeline_stop(input: OwnedSpan) -> Res<OwnedSpan, PipelineStopCtx> {
    tag("&")(input).map(|(next, _)| (next, PipelineStopCtx::Respond))
}

pub fn call_pipeline_stop(input: OwnedSpan) -> Res<OwnedSpan, PipelineStopCtx> {
    context("Call", call)(input).map(|(next, call)| (next, PipelineStopCtx::Call(call)))
}

pub fn point_pipeline_stop(input: OwnedSpan) -> Res<OwnedSpan, PipelineStopCtx> {
    context("pipeline:stop:point", point_ctx)(input)
        .map(|(next, point)| (next, PipelineStopCtx::Point(point)))
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct PipelineStopCtxParser();
impl SubstParser<PipelineStopCtx> for PipelineStopCtxParser {
    fn parse_span<'a>(&self, input: OwnedSpan) -> Res<OwnedSpan, PipelineStopCtx> {
println!("parsing: {}",input.to_string());
        pipeline_stop_ctx(input)
    }
}

pub fn pipeline_stop_ctx(input: OwnedSpan) -> Res<OwnedSpan, PipelineStopCtx> {
    context(
        "Stop",
        pair(
            context(
                "pipeline:stop:expecting",
                cut(peek(alt(( tag("."), alpha1, tag("&"))))),
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

pub fn consume_pipeline_step(input: OwnedSpan) -> Res<OwnedSpan, PipelineStepCtx> {
    all_consuming(pipeline_step_ctx)(input)
}

pub fn consume_pipeline_stop(input: OwnedSpan) -> Res<OwnedSpan, PipelineStopCtx> {
    all_consuming(pipeline_stop_ctx)(input)
}

pub fn pipeline_segment(input: OwnedSpan) -> Res<OwnedSpan, PipelineSegmentCtx> {
    tuple((
        multispace0,
        pipeline_step_ctx,
        multispace0,
        pipeline_stop_ctx,
        multispace0,
    ))(input)
    .map(|(next, (_, step, _, stop, _))| (next, PipelineSegmentCtx { step, stop }))
}

pub fn pipeline(input: OwnedSpan) -> Res<OwnedSpan, PipelineCtx> {
    context(
        "pipeline",
        many0(delimited(multispace0, pipeline_segment, multispace0)),
    )(input)
    .map(|(next, segments)| (next, PipelineCtx { segments }))
}

pub fn consume_pipeline(input: OwnedSpan) -> Res<OwnedSpan, PipelineCtx> {
    all_consuming(pipeline)(input)
}

/*
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

 */

pub fn subst<T,P>(parser: P) -> impl FnMut(OwnedSpan) -> Res<OwnedSpan, Subst<T,OwnedSpan,P>>
where
    P: SubstParser<T> + 'static + Clone,
{
    move |input: OwnedSpan| {
        many1(chunk)(input.clone()).map(|(next, chunks)| {
            let len: usize = chunks.iter().map(|c| c.len()).sum();
            let span = input.slice(..input.len() - next.len());
            let chunks = Subst {
                chunks,
                parser: parser.clone(),
                span,
                phantom: PhantomData {},
            };
            (next, chunks)
        })
    }
}

pub fn chunk(input: OwnedSpan) -> Res<OwnedSpan, Chunk<OwnedSpan>> {
    alt((text_chunk, var_chunk))(input)
}

pub fn text_chunk(input: OwnedSpan) -> Res<OwnedSpan, Chunk<OwnedSpan>> {
    recognize(many1(alt((
        recognize(any_soround_lex_block),
        tag("\\$"),
        recognize(satisfy(|c| c != '$' && !c.is_whitespace())),
    ))))(input)
    .map(|(next, text)| (next, Chunk::Text(text)))
}

pub fn var_chunk(input: OwnedSpan) -> Res<OwnedSpan, Chunk<OwnedSpan>> {
    preceded(
        tag("$"),
        context(
            "variable",
            cut(delimited(
                context("variable:open", cut(tag("{"))),
                context("variable:name", variable_name),
                context("variable:close", cut(tag("}"))),
            )),
        ),
    )(input)
    .map(|(next, variable_name)| (next, Chunk::Var(variable_name)))
}

#[cfg(test)]
pub mod test {
    use crate::error::{MsgErr, ParseErrs};
    use crate::version::v0_0_1::config::config::Document;
    use crate::version::v0_0_1::parse::error::result;
    use crate::version::v0_0_1::parse::model::{
        BlockKind, DelimitedBlockKind, LexScope, NestedBlockKind, TerminatedBlockKind,
    };
    use crate::version::v0_0_1::parse::{args, bind_config, comment, config, ctx_seg, expected_block_terminator_or_non_terminator, lex_block, lex_child_scopes, lex_nested_block, lex_scope, lex_scope_pipeline_step_and_block, lex_scope_selector, lex_scope_selector_and_filters, lex_scopes, lowercase1, MapResolver, mesh_eos, nested_block, nested_block_content, next_selector, no_comment, parse_include_blocks, parse_inner_block, path_regex, pipeline, pipeline_segment, pipeline_step_ctx, pipeline_stop_ctx, point, point_ctx, point_var, rec_version, Res, root_scope, root_scope_selector, scope_filter, scope_filters, skewer_case, skewer_dot, space_chars, space_no_dupe_dots, space_point_segment, strip_comments, subst, SubstParser, var_pipeline, variable_name, VarResolver, VarSubst, version, wrapper};
    use nom::branch::alt;
    use nom::bytes::complete::{escaped, tag};
    use nom::character::complete::{alpha1, alphanumeric1, anychar, multispace0};
    use nom::character::is_alphanumeric;
    use nom::combinator::{all_consuming, eof, not, opt, peek, recognize};
    use nom::error::context;
    use nom::multi::{many0, many1};
    use nom::sequence::{delimited, pair, terminated, tuple};
    use nom::IResult;
    use nom_locate::LocatedSpan;
    use nom_supreme::error::ErrorTree;
    use std::rc::Rc;
    use std::sync::Arc;
    use crate::version::v0_0_1::span::{new_span, OwnedSpan, span_with_extra};

    #[test]
    pub fn test_point_var() -> Result<(),MsgErr>{

        let pipeline = log(result(point_var(new_span("my-domain.com:base") )))?;

       Ok(())
    }


    #[test]
    pub fn test_var_subst() -> Result<(),MsgErr>{

        let pipeline_var_cfg_str = r#"-> mr:${user}:iv => &"#;
        let pipeline = log(result(var_pipeline(new_span(pipeline_var_cfg_str) )))?;

        let mut resolver = MapResolver::new();
        //resolver.insert("user","paul:scott:williams");

        let pipeline = log(pipeline.resolve_vars(&resolver))?;

        Ok(())
    }

    #[test]
    pub fn test_lex_block() -> Result<(), MsgErr> {
        let esc = result(escaped(anychar, '\\', anychar)(new_span("\\}")))?;
        //println!("esc: {}", esc);
        log(result(all_consuming(lex_block(BlockKind::Nested(
            NestedBlockKind::Curly,
        )))(new_span("{}"))))?;
        log(result(all_consuming(lex_block(BlockKind::Nested(
            NestedBlockKind::Curly,
        )))(new_span("{x}"))))?;
        log(result(all_consuming(lex_block(BlockKind::Nested(
            NestedBlockKind::Curly,
        )))(new_span("{\\}}"))))?;
        log(result(all_consuming(lex_block(BlockKind::Delimited(
            DelimitedBlockKind::SingleQuotes,
        )))(new_span("'hello'"))))?;
        log(result(all_consuming(lex_block(BlockKind::Delimited(
            DelimitedBlockKind::SingleQuotes,
        )))(new_span("'ain\\'t it cool?'"))))?;

        //assert!(log(result(all_consuming(lex_block( BlockKind::Nested(NestedBlockKind::Curly)))(create_span("{ }}")))).is_err());
        Ok(())
    }
    #[test]
    pub fn test_path_regex2() -> Result<(), MsgErr> {
        log(result(path_regex(new_span("/xyz"))))?;
        Ok(())
    }
    #[test]
    pub fn test_bind_config() -> Result<(), MsgErr> {
        let bind_config_str = r#"Bind(version=1.0.0)  { Pipeline<Rc> -> { <Create> -> localhost:app => &; } }
        "#;

        log(config(bind_config_str))?;
        if let Document::BindConfig(bind) = log(config(bind_config_str))? {
            assert_eq!(bind.request_scopes().len(), 1);
            let mut pipelines = bind.request_scopes();
            let pipeline_scope = pipelines.pop().unwrap();
            assert_eq!(pipeline_scope.selector.selector.name.as_str(), "Pipeline");
            let message_scope = pipeline_scope.block.first().unwrap();
            assert_eq!(
                message_scope.selector.selector.name.to_string().as_str(),
                "Rc"
            );
            let action_scope = message_scope.block.first().unwrap();
            assert_eq!(
                action_scope.selector.selector.name.to_string().as_str(),
                "Create"
            );
        } else {
            assert!(false);
        }

        let bind_config_str = r#"Bind(version=1.0.0)  {
              Pipeline<Rc<Create>> -> localhost:app => &;
           }"#;

        if let Document::BindConfig(bind) = log(config(bind_config_str))? {
            assert_eq!(bind.request_scopes().len(), 1);
            let mut pipelines = bind.request_scopes();
            let pipeline_scope = pipelines.pop().unwrap();
            assert_eq!(pipeline_scope.selector.selector.name.as_str(), "Pipeline");
            let message_scope = pipeline_scope.block.first().unwrap();
            assert_eq!(
                message_scope.selector.selector.name.to_string().as_str(),
                "Rc"
            );
            let action_scope = message_scope.block.first().unwrap();
            assert_eq!(
                action_scope.selector.selector.name.to_string().as_str(),
                "Create"
            );
        } else {
            assert!(false);
        }

        let bind_config_str = r#"  Bind(version=1.0.0) {
              Pipeline -> {
                 <*> -> {
                    <Get>(auth)/users/(?P<user>)/.* -> localhost:users:${user} => &;
                 }
              }
           }

           "#;
        log(config(bind_config_str))?;

        let bind_config_str = r#"  Bind(version=1.0.0) {
              Pipeline -> {
                 <*> -> {
                    <*>/users -> localhost:users => &;
                 }
              }
           }

           "#;
        log(config(bind_config_str))?;

        let bind_config_str = r#"  Bind(version=1.0.0) {
              * -> { // This should fail since Pipeline needs to be defined
                 <*> -> {
                    <Get>/users -> localhost:users => &;
                 }
              }
           }

           "#;
        assert!(log(config(bind_config_str)).is_err());
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
        log(result(pipeline_segment(new_span("-> localhost"))))?;
        assert!(log(result(pipeline_segment(new_span("->")))).is_err());
        assert!(log(result(pipeline_segment(new_span("localhost")))).is_err());
        Ok(())
    }

    #[test]
    pub fn test_pipeline_stop() -> Result<(), MsgErr> {
        log(result(space_chars(new_span("localhost"))))?;
        log(result(space_no_dupe_dots(new_span("localhost"))))?;

        log(result(mesh_eos(new_span(""))))?;
        log(result(mesh_eos(new_span(":"))))?;

        log(result(recognize(tuple((
            context("point:space_segment_leading", peek(alpha1)),
            space_no_dupe_dots,
            space_chars,
        )))(new_span("localhost"))))?;
        log(result(space_point_segment(new_span("localhost.com"))))?;

        log(result(point(new_span("mechtron.io:app:hello"))))?;
        log(result(pipeline_stop_ctx(new_span(
            "localhost:app:hello",
        ))))?;
        Ok(())
    }

    #[test]
    pub fn test_pipeline() -> Result<(), MsgErr> {
        log(result(pipeline(new_span("-> localhost => &"))))?;
        Ok(())
    }

    #[test]
    pub fn test_pipeline_step() -> Result<(), MsgErr> {
        log(result(pipeline_step_ctx(new_span("->"))))?;
        log(result(pipeline_step_ctx(new_span("-[ Text ]->"))))?;
        log(result(pipeline_step_ctx(new_span("-[ Text ]=>"))))?;
        log(result(pipeline_step_ctx(new_span("=[ Text ]=>"))))?;

        assert!(log(result(pipeline_step_ctx(new_span("=")))).is_err());
        assert!(log(result(pipeline_step_ctx(new_span("-[ Bin ]=")))).is_err());
        assert!(log(result(pipeline_step_ctx(new_span("[ Bin ]=>")))).is_err());
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
        rec_version(new_span("1.0.0"))?;
        rec_version(new_span("1.0.0-alpha"))?;
        version(new_span("1.0.0-alpha"))?;

        Ok(())
    }
    #[test]
    pub fn test_rough_block() -> Result<(), MsgErr> {
        result(all_consuming(lex_nested_block(NestedBlockKind::Curly))(
            new_span("{  }"),
        ))?;
        result(all_consuming(lex_nested_block(NestedBlockKind::Curly))(
            new_span("{ {} }"),
        ))?;
        assert!(
            result(all_consuming(lex_nested_block(NestedBlockKind::Curly))(
                new_span("{ } }")
            ))
            .is_err()
        );
        // this is allowed by rough_block
        result(all_consuming(lex_nested_block(NestedBlockKind::Curly))(
            new_span("{ ] }"),
        ))?;

        result(lex_nested_block(NestedBlockKind::Curly)(new_span(
            r#"x blah


Hello my friend


        }"#,
        )))
        .err()
        .unwrap()
        .print();

        result(lex_nested_block(NestedBlockKind::Curly)(new_span(
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
        log(result(lex_nested_block(NestedBlockKind::Curly)(
            new_span("{ <Get> -> localhost; }    "),
        )))?;
        if true {
            return Ok(());
        }
        all_consuming(nested_block(NestedBlockKind::Curly))(new_span("{  }"))?;
        all_consuming(nested_block(NestedBlockKind::Curly))(new_span("{ {} }"))?;
        log(result(nested_block(NestedBlockKind::Curly)(new_span(
            "{ [] }",
        ))))?;
        assert!(
            expected_block_terminator_or_non_terminator(NestedBlockKind::Curly)(new_span("}"))
                .is_ok()
        );
        assert!(
            expected_block_terminator_or_non_terminator(NestedBlockKind::Curly)(new_span("]"))
                .is_err()
        );
        assert!(
            expected_block_terminator_or_non_terminator(NestedBlockKind::Square)(new_span("x"))
                .is_ok()
        );
        assert!(nested_block(NestedBlockKind::Curly)(new_span("{ ] }")).is_err());
        result(nested_block(NestedBlockKind::Curly)(new_span(
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
            (result(root_scope_selector(new_span(
                r#"

            Bind(version=1.0.0)->"#,
            )))
            .is_ok())
        );

        assert!(
            (result(root_scope_selector(new_span(
                r#"

            Bind(version=1.0.0-alpha)->"#,
            )))
            .is_ok())
        );

        result(root_scope_selector(new_span(
            r#"

            Bind(version=1.0.0) ->"#,
        )))
        .err()
        .unwrap()
        .print();

        result(root_scope_selector(new_span(
            r#"

        Bind   x"#,
        )))
        .err()
        .unwrap()
        .print();

        result(root_scope_selector(new_span(
            r#"

        (Bind(version=3.2.0)   "#,
        )))
        .err()
        .unwrap()
        .print();

        Ok(())
    }

    #[test]
    pub fn test_scope_filter() -> Result<(), MsgErr> {
        result(scope_filter(new_span("(auth)")))?;
        result(scope_filter(new_span("(auth )")))?;
        result(scope_filter(new_span("(auth hello)")))?;
        result(scope_filter(new_span("(auth +hello)")))?;
        result(scope_filters(new_span("(auth +hello)->")))?;
        result(scope_filters(new_span("(auth +hello)-(filter2)->")))?;
        result(scope_filters(new_span("(3auth +hello)-(filter2)->")))
            .err()
            .unwrap()
            .print();
        result(scope_filters(new_span("(a?th +hello)-(filter2)->")))
            .err()
            .unwrap()
            .print();
        result(scope_filters(new_span("(auth +hello)-(filter2) {}")))
            .err()
            .unwrap()
            .print();

        assert!(skewer_case(new_span("3x")).is_err());

        Ok(())
    }
    #[test]
    pub fn test_next_selector() {
        assert_eq!(
            "Http",
            next_selector(new_span("Http"))
                .unwrap()
                .1
                 .0
                .to_string()
                .as_str()
        );
        assert_eq!(
            "Http",
            next_selector(new_span("<Http>"))
                .unwrap()
                .1
                 .0
                .to_string()
                .as_str()
        );
        assert_eq!(
            "Http",
            next_selector(new_span("Http<Msg>"))
                .unwrap()
                .1
                 .0
                .to_string()
                .as_str()
        );
        assert_eq!(
            "Http",
            next_selector(new_span("<Http<Msg>>"))
                .unwrap()
                .1
                 .0
                .to_string()
                .as_str()
        );

        assert_eq!(
            "*",
            next_selector(new_span("<*<Msg>>"))
                .unwrap()
                .1
                 .0
                .to_string()
                .as_str()
        );

        assert_eq!(
            "*",
            next_selector(new_span("*"))
                .unwrap()
                .1
                 .0
                .to_string()
                .as_str()
        );

        assert!(next_selector(new_span("<*x<Msg>>")).is_err());
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
        ))(new_span(""))))?;
        log(result(path_regex(new_span("/root/$(subst)"))))?;
        log(result(path_regex(new_span("/users/$(user=.*)"))))?;

        Ok(())
    }

    #[test]
    pub fn test_lex_scope() -> Result<(), MsgErr> {
        let pipes = log(result(lex_scope(new_span("Pipes -> {}")))).unwrap();

        //        let pipes = log(result(lex_scope(create_span("Pipes {}"))));

        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Pipes");
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.block.content.len(), 0);
        assert!(pipes.selector.filters.is_empty());
        assert!(pipes.pipeline_step.is_some());

        assert!(log(result(lex_scope(new_span("Pipes {}")))).is_err());

        let pipes = log(result(lex_scope(new_span("Pipes -> 12345;"))))?;
        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Pipes");
        assert_eq!(pipes.block.content.to_string().as_str(), "-> 12345");
        assert_eq!(
            pipes.block.kind,
            BlockKind::Terminated(TerminatedBlockKind::Semicolon)
        );
        assert_eq!(pipes.selector.filters.len(), 0);
        assert!(pipes.pipeline_step.is_none());
        let pipes = log(result(lex_scope(new_span(
            //This time adding a space before the 12345... there should be one space in the content, not two
            r#"Pipes ->  12345;"#,
        ))))?;
        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Pipes");
        assert_eq!(pipes.block.content.to_string().as_str(), "->  12345");
        assert_eq!(
            pipes.block.kind,
            BlockKind::Terminated(TerminatedBlockKind::Semicolon)
        );
        assert_eq!(pipes.selector.filters.len(), 0);
        assert!(pipes.pipeline_step.is_none());

        let pipes = log(result(lex_scope(new_span("Pipes(auth) -> {}"))))?;

        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Pipes");
        assert_eq!(pipes.block.content.len(), 0);
        assert_eq!(pipes.block.kind, BlockKind::Nested(NestedBlockKind::Curly));
        assert_eq!(pipes.selector.filters.len(), 1);
        assert!(pipes.pipeline_step.is_some());

        let pipes = log(result(lex_scope(new_span("Pipeline<Msg> -> {}"))))?;

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
        assert!(pipes.pipeline_step.is_some());

        let pipes = log(result(lex_scope(new_span(
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
        let pipes = log(result(lex_scope(new_span(parseme.as_str()))))?;

        assert_eq!(pipes.selector.selector.name.to_string().as_str(), "Http");
        assert_eq!(
            pipes.block.content.to_string().as_str(),
            format!("-> {}", msg)
        );
        assert_eq!(
            pipes.block.kind,
            BlockKind::Terminated(TerminatedBlockKind::Semicolon)
        );
        assert_eq!(pipes.selector.filters.len(), 0);
        assert!(pipes.pipeline_step.is_none());

        let pipes = log(result(lex_scope(new_span(
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

        let pipes = log(result(lex_scope(new_span(
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

        let pipes = log(result(lex_scope(new_span(
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
        let span = span_with_extra(stripped.as_str(), Arc::new(stripped.to_string()));
        let pipes = log(result(lex_scope(span)))?;

        let pipes = log(result(lex_scope(new_span("* -> {}"))))?;

        /* let pipes = log(result(lex_scope(create_span(
            "* -> {}",
        ))))?;

        */
        Ok(())
    }

    pub fn test_nesting_bind() {
        let pipes = log(result(lex_scope(new_span(
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

        let root = result(root_scope(new_span(config)))?;

        log(lex_scopes(root.block.content.clone()));
        let sub_scopes = lex_scopes(root.block.content.clone())?;

        assert_eq!(sub_scopes.len(), 2);

        Ok(())
    }
    #[test]
    pub fn test_variable_name() -> Result<(), MsgErr> {
        assert_eq!(
            "v".to_string(),
            log(result(lowercase1(new_span("v"))))?.to_string()
        );
        assert_eq!(
            "var".to_string(),
            log(result(skewer_dot(new_span("var"))))?.to_string()
        );

        log(result(variable_name(new_span("var"))))?;
        Ok(())
    }

    #[test]
    pub fn test_subst() -> Result<(), MsgErr> {
        /*
        #[derive(Clone)]
        pub struct SomeParser();
        impl SubstParser<String> for SomeParser {
            fn parse_span<'a>(&self, span: OwnedSpan) -> Res<OwnedSpan, String> {
                recognize(terminated(
                    recognize(many0(pair(peek(not(eof)), recognize(anychar)))),
                    eof,
                ))(span)
                .map(|(next, span)| (next, span.to_string()))
            }
        }

        let chunks = log(result(subst(SomeParser())(create_span("123[]:${var}:abc"))))?;
        assert_eq!(chunks.chunks.len(), 3);
        let mut resolver = MapResolver::new();
        resolver.insert("var", "hello");
        let resolved = log(chunks.resolve_vars(&resolver))?;

        let chunks = log(result(subst(SomeParser())(create_span(
            "123[]:\\${var}:abc",
        ))))?;
        let resolved = log(chunks.resolve_vars(&resolver))?;

        let r = log(result(subst(SomeParser())(create_span(
            "123[    ]:${var}:abc",
        ))))?;
        println!("{}", r.to_string());
        log(result(subst(SomeParser())(create_span("123[]:${vAr}:abc"))));
        log(result(subst(SomeParser())(create_span(
            "123[]:${vAr }:abc",
        ))));

        Ok(())

         */
        unimplemented!()
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
