    use core::fmt;
    use std::collections::HashMap;
    use std::fmt::Formatter;
    use std::ops::{Deref, RangeFrom};
    use std::str::FromStr;

    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::bytes::complete::{is_a, is_not};
    use nom::character::complete::{
        alpha0, alpha1, alphanumeric0, alphanumeric1, anychar, char, digit0, digit1, multispace0,
        multispace1, one_of, space0, space1,
    };
    use nom::combinator::{
        all_consuming, cut, eof, fail, not, opt, peek, recognize, value, verify,
    };
    use nom::error::{context, ContextError, ErrorKind, ParseError, VerboseError};
    use nom::multi::{many0, separated_list0, separated_list1};
    use nom::sequence::{delimited, pair, preceded, terminated, tuple};
    use nom::{
        AsChar, Compare, FindToken, InputIter, InputLength, InputTake, InputTakeAtPosition,
        IResult, Slice, UnspecializedInput,
    };
    use nom_supreme::parse_from_str;

    use crate::error::MsgErr;
    use crate::version::v0_0_1::command::command::common::{PropertyMod, SetProperties, StateSrc};
    use crate::version::v0_0_1::config::config::bind::parse::pipeline_step;
    use crate::version::v0_0_1::entity::entity::request::create::{
        Create, CreateOp, KindTemplate, PointSegFactory, PointTemplate, PointTemplateSeg, Require,
        Strategy, Template,
    };
    use crate::version::v0_0_1::entity::entity::request::get::{Get, GetOp};
    use crate::version::v0_0_1::entity::entity::request::select::{Select, SelectIntoPayload, SelectKind};
    use crate::version::v0_0_1::entity::entity::request::set::Set;
    use crate::version::v0_0_1::id::id::{CaptureAddress, Point, PointSeg, PointSegDelim, PointSegKind, PointSegPairDef, PointSegPairSubst, PointSegSubst, PointSubst, RouteSeg, Version};
    use crate::version::v0_0_1::parse::error::{first_context, result};
    use crate::version::v0_0_1::security::{
        AccessGrant, AccessGrantKindDef, AccessGrantKindSubst, AccessGrantSubst, ChildPerms,
        ParticlePerms, Permissions, PermissionsMask, PermissionsMaskKind, Privilege,
    };
    use crate::version::v0_0_1::selector::selector::parse::{
        delim_kind, generic_kind_base, kind, point_selector, specific, specific_selector, version,
    };
    use crate::version::v0_0_1::selector::selector::{
        PointKindHierarchy, PointKindSeg, PointSelector, skewer, upload_step,
    };
    use crate::version::v0_0_1::util::StringMatcher;
    use crate::version::v0_0_1::{span, Span};
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
        context("point:space_segment:dot_dupes", peek(pair(many0(pair(skewer,opt(tag(".")))),not(tag("..")))))(input).map(|(next,_)|
            {
                (next,())
            })
    }

    pub fn space_point_segment(input: Span) -> Res<Span, PointSeg> {
        context("point:space_segment", cut(pair(recognize(tuple((context("point:space_segment_leading",alpha1), space_no_dupe_dots,space_chars))), mesh_eos)))(input)
            .map(|(next, (space, _))| (next, PointSeg::Space(space.to_string())))
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
        context( "root_point_subst",tuple((
            opt(terminated(var_subst(point_route_segment), tag("::"))),
            tag("ROOT"),
        )))(input)
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
                many0(tuple((seg_delim,peek(context("point:bad_leading",cut(alt((lowercase1,digit1))))),base_point_segment))),
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
                    let mut bases = bases.into_iter().map(|(_,_,seg)|seg).collect();
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
                many0(tuple((tag(":"),peek(context("point:bad_leading",cut(alt((tag("$"),lowercase1,digit1))))),var_subst(base_point_segment)))),
                opt(preceded(tag(":"),var_subst(version_point_segment))),
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
                    let mut bases = bases.into_iter().map(|(_,_,seg)|PointSegPairSubst::new(PointSegDelim::Mesh, seg)).collect();
                    segments.push(space);
                    segments.append(&mut bases);
                    match version {
                        None => {}
                        Some(version) => {
                            let version = PointSegPairDef::new(PointSegDelim::Mesh,version);
                            segments.push(version);
                        }
                    }

                    if let Option::Some((fsroot, mut files, _)) = filesystem {
                        let fsroot = PointSegPairSubst::new(PointSegDelim::Mesh, fsroot );
                        let mut files :Vec<PointSegPairSubst> = files.into_iter().map( |f| PointSegPairDef::new(PointSegDelim::File,f)).collect();
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
        let span = span(input);
        let point = result(context("consume",all_consuming(point))(span))?;
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
        let span = span(input);
        let point = result(context("consume",all_consuming(point_subst))(span))?;
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
                    && !((char_item.is_alpha() && char_item.is_lowercase())
                    || char_item.is_dec_digit())
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
        recognize(tuple((is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alphanumeric0)))(input)
        //recognize(alpha1)(input)
    }

    pub fn skewer_case(input: Span) -> Res<Span, Span> {
        recognize(tuple((lowercase1, skewer)))(input)
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
                Ctx::RelativePoint => Err(
                    "context operators '.' (reference to WorkingPoint) not expected here".into(),
                ),
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

    pub fn mesh_seg<I: Clone, E: ParseError<I>, F>(
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, PointSeg, E>
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
            context("variable", cut(delimited(context("variable:open",tag("(")), variable_name, tag(")")))),
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
        use crate::error::MsgErr;
        use crate::version::v0_0_1::Span;
        use ariadne::Report;
        use ariadne::{Label, ReportKind, Source};
        use nom::{Err, Slice};
        use nom_supreme::error::{BaseErrorKind, ErrorTree, StackContext};

        pub fn result<R>(result: Result<(Span, R), Err<ErrorTree<Span>>>) -> Result<R, MsgErr> {
            match result {
                Ok((_, e)) => Ok(e),
                Err(err) => Err(find(&err)),
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

        fn report(context: &str, loc: Span) -> MsgErr {
            let source = Source::from(loc.extra.to_string().as_str());

            let mut builder = Report::build(ReportKind::Error, (), 23);

            let builder = match context {
                "point" => {
                    builder.with_message("Invalid Point").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Point"))
                },
                "consume" => { builder.with_message("Expected to be able to consume the entire String")}
                "point:space_segment:dot_dupes" => { builder.with_message("Space Segment cannot have consecutive dots i.e. '..'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Consecutive dots not allowed"))}
                "point:version:root_not_trailing" =>{ builder.with_message("Root filesystem is the only segment allowed to follow a bundle version i.e. 'space:base:2.0.0-version:/dir/somefile.txt'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Only root file segment ':/' allowed here"))}
                "point:space_segment_leading" => {builder.with_message("The leading character of a Space segment must be a lowercase letter").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Leading Character"))}
                "point:space_segment" => {builder.with_message("A Point Space Segment must be all lowercase, alphanumeric with dashes and dots.  It follows Host and Domain name rules i.e. 'localhost', 'mechtron.io'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Space Segment Character"))}
                "point:bad_leading" => {builder.with_message("The leading character must be a lowercase letter (for Base Segments) or a digit (for Version Segments)").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Leading Character"))}
                "point:base_segment" => {builder.with_message("A Point Base Segment must be 'skewer-case': all lowercase alphanumeric with dashes. The leading character must be a letter.").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Base Segment Character"))}
                "point:dir_pop" => {builder.with_message("A Point Directory Pop '..'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Something is Wrong"))}
                "point:dir_segment" => {builder.with_message("A Point Dir Segment follows legal filesystem characters and must end in a '/'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:root_filesystem_segment" => {builder.with_message("Root FileSystem ':/'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:file_segment" => {builder.with_message("A Point File Segment follows legal filesystem characters").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:file_or_directory"=> {builder.with_message("A Point File Segment (Files & Directories) follows legal filesystem characters").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:version_segment" => {builder.with_message("A Version Segment allows all legal SemVer characters").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
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
            MsgErr::Report {
                report: builder.finish(),
                source: loc.extra.to_string(),
            }
        }

        pub fn find(err: &Err<ErrorTree<Span>>) -> MsgErr {
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
                            report(*context, span.clone())
                        }
                        _ => "internal parser error: could not find a parse context in order to generate a useful error message".into()
                    }
                }
                ErrorTree::Base { location, kind } => {
                    report ("eof", location.clone() )
                },
                ErrorTree::Alt(alts) => {
                    for alt in alts {
                        return find_tree(alt);
                    }

                    "internal parser error: ErrorTree::Alt could not find a suitable context error in the various alts".into()
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