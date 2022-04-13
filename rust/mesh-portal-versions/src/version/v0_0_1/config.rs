pub mod config {
    use std::collections::HashMap;
    use std::ops::Deref;

    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::config::config::bind::BindConfig;
    use crate::version::v0_0_1::id::id::{GenericKind, Point};
    use crate::version::v0_0_1::particle::particle;
    use crate::version::v0_0_1::particle::particle::Stub;

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
        use crate::version::v0_0_1::entity::entity::request::{Rc, RequestCore};
        use crate::version::v0_0_1::entity::entity::EntityType;
        use crate::version::v0_0_1::id::id::CaptureAddress;
        use crate::version::v0_0_1::payload::payload::Call;
        use crate::version::v0_0_1::payload::payload::{Payload, PayloadPattern};
        use crate::version::v0_0_1::selector::selector::{
            PayloadBlock, EntityPattern, HttpPattern, MsgPattern, RcPattern,
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
            pub blocks: Vec<PayloadBlock>,
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
            use crate::version::v0_0_1::config::config::bind::{
                ConfigScope, Pipeline, PipelineSegment, PipelinesSubScope, PipelineStep,
                PipelineStop, ProtoBind, Selector, StepKind,
            };
            use crate::version::v0_0_1::entity::entity::EntityType;
            use crate::version::v0_0_1::parse::{capture_point, Res};
            use crate::version::v0_0_1::selector::selector::{
                call, entity_pattern, EntityPattern, http_pattern, http_pattern_scoped,
                HttpPattern, msg_pattern_scoped, MsgPattern, pipeline_step_block, rc_pattern_scoped,
                RcPattern,
            };
            use crate::version::v0_0_1::{span, Span};
            use nom::branch::{alt, Alt};
            use nom::bytes::complete::tag;
            use nom::character::complete::multispace0;
            use nom::combinator::{all_consuming, fail, not, opt, peek, success};
            use nom::error::{context, ContextError, ErrorKind, ParseError};
            use nom::multi::{many0, many1};
            use nom::sequence::{delimited, preceded, tuple};
            use nom::{
                AsChar, Compare, Err, FindToken, InputIter, InputLength, InputTake, InputTakeAtPosition,
                IResult, Parser, UnspecializedInput,
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
                            return format!("{}\n\ninternal parsing error: unexpected kind error when processing context hierarchy: '{}'", extract_problem_line(span(input)),kind.description() );
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
                    extract_problem_line(span(input)),
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
