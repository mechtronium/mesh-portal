pub mod config {
    use std::collections::HashMap;
    use std::ops::Deref;
    use std::pin::Pin;

    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::config::config::bind::BindConfig;
    use crate::version::v0_0_1::id::id::{GenericKind, Point};
    use crate::version::v0_0_1::messaging::messaging::Request;
    use crate::version::v0_0_1::parse::model::{MessageScope, MethodScope, RouteScope};
    use crate::version::v0_0_1::particle::particle;
    use crate::version::v0_0_1::particle::particle::Stub;
    use crate::version::v0_0_1::util::ValueMatcher;

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
        pub config: PointConfig<ParticleConfigBody>,
        pub details: particle::ParticleDetails,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PointConfig<Body> {
        pub point: Point,
        pub body: Body,
    }

    impl<Body> Deref for PointConfig<Body> {
        type Target = Body;

        fn deref(&self) -> &Self::Target {
            &self.body
        }
    }

    #[derive(Clone)]
    pub enum Document {
        BindConfig(BindConfig),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ParticleConfigBody {
        Control,
        Named(String),
    }

    pub mod bind {
        use crate::error::{MsgErr, ParseErrs};
        use crate::version::v0_0_1::command::request::{MethodPattern, Rc, RequestCore};
        use crate::version::v0_0_1::entity::MethodKind;
        use crate::version::v0_0_1::id::id::{Point, PointCtx, PointVar, Topic};
        use crate::version::v0_0_1::payload::payload::{Call, CallDef};
        use crate::version::v0_0_1::payload::payload::{Payload, PayloadPattern};

        use crate::version::v0_0_1::messaging::messaging::Request;
        use crate::version::v0_0_1::parse::model::{
            BindScope, MessageScope, MethodScope, PipelineSegment, PipelineSegmentDef, PipelineVar,
            RouteScope, ScopeFilters,
        };
        use crate::version::v0_0_1::parse::Env;
        use crate::version::v0_0_1::selector::{PayloadBlock, PayloadBlockDef};
        use crate::version::v0_0_1::util::{ToResolved, ValueMatcher, ValuePattern};
        use regex::Regex;
        use serde::{Deserialize, Serialize};
        use std::convert::TryInto;

        #[derive(Clone)]
        pub struct BindConfig {
            scopes: Vec<BindScope>, /*pub msg: ConfigScope<EntityKind, Selector<MsgPipelineSelector>>,
                                    pub http: ConfigScope<EntityKind, Selector<HttpPipelineSelector>>,
                                    pub rc: ConfigScope<EntityKind, Selector<RcPipelineSelector>>,

                                     */
        }

        impl BindConfig {
            pub fn new(scopes: Vec<BindScope>) -> Self {
                Self { scopes }
            }

            pub fn route_scopes(&self) -> Vec<&RouteScope> {
                let mut scopes = vec![];
                for scope in &self.scopes {
                    if let BindScope::RequestScope(request_scope) = &scope {
                        scopes.push(request_scope);
                    }
                }
                scopes
            }

            pub fn select(&self, request: &Request) -> Result<&MethodScope, MsgErr> {
                for route_scope in self.route_scopes() {
                    if route_scope.selector.is_match(request).is_ok() {
                        for message_scope in &route_scope.block {
                            if message_scope.selector.is_match(request).is_ok() {
                                for method_scope in &message_scope.block {
                                    if method_scope.selector.is_match(request).is_ok() {
                                        return Ok(method_scope);
                                    }
                                }
                            }
                        }
                    }
                }
                Err(MsgErr::err404())
            }
        }

        pub struct Cursor {}

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

        pub type Pipeline = PipelineDef<Point>;
        pub type PipelineCtx = PipelineDef<PointCtx>;

        #[derive(Debug, Clone)]
        pub struct PipelineDef<Pnt> {
            pub segments: Vec<PipelineSegmentDef<Pnt>>,
        }

        impl<Pnt> PipelineDef<Pnt> {
            pub fn new() -> Self {
                Self { segments: vec![] }
            }

            pub fn consume(&mut self) -> Option<PipelineSegmentDef<Pnt>> {
                if self.segments.is_empty() {
                    Option::None
                } else {
                    Option::Some(self.segments.remove(0))
                }
            }
        }

        pub type PipelineStepVar = PipelineStepDef<PointVar>;
        pub type PipelineStepCtx = PipelineStepDef<PointCtx>;
        pub type PipelineStep = PipelineStepDef<Point>;

        #[derive(Debug, Clone)]
        pub struct PipelineStepDef<Pnt> {
            pub entry: MessageKind,
            pub exit: MessageKind,
            pub blocks: Vec<PayloadBlockDef<Pnt>>,
        }

        impl ToResolved<PipelineStep> for PipelineStepCtx {
            fn to_resolved(self, env: &Env) -> Result<PipelineStep, MsgErr> {
                let mut blocks = vec![];
                for block in self.blocks {
                    blocks.push(block.to_resolved(env)?);
                }

                Ok(PipelineStep {
                    entry: self.entry,
                    exit: self.exit,
                    blocks,
                })
            }
        }

        impl ToResolved<PipelineStepCtx> for PipelineStepVar {
            fn to_resolved(self, env: &Env) -> Result<PipelineStepCtx, MsgErr> {
                let mut blocks = vec![];
                for block in self.blocks {
                    blocks.push(block.to_resolved(env)?);
                }

                Ok(PipelineStepCtx {
                    entry: self.entry,
                    exit: self.exit,
                    blocks,
                })
            }
        }

        /*
        impl CtxSubst<PipelineStep> for PipelineStepCtx{
            fn resolve_ctx(self, resolver: &dyn CtxResolver) -> Result<PipelineStep, MsgErr> {
                let mut errs = vec![];
                let mut blocks = vec![];
                for block in self.blocks {
                    match block.resolve_ctx(resolver) {
                        Ok(block)=>blocks.push(block),
                        Err(err)=>errs.push(err)
                    }
                }
                if errs.is_empty() {
                    Ok(PipelineStep{
                        entry:self.entry,
                        exit: self.exit,
                        blocks
                    })
                } else {
                    Err(ParseErrs::fold(errs).into())
                }
            }
        }

         */

        impl PipelineStep {
            pub fn new(entry: MessageKind, exit: MessageKind) -> Self {
                Self {
                    entry,
                    exit,
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

        pub type PipelineStopCtx = PipelineStopDef<PointCtx>;
        pub type PipelineStopVar = PipelineStopDef<PointVar>;
        pub type PipelineStop = PipelineStopDef<Point>;

        #[derive(Debug, Clone)]
        pub enum PipelineStopDef<Pnt> {
            Internal,
            Call(CallDef<Pnt>),
            Respond,
            Point(Pnt),
        }

        impl ToResolved<PipelineStop> for PipelineStopVar {
            fn to_resolved(self, env: &Env) -> Result<PipelineStop, MsgErr> {
                let stop: PipelineStopCtx = self.to_resolved(env)?;
                stop.to_resolved(env)
            }
        }

        impl ToResolved<PipelineStop> for PipelineStopCtx {
            fn to_resolved(self, env: &Env) -> Result<PipelineStop, MsgErr> {
                Ok(match self {
                    PipelineStopCtx::Internal => PipelineStop::Internal,
                    PipelineStopCtx::Call(call) => PipelineStop::Call(call.to_resolved(env)?),
                    PipelineStopCtx::Respond => PipelineStop::Respond,
                    PipelineStopCtx::Point(point) => PipelineStop::Point(point.to_resolved(env)?),
                })
            }
        }

        impl ToResolved<PipelineStopCtx> for PipelineStopVar {
            fn to_resolved(self, env: &Env) -> Result<PipelineStopCtx, MsgErr> {
                Ok(match self {
                    PipelineStopVar::Internal => PipelineStopCtx::Internal,
                    PipelineStopVar::Call(call) => PipelineStopCtx::Call(call.to_resolved(env)?),
                    PipelineStopVar::Respond => PipelineStopCtx::Respond,
                    PipelineStopVar::Point(point) => {
                        PipelineStopCtx::Point(point.to_resolved(env)?)
                    }
                })
            }
        }

        /*
        impl CtxSubst<PipelineStop> for PipelineStopCtx {
            fn resolve_ctx(self, resolver: &dyn CtxResolver) -> Result<PipelineStop, MsgErr> {
                match self {
                    PipelineStopCtx::Internal => Ok(PipelineStop::Internal),
                    PipelineStopCtx::Call(call) => Ok(PipelineStop::Call(call.resolve_ctx(resolver)?)),
                    PipelineStopCtx::Respond => Ok(PipelineStop::Respond),
                    PipelineStopCtx::Point(point) => Ok(PipelineStop::Point(point.resolve_ctx(resolver)?))
                }
            }
        }

         */

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
        pub enum MessageKind {
            Request,
            Response,
        }

        pub struct RouteSelector {
            pub topic: Option<ValuePattern<Topic>>,
            pub method: ValuePattern<MethodPattern>,
            pub path: Regex,
            pub filters: ScopeFilters,
        }

        impl RouteSelector {
            pub fn new(
                topic: Option<ValuePattern<Topic>>,
                method: ValuePattern<MethodPattern>,
                path: Regex,
                filters: ScopeFilters,
            ) -> Self {
                Self {
                    topic,
                    method,
                    path,
                    filters,
                }
            }

            pub fn is_match(&self, request: &Request) -> Result<(), ()> {
                if let Some(topic) = &self.topic {
                    topic.is_match(&request.from.topic)?;
                } else if Topic::None != request.from.topic {
                    return Err(());
                }

                self.method.is_match(&request.core.method)?;
                match self.path.is_match(request.core.uri.path()) {
                    true => Ok(()),
                    false => Err(()),
                }
            }
        }
    }
}
