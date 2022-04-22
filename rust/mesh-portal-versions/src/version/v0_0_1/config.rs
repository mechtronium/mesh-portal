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
        pub config: PointConfig<ParticleConfigBody>,
        pub stub: particle::Stub,
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

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Config {
        Bind(BindConfig),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ParticleConfigBody {
        Control,
        Named(String),
    }

    pub mod bind {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::entity::entity::request::{Rc, RequestCore};
        use crate::version::v0_0_1::entity::entity::EntityKind;
        use crate::version::v0_0_1::id::id::{Point, PointCtx};
        use crate::version::v0_0_1::payload::payload::{Call, CallDef};
        use crate::version::v0_0_1::payload::payload::{Payload, PayloadPattern};
        use crate::version::v0_0_1::selector::selector::{
            PipelineSelector, HttpPipelineSelector, MsgPipelineSelector, RcPipelineSelector,
        };
        use crate::version::v0_0_1::util::{ValueMatcher, ValuePattern};
        use serde::{Deserialize, Serialize};
        use std::convert::TryInto;
        use crate::version::v0_0_1::parse::model::{BindScope, RequestScope, PipelineSegment, PipelineSegmentDef};
        use crate::version::v0_0_1::selector::{PayloadBlock, PayloadBlockDef};

        #[derive(Debug,Clone, Serialize, Deserialize)]
        pub struct BindConfig {
            pub scopes: Vec<BindScope>
            /*pub msg: ConfigScope<EntityKind, Selector<MsgPipelineSelector>>,
            pub http: ConfigScope<EntityKind, Selector<HttpPipelineSelector>>,
            pub rc: ConfigScope<EntityKind, Selector<RcPipelineSelector>>,

             */
        }

        impl BindConfig {
            pub fn new(scopes: Vec<BindScope>) -> Self {
                Self {
                    scopes
                }
            }

            pub fn request_scopes(&self) -> Vec<&RequestScope> {
                let mut scopes = vec![];
                for scope in &self.scopes {
                    if let BindScope::RequestScope(request_scope) = &scope {
                       scopes.push(request_scope);
                    }
                }
                scopes
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

        impl<T> ConfigScope<T, Selector<HttpPipelineSelector>> {
            pub fn find_match(&self, m: &RequestCore) -> Result<Selector<HttpPipelineSelector>, MsgErr> {
                for e in &self.elements {
                    if e.is_match(m).is_ok() {
                        return Ok(e.clone());
                    }
                }
                Err("no match".into())
            }
        }

        impl<T> ConfigScope<T, Selector<MsgPipelineSelector>> {
            pub fn find_match(&self, m: &RequestCore) -> Result<Selector<MsgPipelineSelector>, MsgErr> {
                for e in &self.elements {
                    if e.is_match(m).is_ok() {
                        return Ok(e.clone());
                    }
                }
                Err("no match".into())
            }
        }

        pub type Pipeline = PipelineDef<Point>;
        pub type PipelineCtx = PipelineDef<PointCtx>;

        #[derive(Debug, Clone, Serialize, Deserialize )]
        pub struct PipelineDef<Pnt> {
            pub segments: Vec<PipelineSegmentDef<Pnt>>,
        }

        impl <Pnt> PipelineDef<Pnt> {
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

        pub type PipelineStepCtx = PipelineStepDef<PointCtx>;
        pub type PipelineStep = PipelineStepDef<Point>;


        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct PipelineStepDef<Pnt> {
            pub entry: MessageKind,
            pub exit: MessageKind,
            pub blocks: Vec<PayloadBlockDef<Pnt>>,
        }


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
        pub type PipelineStop = PipelineStopDef<Point>;

        #[derive(Debug, Clone, Serialize, Deserialize )]
        pub enum PipelineStopDef<Pnt> {
            Internal,
            Call(CallDef<Pnt>),
            Respond,
            Point(Pnt),
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

        impl Selector<PipelineSelector> {
            pub fn is_match(&self, m: &RequestCore) -> Result<(), MsgErr> {
                self.pattern.is_match(m)
            }
        }

        impl Selector<MsgPipelineSelector> {
            pub fn is_match(&self, m: &RequestCore) -> Result<(), MsgErr> {
                self.pattern.is_match(m)
            }
        }

        impl Selector<RcPipelineSelector> {
            pub fn is_match(&self, m: &RequestCore) -> Result<(), MsgErr> {
                self.pattern.is_match(m)
            }
        }

        impl Selector<HttpPipelineSelector> {
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
        pub enum MessageKind {
            Request,
            Response,
        }

        pub enum PipelinesSubScope {
            Msg(ConfigScope<EntityKind, Selector<MsgPipelineSelector>>),
            Http(ConfigScope<EntityKind, Selector<HttpPipelineSelector>>),
            Rc(ConfigScope<EntityKind, Selector<RcPipelineSelector>>),
        }

        pub enum ScopeType {
            Bind,
            Msg,
            Http,
            Rc,
        }
    }
}
