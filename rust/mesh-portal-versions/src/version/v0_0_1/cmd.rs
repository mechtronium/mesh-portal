pub mod command {
    use serde::{Deserialize, Serialize};



    pub mod common {
        use std::collections::HashMap;
        use std::convert::{TryFrom, TryInto};
        use std::ops::{Deref, DerefMut};

        use serde::{Deserialize, Serialize};

        use crate::error::MsgErr;
        use crate::version::v0_0_1::payload::payload::{Payload, PayloadMap};

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum StateSrc {
            Stateless,
            StatefulDirect(Payload),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum PropertyMod {
            Set {
                key: String,
                value: String,
                lock: bool,
            },
            UnSet(String),
        }

        impl PropertyMod {
            pub fn set_or<E>(&self, err: E) -> Result<String, E> {
                match self {
                    Self::Set { key, value, lock } => Ok(value.clone()),
                    Self::UnSet(_) => Err(err),
                }
            }

            pub fn opt(&self) -> Option<String> {
                match self {
                    Self::Set { key, value, lock } => Some(value.clone()),
                    Self::UnSet(_) => None,
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct SetProperties {
            pub map: HashMap<String, PropertyMod>,
        }

        impl Default for SetProperties {
            fn default() -> Self {
                Self {
                    map: Default::default(),
                }
            }
        }

        impl SetProperties {
            pub fn new() -> Self {
                Self {
                    map: HashMap::new(),
                }
            }

            pub fn append(&mut self, properties: SetProperties) {
                for (_, property) in properties.map.into_iter() {
                    self.push(property);
                }
            }

            pub fn push(&mut self, property: PropertyMod) {
                match &property {
                    PropertyMod::Set { key, value, lock } => {
                        self.map.insert(key.clone(), property);
                    }
                    PropertyMod::UnSet(key) => {
                        self.map.insert(key.clone(), property);
                    }
                }
            }
        }

        impl Deref for SetProperties {
            type Target = HashMap<String, PropertyMod>;

            fn deref(&self) -> &Self::Target {
                &self.map
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum SetLabel {
            Set(String),
            SetValue { key: String, value: String },
            Unset(String),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct SetRegistry {
            pub labels: Vec<SetLabel>,
        }

        impl Deref for SetRegistry {
            type Target = Vec<SetLabel>;

            fn deref(&self) -> &Self::Target {
                &self.labels
            }
        }

        impl DerefMut for SetRegistry {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.labels
            }
        }

        impl Default for SetRegistry {
            fn default() -> Self {
                Self {
                    labels: Default::default(),
                }
            }
        }
    }
}

pub mod request {
    use crate::error::{MsgErr, StatusErr};
    use crate::version::v0_0_1::bin::Bin;
    use crate::version::v0_0_1::cmd::request::create::Create;
    use crate::version::v0_0_1::cmd::request::get::Get;
    use crate::version::v0_0_1::cmd::request::query::Query;
    use crate::version::v0_0_1::cmd::request::select::Select;
    use crate::version::v0_0_1::cmd::request::set::Set;
    use crate::version::v0_0_1::cmd::request::update::Update;
    use crate::version::v0_0_1::entity::response::ResponseCore;
    use crate::version::v0_0_1::fail;
    use crate::version::v0_0_1::fail::{BadRequest, Fail, NotFound};
    use crate::version::v0_0_1::id::id::{GenericKind, GenericKindBase, Meta, Point};
    use crate::version::v0_0_1::payload::payload::{Errors, Payload, Primitive};
    use crate::version::v0_0_1::selector::selector::KindSelector;
    use crate::version::v0_0_1::util::ValueMatcher;
    use http::status::InvalidStatusCode;
    use http::{HeaderMap, Request, StatusCode, Uri};
    use serde::{Deserialize, Serialize};
    use crate::version::v0_0_1::entity::MethodKind;
    use crate::version::v0_0_1::http::HttpMethod;
    use crate::version::v0_0_1::msg::MsgMethod;

    #[derive(Debug, Clone, Serialize,Deserialize, Eq,PartialEq)]
    pub enum Method {
        Cmd(Rc),
        Http(#[serde(with = "http_serde::method")]HttpMethod),
        Msg(MsgMethod),
    }

    impl ValueMatcher<Method> for Method {
        fn is_match(&self, x: &Method) -> Result<(), ()> {
            if x == self {
                Ok(())
            } else {
                Err(())
            }
        }
    }


    impl Method {

        pub fn kind(&self) -> MethodKind {
            match self {
                Method::Cmd(_) => MethodKind::Cmd,
                Method::Http(_) => MethodKind::Http,
                Method::Msg(_) => MethodKind::Msg
            }
        }
    }

    impl ToString for Method {
        fn to_string(&self) -> String {
            match self {
                Method::Cmd(_) => "Rc".to_string(),
                Method::Http(method) => method.to_string(),
                Method::Msg(msg) => msg.to_string(),
            }
        }
    }

    impl Into<RequestCore> for Method {
        fn into(self) -> RequestCore {
            RequestCore {
                headers: Default::default(),
                method: self,
                uri: Uri::from_static("/"),
                body: Payload::Empty,
            }
        }
    }

    #[derive(Debug, Clone, Serialize,Deserialize)]
    pub struct RequestCore {
        #[serde(with = "http_serde::header_map")]
        pub headers: HeaderMap,
        pub method: Method,
        #[serde(with = "http_serde::uri")]
        pub uri: Uri,
        pub body: Payload,
    }

    impl RequestCore {
        pub fn kind(&self) -> MethodKind {
            self.method.kind()
        }
    }

    impl From<http::Request<Bin>> for RequestCore {
        fn from(request: Request<Bin>) -> Self {
            Self {
                headers: request.headers().clone(),
                method: Method::Http(request.method().clone()),
                uri: request.uri().clone(),
                body: Payload::Bin(request.body().clone()),
            }
        }
    }

    impl TryInto<http::Request<Bin>> for RequestCore {
        type Error = MsgErr;

        fn try_into(self) -> Result<http::Request<Bin>, MsgErr> {
            let mut builder = http::Request::builder();
            for (name, value) in self.headers {
                match name {
                    Some(name) => {
                        builder =
                            builder.header(name.as_str(), value.to_str()?.to_string().as_str());
                    }
                    None => {}
                }
            }
            match self.method {
                Method::Http(method) => {
                    builder = builder.method(method).uri(self.uri);
                    Ok(builder.body(self.body.to_bin()?)?)
                }
                _ => Err("cannot convert to http response".into()),
            }
        }
    }

    impl Default for RequestCore {
        fn default() -> Self {
            Self {
                headers: Default::default(),
                method: Method::Msg(Default::default()),
                uri: Uri::from_static("/"),
                body: Payload::Empty,
            }
        }
    }

    impl RequestCore {
        pub fn with_new_payload(self, payload: Payload) -> Self {
            Self {
                headers: self.headers,
                uri: self.uri,
                method: self.method,
                body: payload,
            }
        }

        pub fn not_found(&self) -> ResponseCore {
            ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(404u16).unwrap(),
                body: Payload::Empty,
            }
        }

        pub fn ok(&self, payload: Payload) -> ResponseCore {
            ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(200u16).unwrap(),
                body: payload,
            }
        }

        pub fn fail(&self, error: &str) -> ResponseCore {
            let errors = Errors::default(error);
            ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(500u16).unwrap(),
                body: Payload::Errors(errors),
            }
        }

        pub fn err<E: StatusErr>(&self, error: E) -> ResponseCore {
            let errors = Errors::default(error.message().as_str());
            let status = match StatusCode::from_u16(error.status()) {
                Ok(status) => status,
                Err(_) => StatusCode::from_u16(500u16).unwrap(),
            };
            println!("----->   returning STATUS of {}", status.as_str());
            ResponseCore {
                headers: Default::default(),
                status,
                body: Payload::Errors(errors),
            }
        }
    }

    #[derive(Debug, Clone,Serialize,Deserialize)]
    pub enum Rc {
        Create(Create),
        Select(Select),
        Update(Update),
        Query(Query),
        Get(Get),
        Set(Set),
    }

    impl PartialEq<Self> for Rc {
        fn eq(&self, other: &Self) -> bool {
            self.get_type() == other.get_type()
        }
    }

    impl Eq for Rc {

    }

    impl Rc {
        pub fn get_type(&self) -> RcCommandType {
            match self {
                Rc::Create(_) => RcCommandType::Create,
                Rc::Select(_) => RcCommandType::Select,
                Rc::Update(_) => RcCommandType::Update,
                Rc::Query(_) => RcCommandType::Query,
                Rc::Get(_) => RcCommandType::Get,
                Rc::Set(_) => RcCommandType::Set,
            }
        }
    }

    /*
    impl Rc {
        pub fn command_handler(&self, request_to: &Address) -> Result<Address,Error> {
            match self {
                Rc::Create(create) => { Ok(create.template.point.parent.clone()) }
                Rc::Select(select) => { Ok(select.pattern.query_root()) }
                Rc::Update(_) => {request_to.clone()}
                Rc::Query(_) => { request_to.clone()}
                Rc::GET(_) => {request_to.parent().as_ref().ok_or("expected parent for get request").clone()}
                Rc::Set(_) => {request_to.parent().as_ref().ok_or("expected parent for set request").clone()}
            }
        }
    }

     */

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
    pub enum RcCommandType {
        Create,
        Select,
        Update,
        Query,
        Get,
        Set,
    }

    impl ValueMatcher<Rc> for Rc {
        fn is_match(&self, x: &Rc) -> Result<(), ()> {
            if self.get_type() == x.get_type() {
                Ok(())
            } else {
                Err(())
            }
        }
    }

    impl ToString for Rc {
        fn to_string(&self) -> String {
            format!("Rc<{}>", self.get_type().to_string())
        }
    }

    pub mod set {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::cmd::command::common::SetProperties;
        use crate::version::v0_0_1::id::id::{Point, PointCtx, PointVar};
        use crate::version::v0_0_1::parse::Env;
        use serde::{Deserialize, Serialize};
        use crate::version::v0_0_1::util::ToResolved;

        pub type Set = SetDef<Point>;
        pub type SetCtx = SetDef<PointCtx>;
        pub type SetVar = SetDef<PointVar>;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct SetDef<Pnt> {
            pub point: Pnt,
            pub properties: SetProperties,
        }

        impl ToResolved<Set> for SetVar {
            fn to_resolved(self, env: &Env) -> Result<Set, MsgErr> {
                let set: SetCtx = self.to_resolved(env)?;
                set.to_resolved(env)
            }
        }

        impl ToResolved<SetCtx> for SetVar {
            fn to_resolved(self, env: &Env) -> Result<SetCtx, MsgErr> {
                Ok(SetCtx {
                    point: self.point.to_resolved(env)?,
                    properties: self.properties,
                })
            }
        }

        impl ToResolved<Set> for SetCtx {
            fn to_resolved(self, env: &Env) -> Result<Set, MsgErr> {
                Ok(Set {
                    point: self.point.to_resolved(env)?,
                    properties: self.properties,
                })
            }
        }
    }

    pub mod get {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::cmd::command::common::SetProperties;
        use crate::version::v0_0_1::id::id::{Point, PointCtx, PointVar};
        use crate::version::v0_0_1::parse::Env;
        use serde::{Deserialize, Serialize};
        use crate::version::v0_0_1::util::ToResolved;

        pub type Get = GetDef<Point>;
        pub type GetCtx = GetDef<PointCtx>;
        pub type GetVar = GetDef<PointVar>;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct GetDef<Pnt> {
            pub point: Pnt,
            pub op: GetOp,
        }

        impl ToResolved<Get> for GetVar {
            fn to_resolved(self, env: &Env) -> Result<Get, MsgErr> {
                let set: GetCtx = self.to_resolved(env)?;
                set.to_resolved(env)
            }
        }

        impl ToResolved<GetCtx> for GetVar {
            fn to_resolved(self, env: &Env) -> Result<GetCtx, MsgErr> {
                Ok(GetCtx {
                    point: self.point.to_resolved(env)?,
                    op: self.op,
                })
            }
        }

        impl ToResolved<Get> for GetCtx {
            fn to_resolved(self, env: &Env) -> Result<Get, MsgErr> {
                Ok(Get {
                    point: self.point.to_resolved(env)?,
                    op: self.op,
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum GetOp {
            State,
            Properties(Vec<String>),
        }
    }

    pub mod create {
        use std::convert::TryInto;

        use serde::{Deserialize, Serialize};

        use crate::error::MsgErr;
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::cmd::command::common::{
            SetProperties, SetRegistry, StateSrc,
        };
        use crate::version::v0_0_1::id::id::{
            GenericKind, HostKey, Point, PointCtx, PointSeg, PointVar,
        };
        use crate::version::v0_0_1::parse::Env;
        use crate::version::v0_0_1::payload::payload::{Payload, Primitive};
        use crate::version::v0_0_1::selector::selector::SpecificSelector;
        use crate::version::v0_0_1::util::{ConvertFrom, ToResolved};

        pub enum PointTemplateSeg {
            ExactSeg(PointSeg),
            Wildcard(String),
        }

        impl PointTemplateSeg {
            pub fn is_wildcard(&self) -> bool {
                match self {
                    PointTemplateSeg::ExactSeg(_) => false,
                    PointTemplateSeg::Wildcard(_) => true,
                }
            }
        }

        pub type Template = TemplateDef<PointTemplate>;
        pub type TemplateCtx = TemplateDef<PointTemplateCtx>;
        pub type TemplateVar = TemplateDef<PointTemplateVar>;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct TemplateDef<Pnt> {
            pub point: Pnt,
            pub kind: KindTemplate,
        }

        impl ToResolved<Template> for TemplateVar {
            fn to_resolved(self, env: &Env) -> Result<Template, MsgErr> {
                let template: TemplateCtx = self.to_resolved(env)?;
                template.to_resolved(env)
            }
        }

        impl ToResolved<TemplateCtx> for TemplateVar {
            fn to_resolved(self, env: &Env) -> Result<TemplateCtx, MsgErr> {
                let point : PointTemplateCtx = self.point.to_resolved(env)?;

                let template = TemplateCtx {
                    point,
                    kind: KindTemplate {
                        kind: "ArtifactBundle".to_string(),
                        sub_kind: None,
                        specific: None,
                    },
                };
                Ok(template)
            }
        }
        impl ToResolved<Template> for TemplateCtx {
            fn to_resolved(self, env: &Env) -> Result<Template, MsgErr> {
                let point = self.point.to_resolved(env)?;

                let template = Template {
                    point,
                    kind: KindTemplate {
                        kind: "ArtifactBundle".to_string(),
                        sub_kind: None,
                        specific: None,
                    },
                };
                Ok(template)
            }
        }

        impl Template {
            pub fn new(point: PointTemplate, kind: KindTemplate) -> Self {
                Self { point, kind }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct KindTemplate {
            pub kind: String,
            pub sub_kind: Option<String>,
            pub specific: Option<SpecificSelector>,
        }

        impl TryInto<GenericKind> for KindTemplate {
            type Error = MsgErr;

            fn try_into(self) -> Result<GenericKind, Self::Error> {
                if self.specific.is_some() {
                    return Err("cannot create a ResourceKind from a specific pattern when using KindTemplate".into());
                }
                Ok(GenericKind {
                    kind: self.kind,
                    sub_kind: self.sub_kind,
                    specific: None,
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Require {
            File(String),
            Auth(String),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fulfillment {
            File { name: String, content: Bin },
            Complete,
        }

        pub type Create = CreateDef<Point>;
        pub type CreateVar = CreateDef<PointVar>;
        pub type CreateCtx = CreateDef<PointCtx>;

        impl ToResolved<Create> for CreateVar {
            fn to_resolved(self, env: &Env) -> Result<Create, MsgErr> {
                let create: CreateCtx = self.to_resolved(env)?;
                create.to_resolved(env)
            }
        }


        impl ToResolved<CreateCtx> for CreateVar {
            fn to_resolved(self, env: &Env) -> Result<CreateCtx, MsgErr> {
                let template = self.template.to_resolved(env)?;
                Ok(CreateCtx {
                    template,
                    properties: self.properties,
                    strategy: self.strategy,
                    registry: self.registry,
                    state: self.state,
                })
            }
        }

        impl ToResolved<Create> for CreateCtx {
            fn to_resolved(self, env: &Env) -> Result<Create, MsgErr> {
                let template = self.template.to_resolved(env)?;
                Ok(Create {
                    template,
                    properties: self.properties,
                    strategy: self.strategy,
                    registry: self.registry,
                    state: self.state,
                })
            }
        }




        #[derive(Debug, Clone,Serialize,Deserialize)]
        pub struct CreateDef<Pnt> {
            pub template: TemplateDef<PointTemplateDef<Pnt>>,
            pub properties: SetProperties,
            pub strategy: Strategy,
            pub registry: SetRegistry,
            pub state: StateSrc,
        }

        impl Create {
            pub fn fulfillment(mut self, bin: Bin) -> Create {
                Create {
                    template: self.template,
                    state: StateSrc::StatefulDirect(Payload::Bin(bin)),
                    properties: self.properties,
                    strategy: self.strategy,
                    registry: self.registry,
                }
            }
        }


        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub enum Strategy {
            Commit,
            Ensure,
        }

        pub type PointTemplate = PointTemplateDef<Point>;
        pub type PointTemplateCtx = PointTemplateDef<PointCtx>;
        pub type PointTemplateVar = PointTemplateDef<PointVar>;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct PointTemplateDef<Pnt> {
            pub parent: Pnt,
            pub child_segment_template: PointSegFactory,
        }

        impl ToResolved<PointTemplateCtx> for PointTemplateVar {
            fn to_resolved(self, env: &Env) -> Result<PointTemplateCtx, MsgErr> {
                let parent = self.parent.to_resolved(env)?;
                Ok(PointTemplateCtx {
                    parent,
                    child_segment_template: self.child_segment_template
                })
            }
        }

        impl ToResolved<PointTemplate> for PointTemplateCtx{
            fn to_resolved(self, env: &Env) -> Result<PointTemplate, MsgErr> {
                let parent = self.parent.to_resolved(env)?;
                Ok(PointTemplate{
                    parent,
                    child_segment_template: self.child_segment_template
                })
            }
        }

        impl ToResolved<PointTemplate> for PointTemplateVar {
            fn to_resolved(self, env: &Env) -> Result<PointTemplate, MsgErr> {
                let ctx: PointTemplateCtx = self.to_resolved(env)?;
                ctx.to_resolved(env)
            }
        }



        #[derive(Debug, Clone, strum_macros::Display, Serialize, Deserialize)]
        pub enum PointSegFactory {
            Exact(String),
            Pattern(String), // must have a '%'
        }
    }

    pub mod select {
        use std::collections::{HashMap, HashSet};
        use std::convert::{TryFrom, TryInto};
        use std::marker::PhantomData;

        use serde::{Deserialize, Serialize};

        use crate::error::MsgErr;
        use crate::version::v0_0_1::fail::{BadCoercion, Fail};
        use crate::version::v0_0_1::id::id::Point;
        use crate::version::v0_0_1::parse::Env;
        use crate::version::v0_0_1::particle::particle::Stub;
        use crate::version::v0_0_1::payload::payload::{
            MapPattern, Payload, PayloadList, Primitive, PrimitiveType,
        };
        use crate::version::v0_0_1::selector::selector::{Hop, HopCtx, HopVar, PointKindHierarchy, PointSelector, PointSelectorDef};
        use crate::version::v0_0_1::util::{ConvertFrom, ToResolved};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum SelectIntoPayload {
            Stubs,
            Points,
        }

        impl SelectIntoPayload {
            pub fn to_primitive(&self, stubs: Vec<Stub>) -> Result<PayloadList, MsgErr> {
                match self {
                    SelectIntoPayload::Stubs => {
                        let stubs: Vec<Box<Payload>> = stubs
                            .into_iter()
                            .map(|stub| Box::new(Payload::Stub(stub)))
                            .collect();
                        let stubs = PayloadList { list: stubs };
                        Ok(stubs)
                    }
                    SelectIntoPayload::Points => {
                        let pointes: Vec<Box<Payload>> = stubs
                            .into_iter()
                            .map(|stub| Box::new(Payload::Point(stub.point)))
                            .collect();
                        let stubs = PayloadList { list: pointes };
                        Ok(stubs)
                    }
                }
            }
        }

        pub type Select = SelectDef<Hop>;
        pub type SelectCtx = SelectDef<Hop>;
        pub type SelectVar = SelectDef<Hop>;

        impl ToResolved<Select> for Select{
            fn to_resolved(self, env: &Env) -> Result<Select, MsgErr> {
                Ok(self)
            }
        }

        #[derive(Debug, Clone,Serialize,Deserialize)]
        pub struct SelectDef<Hop> {
            pub pattern: PointSelectorDef<Hop>,
            pub properties: PropertiesPattern,
            pub into_payload: SelectIntoPayload,
            pub kind: SelectKind,
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum SelectKind {
            Initial,
            SubSelect {
                point: Point,
                hops: Vec<Hop>,
                hierarchy: PointKindHierarchy,
            },
        }

        impl Select {
            pub fn sub_select(
                self,
                point: Point,
                hops: Vec<Hop>,
                hierarchy: PointKindHierarchy,
            ) -> SubSelect {
                SubSelect {
                    point,
                    pattern: self.pattern,
                    properties: self.properties,
                    into_payload: self.into_payload,
                    hops,
                    hierarchy,
                }
            }
        }

        impl TryInto<SubSelect> for Select {
            type Error = MsgErr;

            fn try_into(self) -> Result<SubSelect, Self::Error> {
                if let SelectKind::SubSelect {
                    point,
                    hops,
                    hierarchy,
                } = self.kind
                {
                    Ok(SubSelect {
                        point,
                        pattern: self.pattern,
                        properties: self.properties,
                        into_payload: self.into_payload,
                        hops: hops,
                        hierarchy,
                    })
                } else {
                    Err("Not of kind SubSelector".into())
                }
            }
        }

        #[derive(Debug, Clone )]
        pub struct SubSelect {
            pub point: Point,
            pub pattern: PointSelector,
            pub properties: PropertiesPattern,
            pub into_payload: SelectIntoPayload,
            pub hops: Vec<Hop>,
            pub hierarchy: PointKindHierarchy,
        }

        impl Into<Select> for SubSelect {
            fn into(self) -> Select {
                Select {
                    pattern: self.pattern,
                    properties: self.properties,
                    into_payload: self.into_payload,
                    kind: SelectKind::SubSelect {
                        point: self.point,
                        hops: self.hops,
                        hierarchy: self.hierarchy,
                    },
                }
            }
        }

        impl SubSelect {
            pub fn sub_select(
                &self,
                point: Point,
                hops: Vec<Hop>,
                hierarchy: PointKindHierarchy,
            ) -> SubSelect {
                SubSelect {
                    point,
                    pattern: self.pattern.clone(),
                    properties: self.properties.clone(),
                    into_payload: self.into_payload.clone(),
                    hops,
                    hierarchy,
                }
            }
        }

        impl Select {
            fn new(pattern: PointSelector) -> Self {
                Self {
                    pattern,
                    properties: Default::default(),
                    into_payload: SelectIntoPayload::Stubs,
                    kind: SelectKind::Initial,
                }
            }
        }

        pub type PropertiesPattern = MapPattern;
    }

    pub mod update {
        use std::convert::TryInto;

        use serde::{Deserialize, Serialize};

        use crate::error::MsgErr;
        use crate::version::v0_0_1::cmd::command::common::SetProperties;
        use crate::version::v0_0_1::id::id::Point;
        use crate::version::v0_0_1::payload::payload::Payload;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Update {
            pub payload: Payload,
        }
    }

    pub mod query {
        use std::convert::TryInto;

        use serde::{Deserialize, Serialize};

        use crate::error::MsgErr;
        use crate::version::v0_0_1::cmd::request::Rc;
        use crate::version::v0_0_1::selector::selector::PointKindHierarchy;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Query {
            PointKindHierarchy,
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum QueryResult {
            PointKindHierarchy(PointKindHierarchy),
        }

        impl TryInto<PointKindHierarchy> for QueryResult {
            type Error = MsgErr;

            fn try_into(self) -> Result<PointKindHierarchy, MsgErr> {
                match self {
                    QueryResult::PointKindHierarchy(hierarchy) => Ok(hierarchy),
                }
            }
        }

        impl ToString for QueryResult {
            fn to_string(&self) -> String {
                match self {
                    QueryResult::PointKindHierarchy(hierarchy) => hierarchy.to_string(),
                }
            }
        }

        impl Into<Rc> for Query {
            fn into(self) -> Rc {
                Rc::Query(self)
            }
        }
    }
}
