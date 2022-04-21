pub mod entity {

    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize,strum_macros::Display,strum_macros::EnumString)]
    pub enum EntityKind {
        Rc,
        Msg,
        Http,
    }

    pub mod request {
        use crate::error::{MsgErr, StatusErr};
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::entity::entity::request::create::Create;
        use crate::version::v0_0_1::entity::entity::request::get::Get;
        use crate::version::v0_0_1::entity::entity::request::query::Query;
        use crate::version::v0_0_1::entity::entity::request::select::Select;
        use crate::version::v0_0_1::entity::entity::request::set::Set;
        use crate::version::v0_0_1::entity::entity::request::update::Update;
        use crate::version::v0_0_1::entity::entity::response::ResponseCore;
        use crate::version::v0_0_1::fail;
        use crate::version::v0_0_1::fail::{BadRequest, Fail, NotFound};
        use crate::version::v0_0_1::id::id::{GenericKind, GenericKindBase, Meta, Point};
        use crate::version::v0_0_1::payload::payload::{Errors, HttpMethod, Payload, Primitive};
        use crate::version::v0_0_1::selector::selector::KindPattern;
        use crate::version::v0_0_1::util::ValueMatcher;
        use http::status::InvalidStatusCode;
        use http::{HeaderMap, Request, StatusCode, Uri};
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Action {
            Rc(Rc),
            Http(#[serde(with = "http_serde::method")] HttpMethod),
            Msg(String),
        }

        impl ToString for Action {
            fn to_string(&self) -> String {
                match self {
                    Action::Rc(_) => "Rc".to_string(),
                    Action::Http(method) => method.to_string(),
                    Action::Msg(msg) => msg.to_string(),
                }
            }
        }

       impl Into<RequestCore> for Action {
            fn into(self) -> RequestCore {
                RequestCore {
                    headers: Default::default(),
                    action: self,
                    uri: Uri::from_static("/"),
                    body: Payload::Empty,
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct RequestCore {
            #[serde(with = "http_serde::header_map")]
            pub headers: HeaderMap,
            pub action: Action,
            #[serde(with = "http_serde::uri")]
            pub uri: Uri,
            pub body: Payload,
        }

        impl From<http::Request<Bin>> for RequestCore {
            fn from(request: Request<Bin>) -> Self {
                Self {
                    headers: request.headers().clone(),
                    action: Action::Http(request.method().clone()),
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
                match self.action {
                    Action::Http(method) => {
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
                    action: Action::Msg("Default".to_string()),
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
                    action: self.action,
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

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Rc {
            Create(Create),
            Select(Select),
            Update(Update),
            Query(Query),
            Get(Get),
            Set(Set),
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
            fn is_match(&self, x: &Rc) -> Result<(), crate::error::MsgErr> {
                if self.get_type() == x.get_type() {
                    Ok(())
                } else {
                    Err(format!(
                        "Rc command expected: '{}' found: '{}'",
                        self.to_string(),
                        x.to_string()
                    )
                    .into())
                }
            }
        }

        impl ToString for Rc {
            fn to_string(&self) -> String {
                format!("Rc<{}>", self.get_type().to_string())
            }
        }

        pub mod set {
            use crate::version::v0_0_1::command::command::common::SetProperties;
            use crate::version::v0_0_1::id::id::Point;
            use serde::{Deserialize, Serialize};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Set {
                pub point: Point,
                pub properties: SetProperties,
            }
        }

        pub mod get {
            use crate::version::v0_0_1::command::command::common::SetProperties;
            use crate::version::v0_0_1::id::id::Point;
            use serde::{Deserialize, Serialize};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Get {
                pub point: Point,
                pub op: GetOp,
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
            use crate::version::v0_0_1::command::command::common::{SetProperties, SetRegistry, StateSrc};
            use crate::version::v0_0_1::id::id::{GenericKind, HostKey, Point, PointSeg};
            use crate::version::v0_0_1::payload::payload::{Payload, Primitive};
            use crate::version::v0_0_1::selector::selector::SpecificSelector;
            use crate::version::v0_0_1::util::ConvertFrom;

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

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Template {
                pub point: PointTemplate,
                pub kind: KindTemplate,
            }

            impl Template {
                pub fn new(point: PointTemplate, kind: KindTemplate) -> Self {
                    Self { point, kind }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct KindTemplate {
                pub resource_type: String,
                pub kind: Option<String>,
                pub specific: Option<SpecificSelector>,
            }

            impl TryInto<GenericKind> for KindTemplate {
                type Error = MsgErr;

                fn try_into(self) -> Result<GenericKind, Self::Error> {
                    if self.specific.is_some() {
                        return Err("cannot create a ResourceKind from a specific pattern when using KindTemplate".into());
                    }
                    Ok(GenericKind {
                        kind: self.resource_type,
                        sub_kind: self.kind,
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

            pub struct CreateOp {
                pub template: Template,
                pub properties: SetProperties,
                pub strategy: Strategy,
                pub registry: SetRegistry,
                pub state: StateSrc,
                pub requirements: Vec<Require>,
            }

            impl CreateOp {
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

            impl Into<Create> for CreateOp {
                fn into(self) -> Create {
                    Create {
                        template: self.template,
                        state: self.state,
                        properties: self.properties,
                        strategy: self.strategy,
                        registry: self.registry,
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Create {
                pub template: Template,
                pub state: StateSrc,
                pub properties: SetProperties,
                pub strategy: Strategy,
                pub registry: SetRegistry,
            }

            impl Create {
                pub fn new(template: Template) -> Self {
                    Self {
                        template,
                        state: StateSrc::Stateless,
                        properties: Default::default(),
                        strategy: Strategy::Create,
                        registry: Default::default(),
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
            pub enum Strategy {
                Create,
                Apply,
                Ensure,
                HostedBy(HostKey),
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct PointTemplate {
                pub parent: Point,
                pub child_segment_template: PointSegFactory,
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
            use crate::version::v0_0_1::particle::particle::Stub;
            use crate::version::v0_0_1::payload::payload::{
                MapPattern, Payload, PayloadList, Primitive, PrimitiveType,
            };
            use crate::version::v0_0_1::selector::selector::{Hop, PointKindHierarchy, PointSelector};
            use crate::version::v0_0_1::util::ConvertFrom;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum SelectIntoPayload {
                Stubs,
                Addresses,
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
                        SelectIntoPayload::Addresses => {
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

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Select {
                pub pattern: PointSelector,
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

            #[derive(Debug, Clone, Serialize, Deserialize)]
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
            use crate::version::v0_0_1::command::command::common::SetProperties;
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
            use crate::version::v0_0_1::entity::entity::request::Rc;
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

    pub mod response {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::entity::entity::request::RequestCore;
        use crate::version::v0_0_1::fail;
        use crate::version::v0_0_1::fail::Fail;
        use crate::version::v0_0_1::id::id::{GenericKind, Meta, Point};
        use crate::version::v0_0_1::messaging::messaging::Response;
        use crate::version::v0_0_1::payload::payload::{Errors, Payload, Primitive};
        use crate::version::v0_0_1::util::unique_id;
        use http::response::Parts;
        use http::{HeaderMap, StatusCode};
        use serde::{Deserialize, Serialize};
        use std::sync::Arc;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct ResponseCore {
            #[serde(with = "http_serde::header_map")]
            pub headers: HeaderMap,

            #[serde(with = "http_serde::status_code")]
            pub status: StatusCode,

            pub body: Payload,
        }

        impl ResponseCore {
            pub fn ok_html(html: &str) -> Self {
                let bin = Arc::new(html.to_string().into_bytes());
                ResponseCore::ok(Payload::Bin(bin))
            }

            pub fn new() -> Self {
                ResponseCore {
                    headers: HeaderMap::new(),
                    status: StatusCode::from_u16(200u16).unwrap(),
                    body: Payload::Empty,
                }
            }

            pub fn ok(body: Payload) -> Self {
                Self {
                    headers: HeaderMap::new(),
                    status: StatusCode::from_u16(200u16).unwrap(),
                    body,
                }
            }

            pub fn server_error() -> Self {
                Self {
                    headers: HeaderMap::new(),
                    status: StatusCode::from_u16(500u16).unwrap(),
                    body: Payload::Empty,
                }
            }

            pub fn fail(message: &str) -> Self {
                let errors = Errors::default(message.clone());
                Self {
                    headers: HeaderMap::new(),
                    status: StatusCode::from_u16(500u16).unwrap(),
                    body: Payload::Errors(errors),
                }
            }

            pub fn with_new_payload(self, payload: Payload) -> Self {
                Self {
                    headers: self.headers,
                    status: self.status,
                    body: payload,
                }
            }

            pub fn is_ok(&self) -> bool {
                return self.status.is_success();
            }

            pub fn into_response(self, from: Point, to: Point, response_to: String) -> Response {
                Response {
                    id: unique_id(),
                    from,
                    to,
                    core: self,
                    response_to,
                }
            }
        }

        impl TryInto<http::response::Builder> for ResponseCore {
            type Error = MsgErr;

            fn try_into(self) -> Result<http::response::Builder, Self::Error> {
                let mut builder = http::response::Builder::new();

                for (name, value) in self.headers {
                    match name {
                        Some(name) => {
                            builder =
                                builder.header(name.as_str(), value.to_str()?.to_string().as_str());
                        }
                        None => {}
                    }
                }

                Ok(builder.status(self.status))
            }
        }

        impl TryInto<http::Response<Bin>> for ResponseCore {
            type Error = MsgErr;

            fn try_into(self) -> Result<http::Response<Bin>, Self::Error> {
                let mut builder = http::response::Builder::new();

                for (name, value) in self.headers {
                    match name {
                        Some(name) => {
                            builder =
                                builder.header(name.as_str(), value.to_str()?.to_string().as_str());
                        }
                        None => {}
                    }
                }

                let response = builder.status(self.status).body(self.body.to_bin()?)?;
                Ok(response)
            }
        }
    }
}
