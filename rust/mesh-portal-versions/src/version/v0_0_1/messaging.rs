use std::sync::Arc;
use http::{HeaderMap, StatusCode};
use crate::error::{MsgErr, StatusErr};
use crate::version::v0_0_1::bin::Bin;
use crate::version::v0_0_1::config::config::bind::RouteSelector;
use crate::version::v0_0_1::id::id::ToPort;
use crate::version::v0_0_1::messaging::messaging::{Message, Request, Response, RootRequestCtx};
use crate::version::v0_0_1::payload::payload::{Errors, Payload};
use crate::version::v0_0_1::util::{uuid, ValueMatcher};
use serde::{Serialize,Deserialize};

pub mod messaging {
    use http::status::InvalidStatusCode;
    use std::collections::HashMap;
    use std::convert::TryInto;
    use std::ops::Deref;
    use std::sync::Arc;

    use http::{StatusCode, Uri};
    use serde::{Deserialize, Serialize};

    use crate::error::{MsgErr, StatusErr};
    use crate::version::v0_0_1::command::request::RequestCore;
    use crate::version::v0_0_1::messaging::ResponseCore;
    use crate::version::v0_0_1::id::id::{Point, Port, ToPort, Uuid};
    use crate::version::v0_0_1::log::{PointLogger, SpanLogger};
    use crate::version::v0_0_1::messaging::{AsyncMessenger, AsyncMessengerRelay, Router, SyncMessenger, SyncMessengerRelay};
    use crate::version::v0_0_1::payload::payload::{Errors, MsgCall, Payload};
    use crate::version::v0_0_1::security::{
        Access, AccessGrant, EnumeratedAccess, EnumeratedPrivileges, Permissions, Privilege,
        Privileges,
    };
    use crate::version::v0_0_1::selector::selector::{PointKindHierarchy, PointSelector};
    use crate::version::v0_0_1::util::uuid;

    pub struct RootRequestCtx<I> {
        pub input: I,
        pub request: Request,
        session: Option<Session>,
        logger: SpanLogger,
    }

    impl RootRequestCtx<Request> {
        pub fn new(request: Request, logger: SpanLogger) -> Self {
            Self {
                request: request.clone(),
                input: request.clone(),
                logger,
                session: None,
            }
        }
    }

    impl<I> RootRequestCtx<I> {
        pub fn transform_input<I2>(self) -> Result<RootRequestCtx<I2>, MsgErr>
        where
            I2: TryFrom<I,Error=MsgErr>,
        {
            Ok(RootRequestCtx {
                logger: self.logger,
                request: self.request,
                input: I2::try_from(self.input)?,
                session: self.session,
            })
        }

        pub fn push<'a>(&'a self) -> RequestCtx<'a, I> {
            RequestCtx::new(self, &self.input, self.logger.clone())
        }
    }

    pub struct RequestCtx<'a, I> {
        root: &'a RootRequestCtx<I>,
        parent: Option<Box<RequestCtx<'a, I>>>,
        pub input: &'a I,
        pub logger: SpanLogger,
    }

    impl<'a, I> Deref for RequestCtx<'a, I> {
        type Target = I;

        fn deref(&self) -> &Self::Target {
            self.input
        }
    }

    impl<'a, I> RequestCtx<'a, I> {
        pub fn new(root: &'a RootRequestCtx<I>, input: &'a I, logger: SpanLogger) -> Self {
            Self {
                root,
                parent: None,
                input,
                logger,
            }
        }

        pub fn push(self) -> RequestCtx<'a, I> {
            RequestCtx {
                root: self.root,
                input: self.input,
                logger: self.logger.span(),
                parent: Some(Box::new(self)),
            }
        }

        pub fn pop(self) -> Option<RequestCtx<'a, I>> {
            match self.parent {
                None => None,
                Some(parent) => Some(*parent)
            }
        }
    }

    impl<'a> RequestCtx<'a, &mut Request> {
        pub fn ok_payload(self, payload: Payload) -> ResponseCore {
            self.input.core.ok(payload)
        }

        pub fn not_found(self) -> ResponseCore {
            self.input.core.not_found()
        }

        pub fn err(self, err: MsgErr) -> ResponseCore {
            self.input.core.err(err)
        }
    }

    impl<'a> RequestCtx<'a, Response> {
        pub fn pass(self) -> ResponseCore {
            self.input.core.clone()
        }

        pub fn not_found(self) -> ResponseCore {
            let mut core = self.input.core.clone();
            core.status = StatusCode::from_u16(404).unwrap();
            core
        }

        pub fn err(self, err: MsgErr) -> ResponseCore {
            let mut core = self.input.core.clone();
            let status = match StatusCode::from_u16(err.status()) {
                Ok(status) => status,
                Err(_) => StatusCode::from_u16(500).unwrap(),
            };
            core.status = status;
            // somehow set the body to a proper Err
            //            core.body =
            core
        }
    }

    pub trait ToMessage {
        fn to_message_in(self) -> MessageIn;
        fn to_message_out(self, reply_to: Uuid) -> MessageOut;
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Request {
        pub id: String,
        pub agent: Agent,
        pub scope: Scope,
        pub handling: Handling,
        pub from: Port,
        pub to: Port,
        pub core: RequestCore,
    }


    impl ToMessage for Request {
        fn to_message_in(self) -> MessageIn {
            MessageIn::new(Message::Request(self))
        }

        fn to_message_out(self, reply_to: Uuid) -> MessageOut {
            MessageOut::new(Message::Request(self), reply_to)
        }
    }

    impl Request {
        pub fn result<E: StatusErr>(self, result: Result<ResponseCore, E>) -> Response {
            match result {
                Ok(core) => Response {
                    id: uuid(),
                    to: self.from,
                    from: self.to,
                    core,
                    response_to: self.id,
                },
                Err(err) => {
                    let core = self.core.err(err);
                    Response {
                        id: uuid(),
                        to: self.from,
                        from: self.to,
                        core,
                        response_to: self.id,
                    }
                }
            }
        }

        pub fn payload_result<E: StatusErr>(self, result: Result<Payload, E>) -> Response {
            match result {
                Ok(payload) => self.ok_payload(payload),
                Err(err) => {
                    let core = self.core.err(err);
                    Response {
                        id: uuid(),
                        to: self.from,
                        from: self.to,
                        core,
                        response_to: self.id,
                    }
                }
            }
        }

        pub fn err(self, err: MsgErr) -> Response {
            let core = self.core.err(err);
            Response {
                id: uuid(),
                to: self.from,
                from: self.to,
                core,
                response_to: self.id,
            }
        }
    }

    impl Request {
        pub fn new(core: RequestCore, from: Point, to: Point) -> Self {
            Self {
                id: uuid(),
                agent: Agent::Anonymous,
                scope: Scope::Full,
                handling: Default::default(),
                from: from.into(),
                to: to.into(),
                core,
            }
        }

        /*
        pub fn result<E>(self, result: Result<ResponseCore,E> ) -> Response where E: ToString {
            match result {
                Ok(core) => {
                    Response {
                        id: uuid(),
                        to: self.from,
                        from: self.to,
                        core,
                        response_to: self.id
                    }
                }
                Err(err) => {
                    self.fail(err.to_string().as_str())
                }
            }
        }

        pub fn payload_result<E>(self, result: Result<Payload,E> ) -> Response where E: ToString {
            match result {
                Ok(payload) => {
                    self.ok_payload(payload)
                }
                Err(err) => {
                    self.fail(err.to_string().as_str())
                }
            }
        }

         */

        pub fn ok(self) -> Response {
            let core = ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(200u16).unwrap(),
                body: Payload::Empty,
            };
            let response = Response {
                id: uuid(),
                from: self.to,
                to: self.from,
                core,
                response_to: self.id,
            };
            response
        }

        pub fn ok_payload(self, payload: Payload) -> Response {
            let core = ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(200u16).unwrap(),
                body: payload,
            };
            let response = Response {
                id: uuid(),
                from: self.to,
                to: self.from,
                core,
                response_to: self.id,
            };
            response
        }

        pub fn fail(self, error: &str) -> Response {
            let core = ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(500u16).unwrap(),
                body: Payload::Errors(Errors::default(error.to_string().as_str())),
            };
            let response = Response {
                id: uuid(),
                from: self.to,
                to: self.from,
                core,
                response_to: self.id,
            };
            response
        }

        pub fn not_found(self) -> Response {
            let core = ResponseCore {
                headers: Default::default(),
                status: StatusCode::from_u16(404u16).unwrap(),
                body: Payload::Empty,
            };
            let response = Response {
                id: uuid(),
                from: self.to,
                to: self.from,
                core,
                response_to: self.id,
            };
            response
        }

        pub fn status(self, status: u16) -> Response {
            fn process(
                request: &Request,
                status: u16,
            ) -> Result<Response, http::status::InvalidStatusCode> {
                let core = ResponseCore {
                    headers: Default::default(),
                    status: StatusCode::from_u16(status)?,
                    body: Payload::Empty,
                };
                let response = Response {
                    id: uuid(),
                    from: request.to.clone(),
                    to: request.from.clone(),
                    core,
                    response_to: request.id.clone(),
                };
                Ok(response)
            }
            match process(&self, status) {
                Ok(response) => response,
                Err(err) => self.fail(format!("bad status: {}", status).as_str()),
            }
        }
    }

    pub struct MessageIn {
        pub id: Uuid,
        pub message: Message,
    }

    impl MessageIn {
        pub fn new(message: Message) -> Self {
            let id = uuid();
            Self { id, message }
        }

        pub fn out(self, message: Message) -> MessageOut {
            MessageOut {
                reply_to: self.id,
                message,
            }
        }

        pub fn to(&self) -> Port {
            self.message.to()
        }
    }

    pub struct MessageOut {
        pub reply_to: Uuid,
        pub message: Message,
    }

    impl MessageOut {
        pub fn new(message: Message, reply_to: Uuid) -> Self {
            Self { message, reply_to }
        }
    }

    pub struct RequestBuilder {
        pub to: Option<Port>,
        pub from: Option<Port>,
        pub core: Option<RequestCore>,
        pub agent: Agent,
        pub session: Option<Session>,
        pub scope: Scope,
        pub handling: Handling,
    }

    impl RequestBuilder {
        pub fn new() -> Self {
            Self {
                ..Default::default()
            }
        }

        pub fn to<P: ToPort>(mut self, point: P) -> Self {
            self.to = Some(point.to_port());
            self
        }

        pub fn from<P: ToPort>(mut self, point: P) -> Self {
            self.from = Some(point.to_port());
            self
        }

        pub fn core(mut self, core: RequestCore) -> Self {
            self.core = Some(core);
            self
        }

        pub fn agent(mut self, agent: Agent) -> Self {
            self.agent = agent;
            self
        }

        pub fn session(mut self, session: Session) -> Self {
            self.session = Some(session);
            self
        }

        pub fn scope(mut self, scope: Scope) -> Self {
            self.scope = scope;
            self
        }

        pub fn handling(mut self, handling: Handling) -> Self {
            self.handling = handling;
            self
        }

        pub fn build(self) -> Result<Request, MsgErr> {
            Ok(Request {
                id: uuid(),
                to: self.to.ok_or("RequestBuilder: 'to' must be set")?,
                from: self.from.ok_or("RequestBuilder: 'from' must be set")?,
                core: self.core.ok_or("RequestBuilder: 'core' must be set")?,
                agent: self.agent,
                scope: self.scope,
                handling: self.handling,
            })
        }
    }

    impl Default for RequestBuilder {
        fn default() -> Self {
            Self {
                to: None,
                from: None,
                core: None,
                agent: Default::default(),
                session: None,
                scope: Default::default(),
                handling: Default::default(),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ProtoRequest {
        pub id: String,
        pub to: Option<Port>,
        pub core: Option<RequestCore>,
    }

    impl ProtoRequest {
        pub fn new() -> Self {
            Self {
                id: uuid(),
                to: Option::None,
                core: Option::None,
            }
        }

        pub fn validate(&self) -> Result<(), MsgErr> {
            self.to.as_ref().ok_or("request.to must be set")?;
            Ok(())
        }

        pub fn to<P>(&mut self, to: P)
        where
            P: ToPort,
        {
            self.to = Option::Some(to.to_port());
        }

        pub fn core(&mut self, core: RequestCore) {
            self.core = Option::Some(core);
        }

        pub fn into_request<P>(self, from: P, agent: Agent, scope: Scope) -> Result<Request, MsgErr>
        where
            P: ToPort,
        {
            self.validate()?;
            let core = self
                .core
                .or(Option::Some(Default::default()))
                .expect("expected RequestCore");
            let request = Request {
                id: self.id,
                from: from.to_port(),
                to: self.to.expect("expected to point"),
                core,
                agent,
                handling: Default::default(),
                scope,
            };
            Ok(request)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Response {
        pub id: String,
        pub from: Port,
        pub to: Port,
        pub core: ResponseCore,
        pub response_to: String,
    }

    impl ToMessage for Response {
        fn to_message_in(self) -> MessageIn {
            MessageIn::new(Message::Response(self))
        }

        fn to_message_out(self, reply_to: Uuid) -> MessageOut {
            MessageOut::new(Message::Response(self), reply_to)
        }
    }

    impl Response {
        pub fn new(core: ResponseCore, from: Point, to: Point, response_to: String) -> Self {
            Self {
                id: uuid(),
                to: to.into(),
                from: from.into(),
                core,
                response_to,
            }
        }

        pub fn ok_or(self) -> Result<Self, MsgErr> {
            if self.core.status.is_success() {
                Ok(self)
            } else {
                if let Payload::Text(error) = self.core.body {
                    Err(error.into())
                } else {
                    Err(format!("error code: {}", self.core.status).into())
                }
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Message {
        Request(Request),
        Response(Response),
    }

    impl Message {
        pub fn id(&self) -> Uuid {
            match self {
                Message::Request(request) => request.id.clone(),
                Message::Response(response) => response.id.clone(),
            }
        }

        pub fn payload(&self) -> Payload {
            match self {
                Message::Request(request) => request.core.body.clone(),
                Message::Response(response) => response.core.body.clone(),
            }
        }

        pub fn to(&self) -> Port {
            match self {
                Message::Request(request) => request.to.clone(),
                Message::Response(response) => response.to.clone(),
            }
        }
    }

    impl From<Request> for Message {
        fn from(request: Request) -> Self {
            Self::Request(request)
        }
    }

    impl From<Response> for Message {
        fn from(response: Response) -> Self {
            Self::Response(response)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum RequestTransform {
        Request(RequestCore),
        Response(ResponseCore),
    }

    pub enum ResponseKindExpected {
        None,
        Synch,          // requestor will wait for response
        Async(Payload), // The payload
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Agent {
        Anonymous,
        Authenticated(AuthedAgent),
    }

    impl Default for Agent {
        fn default() -> Self {
            Self::Anonymous
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct AuthedAgent {
        pub owner: Point,
        pub executor: Point,
    }

    impl AuthedAgent {
        pub fn new(point: Point) -> Self {
            Self {
                owner: point.clone(),
                executor: point,
            }
        }
    }

    impl TryInto<AuthedAgent> for Agent {
        type Error = MsgErr;

        fn try_into(self) -> Result<AuthedAgent, Self::Error> {
            match self {
                Agent::Anonymous => Err(MsgErr::new(401, "Authorization required")),
                Agent::Authenticated(auth) => Ok(auth),
            }
        }
    }

    impl Into<Agent> for AuthedAgent {
        fn into(self) -> Agent {
            Agent::Authenticated(self)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Session {
        pub id: String,
        pub attributes: HashMap<String, String>,
    }

    impl Session {
        pub fn get_preferred_username(&self) -> Option<String> {
            self.attributes
                .get(&"preferred_username".to_string())
                .cloned()
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Scope {
        Full,
        None,
        Grants(Vec<ScopeGrant>),
    }

    impl Scope {
        /*
        pub fn mask( &self, on: &AddressKindPath ) -> Access {
            match self {
                Scope::Full => {
                    access.clone()
                }
                Scope::None => {
                    Access::none()
                }
                Scope::Grants(grants) => {
                    let mut access  = access.clone();
                    let mut privileges = EnumeratedPrivileges::none();
                    let mut permissions = Permissions::full();
                    for grant in grants {
                       if grant.on.matches(on) {
                           match &grant.aspect {
                               ScopeGrantAspect::Perm(and) => permissions.and(and),
                               ScopeGrantAspect::Priv(and) =>  privileges.insert(and.clone())
                           }
                       }
                   }
                }
            }
        }

         */
    }

    impl Default for Scope {
        fn default() -> Self {
            Self::None
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ScopeGrant {
        pub on: PointSelector,
        pub kind: ScopeGrantKind,
        pub aspect: ScopeGrantAspect,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ScopeGrantKind {
        Or,
        And,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ScopeGrantAspect {
        Perm(Permissions),
        Priv(Privilege),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RequestAccess {
        pub permissions: Permissions,
        pub privileges: Privileges,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Roles {
        Full,
        None,
        Enumerated(Vec<String>),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Handling {
        kind: HandlingKind,
        priority: Priority,
        retries: Retries,
        timeout: Timeout,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum HandlingKind {
        Durable,   // Mesh will guarantee delivery eventually once Request call has returned
        Queued,    // Slower but more reliable delivery, message can be lost if a star crashes, etc
        Immediate, // Message should never touch a filesystem, it will be in memory for its entire journey for immediate processing
    }

    impl Default for Handling {
        fn default() -> Self {
            Self {
                kind: HandlingKind::Queued,
                priority: Default::default(),
                retries: Default::default(),
                timeout: Default::default(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Timeout {
        Never,
        Max,
        Medium,
        Min,
    }

    impl Default for Timeout {
        fn default() -> Self {
            Timeout::Medium
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Retries {
        None,
        Max,
        Medium,
        Min,
    }

    impl Default for Retries {
        fn default() -> Self {
            Retries::None
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Priority {
        High,
        Med,
        Low,
    }

    impl Default for Priority {
        fn default() -> Self {
            Self::Med
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Karma {
        Super,
        High,
        Med,
        Low,
        None,
    }

    impl Default for Karma {
        fn default() -> Self {
            Self::High
        }
    }
}

pub trait Router: Send + Sync {
    fn route(&self, message: Message);
}

pub struct AsyncMessengerRelay {
    pub proxy: Arc<dyn AsyncMessenger>
}

#[async_trait]
impl AsyncMessenger for AsyncMessengerRelay {
    async fn send(&self, request: Request) -> Response{
        self.proxy.send(request).await
    }
}


pub struct SyncMessengerRelay {
    pub proxy: Arc<dyn SyncMessenger>
}

impl SyncMessenger for SyncMessengerRelay {
    fn send(&self, request: Request) -> Response{
        self.proxy.send(request)
    }
}


#[async_trait]
pub trait AsyncMessenger: Send + Sync {
    async fn send(&self, request: Request) -> Response;
}

pub trait SyncMessenger: Send + Sync {
    fn send(&self, request: Request) -> Response;
}

pub struct InternalPipeline<H>
{
    pub selector: RouteSelector,
    pub handler: H,
}

impl<H> InternalPipeline<H>
{
    pub fn new(selector: RouteSelector, mut handler: H) -> Self {
        Self {
            selector,
            handler,
        }
    }
}

pub trait RequestHandler {
    fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr>;
}

#[async_trait]
pub trait AsyncRequestHandler: Sync+Send  {
    async fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr>;
}

impl RequestHandler for RequestHandlerRelay {
    fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr> {
        self.relay.handle(ctx)
    }
}

pub struct AsyncRequestHandlerRelay {
    pub relay: Box<dyn AsyncRequestHandler>
}

#[async_trait]
impl AsyncRequestHandler for AsyncRequestHandlerRelay {
    async fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr> {
        self.relay.handle(ctx).await
    }
}

pub struct InternalRequestHandler<H> {
    pipelines: Vec<InternalPipeline<H>>,
}

impl<H> InternalRequestHandler<H> {
    pub fn new() -> Self {
        Self {
            pipelines: vec![],
        }
    }

    pub fn add(&mut self, selector: RouteSelector, handler: H) {
        let pipeline = InternalPipeline { selector, handler };
        self.pipelines.push(pipeline);
    }

    pub fn select(&self, request: &Request ) -> Result<&H,()> {
        for pipeline in self.pipelines.iter() {
            if pipeline.selector.is_match(&request).is_ok() {
                return Ok(&pipeline.handler);
            }
        }
        Err(())
    }
}

impl InternalRequestHandler<RequestHandlerRelay> {
    pub fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr> {
        for pipeline in self.pipelines.iter() {
            if pipeline.selector.is_match(&ctx.request).is_ok() {
                let handler = &pipeline.handler;
                return handler.handle(ctx);
            }
        }
        Ok(ResponseCore::not_found())
    }
}

impl InternalRequestHandler<AsyncRequestHandlerRelay> {
    pub async fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr> {
        for pipeline in self.pipelines.iter() {
            if pipeline.selector.is_match(&ctx.request).is_ok() {
                return pipeline.handler.handle(ctx).await;
            }
        }
        Ok(ResponseCore::not_found())
    }
}

pub struct RequestHandlerRelay {
   pub relay: Box<dyn RequestHandler>
}

#[derive(
    Debug, Clone, Serialize, Deserialize, strum_macros::Display, strum_macros::EnumString,Eq,PartialEq
)]
pub enum MethodKind {
    Cmd,
    Msg,
    Http,
}

impl ValueMatcher<MethodKind> for MethodKind {
    fn is_match(&self, x: &MethodKind) -> Result<(), ()> {
        if self == x {
            Ok(())
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
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


    pub fn not_found() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(404u16).unwrap(),
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

    pub fn err(err: MsgErr) -> Self {
        let errors = Errors::default(err.to_string().as_str() );
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(err.status()).unwrap_or(StatusCode::from_u16(500u16).unwrap()),
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

    pub fn into_response<P>(self, from: P, to: P, response_to: String) -> Response where P: ToPort{
        Response {
            id: uuid(),
            from: from.to_port(),
            to: to.to_port(),
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
