use std::collections::HashMap;
use std::ops::Deref;
use std::sync::{Arc, RwLock};
use http::{HeaderMap, StatusCode, Uri};
use crate::error::{MsgErr, StatusErr};
use crate::version::v0_0_1::bin::Bin;
use crate::version::v0_0_1::config::config::bind::RouteSelector;
use crate::version::v0_0_1::id::id::{Point, Port, TargetLayer, Topic, ToPort, Uuid};
use crate::version::v0_0_1::payload::payload::{Errors, Payload};
use crate::version::v0_0_1::util::{uuid, ValueMatcher, ValuePattern};
use serde::{Deserialize, Serialize};
use crate::version::v0_0_1::cli::RawCommand;
use crate::version::v0_0_1::command::Command;
use crate::version::v0_0_1::http::HttpMethod;
use crate::version::v0_0_1::log::SpanLogger;
use crate::version::v0_0_1::msg::MsgMethod;
use crate::version::v0_0_1::security::{Permissions, Privilege, Privileges};
use crate::version::v0_0_1::selector::selector::PointSelector;


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

        pub fn transform_input<I2,E>(self) -> Result<RootRequestCtx<I2>, MsgErr>
        where
            I2: TryFrom<I,Error=E>,
            E: Into<MsgErr>
        {
            let input = match I2::try_from(self.input) {
                Ok(input) => input,
                Err(err) => {
                    return Err(err.into())
                }
            };
            Ok(RootRequestCtx {
                logger: self.logger,
                request: self.request,
                input,
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

        pub fn request(&self) -> &Request {
            &self.root.request
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

impl TryFrom<Request> for RawCommand {
    type Error = MsgErr;

    fn try_from(request: Request) -> Result<Self, Self::Error> {
        request.core.body.try_into()
    }
}

impl TryFrom<Response> for Payload{
    type Error = MsgErr;

    fn try_from(response: Response) -> Result<Self, Self::Error> {
        Ok(response.core.body)
    }
}

impl TryInto<Bin> for Response{
    type Error = MsgErr;

    fn try_into(self) -> Result<Bin, Self::Error> {
        match self.core.body {
            Payload::Bin(bin) => Ok(bin),
            _  => Err(MsgErr::err400())
        }
    }
}

impl TryInto<RawCommand> for Payload{
    type Error = MsgErr;

    fn try_into(self) -> Result<RawCommand, Self::Error> {
        match self {
            Payload::RawCommand(command)=> Ok(command),
            payload => Err(format!("expected RawCommand received '{}'", payload.payload_type().to_string() ).into())
        }
    }
}


impl Into<RequestCore> for RawCommand{
    fn into(self) -> RequestCore {
        RequestCore::payload(MsgMethod::new("ExecCommand").unwrap().into(), Payload::RawCommand(self))
    }
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
        pub fn new<P:ToPort>(core: RequestCore, from: P, to: P) -> Self {
            Self {
                id: uuid(),
                agent: Agent::Anonymous,
                scope: Scope::Full,
                handling: Default::default(),
                from: from.to_port(),
                to: to.to_port(),
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
        pub to: Port,
        pub core: RequestCore,
    }

    impl ProtoRequest {
        pub fn into_request<P>(self, from: P, agent: Agent, scope: Scope ) -> Request
        where
            P: ToPort,
        {
            let request = Request {
                id: self.id,
                from: from.to_port(),
                to: self.to,
                core: self.core,
                agent,
                handling: Default::default(),
                scope
            };
            request
        }
    }

    impl ProtoRequest {
        pub fn new<P:ToPort>(to: P, method: Method) -> Self {
            Self {
                id: uuid(),
                to: to.to_port(),
                core: RequestCore::new(method)
            }
        }

        pub fn msg<M: Into<MsgMethod>, P:ToPort>( to: P, method: M ) -> Self {
            let method: MsgMethod = method.into();
            let method: Method = method.into();
            Self::new(to, method)
        }

        pub fn http<M: Into<HttpMethod>, P:ToPort>( to: P, method: M ) -> Self {
            let method: HttpMethod = method.into();
            let method: Method = method.into();
            Self::new(to, method)
        }

        pub fn cmd<M: Into<CmdMethod>,P:ToPort>( to: P, method: M ) -> Self {
            let method: CmdMethod = method.into();
            let method: Method = method.into();
            Self::new(to, method)
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

    impl Response {
        pub fn as_result<E:From<&'static str>,P:TryFrom<Payload>>(self) -> Result<P,E> {
            self.core.as_result()
        }
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


pub trait Router: Send + Sync {
    fn route(&self, message: Message);
}

#[derive(Clone)]
pub struct AsyncMessengerAgent {
    pub agent: Agent,
    pub from: Port,
    pub relay: Arc<dyn AsyncMessenger>
}

impl AsyncMessengerAgent {
    pub fn new(agent: Agent, from: Port, relay: Arc<dyn AsyncMessenger>) -> Self {
        Self {
            agent,
            from,
            relay,
        }
    }
}

impl AsyncMessengerAgent {
    pub async fn send(&self, request: ProtoRequest) -> Response{
        let request = request.into_request(self.from.clone(), self.agent.clone(), Scope::None );
        self.relay.send(request).await
    }
}

impl AsyncMessengerAgent {
    pub fn with_from(self, from: Port ) -> Self {
        let mut rtn = self.clone();
        rtn.from = from;
        rtn
    }

    pub fn with_agent(self, agent: Agent) -> Self {
        let mut rtn = self.clone();
        rtn.agent = agent;
        rtn
    }
}


#[derive(Clone)]
pub struct SyncMessengerRelay {
    pub topic: Option<Topic>,
    pub layer: Option<TargetLayer>,
    pub point: Option<Point>,
    pub relay: Arc<dyn SyncMessenger>
}

impl SyncMessenger for SyncMessengerRelay {
    fn send(&self, request: Request) -> Response{
        let mut request = request;
        if let Some(topic) = &self.topic {
            request.from.topic = topic.clone();
        }
        if let Some(layer) = &self.layer{
            request.from.layer = layer.clone();
        }
        if let Some(point) = &self.point{
            request.from.point = point.clone();
        }
        self.relay.send(request)
    }
}

impl SyncMessengerRelay {
    pub fn with_point( self, point: Point) -> Self {
        Self {
            topic: self.topic,
            layer: self.layer,
            point: Some(point),
            relay: self.relay
        }
    }

    pub fn with_topic( self, topic: Topic) -> Self {
        Self {
            topic: Some(topic),
            layer: self.layer,
            point: self.point,
            relay: self.relay
        }
    }

    pub fn with_layer( self, layer: TargetLayer) -> Self {
        Self {
            topic: self.topic,
            layer: Some(layer),
            point: self.point,
            relay: self.relay
        }
    }

    pub fn with_port( self, port: Port ) -> Self {
        Self {
            topic: Some(port.topic),
            layer: Some(port.layer),
            point: Some(port.point),
            relay: self.relay
        }
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
    fn select(&self, request: &Request) -> Result<(), ()>;
    fn handle(&self, request: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr>;
}

#[async_trait]
pub trait AsyncRequestHandler: Sync+Send  {
    fn select(&self, request: &Request) -> Result<(), ()>;
    async fn handle(&self, request: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr>;
}

impl RequestHandler for RequestHandlerRelay {
    fn select(&self, request: &Request) -> Result<(), ()> {
        self.relay.select(request)
    }

    fn handle(&self, request: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr> {
        self.relay.handle(request)
    }
}

pub struct AsyncRequestHandlerRelay {
    pub relay: Box<dyn AsyncRequestHandler>
}


impl AsyncRequestHandlerRelay {
    pub fn new( handler: Box<dyn AsyncRequestHandler> ) -> Self  {
        Self {
            relay: handler
        }
    }
}

#[async_trait]
impl AsyncRequestHandler for AsyncRequestHandlerRelay {

    fn select(&self, request: &Request) -> Result<(), ()>
    {
        self.relay.select(request)
    }

    async fn handle(&self, ctx: RootRequestCtx<Request>) -> Result<ResponseCore, MsgErr> {
        self.relay.handle(ctx).await
    }
}

pub struct InternalRequestHandlers<H> {
    pipelines: Vec<InternalPipeline<H>>,
}

impl<H> InternalRequestHandlers<H> {
    pub fn new() -> Self {
        Self {
            pipelines: vec![],
        }
    }

    pub fn add(&mut self, selector: RouteSelector, handler: H) {
        let pipeline = InternalPipeline { selector, handler };
        self.pipelines.push(pipeline);
    }

    pub fn remove_topic(&mut self, topic: Option<ValuePattern<Topic>>) {
       self.pipelines.retain(|p|p.selector.topic != topic)
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

impl InternalRequestHandlers<RequestHandlerRelay> {
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

impl InternalRequestHandlers<AsyncRequestHandlerRelay> {
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
   pub relay: Box<dyn RequestHandler>,
}

impl RequestHandlerRelay {
    pub fn new( handler: Box<dyn RequestHandler> ) -> Self {
        Self {
            relay: handler
        }
    }
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

    pub fn forbidden() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(403u16).unwrap(),
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

impl ResponseCore {

    pub fn as_result<E:From<&'static str>,P:TryFrom<Payload>>(self) -> Result<P,E> {
        if self.status.is_success() {
            match P::try_from(self.body) {
                Ok(payload) => Ok(payload),
                Err(err) => Err(E::from("error"))
            }
        } else {
            Err(E::from("error"))
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

#[derive(Debug, Clone, Serialize,Deserialize, Eq,PartialEq)]
pub enum Method {
    Cmd(CmdMethod),
    Http(HttpMethod),
    Msg(MsgMethod),
}

#[derive(Debug, Clone,Eq,PartialEq)]
pub enum MethodPattern {
    Cmd(ValuePattern<CmdMethod>),
    Http(ValuePattern<HttpMethod>),
    Msg(ValuePattern<MsgMethod>),
}

impl ToString for MethodPattern {
    fn to_string(&self) -> String {
        match self {
            MethodPattern::Cmd(c) => {
                format!("Cmd<{}>",c.to_string())
            }
            MethodPattern::Http(c) => {
                format!("Http<{}>",c.to_string())
            }
            MethodPattern::Msg(c) => {
                format!("Msg<{}>",c.to_string())
            }
        }
    }
}

impl ValueMatcher<Method> for MethodPattern {
    fn is_match(&self, x: &Method) -> Result<(), ()> {
        match self {
            MethodPattern::Cmd(pattern) => {
                if let Method::Cmd(v) = x {
                    pattern.is_match(v)
                } else {
                    Err(())
                }
            }
            MethodPattern::Http(pattern) => {
                if let Method::Http(v) = x {
                    pattern.is_match(v)
                }else {
                    Err(())
                }
            }
            MethodPattern::Msg(pattern) => {
                if let Method::Msg(v) = x {
                    pattern.is_match(v)
                }else {
                    Err(())
                }
            }
        }
    }
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

#[derive(Debug, Clone, Serialize,Deserialize,Eq,PartialEq)]
pub struct RequestCore {
    #[serde(with = "http_serde::header_map")]
    pub headers: HeaderMap,
    pub method: Method,
    #[serde(with = "http_serde::uri")]
    pub uri: Uri,
    pub body: Payload,
}

impl RequestCore {

    pub fn new( method: Method ) -> Self {
        Self {
            method,
            headers: HeaderMap::new(),
            uri: Default::default(),
            body: Default::default()
        }
    }

    pub fn msg<M: Into<MsgMethod>>( method: M ) -> Self {
        let method: MsgMethod = method.into();
        let method: Method = method.into();
        Self::new(method)
    }

    pub fn http<M: Into<HttpMethod>>( method: M ) -> Self {
        let method: HttpMethod = method.into();
        let method: Method = method.into();
        Self::new(method)
    }

    pub fn cmd<M: Into<CmdMethod>>( method: M ) -> Self {
        let method: CmdMethod = method.into();
        let method: Method = method.into();
        Self::new(method)
    }
}

impl TryFrom<Request> for RequestCore {
    type Error = MsgErr;

    fn try_from(request: Request) -> Result<Self, Self::Error> {
        Ok(request.core)
    }
}

impl RequestCore {
    pub fn kind(&self) -> MethodKind {
        self.method.kind()
    }
}

impl Into<RequestCore> for Command {
    fn into(self) -> RequestCore {
        RequestCore {
            body: Payload::Command(Box::new(self)),
            method: Method::Msg(MsgMethod::new("Command").unwrap()),
            ..Default::default()
        }
    }
}

impl TryFrom<http::Request<Bin>> for RequestCore {

    type Error = MsgErr;

    fn try_from(request: http::Request<Bin>) -> Result<Self, Self::Error> {
        Ok(Self {
            headers: request.headers().clone(),
            method: Method::Http(request.method().clone().try_into()?),
            uri: request.uri().clone(),
            body: Payload::Bin(request.body().clone()),
        })
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



    pub fn payload(method: Method, body: Payload) -> RequestCore{
        RequestCore{
            method,
            body,
            ..Default::default()
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

impl Into<ResponseCore> for Port {
    fn into(self) -> ResponseCore {
         ResponseCore::ok(Payload::Port(self))
    }
}

impl TryFrom<ResponseCore> for Port {
    type Error = MsgErr;

    fn try_from(core: ResponseCore) -> Result<Self, Self::Error> {
        if !core.status.is_success()  {
            Err(MsgErr::new(core.status.as_u16(), "error"))
        } else {
            match core.body {
                Payload::Port(port) => {
                    Ok(port)
                }
                payload => {
                    Err(format!("expecting Port received {}", payload.payload_type().to_string()).into() )
                }
            }
        }
    }
}

#[derive(Debug, Clone,Serialize,Deserialize,Eq,PartialEq,Hash,strum_macros::Display,strum_macros::EnumString)]
pub enum CmdMethod {
    Read,
    Update
}

impl Into<Method> for CmdMethod {
    fn into(self) -> Method {
        Method::Cmd(self)
    }
}


impl ValueMatcher<CmdMethod> for CmdMethod {
    fn is_match(&self, x: &CmdMethod) -> Result<(), ()> {
        if *x == *self {
            Ok(())
        } else {
            Err(())
        }
    }
}
