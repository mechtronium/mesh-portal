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
    use crate::version::v0_0_1::entity::response::ResponseCore;
    use crate::version::v0_0_1::id::id::{Point, Port, ToPort, Uuid};
    use crate::version::v0_0_1::log::{PointLogger, SpanLogger};
    use crate::version::v0_0_1::payload::payload::{Errors, MsgCall, Payload};
    use crate::version::v0_0_1::security::{
        Access, AccessGrant, EnumeratedAccess, EnumeratedPrivileges, Permissions, Privilege,
        Privileges,
    };
    use crate::version::v0_0_1::selector::selector::{PointKindHierarchy, PointSelector};
    use crate::version::v0_0_1::service::{AsyncMessenger, AsyncMessengerProxy, Messenger, MessengerProxy, Router};
    use crate::version::v0_0_1::util::uuid;

    pub struct RootMessageCtx<I, E> {
        pub input: I,
        pub request: Request,
        session: Option<Session>,
        logger: SpanLogger,
        pub messenger: E,
    }

    impl<E> RootMessageCtx<Request, E> {
        pub fn new(request: Request, messenger: E, logger: SpanLogger) -> Self {
            Self {
                request: request.clone(),
                input: request.clone(),
                logger,
                messenger,
                session: None,
            }
        }
    }

    impl<I, E> RootMessageCtx<I, E> {
        pub fn transform_input<I2>(self) -> Result<RootMessageCtx<I2, E>, MsgErr>
        where
            I2: TryFrom<I, Error = MsgErr>,
        {
            Ok(RootMessageCtx {
                logger: self.logger,
                request: self.request,
                input: I2::try_from(self.input)?,
                messenger: self.messenger,
                session: self.session,
            })
        }

        pub fn push<'a>(&'a self) -> MessageCtx<'a, I, E> {
            MessageCtx::new(self, &self.input, self.logger.clone())
        }
    }

    pub struct MessageCtx<'a, I, E> {
        root: &'a RootMessageCtx<I, E>,
        parent: Option<Box<MessageCtx<'a, I, E>>>,
        pub input: &'a I,
        pub logger: SpanLogger,
    }

    impl<'a, I, E> Deref for MessageCtx<'a, I, E> {
        type Target = I;

        fn deref(&self) -> &Self::Target {
            self.input
        }
    }

    impl<'a, I, E> MessageCtx<'a, I, E> {
        pub fn new(root: &'a RootMessageCtx<I, E>, input: &'a I, logger: SpanLogger) -> Self {
            Self {
                root,
                parent: None,
                input,
                logger,
            }
        }

        pub fn push(self) -> MessageCtx<'a, I, E> {
            MessageCtx {
                root: self.root,
                input: self.input,
                logger: self.logger.span(),
                parent: Some(Box::new(self)),
            }
        }

        pub fn pop(self) -> Option<MessageCtx<'a, I, E>> {
            match self.parent {
                None => None,
                Some(parent) => Some(*parent)
            }
        }

    }

    impl<'a, I> MessageCtx<'a, I, MessengerProxy>
    {
        pub fn send(&self, request: Request ) -> Response {
            self.root.messenger.send(request)
        }
    }

    impl<'a, I> MessageCtx<'a, I,AsyncMessengerProxy>
    {
        pub async fn send(&self, request: Request ) -> Response {
            self.root.messenger.send(request).await
        }
    }

    impl<'a, E> MessageCtx<'a, &mut Request, E> {
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

    impl<'a, E> MessageCtx<'a, Response, E> {
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
