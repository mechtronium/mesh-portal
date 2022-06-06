use crate::error::{MsgErr, StatusErr};
use crate::version::v0_0_1::bin::Bin;
use crate::version::v0_0_1::cli::RawCommand;
use crate::version::v0_0_1::command::Command;
use crate::version::v0_0_1::config::config::bind::RouteSelector;
use crate::version::v0_0_1::http::HttpMethod;
use crate::version::v0_0_1::id::id::{Point, Port, TargetLayer, ToPort, Topic, Uuid, ToPoint};
use crate::version::v0_0_1::log::{LogSpan, LogSpanEvent, SpanLogger};
use crate::version::v0_0_1::msg::MsgMethod;
use crate::version::v0_0_1::particle::particle::Details;
use crate::version::v0_0_1::payload::payload::{Errors, MultipartFormBuilder, Payload, Token, ToRequestCore};
use crate::version::v0_0_1::security::{Permissions, Privilege, Privileges};
use crate::version::v0_0_1::selector::selector::PointSelector;
use crate::version::v0_0_1::sys::AssignmentKind;
use crate::version::v0_0_1::util::{uuid, ValueMatcher, ValuePattern};
use cosmic_macros_primitive::Autobox;
use cosmic_nom::{Res, SpanExtra};
use http::{HeaderMap,  StatusCode, Uri};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::convert::Infallible;
use std::ops;
use std::ops::Deref;
use std::sync::Arc;
use ariadne::Color::Default;
use dashmap::DashMap;
use tokio::sync::RwLock;

#[derive(Serialize, Deserialize, Autobox)]
pub enum WaveFrame {
    Request(RequestFrame),
    Response(ResponseFrame),
}

impl WaveFrame {
    pub fn id(&self) -> &Uuid {
        match self {
            WaveFrame::Request(request) => request.id(),
            WaveFrame::Response(response) => response.id()
        }
    }

    pub fn to(&self) -> &Port{
        match self {
            WaveFrame::Request(request) => request.to(),
            WaveFrame::Response(response) => response.to()
        }
    }

    pub fn from(&self) -> &Port{
        match self {
            WaveFrame::Request(request) => request.from(),
            WaveFrame::Response(response) => response.from()
        }
    }
}

pub struct RootInputCtx<I> {
    pub input: I,
    pub request: Request,
    session: Option<Session>,
    logger: SpanLogger,
}

impl <I> Deref for RootInputCtx<I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.input
    }
}

impl RootInputCtx<Request> {
    pub fn new(request: Request, logger: SpanLogger) -> Self {
        Self {
            request: request.clone(),
            input: request.clone(),
            logger,
            session: None,
        }
    }
}

impl<I> RootInputCtx<I> {
    pub fn transform_input<I2, E>(self) -> Result<RootInputCtx<I2>, MsgErr>
    where
        I2: TryFrom<I, Error = E>,
        E: Into<MsgErr>,
    {
        let input = match I2::try_from(self.input) {
            Ok(input) => input,
            Err(err) => return Err(err.into()),
        };
        Ok(RootInputCtx {
            logger: self.logger,
            request: self.request,
            input,
            session: self.session,
        })
    }

    pub fn push<'a>(&'a self, messenger: &'a AsyncMessengerAgent) -> InputCtx<'a, I> {
        InputCtx::new(self, &self.input, messenger, self.logger.clone())
    }
}

pub struct InputCtx<'a, I> {
    root: &'a RootInputCtx<I>,
    messenger: &'a AsyncMessengerAgent,
    parent: Option<Box<InputCtx<'a, I>>>,
    pub input: &'a I,
    pub logger: SpanLogger,
}

impl<'a, I> Deref for InputCtx<'a, I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        self.input
    }
}

impl<'a, I> InputCtx<'a, I> {
    pub fn new(root: &'a RootInputCtx<I>, input: &'a I, messenger: &'a AsyncMessengerAgent, logger: SpanLogger) -> Self {
        Self {
            root,
            parent: None,
            input,
            logger,
            messenger
        }
    }

    pub fn push(self) -> InputCtx<'a, I> {
        InputCtx {
            root: self.root,
            input: self.input,
            logger: self.logger.span(),
            parent: Some(Box::new(self)),
            messenger: & self.messenger
        }
    }

    pub fn pop(self) -> Option<InputCtx<'a, I>> {
        match self.parent {
            None => None,
            Some(parent) => Some(*parent),
        }
    }

    pub fn request(&self) -> &Request {
        &self.root.request
    }
}

impl<'a> InputCtx<'a, &mut Request> {
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

impl<'a> InputCtx<'a, Response> {
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

pub trait Requestable<R> {
    fn forbidden(self) -> R {
        self.status(403)
    }

    fn bad_request(self) -> R {
        self.status(400)
    }

    fn not_found(self) -> R {
        self.status(404)
    }

    fn timeout(self) -> R {
        self.status(408)
    }

    fn server_error(self) -> R {
        self.status(500)
    }

    fn status(self, status: u16) -> R;

    fn err(self, err: MsgErr) -> R;

    fn ok(self) -> R;

    fn body(self, body: Payload) -> R;

    fn core(self, core: ResponseCore) -> R;

    fn result<C:Into<ResponseCore>>(self, result: Result<C,MsgErr>) -> R {
        match result {
            Ok(core) => self.core(core.into()),
            Err(err) => self.core(err.into())
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequestStub {
    pub id: String,
    pub agent: Agent,
    pub handling: Handling,
    pub from: Port,
    pub to: Port,
    pub span: Option<LogSpan>
}

impl Requestable<Response> for RequestStub {
    fn status(self, status: u16) -> Response {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::status(status),
            response_to: self.id,
        }
    }

    fn err(self, err: MsgErr) -> Response {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::err(err),
            response_to: self.id,
        }
    }

    fn ok(self) -> Response {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::ok(Payload::Empty),
            response_to: self.id,
        }
    }

    fn body(self, body: Payload) -> Response {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::ok(body),
            response_to: self.id,
        }
    }

    fn core(self, core: ResponseCore) -> Response {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core,
            response_to: self.id,
        }
    }
}

impl Requestable<ResponseFrame> for RequestStub {
    fn status(self, status: u16) -> ResponseFrame {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::status(status),
            response_to: self.id,
        }
        .to_frame(self.span)
    }

    fn err(self, err: MsgErr) -> ResponseFrame {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::err(err),
            response_to: self.id,
        }
        .to_frame(self.span)
    }

    fn ok(self) -> ResponseFrame {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::ok(Payload::Empty),
            response_to: self.id,
        }
        .to_frame(self.span)
    }

    fn body(self, body: Payload) -> ResponseFrame {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::ok(body),
            response_to: self.id,
        }
        .to_frame(self.span)
    }

    fn core(self, core: ResponseCore) -> ResponseFrame {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core,
            response_to: self.id,
        }
            .to_frame(self.span)
    }
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

impl Into<ProtoRequest> for Request {
    fn into(self) -> ProtoRequest {
        ProtoRequest {
            id: self.id,
            to: self.to,
            core: self.core,
            handling: self.handling,
            scope: self.scope,
        }
    }
}

impl Into<&WaitTime> for &Request {
    fn into(self) -> &WaitTime {
        &self.handling.wait
    }
}

impl Requestable<Response> for Request {
    fn status(self, status: u16) -> Response {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::status(status),
            response_to: self.id,
        }
    }

    fn err(self, err: MsgErr) -> Response {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::err(err),
            response_to: self.id,
        }
    }

    fn ok(self) -> Response {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::ok(Payload::Empty),
            response_to: self.id,
        }
    }

    fn body(self, body: Payload) -> Response {
        Response {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: ResponseCore::ok(body),
            response_to: self.id,
        }
    }
}

impl Request {
    pub fn as_stub(&self) -> RequestStub {
        RequestStub {
            id: self.id.clone(),
            agent: self.agent.clone(),
            handling: self.handling.clone(),
            from: self.from.clone(),
            to: self.to.clone(),
        }
    }

    pub fn require_method<M: Into<Method>>(self, method: M) -> Result<Request, Response> {
        if self.core.method == method.into() {
            Ok(self)
        } else {
            Err(self.err(MsgErr::new(
                400,
                format!("Bad Request: expecting method: {}", method.to_string()).as_str(),
            )))
        }
    }

    pub fn require_body<B>(self, body_kind: &'static str) -> Result<B, Response>
    where
        B: TryFrom<Payload, Error = MsgErr>,
    {
        match B::try_from(self.core.body) {
            Ok(body) => Ok(B),
            Err(err) => Err(self.err(MsgErr::new(
                400,
                format!("Bad Request: expecting body payload kind: {}", body_kind).as_str(),
            ))),
        }
    }

    pub fn server_error(&self) -> Response {
        self.as_stub().server_error()
    }

    pub fn timeout(&self) -> Response {
        self.as_stub().timeout()
    }

    pub fn not_found(&self) -> Response {
        self.as_stub().not_found()
    }

    pub fn forbidden(&self) -> Response {
        self.as_stub().forbidden()
    }

    pub fn bad_request(&self) -> Response {
        self.as_stub().bad_request()
    }

    pub fn status(&self, status: u16) -> Response {
        self.as_stub().status(status)
    }

    pub fn to_frame(self, span: Option<LogSpan>) -> RequestFrame {
        RequestFrame {
            session: None,
            request: self,
            span
        }
    }

}

#[derive(Serialize, Deserialize)]
pub struct RequestFrame {
    pub session: Option<Session>,
    pub request: Request,
    pub span: Option<LogSpan>,
}

impl RequestFrame {
    pub fn from(&self) -> &Port {
        &request.from
    }

    pub fn to(&self) -> &Port {
        &request.to
    }
}

impl RequestFrame {
    pub fn id(&self) -> &Uuid {
        &self.request.id
    }

    pub fn as_stub(&self) -> RequestStub {
        let mut stub = self.request.as_stub();
        stub.span = self.span.clone();
        stub
    }
}

impl Into<&WaitTime> for &RequestFrame {
    fn into(self) -> &WaitTime {
        (&self.request).into()
    }
}

impl Requestable<ResponseFrame> for RequestFrame {
    fn status(self, status: u16) -> ResponseFrame {
        ResponseFrame {
            session: None,
            response: self.request.status(status),
            span: self.span,
        }
    }

    fn err(self, err: MsgErr) -> ResponseFrame {
        ResponseFrame {
            session: None,
            response: self.request.err(err),
            span: self.span,
        }
    }

    fn ok(self) -> ResponseFrame {
        ResponseFrame {
            session: None,
            response: self.request.ok(),
            span: self.span,
        }
    }

    fn body(self, body: Payload) -> ResponseFrame {
        ResponseFrame {
            session: None,
            response: self.request.body(body),
            span: self.span,
        }
    }

    fn core(self, core: ResponseCore) -> ResponseFrame {
        let response = Response::new(core, self.to, self.from, self.id);
        ResponseFrame {
            session: None,
            response,
            span: self.span
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ResponseFrame {
    session: Option<Session>,
    response: Response,
    span: Option<LogSpan>,
}

impl ResponseFrame {
    pub fn id(&self) -> &Uuid {
        &self.response.id
    }
    pub fn from(&self) -> &Port {
        &self.response.from
    }
    pub fn to(&self) -> &Port {
        &self.response.to
    }
    pub fn response_to(&self) -> &Uuid { &self.resonse.response_to }
}

impl TryFrom<Request> for RawCommand {
    type Error = MsgErr;

    fn try_from(request: Request) -> Result<Self, Self::Error> {
        request.core.body.try_into()
    }
}

impl TryFrom<Response> for Payload {
    type Error = MsgErr;

    fn try_from(response: Response) -> Result<Self, Self::Error> {
        Ok(response.core.body)
    }
}

impl TryInto<Bin> for Response {
    type Error = MsgErr;

    fn try_into(self) -> Result<Bin, Self::Error> {
        match self.core.body {
            Payload::Bin(bin) => Ok(bin),
            _ => Err(MsgErr::err400()),
        }
    }
}

impl Into<RequestCore> for RawCommand {
    fn into(self) -> RequestCore {
        RequestCore::payload(
            MsgMethod::new("ExecCommand").unwrap().into(),
            Payload::RawCommand(self),
        )
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
    pub fn new<P: ToPort>(core: RequestCore, from: P, to: P) -> Self {
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
    pub handling: Handling,
    pub scope: Scope,
}

impl ProtoRequest {
    pub fn to_request<P>(self, from: P, agent: Agent, scope: Scope) -> Request
    where
        P: ToPort,
    {
        let scope = self.scope | scope;
        let request = Request {
            id: self.id,
            from: from.to_port(),
            to: self.to,
            core: self.core,
            agent,
            handling: self.handling,
            scope,
        };
        request
    }
}

impl ProtoRequest {
    pub fn new<P: ToPort>(to: P, method: Method) -> Self {
        Self {
            id: uuid(),
            to: to.to_port(),
            core: RequestCore::new(method),
            handling: Default::default(),
            scope: Scope::Full
        }
    }

    pub fn sys<M: Into<SysMethod>, P: ToPort>(to: P, method: M) -> Self {
        let method: SysMethod = method.into();
        let method: Method = method.into();
        Self::new(to, method)
    }

    pub fn msg<M: Into<MsgMethod>, P: ToPort>(to: P, method: M) -> Self {
        let method: MsgMethod = method.into();
        let method: Method = method.into();
        Self::new(to, method)
    }

    pub fn http<M: Into<HttpMethod>, P: ToPort>(to: P, method: M) -> Self {
        let method: HttpMethod = method.into();
        let method: Method = method.into();
        Self::new(to, method)
    }

    pub fn cmd<M: Into<CmdMethod>, P: ToPort>(to: P, method: M) -> Self {
        let method: CmdMethod = method.into();
        let method: Method = method.into();
        Self::new(to, method)
    }

    pub fn core<P: ToPort>(to: P, core: RequestCore ) -> Self {
        Self {
            id: uuid(),
            to: to.to_port(),
            core,
            scope: Default::default(),
            handling: Default::default()
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response {
    pub id: Uuid,
    pub from: Port,
    pub to: Port,
    pub core: ResponseCore,
    pub response_to: Uuid,
}

impl Response {
    pub fn to_frame(self, span: Option<LogSpan>) -> ResponseFrame {
        ResponseFrame {
            session: None,
            response: self,
            span,
        }
    }

    pub fn to_span_frame(self, span: LogSpanEvent) -> RequestFrame {
        WaveFrame {
            session: None,
            request: self,
            span: Some(span),
        }
    }

    pub fn as_result<E: From<&'static str>, P: TryFrom<Payload>>(self) -> Result<P, E> {
        self.core.as_result()
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

#[derive(Debug, Clone, Serialize, Deserialize, Autobox)]
pub enum Wave {
    Request(Request),
    Response(Response),
}

impl Wave {

    pub fn id(&self) -> Uuid {
        match self {
            Wave::Request(request) => request.id.clone(),
            Wave::Response(response) => response.id.clone(),
        }
    }

    pub fn payload(&self) -> Payload {
        match self {
            Wave::Request(request) => request.core.body.clone(),
            Wave::Response(response) => response.core.body.clone(),
        }
    }

    pub fn to(&self) -> &Port {
        match self {
            Wave::Request(request) => &request.to,
            Wave::Response(response) => &response.to,
        }
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

impl ops::BitAnd<Scope> for Scope{
    type Output = Scope;

    fn bitand(self, rhs: Scope) -> Self::Output {
        if self == Self::Full && rhs == Self::Full {
            Self::Full
        } else if self == Self::None || rhs == Self::None {
            Self::None
        } else {
            // merge scope grants
            unimplemented!()
        }
    }
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
    pub kind: HandlingKind,
    pub priority: Priority,
    pub retries: Retries,
    pub wait: WaitTime,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HandlingKind {
    Durable,   // Mesh will guarantee delivery eventually once Request call has returned
    Queued,    // Slower but more reliable delivery, message can be lost if a star crashes, etc
    Immediate, // Wave should never touch a filesystem, it will be in memory for its entire journey for immediate processing
}

impl Default for Handling {
    fn default() -> Self {
        Self {
            kind: HandlingKind::Queued,
            priority: Default::default(),
            retries: Default::default(),
            wait: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WaitTime {
    High,
    Med,
    Low,
}

impl Default for WaitTime {
    fn default() -> Self {
        WaitTime::Low
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

#[async_trait]
pub trait AsyncRouter: Send + Sync {
    async fn route(&self, wave: Wave);
}

#[derive(Clone)]
pub struct AsyncPointRequestHandlers{
    pub handlers: Arc<DashMap<Point,AsyncRequestHandlerRelay>>
}

#[async_trait]
impl AsyncRequestHandler for  AsyncPointRequestHandlers {

    fn select(&self, request: &Request) -> Result<(), ()> {
        if let Some(handler) = self.handlers.get( &request.to.to_point() ) {
            handler.select(request)
        } else {
            Err(())
        }
    }

    async fn handle(&self, request: RootInputCtx<Request>) -> Result<ResponseCore, MsgErr> {
        if let Some(handler) = self.handlers.get( &request.to ) {
            handler.handle(request).await
        } else {
            Err(MsgErr::not_found())
        }
    }
}



#[derive(Clone)]
pub struct AsyncMessengerAgent {
    pub agent: Agent,
    pub from: Port,
    pub relay: Arc<dyn AsyncMessenger<Request, Response>>,
}

impl AsyncMessengerAgent {
    pub fn new(
        agent: Agent,
        from: Port,
        relay: Arc<dyn AsyncMessenger<Request, Response>>,
    ) -> Self {
        Self { agent, from, relay }
    }
}

impl AsyncMessengerAgent {
    pub async fn send<R>(&self, request: ProtoRequest) -> Result<R,MsgErr> where R: TryFrom<Response,Error=MsgErr>{
        let request = request.to_request(self.from.clone(), self.agent.clone(), Scope::None);
        R::try_from(self.relay.send(request).await)
    }

    pub fn send_sync<R>(&self, request: ProtoRequest) ->  Result<R,MsgErr> where R: TryFrom<Response,Error=MsgErr>{
        let request = request.to_request(self.from.clone(), self.agent.clone(), Scope::None);
        R::try_from(self.relay.send_sync(request))
    }

    pub async fn route(&self, wave: Wave )  {
        self.relay.route(wave).await;
    }
}

impl AsyncMessengerAgent {
    pub fn with_from(self, from: Port) -> Self {
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
    pub relay: Arc<dyn SyncMessenger>,
}

impl SyncMessenger for SyncMessengerRelay {
    fn send(&self, request: Request) -> Response {
        let mut request = request;
        if let Some(topic) = &self.topic {
            request.from.topic = topic.clone();
        }
        if let Some(layer) = &self.layer {
            request.from.layer = layer.clone();
        }
        if let Some(point) = &self.point {
            request.from.point = point.clone();
        }
        self.relay.send(request)
    }
}

impl SyncMessengerRelay {
    pub fn with_point(self, point: Point) -> Self {
        Self {
            topic: self.topic,
            layer: self.layer,
            point: Some(point),
            relay: self.relay,
        }
    }

    pub fn with_topic(self, topic: Topic) -> Self {
        Self {
            topic: Some(topic),
            layer: self.layer,
            point: self.point,
            relay: self.relay,
        }
    }

    pub fn with_layer(self, layer: TargetLayer) -> Self {
        Self {
            topic: self.topic,
            layer: Some(layer),
            point: self.point,
            relay: self.relay,
        }
    }

    pub fn with_port(self, port: Port) -> Self {
        Self {
            topic: Some(port.topic),
            layer: Some(port.layer),
            point: Some(port.point),
            relay: self.relay,
        }
    }
}

#[async_trait]
pub trait AsyncMessenger<Request, Response>: Send + Sync
where
    Request: Requestable<Response>,
{
    async fn send(&self, request: Request) -> Response;
    fn send_sync(&self, request: Request) -> Response;
    async fn route(&self, wave: Wave);
}

pub trait SyncMessenger: Send + Sync {
    fn send(&self, request: Request) -> Response;
}

pub struct InternalPipeline<H> {
    pub selector: RouteSelector,
    pub handler: H,
}

impl<H> InternalPipeline<H> {
    pub fn new(selector: RouteSelector, mut handler: H) -> Self {
        Self { selector, handler }
    }
}

pub trait RequestHandler {
    fn select(&self, request: &Request) -> Result<(), ()>;
    fn handle(&self, request: RootInputCtx<Request>) -> Result<ResponseCore, MsgErr>;
}

#[async_trait]
pub trait AsyncRequestHandler: Sync + Send {
    fn select(&self, request: &Request) -> Result<(), ()>;
    async fn handle(&self, request: RootInputCtx<Request>) -> Result<ResponseCore, MsgErr>;
}

impl RequestHandler for RequestHandlerRelay {
    fn select(&self, request: &Request) -> Result<(), ()> {
        self.relay.select(request)
    }

    fn handle(&self, request: RootInputCtx<Request>) -> Result<ResponseCore, MsgErr> {
        self.relay.handle(request)
    }
}

pub trait AsyncAuthorizationRequester {
    async fn authorize( self, messenger: AsyncMessengerAgent ) -> Result<(),()>;
}

pub struct Credentials {
    pub username: String,
    pub password: String
}



#[derive(Clone,Serialize,Deserialize)]
pub struct Authorization {
    pub agent: Point
}

pub enum AuthResponse {
    Next(Box<dyn AsyncAuthorizationRequester>),
    Authorized(Authorization)
}


pub struct AsyncCredsAuthorizationRequester {
    pub auth_point: Point,
    pub credentials: Credentials
}

impl AsyncAuthorizationRequester for AsyncCredsAuthorizationRequester {
    async fn authorize(self, messenger: AsyncMessengerAgent) -> Result<AuthResponse, MsgErr> {
        let mut form = MultipartFormBuilder::new();
        form.put( "username", self.credentials.username.as_str() );
        form.put( "password", self.credentials.password.as_str() );
        let form = form.build()?;
        let request = form.to_request_core();
        let mut request = ProtoRequest::core(self.auth_point.clone(), request );
        request.handling.wait = WaitTime::High;
        let refresh_token: Token = messenger.send(request).await?;
        let next = AsyncRefreshTokenAuthorizationRequester{
            auth_point: self.auth_point,
            token: refresh_token
        };
        Ok(AuthResponse::Next(Box::new(next)))
    }
}

pub struct AsyncRefreshTokenAuthorizationRequester {
    pub auth_point: Point,
    pub token: Token
}

impl AsyncAuthorizationRequester for AsyncRefreshTokenAuthorizationRequester {
    async fn authorize(self, messenger: AsyncMessengerAgent) -> Result<AuthResponse, MsgErr> {
        unimplemented!()
    }
}


pub struct AsyncRequestHandlerRelay {
    pub relay: Box<dyn AsyncRequestHandler>,
}

impl AsyncRequestHandlerRelay {
    pub fn new(handler: Box<dyn AsyncRequestHandler>) -> Self {
        Self { relay: handler }
    }
}

#[async_trait]
impl AsyncRequestHandler for AsyncRequestHandlerRelay {
    fn select(&self, request: &Request) -> Result<(), ()> {
        self.relay.select(request)
    }

    async fn handle(&self, ctx: RootInputCtx<Request>) -> Result<ResponseCore, MsgErr> {
        self.relay.handle(ctx).await
    }
}

#[derive(Clone)]
pub struct AsyncInternalRequestHandlers<H> {
    pipelines: Arc<RwLock<Vec<InternalPipeline<H>>>>,
}

impl<H> AsyncInternalRequestHandlers<H> {
    pub fn new() -> Self {
        Self {
            pipelines: Arc::new(RwLock::new(vec![])),
        }
    }

    pub async fn add(&self, selector: RouteSelector, handler: H) {
        let mut write = self.pipelines.write().await;
        let pipeline = InternalPipeline { selector, handler };
        write.push(pipeline);
    }

    pub async fn remove_topic(&mut self, topic: Option<ValuePattern<Topic>>) {
        let mut write = self.pipelines.write().await;
        write.retain(|p| p.selector.topic != topic)
    }
}

impl AsyncRequestHandler for AsyncInternalRequestHandlers<AsyncRequestHandlerRelay> {
    fn select(&self, request: &Request) -> Result<(), ()> {
        let read = self.pipelines.read().await;
        for pipeline in read.iter() {
            if pipeline.selector.is_match(&request).is_ok() {
                return pipeline.handler.select(request)
            }
        }
        Err(())
    }

    async fn handle(&self, ctx: RootInputCtx<Request>) -> Result<ResponseCore, MsgErr> {
        let read = self.pipelines.read().await;
        for pipeline in read.iter() {
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
    pub fn new(handler: Box<dyn RequestHandler>) -> Self {
        Self { relay: handler }
    }
}

#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    strum_macros::Display,
    strum_macros::EnumString,
    Eq,
    PartialEq,
)]
pub enum MethodKind {
    Sys,
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

impl From<Result<ResponseCore,MsgErr>> for ResponseCore {
    fn from(result: Result<ResponseCore, MsgErr>) -> Self {
        match result {
            Ok(response) => response,
            Err(err) => {
                err.into()
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
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

    pub fn timeout() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(408u16).unwrap(),
            body: Payload::Empty,
        }
    }

    pub fn server_error() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(500u16).unwrap(),
            body: Payload::Empty,
        }
    }

    pub fn status(status: u16) -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(status).unwrap_or(StatusCode::from_u16(500).unwrap()),
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

    pub fn bad_request() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(400u16).unwrap(),
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
        let errors = Errors::default(err.to_string().as_str());
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(err.status())
                .unwrap_or(StatusCode::from_u16(500u16).unwrap()),
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

    pub fn into_response<P>(self, from: P, to: P, response_to: String) -> Response
    where
        P: ToPort,
    {
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
    pub fn as_result<E: From<&'static str>, P: TryFrom<Payload>>(self) -> Result<P, E> {
        if self.status.is_success() {
            match P::try_from(self.body) {
                Ok(payload) => Ok(payload),
                Err(err) => Err(E::from("error")),
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
                    builder = builder.header(name.as_str(), value.to_str()?.to_string().as_str());
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
                    builder = builder.header(name.as_str(), value.to_str()?.to_string().as_str());
                }
                None => {}
            }
        }

        let response = builder.status(self.status).body(self.body.to_bin()?)?;
        Ok(response)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq,Autobox)]
pub enum Method {
    Sys(SysMethod),
    Cmd(CmdMethod),
    Http(HttpMethod),
    Msg(MsgMethod),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MethodPattern {
    Sys(ValuePattern<SysMethod>),
    Cmd(ValuePattern<CmdMethod>),
    Http(ValuePattern<HttpMethod>),
    Msg(ValuePattern<MsgMethod>),
}

impl ToString for MethodPattern {
    fn to_string(&self) -> String {
        match self {
            MethodPattern::Cmd(c) => {
                format!("Cmd<{}>", c.to_string())
            }
            MethodPattern::Http(c) => {
                format!("Http<{}>", c.to_string())
            }
            MethodPattern::Msg(c) => {
                format!("Msg<{}>", c.to_string())
            }
            MethodPattern::Sys(c) => {
                format!("Sys<{}>", c.to_string())
            }
        }
    }
}

impl ValueMatcher<Method> for MethodPattern {
    fn is_match(&self, x: &Method) -> Result<(), ()> {
        match self {
            MethodPattern::Sys(pattern) => {
                if let Method::Sys(v) = x {
                    pattern.is_match(v)
                } else {
                    Err(())
                }
            }
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
                } else {
                    Err(())
                }
            }
            MethodPattern::Msg(pattern) => {
                if let Method::Msg(v) = x {
                    pattern.is_match(v)
                } else {
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
            Method::Msg(_) => MethodKind::Msg,
            Method::Sys(_) => MethodKind::Sys,
        }
    }
}

impl ToString for Method {
    fn to_string(&self) -> String {
        match self {
            Method::Cmd(cmd) => format!("Cmd<{}>", cmd.to_string()),
            Method::Http(method) => format!("Http<{}>", method.to_string()),
            Method::Msg(msg) => format!("Msg<{}>", msg.to_string()),
            Method::Sys(sys) => format!("Sys<{}>", sys.to_string()),
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

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct RequestCore {
    #[serde(with = "http_serde::header_map")]
    pub headers: HeaderMap,
    pub method: Method,
    #[serde(with = "http_serde::uri")]
    pub uri: Uri,
    pub body: Payload,
}

impl RequestCore {
    pub fn new(method: Method) -> Self {
        Self {
            method,
            headers: HeaderMap::new(),
            uri: Default::default(),
            body: Default::default(),
        }
    }

    pub fn msg<M: Into<MsgMethod>>(method: M) -> Self {
        let method: MsgMethod = method.into();
        let method: Method = method.into();
        Self::new(method)
    }

    pub fn http<M: Into<HttpMethod>>(method: M) -> Self {
        let method: HttpMethod = method.into();
        let method: Method = method.into();
        Self::new(method)
    }

    pub fn cmd<M: Into<CmdMethod>>(method: M) -> Self {
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
                    builder = builder.header(name.as_str(), value.to_str()?.to_string().as_str());
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
    pub fn with_body(self, body: Payload) -> Self {
        Self {
            headers: self.headers,
            uri: self.uri,
            method: self.method,
            body,
        }
    }

    pub fn not_found(&self) -> ResponseCore {
        ResponseCore {
            headers: Default::default(),
            status: StatusCode::from_u16(404u16).unwrap(),
            body: Payload::Empty,
        }
    }

    pub fn payload(method: Method, body: Payload) -> RequestCore {
        RequestCore {
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
        if !core.status.is_success() {
            Err(MsgErr::new(core.status.as_u16(), "error"))
        } else {
            match core.body {
                Payload::Port(port) => Ok(port),
                payload => {
                    Err(format!("expecting Port received {}", payload.kind().to_string()).into())
                }
            }
        }
    }
}

#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Eq,
    PartialEq,
    Hash,
    strum_macros::Display,
    strum_macros::EnumString,
)]
pub enum CmdMethod {
    Read,
    Update,
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

#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Eq,
    PartialEq,
    Hash,
    strum_macros::Display,
    strum_macros::EnumString,
)]
pub enum SysMethod {
    Command,
    Assign,
    Authenticate,
    AssignPort,
}

impl Into<Method> for SysMethod {
    fn into(self) -> Method {
        Method::Sys(self)
    }
}

impl ValueMatcher<SysMethod> for SysMethod {
    fn is_match(&self, x: &SysMethod) -> Result<(), ()> {
        if *x == *self {
            Ok(())
        } else {
            Err(())
        }
    }
}
