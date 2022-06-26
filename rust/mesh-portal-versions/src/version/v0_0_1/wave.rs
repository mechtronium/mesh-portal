use crate::error::{MsgErr, StatusErr};
use crate::version::v0_0_1::bin::Bin;
use crate::version::v0_0_1::cli::RawCommand;
use crate::version::v0_0_1::command::Command;
use crate::version::v0_0_1::config::config::bind::RouteSelector;
use crate::version::v0_0_1::http::HttpMethod;
use crate::version::v0_0_1::id::id::{Point, Port, Layer, ToPort, Topic, Uuid, ToPoint};
use crate::version::v0_0_1::log::{LogSpan, LogSpanEvent, PointLogger, SpanLogger};
use crate::version::v0_0_1::msg::MsgMethod;
use crate::version::v0_0_1::particle::particle::Details;
use crate::version::v0_0_1::substance::substance::{Call, CallKind, Errors, HttpCall, MsgCall, MultipartFormBuilder, SubstanceKind, Token, ToRequestCore};
use crate::version::v0_0_1::security::{Permissions, Privilege, Privileges};
use crate::version::v0_0_1::selector::selector::PointSelector;
use crate::version::v0_0_1::sys::AssignmentKind;
use crate::version::v0_0_1::util::{uuid, ValueMatcher, ValuePattern};
use cosmic_macros_primitive::Autobox;
use cosmic_nom::{Res, SpanExtra};
use http::{HeaderMap,  StatusCode, Uri};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::convert::Infallible;
use std::ops;
use std::ops::Deref;
use std::sync::Arc;
use dashmap::DashMap;
use tokio::sync::RwLock;
use crate::version::v0_0_1::parse::model::Subst;
use crate::version::v0_0_1::parse::sub;
use crate::version::v0_0_1::substance::substance::Substance;


#[derive(Serialize, Deserialize,Eq,PartialEq,Hash,strum_macros::Display,strum_macros::EnumString)]
pub enum WaveKind {
    Req,
    Res
}

#[derive(Serialize, Deserialize,Eq,PartialEq,Hash)]
pub struct WaveId {
    port: Port,
    uuid: Uuid,
    kind: WaveKind
}

impl WaveId {

    pub fn new( port: Port, kind: WaveKind ) -> Self {
        let uuid = uuid();
        Self::with_uuid(port,kind,uuid)
    }

    pub fn with_uuid( port: Port, kind: WaveKind, uuid: Uuid  ) -> Self {
        Self {
            port,
            uuid,
            kind
        }
    }
}


impl ToString for WaveId {
    fn to_string(&self) -> String {
        format!("{}<Wave<{}>>/{}",self.port.to_string(),self.kind.to_string(),self.uuid)
    }
}

#[derive(Serialize, Deserialize, Autobox)]
pub enum WaveXtra {
    Req(ReqXtra),
    Resp(RespXtra),
}

impl WaveXtra {
    pub fn id(&self) -> &Uuid {
        match self {
            WaveXtra::Req(request) => request.id(),
            WaveXtra::Resp(response) => response.id()
        }
    }

    pub fn to(&self) -> &Port{
        match self {
            WaveXtra::Req(request) => request.to(),
            WaveXtra::Resp(response) => response.to()
        }
    }

    pub fn from(&self) -> &Port{
        match self {
            WaveXtra::Req(request) => request.from(),
            WaveXtra::Resp(response) => response.from()
        }
    }

    pub fn span(&self) -> Option<&LogSpan> {
        match self {
            WaveXtra::Req(req) => req.span.as_ref(),
            WaveXtra::Resp(res) => res.span.as_ref()
        }
    }
}

pub struct RootReqCtx<I> {
    pub input: I,
    pub request: ReqShell,
    session: Option<Session>,
    logger: SpanLogger,
    pub tx: AsyncTransmitterWithAgent
}

impl <I> Deref for RootReqCtx<I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.input
    }
}

impl RootReqCtx<ReqShell> {
    pub fn new(request: ReqShell, logger: SpanLogger, tx: AsyncTransmitterWithAgent ) -> Self {
        Self {
            request: request.clone(),
            input: request.clone(),
            logger,
            session: None,
            tx
        }
    }
}

impl<I> RootReqCtx<I> {
    pub fn transform_input<I2, E>(self) -> Result<RootReqCtx<I2>, MsgErr>
    where
        I2: TryFrom<I, Error = E>,
        E: Into<MsgErr>,
    {
        let input = match I2::try_from(self.input) {
            Ok(input) => input,
            Err(err) => return Err(err.into()),
        };
        Ok(RootReqCtx {
            logger: self.logger,
            request: self.request,
            input,
            session: self.session,
            tx: self.tx,
        })
    }

    pub fn push<'a>(&'a self) -> ReqCtx<'a, I> {
        ReqCtx::new(self, &self.input, &self.tx, self.logger.clone())
    }
}

pub struct ReqCtx<'a, I> {
    root: &'a RootReqCtx<I>,
    tx: &'a AsyncTransmitterWithAgent,
    parent: Option<Box<ReqCtx<'a, I>>>,
    pub input: &'a I,
    pub logger: SpanLogger,
}

impl<'a, I> Deref for ReqCtx<'a, I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        self.input
    }
}

impl<'a, I> ReqCtx<'a, I> {
    pub fn new(root: &'a RootReqCtx<I>, input: &'a I, tx: &'a AsyncTransmitterWithAgent, logger: SpanLogger) -> Self {
        Self {
            root,
            parent: None,
            input,
            logger,
            tx
        }
    }

    pub fn push(self) -> ReqCtx<'a, I> {
        ReqCtx {
            root: self.root,
            input: self.input,
            logger: self.logger.span(),
            tx: self.tx,
            parent: Some(Box::new(self)),
        }
    }

    pub fn pop(self) -> Option<ReqCtx<'a, I>> {
        match self.parent {
            None => None,
            Some(parent) => Some(*parent),
        }
    }

    pub fn request(&self) -> &ReqShell {
        &self.root.request
    }
}

impl<'a> ReqCtx<'a, &mut ReqShell> {
    pub fn ok_substance(self, substance: Substance) -> RespCore {
        self.input.core.ok(substance)
    }

    pub fn not_found(self) -> RespCore {
        self.input.core.not_found()
    }

    pub fn err(self, err: MsgErr) -> RespCore {
        self.input.core.err(err)
    }
}

impl<'a> ReqCtx<'a, RespShell> {
    pub fn pass(self) -> RespCore {
        self.input.core.clone()
    }

    pub fn not_found(self) -> RespCore {
        let mut core = self.input.core.clone();
        core.status = StatusCode::from_u16(404).unwrap();
        core
    }

    pub fn err(self, err: MsgErr) -> RespCore {
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
    fn forbidden(self) -> R where Self: Sized{
        self.status(403)
    }

    fn bad_request(self) -> R  where Self: Sized{
        self.status(400)
    }

    fn not_found(self) -> R  where Self: Sized{
        self.status(404)
    }

    fn timeout(self) -> R  where Self: Sized{
        self.status(408)
    }

    fn server_error(self) -> R  where Self: Sized{
        self.status(500)
    }

    fn status(self, status: u16) -> R where Self: Sized;

    fn fail<M:ToString>(self, status: u16, message: M ) -> R where Self: Sized;

    fn err(self, err: MsgErr) -> R where Self: Sized;

    fn ok(self) -> R where Self: Sized;

    fn body(self, body: Substance) -> R where Self: Sized;

    fn core(self, core: RespCore) -> R where Self: Sized;

    fn result<C:Into<RespCore>>(self, result: Result<C,MsgErr>) -> R  where Self: Sized{
        match result {
            Ok(core) => self.core(core.into()),
            Err(err) => self.core(err.into())
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReqStub {
    pub id: String,
    pub agent: Agent,
    pub handling: Handling,
    pub from: Port,
    pub to: Port,
    pub span: Option<LogSpan>
}

impl Into<WaitTime> for &ReqStub{
    fn into(self) -> WaitTime {
        self.handling.wait.clone()
    }
}


impl Requestable<RespShell> for ReqStub {
    fn status(self, status: u16) -> RespShell {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::status(status),
            response_to: self.id,
        }
    }

    fn err(self, err: MsgErr) -> RespShell {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::err(err),
            response_to: self.id,
        }
    }

    fn ok(self) -> RespShell {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::ok(Substance::Empty),
            response_to: self.id,
        }
    }

    fn body(self, body: Substance) -> RespShell {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::ok(body),
            response_to: self.id,
        }
    }

    fn core(self, core: RespCore) -> RespShell {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core,
            response_to: self.id,
        }
    }
}

impl Requestable<RespXtra> for ReqStub {
    fn status(self, status: u16) -> RespXtra {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::status(status),
            response_to: self.id,
        }
        .to_frame(self.span)
    }

    fn err(self, err: MsgErr) -> RespXtra {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::err(err),
            response_to: self.id,
        }
        .to_frame(self.span)
    }

    fn ok(self) -> RespXtra {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::ok(Substance::Empty),
            response_to: self.id,
        }
        .to_frame(self.span)
    }

    fn body(self, body: Substance) -> RespXtra {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::ok(body),
            response_to: self.id,
        }
        .to_frame(self.span)
    }

    fn core(self, core: RespCore) -> RespXtra {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core,
            response_to: self.id,
        }
            .to_frame(self.span)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
pub struct ReqShell {
    pub id: String,
    pub agent: Agent,
    pub scope: Scope,
    pub handling: Handling,
    pub from: Port,
    pub to: Port,
    pub core: ReqCore,
}

impl Into<ReqProto> for ReqShell {
    fn into(self) -> ReqProto {
        ReqProto {
            id: self.id,
            to: Some(self.to),
            core: Some(self.core),
            handling: self.handling,
            scope: self.scope,
        }
    }
}

impl ReqShell {
    pub fn to_call(&self) -> Result<Call, MsgErr> {
        let kind = match &self.core.method {
            Method::Cmd(_) => {
                unimplemented!()
            }
            Method::Sys(_) => {
                unimplemented!()
            }
            Method::Http(method) => {
                CallKind::Http(HttpCall::new(method.clone(), Subst::new(self.core.uri.path())?))
            }
            Method::Msg(method) => {
                CallKind::Msg(MsgCall::new(method.clone(), Subst::new(self.core.uri.path())?))
            }
        };

        Ok(Call {
            point: self.item.to.clone().to_point(),
            kind: kind.clone()
        })
    }
}

impl Into<WaitTime> for &ReqShell {
    fn into(self) -> WaitTime {
        self.handling.wait.clone()
    }
}

impl Requestable<RespShell> for ReqShell {
    fn status(self, status: u16) -> RespShell {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::status(status),
            response_to: self.id,
        }
    }

    fn fail<M: ToString>(self, status: u16, message: M) -> RespShell where Self: Sized {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: self.core.fail(status, message ),
            response_to: self.id,
        }
    }

    fn err(self, err: MsgErr) -> RespShell {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::err(err),
            response_to: self.id,
        }
    }

    fn ok(self) -> RespShell {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::ok(Substance::Empty),
            response_to: self.id,
        }
    }

    fn body(self, body: Substance) -> RespShell {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core: RespCore::ok(body),
            response_to: self.id,
        }
    }

    fn core(self, core: RespCore) -> RespShell {
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core,
            response_to: self.id
        }
    }


}

impl ReqShell {
    pub fn as_stub(&self) -> ReqStub {
        ReqStub {
            id: self.id.clone(),
            agent: self.agent.clone(),
            handling: self.handling.clone(),
            from: self.from.clone(),
            to: self.to.clone(),
            span: None
        }
    }

    pub fn require_method<M: Into<Method>+ToString+Clone>(self, method: M) -> Result<ReqShell, MsgErr> {
        if self.core.method == method.clone().into() {
            Ok(self)
        } else {
            Err(MsgErr::new(
                400,
                format!("Bad Request: expecting method: {}", method.to_string()).as_str(),
            ))
        }
    }

    pub fn require_body<B>(self) -> Result<B, MsgErr>
    where
        B: TryFrom<Substance, Error = MsgErr>,
    {
        match B::try_from(self.clone().core.body) {
            Ok(body) => Ok(body),
            Err(err) => Err(MsgErr::bad_request()),
        }
    }

    pub fn server_error(&self) -> RespShell {
        self.as_stub().server_error()
    }

    pub fn timeout(&self) -> RespShell {
        self.as_stub().timeout()
    }

    pub fn not_found(&self) -> RespShell {
        self.as_stub().not_found()
    }

    pub fn forbidden(&self) -> RespShell {
        self.as_stub().forbidden()
    }

    pub fn bad_request(&self) -> RespShell {
        self.as_stub().bad_request()
    }

    pub fn status(&self, status: u16) -> RespShell {
        self.as_stub().status(status)
    }

    pub fn to_frame(self, span: Option<LogSpan>) -> ReqXtra {
        ReqXtra {
            session: None,
            request: self,
            span
        }
    }

}

#[derive(Serialize, Deserialize)]
pub struct ReqXtra {
    pub session: Option<Session>,
    pub request: ReqShell,
    pub span: Option<LogSpan>,
}

impl ReqXtra {
    pub fn from(&self) -> &Port {
        &self.request.from
    }

    pub fn to(&self) -> &Port {
        &self.request.to
    }
}

impl ReqXtra {
    pub fn id(&self) -> &Uuid {
        &self.request.id
    }

    pub fn as_stub(&self) -> ReqStub {
        let mut stub = self.request.as_stub();
        stub.span = self.span.clone();
        stub
    }
}

impl Into<WaitTime> for &ReqXtra {
    fn into(self) -> WaitTime {
        (&self.request).into()
    }
}

impl Requestable<RespXtra> for ReqXtra {
    fn status(self, status: u16) -> RespXtra {
        RespXtra {
            session: None,
            response: self.request.status(status),
            span: self.span,
        }
    }

    fn err(self, err: MsgErr) -> RespXtra {
        RespXtra {
            session: None,
            response: self.request.err(err),
            span: self.span,
        }
    }

    fn ok(self) -> RespXtra {
        RespXtra {
            session: None,
            response: self.request.ok(),
            span: self.span,
        }
    }

    fn body(self, body: Substance) -> RespXtra {
        RespXtra {
            session: None,
            response: self.request.body(body),
            span: self.span,
        }
    }

    fn core(self, core: RespCore) -> RespXtra {
        let response = RespShell::new(core, self.request.to, self.request.from, self.request.id);
        RespXtra {
            session: None,
            response,
            span: self.span
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct RespXtra {
    pub session: Option<Session>,
    pub response: RespShell,
    pub span: Option<LogSpan>,
}

impl RespXtra {
    pub fn new(response: RespShell) -> Self {
        Self {
            response,
            session: None,
            span: None
        }
    }
    pub fn id(&self) -> &Uuid {
        &self.response.id
    }
    pub fn from(&self) -> &Port {
        &self.response.from
    }
    pub fn to(&self) -> &Port {
        &self.response.to
    }
    pub fn response_to(&self) -> &Uuid { &self.response.response_to }
}

impl TryFrom<ReqShell> for RawCommand {
    type Error = MsgErr;

    fn try_from(request: ReqShell) -> Result<Self, Self::Error> {
        request.core.body.try_into()
    }
}

impl TryFrom<RespShell> for Substance {
    type Error = MsgErr;

    fn try_from(response: RespShell) -> Result<Self, Self::Error> {
        Ok(response.core.body)
    }
}

impl TryInto<Bin> for RespShell {
    type Error = MsgErr;

    fn try_into(self) -> Result<Bin, Self::Error> {
        match self.core.body {
            Substance::Bin(bin) => Ok(bin),
            _ => Err(MsgErr::err400()),
        }
    }
}

impl Into<ReqCore> for RawCommand {
    fn into(self) -> ReqCore {
        ReqCore::substance(
            MsgMethod::new("ExecCommand").unwrap().into(),
            Substance::RawCommand(self),
        )
    }
}

impl ReqShell {
    pub fn result<E: StatusErr>(self, result: Result<RespCore, E>) -> RespShell {
        match result {
            Ok(core) => RespShell {
                id: uuid(),
                to: self.from,
                from: self.to,
                core,
                response_to: self.id,
            },
            Err(err) => {
                let core = self.core.err(err);
                RespShell {
                    id: uuid(),
                    to: self.from,
                    from: self.to,
                    core,
                    response_to: self.id,
                }
            }
        }
    }

    pub fn body_result<E: StatusErr>(self, result: Result<Substance, E>) -> RespShell {
        match result {
            Ok(substance) => self.ok_body(substance),
            Err(err) => {
                let core = self.core.err(err);
                RespShell {
                    id: uuid(),
                    to: self.from,
                    from: self.to,
                    core,
                    response_to: self.id,
                }
            }
        }
    }

    pub fn err(self, err: MsgErr) -> RespShell {
        let core = self.core.err(err);
        RespShell {
            id: uuid(),
            to: self.from,
            from: self.to,
            core,
            response_to: self.id,
        }
    }
}

impl ReqShell {
    pub fn new<P: ToPort>(core: ReqCore, from: P, to: P) -> Self {
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

    pub fn substance_result<E>(self, result: Result<Payload,E> ) -> Response where E: ToString {
        match result {
            Ok(substance) => {
                self.ok_substance(substance)
            }
            Err(err) => {
                self.fail(err.to_string().as_str())
            }
        }
    }

     */

    pub fn ok(self) -> RespShell {
        let core = RespCore {
            headers: Default::default(),
            status: StatusCode::from_u16(200u16).unwrap(),
            body: Substance::Empty,
        };
        let response = RespShell {
            id: uuid(),
            from: self.to,
            to: self.from,
            core,
            response_to: self.id,
        };
        response
    }

    pub fn ok_body(self, body: Substance) -> RespShell {
        let core = RespCore {
            headers: Default::default(),
            status: StatusCode::from_u16(200u16).unwrap(),
            body,
        };
        let response = RespShell {
            id: uuid(),
            from: self.to,
            to: self.from,
            core,
            response_to: self.id,
        };
        response
    }

    pub fn fail(self, status: u16, error: &str) -> RespShell {
        let core = RespCore {
            headers: Default::default(),
            status: StatusCode::from_u16(status ).or_else(||StatusCode::from_u16(500u16)).unwrap(),
            body: Substance::Errors(Errors::default(error.to_string().as_str())),
        };
        let response = RespShell {
            id: uuid(),
            from: self.to,
            to: self.from,
            core,
            response_to: self.id,
        };
        response
    }
}

pub struct ReqBuilder {
    pub to: Option<Port>,
    pub from: Option<Port>,
    pub core: Option<ReqCore>,
    pub agent: Agent,
    pub session: Option<Session>,
    pub scope: Scope,
    pub handling: Handling,
}

impl ReqBuilder {
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

    pub fn core(mut self, core: ReqCore) -> Self {
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

    pub fn build(self) -> Result<ReqShell, MsgErr> {
        Ok(ReqShell {
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

impl Default for ReqBuilder {
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
pub struct ReqProto {
    pub id: String,
    pub to: Option<Port>,
    pub core: Option<ReqCore>,
    pub handling: Handling,
    pub scope: Scope,
}

impl ReqProto {
    pub fn to_request<P>(self, from: P, agent: Agent, scope: Scope) -> Result<ReqShell,MsgErr>
    where
        P: ToPort,
    {
        let scope = self.scope | scope;
        let request = ReqShell {
            id: self.id,
            from: from.to_port(),
            to: self.to.ok_or(MsgErr::new(500u16,"must set 'to'"))?,
            core: self.core.ok_or(MsgErr::new(500u16,"request core must be set"))?,
            agent,
            handling: self.handling,
            scope,
        };
        Ok(request)
    }

    pub fn body( &mut self, body: Substance ) -> Result<(),MsgErr>{
        self.core.as_mut().ok_or(MsgErr::new(500u16,"core must be set before body") )?.body = body;
        Ok(())
    }

    pub fn method( &mut self, method: Method) -> Result<(),MsgErr>{
        if self.core.is_none() {
            let core:ReqCore = method.into();
            self.core = Some(core);
        } else {
            self.core.as_mut().unwrap().method = method;
        }
        Ok(())
    }
}

impl ReqProto {
    pub fn new<P: ToPort>(to: P, method: Method) -> Self {
        Self {
            id: uuid(),
            to: Some(to.to_port()),
            core: Some(ReqCore::new(method)),
            handling: Default::default(),
            scope: Scope::None
        }
    }

    pub fn from_core(core: ReqCore ) -> Self {
        Self {
            id: uuid(),
            to: None,
            core: Some(core),
            handling: Default::default(),
            scope: Scope::None
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

    pub fn core<P: ToPort>(to: P, core: ReqCore) -> Self {
        Self {
            id: uuid(),
            to: Some(to.to_port()),
            core: Some(core),
            scope: Default::default(),
            handling: Default::default()
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
pub struct RespShell {
    pub id: Uuid,
    pub from: Port,
    pub to: Port,
    pub core: RespCore,
    pub response_to: Uuid,
}

impl RespShell {
    pub fn core_result<E>( result: Result<RespShell,E> ) -> Result<RespCore,E> {
        match result {
            Ok(response) => Ok(response.core),
            Err(err) => Err(err)
        }
    }

    pub fn to_frame(self, span: Option<LogSpan>) -> RespXtra {
        RespXtra {
            session: None,
            response: self,
            span,
        }
    }

    pub fn to_span_frame(self, span: LogSpan ) -> RespXtra {
        RespXtra {
            session: None,
            response: self,
            span: Some(span),
        }
    }

    pub fn as_result<E: From<&'static str>, P: TryFrom<Substance>>(self) -> Result<P, E> {
        self.core.as_result()
    }


}

impl RespShell {
    pub fn new(core: RespCore, from: Port, to: Port, response_to: String) -> Self {
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
            if let Substance::Text(error) = self.core.body {
                Err(error.into())
            } else {
                Err(format!("error code: {}", self.core.status).into())
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Autobox,Eq,PartialEq)]
pub enum Wave {
    Req(ReqShell),
    Resp(RespShell),
}

impl Wave {

    pub fn id(&self) -> Uuid {
        match self {
            Wave::Req(request) => request.id.clone(),
            Wave::Resp(response) => response.id.clone(),
        }
    }

    pub fn substance(&self) -> Substance {
        match self {
            Wave::Req(request) => request.core.body.clone(),
            Wave::Resp(response) => response.core.body.clone(),
        }
    }

    pub fn to(&self) -> &Port {
        match self {
            Wave::Req(request) => &request.to,
            Wave::Resp(response) => &response.to,
        }
    }

    pub fn from(&self) -> &Port {
        match self {
            Wave::Req(request) => &request.from,
            Wave::Resp(response) => &response.from,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RequestTransform {
    Request(ReqCore),
    Response(RespCore),
}

pub enum ResponseKindExpected {
    None,
    Synch,          // requestor will wait for response
    Async(Substance), // The substance
}

#[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
pub enum Agent {
    Anonymous,
    Point(Point),
}

impl Default for Agent {
    fn default() -> Self {
        Self::Anonymous
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

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Scope {
    Full,
    None,
    Grants(HashSet<ScopeGrant>),
}

impl Scope {
    pub fn has_grant( &self, grant: &ScopeGrant ) -> Result<(),()> {
       match self {
           Scope::Full => Ok(()),
           Scope::None => Err(()),
           Scope::Grants(grants)  if grants.contains(grant) => {
               Ok(())
           }
           _ => Err(())
       }
    }

    pub fn enumerated_grants(&self) -> HashSet<ScopeGrant> {
        match self {
            Scope::Full => HashSet::new(),
            Scope::None => HashSet::new(),
            Scope::Grants(grants) => grants.clone()
        }
    }
}

impl From<HashSet<ScopeGrant>> for Scope {
    fn from(grants: HashSet<ScopeGrant>) -> Self {
        Scope::Grants(grants)
    }
}

impl ops::BitAnd<Scope> for Scope{
    type Output = Scope;

    fn bitand(self, rhs: Scope) -> Self::Output {
        if self == Self::Full && rhs == Self::Full {
            Self::Full
        } else if self == Self::None || rhs == Self::None {
            Self::None
        } else {
            let mut grants = self.enumerated_grants();
            grants.retain(|grant|{rhs.has_grant(grant).is_ok()});
            grants.into()
        }
    }
}

impl ops::BitOr<Scope> for Scope{
    type Output = Scope;

    fn bitor(self, rhs: Scope) -> Self::Output {
        if self == Self::Full || rhs == Scope::Full {
            Self::Full
        } else {
            let left = self.enumerated_grants();
            let right = rhs.enumerated_grants();
            Self::Grants(left.union(&right).cloned().collect())
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

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq,Hash)]
pub struct ScopeGrant {
    pub on: PointSelector,
    pub kind: ScopeGrantKind,
    pub aspect: ScopeGrantAspect,
}


#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq,Hash)]
pub enum ScopeGrantKind {
    Or,
    And,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq,Hash)]
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

#[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
pub struct Handling {
    pub kind: HandlingKind,
    pub priority: Priority,
    pub retries: Retries,
    pub wait: WaitTime,
}

#[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
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

#[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
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

#[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
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

#[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
pub enum Priority {
    Hyper,
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
    Hyper,
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

pub trait Router: Send+Sync {
    async fn route( &self, wave: Wave );
}

#[derive(Clone)]
pub struct AsyncPointRequestHandlers{
    pub handlers: Arc<DashMap<Point,Box<dyn AsyncRequestHandler>>>
}

impl AsyncPointRequestHandlers {
    pub fn new() -> Self {
        Self {
            handlers: Arc::new(DashMap::new())
        }
    }

    pub fn add(&self, point: Point, handler: Box<dyn AsyncRequestHandler>) {
        self.handlers.insert( point, handler );
    }

    pub fn remove(&self, point: &Point ) {
        self.handlers.remove(point);
    }
}

#[async_trait]
impl AsyncRequestHandler for  AsyncPointRequestHandlers {

    async fn select(&self, request: &ReqShell) -> Result<(), ()> {
        if let Some(handler) = self.handlers.get( &request.to.clone().to_point() ) {
            handler.select(request).await
        } else {
            Err(())
        }
    }

    async fn handle(&self, request: RootReqCtx<ReqShell>) -> Result<RespCore, MsgErr> {
        if let Some(handler) = self.handlers.get( &request.to ) {
            handler.handle(request).await
        } else {
            Err(MsgErr::not_found())
        }
    }
}

pub trait TransportPlanner {
    fn dest<P: ToPort>( port: P ) -> Port;
}

/// Transporter will always wrap a Wave in another Wave which will then
/// transport the wave to the next desired hop in its journey as determined
/// by the TransportPlanner
pub struct Transporter {
    pub planner: Box<dyn TransportPlanner>,
    pub transmitter: AsyncTransmitterWithAgent
}

impl Transporter {
    pub fn new( planner: Box<dyn TransportPlanner>, transmitter: AsyncTransmitterWithAgent, logger: PointLogger) -> Self {
        Self {
            planner,
            transmitter
        }
    }

    pub async fn request(&self, req: ReqShell ) -> Result<RespShell,MsgErr> {
        let dest = self.planner.dest(wave.to().clone());
        let mut trans = ReqProto::sys(dest, SysMethod::Transport );
        trans.body(Wave::Req(req).into());
        let resp = self.transmitter.send(trans).await?;
        let wave:Wave  = resp.core.body.try_into()?;
        match wave {
            Wave::Req(_) => {
                Err(MsgErr::bad_request())
            }
            Wave::Resp(resp) => {
                Ok(resp)
            }
        }
    }

    pub async fn response(&self, req: ReqShell ) {
        let dest = self.planner.dest(wave.to().clone());
        let mut trans = ReqProto::sys(dest, SysMethod::Transport );
        trans.body(Wave::Req(req).into());
        self.transmitter.send(trans).await;
        // here we don't wait for a response becauase we can't do anything with it anyway
    }

}

#[derive(Clone)]
pub struct AsyncTransmitterWithAgent {
    pub agent: Agent,
    pub from: Port,
    pub relay: Arc<dyn AsyncTransmitter>,
}

impl AsyncTransmitterWithAgent {
    pub fn new(
        agent: Agent,
        from: Port,
        relay: Arc<dyn AsyncTransmitter>,
    ) -> Self {
        Self { agent, from, relay }
    }

    pub fn with_topic( self, topic: Topic ) -> Self {
        Self {
            agent: self.agent,
            from: self.from.with_topic(topic),
            relay: self.relay
        }
    }
}

impl AsyncTransmitterWithAgent {
    pub async fn send(&self, request: ReqProto) -> Result<RespShell,MsgErr> {
        let request = request.to_request(self.from.clone(), self.agent.clone(), Scope::None)?;
        Ok(self.relay.send(request).await)
    }

    pub fn send_sync(&self, request: ReqProto) ->  Result<RespShell,MsgErr>{
        let request = request.to_request(self.from.clone(), self.agent.clone(), Scope::None)?;
        Ok(self.relay.send_sync(request))
    }

    pub async fn route(&self, wave: Wave )  {
        self.relay.route(wave).await;
    }
}

impl AsyncTransmitterWithAgent {
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
pub struct SyncTransmitRelay {
    pub topic: Option<Topic>,
    pub layer: Option<Layer>,
    pub point: Option<Point>,
    pub relay: Arc<dyn SyncTransmitter>,
}

impl SyncTransmitter for SyncTransmitRelay {
    fn send(&self, request: ReqShell) -> RespShell {
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

impl SyncTransmitRelay {
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

    pub fn with_layer(self, layer: Layer) -> Self {
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
pub trait AsyncTransmitter: Send + Sync
{
    async fn send(&self, request: ReqShell ) -> RespShell;
    fn send_sync(&self, request: ReqShell) -> RespShell;
    async fn route(&self, wave: Wave);
}

pub trait SyncTransmitter: Send + Sync {
    fn send(&self, request: ReqShell) -> RespShell;
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
    fn select(&self, request: &ReqShell) -> Result<(), ()>;
    fn handle(&self, request: RootReqCtx<ReqShell>) -> Result<RespCore, MsgErr>;
}

#[async_trait]
pub trait AsyncRequestHandler: Sync + Send {
    async fn select(&self, request: &ReqShell) -> Result<(), ()>;
    async fn handle(&self, request: RootReqCtx<ReqShell>) -> Result<RespCore, MsgErr>;
}

impl RequestHandler for RequestHandlerRelay {
    fn select(&self, request: &ReqShell) -> Result<(), ()> {
        self.relay.select(request)
    }

    fn handle(&self, request: RootReqCtx<ReqShell>) -> Result<RespCore, MsgErr> {
        self.relay.handle(request)
    }
}

#[async_trait]
pub trait AsyncAuthorizationRequester {
    async fn authorize(self, messenger: AsyncTransmitterWithAgent) -> Result<AuthResponse, MsgErr>;
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

#[async_trait]
impl AsyncAuthorizationRequester for AsyncCredsAuthorizationRequester {
    async fn authorize(self, messenger: AsyncTransmitterWithAgent) -> Result<AuthResponse, MsgErr> {
        let mut form = MultipartFormBuilder::new();
        form.put( "username", self.credentials.username.as_str() );
        form.put( "password", self.credentials.password.as_str() );
        let form = form.build()?;
        let request = form.to_request_core();
        let mut request = ReqProto::core(self.auth_point.clone(), request );
        request.handling.wait = WaitTime::High;
        let refresh_token: Token = messenger.send(request).await?.try_into()?;
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

#[async_trait]
impl AsyncAuthorizationRequester for AsyncRefreshTokenAuthorizationRequester {
    async fn authorize(self, messenger: AsyncTransmitterWithAgent) -> Result<AuthResponse, MsgErr> {
        unimplemented!()
    }
}


#[derive(Clone)]
pub struct AsyncRequestHandlerRelay {
    pub relay: Arc<dyn AsyncRequestHandler>,
}

impl AsyncRequestHandlerRelay {
    pub fn new(handler: Arc<dyn AsyncRequestHandler>) -> Self {
        Self { relay: handler }
    }
}

#[async_trait]
impl AsyncRequestHandler for AsyncRequestHandlerRelay {
    async fn select(&self, request: &ReqShell) -> Result<(), ()> {
        self.relay.select(request).await
    }

    async fn handle(&self, ctx: RootReqCtx<ReqShell>) -> Result<RespCore, MsgErr> {
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

#[async_trait]
impl AsyncRequestHandler for AsyncInternalRequestHandlers<AsyncRequestHandlerRelay> {
    async fn select(&self, request: &ReqShell) -> Result<(), ()> {
        let read = self.pipelines.read().await;
        for pipeline in read.iter() {
            if pipeline.selector.is_match(&request).is_ok() {
                return pipeline.handler.select(request).await
            }
        }
        Err(())
    }

    async fn handle(&self, ctx: RootReqCtx<ReqShell>) -> Result<RespCore, MsgErr> {
        let read = self.pipelines.read().await;
        for pipeline in read.iter() {
            if pipeline.selector.is_match(&ctx.request).is_ok() {
                return pipeline.handler.handle(ctx).await;
            }
        }
        Ok(RespCore::not_found())
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

impl From<Result<RespCore,MsgErr>> for RespCore {
    fn from(result: Result<RespCore, MsgErr>) -> Self {
        match result {
            Ok(response) => response,
            Err(err) => {
                err.into()
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct RespCore {
    #[serde(with = "http_serde::header_map")]
    pub headers: HeaderMap,

    #[serde(with = "http_serde::status_code")]
    pub status: StatusCode,

    pub body: Substance,
}

impl RespCore {
    pub fn ok_html(html: &str) -> Self {
        let bin = Arc::new(html.to_string().into_bytes());
        RespCore::ok(Substance::Bin(bin))
    }

    pub fn new() -> Self {
        RespCore {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(200u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn ok(body: Substance) -> Self {
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
            body: Substance::Empty,
        }
    }

    pub fn server_error() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(500u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn status(status: u16) -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(status).unwrap_or(StatusCode::from_u16(500).unwrap()),
            body: Substance::Empty,
        }
    }

    pub fn not_found() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(404u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn forbidden() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(403u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn bad_request() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(400u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn fail(status: u16, message: &str) -> Self {
        let errors = Errors::default(message.clone());
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(status).or_else( ||StatusCode::from_u16(500u16) ).unwrap(),
            body: Substance::Errors(errors),
        }
    }

    pub fn err(err: MsgErr) -> Self {
        let errors = Errors::default(err.to_string().as_str());
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(err.status())
                .unwrap_or(StatusCode::from_u16(500u16).unwrap()),
            body: Substance::Errors(errors),
        }
    }

    pub fn with_new_substance(self, substance: Substance) -> Self {
        Self {
            headers: self.headers,
            status: self.status,
            body: substance,
        }
    }

    pub fn is_ok(&self) -> bool {
        return self.status.is_success();
    }

    pub fn into_response<P>(self, from: P, to: P, response_to: String) -> RespShell
    where
        P: ToPort,
    {
        RespShell {
            id: uuid(),
            from: from.to_port(),
            to: to.to_port(),
            core: self,
            response_to,
        }
    }
}

impl RespCore {
    pub fn as_result<E: From<&'static str>, P: TryFrom<Substance>>(self) -> Result<P, E> {
        if self.status.is_success() {
            match P::try_from(self.body) {
                Ok(substance) => Ok(substance),
                Err(err) => Err(E::from("error")),
            }
        } else {
            Err(E::from("error"))
        }
    }
}

impl TryInto<http::response::Builder> for RespCore {
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

impl TryInto<http::Response<Bin>> for RespCore {
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

impl Into<ReqCore> for Method {
    fn into(self) -> ReqCore {
        ReqCore {
            headers: Default::default(),
            method: self,
            uri: Uri::from_static("/"),
            body: Substance::Empty,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ReqCore {
    #[serde(with = "http_serde::header_map")]
    pub headers: HeaderMap,
    pub method: Method,
    #[serde(with = "http_serde::uri")]
    pub uri: Uri,
    pub body: Substance,
}

impl ReqCore {
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

impl TryFrom<ReqShell> for ReqCore {
    type Error = MsgErr;

    fn try_from(request: ReqShell) -> Result<Self, Self::Error> {
        Ok(request.core)
    }
}

impl ReqCore {
    pub fn kind(&self) -> MethodKind {
        self.method.kind()
    }
}

impl Into<ReqCore> for Command {
    fn into(self) -> ReqCore {
        ReqCore {
            body: Substance::Command(Box::new(self)),
            method: Method::Msg(MsgMethod::new("Command").unwrap()),
            ..Default::default()
        }
    }
}

impl TryFrom<http::Request<Bin>> for ReqCore {
    type Error = MsgErr;

    fn try_from(request: http::Request<Bin>) -> Result<Self, Self::Error> {
        Ok(Self {
            headers: request.headers().clone(),
            method: Method::Http(request.method().clone().try_into()?),
            uri: request.uri().clone(),
            body: Substance::Bin(request.body().clone()),
        })
    }
}

impl TryInto<http::Request<Bin>> for ReqCore {
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

impl Default for ReqCore {
    fn default() -> Self {
        Self {
            headers: Default::default(),
            method: Method::Msg(Default::default()),
            uri: Uri::from_static("/"),
            body: Substance::Empty,
        }
    }
}

impl ReqCore {
    pub fn with_body(self, body: Substance) -> Self {
        Self {
            headers: self.headers,
            uri: self.uri,
            method: self.method,
            body,
        }
    }

    pub fn not_found(&self) -> RespCore {
        RespCore {
            headers: Default::default(),
            status: StatusCode::from_u16(404u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn substance(method: Method, body: Substance) -> ReqCore {
        ReqCore {
            method,
            body,
            ..Default::default()
        }
    }

    pub fn ok(&self, substance: Substance) -> RespCore {
        RespCore {
            headers: Default::default(),
            status: StatusCode::from_u16(200u16).unwrap(),
            body: substance,
        }
    }

    pub fn fail<M:ToString>(&self, status: u16, message: M) -> RespCore {
        let errors = Errors::default(error);
        RespCore {
            headers: Default::default(),
            status: StatusCode::from_u16(status).or_else(||StatusCode::from_u16(500u16)).unwrap(),
            body: Substance::Errors(errors),
        }
    }

    pub fn err<E: StatusErr>(&self, error: E) -> RespCore {
        let errors = Errors::default(error.message().as_str());
        let status = match StatusCode::from_u16(error.status()) {
            Ok(status) => status,
            Err(_) => StatusCode::from_u16(500u16).unwrap(),
        };
        println!("----->   returning STATUS of {}", status.as_str());
        RespCore {
            headers: Default::default(),
            status,
            body: Substance::Errors(errors),
        }
    }
}

impl Into<RespCore> for Port {
    fn into(self) -> RespCore {
        RespCore::ok(Substance::Port(self))
    }
}

impl TryFrom<RespCore> for Port {
    type Error = MsgErr;

    fn try_from(core: RespCore) -> Result<Self, Self::Error> {
        if !core.status.is_success() {
            Err(MsgErr::new(core.status.as_u16(), "error"))
        } else {
            match core.body {
                Substance::Port(port) => Ok(port),
                substance => {
                    Err(format!("expecting Port received {}", substance.kind().to_string()).into())
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
    AssignPort,
    EntryReq,
    Transport
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
