use alloc::string::String;
use alloc::vec::Vec;
use core::option::Option;
use core::option::Option::None;
use core::result::Result::{Err, Ok};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use mesh_portal::error::MsgErr;
use mesh_portal::version::latest::cli::{RawCommand, Transfer};
use mesh_portal::version::latest::entity::request::create::{CreateOp, Fulfillment, KindTemplate, Set};
use mesh_portal::version::latest::entity::request::{Method, Rc};
use mesh_portal::version::latest::entity::request::get::Get;
use mesh_portal::version::latest::entity::request::select::Select;
use mesh_portal::version::latest::entity::response::ResponseCore;
use mesh_portal::version::latest::id::{Point, Port, TargetLayer, Topic, Uuid};
use mesh_portal::version::latest::messaging::{Request, Response};
use mesh_portal::version::latest::particle::Stub;
use mesh_portal::version::latest::payload::{Payload, PayloadType};
use mesh_portal::version::latest::Port;
use mesh_portal::version::latest::util::uuid;
use mesh_portal_versions::version::v0_0_1::id::id::{ToPoint, ToPort};
use mesh_portal_versions::version::v0_0_1::messaging::messaging::{MessageCtx, RootMessageCtx};
use mesh_portal_versions::version::v0_0_1::parse::{command, command_line, Env};
use mesh_portal_versions::version::v0_0_1::parse::error::result;
use mesh_portal_versions::version::v0_0_1::service::{Handlers, Handler, Router, MessengerProxy, AsyncMessenger, AsyncMessengerProxy, HandlerPair, HandlerSelector};
use mesh_portal_versions::version::v0_0_1::span::new_span;
use mesh_portal_versions::version::v0_0_1::util::ToResolved;
use starlane_core::particle::KindBase;
use starlane_core::starlane::api::StarlaneApi;
use tokio::sync::mpsc::bounded;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}

pub struct CliRelay {
  pub sessions: HashMap<Port,CliSession>,
  pub port: Port,
  pub router: Arc<dyn Router>,
  pub handlers: Arc<Handlers<AsyncMessengerProxy<Request>,dyn FnMut(RootMessageCtx<Request,AsyncMessengerProxy<Request>>)->Result<ResponseCore,MsgErr>>>
}

impl CliRelay {
    fn new_session(&mut self, source: Port ) -> Port {
        let mut port = self.port.clone();
        port.layer = TargetLayer::Shell;
        port.topic = Topic::Uuid(uuid());

        let session = CliSession{
            stub: stub.clone(),
            env: Env::new(self.port.clone().to_point() ),
            router: self.router.clone(),
            source
        };



        HandlerPair::new( HandlerSelector::Topic(port.topic.clone()), )
        self.sessions.insert(port.clone(), session );

        self.handlers.add( )

        port
    }

    pub async fn handle( & mut self, request: Request ) -> ResponseCore {

        async fn inner_handle( relay: &mut CliRelay, request: Request ) -> Result<Response,MsgErr> {
            match &request.core.method {
                Method::Msg(method) if method.as_str() == "NewSession" && request.core.body.payload_type() == PayloadType::Empty => {
                    let session = relay.new_session();
                    Ok(request.ok_payload(Payload::Port(session)))
                }
                _ => {
                    Ok(request.fail("expected method 'NewSession' with payload 'Empty'"))
                }
            }
        }

        match inner_handle(self, request.clone() ).await {
            Ok(response) => response,
            Err(err) => request.core.fail(err.to_string().as_str() )
        }
    }
}

#[derive(Serialize,Deserialize)]
pub struct CliSessionStub {
   pub relay: Port,
   pub port: Port
}


pub struct CliSession {
    pub stub: CliSessionStub,
    pub env: Env,
    pub router: Arc<dyn Router>,
    // will only handle requests from THIS port
    pub source: Port
}

impl CliSession {
    pub async fn handle( & mut self, request: Request ) -> Response {

        async fn inner_handle( relay: &CliRelay, request: Request ) -> Result<Response,MsgErr> {
            match &request.core.method {
                Method::Msg(method) if method.as_str() == "Command" && request.core.body.payload_type() == PayloadType::RawCommand => {
                    if let Payload::RawCommand(raw) = &request.core.body {
                        let command = result(command(new_span(raw.line.as_str())))?;
                        let command = command.to_resolved(relay.)

                        // resolve command
                        // transfers...
                        // send to GLOBAL::command-service
                        // respond to request
                        Ok(request.ok())
                    } else {
                        Ok(request.fail("expected method 'Command' with payload 'RawCommand'"))
                    }
                }
                _ => {
                    Ok(request.fail("expected method 'Command' with payload 'RawCommand'"))
                }
            }
        }

        match inner_handle(self, request.clone() ).await {
            Ok(response) => response,
            Err(err) => request.fail(err.to_string().as_str() )
        }
    }
}

pub struct CommandExecutor {
    router: Arc<dyn Router>,
    stub: Stub,
    raw_command: RawCommand,
    env: Env
}

impl CommandExecutor {
    pub async fn exec_simple(
        raw_command: RawCommand,
        stub: Stub,
        api: StarlaneApi,
    ) -> Response {
        let (output_tx, output_rx) = bounded::channel(1024);
        tokio::spawn(async move {
            CommandExecutor::execute(raw_command, output_tx, stub, api, vec![]);
        });
        output_rx
    }

    pub async fn execute(
        line: String,
        output_tx: bounded::Sender<outlet::Frame>,
        stub: Stub,
        api: StarlaneApi,
        fulfillments: Vec<Fulfillment>,
    ) {
info!("executing line: {}", line);
        let executor = Self {
            api,
            stub,
            output_tx,
            raw_command: line,
            fulfillments,
            env: Env::no_point()
        };
        tokio::task::spawn_blocking(move || {
            tokio::spawn(async move {
                executor.parse().await;
            })
        })
        .await;
    }

    async fn parse(&self) {
info!("parsing command...");
        match command_line(new_span(self.raw_command.as_str())) {
            Ok((_, op)) => match op {
                CommandOp::Create(create) => {
info!("executing create...");
                    self.exec_create(create).await;
                }
                CommandOp::Select(select) => {
                    self.exec_select(select).await;
                }
                CommandOp::Publish(create_op) => {
info!("executing publish...");
                    let create_op = create_op.to_resolved(&self.env)?;
                    self.exec_publish(create_op).await;
                }
                CommandOp::Set(set) => {
                    let set = set.collapse().unwrap();
                    self.exec_set(set).await;
                }
                CommandOp::Get(get) => {
                    let get = get.collapse().unwrap();
                    self.exec_get(get).await;
                }
            },
            Err(err) => {
error!("could not parse command line...");
                self.output_tx
                    .send(outlet::Frame::StdErr(err.to_string()))
                    .await;
                self.output_tx.send(outlet::Frame::End(1)).await;
                return;
            }
        }
    }

    async fn exec_create(&self, create: CreateOpVar) {
        let create: CreateOp = match create.to_resolved(&self.env ) {
            Ok(create) => create,
            Err(err) => {
                err.print();
                self.output_tx
                    .send(outlet::Frame::StdErr(err.to_string()))
                    .await;
                return;
            }
        };
        let parent = create.template.point.parent.clone();
        let action = Method::Cmd(Rc::Create(create));
        let request = Request::new(action.into(), self.stub.point.clone(), parent);
        let response = self.api.exchange(request).await;

        match response.ok_or() {
            Ok(_) => {
                self.output_tx.send(outlet::Frame::End(0)).await;
            }
            Err(err) => {
                self.output_tx
                    .send(outlet::Frame::StdErr(err.to_string()))
                    .await;
                self.output_tx.send(outlet::Frame::End(1)).await;
            }
        }
    }

    async fn exec_select(&self, select: Select) {
        let query_root = select.pattern.query_root();
        let action = Method::Cmd(Rc::Select(select));
        let core = action.into();
        let request = Request::new(core, self.stub.point.clone(), query_root);
        let response = self.api.exchange(request).await;
        match response.ok_or() {
            Ok(response) => {
                let response = match response.ok_or() {
                    Ok(response) => response,
                    Err(fail) => {
                        self.output_tx
                            .send(outlet::Frame::StdErr(fail.to_string()))
                            .await;
                        self.output_tx.send(outlet::Frame::End(1)).await;
                        return;
                    }
                };
                match &response.core.body {
                    Payload::List(list) => {
                        for stub in list.iter() {
                            if let Payload::Stub(stub) = &**stub {
                                self.output_tx
                                    .send(outlet::Frame::StdOut(
                                        stub.clone().point_and_kind().to_string(),
                                    ))
                                    .await;
                            }
                        }
                        self.output_tx.send(outlet::Frame::End(0)).await;
                    }
                    _ => {
                        self.output_tx
                            .send(outlet::Frame::StdErr("unexpected response".to_string()))
                            .await;
                        self.output_tx.send(outlet::Frame::End(1)).await;
                    }
                }
            }
            Err(err) => {
                self.output_tx
                    .send(outlet::Frame::StdErr(err.to_string()))
                    .await;
                self.output_tx.send(outlet::Frame::End(1)).await;
            }
        }
    }

    async fn exec_publish(&self, mut create: CreateOp) {
        if self.fulfillments.len() != 1 {
            self.output_tx
                .send(outlet::Frame::StdErr(
                    "Expected one and only one TransferFile fulfillment for publish".to_string(),
                ))
                .await;
            self.output_tx.send(outlet::Frame::End(1)).await;
            return;
        }
        if let Option::Some(Fulfillment::File { name, content }) = self.fulfillments.get(0).cloned()
        {
            let mut create = create.fulfillment(content);
            create.template.kind = KindTemplate {
                kind: KindBase::ArtifactBundle.to_string(),
                sub_kind: None,
                specific: None,
            };

            let parent = create.template.point.parent.clone();
            let action = Method::Cmd(Rc::Create(create));
            let core = action.into();
            let request = Request::new(core, self.stub.point.clone(), parent);
            match self.api.exchange(request).await.ok_or() {
                Ok(response) => match response.ok_or() {
                    Ok(_) => {
                        self.output_tx.send(outlet::Frame::End(0)).await;
                    }
                    Err(fail) => {
                        self.output_tx
                            .send(outlet::Frame::StdErr(fail.to_string()))
                            .await;
                        self.output_tx.send(outlet::Frame::End(1)).await;
                    }
                },
                Err(err) => {
                    self.output_tx
                        .send(outlet::Frame::StdErr(err.to_string()))
                        .await;
                    self.output_tx.send(outlet::Frame::End(1)).await;
                }
            }
        } else {
            self.output_tx
                .send(outlet::Frame::StdErr(
                    "Expected TransferFile fulfillment for publish".to_string(),
                ))
                .await;
            self.output_tx.send(outlet::Frame::End(1)).await;
            return;
        }
    }

    async fn exec_set(&self, set: Set) {
        let to = set.point.parent().clone().expect("expect parent");
        let action = Method::Cmd(Rc::Set(set));
        let core = action.into();
        let request = Request::new(core, self.stub.point.clone(), to);
        match self.api.exchange(request).await.ok_or() {
            Ok(response) => {
                    self.output_tx.send(outlet::Frame::End(0)).await;
            },
            Err(err) => {
                self.output_tx
                    .send(outlet::Frame::StdErr(err.to_string()))
                    .await;
                self.output_tx.send(outlet::Frame::End(1)).await;
            }
        }
    }

    async fn exec_get(&self, get: Get) {
        let to = get.point.parent().clone().expect("expect parent");
        let action = Method::Cmd(Rc::Get(get.clone()));
        let core = action.into();
        let request = Request::new(core, self.stub.point.clone(), to);
        match self.api.exchange(request).await.ok_or() {
            Ok(response) => match response.core.body {
                Payload::Bin(bin) => {
                    match String::from_utf8((*bin).clone()) {
                        Ok(text) => {
                            self.output_tx.send(outlet::Frame::StdOut(text)).await;
                            self.output_tx.send(outlet::Frame::End(0)).await;
                        }
                        Err(err) => {
                            self.output_tx
                                .send(outlet::Frame::StdErr(
                                    "Bin File Cannot be displayed on console".to_string(),
                                ))
                                .await;
                            self.output_tx.send(outlet::Frame::End(1)).await;
                        }
                    }
                }
                Payload::Text(text) => {
                    self.output_tx.send(outlet::Frame::StdOut(text)).await;
                    self.output_tx.send(outlet::Frame::End(0)).await;
                }
                Payload::Map(map) => {
                    let mut rtn = String::new();
                    rtn.push_str(get.point.to_string().as_str());
                    rtn.push_str("{ ");
                    for (index, (key, payload)) in map.iter().enumerate() {
                        if let Payload::Text(value) = payload {
                            rtn.push_str(key.as_str());
                            rtn.push_str("=");
                            rtn.push_str(value.as_str());
                            if index != map.len() - 1 {
                                rtn.push_str(", ");
                            }
                        }
                    }
                    rtn.push_str(" }");
                    self.output_tx.send(outlet::Frame::StdOut(rtn)).await;
                    self.output_tx.send(outlet::Frame::End(0)).await;
                }

                _ => {
                    self.output_tx
                        .send(outlet::Frame::StdErr(
                            "unexpected payload response format".to_string(),
                        ))
                        .await;
                    self.output_tx.send(outlet::Frame::End(1)).await;
                }
            },
            Err(err) => {
                self.output_tx
                    .send(outlet::Frame::StdErr(err.to_string()))
                    .await;
                self.output_tx.send(outlet::Frame::End(1)).await;
            }
        }
    }
}
