#[macro_use]
extern crate async_trait;

#[macro_use]
extern crate anyhow;



use std::sync::{Arc, RwLock};
use std::time::Duration;

use anyhow::Error;
use dashmap::DashMap;
use tokio::sync::oneshot;
use uuid::Uuid;


use std::prelude::rust_2021::TryFrom;
use std::ops::Deref;
use std::collections::HashMap;
use tokio::sync::watch::Receiver;
use client::{Request,RequestContext};
use mesh_portal_serde::std_logger;
use mesh_portal_serde::version::latest::http::{HttpRequest, HttpResponse};
use mesh_portal_serde::version::latest::portal::{inlet, outlet};
use mesh_portal_serde::version::latest::resource::Status;
use mesh_portal_serde::version::latest::messaging::{ExchangeId, Exchange};
use mesh_portal_serde::version::latest::config::Info;
use mesh_portal_serde::version::latest::log::Log;
use mesh_portal_serde::version::latest::{portal, entity};
use mesh_portal_serde::version::v0_0_1::util::ConvertFrom;
use std::convert::TryInto;


struct EmptySkel {

}

#[async_trait]
pub trait PortalCtrl: Sync+Send {
    async fn init(&mut self) -> Result<(), Error>
    {
        Ok(())
    }

    async fn handle_request(&self, request: outlet::Request ) -> Result<Option<inlet::Response>,Error> {
        Ok(Option::None)
    }

    async fn handle_response(&self, response: inlet::Response) {
    }
}

pub fn log(message: &str) {
    println!("{}",message);
}

pub trait Inlet: Sync+Send {
    fn send_frame(&self, frame: inlet::Frame);
}

pub trait Outlet: Sync+Send {
    fn receive(&mut self, frame: outlet::Frame);
}

pub struct StatusChamber {
    pub status: Status
}

impl StatusChamber{
    pub fn new( status: Status ) -> Self {
        Self {
            status
        }
    }
}

pub type Exchanges = Arc<DashMap<ExchangeId, oneshot::Sender<outlet::Response>>>;
pub type PortalStatus = Arc<RwLock<StatusChamber>>;


#[derive(Clone)]
pub struct PortalSkel {
    pub info: Info,
    pub inlet: Arc<dyn Inlet>,
    pub logger: fn(message: &str),
    pub exchanges: Exchanges,
    pub status: PortalStatus
}

impl PortalSkel {
    pub fn status(&self) -> Status {
        (*self.status.read().expect("expected status read lock")).status.clone()
    }

    pub fn set_status(&mut self, status: Status) {
        {
            self.status.write().expect("expected to get status write lock").status = status.clone();
        }
        self.inlet.send_frame(inlet::Frame::Status(status));
    }

    pub fn api(&self) -> InletApi {
        InletApi::new( self.info.clone(), self.inlet.clone(), self.exchanges.clone(), std_logger )
    }

}


pub struct Portal {
    pub skel: PortalSkel,
    pub ctrl: Arc<dyn PortalCtrl>,
}

impl Portal {
    pub async fn new(
        info: Info,
        inlet: Box<dyn Inlet>,
        ctrl_factory: Box<dyn Fn(PortalSkel) ->Result<Box<dyn PortalCtrl>,Error>>,
        logger: fn(message: &str)
    ) -> Result<Arc<Portal>, Error> {

        let inlet :Arc<dyn Inlet>= inlet.into();
        let status = Arc::new(RwLock::new(StatusChamber::new( Status::Unknown )));
        inlet.send_frame(inlet::Frame::Status(Status::Unknown ));
        let exchanges = Arc::new(DashMap::new());
        let skel =  PortalSkel {
            info: info.clone(),
            inlet,
            logger,
            exchanges,
            status
        };

        let mut ctrl = ctrl_factory(skel.clone())?;
        ctrl.init().await?;
        let ctrl = ctrl.into();
        let portal = Self {
            skel: skel.clone(),
            ctrl,
        };

        Ok(Arc::new(portal))
    }

    pub fn log( &self, log: Log ) {
        self.skel.inlet.send_frame(inlet::Frame::Log(log));
    }

}

#[async_trait]
impl Outlet for Portal {
    fn receive(&mut self, frame: outlet::Frame) {
        match self.skel.status() {
            Status::Ready => match frame {
                outlet::Frame::CommandEvent(_) => {}
                outlet::Frame::Request(request) => {
                    let ctrl = self.ctrl.clone();
                    let inlet_api = self.skel.api();
                    let ctx = RequestContext::new(self.skel.info.clone(), self.skel.logger );

                    tokio::spawn( async move {

                        async fn handle( ctrl: Arc<dyn PortalCtrl>, inlet_api: InletApi, request: outlet::Request ) -> Result<(),Error> {
                            let exchange = request.exchange.clone();
                            let response = ctrl.handle_request(request).await?;

                            if exchange.requires_response() {
                                if let Some(mut response) = response {
                                    let exchange_id = exchange.try_into()?;
                                    response.exchange = exchange_id;
                                    inlet_api.respond(response);
                                } else {
                                    return Err(anyhow!("response expected"));
                                }
                            }

                            Ok(())
                        }

                        match handle( ctrl, inlet_api, request ).await {
                            Ok(_) => {}
                            Err(err) => {
                                (ctx.logger)(err.to_string().as_str());
                            }
                        }
                    });
                }
                outlet::Frame::Response(response) => {
                    if let Option::Some((_,tx)) =
                        self.skel.exchanges.remove(&response.exchange)
                    {
                        tx.send(response).unwrap_or(());
                    } else {
                        (self.skel.logger)("SEVERE: do not have a matching exchange_id for response");
                    }
                }
                outlet::Frame::Close(_) => {}
                _ => {
                    (self.skel.logger)(format!("SEVERE: frame ignored because status: '{}' does not allow handling of frame '{}'",self.skel.status().to_string(),frame.to_string()).as_str());
                }
            },
            _ => {
                (self.skel.logger)(format!("SEVERE: frame ignored because status: '{}' does not allow handling of frame '{}'",self.skel.status().to_string(),frame.to_string()).as_str());
            }
        }
    }
}


pub struct InletApi {
    info: Info,
    inlet: Arc<dyn Inlet>,
    exchanges: Exchanges,
    logger: fn( log: Log )
}

impl InletApi {
    pub fn new(info: Info, inlet: Arc<dyn Inlet>, exchanges: Exchanges, logger: fn( log: Log ) ) -> Self {
        Self {
            info,
            inlet,
            exchanges,
            logger
        }
    }


    pub fn notify(&self, request: inlet::Request) {
        let request = request.exchange(Exchange::Notification);

        self.inlet.send_frame(inlet::Frame::Request(request));
    }

    pub async fn exchange(
        &mut self,
        request: inlet::Request
    ) -> Result<outlet::Response, Error> {

        let mut request = request;
        let exchange_id: ExchangeId = Uuid::new_v4().to_string();
        let request = request.exchange(Exchange::RequestResponse(exchange_id.clone()));
        let (tx,rx) = oneshot::channel();
        self.exchanges.insert(exchange_id, tx);
        self.inlet.send_frame(inlet::Frame::Request(request));

        let result = tokio::time::timeout(Duration::from_secs(self.info.config.response_timeout.clone()),rx).await;
        Ok(result??)
    }

    pub fn respond( &self, response: inlet::Response ) {
        self.inlet.send_frame( inlet::Frame::Response(response) );
    }
}

pub mod client {
    use std::ops::Deref;
    use anyhow::Error;
    use mesh_portal_serde::version::latest::portal::outlet;
    use mesh_portal_serde::version::latest::id::{Address};
    use mesh_portal_serde::version::latest::config::Info;
    use mesh_portal_serde::version::latest::http::HttpRequest;

    #[derive(Clone)]
    pub struct RequestContext {
        pub portal_info: Info,
        pub logger: fn(message: &str),
    }

    impl RequestContext {
        pub fn new(portal_info: Info, logger: fn(message: &str)) -> Self {
            Self {
                portal_info,
                logger
            }
        }
    }

    pub struct Request<REQUEST> {
        pub context: RequestContext,
        pub from: Address,
        pub request: REQUEST
    }

    impl<REQUEST> Deref for Request<REQUEST> {
        type Target = REQUEST;

        fn deref(&self) -> &Self::Target {
            &self.request
        }
    }
}


pub mod example {
    use std::sync::Arc;

    use anyhow::Error;


    use crate::{InletApi, PortalCtrl, PortalSkel, Request, inlet};
    use std::collections::HashMap;
    use mesh_portal_serde::version::latest::payload::{Payload, Primitive};
    use mesh_portal_serde::version::latest::entity;
    use mesh_portal_serde::version::latest::entity::request::Msg;
    use mesh_portal_serde::version::v0_0_1::generic::payload::PayloadDelivery;

    pub struct HelloCtrl {
        pub skel: Arc<PortalSkel>,
        pub inlet_api: InletApi
    }

    impl HelloCtrl {
        #[allow(dead_code)]
        fn new(skel: Arc<PortalSkel>, inlet_api: InletApi) -> Box<Self> {
            Box::new(Self { skel, inlet_api } )
        }
    }

    #[async_trait]
    impl PortalCtrl for HelloCtrl {

        async fn init(&mut self) -> Result<(), Error> {
            unimplemented!();
            /*
            let mut request =
                inlet::Request::new(entity::request::ReqEntity::Msg( Msg {
                    action: "HelloWorld".to_string(),
                    payload: Payload::Empty,

                    path: "/".to_string()
                }));

            request.to.push(self.inlet_api.info.address.clone());

            let response = self.inlet_api.exchange(request).await?;

            if let entity::response::RespEntity::Ok(Payload::Primitive(Primitive::Text(text))) = response.entity {
                println!("{}",text);
            } else {
                return Err(anyhow!("unexpected signal"));
            }


             */
            Ok(())
        }


    }


}




/*                        match request.entity.clone() {
                            ExtOperation::Http(_) => {
                                if let Exchange::RequestResponse(exchange_id) = &kind
                                {
                                    let result = Request::try_from_http(request, context);
                                    match result {
                                        Ok(request) => {
                                            let path = request.path.clone();
                                            let result = ctrl.http_request(request).await;
                                            match result {
                                                Ok(response) => {
                                                    let response = inlet::Response {
                                                        to: from,
                                                        exchange:exchange_id.clone(),
                                                        entity: ResponseEntity::Ok(Entity::HttpResponse(response))
                                                    };
                                                    inlet_api.respond( response );
                                                }
                                                Err(err) => {
                                                    (skel.logger)(format!("ERROR: HttpRequest.path: '{}' error: '{}' ",  path, err.to_string()).as_str());
                                                    let response = inlet::Response {
                                                        to: from,
                                                        exchange:exchange_id.clone(),
                                                        entity: ResponseEntity::Ok(Entity::HttpResponse(HttpResponse::server_side_error()))
                                                    };
                                                    inlet_api.respond( response );
                                                }
                                            }
                                        }
                                        Err(err) => {
                                            (skel.logger)(format!("FATAL: could not modify HttpRequest into Request<HttpRequest>: {}", err.to_string()).as_str());
                                        }
                                    }
                                } else {
                                    (skel.logger)("FATAL: http request MUST be of ExchangeKind::RequestResponse");
                                }
                            }
                            ExtOperation::Port(port_request) => {
                                match ports.get(&port_request.port ) {
                                    Some(port) => {
                                        let result = Request::try_from_port(request, context );
                                        match result {
                                            Ok(request) => {
                                                let request_from = request.from.clone();
                                                let result = port.request(request).await;
                                                match result {
                                                    Ok(response) => {
                                                        match response {
                                                            Some(signal) => {
                                                                if let Exchange::RequestResponse(exchange_id) = &kind
                                                                {
                                                                   let response = inlet::Response {
                                                                       to: request_from,
                                                                       exchange: exchange_id.clone(),
                                                                       entity: signal
                                                                   };

                                                                   inlet_api.respond(response);
                                                                } else {
                                                                    let message = format!("WARN: PortOperation.port '{}' generated a response to a ExchangeKind::Notification", port_request.port);
                                                                    (skel.logger)(message.as_str());
                                                                }
                                                            }
                                                            None => {
                                                                let message = format!("ERROR: PortOperation.port '{}' generated no response", port_request.port);
                                                                (skel.logger)(message.as_str());
                                                                if let Exchange::RequestResponse(exchange_id) = &kind
                                                                {
                                                                    let response = inlet::Response {
                                                                        to: request_from,
                                                                        exchange: exchange_id.clone(),
                                                                        entity: ResponseEntity::Error(message)
                                                                    };
                                                                    inlet_api.respond(response);
                                                                }
                                                            }
                                                        }
                                                    }
                                                    Err(err) => {
                                                        let message = format!("ERROR: PortOperation.port '{}' message: '{}'", port_request.port, err.to_string());
                                                        (skel.logger)(message.as_str());
                                                        if let Exchange::RequestResponse(exchange_id) = &kind
                                                        {
                                                            let response = inlet::Response {
                                                                to: request_from,
                                                                exchange: exchange_id.clone(),
                                                                entity: ResponseEntity::Error(message)
                                                            };
                                                            inlet_api.respond(response);
                                                        }
                                                    }
                                                }

                                            }
                                            Err(err) => {
                                                let message = format!("FATAL: could not modify PortOperation into Request<PortOperation>: {}", err.to_string());
                                                (skel.logger)(message.as_str());
                                                if let Exchange::RequestResponse(exchange_id) = &kind
                                                {
                                                    let response = inlet::Response {
                                                        to: from,
                                                        exchange: exchange_id.clone(),
                                                        entity: ResponseEntity::Error(message)
                                                    };
                                                    inlet_api.respond(response);
                                                }
                                            }
                                        }

                                    }
                                    None => {
                                        let message =format!("ERROR: message port: '{}' not defined ", port_request.port );
                                        (skel.logger)(message.as_str());
                                        if let Exchange::RequestResponse(exchange_id) = &kind
                                        {
                                            let response = inlet::Response {
                                                to: from,
                                                exchange: exchange_id.clone(),
                                                entity: ResponseEntity::Error(message)
                                            };
                                            inlet_api.respond(response);
                                        }
                                    }
                                }
                            }
                        }*/