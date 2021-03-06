#[macro_use]
extern crate async_trait;

#[macro_use]
extern crate anyhow;



use std::sync::{Arc, RwLock};
use std::time::Duration;

use anyhow::Error;
use dashmap::DashMap;
use tokio::sync::{oneshot, mpsc};
use uuid::Uuid;


use std::prelude::rust_2021::TryFrom;
use std::ops::Deref;
use std::collections::HashMap;
use tokio::sync::watch::Receiver;
use mesh_portal_serde::std_logger;
use mesh_portal_serde::version::latest::http::{HttpRequest, HttpResponse};
use mesh_portal_serde::version::latest::portal::{Exchanger, inlet, outlet};
use mesh_portal_serde::version::latest::resource::{ResourceStub, Status};
use mesh_portal_serde::version::latest::log::Log;
use mesh_portal_serde::version::latest::{portal, entity};
use std::convert::TryInto;
use dashmap::mapref::one::Ref;
use tokio::sync::oneshot::Sender;
use tokio::task::yield_now;
use mesh_portal_serde::version::latest::config::{Config, PortalConfig, ResourceConfigBody};
use mesh_portal_serde::version::latest::id::Address;
use mesh_portal_serde::version::latest::messaging::{Exchange, ExchangeId, Request, Response};
use mesh_portal_serde::version::latest::portal::inlet::AssignRequest;
use mesh_portal_serde::version::latest::portal::outlet::Frame;


#[derive(Clone)]
pub struct ResourceSkel {
  pub portal: PortalSkel,
  pub stub: ResourceStub,
  pub logger: fn(message: &str),
}

pub trait ResourceCtrlFactory: Sync+Send {
    fn matches(&self,config:Config<ResourceConfigBody>) -> bool;
    fn create(&self, skel: ResourceSkel) -> Result<Arc<dyn ResourceCtrl>, Error>;
}

#[async_trait]
pub trait ResourceCtrl: Sync+Send {
    async fn init(&self) -> Result<(), Error>
    {
        Ok(())
    }

    async fn outlet_frame(&self, frame: outlet::Frame ) -> Result<Option<Response>,Error> {
        Ok(Option::None)
    }
}

pub fn log(message: &str) {
    println!("{}",message);
}

pub trait Inlet: Sync+Send {
    fn inlet_frame(&self, frame: inlet::Frame);
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

pub type Exchanges = Arc<DashMap<ExchangeId, oneshot::Sender<Response>>>;



#[derive(Clone)]
pub struct PortalSkel {
    pub config: PortalConfig,
    pub inlet: Arc<dyn Inlet>,
    pub logger: fn(message: &str),
    pub exchanges: Exchanges,
    pub assign_exchange: Arc<DashMap<String, oneshot::Sender<Arc<dyn ResourceCtrl>>>>,
    pub tx: mpsc::Sender<outlet::Frame>,
    pub ctrl_factory: Arc<dyn ResourceCtrlFactory>,
}


impl PortalSkel {

    pub fn api(&self) -> InletApi {
        InletApi::new( self.config.clone(), self.inlet.clone(), self.exchanges.clone(), std_logger )
    }

}


pub enum ResourceCommand {
    Add{address: Address, resource: Arc<dyn ResourceCtrl> },
    Remove(Address),
    None
}


pub struct Portal {
    pub skel: PortalSkel,
}

impl Portal {
    pub async fn new(
        config: PortalConfig,
        inlet: Box<dyn Inlet>,
        outlet_tx: mpsc::Sender<outlet::Frame>,
        mut outlet_rx: mpsc::Receiver<outlet::Frame>,
        ctrl_factory: Arc<dyn ResourceCtrlFactory>,
        logger: fn(message: &str)
    ) -> Result<Arc<Portal>, Error> {

        let inlet :Arc<dyn Inlet>= inlet.into();
        let exchanges = Arc::new(DashMap::new());
        let assign_exchange = Arc::new(DashMap::new());
        let skel =  PortalSkel {
            config,
            inlet,
            logger,
            exchanges,
            assign_exchange,
            tx: outlet_tx,
            ctrl_factory,
        };

        {
println!("NEW PORTAL!" );
            let skel = skel.clone();
            tokio::spawn(async move {
                let mut resources = HashMap::new();
println!("portal listening...");
                while let Option::Some(frame) = outlet_rx.recv().await {
println!("Portal received frame: {}", frame.to_string());
                    if let Frame::Close(_) = frame {
                        println!("XXX>>> Client exiting outlet_rx loop");
                        break;
                    }
                    process(skel.clone(), &mut resources, frame).await;

                        async fn process( skel: PortalSkel,  resources: &mut HashMap<Address,Arc<dyn ResourceCtrl>>, frame: outlet::Frame ) -> Result<(),Error> {
println!("CLIENT PROCESS");
                            if let Frame::Assign(assign) = &frame {
                                let resource_skel = ResourceSkel {
                                    portal: skel.clone(),
                                    stub: assign.stub.clone(),
                                    logger: skel.logger,
                                };
                                let resource = skel.ctrl_factory.create(resource_skel)?;
                                resources.insert( assign.stub.address.clone(), resource.clone() );
                                let assign = assign.clone();
                                tokio::spawn( async move {
println!("CLIENT INIT");
                                    resource.init().await;
                                    match skel.assign_exchange.remove( &assign.id ) {
                                        None => {}
                                        Some((_,tx)) => {
                                            tx.send( resource );
                                        }
                                    }
println!("CLIENT INIT COMPLETE");
                                });


println!("CLIENT RTN OK");
                                return Ok(());
                            }

                            if let Frame::Response(response)= &frame {
println!("RECEIVE RESPONSE");
                                if let Option::Some((id,tx)) = skel.exchanges.remove(&response.response_to) {
println!("... EXchange Id {}", id);
                                    tx.send(response.clone());
                                    return Ok(())
                                }
                            } else {
                                eprintln!("could not find exchange for response: ")
                            }
println!("Client outlet::Frame{}",frame.to_string());
                            let to = frame.to().ok_or::<Error>(anyhow!("expected frame to have a to"))?;
                            let resource = resources.get(&to).ok_or(anyhow!("expected to find resource for address"))?;

                            println!("SSS SEnding to outlet_frame....");
                            if let Option::Some(response) = resource.outlet_frame(frame).await? {
                                println!("GGG GOT a response !");
                                skel.inlet.inlet_frame(inlet::Frame::Response(response));
                            }

                            Ok(())
                        }


                }



            });
        }

        let portal = Self {
            skel: skel.clone(),
        };

        Ok(Arc::new(portal))
    }

    pub fn log( &self, log: Log ) {
        self.skel.inlet.inlet_frame(inlet::Frame::Log(log));
    }

    pub async fn request_assign( &self, request: AssignRequest) -> Result<Arc<dyn ResourceCtrl>,Error> {
       let (tx,rx) = oneshot::channel();
       let request = Exchanger::new(request);
       self.skel.assign_exchange.insert( request.id.clone(), tx );
       self.skel.inlet.inlet_frame(inlet::Frame::AssignRequest(request) );
       Ok(rx.await?)
    }
}

#[async_trait]
impl Outlet for Portal {
    fn receive(&mut self, frame: outlet::Frame) {
        self.skel.tx.send( frame );
    }
}


pub struct InletApi {
    config: PortalConfig,
    inlet: Arc<dyn Inlet>,
    exchanges: Exchanges,
    logger: fn( log: Log )
}

impl InletApi {
    pub fn new(config: PortalConfig, inlet: Arc<dyn Inlet>, exchanges: Exchanges, logger: fn( log: Log ) ) -> Self {
        Self {
            config,
            inlet,
            exchanges,
            logger
        }
    }


    pub fn notify(&self, request: Request) {
        self.inlet.inlet_frame(inlet::Frame::Request(request));
    }

    pub async fn exchange(
        &mut self,
        request: Request
    ) -> Result<Response, Error> {

        let (tx,rx) = oneshot::channel();
        self.exchanges.insert(request.id.clone(), tx);
        self.inlet.inlet_frame(inlet::Frame::Request(request));

        let result = tokio::time::timeout(Duration::from_secs(self.config.response_timeout.clone()),rx).await;
        Ok(result??)
    }

    pub fn send_response(&self, response: Response ) {
        self.inlet.inlet_frame( inlet::Frame::Response(response) );
    }
}

pub mod client {
    use std::ops::Deref;
    use anyhow::Error;
    use mesh_portal_serde::version::latest::portal::outlet;
    use mesh_portal_serde::version::latest::id::{Address};
    use mesh_portal_serde::version::latest::http::HttpRequest;

    /*
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

     */
}


pub mod example {
    use std::sync::Arc;

    use anyhow::Error;


    use crate::{InletApi, ResourceCtrl, PortalSkel, inlet};
    use std::collections::HashMap;
    use mesh_portal_serde::version::latest::payload::{Payload, Primitive};
    use mesh_portal_serde::version::latest::entity;
    use mesh_portal_serde::version::latest::entity::request::Msg;

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
    impl ResourceCtrl for HelloCtrl {

        async fn init(&self) -> Result<(), Error> {
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