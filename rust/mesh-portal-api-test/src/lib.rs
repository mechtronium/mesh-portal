
#[macro_use]
extern crate async_trait;

#[macro_use]
extern crate anyhow;

#[macro_use]
extern crate lazy_static;


#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::convert::TryInto;
    use std::io::Write;
    use std::str::FromStr;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicU32, Ordering, AtomicUsize};
    use std::thread;

    use anyhow::Error;
    use tokio::io;
    use tokio::io::AsyncWriteExt;
    use tokio::net::TcpStream;
    use tokio::runtime::{Builder, Runtime};
    use tokio::sync::{mpsc, oneshot, broadcast};
    use tokio::sync::broadcast::Receiver;
    use tokio::sync::mpsc::Sender;
    use tokio::sync::oneshot::error::RecvError;
    use tokio::time::Duration;

    use mesh_portal_api_client::{client, InletApi, ResourceCtrl, PortalSkel, ResourceCtrlFactory, ResourceSkel};
    use mesh_portal_api_server::{MuxCall, Portal, PortalMuxer, Router, PortalRequestHandler, DefaultPortalRequestHandler};
    use mesh_portal_serde::mesh;
    use mesh_portal_serde::version::latest::entity::request::{Msg, Rc, ReqEntity};
    use mesh_portal_serde::version::latest::entity::response;
    use mesh_portal_serde::version::latest::id::{Address, Kind};
    use mesh_portal_serde::version::latest::messaging::Exchange;
    use mesh_portal_serde::version::latest::payload::{Payload, Primitive, PrimitiveType};
    use mesh_portal_serde::version::latest::portal::{inlet, outlet};
    use mesh_portal_serde::version::latest::resource::Status;
    use mesh_portal_serde::version::latest::resource::ResourceStub;
    use mesh_portal_serde::version::latest::payload::{RcCommand, PrimitiveList};
    use mesh_portal_serde::version::latest::resource::Archetype;
    use mesh_portal_tcp_client::{PortalClient, PortalTcpClient};
    use mesh_portal_tcp_common::{
        FrameReader, FrameWriter, PrimitiveFrameReader, PrimitiveFrameWriter,
    };
    use mesh_portal_tcp_server::{Call, Event, PortalServer, PortalTcpServer};
    use mesh_portal_api::message;
    use mesh_portal_serde::version::v0_0_1::config::{Config, ResourceConfigBody, Assign, ConfigBody};
    use mesh_portal_serde::version::latest::resource::command::select::Select;
    use mesh_portal_serde::version::latest::pattern::AddressKindPattern;
    use mesh_portal_serde::version::v0_0_1::artifact::{ArtifactRequest, ArtifactResponse, Artifact};
    use mesh_portal_serde::version::latest::util::unique_id;
    use mesh_portal_serde::version::latest::entity::response::RespEntity;
    use mesh_portal_serde::version::latest::generic::resource::command::select::{SelectIntoPayload, SelectionKind};
    use mesh_portal_serde::version::latest::generic::id::KindParts;
    use mesh_portal_serde::mesh::Response;

    lazy_static! {
    static ref GLOBAL_TX : tokio::sync::broadcast::Sender<GlobalEvent> = {
        tokio::sync::broadcast::channel(128).0
    };
}

    #[derive(Clone)]
    pub enum GlobalEvent {
        Progress(String),
        Finished(String),
        Fail(String),
        Shutdown
    }

    #[test]
    fn test() -> Result<(), Error> {
        let runtime = Builder::new_multi_thread().enable_all().max_blocking_threads(4).build()?;
        runtime.block_on( async { server_up().await } )
    }

    async fn server_up() -> Result<(), Error> {
        let port = 32355;
        let server = PortalTcpServer::new(port, Box::new(TestPortalServer::new()));
        let (shutdown_tx, shutdown_rx) = tokio::sync::oneshot::channel();

        {
            let server = server.clone();
            tokio::spawn(async move {
                let mut finished_count = 0;
                let mut GLOBAL_RX = GLOBAL_TX.subscribe();
                while let Result::Ok(event) = GLOBAL_RX.recv().await {
                    match event {
                        GlobalEvent::Finished(_) => {
                            finished_count = finished_count+1;
                        }
                        GlobalEvent::Fail(_) => {
                            finished_count = finished_count+1;
                        }
                        GlobalEvent::Shutdown => {
                            server.send( Call::Shutdown ).await.unwrap_or_default();
                            return;
                        }
                        GlobalEvent::Progress(progress) => {
                            println!("Progress: {}",progress );
                        }
                    }
                    if finished_count >= 2 {
                       server.send( Call::Shutdown ).await.unwrap_or_default();
                       return;
                    }
                }
            });
        }

        {
            let server = server.clone();
            tokio::spawn(async move {
                let (listen_tx, listen_rx) = tokio::sync::oneshot::channel();
                server.send(Call::ListenEvents(listen_tx)).await;
                let mut broadcast_rx = listen_rx.await.unwrap();
                while let Result::Ok(event) = broadcast_rx.recv().await {
                    if let Event::Status(status) = &event {
                        println!("event: Status({})", status.to_string());
                    } else {
                        println!("event: {}", event.to_string());
                    }
                    match event {
                        // fix this: it should not be Unknown (but Done isn't working)
                        Event::Status(Status::Unknown) => {
                            shutdown_tx.send(());
                            return;
                        }
                        Event::Status(Status::Panic(error)) => {
                            eprintln!("PANIC: {}", error);
                            shutdown_tx.send(());
                            return;
                        }

                        _ => {}
                    }
                }
            });
        }

        let client1 = Box::new(TestPortalClient::new("scott".to_string()));
        let client2 = Box::new(TestPortalClient::new("fred".to_string()));

        let client1 = PortalTcpClient::new(format!("localhost:{}", port), client1).await?;
        let client2 = PortalTcpClient::new(format!("localhost:{}", port), client2).await?;

        tokio::spawn( async move {
            tokio::time::sleep( Duration::from_secs( 2 ) ).await;
            GLOBAL_TX.send( GlobalEvent::Shutdown ).unwrap_or_default();
        });

        shutdown_rx.await;

        println!("got to the end...");
        Ok(())
    }

    pub struct TestRouter {}

    impl Router for TestRouter {
        fn route(&self, message: message::Message) {
            todo!()
        }
    }

    pub struct TestPortalServer {
        pub atomic: AtomicU32,
        pub request_handler: Arc<dyn PortalRequestHandler>
    }

    impl TestPortalServer {
        pub fn new() -> Self {
            Self {
                atomic: AtomicU32::new(0),
                request_handler: Arc::new(TestPortalRequestHandler::new() )
            }
        }
    }

    fn test_logger(message: &str) {
        println!("{}", message);
    }

    #[async_trait]
    impl PortalServer for TestPortalServer {
        fn flavor(&self) -> String {
            "test".to_string()
        }

        async fn auth(
            &self,
            reader: &mut PrimitiveFrameReader,
            writer: &mut PrimitiveFrameWriter,
        ) -> Result<String, anyhow::Error> {
            let username = reader.read_string().await?;
            tokio::time::sleep(Duration::from_secs(0)).await;
            Ok(username)
        }

        /*
        async fn info(&self, user: String) -> Result<Info, anyhow::Error> {
            let index = self.atomic.fetch_add(1, Ordering::Relaxed);
            let key = format!("({})", index);
            let address = Address::from_str(format!("parent:portal-{}", index).as_str() )?;

            let info = Info {
                address: address.clone(),
                owner: user,
                parent: Address::from_str("parent")?,
                archetype: Archetype {
                    kind: Kind::new( "Portal".to_string(), Option::None, Option::None ),
                    properties: Default::default()
                },
                config: Default::default(),
                ext_config: None,
                kind: PortalKind::Portal,
            };

            Ok(info)
        }
         */

        fn logger(&self) -> fn(&str) {
            test_logger
        }

        fn router_factory(&self, mux_tx: Sender<MuxCall>) -> Box<dyn Router> {
            Box::new(InYourFaceRouter { mux_tx })
        }

        fn portal_request_handler(&self) -> Arc<dyn PortalRequestHandler> {
            self.request_handler.clone()
        }
    }

    #[derive(Debug)]
    pub struct TestPortalRequestHandler {
       seq: AtomicUsize
    }

    impl TestPortalRequestHandler {
        pub fn new()-> Self {
            TestPortalRequestHandler{
                seq: AtomicUsize::new(0)
            }
        }
    }

    #[async_trait]
    impl PortalRequestHandler for TestPortalRequestHandler {
        async fn default_assign(&self) -> Result<Assign, Error> {
            let index = self.seq.fetch_add(1, Ordering::Relaxed );
            let address = Address::from_str( format!("space:resource-{}",index).as_str() )?;
            let config = Config{
                address: Address::from_str("space:resource:config:/friendly.config")?,
                body: ResourceConfigBody::Control
            };
            let stub = ResourceStub {
                address,
                kind: KindParts::from_str("Control")?,
                properties: Default::default(),
                status: Status::Unknown
            };
            let assign = Assign {
                config,
                stub
            };
            Ok(assign)
        }
    }

    pub struct InYourFaceRouter {
        mux_tx: Sender<MuxCall>,
    }
    impl Router for InYourFaceRouter {
        fn route(&self, message: message::Message) {

println!("InYourFace: message.to: {}",message.to().to_string());
            let mux_tx = self.mux_tx.clone();
            tokio::spawn(async move {
               match message.clone() {
                    message::Message::Request(request) => {
                        match &request.entity{
                            ReqEntity::Rc(rc) => {
                                match &rc.command{
                                    RcCommand::Select(select) => {
                                        let (tx,mut rx) = oneshot::channel();
                                        mux_tx.send(MuxCall::SelectAll(tx)).await;
                                        match rx.await {
                                            Ok(stubs) => {

                                                println!("Stub count: {}", stubs.len());
                                                let stubs = stubs.into_iter().map(|stub| Primitive::Stub(stub)).collect();

                                                let list = PrimitiveList{
                                                    primitive_type: PrimitiveType::Stub,
                                                    list: stubs
                                                };

                                                let exchange_id = match request.exchange {
                                                    Exchange::Notification => {
                                                        unique_id()
                                                    }
                                                    Exchange::RequestResponse(exchange_id) => exchange_id
                                                };

                                                let response = Response{
                                                    id: unique_id(),
                                                    to: request.from.clone(),
                                                    from: request.to.clone(),
                                                    entity: RespEntity::Ok(Payload::List(list)),
                                                    exchange: exchange_id
                                                };

                                                mux_tx.send( MuxCall::MessageOut(message::Message::Response(response))).await;
                                            },
                                            Err(err) => {
                                                GLOBAL_TX.send( GlobalEvent::Fail(err.to_string()));
                                            }
                                        }
                                    }
                                    _ => {
                                        GLOBAL_TX.send( GlobalEvent::Fail("Primitive router cannot handle Rc commands other than Select".into()));
                                    }
                                }
                            }

                            _ => {
                                mux_tx.send( MuxCall::MessageIn(message.clone()) ).await;
                            }
                        }
                    }
                    message::Message::Response(response) => {
                        // since we are not connected to a mesh all inbound messages are just sent back to the outbound
                        mux_tx.send(MuxCall::MessageOut(message::Message::Response(response))).await;
                    }
                }

            });
        }
    }
    pub struct TestPortalClient {
        pub user: String,
    }

    impl TestPortalClient {
        pub fn new(user: String) -> Self {
            Self { user}
        }
    }

    #[async_trait]
    impl PortalClient for TestPortalClient {
        fn flavor(&self) -> String {
            return "test".to_string();
        }

        async fn auth(
            &self,
            reader: &mut PrimitiveFrameReader,
            writer: &mut PrimitiveFrameWriter,
        ) -> Result<(), Error> {
            writer.write_string(self.user.clone()).await?;
            Ok(())
        }


        fn resource_ctrl_factory(&self) ->Arc<dyn ResourceCtrlFactory> {
            Arc::new(FriendlyResourceCtrlFactory{})
        }

        fn logger(&self) -> fn(m: &str) {
            fn logger(message: &str) {
                println!("{}", message);
            }
            logger
        }
    }

    pub struct FriendlyResourceCtrlFactory {}

    impl ResourceCtrlFactory for FriendlyResourceCtrlFactory {
        fn matches(&self, config: Config<ResourceConfigBody>) -> bool {
            true
        }

        fn create(&self, skel: ResourceSkel) -> Result<Arc<dyn ResourceCtrl>, Error> {
            GLOBAL_TX.send(GlobalEvent::Progress("creating FriendlyResourceCtrl".to_string()));

            Ok(Arc::new(FriendlyResourceCtrl{
                skel
            }))
        }
    }

    pub struct FriendlyResourceCtrl {
        pub skel: ResourceSkel
    }

    #[async_trait]
    impl ResourceCtrl for FriendlyResourceCtrl {
        async fn init(&self) -> Result<(), Error> {
            GLOBAL_TX.send(GlobalEvent::Progress("FriendlyResourceCtrl.init()".to_string()));
            // wait just a bit to make sure everyone got chance to be in the muxer
            tokio::time::sleep(Duration::from_millis(50)).await;

            let mut request = inlet::Request::new(ReqEntity::Rc(Rc {
                command: RcCommand::Select(Select{
                    pattern: AddressKindPattern::from_str("**")?,
                    properties: Default::default(),
                    into_payload: SelectIntoPayload::Stubs,
                    kind: SelectionKind::Initial
                }),

                payload: Payload::Empty
            }),
            self.skel.stub.address.clone());
            request.to.push(self.skel.stub.address.parent().expect("expected a parent"));


println!("FriendlyPortalCtrl::exchange... from: {} to: {}", self.skel.stub.address.to_string(), self.skel.stub.address.parent().expect("expected a parent").to_string() );
            match self.skel.portal.api().exchange(request).await {
                Ok(response) => match response.entity {
                    response::RespEntity::Ok(Payload::List(resources)) => {
println!("FriendlyPortalCtrl::Ok");
                        for resource in resources.iter() {
                            if let Primitive::Stub(resource) = resource {
                                if resource.address != self.skel.stub.address {
                                    (self.skel.portal.logger)(format!(
                                        "INFO: found resource: {}",
                                        resource.address.to_string()
                                    ).as_str());
                                    let mut request = inlet::Request::new(ReqEntity::Msg(
                                        Msg {
                                            action: "Greet".to_string(),
                                            path: "/".to_string(),
                                            payload: Payload::Primitive(Primitive::Text(format!(
                                                "Hello, my name is '{}'",
                                                self.skel.stub.address.to_string()
                                            ))),
                                        }),
                                        self.skel.stub.address.clone()
                                    );
                                    let result = self.skel.portal.api().exchange(request).await;
                                    match result {
                                        Ok(response) => {
                                            match &response.entity {
                                                response::RespEntity::Ok(Payload::Primitive(Primitive::Text(response))) => {
                                                    println!("got response: {}", response);
                                                    GLOBAL_TX.send(GlobalEvent::Finished(self.skel.stub.address.to_string()));
                                                }
                                                _ => {
                                                    GLOBAL_TX.send(GlobalEvent::Fail(self.skel.stub.address.to_string()));
                                                }
                                            }
                                        }
                                        Err(_) => {
                                            GLOBAL_TX.send(GlobalEvent::Fail(self.skel.stub.address.to_string()));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => (self.skel.logger)("ERROR: unexpected response!"),
                },
                Err(_) => {}
            }

            Ok(())
        }

        /*
        fn ports(&self) -> HashMap<String,Box<dyn PortCtrl>> {

            struct GreetPort {
                skel: PortalSkel
            }

            #[async_trait]
            impl PortCtrl for GreetPort {
                async fn request( &self, request: outlet::Request ) -> Result<Option<response::RespEntity>,Error>{
                    match &request.entity {
                        ReqEntity::Msg(Msg { path: _, action:_, payload: delivery } ) => Ok(Option::Some(response::RespEntity::Ok(
                            PayloadDelivery::Payload(Payload::Primitive(Primitive::Text("Hello, <username>".to_string()))),
                        ))),
                        _ => Err(anyhow!("unexpected request entity")),
                    }
                }
            }

            impl GreetPort {
                pub fn new( skel: PortalSkel ) -> Self {
                    Self {
                        skel
                    }
                }
            }

            let mut ports = HashMap::new();
            let port : Box<dyn PortCtrl> = Box::new(GreetPort::new(self.skel.clone()));
            ports.insert( "greet".to_string(), port );
            ports
        }

         */

        /*
        fn ports( &self, ) -> HashMap< String, fn( request: client::Request<PortOperation>, ) -> Result<Option<ResponseEntity>, Error>> {

             fn greet( request: client::Request<PortOperation>, ) -> Result<Option<ResponseEntity>, Error> {
                match &request.entity {
                    Entity::Payload(Payload::Text(text)) => Ok(Option::Some(ResponseEntity::Ok(
                        Entity::Payload(Payload::Text("Hello, <username>".to_string())),
                    ))),
                    _ => Err(anyhow!("unexpected request entity")),
                }
            }

            let mut ports = HashMap::new();
            ports.insert("greet".to_string(), greet );
            ports
        }

         */
    }
}
