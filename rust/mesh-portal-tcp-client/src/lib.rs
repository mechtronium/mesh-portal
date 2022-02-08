#[macro_use]
extern crate async_trait;

#[macro_use]
extern crate anyhow;

use mesh_portal_tcp_common::{PrimitiveFrameReader, PrimitiveFrameWriter, FrameWriter, FrameReader, PortalAuth};
use anyhow::Error;
use mesh_portal_api_client::{Portal, ResourceCtrl, PortalSkel, InletApi, Inlet, ResourceCtrlFactory};
use std::sync::Arc;
use tokio::net::TcpStream;
use tokio::sync::mpsc;
use mesh_portal_serde::version::latest::portal;
use mesh_portal_serde::version::latest::log::Log;
use tokio::sync::mpsc::error::TrySendError;
use tokio::task::yield_now;
use mesh_portal_serde::version;
use tokio::time::Duration;
use mesh_portal_serde::version::latest::portal::{outlet, inlet, Exchanger, initin, initout};
use mesh_portal_serde::version::latest::portal::inlet::AssignRequest;

pub struct PortalTcpClient {
    pub host: String,
    pub portal: Arc<Portal>
}

impl PortalTcpClient {

    pub async fn new( host: String, mut client: Box<dyn PortalClient> ) -> Result<Self,Error> {
        let stream = TcpStream::connect(host.clone()).await?;

        let (reader,writer) = stream.into_split();
        let mut reader = PrimitiveFrameReader::new(reader);
        let mut writer = PrimitiveFrameWriter::new(writer);

        let mut reader : FrameReader<initout::Frame> = FrameReader::new(reader );
        let mut writer : FrameWriter<initin::Frame>  = FrameWriter::new(writer );

        writer.write(initin::Frame::Flavor(client.flavor())).await?;

        if let initout::Frame::Ok = reader.read().await? {

        } else {
            let message = format!("AUTH FAILED: {}",result);
            (client.logger())(message.as_str());
            return Err(anyhow!(message));
        }

        let auth = client.auth();
        writer.write( initin::Frame::Auth(auth)).await?;

        if let initout::Frame::Ok = reader.read().await? {

        } else {
            let message = format!("AUTH FAILED: {}",result);
            (client.logger())(message.as_str());
            return Err(anyhow!(message));
        }

        let factory = client.init( &mut reader, &mut writer ).await?;

        let mut reader : FrameReader<outlet::Frame> = FrameReader::new(reader.done() );
        let mut writer : FrameWriter<inlet::Frame>  = FrameWriter::new(writer.done() );

        let (inlet_tx, mut inlet_rx) = mpsc::channel(1024 );
        let (outlet_tx, mut outlet_rx) = mpsc::channel(1024 );

        {
            let logger = client.logger();
            tokio::spawn(async move {
                while let Option::Some(frame) = inlet_rx.recv().await {
                    match writer.write(frame).await {
                        Ok(_) => {}
                        Err(err) => {
                            (logger)("FATAL: writer disconnected");
                            break;
                        }
                    }
                    yield_now().await;
                }
            });
        }

        let inlet = Box::new(TcpInlet{
          sender: inlet_tx,
          logger: client.logger()
        });

        let portal = Portal::new(Default::default(), inlet, outlet_tx.clone(), outlet_rx, factory, client.logger()).await?;
        {
            let logger = client.logger();
            tokio::spawn(async move {
                while let Result::Ok(frame) = reader.read().await {
println!("reading frame: {}",frame.to_string());
                    match outlet_tx.send( frame ).await {
                        Result::Ok(_) => {

                        }
                        Result::Err(err) => {
                            (logger)("FATAL: reader disconnected");
                            break;
                        }
                    }
                    yield_now().await;
                }
            });
        }

        return Ok(Self {
            host,
            portal
        });

    }

    pub async fn request_assign( &self, request: AssignRequest ) -> Result<Arc<dyn ResourceCtrl>,Error> {
        self.portal.request_assign(request).await
    }
}

#[async_trait]
pub trait PortalClient: Send+Sync {
    fn flavor(&self) -> String;
    fn auth( &self ) -> PortalAuth;
    fn logger(&self) -> fn(message: &str);

    async fn init( &self, reader: & mut FrameReader<initout::Frame>, writer: & mut FrameWriter<initin::Frame> ) -> Result<Arc< dyn ResourceCtrlFactory >,Error>;

}

struct TcpInlet {
    pub sender: mpsc::Sender<inlet::Frame>,
    pub logger: fn( message: &str )
}

impl Inlet for TcpInlet {
    fn inlet_frame(&self, frame: inlet::Frame) {
        let sender = self.sender.clone();
        let logger = self.logger;
        tokio::spawn(async move {
println!("Sending FRAME via inlet api...{}", frame.to_string());
            match sender.send(frame).await
            {
                Ok(_) => {
                    println!("SENT FRAME via inlet!");
                }
                Err(err) => {
                    (logger)(format!("ERROR: frame failed to send to client inlet").as_str())
                }
            }
        });
    }
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
