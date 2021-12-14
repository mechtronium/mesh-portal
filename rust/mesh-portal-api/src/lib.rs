use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use tokio::sync::mpsc;
use tokio::sync::mpsc::error::SendError;

use mesh_portal_serde::error::Error;
use mesh_portal_serde::version::v0_0_1::util::ConvertFrom;
use mesh_portal_serde::version::latest::portal::inlet;

pub fn converter<From,To>(handle_error:fn (error:Error) ) -> (mpsc::Sender<From>, mpsc::Receiver<To>) where
    From: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ TryInto<To,Error=Error> + 'static,
    To: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync + 'static,
{
    let (from_tx,mut from_rx) : (mpsc::Sender<From>,mpsc::Receiver<From>)= mpsc::channel(128);
    let (to_tx,mut to_rx) : (mpsc::Sender<To>,mpsc::Receiver<To>)= mpsc::channel(128);
    tokio::spawn ( async move {
        while let Option::Some(from) = from_rx.recv().await{
            let to : Result<To,Error> = from.try_into();
            match to {
                Ok(to) => {
                    match to_tx.send(to).await {
                        Err(err) => {
                            let err = Error { message : err.to_string() };
                            handle_error(err);
                            break;
                        }
                        _ => {}
                    }
                }
                Err(err) => {
                    handle_error(err.into())
                }
            }
        }
    });

    (from_tx,to_rx)
}




pub mod message {
    use mesh_portal_serde::version::latest::entity::request::ReqEntity;
    use mesh_portal_serde::version::latest::entity::response::RespEntity;
    use mesh_portal_serde::version::latest::id::Address;

    pub type Message = generic::Message<ReqEntity>;

    pub mod generic {
        use serde::{Serialize,Deserialize};
        use mesh_portal_serde::mesh::generic::{Request, Response};
        use mesh_portal_serde::version::latest::id::Address;

        #[derive(Clone,Serialize,Deserialize)]
        pub enum Message<ReqEntity> {
            Request(Request<ReqEntity>),
            Response(Response)
        }

        impl<ReqEntity> Message<ReqEntity>{
            pub fn to(&self) -> Address {
                match self {
                    Message::Request(request) => {
                        request.to.clone()
                    }
                    Message::Response(response) => {
                        response.to.clone()
                    }
                }
            }
        }

        impl<ReqEntity> From<Request<ReqEntity>> for Message<ReqEntity> {
            fn from(request: Request<ReqEntity>) -> Self {
                Self::Request(request)
            }
        }

        impl<ReqEntity> From<Response> for Message<ReqEntity> {
            fn from(response: Response) -> Self {
                Self::Response(response)
            }
        }

    }

}


#[cfg(test)]
pub mod test {
    #[test]
    pub fn test(){

    }
}