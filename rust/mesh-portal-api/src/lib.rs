use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use tokio::sync::mpsc;
use tokio::sync::mpsc::error::SendError;

use mesh_portal_serde::version::latest::error::Error;
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
    use mesh_portal_serde::version::latest::id::Identifier;

    pub type Message = generic::Message<ReqEntity,Identifier>;

    pub mod generic {
        use serde::{Serialize,Deserialize};
        use mesh_portal_serde::mesh::generic::{Request, Response};

        #[derive(Clone,Serialize,Deserialize)]
        pub enum Message<OPERATION,ID> {
            Request(Request<OPERATION,ID>),
            Response(Response<ID>)
        }

        impl<OPERATION,ID> Message<OPERATION,ID> where ID: Clone {
            pub fn to(&self) -> ID {
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

        impl<OPERATION,ID> From<Request<OPERATION,ID>> for Message<OPERATION,ID> {
            fn from(request: Request<OPERATION, ID>) -> Self {
                Self::Request(request)
            }
        }

        impl<OPERATION,ID> From<Response<ID>> for Message<OPERATION,ID> {
            fn from(response: Response<ID>) -> Self {
                Self::Response(response)
            }
        }

    }

}
