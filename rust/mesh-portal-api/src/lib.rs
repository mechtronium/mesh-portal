use tokio::sync::mpsc;
use mesh_portal_serde::version::latest::generic::portal::inlet;
use mesh_portal_serde::version::latest::generic::portal::outlet;
use mesh_portal_serde::version::latest::error::Error;
use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;
use std::convert::{TryInto, TryFrom};
use tokio::sync::mpsc::error::SendError;
use serde::{Serialize,Deserialize};


pub fn inlet_converter<FROM_KEY,FROM_ADDRESS,FROM_KIND,FROM_RESOURCE_TYPE,TO_KEY,TO_ADDRESS,TO_KIND,TO_RESOURCE_TYPE>(handle_error:fn (error:Error) ) -> (mpsc::Sender<inlet::Frame<FROM_KEY,FROM_ADDRESS,FROM_KIND,FROM_RESOURCE_TYPE>>, mpsc::Receiver<inlet::Frame<TO_KEY,TO_ADDRESS,TO_KIND,TO_RESOURCE_TYPE>>) where
        FROM_KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ 'static,
        FROM_ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync + 'static,
        FROM_KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync + 'static,
        FROM_RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync + 'static,
        TO_KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ TryFrom<FROM_KEY,Error=Error>  + 'static,
        TO_ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ TryFrom<FROM_ADDRESS,Error=Error> + 'static,
        TO_KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ TryFrom<FROM_KIND,Error=Error>  + 'static,
        TO_RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ TryFrom<FROM_RESOURCE_TYPE,Error=Error> + 'static

{
    let (from_tx,mut from_rx) : (mpsc::Sender<inlet::Frame<FROM_KEY,FROM_ADDRESS,FROM_KIND,FROM_RESOURCE_TYPE>>,mpsc::Receiver<inlet::Frame<FROM_KEY,FROM_ADDRESS,FROM_KIND,FROM_RESOURCE_TYPE>>)= mpsc::channel(128);
    let (to_tx,mut to_rx) : (mpsc::Sender<inlet::Frame<TO_KEY,TO_ADDRESS,TO_KIND,TO_RESOURCE_TYPE>>,mpsc::Receiver<inlet::Frame<TO_KEY,TO_ADDRESS,TO_KIND,TO_RESOURCE_TYPE>>)= mpsc::channel(128);

    tokio::spawn ( async move {
        while let Option::Some(from) = from_rx.recv().await{
            let to : Result<inlet::Frame<TO_KEY,TO_ADDRESS,TO_KIND,TO_RESOURCE_TYPE>,Error> = from.convert();
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

    (from_tx, to_rx)
}

pub fn outlet_converter<FROM_KEY,FROM_ADDRESS,FROM_KIND,FROM_RESOURCE_TYPE,TO_KEY,TO_ADDRESS,TO_KIND,TO_RESOURCE_TYPE>(handle_error:fn (error:Error) ) -> (mpsc::Sender<outlet::Frame<FROM_KEY,FROM_ADDRESS,FROM_KIND,FROM_RESOURCE_TYPE>>, mpsc::Receiver<outlet::Frame<TO_KEY,TO_ADDRESS,TO_KIND,TO_RESOURCE_TYPE>>) where
    FROM_KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ 'static,
    FROM_ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync + 'static,
    FROM_KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync + 'static,
    FROM_RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync + 'static,
    TO_KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ TryFrom<FROM_KEY,Error=Error>  + 'static,
    TO_ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ TryFrom<FROM_ADDRESS,Error=Error> + 'static,
    TO_KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ TryFrom<FROM_KIND,Error=Error>  + 'static,
    TO_RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync+ TryFrom<FROM_RESOURCE_TYPE,Error=Error> + 'static

{
    let (from_tx,mut from_rx) : (mpsc::Sender<outlet::Frame<FROM_KEY,FROM_ADDRESS,FROM_KIND,FROM_RESOURCE_TYPE>>,mpsc::Receiver<outlet::Frame<FROM_KEY,FROM_ADDRESS,FROM_KIND,FROM_RESOURCE_TYPE>>)= mpsc::channel(128);
    let (to_tx,mut to_rx) : (mpsc::Sender<outlet::Frame<TO_KEY,TO_ADDRESS,TO_KIND,TO_RESOURCE_TYPE>>,mpsc::Receiver<outlet::Frame<TO_KEY,TO_ADDRESS,TO_KIND,TO_RESOURCE_TYPE>>)= mpsc::channel(128);

    tokio::spawn ( async move {
        while let Option::Some(from) = from_rx.recv().await{
            let to : Result<outlet::Frame<TO_KEY,TO_ADDRESS,TO_KIND,TO_RESOURCE_TYPE>,Error> = from.convert();
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

    (from_tx, to_rx)
}