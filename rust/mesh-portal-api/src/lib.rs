use tokio::sync::mpsc;
use mesh_portal_serde::version::latest::generic::portal::inlet;
use mesh_portal_serde::version::latest::generic::portal::outlet;
use mesh_portal_serde::version::latest::error::Error;
use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;
use std::convert::TryInto;
use tokio::sync::mpsc::error::SendError;

pub fn translate<FROM,TO>( handle_error:fn (error:Error) ) -> (mpsc::Sender<FROM>,mpsc::Receiver<TO>) {
  let (from_tx,mut from_rx) = mpsc::channel(128);
  let (to_tx, to_rx) = mpsc::channel(128);

  tokio::spawn ( async move {
      while let Option::Some(from) = from_rx.recv().await{
          match from.try_into() {
              Ok(to) => {
                  match to_tx.send(to).await {
                      Err(err) => {
                          handle_error(err.into());
                          break;
                      }
                      _ => {}
                  }
              }
              Err(err) => {
                  handle_error(err)
              }
          }
      }
  });

 (from_tx, to_rx)
}

