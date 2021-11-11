#[macro_use]
extern crate strum_macros;
#[macro_use]
extern crate anyhow;


pub mod parse;
pub mod token;
pub mod symbol;
pub mod pattern;
pub mod bind;

use serde::*;
use crate::bind::Port;

#[derive(Serialize,Deserialize)]
pub struct Host<T> {
    pub t: T
}


pub struct Bind {
  pub init: Init,
  pub request: Request,
}

impl Default for Bind{
    fn default() -> Self {

        Self {
            init: Default::default(),
            request: Default::default()
        }
    }
}

pub struct Request {
    pub rc: Rc,
    pub msg: Msg,
    pub http: Http,
}

impl Default for Request {
    fn default() -> Self {
        Request {
            rc: Default::default(),
            msg: Default::default(),
            http: Default::default()
        }
    }
}

impl Default for Rc{
    fn default() -> Self {
        Self{}
    }
}

impl Default for Msg{
    fn default() -> Self {
        Self{}
    }
}

impl Default for Http{
    fn default() -> Self {
        Self{}
    }
}

impl Default for Init {
    fn default() -> Self {
        Self{}
    }
}
pub struct Init {

}


pub struct Rc {

}

pub struct Msg {
  pub ports: Vec<Port>
}

pub struct Http {

}




#[cfg(test)]
pub mod test {
    use crate::Host;
    use bincode;
    use anyhow::Error;



    #[test]
    pub fn test() -> Result<(),Error>{
        let host = Host { t: "hello".to_string() };
        let x = bincode::serialize(&host)?;

        bincode::deserialize(x.as_slice() )?;

        Ok(())

    }
}
