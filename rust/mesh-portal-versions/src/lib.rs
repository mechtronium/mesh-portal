
#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate strum_macros;



use std::collections::HashMap;
use std::sync::Arc;

use serde::{Deserialize, Serialize};
use crate::version::v0_0_1::log::Log;

pub mod version;
pub mod error;

pub fn std_logger(log: Log ) {
    match log {
        Log::Warn(m) => {
            println!("WARN: {}", m);
        }
        Log::Info(m) => {
            println!("INFO: {}", m);
        }
        Log::Error(m) => {
            eprintln!("ERROR: {}", m);
        }
        Log::Fatal(m) => {
            eprintln!("FATAL: {}", m);
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
