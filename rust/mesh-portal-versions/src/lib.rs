#![allow(warnings)]
//# ! [feature(unboxed_closures)]
#[no_std]

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate strum_macros;
extern crate core;


use std::collections::HashMap;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

pub mod version;
pub mod error;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
