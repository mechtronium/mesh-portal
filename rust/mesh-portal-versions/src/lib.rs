#![allow(warnings)]
#![feature(integer_atomics)]
//# ! [feature(unboxed_closures)]
#[no_std]


#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate strum_macros;
extern crate core;
extern crate alloc;

#[macro_use]
extern crate async_trait;
extern crate core;

use serde::{Deserialize, Serialize};

pub mod version;
pub mod error;

lazy_static!{
    pub static ref VERSION: semver::Version = semver::Version::from_str("1.0.0").unwrap();
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
