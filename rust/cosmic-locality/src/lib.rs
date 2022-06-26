#![allow(warnings)]

#[macro_use]
extern crate cosmic_macros;

#[macro_use]
extern crate lazy_static;


pub mod driver;
pub mod state;
pub mod star;
pub mod lifecycle;
pub mod field;
pub mod shell;
pub mod portal;
pub mod traversal;
pub mod router;
pub mod machine;
pub mod host;
pub mod guest;


pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
