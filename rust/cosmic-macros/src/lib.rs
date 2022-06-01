#![crate_type = "lib"]
#![allow(warnings)]

use proc_macro::{self, TokenStream};
use quote::quote;
use syn::{parse_macro_input, DataEnum, DataUnion, DeriveInput, FieldsNamed, FieldsUnnamed, ItemStruct};



#[proc_macro_attribute]
pub fn select(attr: TokenStream, item: TokenStream ) -> TokenStream {
  println!("attr: {}",attr.to_string());
  println!("item : {}",item.to_string());
  item
}