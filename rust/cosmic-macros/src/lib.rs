#![crate_type = "lib"]
#![allow(warnings)]

#[macro_use]
extern crate strum_macros;
use proc_macro::{self, TokenStream};
use std::str::FromStr;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, DataEnum, DataUnion, DeriveInput, FieldsNamed, FieldsUnnamed, ItemStruct, FnArg, Type, PathArguments, GenericArgument, ReturnType, PathSegment};




#[proc_macro_attribute]
pub fn route(attr: TokenStream, item: TokenStream ) -> TokenStream {
//  println!("attr: {}",attr.to_string());
//  println!("item : {}",item.to_string());

  let item = parse_macro_input!(item as syn::ImplItemMethod);
  let block = item.block;

  let params :Vec<FnArg> = item.sig.inputs.clone().into_iter().collect();
  let ctx = params.get(1).expect("route expected MessageCtx<I,M> as first parameter");
  let ctx = messsage_ctx(ctx).expect("route expected MessageCtx<I,M> as first parameter");

  let ident = item.sig.ident;
  let messenger = ctx.messenger;
  let rtn_type = rtn_type( &item.sig.output );
  let item = ctx.item;


  let expanded = quote! {
      fn #ident( mut ctx: mesh_portal::version::latest::messaging::RootMessageCtx<mesh_portal::version::latest::messaging::Request,#messenger> ) -> Result<mesh_portal::version::latest::entity::response::ResponseCore,MsgErr> {
          let mut ctx : mesh_portal::version::latest::messaging::RootMessageCtx<#item,#messenger> = ctx.transform_input()?;
          let ctx = ctx.push();

          fn inner( ctx: MessageCtx<#item,#messenger>) -> Result<#rtn_type,MsgErr>
              #block

          match inner(ctx) {
              Ok(rtn) => Ok(mesh_portal::version::latest::entity::response::ResponseCore::from(rtn)),
              Err(err) => Err(err)
          }
      }
    };
println!("expanded: {}",expanded.to_string());

  TokenStream::from(expanded)
}

pub(crate) enum Item {
    Request,
    RequestCore,
    Payload
}

impl FromStr for Item {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Request" => Ok(Item::Request),
            "RequestCore" => Ok(Item::RequestCore),
            "Payload" => Ok(Item::Payload),
            what => panic!("cannot convert Request to type '{}'", what)
        }
    }
}

pub(crate) struct MessageCtx {
    pub item: GenericArgument,
    pub messenger: GenericArgument
}

fn messsage_ctx( input: &FnArg )  -> Result<MessageCtx,String>{
   if let FnArg::Typed(i) = input {
            if let Type::Path(path) = &*i.ty {
                if let PathArguments::AngleBracketed(generics) = &path.path.segments.last().expect("expected last segment").arguments
                {
                    let mut args = generics.args.clone();
                    let messenger = args.pop().expect("expecting a generic for Messenger").into_value();
                    let item = args.pop().expect("expecting a generic for Context Item").into_value();

                    let ctx = MessageCtx {
                        item,
                        messenger
                    };

                    return Ok(ctx);
                }
            }
    }
    Err("Parameter is not a MessageCtx".to_string())
}

fn rtn_type( output: &ReturnType ) -> GenericArgument {

        if let ReturnType::Type(_, t) = output {
            if let Type::Path(path) = &**t {
                if let PathSegment{arguments,..} = path.path.segments.last().expect("expecting Result") {
                    if let PathArguments::AngleBracketed(args) = arguments {
                        return args.args.first().expect("expected Result Ok to be ResponseCore::from(...) compatible").clone()
                    }
                }
            }
        }

        panic!("route must return Result<R,MsgErr>")
}