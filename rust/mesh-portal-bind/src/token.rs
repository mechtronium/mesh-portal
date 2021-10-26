use std::convert::TryInto;
use anyhow::Error;

#[derive(strum_macros::Display )]
pub enum EntityIdent {
    Rc,
    Msg,
    Http
}



#[derive(strum_macros::Display )]
pub enum RcIdent {
    Create,
    Select,
    Read
}

#[derive(strum_macros::Display )]
pub enum Rc {
    Create,
    Select,
    Read
}

pub struct Msg {

}


#[derive(strum_macros::Display,strum_macros::EnumString)]
pub enum PayloadType
{
    Empty,
    Text,
    Key,
    Address,
    Stub,
    Meta,
    Bin,
    Boolean,
    Code,
    Int,
    Status,
    Resource,
    List,
    Map
}

impl TryInto<PayloadPrimitive> for PayloadType {

    type Error = Error;

    fn try_into(self) -> Result<PayloadPrimitive, Self::Error> {
        match self {
            PayloadType::Empty => Ok(PayloadPrimitive::Empty),
            PayloadType::Text => Ok(PayloadPrimitive::Text),
            PayloadType::Key => Ok(PayloadPrimitive::Key),
            PayloadType::Address => Ok(PayloadPrimitive::Address),
            PayloadType::Stub => Ok(PayloadPrimitive::Stub),
            PayloadType::Meta => Ok(PayloadPrimitive::Meta),
            PayloadType::Bin => Ok(PayloadPrimitive::Bin),
            PayloadType::Boolean => Ok(PayloadPrimitive::Boolean),
            PayloadType::Code => Ok(PayloadPrimitive::Code),
            PayloadType::Int => Ok(PayloadPrimitive::Int),
            PayloadType::Status => Ok(PayloadPrimitive::Status),
            PayloadType::Resource => Ok(PayloadPrimitive::Resource),
            payload_type => {
                let message = format!("PayloadType: {} does not have a primitive",payload_type.to_string());
                Err(anyhow!(message))
            }
        }
    }
}

#[derive(strum_macros::Display,strum_macros::EnumString)]
pub enum PayloadPrimitive
{
    Empty,
    Text,
    Key,
    Address,
    Stub,
    Meta,
    Bin,
    Boolean,
    Code,
    Int,
    Status,
    Resource
}

impl Into<PayloadType> for PayloadPrimitive {
    fn into(self) -> PayloadType {
        match self {
            Self::Empty => PayloadType::Empty,
            Self::Text => PayloadType::Text,
            Self::Key => PayloadType::Key,
            Self::Address => PayloadType::Address,
            Self::Stub => PayloadType::Stub,
            Self::Meta => PayloadType::Meta,
            Self::Bin => PayloadType::Bin,
            Self::Boolean => PayloadType::Boolean,
            Self::Code => PayloadType::Code,
            Self::Int => PayloadType::Int,
            Self::Status => PayloadType::Status,
            Self::Resource => PayloadType::Resource
        }
    }
}


pub enum StackCmd {
    Push,
    Pop
}

pub enum Selector {
    Bind
}







pub mod generic {
    use std::marker::PhantomData;
    use std::collections::HashMap;
    use std::str::FromStr;
    use anyhow::Error;
    use crate::parse::PayloadDef;
    use crate::token::PayloadType;


    pub enum Ident {
        CurlyOpen,
        CurlyClose,
        BracketOpen,
        BracketClose
    }

    impl FromStr for Ident {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "{" => Ok(Self::CurlyOpen),
                "}" => Ok(Self::CurlyClose),
                "[" => Ok(Self::BracketOpen),
                "]" => Ok(Self::BracketClose),
                s =>  {
                    let message = format!("cannot match Ident '{}'", s);
                    Err(anyhow!(message))
                }
            }
        }
    }

    pub struct Span {
        pub block: Block,
        pub spans: HashMap<Block,Span>
    }

    #[derive(Eq,PartialEq,Hash)]
    pub enum Block {
        Curly,
        Bracket
    }

    impl Block {
        fn pair(&self) -> BlockEndPair{
            match self {
                Block::Curly => BlockEndPair::new(Ident::CurlyOpen, Ident::CurlyClose ),
                Block::Bracket => BlockEndPair::new(Ident::BracketOpen, Ident::BracketClose )
            }
        }
    }

    pub struct BlockEndPair{
        pub open: Ident,
        pub close: Ident
    }

    impl BlockEndPair {
        pub fn new( open: Ident, close: Ident ) -> Self {
            Self {
                open,
                close
            }
        }
    }

    pub struct Selection {
        pub selector: String,
        pub span: Span
    }


    pub enum BlockPart<IDENT> {
        Fragment(String),
        Ident(IDENT)
    }

    impl FromStr for BlockPart<Ident> {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match Ident::from_str(s) {
                Ok(ident) => {Ok(Self::Ident(ident))}
                Err(_) => {
                    Ok(Self::Fragment(s.to_string()))
                }
            }
        }
    }

    impl FromStr for BlockPart<BindSelectionIdent> {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match BindSelectionIdent::from_str(s) {
                Ok(ident) => {Ok(Self::Ident(ident))}
                Err(_) => {
                    Ok(Self::Fragment(s.to_string()))
                }
            }
        }
    }


    pub enum BindSelectionIdent {
        CurlyOpen,
        CurlyClose
    }

    impl FromStr for BindSelectionIdent {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "{" => Ok(Self::CurlyOpen),
                "}" => Ok(Self::CurlyClose),
                s =>  {
                    let message = format!("cannot match Ident '{}'", s);
                    Err(anyhow!(message))
                }
            }
        }
    }




    #[derive(strum_macros::EnumString)]
    pub enum BindIdent {
        Request
    }

    pub enum Entity<P>{
        Rc,
        Msg(P),
        Http
    }


    impl ToString for Entity<PayloadDef> {
        fn to_string(&self) -> String {
            match self {
                Entity::Rc => {
                    format!("Rc")
                }
                Entity::Msg(p) => {
                    if let PayloadType::Empty = p.type_def.kind {
                        format!("Msg")
                    } else {
                        format!("Msg<{}>",p.to_string())
                    }

                }
                Entity::Http => {
                    format!("Http")
                }
            }
        }
    }


    #[derive(strum_macros::EnumString,strum_macros::Display)]
    pub enum EntityKind {
        Rc,
        Msg,
        Http
    }


}