#[derive(strum_macros::Display )]
pub enum EntityIdent {
    Rc,
    Msg,
    Http
}

pub enum Entity{
    Rc(Rc),
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
pub enum PayloadIdent
{
    Empty,
    Text,
    Texts,
    Key,
    Keys,
    Address,
    Addresses,
    Stub,
    Stubs,
    Meta,
    Bin,
    Bins,
    Boolean,
    Code,
    Num,
    Status,
    Resource
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

    #[derive(strum_macros::EnumString)]
    pub enum RequestIdent {
        Rc,
        Msg,
        Http
    }


}