use std::str::FromStr;
use anyhow::Error;
use crate::token::generic::{Span, Block};
use std::collections::HashMap;
use std::string::ParseError;
use crate::parse::{parse_address_segments, MyError, consume_address_segments, RcCommand, consume_call};

pub enum RootSelector {
    Bind
}

impl FromStr for RootSelector {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s{
            "Bind" => Ok(RootSelector::Bind),
            _ => {
                let message =format!("invalid selector at this document level: '{}'",s);
                Err(anyhow!(message))
            }
        }
    }
}

impl RootSelector {
    pub fn span(&self) -> Span {
        match self {
            RootSelector::Bind => {
                Span{ block: Block::Curly, spans: Default::default() }
            }
        }
    }
}


#[derive(Debug,Clone,Eq,PartialEq)]
pub struct Call {
    pub address: Address,
    pub kind: CallKind
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub enum CallKind{
    Rc(RcCommand),
    Msg(String),
    Http
}

impl ToString for CallKind {
    fn to_string(&self) -> String {
        match self {
            CallKind::Rc(command) => {
                format!("Rc<{}>",command.to_string())
            }
            CallKind::Msg(port) => {
                format!("Msg<{}>",port.clone())
            }
            CallKind::Http => "Http".to_string()
        }
    }
}


#[derive(Debug,Clone,Eq,PartialEq)]
pub struct CallWithConfig {
    pub call: Call,
    pub config: Option<Address>
}

impl FromStr for Call {
    type Err = MyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(consume_call(s)?.1)
    }
}

impl ToString for Call {
    fn to_string(&self) -> String {
       format!("{}^{}", self.address.to_string(), self.kind.to_string())
    }
}

#[derive(Debug,Clone,Eq,PartialEq,Hash)]
pub struct Address {
    segments: Vec<String>
}

impl Address {
    pub fn parent(&self) -> Option<Address> {
        if self.segments.is_empty() {
            return Option::None;
        }
        let mut segments = self.segments.clone();
        segments.remove( segments.len() );
        Option::Some( Self {
            segments
        })
    }
}

impl FromStr for Address {
    type Err = MyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (_,segments) = consume_address_segments(s)?;
        Ok(Self{ segments })
    }
}

impl ToString for Address {
    fn to_string(&self) -> String {
        let mut rtn = String::new();
        for (i, segment) in self.segments.iter().enumerate() {
            rtn.push_str( segment.as_str() );
            if i != self.segments.len()-1 {
                rtn.push_str(":");
            }
        }
        rtn.to_string()
    }
}