use std::str::FromStr;
use anyhow::Error;
use crate::token::generic::{Span, Block};
use std::collections::HashMap;
use std::string::ParseError;
use crate::parse::{parse_address_segments, MyError, consume_address_segments, consume_port_call};

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


pub struct PortCall {
    pub address: Address,
    pub port: String
}
impl FromStr for PortCall{
    type Err = MyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(consume_port_call(s)?.1)
    }
}

impl ToString for PortCall {
    fn to_string(&self) -> String {
        format!("{}!{}", self.address.to_string(), self.port.to_string())
    }
}

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
            if i != self.segments.len() {
                rtn.push_str(":");
            }
        }
        rtn.to_string()
    }
}