use std::str::FromStr;
use anyhow::Error;
use std::collections::HashMap;
use std::string::ParseError;
use crate::parse::{parse_address_segments, MyError, consume_address_segments, consume_call};

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












