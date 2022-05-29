use core::str::FromStr;
use nom::combinator::all_consuming;

use crate::error::MsgErr;
use crate::version::v0_0_1::bin::Bin;
use crate::version::v0_0_1::entity::request::create::{Create, CreateCtx, CreateVar, Strategy};
use crate::version::v0_0_1::entity::request::select::{Select, SelectCtx, SelectVar};
use crate::version::v0_0_1::entity::request::get::{Get, GetCtx, GetVar};
use crate::version::v0_0_1::entity::request::set::{Set, SetCtx, SetVar};
use crate::version::v0_0_1::parse::{command_line, Env};
use crate::version::v0_0_1::parse::error::result;
use crate::version::v0_0_1::payload::payload::Payload;
use crate::version::v0_0_1::span::{new_span, Trace};
use crate::version::v0_0_1::util::ToResolved;
use serde::{Serialize,Deserialize};

pub struct CommandTemplate {
    pub line: String,
    pub transfers: Vec<Trace>
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct RawCommand {
   pub line: String,
   pub transfers: Vec<Transfer>
}

impl Into<Payload> for RawCommand {
    fn into(self) -> Payload {
        Payload::CommandLine(self)
    }
}

#[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq)]
pub struct Transfer {
    pub id: String,
    pub content: Bin
}



pub enum Command{
    Create(Create),
    Select(Select),
    Publish(Create),
    Set(Set),
    Get(Get)
}

pub enum CommandCtx{
    Create(CreateCtx),
    Select(SelectCtx),
    Publish(CreateCtx),
    Set(SetCtx),
    Get(GetCtx)
}

pub enum CommandVar {
    Create(CreateVar),
    Select(SelectVar),
    Publish(CreateVar),
    Set(SetVar),
    Get(GetVar)
}

impl CommandVar {
    pub fn set_strategy( &mut self, strategy: Strategy ) {
        match self {
            CommandVar::Create(create) => {
                create.strategy = strategy;
            }
            CommandVar::Select(_) => {}
            CommandVar::Publish(_) => {}
            CommandVar::Set(_) => {}
            CommandVar::Get(_) => {}
        }
    }
}

impl FromStr for CommandVar {
    type Err = MsgErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = new_span(s);
        result(all_consuming(command_line)(s))
    }
}

impl CommandVar {

    /*
    pub fn requires(&self) -> Vec<Require> {
        match self {
            CommandVar::Create(_) => {vec![]}
            CommandVar::Select(_) => {vec![]}
            CommandVar::Publish(publish) => {
                publish.requirements.clone()
            }
            CommandVar::Set(_) => {vec![]}
            CommandVar::Get(_) => {vec![]}
        }
    }
     */

}

impl ToResolved<Command> for CommandVar {
    fn to_resolved(self, env: &Env) -> Result<Command, MsgErr> {
        let command: CommandCtx = self.to_resolved(env)?;
        command.to_resolved(env)
    }
}

impl ToResolved<CommandCtx> for CommandVar {
    fn to_resolved(self, env: &Env) -> Result<CommandCtx, MsgErr> {
        Ok(match self {
            CommandVar::Create(i) => CommandCtx::Create(i.to_resolved(env)?),
            CommandVar::Select(i) => CommandCtx::Select(i.to_resolved(env)?),
            CommandVar::Publish(i) => CommandCtx::Publish(i.to_resolved(env)?),
            CommandVar::Set(i) => CommandCtx::Set(i.to_resolved(env)?),
            CommandVar::Get(i) => CommandCtx::Get(i.to_resolved(env)?)
        })
    }
}

impl ToResolved<Command> for CommandCtx {
    fn to_resolved(self, env: &Env) -> Result<Command, MsgErr> {
        Ok(match self {
            CommandCtx::Create(i) => Command::Create(i.to_resolved(env)?),
            CommandCtx::Select(i) => Command::Select(i.to_resolved(env)?),
            CommandCtx::Publish(i) => Command::Publish(i.to_resolved(env)?),
            CommandCtx::Set(i) => Command::Set(i.to_resolved(env)?),
            CommandCtx::Get(i) => Command::Get(i.to_resolved(env)?)
        })
    }
}