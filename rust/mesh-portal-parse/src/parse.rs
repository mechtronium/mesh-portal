use std::fmt;
use std::str::FromStr;

use anyhow::Error;
use nom::{AsChar, InputTakeAtPosition, IResult, Needed, InputTake, Compare, InputLength};
use nom::{Err, Parser};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_till, escaped, is_not, take, is_a};
use nom::character::complete::{alpha0, alpha1, digit0, digit1, multispace0, multispace1, anychar, alphanumeric1, alphanumeric0};
use nom::combinator::{all_consuming, opt, recognize, not};
use nom::error::{context, ErrorKind, ParseError, VerboseError, VerboseErrorKind, FromExternalError};
use nom::multi::{fold_many0, separated_list0, many1, separated_list1, many0};
use nom::sequence::{delimited, tuple, preceded, terminated};
use nom_supreme::{parse_from_str, ParserExt};

use crate::symbol::{RootSelector };
use mesh_portal_serde::version::latest::payload::{PayloadType, PrimitiveType};
use mesh_portal_serde::version::latest::payload::Payload;
use crate::pattern::{primitive};
use mesh_portal_serde::version::latest::util::{ValuePattern, StringMatcher};
use mesh_portal_serde::version::latest::payload::{CallWithConfig, RcCommand};
use mesh_portal_serde::version::latest::payload::{CallKind, Call};
use mesh_portal_serde::version::latest::payload::{PayloadFormat, PayloadPattern};
use mesh_portal_serde::version::latest::id::Address;
use mesh_portal_serde::version::latest::payload::MapPattern;
use std::iter::Map;
use std::collections::HashMap;
use mesh_portal_serde::version::v0_0_1::generic::payload::{MsgCall, HttpCall};
use mesh_portal_serde::version::v0_0_1::parse::{Res, filepath_chars, parse_version, rec_version, path, address_segment_chars, domain_chars, skewer_chars, in_double_quotes};
use mesh_portal_serde::version::v0_0_1::generic::resource::command::RcCommandType;


pub enum BindSection {
    Request(RequestSection)
}

impl From<RequestSection> for BindSection {
    fn from(request: RequestSection) -> Self {
        Self::Request(request)
    }
}

pub enum RequestSection {
    Rc(RcCommand),
    Msg(MsgCall),
    Http(HttpCall),
}

impl From<RcCommand> for RequestSection{
    fn from(rc: RcCommand) -> Self {
        Self::Rc(rc)
    }
}

impl From<MsgCall> for RequestSection{
    fn from(rc: MsgCall) -> Self {
        Self::Msg(rc)
    }
}
impl From<HttpCall> for RequestSection{
    fn from(rc: HttpCall) -> Self {
        Self::Http(rc)
    }
}


pub fn parse_address_segments(input: &str) -> Res<&str, Vec<String>> {
    context(
        "address",
        separated_list1(
            nom::character::complete::char(':'),
            alt((domain_chars, skewer_chars, filepath_chars, recognize(parse_version)))
        ),
    )(input).map( |(next,segments)|{
        let segments = segments.iter().map(|s|s.to_string()).collect();
        (next,segments)
    })
}

pub fn parse_mechtron_segments(input: &str) -> Res<&str, Vec<String>> {
    context(
        "mechtron",
        separated_list1(
            nom::character::complete::char(':'),
            alt((domain_chars,skewer_chars))
        ),
    )(input).map( |(next,segments)|{
        let segments = segments.iter().map(|s|s.to_string()).collect();
        (next,segments)
    })
}

pub fn rec_address(input: &str) -> Res<&str, &str> {
    recognize(tuple( (domain_chars,many0(preceded(tag(":"), alt((skewer_chars, rec_version, filepath_chars)) )))))(input)
}

pub fn rec_address_segment(input: &str) -> Res<&str, &str> {
    recognize(alt((rec_version, skewer_chars, filepath_chars)) )(input)
}



pub fn consume_address(input: &str) -> Res<&str, &str> {
    all_consuming(rec_address)(input)
}

pub fn rec_artifact(input: &str) -> Res<&str, &str> {
    recognize(tuple( (domain_chars,many0(preceded(tag(":"), skewer_chars)),preceded(tag(":"), rec_version),preceded(tag(":"), filepath_chars))))(input)
}

pub fn rec_mechtron(input: &str) -> Res<&str, &str> {
    recognize(tuple( (domain_chars,many0(preceded(tag(":"), skewer_chars)))))(input)
}

pub fn consume_artifact(input: &str) -> Res<&str, &str> {
    all_consuming(rec_artifact )(input)
}


pub fn consume_mechtron(input: &str) -> Res<&str, &str> {
    all_consuming(rec_mechtron)(input)
}

pub fn consume_address_segments(input: &str) -> Res<&str, Vec<String>> {
  all_consuming( parse_address_segments )(input)
}

pub fn address_path(input: &str) -> Res<&str, &str> {
    recognize(separated_list1(
        nom::character::complete::char(':'),
       address_segment_chars
    ))(input)
}

pub fn mechtron_path(input: &str) -> Res<&str, &str> {
    recognize(separated_list1(
        nom::character::complete::char(':'),
       address_segment_chars
    ))(input)
}


pub fn mechtron_address(input: &str) -> Res<&str, Address> {
    parse_from_str( mechtron_path ).parse(input)
}


fn payload_type_empty(input: &str ) -> Res<&str, PayloadTypeDef> {
    alt((tag("Empty"), multispace0 )) (input).map( |(next,_)| {
        (next,PayloadTypeDef::Empty)
    } )
}

fn payload_type_primitive(input: &str ) -> Res<&str, PayloadTypeDef> {
    primitive(input).map( |(next,primitive)| {
        (next,PayloadTypeDef::Primitive(primitive))
    } )
}

fn payload_type_list(input: &str ) -> Res<&str, PayloadTypeDef> {
    tuple((primitive,tag("[]")))(input).map( |(next,(primitive,_))| {
        (next,PayloadTypeDef::List(primitive))
    } )
}

fn payload_type_map(input: &str ) -> Res<&str, PayloadTypeDef> {
    tag("Map")(input).map( |(next,_)| {
        (next,PayloadTypeDef::Map)
    } )
}



/// use nom_supreme::{parse_from_str, parser_ext::ParserExt};
pub fn payload_type_def(input: &str ) -> Res<&str, PayloadTypeDef> {
    alt((payload_type_list, payload_type_primitive, payload_type_empty))(input)
}



pub enum PayloadTypeDef {
    Empty,
    Primitive(PrimitiveType),
    List(PrimitiveType),
    Map,
}

impl ToString for PayloadTypeDef {
    fn to_string(&self) -> String {
        match self {
            PayloadTypeDef::Empty => "Empty".to_string(),
            PayloadTypeDef::Primitive(primitive) => {
                primitive.to_string()
            }
            PayloadTypeDef::List(primitive) => {
                format!("{}[]",primitive.to_string())
            }
            PayloadTypeDef::Map => {
                "Map".to_string()
            }
        }
    }
}


/// use nom_supreme::{parse_from_str, parser_ext::ParserExt};
pub fn payload_format(input: &str ) -> Res<&str, PayloadFormat> {
    parse_from_str(alpha1).parse(input)
}

pub fn payload_validator(input: &str ) -> Res<&str, PayloadValidator> {
    tuple(( call, opt(preceded(tag("+"), Address::parse ))))(input).map( |(next,(port_call,schema))| {
        (next,PayloadValidator{
            port_call,
            schema
        })
    } )
}

pub fn payload_validation(input: &str ) -> Res<&str, PayloadValidation> {
     tuple((tag("~"), opt(payload_format), opt(preceded(tag("~"), payload_validator) )) )(input).map( |(next,(_,format,validator)) | {
         (next, PayloadValidation{
             format,
             validator
         })
     })
}



pub fn tagged_payload_type_def(input: &str ) -> Res<&str, PayloadTypeDef> {
    delimited(tag("<"), payload_type_def, tag(">") )(input)
}

pub fn opt_tagged_payload_type_def(input: &str ) -> Res<&str, PayloadTypeDef> {
    alt( ((tagged_payload_type_def, payload_type_def)) ) (input)
}


pub fn labeled_payload_def(input: &str ) -> Res<&str, PayloadDef> {
    (tuple( (skewer_chars, tagged_payload_type_def))) (input).map( |(next,(label,type_def))| {

        (next,
         PayloadDef{
             label: Option::Some(label.to_string()),
             type_def
         })
    } )
}

pub fn unlabeled_payload_def(input: &str ) -> Res<&str, PayloadDef> {
    opt_tagged_payload_type_def(input).map ( | (next, type_def) | {
        (next,
         PayloadDef{
             label: Option::None,
             type_def
         })
    } )
}



pub fn payload_def(input: &str ) -> Res<&str, PayloadDef> {
    alt( (labeled_payload_def,unlabeled_payload_def) )(input)
}

pub fn consume_payload_def(input: &str ) -> Res<&str, PayloadDef> {
    all_consuming(payload_def).parse(input)
}

pub fn text_value(input: &str ) -> Res<&str, PayloadValueSrc> {
  delimited(tag("\""), in_double_quotes, tag("\""))(input).map ( |(next,text) | {

      (next,
      PayloadValueSrc::Text(text.to_string()))

  } )
}

pub fn address_value(input: &str ) -> Res<&str, PayloadValueSrc> {
    delimited(tag("'"), Address::parse, tag("'"))(input).map(|(next, address)| {
        (next,
         PayloadValueSrc::Address(address))
    })
}

pub fn state_value(input: &str ) -> Res<&str, PayloadValueSrc> {
    Address::parse(input).map(|(next, address)| {
        (next,
         PayloadValueSrc::State(address))
    })
}
pub fn assigned_value(input: &str ) -> Res<&str, PayloadValueSrc> {
  //alt((text_value,address_value,state_value))(input)
    alt( ( text_value, address_value, state_value ) )(input)

}


pub fn payload_assignment(input: &str ) -> Res<&str, PayloadAssign> {
    tuple((payload_def, tag("="), assigned_value))(input).map( |(next,(def,_,value))|{
        (next,
         PayloadAssign {
            def,
            value
        })
    })
}

pub fn consume_payload_assignment(input: &str ) -> Res<&str, PayloadAssign> {
    all_consuming(payload_assignment)(input)
}

pub fn consume_pipeline_step(input: &str ) -> Res<&str, PayloadAssign> {
    all_consuming(payload_assignment)(input)
}







pub struct PayloadDef {
    pub label: Option<String>,
    pub type_def: PayloadTypeDef
}

impl ToString for PayloadDef {
    fn to_string(&self) -> String {
        match &self.label {
            None => {
                self.type_def.to_string()
            }
            Some(label) => {
                format!("{}<{}>",label, self.type_def.to_string() )
            }
        }
    }
}

pub enum PayloadValueSrc {
    Text(String),
    Address(Address),
    State(Address)
}

pub struct PayloadAssign {
    pub def: PayloadDef,
    pub value: PayloadValueSrc
}

pub struct PayloadValidation{
    pub format: Option<PayloadFormat>,
    pub validator: Option<PayloadValidator>
}

impl ToString for PayloadValidation{
    fn to_string(&self) -> String {
        let mut rtn = String::new();
        if let Some(format) = &self.format {
            rtn.push_str( "~" );
            rtn.push_str( format.to_string().as_str() );

        } else if  self.validator.is_some() {
            rtn.push_str("~")
        }

        if let Some(validator) = &self.validator{
            rtn.push_str("~");
            rtn.push_str(validator.to_string().as_str() );
        }

        rtn
    }
}


pub struct PayloadValidator {
    pub port_call: Call,
    pub schema: Option<Address>
}

impl ToString for PayloadValidator {
    fn to_string(&self) -> String {
        match &self.schema {
            None => {
                self.port_call.to_string()
            }
            Some(schema) => {
                format!("{}+{}",self.port_call.to_string(), schema.to_string()  )
            }
        }
    }
}


pub enum PayloadValidationPattern {
   Any,
   Exact( PayloadValidation )
}

impl ToString for PayloadValidationPattern {
    fn to_string(&self) -> String {
        match &self{
            PayloadValidationPattern::Any => {
                "*".to_string()
            }
            PayloadValidationPattern::Exact(exact) => {
                exact.to_string()
            }
        }
    }
}




/*
/// use nom_supreme::{parse_from_str, parser_ext::ParserExt};
pub fn payload_labeling(input: &str ) -> Res<&str, PayloadAssignment> {
    tuple((opt(alpha1),delimited(tag("<"), payload_assignment, tag(">") )))(input)
}

 */


#[derive(Debug, Clone)]
pub struct MyError {
    pub message: String,
}

impl From<nom::Err<VerboseError<&str>>> for MyError {
    fn from(e: Err<VerboseError<&str>>) -> Self {
        Self {
            message: e.to_string()
        }
    }
}

impl std::error::Error for MyError {

}


impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message )
    }
}



impl From<&str> for MyError {
    fn from(s: &str) -> Self {
        Self {
            message: s.to_string()
        }
    }
}

impl From<strum::ParseError> for MyError {
    fn from(s: strum::ParseError) -> Self {
        Self {
            message: s.to_string()
        }
    }
}


pub enum PipeSegEntry {
    Request,
    Create,
    Upload
}





