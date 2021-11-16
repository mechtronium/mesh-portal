use std::fmt;
use std::str::FromStr;

use anyhow::Error;
use nom::{AsChar, InputTakeAtPosition, IResult, Needed, InputTake, Compare, InputLength};
use nom::{Err, Parser};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_till, escaped};
use nom::character::complete::{alpha0, alpha1, digit0, digit1, multispace0, multispace1, anychar};
use nom::combinator::{all_consuming, opt, recognize, not};
use nom::error::{context, ErrorKind, ParseError, VerboseError, VerboseErrorKind, FromExternalError};
use nom::multi::{fold_many0, separated_list0, many1, separated_list1, many0};
use nom::sequence::{delimited, tuple, preceded, terminated};
use nom_supreme::{parse_from_str, ParserExt};

use crate::{parse, Bind, Request, Rc, Msg, Http};
use crate::token::{PayloadType, StackCmd };
use crate::symbol::{RootSelector, Address, Call, CallWithConfig, CallKind};
use crate::token::generic::{BlockPart, EntityKind, Entity};

pub type Res<I,O>=IResult<I,O, VerboseError<I>>;

pub fn asterisk<T, E: nom::error::ParseError<T>>(input: T) -> IResult<T, T, E>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| item.as_char() != '*' )
}

fn any_resource_path_segment<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item == '/')
                && !(char_item == '_')
                && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

fn in_double_quotes<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item == '\"'
        },
        ErrorKind::AlphaNumeric,
    )
}


fn domain<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-') &&
            !(char_item == '.')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}


pub fn skewer<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

fn filepath<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item == '/')
                && !(char_item == '_')
                && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}


fn parse_version_major_minor_patch(input: &str) -> Res<&str, (&str, &str, &str)> {
    context(
        "version_major_minor_patch",
        tuple((
            terminated(digit1, tag(".")),
            terminated(digit1, tag(".")),
            terminated(digit1, not(digit1)),
        )),
    )(input)
}

pub fn parse_version(input: &str) -> Res<&str, ((&str,&str,&str), Option<&str>)> {
        tuple((parse_version_major_minor_patch, opt(preceded(tag("-"), skewer))))(input)
}


pub fn rec_version(input: &str) -> Res<&str, &str> {
    recognize(parse_version)(input)
}
/*
fn bind(input: &str) -> Res<&str, Bind> {
    preceded( multispace0,tuple((preceded( multispace0,tag("bind")), preceded( multispace0,delimited(tag("{"), preceded( multispace0,request), tag("}")))) ))(input).map( |(next,(_,bind))| {
        (next, Bind::default() )
    } )
}

 */










pub enum BindSection {
    Request(Request)
}

impl From<Request> for BindSection {
    fn from(request: Request) -> Self {
        Self::Request(request)
    }
}

pub enum RequestSection {
    Rc(Rc),
    Msg(Msg),
    Http(Http),
}

impl From<Rc> for RequestSection{
    fn from(rc: Rc) -> Self {
        Self::Rc(rc)
    }
}

impl From<Msg> for RequestSection{
    fn from(rc: Msg) -> Self {
        Self::Msg(rc)
    }
}
impl From<Http> for RequestSection{
    fn from(rc: Http) -> Self {
        Self::Http(rc)
    }
}




pub struct Group {
    pub group_type: GroupType,
    pub content: String
}

pub enum GroupToken {
    Fragment(String),
    Ident(GroupIdent)
}

impl ToString for GroupToken {
    fn to_string(&self) -> String {
        match self {
            GroupToken::Fragment(s) => {s.clone()}
            GroupToken::Ident(ident) => {ident.to_string()}
        }
    }
}

impl FromStr for GroupToken {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match GroupIdent::from_str(s) {
            Ok(ident) => {Ok(Self::Ident(ident))}
            Err(_) => {
                Ok(Self::Fragment(s.to_string()))
            }
        }
    }
}

pub enum GroupType {
   Bracket,
   Paren,
   Tag,
   Curly
}

impl GroupType {

    pub fn push_token(&self)-> &str {
        match self {
            GroupType::Bracket => "[",
            GroupType::Paren => "(",
            GroupType::Tag => "<",
            GroupType::Curly=> "{"
        }
    }

    pub fn pop_token(&self)-> &str {
        match self {
            GroupType::Bracket => "]",
            GroupType::Paren => ")",
            GroupType::Tag => ">",
            GroupType::Curly=> "}"
        }
    }
}


pub enum GroupIdent {
    Bracket(StackCmd),
    Paren(StackCmd),
    Tag(StackCmd),
    Curl(StackCmd)
}

impl GroupIdent {
    pub fn stack(&self) -> &StackCmd{
        match self {
            GroupIdent::Bracket(p) =>p,
            GroupIdent::Paren(p) =>p,
            GroupIdent::Tag(p) => p,
            GroupIdent::Curl(p) => p
        }
    }
}

impl ToString for GroupIdent {
    fn to_string(&self) -> String {
       match self {
           GroupIdent::Bracket(StackCmd::Push) => "[".to_string(),
           GroupIdent::Paren(StackCmd::Push) =>  "(".to_string(),
           GroupIdent::Tag(StackCmd::Push) =>  "<".to_string(),
           GroupIdent::Curl(StackCmd::Push) =>  "{".to_string(),
           GroupIdent::Bracket(StackCmd::Pop) => "]".to_string(),
           GroupIdent::Paren(StackCmd::Pop) =>  ")".to_string(),
           GroupIdent::Tag(StackCmd::Pop) =>  ">".to_string(),
           GroupIdent::Curl(StackCmd::Pop) =>  "}".to_string(),
       }
    }


}

impl GroupIdent {
    pub fn get_type(&self) -> GroupType {
        match self {
            GroupIdent::Bracket(_) => GroupType::Bracket,
            GroupIdent::Paren(_) => GroupType::Paren,
            GroupIdent::Tag(_) => GroupType::Tag,
            GroupIdent::Curl(_) => GroupType::Curly
        }
    }
}

impl FromStr for GroupIdent {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "[" => {
                Ok(Self::Bracket(StackCmd::Push) )
             }
            "(" => {
                Ok(Self::Paren(StackCmd::Push) )
            }
            "<" => {
                Ok(Self::Tag(StackCmd::Push) )
            }
            "]" => {
                Ok(Self::Bracket(StackCmd::Pop) )
            }
            ")" => {
                Ok(Self::Paren(StackCmd::Pop) )
            }
            ">" => {
                Ok(Self::Tag(StackCmd::Pop) )
            }

            _ => {

                Err(anyhow!("not a GroupIdent!"))
            }
        }
    }
}

pub fn parse_address_segments(input: &str) -> Res<&str, Vec<String>> {
    context(
        "address",
        separated_list1(
            nom::character::complete::char(':'),
            alt((domain,skewer,filepath,recognize(parse_version)))
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
            alt((domain,skewer))
        ),
    )(input).map( |(next,segments)|{
        let segments = segments.iter().map(|s|s.to_string()).collect();
        (next,segments)
    })
}

pub fn rec_address(input: &str) -> Res<&str, &str> {
    recognize(tuple( (domain,many0(preceded(tag(":"), alt((skewer,rec_version,filepath)) )))))(input)
}

pub fn rec_address_segment(input: &str) -> Res<&str, &str> {
    recognize(alt((rec_version,skewer,filepath)) )(input)
}



pub fn consume_address(input: &str) -> Res<&str, &str> {
    all_consuming(rec_address)(input)
}

pub fn rec_artifact(input: &str) -> Res<&str, &str> {
    recognize(tuple( (domain,many0(preceded(tag(":"), skewer)),preceded(tag(":"), rec_version),preceded(tag(":"),filepath))))(input)
}

pub fn rec_mechtron(input: &str) -> Res<&str, &str> {
    recognize(tuple( (domain,many0(preceded(tag(":"), skewer)))))(input)
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
        any_resource_path_segment
    ))(input)
}

pub fn mechtron_path(input: &str) -> Res<&str, &str> {
    recognize(separated_list1(
        nom::character::complete::char(':'),
        any_resource_path_segment
    ))(input)
}

pub fn address(input: &str) -> Res<&str, Address> {
   parse_from_str( address_path ).parse(input)
}

pub fn mechtron_address(input: &str) -> Res<&str, Address> {
    parse_from_str( mechtron_path ).parse(input)
}


pub fn call_with_config(input: &str) -> Res<&str, CallWithConfig> {
    tuple( (call, opt(preceded(tag("+"), address ))) )(input).map( |(next, (call,config)) |{

        (next,
         CallWithConfig {
            call,
            config
        })
    } )
}

pub fn rc_command(input: &str) -> Res<&str, RcCommand> {
    parse_from_str(alpha1).parse(input)
}

pub fn rc_call_kind(input: &str) -> Res<&str, CallKind> {
     delimited( tag("Rc<"), rc_command, tag(">"))(input).map( |(next,rc_command)| {
         (next,CallKind::Rc(rc_command))
     })
}

pub fn port_call_kind(input: &str) -> Res<&str, CallKind> {
    preceded( tag("Msg!"), skewer )(input).map( |(next,port)| {
        (next,CallKind::Msg(port.to_string()))
    })
}

pub fn http_call_kind(input: &str) -> Res<&str, CallKind> {
    tag("Http")(input).map( |(next,_)| {
        (next,CallKind::Http)
    })
}

pub fn call_kind( input: &str ) -> Res<&str,CallKind> {
    alt( (rc_call_kind,port_call_kind,http_call_kind))(input)
}



pub fn call(input: &str) -> Res<&str, Call> {
    tuple( ( mechtron_address, preceded(tag("^"), call_kind )) )(input).map( |(next,(address,kind))| {
        (next,Call{
            address,
            kind
        })
    } )
}

pub fn consume_call(input: &str) -> Res<&str, Call> {
    all_consuming( call )(input)
}

/// use nom_supreme::{parse_from_str, parser_ext::ParserExt};
pub fn payload_kind(input: &str ) -> Res<&str, PayloadType> {
    parse_from_str(alpha1).parse(input)
}

/// use nom_supreme::{parse_from_str, parser_ext::ParserExt};
pub fn payload_format(input: &str ) -> Res<&str, PayloadFormat> {
    parse_from_str(alpha1).parse(input)
}

pub fn payload_validator(input: &str ) -> Res<&str, PayloadValidator> {
    tuple(( call, opt(preceded(tag("+"), address ))))(input).map( |(next,(port_call,schema))| {
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



/// use nom_supreme::{parse_from_str, parser_ext::ParserExt};
pub fn payload_type_def(input: &str ) -> Res<&str, PayloadTypeDef> {
    tuple( (payload_kind, opt( payload_validation )))(input).map( |(next, (kind, validation)) | {

        (next, PayloadTypeDef {
            kind,
            validation
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
    (tuple( (skewer, tagged_payload_type_def))) (input).map( |(next,(label,type_def))| {

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
    delimited(tag("'"), address, tag("'"))(input).map(|(next, address)| {
        (next,
         PayloadValueSrc::Address(address))
    })
}

pub fn state_value(input: &str ) -> Res<&str, PayloadValueSrc> {
    address(input).map(|(next, address)| {
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

/*
/// use nom_supreme::{parse_from_str, parser_ext::ParserExt};
pub fn payload_assignment(input: &str ) -> Res<&str, PayloadAssignment> {
  tuple( (payload_type,opt( tuple( ( preceded(tag("~"), opt(parse_from_str ) ), opt(preceded(tag("~"),address)))))) )(input).map( |(next,(payload_type, validation)) | {

      let validation = match validation {
          None => {Option::None}
          Some((format, validator)) => {
              let format : Option<PayloadFormat> = match format {
                  None => { Option::None }
                  Some(format) => {
                     Option::Some(format)
                  }
              };

              let validator: Option<Address> = match validator {
                  None => { Option::None }
                  Some(valdator) => {
                      Option::Some(valdator)
                  }
              };


              Option::Some( PayloadValidation {
                 format,
                  validator
              });
          }
      };

      (next, PayloadAssignment{
          payload_ident: payload_type,
          validation
      } )

  })
}

 */

#[derive(strum_macros::Display,strum_macros::EnumString)]
pub enum PayloadFormat {
    #[strum(serialize = "json")]
    Json,
    #[strum(serialize = "image")]
    Image
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


pub struct PayloadTypeDef {
  pub kind: PayloadType,
  pub validation: Option<PayloadValidation>,
}

impl ToString for PayloadTypeDef {
    fn to_string(&self) -> String {
        match &self.validation {
            None => {
                format!("{}",self.kind.to_string() )
            }
            Some(validation) => {
                format!("{}{}",self.kind.to_string(),validation.to_string()  )
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



/// use nom_supreme::{parse_from_str, parser_ext::ParserExt};
pub fn push_block(input: &str) -> Res<&str, &str> {
    tag("[")(input)
}

pub fn block_content(input: &str) -> Res<&str,&str> {
    recognize(many1(alt((multispace1,alpha1)))) (input)
}


pub fn block_seg(input: &str) -> Res<&str, &str> {
    alt((block_content,tag("["), tag("]"))) (input)
}


pub fn block_seg2(input: &str) -> Res<&str, GroupToken> {
    parse_from_str(alt((block_content,tag("["), tag("]"))) ).parse(input)
}



pub fn selector(input: &str) -> Res<&str, GroupToken> {
    parse_from_str(alt((block_content,tag("["), tag("]"))) ).parse(input)
}


pub fn entity_def(input: &str) -> Res<&str, EntityKind> {
    parse_from_str(alpha1 ).parse(input)
}

#[derive(Debug,Clone,Eq,PartialEq,strum_macros::Display,strum_macros::EnumString)]
pub enum RcCommand{
    Create,
    Select
}




pub enum PipeSegEntry {
    Request,
    Create,
    Upload
}



#[derive(Debug,Clone,Eq,PartialEq)]
pub enum PipelineStop {
   Internal,
   Call(Call),
   Return
}



