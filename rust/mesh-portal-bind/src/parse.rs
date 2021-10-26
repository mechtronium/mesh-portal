use std::fmt;
use std::str::FromStr;

use anyhow::Error;
use nom::{AsChar, InputTakeAtPosition, IResult, Needed, InputTake, Compare, InputLength};
use nom::{Err, Parser};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until};
use nom::character::complete::{alpha0, alpha1, digit0, digit1, multispace0, multispace1, anychar};
use nom::combinator::{all_consuming, opt, recognize, not};
use nom::error::{context, ErrorKind, ParseError, VerboseError, VerboseErrorKind, FromExternalError};
use nom::multi::{fold_many0, separated_list0, many1, separated_list1};
use nom::sequence::{delimited, tuple, preceded, terminated};
use nom_supreme::{parse_from_str, ParserExt};

use crate::{parse, Bind, Request, Rc, Msg, Http};
use crate::token::{PayloadIdent, StackCmd};
use crate::symbol::{RootSelector, Address, PortCall};
use crate::token::generic::BlockPart;

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



fn skewer<T>(i: T) -> Res<T, T>
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
            any_resource_path_segment
        ),
    )(input).map( |(next,segments)|{
        let segments = segments.iter().map(|s|s.to_string()).collect();
        (next,segments)
    })
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

pub fn port_call(input: &str) -> Res<&str, PortCall> {
    parse_from_str( recognize(parse_port_call_path) ).parse(input)
}

pub fn parse_port_call_path(input: &str) -> Res<&str, (&str,&str)> {
    tuple( ( mechtron_path, preceded(tag("!"), skewer)) )(input)
}

pub fn port_call_parts(input: &str) -> Res<&str, (Address,&str)> {
    tuple( ( mechtron_address, preceded(tag("!"), skewer)) )(input)
}


pub fn consume_port_call(input: &str) -> Res<&str, PortCall> {
    all_consuming( port_call_parts )(input).map( |(next,(address,port))| {
        let port = port.to_string();
        ( next, PortCall {
            address,
            port
        })
    })
}

/// use nom_supreme::{parse_from_str, parser_ext::ParserExt};
pub fn payload_kind(input: &str ) -> Res<&str, PayloadIdent> {
    parse_from_str(alpha1).parse(input)
}

/// use nom_supreme::{parse_from_str, parser_ext::ParserExt};
pub fn payload_format(input: &str ) -> Res<&str, PayloadFormat> {
    parse_from_str(alpha1).parse(input)
}

pub fn payload_validator(input: &str ) -> Res<&str, PayloadValidator> {
    tuple(( address, opt(preceded(tag("+"), address ))))(input).map( |(next,(address,schema))| {
        (next,PayloadValidator{
            mechtron: address,
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
pub fn payload_assignment(input: &str ) -> Res<&str, PayloadAssignment> {
    tuple( (payload_kind, opt( payload_validation )))(input).map( |(next, (kind, validation)) | {

        ( next, PayloadAssignment{
            kind,
            validation
        })
    })
}

pub fn consume_payload_assignment( input: &str ) -> Res<&str, PayloadAssignment> {
    all_consuming(payload_assignment).parse(input)
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


pub struct PayloadValidation{
    pub format: Option<PayloadFormat>,
    pub validator: Option<PayloadValidator>
}


pub struct PayloadValidator {
    pub mechtron: Address,
    pub schema: Option<Address>
}

pub struct PayloadAssignment {
  pub kind: PayloadIdent,
  pub validation: Option<PayloadValidation>,
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








#[cfg(test)]
pub mod test {
    use std::str::FromStr;

    use nom::Err;
    use nom::error::VerboseError;

    use crate::parse::{payload_kind, block_seg, block_seg2, push_block, payload_assignment, PayloadValidator, PayloadFormat, consume_payload_assignment, MyError, PayloadValidation};
    use crate::token::PayloadIdent;

    /*
    #[test]
    pub fn test_bind() -> Result<(),anyhow::Error>{
        let data= " bind {   request {} }";
        let (input,bind) = bind(data)?;
        Ok(())
    }

     */


    #[test]
    pub fn block2() -> Result<(),anyhow::Error>{

        let mut i = "[xyz[abc]]";

        while i.len() > 0 {
            let (i2,t) = block_seg2(i)?;
            println!("{}",t.to_string() );
            i = i2;
        }

        Ok(())
    }


    /*
        #[test]
        pub fn group() -> Result<(),anyhow::Error>{
            let result = parse_group( "<Hello<Flerb>>" );
            match  &result {
                Ok((input, pay)) => {
                    println!("payload: {}", pay  );
                }
                Err(error) => {
                    println!("{}", error.to_string());
                }
            }
            result?;
            Ok(())
        }
        k
         */
    #[test]
    pub fn test() -> Result<(),anyhow::Error>{
        let (_,payload) = payload_kind("Bin")?;

       Ok(())
    }

    pub fn to_string<S: ToString>( s: Option<S> ) -> String {
        match s {
            None => {"None".to_string()}
            Some(s) => {s.to_string()}
        }
    }

    pub fn v_to_string( s: Option<PayloadValidation> ) -> String {
        match s {
            None => {"None".to_string()}
            Some(s) => {
                format!( "format: {}", to_string(s.format) )
            }
        }
    }

    pub fn test_payload_assignment( s: &str ) -> Result<(),MyError>{
        let (_,payload) = consume_payload_assignment(s)?;
        println!("'{}' type: '{}' validation {{ '{}' }}", s, payload.kind.to_string(), v_to_string(payload.validation));

        Ok(())
    }

    #[test]
    pub fn test2() -> Result<(),MyError>{

        test_payload_assignment("Bin")?;
        test_payload_assignment("Bin~image")?;
        test_payload_assignment("Bin~image~mechtron:from:heaven")?;
        test_payload_assignment("Bin~image~mechtron:from:heaven+some:artifact:1.0.0/blah.txt")?;
        test_payload_assignment("<Bin>")?;
        test_payload_assignment("<Bin~image~mechtron:from:heaven>")?;
        test_payload_assignment("label<Bin>")?;
        test_payload_assignment("label<Bin~image>")?;
        test_payload_assignment("label<Bin~image~mechtron:from:heaven>")?;

        Ok(())
    }

}