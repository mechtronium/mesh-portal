use std::collections::HashMap;
use std::convert::TryInto;
use std::str::FromStr;

use nom::{AsChar, Err, InputTakeAtPosition, Parser, IResult, Compare, InputTake, InputLength, Offset, Slice};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha0, alpha1, alphanumeric0, alphanumeric1, digit0, digit1, multispace0, space0};
use nom::combinator::{all_consuming, not, opt, recognize};
use nom::error::{ErrorKind, ParseError, VerboseError};
use nom::multi::{separated_list1, separated_list0};
use nom::sequence::{delimited, preceded, tuple};
use nom_supreme::parse_from_str;

use mesh_portal_serde::version::latest::payload::{PrimitiveType, PayloadType};
use mesh_portal_serde::version::latest::payload::Primitive;
use mesh_portal_serde::version::latest::payload::Payload;

use crate::parse::{call, call_with_config, Res, path_regex, camel_case, camel_case_to_string};
use mesh_portal_serde::version::latest::error::Error;
use std::iter::Map;
use mesh_portal_serde::version::latest::util::{ValuePattern, RegexMatcher, StringMatcher};
use mesh_portal_serde::version::latest::payload::MapPattern;
use mesh_portal_serde::version::latest::payload::PayloadPattern;
use mesh_portal_serde::version::latest::payload::PayloadTypePattern;
use mesh_portal_serde::version::latest::payload::ListPattern;
use mesh_portal_serde::version::latest::payload::PayloadMap;
use mesh_portal_serde::version::latest::payload::CallWithConfig;
use mesh_portal_serde::version::latest::payload::Call;
use mesh_portal_serde::version::latest::payload::Range;
use mesh_portal_serde::version::latest::payload::PayloadFormat;
use std::ops::RangeTo;
use mesh_portal_serde::version::latest::generic::entity::request::{ReqEntity, Rc,Msg,Http};
use mesh_portal_serde::version::latest::payload::{PayloadDelivery, PayloadRef};
use mesh_portal_serde::version::v0_0_1::generic::id::Identifier;
use mesh_portal_serde::version::v0_0_1::id::Address;
use mesh_portal_serde::version::v0_0_1::util::ValueMatcher;
use mesh_portal_serde::version::latest::generic::payload::RcCommand;
use mesh_portal_serde::version::v0_0_1::generic::payload::{HttpMethod};

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

fn not_quote<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            (char_item == '"')
        },
        ErrorKind::AlphaNumeric,
    )
}


fn filename<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item.is_alpha()  || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}











#[derive(Eq,PartialEq)]
pub struct LabeledPrimitiveTypeDef {
    pub label: String,
    pub def: PrimitiveTypeDef
}

#[derive(Eq,PartialEq)]
pub struct PrimitiveTypeDef {
    pub primitive: PrimitiveType,
    pub format: Option<PayloadFormat>,
    pub verifier: Option<CallWithConfig>,
}


#[derive(Debug,Clone,strum_macros::Display,strum_macros::EnumString,Eq,PartialEq)]
pub enum Format{
    #[strum(serialize = "json")]
    Json,
    #[strum(serialize = "image")]
    Image
}

pub enum EntityPattern {
   Rc(RcPattern),
   Msg(MsgPattern),
   Http(HttpPattern)
}

impl <P> ValueMatcher<ReqEntity<P>> for EntityPattern {
    fn is_match( &self, entity: &ReqEntity<P> ) -> Result<(),Error>{
        match entity {
            ReqEntity::Rc(found) => {
                if let EntityPattern::Rc(pattern) = self {
                    pattern.is_match(found)
                } else {
                    Err(format!("Entity pattern mismatch. expected: '{}' found: '{}'", self.to_string(), found.to_string() ).into())
                }
            }
            ReqEntity::Msg(found) => {
                if let EntityPattern::Msg(pattern) = self {
                    pattern.is_match(found)
                } else {
                    Err(format!("Entity pattern mismatch. expected: '{}' found: '{}'", self.to_string(), found.to_string() ).into())
                }
            }
            ReqEntity::Http(found) => {
                if let EntityPattern::Http(pattern) = self {
                    pattern.is_match(found)
                } else {
                    Err(format!("Entity pattern mismatch. expected: '{}' found: '{}'", self.to_string(), found.to_string() ).into())
                }
            }
        }
    }
}

impl ToString for EntityPattern {
    fn to_string(&self) -> String {
        match self{
            EntityPattern::Rc(rc) => {
                rc.to_string()
            }
            EntityPattern::Msg(msg) => {
                msg.to_string()
            }
            EntityPattern::Http(http) => {
                http.to_string()
            }
        }
    }
}



pub struct RcPattern {
    pub command: ValuePattern<RcCommand>
}

impl ToString for RcPattern {
    fn to_string(&self) -> String {
        format!("Rc<{}>",self.command.to_string())
    }
}

impl <P> ValueMatcher<Rc<P>> for RcPattern {
    fn is_match(&self, found: &Rc<P>) -> Result<(), Error> {
        self.command.is_match(&found.command )
    }
}

pub struct MsgPattern{
    pub action: ValuePattern<StringMatcher>,
    pub path_regex: String
}

impl ToString for MsgPattern {
    fn to_string(&self) -> String {
        format!("Msg<{}>{}", self.action.to_string(),self.path_regex)
    }
}

impl <P> ValueMatcher<Msg<P>> for MsgPattern{
    fn is_match(&self, found: &Msg<P>) -> Result<(), Error> {
        self.action.is_match(&found.action )?;
        let matches = found.path.matches(&self.path_regex);
        if matches.count() > 0 {
            Ok(())
        } else {
            Err(format!("Could not match Msg path: '{}' with: '{}'", found.path, self.path_regex).into())
        }
    }
}


pub struct HttpPattern{
    pub method: ValuePattern<HttpMethod>,
    pub path_regex: String
}

impl ToString for HttpPattern {
    fn to_string(&self) -> String {
        format!("Http<{}>{}", self.method.to_string(),self.path_regex)
    }
}

impl <P> ValueMatcher<Http<P>> for HttpPattern{
    fn is_match(&self, found: &Http<P>) -> Result<(), Error> {

        self.method.is_match(&found.method)?;

        let matches = found.path.matches(&self.path_regex);
        if matches.count() > 0 {
            Ok(())
        } else {
            Err(format!("Could not match Msg path: '{}' with: '{}'", found.path, self.path_regex).into())
        }
    }
}
pub fn primitive(input: &str) -> Res<&str, PrimitiveType> {
     parse_from_str(recognize(alpha1) ).parse(input)
}

pub fn format(input: &str) -> Res<&str, PayloadFormat> {
    parse_from_str(recognize(alpha1) ).parse(input)
}



pub fn primitive_def(input: &str) -> Res<&str, PrimitiveTypeDef> {
    tuple( ( primitive, opt(preceded( tag("~"), opt(format)), ),opt(preceded(tag("~"), call_with_config), )  ) )(input).map( |(next,(primitive,format,verifier))| {
        (next,

         PrimitiveTypeDef {
             primitive,
             format: match format {
                 Some(Some(format)) => {
                     Some(format)
                 }
                 _ => Option::None,
             },
             verifier
         })

    })
}
pub fn consume_primitive_def(input: &str) -> Res<&str, PrimitiveTypeDef> {
    all_consuming(primitive_def)(input)
}



    pub fn labeled_primitive_def(input: &str) -> Res<&str, LabeledPrimitiveTypeDef> {
    tuple( ( skewer, delimited(tag("<"), primitive_def, tag(">") ) ))(input).map( |(next,(label,primitive_def))| {

        let labeled_def = LabeledPrimitiveTypeDef {
            label: label.to_string(),
            def: primitive_def
        };
        (next, labeled_def)
    })
}


#[derive(Clone,Eq,PartialEq)]
pub enum Block {
    Upload(UploadBlock),
    RequestPattern(PatternBlock),
    ResponsePattern(PatternBlock),
    Payload(Payload),
}


#[derive(Debug,Clone,Eq,PartialEq)]
pub struct UploadBlock{
    pub name: String
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub struct CreateBlock{
    pub payload: Payload
}


pub type PatternBlock = ValuePattern<PayloadPattern>;



pub fn digit_range( input: &str ) -> Res<&str,Range> {
  tuple( ( digit1, tag("-"), digit1 ) )(input).map( |(next,(min,_,max))| {
      let min :usize = usize::from_str(min ).expect("usize");
      let max:usize = usize::from_str(max).expect("usize");
      let range = Range::MinMax {min,max};

      (next,range)
  } )
}

pub fn exact_range (input: &str) -> Res<&str,Range> {
    digit1(input).map( |(next,exact)| {
        (next,
         Range::Exact(usize::from_str(exact).expect("expect to be able to change digit string into usize")) )
    } )
}






pub fn range( input: &str ) -> Res<&str,Range> {
    delimited(multispace0,opt(alt( (digit_range,exact_range) )),multispace0)(input).map(|(next,range)|{
        let range = match range {
            Some(range) => {
                range
            }
            None=>Range::Any
        };
            (next, range)
    })
}





pub fn primitive_data_struct( input: &str ) -> Res< &str, PayloadTypePattern> {
    primitive(input).map( |(next,primitive)| {
        (next, PayloadTypePattern::Primitive(primitive))
    } )
}


pub fn array_data_struct( input: &str ) -> Res< &str, PayloadTypePattern> {
    tuple( (primitive, delimited(tag("["), range, tag("]") ) ) )(input).map( |(next, (primitive,range))| {

        (next, PayloadTypePattern::List(ListPattern {
            primitive,
            range
        }))
    } )
}


pub fn map_entry_pattern_any(input: &str ) -> Res<&str, ValuePattern<MapEntryPattern>> {
    delimited( multispace0,tag("*"),multispace0 )(input).map( |(next,_)| {
        (next, ValuePattern::Any)
    })
}

pub fn map_entry_pattern(input: &str ) -> Res<&str, MapEntryPattern> {
    tuple((skewer, opt(delimited(tag("<"), payload_pattern, tag(">")))))(input).map(|(next,(key_con, payload_con))| {
        let payload_con = match payload_con {
            None => ValuePattern::Any,
            Some(payload_con) => payload_con
        };

       let map_entry_con = MapEntryPattern { key: key_con.to_string(), payload: payload_con };
        (next,map_entry_con)
    })
}


pub fn map_entry_patterns(input: &str ) -> Res<&str, Vec<MapEntryPattern>> {
    separated_list0( delimited(multispace0,tag(","),multispace0), map_entry_pattern )(input)
}

pub fn consume_map_entry_pattern(input: &str ) -> Res<&str, MapEntryPattern> {
    all_consuming(map_entry_pattern)(input)
}

pub fn required_map_entry_pattern( input: &str ) -> Res<&str, Vec<MapEntryPattern>> {
    delimited(tag("["), map_entry_patterns, tag("]"))(input).map( |(next,params)|{
        (next,params)
    } )
}

pub fn allowed_map_entry_pattern( input: &str ) -> Res<&str, ValuePattern<PayloadPattern>> {
    payload_pattern(input).map( |(next,con)|{
        (next,con)
    } )
}


//  [ required1<Bin>, required2<Text> ] *<Bin>
pub fn map_pattern_params(input: &str ) -> Res<&str, MapPattern> {
    tuple( (opt(map_entry_patterns),multispace0,opt(allowed_map_entry_pattern)))(input).map( |(next,(required,_,allowed)) | {

        let mut required_map = HashMap::new();
        match required {
            Option::Some(required) => {
                for require in required {
                    required_map.insert( require.key, require.payload );
                }
            }
            Option::None => {}
        }

        let allowed = match allowed {
            Some(allowed) => allowed,
            None => ValuePattern::None
        };

        let con = MapPattern::new(required_map, allowed );

        (next,con)

    } )
}

enum MapConParam{
    Required(Vec<ValuePattern<MapEntryPattern>>),
    Allowed(ValuePattern<PayloadPattern>)
}


// EXAMPLE:
//  Map { [ required1<Bin>, required2<Text> ] *<Bin> }
pub fn map_pattern(input: &str ) -> Res<&str, MapPattern> {
    tuple( (delimited(multispace0,tag("Map"),multispace0), opt(delimited(tag("{"),  delimited(multispace0,map_pattern_params, multispace0),tag("}") ) ) ) )(input).map( |(next, (_, entries))| {



        let mut entries = entries;
        let con = match entries {
            None => {
                MapPattern::any()
            },
            Some(con) => {
                con
            }
        };

        (next, con )
    } )
}

/*
fn value_pattern<I,O,E>(input: I) -> IResult<I, ValuePattern<O>, E>
    where
        I: InputTake + Clone + Compare<I>+ InputTakeAtPosition, <I as InputTakeAtPosition>::Item: AsChar + Clone,
        E: ParseError<I>

{
    alt((tag("*"),multispace0))(input).map( |(next,tag):(&str,&str)|{
        let rtn = match tag{
            "*" => ValuePattern::Any,
            _ => ValuePattern::None
        };
        (next,rtn)
    })
}*/

pub fn value_pattern<V>(input: &str, parser: fn(&str)->Res<&str,V> ) -> Res<&str,ValuePattern<V>> {

    let result  = parser(input);
    match result {
        Ok((next,v)) => {
            return Ok((next,ValuePattern::Pattern(v)));
        }
        Err(error) => {
            // do nothing
        }
    }


    alt((tag("*"),multispace0))(input).map( |(next,tag)|{
        let rtn = match tag{
            "*" => ValuePattern::Any,
            _ => ValuePattern::None
        };
        (next,rtn)
    })

}



/*
pub fn value_pattern_wrapper<'a,'b,V,F>( mut parser: F ) -> impl FnMut(&'a str) -> Res<&'b str,ValuePattern<V>>
  where F: 'a+ Parser<&'a str,ValuePattern<V>,VerboseError<&'b str>>
{
    move |input: &str| {
        parser.parse(input)
    }
}

 */

/*
pub fn value_pattern_wrapper<I:Clone, O, E: ParseError<I>, F>(
    mut first: F,
) -> impl FnMut(I) -> IResult<I, ValuePattern<O>, E>

    where
        F: Parser<I, ValuePattern<O>, E>,
        I: InputTake + Clone + Compare<I>+ InputTakeAtPosition, <I as InputTakeAtPosition>::Item: AsChar + Clone
{
    move |input: I| {
        //let result1 = value_pattern(input.clone());
        first.parse(input)//.or(result1)
    }
}

 */

/*
pub fn value_pattern_wrapper<O>(
    mut parser: F,
) -> impl FnMut(&str) -> Res<&str,ValuePattern<O>>
    where
        F: Parser<Res<&str,ValyePattern<O>>>,
{
    move |input: &str| {
        match parser.parse(input ).or( ) {
            Ok((i, out)) => {
                Ok((i, ValuePattern::Pattern(out)))
            }
            Err(e) => {
                value_pattern::<O>(input)
            }
        }
    }
}

 */


pub fn value_constrained_map_pattern(input: &str ) -> Res<&str, ValuePattern<MapPattern>> {
    value_pattern(input, map_pattern)
}

pub fn msg_action (input: &str) -> Res<&str,ValuePattern<StringMatcher>>{
  value_pattern( input, camel_case_to_string)
}

pub fn msg_pattern( input: &str ) -> Res<&str,MsgPattern> {
    tuple((tag("Msg"), delimited(tag("<"), msg_action, tag(">") ), opt(path_regex)))(input).map( |(next,(_,action,path_regex))| {
        let path_regex = match path_regex {
            None => {"*".to_string()}
            Some(path_regex) => {path_regex.to_string() }
        };
        let rtn = MsgPattern{
            action,
            path_regex: path_regex.to_string()
        };
        (next,rtn)
    } )
}

pub fn http_method( input: &str ) -> Res<&str,HttpMethod> {
    parse_from_str(alpha1).parse(input)
}

pub fn http_method_pattern( input: &str ) -> Res<&str,ValuePattern<HttpMethod>> {
    value_pattern(input, http_method )
}

pub fn http_pattern( input: &str ) -> Res<&str,HttpPattern> {
    tuple((tag("Http"), delimited(tag("<"), http_method_pattern, tag(">") ), opt(path_regex)))(input).map( |(next,(_,method,path_regex))| {
        let path_regex = match path_regex {
            None => {"*".to_string()}
            Some(path_regex) => {path_regex.to_string() }
        };
        let rtn = HttpPattern{
            method,
            path_regex: path_regex.to_string()
        };
        (next,rtn)
    } )
}

pub fn rc_command( input: &str ) -> Res<&str,RcCommand> {
    parse_from_str(alpha1).parse(input)
}

pub fn rc_command_pattern( input: &str ) -> Res<&str,ValuePattern<RcCommand>> {
    value_pattern(input, rc_command )
}
pub fn rc_pattern( input: &str ) -> Res<&str,RcPattern> {
    tuple((tag("Rc"), delimited(tag("<"), rc_command_pattern, tag(">") )))(input).map( |(next,(_,command))| {

        (next,RcPattern{ command })
    } )
}

pub fn map_pattern_payload_structure(input: &str ) -> Res<&str, PayloadTypePattern> {
    map_pattern(input).map( |(next,con)| {
        (next, PayloadTypePattern::Map(Box::new(con)))
    } )
}


pub fn payload_structure(input: &str ) -> Res< &str, PayloadTypePattern> {
    alt( (array_data_struct, primitive_data_struct,map_pattern_payload_structure))(input)
}

pub fn msg_entity_pattern( input: &str ) -> Res<&str,EntityPattern> {
    msg_pattern(input).map( |(next,pattern)| {
        (next,EntityPattern::Msg(pattern))
    } )
}
pub fn http_entity_pattern( input: &str ) -> Res<&str,EntityPattern> {
    http_pattern(input).map( |(next,pattern)| {
        (next,EntityPattern::Http(pattern))
    } )
}

pub fn rc_entity_pattern( input: &str ) -> Res<&str,EntityPattern> {
    rc_pattern(input).map( |(next,pattern)| {
        (next,EntityPattern::Rc(pattern))
    } )
}

pub fn entity_pattern( input: &str ) -> Res<&str,EntityPattern> {
    alt( (msg_entity_pattern,http_entity_pattern,rc_entity_pattern) )(input)
}



pub fn payload_structure_with_validation(input: &str ) -> Res< &str, PayloadPattern> {
    tuple( (payload_structure, opt(preceded(tag("~"), opt(format)), ), opt(preceded(tag("~"), call_with_config), )  ) )(input).map( |(next,(data,format,verifier))| {
        (next,

         PayloadPattern {
             structure: data,
             format: match format {
                 Some(Some(format)) => {
                     Some(format)
                 }
                 _ => Option::None,
             },
             validator: verifier
         })

    })
}

pub fn consume_payload_structure(input: &str) -> Res<&str, PayloadTypePattern> {
    all_consuming(payload_structure)(input)
}

pub fn consume_data_struct_def( input: &str) -> Res<&str, PayloadPattern> {
    all_consuming(payload_structure_with_validation)(input)
}


pub fn payload_pattern_any(input: &str ) -> Res<&str, ValuePattern<PayloadPattern>> {
    tag("*")(input).map( |(next,_)|{
        (next, ValuePattern::Any)
    } )
}

pub fn payload_pattern(input: &str ) -> Res<&str, ValuePattern<PayloadPattern>> {
    value_pattern(input, payload_structure_with_validation) .map( |(next,payload_pattern)|{
        (next, payload_pattern)
    } )
}

pub fn payload_patterns(input: &str ) -> Res<&str, ValuePattern<PayloadPattern>> {
    alt( (tag("*"), recognize(payload_structure_with_validation)) )(input).map( |(next,data)|{

        let data = match data{
            "*" => ValuePattern::Any,
            exact => ValuePattern::Pattern(payload_structure_with_validation(input).expect("recognize already passed this...").1)
        };
        (next,data)
    } )
}


pub fn pattern_block_empty(input: &str) -> Res<&str,PatternBlock> {
    multispace0(input).map( |(next,_)| {
        (input, PatternBlock::None )
    } )
}

pub fn pattern_block_any(input: &str) -> Res<&str,PatternBlock> {
    let (next,_)= delimited(multispace0,tag("*"),multispace0)(input)?;

        Ok((next, PatternBlock::Any))
}

pub fn pattern_block_def(input: &str) -> Res<&str,PatternBlock> {
    payload_structure_with_validation(input).map ( |(next,pattern)|{
        (next,PatternBlock::Pattern(pattern))
    })

}

fn insert_block_pattern(input: &str) -> Res<&str,UploadBlock> {
    delimited(multispace0,filename, multispace0 )(input).map( |(next,filename) | {
        (next,
        UploadBlock{
            name: filename.to_string()
        })
    } )
}



pub fn text_payload_block(input: &str ) -> Res<&str,Block> {
    delimited( tag("+["), tuple((multispace0,delimited(tag("\""), not_quote, tag("\"")),multispace0)), tag("]") )(input).map( |(next,(_,text,_))| {
        (next,Block::Payload(Payload::Primitive(Primitive::Text(text.to_string()))))
    })
}

pub fn upload_pattern_block(input: &str ) -> Res<&str,Block> {
    delimited( tag("^["), tuple((multispace0,filename,multispace0)), tag("]") )(input).map( |(next,(_,block,filename))| {

        (next,Block::Upload(UploadBlock{name:filename.to_string()}))
    })
}

pub fn request_pattern_block(input: &str ) -> Res<&str,Block> {
 delimited( tag("-["), tuple((multispace0,alt((pattern_block_empty,pattern_block_any,pattern_block_def)),multispace0)), tag("]") )(input).map( |(next,(_,block,_))| {
     (next,Block::RequestPattern(block))
 })
}

pub fn response_pattern_block(input: &str ) -> Res<&str,Block> {
    delimited( tag("=["), tuple((multispace0,alt((pattern_block_empty,pattern_block_any,pattern_block_def)),multispace0)), tag("]") )(input).map( |(next,(_,block,_))| {
        (next,Block::ResponsePattern(block))
    })
}

pub fn pipeline_block(input: &str ) -> Res<&str,Block> {
    alt( (request_pattern_block, response_pattern_block ) )(input)
}

pub fn consume_pipeline_block(input: &str ) -> Res<&str,Block> {
    all_consuming(pipeline_block)(input)
}

#[derive(Clone,Eq,PartialEq)]
pub struct MapEntryPattern {
    pub key: String,
    pub payload: ValuePattern<PayloadPattern>
}

#[cfg(test)]
pub mod test {
    use std::collections::HashMap;
    use std::str::FromStr;

    use anyhow::Error;
    use nom::combinator::all_consuming;


    use crate::pattern::{ListPattern, consume_payload_structure, consume_data_struct_def, consume_map_entry_pattern, consume_pipeline_block, consume_primitive_def, PayloadTypePattern, ValuePattern, PayloadPattern, Format, primitive, primitive_def, Range, MapEntryPattern, map_pattern_params, pattern_block_def};
    use mesh_portal_serde::version::latest::payload::{PrimitiveType, MapPattern};
    use mesh_portal_serde::version::latest::generic::payload::PayloadFormat;
    use crate::parse::call;

    #[test]
    pub fn test_call() -> Result<(),Error>{
        call("hello:kitty^Msg<Go>")?;
        call("hello:kitty^Msg<Go>/some-path")?;
        call("hello:kitty^Http<Get>/")?;
        Ok(())
    }


        #[test]
    pub fn test_primative() -> Result<(),Error>{

        assert!( consume_payload_structure("Text")?.1 == PayloadTypePattern::Primitive( PrimitiveType::Text ) );
        assert!( consume_payload_structure("Text[1-3]")?.1 == PayloadTypePattern::List( ListPattern { primitive: PrimitiveType::Text, range: Range::MinMax{min:1,max:3}} ) );
        assert!( consume_payload_structure("Text[]")?.1 == PayloadTypePattern::List( ListPattern { primitive: PrimitiveType::Text, range: Range::Any } ) );
        assert!( consume_data_struct_def("Text~json")?.1 == PayloadPattern { structure: PayloadTypePattern::Primitive(PrimitiveType::Text), format: Option::Some(PayloadFormat::Json), validator: Option::None }) ;
        assert!( consume_data_struct_def("Map")?.1 == PayloadPattern { structure: PayloadTypePattern::Map(Default::default()), format: Option::None, validator: Option::None }) ;
        assert!( consume_map_entry_pattern("key<Bin>")?.1== MapEntryPattern { key: "key".to_string(), payload: ValuePattern::Pattern(PayloadPattern { structure: PayloadTypePattern::Primitive(PrimitiveType::Bin), format: Option::None, validator: Option::None })});
        assert!( consume_map_entry_pattern("key<Bin~json>")?.1== MapEntryPattern { key: "key".to_string(), payload: ValuePattern::Pattern(PayloadPattern { structure: PayloadTypePattern::Primitive(PrimitiveType::Bin), format: Option::Some(PayloadFormat::Json), validator: Option::None })});
        consume_map_entry_pattern("key<Bin~json~verify:zoinks^Msg<Verify>>")?;
        assert!( consume_map_entry_pattern("key<*>")?.1== MapEntryPattern { key: "key".to_string(), payload: ValuePattern::Any });

        let mut map = HashMap::new();
        map.insert("first".to_string(), ValuePattern::Pattern(PayloadPattern { structure: PayloadTypePattern::Primitive(PrimitiveType::Text), format: Option::None, validator: Option::None }) );
        map.insert("last".to_string(), ValuePattern::Pattern(PayloadPattern { structure: PayloadTypePattern::Primitive(PrimitiveType::Text), format: Option::None, validator: Option::None }) );

        all_consuming(map_pattern_params)("first<Text>,last<Text>")?;
        let def = consume_data_struct_def("Map{first<Text>,last<Text>}")?;
        let def = consume_data_struct_def("Map { first<Text>, last<Text> }")?;
        let def = consume_data_struct_def("Map { first<Text~json>, last<Text> }")?;
        let def = consume_data_struct_def("Map { first<Text~json~verify:go^Msg<Go>>, last<Text> }")?;

        //somethign insane
        consume_data_struct_def("Map {first<Text~~verifier^Msg<Go>+std:1.0.0:/firstname.conf>,last<Text~~verifier^Msg<Go>+std:1.0.0:/lastname.conf> }~~verifier^Msg<Complete>")?;

        Ok(())
    }

    #[test]
    pub fn test_pattern_block() -> Result<(),Error>{
        pattern_block_def( "Text")?;

        consume_pipeline_block( "-[ Text ]")?;
        consume_pipeline_block( "-[ Text[] ]")?;
        consume_pipeline_block( "-[*]")?;
        consume_pipeline_block( "-[ * ]")?;
        consume_pipeline_block( "-[]")?;
        consume_pipeline_block( "-[   ]")?;
        Ok(())
    }
}
