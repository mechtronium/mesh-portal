use std::collections::HashMap;
use std::convert::TryInto;
use std::str::FromStr;

use nom::{AsChar, Err, InputTakeAtPosition, Parser};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha0, alpha1, alphanumeric0, alphanumeric1, digit0, digit1, multispace0, space0};
use nom::combinator::{all_consuming, not, opt, recognize};
use nom::error::{ErrorKind, ParseError, VerboseError};
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, tuple};
use nom_supreme::parse_from_str;

use mesh_portal_serde::version::latest::payload::PrimitiveType;

use crate::parse::{call, call_with_config, Res};
use crate::symbol::{Address, Call, CallWithConfig};
use crate::token::PayloadPrimitiveType;

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

#[derive(Debug,Clone,Eq,PartialEq)]
pub enum Pattern {
    Any,
    Empty,
    Data(DataStructDef),
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub struct Array{
    pub primitive: PrimitiveType,
    pub range: Range,
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub enum DataStruct{
    PrimitiveType(PrimitiveType),
    Array(Array),
    Map(Map)
}



#[derive(Debug,Clone,Eq,PartialEq)]
pub struct DataStructDef {
    pub data: DataStruct,
    pub format: Option<Format>,
    pub verifier: Option<CallWithConfig>,
}


#[derive(Debug,Clone,Eq,PartialEq)]
pub struct Map{
    pub required: HashMap<String,DataStructConstraint>,
    pub allowed: Box<DataStructConstraint>
}

impl Map {
    pub fn empty() -> Self {
        Self {
            required: HashMap::new(),
            allowed: Box::new(DataStructConstraint::None)
        }
    }
}

impl Default for Map{
    fn default() -> Self {
        Map{
            required: HashMap::new(),
            allowed: Box::new(DataStructConstraint::Any)
        }
    }
}

#[derive(Eq,PartialEq)]
pub struct MapConstraint{
 pub label: LabelConstraint,
 pub data: DataStructConstraint
}

impl MapConstraint {

    pub fn has_label(&self) -> bool {
        match self.label {
            LabelConstraint::Exact(_) =>  true,
            _ => false
        }
    }
    pub fn is_exact(&self) -> bool {
      match self.label {
          LabelConstraint::Exact(_) => {
              match self.data {
                  DataStructConstraint::Exact(_) => true,
                  _ => false
              }
          }
          _ => false
      }
    }
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub enum DataStructConstraint{
    None,
    Any,
    Exact(DataStructDef)
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub enum LabelConstraint {
    Any,
    Exact(String)
}

#[derive(Eq,PartialEq)]
pub struct LabeledPrimitiveTypeDef {
    pub label: String,
    pub def: PrimitiveTypeDef
}

#[derive(Eq,PartialEq)]
pub struct PrimitiveTypeDef {
    pub primitive: PrimitiveType,
    pub format: Option<Format>,
    pub verifier: Option<CallWithConfig>,
}


#[derive(Debug,Clone,strum_macros::Display,strum_macros::EnumString,Eq,PartialEq)]
pub enum Format{
    #[strum(serialize = "json")]
    Json,
    #[strum(serialize = "image")]
    Image
}


pub fn primitive(input: &str) -> Res<&str, PrimitiveType> {
     parse_from_str(recognize(alpha1) ).parse(input)
}

pub fn format(input: &str) -> Res<&str, Format> {
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


#[derive(Debug,Clone,Eq,PartialEq)]
pub enum Block {
    RequestPattern(PatternBlock ),
    ResponsePattern(PatternBlock )
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub struct PatternBlock {
    pub pattern: Pattern
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub enum Range {
    MinMax { min: usize, max: usize },
    Exact(usize),
    Any
}

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

pub fn any_range(input: &str) -> Res<&str,Range> {
    let (next,scan)= tag("]")(input)?;
    if scan != "]" {
        Err(nom::Err::Error(VerboseError::from_error_kind(input, ErrorKind::TooLarge)))
    }
    else {
        Ok((input, Range::Any))
    }
}





pub fn range( input: &str ) -> Res<&str,Range> {
    alt( (digit_range,exact_range,any_range) )(input)
}





pub fn primitive_data_struct( input: &str ) -> Res< &str, DataStruct > {
    primitive(input).map( |(next,primitive)| {
        (next,DataStruct::PrimitiveType(primitive))
    } )
}


pub fn array_data_struct( input: &str ) -> Res< &str, DataStruct > {
    tuple( (primitive, delimited(tag("["), range, tag("]") ) ) )(input).map( |(next, (primitive,range))| {

        (next,DataStruct::Array(Array{
            primitive,
            range
        }))
    } )
}

pub fn map_constraint(input: &str ) -> Res<&str, MapConstraint> {
    alt( (recognize( tuple((label_constraint,delimited(tag("<"), data_constraint, tag(">"))))),tag("*")) )(input).map( | (next,con)| {
        let con = match con {
            "*" => MapConstraint {
                label: LabelConstraint::Any,
                data: DataStructConstraint::Any,
            },
            _ => {
                tuple((label_constraint,delimited(tag("<"), data_constraint, tag(">"))))(input).map( | (next,(label,data))| {

                    (next, MapConstraint{
                        label,
                        data
                    })
                } ).expect("expected this to work since Recognize passed").1
            }
        };

        (next,con)
    } )
}

pub fn consume_map_constraint(input: &str ) -> Res<&str, MapConstraint> {
    all_consuming(map_constraint)(input)
}


pub fn any_map_con(input: &str) -> Res<&str,Map> {
    let (next,scan)= tag("]")(input)?;
    if scan != "]" {
        Err(nom::Err::Error(VerboseError::from_error_kind(input, ErrorKind::TooLarge)))
    }
    else {
        Ok((input, Map::default() ))
    }
}

pub fn sep_map_con(input: &str) -> Res<&str,Map> {
    separated_list1( tag(","), map_constraint  )(input).map( |(next,cons)|{
        let mut map = Map::empty();
        for con in cons {
            match con.label{
                LabelConstraint::Any => {
                    map.allowed = Box::new(con.data);
                }
                LabelConstraint::Exact(label) => {
                    map.required.insert(label, con.data);
                }

            }
        }

        (next, map)
    } )
}

/*
pub fn sep_list_map_con(input: &str) -> Res<&str,Map> {
  separated_list1( tag(","),  )
}

pub fn map_constraints( input: &str ) -> Res<&str,Map> {
    alt( (any_map_con) )(input)
}

 */

pub fn map_constraints( input: &str ) -> Res<&str,Map> {
    alt( (any_map_con,sep_map_con) )(input)
}
pub fn map_data_struct( input: &str ) -> Res<&str,DataStruct> {
    tuple( (tag("Map"), opt(delimited(tag("["), map_constraints, tag("]") ) ) ) )(input).map( |(next, (primitive, map))| {

        let map = match map {
            None => {
                Map::default()
            }
            Some(map) => {
                map
            }
        };

        (next,DataStruct::Map(map))
    } )
}

pub fn data_struct( input: &str ) -> Res< &str, DataStruct > {
    alt( (array_data_struct, primitive_data_struct,map_data_struct ))(input)
}

pub fn data_struct_def( input: &str ) -> Res< &str, DataStructDef > {
    tuple( ( data_struct, opt(preceded( tag("~"), opt(format)), ),opt(preceded(tag("~"), call_with_config), )  ) )(input).map( |(next,(data,format,verifier))| {
        (next,

         DataStructDef{
             data,
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

pub fn consume_data_struct( input: &str) -> Res<&str, DataStruct> {
    all_consuming(data_struct)(input)
}

pub fn consume_data_struct_def( input: &str) -> Res<&str, DataStructDef> {
    all_consuming(data_struct_def)(input)
}

pub fn label_constraint(input: &str ) -> Res<&str, LabelConstraint> {
    alt( (tag("*"), skewer ) )(input).map( | (next,label)|{

        let label = match label {
            "*" => LabelConstraint::Any,
            exact => LabelConstraint::Exact(exact.to_string())
        };
        (next,label)
    } )
}

pub fn data_constraint(input: &str ) -> Res<&str, DataStructConstraint> {
    alt( (tag("*"), recognize( data_struct_def )) )(input).map( | (next,data)|{

        let data = match data{
            "*" => DataStructConstraint::Any,
            exact => DataStructConstraint::Exact(data_struct_def(input).expect("recognize already passed this...").1)
        };
        (next,data)
    } )
}


pub fn pattern_block_empty(input: &str) -> Res<&str,PatternBlock> {
    let (next,scan)= tag("]")(input)?;

    if scan != "]" {
        Err(nom::Err::Error(VerboseError::from_error_kind(input, ErrorKind::TooLarge)))
    }
    else {
        Ok((input, PatternBlock{
            pattern: Pattern::Empty
        }))
    }
}

pub fn pattern_block_any(input: &str) -> Res<&str,PatternBlock> {
    let (next,_)= tag("*")(input)?;

        Ok((next, PatternBlock{
            pattern: Pattern::Any
        }))
}

pub fn pattern_block_def(input: &str) -> Res<&str,PatternBlock> {
    let (next,data)= data_struct_def(input)?;

    Ok((next, PatternBlock{
        pattern: Pattern::Data(data)
    }))
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


#[cfg(test)]
pub mod test {
    use std::collections::HashMap;
    use std::str::FromStr;

    use anyhow::Error;
    use nom::combinator::all_consuming;

    use mesh_portal_serde::version::v0_0_1::PrimitiveType;

    use crate::pattern::{Array, consume_data_struct, consume_data_struct_def, consume_map_constraint, consume_pipeline_block, consume_primitive_def, DataStruct, DataStructConstraint, DataStructDef, Format, LabelConstraint, Map, MapConstraint, primitive, primitive_def, Range};
    use crate::symbol::{Address, Call, CallWithConfig};
    use mesh_portal_serde::version::latest::payload::PrimitiveType;

    #[test]
    pub fn test_primative() -> Result<(),Error>{
        assert!( consume_data_struct("Text")?.1 == DataStruct::PrimitiveType( PrimitiveType::Text ) );
        assert!( consume_data_struct("Text[]")?.1 == DataStruct::Array( Array { primitive: PrimitiveType::Text, range: Range::Any } ) );
        assert!( consume_data_struct("Text[1-3]")?.1 == DataStruct::Array( Array { primitive: PrimitiveType::Text, range: Range::MinMax{min:1,max:3}} ) );
        assert!( consume_data_struct_def("Text~json")?.1 == DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::Some(Format::Json),verifier: Option::None }) ;
/*        assert!( consume_data_struct_def("Text~json~verifier!go")?.1 == DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::Some(Format::Json),verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::None})}) ;
        assert!( consume_data_struct_def("Text~~verifier!go")?.1 == DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::None,verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::None})}) ;
        assert!( consume_data_struct_def("Text[]~json~verifier!go")?.1 == DataStructDef{data:DataStruct::Array(Array{primitive: PrimitiveType::Text,range:Range::Any}), format: Option::Some(Format::Json),verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::None})}) ;
        assert!( consume_data_struct_def("Text~json~verifier!go+config:1.0.0:/some-file.conf")?.1 == DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::Some(Format::Json),verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::Some(Address::from_str("config:1.0.0:/some-file.conf")?)})}) ;*/
        assert!( consume_data_struct_def("Map")?.1 == DataStructDef{data:DataStruct::Map(Map::default()), format: Option::None,verifier: Option::None }) ;
        assert!( consume_data_struct_def("Map[]")?.1 == DataStructDef{data:DataStruct::Map(Map::default()), format: Option::None,verifier: Option::None }) ;


        assert!( consume_map_constraint("label<Bin>")?.1==MapConstraint{ label: LabelConstraint::Exact("label".to_string()), data: DataStructConstraint::Exact(DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Bin), format: Option::None,verifier: Option::None })});
        assert!( consume_map_constraint("label<Bin~json>")?.1==MapConstraint{ label: LabelConstraint::Exact("label".to_string()), data: DataStructConstraint::Exact(DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Bin), format: Option::Some(Format::Json),verifier: Option::None })});
        assert!( consume_map_constraint("*")?.1==MapConstraint{ label: LabelConstraint::Any, data: DataStructConstraint::Any });
        assert!( consume_map_constraint("*<*>")?.1==MapConstraint{ label: LabelConstraint::Any, data: DataStructConstraint::Any });
        assert!( consume_map_constraint("*<Bin>")?.1==MapConstraint{ label: LabelConstraint::Any, data: DataStructConstraint::Exact(DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Bin), format: Option::None,verifier: Option::None })});
        assert!( consume_map_constraint("label<*>")?.1==MapConstraint{ label: LabelConstraint::Exact("label".to_string()), data: DataStructConstraint::Any });

        let mut map = HashMap::new();
        map.insert( "first".to_string(), DataStructConstraint::Exact(DataStructDef { data: DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::None, verifier: Option::None }) );
        map.insert( "last".to_string(), DataStructConstraint::Exact(DataStructDef { data: DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::None, verifier: Option::None }) );

        let def = consume_data_struct_def("Map[first<Text>,last<Text>]")?;
        println!("{:?}",def);

        assert!(consume_data_struct_def("Map[first<Text>,last<Text>]")?.1==DataStructDef{format: Option::None, verifier:Option::None, data:DataStruct::Map(Map{
            required: map.clone(),
            allowed: Box::new(DataStructConstraint::None)
        })});

        assert!(consume_data_struct_def("Map[first<Text>,last<Text>,*<Bin>]")?.1==DataStructDef{format: Option::None, verifier:Option::None, data:DataStruct::Map(Map{
            required: map,
            allowed: Box::new(DataStructConstraint::Exact(DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Bin),verifier:Option::None,format:Option::None}))
        })});


        //somethign insane
        consume_data_struct_def("Map[first<Text~~verifier!go+std:1.0.0:/firstname.conf>,last<Text~~verifier!go+std:1.0.0:/lastname.conf>]~~verifier!complete")?;

        Ok(())
    }

    #[test]
    pub fn test_pattern_block() -> Result<(),Error>{

        consume_pipeline_block( "-[ Text ]")?;
        consume_pipeline_block( "-[ Text[] ]")?;
        consume_pipeline_block( "-[*]")?;
        consume_pipeline_block( "-[ * ]")?;
        consume_pipeline_block( "-[]")?;
        consume_pipeline_block( "-[   ]")?;
        Ok(())
    }
}
