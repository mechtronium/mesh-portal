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

use mesh_portal_serde::version::latest::payload::{PrimitiveType, PayloadType};
use mesh_portal_serde::version::latest::payload::Primitive;
use mesh_portal_serde::version::latest::payload::Payload;
use mesh_portal_serde::version::latest::payload::MapConstraints;
use mesh_portal_serde::version::latest::payload::ValueConstraint;

use crate::parse::{call, call_with_config, Res, MapEntryConstraint};
use crate::symbol::{Address, Call, CallWithConfig};
use mesh_portal_serde::version::latest::error::Error;
use std::iter::Map;
use mesh_portal_serde::version::v0_0_1::generic::payload::ValuePattern;

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

#[derive(Debug,Clone,Eq,PartialEq)]
pub enum PayloadPattern {
    Any,
    Empty,
    Structure(PayloadStructureWithValidation),
}

impl PayloadPattern {
    pub fn is_match( &self, payload: &Payload ) -> Result<(),Error> {
        if self == Self::Any {
           Ok(())
        }
        match self {
            PayloadPattern::Any => {
                Ok(())
            }
            PayloadPattern::Empty => {
                if payload == Payload::Empty {
                    Ok(())
                } else {
                    Err("expected Empty payload".into())
                }
            }
            PayloadPattern::Structure(data) => {
                match &data.structure {
                    PayloadStructure::Primitive(primitive_type) => {
                        if let Payload::Primitive(primitive) = payload {
                            primitive_type.is_match( primitive )
                        } else {
                            Err("expected single primitive".into())
                        }
                    }
                    PayloadStructure::List(list_type) => {
                        if let Payload::List(list) = payload {
                           if list.first().is_none() {
                               Ok(())
                           }  else {
                               list_type.primitive.is_match(list.first().expect("expected first"))
                           }
                        } else {
                            Err("expected primitive[] list".into())
                        }
                    }
                    PayloadStructure::Map(map_type) => {

                        if let Payload::Map(map) = payload {
                            for (key,structure) in &map_type.required {
                                if !map.contains_key(key) {
                                    Err(format!("missing required Map key: '{}' ",key).into())
                                }
                                match structure {
                                    ValueConstraint::Any => {
                                        // so far so good
                                    }
                                    ValueConstraint::Pattern(exact) => {
                                       let payload = map.get(key).expect("expected payload entry");
                                    }
                                    ValueConstraint::None => {

                                    }
                                }
                            }
                       } else {
                            Err("expected Map Payload".into())
                        }
                    }
                    PayloadStructure::Empty => {
                        if let Payload::Empty = payload {
                            Ok(())
                        } else {
                            Err("expected Empty Payload".into())
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub struct List {
    pub primitive: PrimitiveType,
    pub range: Range,
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub enum PayloadStructure {
    Empty,
    Primitive(PrimitiveType),
    List(List),
    Map(Box<MapConstraints<PayloadStructure>>)
}

impl ValuePattern<PayloadType> for PayloadStructure {
    fn is_match(&self, x: &PayloadType) -> Result<(), Error> {
        match self {
            PayloadStructure::Empty => {
                if PayloadType::Empty == *x {
                    Ok(())
                } else {
                    Err("expected Empty payload".into())
                }
            }
            PayloadStructure::Primitive(primitive) => {
               if let PayloadType::Primitive(found) = x {
                    if found == primitive {
                        Ok(())
                    } else {
                        Err(format!("primitive Payload expected: '{}' found: '{}'", primitive.to_string(), found.to_string() ).into())
                    }
                }  else {

                   Err(format!("Payload expected: '{}' found: '{}'", primitive.to_string(), x.to_string() ).into())
               }
            }
            PayloadStructure::List(list) => {
                if let PayloadType::List(found) = x {
                    if *found == list.primitive {
                        Ok(())
                    } else {
                        Err(format!("Payload expected: '{}[]' found: '{}[]'", primitive.to_string(), found.to_string() ).into())
                    }
                }  else {
                    Err(format!("Payload expected: '{}[]' found: '{}'", primitive.to_string(), x.to_string() ).into())
                }
            }
            PayloadStructure::Map(map) => {
                if let PayloadType::Map(found) = x {
                    map.is_match(found)?
                } else {
                    Err(format!("Payload expected: '{}' found: '{}'", map.to_string(), x.to_string() ).into())
                }
            }
        }
    }
}

impl PayloadStructure {
    pub fn payload_type(&self) -> PayloadType {
        match self {
            PayloadStructure::Primitive(primitive) => {
                PayloadType::Primitive(primitive.clone())
            }
            PayloadStructure::List(list) => {
                PayloadType::List(list.primitive.clone())
            }
            PayloadStructure::Map(map) => {
                for (key,p) in map.allowed {
unimplemented!()
                }
            }
            PayloadStructure::Empty => {

unimplemented!()
            }
        }
    }
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub struct PayloadStructureWithValidation {
    pub structure: PayloadStructure,
    pub format: Option<Format>,
    pub validator: Option<CallWithConfig>,
}

impl PayloadStructureWithValidation {
    pub fn is_match( &self, primitive: Primitive ) {

    }
}




#[derive(Debug,Clone,Eq,PartialEq)]
pub enum KeyConstraint {
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


#[derive(Debug,Clone,Eq,PartialEq)]
pub struct PatternBlock {
    pub pattern: PayloadPattern
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





pub fn primitive_data_struct( input: &str ) -> Res< &str, PayloadStructure> {
    primitive(input).map( |(next,primitive)| {
        (next, PayloadStructure::Primitive(primitive))
    } )
}


pub fn array_data_struct( input: &str ) -> Res< &str, PayloadStructure> {
    tuple( (primitive, delimited(tag("["), range, tag("]") ) ) )(input).map( |(next, (primitive,range))| {

        (next, PayloadStructure::List(List {
            primitive,
            range
        }))
    } )
}

pub fn map_constraint(input: &str ) -> Res<&str, MapEntryConstraint> {
    alt( (recognize( tuple((label_constraint,delimited(tag("<"), data_constraint, tag(">"))))),tag("*")) )(input).map( | (next,con)| {
        let con = match con {
            "*" => MapEntryConstraint {
                key: KeyConstraint::Any,
                data: ValueConstraint::Any,
            },
            _ => {
                tuple((label_constraint,delimited(tag("<"), data_constraint, tag(">"))))(input).map( | (next,(label,data))| {

                    (next, MapEntryConstraint {
                        key: label,
                        data
                    })
                } ).expect("expected this to work since Recognize passed").1
            }
        };

        (next,con)
    } )
}

pub fn consume_map_constraint(input: &str ) -> Res<&str, MapEntryConstraint> {
    all_consuming(map_constraint)(input)
}



pub fn any_map_con(input: &str) -> Res<&str, MapConstraints> {
    let (next,scan)= tag("[")(input)?;
    if scan != "]" {
        Err(nom::Err::Error(VerboseError::from_error_kind(input, ErrorKind::TooLarge)))
    }
    else {
        Ok((input, MapConstraints::default() ))
    }
}

pub fn sep_map_con(input: &str) -> Res<&str, MapConstraints> {
    separated_list1( tag(","), map_constraints  )(input).map( |(next,cons)|{
        let mut map = MapConstraints::empty();
        for con in cons {
            match con.key {
                KeyConstraint::Any => {
                    map.allowed = Box::new(con.data);
                }
                KeyConstraint::Exact(label) => {
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

pub fn map_constraints( input: &str ) -> Res<&str, MapConstraints> {
    alt( (any_map_con,sep_map_con) )(input)
}
pub fn map_data_struct( input: &str ) -> Res<&str, PayloadStructure> {
    tuple( (tag("Map"), opt(delimited(tag("["), map_constraints, tag("]") ) ) ) )(input).map( |(next, (primitive, map))| {

        let map = match map {
            None => {
                MapConstraints::default()
            }
            Some(map) => {
                map
            }
        };

        (next, PayloadStructure::Map(map))
    } )
}

pub fn data_struct( input: &str ) -> Res< &str, PayloadStructure> {
    alt( (array_data_struct, primitive_data_struct,map_data_struct ))(input)
}

pub fn data_struct_def( input: &str ) -> Res< &str, PayloadStructureWithValidation> {
    tuple( ( data_struct, opt(preceded( tag("~"), opt(format)), ),opt(preceded(tag("~"), call_with_config), )  ) )(input).map( |(next,(data,format,verifier))| {
        (next,

         PayloadStructureWithValidation {
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

pub fn consume_data_struct( input: &str) -> Res<&str, PayloadStructure> {
    all_consuming(data_struct)(input)
}

pub fn consume_data_struct_def( input: &str) -> Res<&str, PayloadStructureWithValidation> {
    all_consuming(data_struct_def)(input)
}

pub fn label_constraint(input: &str ) -> Res<&str, KeyConstraint> {
    alt( (tag("*"), skewer ) )(input).map( | (next,label)|{

        let label = match label {
            "*" => KeyConstraint::Any,
            exact => KeyConstraint::Exact(exact.to_string())
        };
        (next,label)
    } )
}

pub fn data_constraint(input: &str ) -> Res<&str, ValueConstraint> {
    alt( (tag("*"), recognize( data_struct_def )) )(input).map( | (next,data)|{

        let data = match data{
            "*" => ValueConstraint::Any,
            exact => ValueConstraint::Pattern(data_struct_def(input).expect("recognize already passed this...").1)
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
            pattern: PayloadPattern::Empty
        }))
    }
}

pub fn pattern_block_any(input: &str) -> Res<&str,PatternBlock> {
    let (next,_)= tag("*")(input)?;

        Ok((next, PatternBlock{
            pattern: PayloadPattern::Any
        }))
}

pub fn pattern_block_def(input: &str) -> Res<&str,PatternBlock> {
    let (next,data)= data_struct_def(input)?;

    Ok((next, PatternBlock{
        pattern: PayloadPattern::Structure(data)
    }))
}

fn upload_block_payload(input: &str) -> Res<&str,UploadBlock> {
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


#[cfg(test)]
pub mod test {
    use std::collections::HashMap;
    use std::str::FromStr;

    use anyhow::Error;
    use nom::combinator::all_consuming;


    use crate::pattern::{List, consume_data_struct, consume_data_struct_def, consume_map_constraint, consume_pipeline_block, consume_primitive_def, PayloadStructure, ValueConstraint, PayloadStructureWithValidation, Format, KeyConstraint, MapConstraints, MapEntryConstraint, primitive, primitive_def, Range};
    use crate::symbol::{Address, Call, CallWithConfig};
    use mesh_portal_serde::version::latest::payload::PrimitiveType;

    #[test]
    pub fn test_primative() -> Result<(),Error>{
        assert!( consume_data_struct("Text")?.1 == PayloadStructure::Primitive( PrimitiveType::Text ) );
        assert!( consume_data_struct("Text[]")?.1 == PayloadStructure::List( List { primitive: PrimitiveType::Text, range: Range::Any } ) );
        assert!( consume_data_struct("Text[1-3]")?.1 == PayloadStructure::List( List { primitive: PrimitiveType::Text, range: Range::MinMax{min:1,max:3}} ) );
        assert!( consume_data_struct_def("Text~json")?.1 == PayloadStructureWithValidation { structure: PayloadStructure::Primitive(PrimitiveType::Text), format: Option::Some(Format::Json), validator: Option::None }) ;
/*        assert!( consume_data_struct_def("Text~json~verifier!go")?.1 == DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::Some(Format::Json),verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::None})}) ;
        assert!( consume_data_struct_def("Text~~verifier!go")?.1 == DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::None,verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::None})}) ;
        assert!( consume_data_struct_def("Text[]~json~verifier!go")?.1 == DataStructDef{data:DataStruct::Array(Array{primitive: PrimitiveType::Text,range:Range::Any}), format: Option::Some(Format::Json),verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::None})}) ;
        assert!( consume_data_struct_def("Text~json~verifier!go+config:1.0.0:/some-file.conf")?.1 == DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::Some(Format::Json),verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::Some(Address::from_str("config:1.0.0:/some-file.conf")?)})}) ;*/
        assert!( consume_data_struct_def("Map")?.1 == PayloadStructureWithValidation { structure: PayloadStructure::Map(MapConstraints::default()), format: Option::None, validator: Option::None }) ;
        assert!( consume_data_struct_def("Map[]")?.1 == PayloadStructureWithValidation { structure: PayloadStructure::Map(MapConstraints::default()), format: Option::None, validator: Option::None }) ;


        assert!( consume_map_constraint("label<Bin>")?.1== MapEntryConstraint { key: KeyConstraint::Exact("label".to_string()), data: ValueConstraint::Pattern(PayloadStructureWithValidation { structure: PayloadStructure::Primitive(PrimitiveType::Bin), format: Option::None, validator: Option::None })});
        assert!( consume_map_constraint("label<Bin~json>")?.1== MapEntryConstraint { key: KeyConstraint::Exact("label".to_string()), data: ValueConstraint::Pattern(PayloadStructureWithValidation { structure: PayloadStructure::Primitive(PrimitiveType::Bin), format: Option::Some(Format::Json), validator: Option::None })});
        assert!( consume_map_constraint("*")?.1== MapEntryConstraint { key: KeyConstraint::Any, data: ValueConstraint::Any });
        assert!( consume_map_constraint("*<*>")?.1== MapEntryConstraint { key: KeyConstraint::Any, data: ValueConstraint::Any });
        assert!( consume_map_constraint("*<Bin>")?.1== MapEntryConstraint { key: KeyConstraint::Any, data: ValueConstraint::Pattern(PayloadStructureWithValidation { structure: PayloadStructure::Primitive(PrimitiveType::Bin), format: Option::None, validator: Option::None })});
        assert!( consume_map_constraint("label<*>")?.1== MapEntryConstraint { key: KeyConstraint::Exact("label".to_string()), data: ValueConstraint::Any });

        let mut map = HashMap::new();
        map.insert("first".to_string(), ValueConstraint::Pattern(PayloadStructureWithValidation { structure: PayloadStructure::Primitive(PrimitiveType::Text), format: Option::None, validator: Option::None }) );
        map.insert("last".to_string(), ValueConstraint::Pattern(PayloadStructureWithValidation { structure: PayloadStructure::Primitive(PrimitiveType::Text), format: Option::None, validator: Option::None }) );

        let def = consume_data_struct_def("Map[first<Text>,last<Text>]")?;
        println!("{:?}",def);

        assert!(consume_data_struct_def("Map[first<Text>,last<Text>]")?.1== PayloadStructureWithValidation {format: Option::None, validator:Option::None, structure: PayloadStructure::Map(MapConstraints {
            required: map.clone(),
            allowed: Box::new(ValueConstraint::None)
        })});

        assert!(consume_data_struct_def("Map[first<Text>,last<Text>,*<Bin>]")?.1== PayloadStructureWithValidation {format: Option::None, validator:Option::None, structure: PayloadStructure::Map(MapConstraints {
            required: map,
            allowed: Box::new(ValueConstraint::Pattern(PayloadStructureWithValidation { structure: PayloadStructure::Primitive(PrimitiveType::Bin), validator:Option::None,format:Option::None}))
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
