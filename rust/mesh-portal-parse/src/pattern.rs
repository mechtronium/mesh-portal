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

use crate::parse::{call, call_with_config, Res};
use mesh_portal_serde::version::latest::error::Error;
use std::iter::Map;
use mesh_portal_serde::version::latest::util::ValueConstraint;
use mesh_portal_serde::version::latest::payload::MapConstraints;
use mesh_portal_serde::version::latest::payload::PayloadStructureAndValidation;
use mesh_portal_serde::version::latest::payload::PayloadStructure;
use mesh_portal_serde::version::latest::payload::PayloadListConstraints;
use mesh_portal_serde::version::latest::payload::PayloadMap;
use mesh_portal_serde::version::latest::payload::CallWithConfig;
use mesh_portal_serde::version::latest::payload::Call;
use mesh_portal_serde::version::latest::payload::Range;
use mesh_portal_serde::version::latest::payload::PayloadFormat;
use std::ops::RangeTo;

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
    Structure(PayloadStructureAndValidation),
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

        (next, PayloadStructure::List(PayloadListConstraints {
            primitive,
            range
        }))
    } )
}


pub fn map_entry_constraint_any(input: &str ) -> Res<&str, ValueConstraint<MapEntryConstraint>> {
    delimited( multispace0,tag("*"),multispace0 )(input).map( |(next,_)| {
        (next, ValueConstraint::Any)
    })
}

pub fn map_entry_constraint(input: &str ) -> Res<&str, MapEntryConstraint> {
    tuple((skewer, opt(delimited(tag("<"), payload_constraint, tag(">")))))(input).map(|(next,(key_con, payload_con))| {
        let payload_con = match payload_con {
            None => ValueConstraint::Any,
            Some(payload_con) => payload_con
        };

       let map_entry_con = MapEntryConstraint { key: key_con.to_string(), payload: payload_con };
        (next,map_entry_con)
    })
}


pub fn map_entry_constraints(input: &str ) -> Res<&str, Vec<MapEntryConstraint>> {
    separated_list0( delimited(multispace0,tag(","),multispace0), map_entry_constraint )(input)
}

pub fn consume_map_entry_constraint(input: &str ) -> Res<&str, Vec<MapEntryConstraint>> {
    all_consuming(map_entry_constraints)(input)
}

pub fn required_map_entry_constraints( input: &str ) -> Res<&str, Vec<MapEntryConstraint>> {
    delimited(tag("["), map_entry_constraints, tag("]"))(input).map( |(next,params)|{
        (next,params)
    } )
}

pub fn allowed_map_entry_constraints( input: &str ) -> Res<&str, ValueConstraint<PayloadStructureAndValidation>> {
    payload_constraint(input).map( |(next,con)|{
        (next,con)
    } )
}


//  [ required1<Bin>, required2<Text> ] *<Bin>
pub fn map_constraints_params(input: &str ) -> Res<&str, MapConstraints> {
    tuple( (opt(required_map_entry_constraints),multispace0,opt(allowed_map_entry_constraints)))(input).map( |(next,(required,_,allowed)) | {

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
            None => ValueConstraint::None
        };

        let con = MapConstraints::new( required_map, allowed );

        (next,con)

    } )
}

enum MapConParam{
    Required(Vec<ValueConstraint<MapEntryConstraint>>),
    Allowed(ValueConstraint<PayloadStructureAndValidation>)
}


// EXAMPLE:
//  Map { [ required1<Bin>, required2<Text> ] *<Bin> }
pub fn map_constraints(input: &str ) -> Res<&str, MapConstraints> {
    tuple( (tag("Map"), opt(delimited(delimited(multispace0,tag("{"),multispace0),  map_constraints_params,delimited(multispace0,tag("}"),multispace0) ) ) ) )(input).map( |(next, (_, entries))| {

        let mut entries = entries;
        let con = match entries {
            None => {
                MapConstraints::any()
            },
            Some(con) => {
                con
            }
        };

        (next, con)
    } )
}

fn value_constraint<I: Clone + Offset + Slice<RangeTo<usize>>, O, E: ParseError<I>>(input: &str ) -> IResult<I, ValueConstraint<O>, E> {
    alt((tag("Any"),tag("None")))(input).map( |(next,tag)|{
        let rtn = match tag{
            "Any" => ValueConstraint::Any,
            "None" => ValueConstraint::None,
            _ => panic!("The only valid ValueConstraitn values should be Any or None")
        };
        (next,rtn)
    })
}

pub fn value_constraint_wrapper<I: Clone + Offset + Slice<RangeTo<usize>>, O, E: ParseError<I>, F>(
    mut parser: F,
) -> impl FnMut(I) -> IResult<I, ValueConstraint<O>, E>
    where
        F: Parser<I, O, E>,
{
    move |input: I| {
        let i = input.clone();
        match parser.parse(i) {
            Ok((i, out)) => {
                Ok((i, ValueConstraint::Pattern(out)))
            }
            Err(e) => {
                value_constraint(input)
            }
        }
    }
}


pub fn value_constrained_map_constraints(input: &str ) -> Res<&str, ValueConstraint<MapConstraints>> {
    value_constraint_wrapper(map_constraints)(input)
}

pub fn map_constraints_payload_structure(input: &str ) -> Res<&str, PayloadStructure> {
    map_constraints(input).map( |(next,con)| {
        (next, PayloadStructure::Map(Box::new(con)))
    } )
}


pub fn payload_structure(input: &str ) -> Res< &str, PayloadStructure> {
    alt( (array_data_struct, primitive_data_struct,map_constraints))(input)

}

pub fn payload_structure_with_validation(input: &str ) -> Res< &str, PayloadStructureAndValidation> {
    tuple( (payload_structure, opt(preceded(tag("~"), opt(format)), ), opt(preceded(tag("~"), call_with_config), )  ) )(input).map( |(next,(data,format,verifier))| {
        (next,

         PayloadStructureAndValidation {
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

pub fn consume_payload_structure(input: &str) -> Res<&str, PayloadStructure> {
    all_consuming(payload_structure)(input)
}

pub fn consume_data_struct_def( input: &str) -> Res<&str, PayloadStructureAndValidation> {
    all_consuming(payload_structure_with_validation)(input)
}


pub fn payload_constraint_any(input: &str ) -> Res<&str, ValueConstraint<PayloadStructureAndValidation>> {
    tag("*")(input).map( |(next,_)|{
        (next,ValueConstraint::Any)
    } )
}

pub fn payload_constraint(input: &str ) -> Res<&str, ValueConstraint<PayloadStructureAndValidation>> {
    payload_structure_with_validation(input).map( |(next,payload_constraint)|{
        (next, ValueConstraint::Exact(payload_constraint))
    } )
}

pub fn payload_constraints(input: &str ) -> Res<&str, ValueConstraint<PayloadStructureAndValidation>> {
    alt( (tag("*"), recognize(payload_structure_with_validation)) )(input).map( |(next,data)|{

        let data = match data{
            "*" => ValueConstraint::Any,
            exact => ValueConstraint::Pattern(payload_structure_with_validation(input).expect("recognize already passed this...").1)
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
    let (next,data)= payload_structure_with_validation(input)?;

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

pub struct MapEntryConstraint {
    pub key: String,
    pub payload: ValueConstraint<PayloadStructureAndValidation>
}

#[cfg(test)]
pub mod test {
    use std::collections::HashMap;
    use std::str::FromStr;

    use anyhow::Error;
    use nom::combinator::all_consuming;


    use crate::pattern::{PayloadListConstraints, consume_payload_structure, consume_data_struct_def, consume_map_entry_constraint, consume_pipeline_block, consume_primitive_def, PayloadStructure, ValueConstraint, PayloadStructureAndValidation, Format, primitive, primitive_def, Range, MapEntryConstraint};
    use mesh_portal_serde::version::latest::payload::PrimitiveType;
    use mesh_portal_serde::version::latest::generic::payload::PayloadFormat;

    #[test]
    pub fn test_primative() -> Result<(),Error>{
        assert!( consume_payload_structure("Text")?.1 == PayloadStructure::Primitive( PrimitiveType::Text ) );
        assert!( consume_payload_structure("Text[]")?.1 == PayloadStructure::List( PayloadListConstraints { primitive: PrimitiveType::Text, range: Range::Any } ) );
        assert!( consume_payload_structure("Text[1-3]")?.1 == PayloadStructure::List( PayloadListConstraints { primitive: PrimitiveType::Text, range: Range::MinMax{min:1,max:3}} ) );
        assert!( consume_data_struct_def("Text~json")?.1 == PayloadStructureAndValidation { structure: PayloadStructure::Primitive(PrimitiveType::Text), format: Option::Some(PayloadFormat::Json), validator: Option::None }) ;
/*        assert!( consume_data_struct_def("Text~json~verifier!go")?.1 == DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::Some(Format::Json),verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::None})}) ;
        assert!( consume_data_struct_def("Text~~verifier!go")?.1 == DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::None,verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::None})}) ;
        assert!( consume_data_struct_def("Text[]~json~verifier!go")?.1 == DataStructDef{data:DataStruct::Array(Array{primitive: PrimitiveType::Text,range:Range::Any}), format: Option::Some(Format::Json),verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::None})}) ;
        assert!( consume_data_struct_def("Text~json~verifier!go+config:1.0.0:/some-file.conf")?.1 == DataStructDef{data:DataStruct::PrimitiveType(PrimitiveType::Text), format: Option::Some(Format::Json),verifier: Option::Some(CallWithConfig {call: Call {address: Address::from_str("verifier")?, port:"go".to_string() },config:Option::Some(Address::from_str("config:1.0.0:/some-file.conf")?)})}) ;*/
        assert!( consume_data_struct_def("Map")?.1 == PayloadStructureAndValidation { structure: PayloadStructure::Map(MapConstraints::default()), format: Option::None, validator: Option::None }) ;
        assert!( consume_data_struct_def("Map[]")?.1 == PayloadStructureAndValidation { structure: PayloadStructure::Map(MapConstraints::default()), format: Option::None, validator: Option::None }) ;


        assert!( consume_map_entry_constraint("label<Bin>")?.1== MapEntryConstraint { key: "label".to_string(), payload: ValueConstraint::Pattern(PayloadStructureAndValidation { structure: PayloadStructure::Primitive(PrimitiveType::Bin), format: Option::None, validator: Option::None })});
        assert!( consume_map_entry_constraint("label<Bin~json>")?.1== MapEntryConstraint { key: "label".to_string(), payload: ValueConstraint::Pattern(PayloadStructureAndValidation { structure: PayloadStructure::Primitive(PrimitiveType::Bin), format: Option::Some(PayloadFormat::Json), validator: Option::None })});
        assert!( consume_map_entry_constraint("label<*>")?.1== MapEntryConstraint { key: "label".to_string(), payload: ValueConstraint::Any });

        let mut map = HashMap::new();
        map.insert("first".to_string(), ValueConstraint::Pattern(PayloadStructureAndValidation { structure: PayloadStructure::Primitive(PrimitiveType::Text), format: Option::None, validator: Option::None }) );
        map.insert("last".to_string(), ValueConstraint::Pattern(PayloadStructureAndValidation { structure: PayloadStructure::Primitive(PrimitiveType::Text), format: Option::None, validator: Option::None }) );

        let def = consume_data_struct_def("Map[first<Text>,last<Text>]")?;
//        println!("{:?}",def.1);

/*        assert!(consume_data_struct_def("Map[first<Text>,last<Text>]")?.1== PayloadStructureWithValidation {format: Option::None, validator:Option::None, structure: PayloadStructure::Map(MapConstraints {
            required: map.clone(),
            allowed: Box::new(ValueConstraint::None)
        })});

        assert!(consume_data_struct_def("Map[first<Text>,last<Text>,*<Bin>]")?.1== PayloadStructureWithValidation {format: Option::None, validator:Option::None, structure: PayloadStructure::Map(MapConstraints {
            required: map,
            allowed: Box::new(ValueConstraint::Pattern(PayloadStructureWithValidation { structure: PayloadStructure::Primitive(PrimitiveType::Bin), validator:Option::None,format:Option::None}))
        })});


 */

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
