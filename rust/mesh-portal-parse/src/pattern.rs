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

use mesh_portal_serde::error::Error;
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
use mesh_portal_serde::version::latest::payload::{PayloadDelivery, PayloadRef};
use mesh_portal_serde::version::v0_0_1::id::Address;
use mesh_portal_serde::version::v0_0_1::util::ValueMatcher;
use mesh_portal_serde::version::latest::payload::RcCommand;
use mesh_portal_serde::version::v0_0_1::parse::{Res, camel_case_to_string, path_regex};
use mesh_portal_serde::version::latest::generic::entity::request::{ReqEntity,Rc,Msg,Http};
use crate::parse::call_with_config;
use mesh_portal_serde::version::v0_0_1::generic::payload::HttpMethod;
use mesh_portal_serde::version::v0_0_1::generic::resource::command::RcCommandType;
use mesh_portal_serde::version::v0_0_1::generic::pattern::Pattern;
use mesh_portal_serde::version::v0_0_1::pattern::parse::pattern;


#[cfg(test)]
pub mod test {
    use std::collections::HashMap;
    use std::str::FromStr;

    use anyhow::Error;
    use nom::combinator::all_consuming;


    use crate::pattern::{ListPattern, consume_payload_structure, consume_data_struct_def, consume_map_entry_pattern, consume_pipeline_block, consume_primitive_def, PayloadTypePattern, ValuePattern, PayloadPattern, Format, primitive, primitive_def, Range, MapEntryPattern, map_pattern_params, pattern_block_def};
    use mesh_portal_serde::version::latest::payload::{PrimitiveType, MapPattern};
    use mesh_portal_serde::version::latest::payload::PayloadFormat;
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
