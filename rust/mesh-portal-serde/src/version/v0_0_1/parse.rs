use std::convert::TryInto;
use std::str::FromStr;

use anyhow::Error;
use nom::{AsChar, InputTakeAtPosition, IResult, Parser};
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::character::complete::{alpha0, alpha1, alphanumeric1, anychar, digit0, digit1, multispace0, one_of};
use nom::combinator::{all_consuming, not, opt, recognize};
use nom::error::{context, ErrorKind, VerboseError, FromExternalError};
use nom::multi::{many1, many_m_n, separated_list0, separated_list1};
use nom::sequence::{delimited, preceded, terminated, tuple};
use serde::{Deserialize, Serialize};

use crate::version::v0_0_1::id::{ResourceType, Specific, Version, Address, Kind, AddressAndKind};
use std::fmt;
use crate::version::v0_0_1::resource::property::{ResourcePropertyAssignment, ResourceHostPropertyValueSelector, ResourcePropertiesKind};
use crate::version::v0_0_1::resource::property::{ResourcePropertyValueSelector, ResourceRegistryPropertyValueSelector, ResourceProperty, ResourceRegistryProperty, ResourceValueSelector};
use crate::version::v0_0_1::id::AddressAndType;
use crate::version::v0_0_1::resource::ConfigSrc;
use std::marker::PhantomData;

pub type Res<T, U> = IResult<T, U, VerboseError<T>>;




pub fn parse_address(input: &str) -> Res<&str, Vec<String>> {
    context(
        "address",
        all_consuming(separated_list1(
            nom::character::complete::char(':'),
            any_resource_path_segment
        )),
    )(input).map( |(next,segments)|{
        let segments: Vec<String> = segments.iter().map( |s| s.to_string() ).collect();
        (next,segments)
    })
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
}



impl std::error::Error for ParseError {

}


impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message )
    }
}

impl <T> From<nom::Err<VerboseError<T>>>  for ParseError {
    fn from(_: nom::Err<VerboseError<T>>) -> Self {
        Self {
            message: "parse error".to_string()
        }
    }
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

fn loweralphanumerichyphen1<T>(i: T) -> Res<T, T>
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

fn not_whitespace<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            (char_item == ' ')
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn not_whitespace_or_semi<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            (char_item == ' ' || char_item == ';')
        },
        ErrorKind::AlphaNumeric,
    )
}

fn anything_but_single_quote<T>(i: T) -> Res<T, T>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item == '\''
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn parse_domain(input: &str) -> Res<&str, &str> {
    context(
        "domain",
        recognize( tuple((
            many1(terminated(loweralphanumerichyphen1, tag("."))),
            loweralphanumerichyphen1,
        ))),
    )(input)

}

fn parse_version_major_minor_patch(input: &str) -> Res<&str, (usize, usize, usize)> {
    context(
        "version_major_minor_patch",
        tuple((
            terminated(digit1, tag(".")),
            terminated(digit1, tag(".")),
            terminated(digit1, not(digit1)),
        )),
    )(input)
        .map(|(next_input, res)| {
            (
                next_input,
                (
                    res.0.parse().unwrap(),
                    res.1.parse().unwrap(),
                    res.2.parse().unwrap(),
                ),
            )
        })
}

pub fn parse_version(input: &str) -> Res<&str, Version> {
    context(
        "version",
        tuple((parse_version_major_minor_patch, opt(preceded(tag("-"), parse_skewer)))),
    )(input)
        .map(|(next_input, ((major, minor, patch), release))| {
            let release = match release {
                None => Option::None,
                Some(v) => Option::Some(v.to_string())
            };

                (next_input, Version::new(major, minor, patch, release))
        })
}

fn parse_skewer(input: &str) -> Res<&str, &str> {
    context("skewer-case", loweralphanumerichyphen1)(input)
}

pub fn parse_specific(input: &str) -> Res<&str, Specific> {
    context(
        "specific",
        tuple((
            terminated(parse_domain, tag(":")),
            terminated(loweralphanumerichyphen1, tag(":")),
            terminated(loweralphanumerichyphen1, tag(":")),
            parse_version,
        )),
    )(input)
        .map(|(next_input, (vendor, product, variant, version))| {
            (
                next_input,
                Specific {
                    vendor: vendor.to_string(),
                    product: product.to_string(),
                    variant: variant.to_string(),
                    version: version,
                },
            )
        })
}


pub fn parse_resource_type(input: &str) -> Res<&str, Result<ResourceType,Error>> {
    context(
        "resource_type",
        delimited(
            tag("<"),
            tuple((
                alpha1,
                opt(tag("<?>")),
            )),
            tag(">"),
        ),
    )(input)
        .map(|(input, (rt, _))| {
            (input, ResourceType::from_str(rt))
        })
}

pub fn parse_kind(input: &str) -> Res<&str, Result<Kind,Error>> {
    context(
        "kind",
        delimited(
            tag("<"),
            tuple((
                alpha1,
                opt(delimited(
                    tag("<"),
                    tuple((alpha1, opt(delimited(tag("<"), parse_specific, tag(">"))))),
                    tag(">"),
                )),
            )),
            tag(">"),
        ),
    )(input)
        .map(|(input, (rt, more))| {
            let kind = match &more {
                None => Option::None,
                Some((kind, _)) => Option::Some((*kind).clone().to_string()),
            };
            let spec = match &more {
                None => Option::None,
                Some((_, Option::Some(spec))) => Option::Some(spec.clone()),
                _ => Option::None,
            };
            (
                input,
                Kind {
                    resource_type: rt.to_string(),
                    kind: kind,
                    specific: spec,
                }.try_into(),
            )
        })
}
pub fn parse_resource_path(input: &str) -> Res<&str, Address> {
    context(
        "resource-path",
        separated_list0(
            nom::character::complete::char(':'),
            any_resource_path_segment,
        ),
    )(input).map( |(next_input, segments) | {
        let segments : Vec<String> = segments.iter().map(|s|s.to_string()).collect();
        (next_input, Address {
            segments
        })
    } )
}

pub fn parse_resource_path_and_kind(input: &str) -> Res<&str, Result<AddressAndKind,Error>> {
    context(
        "parse_resource_path_and_kind",
        tuple(
            (parse_resource_path,
             parse_kind)
        ),
    )(input).map( |(next_input, (path, resource_kind)) | {

        (next_input,

         {
             match resource_kind {
                 Ok(kind) => {
                     AddressAndKind::new(path,kind)
                 }
                 Err(error) => {
                     Err(error.into())
                 }
             }
         }


        )
    } )
}

pub fn parse_resource_path_and_type(input: &str) -> Res<&str, Result<AddressAndType,Error>> {
    context(
        "parse_resource_path_and_type",
        tuple(
            (parse_address,
             parse_resource_type)
        ),
    )(input).map( |(next_input, (segments, resource_type)) | {

        let address = Address { segments };

        (next_input,

         {
             match resource_type{
                 Ok(resource_type) => {
                     Ok(AddressAndType {
                         address,
                         resource_type
                     })
                 }
                 Err(error) => {
                     Err(error.into())
                 }
             }
         }


        )
    } )
}

pub fn parse_resource_properties_kind(input: &str) -> Res<&str, ResourcePropertiesKind> {
    context( "parse_resource_properties_kind",
             alpha1
    ) (input).map(|(input_next,kind)| {

        let kind = if kind == "reg" {
            ResourcePropertiesKind::Registry
        } else {
            // a dirty hack here:
            ResourcePropertiesKind::Host
        };
        (input_next,kind)
    })
}


pub fn parse_mapping(input: &str) -> Res<&str, &str> {
    context( "parse_mapping",
             delimited(
                 tag("['"),
                 anything_but_single_quote,
                 tag("']"),
             )  ) (input)
}

pub fn parse_aspect_mapping(input: &str) -> Res<&str, &str> {
    context(
        "parse_aspect_mapping",
        delimited(
            tag("['"),
            parse_skewer,
            tag("']"),
        ) ) (input)
}

pub fn parse_resource_property_value_selector(input: &str) -> Res<&str, Result<ResourcePropertyValueSelector,Error>> {
    context(
        "parse_resource_property_value_selector",
        tuple(
            (parse_skewer,opt(tuple( (alt( (parse_skewer,parse_aspect_mapping) ), opt(parse_mapping)) ) )
            ),
        ))(input) .map( |(next_input, (property,aspect))|  {

        match property.to_string().as_str() {
            "state" => {
                match aspect {
                    None => {
                        (next_input,Ok(ResourcePropertyValueSelector::state()))
                    }
                    Some((aspect, field) ) => {
                        match field {
                            None => {
                                (next_input,Ok(ResourcePropertyValueSelector::state_aspect(aspect.to_string().as_str())))
                            }
                            Some(field) => {
                                (next_input,Ok(ResourcePropertyValueSelector::state_aspect_field(aspect.to_string().as_str(), field )))
                            }
                        }
                    }
                }
            },
            "config" => {
                (next_input,Ok(ResourcePropertyValueSelector::Registry(ResourceRegistryPropertyValueSelector::Config)))
            }
            property => return (next_input, Err(format!("cannot match a selector for resource property '{}'",property).into()))
        }

    })
}

pub fn parse_resource_value_selector(input: &str) -> Res<&str, Result<ResourceValueSelector,Error>> {
    context(
        "parse_resource_value_selector",
        tuple(
            (terminated(parse_resource_path, tag("::")), parse_resource_property_value_selector )

        ),
    )(input).map( |(next_input, (path, property )) | {
        match property {
            Ok(property) => {
                (next_input, Ok(ResourceValueSelector {
                    resource: path,
                    property
                }))
            }
            Err(err) => {
                (next_input, Err(err))
            }
        }
    })
}

pub fn parse_resource_property_assignment( input: &str ) -> Res<&str, Result<ResourcePropertyAssignment,Error>> {
    context ("parse_resource_property_assignment",
             tuple((parse_resource_value_selector,multispace0,tag("="),multispace0,not_whitespace,multispace0))
    )(input).map( |(input_next,(resource_value_selector, _, _, _, value, _))| {
        match resource_value_selector {
            Ok(resource_value_selector ) => {
                match resource_value_selector.property {
                    ResourcePropertyValueSelector::Host(ResourceHostPropertyValueSelector::State{..} ) => {
                        ( input_next, Err("cannot set Resource State via the assignment operator".into()) )
                    }
                    ResourcePropertyValueSelector::Registry(selector)=> {
                        match selector {
                            ResourceRegistryPropertyValueSelector::Status => {

                                ( input_next, Err("cannot set Resource Status via the assignment operator".into()) )
                            }
                            ResourceRegistryPropertyValueSelector::Config => {
                                let config = Address::from_str(value);
                                match config {
                                    Ok(config) => {
                                        let assignment = ResourcePropertyAssignment {
                                            resource: resource_value_selector.resource.into(),
                                            property: ResourceProperty::Registry( ResourceRegistryProperty::Config(ConfigSrc::Artifact(config)) )
                                        };
                                        (input_next,Ok(assignment))
                                    }
                                    Err(error) => {
                                        ( input_next, Err(error.into()) )
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Err(error) => {
                (input_next, Err(error.into()) )
            }
        }

    })
}




