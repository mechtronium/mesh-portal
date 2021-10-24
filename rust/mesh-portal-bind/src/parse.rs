use anyhow::Error;
use nom::error::{context, VerboseError, ParseError};
use nom::sequence::{delimited, tuple};
use nom::bytes::complete::{tag, take_until};
use nom::character::complete::{alpha1, alpha0, digit1, digit0};
use nom::combinator::{opt, all_consuming};
use nom::{IResult, InputTakeAtPosition, AsChar};
use nom::branch::alt;
use nom::multi::{separated_list0, fold_many0};


pub type Res<I,O>=IResult<I,O, VerboseError<I>>;

pub fn asterisk<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
    where
        T: InputTakeAtPosition,
        <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| item.as_char() != '*' )
}

pub struct Group {
    pub group_type: GroupType,
    pub content: String
}

pub enum GroupType {
   Bracket,
   Paren,
   Tag
}

impl GroupType {

    pub fn push_token(&self)-> &str {
        match self {
            GroupType::Bracket => "[",
            GroupType::Paren => "(",
            GroupType::Tag => "<"
        }
    }

    pub fn pop_token(&self)-> &str {
        match self {
            GroupType::Bracket => "]",
            GroupType::Paren => ")",
            GroupType::Tag => ">"
        }
    }
}

pub enum GroupIdent {
    Bracket(StackCmd),
    Paren(StackCmd),
    Tag(StackCmd)
}

impl GroupIdent {
    pub fn get_type(&self) -> GroupType {
        match self {
            GroupIdent::Bracket(_) => GroupType::Bracket,
            GroupIdent::Paren(_) => GroupType::Paren,
            GroupIdent::Tag(_) => GroupType::Tag
        }
    }
}



pub enum StackCmd {
    Push,
    Pop
}



pub fn group_open(input: &str ) -> Res<&str, GroupIdent> {
    alt( (tag("["),tag("("),tag("<")) )(input).map( |(input,s) | {
        let rtn = match s{
           "[" => GroupIdent::Bracket(StackCmd::Pop),
           "(" => GroupIdent::Paren(StackCmd::Pop),
           "<" => GroupIdent::Tag(StackCmd::Pop),
            _ => {
                panic!("how is this possible?")
            }
        };
        (input,rtn)
    } )
}



/*
pub fn parse_group(input: &str) -> Res<&str, &str> {
    let x = take_until( group_open )(input);

    alpha0(input)
}

 */



pub fn parse_payload(input: &str) -> Res<&str, Vec<&str> > {
        delimited(
            tag("<"),
            separated_list0( tag("<"), alt( (digit0,alpha0) )  ),
            tag(">"),
        )
    (input)
}





#[cfg(test)]
pub mod test {
    use crate::parse::{parse_payload, group_open };
    use nom::Err;
    use nom::error::VerboseError;
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
        let result = parse_payload( "<Hello<Flerb>>" );


        match  &result {
            Ok((input, pay)) => {
                for segment in pay {
                    println!("payload: {}", segment.to_string() );
                }
            }
            Err(error) => {
                println!("{}", error.to_string());
            }
        }
        result?;
        Ok(())
    }

}