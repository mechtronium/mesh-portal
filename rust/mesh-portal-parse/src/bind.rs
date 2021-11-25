use crate::parse::{Res, skewer, PipelineStop, call};
use crate::pattern::{PatternBlock, Block, pipeline_block, entity_pattern, EntityPattern};
use nom::sequence::{terminated, delimited, tuple};
use nom::bytes::complete::tag;
use nom::multi::{many0, many1};
use nom::combinator::all_consuming;
use nom::character::complete::{space0, multispace0, alphanumeric1};
use nom::branch::alt;
use mesh_portal_serde::version::latest::entity::request::Msg;
use mesh_portal_serde::version::latest::util::ValuePattern;
use mesh_portal_serde::version::latest::generic::payload::RcCommand;
use mesh_portal_serde::version::v0_0_1::generic::payload::HttpMethod;


pub struct Bind {
    pub msg: MessageSection
}

impl Default for Bind {
    fn default() -> Self {
        Self {
            msg: Default::default()
        }
    }
}

pub struct MessageSection {

}

impl Default for MessageSection {
    fn default() -> Self {
        MessageSection{}
    }
}

pub enum Whitelist {
    Any,
    None,
    Enumerated(Vec<CallPattern>)
}

pub enum CallPattern {
    Any,
    Call
}




#[derive(Clone,Eq,PartialEq)]
pub struct Pipeline {
    pub segments: Vec<PipelineSegment>
}

impl Pipeline{
    pub fn new()->Self {
        Self{
            segments: vec![]
        }
    }
}

#[derive(Clone,Eq,PartialEq)]
pub struct PipelineSegment {
    pub step: PipelineStep,
    pub stop: PipelineStop
}


#[derive(Clone,Eq,PartialEq)]
pub struct PipelineStep {
  pub kind: StepKind,
  pub blocks: Vec<Block>
}

#[derive(Debug,Clone,Eq,PartialEq)]
pub enum StepKind {
   Request,
   Response
}

impl PipelineStep  {
    pub fn new(kind: StepKind) ->Self {
        Self{
            kind,
            blocks: vec![]
        }
    }
}

pub enum Section {
    Msg(MessageSection)
}

pub fn bind(input: &str ) -> Res<&str,Bind> {
    delimited( multispace0, tuple((tag("bind"), multispace0, delimited(tag("{"),sections,tag("}")))), multispace0)(input).map(|(next,(_,_,sections))|{

        let mut bind = Bind::default();
        for section in sections {
            match section {
                Section::Msg(msg) => {
                    bind.msg = msg;
                }
            }
        }

        (next,bind)
    })
}

pub fn sections(input: &str ) -> Res<&str, Vec<Section>> {
    delimited(multispace0,many0(delimited( multispace0,section, multispace0)),multispace0)(input)
}

pub fn section(input: &str ) -> Res<&str, Section> {
  msg_section(input)
}


pub fn msg_section(input: &str ) -> Res<&str, Section> {
   tuple((tag("Msg"),multispace0, delimited(tag("{"), delimited(multispace0, many0(delimited(multispace0, selector, multispace0)), multispace0), tag("}"))) )(input).map( |(next, (_,_,selectors))| {
       (next,
        Section::Msg(MessageSection{}))
   } )
}



pub fn pipeline_step( input: &str ) -> Res<&str, PipelineStep> {
   tuple( (many0(pipeline_block ), alt( (tag("->"),tag("=>")))))(input).map( |(next,(blocks,kind))| {
       let kind = match kind {
           "->" => StepKind::Request,
           "=>" => StepKind::Response,
           _ => panic!("nom parse rules should have selected -> or =>")
       };
       (next,
       PipelineStep{
           kind,
           blocks
       })
   } )
}

pub fn inner_pipeline_stop( input: &str ) -> Res<&str, PipelineStop> {

  delimited( tag("{"), delimited(multispace0,tag("*"),multispace0), tag("}") )(input).map( |(next,_)|{
      (next,PipelineStop::Internal)
  } )
}

pub fn return_pipeline_stop(input: &str ) -> Res<&str, PipelineStop> {
   tag("&")(input).map( |(next,_)|{
       (next,
       PipelineStop::Return)
   })
}

pub fn call_pipeline_stop(input: &str ) -> Res<&str, PipelineStop> {
    call(input).map( |(next, call)| {
        (next,
         PipelineStop::Call(call))
    })
}


pub fn pipeline_stop( input: &str ) -> Res<&str, PipelineStop> {
   alt( (inner_pipeline_stop, return_pipeline_stop, call_pipeline_stop))(input)
}

pub fn consume_pipeline_step( input: &str ) -> Res<&str,PipelineStep> {
    all_consuming(pipeline_step)(input)
}

pub fn consume_pipeline_stop( input: &str ) -> Res<&str,PipelineStop> {
    all_consuming(pipeline_stop)(input)
}




pub fn pipeline_segment( input: &str ) -> Res<&str, PipelineSegment> {
    tuple( (multispace0,pipeline_step,multispace0,pipeline_stop,multispace0))(input).map( |(next,(_,step,_,stop,_))| {

        (next,
        PipelineSegment{
            step,
            stop
        })
    } )
}

pub fn pipeline( input: &str ) -> Res<&str, Pipeline> {
    many1(pipeline_segment )(input).map( |(next,segments) |{
        (next,
        Pipeline{
            segments
        })
    } )
}

pub fn consume_pipeline( input: &str ) -> Res<&str,Pipeline>{
    all_consuming(pipeline)(input)
}


pub struct Selector {
    pub pattern: EntityPattern,
    pub pipeline: Pipeline
}

impl Selector {
    pub fn new( pattern: EntityPattern, pipeline: Pipeline ) -> Self {
        Selector {
            pattern,
            pipeline
        }
    }
}

pub fn selector(input: &str ) -> Res<&str, Selector> {
    tuple( (entity_pattern, multispace0, pipeline, tag(";") ) )(input).map( |(next,(pattern,_,pipeline,_))| {
        (next, Selector::new(pattern, pipeline) )
    } )
}

pub fn consume_selector(input: &str ) -> Res<&str, Selector>{
    all_consuming(selector)(input)
}



#[cfg(test)]
pub mod test {
    use anyhow::Error;
    use crate::bind::{consume_pipeline_step, consume_pipeline_stop, consume_pipeline, Pipeline, PipelineStep, PipelineSegment, StepKind, selector, bind};
    use crate::parse::{PipelineStop, camel_case, path_regex, upper, camel_case_to_string};
    use mesh_portal_serde::version::latest::payload::Call;
    use std::str::FromStr;
    use crate::pattern::{PatternBlock, entity_pattern, msg_pattern, msg_action, msg_entity_pattern};
    use nom::combinator::all_consuming;
    use nom::sequence::delimited;
    use nom::bytes::complete::tag;
    use mesh_portal_serde::version::latest::util::{ValuePattern, StringMatcher};

    #[test]
    pub fn test_pipeline_step() -> Result<(),Error> {
        consume_pipeline_step( "-[ Text ]->")?;
        //consume_pipeline_step( "-[Text]->")?;
        consume_pipeline_step( "=[ Text ]=>")?;

        Ok(())
    }

    #[test]
    pub fn test_pipeline_stop() -> Result<(),Error> {
        assert!(consume_pipeline_stop( "&")?.1 == PipelineStop::Return);
        assert!(consume_pipeline_stop( "{*}")?.1 == PipelineStop::Internal);
        assert!(consume_pipeline_stop( "{ * }")?.1 == PipelineStop::Internal);
//        assert!(consume_pipeline_stop( "some:address^Msg!go")?.1 == PipelineStop::Call(Call::from_str("some:address^Msg!go")?));

        Ok(())
    }

    #[test]
    pub fn test_pipeline() -> Result<(),Error> {
        assert!(consume_pipeline( "-> &")?.1 == Pipeline { segments: vec![PipelineSegment { step: PipelineStep::new(StepKind::Request), stop: PipelineStop::Return}] });
        assert!(consume_pipeline( "-> {*}")?.1 == Pipeline { segments: vec![PipelineSegment { step: PipelineStep::new(StepKind::Request), stop: PipelineStop::Internal}] });
        consume_pipeline( "-[Text]-> {*} =[ Text ]=> &")?;
        assert!(consume_pipeline( "-[Text]-> {*} =[ Text ]=>").is_err()); // expect an error because does not end in a PipelineStop

        Ok(())
    }

    #[test]
    pub fn test_selector() -> Result<(),Error> {
        selector( "Msg<Tick>/* -> {*};")?;
        selector( "Http<Get>/* -> {*} => &;")?;

        selector( "Msg<Signup> -[ Map{ username<Text>,password<Text>} ]-> strip:passsword:mechtron^Msg<Strip> -[ Map[username<Text>] ]-> {*} =[ Text ]=> &;")?;

        Ok(())
    }

    #[test]
    pub fn test_entity_pattern() -> Result<(),Error> {


        assert_eq!((">","Over"),camel_case("Over>")?);

        all_consuming(msg_pattern )("Msg<Over>/")?;
        all_consuming(msg_pattern )("Msg<Over>")?;
        all_consuming(msg_entity_pattern )("Msg<Over>/")?;
        all_consuming(msg_entity_pattern )("Msg<Over>")?;
        all_consuming(entity_pattern )("Msg<Over>/")?;
        all_consuming(entity_pattern )("Http<Get>/")?;
        all_consuming(entity_pattern )("Http<Get>")?;
        assert!(all_consuming(entity_pattern )("Http<What>/").is_err());
        all_consuming(entity_pattern )("Rc<Create>")?;


        Ok(())
    }



    #[test]
    pub fn test_bind() -> Result<(),Error> {
        bind( r#"

        bind {

           Msg{

              Msg<Tick>/ -> {*};

              Msg<Ping> -> { * } => &;

              Msg<Signup> -[ Map[username<Text>,password<Text>] ]-> strip:passsword:mechtron^Msg<Strip> -[ Map[username<Text>] ]-> {*} =[ Text ]=> &;

              Msg<DoWhateverYouWant> -[ * ]-> { * } =[ * ]=> &;

           }

        }   "# )?;

        Ok(())
    }
}
