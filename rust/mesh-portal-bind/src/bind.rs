use crate::parse::{Res, skewer, PipelineStop, address, call};
use crate::Msg;
use crate::pattern::{PatternBlock, Block, pipeline_block};
use nom::sequence::{terminated, delimited, tuple};
use nom::bytes::complete::tag;
use nom::multi::{many0, many1};
use nom::combinator::all_consuming;
use nom::character::complete::{space0, multispace0};
use nom::branch::alt;


pub struct Bind {
    pub msg: Msg
}

impl Default for Bind {
    fn default() -> Self {
        Self {
            msg: Msg::default()
        }
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

pub struct Port{
    pub name: String,
    pub pipeline: Pipeline
}


#[derive(Debug,Clone,Eq,PartialEq)]
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

#[derive(Debug,Clone,Eq,PartialEq)]
pub struct PipelineSegment {
    pub step: PipelineStep,
    pub stop: PipelineStop
}


#[derive(Debug,Clone,Eq,PartialEq)]
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
    Msg(Msg)
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
   tuple((tag("Msg"),multispace0, delimited(tag("{"), delimited(multispace0,many0(delimited( multispace0,port, multispace0)),multispace0),tag("}"))) )(input).map( |(next, (_,_,ports))| {
       (next,
        Section::Msg(Msg{
           ports
       }))
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

  delimited( tag("{"), space0, tag("}") )(input).map( |(next,_)|{
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


pub fn port_selector( input: &str ) -> Res<&str, &str> {
    skewer(input)
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

pub fn port(input: &str ) -> Res<&str,Port> {
    tuple( ( port_selector, multispace0, pipeline,tag(";") ) )(input).map( |(next,(name,_,pipeline,_))| {
        (next,
         Port{ name: name.to_string(),
               pipeline})

    } )
}

pub fn consume_port( input: &str ) -> Res<&str,Port>{
    all_consuming(port)(input)
}



#[cfg(test)]
pub mod test {
    use anyhow::Error;
    use crate::bind::{consume_pipeline_step, consume_pipeline_stop, consume_pipeline, Pipeline, PipelineStep, PipelineSegment, StepKind, consume_port, bind};
    use crate::parse::PipelineStop;
    use crate::symbol::{Address, Call};
    use std::str::FromStr;
    use crate::pattern::PatternBlock;

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
        assert!(consume_pipeline_stop( "{}")?.1 == PipelineStop::Internal);
        assert!(consume_pipeline_stop( "{ }")?.1 == PipelineStop::Internal);
        assert!(consume_pipeline_stop( "some:address!go")?.1 == PipelineStop::Call(Call::from_str("some:address!go")?));

        Ok(())
    }

    #[test]
    pub fn test_pipeline() -> Result<(),Error> {
        assert!(consume_pipeline( "-> &")?.1 == Pipeline { segments: vec![PipelineSegment { step: PipelineStep::new(StepKind::Request), stop: PipelineStop::Return}] });
        assert!(consume_pipeline( "-> {}")?.1 == Pipeline { segments: vec![PipelineSegment { step: PipelineStep::new(StepKind::Request), stop: PipelineStop::Internal}] });
        consume_pipeline( "-[Text]-> {} =[ Text ]=> &")?;
        assert!(consume_pipeline( "-[Text]-> {} =[ Text ]=>").is_err()); // expect an error because does not end in a PipelineStop

        Ok(())
    }

    #[test]
    pub fn test_port() -> Result<(),Error> {
        consume_port( "tick -> {};")?;
        consume_port( "ping -> {} => &;")?;

        Ok(())
    }


    #[test]
    pub fn test_bind() -> Result<(),Error> {
        bind( r#"

        bind {

           Msg{

              tick -> {};

              ping -> { } => &;

              signup -[ Map[username<Text>,password<Text>] ]-> strip:passsword:mechtron^Msg<strip> -[ Map[username<Text>] ]-> { } =[ Text ]=> &;

              do-whatever-you-want -[ * ]-> { } =[ * ]=> &;

           }

        }   "# )?;

        Ok(())
    }
}
