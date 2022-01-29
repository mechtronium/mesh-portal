use std::convert::TryInto;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, multispace0, space0};
use nom::combinator::{all_consuming, opt};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, terminated, tuple};

use mesh_portal_serde::error::Error;
use mesh_portal_serde::version::latest::entity::EntityType;
use mesh_portal_serde::version::latest::entity::request::Msg;
use mesh_portal_serde::version::latest::generic::entity::request::ReqEntity;
use mesh_portal_serde::version::latest::generic::payload::RcCommand;
use mesh_portal_serde::version::latest::util::ValuePattern;
use mesh_portal_serde::version::v1::{Pipeline, PipelineStep, ProtoBind, Scope, Selector};
use mesh_portal_serde::version::v1::parse::Res;

use crate::parse::{call, PipelineStop};
use crate::pattern::{Block, entity_pattern, EntityPattern, http_pattern, http_pattern_scoped, HttpPattern, msg_pattern, msg_pattern_scoped, MsgPattern, PatternBlock, pipeline_block, rc_pattern, rc_pattern_scoped, RcPattern};


#[cfg(test)]
pub mod test {
    use std::str::FromStr;

    use anyhow::Error;
    use nom::bytes::complete::tag;
    use nom::combinator::all_consuming;
    use nom::sequence::delimited;

    use mesh_portal_serde::version::latest::payload::Call;
    use mesh_portal_serde::version::latest::util::{StringMatcher, ValuePattern};
    use mesh_portal_serde::version::v1::{Pipeline, PipelineStep};
    use mesh_portal_serde::version::v1::parse::camel_case;

    use crate::bind::{bind, consume_pipeline, consume_pipeline_step, consume_pipeline_stop, entity_selector, msg_section, msg_selectors, PipelineSegment, StepKind};
    use crate::parse::PipelineStop;
    use crate::pattern::{entity_pattern, msg_action, msg_entity_pattern, msg_pattern, PatternBlock};

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
        assert!(consume_pipeline_stop( "{{*}}")?.1 == PipelineStop::Internal);
        assert!(consume_pipeline_stop( "{{ * }}")?.1 == PipelineStop::Internal);
//        assert!(consume_pipeline_stop( "some:address^Msg!go")?.1 == PipelineStop::Call(Call::from_str("some:address^Msg!go")?));

        Ok(())
    }

    #[test]
    pub fn test_pipeline() -> Result<(),Error> {
        assert!(consume_pipeline( "-> &")?.1 == Pipeline { segments: vec![PipelineSegment { step: PipelineStep::new(StepKind::Request), stop: PipelineStop::Return}] });
        assert!(consume_pipeline( "-> {{*}}")?.1 == Pipeline { segments: vec![PipelineSegment { step: PipelineStep::new(StepKind::Request), stop: PipelineStop::Internal}] });
        assert!(consume_pipeline( "-> {{}}")?.1 == Pipeline { segments: vec![PipelineSegment { step: PipelineStep::new(StepKind::Request), stop: PipelineStop::Internal}] });
        consume_pipeline( "-[Text]-> {{*}} =[ Text ]=> &")?;
        assert!(consume_pipeline( "-[Text]-> {{*}} =[ Text ]=>").is_err()); // expect an error because does not end in a PipelineStop

        Ok(())
    }

    #[test]
    pub fn test_selector() -> Result<(),Error> {
        entity_selector( "Msg<Tick>/* -> {{*}};")?;
        entity_selector( "Http<Get>/* -> {{*}} => &;")?;

        entity_selector( "Msg<Signup> -[ Map{ username<Text>,password<Text>} ]-> strip:passsword:mechtron^Msg<Strip> -[ Map{username<Text>} ]-> {{*}} =[ Text ]=> &;")?;

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
    pub fn test_msg_selectors() -> Result<(),Error> {
        msg_selectors( r#"

               Tick/ -> {{ }};

               Ping -> {{  }} => &;

               Signup -[ Map{username<Text>,password<Text>} ]-> strip:passsword:mechtron^Msg<Strip> -[ Map{username<Text>} ]-> {{*}} =[ Text ]=> &;

               DoWhateverYouWant -[ * ]-> {{ * }} =[ * ]=> &;

           "# )?;

        msg_selectors( " ")?;

        Ok(())
    }

    #[test]
    pub fn test_msg_section() -> Result<(),Error> {
        msg_section( r#"Msg {

               Tick/ -> {*};

        }"# )?;

        msg_section( r#"Msg {


         }"# )?;



        Ok(())
    }



    #[test]
    pub fn test_bind() -> Result<(),Error> {

        bind( r#"

        Bind {

           Msg {

               Tick -> {{}};

               Ping -> {{  }} => &;

               Signup -[ Map{username<Text>,password<Text>} ]-> strip:passsword:mechtron^Msg<Strip> -[ Map{username<Text>} ]-> {{*}} =[ Text ]=> &;

               DoWhateverYouWant -[ * ]-> {{ * }} =[ * ]=> &;

               FormSubmition -[ Text~json~mechtron-verifier^Msg<ValidateForm> ]-> {{ * }} =[ ]=> &;

           }

        }   "# )?;

        Ok(())
    }
}
