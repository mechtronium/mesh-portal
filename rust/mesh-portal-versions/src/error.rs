use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter};
use std::io;
use std::string::FromUtf8Error;

use http::header::ToStrError;
use http::status::InvalidStatusCode;
use http::uri::InvalidUri;
use nom::error::VerboseError;
use nom::Err;
use nom_locate::LocatedSpan;
use nom_supreme::error::{ErrorTree, StackContext};
use semver::{ReqParseError, SemVerError};
use std::num::ParseIntError;
use std::ops::Range;
use std::rc::Rc;
use std::sync::{Arc, PoisonError};
use ariadne::{Label, Report, ReportBuilder, ReportKind, Source};
use crate::version::v0_0_1::parse::error::find_parse_err;
use cosmic_nom::SpanExtra;
use cosmic_nom::Span;

pub enum MsgErr {
    Status {
        status: u16,
        message: String,
    },
    ParseErrs(ParseErrs),
//    SubstErr(SubstErr)
}

impl Into<ParseErrs> for MsgErr {
    fn into(self) -> ParseErrs {
        match self {
            MsgErr::Status { status, message } => {
                let mut builder = Report::build(ReportKind::Error, (), 0);
                let report = builder.with_message(message).finish();
                let errs = ParseErrs {
                    report: vec![report],
                    source: None
                };
                errs
            }
            MsgErr::ParseErrs(errs) => errs
        }
    }
}

impl Debug for MsgErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MsgErr::Status { status, message } => {
                f.write_str(format!("{}: {}",status,message).as_str() )
            }
            MsgErr::ParseErrs(_) => {
                f.write_str("Error Report..." )
            }
        }
    }
}
/*
impl ToString for MsgErr {
    fn to_string(&self) -> String {
        match self {
            MsgErr::Status { status, message } => {
                format!("Status {}: {}", status, message )
            }
            MsgErr::Report { .. } => {
                format!("MsgErr reports cannot be converted into a String at the moment...")
            }
        }

    }
}

 */

impl MsgErr {
    pub fn print(&self) {
        match self {
            MsgErr::Status { .. } => {
                println!("{}", self.to_string());
            }
            MsgErr::ParseErrs(err) => {
                err.print()
            }
        }
    }
}

impl MsgErr {
    pub fn new(status: u16, message: &str) -> Self {
        Self::Status {
            status,
            message: message.to_string(),
        }
    }

    pub fn err404() -> Self {
        Self::Status {
            status: 404,
            message: "Not Found".to_string(),
        }
    }

    pub fn err500() -> Self {
        Self::Status {
            status: 500,
            message: "Internal Server Error".to_string(),
        }
    }

    pub fn from_500(message: &str) -> Self {
        Self::Status {
            status: 500,
            message: message.to_string(),
        }
    }
}

impl StatusErr for MsgErr {
    fn status(&self) -> u16 {
        match self {
            MsgErr::Status { status,message } => {
                status.clone()
            }
            MsgErr::ParseErrs(_) => {
                500u16
            }
        }
    }

    fn message(&self) -> String {
        match self {
            MsgErr::Status { status,message } => {
                message.clone()
            }
            MsgErr::ParseErrs(_) => {
                "Error report".to_string()
            }
        }
    }
}

pub trait StatusErr {
    fn status(&self) -> u16;
    fn message(&self) -> String;
}

impl Display for MsgErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MsgErr::Status { status, message } => {
                f.write_str(format!("{}: {}",status,message).as_str() )
            }
            MsgErr::ParseErrs(_) => {
                f.write_str("Error Report..." )
            }
        }
    }
}

impl std::error::Error for MsgErr {}

impl From<String> for MsgErr {
    fn from(message: String) -> Self {
        Self::Status {
            status: 500,
            message,
        }
    }
}

impl <T> From<PoisonError<T>> for MsgErr {
    fn from(e: PoisonError<T>) -> Self {
        MsgErr::Status {
            status: 500,
            message: e.to_string()
        }
    }
}

impl From<InvalidStatusCode> for MsgErr {
    fn from(error: InvalidStatusCode) -> Self {
        Self::Status {
            status: 500,
            message: error.to_string(),
        }
    }
}

impl From<FromUtf8Error> for MsgErr {
    fn from(message: FromUtf8Error) -> Self {
        Self::Status {
            status: 500,
            message: message.to_string(),
        }
    }
}

impl From<&str> for MsgErr {
    fn from(message: &str) -> Self {
        Self::Status  {
            status: 500,
            message: message.to_string(),
        }
    }
}

impl From<Box<bincode::ErrorKind>> for MsgErr {
    fn from(message: Box<bincode::ErrorKind>) -> Self {
        Self::Status  {
            status: 500,
            message: message.to_string(),
        }
    }
}

impl From<Infallible> for MsgErr {
    fn from(i: Infallible) -> Self {
        Self::Status  {
            status: 500,
            message: i.to_string(),
        }
    }
}

impl From<nom::Err<VerboseError<&str>>> for MsgErr {
    fn from(error: nom::Err<VerboseError<&str>>) -> Self {
        Self::Status  {
            status: 500,
            message: error.to_string(),
        }
    }
}



impl From<ErrorTree<&str>> for MsgErr {
    fn from(error: ErrorTree<&str>) -> Self {
        Self::Status  {
            status: 500,
            message: error.to_string(),
        }
    }
}

impl From<ReqParseError> for MsgErr {
    fn from(error: ReqParseError) -> Self {
        Self::Status  {
            status: 500,
            message: error.to_string(),
        }
    }
}

impl From<SemVerError> for MsgErr {
    fn from(error: SemVerError) -> Self {
        Self::Status  {
            status: 500,
            message: error.to_string(),
        }
    }
}

impl From<strum::ParseError> for MsgErr {
    fn from(error: strum::ParseError) -> Self {
        Self::Status  {
            status: 500,
            message: error.to_string(),
        }
    }
}

impl From<ParseIntError> for MsgErr {
    fn from(x: ParseIntError) -> Self {
        Self::Status  {
            status: 500,
            message: x.to_string(),
        }
    }
}

impl From<regex::Error> for MsgErr {
    fn from(x: regex::Error) -> Self {
        Self::Status  {
            status: 500,
            message: x.to_string(),
        }
    }
}

impl From<InvalidUri> for MsgErr {
    fn from(x: InvalidUri) -> Self {
        Self::Status  {
            status: 500,
            message: x.to_string(),
        }
    }
}

impl From<http::Error> for MsgErr {
    fn from(x: http::Error) -> Self {
        Self::Status  {
            status: 500,
            message: x.to_string(),
        }
    }
}

impl From<ToStrError> for MsgErr {
    fn from(x: ToStrError) -> Self {
        Self::Status  {
            status: 500,
            message: x.to_string(),
        }
    }
}

impl <I:Span> From<nom::Err<ErrorTree<I>>> for MsgErr {
    fn from(err: Err<ErrorTree<I>>) -> Self {
        fn handle<I:Span>(err: ErrorTree<I>) -> MsgErr {
            match err {
                ErrorTree::Base {
                    location,
                    kind: _kind,
                } => MsgErr::Status {
                    status: 500,
                    message: format!(
                        "parse error line: {} column: {}",
                        location.location_line(),
                        location.get_column()
                    ),
                },
                ErrorTree::Stack { base, contexts } => match contexts.first() {
                    None => MsgErr::Status {
                        status: 500,
                        message: "error, cannot find location".to_string(),
                    },
                    Some((location, _)) => MsgErr::Status {
                        status: 500,
                        message: format!(
                            "Stack parse error line: {} column: {}",
                            location.location_line(),
                            location.get_column()
                        ),
                    },
                },
                ErrorTree::Alt(what) => MsgErr::Status {
                    status: 500,
                    message: "alt error".to_string(),
                },
            }
        }
        match err {
            Err::Incomplete(_) => MsgErr::Status {
                status: 500,
                message: "unexpected incomplete parsing error".to_string(),
            },

            Err::Error(err) => handle(err),
            Err::Failure(err) => handle(err),
        }
    }
}


impl Into<String> for MsgErr {
    fn into(self) -> String {
        self.to_string()
    }
}



impl From<io::Error> for MsgErr {
    fn from(e: io::Error) -> Self {
        MsgErr::new(500,e.to_string().as_str())
    }
}


impl From<ParseErrs> for MsgErr {
    fn from(errs: ParseErrs) -> Self {
        MsgErr::ParseErrs(errs)
    }
}
impl <I:Span> From<nom::Err<ErrorTree<I>>> for ParseErrs {
    fn from(err: Err<ErrorTree<I>>) -> Self {
        match find_parse_err(&err) {
            MsgErr::Status { .. } => {
                ParseErrs {
                   report: vec![],
                   source: None
                }
            }
            MsgErr::ParseErrs(parse_errs) => parse_errs
        }
    }
}


pub struct SubstErr {
}

impl SubstErr {

    pub fn report( &self ) -> Result<Report,MsgErr> {
        unimplemented!()
    }

}

pub struct ParseErrs {
    pub report: Vec<Report>,
    pub source: Option<Arc<String>>
}


impl ParseErrs {

    pub fn from_report(report: Report, source: Arc<String>) -> Self {
        Self {
            report: vec![report],
            source: Some(source)
        }
    }

    pub fn from_loc_span<I:Span>(message: &str, label: &str, span: I) -> MsgErr{

        let mut builder = Report::build(ReportKind::Error, (), 23);
        let report = builder.with_message(message).with_label(
            Label::new(span.location_offset()..(span.location_offset()+span.len())).with_message(label),
        ).finish();
        return ParseErrs::from_report(report, span.extra()).into();
    }

    pub fn from_range(message: &str, label: &str, range: Range<usize>, extra: SpanExtra ) -> MsgErr{

        let mut builder = Report::build(ReportKind::Error, (), 23);
        let report = builder.with_message(message).with_label(
            Label::new(range).with_message(label),
        ).finish();
        return ParseErrs::from_report(report, extra).into();
    }



    pub fn from_owned_span<I:Span>(message: &str, label: &str, span: I) -> MsgErr{
        let mut builder = Report::build(ReportKind::Error, (), 23);
        let report = builder.with_message(message).with_label(
            Label::new(span.location_offset()..(span.location_offset()+span.len())).with_message(label),
        ).finish();
        return ParseErrs::from_report(report, span.extra()).into();
    }


    pub fn print(&self) {
        if let Some(source) = self.source.as_ref() {
            for report in &self.report
            {
                report.print(Source::from(source.as_str())).unwrap_or_default()
            }
        }
    }

    pub fn fold<E:Into<ParseErrs>>( errs: Vec<E> ) -> ParseErrs {

        let errs : Vec<ParseErrs> = errs.into_iter().map( |e| e.into() ).collect();

        let source = if let Some(first) = errs.first() {
            if let Some(source) = first.source.as_ref().cloned() {
                Some(source)
            } else {
                None
            }
        } else {
            None
        };

        let mut rtn = ParseErrs{
            report: vec![],
            source
        };

        for err in errs {
            for report in err.report {
                rtn.report.push(report)
            }
        }
        rtn
    }
}
