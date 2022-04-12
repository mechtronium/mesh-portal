use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter};
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
use ariadne::{Label, Report, ReportKind, Source};
use crate::version::v0_0_1::Span;

pub enum MsgErr {
    Status {
        status: u16,
        message: String,
    },
    Report{
        report: Report,
        source: String
    }
}

impl Debug for MsgErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MsgErr::Status { status, message } => {
                f.write_str(format!("{}: {}",status,message).as_str() )
            }
            MsgErr::Report { .. } => {
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
            MsgErr::Report { report, source } => {
                report.print(Source::from(source)).unwrap_or_default()
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
            MsgErr::Report { .. } => {
                500u16
            }
        }
    }

    fn message(&self) -> String {
        match self {
            MsgErr::Status { status,message } => {
                message.clone()
            }
            MsgErr::Report { .. } => {
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
            MsgErr::Report { .. } => {
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

impl From<nom::Err<ErrorTree<&str>>> for MsgErr {
    fn from(error: nom::Err<ErrorTree<&str>>) -> Self {
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

impl <'a> From<nom::Err<ErrorTree<Span<'a>>>> for MsgErr {
    fn from(err: Err<ErrorTree<Span<'a>>>) -> Self {
        fn handle<'b>(err: ErrorTree<Span<'b>>) -> MsgErr {
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

