use std::convert::Infallible;
use std::fmt::{Display, Formatter};
use std::string::FromUtf8Error;

use nom::error::VerboseError;
use semver::{ReqParseError, SemVerError};
use std::num::ParseIntError;
use http::header::ToStrError;
use http::uri::InvalidUri;
use nom_supreme::error::ErrorTree;

#[derive(Debug,Eq,PartialEq)]
pub struct Error {
    pub message: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message.as_str())
    }
}

impl std::error::Error for Error {}

impl From<String> for Error {
    fn from(message: String) -> Self {
        Self { message }
    }
}

impl From<FromUtf8Error> for Error {
    fn from(message: FromUtf8Error) -> Self {
        Self {
            message: message.to_string(),
        }
    }
}

impl From<&str> for Error {
    fn from(message: &str) -> Self {
        Self {
            message: message.to_string(),
        }
    }
}

impl From<Box<bincode::ErrorKind>> for Error {
    fn from(message: Box<bincode::ErrorKind>) -> Self {
        Self {
            message: message.to_string(),
        }
    }
}

impl From<Infallible> for Error {
    fn from(i: Infallible) -> Self {
        Self {
            message: i.to_string(),
        }
    }
}

impl From<nom::Err<VerboseError<&str>>> for Error {
    fn from(error: nom::Err<VerboseError<&str>>) -> Self {
        Self {
            message: error.to_string()
        }
    }
}

impl From<nom::Err<ErrorTree<&str>>> for Error {
    fn from(error: nom::Err<ErrorTree<&str>>) -> Self {
        Self {
            message: error.to_string()
        }
    }
}

impl From<ErrorTree<&str>> for Error {
    fn from(error: ErrorTree<&str>) -> Self {
        Self {
            message: error.to_string()
        }
    }
}

impl From<ReqParseError> for Error {
    fn from(error: ReqParseError) -> Self {
        Self {
            message: error.to_string()
        }
    }
}

impl From<SemVerError> for Error {
    fn from(error: SemVerError) -> Self {
        Self {
            message: error.to_string()
        }
    }
}

impl From<strum::ParseError> for Error {
    fn from(error: strum::ParseError) -> Self {
        Self {
            message: error.to_string()
        }
    }
}

impl From<ParseIntError> for Error {
    fn from(x: ParseIntError) -> Self {
        Self{
            message: x.to_string()
        }
    }
}

impl From<regex::Error> for Error {
    fn from(x: regex::Error) -> Self {
        Self{
            message: x.to_string()
        }
    }
}

impl From<InvalidUri> for Error {
    fn from(x: InvalidUri) -> Self {
        Self{
            message: x.to_string()
        }
    }
}

impl From<http::Error> for Error {
    fn from(x: http::Error) -> Self {
        Self{
            message: x.to_string()
        }
    }
}


impl From<ToStrError> for Error {
    fn from(x: ToStrError) -> Self {
        Self{
            message: x.to_string()
        }
    }
}
