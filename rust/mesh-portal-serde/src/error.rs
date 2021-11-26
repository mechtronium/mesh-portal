use std::convert::Infallible;
use std::fmt::{Display, Formatter};
use std::string::FromUtf8Error;

use nom::error::VerboseError;

#[derive(Debug)]
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
