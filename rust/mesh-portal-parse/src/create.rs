use nom::{Err, Parser};
use nom::combinator::map_res;
use nom::error::{ErrorKind, make_error, ParseError, VerboseError};
use nom_supreme::ParserExt;

use mesh_portal_serde::version::latest::id::Address;

use crate::parse::{parse_address_segments, Res};

