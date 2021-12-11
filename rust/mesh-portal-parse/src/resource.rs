use nom::error::{ErrorKind, VerboseError};

use mesh_portal_serde::version::latest::id::Address;

use crate::parse::{parse_address_segments, Res};

pub mod create {
    use mesh_portal_serde::version::latest::id::Address;
    use crate::parse::{Res, parse_address_segments};
    use nom::error::VerboseError;

    pub fn create_address(input: &str) -> Res<&str, CreateAddress> {

        let mut result = parse_address_segments(input);
        match result {
            Ok((next,mut segments)) => {
                let child_segment =  AddressCreationSegment::Exact(segments.last().ok_or(nom::Err::Error(VerboseError::from_error_kind(input,ErrorKind::Eof )))?.clone());
                segments.remove( segments.len() -1 );
                let parent = Address {
                    segments
                };

                let rtn = CreateAddress {
                    parent,
                    child_segment
                };

                Ok((next,rtn))
            }
            Err(err) => {
                Err(err.into())
            }
        }

    }

}



