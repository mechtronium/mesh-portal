use core::fmt::Formatter;
use nom::error::{ErrorKind, ParseError};
use nom::{
    AsBytes, AsChar, Compare, CompareResult, FindSubstring, InputIter, InputLength, InputTake,
    InputTakeAtPosition, IResult, Needed, Offset, Slice,
};
use nom_locate::LocatedSpan;
use regex::internal::Input;
use std::ops::{Deref, DerefMut, Range, RangeFrom, RangeTo};
use std::sync::Arc;
use serde::{Deserialize, Serialize};
use cosmic_nom::len;
use cosmic_nom::Res;

/*
impl <'a> Compare<&str> for Wrap<LocatedSpan<&'a str,Arc<String>>>
{
    fn compare(&self, t: &str) -> CompareResult {
        self.input.compare(t)
    }

    fn compare_no_case(&self, t: &str) -> CompareResult {
        self.input.compare_no_case(t)
    }
}

 */
