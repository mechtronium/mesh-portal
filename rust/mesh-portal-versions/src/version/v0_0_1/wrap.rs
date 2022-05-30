use crate::version::v0_0_1::span::Trace;
use core::fmt::Formatter;
use nom::error::{ErrorKind, ParseError};
use nom::{
    AsBytes, AsChar, Compare, CompareResult, FindSubstring, IResult, InputIter, InputLength,
    InputTake, InputTakeAtPosition, Needed, Offset, Slice,
};
use nom_locate::LocatedSpan;
use regex::internal::Input;
use std::ops::{Deref, Range, RangeFrom, RangeTo};
use std::sync::Arc;
use serde::{Serialize,Deserialize};

pub trait Span:
    Clone
    + ToString
    + AsBytes
    + Slice<Range<usize>>
    + Slice<RangeTo<usize>>
    + Slice<RangeFrom<usize>>
    + InputLength
    + Offset
    + InputTake
    + InputIter<Item = char>
    + InputTakeAtPosition<Item = char>
    + Compare<&'static str>
    + FindSubstring<&'static str>
    + core::fmt::Debug
where
    Self: Sized,
    <Self as InputTakeAtPosition>::Item: AsChar,
{
    fn location_offset(&self) -> usize;

    fn location_line(&self) -> u32;

    fn get_column(&self) -> usize;

    fn extra(&self) -> Arc<String>;

    fn len(&self) -> usize;

    fn range(&self) -> Range<usize>;

    fn trace(&self) -> Trace {
        Trace {
            range: self.range(),
            extra: self.extra(),
        }
    }
}

#[derive(Debug,Clone)]
pub struct Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    input: I,
}

impl<I> Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    pub fn new(input: I) -> Self {
        Self { input }
    }
}

impl<I> Deref for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.input
    }
}

impl<'a> Span for Wrap<LocatedSpan<&'a str, Arc<String>>> {
    fn location_offset(&self) -> usize {
        self.input.location_offset()
    }

    fn get_column(&self) -> usize {
        self.input.get_column()
    }

    fn location_line(&self) -> u32 {
        self.input.location_line()
    }

    fn extra(&self) -> Arc<String> {
        self.input.extra.clone()
    }

    fn len(&self) -> usize {
        self.input.len()
    }

    fn range(&self) -> Range<usize> {
        Range {
            start: self.location_offset(),
            end: self.location_offset() + self.len(),
        }
    }
}

impl<I> AsBytes for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn as_bytes(&self) -> &[u8] {
        self.input.as_bytes()
    }
}

impl<I> Slice<Range<usize>> for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn slice(&self, range: Range<usize>) -> Self {
        Self::new(self.input.slice(range))
    }
}

impl<I> Slice<RangeFrom<usize>> for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Self::new(self.input.slice(range))
    }
}

impl<I> Slice<RangeTo<usize>> for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self::new(self.input.slice(range))
    }
}

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

impl<'a> Compare<&'static str> for Wrap<LocatedSpan<&'a str, Arc<String>>> {
    fn compare(&self, t: &str) -> CompareResult {
        self.input.compare(t)
    }

    fn compare_no_case(&self, t: &str) -> CompareResult {
        self.input.compare_no_case(t)
    }
}

impl<I> InputLength for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn input_len(&self) -> usize {
        self.input.input_len()
    }
}

impl<I> Offset for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn offset(&self, second: &Self) -> usize {
        self.input.offset(&second.input)
    }
}

impl<I> InputIter for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    type Item = <I as InputIter>::Item;
    type Iter = <I as InputIter>::Iter;
    type IterElem = <I as InputIter>::IterElem;

    fn iter_indices(&self) -> Self::Iter {
        self.input.iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.input.iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.input.position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        self.input.slice_index(count)
    }
}

impl<I> InputTake for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn take(&self, count: usize) -> Self {
        Wrap::new(self.input.take(count))
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (left, right) = self.input.take_split(count);
        (Wrap::new(left), Wrap::new(right))
    }
}

impl<I> ToString for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn to_string(&self) -> String {
        self.input.to_string()
    }
}

impl<'a> FindSubstring<&str> for Wrap<LocatedSpan<&'a str, Arc<String>>> {
    fn find_substring(&self, substr: &str) -> Option<usize> {
        self.input.find_substring(substr)
    }
}

impl<I> InputTakeAtPosition for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    type Item = <I as InputIter>::Item;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.position(predicate) {
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position1(predicate, e) {
            Err(nom::Err::Incomplete(_)) => {
                if self.input_len() == 0 {
                    Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
            res => res,
        }
    }
}

// TraceWrap
#[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq)]
pub struct Tw<W> {
    pub trace: Trace,
    pub w: W
}

impl <W> Tw<W> {
    pub fn new<I:Span>( span: I, w: W ) -> Self {
        Self {
            trace: span.trace(),
            w
        }
    }
}

impl <W> ToString for Tw<W> where W:ToString {
    fn to_string(&self) -> String {
        self.w.to_string()
    }
}

impl <W> Deref for Tw<W> {
    type Target = W;

    fn deref(&self) -> &Self::Target {
        &self.w
    }
}