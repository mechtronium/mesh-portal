use crate::error::{MsgErr, ParseErrs};
use nom::error::{ErrorKind, ParseError};
use nom::{AsBytes, FindSubstring, FindToken, IResult, InputLength, InputTake, InputTakeAtPosition, Slice, Err, Offset, Compare, CompareResult, InputIter, Needed};
use nom_locate::LocatedSpan;
use std::borrow::Borrow;
use std::cell::Cell;
use std::ops;
use std::ops::{Deref, DerefMut, Range, RangeFrom, RangeTo};
use std::str::{CharIndices, Chars};
use std::sync::{Arc, Mutex};
use nom_supreme::error::{ErrorTree, StackContext};
use nom_supreme::ParserExt;
use crate::version::v0_0_1::parse::model::len;
use crate::version::v0_0_1::parse::Res;
use crate::version::v0_0_1::wrap::{Span, Wrap};
use serde::{Serialize,Deserialize};

//pub type OwnedSpan<'a> = LocatedSpan<&'a str, SpanExtra>;
pub type SpanExtra = Arc<String>;

pub fn new_span<'a>(s: &'a str) -> Wrap<LocatedSpan<&'a str,Arc<String>>>{
    let extra = Arc::new(s.to_string());
    let span = LocatedSpan::new_extra(s, extra);
    Wrap::new(span )
}

pub fn span_with_extra<'a>(s: &'a str, extra: Arc<String>) -> Wrap<LocatedSpan<&'a str,Arc<String>>>{
    Wrap::new(LocatedSpan::new_extra(s, extra))
}


#[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
pub struct Trace {
    pub range: Range<usize>,
    pub extra: SpanExtra
}

impl Trace {
    pub fn new( range: Range<usize>, extra: SpanExtra) -> Self {
        Self {
            range,
            extra
        }
    }

    pub fn at_offset( offset: usize, extra: SpanExtra ) -> Self {
        Self {
            range: offset..offset,
            extra
        }
    }

    pub fn scan<F,I:Span,O>( f: F, input: I ) -> Self where F: FnMut(I) -> Res<I,O>+Copy {
        let extra = input.extra();
        let range = input.location_offset()..len(f)(input);
        Self {
            range,
            extra
        }
    }
}


/*
// Return a TraceWrapper
pub fn tw<I,F,O>( f: F ) -> impl FnMut(I) -> Res<I,Tw<O>> where I:Span, F: FnMut(I) -> Res<I,O>+Copy {
  move |input:I| {
      let (next,output) = f(input.clone())?;
      let range = input.location_offset()..next.location_offset();
      let trace = Trace::new(range,input.extra());
      let tw = Tw {
          trace,
          item: output
      };
      Ok((next,tw))
  }
}

 */

// TraceWrapper
pub struct Tw<I> {
    pub trace: Trace,
    pub item: I
}

impl <I> Deref for Tw<I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl <I> DerefMut for Tw<I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
       &mut self.item
    }
}



#[derive(Debug,Clone)]
pub struct SliceStr {
    location_offset: usize,
    len: usize,
    string: Arc<String>,
}

impl ToString for SliceStr {
    fn to_string(&self) -> String {
        self.string.as_str().slice(self.location_offset..self.location_offset+self.len ).to_string()
    }
}

impl  SliceStr {
    pub fn new(string: String) -> Self {
        Self::from_arc(Arc::new(string))
    }

    pub fn from_arc(string: Arc<String>) -> Self {
        Self {
            len: string.len(),
            string,
            location_offset: 0,
            }
    }


    pub fn from(string: Arc<String>, location_offset: usize, len: usize) -> Self {
        Self { string,
            location_offset,
            len }
    }
}

impl SliceStr {
    pub fn as_str(&self) -> &str {
        &self.string.as_str().slice(self.location_offset..self.location_offset+self.len)
    }
}

impl Deref for SliceStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl AsBytes for SliceStr {
    fn as_bytes(&self) -> &[u8] {
println!("AS BYTES: {}",self.string.as_bytes().len());
        self.string.as_bytes().slice(self.location_offset..self.location_offset+self.len)
    }
}

impl Slice<Range<usize>> for SliceStr {
    fn slice(&self, range: Range<usize>) -> Self {
        SliceStr{
            location_offset: self.location_offset+range.start,
            len: range.end-range.start,
            string: self.string.clone()
        }
    }
}

impl Slice<RangeFrom<usize>> for SliceStr {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        SliceStr{
            location_offset: self.location_offset+range.start,
            len: self.len-range.start,
            string: self.string.clone()
        }
    }
}

impl Slice<RangeTo<usize>> for SliceStr {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        SliceStr{
            location_offset: self.location_offset,
            len: range.end,
            string: self.string.clone()
        }
    }
}

impl Compare<&str> for SliceStr {
    fn compare(&self, t: &str) -> CompareResult {
        self.as_str().compare(t)
    }

    fn compare_no_case(&self, t: &str) -> CompareResult {
        self.as_str().compare_no_case(t)
    }
}

impl InputLength for SliceStr {
    fn input_len(&self) -> usize {
        self.len
    }
}

impl Offset for SliceStr {
    fn offset(&self, second: &Self) -> usize {
        self.location_offset
    }
}

pub struct MyCharIterator {

}

pub struct MyChars {
    index:usize,
    slice:SliceStr
}

impl MyChars {
    pub fn new( slice: SliceStr ) -> Self {
        Self {
            index: 0,
            slice
        }
    }
}


impl Iterator for MyChars {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.slice.as_str().chars();
        let next = chars.nth(self.index );
        match next {
            None => None,
            Some(next) => {
                self.index = self.index +1;
                Some(next)
            }
        }
    }
}

pub struct CharIterator {
    index:usize,
    slice:SliceStr
}

impl CharIterator {
    pub fn new( slice: SliceStr ) -> Self {
        Self {
            index: 0,
            slice
        }
    }
}
impl Iterator for CharIterator {
    type Item = (usize,char);

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.slice.as_str().chars();
        let next = chars.nth(self.index );
        match next {
            None => None,
            Some(next) => {
                //let byte_index = self.index * std::mem::size_of::<char>();
                let byte_index = self.index;
                self.index = self.index +1;
                Some((byte_index,next))
            }
        }
    }
}




impl  InputIter for SliceStr {
    type Item = char;
    type Iter = CharIterator;
    type IterElem = MyChars;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        CharIterator::new(self.clone())
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        MyChars::new(self.clone())
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
    {
        self.as_str().position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.as_str().slice_index(count)
    }

}


impl  InputTakeAtPosition for SliceStr{
    type Item = char;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E> where P: Fn(Self::Item) -> bool {
        match self.split_at_position(predicate) {
            Err(Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(&self, predicate: P, e: ErrorKind) -> IResult<Self, Self, E> where P: Fn(Self::Item) -> bool {
        match self.as_str().position(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E> where P: Fn(Self::Item) -> bool {
        match self.split_at_position(predicate) {
            Err(Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(&self, predicate: P, e: ErrorKind) -> IResult<Self, Self, E> where P: Fn(Self::Item) -> bool {
        match self.as_str().position(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => {
                if self.as_str().input_len() == 0 {
                    Err(Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
        }
    }
}

impl InputTake for SliceStr {
    fn take(&self, count: usize) -> Self {
        self.slice(count..)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
         (self.slice(count..), self.slice(..count))
    }
}

impl FindSubstring<&str> for SliceStr {
    fn find_substring(&self, substr: &str) -> Option<usize> {
        self.as_str().find_substring(substr)
    }
}


#[cfg(test)]
pub mod test {
    use nom::Slice;
    use crate::version::v0_0_1::span::SliceStr;

    #[test]
    pub fn test () {
        let s = SliceStr::new("abc123".to_string() );
        assert_eq!( 6, s.len() );

        let s = s.slice(0..3);
        assert_eq!( 3, s.len() );
        assert_eq!( "abc", s.as_str() );

        println!("bytes: {}", s.as_bytes().len());
        println!("chars: {}", s.chars().count());

        let s = SliceStr::new("abc123".to_string() );
        assert_eq!( "123", s.slice(3..).as_str());
        assert_eq!( "abc", s.slice(..3).as_str());
    }

}