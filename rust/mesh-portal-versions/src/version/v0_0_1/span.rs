use nom_locate::LocatedSpan;
use std::sync::Arc;
use nom::{AsBytes, FindSubstring, FindToken, InputLength, InputTake, InputTakeAtPosition, IResult, Slice};
use std::ops::{Deref, Range, RangeFrom, RangeTo};
use nom::error::{ErrorKind, ParseError};

pub type BorrowedSpan<'a> = LocatedSpan<&'a str, SpanExtra>;
pub type OwnedSpan = LocatedSpan<Morphile,SpanExtra>;
pub type SpanExtra = Arc<String>;

/*
pub trait ISpan<T> :InputLength+InputTake+InputTakeAtPosition+FindToken<T>+FindSubstring<T> where Self: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Clone{
    fn location_offset(&self) -> usize;
    fn location_line(&self) -> u32;
    fn get_line_beginning(&self) -> &[u8];
    fn get_column(&self) -> usize;
    fn get_utf8_column(&self) -> usize;
    fn naive_get_utf8_column(&self) -> usize;
}



 */

pub struct Spanner<I> {
    pub spans: Vec<I>
}

impl <I> Spanner<I> {
    pub fn new()->Self {
        Self {
            spans: vec![]
        }
    }
}

impl <I:ToString> ToString for Spanner<I> {
    fn to_string(&self) -> String {
        let mut rtn = String::new();

        for span in &self.spans {
            rtn.push_str(span.to_string().as_str() );
        }

        rtn
    }
}

pub struct NamedSpan<S> {
    pub name: String,
    pub span: S
}

impl <S:ToString> ToString for NamedSpan<S> {
    fn to_string(&self) -> String {
        self.span.to_string()
    }
}

/*
#[derive(Clone)]
pub struct OwnedSpan {
    pub extra: SpanExtra,
    pub offset: usize,
    pub len: usize,
    pub line: u32
}


impl OwnedSpan {
    pub fn as_str(&self) -> &str {
        self.extra.as_str().slice( self.offset..self.offset+self.len)
    }
}

impl ToString for OwnedSpan {
    fn to_string(&self) -> String {
        self.as_str().to_string()
    }
}

impl Slice<Range<usize>> for OwnedSpan
where

{
    fn slice(&self, range: Range<usize>) -> Self {
        Self {
            extra: self.extra.clone(),
            offset: self.offset+range.start,
            len: range.end-range.start,
            line: self.line
        }
    }
}


 */

pub fn create_span(s: &str) -> BorrowedSpan {
    BorrowedSpan::new_extra(s, Arc::new(s.to_string()))
}

pub struct SpanRevision {
    pub name: String,
    pub before: OwnedSpan,
}

impl SpanRevision {
    pub fn new(name: &str, before: &BorrowedSpan) -> Self {
        let before = to_owned(before);
        Self {
            name: name.to_string(),
            before
        }
    }
}



pub fn to_owned<'a>(span: &BorrowedSpan<'a>) -> OwnedSpan {
        LocatedSpan::new_extra(Morphile::new(span.to_string()),span.extra.clone())
}

#[derive(Clone)]
pub struct Morphile {
    pub string: String
}

impl ToString for Morphile {
    fn to_string(&self) -> String {
        self.string.clone()
    }
}

impl Morphile {
    pub fn new(string: String) -> Self {
        Self{string}
    }
}

impl Deref for Morphile {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl  AsBytes for Morphile {
    fn as_bytes(&self) -> &[u8] {
        self.string.as_bytes()
    }
}

pub struct SpanHistory {
    pub revisions: Vec<SpanRevision>,
    pub span: OwnedSpan
}

impl  SpanHistory {
    pub fn new(span: &BorrowedSpan) -> Self{
        Self {
            revisions: vec![],
            span: to_owned(span)
        }
    }

    pub fn with_revision(mut self, revision_name: &str, revision: &str ) -> Self {
        self.revisions.push(SpanRevision::new(revision_name, &create_span(revision) ));
        self
    }
}

impl Deref for SpanHistory {
    type Target = OwnedSpan;

    fn deref(&self) -> &Self::Target {
        &self.span
    }
}

