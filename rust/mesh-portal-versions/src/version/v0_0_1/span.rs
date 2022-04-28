use nom_locate::LocatedSpan;
use std::sync::Arc;
use nom::{FindSubstring, FindToken, InputLength, InputTake, InputTakeAtPosition, Slice};
use std::ops::{Deref, Range, RangeFrom, RangeTo};

pub type Span<'a> = LocatedSpan<&'a str, SpanExtra>;
pub type SpanExtra = Arc<String>;

pub trait ISpan<T> :InputLength+InputTake+InputTakeAtPosition+FindToken<T>+FindSubstring<T> where Self: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Clone{
    fn location_offset(&self) -> usize;
    fn location_line(&self) -> u32;
    fn get_line_beginning(&self) -> &[u8];
    fn get_column(&self) -> usize;
    fn get_utf8_column(&self) -> usize;
    fn naive_get_utf8_column(&self) -> usize;
}



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

#[derive(Clone)]
pub struct OwnedSpan {
    pub extra: SpanExtra,
    pub offset: usize,
    pub len: usize
}

impl <'a> From<Span<'a>> for OwnedSpan {
    fn from( span: Span<'a> ) -> Self {
        Self {
            extra: span.extra.clone(),
            offset: span.location_offset(),
            len: span.len()
        }
    }
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
            len: range.end-range.start
        }
    }
}


pub fn create_span(s: &str) -> Span {

    Span::new_extra(s, Arc::new(s.to_string()))
}

pub struct SpanRevision {
    pub name: String,
    pub before: OwnedSpan,
}

impl SpanRevision {
    pub fn new( name: &str, before: Span ) -> Self {
        let before = OwnedSpan::from(before);
        Self {
            name: name.to_string(),
            before
        }
    }
}

pub struct SpanHistory {
    pub revisions: Vec<SpanRevision>,
    pub span: OwnedSpan
}

impl  SpanHistory {
    pub fn new<'a>(span: Span<'a>) -> Self{
        Self {
            revisions: vec![],
            span: OwnedSpan::from(span)
        }
    }

    pub fn push( &mut self, revision_name: &str, span: Span) {
        self.revisions.push(SpanRevision::new(revision_name, span));
    }
}

impl Deref for SpanHistory {
    type Target = OwnedSpan;

    fn deref(&self) -> &Self::Target {
        &self.span
    }
}
