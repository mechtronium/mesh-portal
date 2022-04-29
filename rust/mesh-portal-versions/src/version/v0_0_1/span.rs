use crate::error::{MsgErr, ParseErrs};
use nom::error::{ErrorKind, ParseError};
use nom::{
    AsBytes, FindSubstring, FindToken, IResult, InputLength, InputTake, InputTakeAtPosition, Slice,
};
use nom_locate::LocatedSpan;
use std::borrow::Borrow;
use std::cell::Cell;
use std::ops;
use std::ops::{Deref, Range, RangeFrom, RangeTo};
use std::sync::{Arc, Mutex};
use nom_supreme::ParserExt;

pub type BorrowedSpan<'a> = LocatedSpan<&'a str, SpanExtra>;
pub type OwnedSpan = LocatedSpan<Morphile, SpanExtra>;
pub type SpanExtra = Arc<String>;

#[derive(Clone)]
pub enum Span<'a> {
    Borrowed(BorrowedSpan<'a>),
    Owned(OwnedSpan),
}

impl <'a> From<BorrowedSpan<'a>> for Span<'a> {
    fn from(span: BorrowedSpan<'a>) -> Self {
        Span::Borrowed(span)
    }
}

impl <'a> From<OwnedSpan> for Span<'a> {
    fn from(span: OwnedSpan) -> Self {
        Span::Owned(span)
    }
}

/*
impl<'a,'b> Into<BorrowedSpan<'b>> for Span<'a> {
    fn into(self) -> BorrowedSpan<'b> {
        match self {
            Span::Borrowed(borrowed) => {
                let extra = borrowed.extra.clone();
                let string = extra.as_str().slice( borrowed.location_offset()..borrowed.location_offset()+borrowed.len() );
                LocatedSpan::<&'b str,SpanExtra>::new_extra(string,extra.clone() )
            },
            Span::Owned(owned) => to_borrowed_span(&owned)
        }
    }
}
 */




impl<'a> Into<OwnedSpan> for Span<'a> {
    fn into(self) -> OwnedSpan {
        match self {
            Span::Borrowed(borrowed) => to_owned_span(&borrowed),
            Span::Owned(owned) => owned,
        }
    }
}

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
    pub spans: Vec<I>,
}

impl<I> Spanner<I> {
    pub fn new() -> Self {
        Self { spans: vec![] }
    }
}

impl<I: ToString> ToString for Spanner<I> {
    fn to_string(&self) -> String {
        let mut rtn = String::new();

        for span in &self.spans {
            rtn.push_str(span.to_string().as_str());
        }

        rtn
    }
}

pub struct NamedSpan<S> {
    pub name: String,
    pub span: S,
}

impl<S: ToString> ToString for NamedSpan<S> {
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

#[derive(Clone)]
pub struct SpanRevision<V> {
    pub kind: SpanRevisionKind,
    pub value: V,
    pub err: Option<SpanErr>
}

impl Into<SpanRevision<SpanValue>> for SpanRevision<OwnedSpan> {
    fn into(self) -> SpanRevision<SpanValue> {
        SpanRevision {
            kind: self.kind,
            value: SpanValue::Owned(self.value),
            err: None
        }
    }
}

impl SpanRevision<SpanValue> {
    pub fn to_owned_span(self, span_data: &SpanData) -> SpanRevision<OwnedSpan> {
        match &self.value {
            SpanValue::String(value) => {
                let value = unsafe {
                    OwnedSpan::new_from_raw_offset(
                        span_data.offset,
                        span_data.line,
                        Morphile::new(self.value.to_string()),
                        span_data.extra.clone(),
                    )
                };
                SpanRevision {
                    kind: self.kind,
                    value,
                    err: None
                }
            }
            SpanValue::Owned(value) => SpanRevision {
                kind: self.kind,
                value: value.clone(),
                err: None
            },
        }
    }
}

#[derive(Clone)]
pub enum SpanRevisionKind {
    VarSubst(String),
}

impl SpanRevision<SpanValue> {
    pub fn new(kind: SpanRevisionKind, value: &str) -> Self {
        Self {
            kind,
            value: SpanValue::String(value.to_string()),
            err: None
        }
    }
}

pub fn to_owned_span<'a>(span: &BorrowedSpan<'a>) -> OwnedSpan {
    LocatedSpan::new_extra(Morphile::new(span.to_string()), span.extra.clone())
}

pub fn to_borrowed_span(span: &OwnedSpan) -> BorrowedSpan {
    LocatedSpan::new_extra(span.string.as_str(), span.extra.clone())
}

#[derive(Clone)]
pub struct Morphile {
    pub string: String,
}

impl ToString for Morphile {
    fn to_string(&self) -> String {
        self.string.clone()
    }
}

impl Morphile {
    pub fn new(string: String) -> Self {
        Self { string }
    }
}

impl Deref for Morphile {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl AsBytes for Morphile {
    fn as_bytes(&self) -> &[u8] {
        self.string.as_bytes()
    }
}

#[derive(Clone)]
pub enum SpanValue {
    String(String),
    Owned(OwnedSpan),
}

impl SpanValue {
    pub fn as_str(&self) -> &str {
        match self {
            SpanValue::String(string) => string.as_str(),
            SpanValue::Owned(span) => span.as_str(),
        }
    }
}

impl ToString for SpanValue {
    fn to_string(&self) -> String {
        self.as_str().to_string()
    }
}
#[derive(Clone)]
pub struct SpanHistory<V> {
    pub original: OwnedSpan,
    pub revisions: Vec<SpanRevision<V>>,
    pub error: Box<Option<SpanErr>>
}

impl<V> SpanHistory<V> {
    pub fn new(original: &BorrowedSpan) -> Self {
        Self {
            original: to_owned_span(original),
            revisions: vec![],
            error: Box::new(None),
        }
    }

    pub fn last_revision(&self) -> Option<&SpanRevision<V>> {
        self.revisions.last()
    }
}

impl SpanHistory<SpanValue> {
    pub fn to_owned_span(self, span_data: &SpanData) -> SpanHistory<OwnedSpan> {
        let mut revisions = vec![];
        for revision in self.revisions {
            revisions.push(revision.to_owned_span(span_data));
        }
        SpanHistory {
            original: self.original,
            revisions,
            error: self.error,
        }
    }
}
impl SpanHistory<SpanValue> {
    pub fn value(&self) -> SpanValue {
        match self.last_revision() {
            None => SpanValue::String(self.original.string.clone()),
            Some(last) => last.value.clone(),
        }
    }

    pub fn revise(mut self, kind: SpanRevisionKind, value: &str) -> Self {
        self.revisions.push(SpanRevision::new(kind, value));
        self
    }

}

impl SpanHistory<OwnedSpan> {
    pub fn value(&self) -> OwnedSpan {
        match self.last_revision() {
            None => self.original.clone(),
            Some(last) => last.value.clone(),
        }
    }
}

impl<V> SpanHistory<V> {

    pub fn err(mut self, err: SpanErr) -> Self {
        self.error.replace(err);
        self
    }
}

impl<V> Deref for SpanHistory<V> {
    type Target = OwnedSpan;

    fn deref(&self) -> &Self::Target {
        &self.original
    }
}

#[derive(Clone)]
pub struct SpanData {
    pub extra: SpanExtra,
    pub offset: usize,
    pub line: u32,
    pub len: usize,
}

impl SpanData {
    pub fn new(extra: SpanExtra) -> Self {
        Self {
            extra,
            offset: 0,
            len: 0,
            line: 0,
        }
    }
}

/*
impl ops::Add<&String> for  SpanData {
    type Output = SpanData;

    fn add(self, rhs: &String) -> Self::Output {
        SpanData {
            offset: self.offset+rhs.len(),
            len: rhs.len(),
            line: self.line+rhs.as_bytes().iter().filter(|&&c| c == b'\n').count()
        }
    }
}


 */

impl ops::Add<SpanData> for SpanData {
    type Output = SpanData;

    fn add(self, rhs: SpanData) -> Self::Output {
        SpanData {
            extra: rhs.extra.clone(),
            offset: self.offset + rhs.offset,
            len: self.len + rhs.len,
            line: self.line + rhs.line,
        }
    }
}

impl<'a> ops::Add<&BorrowedSpan<'a>> for SpanData {
    type Output = SpanData;

    fn add(self, rhs: &BorrowedSpan<'a>) -> Self::Output {
        SpanData {
            extra: rhs.extra.clone(),
            offset: self.offset + rhs.location_offset(),
            len: self.len + rhs.len(),
            line: self.line + (rhs.location_line() - 1),
        }
    }
}

impl ops::Add<&OwnedSpan> for SpanData {
    type Output = SpanData;

    fn add(self, rhs: &OwnedSpan) -> Self::Output {
        SpanData {
            extra: rhs.extra.clone(),
            offset: self.offset + rhs.location_offset(),
            len: self.len + rhs.len(),
            line: self.line + (rhs.location_line() - 1),
        }
    }
}

impl<'a> From<BorrowedSpan<'a>> for SpanData {
    fn from(span: BorrowedSpan) -> Self {
        SpanData {
            extra: span.extra.clone(),
            offset: span.location_offset(),
            len: span.len(),
            line: span.location_line() - 1,
        }
    }
}

impl From<OwnedSpan> for SpanData {
    fn from(span: OwnedSpan) -> Self {
        SpanData {
            extra: span.extra.clone(),
            offset: span.location_offset(),
            len: span.len(),
            line: span.location_line() - 1,
        }
    }
}

pub struct SpanBox<I> {
    pub span: I,
}

pub struct SpanSegment<S> {
    pub history: SpanHistory<S>,
}

impl<S> SpanSegment<S> {
    pub fn new(history: SpanHistory<S>) -> Self {
        Self { history }
    }
}

impl SpanSegment<SpanValue> {
    pub fn value(&self) -> String {
        self.history.value().to_string()
    }

    pub fn to_owned_span(&self, span_data: &SpanData ) -> SpanSegment<OwnedSpan> {
        SpanSegment {
            history:self.history.clone().to_owned_span(span_data)
        }
    }

}

impl SpanSegment<OwnedSpan> {
    pub fn value(&self) -> OwnedSpan {
        self.history.value()
    }
}

pub struct SuperSpanBuilder {
    span: OwnedSpan,
    segments: Vec<SpanResult<SpanSegment<SpanValue>, SpanErr>>,
}

impl SuperSpanBuilder {
    pub fn new(span: OwnedSpan) -> Self {
        Self {
            span,
            segments: vec![],
        }
    }

    pub fn original<'b>(&'b mut self, original: &BorrowedSpan) -> SpanSegmentBuilder<'b> {
        SpanSegmentBuilder {
            err: None,
            builder: self,
            history: SpanHistory::new(original),
        }
    }

    fn commit(&mut self, segment: SpanResult<SpanSegment<SpanValue>, SpanErr>) {
        self.segments.push(segment);
    }

    pub fn build(self) -> SuperSpan {
        let mut span_data = {
            let offset = self.span.location_offset();
            let mut line = self.span.location_line() - 1;
            SpanData {
                extra: self.span.extra.clone(),
                offset,
                line,
                len: 0,
            }
        };

        let mut segments = vec![];
        let mut errs = vec![];
        for segment in self.segments {
            match segment {
                SpanResult::Ok(segment) => {
                    let value = segment.value();
                    let lines = value
                        .as_str()
                        .as_bytes()
                        .iter()
                        .filter(|&&c| c == b'\n')
                        .count() as u32;
                    let segment = segment.to_owned_span(&span_data);
                    span_data.line = span_data.line + lines;
                    span_data = span_data + &segment.value();
                    segments.push(SpanResult::Ok(segment));
                }
                SpanResult::Err(err) => segments.push(SpanResult::Err(err)),
            }
        }
        SuperSpan {
            span: self.span,
            segments,
            errs,
        }
    }
}

pub struct SuperSpan {
    pub span: OwnedSpan,
    pub segments: Vec<SpanResult<SpanSegment<OwnedSpan>, SpanErr>>,
    pub errs: Vec<MsgErr>,
}

pub struct SpanSegmentBuilder<'a> {
    builder: &'a mut SuperSpanBuilder,
    err: Option<SpanErrKind>,
    history: SpanHistory<SpanValue>,
}

impl<'a> SpanSegmentBuilder<'a> {
    pub fn new(builder: &'a mut SuperSpanBuilder, original: &BorrowedSpan) -> Self {
        Self {
            err: None,
            builder,
            history: SpanHistory::new(original),
        }
    }

    pub fn revise(&mut self, kind: SpanRevisionKind, value: &str) {
        self.history = self.history.clone().revise(kind, value);
    }

    pub fn err(mut self, err: SpanErrKind ) {
        self.err.replace(err);
    }
}

impl<'a> Drop for SpanSegmentBuilder<'a> {
    fn drop(&mut self) {
        if self.err.is_none() {
            self.builder.commit(SpanResult::Ok(SpanSegment::new(self.history.clone())))
        } else {
            self.builder.commit(SpanResult::Err(SpanErr::new(
                self.history.clone(),
                self.err.take().unwrap(),
            )))
        }
    }
}

#[derive(Clone)]
pub struct SpanErr {
    pub err: SpanErrKind,
    pub history: SpanHistory<SpanValue>,
}

impl SpanErr {
    pub fn new(history: SpanHistory<SpanValue>, err: SpanErrKind ) -> Self {
        Self { err, history }
    }
}

#[derive(Clone)]
pub enum SpanErrKind {
    VarNotFound(String),
    SubstNoParseErr(String)
}

pub enum SpanResult<T, E> {
    Ok(T),
    Err(E),
}

impl<T, E> ToString for SpanResult<T, E>
where
    T: ToString,
    E: ToString,
{
    fn to_string(&self) -> String {
        match self {
            SpanResult::Ok(ok) => ok.to_string(),
            SpanResult::Err(err) => err.to_string(),
        }
    }
}
