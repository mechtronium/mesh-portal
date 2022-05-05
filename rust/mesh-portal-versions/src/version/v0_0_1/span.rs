use crate::error::{MsgErr, ParseErrs};
use nom::error::{ErrorKind, ParseError};
use nom::{AsBytes, FindSubstring, FindToken, IResult, InputLength, InputTake, InputTakeAtPosition, Slice, Err, Offset, Compare, CompareResult, InputIter, Needed};
use nom_locate::LocatedSpan;
use std::borrow::Borrow;
use std::cell::Cell;
use std::ops;
use std::ops::{Deref, Range, RangeFrom, RangeTo};
use std::str::{CharIndices, Chars};
use std::sync::{Arc, Mutex};
use ariadne::Span;
use nom_supreme::error::{ErrorTree, StackContext};
use nom_supreme::ParserExt;

pub type OwnedSpan<'a> = LocatedSpan<SliceStr<'a>, SpanExtra>;
pub type SpanExtra = Arc<String>;

pub fn new_span<S: ToString>(s: S) -> OwnedSpan{
    let s = Arc::new(s.to_string());
    let slice = SliceStr::from_arc(s.clone());
    OwnedSpan::new_extra(slice, s)
}

pub fn span_with_extra<S: ToString>(s: S, extra: Arc<String>) -> OwnedSpan{
    let slice = SliceStr::new(s.to_string() );
    OwnedSpan::new_extra(slice, extra)
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
                        span_data.line+1,
                        SliceStr::new(self.value.to_string()),
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
    pub fn new(original: &OwnedSpan) -> Self {
        Self {
            original: original.clone(),
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
            None => SpanValue::String(self.original.to_string()),
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
    previous: OwnedSpan,
    segments: Vec<SpanResult<SpanSegment<SpanValue>, SpanErr>>,
}

impl SuperSpanBuilder {
    pub fn new(previous: OwnedSpan) -> Self {
        Self {
            previous,
            segments: vec![],
        }
    }

    pub fn original<'b>(&'b mut self, original: &OwnedSpan) -> SpanSegmentBuilder<'b> {
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
            let offset = self.previous.location_offset();
            let mut line = self.previous.location_line() - 1;
            SpanData {
                extra: self.previous.extra.clone(),
                offset,
                line,
                len: 0,
            }
        };

        let mut segments = vec![];
        let mut errs = vec![];
        let mut span = String::new();
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
                    span.push_str( segment.value().to_string().as_str() );
                    segments.push(SpanResult::Ok(segment));
                }
                SpanResult::Err(err) => {
                    match &err.err {
                        SpanErrKind::VarNotFound(var) => {
                            span.push_str(format!("${{{}}}", var).as_str() );
                        }
                        SpanErrKind::SubstNoParseErr(_) => {}
                    }
                    segments.push(SpanResult::Err(err))
                },
            }
        }

        use crate::version::v0_0_1::span::new_span as create_span;
        let span = create_span( span.as_str());
        // now lets colate the errors
        {
            let mut offset = 0;
            for segment in &segments {
                match segment {
                    SpanResult::Err(err) => {
                        let span = span.slice( offset..offset+err.history.len()+3);
                        match &err.err {
                            SpanErrKind::VarNotFound(var) => {
                                errs.push(ParseErrs::from_loc_span(format!("Variable '{}' cannot be resolved in this scope", var ).as_str(),"Var Not Found", span));
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
                offset = segment.offset()+segment.len();
            }
        }

        SuperSpan {
            span,
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
    pub fn new(builder: &'a mut SuperSpanBuilder, original: &OwnedSpan) -> Self {
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

impl SpanResult<SpanSegment<OwnedSpan>,SpanErr> {
    pub fn offset(&self) -> usize {
        match self {
            SpanResult::Ok(segment) => {
                segment.history.location_offset()
            }
            SpanResult::Err(err) => {
                err.history.location_offset()
            }
        }
    }
    pub fn len(&self) -> usize {
        match self {
            SpanResult::Ok(segment) => {
                segment.history.len()
            }
            SpanResult::Err(err) => {
                err.history.len()
            }
        }
    }
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









#[derive(Debug,Clone)]
pub struct SliceStr<'a> {
    location_offset: usize,
    len: usize,
    string: Arc<String>,
}

impl <'a> ToString for SliceStr<'a> {
    fn to_string(&self) -> String {
        self.string.as_str().slice(self.location_offset..self.location_offset+self.len ).to_string()
    }
}

impl <'a> SliceStr<'a> {
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
            location_offset: self.location_offset+range.start(),
            len: range.end()-range.start(),
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

/*
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
 */


impl<'a> InputIter for SliceStr<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;
    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.as_str().char_indices()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.chars()
    }
    fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
    {
        for (o, c) in self.as_str().char_indices() {
            if predicate(c) {
                return Some(o);
            }
        }
        None
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        let mut cnt = 0;
        for (index, _) in self.as_str().char_indices() {
            if cnt == count {
                return Ok(index);
            }
            cnt += 1;
        }
        if cnt == count {
            return Ok(self.len());
        }
        Err(Needed::Unknown)
    }
}

/*impl  InputIter for SliceStr {
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

 */

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