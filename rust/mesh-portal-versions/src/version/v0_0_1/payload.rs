pub mod payload {
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;
    use std::ops::{Deref, DerefMut};

    use crate::error::{MsgErr, ParseErrs};
    use crate::version::v0_0_1::bin::Bin;
    use crate::version::v0_0_1::entity::request::{Method, Rc, RcCommandType, RequestCore};
    use crate::version::v0_0_1::id::id::{GenericKind, GenericKindBase, Meta, Point, PointCtx, PointVar};
    use crate::version::v0_0_1::particle::particle::{Particle, Status, Stub};
    use crate::version::v0_0_1::selector::selector::{KindSelector, PointSelector};
    use crate::version::v0_0_1::util::{ValueMatcher, ValuePattern};
    use http::{Uri};
    use std::str::FromStr;
    use std::sync::Arc;
    use crate::version::v0_0_1::http::HttpMethod;
    use crate::version::v0_0_1::msg::MsgMethod;
    use crate::version::v0_0_1::parse::{CtxResolver, Env, ToResolved};

    #[derive(
        Debug,
        Clone,
        Serialize,
        Deserialize,
        Eq,
        PartialEq,
        strum_macros::Display,
        strum_macros::EnumString,
    )]
    pub enum PayloadType {
        Empty,
        List,
        Map,
        Point,
        Text,
        Boolean,
        Int,
        Meta,
        Bin,
        Stub,
        Status,
        Particle,
        Errors,
        Json,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
    pub enum Payload {
        Empty,
        List(PayloadList),
        Map(PayloadMap),
        Point(Point),
        Text(String),
        Stub(Stub),
        Meta(Meta),
        Bin(Bin),
        Boolean(bool),
        Int(i64),
        Status(Status),
        Particle(Particle),
        Errors(Errors),
        Json(serde_json::Value),
    }

    impl Payload {
        pub fn to_text(self) -> Result<String, MsgErr> {
            if let Payload::Text(text) = self {
                Ok(text)
            } else {
                Err("not a 'Text' payload".into())
            }
        }

        pub fn is_some(&self) -> bool {
            if let Self::Empty = self {
                false
            } else {
                true
            }
        }

        pub fn from_bin(bin: Bin) -> Self {
            Self::Bin(bin)
        }

        pub fn payload_type(&self) -> PayloadType {
            match self {
                Payload::Empty => PayloadType::Empty,
                Payload::List(list) => PayloadType::List,
                Payload::Map(map) => PayloadType::Map,
                Payload::Point(_) => PayloadType::Point,
                Payload::Text(_) => PayloadType::Text,
                Payload::Stub(_) => PayloadType::Stub,
                Payload::Meta(_) => PayloadType::Meta,
                Payload::Bin(_) => PayloadType::Bin,
                Payload::Boolean(_) => PayloadType::Boolean,
                Payload::Int(_) => PayloadType::Int,
                Payload::Status(_) => PayloadType::Status,
                Payload::Particle(_) => PayloadType::Particle,
                Payload::Errors(_) => PayloadType::Errors,
                Payload::Json(_) => PayloadType::Json,
            }
        }

        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            match self {
                Payload::Empty => Ok(Arc::new(vec![])),
                Payload::List(list) => list.to_bin(),
                Payload::Map(map) => map.to_bin(),
                _ => Err("not supported".into()),
            }
        }
    }

    impl TryInto<HashMap<String, Payload>> for Payload {
        type Error = MsgErr;

        fn try_into(self) -> Result<HashMap<String, Payload>, Self::Error> {
            match self {
                Payload::Map(map) => Ok(map.map),
                _ => Err("Payload type must a Map".into()),
            }
        }
    }

    impl TryInto<String> for Payload {
        type Error = MsgErr;

        fn try_into(self) -> Result<String, Self::Error> {
            match self {
                Payload::Text(text) => Ok(text),
                Payload::Bin(bin) => Ok(String::from_utf8(bin.to_vec())?),
                _ => Err("Payload type must an Text".into()),
            }
        }
    }

    impl TryInto<Point> for Payload {
        type Error = MsgErr;

        fn try_into(self) -> Result<Point, Self::Error> {
            match self {
                Payload::Point(point) => Ok(point),
                _ => Err("Payload type must an Address".into()),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct PayloadMap {
        pub map: HashMap<String, Payload>,
    }

    impl Into<Payload> for PayloadMap {
        fn into(self) -> Payload {
            Payload::Map(self)
        }
    }

    impl Deref for PayloadMap {
        type Target = HashMap<String, Payload>;

        fn deref(&self) -> &Self::Target {
            &self.map
        }
    }

    impl DerefMut for PayloadMap {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.map
        }
    }

    impl Default for PayloadMap {
        fn default() -> Self {
            Self {
                map: Default::default(),
            }
        }
    }
    /*
    impl <ToKind,FromKind> TryInto<ToKind> for PayloadMap<FromKind> {
        type Error = Error;

        fn try_into(self) -> Result<ToKind, Self::Error> {
            let mut map = HashMap::new();
            for (k,v) in self.map {
                map.insert( k, v.try_into()? );
            }
            Ok(Self{map})
        }
    }

     */

    impl PayloadMap {
        /*
        pub fn new(constraints: MapConstraints<KEY,ADDRESS,IDENTIFIER,KIND> ) -> Self {
            Self{
        //        constraints,
                map: HashMap::new()
            }
        }

         */
        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            Ok(Arc::new(bincode::serialize(&self)?))
        }

        pub fn new() -> Self {
            Self {
                map: HashMap::new(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Errors {
        map: HashMap<String, String>,
    }

    impl Errors {
        pub fn empty() -> Self {
            Self {
                map: HashMap::new(),
            }
        }

        pub fn default(message: &str) -> Self {
            let mut map = HashMap::new();
            map.insert("default".to_string(), message.to_string());
            Self { map }
        }
    }

    impl ToString for Errors {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            for (index, (_, value)) in self.iter().enumerate() {
                rtn.push_str(value.as_str());
                if index == self.len() - 1 {
                    rtn.push_str("\n");
                }
            }
            rtn
        }
    }

    impl Deref for Errors {
        type Target = HashMap<String, String>;

        fn deref(&self) -> &Self::Target {
            &self.map
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
    pub enum Primitive {
        Text(String),
        Point(Point),
        Stub(Stub),
        Meta(Meta),
        Bin(Bin),
        Boolean(bool),
        Int(i64),
        Status(Status),
        Particle(Particle),
        Errors(Errors),
    }

    impl Primitive {
        pub fn primitive_type(&self) -> PrimitiveType {
            match self {
                Primitive::Text(_) => PrimitiveType::Text,
                Primitive::Point(_) => PrimitiveType::Address,
                Primitive::Stub(_) => PrimitiveType::Stub,
                Primitive::Meta(_) => PrimitiveType::Meta,
                Primitive::Bin(_) => PrimitiveType::Bin,
                Primitive::Boolean(_) => PrimitiveType::Boolean,
                Primitive::Int(_) => PrimitiveType::Int,
                Primitive::Status(_) => PrimitiveType::Status,
                Primitive::Particle(_) => PrimitiveType::Resource,
                Primitive::Errors(_) => PrimitiveType::Errors,
            }
        }

        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            match self {
                Primitive::Text(text) => {
                    let text = text.into_bytes();
                    Ok(Arc::new(text))
                }
                Primitive::Point(point) => {
                    let point = point.to_string().into_bytes();
                    Ok(Arc::new(point))
                }
                Primitive::Stub(stub) => Ok(Arc::new(bincode::serialize(&stub)?)),
                Primitive::Meta(meta) => Ok(Arc::new(bincode::serialize(&meta)?)),
                Primitive::Bin(bin) => Ok(bin),
                Primitive::Boolean(flag) => Ok(Arc::new(flag.to_string().into_bytes())),
                Primitive::Int(int) => Ok(Arc::new(int.to_string().into_bytes())),
                Primitive::Status(status) => {
                    let status = status.to_string().into_bytes();
                    Ok(Arc::new(status))
                }
                Primitive::Particle(resource) => Ok(Arc::new(bincode::serialize(&resource)?)),
                Primitive::Errors(errors) => Ok(Arc::new(bincode::serialize(&errors)?)),
            }
        }
    }

    impl TryInto<Bin> for Primitive {
        type Error = MsgErr;

        fn try_into(self) -> Result<Bin, Self::Error> {
            match self {
                Primitive::Bin(bin) => Ok(bin),
                _ => Err("Primitive must be of type Bin".into()),
            }
        }
    }

    impl TryInto<Point> for Primitive {
        type Error = MsgErr;

        fn try_into(self) -> Result<Point, Self::Error> {
            match self {
                Primitive::Point(point) => Ok(point),
                _ => Err("Primitive must be of type Address".into()),
            }
        }
    }

    impl TryInto<String> for Primitive {
        type Error = MsgErr;

        fn try_into(self) -> Result<String, Self::Error> {
            match self {
                Primitive::Text(text) => Ok(text),
                _ => Err("Primitive must be of type Text".into()),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct PayloadList {
        pub list: Vec<Box<Payload>>,
    }

    impl ToString for PayloadList {
        fn to_string(&self) -> String {
            "[]".to_string()
        }
    }

    impl PayloadList {
        pub fn new() -> Self {
            Self { list: vec![] }
        }
        pub fn to_bin(self) -> Result<Bin, MsgErr> {
            Ok(Arc::new(bincode::serialize(&self)?))
        }
    }

    impl Deref for PayloadList {
        type Target = Vec<Box<Payload>>;

        fn deref(&self) -> &Self::Target {
            &self.list
        }
    }

    impl DerefMut for PayloadList {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.list
        }
    }
    #[derive(Debug, Clone, strum_macros::Display, Eq, PartialEq, Hash, Serialize, Deserialize)]
    pub enum PrimitiveType {
        Address,
        Text,
        Boolean,
        Code,
        Int,
        Meta,
        Bin,
        Stub,
        Status,
        Resource,
        Errors,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct ListPattern {
        pub primitive: PayloadType,
        pub range: NumRange,
    }

    impl ListPattern {
        pub fn is_match(&self, list: &PayloadList) -> Result<(), MsgErr> {
            /*
            for i in &list.list {
                if self.primitive != i.primitive_type() {
                    return Err(format!(
                        "Primitive List expected: {} found: {}",
                        self.primitive.to_string(),
                        i.primitive_type().to_string()
                    )
                    .into());
                }
            }

            Ok(())

             */
            unimplemented!()
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum NumRange {
        MinMax { min: usize, max: usize },
        Exact(usize),
        Any,
    }
    pub type PayloadTypePatternCtx = PayloadTypePatternDef<PointCtx>;
    pub type PayloadTypePatternVar = PayloadTypePatternDef<PointVar>;

    #[derive(Debug, Clone, Serialize, Deserialize )]
    pub enum PayloadTypePatternDef<Pnt> {
        Empty,
        Primitive(PayloadType),
        List(ListPattern),
        Map(Box<MapPatternDef<Pnt>>),
    }



    impl ToResolved<PayloadTypePatternDef<Point>> for PayloadTypePatternDef<PointCtx>{
        fn to_resolved(self, env: &Env ) -> Result<PayloadTypePatternDef<Point>, MsgErr> {
            match self {
                PayloadTypePatternDef::Empty => Ok(PayloadTypePatternDef::Empty),
                PayloadTypePatternDef::Primitive(payload_type) =>Ok(PayloadTypePatternDef::Primitive(payload_type)),
                PayloadTypePatternDef::List(list)=>Ok(PayloadTypePatternDef::List(list)),
                PayloadTypePatternDef::Map(map)  => {
                    Err("MapPatternCtx resolution not supported yet...".into())
                }
            }
        }
    }


    impl ToResolved<PayloadTypePatternCtx> for PayloadTypePatternVar{
        fn to_resolved(self, env: &Env ) -> Result<PayloadTypePatternCtx, MsgErr> {
            match self {
                PayloadTypePatternVar::Empty => Ok(PayloadTypePatternCtx::Empty),
                PayloadTypePatternVar::Primitive(payload_type) =>Ok(PayloadTypePatternCtx::Primitive(payload_type)),
                PayloadTypePatternVar::List(list)=>Ok(PayloadTypePatternCtx::List(list)),
                PayloadTypePatternVar::Map(map)  => {
                    Err("MapPatternCtx resolution not supported yet...".into())
                }
            }
        }
    }



    impl <Pnt> PayloadTypePatternDef<Pnt> {
        pub fn is_match(&self, payload: &Payload) -> Result<(), ()> {
            unimplemented!();
            /*
            match self {
                PayloadTypePattern::Empty => {
                    if payload.payload_type() == PayloadType::Empty {
                        Ok(())
                    } else {
                        Err(format!(
                            "Payload expected: Empty found: {}",
                            payload.payload_type().to_string()
                        )
                        .into())
                    }
                }
                PayloadTypePattern::Primitive(expected) => {
                    if let Payload::Primitive(found) = payload {
                        if *expected == found.primitive_type() {
                            Ok(())
                        } else {
                            Err(format!(
                                "Payload Primitive expected: {} found: {}",
                                expected.to_string(),
                                found.primitive_type().to_string()
                            )
                            .into())
                        }
                    } else {
                        Err(format!(
                            "Payload expected: {} found: {}",
                            expected.to_string(),
                            payload.payload_type().to_string()
                        )
                        .into())
                    }
                }
                PayloadTypePattern::List(expected) => {
                    if let Payload::List(found) = payload {
                        expected.is_match(found)
                    } else {
                        Err(format!(
                            "Payload expected: List found: {}",
                            payload.payload_type().to_string()
                        )
                        .into())
                    }
                }
                PayloadTypePattern::Map(expected) => {
                    if let Payload::Map(found) = payload {
                        expected.is_match(found)
                    } else {
                        Err(format!(
                            "Payload expected: {} found: {}",
                            expected.to_string(),
                            payload.payload_type().to_string()
                        )
                        .into())
                    }
                }
            }

             */
        }
    }

    pub type PayloadPatternVar= PayloadPatternDef<PointVar>;
    pub type PayloadPatternCtx = PayloadPatternDef<PointCtx>;
    pub type PayloadPattern = PayloadPatternDef<Point>;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PayloadPatternDef<Pnt> {
        pub structure: PayloadTypePatternDef<Pnt>,
        pub format: Option<PayloadFormat>,
        pub validator: Option<CallWithConfigDef<Pnt>>,
    }
    impl ToResolved<PayloadPatternCtx> for PayloadPatternVar{
        fn to_resolved(self, env: &Env) -> Result<PayloadPatternCtx, MsgErr> {
            let mut errs = vec![];
            let structure = match self.structure.to_resolved(env) {
                Ok(structure) => Some(structure),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };
            let validator = match self.validator {
                None => None,
                Some(validator) => {
                    match validator.to_resolved(env) {
                        Ok(validator) => Some(validator),
                        Err(err) => {
                            errs.push(err);
                            None
                        }
                    }
                }
            };


            if errs.is_empty() {
                Ok(PayloadPatternCtx {
                    structure: structure.expect("structure"),
                    validator: validator,
                    format: self.format
                })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }

    impl ToResolved<PayloadPattern> for PayloadPatternCtx{
        fn to_resolved(self, resolver: &Env) -> Result<PayloadPattern, MsgErr> {
            let mut errs = vec![];
            let structure = match self.structure.to_resolved(resolver) {
              Ok(structure) => Some(structure),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };
            let validator = match self.validator {
                None => None,
                Some(validator) => {
                    match validator.to_resolved(resolver) {
                        Ok(validator) => Some(validator),
                        Err(err) => {
                            errs.push(err);
                            None
                        }
                    }
                }
            };


            if errs.is_empty() {
                Ok(PayloadPattern {
                    structure: structure.expect("structure"),
                    validator: validator,
                    format: self.format
                })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }


        impl <Pnt> ValueMatcher<Payload> for PayloadPatternDef<Pnt> {
        fn is_match(&self, payload: &Payload) -> Result<(), ()> {
            self.structure.is_match(&payload)?;

            // more matching to come... not sure exactly how to match Format and Validation...
            Ok(())
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CallWithConfigDef<Pnt> {
        pub call: CallDef<Pnt>,
        pub config: Option<Pnt>,
    }

    pub type CallWithConfig = CallWithConfigDef<Point>;
    pub type CallWithConfigCtx = CallWithConfigDef<PointCtx>;
    pub type CallWithConfigVar = CallWithConfigDef<PointVar>;

    impl ToResolved<CallWithConfigCtx> for CallWithConfigVar {
        fn to_resolved(self, resolver: &Env) -> Result<CallWithConfigCtx, MsgErr> {
            let mut errs = vec![];
            let call = match self.call.to_resolved(resolver) {
                Ok(call) => Some(call),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };
            let config = match self.config {
                None => None,
                Some(config) => {
                    match config.to_resolved(resolver) {
                        Ok(config) => Some(config),
                        Err(err) => {
                            errs.push(err);
                            None
                        }
                    }
                }
            };

            if errs.is_empty() {
                Ok(CallWithConfigCtx {
                    call: call.expect("call"),
                    config
                })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }
    impl ToResolved<CallWithConfig> for CallWithConfigCtx {
        fn to_resolved(self, resolver: &Env) -> Result<CallWithConfig, MsgErr> {
            let mut errs = vec![];
            let call = match self.call.to_resolved(resolver) {
                Ok(call) => Some(call),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };
            let config = match self.config {
                None => None,
                Some(config) => {
                    match config.to_resolved(resolver) {
                        Ok(config) => Some(config),
                        Err(err) => {
                            errs.push(err);
                            None
                        }
                    }
                }
            };

            if errs.is_empty() {
                Ok(CallWithConfig {
                    call: call.expect("call"),
                    config
                })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }



    pub type Call = CallDef<Point>;
    pub type CallCtx = CallDef<PointCtx>;
    pub type CallVar = CallDef<PointVar>;

    impl ToResolved<Call> for CallCtx{
        fn to_resolved(self, resolver: &Env ) -> Result<Call, MsgErr> {
            Ok(Call {
                point: self.point.to_resolved(resolver)?,
                kind: self.kind
            })
        }
    }

    impl ToResolved<CallCtx> for CallVar{
        fn to_resolved(self, resolver: &Env ) -> Result<CallCtx, MsgErr> {
            Ok(CallCtx {
                point: self.point.to_resolved(resolver)?,
                kind: self.kind
            })
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CallDef<Pnt> {
        pub point: Pnt,
        pub kind: CallKind,
    }



    #[derive(Debug, Clone, Serialize, Deserialize )]
    pub enum CallKind {
        Msg(MsgCall),
        Http(HttpCall),
    }

    impl CallKind {
        pub fn core_with_body(self, body: Payload) -> Result<RequestCore, MsgErr> {
            Ok(match self {
                CallKind::Msg(msg) => RequestCore {
                    headers: Default::default(),
                    method: Method::Msg(MsgMethod::new(msg.method)?),
                    uri: Uri::from_str(msg.path.as_str())?,
                    body,
                },
                CallKind::Http(http) => RequestCore {
                    headers: Default::default(),
                    method: Method::Http(http.method),
                    uri: Uri::from_str(http.path.as_str())?,
                    body,
                },
            })
        }
    }

    impl ToString for Call {
        fn to_string(&self) -> String {
            format!("{}^{}", self.point.to_string(), self.kind.to_string())
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MsgCall {
        pub path: String,
        pub method: String,
    }

    impl MsgCall {
        pub fn new(action: String, path: String) -> Self {
            Self { method: action, path }
        }
    }

    impl ToString for MsgCall {
        fn to_string(&self) -> String {
            format!("Msg<{}>{}", self.method, self.path)
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
    pub struct HttpCall {
        pub path: String,

        #[serde(with = "http_serde::method")]
        pub method: HttpMethod,
    }

    impl HttpCall {
        pub fn new(method: HttpMethod, path: String) -> Self {
            Self { method, path }
        }
    }

    impl ToString for HttpCall {
        fn to_string(&self) -> String {
            format!("Http<{}>{}", self.method.to_string(), self.path)
        }
    }

    #[derive(
        Debug,
        Clone,
        Eq,
        PartialEq,
        Serialize,
        Deserialize,
        Hash,
        strum_macros::Display,
        strum_macros::EnumString,
    )]
    pub enum HttpMethodType {
        Get,
        Post,
        Put,
        Delete,
        Patch,
        Head,
        Connect,
        Options,
        Trace,
    }

    impl HttpMethodType {
        pub fn to_method(self) -> HttpMethod {
            match self {
                HttpMethodType::Get => HttpMethod::GET,
                HttpMethodType::Post => HttpMethod::POST,
                HttpMethodType::Put => HttpMethod::PUT,
                HttpMethodType::Delete => HttpMethod::DELETE,
                HttpMethodType::Patch => HttpMethod::PATCH,
                HttpMethodType::Head => HttpMethod::HEAD,
                HttpMethodType::Connect => HttpMethod::CONNECT,
                HttpMethodType::Options => HttpMethod::OPTIONS,
                HttpMethodType::Trace => HttpMethod::TRACE,
            }
        }
    }


    /*
    impl FromStr for HttpMethod {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let input = s.to_uppercase();
            match input.as_str() {
                "GET" => Ok(HttpMethod::GET),
                "POST" => Ok(HttpMethod::POST),
                "PUT" => Ok(HttpMethod::Put),
                "DELETE" => Ok(HttpMethod::DELETE),
                "PATCH" => Ok(HttpMethod::Patch),
                "HEAD" => Ok(HttpMethod::Head),
                "CONNECT" => Ok(HttpMethod::Connect),
                "OPTIONS" => Ok(HttpMethod::Options),
                "TRACE"=>  Ok(HttpMethod::Trace),
                what => Err(format!("unrecognized http method.  found: {}", what ).into())
            }
        }
    }

     */

    impl ValueMatcher<HttpMethod> for HttpMethod {
        fn is_match(&self, found: &HttpMethod) -> Result<(), ()> {
            if *self == *found {
                Ok(())
            } else {
                Err(())
            }
        }
    }

    impl ToString for CallKind {
        fn to_string(&self) -> String {
            match self {
                CallKind::Msg(msg) => msg.to_string(),
                CallKind::Http(http) => http.to_string(),
            }
        }
    }

    #[derive(
        Debug,
        Clone,
        Eq,
        PartialEq,
        strum_macros::Display,
        strum_macros::EnumString,
        Serialize,
        Deserialize,
    )]
    pub enum PayloadFormat {
        #[strum(serialize = "json")]
        Json,
        #[strum(serialize = "image")]
        Image,
    }

    impl PrimitiveType {
        pub fn is_match(&self, primitive: &Primitive) -> Result<(), MsgErr> {
            match primitive {
                Primitive::Text(_) => {
                    if *self == Self::Text {
                        Ok(())
                    } else {
                        Err("expected Text primitive".into())
                    }
                }
                Primitive::Point(_) => {
                    if *self == Self::Address {
                        Ok(())
                    } else {
                        Err("expected Address primitive".into())
                    }
                }
                Primitive::Stub(_) => {
                    if *self == Self::Stub {
                        Ok(())
                    } else {
                        Err("expected Stub primitive".into())
                    }
                }
                Primitive::Meta(_) => {
                    if *self == Self::Meta {
                        Ok(())
                    } else {
                        Err("expected Meta primitive".into())
                    }
                }
                Primitive::Bin(_) => {
                    if *self == Self::Bin {
                        Ok(())
                    } else {
                        Err("expected Bin primitive".into())
                    }
                }
                Primitive::Boolean(_) => {
                    if *self == Self::Boolean {
                        Ok(())
                    } else {
                        Err("expected Boolean primitive".into())
                    }
                }
                Primitive::Int(_) => {
                    if *self == Self::Int {
                        Ok(())
                    } else {
                        Err("expected Int primitive".into())
                    }
                }
                Primitive::Status(_) => {
                    if *self == Self::Status {
                        Ok(())
                    } else {
                        Err("expected Status primitive".into())
                    }
                }
                Primitive::Particle(_) => {
                    if *self == Self::Resource {
                        Ok(())
                    } else {
                        Err("expected Resource primitive".into())
                    }
                }
                Primitive::Errors(errors) => {
                    if *self == Self::Errors {
                        Ok(())
                    } else {
                        Err("expected Errors primitive".into())
                    }
                }
            }
        }
    }

    pub type MapPattern = MapPatternDef<Point>;
    pub type MapPatternCtx = MapPatternDef<PointCtx>;
    pub type MapPatternVar = MapPatternDef<PointVar>;


    #[derive(Debug, Clone, Serialize, Deserialize )]
    pub struct MapPatternDef<Pnt> {
        pub required: HashMap<String, ValuePattern<PayloadPatternDef<Pnt>>>,
        pub allowed: ValuePattern<PayloadPatternDef<Pnt>>,
    }

    impl <Pnt> Default for MapPatternDef<Pnt> {
        fn default() -> Self {
            MapPatternDef {
                required: Default::default(),
                allowed: ValuePattern::Any,
            }
        }
    }

    impl <Pnt> ToString for MapPatternDef<Pnt> {
        fn to_string(&self) -> String {
            "Map?".to_string()
        }
    }

    impl <Pnt>MapPatternDef<Pnt> {
        pub fn new(
            required: HashMap<String, ValuePattern<PayloadPatternDef<Pnt>>>,
            allowed: ValuePattern<PayloadPatternDef<Pnt>>,
        ) -> Self {
            MapPatternDef { required, allowed }
        }

        pub fn empty() -> Self {
            Self {
                required: HashMap::new(),
                allowed: ValuePattern::None,
            }
        }

        pub fn any() -> Self {
            Self {
                required: HashMap::new(),
                allowed: ValuePattern::Any,
            }
        }

        pub fn is_match(&self, map: &PayloadMap) -> Result<(), ()> {
            // if Any keys are allowed then skip
            for (key, payload) in &map.map {
                if !self.required.contains_key(key) {
                    match &self.allowed {
                        ValuePattern::Any => {}
                        ValuePattern::None => {
                            return Err(());
                        }
                        ValuePattern::Pattern(pattern) => {
                            pattern.is_match(payload)?;
                        }
                    }
                }
            }

            // now make sure all required are present and meet constraints
            for (key, constraint) in &self.required {
                if !map.contains_key(key) {
                    return Err(());
                }
                constraint.is_match(
                    &map.get(key)
                        .expect("expected map element after testing for it"),
                )?;
            }

            Ok(())
        }
    }

    /*
    impl<FromResourceType,FromKind,FromPayload,FromTksPattern, ToResourceType,ToKind,ToPayload,ToTksPattern> ConvertFrom<Valuepattern<PayloadPattern<FromResourceType,FromKind,FromPayload,FromTksPattern>>>
    for ValuePattern<PayloadPattern<FromResourceType,FromKind,FromPayload,FromTksPattern>>
        where
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            FromTksPattern: TryInto<ToTksPattern, Error = Error> + Clone,
            FromPayload: TryInto<ToPayload, Error = Error> + Clone,
            FromResourceType: TryInto<ToResourceType, Error = Error> + Clone,
            ToKind: Clone,

    {

        fn convert_from(
            a: HashMap<String, ValuePattern<PayloadPattern<FromKind>>>
        ) -> Result<Self, Error>
            where
                Self: Sized,
        {
            let mut rtn = HashMap::new();
            for (k,v) in a {
                rtn.insert( k, ConvertFrom::convert_from(v)?);
            }
            Ok(rtn)
        }
    }

     */

    /*
    impl <KEY,ADDRESS,IDENTIFIER,KIND> ValuePattern<Payload<KEY,ADDRESS,IDENTIFIER,KIND>> for PayloadType<KEY,ADDRESS,IDENTIFIER,KIND> {
        fn is_match(&self, payload: &Payload<KEY,ADDRESS,IDENTIFIER,KIND>) -> Result<(), Error> {
            match **self {
                PayloadType::Empty => {
                    if let Payload::Empty = payload {
                        Ok(())
                    } else {
                        Err(format!("Payload expected: '{}' found: Empty",self.to_string()).into())
                    }
                }
                PayloadType::Primitive(expected) => {
                    if let Payload::Primitive(found)= payload {
                        expected.is_match(found)
                    } else {
                        Err(format!("Payload expected: '{}' found: '{}'",self.to_string(), payload.to_string()).into())
                    }
                }
                PayloadType::List(expected) => {
                    if let Payload::List(found)= payload {
                        expected.is_match(&found.primitive_type )
                    } else {
                        Err(format!("Payload expected: '{}' found: '{}'",self.to_string(), payload.to_string()).into())
                    }
                }
                PayloadType::Map(expected) => {
                    if let Payload::Map(found)= payload {
                        expected.is_match(&found.primitive_type )
                    } else {
                        Err(format!("Payload expected: '{}' found: '{}'",self.to_string(), payload.to_string()).into())
                    }
                }
            }
        }
    }

     */

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PayloadRef<PAYLOAD_CLAIM, PAYLOAD_PATTERN> {
        pub claim: PAYLOAD_CLAIM,
        pub pattern: PAYLOAD_PATTERN,
    }

    /*
    impl<FromPayloadClaim, FromPayloadPattern> PayloadRef<FromPayloadClaim, FromPayloadPattern> {
        pub fn convert<ToPayloadClaim, ToPayloadPattern>(
            self,
        ) -> Result<PayloadRef<ToPayloadClaim, ToPayloadPattern>, Error>
        where
            ToPayloadClaim: TryFrom<FromPayloadClaim, Error = Error>,
            ToPayloadPattern: TryFrom<FromPayloadPattern, Error = Error>,
        {
            Ok(Self {
                claim: self.claim.try_into()?,
                pattern: self.pattern.try_into()?,
            })
        }
    }

     */

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum PayloadDelivery<PAYLOAD, PAYLOAD_REF> {
        Payload(PAYLOAD),
        Ref(PAYLOAD_REF),
    }

    /*
    impl<FromPayload, FromPayloadRef> PayloadDelivery<FromPayload, FromPayloadRef> {
        pub fn convert<ToPayload, ToPayloadRef>(
            self,
        ) -> Result<PayloadDelivery<ToPayload, ToPayloadRef>, Error>
        where
            ToPayload: TryFrom<FromPayload,Error=Error>,
            ToPayloadRef: TryFrom<FromPayloadRef,Error=Error>,
        {
            match self {
                PayloadDelivery::Payload(payload) => Ok(payload.try_into()?),
                PayloadDelivery::Ref(payload_ref) => {
                    Ok(payload_ref.try_into()?)
                }
            }
        }
    }
     */
}
