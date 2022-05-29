use crate::error::MsgErr;
use crate::version::v0_0_1::id::id::Point;
use crate::version::v0_0_1::util::{timestamp, uuid};
use crate::version::v0_0_1::{mesh_portal_timestamp};
use serde_json::Value;
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;
use chrono::{DateTime, Utc};
use serde::{Serialize,Deserialize};
use serde;
use crate::version::v0_0_1::cmd::command::common::StateSrc::StatefulDirect;
use chrono::serde::ts_milliseconds;

#[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
pub enum Level {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl Default for Level {
    fn default() -> Self {
        Level::Info
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Log {
    pub point: Point,
    pub source: LogSource,
    pub span: Option<String>,
    #[serde(with= "ts_milliseconds")]
    pub timestamp: DateTime<Utc>,
    pub payload: LogPayload,
    pub level: Level,
}

impl ToString for Log {
    fn to_string(&self) -> String {
        format!(
            "{} {} {}",
            self.point.to_string(),
            self.level.to_string(),
            self.payload.to_string()
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LogSource {
   Shell,
   Core
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LogSpanEventKind {
    Entry,
    Exit
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogSpanEvent {
    pub point: Point,
    pub kind: LogSpanEventKind,
    pub id: String,
    pub parent: Option<String>,
    pub attributes: HashMap<String,String>,

    #[serde(with= "ts_milliseconds")]
    pub timestamp: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PointlessLog {
    #[serde(with= "ts_milliseconds")]
    timestamp: DateTime<Utc>,
    message: String,
    level: Level,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LogPayload {
    Message(String),
    Json(Value),
    Both { message: String, json: Value },
}

impl ToString for LogPayload {
    fn to_string(&self) -> String {
        match self {
            LogPayload::Message(message) => message.clone(),
            LogPayload::Json(json) => json.to_string(),
            LogPayload::Both { json, message } => {
                format!("{} {}", json.to_string(), message.clone())
            }
        }
    }
}


pub struct RootLogBuilder
{
    pub point: Option<Point>,
    pub span: Option<String>,
    pub logger: RootLogger,
    pub level: Level,
    pub message: Option<String>,
    pub json: Option<Value>,
    msg_overrides: Vec<String>,
}

impl RootLogBuilder
{
    pub fn new(logger: RootLogger, span: Option<String>) -> Self {
        RootLogBuilder {
            logger,
            span,
            point: None,
            level: Level::default(),
            message: None,
            json: None,
            msg_overrides: vec![],
        }
    }

    pub fn level(mut self, level: Level) -> Self {
        self.level = level;
        self
    }

    pub fn trace(mut self) -> Self {
        self.level = Level::Trace;
        self
    }
    pub fn debug(mut self) -> Self {
        self.level = Level::Debug;
        self
    }
    pub fn info(mut self) -> Self {
        self.level = Level::Info;
        self
    }
    pub fn warn(mut self) -> Self {
        self.level = Level::Warn;
        self
    }
    pub fn error(mut self) -> Self {
        self.level = Level::Error;
        self
    }

    pub fn point(mut self, p: Point) -> Self {
        self.point = Some(p);
        self
    }

    pub fn msg<M>(mut self, m: M) -> Self
    where
        M: ToString,
    {
        self.message = Some(m.to_string());
        self
    }

    pub fn json<'a,J>(mut self, json: J) -> Self
    where
        J: Into<&'a str>,
    {
        match serde_json::from_str(json.into()) {
            Ok(json) => {
                self.json = Some(json);
            }
            Err(err) => {
                self.msg_overrides
                    .push(format!("error parsing log json: {}", err.to_string()));
            }
        }
        self
    }

    pub fn json_value(mut self, json: Value) -> Self {
        self.json = Some(json);
        self
    }

    pub fn commit(mut self) {

        if self.message.is_none() && self.json.is_none() {
            self.msg_overrides
                .push("Log must have either a message or json or both".to_string())
        }

        if self.point.is_none() {
            self.msg_overrides
                .push("Particle Point must be set for a Log".to_string())
        }

        let message = if self.msg_overrides.is_empty() {
            self.message
        } else {
            let mut rtn = String::new();
            rtn.push_str("LOG ERROR OVERRIDES: this means there was an error int he logging process itself.\n");
            for over in self.msg_overrides {
                rtn.push_str(over.as_str());
            }
            match self.message {
                None => {}
                Some(message) => {
                    rtn.push_str(format!("original message: {}", message).as_str());
                }
            }
            Some(rtn)
        };

        if self.point.is_none() {
            let log = PointlessLog {
                timestamp: timestamp(),
                message: message.expect("message"),
                level: Level::Error
            };
            self.logger.pointless(log);
            return;
        }

        let content = if message.is_some() && self.json.is_none() {
            LogPayload::Message(message.expect("message"))
        } else if message.is_none() && self.json.is_some() {
            LogPayload::Json(self.json.expect("message"))
        } else if message.is_some() && self.json.is_some() {
            LogPayload::Both {
                message: message.expect("message"),
                json: self.json.expect("json"),
            }
        } else {
            panic!("LogBuilder: must set Logger before LogBuilder.send() can be called")
        };


            let point = self.point.expect("point");
            let log = Log {
                point,
                level: self.level,
                timestamp: timestamp(),
                payload: content,
                source: self.logger.source(),
                span: self.span
            };
        self.logger.log(log);
    }
}

pub trait LogAppender: Send+Sync{
    fn log(&self, log: Log);

    fn audit(&self, log: AuditLog);

    fn span(&self, log: LogSpanEvent);

    /// PointlessLog is used for error diagnosis of the logging system itself, particularly
    /// where there is parsing error due to a bad point
    fn pointless(&self, log: PointlessLog);
}

#[derive(Clone)]
pub struct RootLogger {
   source: LogSource,
   appender: Arc<dyn LogAppender>
}

impl RootLogger {

    pub fn new( source: LogSource, appender: Arc<dyn LogAppender>) -> Self {
        Self {
            source,
            appender
        }
    }

    pub fn stdout(source: LogSource) -> Self {
        Self{
            source,
            ..RootLogger::default()
        }
    }


    fn source(&self) -> LogSource {
        self.source.clone()
    }

    fn log(&self, log: Log) {
        self.appender.log(log);
    }

    fn audit(&self, log: AuditLog) {
        self.appender.audit(log);
    }

    fn span(&self, log: LogSpanEvent) {
        self.appender.span(log);
    }

    /// PointlessLog is used for error diagnosis of the logging system itself, particularly
    /// where there is parsing error due to a bad point
    fn pointless(&self, log: PointlessLog) {
        self.appender.pointless(log);
    }

    pub fn point(&self, point: Point) -> PointLogger {
        PointLogger {
            logger: self.clone(),
            point
        }
    }
}

pub struct StdOutAppender();

impl LogAppender for StdOutAppender {
    fn log(&self, log: Log) {
        println!("{}",log.payload.to_string() )
    }

    fn audit(&self, log: AuditLog) {
        println!("audit log..." )
    }

    fn span(&self, log: LogSpanEvent) {
        println!("span..." )
    }

    fn pointless(&self, log: PointlessLog) {
        println!("{}",log.message  );
    }
}

impl Default for RootLogger {
    fn default() -> Self {
        Self {
            appender: Arc::new(StdOutAppender()),
            source: LogSource::Core
        }
    }
}

#[derive(Clone)]
pub struct PointLogger {
    pub logger: RootLogger,
    pub point: Point,
}

impl PointLogger {

    pub fn source(&self) -> LogSource {
        self.logger.source()
    }

    pub fn span(&self) -> SpanLogger {
        SpanLogger {
            root_logger: self.logger.clone(),
            point: self.point.clone(),
            span: uuid(),
            entry_timestamp: timestamp(),
            attributes: Default::default(),
            parent: None
        }
    }

    fn point(&self, point: Point) -> PointLogger {
        PointLogger {
            logger: self.logger.clone(),
            point
        }
    }
}



pub struct SpanLogBuilder {

    pub entry_timestamp: DateTime<Utc>,
    pub attributes: HashMap<String,String>,
}

impl SpanLogBuilder {
    pub fn new() -> Self {
        Self {
            entry_timestamp: timestamp(),
            attributes: HashMap::new()
        }
    }
}

#[derive(Clone)]
pub struct SpanLogger {
    root_logger: RootLogger,
    point: Point,
    span: String,
    entry_timestamp: DateTime<Utc>,
    attributes: HashMap<String,String>,
    parent: Option<String>
}

impl SpanLogger {
    pub fn span_id(&self) -> String {
        self.span.clone()
    }

    pub fn span(&self) -> SpanLogger {
        SpanLogger {
            root_logger: self.root_logger.clone(),
            point: self.point.clone(),
            span: uuid(),
            entry_timestamp: timestamp(),
            attributes: Default::default(),
            parent: Some(self.span.clone())
        }
    }

    pub fn current_span(&self) -> LogSpanEvent {
        LogSpanEvent {
            kind: LogSpanEventKind::Entry,
            point: self.point.clone(),
            id: self.span.clone(),
            parent: self.parent.clone(),
            attributes: self.attributes.clone(),
            timestamp: self.entry_timestamp.clone()
        }
    }

    pub fn entry_timestamp(&self) -> DateTime<Utc>{
        self.entry_timestamp.clone()
    }

    pub fn set_span_attr<K,V>( &mut self, key: K, value: V) where K: ToString, V: ToString {
        self.attributes.insert( key.to_string(), value.to_string() );
    }

    pub fn get_span_attr<K>( &self, key: K) -> Option<String> where K: ToString {
        self.attributes.get( &key.to_string() ).cloned()
    }

    pub fn msg<M>(&self, level: Level, message :M ) where M: ToString {
        self.root_logger.log(Log {
            point: self.point.clone(),
            level,
            timestamp: timestamp(),
            payload: LogPayload::Message(message.to_string()),
            span: None,
            source: self.root_logger.source()
        })
    }

    pub fn trace<M>(&self, message: M)
        where
            M: ToString,
    {
        self.msg(Level::Trace,message);
    }

    pub fn debug<M>(&self, message: M) where M:ToString {
        self.msg(Level::Trace,message);
    }

    pub fn info<M>(&self, message: M) where M:ToString {
        self.msg(Level::Trace,message);
    }

    pub fn warn<M>(&self, message: M) where M:ToString {
        self.msg(Level::Warn, message );
    }

    pub fn error<M>(&self, message: M) where M:ToString {
        self.msg(Level::Error, message );
    }

    pub fn audit(&self) -> AuditLogBuilder {
        AuditLogBuilder {
            logger: self.root_logger.clone(),
            point: self.point.clone(),
            span: self.span.clone(),
            attributes: HashMap::new(),
        }
    }

    pub fn builder(&self) -> LogBuilder {
        let builder = RootLogBuilder::new( self.root_logger.clone(), None);
        let builder = LogBuilder::new(self.root_logger.clone(), builder);
        builder
    }

    pub fn log_audit(&self, log: AuditLog) {
        self.root_logger.audit(log);
    }
}

impl Drop for SpanLogger {
    fn drop(&mut self) {
        let log = LogSpanEvent {
            kind: LogSpanEventKind::Exit,
            point: self.point.clone(),
            id: self.span.clone(),
            parent: self.parent.clone(),
            attributes: self.attributes.clone(),
            timestamp: timestamp()
        };
        self.root_logger.span(log)
    }
}



pub struct LogBuilder
{
    logger: RootLogger,
    builder: RootLogBuilder,
}

impl LogBuilder
{
    pub fn new(logger: RootLogger, builder: RootLogBuilder) -> Self {
        LogBuilder { logger, builder }
    }

    pub fn trace(mut self) -> Self {
        self.builder = self.builder.trace();
        self
    }
    pub fn debug(mut self) -> Self {
        self.builder = self.builder.debug();
        self
    }
    pub fn info(mut self) -> Self {
        self.builder = self.builder.info();
        self
    }
    pub fn warn(mut self) -> Self {
        self.builder = self.builder.warn();
        self
    }
    pub fn error(mut self) -> Self {
        self.builder = self.builder.error();
        self
    }

    pub fn msg<M>(mut self, m: M) -> Self
    where
        M: ToString,
    {
        self.builder = self.builder.msg(m);
        self
    }

    pub fn json<'a,J>(mut self, json: J) -> Self
    where
        J: Into<&'a str>,
    {
        self.builder = self.builder.json(json);
        self
    }

    pub fn json_value(mut self, json: Value) -> Self {
        self.builder = self.builder.json_value(json);
        self
    }

    pub fn commit(mut self) {
        self.builder.commit();
    }
}

pub struct AuditLogBuilder {
    logger: RootLogger,
    point: Point,
    span: String,
    attributes: HashMap<String, String>,
}

impl AuditLogBuilder {
    pub fn new(logger: RootLogger, point: Point, span: String) -> Self {
        AuditLogBuilder {
            logger,
            point,
            attributes: HashMap::new(),
            span
        }
    }

    // make nice appended call:
    // logger.audit().append("hello","kitty").commit();
    pub fn append<K: ToString, V: ToString>(mut self, key: K, value: V) -> Self {
        self.attributes.insert(key.to_string(), value.to_string());
        self
    }

    pub fn add<K: ToString, V: ToString>(&mut self, key: K, value: V) {
        self.attributes.insert(key.to_string(), value.to_string());
    }

    pub fn kind<K>(mut self, kind: K) -> Self
    where
        K: ToString,
    {
        self.attributes.insert("kind".to_string(), kind.to_string());
        self
    }

    pub fn commit(mut self) {
        let log = AuditLog {
            point: self.point,
            timestamp: timestamp(),
            metrics: self.attributes,
        };
        self.logger.audit(log)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditLog {
    pub point: Point,
    #[serde(with= "ts_milliseconds")]
    pub timestamp: DateTime<Utc>,
    pub metrics: HashMap<String, String>,
}
