use std::ops::Deref;
use serde_json::Value;
use crate::error::MsgErr;
use crate::version::v0_0_1::id::id::Point;
use crate::version::v0_0_1::selector::selector::parse::delim_kind;

#[derive(Debug, Clone, Serialize, Deserialize,strum_macros::Display)]
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
     pub payload: LogPayload,
     pub level: Level,
}

impl Log {
    pub fn timestamp<T>(self, timestamp: T) -> TimestampedLog<T> where T: ToString {
        TimestampedLog {
            log: self,
            timestamp
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimestampedLog<T> where T: ToString{
    pub log: Log,
    pub timestamp: T
}

impl Deref for ShellLog {
    type Target = Log;

    fn deref(&self) -> &Self::Target {
        &self.log
    }
}

impl ToString for Log {
    fn to_string(&self) -> String {
        format!("{} {} {}", self.point.to_string(), self.level.to_string(), self.payload.to_string())
    }
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PointlessLog {
    message: String,
    level: Level,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LogPayload {
    Message(String),
    Json(Value),
    Both{message:String,json:Value}
}

impl ToString for LogPayload {
    fn to_string(&self) -> String {
        match self {
            LogPayload::Message(message) => message.clone(),
            LogPayload::Json(json) => json.to_string(),
            LogPayload::Both { json, message } => { format!("{} {}",json.to_string(), message.clone() )}
        }
    }
}

impl ToString for Log {
    fn to_string(&self) -> String {
        format!("{} {}", self.point.to_string(), self.message)
    }
}

impl Log {
    fn trace(point: Point, message: &str) -> Self {
        Self {
            level: Level::Trace,
            point,
            payload: LogPayload::Message(message.to_string()),
        }
    }
    fn debug(point: Point, message: &str) -> Self {
        Self {
            level: Level::Debug,
            point,
            payload: LogPayload::Message(message.to_string()),
        }
    }
    fn info(point: Point, message: &str) -> Self {
        Self {
            level: Level::Info,
            point,
            payload: LogPayload::Message(message.to_string()),
        }
    }
     fn warn(point: Point, message: &str) -> Self {
        Self {
            level: Level::Warn,
            point,
            payload: LogPayload::Message(message.to_string()),
        }
    }
     fn error(point: Point, message: &str) -> Self {
        Self {
            level: Level::Error,
            point,
            payload: LogPayload::Message(message.to_string()),
        }
    }
}

pub struct LogBuilder<P,L> where P: TryInto<Point,Error=MsgErr>, L: Logger{
    pub point: Option<P>,
    pub logger: L,
    pub level: Level,
    pub message: Option<String>,
    pub json: Option<Value>,
    msg_overrides: Vec<String>
}

impl <P,L> LogBuilder<P,L> where P: TryInto<Point,Error=MsgErr>, L: Logger {
    pub fn new(logger: L) -> Self {
        LogBuilder {
            logger,
            point: None,
            level: Default::default(),
            message: None,
            json: None,
            msg_overrides: vec![]
        }
    }


    pub fn level( mut self, level: Level ) -> Self {
        self.level = level;
        self
    }

    pub fn trace(mut self)->Self { self.level = Level::Trace; self }
    pub fn debug(mut self)->Self { self.level = Level::Debug; self }
    pub fn info(mut self)->Self { self.level = Level::Info; self }
    pub fn warn(mut self)->Self { self.level = Level::Warn; self }
    pub fn error(mut self)->Self { self.level = Level::Error; self }

    pub fn point(mut self, p: P) -> Self {
        self.point = Some(p);
        self
    }

    pub fn msg<M>( mut self, m: M ) -> Self where M: ToString{
        self.message = Some(m.to_string());
        self
    }

    pub fn json<J>( mut self, json: J) -> Self where J:Into<&str> {
        match serde_json::from_str(json.into() ) {
            Ok(json) => {
                self.json = Some(json);
            }
            Err(err) => {
                self.msg_overrides.push(format!("error parsing log json: {}", err.to_string()));
            }
        }
        self
    }

    pub fn json_value( mut self, json: Value) -> Self {
        self.json = Some(json);
        self
    }


    pub fn validate(&self) -> Result<(),MsgErr> {
        if self.point.is_none() {
            Err(MsgErr::from("Log must reference a valid point"))
        }
        if self.message.is_none() && self.json.is_none() {
            Err(MsgErr::from("Log must have either a message or json or both"))
        } else {
            Ok(())
        }
    }


    pub fn commit(mut self) {
        if self.logger.is_none() {
            Err(MsgErr::from("LogBuilder: must set Logger before LogBuilder.send() can be called"))
        }
        self.validate()?;

            if self.message.is_none() && self.json.is_none() {
                self.msg_overrides.push("Log must have either a message or json or both".to_string())
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
                            rtn.push_str(format!("original message: {}",message).as_str() );
                        }
                    }
                Some(rtn)
            };

        let content = if message.is_some() && self.json.is_none() {
            LogPayload::Message(self.message.expect("message"))
        } else if message.is_none() && self.json.is_some() {
            LogPayload::Json(self.json.expect("message"))
        } else if message.is_some() && self.json.is_some() {
            LogPayload::Both{message: message.expect("message"), json: self.json.expect("json")}
        } else {
            panic!("LogBuilder: must set Logger before LogBuilder.send() can be called")
        };

        if self.point.is_none() {
            self.msg_overrides.push("Particle Point must be set for a Log".to_string())
        } else {
            let point = self.point.expect("point");
            match point.try_into() {
                Ok(point) => {
                    let log  = Log {
                        point,
                        level: self.level.expect("level"),
                        payload: content
                    };
                    self.logger.log(log);
                }
                Err( err ) => {
                    self.msg_overrides.push(err.to_string() );
                    let log = PointlessLog {
                        message: format!("Bad logging point: {}", err.to_string() ),
                        level: Level::Error
                    };
                    self.logger.pointless(log);
                }
            }
        }



    }
}


 trait Logger: Clone+Sized {
     fn log(&self, log: Log);

     /// PointlessLog is used for error diagnosis of the logging system itself, particularly
     /// where there is parsing error due to a bad point
     fn pointless(&self, log: PointlessLog);

     /// it seems like you would want ALL logs to be timestamped, but in some cases, like in
     /// Wasm system time is not available, so it is the Wasm Host that upgrades to a TimestampedLog
     /// but if you are worried about the small delay of the log traveling though the portal and
     /// your implementation has access to system time then timestamped is what you want.
     fn timestamped(&self, log: Log);

     // optionally return the point being logged
     fn get_logging_point(&self) -> Option<Point>;

     fn point<P,L>(&self, point:P ) ->  LogBuilder<P,L> where P: TryInto<Point>, L:Logger{
         let mut builder = LogBuilder::new(self.clone());
         builder.point(point)
     }

     fn builder<P,L>(&self) -> LogBuilder<P,L> where P: TryInto<Point,Error=ErrMsg>,L:Logger{
         match &self.get_logging_point() {
             None => {
                 LogBuilder::new(self.clone())
             }
             Some(point) => {
                 LogBuilder::new(self.clone()).point(point.clone() )
             }
         }
     }

     fn audit( &self, point: Point, metric: String, value: String );

     fn point_logger( &self, point: Point ) -> PointLogger {
         PointLogger {
             logger: Box::new(self.clone()),
             point
         }
     }
}

#[derive(Clone)]
pub struct PointLogger {
    pub logger: Box<dyn Logger>,
    pub point: Point
}

impl PointLogger {
    fn trace<M>(&self, message: M) where M: ToString{
        self.logger.log( Log {
            point: self.point.clone(),
            level: Level::Trace,
            payload: LogPayload::Message(message.to_string())
        })
    }

    fn debug(&self, message: &str) {
        self.logger.log( Log {
            point: self.point.clone(),
            level: Level::Debug,
            payload: LogPayload::Message(message.to_string())
        })
    }

    fn info(&self, message: &str) {
        self.logger.log( Log {
            point: self.point.clone(),
            level: Level::Info,
            payload: LogPayload::Message(message.to_string())
        })
    }

    fn warn(&self, message: &str) {
        self.logger.log( Log {
            point: self.point.clone(),
            level: Level::Warn,
            payload: LogPayload::Message(message.to_string())
        })
    }

    fn error(&self, message: &str) {
        self.logger.log( Log {
            point: self.point.clone(),
            level: Level::Error,
            payload: LogPayload::Message(message.to_string())
        })
    }

    fn timestamped( &self, log: Log ) where T:ToString {
        self.logger.timestamped(log);
    }

    fn audit( &self,  metric: String, value: String ) {
        self.logger.audit(self.point.clone(), metric, value );
    }

    fn builder<L>(&self) -> PointLogBuilder<L> {
        let builder = self.logger.builder().point(self.point.clone());
        let builder = PointLogBuilder::new( self.clone(), builder );
        builder
    }

}
pub struct PointLogBuilder<L> where L: Logger {
    logger: PointLogger,
    builder: LogBuilder<Point,L>,
}

impl <L> PointLogBuilder<L> where  L: Logger {
    pub fn new(logger: PointLogger, builder: LogBuilder<Point,L>) -> Self {
        PointLogBuilder {
            logger,
            builder
        }
    }

    pub fn trace(mut self)->Self { self.builder = self.builder.trace(); self }
    pub fn debug(mut self)->Self { self.builder = self.builder.debug(); self }
    pub fn info(mut self)->Self { self.builder = self.builder.info(); self }
    pub fn warn(mut self)->Self { self.builder = self.builder.warn(); self }
    pub fn error(mut self)->Self { self.builder = self.builder.error(); self }


    pub fn msg<M>( mut self, m: M ) -> Self where M: ToString{
        self.builder = self.builder.msg(m);
        self
    }

    pub fn json<J>( mut self, json: J) -> Self where J:Into<&str> {
        self.builder = self.builder.json(json);
        self
    }

    pub fn json_value( mut self, json: Value) -> Self {
        self.builder = self.builder.json_value(json);
        self
    }

    pub fn commit(mut self) {
        self.builder.commit();
    }
}

pub mod audit {
    use crate::version::v0_0_1::id::id::Point;
    use crate::version::v0_0_1::log::PointLogger;

    pub struct AuditLog {
        pub point: Point,
        pub timestamp: Timestamp,
        pub metric: String,
        pub value: String
    }
}