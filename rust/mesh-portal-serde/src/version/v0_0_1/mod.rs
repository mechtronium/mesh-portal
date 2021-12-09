use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::version::v0_0_1::bin::Bin;

pub type State = HashMap<String, Bin>;

pub type ArtifactRef = String;
pub type Artifact = Arc<Vec<u8>>;
pub type Port = String;

pub mod id {
    use std::collections::HashMap;
    use std::str::FromStr;

    use serde::{Deserialize, Serialize};

    use crate::error::Error;
    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::parse::{address, consume_address, Res};

    pub type Key = String;
    pub type ResourceType = String;
    pub type Kind = String;
    pub type Specific = String;
    pub type Version = String;
    pub type Identifier = generic::id::Identifier<Key, Address>;
    pub type Identifiers = generic::id::Identifiers<Key, Address>;
    pub type AddressAndKind = generic::id::AddressAndKind<Address, Kind>;
    pub type AddressAndType = generic::id::AddressAndType<Address, ResourceType>;
    pub type Meta = HashMap<String, String>;
    pub type PayloadClaim = String;


    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
    pub struct Address {
        pub segments: Vec<String>
    }

    impl FromStr for Address {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(consume_address(s)?.1)
        }
    }

    impl Into<String> for Address {
        fn into(self) -> String {
            self.to_string()
        }
    }

    impl ToString for Address {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            for (i, segment) in self.segments.iter().enumerate() {
                rtn.push_str( segment.as_str() );
                if i != self.segments.len()-1 {
                    rtn.push_str(":");
                }
            }
            rtn.to_string()
        }
    }
    impl Address {
        pub fn parent(&self) -> Option<Address> {
            if self.segments.is_empty() {
                return Option::None;
            }
            let mut segments = self.segments.clone();
            segments.remove( segments.len() );
            Option::Some( Self {
                segments
            })
        }

        pub fn parse( input: &str ) -> Res<&str,Self> {
            address(input)
        }
    }
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum IdentifierKind {
        Key,
        Address,
    }
}

pub mod messaging {
    use serde::{Deserialize, Serialize};
    use std::convert::TryInto;
    use crate::error::Error;

    pub type ExchangeId = String;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ExchangeType {
        Notification,
        RequestResponse,
    }


    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Exchange {
        Notification,
        RequestResponse(ExchangeId),
    }

    impl Exchange {
        pub fn requires_response(&self) -> bool {
            match self {
                Exchange::Notification => false,
                Exchange::RequestResponse(_) => true,
            }
        }
    }

    impl TryInto<ExchangeId> for Exchange {
        type Error = Error;

        fn try_into(self) -> Result<ExchangeId, Self::Error> {
            match self {
                Exchange::Notification => {
                    Err("Exchange Notification cannot be converted into a RequestResponse Exchange".into())
                }
                Exchange::RequestResponse(id) => {
                    Ok(id)
                }
            }
        }
    }

    impl Into<ExchangeType> for Exchange {
        fn into(self) -> ExchangeType {
            match self {
                Exchange::Notification => {
                    ExchangeType::Notification
                }
                Exchange::RequestResponse(_) => {
                    ExchangeType::RequestResponse
                }
            }
        }
    }
}

pub mod log {
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Log {
        Warn(String),
        Info(String),
        Error(String),
        Fatal(String),
    }

    impl ToString for Log {
        fn to_string(&self) -> String {
            match self {
                Log::Warn(message) => {
                    format!("WARN: {}", message)
                }
                Log::Info(message) => {
                    format!("INFO: {}", message)
                }
                Log::Error(message) => {
                    format!("ERROR: {}", message)
                }
                Log::Fatal(message) => {
                    format!("FATAL: {}", message)
                }
            }
        }
    }
}

pub mod frame {
    use std::convert::TryInto;

    use serde::{Deserialize, Serialize};

    use crate::error::Error;

    pub struct PrimitiveFrame {
        pub data: Vec<u8>,
    }

    impl PrimitiveFrame {
        pub fn size(&self) -> u32 {
            self.data.len() as u32
        }
    }

    impl From<String> for PrimitiveFrame {
        fn from(value: String) -> Self {
            let bytes = value.as_bytes();
            Self {
                data: bytes.to_vec(),
            }
        }
    }

    impl TryInto<String> for PrimitiveFrame {
        type Error = Error;

        fn try_into(self) -> Result<String, Self::Error> {
            Ok(String::from_utf8(self.data)?)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
    pub enum CloseReason {
        Done,
        Error(String),
    }
}

pub mod bin {
    use std::collections::HashMap;
    use std::sync::Arc;

    use serde::{Deserialize, Serialize};

    pub type Bin = Arc<Vec<u8>>;
}

pub mod payload {
    use serde::{Deserialize, Serialize};

    use crate::error::Error;
    use crate::version::v0_0_1::bin::Bin;
    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{
        Address, Identifier, Key, Kind, PayloadClaim,
    };

    pub type Primitive = generic::payload::Primitive<Key, Address, Identifier, Kind>;
    pub type Payload = generic::payload::Payload<Key, Address, Identifier, Kind>;
    pub type PayloadType = generic::payload::PayloadType;
    pub type PayloadRef = generic::payload::PayloadRef<PayloadClaim, PayloadPattern>;
    pub type PayloadDelivery = generic::payload::PayloadDelivery<Payload, PayloadRef>;
    pub type Call = generic::payload::Call<Address>;
    pub type CallWithConfig = generic::payload::CallWithConfig<Address>;
    pub type MapPattern = generic::payload::MapPattern<Key,Address,Identifier,Kind>;
    pub type PayloadTypePattern = generic::payload::PayloadTypePattern<Key,Address,Identifier,Kind>;
    pub type PayloadPattern = generic::payload::PayloadPattern<Key,Address,Identifier,Kind>;
    pub type ListPattern = generic::payload::ListPattern;
    pub type PayloadMap = generic::payload::PayloadMap<Key,Address,Identifier,Kind>;
    pub type RcCommand = generic::payload::RcCommand;

    #[derive(
        Debug,
        Clone,
        strum_macros::Display,
        strum_macros::EnumString,
        Eq,
        PartialEq,
        Hash,
        Serialize,
        Deserialize,
    )]

    pub enum PrimitiveType {
        Key,
        Address,
        Identifier,
        Text,
        Boolean,
        Code,
        Int,
        Meta,
        Bin,
        Stub,
        Status,
        Resource,
    }


}

pub mod command {
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Command {
        pub cli: CliId,
        pub payload: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum CommandStatus {
        Running,
        Exit(i32),
    }

    pub type CliId = String;
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CommandEvent {
        pub cli: CliId,
        pub line: Option<String>,
        pub status: CommandStatus,
    }
}

pub mod http {
    use std::collections::HashMap;

    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::Bin;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HttpRequest {
        pub headers: HashMap<String, String>,
        pub path: String,
        pub body: Option<Bin>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HttpResponse {
        pub headers: HashMap<String, String>,
        pub code: usize,
        pub body: Option<Bin>,
    }

    impl HttpResponse {
        pub fn server_side_error() -> Self {
            Self {
                headers: Default::default(),
                code: 500,
                body: None,
            }
        }
    }
}

pub mod config {
    use std::collections::HashMap;

    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::ArtifactRef;
    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Address, Key, Kind};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum PortalKind {
        Mechtron,
        Portal,
    }

    impl ToString for PortalKind {
        fn to_string(&self) -> String {
            match self {
                PortalKind::Mechtron => "Mechtron".to_string(),
                PortalKind::Portal => "Portal".to_string(),
            }
        }
    }

    pub type Info = generic::config::Info<Key, Address, Kind>;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Config {
        pub max_payload_size: u32,
        pub init_timeout: u64,
        pub frame_timeout: u64,
        pub response_timeout: u64,
        pub bind: BindConfig,
    }

    impl Config {
        pub fn with_bind_config(bind: BindConfig) -> Self {
            Self {
                max_payload_size: 128 * 1024,
                init_timeout: 30,
                frame_timeout: 5,
                response_timeout: 15,
                bind,
            }
        }
    }

    impl Default for Config {
        fn default() -> Self {
            Self {
                max_payload_size: 128 * 1024,
                init_timeout: 30,
                frame_timeout: 5,
                response_timeout: 15,
                bind: Default::default(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SchemaRef {
        pub schema: String,
        pub artifact: Option<ArtifactRef>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BindConfig {
        pub ports: HashMap<String, PortConfig>,
    }

    impl Default for BindConfig {
        fn default() -> Self {
            Self {
                ports: Default::default(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PortConfig {
        pub payload: PayloadConfig,
        pub response: EntityConfig,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum EntityConfig {
        Empty,
        Resource(ResourceConfig),
        Payload(PayloadConfig),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum ResourceConfig {
        None,
        Resource,
        Resources,
        State,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum PayloadConfig {
        Text,
        Bin(SchemaRef),
        Bins(HashMap<String, SchemaRef>),
    }
}

pub mod entity {

    #[derive(Debug, Clone)]
    pub enum EntityType {
        Rc,
        Msg,
        Http
    }

    pub mod request {
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{
            Address, Key, Kind, PayloadClaim, ResourceType,
        };
        use crate::version::v0_0_1::payload::Payload;

        pub type ReqEntity = generic::entity::request::ReqEntity<Payload>;
        pub type Rc = generic::entity::request::Rc<Payload>;
        pub type Msg = generic::entity::request::Msg<Payload>;
        pub type Http = generic::entity::request::Http<Payload>;
    }

    pub mod response {
        use crate::version::v0_0_1::{fail, generic};
        use crate::version::v0_0_1::id::{Address, Key, Kind};
        use crate::version::v0_0_1::payload::Payload;

        pub type RespEntity = generic::entity::response::RespEntity<Payload, fail::Fail>;
    }
}

pub mod resource {
    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Address, Identifier, Key, Kind, ResourceType};

    #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display, Eq,PartialEq)]
    pub enum Status {
        Unknown,
        Initializing,
        Ready,
        Panic(String),
        Done,
    }

    pub type Archetype = generic::resource::Archetype<Kind, Address>;
    pub type ResourceStub = generic::resource::ResourceStub<Key, Address, Kind>;
}

pub mod portal {
    pub mod inlet {
        use std::convert::TryFrom;

        use crate::error::Error;
        use crate::version::v0_0_1::frame::PrimitiveFrame;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Identifier, Key, Kind, ResourceType};
        use crate::version::v0_0_1::payload::Payload;

        pub type Request = generic::portal::inlet::Request<Identifier, Payload>;
        pub type Response = generic::portal::inlet::Response<Identifier, Payload>;
        pub type Frame = generic::portal::inlet::Frame<Identifier, Payload>;

        impl TryFrom<PrimitiveFrame> for generic::portal::inlet::Frame<Identifier, Payload> {
            type Error = Error;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }

    pub mod outlet {
        use std::convert::TryFrom;

        use crate::error::Error;
        use crate::version::v0_0_1::frame::PrimitiveFrame;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Identifier, Key, Kind, ResourceType};
        use crate::version::v0_0_1::payload::Payload;

        pub type Request = generic::portal::outlet::Request<Identifier, Payload>;
        pub type Response = generic::portal::outlet::Response<Identifier,Payload>;
        pub type Frame = generic::portal::outlet::Frame<Key, Address, Identifier, Kind, Payload>;

        impl TryFrom<PrimitiveFrame>
            for generic::portal::outlet::Frame<Key, Address, Identifier, Kind, Payload>
        {
            type Error = Error;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }
}

pub mod generic {
    use std::collections::HashMap;
    use std::convert::From;
    use std::convert::TryInto;
    use std::fmt::Debug;
    use std::hash::Hash;
    use std::str::FromStr;
    use std::sync::Arc;

    use serde::{Deserialize, Serialize};

    pub mod id {
        use std::convert::{TryFrom, TryInto};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::error::Error;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::util::Convert;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
        pub enum Identifier<KEY, ADDRESS> {
            Key(KEY),
            Address(ADDRESS),
        }

        impl <KEY,ADDRESS> ToString for Identifier<KEY,ADDRESS> where KEY: ToString, ADDRESS: ToString{
            fn to_string(&self) -> String {
                match self {
                    Identifier::Key(key) => {key.to_string()}
                    Identifier::Address(address) => {address.to_string()}
                }
            }
        }

        impl<FromKey, FromAddress> Identifier<FromKey, FromAddress>
        where
            FromKey: ,
            FromAddress: ,
        {
            pub fn convert<ToKey, ToAddress>(self) -> Result<Identifier<ToKey, ToAddress>, Error>
            where
                ToKey: TryFrom<FromKey, Error = Error>,
                ToAddress: TryFrom<FromAddress, Error = Error>,
            {
                match self {
                    Identifier::Key(key) => Ok(Identifier::Key(key.try_into()?)),
                    Identifier::Address(address) => Ok(Identifier::Address(address.try_into()?)),
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
        pub struct Identifiers<KEY, ADDRESS> {
            pub key: KEY,
            pub address: ADDRESS,
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
        pub struct AddressAndKind<ADDRESS, KIND> {
            pub address: ADDRESS,
            pub kind: KIND,
        }

        impl<ADDRESS, KIND> AddressAndKind<ADDRESS, KIND> {
            pub fn new(address: ADDRESS, kind: KIND) -> Self {
                Self { address, kind }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
        pub struct AddressAndType<ADDRESS, RESOURCE_TYPE> {
            pub address: ADDRESS,
            pub resource_type: RESOURCE_TYPE,
        }
    }

    pub mod config {
        use std::convert::{TryFrom, TryInto};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::error::Error;
        use crate::version::v0_0_1::ArtifactRef;
        use crate::version::v0_0_1::config::{Config, PortalKind};
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::id::{Identifier, Identifiers};
        use crate::version::v0_0_1::generic::resource::Archetype;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Info<KEY, ADDRESS, KIND> {
            pub key: KEY,
            pub address: ADDRESS,
            pub owner: String,
            pub parent: Identifier<KEY, ADDRESS>,
            pub archetype: Archetype<KIND, ADDRESS>,
            pub config: Config,
            pub ext_config: Option<ArtifactRef>,
            pub kind: PortalKind,
        }

        impl<KEY, ADDRESS, KIND> Info<KEY, ADDRESS, KIND>
        where
            KEY: Clone,
            ADDRESS: Clone,
            KIND: Clone,
        {
            pub fn identity(&self) -> Identifiers<KEY, ADDRESS> {
                Identifiers {
                    key: self.key.clone(),
                    address: self.address.clone(),
                }
            }
        }

        impl<FromKey, FromAddress, FromKind> Info<FromKey, FromAddress, FromKind>
        where
            FromKey: ,
            FromAddress: ,
            FromKind: ,
        {
            pub fn convert<ToKey, ToAddress, ToKind>(
                self,
            ) -> Result<Info<ToKey, ToAddress, ToKind>, Error>
            where
                ToKey: TryFrom<FromKey, Error = Error>,
                ToAddress: TryFrom<FromAddress, Error = Error>,
                ToKind: TryFrom<FromKind, Error = Error>,
            {
                Ok(Info {
                    key: self.key.try_into()?,
                    address: self.address.try_into()?,
                    owner: self.owner,
                    parent: self.parent.convert()?,
                    archetype: self.archetype.convert()?,
                    config: self.config,
                    ext_config: self.ext_config,
                    kind: self.kind,
                })
            }
        }
    }

    pub mod entity {
        pub mod request {
            use std::convert::{TryFrom, TryInto};
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};
            use serde::__private::fmt::Debug;

            use crate::error::Error;
            use crate::version::v0_0_1::{http, State};
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::generic;
            use crate::version::v0_0_1::generic::payload::{HttpMethod, Payload};
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::generic::payload::RcCommand;
            use crate::version::v0_0_1::id::Meta;
            use crate::version::v0_0_1::util::{Convert, ConvertFrom};
            use crate::version::v0_0_1::generic::entity::response::RespEntity;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum ReqEntity<PAYLOAD> {
                Rc(Rc<PAYLOAD>),
                Msg(Msg<PAYLOAD>),
                Http(Http<PAYLOAD>),
            }

            impl <PAYLOAD> ReqEntity<PAYLOAD> {
                pub fn ok<FAIL>(&self, payload: PAYLOAD) -> RespEntity<PAYLOAD,FAIL> {
                    RespEntity::Ok(payload)
                }

                pub fn fail<FAIL>(&self, fail: FAIL ) -> RespEntity<PAYLOAD,FAIL>{
                    RespEntity::Fail(fail)
                }
            }

            impl<FromPayload, ToPayload> ConvertFrom<ReqEntity<FromPayload>> for ReqEntity<ToPayload>
            where
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(a: ReqEntity<FromPayload>) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    match a {
                        ReqEntity::Rc(rc) => Ok(ReqEntity::Rc(ConvertFrom::convert_from(rc)?)),
                        ReqEntity::Msg(msg) => Ok(ReqEntity::Msg(ConvertFrom::convert_from(msg)?)),
                        ReqEntity::Http(http) => Ok(ReqEntity::Http(ConvertFrom::convert_from(http)?)),
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Rc<PAYLOAD> {
                pub command: RcCommand,
                pub payload: PAYLOAD,
            }

            impl <PAYLOAD> ToString for Rc<PAYLOAD> {
                fn to_string(&self) -> String {
                    format!("Rc<{}>",self.command.to_string())
                }
            }

            impl<FromPayload, ToPayload> ConvertFrom<Rc<FromPayload>> for Rc<ToPayload>
            where
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(a: Rc<FromPayload>) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    Ok(Rc {
                        command: a.command,
                        payload: a.payload.try_into()?,
                    })
                }
            }

            impl<FromPayload> Rc<FromPayload> {
                pub fn convert<ToPayload>(self) -> Result<Rc<ToPayload>, Error>
                where
                    ToPayload: ConvertFrom<FromPayload>,
                {
                    Ok(Rc {
                        command: self.command,
                        payload: ConvertFrom::convert_from(self.payload)?,
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Msg<PAYLOAD> {
                pub action: String,
                pub path: String,
                pub payload: PAYLOAD,
            }

            impl <PAYLOAD> ToString for Msg<PAYLOAD> {
                fn to_string(&self) -> String {
                    format!("Msg<{}>{}",self.action,self.path)
                }
            }


            impl<FromPayload, ToPayload> ConvertFrom<Msg<FromPayload>> for Msg<ToPayload>
            where
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(a: Msg<FromPayload>) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    Ok(Msg {
                        action: a.action,
                        path: a.path,
                        payload: a.payload.try_into()?,
                    })
                }
            }

            impl<FromPayload> Msg<FromPayload> {
                pub fn convert<ToPayload>(self) -> Result<Msg<ToPayload>, Error>
                where
                    ToPayload: ConvertFrom<FromPayload>,
                {
                    Ok(Msg {
                        action: self.action,
                        path: self.path,
                        payload: ConvertFrom::convert_from(self.payload)?,
                    })
                }
            }



            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Http<PAYLOAD> {
                pub headers: Meta,
                pub method: HttpMethod,
                pub path: String,
                pub body: PAYLOAD,
            }

            impl <PAYLOAD> ToString for Http<PAYLOAD> {
                fn to_string(&self) -> String {
                    format!("Http<{}>{}",self.method.to_string(),self.path)
                }
            }

            impl<FromPayload, ToPayload> ConvertFrom<Http<FromPayload>> for Http<ToPayload>
                where
                    FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(a: Http<FromPayload>) -> Result<Self, Error>
                    where
                        Self: Sized,
                {
                    Ok(Http{
                        headers: a.headers,
                        method: a.method,
                        path: a.path,
                        body: a.body.try_into()?,
                    })
                }
            }

            impl<FromPayload> Http<FromPayload> {
                pub fn convert<ToPayload>(self) -> Result<Http<ToPayload>, Error>
                    where
                        ToPayload: ConvertFrom<FromPayload>,
                {
                    Ok(Http{
                        headers: self.headers,
                        method: self.method,
                        path: self.path,
                        body: ConvertFrom::convert_from(self.body)?,
                    })
                }
            }



        }

        pub mod response {
            use std::convert::{TryFrom, TryInto};
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::error::Error;
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::fail::portal::Fail;
            use crate::version::v0_0_1::generic::payload::Payload;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::util::ConvertFrom;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum RespEntity<PAYLOAD, FAIL> {
                Ok(PAYLOAD),
                Fail(FAIL),
            }

            impl<FromPayload, ToPayload, FAIL> ConvertFrom<RespEntity<FromPayload, FAIL>>
                for RespEntity<ToPayload, FAIL>
            where
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(
                    a: RespEntity<FromPayload, FAIL>,
                ) -> Result<Self, crate::error::Error>
                where
                    Self: Sized,
                {
                    match a {
                        RespEntity::Ok(payload) => Ok(RespEntity::Ok(payload.try_into()?)),
                        RespEntity::Fail(fail) => Ok(RespEntity::Fail(fail)),
                    }
                }
            }

            impl<FromPayload, FAIL> RespEntity<FromPayload, FAIL> {
                pub fn convert<ToPayload>(self) -> Result<RespEntity<FromPayload, FAIL>, Error>
                where
                    ToPayload: TryFrom<FromPayload, Error = Error>,
                {
                    match self {
                        RespEntity::Ok(ok) => Ok(RespEntity::Ok(ok.try_into()?)),
                        RespEntity::Fail(fail) => Ok(RespEntity::Fail(fail)),
                    }
                }
            }
        }
    }

    pub mod resource {
        use std::collections::{HashMap, HashSet};
        use std::convert::{TryFrom, TryInto};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::error::Error;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::id::{AddressAndKind, Identifier};
        use crate::version::v0_0_1::generic::payload::{MapPattern, Payload, PayloadType};
        use crate::version::v0_0_1::generic::payload::Primitive;
        use crate::version::v0_0_1::State;
        use crate::version::v0_0_1::util::ConvertFrom;

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct Archetype<KIND, ADDRESS> {
            pub kind: KIND,
            pub config_src: Option<ADDRESS>,
        }

        impl<FromAddress, FromKind, ToAddress, ToKind> ConvertFrom<Archetype<FromAddress, FromKind>>
            for Archetype<ToAddress, ToKind>
        where
            FromAddress: TryInto<ToAddress, Error = Error>,
            FromKind: TryInto<ToKind, Error = Error>,
        {
            fn convert_from(a: Archetype<FromAddress, FromKind>) -> Result<Self, Error>
            where
                Self: Sized,
            {
                Ok(Archetype {
                    kind: a.kind.try_into()?,
                    config_src: match a.config_src {
                        None => None,
                        Some(some) => Some(some.try_into()?),
                    },
                })
            }
        }

        impl<FromKind, FromAddress> Archetype<FromKind, FromAddress>
        where
            FromKind: ,
            FromAddress: ,
        {
            pub fn convert<ToKind, ToAddress>(self) -> Result<Archetype<ToKind, ToAddress>, Error>
            where
                ToKind: TryFrom<FromKind, Error = Error>,
                ToAddress: TryFrom<FromAddress, Error = Error>,
            {
                let config_src = match self.config_src {
                    None => None,
                    Some(config_src) => Some(config_src.try_into()?),
                };
                Ok(Archetype {
                    kind: self.kind.try_into()?,
                    config_src,
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct ResourceStub<KEY, ADDRESS, KIND> {
            pub key: KEY,
            pub address: ADDRESS,
            pub archetype: Archetype<KIND, ADDRESS>,
        }

        impl<FromKey, FromAddress, FromKind> ResourceStub<FromKey, FromAddress, FromKind> {
            pub fn convert<ToKey, ToAddress, ToKind>(
                self,
            ) -> Result<ResourceStub<ToKey, ToAddress, ToKind>, Error>
            where
                ToKey: TryFrom<FromKey, Error = Error>,
                ToAddress: TryFrom<FromAddress, Error = Error>,
                ToKind: TryFrom<FromKind, Error = Error>,
            {
                Ok(ResourceStub {
                    key: self.key.try_into()?,
                    address: self.address.try_into()?,
                    archetype: self.archetype.convert()?,
                })
            }
        }

        impl<FromKey, FromAddress, FromKind, ToKey, ToAddress, ToKind>
            ConvertFrom<ResourceStub<FromKey, FromAddress, FromKind>>
            for ResourceStub<ToKey, ToAddress, ToKind>
        where
            FromKey: TryInto<ToKey, Error = Error>,
            FromAddress: TryInto<ToAddress, Error = Error>,
            FromKind: TryInto<ToKind, Error = Error>,
        {
            fn convert_from(a: ResourceStub<FromKey, FromAddress, FromKind>) -> Result<Self, Error>
            where
                Self: Sized,
            {
                Ok(ResourceStub {
                    key: a.key.try_into()?,
                    address: a.address.try_into()?,
                    archetype: ConvertFrom::convert_from(a.archetype)?,
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct Resource<KEY, ADDRESS, IDENTIFIER, KIND>
        {
            pub stub: ResourceStub<KEY, ADDRESS, KIND>,
            pub state: Box<Payload<KEY, ADDRESS, IDENTIFIER, KIND>>,
        }

        impl<
                FromKey,
                FromAddress,
                FromIdentifier,
                FromKind,
                ToKey,
                ToAddress,
                ToIdentifier,
                ToKind,
            > ConvertFrom<Resource<FromKey, FromAddress, FromIdentifier, FromKind>>
            for Resource<ToKey, ToAddress, ToIdentifier, ToKind>
        where
            FromKey: TryInto<ToKey, Error = Error> + Clone,
            FromAddress: TryInto<ToAddress, Error = Error> + Clone,
            FromIdentifier: TryInto<ToIdentifier, Error = Error> + Clone,
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            ToKey: Clone,
            ToAddress: Clone,
            ToIdentifier: Clone,
            ToKind: Clone,
        {
            fn convert_from(
                a: Resource<FromKey, FromAddress, FromIdentifier, FromKind>,
            ) -> Result<Self, Error>
            where
                Self: Sized,
            {
                Ok(Resource {
                    stub: ConvertFrom::convert_from(a.stub)?,
                    state: Box::new(ConvertFrom::convert_from(*a.state)?),
                })
            }
        }
    }

    pub mod portal {
        pub mod inlet {
            use std::convert::TryFrom;
            use std::convert::TryInto;
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::error::Error;
            use crate::version::v0_0_1::command::Command;
            use crate::version::v0_0_1::fail;
            use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
            use crate::version::v0_0_1::generic::entity::request;
            use crate::version::v0_0_1::generic::entity::request::ReqEntity;
            use crate::version::v0_0_1::generic::entity::response;
            use crate::version::v0_0_1::generic::id::Identifier;
            use crate::version::v0_0_1::id::{Address, Key, Kind, ResourceType};
            use crate::version::v0_0_1::log::Log;
            use crate::version::v0_0_1::messaging::Exchange;
            use crate::version::v0_0_1::messaging::ExchangeId;
            use crate::version::v0_0_1::resource::Status;
            use crate::version::v0_0_1::util::{ConvertFrom, unique_id};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<IDENTIFIER, PAYLOAD> {
                pub id: String,
                pub to: Vec<IDENTIFIER>,
                pub entity: request::ReqEntity<PAYLOAD>,
                pub exchange: Exchange
            }

            impl<IDENTIFIER, PAYLOAD> Request<IDENTIFIER, PAYLOAD> {
                pub fn new(entity: request::ReqEntity<PAYLOAD>) -> Self {
                    Self {
                        id: unique_id(),
                        to: vec![],
                        entity,
                        exchange: Exchange::Notification
                    }
                }
            }

            impl<IDENTIFIER, PAYLOAD> Request<IDENTIFIER, PAYLOAD> {
                pub fn exchange(
                    self,
                    exchange: Exchange,
                ) -> Request<IDENTIFIER, PAYLOAD> {
                    Request {
                        id: self.id,
                        to: self.to,
                        entity: self.entity,
                        exchange,
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Response<IDENTIFIER, PAYLOAD> {
                pub id: String,
                pub to: IDENTIFIER,
                pub exchange: ExchangeId,
                pub entity: response::RespEntity<PAYLOAD, fail::portal::Fail>,
            }

            impl<FromIdentifier, FromPayload, ToIdentifier, ToPayload>
                ConvertFrom<Response<FromIdentifier, FromPayload>>
                for Response<ToIdentifier, ToPayload>
            where
                FromIdentifier: TryInto<ToIdentifier, Error = Error>,
                FromPayload: TryInto<ToPayload, Error = Error>,
            {
                fn convert_from(a: Response<FromIdentifier, FromPayload>) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    Ok(Response {
                        id: a.id,
                        to: a.to.try_into()?,
                        entity: ConvertFrom::convert_from(a.entity)?,
                        exchange: a.exchange,
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
            pub enum Frame<IDENTIFIER, PAYLOAD> {
                Log(Log),
                Command(Command),
                Request(Request<IDENTIFIER, PAYLOAD>),
                Response(Response<IDENTIFIER, PAYLOAD>),
                Status(Status),
                Close(CloseReason),
            }

            impl<IDENTIFIER: Serialize, PAYLOAD: Serialize> TryInto<PrimitiveFrame>
                for Frame<IDENTIFIER, PAYLOAD>
            {
                type Error = Error;

                fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                    Ok(PrimitiveFrame {
                        data: bincode::serialize(&self)?,
                    })
                }
            }

        }

        pub mod outlet {
            use std::convert::TryFrom;
            use std::convert::TryInto;
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::error::Error;
            use crate::version::v0_0_1::generic;
            use crate::version::v0_0_1::command::CommandEvent;
            use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
            use crate::version::v0_0_1::generic::config::Info;
            use crate::version::v0_0_1::generic::entity::request;
            use crate::version::v0_0_1::generic::entity::response;
            use crate::version::v0_0_1::generic::id::Identifier;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::generic::portal::inlet;
            use crate::version::v0_0_1::id::{Address, Key, Kind, ResourceType};
            use crate::version::v0_0_1::messaging::{Exchange, ExchangeId};
            use crate::version::v0_0_1::util::{ConvertFrom, unique_id};
            use crate::version::v0_0_1::fail;
            use crate::version::v0_0_1::generic::entity::response::RespEntity;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<IDENTIFIER, PAYLOAD> {
                pub from: IDENTIFIER,
                pub entity: request::ReqEntity<PAYLOAD>,
                pub exchange: Exchange
            }

            impl<IDENTIFIER, PAYLOAD> Request<IDENTIFIER, PAYLOAD> {
                pub fn exchange(
                    self,
                    exchange: Exchange,
                ) -> Request<IDENTIFIER, PAYLOAD> {
                    Self {
                        from: self.from,
                        entity: self.entity,
                        exchange,
                    }
                }

                pub fn ensure_notification(&self) -> Result<(),Error> {
                    match self.exchange {
                        Exchange::Notification => {
                            Ok(())
                        }
                        Exchange::RequestResponse(_) => {
                            Err("expected Notification Exchange but found RequestResponse Exchange".into())
                        }
                    }
                }

                pub fn ok( self, payload: PAYLOAD ) -> Result<inlet::Response<IDENTIFIER,PAYLOAD>,Error> {
                    Ok(inlet::Response {
                        id: unique_id(),
                        to: self.from,
                        exchange: self.exchange.try_into()?,
                        entity: RespEntity::Ok(payload)
                    })
                }

                pub fn fail( self, fail: fail::portal::Fail ) -> Result<inlet::Response<IDENTIFIER,PAYLOAD>,Error> {
                    Ok(inlet::Response {
                        id: unique_id(),
                        to: self.from,
                        exchange: self.exchange.try_into()?,
                        entity: RespEntity::Fail(fail)
                    })
                }
            }



            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Response<IDENTIFIER,PAYLOAD> {
                pub id: String,
                pub to: IDENTIFIER,
                pub from: IDENTIFIER,
                pub entity: response::RespEntity<PAYLOAD, fail::Fail>,
                pub exchange: ExchangeId
            }

            impl <IDENTIFIER,PAYLOAD> Response<IDENTIFIER,PAYLOAD> {
                pub fn new( to: IDENTIFIER, from: IDENTIFIER, entity: response::RespEntity<PAYLOAD, fail::Fail>, exchange: ExchangeId ) -> Self {
                    Self {
                        id: unique_id(),
                        to,
                        from,
                        entity,
                        exchange
                    }
                }
            }

            impl<FromIdentifier, FromPayload> Response<FromIdentifier, FromPayload> {
                pub fn convert<ToIdentifier, ToPayload>(
                    self,
                ) -> Result<Response<ToIdentifier, ToPayload>, Error>
                where
                    ToIdentifier: TryFrom<FromIdentifier, Error = Error>,
                    ToPayload: TryFrom<FromPayload, Error = Error>,
                {
                    Ok(Response {
                        id: self.id,
                        to: self.to.try_into()?,
                        from: self.from.try_into()?,
                        entity: ConvertFrom::convert_from(self.entity)?,
                        exchange: self.exchange
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
            pub enum Frame<KEY, ADDRESS, IDENTIFIER, KIND, PAYLOAD> {
                Create(Info<KEY, ADDRESS, KIND>),
                CommandEvent(CommandEvent),
                Request(Request<IDENTIFIER,PAYLOAD>),
                Response(Response<IDENTIFIER,PAYLOAD>),
                Close(CloseReason),
            }

            /*
            impl<FromKey, FromAddress, FromIdentifier, FromKind, FromPayload>
                Frame<FromKey, FromAddress, FromKind, FromPayload>
            {
                pub fn convert<ToKey, ToAddress, ToKind, ToPayload>(
                    self,
                ) -> Result<Frame<ToKey, ToAddress, ToKind, ToPayload>, Error>
                where
                    ToKey: TryFrom<FromKey, Error = Error>,
                    ToAddress: TryFrom<FromAddress, Error = Error>,
                    ToKind: TryFrom<FromKind, Error = Error>,
                    ToPayload: ConvertFrom<FromPayload>,
                {
                    match self {
                        Frame::Init(init) => Ok(Frame::Init(init.convert()?)),
                        Frame::CommandEvent(event) => Ok(Frame::CommandEvent(event)),
                        Frame::Request(request) => Ok(Frame::Request(request.convert()?)),
                        Frame::Response(response) => Ok(Frame::Response(response.convert()?)),
                        Frame::BinParcel(parcel) => Ok(Frame::BinParcel(parcel)),
                        Frame::Close(close) => Ok(Frame::Close(close)),
                    }
                }
            }
             */

            impl<
                    KEY: Serialize,
                    ADDRESS: Serialize,
                    IDENTIFIER: Serialize,
                    KIND: Serialize,
                    PAYLOAD: Serialize,
                > TryInto<PrimitiveFrame> for Frame<KEY, ADDRESS, IDENTIFIER, KIND, PAYLOAD>
            {
                type Error = Error;

                fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                    Ok(PrimitiveFrame {
                        data: bincode::serialize(&self)?,
                    })
                }
            }

            /*
            impl<
                KEY: Serialize,
                ADDRESS: Serialize,
                IDENTIFIER: Serialize,
                KIND: Serialize,
                PAYLOAD: Serialize,
            > TryFrom<PrimitiveFrame> for Frame<KEY, ADDRESS, IDENTIFIER, KIND, PAYLOAD>
            {
                type Error = Error;

                fn try_from(frame: PrimitiveFrame) -> Result<PrimitiveFrame, Self::Error> {
                    Ok(
                        bincode::deserialize(frame.data.as_slice())?
                     )
                }
            }

             */

        }
    }

    pub mod payload {
        use std::collections::HashMap;
        use std::convert::{TryFrom, TryInto};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::marker::PhantomData;
        use std::ops::{Deref, DerefMut};
        use std::path::Path;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::error::Error;
        use crate::version::v0_0_1::{http, State};
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::resource::{Resource, ResourceStub};
        use crate::version::v0_0_1::payload::PrimitiveType;
        use crate::version::v0_0_1::resource::Status;
        use crate::version::v0_0_1::util::{Convert, ConvertFrom, ValueMatcher, ValuePattern};

        #[derive(
            Debug, Clone, Serialize, Deserialize, Eq, PartialEq,strum_macros::Display,strum_macros::EnumString
        )]
        pub enum PayloadType
        {
            Empty,
            Primitive,
            List,
            Map,
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct ListPattern {
            pub primitive: PrimitiveType,
            pub range: Range,
        }

        impl ListPattern {
            pub fn is_match<KEY,ADDRESS,IDENTIFIER,KIND>( &self, list: &PrimitiveList<KEY,ADDRESS,IDENTIFIER,KIND> ) -> Result<(),Error> {
                for i in &list.list {
                    if self.primitive != i.primitive_type()  {
                        return Err(format!("Primitive List expected: {} found: {}",self.primitive.to_string(),i.primitive_type().to_string()).into());
                    }
                }

                Ok(())
            }
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub enum Range {
            MinMax { min: usize, max: usize },
            Exact(usize),
            Any
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub enum PayloadTypePattern<KEY,ADDRESS,IDENTIFIER,KIND> {
            Empty,
            Primitive(PrimitiveType),
            List(ListPattern),
            Map(Box<MapPattern<KEY,ADDRESS,IDENTIFIER,KIND>>)
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> PayloadTypePattern<KEY,ADDRESS,IDENTIFIER,KIND> {
            pub fn is_match( &self, payload: &Payload<KEY,ADDRESS,IDENTIFIER,KIND> ) -> Result<(),Error> {
                match self {
                    PayloadTypePattern::Empty => {
                        if payload.payload_type() == PayloadType::Empty {
                            Ok(())
                        } else {

                            Err(format!("Payload expected: Empty found: {}",payload.payload_type().to_string()).into())
                        }
                    }
                    PayloadTypePattern::Primitive(expected ) => {
                        if let Payload::Primitive(found) = payload {
                            if *expected == found.primitive_type() {
                                Ok(())
                            } else {
                                Err(format!("Payload Primitive expected: {} found: {}", expected.to_string(), found.primitive_type().to_string()).into())
                            }
                        } else {
                            Err(format!("Payload expected: {} found: {}", expected.to_string(), payload.payload_type().to_string()).into())
                        }
                    }
                    PayloadTypePattern::List(expected) => {
                        if let Payload::List(found) = payload {
                            expected.is_match(found )
                        } else {
                            Err(format!("Payload expected: List found: {}", payload.payload_type().to_string()).into())
                        }
                    }
                    PayloadTypePattern::Map(expected) => {
                        if let Payload::Map(found) = payload {
                            expected.is_match(found)
                        } else {
                            Err(format!("Payload expected: {} found: {}",expected.to_string(), payload.payload_type().to_string()).into())
                        }
                    }
                }

            }
        }
        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct PayloadPattern<KEY,ADDRESS,IDENTIFIER,KIND> {
            pub structure: PayloadTypePattern<KEY,ADDRESS,IDENTIFIER,KIND>,
            pub format: Option<PayloadFormat>,
            pub validator: Option<CallWithConfig<ADDRESS>>,
        }


        impl<
            FromKey,
            FromAddress,
            FromIdentifier,
            FromKind,
            ToKey,
            ToAddress,
            ToIdentifier,
            ToKind,
        > ConvertFrom<Option<PayloadPattern<FromKey, FromAddress, FromIdentifier, FromKind>>>
        for Option<PayloadPattern<ToKey, ToAddress, ToIdentifier, ToKind>>
            where
                FromKey: TryInto<ToKey, Error = Error> + Clone,
                FromAddress: TryInto<ToAddress, Error = Error> + Clone,
                FromIdentifier: TryInto<ToIdentifier, Error = Error> + Clone,
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKey: Clone,
                ToAddress: Clone,
                ToIdentifier: Clone,
                ToKind: Clone,
                PayloadTypePattern<ToKey,ToAddress,ToIdentifier,ToKind>: ConvertFrom<PayloadTypePattern<FromKey,FromAddress,FromIdentifier,FromKind>>,
                PayloadPattern<ToKey,ToAddress,ToIdentifier,ToKind>: ConvertFrom<PayloadPattern<FromKey,FromAddress,FromIdentifier,FromKind>>

        {
            fn convert_from(
                a: Option<PayloadPattern<FromKey, FromAddress, FromIdentifier, FromKind>>,
            ) -> Result<Self, Error>
                where
                    Self: Sized,
            {
                let rtn = match a{
                    None => None,
                    Some(rtn) => {
                        Option::Some(ConvertFrom::convert_from(rtn)?)
                    }
                };
                Ok(rtn)
            }
        }

        impl<
            FromKey,
            FromAddress,
            FromIdentifier,
            FromKind,
            ToKey,
            ToAddress,
            ToIdentifier,
            ToKind,
        > ConvertFrom<PayloadPattern<FromKey, FromAddress, FromIdentifier, FromKind>>
        for PayloadPattern<ToKey, ToAddress, ToIdentifier, ToKind>
            where
                FromKey: TryInto<ToKey, Error = Error> + Clone,
                FromAddress: TryInto<ToAddress, Error = Error> + Clone,
                FromIdentifier: TryInto<ToIdentifier, Error = Error> + Clone,
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKey: Clone,
                ToAddress: Clone,
                ToIdentifier: Clone,
                ToKind: Clone,
                Call<ToAddress>: TryFrom<Call<FromAddress>,Error=Error>,
                Option<ToAddress>: TryFrom<Option<FromAddress>,Error=Error>

        {
            fn convert_from(
                a: PayloadPattern<FromKey, FromAddress, FromIdentifier, FromKind>,
            ) -> Result<Self, Error>
                where
                    Self: Sized,
            {
                Ok(Self {
                    structure: ConvertFrom::convert_from(a.structure)?,
                    format: a.format,
                    validator: ConvertFrom::convert_from(a.validator)?
                })
            }
        }

        impl<
            FromKey,
            FromAddress,
            FromIdentifier,
            FromKind,
            ToKey,
            ToAddress,
            ToIdentifier,
            ToKind,
        > ConvertFrom<PayloadTypePattern<FromKey, FromAddress, FromIdentifier, FromKind>>
        for PayloadTypePattern<ToKey, ToAddress, ToIdentifier, ToKind>
            where
                FromKey: TryInto<ToKey, Error = Error> + Clone,
                FromAddress: TryInto<ToAddress, Error = Error> + Clone,
                FromIdentifier: TryInto<ToIdentifier, Error = Error> + Clone,
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKey: Clone,
                ToAddress: Clone,
                ToIdentifier: Clone,
                ToKind: Clone,
                Call<ToAddress> : TryFrom<Call<FromAddress>,Error=Error>,
                Option<ToAddress> : TryFrom<Option<FromAddress>,Error=Error>,

        {
            fn convert_from(
                a: PayloadTypePattern<FromKey, FromAddress, FromIdentifier, FromKind>,
            ) -> Result<Self, Error>
                where
                    Self: Sized,
            {
                let rtn = match a {
                    PayloadTypePattern::Empty => {
                        PayloadTypePattern::Empty
                    }
                    PayloadTypePattern::Primitive(primitive) => {
                        PayloadTypePattern::Primitive(primitive)
                    }
                    PayloadTypePattern::List(list) => {
                        PayloadTypePattern::List(list)
                    }
                    PayloadTypePattern::Map(map) => {
                        PayloadTypePattern::Map(Box::new(ConvertFrom::convert_from(*map)?))
                    }
                };

                Ok(rtn)
            }
        }


        impl<KEY,ADDRESS,IDENTIFIER,KIND> ValueMatcher<Payload<KEY,ADDRESS,IDENTIFIER,KIND>> for  PayloadPattern<KEY,ADDRESS,IDENTIFIER,KIND> {
            fn is_match(&self, payload: &Payload<KEY, ADDRESS, IDENTIFIER, KIND>) -> Result<(), Error> {
                self.structure.is_match(&payload)?;

                // more validation to come...
                Ok(())
            }
        }
        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct CallWithConfig<ADDRESS> {
            pub call: Call<ADDRESS>,
            pub config: Option<ADDRESS>
        }


        impl <FromAddress,ToAddress> ConvertFrom<Option<CallWithConfig<FromAddress>>> for Option<CallWithConfig<ToAddress>>
            where FromAddress: TryInto<ToAddress,Error=Error>, Call<ToAddress>: TryFrom<Call<FromAddress>,Error=Error>, Option<ToAddress>: TryFrom<Option<FromAddress>>
        {
            fn convert_from(a: Option<CallWithConfig<FromAddress>>) -> Result<Self, crate::error::Error> where Self: Sized {
                let rtn = match a{
                    None => None,
                    Some(some) => {
                        Option::Some(ConvertFrom::convert_from(some)?)
                    }
                };

                Ok(rtn)
            }
        }

        impl <FromAddress,ToAddress> ConvertFrom<CallWithConfig<FromAddress>> for CallWithConfig<ToAddress>
          where FromAddress: TryInto<ToAddress,Error=Error>, Call<ToAddress>: TryFrom<Call<FromAddress>,Error=Error>
        {
            fn convert_from(a: CallWithConfig<FromAddress>) -> Result<Self, crate::error::Error> where Self: Sized {
                let config = match a.config {
                    None => None,
                    Some(from) => {
                        Some(from.try_into()?)
                    }
                };
                Ok(Self {
                    call: a.call.try_into()?,
                    config: config
                })
            }
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct Call<ADDRESS> {
            pub address: ADDRESS,
            pub kind: CallKind
        }

        impl <ADDRESS> ToString for Call<ADDRESS> where ADDRESS: ToString {
            fn to_string(&self) -> String {
                format!("{}^{}",self.address.to_string(), self.kind.to_string())
            }
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub enum CallKind{
            Rc(RcCommand),
            Msg(MsgCall),
            Http(HttpCall)
        }


        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct MsgCall {
            pub path: String,
            pub action: String
        }

        impl MsgCall {
            pub fn new( action: String, path: String ) -> Self {
                Self{
                    action,
                    path
                }
            }
        }

        impl ToString for MsgCall {
            fn to_string(&self) -> String {
                format!("Msg<{}>{}",self.action,self.path)
            }
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize)]
        pub struct HttpCall {
            pub path: String,
            pub method: HttpMethod
        }

        impl HttpCall {
            pub fn new( method: HttpMethod, path: String) -> Self {
                Self{
                    method,
                    path
                }
            }
        }


        impl ToString for HttpCall {
            fn to_string(&self) -> String {
                format!("Http<{}>{}",self.method.to_string(),self.path)
            }
        }


        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize,strum_macros::Display,strum_macros::EnumString)]
        pub enum HttpMethod {
            Get,
            Post,
            Put,
            Delete,
            Patch,
            Head,
            Connect,
            Options,
            Trace
        }

        impl ValueMatcher<HttpMethod> for HttpMethod {
            fn is_match(&self, found: &HttpMethod) -> Result<(), crate::error::Error> {
                if *self == *found {
                    Ok(())
                } else {
                    Err(format!("Http Method mismatch. expected: '{}', found: '{}'", self.to_string(), found.to_string()).into())
                }
            }
        }


        impl ToString for CallKind {
            fn to_string(&self) -> String {
                match self {
                    CallKind::Rc(command) => {
                        format!("Rc<{}>",command.to_string())
                    }
                    CallKind::Msg(msg) => {
                        msg.to_string()
                    }
                    CallKind::Http(http) => {
                        http.to_string()
                    }

                }
            }
        }


        #[derive(Debug,Clone,Eq,PartialEq,strum_macros::Display,strum_macros::EnumString,Serialize,Deserialize)]
        pub enum RcCommand{
            Create,
            Select
        }

        impl ValueMatcher<RcCommand> for RcCommand {
            fn is_match(&self, x: &RcCommand) -> Result<(), crate::error::Error> {
                if *self == *x {
                    Ok(())
                } else {
                    Err(format!("Rc command expected: '{}' found: '{}'", self.to_string(),x.to_string()).into())
                }
            }
        }



        #[derive(Debug,Clone,Eq,PartialEq,strum_macros::Display,strum_macros::EnumString,Serialize,Deserialize)]
        pub enum PayloadFormat {
            #[strum(serialize = "json")]
            Json,
            #[strum(serialize = "image")]
            Image
        }


        impl PrimitiveType {
            pub fn is_match<KEY, ADDRESS, IDENTIFIER, KIND>( &self, primitive: &generic::payload::Primitive<KEY, ADDRESS, IDENTIFIER, KIND>) -> Result<(),Error>
                where KEY: Clone, ADDRESS: Clone, IDENTIFIER: Clone, KIND: Clone
            {
                match primitive {
                    Primitive::Text(_) => {
                        if *self == Self::Text {
                            Ok(())
                        } else {
                            Err("expected Text primitive".into())
                        }
                    }
                    Primitive::Key(_) => {
                        if *self == Self::Key{
                            Ok(())
                        } else {
                            Err("expected Key primitive".into())
                        }
                    }
                    Primitive::Address(_) => {
                        if *self == Self::Address{
                            Ok(())
                        } else {
                            Err("expected Address primitive".into())
                        }
                    }
                    Primitive::Identifier(_) => {
                        if *self == Self::Identifier{
                            Ok(())
                        } else {
                            Err("expected Identifier primitive".into())
                        }
                    }
                    Primitive::Stub(_) => {
                        if *self == Self::Stub{
                            Ok(())
                        } else {
                            Err("expected Stub primitive".into())
                        }
                    }
                    Primitive::Meta(_) => {
                        if *self == Self::Meta{
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
                        if *self == Self::Boolean{
                            Ok(())
                        } else {
                            Err("expected Boolean primitive".into())
                        }
                    }
                    Primitive::Int(_) => {
                        if *self == Self::Int{
                            Ok(())
                        } else {
                            Err("expected Int primitive".into())
                        }
                    }
                    Primitive::Status(_) => {
                        if *self == Self::Status{
                            Ok(())
                        } else {
                            Err("expected Status primitive".into())
                        }
                    }
                    Primitive::Resource(_) => {
                        if *self == Self::Resource{
                            Ok(())
                        } else {
                            Err("expected Resource primitive".into())
                        }
                    }
                }
            }
        }

        #[derive(Debug,Clone,Eq,PartialEq,Serialize,Deserialize,)]
        pub struct MapPattern<KEY,ADDRESS,IDENTIFIER,KIND> {
            key_phantom: PhantomData<KEY>,
            address_phantom: PhantomData<ADDRESS>,
            identifier_phantom: PhantomData<IDENTIFIER>,
            kind_phantom: PhantomData<KIND>,
            pub required: HashMap<String, ValuePattern<PayloadPattern<KEY,ADDRESS,IDENTIFIER,KIND>>>,
            pub allowed: ValuePattern<PayloadPattern<KEY,ADDRESS,IDENTIFIER,KIND>>
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> Default for MapPattern<KEY,ADDRESS,IDENTIFIER,KIND> {
            fn default() -> Self {
                MapPattern {
                    key_phantom: Default::default(),
                    address_phantom: Default::default(),
                    identifier_phantom: Default::default(),
                    kind_phantom: Default::default(),
                    required: Default::default(),
                    allowed: ValuePattern::Any
                }
            }
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> ToString for MapPattern<KEY,ADDRESS,IDENTIFIER,KIND> {
            fn to_string(&self) -> String {
                "Map?".to_string()
            }
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> MapPattern<KEY,ADDRESS,IDENTIFIER,KIND> {
            pub fn new(required: HashMap<String, ValuePattern<PayloadPattern<KEY,ADDRESS,IDENTIFIER,KIND>>>, allowed: ValuePattern<PayloadPattern<KEY,ADDRESS,IDENTIFIER,KIND>>) -> Self {
                MapPattern {
                    key_phantom: Default::default(),
                    address_phantom: Default::default(),
                    identifier_phantom: Default::default(),
                    kind_phantom: Default::default(),
                    required,
                    allowed
                }
            }
        }



        impl <KEY,ADDRESS,IDENTIFIER,KIND> MapPattern<KEY,ADDRESS,IDENTIFIER,KIND> {
            pub fn empty() -> Self {
                Self {
                    key_phantom: Default::default(),
                    address_phantom: Default::default(),
                    identifier_phantom: Default::default(),
                    kind_phantom: Default::default(),
                    required: HashMap::new(),
                    allowed: ValuePattern::None
                }
            }

            pub fn any() -> Self {
                Self {
                    key_phantom: Default::default(),
                    address_phantom: Default::default(),
                    identifier_phantom: Default::default(),
                    kind_phantom: Default::default(),
                    required: HashMap::new(),
                    allowed: ValuePattern::Any
                }
            }


            pub fn is_match( &self, map:&PayloadMap<KEY,ADDRESS,IDENTIFIER,KIND> ) -> Result<(),Error>
            {

                // if Any keys are allowed then skip
                    for (key, payload) in &map.map {
                        if !self.required.contains_key(key)
                        {
                            match &self.allowed {
                                ValuePattern::Any => { }
                                ValuePattern::None => {
                                    return Err(format!("key: '{}' not required or allowed by Map constraints", key).into());
                                }
                                ValuePattern::Pattern(pattern) => {
                                    pattern.is_match(payload)?;
                                }
                            }
                        }
                    }

                // now make sure all required are present and meet constraints
                for (key,constraint) in &self.required
                {
                    if !map.contains_key(key) {
                        return Err(format!("missing required key : '{}' defined in Map constraints", key).into())
                    }
                    constraint.is_match(&map.get(key).expect("expected map element after testing for it"))?;
                }

                Ok(())
            }
        }
        impl<
            FromKey,
            FromAddress,
            FromIdentifier,
            FromKind,
            ToKey,
            ToAddress,
            ToIdentifier,
            ToKind,
        > ConvertFrom<MapPattern<FromKey, FromAddress, FromIdentifier, FromKind>>
        for MapPattern<ToKey, ToAddress, ToIdentifier, ToKind>
            where
                FromKey: TryInto<ToKey, Error = Error> + Clone,
                FromAddress: TryInto<ToAddress, Error = Error> + Clone,
                FromIdentifier: TryInto<ToIdentifier, Error = Error> + Clone,
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKey: Clone,
                ToAddress: Clone,
                ToIdentifier: Clone,
                ToKind: Clone,
                Call<ToAddress> : TryFrom<Call<FromAddress>,Error=Error>,
                Option<ToAddress> : TryFrom<Option<FromAddress>,Error=Error>,
        {
            fn convert_from(
                a: MapPattern<FromKey, FromAddress, FromIdentifier, FromKind>,
            ) -> Result<Self, Error>
                where
                    Self: Sized,
            {

                    let mut required = HashMap::new();
                    for (k,v) in a.required {
                        required.insert( k, ConvertFrom::convert_from(v)?);
                    }

                    Ok(MapPattern::new(required, ConvertFrom::convert_from(a.allowed)?))
            }
        }


        impl<
            FromKey,
            FromAddress,
            FromIdentifier,
            FromKind,
            ToKey,
            ToAddress,
            ToIdentifier,
            ToKind,
        > ConvertFrom<ValuePattern<PayloadPattern<FromKey,FromAddress,FromIdentifier,FromKind>>>
        for ValuePattern<PayloadPattern<ToKey,ToAddress,ToIdentifier,ToKind>>
            where
                FromKey: TryInto<ToKey, Error = Error> + Clone,
                FromAddress: TryInto<ToAddress, Error = Error> + Clone,
                FromIdentifier: TryInto<ToIdentifier, Error = Error> + Clone,
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKey: Clone,
                ToAddress: Clone,
                ToIdentifier: Clone,
                ToKind: Clone,
                Call<ToAddress> : TryFrom<Call<FromAddress>,Error=Error>,
                Option<ToAddress> : TryFrom<Option<FromAddress>,Error=Error>,
        {
            fn convert_from(
                a: ValuePattern<PayloadPattern<FromKey,FromAddress,FromIdentifier,FromKind>>
            ) -> Result<Self, Error>
                where Self: Sized,
            {


                    Ok(match a {
                        ValuePattern::Any => {
                            ValuePattern::Any
                        }
                        ValuePattern::None => {
                            ValuePattern::None
                        }
                        ValuePattern::Pattern(pattern) => {
                            ValuePattern::Pattern(ConvertFrom::convert_from(pattern)?)
                        }
                    })

            }
        }


        impl<
            FromKey,
            FromAddress,
            FromIdentifier,
            FromKind,
            ToKey,
            ToAddress,
            ToIdentifier,
            ToKind,
        > ConvertFrom<HashMap<String, ValuePattern<PayloadPattern<FromKey,FromAddress,FromIdentifier,FromKind>>>>
        for HashMap<String, ValuePattern<PayloadPattern<ToKey,ToAddress,ToIdentifier,ToKind>>>
            where
                FromKey: TryInto<ToKey, Error = Error> + Clone,
                FromAddress: TryInto<ToAddress, Error = Error> + Clone,
                FromIdentifier: TryInto<ToIdentifier, Error = Error> + Clone,
                FromKind: TryInto<ToKind, Error = Error> + Clone,
                ToKey: Clone,
                ToAddress: Clone,
                ToIdentifier: Clone,
                ToKind: Clone,
                Call<ToAddress> : TryFrom<Call<FromAddress>,Error=Error>,
                Option<ToAddress> : TryFrom<Option<FromAddress>,Error=Error>,
        {
            fn convert_from(
                a: HashMap<String, ValuePattern<PayloadPattern<FromKey,FromAddress,FromIdentifier,FromKind>>>
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

        #[derive(Debug,Clone,Serialize,Deserialize)]
        pub enum PayloadDelivery<PAYLOAD, PAYLOAD_REF> {
            Payload(PAYLOAD),
            Ref(PAYLOAD_REF),
        }

        impl<PAYLOAD, PAYLOAD_REF> Convert<PAYLOAD> for PayloadDelivery<PAYLOAD, PAYLOAD_REF> {
            fn convert(self) -> Result<PAYLOAD, crate::error::Error> {
                match self {
                    PayloadDelivery::Payload(payload) => Ok(payload),
                    PayloadDelivery::Ref(_) => {
                        Err("PayloadDelivery was a reference to a payload".into())
                    }
                }
            }
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

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display )]
        pub enum Payload<KEY, ADDRESS, IDENTIFIER, KIND>
        {
            Empty,
            Primitive(Primitive<KEY, ADDRESS, IDENTIFIER, KIND>),
            List(PrimitiveList<KEY, ADDRESS, IDENTIFIER, KIND>),
            Map(PayloadMap<KEY, ADDRESS, IDENTIFIER, KIND>),
        }

        impl<KEY, ADDRESS, IDENTIFIER, KIND> Payload<KEY, ADDRESS, IDENTIFIER, KIND>
        {
            pub fn payload_type(&self) -> PayloadType {
                match self {
                    Payload::Empty => {
                        PayloadType::Empty
                    }
                    Payload::Primitive(primitive) => {
                       PayloadType::Primitive
                    }
                    Payload::List(list) => {
                        PayloadType::List
                    }
                    Payload::Map(map) => {
                       PayloadType::Map
                    }
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct PayloadMap<KEY,ADDRESS,IDENTIFIER,KIND>
        {
            pub map: HashMap<String,Payload<KEY,ADDRESS,IDENTIFIER,KIND>>
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> PayloadMap<KEY,ADDRESS,IDENTIFIER,KIND>
        {
            /*
            pub fn new(constraints: MapConstraints<KEY,ADDRESS,IDENTIFIER,KIND> ) -> Self {
                Self{
            //        constraints,
                    map: HashMap::new()
                }
            }

             */

            pub fn new() -> Self {
                Self{
                    map: HashMap::new()
                }
            }
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> Deref for PayloadMap<KEY,ADDRESS,IDENTIFIER,KIND>
        {
            type Target = HashMap<String,Payload<KEY,ADDRESS,IDENTIFIER,KIND>>;

            fn deref(&self) -> &Self::Target {
                &self.map
            }
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> DerefMut for PayloadMap<KEY,ADDRESS,IDENTIFIER,KIND>
        {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.map
            }
        }


        impl<
                FromKey,
                FromAddress,
                FromIdentifier,
                FromKind,
                ToKey,
                ToAddress,
                ToIdentifier,
                ToKind,
            > ConvertFrom<Payload<FromKey, FromAddress, FromIdentifier, FromKind>>
            for Payload<ToKey, ToAddress, ToIdentifier, ToKind>
        where
            FromKey: TryInto<ToKey, Error = Error> + Clone,
            FromAddress: TryInto<ToAddress, Error = Error> + Clone,
            FromIdentifier: TryInto<ToIdentifier, Error = Error> + Clone,
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            ToKey: Clone,
            ToAddress: Clone,
            ToIdentifier: Clone,
            ToKind: Clone,
        {
            fn convert_from(
                a: Payload<FromKey, FromAddress, FromIdentifier, FromKind>,
            ) -> Result<Self, Error>
            where
                Self: Sized,
            {
                match a {
                    Payload::Empty => Ok(Payload::Empty),
                    Payload::Primitive(primitive) => Ok(Payload::Primitive(ConvertFrom::convert_from(primitive)?)),
                    Payload::List(list) => {
                        let mut rtn: PrimitiveList<ToKey, ToAddress, ToIdentifier, ToKind> = PrimitiveList::new(list.primitive_type);
                        for p in list.list {
                            rtn.push(ConvertFrom::convert_from(p)?);
                        }
                        Ok(Payload::List(rtn))
                    }
                    Payload::Map(map) => {
                        //let mut rtn: PayloadMap<ToKey,ToAddress,ToIdentifier,ToKind> = PayloadMap::new(ConvertFrom::convert_from(map.constraints )? );
                        let mut rtn: PayloadMap<ToKey,ToAddress,ToIdentifier,ToKind> = PayloadMap::new();

                        for (key, payload) in map.map {
                            rtn.insert(key, ConvertFrom::convert_from(payload)?);
                        }

                        Ok(Payload::Map(rtn))
                    }
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub enum Primitive<KEY, ADDRESS, IDENTIFIER, KIND>
        {
            Text(String),
            Key(KEY),
            Address(ADDRESS),
            Identifier(IDENTIFIER),
            Stub(ResourceStub<KEY, ADDRESS, KIND>),
            Meta(HashMap<String, String>),
            Bin(Bin),
            Boolean(bool),
            Int(i64),
            Status(Status),
            Resource(Resource<KEY, ADDRESS, IDENTIFIER, KIND>),
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> Primitive<KEY,ADDRESS,IDENTIFIER,KIND>
        {

           pub fn primitive_type(&self) -> PrimitiveType  {
               match self {
                   Primitive::Text(_) => {
                       PrimitiveType::Text
                   }
                   Primitive::Key(_) => {
                       PrimitiveType::Key
                   }
                   Primitive::Address(_) => {
                       PrimitiveType::Address
                   }
                   Primitive::Identifier(_) => {
                       PrimitiveType::Identifier
                   }
                   Primitive::Stub(_) => {
                       PrimitiveType::Stub
                   }
                   Primitive::Meta(_) => {
                       PrimitiveType::Meta
                   }
                   Primitive::Bin(_) => {
                       PrimitiveType::Bin
                   }
                   Primitive::Boolean(_) => {
                       PrimitiveType::Boolean
                   }
                   Primitive::Int(_) => {
                       PrimitiveType::Int
                   }
                   Primitive::Status(_) => {
                       PrimitiveType::Status
                   }
                   Primitive::Resource(_) => {
                       PrimitiveType::Resource
                   }
               }
           }

        }

        #[derive(Debug, Clone, Serialize, Deserialize,Eq,PartialEq)]
        pub struct PrimitiveList<KEY,ADDRESS,IDENTIFIER,KIND>
        {
           pub primitive_type: PrimitiveType,
           pub list: Vec<Primitive<KEY,ADDRESS,IDENTIFIER,KIND>>
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND>ToString for PrimitiveList<KEY,ADDRESS,IDENTIFIER,KIND>
            where KEY: Clone, ADDRESS: Clone, IDENTIFIER: Clone, KIND: Clone
        {
            fn to_string(&self) -> String {
                format!("{}[]",self.primitive_type.to_string())
            }
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> PrimitiveList<KEY,ADDRESS,IDENTIFIER,KIND>
            where KEY: Clone, ADDRESS: Clone, IDENTIFIER: Clone, KIND: Clone
        {
            pub fn new(primitive_type: PrimitiveType) -> Self {
                Self {
                    primitive_type,
                    list: vec![]
                }
            }
            pub fn validate( &self ) -> Result<(),Error> {
                for primitive in &self.list {
                    if primitive.primitive_type() != self.primitive_type {
                        return Err(format!("PrimitiveList type mismatch expected: {} received: {}", self.primitive_type.to_string(), primitive.primitive_type().to_string() ).into());
                    }
                }
                Ok(())
            }
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> Deref for PrimitiveList<KEY,ADDRESS,IDENTIFIER,KIND>
            where KEY: Clone, ADDRESS: Clone, IDENTIFIER: Clone, KIND: Clone
        {
            type Target = Vec<Primitive<KEY,ADDRESS,IDENTIFIER,KIND>>;

            fn deref(&self) -> &Self::Target {
                &self.list
            }
        }

        impl <KEY,ADDRESS,IDENTIFIER,KIND> DerefMut for PrimitiveList<KEY,ADDRESS,IDENTIFIER,KIND>
            where KEY: Clone, ADDRESS: Clone, IDENTIFIER: Clone, KIND: Clone
        {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.list
            }
        }


        impl<FromKey,
                FromAddress,
                FromIdentifier,
                FromKind,
                ToKey,
                ToAddress,
                ToIdentifier,
                ToKind,
            > ConvertFrom<Primitive<FromKey, FromAddress, FromIdentifier, FromKind>>
            for Primitive<ToKey, ToAddress, ToIdentifier, ToKind>
        where
            FromKey: TryInto<ToKey, Error = Error> + Clone,
            FromAddress: TryInto<ToAddress, Error = Error> + Clone,
            FromIdentifier: TryInto<ToIdentifier, Error = Error> + Clone,
            FromKind: TryInto<ToKind, Error = Error> + Clone,
            ToKey: Clone,
            ToAddress: Clone,
            ToIdentifier: Clone,
            ToKind: Clone,
        {
            fn convert_from(
                a: Primitive<FromKey, FromAddress, FromIdentifier, FromKind>,
            ) -> Result<Self, Error>
            where
                Self: Sized,
            {
                match a {
                    Primitive::Text(text) => Ok(Primitive::Text(text)),
                    Primitive::Key(key) => Ok(Primitive::Key(key.try_into()?)),
                    Primitive::Address(address) => Ok(Primitive::Address(address.try_into()?)),
                    Primitive::Stub(stub) => Ok(Primitive::Stub(ConvertFrom::convert_from(stub)?)),
                    Primitive::Meta(meta) => Ok(Primitive::Meta(meta)),
                    Primitive::Bin(bin) => Ok(Primitive::Bin(bin)),
                    Primitive::Boolean(boolean) => Ok(Primitive::Boolean(boolean)),
                    Primitive::Int(int) => Ok(Primitive::Int(int)),
                    Primitive::Status(status) => Ok(Primitive::Status(status)),
                    Primitive::Resource(resource) => {
                        Ok(Primitive::Resource(ConvertFrom::convert_from(resource)?))
                    }
                    Primitive::Identifier(id) => Ok(Primitive::Identifier(id.try_into()?)),
                }
            }
        }
    }
}

pub mod util {
    use serde::{Deserialize, Serialize};
    use uuid::Uuid;

    use crate::error::Error;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub enum ValuePattern<T> {
        Any,
        None,
        Pattern(T)
    }

    impl <T> ValuePattern<T> {
        pub fn is_match<X>( &self, x: &X ) -> Result<(),Error>
            where T: ValueMatcher<X>
        {
            match self {
                ValuePattern::Any => {
                    Ok(())
                }
                ValuePattern::Pattern(exact) => {
                    exact.is_match(x)
                }
                ValuePattern::None => {
                    Err("None pattern".into())
                }
            }
        }
    }

    impl <V:ToString> ToString for ValuePattern<V> {
        fn to_string(&self) -> String {
            match self {
                ValuePattern::Any => "*".to_string(),
                ValuePattern::None => "!".to_string(),
                ValuePattern::Pattern(pattern) => {
                    pattern.to_string()
                }
            }
        }
    }


    pub trait ValueMatcher<X> {
        fn is_match(&self, x: &X) -> Result<(),Error>;
    }

    pub struct RegexMatcher {
        pub pattern: String
    }

    impl ToString for RegexMatcher {
        fn to_string(&self) -> String {
            self.pattern.clone()
        }
    }

    impl RegexMatcher {
        pub fn new(string: String) -> Self {
            Self {
                pattern: string
            }
        }
    }

    impl ValueMatcher<String> for RegexMatcher {
        fn is_match(&self, x: &String) -> Result<(), Error> {
            let matches = x.matches(x);
            if matches.count() > 0 {
                Ok(())
            } else {
                Err(format!("could not match pattern '{}' in '{}'",self.pattern, x).into())
            }
        }
    }



    #[derive(Debug,Eq,PartialEq)]
    pub struct StringMatcher {
        pub pattern: String
    }

    impl ToString for StringMatcher {
        fn to_string(&self) -> String {
            self.pattern.clone()
        }
    }

    impl StringMatcher {
        pub fn new(string: String) -> Self {
            Self {
                pattern: string
            }
        }
    }

    impl ValueMatcher<String> for StringMatcher {
        fn is_match(&self, x: &String) -> Result<(), Error> {
            if self.pattern == *x {
                Ok(())
            } else {
                Err(format!("expecting pattern: '{}' found: '{}'", self.pattern, x).into())
            }
        }
    }



    pub trait Convert<A> {
        fn convert(self) -> Result<A, Error>;
    }

    pub trait ConvertFrom<A> where Self:Sized {
        fn convert_from(a: A) -> Result<Self, Error>;
    }

    /*
    impl<A: Convert<B>, B> ConvertFrom<B> for A where A: Sized {
        fn convert_from(a: A) -> Result<Self, Error> {
            a.convert()
        }
    }
     */

    pub fn unique_id() -> String {
        Uuid::new_v4().to_string()
    }
}

pub mod fail {
    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::id::Specific;

    pub mod mesh {
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Error(String),
            QueueOverflow,
        }
    }

    pub mod portal {
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::fail::{http, port, resource};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Error(String),
            Resource(resource::Fail),
            Port(port::Fail),
            Http(http::Error),
        }
    }

    pub mod resource {
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::fail::{Bad, BadRequest, Conditional, Messaging, NotFound};
        use crate::version::v0_0_1::id::Address;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Create(Create),
            Update(Update),
            BadRequest(BadRequest),
            Conditional(Conditional),
            Messaging(Messaging),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Create {
            AddressAlreadyInUse(String),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Update {
            Immutable,
        }
    }

    pub mod port {
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::fail::{BadRequest, Conditional};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Error(String),
            BadRequest(BadRequest),
            Conditional(Conditional),
        }
    }

    pub mod http {
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Error {
            pub code: u32,
            pub message: String,
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum BadRequest {
        NotFound(NotFound),
        Bad(Bad),
        Illegal(Illegal),
        Wrong(Wrong),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Conditional {
        Timeout(Timeout),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Timeout {
        pub waited: i32,
        pub message: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum NotFound {
        ResourceType(String),
        Kind(String),
        Specific(String),
        Address(String),
        Key(String),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Bad {
        ResourceType(String),
        Kind(String),
        Specific(String),
        Address(String),
        Key(String),
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Identifier {
        ResourceType,
        Kind,
        Specific,
        Address,
        Key,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Illegal {
        Immutable,
        EmptyToFieldOnMessage,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Wrong {
        pub received: String,
        pub expected: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Messaging {
        RequestReplyExchangesRequireOneAndOnlyOneRecipient,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Fail {
        Mesh(mesh::Fail),
        Resource(resource::Fail),
        Portal(portal::Fail),
    }
}


pub mod parse {
    use nom::{AsChar, InputTakeAtPosition, IResult};
    use nom::combinator::{all_consuming, recognize};
    use nom::error::{ErrorKind, VerboseError};
    use nom::multi::separated_list1;

    use crate::version::v0_0_1::id::Address;

    pub struct Parser {
    }

    impl Parser {
        pub fn address( input: &str ) -> Res<&str,Address> {
            address(input)
        }

        pub fn consume_address( input: &str ) -> Res<&str,Address> {
            consume_address(input)
        }
    }

    pub type Res<I,O>=IResult<I,O, VerboseError<I>>;

    fn any_resource_path_segment<T>(i: T) -> Res<T, T>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
    {
        i.split_at_position1_complete(
            |item| {
                let char_item = item.as_char();
                !(char_item == '-')
                    && !(char_item == '.')
                    && !(char_item == '/')
                    && !(char_item == '_')
                    && !(char_item.is_alpha() || char_item.is_dec_digit())
            },
            ErrorKind::AlphaNumeric,
        )
    }


    pub fn address(input: &str) -> Res<&str, Address> {
        separated_list1(
            nom::character::complete::char(':'),
            any_resource_path_segment
        )(input).map( |(next,segments)| {
            let segments = segments.iter().map( |s| s.to_string() ).collect();
            let address = Address{ segments };
            (next,address)
        })
    }

    pub fn consume_address(input: &str) -> Res<&str,Address> {
        all_consuming(address)(input)
    }

}
