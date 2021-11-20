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

    use crate::version::v0_0_1::generic;

    pub type Key = String;
    pub type Address = String;
    pub type ResourceType = String;
    pub type Kind = String;
    pub type Specific = String;
    pub type Version = String;
    pub type Identifier = generic::id::Identifier<Key, Address>;
    pub type Identifiers = generic::id::Identifiers<Key, Address>;
    pub type AddressAndKind = generic::id::AddressAndKind<Address, Kind>;
    pub type AddressAndType = generic::id::AddressAndType<Address, ResourceType>;
    pub type Meta = HashMap<String, String>;
    pub type PayloadPattern = String;
    pub type PayloadClaim = String;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum IdentifierKind {
        Key,
        Address,
    }
}

pub mod messaging {
    use serde::{Deserialize, Serialize};

    pub type ExchangeId = String;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Exchange {
        Notification,
        RequestResponse(ExchangeId),
    }

    impl Exchange {
        pub fn is_singular_recipient(&self) -> bool {
            match self {
                Exchange::Notification => false,
                Exchange::RequestResponse(_) => true,
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
    use crate::version::v0_0_1::error::Error;
    use std::convert::TryInto;

    use serde::{Deserialize, Serialize};

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
    use crate::version::v0_0_1::bin::Bin;
    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{
        Address, Identifier, Key, Kind, PayloadClaim, PayloadPattern,
    };
    use serde::{Deserialize, Serialize};

    pub type Primitive = generic::payload::Primitive<Key, Address, Identifier, Kind>;
    pub type Payload = generic::payload::Payload<Key, Address, Identifier, Kind>;
    pub type PayloadType = generic::payload::PayloadType;
    pub type PayloadRef = generic::payload::PayloadRef<PayloadClaim, PayloadPattern>;
    pub type PayloadDelivery = generic::payload::PayloadDelivery<Payload, PayloadRef>;

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

    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Address, Key, Kind};
    use crate::version::v0_0_1::ArtifactRef;

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
        pub payload_parcel_size: u32,
        pub init_timeout: u64,
        pub frame_timeout: u64,
        pub response_timeout: u64,
        pub bind: BindConfig,
    }

    impl Config {
        pub fn with_bind_config(bind: BindConfig) -> Self {
            Self {
                max_payload_size: 128 * 1024,
                payload_parcel_size: 16 * 1024,
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
                payload_parcel_size: 16 * 1024,
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

    pub mod request {
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{
            Address, Key, Kind, PayloadClaim, PayloadPattern, ResourceType,
        };
        use crate::version::v0_0_1::payload::PayloadDelivery;

        pub type ReqEntity = generic::entity::request::ReqEntity<PayloadDelivery>;
        pub type Rc = generic::entity::request::Rc<PayloadDelivery>;
        pub type Msg = generic::entity::request::Msg<PayloadDelivery>;
        pub type Http = generic::entity::request::Http;
    }

    pub mod response {
        use crate::version::v0_0_1::id::{Address, Key, Kind};
        use crate::version::v0_0_1::payload::PayloadDelivery;
        use crate::version::v0_0_1::{fail, generic};

        pub type RespEntity = generic::entity::response::RespEntity<PayloadDelivery, fail::Fail>;
    }
}

pub mod resource {
    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Address, Identifier, Key, Kind, ResourceType};

    #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
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
        use crate::version::v0_0_1::error::Error;
        use crate::version::v0_0_1::frame::PrimitiveFrame;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Identifier, Key, Kind, ResourceType};
        use crate::version::v0_0_1::payload::Payload;
        use std::convert::TryFrom;

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
        use crate::version::v0_0_1::error::Error;
        use crate::version::v0_0_1::frame::PrimitiveFrame;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Identifier, Key, Kind, ResourceType};
        use crate::version::v0_0_1::payload::Payload;
        use std::convert::TryFrom;

        pub type Request = generic::portal::outlet::Request<Identifier, Kind>;
        pub type Response = generic::portal::outlet::Response<Identifier, Kind>;
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
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::error::Error;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::util::Convert;
        use std::convert::{TryFrom, TryInto};

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
        pub enum Identifier<KEY, ADDRESS> {
            Key(KEY),
            Address(ADDRESS),
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
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::config::{Config, PortalKind};
        use crate::version::v0_0_1::error::Error;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::id::{Identifier, Identifiers};
        use crate::version::v0_0_1::generic::resource::Archetype;
        use crate::version::v0_0_1::ArtifactRef;
        use std::convert::{TryFrom, TryInto};

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
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::__private::fmt::Debug;
            use serde::{Deserialize, Serialize};

            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::error::Error;
            use crate::version::v0_0_1::generic;
            use crate::version::v0_0_1::generic::payload::Payload;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::util::{Convert, ConvertFrom};
            use crate::version::v0_0_1::{http, State};
            use std::convert::{TryFrom, TryInto};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum ReqEntity<PAYLOAD> {
                Rc(Rc<PAYLOAD>),
                Msg(Msg<PAYLOAD>),
                Http(Http),
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
                        ReqEntity::Http(http) => Ok(ReqEntity::Http(http)),
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Rc<PAYLOAD> {
                pub command: String,
                pub payload: PAYLOAD,
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
                pub port: String,
                pub payload: PAYLOAD,
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
                        port: a.port,
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
                        port: self.port,
                        payload: ConvertFrom::convert_from(self.payload)?,
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Http {
                HttpRequest(http::HttpRequest),
            }
        }

        pub mod response {
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::version::latest::error::Error;
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::fail::portal::Fail;
            use crate::version::v0_0_1::generic::payload::Payload;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::util::ConvertFrom;
            use std::convert::{TryFrom, TryInto};

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
                ) -> Result<Self, crate::version::v0_0_1::error::Error>
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
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::error::Error;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::id::{AddressAndKind, Identifier};
        use crate::version::v0_0_1::generic::payload::Payload;
        use crate::version::v0_0_1::generic::payload::Primitive;
        use crate::version::v0_0_1::util::ConvertFrom;
        use crate::version::v0_0_1::State;
        use std::convert::{TryFrom, TryInto};

        #[derive(Debug, Clone, Serialize, Deserialize)]
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

        #[derive(Debug, Clone, Serialize, Deserialize)]
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

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Resource<KEY, ADDRESS, IDENTIFIER, KIND> {
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
            FromKey: TryInto<ToKey, Error = Error>,
            FromAddress: TryInto<ToAddress, Error = Error>,
            FromIdentifier: TryInto<ToIdentifier, Error = Error>,
            FromKind: TryInto<ToKind, Error = Error>,
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

            use crate::version::v0_0_1::command::Command;
            use crate::version::v0_0_1::error::Error;
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
            use crate::version::v0_0_1::util::{unique_id, ConvertFrom};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<IDENTIFIER, PAYLOAD> {
                pub id: String,
                pub to: Vec<IDENTIFIER>,
                pub entity: request::ReqEntity<PAYLOAD>,
            }

            impl<IDENTIFIER, PAYLOAD> From<exchange::Request<IDENTIFIER, PAYLOAD>>
                for Request<IDENTIFIER, PAYLOAD>
            {
                fn from(request: exchange::Request<IDENTIFIER, PAYLOAD>) -> Self {
                    Request {
                        id: request.id,
                        to: request.to,
                        entity: request.entity,
                    }
                }
            }

            impl<IDENTIFIER, PAYLOAD> Request<IDENTIFIER, PAYLOAD> {
                pub fn new(entity: request::ReqEntity<PAYLOAD>) -> Self {
                    Self {
                        id: unique_id(),
                        to: vec![],
                        entity,
                    }
                }
            }

            impl<IDENTIFIER, PAYLOAD> Request<IDENTIFIER, PAYLOAD> {
                pub fn exchange(
                    self,
                    exchange: Exchange,
                ) -> exchange::Request<IDENTIFIER, PAYLOAD> {
                    exchange::Request {
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
                Request(exchange::Request<IDENTIFIER, PAYLOAD>),
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

            pub mod exchange {
                use std::fmt::Debug;
                use std::hash::Hash;
                use std::str::FromStr;

                use serde::{Deserialize, Serialize};

                use crate::version::latest::error::Error;
                use crate::version::v0_0_1::generic::entity::request::ReqEntity;
                use crate::version::v0_0_1::generic::id::Identifier;
                use crate::version::v0_0_1::generic::portal::inlet;
                use crate::version::v0_0_1::messaging::Exchange;
                use crate::version::v0_0_1::util::ConvertFrom;
                use std::convert::{TryFrom, TryInto};

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct Request<IDENTIFIER, PAYLOAD> {
                    pub id: String,
                    pub to: Vec<IDENTIFIER>,
                    pub entity: ReqEntity<PAYLOAD>,
                    pub exchange: Exchange,
                }

                impl<FromIdentifier, FromPayload> Request<FromIdentifier, FromPayload> {
                    pub fn convert<ToIdentifier, ToAddress, ToKind, ToPayload>(
                        self,
                    ) -> Result<Request<ToIdentifier, ToPayload>, Error>
                    where
                        ToIdentifier: TryFrom<FromIdentifier, Error = Error>,
                        ToPayload: TryFrom<FromPayload, Error = Error>,
                    {
                        let mut to_list = vec![];
                        for to in self.to {
                            to_list.push(to.try_into()?)
                        }
                        Ok(Request {
                            id: self.id,
                            to: to_list,
                            entity: ConvertFrom::convert_from(self.entity)?,
                            exchange: self.exchange,
                        })
                    }
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

            use crate::version::v0_0_1::command::CommandEvent;
            use crate::version::v0_0_1::error::Error;
            use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
            use crate::version::v0_0_1::generic::config::Info;
            use crate::version::v0_0_1::generic::entity::request;
            use crate::version::v0_0_1::generic::entity::response;
            use crate::version::v0_0_1::generic::id::Identifier;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::id::{Address, Key, Kind, ResourceType};
            use crate::version::v0_0_1::messaging::{Exchange, ExchangeId};
            use crate::version::v0_0_1::util::ConvertFrom;
            use crate::version::v0_0_1::{fail, generic};

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<IDENTIFIER, PAYLOAD> {
                pub from: IDENTIFIER,
                pub entity: request::ReqEntity<PAYLOAD>,
            }

            impl<IDENTIFIER, PAYLOAD> From<exchange::Request<IDENTIFIER, PAYLOAD>>
                for Request<IDENTIFIER, PAYLOAD>
            {
                fn from(request: exchange::Request<IDENTIFIER, PAYLOAD>) -> Self {
                    Request {
                        from: request.from,
                        entity: request.entity,
                    }
                }
            }

            impl<IDENTIFIER, PAYLOAD> Request<IDENTIFIER, PAYLOAD> {
                pub fn exchange(
                    self,
                    exchange: Exchange,
                ) -> exchange::Request<IDENTIFIER, PAYLOAD> {
                    exchange::Request {
                        from: self.from,
                        entity: self.entity,
                        exchange,
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Response<IDENTIFIER, PAYLOAD> {
                pub id: String,
                pub from: IDENTIFIER,
                pub exchange: ExchangeId,
                pub entity: response::RespEntity<PAYLOAD, fail::Fail>,
            }

            impl<FromIdentifier, FromPayload> Response<FromIdentifier, FromPayload> {
                pub fn convert<ToIdentifier, ToAddress, ToPayload>(
                    self,
                ) -> Result<Response<ToIdentifier, ToPayload>, Error>
                where
                    ToIdentifier: TryFrom<FromIdentifier, Error = Error>,
                    ToPayload: TryFrom<FromPayload, Error = Error>,
                {
                    Ok(Response {
                        id: self.id,
                        from: self.from.try_into()?,
                        entity: ConvertFrom::convert_from(self.entity)?,
                        exchange: self.exchange,
                    })
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
            pub enum Frame<KEY, ADDRESS, IDENTIFIER, KIND, PAYLOAD> {
                Init(Info<KEY, ADDRESS, KIND>),
                CommandEvent(CommandEvent),
                Request(exchange::Request<IDENTIFIER, PAYLOAD>),
                Response(Response<IDENTIFIER, PAYLOAD>),
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

            pub mod exchange {
                use std::fmt::Debug;
                use std::hash::Hash;
                use std::str::FromStr;

                use serde::{Deserialize, Serialize};

                use crate::version::latest::error::Error;
                use crate::version::v0_0_1::generic::entity::request::ReqEntity;
                use crate::version::v0_0_1::generic::id::Identifier;
                use crate::version::v0_0_1::generic::portal::outlet;
                use crate::version::v0_0_1::messaging::Exchange;
                use std::convert::{TryFrom, TryInto};

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct Request<IDENTIFIER, PAYLOAD> {
                    pub from: IDENTIFIER,
                    pub entity: ReqEntity<PAYLOAD>,
                    pub exchange: Exchange,
                }

                /*
                impl<FromKey, FromAddress, FromKind> Request<FromKey, FromAddress, FromKind> {
                    pub fn convert<ToKey, ToAddress, ToKind>(
                        self,
                    ) -> Result<Request<ToKey, ToAddress, ToKind>, Error>
                    where
                        ToKey: TryFrom<FromKey, Error = Error>,
                        ToAddress: TryFrom<FromAddress, Error = Error>,
                        ToKind: TryFrom<FromKind, Error = Error>,
                    {
                        Ok(Request {
                            from: self.from.convert()?,
                            entity: self.entity.convert()?,
                            exchange: self.exchange,
                        })
                    }
                }

                 */
            }
        }
    }

    pub mod payload {
        use std::collections::HashMap;
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::latest::error::Error;
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::resource::{Resource, ResourceStub};
        use crate::version::v0_0_1::payload::PrimitiveType;
        use crate::version::v0_0_1::resource::Status;
        use crate::version::v0_0_1::util::{Convert, ConvertFrom};
        use crate::version::v0_0_1::{http, State};
        use std::convert::{TryFrom, TryInto};

        #[derive(
            Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash, strum_macros::Display,
        )]
        pub enum PayloadType {
            Empty,
            Primitive(PrimitiveType),
            List(PrimitiveType),
            Map,
        }

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

        #[derive(Clone,Serialize,Deserialize)]
        pub enum PayloadDelivery<PAYLOAD, PAYLOAD_REF> {
            Payload(PAYLOAD),
            Ref(PAYLOAD_REF),
        }

        impl<PAYLOAD, PAYLOAD_REF> Convert<PAYLOAD> for PayloadDelivery<PAYLOAD, PAYLOAD_REF> {
            fn convert(self) -> Result<PAYLOAD, crate::version::v0_0_1::error::Error> {
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

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Payload<KEY, ADDRESS, IDENTIFIER, KIND> {
            Empty,
            Single(Primitive<KEY, ADDRESS, IDENTIFIER, KIND>),
            List(Vec<Primitive<KEY, ADDRESS, IDENTIFIER, KIND>>),
            Map(HashMap<String, Box<Payload<KEY, ADDRESS, IDENTIFIER, KIND>>>),
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
            FromKey: TryInto<ToKey, Error = Error>,
            FromAddress: TryInto<ToAddress, Error = Error>,
            FromIdentifier: TryInto<ToIdentifier, Error = Error>,
            FromKind: TryInto<ToKind, Error = Error>,
        {
            fn convert_from(
                a: Payload<FromKey, FromAddress, FromIdentifier, FromKind>,
            ) -> Result<Self, Error>
            where
                Self: Sized,
            {
                match a {
                    Payload::Empty => Ok(Payload::Empty),
                    Payload::Single(primitive) => Ok(Payload::Single(ConvertFrom::convert_from(primitive)?)),
                    Payload::List(list) => {
                        let mut rtn: Vec<Primitive<ToKey, ToAddress, ToIdentifier, ToKind>> =
                            vec![];
                        for p in list {
                            rtn.push(ConvertFrom::convert_from(p)?);
                        }
                        Ok(Payload::List(rtn))
                    }
                    Payload::Map(map) => {
                        let mut rtn = HashMap::new();

                        for (key, payload) in map {
                            rtn.insert(key, Box::new(ConvertFrom::convert_from(*payload)?));
                        }

                        Ok(Payload::Map(rtn))
                    }
                }
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Primitive<KEY, ADDRESS, IDENTIFIER, KIND> {
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
            FromKey: TryInto<ToKey, Error = Error>,
            FromAddress: TryInto<ToAddress, Error = Error>,
            FromIdentifier: TryInto<ToIdentifier, Error = Error>,
            FromKind: TryInto<ToKind, Error = Error>,
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

    use crate::version::v0_0_1::error::Error;
    use uuid::Uuid;

    pub trait Convert<A> {
        fn convert(self) -> Result<A, Error>;
    }

    pub trait ConvertFrom<A> {
        fn convert_from(a: A) -> Result<Self, Error>
        where
            Self: Sized;
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

pub mod error {
    use std::convert::Infallible;
    use std::fmt::{Display, Formatter};
    use std::string::FromUtf8Error;

    #[derive(Debug)]
    pub struct Error {
        pub message: String,
    }

    impl Display for Error {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_str(self.message.as_str())
        }
    }

    impl std::error::Error for Error {}

    impl From<String> for Error {
        fn from(message: String) -> Self {
            Self { message }
        }
    }

    impl From<FromUtf8Error> for Error {
        fn from(message: FromUtf8Error) -> Self {
            Self {
                message: message.to_string(),
            }
        }
    }

    impl From<&str> for Error {
        fn from(message: &str) -> Self {
            Self {
                message: message.to_string(),
            }
        }
    }

    impl From<Box<bincode::ErrorKind>> for Error {
        fn from(message: Box<bincode::ErrorKind>) -> Self {
            Self {
                message: message.to_string(),
            }
        }
    }

    impl From<Infallible> for Error {
        fn from(i: Infallible) -> Self {
            Self {
                message: i.to_string(),
            }
        }
    }
}
