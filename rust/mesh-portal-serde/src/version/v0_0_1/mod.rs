use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::version::v0_0_1::bin::Bin;

pub type State=HashMap<String,Bin>;

pub type ArtifactRef=String;
pub type Artifact=Arc<Vec<u8>>;
pub type Port=String;

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
    pub type Identifier = generic::id::Identifier<Key,Address>;
    pub type Identifiers = generic::id::Identifiers<Key,Address>;
    pub type AddressAndKind = generic::id::AddressAndKind<Address,Kind>;
    pub type AddressAndType = generic::id::AddressAndType<Address,ResourceType>;
    pub type Meta=HashMap<String,String>;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum IdentifierKind {
        Key,
        Address
    }

}

pub mod messaging {
    use serde::{Deserialize, Serialize};

    pub type ExchangeId = String;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Exchange {
        Notification,
        RequestResponse(ExchangeId)
    }


    impl Exchange {
        pub fn is_singular_recipient(&self) -> bool {
            match self {
                Exchange::Notification => false,
                Exchange::RequestResponse(_) => true
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
        Fatal(String)
    }

    impl ToString for Log {
        fn to_string(&self) -> String {
            match self {
                Log::Warn(message) => { format!("WARN: {}", message) }
                Log::Info(message) => { format!("INFO: {}", message) }
                Log::Error(message) => { format!("ERROR: {}", message) }
                Log::Fatal(message) => { format!("FATAL: {}", message) }
            }
        }
    }
}

pub mod frame {
    use std::convert::TryInto;
    use crate::version::v0_0_1::error::Error;

    use serde::{Deserialize, Serialize};

    pub struct PrimitiveFrame {
        pub data: Vec<u8>
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
                data: bytes.to_vec()
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

    pub type BinSrc=String;
    pub type BinRaw=Arc<Vec<u8>>;
    pub type BinSet=HashMap<String,Bin>;


    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Bin {
        Raw(BinRaw),
        Src(BinSrc)
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BinParcel {
        pub src: BinSrc,
        pub index: u32,
        pub raw: BinRaw
    }
}

pub mod payload {
    use crate::version::v0_0_1::bin::Bin;
    use crate::version::v0_0_1::generic;
    use crate::version::v0_0_1::id::{Address, Key, Kind};
    use serde::{Serialize,Deserialize};

    pub type Primitive = generic::payload::Primitive<Key,Address,Kind,Bin>;
    pub type Payload = generic::payload::Payload<Key,Address,Kind,Bin>;

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash, strum_macros::Display )]
    pub enum PayloadType
    {
        Empty,
        Primitive(PrimitiveType),
        List(PrimitiveType),
        Map
    }

    #[derive(Debug,Clone,strum_macros::Display,strum_macros::EnumString,Eq,PartialEq,Hash,Serialize,Deserialize)]
    pub enum PrimitiveType
    {
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
        pub payload: String
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum CommandStatus {
        Running,
        Exit(i32)
    }

    pub type CliId=String;
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CommandEvent {
        pub cli: CliId,
        pub line: Option<String>,
        pub status: CommandStatus
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
        pub body: Option<Bin>
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HttpResponse {
        pub headers: HashMap<String, String>,
        pub code: usize,
        pub body: Option<Bin>
    }

    impl HttpResponse {
        pub fn server_side_error() -> Self {
            Self {
                headers: Default::default(),
                code: 500,
                body: None
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
        Portal
    }

    impl ToString for PortalKind {
        fn to_string(&self) -> String {
            match self {
                PortalKind::Mechtron => "Mechtron".to_string(),
                PortalKind::Portal => "Portal".to_string()
            }
        }
    }

    pub type Info = generic::config::Info<Key,Address,Kind>;


    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Config {
        pub max_bin_size: u32,
        pub bin_parcel_size: u32,
        pub init_timeout: u64,
        pub frame_timeout: u64,
        pub response_timeout: u64,
        pub bind: BindConfig
    }

    impl Config {
        pub fn with_bind_config(bind: BindConfig) -> Self {
            Self {
                max_bin_size: 128 * 1024,
                bin_parcel_size: 16 * 1024,
                init_timeout: 30,
                frame_timeout: 5,
                response_timeout: 15,
                bind
            }
        }
    }

    impl Default for Config {
        fn default() -> Self {
            Self {
                max_bin_size: 128 * 1024,
                bin_parcel_size: 16 * 1024,
                init_timeout: 30,
                frame_timeout: 5,
                response_timeout: 15,
                bind: Default::default()
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SchemaRef {
        pub schema: String,
        pub artifact: Option<ArtifactRef>
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BindConfig {
        pub ports: HashMap<String, PortConfig>,
    }

    impl Default for BindConfig {
        fn default() -> Self {
            Self {
                ports: Default::default()
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PortConfig {
        pub payload: PayloadConfig,
        pub response: EntityConfig
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
        State
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum PayloadConfig {
        Text,
        Bin(SchemaRef),
        Bins(HashMap<String, SchemaRef>)
    }
}

pub mod entity {

    pub mod request {
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Key, Kind, ResourceType};

        pub type ReqEntity = generic::entity::request::ReqEntity<Key,Address,Kind,ResourceType>;
        pub type Rc = generic::entity::request::Rc<ResourceType>;
        pub type Msg = generic::entity::request::Msg<Key,Address,Kind>;
        pub type Http = generic::entity::request::Http;
    }

    pub mod response{
        use crate::version::v0_0_1::{fail, generic};
        use crate::version::v0_0_1::id::{Address, Key, Kind};

        pub type RespEntity = generic::entity::response::RespEntity<Key,Address,Kind,fail::Fail>;
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
        Done
    }

    pub type Archetype= generic::resource::Archetype<Kind>;
    pub type ResourceStub = generic::resource::ResourceStub<Key,Address,Kind>;

}

pub mod portal {
    pub mod inlet {
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Key, Kind, ResourceType};
        use std::convert::TryFrom;
        use crate::version::v0_0_1::frame::PrimitiveFrame;
        use crate::version::v0_0_1::error::Error;

        pub type Request=generic::portal::inlet::Request<Key,Address,Kind,ResourceType>;
        pub type Response=generic::portal::inlet::Response<Key,Address,Kind>;
        pub type Frame=generic::portal::inlet::Frame<Key,Address,Kind,ResourceType>;

        impl TryFrom<PrimitiveFrame> for generic::portal::inlet::Frame<Key,Address,Kind,ResourceType>{
            type Error = Error;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice() )?)
            }
        }
    }

    pub mod outlet {
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::id::{Address, Key, Kind, ResourceType};
        use crate::version::v0_0_1::frame::PrimitiveFrame;
        use std::convert::TryFrom;
        use crate::version::v0_0_1::error::Error;

        pub type Request=generic::portal::outlet::Request<Key,Address,Kind,ResourceType>;
        pub type Response=generic::portal::outlet::Response<Key,Address,Kind>;
        pub type Frame=generic::portal::outlet::Frame<Key,Address,Kind,ResourceType>;

        impl TryFrom<PrimitiveFrame> for generic::portal::outlet::Frame<Key,Address,Kind,ResourceType>{
            type Error = Error;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice() )?)
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

        use crate::version::v0_0_1::generic;
        use std::convert::{TryFrom, TryInto};
        use crate::version::v0_0_1::error::Error;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
        pub enum Identifier<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
            Key(KEY),
            Address(ADDRESS)
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
        pub struct Identifiers<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
            pub key: KEY,
            pub address: ADDRESS
        }

        #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
        pub struct AddressAndKind<ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
            pub address: ADDRESS,
            pub kind: KIND,
        }

        impl <ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  AddressAndKind<ADDRESS,KIND>{
            pub fn new( address: ADDRESS, kind: KIND ) -> Self {
                Self {
                    address,
                    kind
                }
            }
        }

        #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
        pub struct AddressAndType<ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
            pub address: ADDRESS,
            pub resource_type: RESOURCE_TYPE,
        }

    }




    pub mod config {
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::ArtifactRef;
        use crate::version::v0_0_1::config::{Config, PortalKind};
        use crate::version::v0_0_1::generic::id::{Identifier, Identifiers};
        use crate::version::v0_0_1::generic::resource::Archetype;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Info<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
            pub key: KEY,
            pub address: ADDRESS,
            pub owner: String,
            pub parent: Identifier<KEY,ADDRESS>,
            pub archetype: Archetype<KIND>,
            pub config: Config,
            pub ext_config: Option<ArtifactRef>,
            pub kind: PortalKind
        }

        impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> Info<KEY, ADDRESS, KIND> {
            pub fn identity(&self) -> Identifiers<KEY,ADDRESS> {
                Identifiers {
                    key: self.key.clone(),
                    address: self.address.clone()
                }
            }
        }
    }

    pub mod entity {
        pub mod request {
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};
            use serde::__private::fmt::Debug;

            use crate::version::v0_0_1::{http, State};
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::generic;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::generic::payload::Payload;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum ReqEntity<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                Rc(Rc<RESOURCE_TYPE>),
                Msg(Msg<KEY, ADDRESS, KIND>),
                Http(Http)
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Rc<RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                Create(String),
                Select(String),
                Read,
                Update(State),
                Delete,
                Unique(RESOURCE_TYPE)
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Msg<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                pub port: String,
                pub payload: Payload<KEY, ADDRESS, KIND, Bin>
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Http {
                HttpRequest(http::HttpRequest)
            }
        }

        pub mod response {
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::generic::payload::Payload;
            use std::convert::{TryFrom, TryInto};
            use crate::version::latest::error::Error;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum RespEntity<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,FAIL>  {
                Ok(Payload<KEY,ADDRESS,KIND,Bin>),
                Fail(FAIL)
            }
        }
    }


    pub mod resource {
        use std::collections::{HashMap, HashSet};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::bin::BinSet;
        use crate::version::v0_0_1::error::Error;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::id::{AddressAndKind, Identifier};
        use crate::version::v0_0_1::State;
        use std::convert::{TryFrom, TryInto};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Archetype<KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
            pub kind: KIND,
            pub config_src: Option<String>
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct ResourceStub<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync > {
            pub id: Identifier<KEY,ADDRESS>,
            pub key: KEY,
            pub address: ADDRESS,
            pub archetype: Archetype<KIND>
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Resource<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,BIN: Debug + Clone + Serialize + Send + Sync> {
            pub stub: ResourceStub<KEY,ADDRESS,KIND>,
            pub state: HashMap<String,BIN>
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

            use crate::version::v0_0_1::bin::{Bin, BinParcel};
            use crate::version::v0_0_1::command::Command;
            use crate::version::v0_0_1::fail;
            use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
            use crate::version::v0_0_1::generic::entity::request;
            use crate::version::v0_0_1::generic::entity::response;
            use crate::version::v0_0_1::generic::id::Identifier;
            use crate::version::v0_0_1::id::{Address, Key, Kind, ResourceType};
            use crate::version::v0_0_1::log::Log;
            use crate::version::v0_0_1::messaging::Exchange;
            use crate::version::v0_0_1::messaging::ExchangeId;
            use crate::version::v0_0_1::resource::Status;
            use crate::version::v0_0_1::error::Error;
            use crate::version::v0_0_1::generic::entity::request::ReqEntity;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
                pub to: Vec<Identifier<KEY,ADDRESS>>,
                pub entity: request::ReqEntity<KEY,ADDRESS,KIND,RESOURCE_TYPE>,
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> From<exchange::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE>> for Request<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                fn from(request: exchange::Request<KEY, ADDRESS, KIND, RESOURCE_TYPE>) -> Self {
                    Request {
                        to: request.to,
                        entity: request.entity
                    }
                }
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> Request<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                pub fn new(entity: request::ReqEntity<KEY,ADDRESS,KIND,RESOURCE_TYPE>) -> Self {
                    Self {
                        to: vec![],
                        entity,
                    }
                }
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> Request<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                pub fn exchange(self, exchange: Exchange) -> exchange::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                    exchange::Request {
                        to: self.to,
                        entity: self.entity,
                        exchange
                    }
                }
            }


            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Response<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                pub to: Identifier<KEY,ADDRESS>,
                pub exchange: ExchangeId,
                pub entity: response::RespEntity<KEY,ADDRESS,KIND,fail::portal::Fail>,
            }

            #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
            pub enum Frame<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                Log(Log),
                Command(Command),
                Request(exchange::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE>),
                Response(Response<KEY,ADDRESS,KIND>),
                Status(Status),
                BinParcel(BinParcel),
                Close(CloseReason)
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>TryInto<PrimitiveFrame> for Frame<KEY,ADDRESS,KIND,RESOURCE_TYPE>{
                type Error = Error;

                fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                    Ok(PrimitiveFrame {
                        data: bincode::serialize(&self)?
                    })
                }
            }

            pub mod exchange {
                use std::fmt::Debug;
                use std::hash::Hash;
                use std::str::FromStr;

                use serde::{Deserialize, Serialize};

                use crate::version::v0_0_1::messaging::Exchange;
                use crate::version::v0_0_1::generic::entity::request::ReqEntity;
                use crate::version::v0_0_1::generic::id::Identifier;
                use crate::version::v0_0_1::generic::portal::inlet;
                use std::convert::{TryFrom, TryInto};
                use crate::version::latest::error::Error;

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
                    pub to: Vec<Identifier<KEY,ADDRESS>>,
                    pub entity: ReqEntity<KEY,ADDRESS,KIND,RESOURCE_TYPE>,
                    pub exchange: Exchange
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

            use crate::version::v0_0_1::{fail, generic};
            use crate::version::v0_0_1::bin::{Bin, BinParcel};
            use crate::version::v0_0_1::command::CommandEvent;
            use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
            use crate::version::v0_0_1::generic::config::Info;
            use crate::version::v0_0_1::generic::entity::request;
            use crate::version::v0_0_1::generic::entity::response;
            use crate::version::v0_0_1::generic::id::Identifier;
            use crate::version::v0_0_1::generic::payload::Primitive;
            use crate::version::v0_0_1::id::{Address, Key, Kind, ResourceType};
            use crate::version::v0_0_1::messaging::{Exchange, ExchangeId};
            use crate::version::v0_0_1::error::Error;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                pub from: Identifier<KEY,ADDRESS>,
                pub entity: request::ReqEntity<KEY,ADDRESS,KIND,RESOURCE_TYPE>
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> From<exchange::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE>> for Request<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                fn from(request: exchange::Request<KEY, ADDRESS, KIND, RESOURCE_TYPE>) -> Self {
                    Request {
                        from: request.from,
                        entity: request.entity
                    }
                }
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> Request<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                pub fn exchange(self, exchange: Exchange) -> exchange::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                    exchange::Request {
                        from: self.from,
                        entity: self.entity,
                        exchange
                    }
                }



            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Response<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                pub from: Identifier<KEY,ADDRESS>,
                pub exchange: ExchangeId,
                pub entity: response::RespEntity<KEY,ADDRESS,KIND,fail::Fail>,
            }

            #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
            pub enum Frame<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                Init(Info<KEY,ADDRESS,KIND>),
                CommandEvent(CommandEvent),
                Request(exchange::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE>),
                Response(Response<KEY,ADDRESS,KIND>),
                BinParcel(BinParcel),
                Close(CloseReason)
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>TryInto<PrimitiveFrame> for Frame<KEY,ADDRESS,KIND,RESOURCE_TYPE>{
                type Error = Error;

                fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                    Ok(PrimitiveFrame {
                        data: bincode::serialize(&self)?
                    })
                }
            }

            pub mod exchange {
                use std::fmt::Debug;
                use std::hash::Hash;
                use std::str::FromStr;

                use serde::{Deserialize, Serialize};

                use crate::version::v0_0_1::messaging::Exchange;
                use crate::version::v0_0_1::generic::entity::request::ReqEntity;
                use crate::version::v0_0_1::generic::id::Identifier;
                use crate::version::v0_0_1::generic::portal::outlet;
                use std::convert::{TryFrom, TryInto};
                use crate::version::latest::error::Error;

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                    pub from: Identifier<KEY,ADDRESS>,
                    pub entity: ReqEntity<KEY,ADDRESS,KIND,RESOURCE_TYPE>,
                    pub exchange: Exchange
                }
            }
        }
    }

    pub mod payload {
        use std::collections::HashMap;
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::resource::Status;
        use crate::version::v0_0_1::{http, State};
        use crate::version::v0_0_1::bin::Bin;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::resource::{Resource, ResourceStub};
        use std::convert::{TryFrom, TryInto};
        use crate::version::latest::error::Error;


        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Payload<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, BIN: Debug + Clone + Serialize + Send + Sync>  {
            Empty,
            Single(Primitive<KEY,ADDRESS,KIND,BIN>),
            List(Vec<Primitive<KEY,ADDRESS,KIND,BIN>>),
            Map(HashMap<String,Box<Payload<KEY,ADDRESS,KIND,BIN>>>)
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Primitive<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, BIN: Debug + Clone + Serialize + Send + Sync>  {
            Text(String),
            Key(KEY),
            Address(ADDRESS),
            Stub(ResourceStub<KEY,ADDRESS,KIND>),
            Meta(HashMap<String,String>),
            Bin(BIN),
            Boolean(bool),
            Int(i64),
            Status(Status),
            Resource(Resource<KEY,ADDRESS,KIND,BIN>)
        }
    }

}

pub mod fail {
    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::id::Specific;

    pub mod mesh {
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail{
            Error(String),
            QueueOverflow
        }
    }

    pub mod portal {
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::fail::{http, port, resource};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail{
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
            Messaging(Messaging)
    }
        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Create{
            AddressAlreadyInUse(String),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Update{
            Immutable
        }
    }




    pub mod port {
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::fail::{BadRequest, Conditional};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fail {
            Error(String),
            BadRequest(BadRequest),
            Conditional(Conditional)
        }
    }

    pub mod http {
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Error{
            pub code: u32,
            pub message: String
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
        pub message: String
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
    pub enum Illegal{
        Immutable,
        EmptyToFieldOnMessage
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Wrong {
        pub received: String,
        pub expected: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Messaging {
        RequestReplyExchangesRequireOneAndOnlyOneRecipient
    }


    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Fail {
        Mesh(mesh::Fail),
        Resource(resource::Fail),
        Portal(portal::Fail),
    }
}

pub mod error {
    use std::string::FromUtf8Error;
    use std::fmt::{Display, Formatter};

    #[derive(Debug)]
    pub struct Error {
        pub message: String
    }

    impl Display for Error {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_str(self.message.as_str())
        }
    }

    impl std::error::Error for Error {

    }

    impl From<String> for Error {
        fn from(message: String) -> Self {
            Self {
                message
            }
        }
    }


    impl From<FromUtf8Error> for Error {
        fn from(message: FromUtf8Error) -> Self {
            Self {
                message: message.to_string()
            }
        }
    }

    impl From<&str> for Error {
        fn from(message: &str) -> Self {
            Self {
                message: message.to_string()
            }
        }
    }

    impl From<Box<bincode::ErrorKind>> for Error {
        fn from(message: Box<bincode::ErrorKind>) -> Self {
            Self {
                message: message.to_string()
            }
        }
    }



}




