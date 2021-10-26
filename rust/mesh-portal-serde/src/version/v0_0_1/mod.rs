mod parse;

use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::sync::Arc;

use anyhow::Error;
use serde::{Deserialize, Serialize};
use std::hash::Hash;
use std::fmt::Debug;
use crate::version::v0_0_1::bin::Bin;


pub type State=HashMap<String,Bin>;

pub type ArtifactRef=String;
pub type Artifact=Arc<Vec<u8>>;
pub type Port=String;

pub mod id {
    use crate::version::v0_0_1::generic;
    use serde::{Serialize,Deserialize};
    use std::str::FromStr;
    use anyhow::Error;
    use crate::version::v0_0_1::parse::{parse_address, ParseError, parse_specific, parse_kind, parse_version};
    use std::collections::HashMap;

    pub type Key = String;
    pub type ResourceType = String;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum IdentifierKind {
        Key,
        Address
    }

    pub type Identifier = generic::id::Identifier<Key,Address>;
    pub type Identifiers = generic::id::Identifiers<Key,Address>;
    pub type AddressAndKind = generic::id::AddressAndKind<Address,Kind>;
    pub type AddressAndType = generic::id::AddressAndType<Address,ResourceType>;
    pub type Meta=HashMap<String,String>;


    #[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
    pub struct Kind {
        pub resource_type: String,
        pub kind: Option<String>,
        pub specific: Option<Specific>,
    }

    impl ToString for Kind {
        fn to_string(&self) -> String {
            if self.specific.is_some() && self.kind.is_some() {
                format!(
                    "<{}<{}<{}>>>",
                    self.resource_type,
                    self.kind.as_ref().unwrap().to_string(),
                    self.specific.as_ref().unwrap().to_string()
                )
            } else if self.kind.is_some() {
                format!(
                    "<{}<{}>>",
                    self.resource_type,
                    self.kind.as_ref().unwrap().to_string()
                )
            } else {
                format!("<{}>", self.resource_type)
            }
        }
    }

    impl FromStr for Kind {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (leftover, rtn) = parse_kind(s)?;
            if leftover.len() > 0 {
                return Err(format!(
                    "ResourceKindParts ERROR: could not parse extra: '{}' in string '{}'",
                    leftover, s
                )
                    .into());
            }
            Ok(rtn?)
        }
    }


    #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
    pub struct Address {
        segments: Vec<String>
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
    }

    impl FromStr for Address {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_,segments) = parse_address(s)?;
            Ok(Self{ segments })
        }
    }

    impl ToString for Address {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            for (i, segment) in self.segments.iter().enumerate() {
                rtn.push_str( segment.as_str() );
                if i != self.segments.len() {
                    rtn.push_str(":");
                }
            }
            rtn.to_string()
        }
    }


    #[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
    pub struct Specific {
        pub vendor: String,
        pub product: String,
        pub variant: String,
        pub version: Version,
    }

    impl ToString for Specific {
        fn to_string(&self) -> String {
            format!(
                "{}:{}:{}:{}",
                self.vendor.to_string(),
                self.product,
                self.variant,
                self.version.to_string()
            )
        }
    }

    impl FromStr for Specific {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_, specific) = parse_specific(s)?;
            Ok(specific)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
    pub struct Version {
        major: usize,
        minor: usize,
        patch: usize,
        release: Option<String>,
    }

    impl Version {
        pub fn new(major: usize, minor: usize, patch: usize, release: Option<String>) -> Self {
            Self {
                major,
                minor,
                patch,
                release,
            }
        }
    }


    impl ToString for Version {
        fn to_string(&self) -> String {
            match &self.release {
                None => {
                    format!("{}.{}.{}", self.major, self.minor, self.patch)
                }
                Some(release) => {
                    format!(
                        "{}.{}.{}-{}",
                        self.major,
                        self.minor,
                        self.patch,
                        release.to_string()
                    )
                }
            }
        }
    }

    impl FromStr for Version {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let string = s.to_string();
            let (_, version) = parse_version(s)?;
            Ok(version)
        }
    }

}

pub mod messaging {
    use serde::{Serialize,Deserialize};
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
    use serde::{Serialize,Deserialize};
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
    use serde::{Serialize,Deserialize};
    use anyhow::Error;

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
    use serde::{Serialize,Deserialize};
    use std::sync::Arc;
    use std::collections::HashMap;

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
    use crate::version::v0_0_1::id::{Key, Address, Kind};
    use crate::version::v0_0_1::generic;

    pub type PayloadType = generic::payload::PayloadType;
    pub type Payload = generic::payload::Payload<Key,Address,Kind,Bin>;
    pub type PayloadAspect = generic::payload::PayloadAspect<Key,Address,Kind,Bin>;
}

pub mod command {

    use serde::{Serialize,Deserialize};

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

    use serde::{Serialize,Deserialize};
    use crate::version::v0_0_1::id::{Key, Address, Kind};

    use crate::version::v0_0_1::generic;
    use std::collections::HashMap;
    use crate::version::latest::ArtifactRef;

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
        use crate::version::v0_0_1::id::{Key, Address, Kind,ResourceType};

        pub type ReqEntity = generic::entity::request::ReqEntity<Key,Address,Kind,ResourceType>;
        pub type Rc = generic::entity::request::Rc<Key,Address,Kind,ResourceType>;
        pub type Msg = generic::entity::request::Msg<Key,Address,Kind>;
        pub type Http = generic::entity::request::Http;
    }

    pub mod response{
        use crate::version::v0_0_1::{generic, fail};
        use crate::version::v0_0_1::id::{Key, Address, Kind};

        pub type RespEntity = generic::entity::response::RespEntity<Key,Address,Kind,fail::Fail>;
    }

}

pub mod resource {
    use crate::version::v0_0_1::id::{Key, Address, Kind,ResourceType,Identifier};
    use serde::{Deserialize, Serialize};
    use crate::version::v0_0_1::generic;

    #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
    pub enum Status {
        Unknown,
        Initializing,
        Ready,
        Panic(String),
        Done
    }


    pub type Create=generic::resource::Create<Key,Address,Kind>;

    pub type ResourceStub = generic::resource::ResourceStub<Key,Address,Kind>;
    pub type StateSrc=generic::resource::StateSrc;
    pub type CreateStrategy=generic::resource::CreateStrategy;
    pub type AddressSrc=generic::resource::AddressSrc;
    pub type Selector=generic::resource::Selector<Key,Address,Kind,ResourceType>;
    pub type ConfigSrc = generic::resource::ConfigSrc<Address>;


    pub mod property {
        use crate::version::v0_0_1::id::*;
        use crate::version::v0_0_1::generic::resource::property;
        use crate::version::v0_0_1::bin::BinSet;

        pub type ResourceValueSelector=property::ResourceValueSelector<Key,Address,Kind,ResourceType>;
        pub type BinSetAspectSelector=property::BinSetAspectSelector;
        pub type ResourcePropertyOp<P> = property::ResourcePropertyOp<P,Key,Address>;
        pub type ResourcePropertyValueSelector = property::ResourcePropertyValueSelector<Key,Address,Kind,ResourceType>;
        pub type ResourceHostPropertyValueSelector = property::ResourceHostPropertyValueSelector<Key,Address,Kind,ResourceType>;
        pub type ResourceRegistryPropertyValueSelector =property::ResourceRegistryPropertyValueSelector;
        pub type FieldValueSelector = property::FieldValueSelector<Key,Address,Kind,ResourceType>;
        pub type MetaFieldValueSelector = property::MetaFieldValueSelector<Key,Address,Kind,ResourceType>;
        pub type ResourceValue = property::ResourceValue<Key,Address,Kind,ResourceType>;
        pub type ResourceValues<R> = property::ResourceValues<R,Key,Address,Kind,ResourceType>;
        pub type ResourceRegistryPropertyAssignment = property::ResourceRegistryPropertyAssignment<Key,Address>;
        pub type ResourceProperty = property::ResourceProperty<Address>;
        pub type ResourceRegistryProperty = property::ResourceRegistryProperty<Address>;
        pub type ResourcePropertyAssignment = property::ResourcePropertyAssignment<Key,Address>;
        pub type ResourcePropertiesKind = property::ResourcePropertiesKind;

        impl BinSetAspectSelector {
            pub fn filter( &self, set: BinSet ) -> ResourceValue {
                match self {
                    BinSetAspectSelector::Exact(aspect) => {
                        let mut rtn = BinSet::new();
                        if set.contains_key(aspect) {
                            rtn.insert( aspect.clone(), set.get(aspect).expect(format!("expected aspect: {}", aspect).as_str() ).clone());
                        }
                        ResourceValue::BinSet(rtn)
                    }
                    BinSetAspectSelector::All => {
                        ResourceValue::BinSet(set)
                    }
                }
            }
        }


    }


}

pub mod portal {
    pub mod inlet {
        use crate::version::v0_0_1::id::{Key, Address, Kind,ResourceType};
        use crate::version::v0_0_1::generic;

        pub type Request=generic::portal::inlet::Request<Key,Address,Kind,ResourceType>;
        pub type Response=generic::portal::inlet::Response<Key,Address,Kind>;
        pub type Frame=generic::portal::inlet::Frame<Key,Address,Kind,ResourceType>;
    }

    pub mod outlet {
        use crate::version::v0_0_1::id::{Key, Address, Kind,ResourceType};
        use crate::version::v0_0_1::generic;

        pub type Request=generic::portal::outlet::Request<Key,Address,Kind,ResourceType>;
        pub type Response=generic::portal::outlet::Response<Key,Address,Kind>;
        pub type Frame=generic::portal::outlet::Frame<Key,Address,Kind,ResourceType>;
    }
}

pub mod generic {
    use std::collections::HashMap;
    use std::convert::From;
    use std::convert::TryInto;
    use std::sync::Arc;

    use anyhow::Error;
    use serde::{Deserialize, Serialize};
    use std::hash::Hash;
    use std::fmt::Debug;
    use std::str::FromStr;

    //pub trait Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync {}

    pub mod id {
        use serde::{Serialize,Deserialize};
        use crate::version::v0_0_1::generic;
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

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

        #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
        pub struct AddressAndType<ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
            pub address: ADDRESS,
            pub resource_type: RESOURCE_TYPE,
        }

    }




    pub mod config {
        use serde::{Serialize,Deserialize};
        use crate::version::v0_0_1::config::{PortalKind, Config};
        use crate::version::v0_0_1::ArtifactRef;
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;
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
            use serde::{Serialize, Deserialize};
            use crate::version::v0_0_1::generic;
            use crate::version::latest::{State, http};
            use serde::__private::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;
            use crate::version::v0_0_1::generic::resource::{Create, Selector};
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::generic::payload::Payload;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum ReqEntity<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                Rc(Rc<KEY, ADDRESS, KIND, RESOURCE_TYPE>),
                Msg(Msg<KEY, ADDRESS, KIND>),
                Http(Http)
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum Rc<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                Create(Create<KEY, ADDRESS, KIND>),
                Select(Selector<KEY,ADDRESS,KIND,RESOURCE_TYPE>),
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
            use serde::{Serialize,Deserialize};
            use crate::version::v0_0_1::generic::payload::Payload;
            use crate::version::v0_0_1::bin::Bin;
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum RespEntity<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,FAIL>  {
                Ok(Payload<KEY,ADDRESS,KIND,Bin>),
                Fail(FAIL)
            }
        }
    }


    pub mod resource {
        use serde::{Deserialize, Serialize};
        use crate::version::v0_0_1::State;
        use crate::version::v0_0_1::generic;
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;
        use crate::version::v0_0_1::generic::id::{Identifier, AddressAndKind};
        use std::collections::{HashSet, HashMap};
        use crate::version::latest::fail::http::Error;
        use crate::version::v0_0_1::bin::BinSet;

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Archetype<KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
            pub kind: KIND,
            pub specific: Option<String>,
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


        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Create<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
            pub parent: Identifier<KEY,ADDRESS>,
            pub archetype: Archetype<KIND>,
            pub address: AddressSrc,
            pub strategy: CreateStrategy,
            pub state: StateSrc,
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum StateSrc {
            Stateless,
            State(State),
            CreateArgs(String),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum CreateStrategy {
            Create,
            CreateOrUpdate,
            Ensure,
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum AddressSrc {
            Append(String),
            Pattern(String)
        }

        #[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
        pub enum ResourceExpression<ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
            Path(ADDRESS),
            Kind(AddressAndKind<ADDRESS,KIND>)
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct Selector<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
            pub meta: select::MetaSelector,
            pub fields: HashSet<select::FieldSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>>,
        }

        impl<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> Selector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
            pub fn new() -> Self {
                let fields = HashSet::new();
                Selector {
                    meta: select::MetaSelector::None,
                    fields: fields,
                }
            }

            pub fn resource_types(&self) -> HashSet<RESOURCE_TYPE> {
                let mut rtn = HashSet::new();
                for field in &self.fields {
                    if let select::FieldSelector::Type(resource_type) = field {
                        rtn.insert(resource_type.clone());
                    }
                }
                rtn
            }

            pub fn add(&mut self, field: select::FieldSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>) {
                self.fields.retain(|f| !f.is_matching_kind(&field));
                self.fields.insert(field);
            }

            pub fn is_empty(&self) -> bool {
                if !self.fields.is_empty() {
                    return false;
                }

                match &self.meta {
                    select::MetaSelector::None => {
                        return true;
                    }
                    select::MetaSelector::Name(_) => {
                        return false;
                    }
                    select::MetaSelector::Label(labels) => {
                        return labels.labels.is_empty();
                    }
                };
            }

            pub fn name(&mut self, name: String) -> Result<(), Error> {
                match &mut self.meta {
                    select::MetaSelector::None => {
                        self.meta = select::MetaSelector::Name(name.clone());
                        Ok(())
                    }
                    select::MetaSelector::Name(_) => {
                        self.meta = select::MetaSelector::Name(name.clone());
                        Ok(())
                    }
                    select::MetaSelector::Label(_selector) => {
                        Err("Selector is already set to a LABEL meta selector".into())
                    }
                }
            }

            pub fn add_label(&mut self, label: select::LabelSelection) -> Result<(), Error> {
                match &mut self.meta {
                    select::MetaSelector::None => {
                        self.meta = select::MetaSelector::Label(select::LabelSelector {
                            labels: HashSet::new(),
                        });
                        self.add_label(label)
                    }
                    select::MetaSelector::Name(_) => Err("Selector is already set to a NAME meta selector".into()),
                    select::MetaSelector::Label(selector) => {
                        selector.labels.insert(label);
                        Ok(())
                    }
                }
            }

            pub fn add_field(&mut self, field: select::FieldSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>) {
                self.fields.insert(field);
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
        pub enum ConfigSrc<ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
            None,
            Artifact(ADDRESS)
        }


        pub mod select {
            use std::collections::HashSet;
            use crate::version::v0_0_1::id::Specific;
            use std::fmt::Debug;
            use serde::{Serialize,Deserialize};
            use std::hash::Hash;
            use std::str::FromStr;
            use crate::version::v0_0_1::generic::id::Identifier;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum MetaSelector {
                None,
                Name(String),
                Label(LabelSelector),
            }


            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct LabelSelector {
                pub labels: HashSet<LabelSelection>,
            }



            #[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
            pub enum LabelSelection {
                Exact(Label),
            }

            impl LabelSelection {
                pub fn exact(name: &str, value: &str) -> Self {
                    LabelSelection::Exact(Label {
                        name: name.to_string(),
                        value: value.to_string(),
                    })
                }
            }

            #[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
            pub struct Label {
                pub name: String,
                pub value: String,
            }

            #[derive(Clone, Serialize, Deserialize)]
            pub struct LabelConfig {
                pub name: String,
                pub index: bool,
            }

            #[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
            pub enum FieldSelector<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                Identifier(Identifier<KEY,ADDRESS>),
                Type(RESOURCE_TYPE),
                Kind(KIND),
                Specific(Specific),
                Parent(Identifier<KEY,ADDRESS>),
            }


            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  ToString for FieldSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                fn to_string(&self) -> String {
                    match self {
                        FieldSelector::Identifier(id) => id.to_string(),
                        FieldSelector::Type(rt) => rt.to_string(),
                        FieldSelector::Kind(kind) => kind.to_string(),
                        FieldSelector::Specific(specific) => specific.to_string(),
                        FieldSelector::Parent(parent) => parent.to_string(),
                    }
                }
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> FieldSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                pub fn is_matching_kind(&self, field: &FieldSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>) -> bool {
                    match self {
                        FieldSelector::Identifier(_) => {
                            if let FieldSelector::Identifier(_) = field {
                                return true;
                            }
                        }
                        FieldSelector::Type(_) => {
                            if let FieldSelector::Type(_) = field {
                                return true;
                            }
                        }
                        FieldSelector::Kind(_) => {
                            if let FieldSelector::Kind(_) = field {
                                return true;
                            }
                        }
                        FieldSelector::Specific(_) => {
                            if let FieldSelector::Specific(_) = field {
                                return true;
                            }
                        }
                        FieldSelector::Owner(_) => {
                            if let FieldSelector::Owner(_) = field {
                                return true;
                            }
                        }
                        FieldSelector::Parent(_) => {
                            if let FieldSelector::Parent(_) = field {
                                return true;
                            }
                        }
                    };
                    return false;
                }
            }
        }

        pub mod property {
            use std::str::FromStr;
            use anyhow::Error;
            use crate::version::v0_0_1::bin::Bin;
            use crate::version::v0_0_1::bin::BinSet;
            use crate::version::v0_0_1::resource::Status;
            use std::collections::HashMap;
            use std::convert::TryInto;
            use std::fmt::Debug;
            use serde::{Serialize,Deserialize};
            use std::hash::Hash;
            use crate::version::v0_0_1::generic::resource::{Resource, ConfigSrc};
            use crate::version::v0_0_1::generic::id::Identifier;
            use crate::version::v0_0_1::parse::{parse_resource_property_value_selector, parse_resource_property_assignment, parse_resource_value_selector};
            use crate::version::v0_0_1::id::Meta;
            use std::marker::PhantomData;

            struct Phantoms<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
                key: KEY,
                address: ADDRESS,
                kind: KIND,
                resource_type: RESOURCE_TYPE
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct ResourceValueSelector<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                pub resource: ADDRESS,
                pub property: ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>,
                phantom: PhantomData<Option<Phantoms<KEY,ADDRESS,KIND,RESOURCE_TYPE>>>,
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> ResourceValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                pub fn new( resource: ADDRESS, property: ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> ) -> Self {
                    Self{
                        resource,
                        property,
                        phantom: Default::default()
                    }
                }
            }

            impl<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> FromStr for ResourceValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                type Err = Error;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    let (leftover, selector ) = parse_resource_value_selector(s)?;

                    if !leftover.is_empty() {
                        return Err(format!("could not parse ResourceValueSelector: '{}' trailing portion '{}'", s, leftover).into() );
                    } else {
                        return Ok(selector?);
                    }
                }
            }



            #[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
            pub enum BinSetAspectSelector {
                All,
                Exact(String)
            }


            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct ResourcePropertyOp<P,KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>{
                pub resource: Identifier<KEY,ADDRESS>,
                pub property: P
            }

            #[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, strum_macros::Display)]
            pub enum ResourcePropertyValueSelector<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                Registry(ResourceRegistryPropertyValueSelector),
                Host(ResourceHostPropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>),
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {

                pub fn is_registry(&self) -> bool {
                    match self {
                        ResourcePropertyValueSelector::Registry(_)=> true,
                        _ => false
                    }
                }

                pub fn state() -> Self {
                    Self::Host(ResourceHostPropertyValueSelector::State {
                        aspect: BinSetAspectSelector::All,
                        field: FieldValueSelector::All
                    })
                }

                pub fn state_aspect(aspect: &str) -> Self {
                    Self::Host(ResourceHostPropertyValueSelector::State {
                        aspect: BinSetAspectSelector::Exact(aspect.to_string()),
                        field: FieldValueSelector::All
                    })
                }

                pub fn state_aspect_field(aspect: &str, field: &str) -> Self {
                    Self::Host(ResourceHostPropertyValueSelector::State {
                        aspect: BinSetAspectSelector::Exact(aspect.to_string()),
                        field: FieldValueSelector::Meta(MetaFieldValueSelector::Exact(field.to_string()))
                    })
                }

                pub fn filter( &self, resource: Resource<KEY,ADDRESS,KIND,Bin> ) -> ResourceValue<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                    match self {
                        Self::Host(ResourceHostPropertyValueSelector::State{ aspect, field }) => {
                            field.filter( aspect.filter(resource.state) )
                        }
                        Self::Registry(ResourceRegistryPropertyValueSelector::Config)=> {
                            ResourceValue::Config(resource.archetype.config)
                        }
                        Self::Registry(ResourceRegistryPropertyValueSelector::Status) => {
                            ResourceValue::None
                        }
                    }
                }
            }


            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>TryInto<ResourceRegistryPropertyValueSelector> for ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                type Error = Error;

                fn try_into(self) -> Result<ResourceRegistryPropertyValueSelector, Self::Error> {
                    match self {
                        Self::Registry(registry) => {
                            Ok(registry)
                        }
                        what => {
                            Err(format!("'{}' is not a Registry Resource Property",what.to_string()).into())
                        }
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
            pub enum ResourceHostPropertyValueSelector<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                State{ aspect: BinSetAspectSelector, field: FieldValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> }
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> Into<ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>> for ResourceHostPropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                fn into(self) -> ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                    ResourcePropertyValueSelector::Host(self)
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
            pub enum ResourceRegistryPropertyValueSelector {
                Status,
                Config
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  Into<ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>> for ResourceRegistryPropertyValueSelector {
                fn into(self) -> ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                    ResourcePropertyValueSelector::Registry(self)
                }
            }

            impl<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  FromStr for ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                type Err = Error;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    let (leftover,selector) = parse_resource_property_value_selector(s)?;
                    if !leftover.is_empty() {
                        Err(format!("could not parse entire ResourcePropertyValueSelector: {} because of remaining string: {}", s, leftover ).into())
                    } else {
                        Ok(selector?)
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
            pub enum FieldValueSelector {
                All,
                Meta(MetaFieldValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>)
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> FieldValueSelector {
                pub fn filter( &self, selection: ResourceValue<KEY,ADDRESS,KIND,RESOURCE_TYPE> ) -> ResourceValue<KEY,ADDRESS,KIND,RESOURCE_TYPE> {

                    match self {
                        Self::All => {
                            if let ResourceValue::Meta(meta)  = selection {
                                ResourceValue::Meta(meta)
                            } else {
                                selection
                            }
                        }
                        Self::Meta(selector) => {
                            if let ResourceValue::Meta(meta)  = selection {
                                selector.filter(meta)
                            } else {
                                ResourceValue::None
                            }
                        }
                    }
                }

            }

            #[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
            pub enum MetaFieldValueSelector<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
                All,
                Exact(String)
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> MetaFieldValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                pub fn filter( &self, meta: Meta ) -> ResourceValue<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                    match self {
                        MetaFieldValueSelector::Exact(field) => {
                            if meta.contains_key(field) {
                                let value = meta.get(field).expect(format!("expecting field: {}",field).as_str() );
                                ResourceValue::String(value.clone())
                            } else {
                                ResourceValue::None
                            }
                        }
                        MetaFieldValueSelector::All => {
                            ResourceValue::Meta(meta)
                        }
                    }
                }
            }




            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub enum ResourceValue<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                None,
                BinSet(BinSet),
                BinSrc(Bin),
                String(String),
                Meta(Meta),
                Resource(Resource<KEY,ADDRESS,KIND,RESOURCE_TYPE>),
                Status(Status),
                Config(ConfigSrc<ADDRESS>)
            }

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>ToString for ResourceValue<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                fn to_string(&self) -> String {
                    match self {
                        ResourceValue::None => {
                            "".to_string()
                        }
                        ResourceValue::BinSet(data) => {
                            let mut rtn = String::new();
                            for (k,v) in data {
                                match v {
                                    Bin::Raw(bin) => {
                                        rtn.push_str( String::from_utf8(bin.to_vec()).unwrap_or("UTF ERROR!".to_string() ).as_str() )
                                    }
                                    Bin::Src(src) => {
                                        rtn.push_str( src.as_str() )
                                    }
                                }
                            }
                            rtn
                        }
                        ResourceValue::BinSrc(v) => {
                            match v {
                                Bin::Raw(bin) => {
                                    String::from_utf8(bin.to_vec()).unwrap_or("UTF ERROR!".to_string() )
                                }
                                Bin::Src(src) => {
                                    src.clone()
                                }
                            }
                        }
                        ResourceValue::String(string) => {
                            string.clone()
                        }
                        ResourceValue::Meta(_) => {
                            "Meta printing not supported yet.".to_string()
                        }
                        ResourceValue::Resource(_) => {
                            "Resource string printing not supported yet.".to_string()
                        }
                        ResourceValue::Status(status) => {
                            status.to_string()
                        }
                        ResourceValue::Config(config) => {
                            config.to_string()
                        }
                    }
                }
            }

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct ResourceValues<R,KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                pub resource: R,
                pub values: HashMap<ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>,ResourceValue<KEY,ADDRESS,KIND,RESOURCE_TYPE>>
            }

            impl <R,KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> ResourceValues <R,KEY,ADDRESS,KIND,RESOURCE_TYPE> {

                pub fn empty(resource: R ) -> Self {
                    Self {
                        resource,
                        values: HashMap::new()
                    }
                }

                pub fn new(resource: R, values: HashMap<ResourcePropertyValueSelector<KEY,ADDRESS,KIND,RESOURCE_TYPE>,ResourceValue<KEY,ADDRESS,KIND,RESOURCE_TYPE>>) -> Self {
                    Self {
                        resource,
                        values
                    }
                }

                pub fn with<T>(self, resource: T) -> ResourceValues<T,KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                    ResourceValues{
                        resource,
                        values: self.values
                    }
                }
            }

            #[derive(Debug,Clone, Serialize, Deserialize)]
            pub struct ResourceRegistryPropertyAssignment<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                pub resource: Identifier<KEY,ADDRESS>,
                pub property: ResourceRegistryProperty<ADDRESS>
            }


            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct ResourcePropertyAssignment<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                pub resource: Identifier<KEY,ADDRESS>,
                pub property: ResourceProperty<ADDRESS>
            }

            impl<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> ToString for ResourcePropertyAssignment<KEY,ADDRESS> {
                fn to_string(&self) -> String {
                    return format!( "{}::{}", self.resource.to_string(), self.property.to_string() )
                }
            }

            impl<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> TryInto<ResourceRegistryPropertyAssignment<KEY,ADDRESS>> for ResourcePropertyAssignment<KEY,ADDRESS> {
                type Error = Error;

                fn try_into(self) -> Result<ResourceRegistryPropertyAssignment<KEY,ADDRESS>, Self::Error> {
                    Ok(ResourceRegistryPropertyAssignment {
                        resource: self.resource,
                        property: self.property.try_into()?
                    })
                }
            }

            impl<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> FromStr for ResourcePropertyAssignment<KEY,ADDRESS> {
                type Err = Error;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    let (leftover,result) = parse_resource_property_assignment(s)?;
                    if leftover.len() > 0 {
                        Err(format!("could not parse part of resource property assignment: '{}' unprocessed portion: '{}'", s,leftover ).into())
                    } else {
                        result
                    }
                }
            }


            #[derive(Debug,Clone, Serialize, Deserialize)]
            pub enum ResourceProperty<ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
                Registry(ResourceRegistryProperty<ADDRESS>)
            }

            #[derive(Debug,Clone, Serialize, Deserialize)]
            pub enum ResourceRegistryProperty<ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
                Config(ConfigSrc<ADDRESS>)
            }

            impl <ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  ToString for ResourceProperty<ADDRESS> {
                fn to_string(&self) -> String {
                    match self {
                        ResourceProperty::Registry(ResourceRegistryProperty::Config(_)) => {
                            return "config".to_string()
                        }
                    }
                }
            }


            impl <ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> ResourceProperty<ADDRESS> {
                pub fn is_registry_property(&self) -> bool {
                    match self {
                        ResourceProperty::Registry(_) => {
                            true
                        }
                    }
                }
            }

            impl <ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  TryInto<ResourceRegistryProperty<ADDRESS>> for ResourceProperty<ADDRESS> {
                type Error = Error;

                fn try_into(self) -> Result<ResourceRegistryProperty<ADDRESS>, Self::Error> {
                    match self {
                        ResourceProperty::Registry(property) => {
                            Ok(property)
                        }
                    }
                }
            }

            #[derive(Debug,Clone,Serialize,Deserialize)]
            pub enum ResourcePropertiesKind {
                Registry,
                Host
            }

        }
    }

    pub mod portal {
        pub mod inlet {
            use std::convert::TryFrom;
            use std::convert::TryInto;

            use anyhow::Error;
            use serde::{Deserialize, Serialize};
            use crate::version::v0_0_1::messaging::Exchange;
            use crate::version::v0_0_1::messaging::ExchangeId;
            use crate::version::v0_0_1::log::Log;
            use crate::version::v0_0_1::command::Command;
            use crate::version::v0_0_1::resource::Status;
            use crate::version::v0_0_1::bin::{BinParcel, Bin};
            use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
            use crate::version::v0_0_1::id::{Address, Kind, Key, ResourceType};
            use std::fmt::Debug;
            use crate::version::v0_0_1::generic::id::Identifier;
            use std::hash::Hash;
            use std::str::FromStr;
            use crate::version::v0_0_1::generic::entity::request;
            use crate::version::v0_0_1::generic::entity::response;
            use crate::version::v0_0_1::fail;


            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
                pub to: Vec<Identifier<KEY,ADDRESS>>,
                pub entity: request::ReqEntity<KEY,ADDRESS,KIND,RESOURCE_TYPE>,
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

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> TryInto<PrimitiveFrame> for Frame<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                type Error = Error;

                fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                    let data = bincode::serialize(&self)?;
                    Ok(PrimitiveFrame {
                        data
                    })
                }
            }

            impl TryFrom<PrimitiveFrame> for Frame<Key,Address,Kind,ResourceType>{
                type Error = Error;

                fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                    let frame = bincode::deserialize(value.data.as_slice())?;
                    Ok(frame)
                }
            }

            /*
            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> TryFrom<PrimitiveFrame> for Frame<KEY,ADDRESS,KIND>{
                type Error = Error;

                fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                    let frame = bincode::deserialize(value.data.as_slice())?;
                    Ok(frame)
                }
            }

             */

            pub mod exchange {
                use crate::version::v0_0_1::generic::id::Identifier;
                use crate::version::latest::messaging::Exchange;
                use serde::{Serialize,Deserialize};
                use std::hash::Hash;
                use std::str::FromStr;
                use std::fmt::Debug;
                use crate::version::v0_0_1::generic::entity::request::ReqEntity;
                use crate::version::v0_0_1::generic::portal::inlet;

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>  {
                    pub to: Vec<Identifier<KEY,ADDRESS>>,
                    pub entity: ReqEntity<KEY,ADDRESS,KIND,RESOURCE_TYPE>,
                    pub exchange: Exchange
                }

                impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> Into<inlet::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE>> for Request<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                    fn into(self) -> inlet::Request<KEY, ADDRESS, KIND,RESOURCE_TYPE> {
                        inlet::Request {
                            to: self.to,
                            entity: self.entity
                        }
                    }
                }
            }
        }

        pub mod outlet {
            use std::convert::TryFrom;
            use std::convert::TryInto;

            use anyhow::Error;
            use serde::{Deserialize, Serialize};
            use crate::version::v0_0_1::messaging::{Exchange, ExchangeId};
            use crate::version::v0_0_1::{generic, fail};
            use crate::version::v0_0_1::command::CommandEvent;
            use crate::version::v0_0_1::bin::{BinParcel, Bin};
            use crate::version::v0_0_1::frame::{CloseReason, PrimitiveFrame};
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;
            use crate::version::v0_0_1::generic::id::Identifier;
            use crate::version::v0_0_1::generic::config::Info;
            use crate::version::v0_0_1::id::{Key, Address, Kind, ResourceType};
            use crate::version::v0_0_1::generic::payload::Payload;
            use crate::version::v0_0_1::generic::entity::request;
            use crate::version::v0_0_1::generic::entity::response;

            #[derive(Debug, Clone, Serialize, Deserialize)]
            pub struct Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                pub from: Identifier<KEY,ADDRESS>,
                pub entity: request::ReqEntity<KEY,ADDRESS,KIND,RESOURCE_TYPE>
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

            impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> TryInto<PrimitiveFrame> for Frame<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                type Error = Error;

                fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                    let data = bincode::serialize(&self)?;
                    Ok(PrimitiveFrame {
                        data
                    })
                }
            }


            impl TryFrom<PrimitiveFrame> for Frame<Key,Address,Kind,ResourceType> {
                type Error = Error;

                fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                    let frame = bincode::deserialize(value.data.as_slice())?;
                    Ok(frame)
                }
            }

            pub mod exchange {
                use crate::version::v0_0_1::generic::id::Identifier;
                use crate::version::latest::messaging::Exchange;
                use serde::{Serialize,Deserialize};
                use std::hash::Hash;
                use std::str::FromStr;
                use std::fmt::Debug;
                use crate::version::v0_0_1::generic::entity::request::ReqEntity;
                use crate::version::v0_0_1::generic::portal::outlet;

                #[derive(Debug, Clone, Serialize, Deserialize)]
                pub struct Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> {
                    pub from: Identifier<KEY,ADDRESS>,
                    pub entity: ReqEntity<KEY,ADDRESS,KIND,RESOURCE_TYPE>,
                    pub exchange: Exchange
                }

                impl <KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> Into<outlet::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE>> for Request<KEY,ADDRESS,KIND,RESOURCE_TYPE> {
                    fn into(self) -> outlet::Request<KEY, ADDRESS, KIND,RESOURCE_TYPE> {
                        outlet::Request {
                            from: self.from,
                            entity: self.entity
                        }
                    }
                }
            }
        }
    }

    pub mod payload {
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::{State, http};
        use crate::version::v0_0_1::generic;
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;
        use crate::version::v0_0_1::generic::resource::{ResourceStub, Resource};
        use crate::version::v0_0_1::bin::Bin;
        use std::collections::HashMap;
        use crate::version::latest::resource::Status;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash, strum_macros::Display )]
        pub enum PayloadType
        {
            Empty,
            Text,
            Texts,
            Key,
            Keys,
            Address,
            Addresses,
            Stub,
            Stubs,
            Meta,
            Bin,
            Bins,
            Boolean,
            Code,
            Num,
            Status,
            Resource
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Payload<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, BIN: Debug + Clone + Serialize + Send + Sync>  {
            Empty,
            Text(String),
            Texts(Vec<String>),
            Key(KEY),
            Keys(Vec<KEY>),
            Address(ADDRESS),
            Addresses(Vec<ADDRESS>),
            Stub(ResourceStub<KEY,ADDRESS,KIND>),
            Stubs(Vec<ResourceStub<KEY,ADDRESS,KIND>>),
            Meta(HashMap<String,String>),
            Bin(BIN),
            Bins(HashMap<String, BIN>),
            Mix(HashMap<String,PayloadAspect<KEY,ADDRESS,KIND,BIN>>),
            Boolean(bool),
            Code(i64),
            Num(i64),
            Status(Status),
            Resource(Resource<KEY,ADDRESS,KIND,BIN>)
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum PayloadAspect<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, BIN: Debug + Clone + Serialize + Send + Sync>  {
            Empty,
            Text(String),
            Key(KEY),
            Address(ADDRESS),
            Stub(ResourceStub<KEY,ADDRESS,KIND>),
            Meta(HashMap<String,String>),
            Bin(BIN),
            Boolean(bool),
            Code(i64),
            Num(i64),
            Status(Status)
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
        use crate::version::v0_0_1::fail::{resource, port, http};

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
        use crate::version::v0_0_1::id::Address;
        use crate::version::v0_0_1::fail::{NotFound, Bad, BadRequest,Conditional,Messaging};

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

