use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::version::v0_0_1::bin::Bin;

pub type State=crate::version::v0_0_1::State;

pub type ArtifactRef=crate::version::v0_0_1::ArtifactRef;
pub type Artifact=crate::version::v0_0_1::Artifact;
pub type Port=crate::version::v0_0_1::Port;

pub mod id {
    use crate::version::v0_0_1::id;
    use crate::version::latest::generic;

    pub type Key = id::Key;
    pub type Address = id::Address;
    pub type ResourceType = id::ResourceType;
    pub type Kind = id::Kind;
    pub type Specific = id::Specific;
    pub type Version = id::Version;
    pub type Identifier = generic::id::Identifier<Key,Address>;
    pub type Identifiers = generic::id::Identifiers<Key,Address>;
    pub type AddressAndKind = generic::id::AddressAndKind<Address,Kind>;
    pub type AddressAndType = generic::id::AddressAndType<Address,ResourceType>;
    pub type Meta=id::Meta;
    pub type IdentityKind = id::IdentifierKind;
}

pub mod messaging {
    use crate::version::v0_0_1::messaging;

    pub type ExchangeId = messaging::ExchangeId;
    pub type Exchange = messaging::Exchange;
}


pub mod log {
    use crate::version::v0_0_1::log;
    pub type Log = log::Log;
}

pub mod frame {
    use crate::version::v0_0_1::frame;
    pub type PrimitiveFrame = frame::PrimitiveFrame;
    pub type CloseReason = frame::CloseReason;
}

pub mod bin {
    use crate::version::v0_0_1::bin;

    pub type BinSrc = bin::BinSrc;
    pub type BinRaw = bin::BinRaw;
    pub type BinSet = bin::BinSet;
    pub type Bin = bin::Bin;
    pub type BinParcel = bin::BinParcel;
}

pub mod payload {
    use crate::version::latest::generic;
    use crate::version::latest::bin::Bin;
    use crate::version::latest::id::{Address, Key, Kind};
    use crate::version::v0_0_1::payload;

    pub type Primitive = generic::payload::Primitive<Key,Address,Kind,Bin>;
    pub type Payload = generic::payload::Payload<Key,Address,Kind,Bin>;
    pub type PayloadType = payload::PayloadType;
    pub type PrimitiveType= payload::PrimitiveType;
}

pub mod command {
    use crate::version::v0_0_1::command;

    pub type Command = command::Command;
    pub type CommandStatus = command::CommandStatus;
    pub type CommandEvent = command::CommandEvent;
}

pub mod http {
    use crate::version::v0_0_1::http;
    use crate::version::latest::Bin;

    pub type HttpRequest = http::HttpRequest;
    pub type HttpResponse = http::HttpResponse;
}


pub mod config {
    use crate::version::latest::generic;
    use crate::version::latest::id::{Address, Key, Kind};
    use crate::version::v0_0_1::config;

    pub type PortalKind = config::PortalKind;
    pub type Info = generic::config::Info<Key,Address,Kind>;
    pub type Config = config::Config;
    pub type SchemaRef = config::SchemaRef;
    pub type BindConfig = config::BindConfig;
    pub type PortConfig = config::PortConfig;
    pub type EntityConfig = config::EntityConfig;
    pub type ResourceConfig = config::ResourceConfig;
    pub type PayloadConfig = config::PayloadConfig;
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

    use crate::version::v0_0_1::resource;
    use crate::version::latest::generic;
    use crate::version::latest::id::{Address, Identifier, Key, Kind, ResourceType};

    pub type Status = resource::Status;

    pub type Archetype= generic::resource::Archetype<Kind>;
    pub type ResourceStub = generic::resource::ResourceStub<Key,Address,Kind>;
}

pub mod portal {

    pub mod inlet {
        use crate::version::latest::generic;
        use crate::version::latest::id::{Address, Key, Kind, ResourceType};
        use crate::version::latest::frame::PrimitiveFrame;
        use crate::version::latest::error::Error;

        pub type Request=generic::portal::inlet::Request<Key,Address,Kind,ResourceType>;
        pub type Response=generic::portal::inlet::Response<Key,Address,Kind>;
        pub type Frame=generic::portal::inlet::Frame<Key,Address,Kind,ResourceType>;

        pub mod exchange {
            use crate::version::latest::id::{Address, Key, Kind, ResourceType};
            use crate::version::latest::generic;
            pub type Request=generic::portal::inlet::exchange::Request<Key,Address,Kind,ResourceType>;
        }
    }

    pub mod outlet {
        use crate::version::latest::generic;
        use crate::version::latest::id::{Address, Key, Kind, ResourceType};
        use crate::version::latest::frame::PrimitiveFrame;
        use crate::version::latest::error::Error;

        pub type Request=generic::portal::outlet::Request<Key,Address,Kind,ResourceType>;
        pub type Response=generic::portal::outlet::Response<Key,Address,Kind>;
        pub type Frame=generic::portal::outlet::Frame<Key,Address,Kind,ResourceType>;

        pub mod exchange {
            use crate::version::latest::id::{Address, Key, Kind, ResourceType};
            use crate::version::latest::generic;
            pub type Request=generic::portal::outlet::exchange::Request<Key,Address,Kind,ResourceType>;
        }
    }
}

pub mod generic {

    pub mod id {
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::generic;

        pub type Identifier<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = generic::id::Identifier<KEY,ADDRESS>;
        pub type Identifiers<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = generic::id::Identifiers<KEY,ADDRESS>;
        pub type AddressAndKind<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = generic::id::AddressAndKind<KEY,ADDRESS>;
        pub type AddressAndType<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = generic::id::AddressAndType<KEY,RESOURCE_TYPE>;
    }

    pub mod config {
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::latest::ArtifactRef;
        use crate::version::latest::config::{Config, PortalKind};
        use crate::version::latest::generic::id::{Identifier, Identifiers};
        use crate::version::latest::generic::resource::Archetype;
        use crate::version::v0_0_1::generic;

        pub type Info<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>=generic::config::Info<KEY,ADDRESS,KIND>;
    }

    pub mod entity {
        pub mod request {
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};
            use serde::__private::fmt::Debug;

            use crate::version::latest::{http, State};
            use crate::version::latest::bin::Bin;
            use crate::version::v0_0_1::generic;
            use crate::version::latest::generic::payload::Primitive;
            use crate::version::latest::generic::payload::Payload;

            pub type ReqEntity<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = generic::entity::request::ReqEntity<KEY,ADDRESS,KIND,RESOURCE_TYPE>;
            pub type Rc<RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = generic::entity::request::Rc<RESOURCE_TYPE>;
            pub type Msg<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = generic::entity::request::Msg<KEY,ADDRESS,KIND>;
            pub type Http = generic::entity::request::Http;
        }

        pub mod response {
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use crate::version::latest::bin::Bin;
            use crate::version::v0_0_1::generic;

            use serde::{Deserialize, Serialize};

            pub type RespEntity<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,FAIL> = generic::entity::response::RespEntity<KEY,ADDRESS,KIND,FAIL>;
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

        pub type Archetype<KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync>=generic::resource::Archetype<KIND>;
        pub type ResourceStub<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync > = generic::resource::ResourceStub<KEY,ADDRESS,KIND>;
        pub type Resource<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,BIN: Debug + Clone + Serialize + Send + Sync > = generic::resource::Resource<KEY,ADDRESS,KIND,BIN>;
    }

    pub mod portal {
        pub mod inlet {
            use std::convert::TryFrom;
            use std::convert::TryInto;
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::version::v0_0_1::generic::portal::inlet;

            pub type Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = inlet::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE>;
            pub type Response<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = inlet::Response<KEY,ADDRESS,KIND>;
            pub type Frame<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = inlet::Frame<KEY,ADDRESS,KIND,RESOURCE_TYPE>;

            pub mod exchange {
                use std::fmt::Debug;
                use std::hash::Hash;
                use std::str::FromStr;

                use serde::{Deserialize, Serialize};
                use crate::version::v0_0_1::generic::portal::inlet::exchange;

                pub type Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = exchange::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE>;
            }
        }

        pub mod outlet {
            use std::convert::TryFrom;
            use std::convert::TryInto;
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::version::v0_0_1::generic::portal::outlet;

            pub type Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> =  outlet::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE>;
            pub type Response<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> =  outlet::Response<KEY,ADDRESS,KIND>;
            pub type Frame<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> =  outlet::Frame<KEY,ADDRESS,KIND,RESOURCE_TYPE>;

            pub mod exchange {
                use std::fmt::Debug;
                use std::hash::Hash;
                use std::str::FromStr;

                use serde::{Deserialize, Serialize};

                use crate::version::v0_0_1::generic::portal::outlet::exchange;

                pub type Request<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync,RESOURCE_TYPE: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync> = exchange::Request<KEY,ADDRESS,KIND,RESOURCE_TYPE>;
            }
        }
    }

    pub mod payload {
        use std::collections::HashMap;
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::generic::payload;

        pub type Payload<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, BIN: Debug + Clone + Serialize + Send + Sync> = payload::Payload<KEY,ADDRESS,KIND,BIN>;
        pub type Primitive<KEY: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, ADDRESS: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, KIND: Debug + Clone + Serialize + Eq + PartialEq + Hash + ToString + FromStr + Send + Sync, BIN: Debug + Clone + Serialize + Send + Sync> = payload::Primitive<KEY,ADDRESS,KIND,BIN>;
    }

}


pub mod fail {
    use serde::{Deserialize, Serialize};

    pub mod mesh {
        pub type Fail=crate::version::v0_0_1::fail::mesh::Fail;
    }

    pub mod portal {
        pub type Fail=crate::version::v0_0_1::fail::portal::Fail;
    }

    pub mod resource {
        pub type Fail=crate::version::v0_0_1::fail::resource::Fail;
        pub type Create=crate::version::v0_0_1::fail::resource::Create;
        pub type Update=crate::version::v0_0_1::fail::resource::Update;
    }

    pub mod port {
        pub type Fail=crate::version::v0_0_1::fail::port::Fail;
    }

    pub mod http {
        pub type Error=crate::version::v0_0_1::fail::http::Error;
    }

    pub type BadRequest=crate::version::v0_0_1::fail::BadRequest;
    pub type Conditional=crate::version::v0_0_1::fail::Conditional;
    pub type Timeout=crate::version::v0_0_1::fail::Timeout;
    pub type NotFound=crate::version::v0_0_1::fail::NotFound;
    pub type Bad=crate::version::v0_0_1::fail::Bad;
    pub type Identifier=crate::version::v0_0_1::fail::Identifier;
    pub type Illegal=crate::version::v0_0_1::fail::Illegal;
    pub type Wrong=crate::version::v0_0_1::fail::Wrong;
    pub type Messaging=crate::version::v0_0_1::fail::Messaging;
    pub type Fail=crate::version::v0_0_1::fail::Fail;
}

pub mod error {
    pub type Error=crate::version::v0_0_1::error::Error;

}




