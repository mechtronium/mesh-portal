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

    pub type Address = id::Address;
    pub type ResourceType = id::ResourceType;
    pub type Kind = id::Kind;
    pub type Specific = id::Specific;
    pub type Version = id::Version;
    pub type AddressAndKind = generic::id::AddressAndKind<Address,Kind>;
    pub type AddressAndType = generic::id::AddressAndType<Address,ResourceType>;
    pub type Meta=id::Meta;
    pub type IdentifierKind = id::IdentifierKind;
}

pub mod messaging {
    use crate::version::v0_0_1::messaging;

    pub type ExchangeId = messaging::ExchangeId;
    pub type Exchange = messaging::Exchange;
    pub type ExchangeType = messaging::ExchangeType;
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
    pub type Bin = bin::Bin;
}

pub mod payload {
    use crate::version::latest::generic;
    use crate::version::latest::bin::Bin;
    use crate::version::latest::id::{Address, Kind};
    use crate::version::v0_0_1::payload;

    pub type Primitive = generic::payload::Primitive<Address,Kind>;
    pub type Payload = generic::payload::Payload<Address,Kind>;
    pub type PayloadType = payload::PayloadType;
    pub type PrimitiveType= payload::PrimitiveType;
    pub type PayloadRef = payload::PayloadRef;
    pub type PayloadDelivery = generic::payload::PayloadDelivery<Payload,PayloadRef>;
    pub type Call = generic::payload::Call<Address>;
    pub type CallKind = generic::payload::CallKind;
    pub type CallWithConfig = generic::payload::CallWithConfig<Address>;
    pub type MapPattern = generic::payload::MapPattern<Address,Kind>;
    pub type PayloadTypePattern = generic::payload::PayloadListPattern<Address,Kind>;
    pub type PayloadPattern = generic::payload::PayloadPattern<Address,Kind>;
    pub type ListPattern = generic::payload::ListPattern;
    pub type PayloadMap = generic::payload::PayloadMap<Address,Kind>;
    pub type PayloadFormat= generic::payload::PayloadFormat;
    pub type Range = generic::payload::Range;
    pub type RcCommand = payload::RcCommand;

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
    use crate::version::latest::id::{Address, Kind};
    use crate::version::v0_0_1::config;

    pub type PortalKind = config::PortalKind;
    pub type Info = generic::config::Info<Address,Kind>;
    pub type Config = config::Config;
    pub type SchemaRef = config::SchemaRef;
    pub type BindConfig = config::BindConfig;
    pub type PortConfig = config::PortConfig;
    pub type EntityConfig = config::EntityConfig;
    pub type ResourceConfig = config::ResourceConfig;
    pub type PayloadConfig = config::PayloadConfig;
}

pub mod entity {

    use crate::version::v0_0_1::entity;
    pub type EntityType= entity::EntityType;

    pub mod request {
        use crate::version::v0_0_1::generic;
        use crate::version::latest::id::{Address, Kind, ResourceType};
        use crate::version::latest::bin::Bin;
        use crate::version::latest::payload::Payload;

        pub type ReqEntity = generic::entity::request::ReqEntity<Payload>;
        pub type Rc = generic::entity::request::Rc<Payload>;
        pub type Msg = generic::entity::request::Msg<Payload>;
        pub type Http = generic::entity::request::Http<Payload>;
    }

    pub mod response{
        use crate::version::v0_0_1::{fail, generic};
        use crate::version::latest::id::{Address, Kind};
        use crate::version::latest::payload::Payload;

        pub type RespEntity = generic::entity::response::RespEntity<Payload,fail::Fail>;
    }

}

pub mod resource {
    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::resource;
    use crate::version::latest::generic;
    use crate::version::latest::id::{Address, Kind, ResourceType};

    pub type Status = resource::Status;

    pub type Archetype= generic::resource::Archetype<Kind,Address>;
    pub type ResourceStub = generic::resource::ResourceStub<Address,Kind>;
}

pub mod portal {

    pub mod inlet {
        use crate::version::latest::generic;
        use crate::version::latest::id::{Address, Kind, ResourceType};
        use crate::version::latest::frame::PrimitiveFrame;
        use crate::error::Error;
        use crate::version::latest::payload::Payload;

        pub type Request=generic::portal::inlet::Request<Address,Payload>;
        pub type Response=generic::portal::inlet::Response<Address,Payload>;
        pub type Frame=generic::portal::inlet::Frame<Address,Payload>;

        pub mod exchange {
            use crate::version::latest::id::{Address, Kind, ResourceType};
            use crate::version::latest::generic;
            use crate::version::latest::payload::Payload;
        }
    }

    pub mod outlet {
        use crate::version::latest::generic;
        use crate::version::v0_0_1::portal;
        use crate::version::latest::id::{Address, Kind, ResourceType};
        use crate::version::latest::frame::PrimitiveFrame;
        use crate::error::Error;
        use crate::version::latest::payload::Payload;

        pub type Request=portal::outlet::Request;
        pub type Response=portal::outlet::Response;
        pub type Frame=portal::outlet::Frame;
    }
}

pub mod generic {

    pub mod id {
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;
        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::generic;


        pub type AddressAndKind<ADDRESS,KIND> = generic::id::AddressAndKind<ADDRESS,KIND>;
        pub type AddressAndType<ADDRESS,RESOURCE_TYPE> = generic::id::AddressAndType<ADDRESS,RESOURCE_TYPE>;
    }

    pub mod config {
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::latest::ArtifactRef;
        use crate::version::latest::config::{Config, PortalKind};
        use crate::version::latest::generic::resource::Archetype;
        use crate::version::v0_0_1::generic;

        pub type Info<ADDRESS, KIND>=generic::config::Info<ADDRESS,KIND>;
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

            pub type ReqEntity<PAYLOAD> = generic::entity::request::ReqEntity<PAYLOAD>;
            pub type Rc<PAYLOAD> = generic::entity::request::Rc<PAYLOAD>;
            pub type Msg<PAYLOAD> = generic::entity::request::Msg<PAYLOAD>;
            pub type Http<PAYLOAD> = generic::entity::request::Http<PAYLOAD>;
        }

        pub mod response {
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use crate::version::latest::bin::Bin;
            use crate::version::v0_0_1::generic;

            use serde::{Deserialize, Serialize};

            pub type RespEntity<PAYLOAD,FAIL> = generic::entity::response::RespEntity<PAYLOAD,FAIL>;
        }
    }


    pub mod resource {
        use std::collections::{HashMap, HashSet};
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::error::Error;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::id::{AddressAndKind};
        use crate::version::v0_0_1::State;

        pub type Archetype<KIND,ADDRESS>=generic::resource::Archetype<KIND,ADDRESS>;
        pub type ResourceStub<ADDRESS, KIND > = generic::resource::ResourceStub<ADDRESS, KIND>;
        pub type Resource<ADDRESS, KIND> = generic::resource::Resource<ADDRESS,KIND>;
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

            pub type Request<ADDRESS, PAYLOAD> = inlet::Request<ADDRESS,PAYLOAD>;
            pub type Response<ADDRESS, PAYLOAD> = inlet::Response<ADDRESS,PAYLOAD>;
            pub type Frame<ADDRESS, PAYLOAD> = inlet::Frame<ADDRESS,PAYLOAD>;
        }

        pub mod outlet {
            use std::convert::TryFrom;
            use std::convert::TryInto;
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::version::v0_0_1::generic::portal::outlet;

            pub type Request<ADDRESS, PAYLOAD> =  outlet::Request<ADDRESS,PAYLOAD>;
            pub type Response<ADDRESS, PAYLOAD> =  outlet::Response<ADDRESS,PAYLOAD>;
            pub type Frame<ADDRESS, KIND,PAYLOAD> =  outlet::Frame<ADDRESS,KIND,PAYLOAD>;
        }
    }

    pub mod payload {
        use std::collections::HashMap;
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::generic::payload;

        pub type Payload<ADDRESS, KIND> = payload::Payload<ADDRESS,KIND>;
        pub type PayloadMap<ADDRESS, KIND> = payload::PayloadMap<ADDRESS,KIND>;
        pub type Primitive<ADDRESS,KIND> = payload::Primitive<ADDRESS,KIND>;
        pub type PayloadDelivery<PAYLOAD,PAYLOAD_REF> = payload::PayloadDelivery<PAYLOAD,PAYLOAD_REF>;
        pub type Call<ADDRESS> = payload::Call<ADDRESS>;
        pub type CallKind = payload::CallKind;
        pub type CallWithConfig<ADDRESS> = payload::CallWithConfig<ADDRESS>;
        pub type MapPattern<ADDRESS,KIND>= payload::MapPattern<ADDRESS,KIND>;
        pub type ListPattern = payload::ListPattern;
        pub type PayloadListPattern<ADDRESS,KIND>= payload::PayloadTypePattern<ADDRESS,KIND>;
        pub type PayloadPattern<ADDRESS,KIND> = payload::PayloadPattern<ADDRESS,KIND>;
        pub type Range= payload::Range;
        pub type RcCommand = payload::RcCommand;
        pub type PayloadFormat = payload::PayloadFormat;
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

pub mod util {
    use crate::version::v0_0_1::util;

    pub type ValuePattern<V> = util::ValuePattern<V>;
    pub type ValueMatcher<V> = dyn util::ValueMatcher<V>;
    pub type RegexMatcher = util::RegexMatcher;
    pub type StringMatcher= util::StringMatcher;

    pub fn unique_id() -> String {
        util::unique_id()
    }
}

pub mod parse {
    pub type Parser = crate::version::v0_0_1::parse::Parser;
}




