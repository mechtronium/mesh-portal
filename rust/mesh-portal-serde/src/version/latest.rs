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
    pub type IdentifierKind = id::IdentifierKind;
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
    pub type Bin = bin::Bin;
}

pub mod payload {
    use crate::version::latest::generic;
    use crate::version::latest::bin::Bin;
    use crate::version::latest::id::{Address, Key, Kind};
    use crate::version::v0_0_1::payload;
    use crate::version::latest::id::Identifier;

    pub type Primitive = generic::payload::Primitive<Key,Address,Identifier,Kind>;
    pub type Payload = generic::payload::Payload<Key,Address,Identifier,Kind>;
    pub type PayloadType = payload::PayloadType;
    pub type PrimitiveType= payload::PrimitiveType;
    pub type PayloadRef = payload::PayloadRef;
    pub type PayloadDelivery = generic::payload::PayloadDelivery<Payload,PayloadRef>;
    pub type Call = generic::payload::Call<Address>;
    pub type CallKind = generic::payload::CallKind;
    pub type CallWithConfig = generic::payload::CallWithConfig<Address>;
    pub type MapPattern = generic::payload::MapPattern<Key,Address,Identifier,Kind>;
    pub type PayloadTypePattern = generic::payload::PayloadListPattern<Key,Address,Identifier,Kind>;
    pub type PayloadPattern = generic::payload::PayloadPattern<Key,Address,Identifier,Kind>;
    pub type ListPattern = generic::payload::ListPattern;
    pub type PayloadMap = generic::payload::PayloadMap<Key,Address,Identifier,Kind>;
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

    use crate::version::v0_0_1::entity;
    pub type EntityType= entity::EntityType;

    pub mod request {
        use crate::version::v0_0_1::generic;
        use crate::version::latest::id::{Address, Key, Kind, ResourceType};
        use crate::version::latest::bin::Bin;
        use crate::version::latest::payload::PayloadDelivery;

        pub type ReqEntity = generic::entity::request::ReqEntity<PayloadDelivery>;
        pub type Rc = generic::entity::request::Rc<PayloadDelivery>;
        pub type Msg = generic::entity::request::Msg<PayloadDelivery>;
        pub type Http = generic::entity::request::Http<PayloadDelivery>;
    }

    pub mod response{
        use crate::version::v0_0_1::{fail, generic};
        use crate::version::latest::id::{Address, Key, Kind};
        use crate::version::latest::payload::PayloadDelivery;

        pub type RespEntity = generic::entity::response::RespEntity<PayloadDelivery,fail::Fail>;
    }

}

pub mod resource {
    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::resource;
    use crate::version::latest::generic;
    use crate::version::latest::id::{Address, Identifier, Key, Kind, ResourceType};

    pub type Status = resource::Status;

    pub type Archetype= generic::resource::Archetype<Kind,Address>;
    pub type ResourceStub = generic::resource::ResourceStub<Key,Address,Kind>;
}

pub mod portal {

    pub mod inlet {
        use crate::version::latest::generic;
        use crate::version::latest::id::{Address, Key, Kind, ResourceType,Identifier};
        use crate::version::latest::frame::PrimitiveFrame;
        use crate::version::latest::error::Error;
        use crate::version::latest::payload::PayloadDelivery;

        pub type Request=generic::portal::inlet::Request<Identifier,PayloadDelivery>;
        pub type Response=generic::portal::inlet::Response<Identifier,PayloadDelivery>;
        pub type Frame=generic::portal::inlet::Frame<Identifier,PayloadDelivery>;

        pub mod exchange {
            use crate::version::latest::id::{Address, Key, Kind, ResourceType, Identifier};
            use crate::version::latest::generic;
            use crate::version::latest::payload::PayloadDelivery;
            pub type Request=generic::portal::inlet::exchange::Request<Identifier,PayloadDelivery>;
        }
    }

    pub mod outlet {
        use crate::version::latest::generic;
        use crate::version::latest::id::{Address, Key, Kind, ResourceType, Identifier};
        use crate::version::latest::frame::PrimitiveFrame;
        use crate::version::latest::error::Error;
        use crate::version::latest::payload::PayloadDelivery;

        pub type Request=generic::portal::outlet::Request<Identifier,PayloadDelivery>;
        pub type Response=generic::portal::outlet::Response<Identifier,PayloadDelivery>;
        pub type Frame=generic::portal::outlet::Frame<Key,Address,Identifier,Kind,ResourceType>;

        pub mod exchange {
            use crate::version::latest::id::{Address, Key, Kind, ResourceType,Identifier};
            use crate::version::latest::generic;
            use crate::version::latest::payload::PayloadDelivery;

            pub type Request=generic::portal::outlet::exchange::Request<Identifier,PayloadDelivery>;
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

        pub type Identifier<KEY, ADDRESS> = generic::id::Identifier<KEY,ADDRESS>;
        pub type Identifiers<KEY, ADDRESS> = generic::id::Identifiers<KEY,ADDRESS>;
        pub type AddressAndKind<KEY, ADDRESS> = generic::id::AddressAndKind<KEY,ADDRESS>;
        pub type AddressAndType<KEY, RESOURCE_TYPE> = generic::id::AddressAndType<KEY,RESOURCE_TYPE>;
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

        pub type Info<KEY, ADDRESS, KIND>=generic::config::Info<KEY,ADDRESS,KIND>;
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

        use crate::version::v0_0_1::error::Error;
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::generic::id::{AddressAndKind, Identifier};
        use crate::version::v0_0_1::State;

        pub type Archetype<KIND,ADDRESS>=generic::resource::Archetype<KIND,ADDRESS>;
        pub type ResourceStub<KEY, ADDRESS, KIND > = generic::resource::ResourceStub<KEY,ADDRESS, KIND>;
        pub type Resource<KEY, ADDRESS, IDENTIFIER, KIND> = generic::resource::Resource<KEY,ADDRESS,IDENTIFIER,KIND>;
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

            pub type Request<IDENTIFIER, PAYLOAD> = inlet::Request<IDENTIFIER,PAYLOAD>;
            pub type Response<IDENTIFIER, PAYLOAD> = inlet::Response<IDENTIFIER,PAYLOAD>;
            pub type Frame<IDENTIFIER, PAYLOAD> = inlet::Frame<IDENTIFIER,PAYLOAD>;

            pub mod exchange {
                use std::fmt::Debug;
                use std::hash::Hash;
                use std::str::FromStr;

                use serde::{Deserialize, Serialize};
                use crate::version::v0_0_1::generic::portal::inlet::exchange;

                pub type Request<IDENTIFIER, PAYLOAD> = exchange::Request<IDENTIFIER,PAYLOAD>;
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

            pub type Request<IDENTIFIER, PAYLOAD> =  outlet::Request<IDENTIFIER,PAYLOAD>;
            pub type Response<IDENTIFIER, PAYLOAD> =  outlet::Response<IDENTIFIER,PAYLOAD>;
            pub type Frame<KEY, ADDRESS, IDENTIFIER, KIND,PAYLOAD> =  outlet::Frame<KEY,ADDRESS,IDENTIFIER,KIND,PAYLOAD>;

            pub mod exchange {
                use std::fmt::Debug;
                use std::hash::Hash;
                use std::str::FromStr;

                use serde::{Deserialize, Serialize};

                use crate::version::v0_0_1::generic::portal::outlet::exchange;

                pub type Request<IDENTIFIER, PAYLOAD> = exchange::Request<IDENTIFIER,PAYLOAD>;
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

        pub type Payload<KEY, ADDRESS, IDENTIFIER,KIND> = payload::Payload<KEY,ADDRESS,IDENTIFIER,KIND>;
        pub type PayloadMap<KEY, ADDRESS, IDENTIFIER,KIND> = payload::PayloadMap<KEY,ADDRESS,IDENTIFIER,KIND>;
        pub type Primitive<KEY, ADDRESS, IDENTIFIER,KIND> = payload::Primitive<KEY,ADDRESS,IDENTIFIER,KIND>;
        pub type PayloadDelivery<PAYLOAD,PAYLOAD_REF> = payload::PayloadDelivery<PAYLOAD,PAYLOAD_REF>;
        pub type Call<ADDRESS> = payload::Call<ADDRESS>;
        pub type CallKind = payload::CallKind;
        pub type CallWithConfig<ADDRESS> = payload::CallWithConfig<ADDRESS>;
        pub type MapPattern<KEY, ADDRESS, IDENTIFIER,KIND>= payload::MapPattern<KEY,ADDRESS,IDENTIFIER,KIND>;
        pub type ListPattern = payload::ListPattern;
        pub type PayloadListPattern<KEY, ADDRESS, IDENTIFIER,KIND>= payload::PayloadTypePattern<KEY, ADDRESS, IDENTIFIER,KIND>;
        pub type PayloadPattern<KEY, ADDRESS, IDENTIFIER,KIND> = payload::PayloadPattern<KEY, ADDRESS, IDENTIFIER,KIND>;
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
}

pub mod error {
    pub type Error=crate::version::v0_0_1::error::Error;

}





