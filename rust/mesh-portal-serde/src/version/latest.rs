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
    pub type AddressAndKind = generic::id::AddressAndKind<Kind>;
    pub type AddressAndType = generic::id::AddressAndType<ResourceType>;
    pub type AddressSegment= id::AddressSegment;
    pub type Meta=id::Meta;
}

pub mod pattern {
    use crate::version::latest::id;
    pub type Kind= id::Kind;
    pub type ResourceType = id::ResourceType;
    use crate::version::v0_0_1::pattern;
    pub type TksPattern=pattern::TksPattern;
    pub type KindPattern = pattern::KindPattern;
    pub type AddressKindPattern = pattern::AddressKindPattern;
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
    use crate::version::latest::id::{Address, Kind,ResourceType,};
    use crate::version::v0_0_1::payload;
    use crate::version::latest::pattern::TksPattern;

    pub type Primitive = generic::payload::Primitive<Kind>;
    pub type Payload = generic::payload::Payload<Kind>;
    pub type PayloadType = payload::PayloadType;
    pub type PrimitiveType= payload::PrimitiveType;
    pub type PayloadRef = payload::PayloadRef;
    pub type PayloadDelivery = generic::payload::PayloadDelivery<Payload,PayloadRef>;
    pub type Call = payload::Call;
    pub type CallKind = generic::payload::CallKind;
    pub type CallWithConfig = payload::CallWithConfig;
    pub type MapPattern = generic::payload::MapPattern;
    pub type PayloadTypePattern = generic::payload::PayloadTypePattern;
    pub type PayloadPattern = generic::payload::PayloadPattern;
    pub type ListPattern = generic::payload::ListPattern;
    pub type PayloadMap = generic::payload::PayloadMap<Kind>;
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
    pub type Info = generic::config::Info<Kind>;
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
        use crate::version::latest::pattern::TksPattern;

        pub type ReqEntity = generic::entity::request::ReqEntity<ResourceType,Kind>;
        pub type Rc = generic::entity::request::Rc<ResourceType,Kind>;
        pub type Msg = generic::entity::request::Msg<Kind>;
        pub type Http = generic::entity::request::Http<Kind>;
    }

    pub mod response{
        use crate::version::v0_0_1::{fail, generic};
        use crate::version::latest::id::{Address, Kind};
        use crate::version::latest::payload::Payload;

        pub type RespEntity = generic::entity::response::RespEntity<Payload>;
    }

}

pub mod resource {
    use serde::{Deserialize, Serialize};

    use crate::version::v0_0_1::resource;
    use crate::version::latest::generic;
    use crate::version::latest::id::{Address, Kind, ResourceType};

    pub type Status = resource::Status;

    pub type Archetype= generic::resource::Archetype<Kind>;
    pub type ResourceStub = generic::resource::ResourceStub<Kind>;
    pub type Properties = generic::resource::Properties<Kind>;
}

pub mod portal {

    pub mod inlet {
        use crate::version::latest::generic;
        use crate::version::latest::id::{Address, Kind, ResourceType};
        use crate::version::latest::frame::PrimitiveFrame;
        use crate::error::Error;
        use crate::version::latest::payload::Payload;
        use crate::version::latest::pattern::TksPattern;

        pub type ReqEntity=generic::entity::request::ReqEntity<ResourceType,Kind>;
        pub type Request=generic::portal::inlet::Request<ReqEntity>;
        pub type Response=generic::portal::inlet::Response<Payload>;
        pub type Frame=generic::portal::inlet::Frame<ReqEntity,Payload>;

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


        pub type AddressAndKind<KIND> = generic::id::AddressAndKind<KIND>;
        pub type AddressAndType<RESOURCE_TYPE> = generic::id::AddressAndType<RESOURCE_TYPE>;
    }

    pub mod pattern {
        use crate::version::v0_0_1::generic;
        use crate::version::v0_0_1::pattern;
        use crate::version::v0_0_1::util;
        pub type TksPattern<ResourceType, Kind> = generic::pattern::TksPattern<ResourceType, Kind>;
        pub type AddressKindPattern<ResourceType, Kind> = generic::pattern::AddressKindPattern<ResourceType, Kind>;
        pub type AddressTksPath<Kind> = generic::pattern::AddressTksPath<Kind>;
        pub type AddressTksSegment<Kind> = generic::pattern::AddressTksSegment<Kind>;
        pub type KindPattern<Kind> = generic::pattern::KindPattern<Kind>;
        pub type Hop<ResourceType, Kind> = generic::pattern::Hop<ResourceType, Kind>;
        pub type SegmentPattern = pattern::SegmentPattern;
        pub type ExactSegment = pattern::ExactSegment;
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

        pub type Info<KIND>=generic::config::Info<KIND>;
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

            pub type ReqEntity<ResourceType,Kind> = generic::entity::request::ReqEntity<ResourceType,Kind>;
            pub type Rc<ResourceType,Kind> = generic::entity::request::Rc<ResourceType,Kind>;
            pub type Msg<Kind> = generic::entity::request::Msg<Kind>;
            pub type Http<Kind> = generic::entity::request::Http<Kind>;
        }

        pub mod response {
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use crate::version::latest::bin::Bin;
            use crate::version::v0_0_1::generic;

            use serde::{Deserialize, Serialize};

            pub type RespEntity<PAYLOAD> = generic::entity::response::RespEntity<PAYLOAD>;
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

        pub type Archetype<KIND>=generic::resource::Archetype<KIND>;
        pub type ResourceStub<KIND> = generic::resource::ResourceStub<KIND>;
        pub type Properties<KIND> = generic::resource::Properties<KIND>;
        pub type Resource<KIND> = generic::resource::Resource<KIND>;

        pub mod command {
            use crate::version::v0_0_1::generic;
            use crate::version::v0_0_1::util::ValueMatcher;
            use serde::{Deserialize, Serialize};

            pub type RcCommand<ResourceType, Kind> = generic::resource::command::RcCommand<ResourceType,Kind>;
            pub type RcCommandType = generic::resource::command::RcCommandType;

            pub mod common {
                use crate::version::v0_0_1::generic;
                pub type StateSrc<Kind> = generic::resource::command::common::StateSrc<Kind>;
                pub type SetProperties<Kind> = generic::resource::command::common::SetProperties<Kind>;
                pub type SetLabel = generic::resource::command::common::SetLabel;
                pub type SetRegistry = generic::resource::command::common::SetRegistry;
            }

            pub mod create {
                use crate::version::v0_0_1::generic;
                pub type Create<Kind> = generic::resource::command::create::Create<Kind>;
                pub type AddressTemplate  = generic::resource::command::create::AddressTemplate;
                pub type AddressSegmentTemplate = generic::resource::command::create::AddressSegmentTemplate;
                pub type KindTemplate= generic::resource::command::create::KindTemplate;
                pub type Strategy = generic::resource::command::create::Strategy;
                pub type Template = generic::resource::command::create::Template;
            }

            pub mod select {
                use crate::version::v0_0_1::generic;
                pub type SelectIntoPayload=generic::resource::command::select::SelectIntoPayload;
                pub type Select<ResourceType,Kind>=generic::resource::command::select::Select<ResourceType,Kind>;
                pub type SubSelector<ResourceType,Kind>=generic::resource::command::select::SubSelector<ResourceType,Kind>;
                pub type PropertiesPattern = generic::resource::command::select::PropertiesPattern;
            }

            pub mod update {
                use crate::version::v0_0_1::generic;
                pub type Update<Kind>=generic::resource::command::update::Update<Kind>;
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

            use crate::version::v0_0_1::generic::portal::inlet;

            pub type Request<Entity> = inlet::Request<Entity>;
            pub type Response<PAYLOAD> = inlet::Response<PAYLOAD>;
            pub type Frame<ReqEntity,PAYLOAD> = inlet::Frame<ReqEntity,PAYLOAD>;
        }

        pub mod outlet {
            use std::convert::TryFrom;
            use std::convert::TryInto;
            use std::fmt::Debug;
            use std::hash::Hash;
            use std::str::FromStr;

            use serde::{Deserialize, Serialize};

            use crate::version::v0_0_1::generic::portal::outlet;

            pub type Request<ENTITY> =  outlet::Request<ENTITY>;
            pub type Response<PAYLOAD> =  outlet::Response<PAYLOAD>;
            pub type Frame<KIND,PAYLOAD,ReqEntity> =  outlet::Frame<KIND,PAYLOAD,ReqEntity>;
        }
    }

    pub mod payload {
        use std::collections::HashMap;
        use std::fmt::Debug;
        use std::hash::Hash;
        use std::str::FromStr;

        use serde::{Deserialize, Serialize};

        use crate::version::v0_0_1::generic::payload;
        use crate::version::v0_0_1::generic;

        pub type Payload<KIND> = payload::Payload<KIND>;
        pub type PayloadMap<KIND> = payload::PayloadMap<KIND>;
        pub type Primitive<KIND> = payload::Primitive<KIND>;
        pub type PayloadDelivery<PAYLOAD,PAYLOAD_REF> = payload::PayloadDelivery<PAYLOAD,PAYLOAD_REF>;
        pub type Call = payload::Call;
        pub type CallKind = payload::CallKind;
        pub type CallWithConfig = payload::CallWithConfig;
        pub type MapPattern= payload::MapPattern;
        pub type ListPattern = payload::ListPattern;
        pub type PayloadTypePattern= payload::PayloadTypePattern;
        pub type PayloadPattern = payload::PayloadPattern;
        pub type Range= payload::Range;
        pub type RcCommand<ResourceType,Kind> = generic::resource::command::RcCommand<ResourceType,Kind>;
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
        pub type Select=crate::version::v0_0_1::fail::resource::Select;
    }

    pub mod port {
        pub type Fail=crate::version::v0_0_1::fail::msg::Fail;
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




