use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use mesh_portal_versions::version::v0_0_1 as current;


pub type State = current::State;
pub type Port = current::Port;

pub mod artifact {
    use mesh_portal_versions::version::v0_0_1 as current;
    pub type Artifact = current::artifact::Artifact;
    pub type ArtifactRequest = current::artifact::ArtifactRequest;
    pub type ArtifactResponse = current::artifact::ArtifactResponse;
}


pub mod id {
    use mesh_portal_versions::version::v0_0_1 as current;
    pub type ResourceType = current::id::ResourceType;
    pub type ResourceKind = current::id::ResourceKind;
    pub type AddressAndKind = current::id::AddressAndKind;
    pub type AddressAndType = current::id::AddressAndType;
    pub type Meta = current::id::Meta;
    pub type PayloadClaim = current::id::PayloadClaim;
    pub type HostKey = current::id::HostKey;
    pub type Version = current::id::Version;
    pub type Tks = dyn current::id::Tks;
    pub type Specific = current::id::Specific;
    pub type RouteSegment = current::id::RouteSegment;
    pub type AddressSegment = current::id::AddressSegment;
    pub type Address = current::id::Address;
    pub type KindParts = current::id::KindParts;
}

pub mod path {
    use mesh_portal_versions::version::v0_0_1 as current;
    pub type Path=current::path::Path;
}

pub mod pattern {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type TksPattern = current::pattern::TksPattern;
    pub type KindPattern = current::pattern::KindPattern;
    pub type AddressKindPattern = current::pattern::AddressKindPattern;
    pub type VersionReq = current::pattern::VersionReq;
    pub type SegmentPattern = current::pattern::SegmentPattern;
    pub type KeySegment = current::pattern::KeySegment;
    pub type ExactSegment = current::pattern::ExactSegment;
    pub type SpecificPattern = current::pattern::SpecificPattern;
    pub type LabeledPrimitiveTypeDef = current::pattern::LabeledPrimitiveTypeDef;
    pub type PrimitiveTypeDef = current::pattern::PrimitiveTypeDef;
    pub type Format = current::pattern::Format;
    pub type EntityPattern = current::pattern::EntityPattern;
    pub type RcPattern = current::pattern::RcPattern;
    pub type MsgPattern = current::pattern::MsgPattern;
    pub type HttpPattern = current::pattern::HttpPattern;
    pub type Block = current::pattern::Block;
    pub type UploadBlock = current::pattern::UploadBlock;
    pub type CreateBlock = current::pattern::CreateBlock;
    pub type PatternBlock = current::pattern::PatternBlock;
    pub type MapEntryPattern= current::pattern::MapEntryPattern;
    pub type Hop = current::pattern::Hop;
    pub type Pattern<P> = current::pattern::Pattern<P>;
    pub type EmptyPattern<P> = current::pattern::EmptyPattern<P>;
    pub type ResourceTypePattern  = current::pattern::ResourceTypePattern;
    pub type AddressKindPath = current::pattern::AddressKindPath;
    pub type AddressKindSegment = current::pattern::AddressKindSegment;

    pub mod specific {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type VersionReq = current::pattern::specific::VersionReq;
        pub type VendorPattern = current::pattern::specific::VendorPattern;
        pub type ProductPattern = current::pattern::specific::ProductPattern;
        pub type VariantPattern = current::pattern::specific::VariantPattern;
        pub type VersionPattern = current::pattern::specific::VersionPattern;
   }
}

pub mod messaging {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type Request = current::messaging::Request;
    pub type Response= current::messaging::Response;
    pub type ProtoRequest= current::messaging::ProtoRequest;
    pub type Message = current::messaging::Message;
}


pub mod frame {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type PrimitiveFrame = current::frame::PrimitiveFrame;
    pub type CloseReason = current::frame::CloseReason;
}

pub mod bin {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type Bin = current::bin::Bin;
}

pub mod payload {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type Payload = current::payload::Payload;
    pub type PayloadMap = current::payload::PayloadMap;
    pub type Primitive = current::payload::Primitive;
    pub type PrimitiveList = current::payload::PrimitiveList;
    pub type PrimitiveType = current::payload::PrimitiveType;
    pub type PayloadType = current::payload::PayloadType;
    pub type Errors = current::payload::Errors;
    pub type ListPattern = current::payload::ListPattern;
    pub type Range = current::payload::Range;
    pub type PayloadTypePattern = current::payload::PayloadTypePattern;
    pub type PayloadPattern = current::payload::PayloadPattern;
    pub type CallWithConfig = current::payload::CallWithConfig;
    pub type Call = current::payload::Call;
    pub type CallKind = current::payload::CallKind;
    pub type MsgCall = current::payload::MsgCall;
    pub type HttpCall = current::payload::HttpCall;
    pub type HttpMethod = current::payload::HttpMethod;
    pub type PayloadFormat = current::payload::PayloadFormat;
    pub type MapPattern = current::payload::MapPattern;
}

pub mod command {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type Command = current::command::Command;
    pub type CommandStatus = current::command::CommandStatus;
    pub type CommandEvent = current::command::CommandEvent;
    pub type CliId = current::command::CliId;

    pub mod common {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type StateSrc = current::command::common::StateSrc;
        pub type SetLabel = current::command::common::SetLabel;
        pub type SetProperties = current::command::common::SetProperties;
        pub type PropertyMod = current::command::common::PropertyMod;
        pub type SetRegistry = current::command::common::SetRegistry;
    }
}
pub mod msg {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type MsgRequest = current::msg::MsgRequest;
}

pub mod http {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type HttpRequest = current::http::HttpRequest;
    pub type HttpResponse = current::http::HttpResponse;
}

pub mod config {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type PortalKind = current::config::PortalKind;
    pub type Info = current::config::Info;
    pub type PortalConfig = current::config::PortalConfig;
    pub type Assign = current::config::Assign;
    pub type Config<BODY> = current::config::Config<BODY>;
    pub type ConfigBody = current::config::ConfigBody;
    pub type ResourceConfigBody = current::config::ResourceConfigBody;


    pub mod bind {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type ProtoBind = current::config::bind::ProtoBind;
        pub type BindConfig = current::config::bind::BindConfig;
        pub type Scope<T,E>= current::config::bind::Scope<T,E>;
        pub type Pipeline=current::config::bind::Pipeline;
        pub type PipelineStep=current::config::bind::PipelineStep;
        pub type PipelineStop=current::config::bind::PipelineStop;
        pub type PatternBlock =current::config::bind::PatternBlock;
        pub type Selector<P> =current::config::bind::Selector<P>;
        pub type Whitelist =current::config::bind::Whitelist;
        pub type CallPattern =current::config::bind::CallPattern;
        pub type PipelineSegment =current::config::bind::PipelineSegment;
        pub type StepKind =current::config::bind::StepKind;
        pub type PipelineSubScope =current::config::bind::PipelinesSubScope;
        pub type ScopeType =current::config::bind::ScopeType;
    }
}

pub mod entity {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type EntityType = current::entity::EntityType;

    pub mod request {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type Action = current::entity::request::Action;
        pub type RequestCore= current::entity::request::RequestCore;
        pub type Rc = current::entity::request::Rc;
        pub type RcCommandType = current::entity::request::RcCommandType;

        pub mod create {
            use mesh_portal_versions::version::v0_0_1 as current;

            pub type Template = current::entity::request::create::Template;
            pub type KindTemplate = current::entity::request::create::KindTemplate;
            pub type Fulfillment = current::entity::request::create::Fulfillment;
            pub type Create = current::entity::request::create::Create;
            pub type Strategy = current::entity::request::create::Strategy;
            pub type AddressTemplate = current::entity::request::create::AddressTemplate;
            pub type AddressSegmentTemplate = current::entity::request::create::AddressSegmentTemplate;
            pub type CreateOp = current::entity::request::create::CreateOp;
            pub type Require = current::entity::request::create::Require;
            pub type Fulfilemment = current::entity::request::create::Fulfillment;
            pub type Set = current::entity::request::set::Set;
        }

        pub mod select {
            use mesh_portal_versions::version::v0_0_1 as current;

            pub type SelectIntoPayload = current::entity::request::select::SelectIntoPayload;
            pub type Select = current::entity::request::select::Select;
            pub type SelectionKind = current::entity::request::select::SelectKind;
            pub type SubSelector = current::entity::request::select::SubSelect;
            pub type PropertiesPattern = current::entity::request::select::PropertiesPattern;
        }

        pub mod update {
            use mesh_portal_versions::version::v0_0_1 as current;

            pub type Update = current::entity::request::update::Update;
        }

        pub mod query {
            use mesh_portal_versions::version::v0_0_1 as current;

            pub type Query= current::entity::request::query::Query;
            pub type QueryResult = current::entity::request::query::QueryResult;
        }

        pub mod get {
            use mesh_portal_versions::version::v0_0_1 as current;

            pub type Get = current::entity::request::get::Get;
            pub type GetOp = current::entity::request::get::GetOp;
        }

        pub mod set {
            use mesh_portal_versions::version::v0_0_1 as current;

            pub type Set = current::entity::request::set::Set;
        }
    }

    pub mod response {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type ResponseCore = current::entity::response::ResponseCore;
    }
}

pub mod resource {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type StatusUpdate = current::resource::StatusUpdate;
    pub type Status = current::resource::Status;
    pub type Code = current::resource::Code;
    pub type Progress = current::resource::Progress;
    pub type Properties = current::resource::Properties;
    pub type Archetype = current::resource::Archetype;
    pub type ResourceStub = current::resource::ResourceStub;
    pub type Resource = current::resource::Resource;
    pub type Property = current::resource::Property;
}

pub mod portal {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type Exchanger<T> = current::portal::Exchanger<T>;

    pub mod inlet {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type Log = current::portal::inlet::Log;
        pub type Frame = current::portal::inlet::Frame;
        pub type AssignRequest = current::portal::inlet::AssignRequest;
    }

    pub mod outlet {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type Frame = current::portal::outlet::Frame;
    }

    pub mod initin {
        use mesh_portal_versions::version::v0_0_1 as current;
        pub type Frame = current::portal::initin::Frame;
        pub type PortalAuth = current::portal::initin::PortalAuth;
    }

    pub mod initout{
        use mesh_portal_versions::version::v0_0_1 as current;
        pub type Frame = current::portal::initout::Frame;
    }
}


pub mod util {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type ValuePattern<T> = current::util::ValuePattern<T>;
    pub type ValueMatcher<T> = dyn current::util::ValueMatcher<T>;
    pub type RegexMatcher = current::util::RegexMatcher;
    pub type StringMatcher = current::util::StringMatcher;
    pub type Convert<A>= dyn current::util::Convert<A>;
    pub type ConvertFrom<A>= dyn current::util::ConvertFrom<A>;

    pub fn unique_id() -> String {
        current::util::unique_id()
    }
}

pub mod fail {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub mod mesh {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type Fail = current::fail::mesh::Fail;
    }

    pub mod portal {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type Fail = current::fail::portal::Fail;
    }

    pub mod resource {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type Fail = current::fail::resource::Fail;
        pub type Create = current::fail::resource::Create;
        pub type Update = current::fail::resource::Update;
        pub type Select = current::fail::resource::Select;
    }

    pub mod msg {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type Fail = current::fail::msg::Fail;
    }

    pub mod http {
        use mesh_portal_versions::version::v0_0_1 as current;

        pub type Fail = current::fail::http::Error;
    }

    pub type BadRequest = current::fail::BadRequest;
    pub type BadCoercion= current::fail::BadCoercion;
    pub type Conditional = current::fail::Conditional;
    pub type Timeout = current::fail::Timeout;
    pub type NotFound = current::fail::NotFound;
    pub type Bad = current::fail::Bad;
    pub type Identifier = current::fail::Identifier;
    pub type Illegal = current::fail::Illegal;
    pub type Wrong = current::fail::Wrong;
    pub type Messaging= current::fail::Messaging;
    pub type Fail = current::fail::Fail;
}


