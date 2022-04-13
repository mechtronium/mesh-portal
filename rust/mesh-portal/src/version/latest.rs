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
    use mesh_portal_versions::version::v0_0_1::id;

    pub type ResourceType = id::id::GenericKindBase;
    pub type ResourceKind = id::id::GenericKind;
    pub type AddressAndKind = id::id::PointKind;
    pub type AddressAndType = id::id::AddressAndType;
    pub type Meta = id::id::Meta;
    pub type PayloadClaim = id::id::PayloadClaim;
    pub type HostKey = id::id::HostKey;
    pub type Version = id::id::Version;
    pub type Tks = dyn id::id::Tks;
    pub type Specific = id::id::Specific;
    pub type RouteSegment = id::id::RouteSeg;
    pub type AddressSegment = id::id::PointSeg;
    pub type Point = id::id::Point;
    pub type KindParts = id::id::GenericKind;
}

pub mod path {
    use mesh_portal_versions::version::v0_0_1 as current;
    pub type Path=current::path::Path;
}

pub mod selector {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::selector;

    pub type TksPattern = selector::selector::KindSelector;
    pub type KindSelector = selector::selector::SubKindSelector;
    pub type PointSelector = selector::selector::PointSelector;
    pub type VersionReq = selector::selector::VersionReq;
    pub type PointSegSelector = selector::selector::PointSegSelector;
    pub type KeySegment = selector::selector::KeySegment;
    pub type ExactSegment = selector::selector::ExactPointSeg;
    pub type SpecificPattern = selector::selector::SpecificSelector;
    pub type LabeledPrimitiveTypeDef = selector::selector::LabeledPrimitiveTypeDef;
    pub type PrimitiveTypeDef = selector::selector::PayloadTypeDef;
    pub type Format = selector::selector::Format;
    pub type EntityPattern = selector::selector::EntityPattern;
    pub type RcPattern = selector::selector::RcPattern;
    pub type MsgPattern = selector::selector::MsgPattern;
    pub type HttpPattern = selector::selector::HttpPattern;
    pub type Block = selector::selector::PayloadBlock;
    pub type UploadBlock = selector::selector::UploadBlock;
    pub type CreateBlock = selector::selector::CreateBlock;
    pub type PatternBlock = selector::selector::PatternBlock;
    pub type MapEntryPattern= selector::selector::MapEntryPattern;
    pub type Hop = selector::selector::SelectorHop;
    pub type Pattern<P> = selector::selector::Pattern<P>;
    pub type EmptyPattern<P> = selector::selector::EmptyPattern<P>;
    pub type BaseKindSelector = selector::selector::KindBaseSelector;
    pub type PointKindHierarchy = selector::selector::PointKindHierarchy;
    pub type PointKindSeg = selector::selector::PointKindSeg;

    pub mod specific {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::selector;

        pub type VersionReq = selector::selector::specific::VersionReq;
        pub type VendorSelector = selector::selector::specific::VendorSelector;
        pub type ProductSelector = selector::selector::specific::ProductSelector;
        pub type VariantSelector = selector::selector::specific::VariantSelector;
        pub type VersionPattern = selector::selector::specific::VersionPattern;
   }
}

pub mod messaging {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::messaging;

    pub type Request = messaging::messaging::Request;
    pub type Response= messaging::messaging::Response;
    pub type RequestBuilder = messaging::messaging::RequestBuilder;
    pub type ProtoRequest= messaging::messaging::ProtoRequest;
    pub type Message = messaging::messaging::Message;
    pub type Agent = messaging::messaging::Agent;
    pub type AuthedAgent = messaging::messaging::AuthedAgent;
    pub type Session = messaging::messaging::Session;
    pub type Scope = messaging::messaging::Scope;
    pub type Priority = messaging::messaging::Priority;
    pub type Karma = messaging::messaging::Karma;
    pub type Handling = messaging::messaging::Handling;
    pub type HandlingKind = messaging::messaging::HandlingKind;
}


pub mod frame {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::frame;

    pub type PrimitiveFrame = frame::frame::PrimitiveFrame;
    pub type CloseReason = frame::frame::CloseReason;
}

pub mod bin {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type Bin = current::bin::Bin;
}

pub mod parse {
    use mesh_portal_versions::version::v0_0_1 as current;
    pub type Res<I,O> = current::parse::Res<I,O>;
}

pub mod payload {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::payload;

    pub type Payload = payload::payload::Payload;
    pub type PayloadMap = payload::payload::PayloadMap;
    pub type Primitive = payload::payload::Primitive;
    pub type PrimitiveList = payload::payload::PayloadList;
    pub type PrimitiveType = payload::payload::PrimitiveType;
    pub type PayloadType = payload::payload::PayloadType;
    pub type Errors = payload::payload::Errors;
    pub type ListPattern = payload::payload::ListPattern;
    pub type Range = payload::payload::Range;
    pub type PayloadTypePattern = payload::payload::PayloadTypePattern;
    pub type PayloadPattern = payload::payload::PayloadPattern;
    pub type CallWithConfig = payload::payload::CallWithConfig;
    pub type Call = payload::payload::Call;
    pub type CallKind = payload::payload::CallKind;
    pub type MsgCall = payload::payload::MsgCall;
    pub type HttpCall = payload::payload::HttpCall;
    pub type HttpMethod = payload::payload::HttpMethod;
    pub type PayloadFormat = payload::payload::PayloadFormat;
    pub type MapPattern = payload::payload::MapPattern;
}

pub mod command {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::command;

    pub type Command = command::command::Command;
    pub type CommandStatus = command::command::CommandStatus;
    pub type CommandEvent = command::command::CommandEvent;
    pub type CliId = command::command::CliId;

    pub mod common {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::command;

        pub type StateSrc = command::command::common::StateSrc;
        pub type SetLabel = command::command::common::SetLabel;
        pub type SetProperties = command::command::common::SetProperties;
        pub type PropertyMod = command::command::common::PropertyMod;
        pub type SetRegistry = command::command::common::SetRegistry;
    }
}

pub mod security {
    use mesh_portal_versions::version::v0_0_1 as current;

    pub type Access = current::security::Access;
    pub type Privileges = current::security::EnumeratedPrivileges;
    pub type EnumeratedAccess = current::security::EnumeratedAccess;
    pub type Permissions = current::security::Permissions;
    pub type PermissionsMask = current::security::PermissionsMask;
    pub type PermissionsMaskKind = current::security::PermissionsMaskKind;
    pub type ChildPerms = current::security::ChildPerms;
    pub type ParticlePerms = current::security::ParticlePerms;
    pub type AccessGrant = current::security::AccessGrant;
    pub type AccessGrantKind = current::security::AccessGrantKindDef;
}


pub mod msg {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::msg;

    pub type MsgRequest = msg::msg::MsgRequest;
}

pub mod config {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::config;

    pub type PortalKind = config::config::PortalKind;
    pub type Info = config::config::Info;
    pub type PortalConfig = config::config::PortalConfig;
    pub type Assign = config::config::Assign;
    pub type Config<BODY> = config::config::Config<BODY>;
    pub type ConfigBody = config::config::ConfigBody;
    pub type ResourceConfigBody = config::config::ResourceConfigBody;


    pub mod bind {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::config;

        pub type ProtoBind = config::config::bind::ProtoBind;
        pub type BindConfig = config::config::bind::BindConfig;
        pub type ConfigScope<T,E>= config::config::bind::ConfigScope<T,E>;
        pub type Pipeline= config::config::bind::Pipeline;
        pub type PipelineStep= config::config::bind::PipelineStep;
        pub type PipelineStop= config::config::bind::PipelineStop;
        pub type PatternBlock = config::config::bind::PatternBlock;
        pub type Selector<P> = config::config::bind::Selector<P>;
        pub type Whitelist = config::config::bind::Whitelist;
        pub type CallPattern = config::config::bind::CallPattern;
        pub type PipelineSegment = config::config::bind::PipelineSegment;
        pub type StepKind = config::config::bind::StepKind;
        pub type PipelineSubScope = config::config::bind::PipelinesSubScope;
        pub type ScopeType = config::config::bind::ScopeType;
    }
}

pub mod entity {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::entity;

    pub type EntityType = entity::entity::EntityType;

    pub mod request {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::entity;

        pub type Action = entity::entity::request::Action;
        pub type RequestCore= entity::entity::request::RequestCore;
        pub type Rc = entity::entity::request::Rc;
        pub type RcCommandType = entity::entity::request::RcCommandType;

        pub mod create {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::entity;

            pub type Create= entity::entity::request::create::Create;
            pub type Template = entity::entity::request::create::Template;
            pub type KindTemplate = entity::entity::request::create::KindTemplate;
            pub type Fulfillment = entity::entity::request::create::Fulfillment;
            pub type Strategy = entity::entity::request::create::Strategy;
            pub type AddressTemplate = entity::entity::request::create::PointTemplate;
            pub type AddressSegmentTemplate = entity::entity::request::create::PointSegFactory;
            pub type CreateOp = entity::entity::request::create::CreateOp;
            pub type Require = entity::entity::request::create::Require;
            pub type Fulfilemment = entity::entity::request::create::Fulfillment;
            pub type Set = entity::entity::request::set::Set;
        }

        pub mod select {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::entity;

            pub type SelectIntoPayload = entity::entity::request::select::SelectIntoPayload;
            pub type Select = entity::entity::request::select::Select;
            pub type SelectionKind = entity::entity::request::select::SelectKind;
            pub type SubSelector = entity::entity::request::select::SubSelect;
            pub type PropertiesPattern = entity::entity::request::select::PropertiesPattern;
        }

        pub mod update {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::entity;

            pub type Update = entity::entity::request::update::Update;
        }

        pub mod query {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::entity;

            pub type Query= entity::entity::request::query::Query;
            pub type QueryResult = entity::entity::request::query::QueryResult;
        }

        pub mod get {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::entity;

            pub type Get = entity::entity::request::get::Get;
            pub type GetOp = entity::entity::request::get::GetOp;
        }

        pub mod set {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::entity;

            pub type Set = entity::entity::request::set::Set;
        }
    }

    pub mod response {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::entity;

        pub type ResponseCore = entity::entity::response::ResponseCore;
    }
}

pub mod resource {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::particle;

    pub type StatusUpdate = particle::particle::StatusUpdate;
    pub type Status = particle::particle::Status;
    pub type Code = particle::particle::Code;
    pub type Progress = particle::particle::Progress;
    pub type Properties = particle::particle::Properties;
    pub type Archetype = particle::particle::Archetype;
    pub type Stub = particle::particle::Stub;
    pub type Resource = particle::particle::Particle;
    pub type Property = particle::particle::Property;
}

pub mod portal {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::portal;

    pub type Exchanger<T> = portal::portal::Exchanger<T>;

    pub mod inlet {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::portal;

        pub type Log = portal::portal::inlet::Log;
        pub type Frame = portal::portal::inlet::Frame;
        pub type AssignRequest = portal::portal::inlet::AssignRequest;
    }

    pub mod outlet {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::portal;

        pub type Frame = portal::portal::outlet::Frame;
    }

    pub mod initin {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::portal;

        pub type Frame = portal::portal::initin::Frame;
        pub type PortalAuth = portal::portal::initin::PortalAuth;
    }

    pub mod initout{
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::portal;

        pub type Frame = portal::portal::initout::Frame;
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


