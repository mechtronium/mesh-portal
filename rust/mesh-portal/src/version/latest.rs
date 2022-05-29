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
    use mesh_portal_versions::version::{v0_0_1 as current, v0_0_1};
    use mesh_portal_versions::version::v0_0_1::selector;

    pub type GenericKindSelector = selector::selector::GenericKindSelector;
    pub type GenericSubKindSelector = selector::selector::GenericSubKindSelector;
    pub type PointSelector = selector::selector::PointSelector;
    pub type KindSelector = selector::selector::KindSelector;
    pub type VersionReq = selector::selector::VersionReq;
    pub type PointSegSelector = selector::selector::PointSegSelector;
    pub type KeySegment = selector::selector::KeySegment;
    pub type ExactSegment = selector::selector::ExactPointSeg;
    pub type SpecificPattern = selector::selector::SpecificSelector;
    pub type LabeledPrimitiveType = selector::selector::LabeledPrimitiveType;
    pub type PrimitiveType = selector::selector::PayloadType2;
    pub type Format = selector::selector::Format;
    pub type EntityPattern = selector::selector::PipelineSelector;
    pub type RcPattern = selector::selector::RcPipelineSelector;
    pub type MsgPattern = selector::selector::MsgPipelineSelector;
    pub type HttpPattern = selector::selector::HttpPipelineSelector;
    pub type Block = v0_0_1::selector::PayloadBlock;
    pub type UploadBlock = v0_0_1::selector::UploadBlock;
    pub type CreateBlock = v0_0_1::selector::CreateBlock;
    pub type PatternBlock = v0_0_1::selector::PatternBlock;
    pub type MapEntryPattern= selector::selector::MapEntryPattern;
    pub type Hop = selector::selector::Hop;
    pub type Pattern<P> = selector::selector::Pattern<P>;
    pub type EmptyPattern<P> = selector::selector::EmptyPattern<P>;
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
    pub type Range = payload::payload::NumRange;
    pub type PayloadPattern = payload::payload::PayloadPattern;
    pub type CallWithConfig = payload::payload::CallWithConfig;
    pub type Call = payload::payload::Call;
    pub type CallKind = payload::payload::CallKind;
    pub type MsgCall = payload::payload::MsgCall;
    pub type HttpCall = payload::payload::HttpCall;
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
    pub type AccessGrantKind = current::security::AccessGrantKind;
}


pub mod msg {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::msg;

    pub type MsgRequest = msg::MsgRequest;
    pub type MsgMethod = msg::MsgMethod;
}

pub mod http {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::http;

    pub type HttpRequest = http::HttpRequest;
    pub type HttpMethod = http::HttpMethod;
}

pub mod config {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::config;

    pub type PortalKind = config::config::PortalKind;
    pub type Info = config::config::Info;
    pub type PortalConfig = config::config::PortalConfig;
    pub type Assign = config::config::Assign;
    pub type Config<BODY> = config::config::PointConfig<BODY>;
    pub type ConfigBody = config::config::Document;
    pub type ParticleConfigBody = config::config::ParticleConfigBody;


    pub mod bind {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::config;

        pub type BindConfig = config::config::bind::BindConfig;
        pub type ConfigScope<T,E>= config::config::bind::ConfigScope<T,E>;
        pub type Pipeline= config::config::bind::Pipeline;
        pub type PipelineStep= config::config::bind::PipelineStep;
        pub type PipelineStop= config::config::bind::PipelineStop;
        pub type PatternBlock = config::config::bind::PatternBlock;
        pub type Selector<P> = config::config::bind::Selector<P>;
        pub type Whitelist = config::config::bind::Whitelist;
        pub type CallPattern = config::config::bind::CallPattern;
        pub type StepKind = config::config::bind::MessageKind;
        pub type PipelineSubScope = config::config::bind::PipelinesSubScope;
        pub type ScopeType = config::config::bind::ScopeType;
    }
}

pub mod entity {
    use mesh_portal_versions::version::v0_0_1 as current;
    use mesh_portal_versions::version::v0_0_1::entity;

    pub type EntityType = entity::MethodKind;

    pub mod request {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::{command, entity};

        pub type Method = command::request::Method;
        pub type RequestCore= command::request::RequestCore;
        pub type Rc = command::request::Rc;
        pub type RcCommandType = command::request::RcCommandType;

        pub mod create {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::{command, entity};

            pub type Create= command::request::create::Create;
            pub type Template = command::request::create::Template;
            pub type KindTemplate = command::request::create::KindTemplate;
            pub type Fulfillment = command::request::create::Fulfillment;
            pub type Strategy = command::request::create::Strategy;
            pub type PointTemplate = command::request::create::PointTemplate;
            pub type PointSegFactory = command::request::create::PointSegFactory;
            pub type CreateOp = command::request::create::Create;
            pub type Require = command::request::create::Require;
            pub type Set = command::request::set::Set;
        }

        pub mod select {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::{command, entity};

            pub type SelectIntoPayload = command::request::select::SelectIntoPayload;
            pub type Select = command::request::select::Select;
            pub type SelectionKind = command::request::select::SelectKind;
            pub type SubSelector = command::request::select::SubSelect;
            pub type PropertiesPattern = command::request::select::PropertiesPattern;
        }

        pub mod update {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::{command, entity};

            pub type Update = command::request::update::Update;
        }

        pub mod query {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::{command, entity};

            pub type Query= command::request::query::Query;
            pub type QueryResult = command::request::query::QueryResult;
        }

        pub mod get {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::{command, entity};

            pub type Get = command::request::get::Get;
            pub type GetOp = command::request::get::GetOp;
        }

        pub mod set {
            use mesh_portal_versions::version::v0_0_1 as current;
            use mesh_portal_versions::version::v0_0_1::{command, entity};

            pub type Set = command::request::set::Set;
        }
    }

    pub mod response {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::entity;

        pub type ResponseCore = entity::response::ResponseCore;
    }
}

pub mod particle {
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

        pub type Frame = portal::portal::inlet::Frame;
        pub type AssignRequest = portal::portal::inlet::AssignRequest;
    }

    pub mod outlet {
        use mesh_portal_versions::version::v0_0_1 as current;
        use mesh_portal_versions::version::v0_0_1::portal;

        pub type Frame = portal::portal::outlet::Frame;
        pub type RequestFrame = portal::portal::outlet::RequestFrame;
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
    pub type ToResolved<R> = dyn current::parse::ToResolved<R>;

    pub fn uuid() -> String {
        current::util::uuid()
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

pub mod log {
    use mesh_portal_versions::version::v0_0_1 as current;
    pub type Log = current::log::Log;
    pub type LogSpan = current::log::LogSpanEvent;
    pub type LogSpanKind = current::log::LogSpanEventKind;
    pub type LogPayload = current::log::LogPayload;
    pub type LogAppender = dyn current::log::LogAppender;
    pub type RootLogger = current::log::RootLogger;
    pub type RootLogBuilder = current::log::RootLogBuilder;
    pub type LogSource = current::log::LogSource;
    pub type SpanLogBuilder = current::log::SpanLogBuilder;
    pub type PointlessLog = current::log::PointlessLog;
    pub type PointLogger = current::log::PointLogger;
    pub type SpanLogger = current::log::SpanLogger;
}

pub mod cli {
    use mesh_portal_versions::version::v0_0_1 as current;
    pub type CommandTemplate = current::cli::CommandTemplate;
    pub type RawCommand = current::cli::RawCommand;
    pub type Transfer = current::cli::Transfer;
}

