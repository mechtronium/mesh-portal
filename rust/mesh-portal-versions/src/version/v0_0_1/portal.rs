pub mod portal {
    use crate::version::v0_0_1::util::unique_id;
    use serde::{Deserialize, Serialize};
    use std::ops::Deref;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Exchanger<T> {
        pub id: String,
        pub item: T,
    }

    impl<T> Exchanger<T> {
        pub fn new(item: T) -> Self {
            Exchanger {
                id: unique_id(),
                item,
            }
        }

        pub fn with<X>(self, item: X) -> Exchanger<X> {
            Exchanger { id: self.id, item }
        }
    }

    impl<T> Deref for Exchanger<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.item
        }
    }

    pub mod inlet {
        use std::convert::TryFrom;
        use std::ops::Deref;

        use crate::error::MsgErr;
        use crate::version::v0_0_1::artifact::ArtifactRequest;
        use crate::version::v0_0_1::frame::frame::{CloseReason, PrimitiveFrame};
        use crate::version::v0_0_1::id::id::{GenericKind, GenericKindBase, Point};
        use crate::version::v0_0_1::messaging::messaging::{Request, Response, Session};
        use crate::version::v0_0_1::particle::particle::StatusUpdate;
        use crate::version::v0_0_1::payload::payload::Payload;
        use crate::version::v0_0_1::portal::portal;
        use crate::version::v0_0_1::portal::portal::Exchanger;
        use crate::version::v0_0_1::selector::selector::KindPattern;
        use crate::version::v0_0_1::util::unique_id;
        use serde::{Deserialize, Serialize};
        use crate::version::v0_0_1::log::{AuditLog, Log, LogSpan, PointlessLog };

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum Frame {
            Log(Log),
            LogSpan(LogSpan),
            Audit(AuditLog),
            PointlessLog(PointlessLog),
            AssignRequest(Exchanger<AssignRequest>),
            Request(Request),
            Response(Response),
            Artifact(Exchanger<ArtifactRequest>), // portal inlet will cache and return artifact
            Status(StatusUpdate),
            Close(CloseReason),
        }

        impl Frame {
            pub fn from(&self) -> Option<Point> {
                match self {
                    Frame::Log(log) => Option::Some(log.point.clone()),
                    Frame::LogSpan(span) => Option::Some(span.point.clone()),
                    Frame::Audit(log) => Option::Some(log.point.clone()),
                    Frame::PointlessLog(_) => Option::None,
                    Frame::Request(request) => Option::Some(request.from.clone()),
                    Frame::Response(response) => {
                        // Response will need a from field for it to work within Ports
                        Option::None
                    }
                    Frame::Artifact(artifact) => Option::None,
                    Frame::Status(status) => Option::Some(status.from.clone()),
                    Frame::Close(_) => Option::None,
                    Frame::AssignRequest(_) => Option::None,
                }
            }
        }


        #[derive(
            Debug, Clone, Serialize, Deserialize, strum_macros::Display, strum_macros::EnumString,
        )]
        pub enum AssignRequest {
            Control,
        }

        impl TryInto<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                Ok(PrimitiveFrame {
                    data: bincode::serialize(&self)?,
                })
            }
        }

        impl TryFrom<PrimitiveFrame> for portal::inlet::Frame {
            type Error = MsgErr;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }

    pub mod outlet {
        use std::convert::TryFrom;

        use crate::error::MsgErr;
        use crate::version::v0_0_1::artifact::{Artifact, ArtifactResponse};
        use crate::version::v0_0_1::config::config::{Assign, PointConfig, Config};
        use crate::version::v0_0_1::frame::frame::{CloseReason, PrimitiveFrame};
        use crate::version::v0_0_1::id::id::{GenericKind, GenericKindBase, Point};
        use crate::version::v0_0_1::messaging::messaging::{Request, Response, Session};
        use crate::version::v0_0_1::payload::payload::Payload;
        use crate::version::v0_0_1::portal::portal::Exchanger;
        use serde::{Deserialize, Serialize};
        use crate::version::v0_0_1::log::LogSpan;

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum Frame {
            Init,
            Assign(Exchanger<Assign>),
            Request(RequestFrame),
            Response(Response),
            Artifact(Exchanger<ArtifactResponse>),
            Close(CloseReason),
        }

        impl Frame {
            pub fn to(&self) -> Option<Point> {
                match self {
                    Frame::Assign(assign) => Option::Some(assign.stub.point.clone()),
                    Frame::Request(request) => Option::Some(request.request.to.clone()),
                    Frame::Response(response) => Option::Some(response.to.clone()),
                    Frame::Artifact(artifact) => Option::Some(artifact.to.clone()),
                    Frame::Close(_) => Option::None,
                    Frame::Init => Option::None,
                }
            }
        }

        impl TryInto<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                Ok(PrimitiveFrame {
                    data: bincode::serialize(&self)?,
                })
            }
        }

        impl TryFrom<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }

        #[derive(Debug,Clone,Serialize,Deserialize)]
        pub struct RequestFrame {
            pub request: Request,
            pub session: Option<Session>,
            pub log_span: LogSpan
        }

    }

    pub mod initin {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::artifact::{Artifact, ArtifactRequest, ArtifactResponse};
        use crate::version::v0_0_1::frame::frame::PrimitiveFrame;
        use crate::version::v0_0_1::portal::portal::Exchanger;
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct PortalAuth {
            pub user: String,
            pub portal_key: Option<String>,
        }

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum Frame {
            Flavor(String),
            Auth(PortalAuth),
            Artifact(Exchanger<ArtifactRequest>),
            Ok,
            Ready,
        }

        impl TryInto<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                Ok(PrimitiveFrame {
                    data: bincode::serialize(&self)?,
                })
            }
        }

        impl TryFrom<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }
    pub mod initout {
        use crate::error::MsgErr;
        use crate::version::v0_0_1::artifact::{Artifact, ArtifactResponse};
        use crate::version::v0_0_1::frame::frame::PrimitiveFrame;
        use crate::version::v0_0_1::portal::portal::Exchanger;
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
        pub enum Frame {
            Ok,
            Artifact(Exchanger<ArtifactResponse>),
        }
        impl TryInto<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
                Ok(PrimitiveFrame {
                    data: bincode::serialize(&self)?,
                })
            }
        }

        impl TryFrom<PrimitiveFrame> for Frame {
            type Error = MsgErr;

            fn try_from(value: PrimitiveFrame) -> Result<Self, Self::Error> {
                Ok(bincode::deserialize(value.data.as_slice())?)
            }
        }
    }
}
