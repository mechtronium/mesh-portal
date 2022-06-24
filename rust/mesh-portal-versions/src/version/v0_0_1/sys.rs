
use crate::error::MsgErr;
use crate::version::v0_0_1::id::id::{GenericKind, Point, ToPoint, ToPort};
use crate::version::v0_0_1::particle::particle::{Details, Status, Stub};
use crate::version::v0_0_1::substance::substance::Substance;
use cosmic_macros_primitive::Autobox;


use serde::{Deserialize, Serialize};
use crate::version::v0_0_1::log::Log;
use crate::version::v0_0_1::substance::substance::SubstanceKind::ReqCore;
use crate::version::v0_0_1::wave::{ReqShell, ReqCore, SysMethod};


#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq,strum_macros::Display)]
pub enum AssignmentKind {
    Create,
    // eventually we will have Move as well as Create
}


#[derive(Debug,Clone,Serialize,Deserialize,strum_macros::Display)]
pub enum ChildRegistry {
    Shell,
    Core
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Location {
    Central,
    Nowhere,
    Somewhere(Point)
}

impl ToString for Location {
    fn to_string(&self) -> String {
        match self {
            Location::Nowhere => {
                "Unassigned".to_string()
            }
            Location::Somewhere(point) => {
                point.to_string()
            }
            Location::Central => {
                Point::central().to_string()
            }
        }
    }
}


impl Location {
    pub fn new(point: Point) -> Self {
        Location::Somewhere( point )
    }

    pub fn ok_or(&self)->Result<Point,MsgErr> {
        match self {
            Location::Nowhere => {
                Err("Particle is presently nowhere".into())
            }
            Location::Somewhere(point ) => {
                Ok(point.clone())
            }

            Location::Central => {
                Ok(Point::central())
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParticleRecord {
    pub details: Details,
    pub location: Location,
}


impl Default for ParticleRecord {
    fn default() -> Self {
        Self::root()
    }
}


impl ParticleRecord {
    pub fn new(details: Details, point: Point ) -> Self {
        ParticleRecord {
            details,
            location: Location::new(point ),
        }
    }

    pub fn root() -> Self {
        Self {
            details: Details {
                stub: Stub {
                    point: Point::root(),
                    kind: GenericKind::root(),
                    status: Status::Ready
                },
                properties: Default::default(),
            },
            location: Location::Central
        }
    }
}

impl Into<Stub> for ParticleRecord {
    fn into(self) -> Stub {
        self.details.stub
    }
}


#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq )]
pub struct Assign {
    pub kind: AssignmentKind,
    pub details: Details
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display, Autobox)]
pub enum Sys {
    Assign(Assign),
    Event(SysEvent),
    Log(Log),
    ConnectReq(ConnectReq)
}

impl TryFrom<ReqShell> for Assign {
    type Error = MsgErr;

    fn try_from(request: ReqShell) -> Result<Self, Self::Error> {
        if let Substance::Sys(Sys::Assign(assign)) = request.core.body {
            Ok(assign)
        } else {
            Err(MsgErr::bad_request())
        }
    }
}

impl Into<Substance> for Assign {
    fn into(self) -> Substance {
        Substance::Sys(Sys::Assign(self))
    }
}

impl Into<ReqCore> for Assign {
    fn into(self) -> ReqCore {
        ReqCore::new( SysMethod::Assign.into() ).with_body(Substance::Sys(Sys::Assign(self)))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display, Autobox)]
pub enum SysEvent {
    Created(Created)
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Created {
    pub point: Point,
    pub kind: GenericKind
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum ServiceSelector {
    Cli,
    Portal,
    Host(String)
}


#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ConnectReq {
  pub selector: ServiceSelector,
  pub auth: Box<Substance>,
  pub end_point: Option<Point>,
}

impl Into<ReqShell> for ConnectReq {
    fn into(self) -> ReqShell {
        let mut core = ReqCore::new(SysMethod::ConnectReq.into() );
        core.body = Sys::ConnectReq(self).into();
        let req = ReqShell::new(core, Point::local_hypergate(), Point::remote_entry_requester() );
        req
    }
}




