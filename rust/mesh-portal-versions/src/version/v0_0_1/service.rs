use crate::error::MsgErr;
use crate::version::v0_0_1::id::id::Point;
use crate::version::v0_0_1::messaging::messaging::{Agent, Message};
use crate::version::v0_0_1::security::Access;

pub trait Router: Send+Sync {
    fn route( &self, message: Message );
}

pub trait AccessProvider: Send+Sync {
    fn access(&self, to: &Agent, on: &Point) -> Result<Access,MsgErr>;
}

pub struct AllAccessProvider();

impl AccessProvider for AllAccessProvider {
    fn access(&self, to: &Agent, on: &Point) -> Result<Access, MsgErr> {
        Ok(Access::SuperOwner)
    }
}

pub trait Global: Send+Sync {
    fn handle(&self, message: Message );
}

/*
#[async_trait]
pub trait ArtifactApi: Send+Sync {
    fn get_artifact<A>(&self, artifact: &Point) -> Result<ArtifactItem<CachedConfig<A>>>;
}

 */