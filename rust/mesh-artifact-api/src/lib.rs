#![no_std]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

pub mod file;

pub struct ArtifactCaches {

}

pub enum ArtifactCacheKind{
  Bind(ArtifactCache<Bind>)
}

pub struct ArtifactCache<T> {
}

pub struct Artifact<T> {

}
