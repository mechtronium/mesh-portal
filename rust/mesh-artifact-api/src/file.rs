use alloc::sync::Arc;
use alloc::vec::Vec;
use std::prelude::rust_2021::String;
use anyhow::Result;

pub trait FileAccess {
    fn remove(&self, path: &str ) -> anyhow::Result<()>;
    fn list(&self, path: &str ) -> anyhow::Result<Vec<String>>;
    fn read(&self, path: &str ) -> anyhow::Result<Vec<u8>>;
    fn write(&self, path: &str, data: Vec<u8>) -> anyhow::Result<()>;
    fn unzip(&self, source: &str, targer: &str) -> anyhow::Result<()>;
    fn mkdir(&self, path: &str) -> anyhow::Result<()>;
    fn exists(&self, path: &str) -> anyhow::Result<()>;
}

#[cfg(feature = "local-filesystem")]
pub mod local{
    use alloc::vec::Vec;
    use core::fmt::Error;
    use std::fs;
    use crate::file::FileAccess;
    use std::path::Path;
    use std::path::PathBuf;

    pub struct LocalFileAccess {
        root: PathBuf
    }

    impl LocalFileAccess {
        fn path( &self, with: &str ) -> anyhow::Result<Path> {
            let mut path = self.root.clone();
            path.push( with );
            Ok(path.as_path().clone())
        }
    }

    impl FileAccess for LocalFileAccess {
        fn remove(&self, path: &str) -> anyhow::Result<()> {
            Ok(fs::remove_dir_all(path)?)
        }

        fn list(&self, path: &str) -> anyhow::Result<Vec<String>> {
            let path = self.path(path)?;
            let mut rtn = vec![];
            for entry in fs::read_dir(path)? {
                rtn.push(entry?.path().to_str().ok_or("expected to be able to convert path to str")?.to_string());
            }
            Ok(rtn)
        }

        fn read(&self, path: &str) -> anyhow::Result<Vec<u8>> {
            let path = self.path(path)?;
            Ok(fs::read(path)?)
        }

        fn write(&self, path: &str, data: Vec<u8>) -> anyhow::Result<()> {
            let path = self.path(path)?;
            Ok(fs::write(path,data)?)
        }

        fn unzip(&self, source: &str, target: &str) -> anyhow::Result<()> {
            let source = self.path(path)?;
            let target = self.path(path)?;
            let source = fs::File::open(source)?;
            let mut archive = zip::ZipArchive::new(source)?;

            for i in 0..archive.len() {
                let mut zip_file = archive.by_index(i)?;
                if zip_file.is_dir() {
                    let path = Path::from_str(format!("/{}/{}", target, zip_file.name()).as_str())?;
                    self.mkdir(&path)?;
                } else {

                    let path = Path::from_str(format!("{}/{}/{}", self.base_dir, target, zip_file.name()).as_str())?;
                    let parent = Path::from_str(format!("/{}/{}", target, zip_file.name()).as_str())?.parent();
                    match parent {
                        None => {}
                        Some(parent) => {
                            self.mkdir(&parent )?;
                        }
                    }

                    let mut file = fs::File::create(path.to_string().clone())?;
                    std::io::copy(&mut zip_file, &mut file)?;
                }
            }

        }

        fn mkdir(&self, path: &str) -> anyhow::Result<()> {
            Ok(fs::create_dir_all(path)?)
        }

        fn exists(&self, path: &str) -> anyhow::Result<()> {
            if fs::try_exists()? == true {
                Ok(())
            } else {
                Err(anyhow!("file does not exist"))
            }
        }
    }
}