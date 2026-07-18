//! Domain Layer - Pure Business Logic

use crate::error::DomainError;
use serde::Serialize;
use std::path::PathBuf;

pub mod file {
    use super::*;

    pub fn copy(
        _source: PathBuf,
        _destination: PathBuf,
    ) -> Result<impl Serialize, DomainError> {
        unimplemented!("file::copy - implement your logic here")
    }

    pub fn move_file(
        _source: PathBuf,
        _destination: PathBuf,
    ) -> Result<impl Serialize, DomainError> {
        unimplemented!("file::move - implement your logic here")
    }

    pub fn delete(
        _path: PathBuf,
        _force: bool,
    ) -> Result<impl Serialize, DomainError> {
        unimplemented!("file::delete - implement your logic here")
    }

    pub fn view(
        _path: PathBuf,
    ) -> Result<impl Serialize, DomainError> {
        unimplemented!("file::view - implement your logic here")
    }
}

pub mod directory {
    use super::*;

    pub fn create(
        _path: PathBuf,
    ) -> Result<impl Serialize, DomainError> {
        unimplemented!("directory::create - implement your logic here")
    }

    pub fn delete(
        _path: PathBuf,
        _recursive: bool,
    ) -> Result<impl Serialize, DomainError> {
        unimplemented!("directory::delete - implement your logic here")
    }

    pub fn list(
        _path: PathBuf,
    ) -> Result<impl Serialize, DomainError> {
        unimplemented!("directory::list - implement your logic here")
    }
}
