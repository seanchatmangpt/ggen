//! Generated CLI - from RDF ontology
//!
//! This CLI was generated using clap-noun-verb from an RDF description.

use clap_noun_verb::Result;
use clap_noun_verb_macros::noun;
use serde::Serialize;
use std::path::PathBuf;

mod domain;
mod error;

use error::CliError;

// ============================================================================
// FILE - File system file
// ============================================================================

#[noun("file", "File system file")]
mod file {
    use super::*;
    use clap_noun_verb_macros::verb;

    /// Copy a file
    #[verb("copy")]
    pub fn copy(
        source: PathBuf,
        destination: PathBuf,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::file::copy(
            source,
            destination,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Move or rename a file
    #[verb("move")]
    pub fn move_file(
        source: PathBuf,
        destination: PathBuf,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::file::move_file(
            source,
            destination,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Delete a file
    #[verb("delete")]
    pub fn delete(
        path: PathBuf,
        force: bool,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::file::delete(
            path,
            force,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// View file contents
    #[verb("view")]
    pub fn view(
        path: PathBuf,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::file::view(
            path,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }
}

// ============================================================================
// DIRECTORY - File system directory
// ============================================================================

#[noun("directory", "File system directory")]
mod directory {
    use super::*;
    use clap_noun_verb_macros::verb;

    /// Create a directory
    #[verb("create")]
    pub fn create(
        path: PathBuf,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::directory::create(
            path,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Delete a directory
    #[verb("delete")]
    pub fn delete(
        path: PathBuf,
        recursive: bool,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::directory::delete(
            path,
            recursive,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// List directory contents
    #[verb("list")]
    pub fn list(
        path: PathBuf,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::directory::list(
            path,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }
}

fn main() -> Result<()> {
    clap_noun_verb::run()
}
