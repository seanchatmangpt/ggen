//! Generated CLI - from RDF ontology
//!
//! This CLI was generated using clap-noun-verb from an RDF description.
//! Architecture:
//! - CLI Layer (this file): Argument parsing, validation, error formatting
//! - Domain Layer (domain.rs): Pure business logic - implement your logic here
//! - Error Types (error.rs): Type-safe error handling

use clap_noun_verb::Result;
use clap_noun_verb_macros::noun;
use serde::Serialize;

mod domain;
mod error;

use error::CliError;

// ============================================================================
// TASK - Individual task item
// ============================================================================

#[noun("task", "Individual task item")]
mod task {
    use super::*;
    use clap_noun_verb_macros::verb;

    /// Create a new task
    #[verb("create")]
    pub fn create(
        title: String,
        description: String,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::task::create(
            title,
            description,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Mark a task as complete
    #[verb("complete")]
    pub fn complete(
        id: i32,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::task::complete(
            id,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Delete a task
    #[verb("delete")]
    pub fn delete(
        id: i32,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::task::delete(
            id,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// List all tasks
    #[verb("list")]
    pub fn list(
        filter: String,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::task::list(
            filter,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }
}

// ============================================================================
// LIST - Task list or project
// ============================================================================

#[noun("list", "Task list or project")]
mod list {
    use super::*;
    use clap_noun_verb_macros::verb;

    /// Create a new list
    #[verb("create")]
    pub fn create(
        name: String,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::list::create(
            name,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Delete a list
    #[verb("delete")]
    pub fn delete(
        id: i32,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::list::delete(
            id,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// View list details
    #[verb("view")]
    pub fn view(
        id: i32,
    ) -> Result<serde_json::Value> {
        let result = crate::domain::list::view(
            id,
        ).map_err(|e| CliError::from(e))?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }
}

fn main() -> Result<()> {
    clap_noun_verb::run()
}
