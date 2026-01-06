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
// CALC - Calculator operations
// ============================================================================

#[noun("calc", "Calculator operations")]
mod calc {
    use super::*;
    use clap_noun_verb_macros::verb;

    /// Add two numbers
    #[verb("add")]
    pub fn add(
        left: i32,
        right: i32,
    ) -> Result<serde_json::Value> {
        // Delegate to domain layer
        let result = crate::domain::calc::add(
            left,
            right,
        ).map_err(|e| CliError::from(e))?;

        // Return as JSON for structured output
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Subtract right from left
    #[verb("subtract")]
    pub fn subtract(
        left: i32,
        right: i32,
    ) -> Result<serde_json::Value> {
        // Delegate to domain layer
        let result = crate::domain::calc::subtract(
            left,
            right,
        ).map_err(|e| CliError::from(e))?;

        // Return as JSON for structured output
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Multiply two numbers
    #[verb("multiply")]
    pub fn multiply(
        left: i32,
        right: i32,
    ) -> Result<serde_json::Value> {
        // Delegate to domain layer
        let result = crate::domain::calc::multiply(
            left,
            right,
        ).map_err(|e| CliError::from(e))?;

        // Return as JSON for structured output
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Divide left by right
    #[verb("divide")]
    pub fn divide(
        left: i32,
        right: i32,
    ) -> Result<serde_json::Value> {
        // Delegate to domain layer
        let result = crate::domain::calc::divide(
            left,
            right,
        ).map_err(|e| CliError::from(e))?;

        // Return as JSON for structured output
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

}

fn main() -> Result<()> {
    clap_noun_verb::run()
}
