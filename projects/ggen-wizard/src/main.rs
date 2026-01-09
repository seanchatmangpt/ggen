//! ggen Wizard CLI - Natural Language to RDF Code Generation
//!
//! This CLI was generated using clap-noun-verb from an RDF specification.
//!
//! Architecture:
//! - CLI Layer (this file): Argument parsing, command routing
//! - Domain Layer (domain.rs): Pure business logic
//! - Error Types (error.rs): Type-safe error handling

use clap_noun_verb::Result;
use clap_noun_verb_macros::noun;
use serde::Serialize;

mod domain;
mod error;

use error::CliError;

// ============================================================================
// PACK - Template pack management for wizard
// ============================================================================

#[noun("pack", "Template pack management for wizard")]
mod pack {
    use super::*;
    use clap_noun_verb_macros::verb;

    /// Apply pack templates to specification
    #[verb("apply")]
    pub fn apply() -> Result<serde_json::Value> {
        let result = crate::domain::pack::apply()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Preview what a pack would generate
    #[verb("preview")]
    pub fn preview() -> Result<serde_json::Value> {
        let result = crate::domain::pack::preview()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Recommend packs based on natural language description
    #[verb("recommend")]
    pub fn recommend() -> Result<serde_json::Value> {
        let result = crate::domain::pack::recommend()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

}


// ============================================================================
// SPEC - RDF specification management
// ============================================================================

#[noun("spec", "RDF specification management")]
mod spec {
    use super::*;
    use clap_noun_verb_macros::verb;

    /// Show differences between two specifications
    #[verb("diff")]
    pub fn diff() -> Result<serde_json::Value> {
        let result = crate::domain::spec::diff()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Export specification to various formats
    #[verb("export")]
    pub fn export() -> Result<serde_json::Value> {
        let result = crate::domain::spec::export()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Import specification from external format
    #[verb("import")]
    pub fn import() -> Result<serde_json::Value> {
        let result = crate::domain::spec::import()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Display specification contents in human-readable format
    #[verb("show")]
    pub fn show() -> Result<serde_json::Value> {
        let result = crate::domain::spec::show()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

}


// ============================================================================
// WIZARD - Wizard command for NL-driven specification generation
// ============================================================================

#[noun("wizard", "Wizard command for NL-driven specification generation")]
mod wizard {
    use super::*;
    use clap_noun_verb_macros::verb;

    /// Add feature to existing project from natural language
    #[verb("add")]
    pub fn add() -> Result<serde_json::Value> {
        let result = crate::domain::wizard::add()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Generate human-readable explanation of specification
    #[verb("explain")]
    pub fn explain() -> Result<serde_json::Value> {
        let result = crate::domain::wizard::explain()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Generate code from validated specification
    #[verb("generate")]
    pub fn generate() -> Result<serde_json::Value> {
        let result = crate::domain::wizard::generate()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Modify existing specification based on natural language changes
    #[verb("modify")]
    pub fn modify() -> Result<serde_json::Value> {
        let result = crate::domain::wizard::modify()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Create new project from natural language description
    #[verb("new")]
    pub fn new() -> Result<serde_json::Value> {
        let result = crate::domain::wizard::new()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

    /// Validate specification closure before generation
    #[verb("validate")]
    pub fn validate() -> Result<serde_json::Value> {
        let result = crate::domain::wizard::validate()
            .map_err(CliError::from)?;
        Ok(serde_json::to_value(&result).map_err(|e| CliError::Serialization(e.to_string()))?)
    }

}

fn main() -> Result<()> {
    clap_noun_verb::run()
}
