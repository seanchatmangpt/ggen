//! Type definitions for CLI generation
//!
//! These types mirror ggen_ai::rdf::types but are defined here
//! to avoid circular dependencies. When integrated, they should
//! match the types in ggen-ai::rdf::types.
//!
//! ## Features
//!
//! - **CLI project structure**: Complete representation of CLI project metadata
//! - **Noun-verb pattern**: Support for clap-noun-verb command structure
//! - **Type-safe arguments**: Argument types with validation rules
//! - **Dependency management**: Track project dependencies
//!
//! ## Examples
//!
//! ### Creating a CLI Project
//!
//! ```rust,no_run
//! use ggen_core::cli_generator::types::{CliProject, Noun, Verb, Argument, ArgumentType};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let project = CliProject {
//!     name: "my-cli".to_string(),
//!     version: "1.0.0".to_string(),
//!     description: "My CLI tool".to_string(),
//!     authors: vec!["Alice".to_string()],
//!     edition: "2021".to_string(),
//!     license: "MIT".to_string(),
//!     nouns: vec![
//!         Noun {
//!             name: "user".to_string(),
//!             description: "User operations".to_string(),
//!             module_path: "user".to_string(),
//!             verbs: vec![
//!                 Verb {
//!                     name: "create".to_string(),
//!                     description: "Create a user".to_string(),
//!                     arguments: vec![
//!                         Argument {
//!                             name: "name".to_string(),
//!                             long: Some("name".to_string()),
//!                             short: Some('n'),
//!                             help: "User name".to_string(),
//!                             required: true,
//!                             default: None,
//!                             value_name: Some("NAME".to_string()),
//!                             position: None,
//!                             arg_type: ArgumentType {
//!                                 name: "String".to_string(),
//!                                 parser: None,
//!                             },
//!                         }
//!                     ],
//!                     validations: vec![],
//!                     alias: None,
//!                     execution_logic: None,
//!                     domain_function: None,
//!                     domain_module: None,
//!                 }
//!             ],
//!         }
//!     ],
//!     dependencies: vec![],
//!     cli_crate: None,
//!     domain_crate: None,
//!     resolver: "2".to_string(),
//! };
//! # Ok(())
//! # }
//! ```
//!
//! ### Working with Nouns and Verbs
//!
//! ```rust,no_run
//! use ggen_core::cli_generator::types::{Noun, Verb};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let noun = Noun {
//!     name: "project".to_string(),
//!     description: "Project management".to_string(),
//!     module_path: "project".to_string(),
//!     verbs: vec![
//!         Verb {
//!             name: "init".to_string(),
//!             description: "Initialize a project".to_string(),
//!             arguments: vec![],
//!             validations: vec![],
//!             alias: None,
//!             execution_logic: None,
//!             domain_function: None,
//!             domain_module: None,
//!         },
//!         Verb {
//!             name: "build".to_string(),
//!             description: "Build the project".to_string(),
//!             arguments: vec![],
//!             validations: vec![],
//!             alias: None,
//!             execution_logic: None,
//!             domain_function: None,
//!             domain_module: None,
//!         },
//!     ],
//! };
//!
//! // Generates: `my-cli project init` and `my-cli project build`
//! # Ok(())
//! # }
//! ```

use serde::{Deserialize, Serialize};

/// Represents a complete CLI project
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CliProject {
    pub name: String,
    pub version: String,
    pub description: String,
    pub authors: Vec<String>,
    pub edition: String,
    pub license: String,
    pub nouns: Vec<Noun>,
    pub dependencies: Vec<Dependency>,
    #[serde(default)]
    pub cli_crate: Option<String>,
    #[serde(default)]
    pub domain_crate: Option<String>,
    #[serde(default = "default_resolver")]
    pub resolver: String,
}

fn default_resolver() -> String {
    "2".to_string()
}

/// Represents a noun (entity/resource) in the CLI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Noun {
    pub name: String,
    pub description: String,
    pub module_path: String,
    pub verbs: Vec<Verb>,
}

/// Represents a verb (action) that can be performed on a noun
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Verb {
    pub name: String,
    pub description: String,
    #[serde(default)]
    pub alias: Option<String>,
    pub arguments: Vec<Argument>,
    pub validations: Vec<Validation>,
    #[serde(default)]
    pub execution_logic: Option<String>,
    #[serde(default)]
    pub domain_function: Option<String>,
    #[serde(default)]
    pub domain_module: Option<String>,
}

/// Represents a command-line argument
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Argument {
    pub name: String,
    pub long: Option<String>,
    pub short: Option<char>,
    pub help: String,
    pub required: bool,
    pub default: Option<String>,
    pub value_name: Option<String>,
    pub position: Option<usize>,
    pub arg_type: ArgumentType,
}

/// Represents the type of an argument
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArgumentType {
    pub name: String,
    #[serde(default)]
    pub parser: Option<String>,
}

/// Represents a validation rule for arguments
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Validation {
    pub rule: String,
    #[serde(default)]
    pub pattern: Option<String>,
    pub message: String,
    pub arg_name: String,
}

/// Represents a Cargo dependency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub name: String,
    pub version: String,
    pub features: Vec<String>,
    #[serde(default)]
    pub optional: bool,
}
