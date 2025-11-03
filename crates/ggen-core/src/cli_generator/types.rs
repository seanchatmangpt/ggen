//! Type definitions for CLI generation
//!
//! These types mirror ggen_ai::rdf::types but are defined here
//! to avoid circular dependencies. When integrated, they should
//! match the types in ggen-ai::rdf::types.

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

