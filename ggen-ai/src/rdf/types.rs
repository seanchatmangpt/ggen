//! Type definitions for CLI project structure from RDF/RDFS.
//!
//! These types represent the structured data extracted from RDF graphs
//! that describe CLI applications in the noun-verb pattern.

use serde::{Deserialize, Serialize};

/// Represents a complete CLI project extracted from RDF.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CliProject {
    /// Project name (e.g., "my-cli")
    pub name: String,
    /// Semantic version (e.g., "0.1.0")
    pub version: String,
    /// Project description
    pub description: String,
    /// List of authors
    pub authors: Vec<String>,
    /// Rust edition (e.g., "2021")
    pub edition: String,
    /// License identifier (e.g., "MIT")
    pub license: String,
    /// List of noun entities (commands)
    pub nouns: Vec<Noun>,
    /// Project dependencies
    pub dependencies: Vec<Dependency>,
}

/// Represents a noun (entity/resource) in the CLI.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Noun {
    /// Noun name (e.g., "user", "project")
    pub name: String,
    /// Human-readable description
    pub description: String,
    /// Module path (e.g., "cmds::user")
    pub module_path: String,
    /// List of verbs (actions) for this noun
    pub verbs: Vec<Verb>,
}

/// Represents a verb (action) that can be performed on a noun.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Verb {
    /// Verb name (e.g., "create", "list", "delete")
    pub name: String,
    /// Human-readable description
    pub description: String,
    /// Optional short alias (e.g., "ls" for "list")
    pub alias: Option<String>,
    /// Arguments accepted by this verb
    pub arguments: Vec<Argument>,
    /// Validation rules for this verb
    pub validations: Vec<Validation>,
    /// Optional execution logic description
    pub execution_logic: Option<String>,
}

/// Represents a command-line argument.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Argument {
    /// Argument name
    pub name: String,
    /// Long form (e.g., "--name")
    pub long: Option<String>,
    /// Short form (e.g., 'n')
    pub short: Option<char>,
    /// Help text
    pub help: String,
    /// Whether the argument is required
    pub required: bool,
    /// Default value if not provided
    pub default: Option<String>,
    /// Value name shown in help (e.g., "<NAME>")
    pub value_name: Option<String>,
    /// Position if positional argument
    pub position: Option<usize>,
    /// Type information
    pub arg_type: ArgumentType,
}

/// Represents the type of an argument.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArgumentType {
    /// Type name (e.g., "String", "PathBuf", "bool")
    pub name: String,
    /// Optional parser function name
    pub parser: Option<String>,
}

/// Represents a validation rule for arguments.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Validation {
    /// Rule type (e.g., "file_exists", "regex")
    pub rule: String,
    /// Optional pattern for regex rules
    pub pattern: Option<String>,
    /// Error message shown on validation failure
    pub message: String,
    /// Argument name this validation applies to
    pub arg_name: String,
}

/// Represents a Cargo dependency.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    /// Dependency name
    pub name: String,
    /// Version requirement (e.g., "1.0")
    pub version: String,
    /// Cargo features to enable
    pub features: Vec<String>,
    /// Whether the dependency is optional
    pub optional: bool,
}

// Legacy types for backward compatibility
// These types are used by existing code but match the new type system

/// Legacy RDF command definition
pub type RdfCommand = Verb;

/// Legacy RDF argument definition
pub type RdfArgument = Argument;

/// Legacy RDF flag definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RdfFlag {
    /// Flag name
    pub name: String,
    /// Short form
    pub short: Option<String>,
    /// Description
    pub description: String,
    /// Flag type
    pub flag_type: String,
}

/// CLI generation configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CliConfig {
    /// CLI name
    pub name: String,
    /// Version
    pub version: String,
    /// Author
    pub author: String,
    /// About text
    pub about: String,
    /// Commands
    pub commands: Vec<RdfCommand>,
}

/// Template context for rendering
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateContext {
    /// Configuration
    pub config: CliConfig,
    /// Metadata
    pub metadata: std::collections::HashMap<String, String>,
}
