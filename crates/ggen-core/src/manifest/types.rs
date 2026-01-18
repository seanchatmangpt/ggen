//! Manifest type definitions for ggen.toml parsing
//!
//! These types map directly to the TOML structure defined in the specification.
//! All collections use BTreeMap for deterministic serialization.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

/// Default SPARQL query timeout (5 seconds)
fn default_sparql_timeout() -> u64 {
    5000
}

/// Default reasoning timeout (5 seconds)
fn default_reasoning_timeout() -> u64 {
    5000
}

/// Default output directory
fn default_output_dir() -> PathBuf {
    PathBuf::from("src/generated")
}

/// Root manifest structure from ggen.toml
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GgenManifest {
    /// Project metadata
    pub project: ProjectConfig,

    /// Ontology loading configuration
    pub ontology: OntologyConfig,

    /// Inference rules (CONSTRUCT-based)
    #[serde(default)]
    pub inference: InferenceConfig,

    /// Code generation rules
    pub generation: GenerationConfig,

    /// Validation settings
    #[serde(default)]
    pub validation: ValidationConfig,
}

/// Project metadata section
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    /// Project name (used in generated code headers)
    pub name: String,

    /// Semantic version
    pub version: String,

    /// Optional description
    #[serde(default)]
    pub description: Option<String>,
}

/// Ontology configuration section
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyConfig {
    /// Primary ontology file path (relative to ggen.toml)
    pub source: PathBuf,

    /// Additional ontology imports
    #[serde(default)]
    pub imports: Vec<PathBuf>,

    /// Base IRI for relative URIs
    #[serde(default)]
    pub base_iri: Option<String>,

    /// Prefix mappings for SPARQL queries (BTreeMap for determinism)
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
}

/// Inference configuration section
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct InferenceConfig {
    /// Named inference rules executed in order
    #[serde(default)]
    pub rules: Vec<InferenceRule>,

    /// Maximum time for all inference (ms)
    #[serde(default = "default_reasoning_timeout")]
    pub max_reasoning_timeout_ms: u64,
}

/// A single inference rule (CONSTRUCT query)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferenceRule {
    /// Rule identifier (for audit trail)
    pub name: String,

    /// Human-readable description
    #[serde(default)]
    pub description: Option<String>,

    /// SPARQL CONSTRUCT query
    pub construct: String,

    /// Execution order (lower = earlier)
    #[serde(default)]
    pub order: i32,

    /// Skip if condition fails (SPARQL ASK)
    #[serde(default)]
    pub when: Option<String>,
}

/// Generation configuration section
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationConfig {
    /// Code generation rules
    pub rules: Vec<GenerationRule>,

    /// Maximum SPARQL query timeout (ms)
    #[serde(default = "default_sparql_timeout")]
    pub max_sparql_timeout_ms: u64,

    /// Generate audit.json
    #[serde(default)]
    pub require_audit_trail: bool,

    /// Salt for deterministic IRI generation
    #[serde(default)]
    pub determinism_salt: Option<String>,

    /// Output directory (relative to ggen.toml)
    #[serde(default = "default_output_dir")]
    pub output_dir: PathBuf,
}

/// A single generation rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationRule {
    /// Rule identifier
    pub name: String,

    /// SPARQL query (file path or inline)
    pub query: QuerySource,

    /// Tera template (file path or inline)
    pub template: TemplateSource,

    /// Output file pattern (supports {{variables}})
    pub output_file: String,

    /// Skip generation if query returns empty
    #[serde(default)]
    pub skip_empty: bool,

    /// File generation mode
    #[serde(default)]
    pub mode: GenerationMode,

    /// Skip if condition fails (SPARQL ASK query)
    #[serde(default)]
    pub when: Option<String>,
}

/// Source for a SPARQL query - file or inline
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum QuerySource {
    /// Load query from file
    File {
        /// Path to .sparql file
        file: PathBuf,
    },
    /// Inline query string
    Inline {
        /// SPARQL query text
        inline: String,
    },
}

/// Source for a Tera template - file or inline
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum TemplateSource {
    /// Load template from file
    File {
        /// Path to .tera file
        file: PathBuf,
    },
    /// Inline template string
    Inline {
        /// Tera template text
        inline: String,
    },
}

/// File generation mode
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
pub enum GenerationMode {
    /// Create new file (fail if exists)
    #[default]
    Create,
    /// Overwrite existing
    Overwrite,
    /// Merge with existing (marker-based)
    Merge,
}

/// Validation configuration section
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ValidationConfig {
    /// SHACL shape files
    #[serde(default)]
    pub shacl: Vec<PathBuf>,

    /// Validate generated Rust syntax
    #[serde(default)]
    pub validate_syntax: bool,

    /// Reject code containing unsafe
    #[serde(default)]
    pub no_unsafe: bool,

    /// Custom validation rules (SPARQL ASK)
    #[serde(default)]
    pub rules: Vec<ValidationRule>,
}

/// A custom validation rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationRule {
    /// Rule identifier
    pub name: String,

    /// Description shown on failure
    pub description: String,

    /// SPARQL ASK query (true = valid)
    pub ask: String,

    /// Error severity
    #[serde(default)]
    pub severity: ValidationSeverity,
}

/// Validation error severity
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
pub enum ValidationSeverity {
    /// Fails generation
    #[default]
    Error,
    /// Logged but continues
    Warning,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_values() {
        assert_eq!(default_sparql_timeout(), 5000);
        assert_eq!(default_reasoning_timeout(), 5000);
        assert_eq!(default_output_dir(), PathBuf::from("src/generated"));
    }

    #[test]
    fn test_generation_mode_default() {
        let mode: GenerationMode = Default::default();
        assert_eq!(mode, GenerationMode::Create);
    }

    #[test]
    fn test_validation_severity_default() {
        let severity: ValidationSeverity = Default::default();
        assert_eq!(severity, ValidationSeverity::Error);
    }
}
