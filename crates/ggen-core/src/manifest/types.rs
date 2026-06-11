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

/// Default output directory (project root, relative to ggen.toml)
fn default_output_dir() -> PathBuf {
    PathBuf::from(".")
}

/// A reference to a ggen pack declared in ggen.toml
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PackRef {
    /// Pack name (used to reference this pack from generation rules)
    pub name: String,

    /// Registry type: "local", "marketplace", etc.
    #[serde(default = "default_registry")]
    pub registry: String,

    /// Local filesystem path (used when registry = "local")
    #[serde(default)]
    pub path: Option<PathBuf>,

    /// Pack version constraint (used when registry != "local")
    #[serde(default)]
    pub version: Option<String>,
}

fn default_registry() -> String {
    "local".to_string()
}

/// Describes a pack's named output directories, read from `<pack_root>/package.toml`.
/// Missing or unparseable file is treated as empty outputs (fail-open, falls back to literal key).
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PackageToml {
    /// Named output directories: key → relative directory path within the pack.
    #[serde(default)]
    pub outputs: std::collections::HashMap<String, String>,
}

impl PackageToml {
    /// Load from `<pack_root>/package.toml`. Returns empty struct if file is missing or invalid.
    pub fn load(pack_root: &std::path::Path) -> Self {
        let path = pack_root.join("package.toml");
        let Ok(content) = std::fs::read_to_string(&path) else {
            return Self::default();
        };
        toml::from_str(&content).unwrap_or_default()
    }

    /// Resolve an output key to its directory path, or return the key itself as fallback.
    pub fn resolve_output_key<'a>(&'a self, key: &'a str) -> &'a str {
        self.outputs.get(key).map(|s| s.as_str()).unwrap_or(key)
    }
}

/// Root manifest structure from ggen.toml
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
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

    /// Pack declarations (resolved before generation)
    #[serde(default)]
    pub packs: Vec<PackRef>,

    /// Ignored config fields from ggen-config
    #[serde(default)]
    pub sync: Option<toml::Value>,
    #[serde(default)]
    pub rdf: Option<toml::Value>,
    #[serde(default)]
    pub templates: Option<toml::Value>,
    #[serde(default)]
    pub output: Option<toml::Value>,
    #[serde(default)]
    pub ai: Option<toml::Value>,
    #[serde(default)]
    pub sparql: Option<toml::Value>,
    #[serde(default)]
    pub lifecycle: Option<toml::Value>,
    #[serde(default)]
    pub security: Option<toml::Value>,
    #[serde(default)]
    pub performance: Option<toml::Value>,
    #[serde(default)]
    pub logging: Option<toml::Value>,
    #[serde(default)]
    pub telemetry: Option<toml::Value>,
    #[serde(default)]
    pub features: Option<toml::Value>,
    #[serde(default)]
    pub env: Option<toml::Value>,
}

/// Project metadata section
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProjectConfig {
    /// Project name (used in generated code headers)
    pub name: String,

    /// Semantic version
    pub version: String,

    /// Optional description
    #[serde(default)]
    pub description: Option<String>,

    /// Project authors (optional)
    #[serde(default)]
    pub authors: Option<Vec<String>>,

    /// Project license (optional)
    #[serde(default)]
    pub license: Option<String>,

    /// Project repository URL (optional)
    #[serde(default)]
    pub repository: Option<String>,
}

/// Ontology configuration section
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
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

    /// Use standard ontologies only
    #[serde(default)]
    pub standard_only: Option<bool>,
}

/// Inference configuration section
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(deny_unknown_fields)]
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
#[serde(deny_unknown_fields)]
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
#[serde(deny_unknown_fields)]
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

    /// Enable LLM-based auto-generation for skills (default: false)
    #[serde(default)]
    pub enable_llm: bool,

    /// LLM provider (groq, openai, anthropic, etc.)
    #[serde(default)]
    pub llm_provider: Option<String>,

    /// LLM model identifier
    #[serde(default)]
    pub llm_model: Option<String>,
}

/// A single generation rule
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
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

/// Source for a SPARQL query - file, inline, or pack output key
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum QuerySource {
    /// Load query from a declared pack's named output
    ///
    /// Example in ggen.toml:
    /// ```toml
    /// query = { pack = "wasm4pm-compat", output = "queries", file = "pm-rust-bridge.rq" }
    /// ```
    Pack {
        /// Pack name (must be declared in [[packs]])
        pack: String,
        /// Named output key from [pack.outputs] in the pack's package.toml
        output: String,
        /// File within that output directory
        file: PathBuf,
    },
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

/// Source for a Tera template - file, inline, git, pack output, or package manager
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum TemplateSource {
    /// Load template from a declared pack's named output
    ///
    /// Example in ggen.toml:
    /// ```toml
    /// template = { pack = "wasm4pm-compat", output = "templates", file = "rust-struct.tera" }
    /// ```
    Pack {
        /// Pack name (must be declared in [[packs]])
        pack: String,
        /// Named output key from [pack.outputs] in the pack's package.toml
        output: String,
        /// File within that output directory
        file: PathBuf,
    },
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
    /// Load template from a Git repository
    Git {
        /// Git repository URL
        git: String,
        /// Optional branch or tag
        branch: Option<String>,
        /// Path within the repository
        path: PathBuf,
    },
    /// Load template from a Package Manager (e.g. ggen marketplace)
    Package {
        /// Package name
        package: String,
        /// Optional version
        version: Option<String>,
        /// Path within the package
        path: PathBuf,
    },
}

/// File generation mode
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
pub enum GenerationMode {
    /// Create file on first pass; skip (no-op) if the file already exists.
    ///
    /// This is the default mode for scaffolding: the first `ggen sync` writes
    /// the file, subsequent syncs leave it untouched so manual edits are
    /// preserved. Use `Overwrite` to replace the file on every sync, or
    /// `Merge` to combine generated sections with hand-written sections.
    #[default]
    Create,
    /// Overwrite existing
    Overwrite,
    /// Merge with existing (marker-based)
    Merge,
}

/// Validation configuration section
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(deny_unknown_fields)]
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

    /// Elevate determinism warnings (e.g. missing ORDER BY) to hard errors
    #[serde(default)]
    pub strict_mode: bool,

    /// Custom validation rules (SPARQL ASK)
    #[serde(default)]
    pub rules: Vec<ValidationRule>,
}

/// A custom validation rule
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
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

impl Default for ProjectConfig {
    fn default() -> Self {
        Self {
            name: String::new(),
            version: String::new(),
            description: None,
            authors: None,
            license: None,
            repository: None,
        }
    }
}

impl Default for OntologyConfig {
    fn default() -> Self {
        Self {
            source: PathBuf::new(),
            imports: vec![],
            base_iri: None,
            prefixes: BTreeMap::new(),
            standard_only: None,
        }
    }
}

impl Default for GenerationConfig {
    fn default() -> Self {
        Self {
            rules: vec![],
            max_sparql_timeout_ms: default_sparql_timeout(),
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: default_output_dir(),
            enable_llm: false,
            llm_model: None,
            llm_provider: None,
        }
    }
}

impl Default for GgenManifest {
    fn default() -> Self {
        Self {
            project: ProjectConfig::default(),
            ontology: OntologyConfig::default(),
            inference: InferenceConfig::default(),
            generation: GenerationConfig::default(),
            validation: ValidationConfig::default(),
            packs: vec![],
            sync: None,
            rdf: None,
            templates: None,
            output: None,
            ai: None,
            sparql: None,
            lifecycle: None,
            security: None,
            performance: None,
            logging: None,
            telemetry: None,
            features: None,
            env: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_values() {
        assert_eq!(default_sparql_timeout(), 5000);
        assert_eq!(default_reasoning_timeout(), 5000);
        assert_eq!(default_output_dir(), PathBuf::from("."));
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
