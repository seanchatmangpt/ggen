//! Schema definitions for ggen.toml configuration
//!
//! This module defines the complete structure of ggen.toml files
//! using serde-compatible Rust structs.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Root configuration structure for ggen.toml
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GgenConfig {
    /// Project metadata
    pub project: ProjectConfig,

    /// AI configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ai: Option<AiConfig>,

    /// Templates configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub templates: Option<TemplatesConfig>,

    /// RDF configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rdf: Option<RdfConfig>,

    /// SPARQL configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sparql: Option<SparqlConfig>,

    /// Lifecycle configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lifecycle: Option<LifecycleConfig>,

    /// Security settings (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub security: Option<SecurityConfig>,

    /// Performance settings (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub performance: Option<PerformanceConfig>,

    /// Logging configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub logging: Option<LoggingConfig>,

    /// Diataxis documentation configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diataxis: Option<DiataxisConfig>,

    /// Feature flags (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub features: Option<HashMap<String, bool>>,

    /// Environment-specific overrides (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub env: Option<HashMap<String, serde_json::Value>>,

    /// Build configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub build: Option<BuildConfig>,

    /// Test configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub test: Option<TestConfig>,

    /// Package metadata (for marketplace packages)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<PackageMetadata>,
}

/// Project metadata configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ProjectConfig {
    /// Project name
    pub name: String,

    /// Project version
    pub version: String,

    /// Project description (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Project authors (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub authors: Option<Vec<String>>,

    /// Project license (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,

    /// Project repository URL (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,
}

/// AI provider configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AiConfig {
    /// AI provider (openai, ollama, anthropic, etc.)
    pub provider: String,

    /// Model name
    pub model: String,

    /// Temperature for generation (0.0 - 1.0)
    #[serde(default = "default_temperature")]
    pub temperature: f32,

    /// Maximum tokens for generation
    #[serde(default = "default_max_tokens")]
    pub max_tokens: u32,

    /// Request timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout: u32,

    /// System and user prompts (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prompts: Option<AiPrompts>,

    /// Validation settings (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub validation: Option<AiValidation>,
}

/// AI prompt configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AiPrompts {
    /// System prompt
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system: Option<String>,

    /// User prompt prefix
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user_prefix: Option<String>,
}

/// AI validation configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AiValidation {
    /// Whether validation is enabled
    #[serde(default)]
    pub enabled: bool,

    /// Quality threshold (0.0 - 1.0)
    #[serde(default = "default_quality_threshold")]
    pub quality_threshold: f32,

    /// Maximum validation iterations
    #[serde(default = "default_max_iterations")]
    pub max_iterations: u32,
}

/// Templates configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TemplatesConfig {
    /// Template source directory
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directory: Option<String>,

    /// Output directory for generated files
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_directory: Option<String>,

    /// Enable backup before overwriting
    #[serde(default)]
    pub backup_enabled: bool,

    /// Idempotent generation (only update if changed)
    #[serde(default)]
    pub idempotent: bool,
}

/// Diataxis documentation configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DiataxisConfig {
    /// Root directory for diataxis docs (defaults to docs)
    #[serde(default = "default_diataxis_root")]
    pub root: String,

    /// Index file path
    #[serde(default = "default_diataxis_index")]
    pub index: String,

    /// Quadrant-specific configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub quadrants: Option<DiataxisQuadrants>,
}

/// Quadrant configuration (tutorials, how-to, reference, explanations)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct DiataxisQuadrants {
    /// Tutorials quadrant configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tutorials: Option<DiataxisSection>,

    /// How-to guides quadrant configuration
    #[serde(rename = "how-to", skip_serializing_if = "Option::is_none")]
    pub how_to: Option<DiataxisSection>,

    /// Reference quadrant configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reference: Option<DiataxisSection>,

    /// Explanations quadrant configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub explanations: Option<DiataxisSection>,
}

/// Per-quadrant configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DiataxisSection {
    /// Source directory for existing docs
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,

    /// Output directory for generated docs
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output: Option<String>,

    /// Navigation entries for this quadrant
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub navigation: Vec<DiataxisNavItem>,
}

/// Navigation entry within a quadrant
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DiataxisNavItem {
    /// Display title for the navigation entry
    pub title: String,

    /// Relative or absolute path to the target content
    pub path: String,

    /// Optional description shown alongside the entry
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

/// RDF configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct RdfConfig {
    /// Base IRI/URI for RDF entities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub base_uri: Option<String>,

    /// Base IRI (alternative name)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub base_iri: Option<String>,

    /// RDF namespace prefixes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prefixes: Option<HashMap<String, String>>,

    /// Default RDF format (turtle, rdfxml, etc.)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_format: Option<String>,

    /// Enable query caching
    #[serde(default)]
    pub cache_queries: bool,
}

/// SPARQL configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SparqlConfig {
    /// Query timeout in seconds
    #[serde(default = "default_sparql_timeout")]
    pub timeout: u32,

    /// Maximum results per query
    #[serde(default = "default_max_results")]
    pub max_results: u32,

    /// Enable query caching
    #[serde(default)]
    pub cache_enabled: bool,
}

/// Lifecycle configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct LifecycleConfig {
    /// Enable lifecycle management
    #[serde(default)]
    pub enabled: bool,

    /// Lifecycle config file (e.g., make.toml)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config_file: Option<String>,

    /// Cache directory
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_directory: Option<String>,

    /// State file path
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state_file: Option<String>,

    /// Lifecycle phases
    #[serde(skip_serializing_if = "Option::is_none")]
    pub phases: Option<HashMap<String, Vec<String>>>,
}

/// Security configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SecurityConfig {
    /// Enable path traversal protection
    #[serde(default = "default_true")]
    pub path_traversal_protection: bool,

    /// Enable shell injection protection
    #[serde(default = "default_true")]
    pub shell_injection_protection: bool,

    /// Enable template sandboxing
    #[serde(default = "default_true")]
    pub template_sandboxing: bool,

    /// Validate file paths
    #[serde(default = "default_true")]
    pub validate_paths: bool,

    /// Require user confirmation for destructive operations
    #[serde(default)]
    pub require_confirmation: bool,

    /// Audit all operations
    #[serde(default)]
    pub audit_operations: bool,

    /// Backup before write operations
    #[serde(default)]
    pub backup_before_write: bool,
}

/// Performance configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PerformanceConfig {
    /// Enable parallel execution
    #[serde(default)]
    pub parallel_execution: bool,

    /// Maximum parallel workers
    #[serde(default = "default_max_workers")]
    pub max_workers: u32,

    /// Cache size (as string, e.g., "1GB")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_size: Option<String>,

    /// Enable profiling
    #[serde(default)]
    pub enable_profiling: bool,

    /// Memory limit in MB
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memory_limit_mb: Option<u32>,
}

/// Logging configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct LoggingConfig {
    /// Log level (debug, info, warn, error)
    #[serde(default = "default_log_level")]
    pub level: String,

    /// Log format (json, text)
    #[serde(default = "default_log_format")]
    pub format: String,

    /// Log file path (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub file: Option<String>,

    /// Log rotation (daily, size-based, etc.)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rotation: Option<String>,
}

/// Build configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct BuildConfig {
    /// Build target (release, debug)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,

    /// Features to enable
    #[serde(skip_serializing_if = "Option::is_none")]
    pub features: Option<Vec<String>>,

    /// Build profile
    #[serde(skip_serializing_if = "Option::is_none")]
    pub profile: Option<String>,

    /// Number of parallel build jobs
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parallel_jobs: Option<u32>,
}

/// Test configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TestConfig {
    /// Test framework
    #[serde(skip_serializing_if = "Option::is_none")]
    pub framework: Option<String>,

    /// Enable parallel test execution
    #[serde(default)]
    pub parallel: bool,

    /// Test timeout in seconds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeout_seconds: Option<u32>,

    /// Enable code coverage
    #[serde(default)]
    pub coverage_enabled: bool,

    /// Coverage threshold percentage
    #[serde(skip_serializing_if = "Option::is_none")]
    pub coverage_threshold: Option<u32>,
}

/// Package metadata (for marketplace packages)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PackageMetadata {
    /// Package name
    pub name: String,

    /// Package version
    pub version: String,

    /// Description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Authors
    #[serde(skip_serializing_if = "Option::is_none")]
    pub authors: Option<Vec<String>>,

    /// License
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,

    /// Repository URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,

    /// Keywords
    #[serde(skip_serializing_if = "Option::is_none")]
    pub keywords: Option<Vec<String>>,

    /// Categories
    #[serde(skip_serializing_if = "Option::is_none")]
    pub categories: Option<Vec<String>>,

    /// Package-specific metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

// Default value functions for serde
fn default_temperature() -> f32 {
    0.7
}

fn default_max_tokens() -> u32 {
    2000
}

fn default_timeout() -> u32 {
    30
}

fn default_quality_threshold() -> f32 {
    0.8
}

fn default_max_iterations() -> u32 {
    3
}

fn default_sparql_timeout() -> u32 {
    10
}

fn default_max_results() -> u32 {
    1000
}

fn default_max_workers() -> u32 {
    num_cpus()
}

fn default_log_level() -> String {
    "info".to_string()
}

fn default_log_format() -> String {
    "text".to_string()
}

fn default_diataxis_root() -> String {
    "docs".to_string()
}

fn default_diataxis_index() -> String {
    "docs/diataxis-index.md".to_string()
}

fn default_true() -> bool {
    true
}

fn num_cpus() -> u32 {
    std::thread::available_parallelism()
        .map(|n| n.get() as u32)
        .unwrap_or(4)
}

impl Default for GgenConfig {
    fn default() -> Self {
        Self {
            project: ProjectConfig {
                name: "unnamed".to_string(),
                version: "0.1.0".to_string(),
                description: None,
                authors: None,
                license: None,
                repository: None,
            },
            ai: None,
            templates: None,
            rdf: None,
            sparql: None,
            lifecycle: None,
            security: None,
            performance: None,
            logging: None,
            diataxis: None,
            features: None,
            env: None,
            build: None,
            test: None,
            package: None,
        }
    }
}
