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

    /// Marketplace configuration (FMEA validation settings for package operations)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub marketplace: Option<MarketplaceConfig>,

    /// Generation configuration (structural Poka-Yoke - directory separation, headers)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub generation: Option<GenerationSafetyConfig>,

    /// CODEOWNERS generation configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub codeowners: Option<CodeownersConfig>,
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

// ============================================================================
// Marketplace Configuration
// Settings for marketplace package operations (install, publish)
// ============================================================================

/// Marketplace configuration
///
/// Controls behavior when interacting with the ggen marketplace:
/// - FMEA validation during package installation
/// - Quality gates for package publishing
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct MarketplaceConfig {
    /// Validate package FMEA during installation
    #[serde(default)]
    pub fmea_validation: bool,

    /// Require packages to have [fmea] section in package.toml
    #[serde(default)]
    pub require_fmea: bool,

    /// RPN threshold for critical failures (reject packages with unmitigated modes above this)
    #[serde(default = "default_critical_threshold")]
    pub critical_threshold: u16,
}

impl Default for MarketplaceConfig {
    fn default() -> Self {
        Self {
            fmea_validation: false,
            require_fmea: false,
            critical_threshold: 200,
        }
    }
}

// ============================================================================
// FMEA Types (for package.toml, NOT ggen.toml)
// These types are used when parsing marketplace package metadata
// ============================================================================

/// FMEA control entry (used in package.toml [fmea] section)
///
/// This struct is used for parsing marketplace packages, NOT for ggen.toml.
/// FMEA documentation belongs in package metadata, not project configuration.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FmeaControl {
    /// Control identifier (e.g., "F1", "F2")
    pub id: String,

    /// Failure mode description
    pub mode: String,

    /// Severity score (1-10, where 10 = catastrophic)
    #[serde(default = "default_fmea_score")]
    pub severity: u8,

    /// Occurrence likelihood (1-10, where 10 = very frequent)
    #[serde(default = "default_fmea_score")]
    pub occurrence: u8,

    /// Detection capability (1-10, where 10 = undetectable)
    #[serde(default = "default_fmea_score")]
    pub detection: u8,

    /// Control measure implemented (required for modes with RPN > threshold)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub control: Option<String>,

    /// Evidence of control effectiveness
    #[serde(skip_serializing_if = "Option::is_none")]
    pub evidence: Option<String>,
}

impl FmeaControl {
    /// Calculate Risk Priority Number (Severity × Occurrence × Detection)
    #[must_use]
    pub fn rpn(&self) -> u16 {
        u16::from(self.severity) * u16::from(self.occurrence) * u16::from(self.detection)
    }

    /// Check if this failure mode is mitigated (has control defined)
    #[must_use]
    pub fn is_mitigated(&self) -> bool {
        self.control.is_some()
    }

    /// Check if this failure mode is critical and unmitigated
    #[must_use]
    pub fn is_critical_unmitigated(&self, threshold: u16) -> bool {
        self.rpn() > threshold && !self.is_mitigated()
    }
}

/// Package FMEA section (used in package.toml)
///
/// This is parsed from marketplace package.toml files, NOT ggen.toml.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct PackageFmeaSection {
    /// Failure mode controls documented for this package
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub controls: Vec<FmeaControl>,
}

// ============================================================================
// Generation Configuration (Path Protection & Poka-Yoke)
// Prevents accidental overwrites of domain logic
// ============================================================================

/// Generation configuration with path protection
///
/// Implements Poka-Yoke (error-proofing) patterns to prevent
/// accidental overwrites of domain logic during code generation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GenerationSafetyConfig {
    /// Enable generation safety checks
    #[serde(default = "default_true")]
    pub enabled: bool,

    /// Protected paths (glob patterns) - NEVER overwrite
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub protected_paths: Vec<String>,

    /// Regeneratable paths (glob patterns) - safe to overwrite
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub regenerate_paths: Vec<String>,

    /// Header to inject into generated files
    #[serde(skip_serializing_if = "Option::is_none")]
    pub generated_header: Option<String>,

    /// Require confirmation before overwriting any file
    #[serde(default)]
    pub require_confirmation: bool,

    /// Create backup before overwriting
    #[serde(default = "default_true")]
    pub backup_before_write: bool,

    /// Poka-yoke controls
    #[serde(skip_serializing_if = "Option::is_none")]
    pub poka_yoke: Option<PokaYokeSettings>,
}

/// Poka-yoke (error-proofing) settings
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PokaYokeSettings {
    /// Add warning headers to generated files
    #[serde(default = "default_true")]
    pub warning_headers: bool,

    /// Add generated files to .gitignore
    #[serde(default)]
    pub gitignore_generated: bool,

    /// Add generated files to .gitattributes (linguist-generated)
    #[serde(default)]
    pub gitattributes_generated: bool,

    /// Validate imports don't cross boundaries
    #[serde(default)]
    pub validate_imports: bool,
}

impl Default for GenerationSafetyConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            protected_paths: vec!["src/domain/**".to_string()],
            regenerate_paths: vec!["src/generated/**".to_string()],
            generated_header: Some(
                "// DO NOT EDIT - Generated by ggen. Changes will be overwritten.".to_string(),
            ),
            require_confirmation: false,
            backup_before_write: true,
            poka_yoke: Some(PokaYokeSettings::default()),
        }
    }
}

impl Default for PokaYokeSettings {
    fn default() -> Self {
        Self {
            warning_headers: true,
            gitignore_generated: false,
            gitattributes_generated: false,
            validate_imports: false,
        }
    }
}

// ============================================================================
// CODEOWNERS Configuration
// Aggregates team ownership from distributed OWNERS files
// ============================================================================

/// CODEOWNERS generation configuration
///
/// Aggregates team ownership from noun-level OWNERS files
/// into a single .github/CODEOWNERS file for GitHub PR approval enforcement.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CodeownersConfig {
    /// Enable CODEOWNERS generation
    #[serde(default)]
    pub enabled: bool,

    /// Source directories to scan for OWNERS files
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub source_dirs: Vec<String>,

    /// Base directories to generate entries for
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub base_dirs: Vec<String>,

    /// Output path (defaults to .github/CODEOWNERS)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_path: Option<String>,

    /// Auto-regenerate on noun changes
    #[serde(default)]
    pub auto_regenerate: bool,
}

// Default value functions for serde - Marketplace/FMEA
fn default_critical_threshold() -> u16 {
    200
}

fn default_fmea_score() -> u8 {
    5
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
            marketplace: None,
            generation: None,
            codeowners: None,
        }
    }
}

impl Default for CodeownersConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            source_dirs: vec!["ontology".to_string()],
            base_dirs: vec![
                "ontology".to_string(),
                "src/generated".to_string(),
                "src/domain".to_string(),
            ],
            output_path: None,
            auto_regenerate: false,
        }
    }
}
