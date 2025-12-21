//! Code generation module
//!
//! This module provides generators for different target languages and frameworks,
//! converting OntologySchema into working code for TypeScript, GraphQL, SQL, and more.
//!
//! ## Semantic Code Generation (v5)
//!
//! The new semantic code generation pipeline transforms RDF ontologies through
//! CONSTRUCT queries into typed code graphs, which are then rendered via Tera templates.
//!
//! ### Pipeline Flow
//!
//! ```text
//! ontology.ttl → Graph → CONSTRUCT rules → Code Graph → Tera → .rs files
//! ```
//!
//! ### Key Types
//!
//! - [`GenerationPipeline`] - Orchestrates the full generation flow
//! - [`CodeGraphBuilder`] - Converts SPARQL results to code entities
//! - [`AuditTrailBuilder`] - Tracks execution for determinism verification

pub mod audit;
pub mod code_graph;
pub mod executor;
pub mod merge;
pub mod pipeline;
pub mod typescript;
pub mod watch;

// Re-export key types
pub use audit::{AuditOutput, AuditStep, AuditTrail, AuditTrailBuilder};
pub use code_graph::{
    CodeEnum, CodeField, CodeGraphBuilder, CodeImpl, CodeImport, CodeItem, CodeMethod, CodeModule,
    CodeParam, CodeStruct, CodeTrait, CodeVariant,
};
pub use executor::{SyncExecutor, SyncResult, SyncedFileInfo, ValidationCheck};
pub use merge::{merge_sections, parse_merge_markers, MergeMarkers, MergedSections};
pub use pipeline::{
    ExecutedRule, GeneratedFile, GenerationPipeline, PipelineState, RuleType, ValidationResult,
    ValidationSeverity,
};
pub use typescript::TypeScriptGenerator;
pub use watch::{collect_watch_paths, FileWatcher, WatchEvent};

// ============================================================================
// Sync Options (CLI Configuration for ggen sync)
// ============================================================================

/// Output format for `ggen sync` command results
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputFormat {
    /// Human-readable text output (default)
    #[default]
    Text,
    /// JSON output for CI/CD and automation
    Json,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "text" => Ok(OutputFormat::Text),
            "json" => Ok(OutputFormat::Json),
            _ => Err(format!(
                "Invalid output format: '{}'. Expected 'text' or 'json'",
                s
            )),
        }
    }
}

impl std::fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputFormat::Text => write!(f, "text"),
            OutputFormat::Json => write!(f, "json"),
        }
    }
}

/// Configuration options for `ggen sync` pipeline execution
///
/// These options control the behavior of the code synchronization pipeline,
/// including dry-run mode, audit trail generation, and timeout limits.
#[derive(Debug, Clone)]
pub struct SyncOptions {
    /// Path to the manifest file (default: ggen.toml)
    pub manifest_path: std::path::PathBuf,

    /// Override output directory from manifest
    pub output_dir: Option<std::path::PathBuf>,

    /// Preview changes without writing files
    pub dry_run: bool,

    /// Force overwrite of protected files
    pub force: bool,

    /// Generate audit trail (audit.json)
    pub audit: bool,

    /// Run only specific rules (by name)
    pub selected_rules: Option<Vec<String>>,

    /// Verbose output during execution
    pub verbose: bool,

    /// Watch for file changes and auto-regenerate
    pub watch: bool,

    /// Validate manifest without generating
    pub validate_only: bool,

    /// Output format for results
    pub output_format: OutputFormat,

    /// Maximum execution timeout in milliseconds
    pub timeout_ms: u64,
}

impl Default for SyncOptions {
    fn default() -> Self {
        Self {
            manifest_path: std::path::PathBuf::from("ggen.toml"),
            output_dir: None,
            dry_run: false,
            force: false,
            audit: false,
            selected_rules: None,
            verbose: false,
            watch: false,
            validate_only: false,
            output_format: OutputFormat::default(),
            timeout_ms: 30000, // 30 second default
        }
    }
}

impl SyncOptions {
    /// Create new SyncOptions with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Create SyncOptions from a manifest path
    pub fn from_manifest<P: AsRef<std::path::Path>>(path: P) -> Self {
        Self {
            manifest_path: path.as_ref().to_path_buf(),
            ..Self::default()
        }
    }

    /// Set dry-run mode
    pub fn with_dry_run(mut self, dry_run: bool) -> Self {
        self.dry_run = dry_run;
        self
    }

    /// Set verbose output
    pub fn with_verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Set audit trail generation
    pub fn with_audit(mut self, audit: bool) -> Self {
        self.audit = audit;
        self
    }

    /// Set validate-only mode
    pub fn with_validate_only(mut self, validate_only: bool) -> Self {
        self.validate_only = validate_only;
        self
    }

    /// Set output format
    pub fn with_output_format(mut self, format: OutputFormat) -> Self {
        self.output_format = format;
        self
    }

    /// Set timeout in milliseconds
    pub fn with_timeout_ms(mut self, timeout_ms: u64) -> Self {
        self.timeout_ms = timeout_ms;
        self
    }

    /// Set selected rules
    pub fn with_rules(mut self, rules: Vec<String>) -> Self {
        self.selected_rules = Some(rules);
        self
    }

    /// Set force overwrite
    pub fn with_force(mut self, force: bool) -> Self {
        self.force = force;
        self
    }

    /// Set output directory override
    pub fn with_output_dir<P: AsRef<std::path::Path>>(mut self, dir: P) -> Self {
        self.output_dir = Some(dir.as_ref().to_path_buf());
        self
    }
}
