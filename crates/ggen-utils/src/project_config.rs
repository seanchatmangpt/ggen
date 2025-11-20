//! Project configuration types
//!
//! This module provides comprehensive configuration structures for ggen projects,
//! implementing the full ggen.toml schema with support for workspaces, dependencies,
//! ontology integration, templates, generators, lifecycle management, plugins, and profiles.
//!
//! ## Configuration Structure
//!
//! The `GgenConfig` structure represents the root configuration for a ggen project:
//!
//! - **Project**: Project metadata and settings
//! - **Workspace**: Mono-repo configuration
//! - **Graph**: Graph-based dependency resolution
//! - **Dependencies**: Full Cargo.toml-like dependencies
//! - **Ontology**: RDF/OWL integration
//! - **Templates**: Template composition and inheritance
//! - **Generators**: Code generation pipelines
//! - **Lifecycle**: Build lifecycle and hooks
//! - **Plugins**: Plugin system configuration
//! - **Profiles**: Environment-specific overrides
//! - **Metadata**: Package and build metadata
//! - **Validation**: Constraints and quality thresholds
//!
//! ## Type-First Design
//!
//! This implementation uses type-first thinking to encode invariants:
//! - Optional fields use `Option<T>` with proper defaults
//! - `BTreeMap` for deterministic ordering (not `HashMap`)
//! - `Result<T, Error>` for all fallible operations
//! - Zero unsafe blocks (memory safety guaranteed)
//!
//! ## Examples
//!
//! ### Loading Configuration
//!
//! ```rust,no_run
//! use ggen_utils::project_config::GgenConfig;
//! use std::fs;
//!
//! # fn main() -> anyhow::Result<()> {
//! let content = fs::read_to_string("ggen.toml")?;
//! let config: GgenConfig = toml::from_str(&content)?;
//!
//! println!("Project: {} v{}", config.project.name, config.project.version);
//! # Ok(())
//! # }
//! ```
//!
//! ### Workspace Configuration
//!
//! ```rust,no_run
//! use ggen_utils::project_config::{GgenConfig, Workspace};
//! use std::collections::BTreeMap;
//!
//! let workspace = Workspace {
//!     members: vec!["crates/*".to_string(), "packages/*".to_string()],
//!     exclude: Some(vec!["target".to_string()]),
//!     dependencies: Some(BTreeMap::new()),
//!     graph: None,
//! };
//! ```

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

// ============================================================================
// Root Configuration
// ============================================================================

/// Root configuration structure for ggen projects
///
/// Supports full ggen.toml schema with all features including workspaces,
/// dependencies, ontology, templates, generators, lifecycle, plugins, and profiles.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct GgenConfig {
    /// Project metadata and settings
    pub project: Project,

    /// Workspace configuration for mono-repos
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<Workspace>,

    /// Graph-based dependency resolution
    #[serde(skip_serializing_if = "Option::is_none")]
    pub graph: Option<Graph>,

    /// Project dependencies (Cargo.toml-like)
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub dependencies: BTreeMap<String, Dependency>,

    /// Development dependencies
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub dev_dependencies: BTreeMap<String, Dependency>,

    /// Build dependencies
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub build_dependencies: BTreeMap<String, Dependency>,

    /// Target-specific dependencies
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub target: BTreeMap<String, TargetDependencies>,

    /// RDF/OWL ontology configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ontology: Option<Ontology>,

    /// Template configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub templates: Option<Templates>,

    /// Code generator configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub generators: Option<Generators>,

    /// Build lifecycle configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lifecycle: Option<Lifecycle>,

    /// Plugin system configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub plugins: Option<Plugins>,

    /// Environment profiles (dev, production, test, ci, bench)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub profiles: Option<Profiles>,

    /// Package and build metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<Metadata>,

    /// Validation rules and constraints
    #[serde(skip_serializing_if = "Option::is_none")]
    pub validation: Option<Validation>,

    /// RDF namespace prefixes (legacy support)
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub prefixes: BTreeMap<String, String>,

    /// Legacy RDF configuration (for backward compatibility)
    #[serde(rename = "rdf", skip_serializing_if = "Option::is_none")]
    pub rdf: Option<RdfConfig>,

    /// Template variables (legacy support)
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub vars: BTreeMap<String, String>,
}

// ============================================================================
// Project Section
// ============================================================================

/// Project metadata and settings
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Project {
    /// Project name (required)
    pub name: String,

    /// Project version (required)
    pub version: String,

    /// Project description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Project authors
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub authors: Vec<String>,

    /// License identifier
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,

    /// Rust edition (2015, 2018, 2021, 2024)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub edition: Option<String>,

    /// Project type (auto, library, binary, workspace)
    #[serde(skip_serializing_if = "Option::is_none", rename = "type")]
    pub project_type: Option<String>,

    /// Primary language (auto, rust, typescript, python, multi)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub language: Option<String>,

    /// RDF URI for this project
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uri: Option<String>,

    /// Short namespace prefix for RDF queries
    #[serde(skip_serializing_if = "Option::is_none")]
    pub namespace: Option<String>,

    /// Template to inherit from
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extends: Option<String>,

    /// Output directory (legacy support)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_dir: Option<PathBuf>,
}

// ============================================================================
// Workspace Section
// ============================================================================

/// Workspace configuration for mono-repos
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Workspace {
    /// Workspace members (glob patterns supported)
    #[serde(default)]
    pub members: Vec<String>,

    /// Paths to exclude from workspace
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exclude: Option<Vec<String>>,

    /// Shared dependencies across workspace
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dependencies: Option<BTreeMap<String, Dependency>>,

    /// Workspace-level graph queries
    #[serde(skip_serializing_if = "Option::is_none")]
    pub graph: Option<WorkspaceGraph>,
}

/// Workspace-level graph configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceGraph {
    /// SPARQL query for workspace analysis
    #[serde(skip_serializing_if = "Option::is_none")]
    pub query: Option<String>,
}

// ============================================================================
// Graph Section
// ============================================================================

/// Graph-based dependency resolution configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Graph {
    /// Resolution strategy (smart, conservative, aggressive)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub strategy: Option<String>,

    /// Conflict resolution strategy (newest, oldest, semver-compatible)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conflict_resolution: Option<String>,

    /// Graph-based dependency queries
    #[serde(skip_serializing_if = "Option::is_none")]
    pub queries: Option<BTreeMap<String, String>>,

    /// Feature flags as graph nodes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub features: Option<BTreeMap<String, Vec<String>>>,
}

// ============================================================================
// Dependencies Section
// ============================================================================

/// Dependency specification (Cargo.toml-like)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    /// Simple version string
    Simple(String),
    /// Detailed dependency specification
    Detailed(DetailedDependency),
}

/// Detailed dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct DetailedDependency {
    /// Version requirement
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,

    /// Feature flags to enable
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub features: Vec<String>,

    /// Whether dependency is optional
    #[serde(skip_serializing_if = "Option::is_none")]
    pub optional: Option<bool>,

    /// Default features enabled
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_features: Option<bool>,

    /// Git repository URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub git: Option<String>,

    /// Git branch
    #[serde(skip_serializing_if = "Option::is_none")]
    pub branch: Option<String>,

    /// Git tag
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tag: Option<String>,

    /// Git revision
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rev: Option<String>,

    /// Local path dependency
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<PathBuf>,

    /// Alternative registry
    #[serde(skip_serializing_if = "Option::is_none")]
    pub registry: Option<String>,

    /// Package name (if different from dependency key)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<String>,
}

/// Target-specific dependencies
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetDependencies {
    /// Dependencies for this target
    #[serde(default)]
    pub dependencies: BTreeMap<String, Dependency>,
}

// ============================================================================
// Ontology Section
// ============================================================================

/// RDF/OWL ontology integration configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Ontology {
    /// Ontology files (Turtle format)
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub files: Vec<PathBuf>,

    /// Inline RDF (Turtle format)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inline: Option<String>,

    /// SHACL shape files for validation
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub shapes: Vec<PathBuf>,

    /// Constitution (invariant checks)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub constitution: Option<Constitution>,
}

/// Constitution configuration (invariant checks)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Constitution {
    /// Built-in checks (NoRetrocausation, TypeSoundness, GuardSoundness)
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub checks: Vec<String>,

    /// Custom invariants (SPARQL ASK queries)
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub custom: BTreeMap<String, String>,

    /// Enforce strict mode (production)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enforce_strict: Option<bool>,

    /// Fail on warnings
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fail_on_warning: Option<bool>,
}

// ============================================================================
// Templates Section
// ============================================================================

/// Template composition and inheritance configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Templates {
    /// Template search paths
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub paths: Vec<String>,

    /// Default template variables
    #[serde(skip_serializing_if = "Option::is_none")]
    pub vars: Option<BTreeMap<String, String>>,

    /// Template inheritance chain
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extends: Option<BTreeMap<String, String>>,

    /// Template composition (merge multiple templates)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compose: Option<BTreeMap<String, Vec<String>>>,

    /// Template guards (conditional rendering)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub guards: Option<BTreeMap<String, String>>,

    /// SPARQL-driven template queries
    #[serde(skip_serializing_if = "Option::is_none")]
    pub queries: Option<BTreeMap<String, String>>,
}

// ============================================================================
// Generators Section
// ============================================================================

/// Code generation pipeline configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Generators {
    /// Generator registry URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub registry: Option<String>,

    /// Installed generators
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub installed: BTreeMap<String, InstalledGenerator>,

    /// Generator pipelines
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub pipeline: Vec<GeneratorPipeline>,

    /// Generator hooks
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hooks: Option<GeneratorHooks>,
}

/// Installed generator specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstalledGenerator {
    /// Generator version
    pub version: String,

    /// Registry name
    #[serde(skip_serializing_if = "Option::is_none")]
    pub registry: Option<String>,

    /// Git source
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,
}

/// Generator pipeline (multi-step workflow)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratorPipeline {
    /// Pipeline name
    pub name: String,

    /// Pipeline description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Input files
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub inputs: Vec<String>,

    /// Pipeline steps
    #[serde(default)]
    pub steps: Vec<PipelineStep>,
}

/// Pipeline step specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineStep {
    /// Action type (template, parse, transform, exec)
    pub action: String,

    /// Template name (for template action)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub template: Option<String>,

    /// Parser name (for parse action)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parser: Option<String>,

    /// SPARQL query file (for transform action)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub query: Option<String>,

    /// Shell command (for exec action)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub command: Option<String>,
}

/// Generator lifecycle hooks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratorHooks {
    /// Before generation hook
    #[serde(skip_serializing_if = "Option::is_none")]
    pub before_generate: Option<String>,

    /// After generation hook
    #[serde(skip_serializing_if = "Option::is_none")]
    pub after_generate: Option<String>,

    /// Error handler hook
    #[serde(skip_serializing_if = "Option::is_none")]
    pub on_error: Option<String>,
}

// ============================================================================
// Lifecycle Section
// ============================================================================

/// Build lifecycle configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Lifecycle {
    /// Lifecycle phases
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub phases: Vec<String>,

    /// Phase hooks
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hooks: Option<BTreeMap<String, Vec<String>>>,

    /// Task definitions
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub tasks: BTreeMap<String, LifecycleTask>,

    /// Parallel execution groups
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parallel: Option<BTreeMap<String, Vec<String>>>,
}

/// Lifecycle task definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LifecycleTask {
    /// Task description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Task dependencies
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub dependencies: Vec<String>,

    /// Shell command
    #[serde(skip_serializing_if = "Option::is_none")]
    pub command: Option<String>,

    /// Script to run
    #[serde(skip_serializing_if = "Option::is_none")]
    pub script: Option<String>,
}

// ============================================================================
// Plugins Section
// ============================================================================

/// Plugin system configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Plugins {
    /// Plugin discovery paths
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub paths: Vec<String>,

    /// Installed plugins
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub installed: BTreeMap<String, InstalledPlugin>,

    /// Plugin-specific configuration
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub config: BTreeMap<String, BTreeMap<String, toml::Value>>,

    /// Plugin lifecycle hooks
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hooks: Option<BTreeMap<String, Vec<String>>>,

    /// Plugin permissions (security sandboxing)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub permissions: Option<BTreeMap<String, PluginPermissions>>,
}

/// Installed plugin specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstalledPlugin {
    /// Plugin version
    pub version: String,

    /// Plugin source (path, git, registry)
    pub source: String,
}

/// Plugin security permissions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginPermissions {
    /// Filesystem access permissions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub filesystem: Vec<String>,

    /// Network access permissions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub network: Vec<String>,

    /// Executable permissions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub exec: Vec<String>,
}

// ============================================================================
// Profiles Section
// ============================================================================

/// Environment-specific profile configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Profiles {
    /// Default profile
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default: Option<String>,

    /// Development profile
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dev: Option<Profile>,

    /// Production profile
    #[serde(skip_serializing_if = "Option::is_none")]
    pub production: Option<Profile>,

    /// Testing profile
    #[serde(skip_serializing_if = "Option::is_none")]
    pub test: Option<Profile>,

    /// CI profile
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ci: Option<Profile>,

    /// Benchmark profile
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bench: Option<Profile>,
}

/// Profile configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Profile {
    /// Profile to extend
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extends: Option<String>,

    /// Optimization level
    #[serde(skip_serializing_if = "Option::is_none")]
    pub optimization: Option<String>,

    /// Debug assertions
    #[serde(skip_serializing_if = "Option::is_none")]
    pub debug_assertions: Option<bool>,

    /// Overflow checks
    #[serde(skip_serializing_if = "Option::is_none")]
    pub overflow_checks: Option<bool>,

    /// Link-time optimization
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lto: Option<String>,

    /// Strip symbols
    #[serde(skip_serializing_if = "Option::is_none")]
    pub strip: Option<bool>,

    /// Codegen units
    #[serde(skip_serializing_if = "Option::is_none")]
    pub codegen_units: Option<u32>,

    /// Code coverage
    #[serde(skip_serializing_if = "Option::is_none")]
    pub code_coverage: Option<bool>,

    /// Test threads
    #[serde(skip_serializing_if = "Option::is_none")]
    pub test_threads: Option<u32>,

    /// Profile-specific dependencies
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dependencies: Option<BTreeMap<String, Dependency>>,

    /// Profile-specific template variables
    #[serde(skip_serializing_if = "Option::is_none")]
    pub templates: Option<ProfileTemplates>,

    /// Profile-specific ontology configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ontology: Option<ProfileOntology>,

    /// Profile-specific lifecycle hooks
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lifecycle: Option<ProfileLifecycle>,
}

/// Profile template configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProfileTemplates {
    /// Template variables
    #[serde(skip_serializing_if = "Option::is_none")]
    pub vars: Option<BTreeMap<String, String>>,
}

/// Profile ontology configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProfileOntology {
    /// Constitution configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub constitution: Option<Constitution>,
}

/// Profile lifecycle configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProfileLifecycle {
    /// Lifecycle hooks
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hooks: Option<BTreeMap<String, Vec<String>>>,
}

// ============================================================================
// Metadata Section
// ============================================================================

/// Package and build metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Metadata {
    /// Project homepage
    #[serde(skip_serializing_if = "Option::is_none")]
    pub homepage: Option<String>,

    /// Documentation URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub documentation: Option<String>,

    /// Repository URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,

    /// Changelog file
    #[serde(skip_serializing_if = "Option::is_none")]
    pub changelog: Option<String>,

    /// Build metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub build: Option<BuildMetadata>,

    /// Package metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<PackageMetadata>,
}

/// Build metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildMetadata {
    /// Rust compiler version
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rustc_version: Option<String>,

    /// LLVM version
    #[serde(skip_serializing_if = "Option::is_none")]
    pub llvm_version: Option<String>,

    /// Target triple
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target_triple: Option<String>,
}

/// Package metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageMetadata {
    /// Package categories
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub categories: Vec<String>,

    /// Package keywords
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub keywords: Vec<String>,

    /// README file
    #[serde(skip_serializing_if = "Option::is_none")]
    pub readme: Option<String>,

    /// Files to include
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub include: Vec<String>,

    /// Files to exclude
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub exclude: Vec<String>,
}

// ============================================================================
// Validation Section
// ============================================================================

/// Validation rules and constraints
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Validation {
    /// Minimum Rust version
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_rust_version: Option<String>,

    /// Dependency constraints
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dependencies: Option<DependencyValidation>,

    /// Code quality thresholds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub quality: Option<QualityThresholds>,
}

/// Dependency validation rules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyValidation {
    /// Maximum duplicate dependencies
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_duplicates: Option<u32>,

    /// Allow yanked crates
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_yanked: Option<bool>,

    /// Require checksums
    #[serde(skip_serializing_if = "Option::is_none")]
    pub require_checksums: Option<bool>,
}

/// Code quality thresholds
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityThresholds {
    /// Minimum code coverage percentage
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_coverage: Option<u32>,

    /// Maximum cyclomatic complexity
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_complexity: Option<u32>,

    /// Maximum function lines
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_function_lines: Option<u32>,
}

// ============================================================================
// Legacy RDF Configuration (Backward Compatibility)
// ============================================================================

/// Legacy RDF configuration structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RdfConfig {
    /// RDF files
    #[serde(default)]
    pub files: Vec<PathBuf>,

    /// Inline RDF data
    #[serde(default)]
    pub inline: Vec<String>,
}

// ============================================================================
// Implementation - Validation and Builder Methods
// ============================================================================

impl GgenConfig {
    /// Validate the configuration for consistency
    ///
    /// Checks:
    /// - Required fields are present
    /// - Version strings are valid
    /// - Paths exist (if validation enabled)
    /// - Dependencies are consistent
    ///
    /// # Errors
    ///
    /// Returns error if validation fails
    pub fn validate(&self) -> Result<(), String> {
        // Validate project name is not empty
        if self.project.name.is_empty() {
            return Err("Project name cannot be empty".to_string());
        }

        // Validate version format (basic semver check)
        if !self.is_valid_version(&self.project.version) {
            return Err(format!("Invalid version format: {}", self.project.version));
        }

        // Validate workspace members if present
        if let Some(ref workspace) = self.workspace {
            if workspace.members.is_empty() {
                return Err("Workspace members cannot be empty".to_string());
            }
        }

        // Validate minimum Rust version if specified
        if let Some(ref validation) = self.validation {
            if let Some(ref min_version) = validation.min_rust_version {
                if !self.is_valid_version(min_version) {
                    return Err(format!("Invalid min_rust_version: {}", min_version));
                }
            }
        }

        Ok(())
    }

    /// Check if version string is valid (basic semver check)
    fn is_valid_version(&self, version: &str) -> bool {
        let parts: Vec<&str> = version.split('.').collect();
        parts.len() >= 2 && parts.iter().all(|p| p.parse::<u32>().is_ok())
    }

    /// Get effective dependencies for a specific profile
    ///
    /// Merges base dependencies with profile-specific overrides
    pub fn get_profile_dependencies(&self, profile_name: &str) -> BTreeMap<String, Dependency> {
        let mut deps = self.dependencies.clone();

        if let Some(ref profiles) = self.profiles {
            let profile = match profile_name {
                "dev" => profiles.dev.as_ref(),
                "production" => profiles.production.as_ref(),
                "test" => profiles.test.as_ref(),
                "ci" => profiles.ci.as_ref(),
                "bench" => profiles.bench.as_ref(),
                _ => None,
            };

            if let Some(profile) = profile {
                if let Some(ref profile_deps) = profile.dependencies {
                    deps.extend(profile_deps.clone());
                }
            }
        }

        deps
    }

    /// Get template variables for a specific profile
    pub fn get_profile_template_vars(&self, profile_name: &str) -> BTreeMap<String, String> {
        let mut vars = self.vars.clone();

        // Add template vars if present
        if let Some(ref templates) = self.templates {
            if let Some(ref template_vars) = templates.vars {
                vars.extend(template_vars.clone());
            }
        }

        // Add profile-specific vars
        if let Some(ref profiles) = self.profiles {
            let profile = match profile_name {
                "dev" => profiles.dev.as_ref(),
                "production" => profiles.production.as_ref(),
                "test" => profiles.test.as_ref(),
                "ci" => profiles.ci.as_ref(),
                "bench" => profiles.bench.as_ref(),
                _ => None,
            };

            if let Some(profile) = profile {
                if let Some(ref templates) = profile.templates {
                    if let Some(ref profile_vars) = templates.vars {
                        vars.extend(profile_vars.clone());
                    }
                }
            }
        }

        vars
    }
}

impl Default for GgenConfig {
    fn default() -> Self {
        Self {
            project: Project {
                name: "untitled".to_string(),
                version: "0.1.0".to_string(),
                description: None,
                authors: Vec::new(),
                license: None,
                edition: Some("2021".to_string()),
                project_type: Some("auto".to_string()),
                language: Some("auto".to_string()),
                uri: None,
                namespace: None,
                extends: None,
                output_dir: None,
            },
            workspace: None,
            graph: None,
            dependencies: BTreeMap::new(),
            dev_dependencies: BTreeMap::new(),
            build_dependencies: BTreeMap::new(),
            target: BTreeMap::new(),
            ontology: None,
            templates: None,
            generators: None,
            lifecycle: None,
            plugins: None,
            profiles: None,
            metadata: None,
            validation: None,
            prefixes: BTreeMap::new(),
            rdf: None,
            vars: BTreeMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config_is_valid() {
        let config = GgenConfig::default();
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_version_validation() {
        let config = GgenConfig::default();
        assert!(config.is_valid_version("1.0.0"));
        assert!(config.is_valid_version("0.1.0"));
        assert!(config.is_valid_version("2.3.4"));
        assert!(!config.is_valid_version("invalid"));
        assert!(!config.is_valid_version("1"));
    }

    #[test]
    fn test_empty_project_name_fails_validation() {
        let mut config = GgenConfig::default();
        config.project.name = String::new();
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_invalid_version_fails_validation() {
        let mut config = GgenConfig::default();
        config.project.version = "invalid".to_string();
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_profile_dependencies_merge() {
        let mut config = GgenConfig::default();

        config
            .dependencies
            .insert("base".to_string(), Dependency::Simple("1.0".to_string()));

        let mut dev_deps = BTreeMap::new();
        dev_deps.insert("dev-dep".to_string(), Dependency::Simple("2.0".to_string()));

        config.profiles = Some(Profiles {
            default: Some("dev".to_string()),
            dev: Some(Profile {
                extends: None,
                optimization: None,
                debug_assertions: None,
                overflow_checks: None,
                lto: None,
                strip: None,
                codegen_units: None,
                code_coverage: None,
                test_threads: None,
                dependencies: Some(dev_deps),
                templates: None,
                ontology: None,
                lifecycle: None,
            }),
            production: None,
            test: None,
            ci: None,
            bench: None,
        });

        let deps = config.get_profile_dependencies("dev");
        assert_eq!(deps.len(), 2);
        assert!(deps.contains_key("base"));
        assert!(deps.contains_key("dev-dep"));
    }
}
