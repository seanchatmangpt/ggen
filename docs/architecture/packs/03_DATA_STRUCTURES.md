# Pack System: Data Structures and Rust Trait Definitions

## Overview

This document defines the core data structures, traits, and abstractions for the packs system. All structures are designed for serialization (TOML/YAML/JSON) and follow Rust best practices.

---

## Core Data Structures

### 1. Pack

The primary data structure representing a pack.

```rust
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use semver::{Version, VersionReq};

/// A pack is a reusable bundle of templates, SPARQL queries, and configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pack {
    /// Pack metadata
    pub metadata: PackMetadata,

    /// Templates included in this pack
    pub templates: Vec<PackTemplate>,

    /// SPARQL queries for semantic generation
    pub queries: Vec<SparqlQuery>,

    /// Pack dependencies
    pub dependencies: BTreeMap<String, VersionReq>,

    /// Configurable variables
    pub variables: Vec<PackVariable>,

    /// Pre/post generation hooks
    pub hooks: PackHooks,

    /// Usage examples
    pub examples: Vec<PackExample>,

    /// Files/patterns to exclude
    pub excludes: Vec<String>,
}

impl Pack {
    /// Create a new empty pack
    pub fn new(name: String, version: Version) -> Self {
        Self {
            metadata: PackMetadata::new(name, version),
            templates: Vec::new(),
            queries: Vec::new(),
            dependencies: BTreeMap::new(),
            variables: Vec::new(),
            hooks: PackHooks::default(),
            examples: Vec::new(),
            excludes: Vec::new(),
        }
    }

    /// Load pack from manifest file (pack.toml or pack.yaml)
    pub fn from_manifest(path: &Path) -> Result<Self> {
        // Implementation loads and parses manifest
    }

    /// Validate pack structure and dependencies
    pub fn validate(&self) -> ValidationResult {
        // Implementation validates all components
    }

    /// Get all required variables
    pub fn required_variables(&self) -> Vec<&PackVariable> {
        self.variables.iter().filter(|v| v.required).collect()
    }

    /// Get all optional variables with defaults
    pub fn optional_variables(&self) -> Vec<&PackVariable> {
        self.variables.iter().filter(|v| !v.required).collect()
    }

    /// Check compatibility with another pack
    pub fn is_compatible_with(&self, other: &Pack) -> CompatibilityResult {
        // Implementation checks for conflicts, variable overlaps, etc.
    }
}
```

---

### 2. PackMetadata

Pack identification and descriptive information.

```rust
/// Pack metadata - identification and description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackMetadata {
    /// Unique pack name (e.g., "startup-pack")
    pub name: String,

    /// Semantic version
    pub version: Version,

    /// Human-readable title
    pub title: String,

    /// Pack description
    pub description: String,

    /// Pack category
    pub category: PackCategory,

    /// Additional tags for searchability
    pub tags: Vec<String>,

    /// Author information
    pub author: Author,

    /// Repository URL
    pub repository: Option<String>,

    /// Homepage URL
    pub homepage: Option<String>,

    /// License identifier (SPDX)
    pub license: String,

    /// Creation timestamp
    pub created_at: chrono::DateTime<chrono::Utc>,

    /// Last update timestamp
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// Minimum ggen version required
    pub min_ggen_version: VersionReq,
}

/// Pack category classification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum PackCategory {
    Startup,
    DevOps,
    Frontend,
    Backend,
    Microservice,
    DataEngineering,
    MachineLearning,
    Monitoring,
    Documentation,
    Testing,
    Security,
    Custom(String),
}

/// Author information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Author {
    pub name: String,
    pub email: Option<String>,
    pub url: Option<String>,
}
```

---

### 3. PackTemplate

Reference to a template within a pack.

```rust
/// A template reference in a pack
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackTemplate {
    /// Template identifier (marketplace package ID or local path)
    pub source: TemplateSource,

    /// Optional alias/name for this template in the pack
    pub alias: Option<String>,

    /// Template-specific configuration
    pub config: TemplateConfig,

    /// Variables specific to this template
    pub variables: HashMap<String, String>,

    /// Output path override (relative to pack output)
    pub output_path: Option<PathBuf>,

    /// Execution order priority (lower = earlier)
    pub priority: u32,

    /// Conditions for template execution
    pub conditions: Vec<ExecutionCondition>,
}

/// Template source location
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum TemplateSource {
    /// Marketplace package
    Marketplace {
        package_id: String,
        version: VersionReq,
    },

    /// Local file path
    Local {
        path: PathBuf,
    },

    /// Remote URL
    Remote {
        url: String,
        checksum: Option<String>,
    },

    /// Inline template content
    Inline {
        content: String,
        format: TemplateFormat,
    },
}

/// Template configuration
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TemplateConfig {
    /// Skip if already exists
    pub skip_if_exists: bool,

    /// Overwrite existing files
    pub overwrite: bool,

    /// Merge with existing files
    pub merge: bool,

    /// Preserve specific files/patterns
    pub preserve: Vec<String>,

    /// Custom template engine settings
    pub engine_options: HashMap<String, serde_json::Value>,
}

/// Template format type
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TemplateFormat {
    FileTree,      // YAML file tree
    SingleFile,    // Single template file
    Handlebars,    // Handlebars template
    Tera,          // Tera template
    Custom(String), // Custom format
}

/// Execution condition for conditional template rendering
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionCondition {
    /// Variable name to check
    pub variable: String,

    /// Condition operator
    pub operator: ConditionOperator,

    /// Expected value
    pub value: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ConditionOperator {
    Equals,
    NotEquals,
    Contains,
    StartsWith,
    EndsWith,
    Matches,  // Regex match
}
```

---

### 4. SparqlQuery

SPARQL query for semantic generation.

```rust
/// SPARQL query definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SparqlQuery {
    /// Query name/identifier
    pub name: String,

    /// Query description
    pub description: String,

    /// Query source
    pub source: QuerySource,

    /// Output file path (relative to pack output)
    pub output_path: PathBuf,

    /// Output format
    pub output_format: QueryOutputFormat,

    /// Execution order priority
    pub priority: u32,

    /// Query parameters/variables
    pub parameters: HashMap<String, String>,
}

/// SPARQL query source
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum QuerySource {
    /// Inline query string
    Inline { query: String },

    /// Path to .sparql file
    File { path: PathBuf },

    /// Remote URL to query
    Remote { url: String },
}

/// Query output format
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum QueryOutputFormat {
    Json,
    Yaml,
    Csv,
    Xml,
    Turtle,
    NTriples,
}
```

---

### 5. PackVariable

Configurable variable for pack generation.

```rust
/// Pack variable definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackVariable {
    /// Variable name (used in templates)
    pub name: String,

    /// Variable description
    pub description: String,

    /// Variable type
    pub var_type: VariableType,

    /// Is required?
    pub required: bool,

    /// Default value
    pub default: Option<String>,

    /// Validation rules
    pub validation: Option<VariableValidation>,

    /// Prompt for interactive mode
    pub prompt: Option<String>,

    /// Example values
    pub examples: Vec<String>,
}

/// Variable type classification
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum VariableType {
    String,
    Integer,
    Boolean,
    Enum { values: Vec<String> },
    List,
    Object,
}

/// Variable validation rules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableValidation {
    /// Regex pattern
    pub pattern: Option<String>,

    /// Minimum value (for integers)
    pub min: Option<i64>,

    /// Maximum value (for integers)
    pub max: Option<i64>,

    /// Minimum length (for strings)
    pub min_length: Option<usize>,

    /// Maximum length (for strings)
    pub max_length: Option<usize>,

    /// Custom validation function (reserved for future use)
    pub custom: Option<String>,
}
```

---

### 6. PackHooks

Pre/post generation hooks.

```rust
/// Pre/post generation hooks
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PackHooks {
    /// Hooks before generation starts
    pub pre_generation: Vec<Hook>,

    /// Hooks after generation completes
    pub post_generation: Vec<Hook>,

    /// Hooks before each template
    pub pre_template: Vec<Hook>,

    /// Hooks after each template
    pub post_template: Vec<Hook>,

    /// Hooks on error
    pub on_error: Vec<Hook>,
}

/// Individual hook definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Hook {
    /// Hook name
    pub name: String,

    /// Hook type
    pub hook_type: HookType,

    /// Continue on failure?
    pub continue_on_error: bool,

    /// Timeout in seconds
    pub timeout: Option<u64>,
}

/// Hook execution type
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum HookType {
    /// Execute shell command
    Command {
        command: String,
        args: Vec<String>,
        working_dir: Option<PathBuf>,
    },

    /// Execute script file
    Script {
        path: PathBuf,
        interpreter: Option<String>,
    },

    /// Built-in hook function
    Builtin {
        function: BuiltinHook,
    },
}

/// Built-in hook functions
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum BuiltinHook {
    FormatCode,
    InstallDependencies,
    RunTests,
    GitInit,
    LintCode,
    GenerateDocs,
}
```

---

### 7. PackComposition

Multi-pack composition configuration.

```rust
/// Multi-pack composition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackComposition {
    /// Composition name
    pub name: String,

    /// Composition description
    pub description: String,

    /// Packs to compose
    pub packs: Vec<ComposedPack>,

    /// Global variables (shared across packs)
    pub global_variables: HashMap<String, String>,

    /// Conflict resolution strategy
    pub conflict_resolution: ConflictResolution,

    /// Output directory
    pub output_dir: PathBuf,
}

/// A pack reference in a composition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComposedPack {
    /// Pack name
    pub name: String,

    /// Pack version constraint
    pub version: VersionReq,

    /// Pack-specific variables
    pub variables: HashMap<String, String>,

    /// Execution order (lower = earlier)
    pub order: Option<u32>,

    /// Enable/disable this pack
    pub enabled: bool,
}

/// Conflict resolution strategy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConflictResolution {
    /// Default mode
    pub mode: ConflictMode,

    /// File-specific rules
    pub rules: Vec<ConflictRule>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ConflictMode {
    /// Ask user for each conflict (interactive)
    Ask,

    /// Overwrite existing files
    Overwrite,

    /// Skip conflicting files
    Skip,

    /// Attempt smart merge
    Merge,

    /// Fail on conflict
    Fail,
}

/// File-specific conflict rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConflictRule {
    /// File pattern (glob)
    pub pattern: String,

    /// Action for this pattern
    pub action: ConflictMode,

    /// Priority pack (for overwrite mode)
    pub priority_pack: Option<String>,
}
```

---

### 8. PackValidation

Pack validation results.

```rust
/// Pack validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    /// Is pack valid?
    pub valid: bool,

    /// Validation errors (blocking)
    pub errors: Vec<ValidationError>,

    /// Validation warnings (non-blocking)
    pub warnings: Vec<ValidationWarning>,

    /// Quality score (0-100)
    pub score: u32,

    /// Individual component scores
    pub component_scores: ComponentScores,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationError {
    pub code: String,
    pub message: String,
    pub location: Option<String>,
    pub suggestion: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationWarning {
    pub code: String,
    pub message: String,
    pub location: Option<String>,
}

/// Component-specific scores
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComponentScores {
    pub metadata: u32,
    pub templates: u32,
    pub queries: u32,
    pub variables: u32,
    pub dependencies: u32,
    pub documentation: u32,
}
```

---

### 9. PackCompatibility

Pack compatibility checking.

```rust
/// Pack compatibility check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompatibilityResult {
    /// Are packs compatible?
    pub compatible: bool,

    /// Compatibility issues
    pub issues: Vec<CompatibilityIssue>,

    /// Compatibility score (0-100)
    pub score: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompatibilityIssue {
    pub severity: IssueSeverity,
    pub category: IssueCategory,
    pub message: String,
    pub affected_packs: Vec<String>,
    pub resolution: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum IssueSeverity {
    Error,    // Blocking incompatibility
    Warning,  // Potential issue
    Info,     // Informational note
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum IssueCategory {
    DependencyConflict,
    VariableConflict,
    TemplateConflict,
    VersionMismatch,
    MissingDependency,
    CircularDependency,
}
```

---

### 10. PackExample

Usage example for pack.

```rust
/// Pack usage example
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackExample {
    /// Example name
    pub name: String,

    /// Example description
    pub description: String,

    /// Command to run
    pub command: String,

    /// Variables for this example
    pub variables: HashMap<String, String>,

    /// Expected output description
    pub expected_output: String,

    /// Estimated execution time
    pub estimated_time: Option<String>,
}
```

---

## Trait Definitions

### 1. PackRepository Trait

Abstract pack storage operations.

```rust
use async_trait::async_trait;

/// Pack repository trait for storage abstraction
#[async_trait]
pub trait PackRepository: Send + Sync {
    /// List all available packs
    async fn list(&self, filter: PackFilter) -> Result<Vec<PackSummary>>;

    /// Search packs by query
    async fn search(&self, query: &str, options: SearchOptions) -> Result<Vec<PackSummary>>;

    /// Get pack by name and version
    async fn get(&self, name: &str, version: Option<&Version>) -> Result<Pack>;

    /// Install pack to local registry
    async fn install(&self, name: &str, version: Option<&Version>) -> Result<PackInstallation>;

    /// Uninstall pack from local registry
    async fn uninstall(&self, name: &str) -> Result<()>;

    /// Check if pack is installed
    async fn is_installed(&self, name: &str, version: Option<&Version>) -> Result<bool>;

    /// Get installed version of pack
    async fn installed_version(&self, name: &str) -> Result<Option<Version>>;

    /// Update pack to latest version
    async fn update(&self, name: &str) -> Result<PackUpdate>;

    /// Publish pack to registry
    async fn publish(&self, pack: &Pack, options: PublishOptions) -> Result<PublishResult>;
}

/// Pack summary (lightweight metadata)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackSummary {
    pub name: String,
    pub version: Version,
    pub title: String,
    pub description: String,
    pub category: PackCategory,
    pub downloads: u64,
    pub rating: f32,
    pub is_installed: bool,
}

/// Pack filter options
#[derive(Debug, Clone, Default)]
pub struct PackFilter {
    pub category: Option<PackCategory>,
    pub tags: Vec<String>,
    pub installed: Option<bool>,
    pub min_rating: Option<f32>,
}

/// Search options
#[derive(Debug, Clone, Default)]
pub struct SearchOptions {
    pub limit: usize,
    pub offset: usize,
    pub sort_by: SortField,
    pub order: SortOrder,
}

#[derive(Debug, Clone)]
pub enum SortField {
    Name,
    Downloads,
    Rating,
    UpdatedAt,
}

#[derive(Debug, Clone)]
pub enum SortOrder {
    Ascending,
    Descending,
}
```

---

### 2. PackGenerator Trait

Abstract pack generation operations.

```rust
/// Pack generator trait
#[async_trait]
pub trait PackGenerator: Send + Sync {
    /// Generate project from pack
    async fn generate(
        &self,
        pack: &Pack,
        options: GenerateOptions,
    ) -> Result<GenerateResult>;

    /// Generate from pack composition
    async fn generate_composition(
        &self,
        composition: &PackComposition,
        options: GenerateOptions,
    ) -> Result<GenerateResult>;

    /// Dry run (show what would be generated)
    async fn plan(
        &self,
        pack: &Pack,
        options: GenerateOptions,
    ) -> Result<GenerationPlan>;

    /// Regenerate parts of existing project
    async fn regenerate(
        &self,
        project_dir: &Path,
        options: RegenerateOptions,
    ) -> Result<GenerateResult>;
}

/// Generation options
#[derive(Debug, Clone)]
pub struct GenerateOptions {
    pub output_dir: PathBuf,
    pub variables: HashMap<String, String>,
    pub overwrite: bool,
    pub skip_hooks: bool,
    pub skip_queries: bool,
    pub interactive: bool,
    pub dry_run: bool,
}

/// Generation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateResult {
    pub success: bool,
    pub output_dir: PathBuf,
    pub files_generated: Vec<PathBuf>,
    pub templates_executed: Vec<String>,
    pub queries_executed: Vec<String>,
    pub hooks_executed: Vec<String>,
    pub duration: std::time::Duration,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

/// Generation plan (dry run result)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationPlan {
    pub output_dir: PathBuf,
    pub templates_to_execute: Vec<String>,
    pub queries_to_execute: Vec<String>,
    pub hooks_to_execute: Vec<String>,
    pub files_to_generate: Vec<PathBuf>,
    pub conflicts: Vec<FileConflict>,
    pub estimated_duration: std::time::Duration,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileConflict {
    pub path: PathBuf,
    pub conflicting_packs: Vec<String>,
    pub resolution: ConflictMode,
}
```

---

### 3. PackValidator Trait

Abstract pack validation operations.

```rust
/// Pack validator trait
#[async_trait]
pub trait PackValidator: Send + Sync {
    /// Validate pack structure
    async fn validate(&self, pack: &Pack) -> Result<ValidationResult>;

    /// Check compatibility between packs
    async fn check_compatibility(
        &self,
        pack1: &Pack,
        pack2: &Pack,
    ) -> Result<CompatibilityResult>;

    /// Lint pack for quality issues
    async fn lint(&self, pack: &Pack, strict: bool) -> Result<LintResult>;

    /// Calculate pack maturity score
    async fn score(&self, pack: &Pack) -> Result<MaturityScore>;
}

/// Lint result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LintResult {
    pub passed: bool,
    pub issues: Vec<LintIssue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LintIssue {
    pub severity: IssueSeverity,
    pub rule: String,
    pub message: String,
    pub location: Option<String>,
    pub fix: Option<String>,
}

/// Maturity score
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MaturityScore {
    pub total: u32,
    pub level: MaturityLevel,
    pub dimensions: HashMap<String, u32>,
    pub feedback: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum MaturityLevel {
    Experimental,
    Beta,
    Production,
    Enterprise,
}
```

---

### 4. PackComposer Trait

Abstract pack composition operations.

```rust
/// Pack composer trait for multi-pack operations
#[async_trait]
pub trait PackComposer: Send + Sync {
    /// Resolve dependencies for pack
    async fn resolve_dependencies(&self, pack: &Pack) -> Result<Vec<Pack>>;

    /// Merge multiple packs into composition
    async fn compose(&self, packs: Vec<Pack>) -> Result<PackComposition>;

    /// Resolve conflicts in composition
    async fn resolve_conflicts(
        &self,
        composition: &PackComposition,
        strategy: ConflictResolution,
    ) -> Result<ResolvedComposition>;

    /// Order templates for execution
    async fn order_templates(&self, composition: &PackComposition) -> Result<Vec<PackTemplate>>;

    /// Aggregate variables from all packs
    async fn aggregate_variables(&self, packs: &[Pack]) -> Result<Vec<PackVariable>>;
}

/// Resolved composition (ready for generation)
#[derive(Debug, Clone)]
pub struct ResolvedComposition {
    pub composition: PackComposition,
    pub execution_order: Vec<String>,
    pub merged_variables: HashMap<String, String>,
    pub conflict_resolutions: Vec<ResolvedConflict>,
}

#[derive(Debug, Clone)]
pub struct ResolvedConflict {
    pub file_path: PathBuf,
    pub winning_pack: String,
    pub resolution: ConflictMode,
}
```

---

## Manifest File Formats

### pack.toml Example

```toml
[metadata]
name = "startup-pack"
version = "1.2.0"
title = "Complete Startup Pack"
description = "Full-stack startup project with backend, frontend, and DevOps"
category = "startup"
tags = ["rust", "react", "microservice", "docker"]
license = "MIT"
min_ggen_version = ">=3.0.0"

[metadata.author]
name = "John Doe"
email = "john@example.com"

[[templates]]
alias = "backend"
priority = 1

[templates.source]
type = "marketplace"
package_id = "io.ggen.rust.api"
version = "^2.1.0"

[templates.config]
overwrite = false
merge = false

[templates.variables]
service_name = "{{ project_name }}-api"
port = "{{ api_port }}"

[[templates]]
alias = "frontend"
priority = 2

[templates.source]
type = "marketplace"
package_id = "io.ggen.react.app"
version = "^1.5.0"

[[queries]]
name = "service-discovery"
description = "Generate service discovery configuration"
output_path = "config/services.yaml"
output_format = "yaml"
priority = 10

[queries.source]
type = "file"
path = "queries/service-discovery.sparql"

[[variables]]
name = "project_name"
description = "Project name"
type = "string"
required = true
prompt = "Enter project name:"

[variables.validation]
pattern = "^[a-z][a-z0-9-]*$"
min_length = 3
max_length = 50

[[variables]]
name = "api_port"
description = "API server port"
type = "integer"
required = false
default = "8080"

[variables.validation]
min = 1024
max = 65535

[dependencies]
devops-pack = "^2.0.0"
monitoring-pack = "^1.0.0"

[hooks]
pre_generation = [
    { name = "validate-env", type = "builtin", function = "format-code" }
]

post_generation = [
    { name = "format-code", type = "builtin", function = "format-code" },
    { name = "install-deps", type = "command", command = "npm install" }
]

[[examples]]
name = "basic"
description = "Basic startup project"
command = "ggen pack generate startup-pack --output my-startup --var project_name=myapp"
expected_output = "Full project structure with backend, frontend, and Docker setup"
```

---

## Error Types

```rust
use thiserror::Error;

#[derive(Debug, Error)]
pub enum PackError {
    #[error("Pack not found: {0}")]
    NotFound(String),

    #[error("Pack validation failed: {0}")]
    ValidationFailed(String),

    #[error("Dependency resolution failed: {0}")]
    DependencyResolutionFailed(String),

    #[error("Conflict detected: {0}")]
    ConflictDetected(String),

    #[error("Generation failed: {0}")]
    GenerationFailed(String),

    #[error("Template execution failed: {0}")]
    TemplateExecutionFailed(String),

    #[error("SPARQL query failed: {0}")]
    QueryFailed(String),

    #[error("Hook execution failed: {0}")]
    HookFailed(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    Serialization(#[from] toml::de::Error),

    #[error("Version error: {0}")]
    Version(#[from] semver::Error),
}

pub type Result<T> = std::result::Result<T, PackError>;
```

---

## Summary

This data structure design provides:

1. **Complete pack representation** with metadata, templates, queries, and hooks
2. **Flexible composition** supporting multi-pack projects
3. **Type safety** with strong Rust types and validation
4. **Extensibility** through traits and plugin points
5. **Serialization** to TOML/YAML/JSON formats
6. **Error handling** with comprehensive error types
7. **Compatibility checking** with detailed conflict resolution
8. **Version management** using semantic versioning

All structures follow ggen's existing patterns and integrate seamlessly with the marketplace infrastructure.
