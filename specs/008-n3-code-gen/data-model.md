# Data Model: ggen v5 - Unified Sync Command

**Branch**: `008-n3-code-gen` | **Date**: 2024-12-14 | **Spec**: [spec.md](./spec.md)

---

## Overview

This document defines the core data structures for the `ggen sync` command. The pipeline transforms domain ontologies through inference rules into code graphs, which are then serialized to Rust source files.

**Key Concept**: All types support the unified `ggen sync` pipeline: `ggen.toml → ontology → CONSTRUCT → SELECT → Template → Code`

---

## Core Types

### 1. GgenManifest

The root configuration structure parsed from `ggen.toml`. This is the single source of truth for `ggen sync`.

```rust
/// Root manifest structure from ggen.toml
/// Used by: ggen sync
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

    /// Prefix mappings for SPARQL queries
    #[serde(default)]
    pub prefixes: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct InferenceConfig {
    /// Named inference rules executed in order
    #[serde(default)]
    pub rules: Vec<InferenceRule>,

    /// Maximum time for all inference (ms)
    #[serde(default = "default_reasoning_timeout")]
    pub max_reasoning_timeout_ms: u64,
}

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
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum QuerySource {
    File { file: PathBuf },
    Inline { inline: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum TemplateSource {
    File { file: PathBuf },
    Inline { inline: String },
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub enum GenerationMode {
    #[default]
    Create,      // Create new file (fail if exists)
    Overwrite,   // Overwrite existing
    Merge,       // Merge with existing (marker-based)
}

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

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub enum ValidationSeverity {
    #[default]
    Error,    // Fails generation
    Warning,  // Logged but continues
}

fn default_sparql_timeout() -> u64 { 5000 }
fn default_reasoning_timeout() -> u64 { 5000 }
fn default_output_dir() -> PathBuf { PathBuf::from("src/generated") }
```

---

### 2. SyncOptions

CLI options for the `ggen sync` command.

```rust
/// Options for ggen sync command
#[derive(Debug, Clone)]
pub struct SyncOptions {
    /// Manifest path
    pub manifest: PathBuf,

    /// Override output directory
    pub output_dir: Option<PathBuf>,

    /// Preview changes without writing
    pub dry_run: bool,

    /// Show detailed progress
    pub verbose: bool,

    /// Watch for changes and regenerate
    pub watch: bool,

    /// Execute specific rule(s) only
    pub rules: Option<Vec<String>>,

    /// Overwrite protected files
    pub force: bool,

    /// Generate audit.json
    pub audit: bool,

    /// Validate manifest only
    pub validate_only: bool,

    /// Output format: text, json
    pub format: OutputFormat,

    /// Overall timeout (ms)
    pub timeout_ms: u64,
}

#[derive(Debug, Clone, Default)]
pub enum OutputFormat {
    #[default]
    Text,
    Json,
}
```

---

### 3. Code Graph Entities

RDF types representing Rust code structures (from `code_ontology.ttl`).

```rust
/// In-memory representation of code graph entities
/// Deserialized from CONSTRUCT query results

#[derive(Debug, Clone, Serialize)]
pub struct CodeModule {
    /// IRI identifying this module
    pub iri: String,

    /// Module name (e.g., "models")
    pub name: String,

    /// Visibility ("pub", "pub(crate)", "")
    #[serde(default)]
    pub visibility: String,

    /// Use statements
    #[serde(default)]
    pub imports: Vec<CodeImport>,

    /// Module items (structs, traits, impls)
    #[serde(default)]
    pub items: Vec<CodeItem>,

    /// Module-level attributes
    #[serde(default)]
    pub attributes: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type")]
pub enum CodeItem {
    Struct(CodeStruct),
    Trait(CodeTrait),
    Impl(CodeImpl),
    Enum(CodeEnum),
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeStruct {
    /// IRI from code graph
    pub iri: String,

    /// Struct name (PascalCase)
    pub name: String,

    /// Visibility
    #[serde(default)]
    pub visibility: String,

    /// Derive macros
    #[serde(default)]
    pub derives: Vec<String>,

    /// Generic parameters
    #[serde(default)]
    pub generics: Option<String>,

    /// Struct fields (ordered)
    #[serde(default)]
    pub fields: Vec<CodeField>,

    /// Documentation string
    #[serde(default)]
    pub docstring: Option<String>,

    /// Additional attributes
    #[serde(default)]
    pub attributes: Vec<String>,

    /// Source traceability
    #[serde(default)]
    pub source_iri: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeField {
    /// IRI from code graph
    pub iri: String,

    /// Field name (snake_case)
    pub name: String,

    /// Rust type
    pub field_type: String,

    /// Visibility
    #[serde(default)]
    pub visibility: String,

    /// Documentation
    #[serde(default)]
    pub docstring: Option<String>,

    /// Field attributes
    #[serde(default)]
    pub attributes: Vec<String>,

    /// Default value expression
    #[serde(default)]
    pub default: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeTrait {
    pub iri: String,
    pub name: String,
    #[serde(default)]
    pub visibility: String,
    #[serde(default)]
    pub bounds: Option<String>,
    #[serde(default)]
    pub methods: Vec<CodeMethod>,
    #[serde(default)]
    pub is_async: bool,
    #[serde(default)]
    pub docstring: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeMethod {
    pub iri: String,
    pub name: String,
    #[serde(default)]
    pub visibility: String,
    #[serde(default)]
    pub is_async: bool,
    #[serde(default)]
    pub self_param: Option<String>,  // "&self", "&mut self", "self", None
    #[serde(default)]
    pub params: Vec<CodeParam>,
    #[serde(default)]
    pub return_type: Option<String>,
    #[serde(default)]
    pub body: Option<String>,
    #[serde(default)]
    pub docstring: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeParam {
    pub name: String,
    pub param_type: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeImpl {
    pub iri: String,
    pub for_type: String,
    #[serde(default)]
    pub trait_name: Option<String>,
    #[serde(default)]
    pub generics: Option<String>,
    #[serde(default)]
    pub methods: Vec<CodeMethod>,
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeEnum {
    pub iri: String,
    pub name: String,
    #[serde(default)]
    pub visibility: String,
    #[serde(default)]
    pub derives: Vec<String>,
    #[serde(default)]
    pub variants: Vec<CodeVariant>,
    #[serde(default)]
    pub docstring: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeVariant {
    pub name: String,
    #[serde(default)]
    pub fields: Vec<CodeField>,
    #[serde(default)]
    pub docstring: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeImport {
    pub path: String,
    #[serde(default)]
    pub alias: Option<String>,
}
```

---

### 4. Pipeline State

Tracks execution state through the `ggen sync` pipeline.

```rust
/// Pipeline execution state for ggen sync
#[derive(Debug)]
pub struct PipelineState {
    /// Loaded manifest
    pub manifest: GgenManifest,

    /// Domain ontology graph
    pub ontology_graph: Graph,

    /// Code graph (built by CONSTRUCT)
    pub code_graph: Graph,

    /// Inference rules executed
    pub executed_rules: Vec<ExecutedRule>,

    /// Generated files
    pub generated_files: Vec<GeneratedFile>,

    /// Validation results
    pub validation_results: Vec<ValidationResult>,

    /// Pipeline start time
    pub started_at: std::time::Instant,
}

#[derive(Debug, Clone, Serialize)]
pub struct ExecutedRule {
    pub name: String,
    pub rule_type: RuleType,
    pub triples_added: usize,
    pub duration_ms: u64,
    pub query_hash: String,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum RuleType {
    Inference,
    Generation,
}

#[derive(Debug, Clone, Serialize)]
pub struct GeneratedFile {
    pub path: PathBuf,
    pub content_hash: String,
    pub size_bytes: usize,
    pub source_rule: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct ValidationResult {
    pub rule_name: String,
    pub passed: bool,
    pub message: Option<String>,
    pub severity: ValidationSeverity,
}
```

---

### 5. Sync Output

Output structure for `ggen sync` command (JSON format).

```rust
/// Output for ggen sync command
#[derive(Debug, Clone, Serialize)]
pub struct SyncOutput {
    /// Overall status
    pub status: String,  // "success", "error"

    /// Number of files synced
    pub files_synced: usize,

    /// Total duration in milliseconds
    pub duration_ms: u64,

    /// Generated files
    pub files: Vec<SyncedFile>,

    /// Number of inference rules executed
    pub inference_rules_executed: usize,

    /// Number of generation rules executed
    pub generation_rules_executed: usize,

    /// Audit trail path (if enabled)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub audit_trail: Option<String>,

    /// Error message (if failed)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SyncedFile {
    pub path: String,
    pub size_bytes: usize,
    pub action: String,  // "created", "updated", "unchanged"
}
```

---

### 6. Audit Trail

Complete record of sync for determinism verification.

```rust
/// Audit trail for ggen sync verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditTrail {
    /// Sync timestamp (ISO 8601)
    pub synced_at: String,

    /// ggen version
    pub ggen_version: String,

    /// Input hashes for determinism verification
    pub inputs: AuditInputs,

    /// Pipeline execution log
    pub pipeline: Vec<AuditStep>,

    /// Generated file manifest
    pub outputs: Vec<AuditOutput>,

    /// Overall validation status
    pub validation_passed: bool,

    /// Total duration (ms)
    pub total_duration_ms: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditInputs {
    /// SHA256 of ggen.toml
    pub manifest_hash: String,

    /// SHA256 of each ontology file
    pub ontology_hashes: BTreeMap<String, String>,

    /// SHA256 of each template file
    pub template_hashes: BTreeMap<String, String>,

    /// SHA256 of each SPARQL file
    pub query_hashes: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditStep {
    /// Step type
    pub step_type: String,  // "load_ontology", "inference", "generation", "render"

    /// Rule/file name
    pub name: String,

    /// Duration (ms)
    pub duration_ms: u64,

    /// Triples added (for graph operations)
    #[serde(default)]
    pub triples_added: Option<usize>,

    /// Status
    pub status: String,  // "success", "skipped", "error"

    /// Error message if failed
    #[serde(default)]
    pub error: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditOutput {
    /// Output file path
    pub path: String,

    /// SHA256 of generated content
    pub content_hash: String,

    /// File size in bytes
    pub size_bytes: usize,

    /// Generation rule that produced this
    pub source_rule: String,
}
```

---

## Entity Relationships

```
┌─────────────────┐
│  GgenManifest   │
└────────┬────────┘
         │
    ┌────┴────┬──────────┬─────────────┐
    ▼         ▼          ▼             ▼
┌────────┐ ┌─────────┐ ┌───────────┐ ┌────────────┐
│Ontology│ │Inference│ │Generation │ │Validation  │
│Config  │ │Config   │ │Config     │ │Config      │
└────┬───┘ └────┬────┘ └─────┬─────┘ └─────┬──────┘
     │          │            │             │
     ▼          ▼            ▼             ▼
┌────────┐ ┌─────────┐ ┌──────────┐  ┌────────────┐
│ Graph  │◀┤Inference│ │Generation│  │Validation  │
│(domain)│ │Rule     │ │Rule      │  │Rule        │
└────┬───┘ └─────────┘ └────┬─────┘  └────────────┘
     │                      │
     ▼                      ▼
┌────────┐            ┌──────────┐
│ Graph  │◀───────────┤ Template │
│(code)  │            │ Render   │
└────┬───┘            └────┬─────┘
     │                     │
     ▼                     ▼
┌────────────┐       ┌──────────┐
│CodeModule  │       │ .rs file │
│CodeStruct  │       │ output   │
│CodeTrait   │       └──────────┘
│CodeImpl    │
└────────────┘
```

---

## ggen sync Data Flow

```
ggen sync
    │
    ├─→ Load ggen.toml → GgenManifest
    │
    ├─→ Load ontology → Graph (domain)
    │
    ├─→ Execute [[inference.rules]] (in order)
    │   └─→ CONSTRUCT → materialize → Graph (enriched)
    │
    ├─→ Execute [[generation.rules]]
    │   ├─→ SPARQL SELECT → query results
    │   ├─→ Template render (with sparql_results context)
    │   └─→ Write file
    │
    ├─→ Validate output (if configured)
    │
    └─→ Write audit.json (if --audit)
```

---

## Exit Codes

| Code | Name | Type |
|------|------|------|
| 0 | Success | - |
| 1 | ManifestError | GgenManifest validation |
| 2 | OntologyError | Graph load/parse |
| 3 | SparqlError | Query execution |
| 4 | TemplateError | Tera rendering |
| 5 | IoError | File system |
| 6 | Timeout | Time limit exceeded |

---

## Type Mapping Reference

### RDF Types → Rust Types

| RDF/XSD Type | Rust Type | Notes |
|--------------|-----------|-------|
| `xsd:string` | `String` | |
| `xsd:integer` | `i64` | |
| `xsd:boolean` | `bool` | |
| `xsd:dateTime` | `DateTime<Utc>` | chrono |
| `xsd:anyURI` | `Url` | url crate |
| `:uuid` | `Uuid` | uuid crate, triggers Serialize/Deserialize |
| `rdf:List` | `Vec<T>` | |
| Optional property | `Option<T>` | `sh:minCount 0` |
