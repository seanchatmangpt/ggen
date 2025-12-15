# Data Model: N3/CONSTRUCT Semantic Code Generator

**Branch**: `008-n3-code-gen` | **Date**: 2024-12-14 | **Spec**: [spec.md](./spec.md)

---

## Overview

This document defines the core data structures for semantic code generation. The pipeline transforms domain ontologies through inference rules into code graphs, which are then serialized to Rust source files.

---

## Core Types

### 1. GgenManifest

The root configuration structure parsed from `ggen.toml`.

```rust
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

### 2. Code Graph Entities

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

### 3. Pipeline State

Tracks execution state through the generation pipeline.

```rust
/// Pipeline execution state
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

#[derive(Debug, Clone, Serialize)]
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

### 4. Audit Trail

Complete record of generation for determinism verification.

```rust
/// Audit trail for generation verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditTrail {
    /// Generation timestamp (ISO 8601)
    pub generated_at: String,

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
    pub step_type: String,  // "load_ontology", "inference", "construct", "render"

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

## SPARQL → Code Graph Mapping

### Domain to Code Transformation

| Domain Pattern | CONSTRUCT Template | Code Graph Result |
|---------------|-------------------|-------------------|
| `?e a rdfs:Class` | `?e a code:Struct` | `CodeStruct` |
| `:property rdfs:domain ?e` | `code:structFields ?field` | `CodeField` |
| `:interface a :Protocol` | `?i a code:Trait` | `CodeTrait` |
| `:has_many :Target` | `code:implMethods ?getter` | `CodeMethod` |
| `:auditable true` | (via inference rule) | `created_at`, `updated_at` fields |

### Example CONSTRUCT Query

```sparql
PREFIX code: <http://ggen.dev/code#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX : <http://example.org/>

CONSTRUCT {
  ?struct a code:Struct ;
          code:structName ?name ;
          code:structVisibility "pub" ;
          code:structDerives ("Debug" "Clone") ;
          code:sourceIri ?class .
}
WHERE {
  ?class a rdfs:Class ;
         rdfs:label ?name .
  BIND(IRI(CONCAT("http://ggen.dev/code#", ?name, "_struct")) AS ?struct)
}
ORDER BY ?name
```

---

## Validation Rules

### Built-in Validations

| Validation | SPARQL ASK | Error Code |
|------------|-----------|------------|
| All fields have types | `ASK { ?f a code:Field . FILTER NOT EXISTS { ?f code:fieldType ?t } }` → false | 1 |
| No duplicate struct names | `ASK { ?s1 code:structName ?n . ?s2 code:structName ?n . FILTER(?s1 != ?s2) }` → false | 1 |
| All methods have return types | `ASK { ?m a code:Method . FILTER NOT EXISTS { ?m code:methodReturn ?r } }` → false | 1 |

### SHACL Shape Example

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix code: <http://ggen.dev/code#> .

code:StructShape a sh:NodeShape ;
    sh:targetClass code:Struct ;
    sh:property [
        sh:path code:structName ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:pattern "^[A-Z][a-zA-Z0-9]*$" ;
    ] ;
    sh:property [
        sh:path code:structFields ;
        sh:node code:FieldShape ;
    ] .

code:FieldShape a sh:NodeShape ;
    sh:targetClass code:Field ;
    sh:property [
        sh:path code:fieldName ;
        sh:minCount 1 ;
        sh:pattern "^[a-z][a-z0-9_]*$" ;
    ] ;
    sh:property [
        sh:path code:fieldType ;
        sh:minCount 1 ;
    ] .
```

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
