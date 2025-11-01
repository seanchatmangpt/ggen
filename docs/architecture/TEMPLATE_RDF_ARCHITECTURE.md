# Template-to-File-Tree Architecture with RDF Integration

**Version:** 1.0.0
**Date:** 2025-11-01
**Status:** Production-Ready Architecture

## Executive Summary

This document defines the complete system architecture for ggen's template-to-file-tree generation engine with semantic RDF metadata integration. The architecture enables generation of entire project structures from templates enriched with knowledge graphs, SPARQL queries, and intelligent code generation patterns.

## Table of Contents

1. [System Overview](#system-overview)
2. [High-Level Architecture](#high-level-architecture)
3. [Component Architecture](#component-architecture)
4. [Data Flow Architecture](#data-flow-architecture)
5. [RDF Schema Design](#rdf-schema-design)
6. [Template Engine Architecture](#template-engine-architecture)
7. [File Generation Pipeline](#file-generation-pipeline)
8. [Integration Points](#integration-points)
9. [Security Architecture](#security-architecture)
10. [Performance Architecture](#performance-architecture)

---

## System Overview

### Purpose

Transform template packages containing YAML frontmatter, Tera templates, and RDF metadata into complete file trees with semantic awareness, supporting:

- Multi-file project generation from single templates
- RDF knowledge graph integration for semantic code generation
- SPARQL-driven template logic and conditional generation
- Hierarchical template composition with dependency resolution
- Production-safe error handling and validation

### Key Features

- **Two-Phase Rendering**: Frontmatter → Graph Processing → Body Rendering
- **RDF Integration**: Inline Turtle, external RDF files, SPARQL queries
- **Template Composition**: Package dependencies, template includes, macros
- **File Tree Generation**: Directory structures, multi-file output, file injection
- **Lifecycle Integration**: Marketplace packages, lockfile management, caching
- **Security**: Path traversal protection, checksum verification, deterministic output

---

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    GGEN TEMPLATE ENGINE                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌───────────────┐      ┌────────────────┐      ┌───────────┐ │
│  │  Marketplace  │─────▶│ Template       │─────▶│ Generator │ │
│  │  Registry     │      │ Resolver       │      │ Pipeline  │ │
│  └───────────────┘      └────────────────┘      └───────────┘ │
│         │                       │                      │        │
│         ▼                       ▼                      ▼        │
│  ┌───────────────┐      ┌────────────────┐      ┌───────────┐ │
│  │  Lockfile     │      │ Cache          │      │ Graph     │ │
│  │  Manager      │      │ Manager        │      │ Store     │ │
│  └───────────────┘      └────────────────┘      └───────────┘ │
│         │                       │                      │        │
│         └───────────────────────┴──────────────────────┘        │
│                                 │                                │
│                                 ▼                                │
│                    ┌────────────────────────┐                   │
│                    │  File Tree Generator   │                   │
│                    └────────────────────────┘                   │
│                                 │                                │
└─────────────────────────────────┼────────────────────────────────┘
                                  ▼
                    ┌────────────────────────┐
                    │   Output File System   │
                    └────────────────────────┘
```

### Data Flow

```
Template Package → Resolver → Cache → Parser → Renderer → File Generator → Output
        ↓              ↓         ↓        ↓         ↓            ↓
    Lockfile      Validation  Graph   Tera    SPARQL     Directory Tree
                                     Engine  Results
```

---

## Component Architecture

### 1. Template Resolver Component

**Purpose**: Resolve template references and load from cache

```rust
┌─────────────────────────────────────┐
│      TemplateResolver               │
├─────────────────────────────────────┤
│ - cache_manager: CacheManager       │
│ - lockfile_manager: LockfileManager │
├─────────────────────────────────────┤
│ + resolve(ref: &str) → Source       │
│ + search_templates(query) → Results │
│ + get_pack_templates(id) → Vec<str> │
│ + get_template_info(ref) → Info     │
└─────────────────────────────────────┘
         │
         ├──▶ CacheManager (loads cached packs)
         ├──▶ LockfileManager (version resolution)
         └──▶ GpackManifest (template discovery)
```

**Responsibilities**:
- Parse template references (`pack_id:template_path`)
- Load cached packages with version resolution
- Discover templates using manifest patterns
- Validate template existence and security

**Data Structures**:

```rust
pub struct TemplateSource {
    pub pack_id: String,
    pub template_path: PathBuf,
    pub pack: CachedPack,
    pub manifest: Option<GpackManifest>,
}

pub struct TemplateSearchResult {
    pub pack_id: String,
    pub template_path: PathBuf,
    pub pack_name: String,
    pub pack_description: String,
}
```

### 2. Template Engine Component

**Purpose**: Parse and process templates with RDF integration

```rust
┌──────────────────────────────────────┐
│           Template                   │
├──────────────────────────────────────┤
│ - raw_frontmatter: Value             │
│ - front: Frontmatter                 │
│ - body: String                       │
├──────────────────────────────────────┤
│ + parse(input: &str) → Result        │
│ + render_frontmatter(tera, vars)     │
│ + process_graph(graph, tera, vars)   │
│ + render(tera, vars) → String        │
└──────────────────────────────────────┘
         │
         ├──▶ Frontmatter (metadata, RDF, SPARQL)
         ├──▶ Graph (RDF store)
         └──▶ Tera (template engine)
```

**Frontmatter Structure**:

```rust
pub struct Frontmatter {
    // Output control
    pub to: Option<String>,              // Output path
    pub from: Option<String>,            // Source file
    pub force: bool,                     // Overwrite existing
    pub unless_exists: bool,             // Skip if exists

    // File injection
    pub inject: bool,                    // Inject into existing file
    pub before: Option<String>,          // Insert before marker
    pub after: Option<String>,           // Insert after marker
    pub at_line: Option<u32>,            // Insert at line number

    // RDF integration
    pub base: Option<String>,            // RDF base URI
    pub prefixes: BTreeMap<String, String>, // RDF prefixes
    pub rdf_inline: Vec<String>,         // Inline Turtle
    pub rdf: Vec<String>,                // RDF file paths
    pub sparql: BTreeMap<String, String>, // Named SPARQL queries

    // Template variables
    pub vars: BTreeMap<String, Value>,   // Template variables

    // Security & idempotency
    pub backup: Option<bool>,            // Create backup
    pub idempotent: bool,                // Idempotent generation

    // Results (populated during processing)
    pub sparql_results: BTreeMap<String, JsonValue>,
}
```

**Processing Phases**:

1. **Parse Phase**: Extract frontmatter and body
2. **Render Frontmatter**: Resolve `{{ }}` variables in YAML
3. **Process Graph**: Load RDF, execute SPARQL, populate results
4. **Render Body**: Generate final output with SPARQL results

### 3. RDF Graph Store Component

**Purpose**: Thread-safe RDF storage with SPARQL caching

```rust
┌──────────────────────────────────────┐
│             Graph                    │
├──────────────────────────────────────┤
│ - inner: Store (Oxigraph)            │
│ - epoch: Arc<AtomicU64>              │
│ - plan_cache: LruCache<u64, String>  │
│ - result_cache: LruCache<CachedResult>│
├──────────────────────────────────────┤
│ + insert_turtle(ttl: &str)           │
│ + load_path(path: Path)              │
│ + query(sparql: &str) → Results      │
│ + query_cached(sparql) → Cached      │
│ + query_with_prolog(sparql, prefixes)│
└──────────────────────────────────────┘
```

**Features**:
- Epoch-based cache invalidation
- LRU caching for queries and plans
- Support for Turtle, N-Triples, RDF/XML
- Thread-safe cloning (shared store)

**Cache Strategy**:

```rust
pub enum CachedResult {
    Boolean(bool),                      // ASK query results
    Solutions(Vec<BTreeMap<String, String>>), // SELECT results
    Graph(Vec<String>),                 // CONSTRUCT results
}

// Cache key: (query_hash, epoch)
// Invalidation: epoch bumps on graph modifications
```

### 4. File Tree Generator Component

**Purpose**: Generate directory structures and multiple files

```rust
┌──────────────────────────────────────┐
│      FileTreeGenerator               │
├──────────────────────────────────────┤
│ - output_root: PathBuf               │
│ - template_resolver: Resolver        │
│ - pipeline: Pipeline                 │
├──────────────────────────────────────┤
│ + generate_tree(manifest) → Tree     │
│ + process_template(source) → Output  │
│ + create_directories(structure)      │
│ + write_files(files) → Results       │
│ + inject_into_file(target, content)  │
└──────────────────────────────────────┘
```

**File Tree Structure**:

```rust
pub struct FileTree {
    pub root: PathBuf,
    pub directories: Vec<PathBuf>,
    pub files: Vec<GeneratedFile>,
}

pub struct GeneratedFile {
    pub path: PathBuf,
    pub content: String,
    pub mode: FileMode,
    pub checksum: String,
}

pub enum FileMode {
    Create,          // Create new file
    Overwrite,       // Replace existing
    Inject,          // Modify existing
    SkipIfExists,    // Conditional create
}
```

### 5. Validation Engine Component

**Purpose**: Validate generated output and security

```rust
┌──────────────────────────────────────┐
│       ValidationEngine               │
├──────────────────────────────────────┤
│ + validate_path_security(path)       │
│ + verify_checksums(files)            │
│ + validate_rdf_schema(graph, shapes) │
│ + check_determinism(output)          │
│ + validate_idempotency(files)        │
└──────────────────────────────────────┘
```

**Security Validations**:
- Path traversal prevention (`..` detection)
- Canonical path verification
- Template directory boundary checks
- Checksum verification (PQC SHA-256)

**Schema Validation**:
- SHACL shapes validation
- RDF graph consistency checks
- Template variable type validation

---

## Data Flow Architecture

### Complete Generation Flow

```
┌─────────────────────────────────────────────────────────────────┐
│ 1. MARKETPLACE RESOLUTION                                       │
└─────────────────────────────────────────────────────────────────┘
   │
   │ ggen template generate pack_id:template.tmpl
   │
   ▼
┌─────────────────────────────────────────────────────────────────┐
│ 2. TEMPLATE RESOLUTION                                          │
│    - Parse template reference                                   │
│    - Load from lockfile                                         │
│    - Retrieve from cache                                        │
│    - Verify checksums                                           │
└─────────────────────────────────────────────────────────────────┘
   │
   ▼
┌─────────────────────────────────────────────────────────────────┐
│ 3. TEMPLATE PARSING                                             │
│    - Parse frontmatter (YAML)                                   │
│    - Extract body (Tera template)                               │
│    - Validate structure                                         │
└─────────────────────────────────────────────────────────────────┘
   │
   ▼
┌─────────────────────────────────────────────────────────────────┐
│ 4. FRONTMATTER RENDERING                                        │
│    - Create Tera context from vars                              │
│    - Render {{ }} in YAML                                       │
│    - Parse rendered YAML → Frontmatter                          │
└─────────────────────────────────────────────────────────────────┘
   │
   ▼
┌─────────────────────────────────────────────────────────────────┐
│ 5. GRAPH PROCESSING                                             │
│    ┌─────────────────────────────────────────────────────────┐ │
│    │ a. Load RDF Data                                        │ │
│    │    - Process inline Turtle (rdf_inline)                 │ │
│    │    - Load external RDF files (rdf)                      │ │
│    │    - Apply prefixes and base URI                        │ │
│    └─────────────────────────────────────────────────────────┘ │
│    ┌─────────────────────────────────────────────────────────┐ │
│    │ b. Execute SPARQL Queries                               │ │
│    │    - For each named query in sparql: {}                 │ │
│    │    - Prepend PREFIX/BASE prolog                         │ │
│    │    - Execute query (cached)                             │ │
│    │    - Store results in frontmatter.sparql_results        │ │
│    └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
   │
   ▼
┌─────────────────────────────────────────────────────────────────┐
│ 6. BODY RENDERING                                               │
│    - Create final context (vars + sparql_results)              │
│    - Render template body with Tera                            │
│    - Apply filters and functions                               │
│    - Handle from: file if specified                            │
└─────────────────────────────────────────────────────────────────┘
   │
   ▼
┌─────────────────────────────────────────────────────────────────┐
│ 7. FILE GENERATION                                              │
│    ┌─────────────────────────────────────────────────────────┐ │
│    │ a. Determine Output Path                                │ │
│    │    - Use to: from frontmatter                           │ │
│    │    - Resolve relative to output_root                    │ │
│    │    - Validate path security                             │ │
│    └─────────────────────────────────────────────────────────┘ │
│    ┌─────────────────────────────────────────────────────────┐ │
│    │ b. Create Directories                                   │ │
│    │    - Create parent directories                          │ │
│    │    - Set permissions                                    │ │
│    └─────────────────────────────────────────────────────────┘ │
│    ┌─────────────────────────────────────────────────────────┐ │
│    │ c. Write File                                           │ │
│    │    - Check force/unless_exists flags                    │ │
│    │    - Handle file injection if inject: true              │ │
│    │    - Write content to disk                              │ │
│    │    - Calculate checksum                                 │ │
│    └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
   │
   ▼
┌─────────────────────────────────────────────────────────────────┐
│ 8. POST-PROCESSING                                              │
│    - Execute sh_after hooks                                     │
│    - Update cache metadata                                      │
│    - Store generation snapshot                                  │
│    - Return generated file paths                                │
└─────────────────────────────────────────────────────────────────┘
```

### File Injection Flow

```
┌─────────────────────────────────────────────────────────────────┐
│ FILE INJECTION FLOW                                             │
└─────────────────────────────────────────────────────────────────┘

1. Read existing file
   ↓
2. Parse content based on injection mode:
   ├─ before: "marker"  → Insert before first occurrence
   ├─ after: "marker"   → Insert after first occurrence
   ├─ at_line: N        → Insert at line number N
   ├─ prepend: true     → Insert at beginning
   └─ append: true      → Insert at end
   ↓
3. Check skip_if condition
   ↓
4. Merge content (preserve existing)
   ↓
5. Write updated file
   ↓
6. Verify eof_last (ensure newline at EOF)
```

---

## RDF Schema Design

### Core Vocabulary

```turtle
@prefix ggen: <http://ggen.io/vocab#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

# Core Classes
ggen:Template a rdfs:Class ;
    rdfs:label "Template" ;
    rdfs:comment "A code generation template" .

ggen:Package a rdfs:Class ;
    rdfs:label "Package" ;
    rdfs:comment "A template package (gpack)" .

ggen:FileTree a rdfs:Class ;
    rdfs:label "FileTree" ;
    rdfs:comment "A generated file tree structure" .

ggen:GeneratedFile a rdfs:Class ;
    rdfs:label "GeneratedFile" ;
    rdfs:comment "A file generated from a template" .

# Properties
ggen:hasTemplate a rdf:Property ;
    rdfs:domain ggen:Package ;
    rdfs:range ggen:Template .

ggen:generates a rdf:Property ;
    rdfs:domain ggen:Template ;
    rdfs:range ggen:GeneratedFile .

ggen:dependsOn a rdf:Property ;
    rdfs:domain ggen:Package ;
    rdfs:range ggen:Package ;
    rdfs:comment "Package dependency relationship" .

ggen:hasVariable a rdf:Property ;
    rdfs:domain ggen:Template ;
    rdfs:comment "Template variable definition" .

ggen:outputPath a rdf:Property ;
    rdfs:domain ggen:GeneratedFile ;
    rdfs:range xsd:string .

ggen:checksum a rdf:Property ;
    rdfs:domain ggen:GeneratedFile ;
    rdfs:range xsd:string ;
    rdfs:comment "SHA-256 checksum of generated file" .
```

### SHACL Shapes for Validation

```turtle
@prefix ggen: <http://ggen.io/vocab#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

# Template Shape
ggen:TemplateShape a sh:NodeShape ;
    sh:targetClass ggen:Template ;
    sh:property [
        sh:path ggen:hasVariable ;
        sh:minCount 0 ;
    ] ;
    sh:property [
        sh:path ggen:generates ;
        sh:class ggen:GeneratedFile ;
        sh:minCount 1 ;
    ] .

# Package Shape
ggen:PackageShape a sh:NodeShape ;
    sh:targetClass ggen:Package ;
    sh:property [
        sh:path ggen:hasTemplate ;
        sh:class ggen:Template ;
        sh:minCount 1 ;
    ] ;
    sh:property [
        sh:path ggen:dependsOn ;
        sh:class ggen:Package ;
    ] .

# GeneratedFile Shape
ggen:GeneratedFileShape a sh:NodeShape ;
    sh:targetClass ggen:GeneratedFile ;
    sh:property [
        sh:path ggen:outputPath ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path ggen:checksum ;
        sh:datatype xsd:string ;
        sh:pattern "^[a-f0-9]{64}$" ;  # SHA-256 hex
    ] .
```

### Example Template Metadata

```turtle
@prefix ggen: <http://ggen.io/vocab#> .
@prefix ex: <http://example.org/> .

# Package definition
ex:rust-service-pack a ggen:Package ;
    ggen:hasTemplate ex:service-template, ex:test-template ;
    ggen:dependsOn ex:common-macros-pack .

# Template definition
ex:service-template a ggen:Template ;
    ggen:hasVariable [
        ggen:name "service_name" ;
        ggen:type "string" ;
        ggen:required true
    ] ;
    ggen:hasVariable [
        ggen:name "port" ;
        ggen:type "integer" ;
        ggen:default 8080
    ] ;
    ggen:generates ex:main-rs, ex:cargo-toml .

# Generated files
ex:main-rs a ggen:GeneratedFile ;
    ggen:outputPath "src/main.rs" ;
    ggen:checksum "abc123..." ;
    ggen:generatedAt "2025-11-01T12:00:00Z"^^xsd:dateTime .
```

---

## Template Engine Architecture

### Tera Integration

```rust
pub struct Pipeline {
    pub tera: Tera,
    pub graph: Graph,
}

impl Pipeline {
    pub fn new() → Result<Self> {
        let mut tera = Tera::default();
        register_all(&mut tera);  // Register custom filters/functions

        Ok(Self {
            tera,
            graph: Graph::new()?,
        })
    }
}
```

### Custom Tera Filters

```rust
// SPARQL result filters
sparql_first(results, column)   // Get first value from column
sparql_values(results, column)  // Get all values from column
sparql_count(results)           // Count results
sparql_empty(results)           // Check if empty

// RDF filters
uri_encode(string)              // URL-encode for URIs
prefix_expand(qname, prefixes)  // Expand prefixed name
iri_to_prefixed(iri, prefixes)  // Convert IRI to prefixed

// Code generation filters
snake_case(string)              // to_snake_case
camel_case(string)              // ToCamelCase
kebab_case(string)              // to-kebab-case
```

### Template Composition

```yaml
---
to: "src/{{ module }}/mod.rs"
# Include shared macros from dependency
includes:
  - "gpack:common-macros:rust/mod.tera"
# Use variables from parent template
vars:
  module: "{{ parent.module }}"
---
{% import "rust/mod.tera" as rust_macros %}

{{ rust_macros::module_header(name=module) }}

// Generated content
```

---

## File Generation Pipeline

### Pipeline Stages

```
┌──────────────────────────────────────────────────────────────┐
│ STAGE 1: TEMPLATE DISCOVERY                                 │
│  - Resolve template references                              │
│  - Load from cache                                           │
│  - Discover dependencies                                     │
└──────────────────────────────────────────────────────────────┘
   ↓
┌──────────────────────────────────────────────────────────────┐
│ STAGE 2: DEPENDENCY RESOLUTION                               │
│  - Build dependency DAG                                      │
│  - Topological sort                                          │
│  - Detect cycles                                             │
└──────────────────────────────────────────────────────────────┘
   ↓
┌──────────────────────────────────────────────────────────────┐
│ STAGE 3: VARIABLE RESOLUTION                                 │
│  - Merge variables from all sources                          │
│  - CLI args > Template vars > Package vars > Defaults        │
│  - Validate required variables                               │
└──────────────────────────────────────────────────────────────┘
   ↓
┌──────────────────────────────────────────────────────────────┐
│ STAGE 4: RDF GRAPH CONSTRUCTION                              │
│  - Load package-level RDF                                    │
│  - Load template-level RDF                                   │
│  - Merge graphs with prefixes                                │
└──────────────────────────────────────────────────────────────┘
   ↓
┌──────────────────────────────────────────────────────────────┐
│ STAGE 5: TEMPLATE PROCESSING                                 │
│  - For each template in topological order:                   │
│    1. Render frontmatter                                     │
│    2. Process graph (execute SPARQL)                         │
│    3. Render body                                            │
│    4. Determine output path                                  │
└──────────────────────────────────────────────────────────────┘
   ↓
┌──────────────────────────────────────────────────────────────┐
│ STAGE 6: FILE TREE CONSTRUCTION                              │
│  - Group outputs by directory                                │
│  - Create directory structure                                │
│  - Handle file conflicts                                     │
└──────────────────────────────────────────────────────────────┘
   ↓
┌──────────────────────────────────────────────────────────────┐
│ STAGE 7: FILE WRITING                                        │
│  - Write files with proper modes                             │
│  - Execute file injections                                   │
│  - Calculate checksums                                       │
└──────────────────────────────────────────────────────────────┘
   ↓
┌──────────────────────────────────────────────────────────────┐
│ STAGE 8: VALIDATION & HOOKS                                  │
│  - Validate generated files                                  │
│  - Execute sh_after hooks                                    │
│  - Store generation snapshot                                 │
└──────────────────────────────────────────────────────────────┘
```

### Error Handling

```rust
pub enum GenerationError {
    TemplateNotFound(String),
    CyclicDependency(Vec<String>),
    MissingVariable { template: String, var: String },
    RdfParseError { source: String, error: String },
    SparqlExecutionError { query: String, error: String },
    PathTraversal { path: String },
    ChecksumMismatch { expected: String, actual: String },
    FileConflict { path: PathBuf, mode: ConflictMode },
}

pub enum ConflictMode {
    BothGenerated,      // Two templates generate same file
    ExistsNotForce,     // File exists, force=false
    InjectionFailed,    // Injection marker not found
}
```

**Error Handling Strategy**:
- Production-safe: No `.unwrap()` or `.expect()` in production code
- Graceful degradation: Continue processing other templates on non-fatal errors
- Detailed context: Include template path, line numbers, variable names in errors
- Rollback support: Restore filesystem state on critical failures

---

## Integration Points

### 1. Marketplace Integration

```rust
// Search for packages
ggen market search "rust web service"

// Install package
ggen market add "io.ggen.rust.axum-service"

// Discover templates in package
ggen template list io.ggen.rust.axum-service

// Generate from package template
ggen template generate io.ggen.rust.axum-service:service.tmpl
```

**Integration Flow**:

```
Market Search → Registry Query → Download Package → Cache → Verify → Lockfile
                                                                         ↓
Template Generate → Resolve from Lockfile → Load from Cache → Process → Output
```

### 2. Lifecycle Integration

```toml
# make.toml
[lifecycle.generate]
description = "Generate code from templates"
commands = [
    "ggen template generate io.ggen.rust.axum-service:service.tmpl",
    "ggen template generate io.ggen.rust.axum-service:tests.tmpl"
]
outputs = ["src/", "tests/"]
cache = true

[lifecycle.build]
description = "Build project"
commands = ["cargo build"]
```

**Lifecycle Integration**:

```
ggen lifecycle run generate → Execute template generation commands
                           → Track outputs for caching
                           → Trigger dependent phases (build, test)
```

### 3. Template Inheritance

```yaml
---
# child.tmpl extends parent.tmpl
extends: "io.ggen.base:parent.tmpl"
to: "src/{{ name }}.rs"
vars:
  name: "user_service"
  parent:
    features: ["async", "logging"]
---
{% extends "parent.tmpl" %}

{% block implementation %}
// Child-specific implementation
{% endblock %}
```

**Inheritance Resolution**:

```
child.tmpl → Resolve extends → Load parent.tmpl → Merge frontmatter
                                                 → Merge RDF graphs
                                                 → Tera extends mechanism
```

### 4. Variable Substitution Precedence

```
CLI Arguments (--var key=value)
    ↓
Template Frontmatter (vars: {})
    ↓
Package Manifest (preset.vars: {})
    ↓
Environment Variables (ENV_*)
    ↓
Default Values (variable defaults)
```

---

## Security Architecture

### Path Traversal Prevention

```rust
fn validate_output_path(
    output_root: &Path,
    template_path: &str
) → Result<PathBuf> {
    // Reject .. components
    if template_path.contains("..") {
        return Err(anyhow!("Path traversal blocked: {}", template_path));
    }

    // Build full path
    let full_path = output_root.join(template_path);

    // Canonicalize and verify
    let canonical_output = full_path.canonicalize()?;
    let canonical_root = output_root.canonicalize()?;

    if !canonical_output.starts_with(&canonical_root) {
        return Err(anyhow!("Path outside output root: {}", template_path));
    }

    Ok(canonical_output)
}
```

### Checksum Verification

```rust
// PQC SHA-256 checksums for all generated files
pub fn verify_generation(
    files: &[GeneratedFile],
    expected_checksums: &BTreeMap<PathBuf, String>
) → Result<()> {
    for file in files {
        let actual = calculate_sha256_file(&file.path)?;

        if let Some(expected) = expected_checksums.get(&file.path) {
            if actual != *expected {
                return Err(GenerationError::ChecksumMismatch {
                    expected: expected.clone(),
                    actual,
                }.into());
            }
        }
    }
    Ok(())
}
```

### RDF Security

```rust
// Prevent malicious RDF from loading arbitrary files
fn load_rdf_file(
    template_dir: &Path,
    rdf_path: &str
) → Result<String> {
    let rdf_path = template_dir.join(rdf_path);

    // Canonicalize both paths
    let canonical_rdf = rdf_path.canonicalize()?;
    let canonical_template = template_dir.canonicalize()?;

    // Ensure RDF file is within template directory
    if !canonical_rdf.starts_with(&canonical_template) {
        return Err(anyhow!(
            "Path traversal blocked: RDF file outside template dir"
        ));
    }

    std::fs::read_to_string(&canonical_rdf)
}
```

---

## Performance Architecture

### Caching Strategy

```
┌──────────────────────────────────────┐
│ CACHE LAYERS                         │
├──────────────────────────────────────┤
│ 1. Package Cache                     │
│    - Cached packs with checksums     │
│    - LRU eviction                    │
│    - 1GB default size                │
├──────────────────────────────────────┤
│ 2. Graph Query Cache                 │
│    - SPARQL plan cache (LRU 100)     │
│    - Result cache (LRU 1000)         │
│    - Epoch-based invalidation        │
├──────────────────────────────────────┤
│ 3. Tera Template Cache               │
│    - Compiled templates              │
│    - Built-in Tera caching           │
├──────────────────────────────────────┤
│ 4. Lifecycle Output Cache            │
│    - Phase output tracking           │
│    - Incremental regeneration        │
└──────────────────────────────────────┘
```

### Parallel Processing

```rust
// Process independent templates in parallel
use rayon::prelude::*;

pub fn generate_parallel(
    templates: Vec<TemplateSource>,
    pipeline: &Pipeline,
    vars: &Context
) → Result<Vec<GeneratedFile>> {
    templates
        .par_iter()
        .map(|source| process_template(source, pipeline, vars))
        .collect()
}
```

### Deterministic Output

```rust
// Ensure deterministic generation for caching
1. Sort all map keys (BTreeMap)
2. Sort file lists (Vec::sort)
3. Use stable RDF serialization
4. Deterministic SPARQL result ordering (ORDER BY)
5. Fixed hash algorithms (PQC SHA-256)
```

---

## Summary

This architecture provides a production-ready foundation for template-to-file-tree generation with semantic RDF integration. Key architectural principles:

1. **Modularity**: Clear component boundaries with well-defined interfaces
2. **Security**: Path traversal prevention, checksum verification, canonical paths
3. **Performance**: Multi-layer caching, parallel processing, deterministic output
4. **Extensibility**: Plugin architecture for filters, functions, and validators
5. **Production Safety**: No `.unwrap()`/`.expect()`, graceful error handling, detailed context

The architecture seamlessly integrates with ggen's marketplace, lifecycle, and AI systems while maintaining clean separation of concerns and production-grade reliability.

---

**Next Steps**:
1. Implement `FileTreeGenerator` component
2. Add SHACL shape validation
3. Extend Tera filters for SPARQL results
4. Create comprehensive integration tests
5. Document template authoring best practices
