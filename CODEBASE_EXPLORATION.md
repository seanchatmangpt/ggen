# GGEN Codebase Exploration Report

## Executive Summary

**ggen** is a knowledge graph-driven code generation framework where RDF (Resource Description Framework) ontologies serve as the single source of truth. Rather than treating code generation as templating, ggen treats code as "projections of knowledge graphs"—semantic artifacts derived from RDF data using SPARQL queries and Tera templates.

**Key Statistics:**
- Language: Rust (2021 edition)
- Version: 2.6.0
- Production Readiness: 89%
- Repository: https://github.com/seanchatmangpt/ggen
- Core Architecture: Multi-crate workspace with modular domain separation

---

## 1. Overall Project Structure & Architecture

### 1.1 Workspace Organization

The project is a **multi-crate Rust workspace** with clear separation of concerns:

```
ggen/
├── crates/
│   ├── ggen-cli/              # Command-line interface layer
│   ├── ggen-core/             # Core generation engine (RDF, templates, graphs)
│   ├── ggen-domain/           # Business logic layer (pure domain, no CLI)
│   ├── ggen-ai/               # AI integration (GPT, Claude, Ollama)
│   ├── ggen-marketplace/      # Package registry and distribution
│   ├── ggen-node/             # Node.js addon integration
│   └── ggen-utils/            # Shared utilities and error handling
├── examples/                   # Real-world examples
├── templates/                  # Built-in template library
├── ontologies/                 # RDF ontology definitions
└── tests/                      # Integration and E2E tests
```

### 1.2 Key Architecture Patterns

**Layered Architecture:**
```
User CLI (clap-noun-verb v3.4)
         ↓
Domain Logic Layer (ggen-domain) - Pure business logic, no CLI dependencies
         ↓
Core Engine (ggen-core) - RDF, Templates, Graphs, Registry
         ↓
Infrastructure (ggen-ai, ggen-marketplace, ggen-utils)
```

**Design Philosophy:**
- **Poka-Yoke Design**: Type-system driven, compiler enforces correctness at compile time
- **Prevention > Detection**: Warnings treated as errors, no `expect()` in production paths
- **RDF-First**: All template metadata and ontologies stored in RDF format
- **Deterministic**: Byte-identical, reproducible builds for all generated code
- **Async-by-Default**: Full tokio async runtime for non-blocking operations

---

## 2. Template & Ontology Management

### 2.1 The Ontology System

**Core Concept**: RDF ontologies define the semantic structure of your domain.

**Main Ontology Namespace:**
```
http://ggen.dev/ontology#
```

**Supported Standard Namespaces:**
- `rdf:` - RDF syntax (http://www.w3.org/1999/02/22-rdf-syntax-ns#)
- `rdfs:` - RDF Schema (http://www.w3.org/2000/01/rdf-schema#)
- `xsd:` - XML Schema datatypes (http://www.w3.org/2001/XMLSchema#)
- `owl:` - OWL ontologies (http://www.w3.org/2002/07/owl#)

**Core Ontology Classes** (`ggen:` namespace):

| Class | Purpose |
|-------|---------|
| `ggen:Template` | Template for generating artifacts |
| `ggen:Variable` | Template variable for substitution |
| `ggen:File` | Generated file artifact |
| `ggen:Directory` | Generated directory structure |
| `ggen:Artifact` | Base class for all generated artifacts |
| `ggen:Dependency` | Template dependency relationship |
| `ggen:FileFormat` | Format/language of generated files |

**Key Properties** (Example):
- `ggen:templateName` - Unique identifier
- `ggen:templateVersion` - Semantic version
- `ggen:templateDescription` - Purpose description
- `ggen:requiresVariable` - Required template variables
- `ggen:dependsOn` - Template dependencies
- `ggen:stability` - Level: experimental/stable/deprecated

**Schema Location:**
```
crates/ggen-core/src/rdf/schema.ttl    # Full RDF schema definition
crates/ggen-core/src/rdf/schema.rs     # Rust API for schema
```

### 2.2 RDF Module Structure

**Location:** `crates/ggen-core/src/rdf/`

```
rdf/
├── mod.rs                    # Module exports and documentation
├── schema.rs                 # GgenOntology class and namespace constants
├── schema.ttl                # W3C standard RDF/Turtle schema definition
├── template_metadata.rs      # Store and retrieve template metadata
├── template_metadata_helper.rs
└── validation.rs             # SHACL-based validation rules
```

**Core RDF Components:**

1. **GgenOntology** (`schema.rs`):
   - Type-safe methods for constructing ontology IRIs
   - Example: `GgenOntology::template()` → `http://ggen.dev/ontology#Template`
   - Programmatic access to all ontology classes and properties

2. **TemplateMetadata** (`template_metadata.rs`):
   - Extract, store, and query template metadata from RDF
   - Relationships: dependencies, variants, compositions
   - Variable extraction from RDF annotations

3. **Validator** (`validation.rs`):
   - SHACL (Shapes Constraint Language) validation
   - Validates template structure and relationships
   - Ensures RDF data integrity

### 2.3 Ontology Examples

**Location:** `ontologies/`

```
ontologies/
└── clap-noun-verb_v3.3.0.ttl  # CLI framework ontology
```

Example ontology structure (clap-noun-verb):
- `cnv:Noun` - Command noun (e.g., "template", "project")
- `cnv:Verb` - Action verb (e.g., "generate", "list")
- `cnv:Argument` - CLI arguments and flags
- Type definitions for custom value parsers, help text, etc.

**Usage Pattern:**
```turtle
@prefix cnv: <http://ggen.dev/schema/clap-noun-verb#> .
@prefix ex: <http://example.org/> .

ex:MyCommand a cnv:Noun ;
    rdfs:label "My Command" ;
    cnv:hasVerb ex:MyVerb .

ex:MyVerb a cnv:Verb ;
    rdfs:label "my-action" ;
    cnv:requiredArg ?arg .
```

### 2.4 Template Definition Format

**Location:** `templates/` and `examples/*/templates/`

Templates use **YAML frontmatter + Tera template syntax**:

```yaml
---
to: "generated/src/{{ name | snake }}.rs"
from: null
prefixes:
  ex: "http://example.org/"
base: "http://example.org/base/"
rdf:
  - "data/domain.ttl"
sparql:
  find_entities: "SELECT ?entity WHERE { ?entity a ex:Entity }"
  find_properties: "SELECT ?prop WHERE { ?prop a ex:Property }"
inject: false
before: null
after: null
---

// Template body with Tera + RDF results
// Access variables: {{ name }}, {{ description }}
// Access RDF results: {{ sparql_results.find_entities }}
```

**Frontmatter Fields:**
| Field | Purpose |
|-------|---------|
| `to` | Output file path (with variable substitution) |
| `from` | Input file path (less common) |
| `rdf` | List of RDF/Turtle files to load |
| `rdf_inline` | Inline Turtle triples |
| `sparql` | Named SPARQL queries to execute |
| `prefixes` | RDF namespace prefixes |
| `base` | RDF base IRI |
| `inject` | Enable file injection mode |
| `backup` | Backup original before modification |

**Body Template:**
- Tera syntax for variable substitution: `{{ variable }}`
- Tera filters: `{{ name | snake }}`, `{{ date | date(format="%Y-%m-%d") }}`
- SPARQL result access: `{{ sparql_results.query_name }}`
- Custom filters via `register.rs`

---

## 3. Template Generation & Modification

### 3.1 Core Generation Pipeline

**Key Types:**
- `Generator` - Main generation orchestrator
- `Pipeline` - Template processing pipeline
- `GenContext` - Generation context with variables and paths
- `Template` - Parsed template with frontmatter + body

**Generation Flow:**

```
Template File
    ↓
Parse (YAML frontmatter + body)
    ↓
Render Frontmatter (resolve {{ vars }})
    ↓
Load RDF (from files/API)
    ↓
Execute SPARQL Queries (populate sparql_results)
    ↓
Render Body (with full context)
    ↓
Handle File Injection (if needed)
    ↓
Write Output File
```

### 3.2 Generation Engine Modules

**Location:** `crates/ggen-core/src/`

**Critical Modules:**

1. **`generator.rs`** - High-level generation orchestrator
   - `Generator` struct - Coordinates entire pipeline
   - `GenContext` - Generation context (paths, variables, settings)
   - Supports dry-run mode, file injection, SPARQL execution

2. **`pipeline.rs`** - Multi-stage processing pipeline
   - `Pipeline` struct - RDF graph + Tera template engine
   - `PipelineBuilder` - Fluent API for configuration
   - SPARQL query registration and execution
   - Prefix management and base IRI handling

3. **`template.rs`** - Template parsing and rendering
   - `Template` struct - Parsed frontmatter + body
   - `Frontmatter` - YAML frontmatter fields
   - Two-phase rendering (frontmatter → body)
   - SPARQL results storage in `sparql_results` field

4. **`graph/core.rs`** - RDF triple store wrapper
   - `Graph` struct - Oxigraph wrapper with caching
   - Query result caching (LRU cache)
   - Query plan caching
   - Epoch-based cache invalidation

5. **`templates/generator.rs`** - File tree generation
   - `FileTreeGenerator` - Generates complete directory structures
   - `GenerationResult` - Output statistics
   - Support for multiple template formats (YAML, TOML, JSON)

### 3.3 Template Processing Details

**Location:** `crates/ggen-core/src/templates/`

```
templates/
├── mod.rs                    # Module organization
├── generator.rs              # FileTreeGenerator
├── file_tree_generator.rs    # Core file tree logic
├── context.rs                # TemplateContext for variable substitution
├── format.rs                 # Template format parsers (YAML/TOML/JSON)
├── frozen.rs                 # Frozen section handling for manual edits
└── business_logic.rs         # Business logic separation
```

**Key Components:**

1. **FileTreeTemplate** - Declarative template structure
2. **FileTreeGenerator** - Renders template tree to filesystem
3. **FrozenMerger** - Preserves manual edits in generated files
4. **TemplateContext** - Variable substitution engine

### 3.4 SPARQL Integration

Templates can execute SPARQL queries against the RDF graph:

```yaml
sparql:
  find_entities: "SELECT ?entity ?name WHERE { ?entity rdf:type ex:Entity ; rdfs:label ?name }"
  find_relationships: "SELECT ?subject ?object WHERE { ?subject ex:relatedTo ?object }"
```

**Access in Template:**
```tera
{% for entity in sparql_results.find_entities %}
  // {{ entity.entity }} - {{ entity.name }}
{% endfor %}
```

**Execution:**
- SPARQL queries execute against loaded RDF graph
- Results converted to JSON (column names as keys)
- Available in template as `sparql_results.{query_name}`

---

## 4. Workflow & Process Management

### 4.1 Lifecycle Management System

**Location:** `crates/ggen-core/src/lifecycle/`

```
lifecycle/
├── mod.rs                    # Module organization
├── model.rs                  # Domain model for lifecycle
├── poka_yoke.rs              # Error prevention mechanisms
├── production.rs             # Production readiness tracking
├── state_machine.rs          # Lifecycle state transitions
├── hooks.rs                  # Hook management
├── exec.rs                   # Hook execution
└── integration_test.rs        # Lifecycle integration tests
```

**Core Lifecycle Phases:**

1. **Define** - Create ontology/requirements
2. **Generate** - Generate code from ontology
3. **Test** - Validate generated code
4. **Build** - Compile/package
5. **Deploy** - Release to production
6. **Monitor** - Track health and metrics
7. **Evolve** - Update ontology and regenerate

**Poka-Yoke Design (Error Prevention):**
- `NonEmptyString` - Prevents empty string bugs
- `NonEmptyPath` - Prevents invalid path operations
- `Counter` - Thread-safe counters
- `ReadinessTracker` - Production readiness verification
- `ReadinessReport` - Detailed readiness analysis

### 4.2 Delta-Driven Projection

**Location:** `crates/ggen-core/src/delta.rs`

Detects changes in RDF ontologies and regenerates affected templates:

```
Old Ontology
    ↓
Graph Comparison
    ↓
Delta Detection (additions, deletions, modifications)
    ↓
Impact Analysis (which templates are affected?)
    ↓
Selective Regeneration (only changed templates)
```

**Delta Types:**
- `Addition` - New triple added
- `Deletion` - Triple removed
- `Modification` - Triple's object changed

**Usage Pattern:**
```rust
let delta = GraphDelta::compute(&old_graph, &new_graph)?;
let analyzer = ImpactAnalyzer::new();
let impacts = analyzer.analyze(&old_graph, &new_graph, &templates)?;
```

### 4.3 Registry & Package Management

**Location:** `crates/ggen-core/src/registry.rs` and `crates/ggen-marketplace/`

Registry client for `registry.ggen.dev`:
- Fetch pack metadata
- Search for templates/packs
- Resolve version specifications
- Check for updates
- Support for local testing via `file://` URLs

**Pack Format (gpack):**
- Manifest-based package format
- Contains templates, ontologies, examples
- Versioning with semantic versions
- Git-based distribution

### 4.4 Git Hook Integration

**Location:** `crates/ggen-core/src/` and domain hooks module

Automated validation and regeneration:
- Pre-commit hooks validate ontologies
- Post-merge hooks sync changes
- Deterministic output ensures clean diffs
- Integration with git workflow

---

## 5. Main Entry Points & Core Modules

### 5.1 CLI Entry Points

**Location:** `crates/ggen-cli/src/`

**Command Structure** (clap-noun-verb v3.4):

```
ggen <NOUN> <VERB> [OPTIONS]
```

**Command Modules:**

| Noun | Verbs | Purpose |
|------|-------|---------|
| `template` | `generate`, `generate-rdf`, `list`, `show`, `new`, `lint` | Template operations |
| `project` | `new`, `gen`, `watch` | Project scaffolding |
| `graph` | `load`, `query`, `export`, `diff` | RDF graph operations |
| `ai` | `generate-ontology`, `chat`, `analyze` | AI-powered operations |
| `marketplace` | `search`, `install`, `publish` | Package management |
| `hook` | `create`, `list`, `monitor` | Git hook management |
| `ci` | (various) | CI/CD pipeline management |

**Auto-Discovery:**
- Uses `#[verb]` macros for automatic command discovery
- Located in `crates/ggen-cli/src/cmds/`
- Async-first implementation

### 5.2 Core Module Hierarchy

```
ggen-core/
├── Graph (RDF storage + SPARQL)
├── Pipeline (Template processing)
├── Generator (High-level orchestration)
├── Template (Parsing + rendering)
├── Registry (Package distribution)
├── Lifecycle (Production readiness)
├── Delta (Change detection)
└── Merge (Three-way merging)
```

### 5.3 Domain Layer Organization

**Location:** `crates/ggen-domain/src/`

Pure business logic separated from CLI:

```
ggen-domain/
├── template/            # Template operations
│   ├── generate.rs
│   ├── generate_rdf.rs
│   ├── generate_tree.rs
│   ├── lint.rs
│   ├── list.rs
│   ├── new.rs
│   ├── regenerate.rs
│   ├── render_with_rdf.rs
│   └── show.rs
├── project/             # Project scaffolding
├── graph/               # RDF graph operations
├── ai/                  # AI integration
├── marketplace/         # Package management
├── hook/                # Git hooks
├── rdf/                 # RDF metadata
├── ci/                  # CI/CD operations
├── audit/               # Security auditing
└── utils/               # Utilities
```

### 5.4 Key Type System

**Generation Context:**
```rust
pub struct GenContext {
    pub template_path: PathBuf,
    pub output_root: PathBuf,
    pub vars: BTreeMap<String, String>,
    pub global_prefixes: BTreeMap<String, String>,
    pub base: Option<String>,
    pub dry_run: bool,
}
```

**Template Metadata:**
```rust
pub struct TemplateMetadata {
    pub name: String,
    pub description: Option<String>,
    pub variables: Vec<TemplateVariable>,
    pub relationships: Vec<TemplateRelationship>,
    pub version: Option<String>,
}
```

**RDF Graph:**
```rust
pub struct Graph {
    inner: Arc<Store>,                    // Oxigraph triple store
    epoch: Arc<AtomicU64>,                // Cache invalidation
    plan_cache: Arc<Mutex<LruCache>>,     // Query plan cache
    result_cache: Arc<Mutex<LruCache>>,   // Result cache
}
```

---

## 6. Template Formats & Features

### 6.1 Template Syntax

**Two-Phase Rendering:**

**Phase 1: Frontmatter**
- Parse YAML frontmatter
- Render with {{ variables }}
- Resolve output paths

**Phase 2: Body**
- Access full context (variables + RDF results)
- Tera template syntax
- Custom filters and functions

### 6.2 Template Filters & Functions

**Custom Filters** (from `register.rs`):
- `snake` - Convert to snake_case
- `pascal` - Convert to PascalCase
- `kebab` - Convert to kebab-case
- `shout_snake` - Convert to SHOUT_SNAKE_CASE
- `date` - Format dates
- And many others...

**SPARQL Functions:**
- `sparql_first()` - Get first result column
- `sparql_values()` - Get all values from column
- Custom SPARQL execution in templates

### 6.3 Template File Structure

**Example Template:**
```
project-template.tmpl
├── Frontmatter
│   ├── to/from paths
│   ├── RDF configuration
│   ├── SPARQL queries
│   └── Template variables
└── Body (Tera syntax)
    ├── Variable substitution
    ├── Control flow (if/for)
    └── Filter application
```

---

## 7. Semantic/Ontology Aspects

### 7.1 Semantic Projection Engine

**Core Concept**: Code = Projection of Knowledge Graph

```
Domain Ontology (RDF)
       ↓
SPARQL Queries (Extract structure)
       ↓
Template Rendering (Generate code)
       ↓
Language-Specific Output
```

### 7.2 Type Mapping

**XSD to Language Types:**

| XSD Type | Rust | TypeScript | Python |
|----------|------|-----------|--------|
| `xsd:string` | `String` | `string` | `str` |
| `xsd:integer` | `i32` | `number` | `int` |
| `xsd:decimal` | `f64` | `number` | `Decimal` |
| `xsd:boolean` | `bool` | `boolean` | `bool` |
| `xsd:date` | `Date` | `Date` | `date` |

### 7.3 Ontology Evolution

**Workflow:**
1. Define domain ontology (RDF)
2. Generate code (any language)
3. Evolve ontology (add properties/classes)
4. Regenerate code (only changed templates)
5. Validate consistency (SPARQL queries)

**Change Detection:**
- Delta-driven projection identifies changes
- Impact analysis determines affected templates
- Selective regeneration improves performance

---

## 8. Testing & Validation

### 8.1 Test Structure

**Location:** `tests/`

Test Types:
- **Unit Tests** - Individual modules
- **Integration Tests** - Multi-module workflows
- **E2E Tests** - Container-based full cycles
- **Chicago TDD Tests** - 782-line production-grade test

### 8.2 Validation Systems

1. **Linting** - Template syntax validation
2. **SHACL** - RDF data shape validation
3. **Type Checking** - Rust compile-time checks
4. **Determinism** - Byte-identical output verification

---

## 9. Performance & Optimization

### 9.1 Caching Strategy

**Graph Caching:**
- Query plan cache (100 entries default)
- Result cache (1000 entries default)
- Epoch-based invalidation on mutations

**Template Caching:**
- `TemplateCache` - Parsed template caching
- Reduces parsing overhead for repeated use

### 9.2 Async Runtime

- Full tokio async runtime
- Non-blocking file I/O
- Concurrent SPARQL queries
- Streaming generation for large projects

---

## 10. Real-World Usage Patterns

### 10.1 Ontology-Driven Development

```bash
# 1. Generate ontology from natural language
ggen ai generate-ontology --prompt "E-commerce: Product, Order, Review" --output domain.ttl

# 2. Generate templates for different languages
ggen template generate-rdf --ontology domain.ttl --template rust-models
ggen template generate-rdf --ontology domain.ttl --template typescript-models
ggen template generate-rdf --ontology domain.ttl --template python-pydantic

# 3. Update ontology
# (Edit domain.ttl to add Review.sentiment: xsd:decimal)

# 4. Regenerate - code in all languages auto-updates
ggen template regenerate --ontology domain.ttl
```

### 10.2 Marketplace Integration

```bash
# Search for pre-built templates
ggen marketplace search "rust microservice"

# Install and use
ggen marketplace install io.ggen.rust.microservice
ggen project gen my-service --template io.ggen.rust.microservice
```

### 10.3 Git Hook Automation

```bash
# Validate ontology before commit
ggen hook create pre-commit --name validate-ontology

# Auto-sync on merge
ggen hook create post-merge --name sync-ontology
```

---

## 11. Unique Technical Achievements

1. **610 files contain "graph"** - Not a feature add-on, it's architectural
2. **Real RDF/SPARQL** - Oxigraph in-memory triple store, not mocks
3. **Deterministic Output** - Byte-identical, reproducible builds
4. **Zero Unsafe Code** - Memory-safe in production paths
5. **Type-Safe RDF** - GgenOntology programmatic API
6. **Production Tested** - 782-line E2E test with real systems
7. **Polyglot Sync** - One ontology → N languages with zero drift

---

## Conclusion

ggen is a mature, production-grade code generation framework that uniquely treats code as projections of RDF knowledge graphs. Its architecture separates concerns (CLI, domain, core), ensures type safety through the Rust compiler, and provides a comprehensive system for ontology-driven development.

The system excels at:
- **Single Source of Truth** - RDF ontology drives all code generation
- **Polyglot Development** - Generate Rust, TypeScript, Python from one ontology
- **Evolutionary Architecture** - Change ontology → code auto-updates
- **Production Readiness** - Deterministic, type-safe, well-tested

Key files to understand:
- `crates/ggen-core/src/rdf/schema.ttl` - Core ontology definition
- `crates/ggen-core/src/generator.rs` - Generation orchestration
- `crates/ggen-core/src/pipeline.rs` - Template processing pipeline
- `crates/ggen-core/src/graph/core.rs` - RDF graph management
- `crates/ggen-domain/src/template/` - Business logic layer
