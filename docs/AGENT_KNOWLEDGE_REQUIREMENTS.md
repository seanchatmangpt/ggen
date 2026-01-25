# Agent Knowledge Requirements for Building Advanced ggen Projects

## Purpose

This document outlines the complete knowledge set an advanced agent needs to autonomously build production-ready ggen projects from scratch. No marketing, no human-focused content—just what agents need to know.

---

## 1. Workspace Architecture

### 1.1 Crate Organization

**Core Crates:**
- `ggen-core` - RDF graph management, template engine, project generation, lifecycle orchestration
- `ggen-cli` - Command-line interface using clap-noun-verb auto-discovery
- `ggen-domain` - Pure business logic layer (zero CLI dependencies)
- `ggen-marketplace-v2` - RDF-backed package marketplace with SPARQL search
- `ggen-utils` - Shared error handling, configuration, logging, alerts

**Supporting Crates:**
- `ggen-config` - Configuration management (ggen.toml parsing)
- `ggen-config-clap` - Clap integration for config
- `ggen-ai` - AI integration (Ollama, OpenAI, code analysis)
- `ggen-macros` - Procedural macros
- `ggen-dod` - Design of Design (DoD) doctrine engine
- `ggen-node` - Node.js bindings
- `ggen-cli-validation` - CLI validation utilities

**Dependency Flow:**
```
ggen (binary)
  └─ ggen-cli (CLI layer)
       └─ ggen-domain (business logic)
            ├─ ggen-core (template/RDF engine)
            ├─ ggen-marketplace-v2 (package system)
            ├─ ggen-ai (AI operations)
            └─ ggen-utils (shared utilities)
```

### 1.2 Module Hierarchy

**ggen-core modules:**
- `graph` - Oxigraph RDF store wrapper with SPARQL caching
- `rdf` - Template metadata, validation, relationships
- `template` - Tera-based template system with RDF integration
- `templates` - File tree generation from templates
- `pipeline` - Template processing pipeline
- `generator` - High-level generation engine
- `project_generator` - Project scaffolding (Rust, Next.js, etc.)
- `cli_generator` - CLI generation from ontologies
- `lifecycle` - Universal lifecycle system (make.toml)
- `ontology` - Ontology extraction, validation, MAPE-K loop
- `marketplace` - Registry client, cache, lockfile
- `security` - Command injection prevention, input validation

**ggen-domain modules:**
- `template` - Template operations (generate, lint, render)
- `graph` - Graph operations (RDF loading, SPARQL queries)
- `marketplace` - Marketplace operations (search, install, publish)
- `project` - Project operations (create, generate)
- `ai` - AI operations (code analysis, generation)
- `rdf` - RDF metadata operations
- `audit` - Security auditing
- `ci` - CI/CD operations
- `shell` - Shell completion generation

**ggen-cli modules:**
- `cmds` - Command router with noun modules (ai, graph, template, project, packs)
- `conventions` - File-based routing conventions
- `runtime` - Async/sync bridge utilities

---

## 2. CLI Command Implementation (clap-noun-verb)

### 2.1 Command Discovery Pattern

**Auto-discovery mechanism:**
```rust
// cmds/mod.rs
pub mod template;  // Automatically discovered
pub mod graph;
pub mod project;
// ...

pub fn run_cli() -> Result<()> {
    clap_noun_verb::run()
        .map_err(|e| Error::new(&format!("CLI execution failed: {}", e)))?;
    Ok(())
}
```

**Verb function pattern (v3.4.0):**
```rust
// cmds/template.rs
use clap_noun_verb_macros::verb;

#[verb]
fn generate(
    template: String,
    output: Option<String>,
    #[option(short, long)] vars: Vec<String>
) -> NounVerbResult<GenerateOutput> {
    // 1. Parse CLI args
    // 2. Call domain logic (ggen_domain::template::generate)
    // 3. Return structured output
    Ok(GenerateOutput { ... })
}
```

### 2.2 CLI-to-Domain Bridge

**Pattern:**
1. CLI layer (`ggen-cli/src/cmds/*.rs`) receives arguments
2. CLI parses args into domain types
3. CLI calls domain function (`ggen-domain::*`)
4. Domain returns `Result<T>`
5. CLI converts to `NounVerbResult<Output>` with structured output

**Example:**
```rust
// CLI layer
#[verb]
fn show(template: String) -> NounVerbResult<ShowOutput> {
    use ggen_domain::template::show;

    let metadata = show::show_template_metadata(&template)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("{}", e)))?;

    Ok(ShowOutput { ... })
}

// Domain layer (ggen-domain/src/template/show.rs)
pub async fn show_template_metadata(template: &str) -> Result<TemplateMetadata> {
    // Pure business logic - no CLI dependencies
}
```

---

## 3. RDF & SPARQL Integration (Oxigraph)

### 3.1 Graph Management

**Core pattern:**
```rust
use ggen_core::graph::Graph;

// Create graph
let graph = Graph::new()?;

// Load RDF (Turtle format)
graph.insert_turtle(r#"
    @prefix ex: <http://example.org/> .
    ex:alice a ex:Person ;
             ex:name "Alice" .
"#)?;

// Query with SPARQL
let results = graph.query("SELECT ?s ?o WHERE { ?s ex:name ?o }")?;
```

**Oxigraph 0.5 Best Practices:**
- **Always use `.map_err()`** for error conversion (oxigraph errors don't implement `From`)
- **RdfSerializer pattern:** `RdfSerializer::from_format().for_writer()` → `serialize_quad()` → `finish()`
- **Resource management:** Store instances are `Arc`-wrapped for thread-safe sharing
- **Query caching:** LRU cache with epoch-based invalidation in `Graph`

### 3.2 Template-RDF Integration

**Frontmatter fields:**
```yaml
---
to: output/{{name}}.rs
base: http://example.org/
prefixes:
  ex: http://example.org/
  schema: http://schema.org/
rdf_inline:
  - |
    @prefix ex: <http://example.org/> .
    ex:{{name}} a ex:Project .
rdf:
  - path/to/ontology.ttl
sparql:
  entities: |
    SELECT ?name ?type WHERE {
      ?s ex:name ?name ;
         a ?type .
    }
---
# Template body can access:
# - sparql_results.entities (array of bindings)
# - sparql_first(results=..., column="name")
# - sparql_values(results=..., column="name")
```

**SPARQL result access in templates:**
```tera
Count: {{ sparql_results.entities | length }}
First: {{ sparql_first(results=sparql_results.entities, column="name") }}
All: {{ sparql_values(results=sparql_results.entities, column="name") }}
```

### 3.3 Ggen Ontology

**Namespace constants:**
```rust
use ggen_core::rdf::GGEN_NAMESPACE;

// Standard predicates
const HAS_VARIABLE: &str = "ggen:hasVariable";
const HAS_DEPENDENCY: &str = "ggen:hasDependency";
const TARGETS_LANGUAGE: &str = "ggen:targetsLanguage";
```

**Template metadata storage:**
```rust
use ggen_core::rdf::TemplateMetadataStore;

let store = TemplateMetadataStore::new(graph);
store.store("template.tmpl", &metadata)?;
let retrieved = store.retrieve("template.tmpl")?;
```

---

## 4. Template Generation System (Tera)

### 4.1 Template Processing Flow

```
Template String → Parse → Render Frontmatter → Load RDF → Process Graph → Render Body
```

**Two-phase rendering:**
1. **Phase 1:** Render frontmatter to resolve `{{vars}}`
2. **Phase 2:** Load RDF, execute SPARQL, render body

### 4.2 Template Structure

```tera
---
to: src/{{module_name}}.rs
rdf_inline:
  - "@prefix ex: <http://example.org/> ."
sparql:
  functions: |
    SELECT ?name ?signature WHERE {
      ?fn a ex:Function ;
          ex:name ?name ;
          ex:signature ?signature .
    }
---
// Module: {{ module_name }}

{% for fn in sparql_results.functions %}
pub fn {{ fn.name }}{{ fn.signature }} {
    // Implementation
}
{% endfor %}
```

### 4.3 Custom Tera Filters

**SPARQL filters:**
- `sparql_first(results, column)` - Get first value from column
- `sparql_values(results, column)` - Get all values from column as array
- `sparql_map(results, from, to)` - Map column values

**Registration:**
```rust
use ggen_core::register::register_all_filters;

let mut tera = Tera::default();
register_all_filters(&mut tera)?;
```

---

## 5. Marketplace Package System

### 5.1 Package Structure

**package.toml manifest:**
```toml
[package]
name = "rest-api-template"
version = "1.0.0"
description = "Production-ready REST API template"
category = "core-power-packages"
tags = ["rest", "api", "backend"]
author = "GGEN Marketplace"
license = "MIT"

[ontology]
primary = "ontology/rest-api.ttl"
format = "turtle"
validation = "shacl"

[sparql]
templates = "sparql/queries.rq"
query_count = 15

[generation]
languages = ["rust", "typescript", "python"]
frameworks = { rust = "axum", typescript = "express", python = "fastapi" }

[generation.rust]
template_dir = "templates/rust"
entry_point = "main.rs"
dependencies = [
  "axum = { version = '0.7', features = ['macros'] }",
  # ...
]

[testing]
framework = "chicago-tdd"
test_dir = "tests/chicago_tdd"
coverage_threshold = 80
```

### 5.2 Package Discovery

**RDF-backed registry:**
```rust
use ggen_marketplace_v2::{RdfRegistry, SearchEngine};

let registry = RdfRegistry::new()?;
let search = SearchEngine::new(registry);

// Search with SPARQL
let results = search.search("REST API backend")?;
```

**Package installation:**
```rust
use ggen_marketplace_v2::Installer;

let installer = Installer::new(cache_dir);
installer.install("rest-api-template", "1.0.0")?;
```

### 5.3 Package Validation

**Manifest validation:**
- Package metadata completeness
- Ontology file existence
- SPARQL query validity
- Template directory structure
- Dependency resolution

**Security checks:**
- Signature verification (Ed25519)
- Path traversal prevention
- Command injection prevention

---

## 6. Configuration System

### 6.1 ggen.toml Structure

**Project configuration:**
```toml
[project]
name = "my-project"
version = "1.0.0"
description = "Project description"
authors = ["author"]

[ai]
provider = "ollama"
model = "qwen2.5-coder"
temperature = 0.7
max_tokens = 4000

[templates]
directory = "templates"
backup_enabled = true

[templates.rust]
style = "core-team"
error_handling = "thiserror"
logging = "tracing"
async_runtime = "tokio"

[rdf]
base_uri = "https://example.com/project/"
prefixes = { ex = "https://example.com/project/", schema = "http://schema.org/" }

[sparql]
timeout = 10
max_results = 1000
cache_enabled = true

[lifecycle]
enabled = true
config_file = "make.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true

[performance]
parallel_execution = true
max_workers = 16
cache_size = "1GB"
```

### 6.2 Makefile.toml (Lifecycle)

**Task definition pattern:**
```toml
[tasks.build]
description = "Build in debug mode"
command = "timeout"
args = ["10s", "cargo", "build"]

[tasks.test]
description = "Run all tests"
command = "timeout"
args = ["30s", "cargo", "test", "--all-features"]
dependencies = ["build"]

[tasks.pre-commit]
description = "Pre-commit validation"
dependencies = ["fmt", "lint", "test-unit"]
```

**Critical tasks:**
- `check` - Quick compilation check (5s timeout)
- `build` - Debug build (10s timeout)
- `build-release` - Release build (30s timeout)
- `test` - All tests (30s timeout)
- `test-unit` - Unit tests only (10s timeout)
- `lint` - Clippy linting (15s timeout)
- `fmt` - Code formatting (5s timeout)
- `pre-commit` - Full validation (30s timeout)

---

## 7. Build System & CI/CD

### 7.1 Cargo Make Integration

**ALWAYS use `cargo make`:**
```bash
# ✅ CORRECT
cargo make check
cargo make test
cargo make lint

# ❌ WRONG - NEVER use direct cargo commands
cargo check
cargo test
cargo clippy
```

**Timeout SLAs:**
- Quick checks: 5s (cargo check, cargo fmt, cargo clippy)
- Compilation: 10s (debug), 30s (release)
- Unit tests: 10s
- Integration tests: 30s
- Pre-push hooks: 30s

### 7.2 Pre-Commit Hooks

**Hook installation:**
```bash
ln -s ../../scripts/pre-commit-hook.sh .git/hooks/pre-commit
```

**Validation sequence:**
1. Verify timeout command exists
2. Compilation check (`cargo make check`)
3. Unit tests (`cargo make test-unit`)
4. Linting (`cargo make lint`)
5. Formatting check (`cargo make fmt`)

**Andon Signal Workflow:**
- Red signal (CRITICAL): Compiler errors, test failures → STOP THE LINE
- Yellow signal (HIGH): Compiler warnings, linting errors → STOP THE LINE
- Green signal: All checks pass → Proceed

### 7.3 Performance SLOs

**Target metrics:**
- First build: ≤ 15s
- Incremental build: ≤ 2s
- RDF processing: ≤ 5s for 1k+ triples
- Generation memory: ≤ 100MB
- CLI scaffolding: ≤ 3s end-to-end
- Deterministic output: 100% reproducible

---

## 8. Error Handling & Validation

### 8.1 Error Type Hierarchy

**Core error type:**
```rust
use ggen_utils::error::{Error, Result, Context};

// Create error
let err = Error::new("Something failed");

// With context
let result: Result<T> = operation()
    .context("Failed to execute operation")?;

// Macros
bail!("Error message");
ensure!(condition, "Error if false");
```

**Error propagation:**
```rust
// Oxigraph errors (always use .map_err())
store.load_from_reader(format, reader)
    .map_err(|e| Error::new(&format!("Failed to load RDF: {}", e)))?;

// Domain errors (use ? operator)
let result = domain_function()?;
```

### 8.2 Validation Chains

**Template validation:**
1. Frontmatter parsing (YAML syntax)
2. RDF validation (Turtle syntax, SHACL shapes)
3. SPARQL validation (query syntax)
4. Variable validation (required variables present)
5. Output path validation (security checks)

**Security validation:**
- Path traversal prevention (reject `..` in paths)
- Command injection prevention (sanitize shell commands)
- Template sandboxing (restricted Tera functions)

---

## 9. Testing Strategy (Chicago TDD)

### 9.1 Test Categories

**Unit tests:**
- Location: `crates/*/src/*_test.rs` or `#[cfg(test)] mod tests`
- Pattern: State-based testing with real collaborators
- Structure: AAA (Arrange-Act-Assert)
- Coverage: 80%+ for public APIs

**Integration tests:**
- Location: `/tests` and `crates/*/tests`
- Pattern: End-to-end workflows with real filesystem
- Tools: chicago-tdd-tools, testcontainers
- Focus: Critical user flows

**Property tests:**
- Tool: proptest
- Target: Parsers, RDF operations, template rendering
- Goal: Edge case discovery

**Snapshot tests:**
- Tool: insta
- Target: Deterministic code generation outputs
- Goal: Regression prevention

### 9.2 Chicago TDD Principles

**State-based testing:**
```rust
#[test]
fn test_template_generation() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();
    let template = "Hello {{name}}";
    let vars = btreemap!{ "name" => "World" };

    // Act
    let output = generate_template(template, vars, temp_dir)?;

    // Assert
    assert!(output.exists());
    assert_eq!(read_to_string(output)?, "Hello World");
    Ok(())
}
```

**Real collaborators (not mocks):**
- Use real filesystem (tempfile)
- Use real RDF store (Oxigraph in-memory)
- Use real HTTP clients (with test servers)

**Behavior verification:**
- Verify observable outputs (files created, content generated)
- Verify state changes (RDF triples added, cache populated)
- Verify side effects (hooks executed, logs written)

### 9.3 Test Utilities

**Common test helpers:**
```rust
// From tests/common/mod.rs
use common::{create_temp_dir, sample_make_toml, write_file_in_temp};

let temp_dir = create_temp_dir();
let make = sample_make_toml();
write_file_in_temp(&temp_dir, "config.toml", "content");
```

**Assertions:**
```rust
use chicago_tdd_tools::test;

assert!(result.is_ok());
assert_eq!(actual, expected);
assert!(output.contains("expected"));
```

---

## 10. Polyglot Code Generation

### 10.1 Language Target Patterns

**Rust generation:**
```rust
use ggen_core::project_generator::{ProjectConfig, ProjectType, create_new_project};

let config = ProjectConfig {
    name: "my-cli".to_string(),
    project_type: ProjectType::RustCli,
    framework: None,
    path: PathBuf::from("."),
};

create_new_project(&config).await?;
```

**TypeScript generation:**
```rust
let config = ProjectConfig {
    project_type: ProjectType::NextJs,
    framework: Some("next".to_string()),
    // ...
};
```

**Python generation:**
```rust
let config = ProjectConfig {
    project_type: ProjectType::Python,
    framework: Some("fastapi".to_string()),
    // ...
};
```

### 10.2 Language-Specific Templates

**Template directory structure:**
```
templates/
  rust/
    main.rs.tmpl
    lib.rs.tmpl
    Cargo.toml.tmpl
  typescript/
    index.ts.tmpl
    package.json.tmpl
    tsconfig.json.tmpl
  python/
    main.py.tmpl
    requirements.txt.tmpl
    pyproject.toml.tmpl
```

**Language detection:**
```rust
// From template frontmatter
[generation]
languages = ["rust", "typescript", "python"]
frameworks = {
  rust = "axum",
  typescript = "express",
  python = "fastapi"
}
```

---

## 11. Key Interfaces & Traits

### 11.1 Core Traits

**Template processing:**
```rust
pub trait Template {
    fn render(&self, context: &Context) -> Result<String>;
    fn parse(content: &str) -> Result<Self>;
}
```

**RDF operations:**
```rust
pub trait GraphOperations {
    fn query(&self, sparql: &str) -> Result<Vec<QueryResult>>;
    fn insert(&self, turtle: &str) -> Result<()>;
    fn export(&self, format: RdfFormat) -> Result<String>;
}
```

**Marketplace:**
```rust
pub trait AsyncRepository {
    async fn search(&self, query: &str) -> Result<Vec<Package>>;
    async fn install(&self, name: &str, version: &str) -> Result<()>;
}

pub trait Validatable {
    fn validate(&self) -> Result<ValidationReport>;
}

pub trait Signable {
    fn sign(&self, key: &SigningKey) -> Result<Signature>;
    fn verify(&self, key: &VerifyingKey) -> Result<bool>;
}
```

### 11.2 Type-Level Patterns

**PhantomData state machines:**
```rust
pub struct FileHandle<S> {
    path: PathBuf,
    _state: PhantomData<S>,
}

pub struct Open;
pub struct Closed;

impl FileHandle<Closed> {
    pub fn open(self) -> Result<FileHandle<Open>> { ... }
}

impl FileHandle<Open> {
    pub fn read(&self) -> Result<String> { ... }
    pub fn close(self) -> FileHandle<Closed> { ... }
}
```

**Const generics:**
```rust
pub struct FixedBuffer<const N: usize> {
    data: [u8; N],
}

impl<const N: usize> FixedBuffer<N> {
    pub fn new() -> Self { ... }
}
```

**Zero-cost newtypes:**
```rust
#[repr(transparent)]
pub struct NonEmptyString(String);

impl NonEmptyString {
    pub fn new(s: String) -> Result<Self> {
        if s.is_empty() {
            bail!("String cannot be empty");
        }
        Ok(Self(s))
    }
}
```

---

## 12. Performance Optimization Points

### 12.1 Compilation Performance

**Profile configuration:**
```toml
[profile.dev]
opt-level = 0
debug = true
codegen-units = 256  # Parallel compilation
incremental = true   # Incremental builds

[profile.release]
opt-level = 3
lto = "thin"         # Link-time optimization
codegen-units = 16   # Balance speed/optimization
strip = true         # Strip symbols
```

**Dependency optimization:**
- Use workspace dependencies for version consistency
- Minimize duplicate crates
- Use `cargo tree -d` to detect duplicates

### 12.2 Runtime Performance

**RDF query caching:**
```rust
// LRU cache with epoch-based invalidation
let graph = Graph::new()?;
let cached_results = graph.query(sparql)?;  // Cached
```

**Parallel execution:**
```rust
use rayon::prelude::*;

templates.par_iter()
    .map(|t| t.generate())
    .collect::<Result<Vec<_>>>()?;
```

**Memory management:**
- Use `Arc` for shared ownership (cheap cloning)
- Use `Cow` for clone-on-write (zero-copy when possible)
- Use `&str` over `String` for read-only data

### 12.3 Benchmarking

**Criterion.rs pattern:**
```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_template_render(c: &mut Criterion) {
    let template = Template::parse("Hello {{name}}").unwrap();

    c.bench_function("template_render", |b| {
        b.iter(|| {
            template.render(black_box(&context))
        });
    });
}

criterion_group!(benches, bench_template_render);
criterion_main!(benches);
```

---

## 13. Advanced Agent Integration Patterns

### 13.1 Autonomous Project Generation

**Pattern:**
1. Agent analyzes user requirements
2. Agent selects marketplace package or creates custom ontology
3. Agent generates RDF ontology representing domain model
4. Agent creates SPARQL queries for code generation
5. Agent generates templates (Rust/TS/Python)
6. Agent executes generation pipeline
7. Agent validates outputs (compilation, tests, security)

**Example workflow:**
```rust
// 1. Create ontology
let ontology = r#"
    @prefix ex: <http://example.org/> .
    ex:UserService a ex:Service ;
        ex:hasEndpoint ex:createUser, ex:getUser, ex:updateUser, ex:deleteUser .
"#;

// 2. Generate SPARQL queries
let queries = btreemap! {
    "endpoints" => r#"
        SELECT ?endpoint WHERE {
            ?service ex:hasEndpoint ?endpoint .
        }
    "#,
};

// 3. Generate from template
let output = generate_project(ProjectConfig {
    name: "user-service",
    project_type: ProjectType::RustCli,
    ontology: Some(ontology.to_string()),
    sparql_queries: queries,
    // ...
})?;
```

### 13.2 Marketplace Package Creation

**Package creation workflow:**
1. Define ontology (domain model in RDF)
2. Create SPARQL queries (extract data from ontology)
3. Build templates (Rust/TS/Python with SPARQL integration)
4. Write package.toml manifest
5. Add tests (Chicago TDD pattern)
6. Validate package structure
7. Sign package (Ed25519)
8. Publish to marketplace

**Manifest template:**
```toml
[package]
name = "{{package_name}}"
version = "1.0.0"
description = "{{description}}"
category = "custom-packages"
tags = [{{tags}}]

[ontology]
primary = "ontology/{{ontology_name}}.ttl"
format = "turtle"

[generation.rust]
template_dir = "templates/rust"
entry_point = "main.rs"
dependencies = [
  # Generated from ontology
]
```

### 13.3 Debugging Compilation Issues

**Common issues:**

1. **Oxigraph error handling:**
   - Symptom: `the trait From<...> is not implemented`
   - Fix: Use `.map_err(|e| Error::new(&format!("{}", e)))?`

2. **Workspace dependencies:**
   - Symptom: Duplicate crate versions
   - Fix: Use `workspace = true` in Cargo.toml dependencies

3. **Clippy lints:**
   - Symptom: `deny(warnings)` fails on clippy warnings
   - Fix: Add `#[allow(clippy::...)]` with justification

4. **Async runtime:**
   - Symptom: `cannot be sent between threads safely`
   - Fix: Ensure all futures are `Send` (use `#[tokio::test]`)

---

## 14. Extending the System

### 14.1 Adding New CLI Commands

**Steps:**
1. Create noun module in `crates/ggen-cli/src/cmds/mynoun.rs`
2. Define verb functions with `#[verb]` attribute
3. Add domain logic in `crates/ggen-domain/src/mynoun/`
4. Add module to `cmds/mod.rs`
5. Auto-discovered by clap-noun-verb

**Example:**
```rust
// crates/ggen-cli/src/cmds/cache.rs
use clap_noun_verb_macros::verb;

#[verb]
fn clear() -> NounVerbResult<ClearOutput> {
    use ggen_domain::cache;

    let result = cache::clear_cache()?;
    Ok(ClearOutput { files_deleted: result.count })
}

// crates/ggen-domain/src/cache.rs
pub fn clear_cache() -> Result<ClearResult> {
    // Pure business logic
}
```

### 14.2 Adding New Template Filters

**Steps:**
1. Define filter function in `crates/ggen-core/src/register.rs`
2. Register in `register_all_filters()`
3. Use in templates

**Example:**
```rust
// crates/ggen-core/src/register.rs
pub fn snake_case(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let s = value.as_str().ok_or("Expected string")?;
    let snake = s.to_lowercase().replace(' ', "_");
    Ok(Value::String(snake))
}

pub fn register_all_filters(tera: &mut Tera) -> Result<()> {
    tera.register_filter("snake_case", snake_case);
    // ...
}
```

### 14.3 Adding New Marketplace Categories

**Steps:**
1. Define category in marketplace ontology
2. Add SPARQL query for category
3. Update search engine to recognize category
4. Add category to package.toml validation

**Category ontology:**
```turtle
@prefix ggen: <https://ggen.ai/ontology#> .

ggen:Category a rdfs:Class .

ggen:WebBackend a ggen:Category ;
    rdfs:label "Web Backend" ;
    ggen:description "Backend API templates" .
```

---

## 15. Critical Implementation Rules

### 15.1 Poka-Yoke (Error Prevention)

**Type-first thinking:**
- Use types to encode invariants (NonEmptyString, NonEmptyPath)
- Use PhantomData state machines (FileHandle<Open> vs FileHandle<Closed>)
- Use const generics for compile-time bounds
- Make invalid states unrepresentable

**Compiler-enforced correctness:**
- `#![deny(warnings)]` - Treat all warnings as errors
- `#![forbid(unsafe_code)]` - No unsafe code
- `unwrap_used = "deny"` - No unwrap() in production code
- `expect_used = "deny"` - No expect() in production code

### 15.2 Andon Signals (Stop the Line)

**Signal workflow:**
1. **Monitor:** Run `cargo make check`, `cargo make test`, `cargo make lint`
2. **Stop:** When signal appears, STOP work immediately
3. **Investigate:** Use 5 Whys root cause analysis
4. **Fix:** Address root cause, not symptom
5. **Verify:** Re-run checks to confirm signal cleared

**Signal types:**
- **CRITICAL (Red):** Compiler errors, test failures → MUST stop
- **HIGH (Yellow):** Compiler warnings, linting errors → SHOULD stop
- **MEDIUM (Yellow):** Performance regressions → INVESTIGATE

### 15.3 Quality Gates

**Before marking task complete:**
1. ✅ Verify timeout command exists (`cargo make timeout-check`)
2. ✅ Check for compiler errors (`cargo make check`)
3. ✅ Check for compiler warnings (review output)
4. ✅ Run tests (`cargo make test`) - ALL must pass
5. ✅ Check linting (`cargo make lint`) - NO warnings/errors
6. ✅ Verify SLOs (`cargo make slo-check`)
7. ✅ All Andon signals cleared

**Never:**
- Never mark complete without running tests
- Never suppress warnings without fixing root cause
- Never use placeholders or `unimplemented!()`
- Never use `unwrap()` or `expect()` in production code
- Never skip timeout wrappers
- Never use direct `cargo` commands (always use `cargo make`)

---

## 16. Example Workflows

### 16.1 Creating a REST API Project

```bash
# 1. Create project from marketplace package
cargo run -- project create \
  --name my-api \
  --package rest-api-template \
  --vars api_name=MyAPI,port=8080

# 2. Generate code from ontology
cargo run -- template generate \
  --template templates/rust/rest-api.tmpl \
  --rdf ontology/api.ttl \
  --output generated/

# 3. Run lifecycle phases
cd my-api
cargo make init
cargo make setup
cargo make build
cargo make test

# 4. Deploy
cargo make docker
cargo make deploy
```

### 16.2 Creating a Custom Marketplace Package

```bash
# 1. Create package structure
mkdir -p my-package/{ontology,templates,sparql,tests}

# 2. Define ontology
cat > my-package/ontology/package.ttl <<EOF
@prefix ex: <http://example.org/> .
ex:MyPackage a ex:Package ;
    ex:name "my-package" ;
    ex:version "1.0.0" .
EOF

# 3. Create SPARQL queries
cat > my-package/sparql/entities.rq <<EOF
SELECT ?name ?type WHERE {
  ?entity ex:name ?name ;
          a ?type .
}
EOF

# 4. Create templates
cat > my-package/templates/rust/main.rs.tmpl <<EOF
---
to: src/main.rs
sparql:
  entities: sparql/entities.rq
---
// Generated entities
{% for entity in sparql_results.entities %}
pub struct {{ entity.name }} { /* ... */ }
{% endfor %}
EOF

# 5. Write package.toml
cat > my-package/package.toml <<EOF
[package]
name = "my-package"
version = "1.0.0"
# ...
EOF

# 6. Validate package
cargo run -- marketplace validate my-package/

# 7. Publish package
cargo run -- marketplace publish my-package/
```

### 16.3 Debugging Generation Issues

```bash
# 1. Enable debug logging
export RUST_LOG=debug

# 2. Generate with verbose output
cargo run -- template generate \
  --template my-template.tmpl \
  --rdf ontology.ttl \
  --verbose

# 3. Inspect RDF graph
cargo run -- graph query \
  --rdf ontology.ttl \
  --sparql "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

# 4. Validate template
cargo run -- template lint my-template.tmpl

# 5. Test template with sample data
cargo run -- template render \
  --template my-template.tmpl \
  --vars name=test,version=1.0 \
  --output test-output/
```

---

## 17. Agent Decision Tree

### 17.1 Command Selection

```
User Request → Classify Intent
  ├─ "Create new project" → project create
  ├─ "Generate code from template" → template generate
  ├─ "Search packages" → marketplace search
  ├─ "Install package" → marketplace install
  ├─ "Query RDF" → graph query
  ├─ "Load RDF" → graph load
  ├─ "Run lifecycle phase" → project run-phase
  └─ "AI code generation" → ai generate
```

### 17.2 Template vs Package

```
User Needs Code Generation
  ├─ Has specific template → Use template generate
  ├─ Has custom requirements → Create custom template
  ├─ Standard use case (REST API, CLI, etc.) → Search marketplace
  │   ├─ Package found → Install and use
  │   └─ No package → Create custom package
  └─ Complex domain model → Create ontology first
```

### 17.3 Language Selection

```
Target Language
  ├─ Rust → Use Rust templates, Cargo.toml generation
  ├─ TypeScript → Use TypeScript templates, package.json generation
  ├─ Python → Use Python templates, pyproject.toml generation
  └─ Multiple languages → Use polyglot package with language-specific templates
```

---

## 18. References

### 18.1 Key Files to Study

**Core implementation:**
- `crates/ggen-core/src/lib.rs` - Core exports and module structure
- `crates/ggen-core/src/graph/core.rs` - RDF graph operations
- `crates/ggen-core/src/template.rs` - Template processing
- `crates/ggen-cli/src/cmds/mod.rs` - CLI command router
- `crates/ggen-domain/src/lib.rs` - Domain logic organization

**Configuration:**
- `Cargo.toml` - Workspace configuration
- `Makefile.toml` - Build tasks and lifecycle
- `config/defaults.toml` - Default configuration
- `examples/microservices-architecture/ggen.toml` - Example project config

**Examples:**
- `marketplace/packages/rest-api-template/` - Complete package example
- `examples/advanced-rust-project/` - Advanced project structure
- `examples/microservices-architecture/` - Multi-service example

**Tests:**
- `tests/integration/lifecycle_tests.rs` - Lifecycle integration tests
- `crates/ggen-core/tests/template_comprehensive_test.rs` - Template tests

### 18.2 Essential Dependencies

**Core libraries:**
- `oxigraph` (0.5.1) - RDF store and SPARQL engine
- `tera` (1.20) - Template engine
- `clap-noun-verb` (5.0.0) - CLI auto-discovery
- `tokio` (1.47) - Async runtime
- `serde` (1.0) - Serialization

**Testing:**
- `chicago-tdd-tools` (1.4.0) - Chicago TDD utilities
- `testcontainers` (0.25) - Container-based testing
- `proptest` (1.8) - Property-based testing
- `insta` (1.43) - Snapshot testing
- `criterion` (0.7) - Benchmarking

**Utilities:**
- `thiserror` (2.0) - Error handling
- `anyhow` (1.0) - Error context
- `tracing` (0.1) - Logging
- `rayon` (1.11) - Parallel execution

---

## Conclusion

This document provides the complete knowledge set an advanced agent needs to autonomously build production-ready ggen projects. The agent should:

1. Understand the workspace architecture and crate dependencies
2. Know how to implement CLI commands using clap-noun-verb
3. Master RDF/SPARQL integration with Oxigraph
4. Use the Tera template system with RDF integration
5. Navigate the marketplace package system
6. Configure projects using ggen.toml and make.toml
7. Follow the build system and CI/CD patterns
8. Implement proper error handling and validation
9. Write Chicago TDD tests with real collaborators
10. Generate polyglot code (Rust/TypeScript/Python)
11. Understand key interfaces and type-level patterns
12. Optimize for performance (compilation and runtime)
13. Integrate with agent workflows for autonomous generation
14. Extend the system with new commands, filters, and packages
15. Follow Poka-Yoke and Andon Signal principles
16. Execute complete workflows from ontology to deployment

By following these patterns and principles, an agent can build complete, production-ready ggen projects with zero human intervention.
