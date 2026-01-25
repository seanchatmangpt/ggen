<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Architecture Gap Analysis - ggen v3.2 → v4.0](#architecture-gap-analysis---ggen-v32-%E2%86%92-v40)
  - [Executive Summary](#executive-summary)
    - [Critical Findings](#critical-findings)
  - [1. Module Cohesion Analysis](#1-module-cohesion-analysis)
    - [1.1 Current Module Structure](#11-current-module-structure)
    - [1.2 Cohesion Issues](#12-cohesion-issues)
  - [2. Dependency Cycles](#2-dependency-cycles)
    - [2.1 Identified Cycles](#21-identified-cycles)
    - [2.2 Module Dependency Graph](#22-module-dependency-graph)
    - [2.3 Breaking Cycles - Recommendations](#23-breaking-cycles---recommendations)
  - [3. API Consistency](#3-api-consistency)
    - [3.1 Error Handling Inconsistencies](#31-error-handling-inconsistencies)
    - [3.2 Async Pattern Inconsistencies](#32-async-pattern-inconsistencies)
    - [3.3 API Surface Inconsistencies](#33-api-surface-inconsistencies)
  - [4. State Management](#4-state-management)
    - [4.1 Lifecycle State Machine Analysis](#41-lifecycle-state-machine-analysis)
    - [4.2 State Persistence Gaps](#42-state-persistence-gaps)
    - [4.3 State Machine Bypass Points](#43-state-machine-bypass-points)
  - [5. Testability Analysis](#5-testability-analysis)
    - [5.1 Modules That Cannot Be Tested in Isolation](#51-modules-that-cannot-be-tested-in-isolation)
    - [5.2 Missing Abstractions for Testing](#52-missing-abstractions-for-testing)
    - [5.3 Test Coverage Gaps](#53-test-coverage-gaps)
  - [6. Extensibility](#6-extensibility)
    - [6.1 Cannot Add New Features Cleanly](#61-cannot-add-new-features-cleanly)
    - [6.2 Hard-Coded Conventions](#62-hard-coded-conventions)
  - [7. Performance Bottlenecks](#7-performance-bottlenecks)
    - [7.1 RDF Graph Performance](#71-rdf-graph-performance)
    - [7.2 Template Rendering Performance](#72-template-rendering-performance)
    - [7.3 File I/O Inefficiencies](#73-file-io-inefficiencies)
    - [7.4 Memory Inefficiencies](#74-memory-inefficiencies)
  - [8. Marketplace Integration](#8-marketplace-integration)
    - [8.1 Dual Marketplace Problem](#81-dual-marketplace-problem)
    - [8.2 Marketplace-Pack System Gap](#82-marketplace-pack-system-gap)
    - [8.3 ggen-marketplace-v2 Readiness](#83-ggen-marketplace-v2-readiness)
  - [9. CLI System Architecture](#9-cli-system-architecture)
    - [9.1 clap-noun-verb Auto-Discovery](#91-clap-noun-verb-auto-discovery)
    - [9.2 Command → Domain Flow](#92-command-%E2%86%92-domain-flow)
    - [9.3 Missing Command Features](#93-missing-command-features)
  - [10. Integration Improvement Plan](#10-integration-improvement-plan)
    - [10.1 High Priority (v4.0 Blockers)](#101-high-priority-v40-blockers)
    - [10.2 Medium Priority (v4.1)](#102-medium-priority-v41)
    - [10.3 Low Priority (v4.2+)](#103-low-priority-v42)
  - [11. Module Reorganization Recommendations](#11-module-reorganization-recommendations)
    - [11.1 Proposed Crate Structure for v4.0](#111-proposed-crate-structure-for-v40)
    - [11.2 Dependency Flow After Reorganization](#112-dependency-flow-after-reorganization)
    - [11.3 Migration Strategy](#113-migration-strategy)
  - [12. Refactoring Effort Estimation](#12-refactoring-effort-estimation)
    - [12.1 Critical Path (Must Complete for v4.0)](#121-critical-path-must-complete-for-v40)
    - [12.2 Important (Should Complete for v4.0)](#122-important-should-complete-for-v40)
    - [12.3 Nice to Have (Can Defer to v4.1)](#123-nice-to-have-can-defer-to-v41)
    - [12.4 Overall Timeline](#124-overall-timeline)
  - [13. Risk Assessment](#13-risk-assessment)
    - [13.1 High Risks](#131-high-risks)
    - [13.2 Medium Risks](#132-medium-risks)
    - [13.3 Low Risks](#133-low-risks)
  - [14. Recommendations Summary](#14-recommendations-summary)
    - [14.1 Immediate Actions (Week 1-2)](#141-immediate-actions-week-1-2)
    - [14.2 Short-Term (v4.0 - 3-4 months)](#142-short-term-v40---3-4-months)
    - [14.3 Medium-Term (v4.1 - 6-9 months)](#143-medium-term-v41---6-9-months)
    - [14.4 Long-Term (v4.2+ - 12+ months)](#144-long-term-v42---12-months)
  - [15. Conclusion](#15-conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Architecture Gap Analysis - ggen v3.2 → v4.0

**Analysis Date:** 2025-11-18
**Current Version:** 3.2.0
**Target Version:** 4.0.0
**Total Lines of Code (ggen-core):** ~40,615 lines across 98 files

## Executive Summary

ggen v3.2 exhibits significant architectural gaps preventing evolution to v4.0. The analysis reveals **7 critical architectural issues**, **15 moderate integration gaps**, and **structural debt** requiring an estimated **12-16 weeks** of refactoring effort.

### Critical Findings

1. **Marketplace Fragmentation** - Dual marketplace implementations (v1/v2) with incomplete migration
2. **Lifecycle Integration Gaps** - State machine not integrated with generation pipeline
3. **Pack System Incompleteness** - Phase 1 only, missing installation/resolution logic
4. **Module Cohesion Issues** - Tight coupling between unrelated subsystems
5. **API Inconsistency** - No unified error handling or async patterns
6. **CLI Dispatch Complexity** - clap-noun-verb auto-discovery hides command organization
7. **Testability Barriers** - Many modules cannot be tested in isolation

---

## 1. Module Cohesion Analysis

### 1.1 Current Module Structure

```
ggen-core (40,615 LOC)
├── Template System (6 modules, ~15,000 LOC)
│   ├── template.rs - Template parsing/rendering
│   ├── templates/ - File tree generation
│   ├── generator.rs - Generation engine
│   ├── pipeline.rs - Template pipeline
│   ├── preprocessor.rs - Template preprocessing
│   └── tera_env.rs - Tera environment
│
├── RDF System (5 modules, ~8,000 LOC)
│   ├── graph/ - RDF graph management
│   ├── rdf/ - Template metadata
│   ├── delta.rs - Delta-driven projection
│   ├── ontology/ - Ontology system
│   └── merge.rs - Three-way merge
│
├── Pack System (7 modules, ~10,000 LOC)
│   ├── gpack.rs - Manifest structure
│   ├── packs/ - Installation system (Phase 1 INCOMPLETE)
│   ├── registry.rs - Registry client
│   ├── resolver.rs - Template resolution
│   ├── cache.rs - Local cache
│   └── lockfile.rs - Dependency tracking
│
├── Lifecycle System (12 modules, ~8,000 LOC)
│   ├── lifecycle/ - Universal lifecycle
│   ├── state_machine.rs - Type-level states
│   ├── exec.rs - Phase execution
│   ├── hooks.rs - Hook system
│   ├── optimization.rs - Performance
│   └── production.rs - Readiness tracking
│
├── Project Generation (3 modules, ~5,000 LOC)
│   ├── project_generator/ - Scaffolding
│   └── cli_generator/ - CLI generation
│
└── Infrastructure (12+ modules, ~5,000 LOC)
    ├── snapshot.rs - Baseline management
    ├── inject.rs - File injection
    ├── github.rs - GitHub integration
    ├── telemetry.rs - Observability
    ├── pqc.rs - Post-quantum crypto
    └── cleanroom/ - Isolated execution
```

### 1.2 Cohesion Issues

**Issue 1: Template System Fragmentation**
- `template.rs` (31,375 LOC) - Monolithic, handles parsing, frontmatter, RDF, rendering
- `generator.rs` (23,238 LOC) - Duplicates template logic, security checks scattered
- `pipeline.rs` (30,885 LOC) - Mixes Tera initialization, RDF loading, preprocessor
- **Impact**: Cannot swap template engines, RDF tightly coupled to templates

**Issue 2: Pack System Split Across Multiple Layers**
```rust
// Pack concerns scattered across 7 files:
gpack.rs          // Manifest structure
packs/lockfile.rs // Lock management
packs/install.rs  // Installation (incomplete)
cache.rs          // Local cache
registry.rs       // Remote registry
resolver.rs       // Template resolution
lockfile.rs       // Legacy lockfile (DUPLICATE!)
```
- **Impact**: No single source of truth for pack management
- Two lockfile implementations exist (`lockfile.rs` vs `packs/lockfile.rs`)
- Installation logic incomplete (Phase 1 only - data structures, no actual install)

**Issue 3: RDF Graph Scattered Across Modules**
```rust
graph/core.rs      // Core graph operations
graph/store.rs     // Persistence
graph/query.rs     // SPARQL queries
graph/export.rs    // Export formats
delta.rs           // Delta computation
ontology/          // Ontology system (11 modules)
rdf/               // Template metadata
merge.rs           // Graph merging
```
- **Impact**: Cannot use RDF graph independently
- Ontology system has 11 submodules but tight coupling to graph
- Delta-driven projection not integrated with lifecycle

**Issue 4: Lifecycle System Disconnected**
- Lifecycle has complete state machine (`lifecycle/state_machine.rs`)
- State machine NOT used by `generator.rs` or `pipeline.rs`
- `exec.rs` runs phases but doesn't update Generator context
- **Impact**: Can have "Built" state but no actual build artifacts
- Type-level guarantees bypassed in practice

---

## 2. Dependency Cycles

### 2.1 Identified Cycles

**Cycle 1: Template ↔ RDF Graph**
```rust
template.rs
  → uses graph::Graph for SPARQL queries
  → process_graph() mutates graph state

graph/core.rs
  → queries return results for template rendering
  → no cycle but tight coupling
```
**Not a true cycle but creates bidirectional dependency**

**Cycle 2: Generator → Pipeline → Template → Generator Context**
```rust
generator.rs
  → creates Pipeline
  → passes GenContext

pipeline.rs
  → initializes Tera
  → loads RDF into Graph
  → returns Pipeline to Generator

generator.rs
  → calls template.render() with pipeline.tera
  → calls template.process_graph() with pipeline.graph
```
**Data flows in circle but through shared state**

**Cycle 3: Packs System Circular Dependencies**
```rust
resolver.rs
  → uses CacheManager to find packs
  → uses LockfileManager to get versions

cache.rs
  → reads gpack.toml manifests
  → calls GpackManifest::load_from_file()

gpack.rs
  → discover_templates() uses glob patterns
  → could trigger resolver for dependencies (not implemented yet)
```
**Potential cycle when dependency resolution added**

### 2.2 Module Dependency Graph

```
High-Level Dependencies (ggen-core internally):

generator.rs
  ↓
pipeline.rs → template.rs → graph/core.rs
  ↓              ↓
tera_env.rs    rdf/schema.rs
  ↓
register.rs

resolver.rs → cache.rs → gpack.rs
               ↓
            lockfile.rs (legacy)
            packs/lockfile.rs (new)

lifecycle/exec.rs → lifecycle/state_machine.rs
                 → lifecycle/hooks.rs

project_generator/ → generator.rs
                  → lifecycle/
```

**Cross-Crate Dependencies:**
```
ggen-cli → ggen-domain → ggen-core
        ↓              ↓
   ggen-marketplace  ggen-marketplace-v2
        ↓              ↓
   ggen-utils       ggen-ai
```

### 2.3 Breaking Cycles - Recommendations

1. **Extract RDF Graph as Separate Crate**
   ```
   ggen-rdf-graph (new crate)
     ├── Graph core operations
     ├── SPARQL query engine
     ├── Delta computation
     └── Store/export

   ggen-core
     └── depends on ggen-rdf-graph (one-way)
   ```

2. **Extract Pack Management as Separate Crate**
   ```
   ggen-pack-manager (new crate)
     ├── GpackManifest
     ├── PackLockfile (unified)
     ├── CacheManager
     ├── RegistryClient
     ├── PackInstaller
     └── TemplateResolver

   ggen-core
     └── uses ggen-pack-manager for pack resolution
   ```

3. **Introduce Adapter Pattern for Template/RDF**
   ```rust
   trait GraphAdapter {
       fn query(&self, sparql: &str) -> Result<QueryResults>;
       fn insert(&mut self, triples: &str) -> Result<()>;
   }

   // Template doesn't depend on Graph directly
   impl Template {
       fn process_graph<G: GraphAdapter>(&self, graph: &mut G) -> Result<()>
   }
   ```

---

## 3. API Consistency

### 3.1 Error Handling Inconsistencies

**Issue: Multiple Error Types**
```rust
// ggen-core uses:
ggen_utils::error::Error     // Custom error type
ggen_utils::error::Result<T> // Alias for Result<T, Error>

// lifecycle module uses:
lifecycle::error::LifecycleError  // Enum with variants
lifecycle::error::Result<T>       // Separate Result alias

// Marketplace v2 uses:
ggen_marketplace_v2::error::Error // Yet another error type
ggen_marketplace_v2::error::Result<T>

// Some modules use anyhow directly
```

**Recommendation:**
- Standardize on `thiserror` for domain errors
- Use `anyhow::Error` for application errors (CLI layer)
- Provide conversion traits between error types

**Issue: Error Propagation Patterns**
```rust
// Some functions use ?
pub fn generate(&mut self) -> Result<PathBuf> {
    let input = fs::read_to_string(&self.ctx.template_path)?;
    // ...
}

// Others use match
pub fn load_cached(&self, pack_id: &str, version: &str) -> Result<CachedPack> {
    match fs::read_dir(&pack_dir) {
        Ok(entries) => { /* ... */ }
        Err(e) => return Err(Error::new(&format!("...")))
    }
}

// Some use map_err
let base_url = Url::parse(&registry_url)
    .map_err(|e| Error::new(&format!("Invalid URL: {}", e)))?;
```

**Recommendation:**
- Use `?` operator consistently
- Use `map_err` only for context enhancement
- Avoid manual `Error::new()` string formatting

### 3.2 Async Pattern Inconsistencies

**Issue: Mixed Sync/Async APIs**
```rust
// RegistryClient is async
impl RegistryClient {
    pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> { }
    pub async fn resolve(&self, pack_id: &str) -> Result<ResolvedPack> { }
}

// But CacheManager is sync
impl CacheManager {
    pub fn load_cached(&self, pack_id: &str) -> Result<CachedPack> { }
    pub fn list_cached(&self) -> Result<Vec<CachedPack>> { }
}

// Generator is sync
impl Generator {
    pub fn generate(&mut self) -> Result<PathBuf> { }
}

// ProjectGenerator is async
pub async fn create_new_project(config: &ProjectConfig) -> Result<()> { }
```

**Impact:**
- CLI commands use `runtime_helper.rs` to bridge sync/async
- Cannot compose sync and async operations cleanly
- Forces runtime creation in multiple places

**Recommendation:**
- Make all I/O operations async
- Provide sync wrappers only at CLI boundary
- Use `tokio::runtime::Handle::current()` for runtime access

### 3.3 API Surface Inconsistencies

**Issue: Constructor Patterns Vary**
```rust
// Some use new()
pub fn new() -> Result<Self> { }

// Others use with_*() builder pattern
pub fn with_dir(cache_dir: PathBuf) -> Result<Self> { }

// Some use default()
impl Default for PackConventions { }

// Some use from_*()
pub fn from_quads(old: Option<&Quad>, new: Option<&Quad>) -> Option<Self> { }

// ProjectConfig is a struct, not builder
pub struct ProjectConfig {
    pub name: String,
    pub project_type: ProjectType,
    pub framework: Option<String>,
    pub path: PathBuf,
}
```

**Recommendation:**
- Use `new()` for infallible construction
- Use `try_new()` or `builder()` for fallible construction
- Consistent builder pattern for complex types
- Use `Default` for zero-cost defaults

**Issue: Method Naming Conventions**
```rust
// Some use get_*
pub fn get_template_info(&self) -> Result<TemplateInfo> { }

// Others omit get_
pub fn template_info(&self) -> Result<TemplateInfo> { }

// Some use has_*
pub fn has_completed_phase(&self) -> bool { }

// Others use is_*
pub fn is_confident(&self, threshold: f64) -> bool { }
pub fn is_empty(&self) -> bool { }
```

**Recommendation:**
- Omit `get_` prefix (Rust convention)
- Use `is_*` for boolean predicates
- Use `has_*` for existence checks
- Use `with_*` for builder pattern

---

## 4. State Management

### 4.1 Lifecycle State Machine Analysis

**Current Implementation:**
```rust
// Type-level state machine (poka-yoke design)
pub struct LifecycleStateMachine<State> {
    state: LifecycleState,
    _marker: PhantomData<State>,
}

// States: Initial, Initialized, Setup, Built, Tested, Deployed

impl LifecycleStateMachine<Initial> {
    pub fn init(self) -> Result<LifecycleStateMachine<Initialized>> { }
}

impl LifecycleStateMachine<Initialized> {
    pub fn setup(self) -> Result<LifecycleStateMachine<Setup>> { }
}

// ... etc
```

**Strengths:**
- ✅ Compile-time state transition validation
- ✅ Prevents invalid phase ordering
- ✅ Zero runtime overhead (PhantomData)

**Weaknesses:**
- ❌ NOT integrated with `Generator`
- ❌ NOT used by `Pipeline`
- ❌ Cannot track which templates belong to which state
- ❌ State transitions don't trigger artifact generation

**Current Generator Flow:**
```rust
// Generator ignores lifecycle state machine
impl Generator {
    pub fn generate(&mut self) -> Result<PathBuf> {
        // 1. Parse template
        // 2. Render frontmatter
        // 3. Process graph
        // 4. Render body
        // 5. Write output

        // NO lifecycle state tracking!
        // NO build/test/deploy phases!
    }
}
```

**Gap: Generator Should Use Lifecycle**
```rust
// What it SHOULD be:
pub struct Generator<State> {
    pipeline: Pipeline,
    ctx: GenContext,
    lifecycle: LifecycleStateMachine<State>,
}

impl Generator<Setup> {
    pub fn generate(self) -> Result<(PathBuf, Generator<Built>)> {
        // Generate during build phase
        let output = self.generate_internal()?;
        let built_lifecycle = self.lifecycle.build()?;

        Ok((output, Generator {
            pipeline: self.pipeline,
            ctx: self.ctx,
            lifecycle: built_lifecycle,
        }))
    }
}
```

### 4.2 State Persistence Gaps

**Issue: Multiple State Files**
```rust
// Lifecycle state
.ggen/lifecycle.toml

// Pack lockfile (legacy)
ggen.lock

// Pack lockfile (new)
.ggen/packs.lock

// Cache state
~/.ggen/cache/

// Snapshot state
.ggen/snapshots/
```

**Impact:**
- No atomic updates across state files
- Race conditions possible
- Rollback requires manual coordination
- No transactional guarantees

**Recommendation:**
- Single state directory: `.ggen/state/`
- Use SQLite or RocksDB for ACID guarantees
- Version state schema
- Support state migration

### 4.3 State Machine Bypass Points

**Issue: Direct File Generation Bypasses State**
```rust
// CLI commands can call Generator directly
pub fn template_generate(/* ... */) -> Result<()> {
    let mut generator = Generator::new(pipeline, ctx);
    generator.generate()?; // No lifecycle check!
}
```

**Issue: Project Generation Bypasses Lifecycle**
```rust
pub async fn create_new_project(config: &ProjectConfig) -> Result<()> {
    // Creates files directly
    fs::create_dir_all(&project_path)?;
    fs::write(&cargo_toml, /* ... */)?;

    // No lifecycle state tracking!
}
```

**Recommendation:**
- ALL file generation must go through lifecycle
- Enforce via types: `fn generate<S: BuildPhase>(generator: Generator<S>)`
- No direct `fs::write` outside lifecycle hooks

---

## 5. Testability Analysis

### 5.1 Modules That Cannot Be Tested in Isolation

**Template System:**
- ❌ `template.rs` - Requires filesystem, RDF graph, Tera environment
- ❌ `generator.rs` - Requires `Pipeline` which needs filesystem setup
- ❌ `pipeline.rs` - Initializes global Tera state, loads RDF files

**Example Test Complexity:**
```rust
#[test]
fn test_generator_simple() {
    // Need temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Need template file
    let template_path = temp_dir.path().join("test.tmpl");
    fs::write(&template_path, "...").unwrap();

    // Need pipeline (requires Tera + Graph setup)
    let pipeline = Pipeline::new().unwrap();

    // Need context
    let ctx = GenContext::new(template_path, output_dir);

    // Finally can test
    let mut generator = Generator::new(pipeline, ctx);
    let result = generator.generate().unwrap();
}
```

**RDF System:**
- ❌ `graph/core.rs` - oxigraph state management complex
- ❌ `delta.rs` - Requires two Graph instances to compare
- ✅ `graph/query.rs` - Can mock Graph trait (but no trait exists!)

**Pack System:**
- ❌ `resolver.rs` - Requires CacheManager + LockfileManager + filesystem
- ❌ `cache.rs` - Hard-coded cache directory, no dependency injection
- ❌ `registry.rs` - HTTP client tightly coupled, no mock support

**Lifecycle System:**
- ✅ `state_machine.rs` - Pure, can test in isolation
- ❌ `exec.rs` - Shells out to `make`, `cargo`, etc. (no mocking)
- ❌ `hooks.rs` - Calls external processes

### 5.2 Missing Abstractions for Testing

**No Filesystem Abstraction:**
```rust
// Hard-coded filesystem calls
pub fn load_cached(&self, pack_id: &str) -> Result<CachedPack> {
    let pack_dir = self.cache_dir.join(pack_id).join(version);
    let manifest_path = pack_dir.join("gpack.toml");
    let content = fs::read_to_string(&manifest_path)?; // Can't mock!
}
```

**Recommendation:**
```rust
trait FileSystem {
    fn read_to_string(&self, path: &Path) -> Result<String>;
    fn write(&self, path: &Path, contents: &str) -> Result<()>;
    fn exists(&self, path: &Path) -> bool;
}

struct RealFileSystem;
struct MockFileSystem { /* HashMap<PathBuf, String> */ }

pub struct CacheManager<F: FileSystem = RealFileSystem> {
    cache_dir: PathBuf,
    fs: F,
}
```

**No HTTP Abstraction:**
```rust
pub struct RegistryClient {
    base_url: Url,
    client: reqwest::Client, // Tightly coupled!
}
```

**Recommendation:**
```rust
#[async_trait]
trait HttpClient {
    async fn get(&self, url: &str) -> Result<Vec<u8>>;
}

struct ReqwestHttpClient(reqwest::Client);
struct MockHttpClient { /* predefined responses */ }

pub struct RegistryClient<H: HttpClient = ReqwestHttpClient> {
    base_url: Url,
    client: H,
}
```

**No Process Execution Abstraction:**
```rust
// lifecycle/exec.rs
let output = std::process::Command::new("cargo")
    .arg("build")
    .output()?; // Can't test without cargo installed!
```

**Recommendation:**
```rust
trait ProcessRunner {
    fn run(&self, cmd: &str, args: &[&str]) -> Result<Output>;
}

struct SystemProcessRunner;
struct MockProcessRunner { /* HashMap<String, Output> */ }
```

### 5.3 Test Coverage Gaps

**Current Test Organization:**
```
crates/ggen-core/tests/
├── unit/                      # Unit tests (7 files)
├── security/                  # Security tests (3 files)
├── integration/               # Integration tests (8 files)
│   ├── lifecycle_tests.rs
│   ├── code_generation_tests.rs
│   ├── cache_tests.rs
│   └── marketplace_*_e2e.rs
├── chicago_tdd_smoke_test.rs # Chicago TDD tests
└── production_validation.rs   # Production readiness tests
```

**Missing Test Coverage:**
- ❌ Pack installation end-to-end (incomplete implementation)
- ❌ Template resolution from multiple packs
- ❌ Lifecycle state persistence/recovery
- ❌ Concurrent pack installation
- ❌ Graph delta computation edge cases
- ❌ Security: Path traversal prevention
- ❌ Performance: Large graph operations

**Test Quality Issues:**
```rust
// Many tests use unwrap() instead of assertions
#[test]
fn test_example() {
    let result = some_function().unwrap(); // No error message!
    assert_eq!(result, expected);
}

// Should be:
#[test]
fn test_example() {
    let result = some_function()
        .expect("some_function should succeed with valid input");
    assert_eq!(result, expected, "result should match expected value");
}
```

---

## 6. Extensibility

### 6.1 Cannot Add New Features Cleanly

**Issue 1: Cannot Add New Template Engines**
```rust
// Template system hard-coded to Tera
pub struct Pipeline {
    pub tera: Tera,          // Tightly coupled!
    pub graph: Graph,
}

pub struct Template {
    pub front: Frontmatter,
    pub body: String,
}

impl Template {
    pub fn render(&self, tera: &mut Tera, ctx: &Context) -> Result<String> {
        tera.render_str(&self.body, ctx)? // Must use Tera!
    }
}
```

**Recommendation:**
```rust
trait TemplateEngine {
    fn render(&self, template: &str, context: &Context) -> Result<String>;
}

struct TeraEngine(Tera);
struct HandlebarEngine(Handlebars);
struct MinijinjaEngine(Environment);

pub struct Pipeline<E: TemplateEngine = TeraEngine> {
    pub engine: E,
    pub graph: Graph,
}
```

**Issue 2: Cannot Add New Pack Sources**
```rust
// Pack sources hard-coded in enum
pub enum PackSource {
    Registry { url: String },
    GitHub { repo: String, rev: String },
    Local { path: PathBuf },
}

// What about:
// - GitLab?
// - BitBucket?
// - HTTP tarball?
// - IPFS?
```

**Recommendation:**
```rust
trait PackSource {
    fn fetch(&self, destination: &Path) -> Result<()>;
    fn checksum(&self) -> Result<String>;
    fn cache_key(&self) -> String;
}

struct RegistrySource { url: String }
struct GitSource { repo: String, rev: String }
struct LocalSource { path: PathBuf }
struct HttpSource { url: String, sha256: String }
struct IpfsSource { cid: String }
```

**Issue 3: Cannot Add New Lifecycle Phases**
```rust
// Phases hard-coded in enum
pub enum Phase {
    Init,
    Setup,
    Build,
    Test,
    Deploy,
}

// State machine hard-coded
pub struct Initial;
pub struct Initialized;
pub struct Setup;
pub struct Built;
pub struct Tested;
pub struct Deployed;

// What about:
// - Lint?
// - Format?
// - Audit?
// - Benchmark?
// - Package?
// - Publish?
```

**Recommendation:**
```rust
trait LifecyclePhase {
    fn name(&self) -> &str;
    fn dependencies(&self) -> Vec<Box<dyn LifecyclePhase>>;
    fn execute(&self, context: &Context) -> Result<()>;
}

struct InitPhase;
struct BuildPhase;
struct CustomPhase { name: String, script: String }

// Register custom phases
lifecycle.register_phase(Box::new(CustomPhase {
    name: "lint".into(),
    script: "cargo clippy".into(),
}))?;
```

### 6.2 Hard-Coded Conventions

**Issue: Template Discovery Patterns**
```rust
// Hard-coded in PackConventions
impl Default for PackConventions {
    fn default() -> Self {
        Self {
            template_patterns: &["templates/**/*.tmpl", "templates/**/*.tera"],
            rdf_patterns: &[
                "templates/**/graphs/*.ttl",
                "templates/**/graphs/*.rdf",
            ],
            // ...
        }
    }
}
```

**Impact:**
- Cannot support other conventions (e.g., `src/`, `lib/`)
- Cannot discover templates in non-standard layouts
- Hard to A/B test new conventions

**Recommendation:**
```rust
pub struct ConventionRegistry {
    conventions: Vec<Box<dyn Convention>>,
}

trait Convention {
    fn template_patterns(&self) -> &[&str];
    fn rdf_patterns(&self) -> &[&str];
    fn priority(&self) -> u8;
}

struct GgenConvention;  // Default ggen convention
struct CustomConvention { patterns: Vec<String> }

// Users can register custom conventions
registry.register(Box::new(CustomConvention { /* ... */ }))?;
```

**Issue: Registry URL Hard-Coded**
```rust
let registry_url = std::env::var("GGEN_REGISTRY_URL")
    .unwrap_or_else(|_| "https://seanchatmangpt.github.io/ggen/registry/".to_string());
```

**Impact:**
- Private registries require env var
- No support for multiple registries
- No fallback logic

**Recommendation:**
```rust
pub struct RegistryConfig {
    pub primary: Url,
    pub mirrors: Vec<Url>,
    pub auth: Option<AuthConfig>,
}

impl RegistryConfig {
    pub fn from_file(path: &Path) -> Result<Self> { }
    pub fn from_env() -> Result<Self> { }
}

// ~/.ggen/registry.toml
[registry]
primary = "https://registry.ggen.io"
mirrors = [
    "https://mirror1.ggen.io",
    "https://mirror2.ggen.io"
]

[auth]
token = "${GGEN_TOKEN}"
```

---

## 7. Performance Bottlenecks

### 7.1 RDF Graph Performance

**Issue: No Query Result Caching**
```rust
// graph/core.rs
pub fn query(&self, sparql: &str) -> Result<Vec<BTreeMap<String, String>>> {
    let query = Query::parse(sparql, None)?;

    // Every query hits oxigraph - no caching!
    let results = self.store.query(query)?;

    // Results converted to BTreeMap every time
    let mut output = Vec::new();
    for solution in results {
        let mut row = BTreeMap::new();
        for (var, term) in solution?.iter() {
            row.insert(var.as_str().to_string(), term.to_string());
        }
        output.push(row);
    }
    Ok(output)
}
```

**Impact:**
- Same query executed multiple times during generation
- No prepared statement caching
- BTreeMap allocation overhead

**Recommendation:**
```rust
pub struct CachedGraph {
    store: Store,
    query_cache: LruCache<String, Arc<QueryResults>>,
    prepared_cache: LruCache<String, Arc<PreparedQuery>>,
}

impl CachedGraph {
    pub fn query(&mut self, sparql: &str) -> Result<Arc<QueryResults>> {
        if let Some(cached) = self.query_cache.get(sparql) {
            return Ok(Arc::clone(cached));
        }

        let results = self.execute_query(sparql)?;
        let results = Arc::new(results);
        self.query_cache.put(sparql.to_string(), Arc::clone(&results));
        Ok(results)
    }
}
```

### 7.2 Template Rendering Performance

**Issue: Tera Re-initialization**
```rust
// pipeline.rs
pub fn new() -> Result<Self> {
    let mut tera = Tera::default();

    // Registers filters/functions every time
    register_all(&mut tera)?; // 20+ filters, 15+ functions

    Ok(Self {
        tera,
        graph: Graph::new()?,
    })
}
```

**Impact:**
- Pipeline created for each template generation
- Tera environment rebuilt every time
- Function registration overhead

**Recommendation:**
```rust
lazy_static! {
    static ref GLOBAL_TERA: Mutex<Tera> = Mutex::new({
        let mut tera = Tera::default();
        register_all(&mut tera).unwrap();
        tera
    });
}

pub struct Pipeline {
    tera: Arc<Tera>, // Shared, pre-initialized
    graph: Graph,
}
```

### 7.3 File I/O Inefficiencies

**Issue: Sequential File Operations**
```rust
// gpack.rs - discover_templates
pub fn discover_templates(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
    let mut templates = Vec::new();

    for pattern in &self.templates.patterns {
        let full_pattern = base_path.join(pattern);

        for entry in glob(&full_pattern.to_string_lossy())? {
            templates.push(entry?); // Sequential I/O!
        }
    }

    Ok(templates)
}
```

**Impact:**
- O(n) file stat calls
- No parallelism
- Slow for large packs

**Recommendation:**
```rust
use rayon::prelude::*;

pub fn discover_templates(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
    let patterns: Vec<_> = self.templates.patterns
        .par_iter()
        .map(|pattern| {
            let full_pattern = base_path.join(pattern);
            glob(&full_pattern.to_string_lossy())
                .map(|entries| entries.collect::<Vec<_>>())
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(patterns.into_iter().flatten().collect())
}
```

### 7.4 Memory Inefficiencies

**Issue: Unnecessary String Cloning**
```rust
// registry.rs
pub struct SearchResult {
    pub id: String,           // Could be &str or Arc<str>
    pub name: String,
    pub description: String,  // Often duplicated
    pub tags: Vec<String>,
    pub keywords: Vec<String>,
}

// Returned as Vec<SearchResult> - lots of allocations!
```

**Recommendation:**
```rust
pub struct SearchResult<'a> {
    pub id: &'a str,
    pub name: &'a str,
    pub description: &'a str,
    pub tags: &'a [String],
    pub keywords: &'a [String],
}

// Or use Arc for shared ownership
pub struct SearchResult {
    pub id: Arc<str>,
    pub name: Arc<str>,
    pub description: Arc<str>,
    pub tags: Arc<[String]>,
}
```

**Issue: Large Graph In-Memory**
```rust
// graph/core.rs loads entire graph into memory
pub fn insert_turtle(&self, turtle: &str) -> Result<()> {
    // oxigraph Store is memory-backed
    self.store.load_dataset(/* ... */)?;
}
```

**Impact:**
- Large RDF files (>100MB) cause OOM
- No streaming parser
- All triples in memory

**Recommendation:**
```rust
// Use oxigraph's RocksDB backend for large graphs
pub struct Graph {
    store: Store,
    backend: GraphBackend,
}

pub enum GraphBackend {
    Memory(MemoryStore),
    RocksDb(RocksDbStore),
}

impl Graph {
    pub fn new_persistent(path: &Path) -> Result<Self> {
        let store = Store::open(path)?;
        Ok(Self {
            store,
            backend: GraphBackend::RocksDb(/* ... */),
        })
    }
}
```

---

## 8. Marketplace Integration

### 8.1 Dual Marketplace Problem

**Current State:**
```
ggen-marketplace (v1)
  - Legacy implementation
  - Used by ggen-domain (default feature)
  - File-based package metadata
  - No RDF backing

ggen-marketplace-v2 (v2)
  - RDF-backed implementation
  - Optional feature flag
  - SPARQL queries for search
  - Ed25519 signatures
  - NOT production-ready
```

**Integration Status:**
```rust
// ggen-domain/Cargo.toml
[features]
marketplace-v1 = []                           # Legacy (default)
marketplace-v2 = ["ggen-marketplace-v2"]     # RDF-backed (new)
marketplace-parallel = ["v1", "v2"]          # Both for A/B testing
default = ["marketplace-v1"]
```

**Problems:**
1. **No Migration Path**
   - v1 data format incompatible with v2
   - No data migration tool
   - Cannot run both simultaneously in production

2. **API Incompatibility**
   ```rust
   // v1 API
   pub fn search_packages(query: &str) -> Result<Vec<Package>> { }

   // v2 API
   pub fn advanced_search(params: &SearchParams) -> Result<Vec<SearchResult>> { }
   ```
   - Different return types
   - Different search parameters
   - CLI commands need feature-gated code

3. **Missing Features in v2**
   - ❌ Package installation (not implemented)
   - ❌ Version resolution (not implemented)
   - ❌ Dependency graph computation (not implemented)
   - ❌ Update checking (not implemented)
   - ❌ Cache invalidation (not implemented)

### 8.2 Marketplace-Pack System Gap

**Issue: No Integration**
```rust
// Pack system (ggen-core)
pub struct RegistryClient { /* ... */ }  // Fetches pack metadata

// Marketplace v2 (separate crate)
pub struct Registry { /* ... */ }         // RDF-backed registry

// NO shared types!
// NO shared protocols!
```

**Impact:**
- `ggen pack install` uses old registry client
- `ggen marketplace search` uses new marketplace
- Results don't match
- Inconsistent user experience

**Recommendation:**
```rust
// Unified registry interface
trait PackageRegistry {
    fn search(&self, query: &str) -> Result<Vec<PackageInfo>>;
    fn get(&self, id: &str, version: &str) -> Result<PackageMetadata>;
    fn install(&self, id: &str, version: &str, dest: &Path) -> Result<()>;
}

// Both marketplace and pack system implement this
impl PackageRegistry for RegistryClient { }
impl PackageRegistry for V3OptimizedRegistry { }

// CLI uses trait, doesn't care which implementation
pub fn install_command(registry: &dyn PackageRegistry, id: &str) -> Result<()> {
    registry.install(id, "latest", Path::new("."))?;
}
```

### 8.3 ggen-marketplace-v2 Readiness

**Current Status:**
- ✅ RDF data model
- ✅ SPARQL search
- ✅ Ed25519 signing
- ✅ Metrics collection
- ✅ Validation system
- ❌ Installation logic (TODO)
- ❌ Registry server (TODO)
- ❌ Migration tools (TODO)
- ❌ Production testing (TODO)

**Blockers for v4.0:**
1. Complete installation implementation
2. Deploy registry server
3. Build data migration pipeline (v1 → v2)
4. Performance testing with large package sets
5. Security audit

**Estimated Effort:** 4-6 weeks

---

## 9. CLI System Architecture

### 9.1 clap-noun-verb Auto-Discovery

**Current Implementation:**
```rust
// crates/ggen-cli/src/cmds/mod.rs
pub mod ai;
pub mod graph;
pub mod marketplace;
pub mod packs;
pub mod template;
// ...

pub fn run_cli() -> Result<()> {
    clap_noun_verb::run() // Auto-discovers all [verb] functions
        .map_err(|e| Error::new(&format!("CLI failed: {}", e)))?;
    Ok(())
}

// crates/ggen-cli/src/cmds/template.rs
use clap_noun_verb_macros::verb;

#[verb(noun = "template")]
pub fn generate(/* args */) -> Result<()> {
    // Command implementation
}

#[verb(noun = "template")]
pub fn list(/* args */) -> Result<()> {
    // Command implementation
}
```

**Problems:**

1. **Hidden Command Structure**
   - No central registry of commands
   - Cannot list all commands programmatically
   - Hard to generate documentation
   - Cannot introspect command tree

2. **No Command Composition**
   - Cannot reuse commands
   - Cannot create command aliases
   - Cannot build command pipelines
   - Each command is isolated

3. **Testing Difficulty**
   ```rust
   // Cannot test command in isolation
   #[test]
   fn test_template_generate() {
       // Have to invoke through clap_noun_verb
       // Cannot pass arguments directly
       // Output goes to stdout
   }
   ```

4. **Feature Flag Complexity**
   ```rust
   #[cfg(feature = "marketplace-v2")]
   #[verb(noun = "marketplace")]
   pub fn search_v2(/* ... */) -> Result<()> { }

   #[cfg(not(feature = "marketplace-v2"))]
   #[verb(noun = "marketplace")]
   pub fn search_v1(/* ... */) -> Result<()> { }
   ```
   - Same command name, different implementations
   - Feature flags in multiple places
   - Hard to maintain

### 9.2 Command → Domain Flow

**Current Architecture:**
```
User Input
  ↓
clap-noun-verb Auto-Discovery
  ↓
cmds::template::generate() [Sync]
  ↓
runtime_helper::run_async() [Bridge]
  ↓
ggen_domain::template::generate_template() [Async]
  ↓
ggen_core::Generator::generate() [Sync]
  ↓
File System
```

**Issues:**

1. **Multiple Async/Sync Boundaries**
   - CLI is sync
   - Domain is async
   - Core is sync
   - Requires runtime creation at each boundary

2. **No Request/Response Types**
   ```rust
   // Commands use raw types
   pub fn generate(
       template: String,      // No validation!
       output: PathBuf,       // No normalization!
       vars: Vec<String>,     // No parsing!
   ) -> Result<()>
   ```
   - No input validation
   - No output standardization
   - Hard to test

**Recommendation:**
```rust
// Define request/response types
pub struct TemplateGenerateRequest {
    pub template: TemplateRef,  // Validated
    pub output: NormalizedPath, // Normalized
    pub vars: BTreeMap<String, String>, // Parsed
}

pub struct TemplateGenerateResponse {
    pub output_path: PathBuf,
    pub files_generated: usize,
    pub duration: Duration,
}

// Command uses types
#[verb(noun = "template")]
pub fn generate(req: TemplateGenerateRequest) -> Result<TemplateGenerateResponse> {
    let domain_result = ggen_domain::template::generate(req)?;
    Ok(TemplateGenerateResponse::from(domain_result))
}

// Easy to test
#[test]
fn test_generate_command() {
    let req = TemplateGenerateRequest { /* ... */ };
    let resp = generate(req).unwrap();
    assert_eq!(resp.files_generated, 5);
}
```

### 9.3 Missing Command Features

**Issue: No Progress Reporting**
```rust
pub fn generate(/* ... */) -> Result<()> {
    // Long-running operation
    // No progress updates!
    generator.generate()?;
}
```

**Recommendation:**
```rust
use indicatif::{ProgressBar, ProgressStyle};

pub fn generate(/* ... */) -> Result<()> {
    let pb = ProgressBar::new(100);
    pb.set_style(ProgressStyle::default_bar()
        .template("{spinner:.green} [{bar:40.cyan/blue}] {pos}/{len} {msg}")
        .unwrap());

    generator.generate_with_progress(|progress| {
        pb.set_position(progress.percent);
        pb.set_message(progress.status.clone());
    })?;

    pb.finish_with_message("Done!");
    Ok(())
}
```

**Issue: No Interactive Mode**
- Cannot prompt for missing parameters
- No confirmation prompts for destructive operations
- No autocomplete

**Recommendation:**
```rust
use dialoguer::{Input, Confirm, Select};

pub fn install(id: Option<String>) -> Result<()> {
    let pack_id = match id {
        Some(id) => id,
        None => {
            // Interactive prompt
            Input::<String>::new()
                .with_prompt("Package ID")
                .validate_with(|input: &String| {
                    if PackageId::parse(input).is_ok() {
                        Ok(())
                    } else {
                        Err("Invalid package ID format")
                    }
                })
                .interact()?
        }
    };

    // Confirmation for install
    if !Confirm::new()
        .with_prompt(format!("Install package '{}'?", pack_id))
        .interact()?
    {
        return Ok(());
    }

    install_package(&pack_id)?;
    Ok(())
}
```

---

## 10. Integration Improvement Plan

### 10.1 High Priority (v4.0 Blockers)

**1. Complete Pack Installation System** (3-4 weeks)
```rust
// Phase 2: Installation Logic
pub struct PackInstaller {
    cache: CacheManager,
    registry: RegistryClient,
}

impl PackInstaller {
    pub async fn install(&self, id: &str, version: &str) -> Result<InstallResult> {
        // 1. Resolve version
        let resolved = self.registry.resolve(id, Some(version)).await?;

        // 2. Check cache
        if let Ok(cached) = self.cache.load_cached(id, &resolved.version) {
            return Ok(InstallResult::Cached(cached));
        }

        // 3. Download
        let tempdir = self.download(&resolved).await?;

        // 4. Verify checksum
        self.verify_checksum(&tempdir, &resolved.sha256)?;

        // 5. Install to cache
        let cached = self.cache.install(id, &resolved.version, &tempdir)?;

        // 6. Update lockfile
        self.update_lockfile(id, &resolved)?;

        Ok(InstallResult::Installed(cached))
    }
}
```

**2. Unify Marketplace Implementations** (2-3 weeks)
```rust
// Extract common interface
trait PackageRegistry {
    async fn search(&self, query: &str) -> Result<Vec<SearchResult>>;
    async fn get(&self, id: &str) -> Result<PackageMetadata>;
    async fn resolve(&self, id: &str, version: Option<&str>) -> Result<ResolvedPackage>;
}

// Implement for both v1 and v2
impl PackageRegistry for LegacyMarketplace { }
impl PackageRegistry for V3OptimizedRegistry { }

// Feature-gated factory
pub fn create_registry() -> Box<dyn PackageRegistry> {
    #[cfg(feature = "marketplace-v2")]
    return Box::new(V3OptimizedRegistry::new());

    #[cfg(not(feature = "marketplace-v2"))]
    return Box::new(LegacyMarketplace::new());
}
```

**3. Integrate Lifecycle with Generator** (2-3 weeks)
```rust
// Generator uses lifecycle state machine
pub struct Generator<State = Initial> {
    pipeline: Pipeline,
    ctx: GenContext,
    lifecycle: LifecycleStateMachine<State>,
    _marker: PhantomData<State>,
}

impl Generator<Setup> {
    pub fn build(self) -> Result<Generator<Built>> {
        // Generate during build phase
        let output = self.generate_internal()?;

        // Transition lifecycle state
        let built_lifecycle = self.lifecycle.build()?;

        Ok(Generator {
            pipeline: self.pipeline,
            ctx: self.ctx,
            lifecycle: built_lifecycle,
            _marker: PhantomData,
        })
    }
}

impl Generator<Built> {
    pub fn test(self) -> Result<Generator<Tested>> {
        // Run tests
        self.run_test_phase()?;

        let tested_lifecycle = self.lifecycle.test()?;

        Ok(Generator {
            pipeline: self.pipeline,
            ctx: self.ctx,
            lifecycle: tested_lifecycle,
            _marker: PhantomData,
        })
    }
}
```

### 10.2 Medium Priority (v4.1)

**4. Extract RDF Graph as Separate Crate** (3-4 weeks)
```
ggen-rdf (new crate)
├── Graph
├── SPARQL query engine
├── Delta computation
├── Serialization/deserialization
└── Store backends (memory, RocksDB)

ggen-core
└── depends on ggen-rdf
```

**5. Add Dependency Injection for Testing** (2-3 weeks)
```rust
// Filesystem abstraction
trait FileSystem { }
struct RealFileSystem;
struct MockFileSystem;

// HTTP abstraction
trait HttpClient { }
struct ReqwestClient;
struct MockHttpClient;

// Process abstraction
trait ProcessRunner { }
struct SystemProcessRunner;
struct MockProcessRunner;

// All managers accept abstractions
pub struct CacheManager<F: FileSystem = RealFileSystem> {
    fs: F,
}

pub struct RegistryClient<H: HttpClient = ReqwestClient> {
    client: H,
}
```

**6. Standardize Error Handling** (1-2 weeks)
```rust
// Use thiserror for domain errors
#[derive(Debug, thiserror::Error)]
pub enum GeneratorError {
    #[error("Template not found: {path}")]
    TemplateNotFound { path: PathBuf },

    #[error("RDF processing failed: {0}")]
    RdfError(#[from] RdfError),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

// Provide conversion to ggen_utils::error::Error
impl From<GeneratorError> for ggen_utils::error::Error {
    fn from(e: GeneratorError) -> Self {
        ggen_utils::error::Error::new(&e.to_string())
    }
}
```

### 10.3 Low Priority (v4.2+)

**7. Add Extension Points** (2-3 weeks)
```rust
// Template engine trait
trait TemplateEngine { }

// Pack source trait
trait PackSource { }

// Lifecycle phase trait
trait LifecyclePhase { }

// Registry for extensions
pub struct ExtensionRegistry {
    template_engines: Vec<Box<dyn TemplateEngine>>,
    pack_sources: Vec<Box<dyn PackSource>>,
    lifecycle_phases: Vec<Box<dyn LifecyclePhase>>,
}
```

**8. Performance Optimizations** (3-4 weeks)
- Query result caching
- Parallel file discovery
- Lazy Tera initialization
- RocksDB backend for large graphs
- Zero-copy string handling

**9. CLI Improvements** (2-3 weeks)
- Progress reporting
- Interactive prompts
- Command aliases
- Output formatting (JSON, YAML, table)
- Shell completion

---

## 11. Module Reorganization Recommendations

### 11.1 Proposed Crate Structure for v4.0

```
ggen-workspace/
├── ggen-utils/              # Utilities (unchanged)
├── ggen-rdf/               # NEW: RDF graph system
│   ├── graph.rs
│   ├── query.rs
│   ├── delta.rs
│   ├── store.rs
│   └── serialization.rs
│
├── ggen-pack-manager/      # NEW: Pack management
│   ├── manifest.rs         # GpackManifest
│   ├── lockfile.rs         # Unified lockfile
│   ├── cache.rs
│   ├── installer.rs        # NEW: Installation logic
│   ├── resolver.rs
│   └── registry.rs
│
├── ggen-template/          # NEW: Template system
│   ├── engine.rs           # Trait for template engines
│   ├── tera_engine.rs      # Tera implementation
│   ├── template.rs
│   ├── pipeline.rs
│   └── generator.rs
│
├── ggen-lifecycle/         # NEW: Lifecycle system
│   ├── state_machine.rs
│   ├── phases.rs
│   ├── hooks.rs
│   ├── executor.rs
│   └── production.rs
│
├── ggen-core/              # SLIMMED: Core orchestration
│   ├── project_generator/
│   ├── snapshot.rs
│   ├── inject.rs
│   ├── merge.rs
│   └── ontology/          # Keep ontology here
│
├── ggen-registry/          # NEW: Unified registry
│   ├── trait.rs           # PackageRegistry trait
│   ├── v1_impl.rs         # Legacy marketplace
│   ├── v2_impl.rs         # RDF-backed
│   └── migration.rs       # v1 → v2 migration
│
├── ggen-ai/                # AI integration (unchanged)
├── ggen-domain/            # Domain logic (slimmed)
└── ggen-cli/               # CLI (unchanged)
```

### 11.2 Dependency Flow After Reorganization

```
ggen-cli
  ↓
ggen-domain
  ↓
ggen-core ────→ ggen-template ────→ ggen-rdf
  ↓                ↓
  ↓             ggen-pack-manager ──→ ggen-registry
  ↓                                      ↓
  ↓                                   ggen-utils
  ↓
ggen-lifecycle
  ↓
ggen-utils
```

**Benefits:**
- ✅ No circular dependencies
- ✅ Clear separation of concerns
- ✅ Each crate can be tested independently
- ✅ Can version crates independently
- ✅ Easier to swap implementations

### 11.3 Migration Strategy

**Phase 1: Extract RDF** (Week 1-2)
1. Create `ggen-rdf` crate
2. Move graph modules
3. Update imports in `ggen-core`
4. Update tests

**Phase 2: Extract Pack Manager** (Week 3-4)
1. Create `ggen-pack-manager` crate
2. Move pack-related modules
3. Unify lockfile implementations
4. Complete installer implementation

**Phase 3: Extract Template System** (Week 5-6)
1. Create `ggen-template` crate
2. Move template modules
3. Define TemplateEngine trait
4. Update pipeline

**Phase 4: Extract Lifecycle** (Week 7-8)
1. Create `ggen-lifecycle` crate
2. Move lifecycle modules
3. Integrate with template system
4. Update CLI commands

**Phase 5: Unify Registry** (Week 9-10)
1. Create `ggen-registry` crate
2. Define PackageRegistry trait
3. Implement for v1 and v2
4. Add migration tool

**Phase 6: Update Core** (Week 11-12)
1. Remove moved modules from `ggen-core`
2. Update dependencies
3. Update documentation
4. Run full test suite

---

## 12. Refactoring Effort Estimation

### 12.1 Critical Path (Must Complete for v4.0)

| Task | Complexity | Estimated Effort | Dependencies |
|------|-----------|-----------------|--------------|
| Complete Pack Installer | High | 3-4 weeks | None |
| Unify Marketplace | High | 2-3 weeks | Pack Installer |
| Integrate Lifecycle | Medium | 2-3 weeks | None |
| Extract RDF Crate | Medium | 2-3 weeks | None |
| **TOTAL** | | **9-13 weeks** | |

### 12.2 Important (Should Complete for v4.0)

| Task | Complexity | Estimated Effort | Dependencies |
|------|-----------|-----------------|--------------|
| Extract Pack Manager | Medium | 2-3 weeks | Pack Installer |
| Dependency Injection | Medium | 2-3 weeks | None |
| Error Handling | Low | 1-2 weeks | None |
| CLI Improvements | Low | 1-2 weeks | None |
| **TOTAL** | | **6-10 weeks** | |

### 12.3 Nice to Have (Can Defer to v4.1)

| Task | Complexity | Estimated Effort | Dependencies |
|------|-----------|-----------------|--------------|
| Extract Template Crate | Medium | 3-4 weeks | Extract RDF |
| Extract Lifecycle Crate | Medium | 2-3 weeks | Lifecycle Integration |
| Extension Points | Medium | 2-3 weeks | Extract Template |
| Performance Optimizations | High | 3-4 weeks | None |
| **TOTAL** | | **10-14 weeks** | |

### 12.4 Overall Timeline

**Minimum (Critical Path Only):** 9-13 weeks
**Recommended (Critical + Important):** 15-23 weeks
**Complete (All Tasks):** 25-37 weeks

**Parallel Work Opportunities:**
- Pack Installer + Extract RDF (can run in parallel)
- Error Handling + CLI Improvements (can run in parallel)
- Performance Optimizations (can start anytime)

**Realistic Timeline for v4.0:** **12-16 weeks** (3-4 months)

---

## 13. Risk Assessment

### 13.1 High Risks

1. **Breaking Changes Impact**
   - Risk: Module reorganization breaks existing projects
   - Mitigation: Provide migration guide, deprecation warnings, compatibility shims
   - Timeline: Add 2-3 weeks for migration tooling

2. **Marketplace Migration**
   - Risk: Data loss during v1 → v2 migration
   - Mitigation: Build robust migration tool, run A/B testing
   - Timeline: Add 1-2 weeks for migration validation

3. **Performance Regression**
   - Risk: Refactoring introduces performance regressions
   - Mitigation: Benchmark before/after, performance tests in CI
   - Timeline: Add 1 week for benchmarking

### 13.2 Medium Risks

4. **API Instability**
   - Risk: Public API changes too frequently
   - Mitigation: Semantic versioning, changelog, stability guarantees
   - Timeline: No additional time

5. **Testing Coverage Gaps**
   - Risk: Refactoring breaks untested code paths
   - Mitigation: Increase test coverage before refactoring
   - Timeline: Add 2-3 weeks for test expansion

### 13.3 Low Risks

6. **Documentation Debt**
   - Risk: Documentation doesn't keep up with changes
   - Mitigation: Update docs as part of each PR
   - Timeline: Add 1 week for doc review

---

## 14. Recommendations Summary

### 14.1 Immediate Actions (Week 1-2)

1. ✅ Create architectural working group
2. ✅ Define v4.0 scope (critical path only vs. full refactor)
3. ✅ Set up benchmarking infrastructure
4. ✅ Expand test coverage for core modules
5. ✅ Create refactoring roadmap with milestones

### 14.2 Short-Term (v4.0 - 3-4 months)

1. ✅ Complete pack installation system
2. ✅ Unify marketplace implementations
3. ✅ Integrate lifecycle with generator
4. ✅ Extract RDF as separate crate
5. ✅ Standardize error handling
6. ✅ Add dependency injection for testing

### 14.3 Medium-Term (v4.1 - 6-9 months)

1. ✅ Extract pack manager crate
2. ✅ Extract template system crate
3. ✅ Extract lifecycle crate
4. ✅ Add extension points
5. ✅ CLI improvements (progress, interactive)
6. ✅ Performance optimizations

### 14.4 Long-Term (v4.2+ - 12+ months)

1. ✅ Plugin system for custom template engines
2. ✅ Plugin system for pack sources
3. ✅ Web UI for marketplace
4. ✅ Cloud-based pack hosting
5. ✅ Distributed build system

---

## 15. Conclusion

ggen v3.2 has a solid foundation but exhibits significant architectural gaps preventing evolution to v4.0. The analysis identified:

- **7 critical issues** requiring immediate attention
- **15 moderate gaps** that should be addressed
- **Estimated 12-16 weeks** for critical path to v4.0
- **Module reorganization** into 7 focused crates
- **Clear migration path** for existing projects

**Primary Blockers:**
1. Incomplete pack installation system
2. Marketplace fragmentation (v1/v2)
3. Lifecycle system not integrated with generator
4. Module cohesion issues
5. Testability barriers

**Recommended Approach:**
- Focus on critical path (9-13 weeks)
- Parallel work on RDF extraction and error standardization
- Defer extension points and performance optimizations to v4.1
- Maintain backward compatibility where possible
- Provide migration tooling and documentation

**Success Metrics:**
- ✅ 100% test coverage for critical path modules
- ✅ Zero breaking changes for 90% of existing users
- ✅ <10% performance regression
- ✅ All pack installation tests passing
- ✅ Marketplace v1→v2 migration tool complete
- ✅ Lifecycle integration tests passing

The path to v4.0 is challenging but achievable with focused effort on the critical path and disciplined scope management.
