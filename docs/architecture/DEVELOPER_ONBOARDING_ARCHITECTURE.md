# Developer Onboarding Architecture Guide

## Executive Summary

ggen is a **knowledge graph-driven code generation framework** that treats software artifacts as projections of RDF ontologies. This document provides the architectural foundation for new developers joining the project.

**Version**: 2.6.0 (November 2025)
**Production Readiness**: 89%
**Codebase Size**: 851 Rust files, 73 Cargo.toml files
**Architecture Philosophy**: Poka-yoke design (type-level error prevention), 80/20 value delivery

---

## 1. System Architecture Overview

### 1.1 High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     ggen Binary CLI                          │
│                   (Main Entry Point)                         │
└────────────────────────┬────────────────────────────────────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
         ▼               ▼               ▼
┌────────────────┐ ┌──────────┐ ┌─────────────┐
│  ggen-cli-lib  │ │ ggen-ai  │ │ ggen-utils  │
│  (CLI Layer)   │ │ (LLM)    │ │ (Logging)   │
└────────┬───────┘ └────┬─────┘ └──────┬──────┘
         │              │                │
         │              │                │
         ▼              ▼                ▼
┌────────────────────────────────────────────────┐
│           ggen-domain (Business Logic)         │
│  (Pure logic - NO CLI dependencies)            │
└────────────────┬───────────────────────────────┘
                 │
    ┌────────────┼────────────┐
    │            │            │
    ▼            ▼            ▼
┌─────────┐ ┌──────────┐ ┌────────────────┐
│ ggen-   │ │ ggen-    │ │ ggen-          │
│ core    │ │ market-  │ │ node           │
│ (Engine)│ │ place    │ │ (Node.js API)  │
└─────────┘ └──────────┘ └────────────────┘
```

### 1.2 Module Dependency Graph (Clean Architecture)

```
Layer 0 (Foundation):
  └─ ggen-utils (logging, error handling)

Layer 1 (Core Engine):
  └─ ggen-core (RDF engine, templates, lifecycle)
      ├─ depends on: ggen-utils
      └─ provides: Graph, Pipeline, Template, Lifecycle

Layer 2 (Specialized Services):
  ├─ ggen-marketplace (package backend)
  │   └─ depends on: ggen-core, ggen-utils
  └─ ggen-ai (LLM integration)
      └─ depends on: ggen-core, ggen-utils

Layer 3 (Business Logic):
  └─ ggen-domain (pure domain logic)
      ├─ depends on: ggen-core, ggen-ai, ggen-marketplace, ggen-utils
      └─ provides: marketplace::*, template::*, graph::*, hook::*
      └─ CRITICAL: NO clap/CLI dependencies

Layer 4 (Presentation):
  ├─ ggen-cli-lib (CLI commands)
  │   ├─ depends on: ggen-domain, ggen-core, ggen-ai, ggen-marketplace, ggen-utils
  │   └─ uses clap-noun-verb v3.4.0 for command routing
  └─ ggen-node (Node.js binding)
      └─ depends on: ggen-cli-lib
```

**Key Architectural Constraint**: `ggen-domain` MUST remain CLI-agnostic for reusability across interfaces (CLI, Node.js, future web API).

---

## 2. Crate Responsibilities & Public APIs

### 2.1 ggen-core (Engine Layer)

**Purpose**: Core code generation engine with RDF/SPARQL support

**Key Modules**:
- `lifecycle::*` - Project lifecycle orchestration (init, setup, build, test, deploy)
- `graph::Graph` - RDF triple store (Oxigraph-based)
- `pipeline::Pipeline` - Generation pipeline orchestration
- `template::Template` - Template processing (Tera + frontmatter)
- `rdf::*` - RDF validation, schema, metadata
- `pqc::*` - Post-quantum cryptography (ML-DSA signatures)

**Public API Highlights**:
```rust
// Lifecycle state machine (poka-yoke design)
pub use lifecycle::{
    LifecycleStateMachine, Initial, Initialized, Built, Tested, Deployed,
    run_phase, run_pipeline, Context, Make, Phase, PhaseBuilder
};

// RDF engine
pub use graph::Graph;
pub use rdf::{GgenOntology, TemplateMetadata, Validator, ValidationReport};

// Template generation
pub use templates::{generate_file_tree, FileTreeGenerator, TemplateContext};
pub use pipeline::{Pipeline, PipelineBuilder};

// Lifecycle hooks & validation
pub use lifecycle::{validate_hooks, ValidatedHooks, HookValidationError};
```

**Integration Points**:
- CLI commands call `lifecycle::run_phase()` for project orchestration
- Domain logic uses `Graph` for RDF queries
- AI module uses `Pipeline` for code generation

### 2.2 ggen-domain (Business Logic Layer)

**Purpose**: Pure business logic - NO CLI dependencies (clean architecture)

**Version**: 3.1.0 (different from core to track domain API changes)

**Key Modules**:
- `marketplace::*` - Package installation, publishing, search
- `template::*` - Template generation, regeneration, linting
- `graph::*` - RDF graph operations (load, query, export, visualize)
- `hook::*` - Git hook creation and management
- `ci::*` - CI/CD workflow generation
- `audit::*` - Security auditing
- `rdf::*` - RDF validation logic

**Public API Pattern**:
```rust
// Example: marketplace::install
pub async fn install_package(
    package_id: &str,
    version: Option<&str>,
    target_dir: &Path,
) -> Result<()> {
    // Pure logic - no clap types, no CLI I/O
}

// Example: template::generate_rdf
pub async fn generate_from_rdf(
    ontology_path: &Path,
    template_name: &str,
    output_dir: &Path,
) -> Result<GenerationResult> {
    // Uses ggen-core::Pipeline internally
}
```

**CRITICAL CONSTRAINT**:
```toml
# ggen-domain/Cargo.toml
# CRITICAL: NO clap or clap-noun-verb dependencies
# Domain must be CLI-agnostic
```

This enables:
- ✅ Reuse in Node.js addon (ggen-node)
- ✅ Future web API integration
- ✅ Testability without CLI framework

### 2.3 ggen-cli-lib (Presentation Layer)

**Purpose**: CLI command routing using clap-noun-verb v3.4.0

**Key Modules**:
- `cmds::*` - Command implementations (noun-verb pattern)
  - `cmds::marketplace` - CLI wrappers for `ggen-domain::marketplace`
  - `cmds::template` - CLI wrappers for `ggen-domain::template`
  - `cmds::ai` - CLI wrappers for `ggen-ai`
- `conventions::*` - File-based routing conventions
- `runtime_helper` - Async/sync bridge for CLI execution

**Command Routing Pattern**:
```rust
// cmds/marketplace.rs
use ggen_domain::marketplace; // Import domain logic

#[verb("marketplace install")]
pub async fn install(package_id: String, version: Option<String>) -> Result<()> {
    // Thin CLI wrapper - delegates to domain layer
    let target_dir = std::env::current_dir()?;
    marketplace::install_package(&package_id, version.as_deref(), &target_dir).await
}
```

**Auto-Discovery**: clap-noun-verb v3.4.0 automatically discovers all `#[verb]` functions

### 2.4 ggen-marketplace (Package Backend)

**Purpose**: Local package storage and search backend

**Key Features**:
- Content-addressed storage (CID/multihash)
- ML-based recommendations (ndarray)
- Full-text search (tantivy)
- WASM plugin support (wasmtime)
- Smart caching (moka)
- OpenTelemetry instrumentation

**Removed Features** (waste elimination after P2P removal):
- ❌ GraphQL server
- ❌ HTTP endpoints (reqwest, axum, tower)
- ❌ Centralized registry
- ❌ P2P networking (libp2p, gossipsub)

**Current State**: CLI-only local backend

### 2.5 ggen-ai (LLM Integration)

**Purpose**: AI-powered code generation and ontology creation

**Supported LLMs**:
- OpenAI GPT-4o
- Anthropic Claude
- Ollama (local)

**Key Functions**:
```rust
pub async fn generate_ontology_from_prompt(
    prompt: &str,
    output_path: &Path,
) -> Result<String>

pub async fn chat_interactive() -> Result<()>

pub async fn analyze_code(
    code_dir: &Path,
    focus: Option<&str>,
) -> Result<AnalysisReport>
```

### 2.6 ggen-utils (Foundation Layer)

**Purpose**: Logging, error handling, app config

**Key Exports**:
```rust
pub mod error; // Result<T> = anyhow::Result<T>
pub mod app_config; // AppConfig for persistent settings
// Logging features: termlog, journald, syslog (optional)
```

---

## 3. Lifecycle System (Poka-Yoke Design)

### 3.1 Type-Level State Machine

The lifecycle system uses **poka-yoke** (mistake-proofing) design to prevent invalid operations at compile time.

```rust
use ggen_core::lifecycle::*;

// Start with initial state
let lifecycle = LifecycleStateMachine::<Initial>::new();

// Valid transitions - compiler enforces order
let lifecycle = lifecycle.init()?;    // Initial -> Initialized
let lifecycle = lifecycle.setup()?;   // Initialized -> Setup
let lifecycle = lifecycle.build()?;   // Setup -> Built
let lifecycle = lifecycle.test()?;    // Built -> Tested
let lifecycle = lifecycle.deploy()?;  // Tested -> Deployed

// Invalid transitions fail at compile time:
// lifecycle.deploy() // ❌ Compile error: no method `deploy` on Initial
```

**Benefits**:
- ✅ Invalid phase transitions are impossible (compiler prevents them)
- ✅ Type system encodes valid lifecycle progression
- ✅ Zero runtime cost (PhantomData is compile-time only)

### 3.2 Validated Types (Preventing Unvalidated Data Usage)

```rust
use ggen_core::lifecycle::*;

// Phases must have commands
let phase = PhaseBuilder::new("build")
    .command("cargo build --release")
    .build()?; // ✅ Returns ValidatedPhase

let phase = PhaseBuilder::new("empty")
    .build(); // ❌ Error - phase has no commands

// State must be validated before use
let validated_state = ValidatedLifecycleState::new(load_state(path)?)?;
use_state(validated_state)?; // Type ensures validated
```

### 3.3 Hook Validation (Preventing Circular Dependencies)

```rust
// Validates hooks before execution
let validated_hooks = validate_hooks(&make)?;
// - Checks for circular dependencies
// - Validates phase references
// - Prevents self-referential hooks
```

See [POKA_YOKE_DESIGN.md](../../crates/ggen-core/src/lifecycle/POKA_YOKE_DESIGN.md) and [error_modes.md](../../crates/ggen-core/src/lifecycle/error_modes.md) for complete design rationale.

---

## 4. Build System & Workspace

### 4.1 Cargo Workspace Structure

```toml
# Cargo.toml (workspace root)
[workspace]
members = [
  "crates/ggen-utils",      # Layer 0: Foundation
  "crates/ggen-core",       # Layer 1: Engine
  "crates/ggen-ai",         # Layer 2: Services
  "crates/ggen-marketplace",
  "crates/ggen-domain",     # Layer 3: Business Logic
  "crates/ggen-cli",        # Layer 4: Presentation
  "crates/ggen-node",       # Layer 4: Node.js API
  "examples/*",
]
resolver = "2" # Cargo resolver v2 for better dependency resolution
```

**Workspace-Wide Dependencies** (version consistency):
- tokio 1.47 (async runtime)
- serde 1.0 (serialization)
- anyhow 1.0 (error handling)
- clap 4.5 (CLI parsing)
- oxigraph 0.5 (RDF store)
- opentelemetry 0.21 (observability)

### 4.2 Poka-Yoke Linting (Compiler as Design Tool)

```toml
[workspace.lints.rust]
warnings = "deny"       # Warnings become compile errors
unsafe_code = "deny"    # Zero unsafe code allowed
missing_docs = "warn"   # Documentation encouraged

[workspace.lints.clippy]
all = { level = "deny", priority = -1 }
pedantic = { level = "deny", priority = -1 }
unwrap_used = "deny"    # Must handle errors explicitly
expect_used = "deny"    # No panic in production code
panic = "deny"
todo = "deny"
```

**Philosophy**: "If it compiles, invariants are enforced" - Use type system for correctness, not runtime checks.

### 4.3 Build Profiles

```toml
[profile.dev]
opt-level = 0
incremental = true           # Fast incremental builds
split-debuginfo = "unpacked" # Faster debug on macOS
codegen-units = 256          # Parallel compilation

[profile.release]
opt-level = 3
lto = "thin"                 # Thin LTO for balance
strip = true                 # Strip symbols
codegen-units = 16

[profile.bench]
opt-level = 3
lto = true                   # Full LTO for benchmarks
codegen-units = 1
```

### 4.4 Makefile Tasks (cargo-make)

```toml
# Makefile.toml
[tasks.check]
command = "timeout"
args = ["5s", "cargo", "check"] # Quick feedback

[tasks.build-release]
command = "timeout"
args = ["30s", "cargo", "build", "--release", "-p", "ggen-cli-lib", "--bin", "ggen"]

[tasks.test]
command = "timeout"
args = ["30s", "cargo", "test", "--workspace"]

[tasks.pre-commit]
dependencies = ["fmt", "lint", "test", "validate-docs"]
```

**Key Commands**:
- `cargo make check` - Quick compile check (5s timeout)
- `cargo make dev` - Format + lint + test
- `cargo make ci` - Full CI pipeline
- `cargo make pre-commit` - Pre-commit validation

---

## 5. Integration Patterns

### 5.1 CLI → Domain → Core Flow

**Example: `ggen marketplace install my-package`**

```
┌──────────────────────────────────────────────┐
│ 1. CLI Entry (ggen-cli-lib)                 │
│    cmds::marketplace::install()              │
└───────────────┬──────────────────────────────┘
                │
                ▼
┌──────────────────────────────────────────────┐
│ 2. Domain Logic (ggen-domain)               │
│    marketplace::install_package()            │
│    - Resolve package version                 │
│    - Download from local registry            │
│    - Extract to target directory             │
└───────────────┬──────────────────────────────┘
                │
                ▼
┌──────────────────────────────────────────────┐
│ 3. Core Engine (ggen-core)                  │
│    - RegistryClient::download()              │
│    - PqcVerifier::verify_signature()         │
│    - Cache::store_package()                  │
└──────────────────────────────────────────────┘
```

### 5.2 RDF-Driven Generation Flow

**Example: `ggen template generate-rdf --ontology domain.ttl --template rust-models`**

```
┌──────────────────────────────────────────────┐
│ 1. CLI: cmds::template::generate_rdf()      │
└───────────────┬──────────────────────────────┘
                │
                ▼
┌──────────────────────────────────────────────┐
│ 2. Domain: template::generate_from_rdf()    │
└───────────────┬──────────────────────────────┘
                │
                ▼
┌──────────────────────────────────────────────┐
│ 3. Core: Graph::load_file(ontology.ttl)    │
│    - Parse RDF/Turtle                        │
│    - Validate with SHACL                     │
│    - Store in Oxigraph triple store          │
└───────────────┬──────────────────────────────┘
                │
                ▼
┌──────────────────────────────────────────────┐
│ 4. Core: Pipeline::execute()                │
│    - Run SPARQL queries to extract classes   │
│    - Map xsd:types to Rust types             │
│    - Render Tera templates                   │
│    - Write generated files                   │
└──────────────────────────────────────────────┘
```

### 5.3 Error Handling Pattern

**Consistent across all layers**:

```rust
// ggen-utils provides Result type
pub type Result<T> = anyhow::Result<T>;

// Usage across all crates
pub async fn install_package(id: &str) -> Result<()> {
    let registry = RegistryClient::new()?; // ? propagates error
    let package = registry.download(id)
        .with_context(|| format!("Failed to download package: {}", id))?;

    verify_signature(&package)?;
    Ok(())
}
```

**Error Context Strategy**:
- Use `anyhow::Context` for user-facing error messages
- Preserve error chains for debugging
- Return `Result<T>` everywhere - NO `.unwrap()` or `.expect()` in production code

---

## 6. Deployment Architecture

### 6.1 Release Artifacts

**Primary Distribution**:
- Homebrew tap: `brew tap seanchatmangpt/tap && brew install ggen`
- Cargo: `cargo install ggen`
- Binary releases: GitHub releases (Linux, macOS, Windows)

**Node.js Addon** (ggen-node):
- npm package: `@ggen/cli`
- Native Node.js binding (NAPI)
- Exposes `run(args: string[]): Promise<RunResult>`

### 6.2 CI/CD Pipelines

**GitHub Actions Workflows**:

```
.github/workflows/
├── ci.yml                    # Main CI (check, test, lint)
├── build.yml                 # Release builds
├── homebrew-release.yml      # Homebrew formula update
├── deploy-docs.yml           # Deploy docs to GitHub Pages
├── london-tdd-tests.yml      # TDD test suite
└── marketplace-test.yml      # Marketplace integration tests
```

**CI Pipeline Steps**:
1. Check (cargo check with timeout)
2. Lint (clippy --deny-warnings)
3. Test (unit + integration + E2E)
4. Build release artifacts
5. Publish (Homebrew, Cargo, npm)

### 6.3 Observability (OpenTelemetry)

**Instrumentation**:
- All lifecycle phases emit OTEL spans
- Marketplace operations tracked
- Template generation metrics
- Performance profiling data

**Configuration**:
```rust
// Enable OTEL tracing
export OTEL_EXPORTER_OTLP_ENDPOINT="http://localhost:4317"
ggen project build
```

**Telemetry Crates**:
- `opentelemetry 0.21`
- `opentelemetry-otlp 0.14`
- `tracing-opentelemetry 0.22`

---

## 7. Developer Quickstart

### 7.1 Setup Development Environment

```bash
# Clone repository
git clone https://github.com/seanchatmangpt/ggen
cd ggen

# Install Rust (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install cargo-make
cargo install cargo-make

# Install pre-commit hooks
scripts/install-git-hooks.sh

# Build project
cargo make build-release

# Verify installation
./target/release/ggen --version
# Expected: ggen 2.6.0
```

### 7.2 Development Workflow

```bash
# Quick check (5s timeout)
cargo make check

# Full development validation
cargo make dev  # Format + lint + test

# Run specific tests
cargo test --package ggen-core lifecycle
cargo test --package ggen-domain marketplace

# Run benchmarks
cargo make bench

# Pre-commit validation
cargo make pre-commit
```

### 7.3 Adding New Features

**Step 1: Add Domain Logic** (ggen-domain):
```rust
// crates/ggen-domain/src/template/my_feature.rs
use ggen_utils::error::Result;
use ggen_core::Pipeline;

pub async fn my_feature(input: &str) -> Result<String> {
    // Pure business logic - no CLI dependencies
    Ok(format!("Processed: {}", input))
}
```

**Step 2: Add CLI Command** (ggen-cli):
```rust
// crates/ggen-cli/src/cmds/template.rs
use ggen_domain::template;

#[verb("template my-feature")]
pub async fn my_feature(input: String) -> Result<()> {
    let result = template::my_feature(&input).await?;
    println!("{}", result);
    Ok(())
}
```

**Step 3: Test**:
```bash
cargo test --package ggen-domain my_feature
cargo test --package ggen-cli-lib my_feature
```

### 7.4 Architecture Decision Records (ADRs)

See `docs/` for key architectural decisions:
- [POKA_YOKE_DESIGN.md](../../crates/ggen-core/src/lifecycle/POKA_YOKE_DESIGN.md) - Type-level error prevention
- [CODING_STANDARDS_TYPE_SAFETY.md](../CODING_STANDARDS_TYPE_SAFETY.md) - Type-first development
- [VARIABLE_SCOPING_BEST_PRACTICES.md](../VARIABLE_SCOPING_BEST_PRACTICES.md) - Variable patterns
- [MARKETPLACE_REGISTRY_COMPLETION.md](../MARKETPLACE_REGISTRY_COMPLETION.md) - Registry architecture

---

## 8. Testing Strategy

### 8.1 Test Pyramid

```
           ┌─────────────┐
           │   E2E Tests │ (10%)
           │  782 lines  │
           └─────────────┘
         ┌─────────────────┐
         │ Integration Tests│ (20%)
         │  testcontainers  │
         └─────────────────┘
      ┌────────────────────────┐
      │    Unit Tests (70%)    │
      │  Chicago TDD patterns  │
      └────────────────────────┘
```

**Test Frameworks**:
- **Chicago TDD**: `chicago-tdd-tools 1.1.0` - Real objects, real systems
- **London TDD**: `mockall 0.13` - Mock-based testing
- **Property Testing**: `proptest 1.8` - Generative testing
- **Containers**: `testcontainers 0.25` - Docker-based integration tests
- **BDD**: `cucumber 0.21` - Behavior-driven scenarios

### 8.2 Test Organization

```
tests/
├── chicago_tdd/          # Chicago-style real system tests
│   └── marketplace/
├── e2e_v2/              # End-to-end scenarios
│   ├── marketplace_discovery.rs
│   └── error_handling.rs
├── integration/         # Integration tests
│   └── otel_validation_tests.rs
└── security/            # Security audits
    └── v2_security_audit.rs

crates/*/tests/          # Crate-specific tests
crates/*/benches/        # Benchmarks
```

### 8.3 Running Tests

```bash
# All tests
cargo test --workspace

# Specific crate
cargo test --package ggen-core

# With coverage
cargo tarpaulin --workspace --out Html

# Benchmarks
cargo bench --workspace

# Docker-based tests
cargo test --features docker
```

---

## 9. Performance Characteristics

### 9.1 Performance Targets

- ✅ Template generation: <2s
- ✅ CLI command startup: <200ms
- ✅ RDF graph query: <100ms
- ✅ Package installation: <5s
- ✅ Full project build: <60s

### 9.2 Optimization Strategies

**Compilation**:
- Incremental compilation enabled
- Parallel codegen (256 units in dev)
- Thin LTO in release builds
- Split debuginfo on macOS

**Runtime**:
- LRU caching (lifecycle state, templates)
- Rayon for parallel processing
- AHash for fast hashing
- Lazy static initialization

**Benchmarks**:
```bash
cargo bench --bench lifecycle_benchmarks
cargo bench --bench marketplace_benchmarks
cargo bench --bench template_generation
```

---

## 10. Security

### 10.1 Post-Quantum Cryptography

**ML-DSA Signatures** (NIST-approved):
```rust
use ggen_core::pqc::{PqcSigner, PqcVerifier};

// Sign package
let signer = PqcSigner::new()?;
let signature = signer.sign_package(package_path)?;

// Verify package
let verifier = PqcVerifier::new(public_key);
verifier.verify_package(package_path, &signature)?;
```

**Dependencies**:
- `pqcrypto-mldsa 0.1` - ML-DSA (Dilithium successor)
- `pqcrypto-traits 0.3` - Post-quantum crypto traits

### 10.2 Zero Unsafe Code

**Enforcement**:
```toml
[workspace.lints.rust]
unsafe_code = "deny" # Compile error on unsafe blocks
```

**Memory Safety**:
- Rust ownership model prevents data races
- No manual memory management
- Type system prevents null pointer dereferences

### 10.3 Dependency Auditing

```bash
# Security audit
cargo audit

# Dependency tree analysis
cargo tree --workspace --depth 3

# Outdated dependencies
cargo outdated
```

---

## 11. Documentation

### 11.1 Documentation Structure

```
docs/
├── architecture/              # Architecture docs (this file)
├── guides/                    # User guides
├── hive-mind/                 # AI orchestration patterns
├── testing/                   # Testing guides
├── validation/                # Validation reports
├── CODING_STANDARDS_TYPE_SAFETY.md
├── MARKETPLACE_REGISTRY_COMPLETION.md
├── RELEASE_NOTES_v2.6.0.md
└── VARIABLE_SCOPING_BEST_PRACTICES.md

crates/*/README.md             # Crate-specific docs
crates/*/src/*/README.md       # Module-specific docs
```

### 11.2 Inline Documentation

**Module-Level Docs**:
```rust
//! Universal lifecycle system for ggen (80/20 Implementation)
//!
//! This module implements the lifecycle orchestration system that enables
//! cross-language project management through make.toml
//!
//! # Examples
//! ```rust
//! use ggen_core::lifecycle::*;
//! let lifecycle = LifecycleStateMachine::<Initial>::new();
//! let lifecycle = lifecycle.init()?;
//! ```
```

**API Documentation**:
```bash
# Generate docs
cargo doc --workspace --no-deps

# Open in browser
cargo doc --workspace --no-deps --open
```

---

## 12. Future Architecture Considerations

### 12.1 After P2P Removal (v2.6.0)

**Removed Components**:
- ❌ P2P networking (libp2p, gossipsub, kad-dht)
- ❌ GraphQL server
- ❌ HTTP registry endpoints
- ❌ Distributed coordination

**Simplified Architecture**:
- ✅ Local-only marketplace backend
- ✅ CLI-driven workflows
- ✅ File-based package storage
- ✅ Direct integration patterns

### 12.2 Potential Future Enhancements

**Not Currently Planned** (evaluate if needed):
- Web UI for marketplace browsing
- Cloud-based registry (HTTP API)
- Multi-user collaboration features
- Advanced DAG-based lifecycle scheduling

**Philosophy**: 80/20 value delivery - Add features only when proven necessary by user demand.

---

## 13. Key Takeaways for New Developers

### 13.1 Architectural Principles

1. **Poka-Yoke Design**: Use types to prevent errors at compile time
2. **Clean Architecture**: Domain logic isolated from presentation (CLI)
3. **80/20 Value**: Focus on 20% of features that deliver 80% of value
4. **Zero Unsafe**: Memory safety through Rust ownership model
5. **Deterministic**: RDF-driven generation ensures reproducibility

### 13.2 Development Mindset

**Think in Types**:
- If it compiles, invariants are enforced
- Use type system for correctness, not runtime checks
- Compiler is your design tool

**YAGNI (You Aren't Gonna Need It)**:
- Don't add features speculatively
- Wait for proven user demand
- Delete unused code aggressively

**Test-First**:
- Chicago TDD: Real systems, real objects
- Property testing: Generative edge cases
- Integration tests: Docker-based real environments

### 13.3 Where to Start

**For New Contributors**:

1. **Read Core Docs**:
   - [POKA_YOKE_DESIGN.md](../../crates/ggen-core/src/lifecycle/POKA_YOKE_DESIGN.md)
   - [CODING_STANDARDS_TYPE_SAFETY.md](../CODING_STANDARDS_TYPE_SAFETY.md)
   - This file (DEVELOPER_ONBOARDING_ARCHITECTURE.md)

2. **Explore Codebase**:
   ```bash
   # Read public APIs
   rg "^pub (fn|struct|enum|trait)" crates/ggen-core/src

   # See module organization
   tree crates/ggen-core/src

   # Run tests to understand behavior
   cargo test --package ggen-core lifecycle
   ```

3. **Run Examples**:
   ```bash
   # Lifecycle example
   ggen ai generate-ontology --prompt "Blog: User, Post, Comment" --output blog.ttl
   ggen template generate-rdf --ontology blog.ttl --template rust-models

   # Marketplace example
   ggen marketplace search "rust"
   ```

4. **Good First Issues**:
   - GitHub: https://github.com/seanchatmangpt/ggen/labels/good%20first%20issue

---

## 14. Contact & Support

**Repository**: https://github.com/seanchatmangpt/ggen
**Documentation**: https://seanchatmangpt.github.io/ggen/
**Issues**: https://github.com/seanchatmangpt/ggen/issues
**Discussions**: https://github.com/seanchatmangpt/ggen/discussions

**Maintainer**: Sean Chatman <sean@chatmangpt.com>

---

**Generated**: 2025-11-14
**ggen Version**: 2.6.0
**Architecture Reviewer**: Hive Mind System Architect Agent
