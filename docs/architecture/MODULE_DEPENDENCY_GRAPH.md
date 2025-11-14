# ggen Module Dependency Graph

## Visual Dependency Architecture

This document provides a detailed module dependency graph for the ggen codebase, showing clean architecture boundaries and integration points.

---

## Layer Dependency Hierarchy

```
┌──────────────────────────────────────────────────────────────┐
│ Layer 4: Presentation (CLI/API Interfaces)                   │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────────────┐         ┌──────────────────────┐   │
│  │   ggen-cli-lib      │         │     ggen-node        │   │
│  │   (CLI Commands)    │         │   (Node.js API)      │   │
│  │                     │         │                      │   │
│  │  - cmds::*          │◄────────┤  - NAPI bindings     │   │
│  │  - clap-noun-verb   │         │  - run_for_node()    │   │
│  └──────────┬──────────┘         └──────────────────────┘   │
│             │                                                 │
│             │ depends on                                     │
└─────────────┼─────────────────────────────────────────────────┘
              │
              ▼
┌──────────────────────────────────────────────────────────────┐
│ Layer 3: Business Logic (Pure Domain Logic)                  │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌───────────────────────────────────────────────────────┐  │
│  │              ggen-domain (v3.1.0)                      │  │
│  │              CLI-AGNOSTIC (NO clap dependency)         │  │
│  │                                                         │  │
│  │  Modules:                                              │  │
│  │  ┌──────────────┐  ┌──────────────┐  ┌─────────────┐ │  │
│  │  │ marketplace::│  │  template::  │  │   graph::   │ │  │
│  │  │   install    │  │   generate   │  │    query    │ │  │
│  │  │   publish    │  │   regenerate │  │    export   │ │  │
│  │  │   search     │  │   lint       │  │   visualize │ │  │
│  │  └──────┬───────┘  └──────┬───────┘  └──────┬──────┘ │  │
│  │         │                 │                 │         │  │
│  │  ┌──────┴──────┐  ┌──────┴────────┐  ┌─────┴──────┐ │  │
│  │  │   hook::    │  │     ci::      │  │  audit::   │ │  │
│  │  │   create    │  │   workflow    │  │  security  │ │  │
│  │  │   list      │  │   generate    │  │            │ │  │
│  │  └─────────────┘  └───────────────┘  └────────────┘ │  │
│  └───────────────────────────────────────────────────────┘  │
│             │                                                 │
│             │ depends on                                     │
└─────────────┼─────────────────────────────────────────────────┘
              │
              ▼
┌──────────────────────────────────────────────────────────────┐
│ Layer 2: Specialized Services                                │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌────────────────────┐              ┌──────────────────┐   │
│  │  ggen-marketplace  │              │    ggen-ai       │   │
│  │                    │              │                  │   │
│  │  - Local backend   │              │  - LLM clients   │   │
│  │  - Search (tantivy)│              │  - GPT-4o        │   │
│  │  - ML recommend    │              │  - Claude        │   │
│  │  - CID storage     │              │  - Ollama        │   │
│  │  - WASM plugins    │              │  - Ontology gen  │   │
│  └────────┬───────────┘              └────────┬─────────┘   │
│           │                                   │              │
│           │ depends on                        │              │
│           └───────────────┬───────────────────┘              │
└───────────────────────────┼──────────────────────────────────┘
                            │
                            ▼
┌──────────────────────────────────────────────────────────────┐
│ Layer 1: Core Engine (RDF + Generation + Lifecycle)          │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                     ggen-core                          │  │
│  │                                                         │  │
│  │  Core Subsystems:                                      │  │
│  │  ┌──────────────┐  ┌───────────────┐  ┌────────────┐ │  │
│  │  │ lifecycle::  │  │    graph::    │  │ pipeline:: │ │  │
│  │  │   model      │  │    Graph      │  │  Pipeline  │ │  │
│  │  │   hooks      │  │   Oxigraph    │  │  Builder   │ │  │
│  │  │   exec       │  │   SPARQL 1.1  │  │  Execute   │ │  │
│  │  │   cache      │  │               │  │            │ │  │
│  │  └──────────────┘  └───────────────┘  └────────────┘ │  │
│  │                                                         │  │
│  │  ┌──────────────┐  ┌───────────────┐  ┌────────────┐ │  │
│  │  │ template::   │  │     rdf::     │  │   pqc::    │ │  │
│  │  │   Template   │  │   Validator   │  │  ML-DSA    │ │  │
│  │  │   Tera       │  │   SHACL       │  │  Signer    │ │  │
│  │  │   Frontmtr   │  │   Ontology    │  │  Verifier  │ │  │
│  │  └──────────────┘  └───────────────┘  └────────────┘ │  │
│  │                                                         │  │
│  │  ┌──────────────┐  ┌───────────────┐  ┌────────────┐ │  │
│  │  │ registry::   │  │   snapshot::  │  │  telemetry │ │  │
│  │  │   Client     │  │   Manager     │  │  OTEL      │ │  │
│  │  │   Index      │  │   Graph       │  │  Tracing   │ │  │
│  │  └──────────────┘  └───────────────┘  └────────────┘ │  │
│  └───────────────────────────────────────────────────────┘  │
│             │                                                 │
│             │ depends on                                     │
└─────────────┼─────────────────────────────────────────────────┘
              │
              ▼
┌──────────────────────────────────────────────────────────────┐
│ Layer 0: Foundation (Utilities & Infrastructure)              │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                    ggen-utils                          │  │
│  │                                                         │  │
│  │  - error::Result (anyhow wrapper)                      │  │
│  │  - app_config::AppConfig (persistent settings)         │  │
│  │  - Logging (termlog, journald, syslog)                 │  │
│  └───────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
```

---

## Detailed Module Interactions

### 1. CLI Command Flow (Example: `ggen marketplace install`)

```
┌────────────────────────────────────────────────────────────────┐
│ User Command: ggen marketplace install rust-template@1.0.0    │
└──────────────────────┬─────────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────────┐
│ ggen-cli-lib::cmds::marketplace::install()                   │
│  - Parse args: package_id, version                           │
│  - Validate user input                                       │
│  - Call domain layer                                         │
└──────────────────────┬───────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────────┐
│ ggen-domain::marketplace::install_package()                  │
│  - Resolve package version                                   │
│  - Check local cache                                         │
│  - Download if needed                                        │
│  - Extract to target directory                               │
└──────────────────────┬───────────────────────────────────────┘
                       │
          ┌────────────┼────────────┐
          │            │            │
          ▼            ▼            ▼
┌──────────────┐ ┌──────────┐ ┌──────────────┐
│ ggen-core::  │ │ ggen-    │ │ ggen-core::  │
│ registry::   │ │ market-  │ │ pqc::        │
│ Client       │ │ place::  │ │ Verifier     │
│              │ │ Local    │ │              │
│ - download() │ │ Backend  │ │ - verify()   │
└──────────────┘ └──────────┘ └──────────────┘
```

### 2. RDF-Driven Generation Flow

```
┌────────────────────────────────────────────────────────────────┐
│ User Command: ggen template generate-rdf                      │
│               --ontology domain.ttl                            │
│               --template rust-models                           │
└──────────────────────┬─────────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────────┐
│ ggen-cli-lib::cmds::template::generate_rdf()                 │
└──────────────────────┬───────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────────┐
│ ggen-domain::template::generate_from_rdf()                   │
└──────────────────────┬───────────────────────────────────────┘
                       │
          ┌────────────┼────────────┐
          │            │            │
          ▼            ▼            ▼
┌──────────────┐ ┌──────────┐ ┌──────────────┐
│ ggen-core::  │ │ ggen-    │ │ ggen-core::  │
│ graph::      │ │ core::   │ │ templates::  │
│ Graph        │ │ pipeline │ │ generator    │
│              │ │          │ │              │
│ 1. Load RDF  │ │ 3. Run   │ │ 4. Render    │
│ 2. Validate  │ │    SPARQL│ │    templates │
│    SHACL     │ │    queries│ │    Write files│
└──────────────┘ └──────────┘ └──────────────┘
```

### 3. AI-Powered Ontology Generation

```
┌────────────────────────────────────────────────────────────────┐
│ User Command: ggen ai generate-ontology                       │
│               --prompt "E-commerce: Product, Order, Review"    │
│               --output domain.ttl                              │
└──────────────────────┬─────────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────────┐
│ ggen-cli-lib::cmds::ai::generate_ontology()                  │
└──────────────────────┬───────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────────┐
│ ggen-ai::generate_ontology_from_prompt()                     │
│  - Select LLM (GPT-4o/Claude/Ollama)                         │
│  - Generate RDF from prompt                                  │
│  - Validate RDF syntax                                       │
└──────────────────────┬───────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────────┐
│ ggen-core::rdf::Validator                                    │
│  - Parse RDF/Turtle                                          │
│  - Validate SHACL constraints                                │
│  - Return ValidationReport                                   │
└──────────────────────────────────────────────────────────────┘
```

---

## Critical Architecture Constraints

### 1. Domain Layer Isolation

**CRITICAL**: `ggen-domain` MUST NOT depend on CLI-specific crates

```toml
# ✅ ALLOWED in ggen-domain
[dependencies]
ggen-core = { path = "../ggen-core" }
ggen-ai = { path = "../ggen-ai" }
ggen-marketplace = { path = "../ggen-marketplace" }
ggen-utils = { path = "../ggen-utils" }
serde = "1.0"
tokio = "1.47"

# ❌ FORBIDDEN in ggen-domain
# clap = "4.5"              # NO - CLI framework
# clap-noun-verb = "3.4.0"  # NO - CLI routing
```

**Why?**
- Enables reuse in Node.js addon (ggen-node)
- Future web API integration
- Testability without CLI framework

### 2. Dependency Flow Direction

**Rule**: Dependencies MUST flow downward through layers

```
✅ ALLOWED:
  ggen-cli → ggen-domain → ggen-core → ggen-utils
  ggen-domain → ggen-marketplace → ggen-utils
  ggen-domain → ggen-ai → ggen-utils

❌ FORBIDDEN:
  ggen-core → ggen-domain  (upward dependency)
  ggen-utils → ggen-core   (upward dependency)
  ggen-marketplace → ggen-cli  (skip layer)
```

### 3. Public API Stability

**Versioning Strategy**:
- `ggen-core`, `ggen-cli`, `ggen-marketplace`, `ggen-ai`: v2.6.0 (synchronized)
- `ggen-domain`: v3.1.0 (independent - tracks domain API changes)
- `ggen-utils`: v2.6.0 (synchronized)

**Breaking Change Policy**:
- Layer 0-1 (utils, core): Semver major bump
- Layer 2 (marketplace, ai): Can evolve faster
- Layer 3 (domain): Independent versioning for API stability
- Layer 4 (cli, node): CLI can change without domain changes

---

## Integration Points

### 1. Error Handling Chain

```rust
// All crates use consistent error handling

// ggen-utils: Foundation
pub type Result<T> = anyhow::Result<T>;

// ggen-core: Propagates errors with context
pub fn load_graph(path: &Path) -> Result<Graph> {
    Graph::load_file(path)
        .with_context(|| format!("Failed to load graph: {}", path.display()))
}

// ggen-domain: Business logic errors
pub async fn install_package(id: &str) -> Result<()> {
    resolve_version(id)?;
    download_package(id).await?;
    verify_signature()?;
    Ok(())
}

// ggen-cli: User-facing errors
pub async fn install(id: String) -> Result<()> {
    ggen_domain::marketplace::install_package(&id).await
        .with_context(|| format!("Installation failed: {}", id))
}
```

### 2. Observability Chain

```rust
// OTEL spans flow through all layers

// ggen-cli: Top-level span
#[tracing::instrument(name = "cli.marketplace.install")]
pub async fn install(id: String) -> Result<()> { ... }

// ggen-domain: Domain span
#[tracing::instrument(name = "domain.marketplace.install")]
pub async fn install_package(id: &str) -> Result<()> { ... }

// ggen-core: Engine span
#[tracing::instrument(name = "core.registry.download")]
pub async fn download(id: &str) -> Result<Vec<u8>> { ... }
```

### 3. Lifecycle Orchestration

```rust
// Lifecycle hooks coordinate across layers

// ggen-cli: Trigger lifecycle
ggen project build

// ggen-domain: Lifecycle execution
pub async fn run_project_lifecycle() -> Result<()> {
    lifecycle::run_pipeline(&make, &["build"]).await
}

// ggen-core: Phase execution
pub async fn run_phase(phase: &Phase, ctx: &Context) -> Result<()> {
    // Run before hooks
    run_hooks(&phase.before)?;

    // Execute commands
    execute_commands(&phase.commands)?;

    // Run after hooks
    run_hooks(&phase.after)?;

    Ok(())
}
```

---

## Workspace Dependency Resolution

### Shared Dependencies (Workspace-Level)

```toml
[workspace.dependencies]
# Async runtime
tokio = { version = "1.47", features = ["full"] }

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
serde_yaml = "0.9"

# Error handling
anyhow = "1.0"
thiserror = "2.0"

# RDF/SPARQL
oxigraph = "0.5.1"

# CLI (only for presentation layer)
clap = { version = "4.5", features = ["derive"] }
clap-noun-verb = "3.4.0"

# Observability
opentelemetry = "0.21"
opentelemetry-otlp = "0.14"
tracing = "0.1"
tracing-subscriber = "0.3"
```

### Crate-Specific Dependencies

```toml
# ggen-marketplace specific
[dependencies]
tantivy = "0.22"       # Full-text search
moka = "0.12"          # Smart caching
wasmtime = "28.0"      # WASM plugins
ndarray = "0.16"       # ML recommendations

# ggen-ai specific
[dependencies]
genai = "0.4"          # LLM client abstraction

# ggen-cli specific
[dependencies]
clap = { workspace = true }
clap-noun-verb = { workspace = true }
gag = "0.1"            # Output capture (for ggen-node)
```

---

## Build Dependency Graph

### Parallel Build Strategy

```
Parallel Build Groups (can build concurrently):

Group 1 (no dependencies):
  └─ ggen-utils

Group 2 (depends on Group 1):
  └─ ggen-core

Group 3 (depends on Groups 1-2):
  ├─ ggen-marketplace
  └─ ggen-ai

Group 4 (depends on Groups 1-3):
  └─ ggen-domain

Group 5 (depends on Groups 1-4):
  ├─ ggen-cli
  └─ ggen-node

Group 6 (depends on Group 5):
  └─ ggen (main binary)
```

**Build Command**:
```bash
# Cargo resolver v2 optimizes parallel builds
cargo build --workspace --release

# Typical build times (release mode):
# - ggen-utils: 5s
# - ggen-core: 30s (largest crate)
# - ggen-marketplace: 25s (tantivy compilation)
# - ggen-ai: 10s
# - ggen-domain: 15s
# - ggen-cli: 20s
# - Total: ~60s (with parallelism)
```

---

## Module Stability Matrix

| Module           | API Stability | Change Frequency | Breaking Change Impact |
|------------------|---------------|------------------|------------------------|
| ggen-utils       | High          | Low              | High (breaks all)      |
| ggen-core        | High          | Medium           | High (breaks all)      |
| ggen-marketplace | Medium        | Medium           | Medium (domain+cli)    |
| ggen-ai          | Medium        | High             | Low (domain+cli)       |
| ggen-domain      | High          | Medium           | High (cli+node)        |
| ggen-cli         | Low           | High             | Low (users only)       |
| ggen-node        | Medium        | Low              | Low (npm users)        |

**Stability Guidelines**:
- **High Stability**: Requires RFC for breaking changes
- **Medium Stability**: Breaking changes allowed in minor versions with deprecation warnings
- **Low Stability**: Can change freely (user-facing layer)

---

## Testing Dependency Graph

```
Test Dependencies Flow:

Unit Tests (per crate):
  └─ Each crate tests its own modules in isolation

Integration Tests:
  ggen-cli tests
    └─ Depends on: ggen-domain, ggen-core, ggen-utils

  ggen-domain tests
    └─ Depends on: ggen-core, ggen-marketplace, ggen-ai, ggen-utils

  ggen-core tests
    └─ Depends on: ggen-utils

E2E Tests (workspace root):
  tests/e2e_v2/
    └─ Depends on: Full workspace (all crates)

  tests/chicago_tdd/
    └─ Depends on: Full workspace + testcontainers
```

---

## Future Architecture Evolution

### Potential Layer Additions

**If Web API Needed** (not currently planned):
```
New Layer 4.5: Web API
  └─ ggen-web (Axum-based REST API)
      ├─ Depends on: ggen-domain (reuse all logic)
      └─ Provides: HTTP endpoints for marketplace, templates, etc.
```

**If Multi-Tenancy Needed** (not currently planned):
```
New Layer 2.5: Multi-Tenancy
  └─ ggen-tenant (tenant isolation)
      ├─ Depends on: ggen-core
      └─ Used by: ggen-domain
```

**Philosophy**: Add layers only when proven necessary by user demand (80/20 rule).

---

## Key Architectural Decisions

### ADR-001: Clean Architecture Separation

**Decision**: Separate domain logic (ggen-domain) from presentation (ggen-cli)

**Rationale**:
- Enables Node.js addon without CLI dependencies
- Future-proofs for web API
- Improves testability

**Status**: Implemented (v3.1.0)

### ADR-002: Poka-Yoke Type-Level Design

**Decision**: Use Rust type system to prevent errors at compile time

**Rationale**:
- Lifecycle state machine prevents invalid transitions
- Validated types prevent unvalidated data usage
- Zero runtime cost (PhantomData)

**Status**: Implemented (lifecycle module)

### ADR-003: P2P Removal

**Decision**: Remove P2P networking, GraphQL, HTTP endpoints

**Rationale**:
- Unused by users (YAGNI)
- High maintenance cost
- Simplifies architecture (80/20 value)

**Status**: Completed (v2.6.0)

### ADR-004: Workspace Dependency Versioning

**Decision**: Synchronize core/cli/marketplace/ai/utils versions, independent domain versioning

**Rationale**:
- Easier release management
- Domain API stability tracked separately
- Clear upgrade paths

**Status**: Implemented (v2.6.0 sync, domain v3.1.0)

---

**Last Updated**: 2025-11-14
**ggen Version**: 2.6.0
**Reviewer**: Hive Mind System Architect Agent
