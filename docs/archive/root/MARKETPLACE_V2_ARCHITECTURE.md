<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen-marketplace Architecture Document](#ggen-marketplace-architecture-document)
  - [Executive Summary](#executive-summary)
  - [Module Dependency Graph](#module-dependency-graph)
  - [Module Catalog (28 Modules)](#module-catalog-28-modules)
    - [Layer 1: Core Data Models](#layer-1-core-data-models)
    - [Layer 2: Registry Implementations](#layer-2-registry-implementations)
    - [Layer 3: RDF/Semantic Layer](#layer-3-rdfsemantic-layer)
    - [Layer 4: Search & Discovery](#layer-4-search--discovery)
    - [Layer 5: Installation & Security](#layer-5-installation--security)
    - [Layer 6: Operations & Migration](#layer-6-operations--migration)
    - [Layer 7: Additional Modules](#layer-7-additional-modules)
  - [Data Flow Architecture](#data-flow-architecture)
  - [Core Trait Definitions](#core-trait-definitions)
    - [AsyncRepository (Primary Storage Trait)](#asyncrepository-primary-storage-trait)
    - [Queryable (Search Interface)](#queryable-search-interface)
    - [Installable (Dependency Resolution)](#installable-dependency-resolution)
  - [API Contract Definitions](#api-contract-definitions)
    - [Registry API Surface](#registry-api-surface)
    - [RdfRegistry API Surface](#rdfregistry-api-surface)
    - [Installer API Surface](#installer-api-surface)
    - [SearchEngine API Surface](#searchengine-api-surface)
    - [SparqlSearchEngine API Surface](#sparqlsearchengine-api-surface)
  - [Error Handling Surface](#error-handling-surface)
  - [Integration Points](#integration-points)
    - [1. ggen-cli -> marketplace-v2](#1-ggen-cli---marketplace-v2)
    - [2. ggen-core -> marketplace-v2](#2-ggen-core---marketplace-v2)
    - [3. ggen-domain -> marketplace-v2](#3-ggen-domain---marketplace-v2)
  - [RdfRegistry Implementation Analysis (Phase 2A)](#rdfregistry-implementation-analysis-phase-2a)
    - [Current State](#current-state)
    - [RdfMapper Analysis](#rdfmapper-analysis)
  - [Critical Path Analysis](#critical-path-analysis)
    - [Path 1: Package Registration](#path-1-package-registration)
    - [Path 2: Package Search](#path-2-package-search)
    - [Path 3: Package Installation](#path-3-package-installation)
  - [Performance Characteristics](#performance-characteristics)
    - [Caching Strategy](#caching-strategy)
    - [Concurrency Model](#concurrency-model)
    - [SLO Targets (from v3.rs)](#slo-targets-from-v3rs)
  - [Security Model](#security-model)
    - [Cryptographic Operations](#cryptographic-operations)
    - [Signature Flow](#signature-flow)
  - [Poka-Yoke (Error Prevention) Patterns](#poka-yoke-error-prevention-patterns)
    - [Type-Level State Machines](#type-level-state-machines)
  - [Migration Strategy](#migration-strategy)
  - [Recommendations for Phase 2B-2C](#recommendations-for-phase-2b-2c)
    - [Phase 2B: Complete RdfRegistry CRUD](#phase-2b-complete-rdfregistry-crud)
    - [Phase 2C: Integration Testing](#phase-2c-integration-testing)
  - [Appendix: File Organization](#appendix-file-organization)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen-marketplace Architecture Document

**Version**: 3.0.0
**Author**: System Architect Agent
**Date**: 2025-01-21
**Status**: Phase 1 Complete - Architecture & Design

---

## Executive Summary

ggen-marketplace is a hyper-advanced marketplace system built on RDF (Resource Description Framework) semantic data store using oxigraph. The system provides:

- **Semantic Package Management**: Packages stored as RDF triples enabling SPARQL queries
- **Type-Safe Operations**: Poka-yoke patterns prevent invalid states at compile time
- **Zero-Copy Performance**: Advanced Rust patterns with minimal allocations
- **Ed25519 Cryptographic Security**: Package signing and verification
- **Comprehensive Observability**: Tracing and metrics collection

---

## Module Dependency Graph

```
                                    lib.rs (Entry Point)
                                         |
         +-------------------+-----------+-----------+-------------------+
         |                   |           |           |                   |
    [Core Data]         [Registry]   [Search]   [Security]        [Observability]
         |                   |           |           |                   |
    models.rs          registry.rs  search.rs   security.rs      metrics.rs
    traits.rs        registry_rdf.rs  search_sparql.rs              |
    builders.rs           v3.rs          |                    validation.rs
         |                   |           |
         +--------+----------+-----------+
                  |
             [RDF Layer]
                  |
    +-------------+-------------+
    |             |             |
  rdf_mapper.rs  ontology.rs   rdf/
                                |
              +-----------------+------------------+
              |         |           |              |
         control.rs  poka_yoke.rs  sparql.rs  state_machine.rs
         rdf_control.rs  sparql_queries.rs  turtle_config.rs
              fmea_mitigations.rs
```

---

## Module Catalog (28 Modules)

### Layer 1: Core Data Models

| Module | LOC | Purpose | Dependencies |
|--------|-----|---------|--------------|
| `models.rs` | ~560 | Core domain models (Package, PackageId, PackageVersion, Metadata) | chrono, serde, uuid |
| `traits.rs` | ~220 | Core trait definitions (AsyncRepository, Queryable, Installable) | async_trait |
| `builders.rs` | ~160 | Type-safe package builders with validation | models, error |
| `error.rs` | ~270 | Comprehensive error types with thiserror | thiserror |

### Layer 2: Registry Implementations

| Module | LOC | Purpose | Dependencies |
|--------|-----|---------|--------------|
| `registry.rs` | ~300 | High-performance async registry with DashMap and moka cache | dashmap, moka, traits |
| `registry_rdf.rs` | ~320 | RDF-backed registry using oxigraph | oxigraph, rdf_mapper, traits |
| `v3.rs` | ~300 | Production-grade registry with distributed caching and indexing | oxigraph, moka, parking_lot |

### Layer 3: RDF/Semantic Layer

| Module | LOC | Purpose | Dependencies |
|--------|-----|---------|--------------|
| `ontology.rs` | ~440 | RDF ontology: Namespaces, Classes, Properties, SPARQL templates | - |
| `rdf_mapper.rs` | ~790 | Bidirectional Package <-> RDF triple conversion | oxigraph, ontology, models |
| `rdf/mod.rs` | ~40 | RDF submodule exports | - |
| `rdf/control.rs` | ~490 | RDF Control Plane with epoch-based cache invalidation | oxigraph, sparql, state_machine |
| `rdf/poka_yoke.rs` | ~650 | Type-safe RDF operations with compile-time guarantees | serde, PhantomData |
| `rdf/sparql.rs` | - | SPARQL executor and query builder | oxigraph |
| `rdf/sparql_queries.rs` | - | Pre-built SPARQL query templates | ontology |
| `rdf/state_machine.rs` | - | Package lifecycle state machine | - |
| `rdf/turtle_config.rs` | - | Configuration loading from .ttl files | - |
| `rdf/fmea_mitigations.rs` | - | Failure detection and auto-recovery | - |
| `rdf/ontology.rs` | - | RDF submodule ontology definitions | - |

### Layer 4: Search & Discovery

| Module | LOC | Purpose | Dependencies |
|--------|-----|---------|--------------|
| `search.rs` | ~370 | Full-text search with fuzzy matching and relevance ranking | models, traits |
| `search_sparql.rs` | ~190 | SPARQL-powered semantic search | oxigraph, ontology |

### Layer 5: Installation & Security

| Module | LOC | Purpose | Dependencies |
|--------|-----|---------|--------------|
| `install.rs` | ~280 | Package installation with dependency resolution | models, traits, registry |
| `security.rs` | ~280 | Ed25519 signing, SHA-256 checksums | ed25519-dalek, sha2, hex |
| `validation.rs` | ~360 | Pluggable package validation framework | models, traits |

### Layer 6: Operations & Migration

| Module | LOC | Purpose | Dependencies |
|--------|-----|---------|--------------|
| `metrics.rs` | ~310 | Observability and metrics collection | dashmap, atomic |
| `migration.rs` | ~440 | v1 -> v2 data migration with verification | registry_rdf, traits |

### Layer 7: Additional Modules

| Module | LOC | Purpose | Dependencies |
|--------|-----|---------|--------------|
| `fmea_mitigations.rs` | - | Top-level FMEA mitigations | - |

---

## Data Flow Architecture

```
[User Request]
      |
      v
+------------------+
|    ggen-cli      | <-- Command parsing (clap)
+------------------+
      |
      v
+------------------+     +------------------+
| SearchEngine     |<--->| SparqlSearchEngine|
+------------------+     +------------------+
      |                        |
      v                        v
+------------------+     +------------------+
|    Registry      |     |   RdfRegistry    |
| (DashMap+Cache)  |     |   (oxigraph)     |
+------------------+     +------------------+
      |                        |
      v                        v
+------------------+     +------------------+
|  RdfMapper       |<--->|  SPARQL Queries  |
+------------------+     +------------------+
      |
      v
+------------------+
| oxigraph Store   | <-- Single Source of Truth
| (RDF Triples)    |
+------------------+
```

---

## Core Trait Definitions

### AsyncRepository (Primary Storage Trait)

```rust
#[async_trait]
pub trait AsyncRepository: Send + Sync {
    type PackageIterator: Iterator<Item = Package> + Send;

    async fn get_package(&self, id: &PackageId) -> Result<Package>;
    async fn get_package_version(&self, id: &PackageId, version: &PackageVersion) -> Result<Package>;
    async fn all_packages(&self) -> Result<Vec<Package>>;
    async fn list_versions(&self, id: &PackageId) -> Result<Vec<PackageVersion>>;
    async fn package_exists(&self, id: &PackageId) -> Result<bool>;
}
```

**Implementations**:
- `Registry` - In-memory DashMap with moka cache
- `RdfRegistry` - oxigraph RDF store with SPARQL
- `V3OptimizedRegistry` - Two-level caching with full-text index

### Queryable (Search Interface)

```rust
#[async_trait]
pub trait Queryable: Send + Sync {
    type Query: Send + Sync;
    type QueryResult: Send + Sync;

    async fn query(&self, query: Self::Query) -> Result<Self::QueryResult>;
    fn explain_query(&self, query: &Self::Query) -> String;
}
```

### Installable (Dependency Resolution)

```rust
#[async_trait]
pub trait Installable: Send + Sync {
    async fn install(&self, manifest: InstallationManifest) -> Result<InstallationManifest>;
    async fn resolve_dependencies(&self, id: &PackageId, version: &PackageVersion)
        -> Result<Vec<(PackageId, PackageVersion)>>;
    async fn dry_run_install(&self, manifest: &InstallationManifest) -> Result<String>;
}
```

---

## API Contract Definitions

### Registry API Surface

| Method | Signature | Description |
|--------|-----------|-------------|
| `insert` | `fn insert(&self, package: Package) -> Result<()>` | Add package to registry |
| `remove` | `fn remove(&self, id: &PackageId) -> Result<Option<Package>>` | Remove package |
| `update` | `fn update(&self, id: &PackageId, package: Package) -> Result<()>` | Update existing |
| `get_package` | `async fn get_package(&self, id: &PackageId) -> Result<Package>` | Retrieve by ID |
| `all_packages` | `async fn all_packages(&self) -> Result<Vec<Package>>` | List all packages |

### RdfRegistry API Surface

| Method | Signature | Description |
|--------|-----------|-------------|
| `new` | `fn new() -> Self` | Create with in-memory store |
| `insert_package_rdf` | `async fn insert_package_rdf(&self, package: &Package) -> Result<()>` | Insert as RDF |
| `batch_insert_packages` | `async fn batch_insert_packages(&self, packages: Vec<Package>) -> Result<usize>` | Batch insert |
| `query_sparql` | `fn query_sparql(&self, query: &str) -> Result<Vec<String>>` | Execute SPARQL |
| `stats` | `fn stats(&self) -> RdfRegistryStats` | Get statistics |

### Installer API Surface

| Method | Signature | Description |
|--------|-----------|-------------|
| `new` | `fn new(repository: R) -> Self` | Create with repository |
| `resolve_dependencies` | `async fn resolve_dependencies(&self, root_id: &PackageId, root_version: &PackageVersion) -> Result<Vec<(PackageId, PackageVersion)>>` | Dependency resolution |
| `create_manifest` | `async fn create_manifest(&self, package_ids: Vec<PackageId>, install_path: String) -> Result<InstallationManifest>` | Create install manifest |
| `dry_run` | `async fn dry_run(&self, manifest: &InstallationManifest) -> Result<InstallationPlan>` | Simulate installation |
| `install` | `async fn install(&self, manifest: InstallationManifest) -> Result<InstallationManifest>` | Execute installation |

### SearchEngine API Surface

| Method | Signature | Description |
|--------|-----------|-------------|
| `new` | `fn new() -> Self` | Create with default ranker |
| `search` | `fn search(&self, packages: Vec<Package>, query: &SearchQuery) -> Result<Vec<SearchResult>>` | Full-text search |

### SparqlSearchEngine API Surface

| Method | Signature | Description |
|--------|-----------|-------------|
| `new` | `fn new(store: Arc<Store>) -> Self` | Create with oxigraph store |
| `search_by_name` | `fn search_by_name(&self, name: &str) -> Result<Vec<String>>` | Semantic name search |
| `search_by_description` | `fn search_by_description(&self, text: &str) -> Result<Vec<String>>` | Description search |
| `search_by_keyword` | `fn search_by_keyword(&self, keyword: &str) -> Result<Vec<String>>` | Keyword/category search |
| `trending_packages` | `fn trending_packages(&self, limit: usize) -> Result<Vec<String>>` | By downloads |
| `search_by_quality` | `fn search_by_quality(&self, min_score: u32) -> Result<Vec<String>>` | Quality filter |

---

## Error Handling Surface

```rust
pub enum Error {
    // Domain Errors
    PackageNotFound { package_id: String },
    InvalidPackageId { reason: String },
    InvalidVersion { version: String, reason: String },
    PackageAlreadyExists { package_id: String },
    VersionAlreadyExists { package_id: String, version: String },
    DependencyResolutionFailed { package_id: String, reason: String },
    InstallationFailed { reason: String },
    ValidationFailed { reason: String },

    // Security Errors
    SecurityCheckFailed { reason: String },
    SignatureVerificationFailed { reason: String },

    // RDF/SPARQL Errors
    SparqlError { query: String, reason: String },
    RdfStoreError { operation: String, reason: String },
    InvalidStateTransition { from: String, to: String },

    // Infrastructure Errors
    IoError(std::io::Error),
    SerializationError(serde_json::Error),
    TomlError(toml::de::Error),
    CryptoError(String),
    SearchError(String),
    Timeout(String),
}
```

---

## Integration Points

### 1. ggen-cli -> marketplace-v2

```
ggen-cli commands:
  - `ggen marketplace search <query>` -> SearchEngine/SparqlSearchEngine
  - `ggen marketplace install <pkg>` -> Installer
  - `ggen marketplace info <pkg>` -> Registry/RdfRegistry
  - `ggen marketplace list` -> AsyncRepository::all_packages()
  - `ggen marketplace validate <pkg>` -> PackageValidator
```

### 2. ggen-core -> marketplace-v2

```
Shared Types:
  - Template validation patterns
  - Error handling conventions
  - Configuration loading (TOML)
  - Logging/tracing integration
```

### 3. ggen-domain -> marketplace-v2

```
Domain Model Relationships:
  - Package metadata flows to domain ontology
  - Version constraints shared with domain model
  - Dependency resolution integrates with domain graph
```

---

## RdfRegistry Implementation Analysis (Phase 2A)

### Current State

The `RdfRegistry` in `registry_rdf.rs` provides:

**Implemented CRUD Operations**:
1. **Create**: `insert_package_rdf(&self, package: &Package)` - Converts Package to RDF triples via `RdfMapper::package_to_rdf()`
2. **Read**: `get_package(&self, id: &PackageId)` - Reconstructs Package from RDF via `RdfMapper::rdf_to_package()`
3. **Update**: Not explicitly implemented - would require delete + insert pattern
4. **Delete**: Not explicitly implemented - would require SPARQL DELETE

**RDF Triple Storage**:
- Uses oxigraph `Store` as single source of truth
- Packages stored with namespace `https://ggen.io/marketplace/`
- Package class: `<GGEN_NS>Package`
- All metadata, versions, releases stored as RDF triples

**Error Handling**:
- Returns `Result<T, Error>` for all operations
- Converts oxigraph errors to `Error::SearchError` or `Error::RegistryError`
- Package not found returns `Error::PackageNotFound`

**Type Safety**:
- `PackageId` validated on construction (alphanumeric, hyphen, underscore)
- `PackageVersion` validated for semantic versioning
- All IDs are strongly typed (no raw strings)

### RdfMapper Analysis

The `RdfMapper` in `rdf_mapper.rs` provides complete bidirectional conversion:

**Package -> RDF** (`package_to_rdf`):
- Inserts package type triple (`rdf:type mp:Package`)
- Inserts all metadata fields (name, description, license, etc.)
- Inserts author relationships with separate author nodes
- Inserts version and release information
- Inserts dependency relationships

**RDF -> Package** (`rdf_to_package`):
- Queries package URI for existence
- Reconstructs `PackageMetadata` via SPARQL SELECT
- Queries authors and keywords separately
- Reconstructs version list and release information
- Handles optional fields gracefully

---

## Critical Path Analysis

### Path 1: Package Registration

```
1. User -> ggen-cli -> RdfRegistry::insert_package_rdf()
2. RdfRegistry -> RdfMapper::package_to_rdf()
3. RdfMapper -> oxigraph::Store::insert() [multiple triples]
4. Return Result<()>
```

### Path 2: Package Search

```
1. User -> ggen-cli -> SparqlSearchEngine::search_by_name()
2. SparqlSearchEngine -> Queries::search_by_name() [SPARQL template]
3. SparqlSearchEngine -> oxigraph::Store::query()
4. Parse QueryResults::Solutions -> Vec<String>
5. Return package URIs
```

### Path 3: Package Installation

```
1. User -> ggen-cli -> Installer::create_manifest()
2. Installer -> AsyncRepository::get_package() [for each pkg]
3. Installer -> resolve_dependencies() [recursive]
4. Installer -> check_conflicts()
5. Installer -> install() [download, verify, extract]
6. Return InstallationManifest
```

---

## Performance Characteristics

### Caching Strategy

| Component | Cache Type | TTL | Size |
|-----------|-----------|-----|------|
| Registry | moka AsyncCache | Configurable | Variable |
| V3OptimizedRegistry (hot) | moka AsyncCache | 5 min | 1000 |
| V3OptimizedRegistry (meta) | moka AsyncCache | 1 hour | 5000 |
| RdfControlPlane (plan) | LRU | Epoch-based | 100 |
| RdfControlPlane (result) | LRU | Epoch-based | 1000 |

### Concurrency Model

- `DashMap` for lock-free concurrent access in `Registry`
- `parking_lot::RwLock` for RDF store write operations
- `Arc<AtomicU64>` for epoch-based cache invalidation
- All traits require `Send + Sync`

### SLO Targets (from v3.rs)

- Lookup latency: < 100ms
- Search latency: < 200ms
- Cache hit rate target: > 80%

---

## Security Model

### Cryptographic Operations

| Operation | Algorithm | Key Size |
|-----------|-----------|----------|
| Package Signing | Ed25519 | 256-bit |
| Checksum | SHA-256 | 256-bit |
| Key Encoding | Hex | - |

### Signature Flow

```
1. Generate KeyPair (Ed25519)
2. Sign package data -> hex-encoded signature
3. Store signature with package in RDF
4. Verify on install via SignatureVerifier
```

---

## Poka-Yoke (Error Prevention) Patterns

### Type-Level State Machines

The `rdf/poka_yoke.rs` module implements:

1. **Triple Builder Typestate**:
   ```rust
   Triple::builder()
       .subject(...)     // NoSubject -> HasSubject
       .predicate(...)   // HasSubject -> HasPredicate
       .object_literal(...)  // HasPredicate -> Complete
       .build()          // Only callable on Complete
   ```

2. **SPARQL Query Validation**:
   ```rust
   SparqlQuery::new()
       .prefix(...)
       .select(...)
       .where_pattern(...)
       .validate()   // Building -> Validated (or PokaYokeError)
       .to_string()  // Only callable on Validated
   ```

3. **Resource ID Validation**:
   - URIs must start with `http://`, `https://`, or `urn:`
   - Invalid URIs return `PokaYokeError::InvalidUri`

---

## Migration Strategy

The `migration.rs` module provides:

1. **Full Migration**: `migrate_packages(v1_packages)` - Batch insert with reporting
2. **Incremental Migration**: `incremental_migrate(v1_packages)` - Skip existing
3. **Verification**: `verify_migration(v1_packages)` - Compare v1 vs v2 data
4. **Consistency Check**: `ConsistencyChecker::periodic_check()` - Ongoing validation

---

## Recommendations for Phase 2B-2C

### Phase 2B: Complete RdfRegistry CRUD

1. **Add explicit Update operation**:
   ```rust
   pub async fn update_package(&self, package: &Package) -> Result<()> {
       self.delete_package(&package.metadata.id).await?;
       self.insert_package_rdf(package).await
   }
   ```

2. **Add explicit Delete operation**:
   ```rust
   pub async fn delete_package(&self, id: &PackageId) -> Result<()> {
       let delete_query = format!(
           "DELETE WHERE {{ <{0}packages/{1}> ?p ?o }}",
           GGEN_NS, id
       );
       // Execute SPARQL DELETE
   }
   ```

### Phase 2C: Integration Testing

1. Round-trip tests: Package -> RDF -> Package
2. SPARQL query correctness tests
3. Concurrent access tests with multiple writers
4. Cache invalidation tests
5. Migration verification tests

---

## Appendix: File Organization

```
ggen/crates/ggen-marketplace/
├── Cargo.toml                    # Dependencies (v3.0.0)
├── src/
│   ├── lib.rs                    # Module exports, prelude
│   ├── builders.rs               # Type-safe builders
│   ├── error.rs                  # Error types
│   ├── fmea_mitigations.rs       # Failure mitigations
│   ├── install.rs                # Package installer
│   ├── metrics.rs                # Observability
│   ├── migration.rs              # v1 -> v2 migration
│   ├── models.rs                 # Domain models
│   ├── ontology.rs               # RDF ontology
│   ├── registry.rs               # In-memory registry
│   ├── registry_rdf.rs           # RDF-backed registry
│   ├── rdf/
│   │   ├── mod.rs                # Submodule exports
│   │   ├── control.rs            # RDF Control Plane
│   │   ├── fmea_mitigations.rs   # RDF failure handling
│   │   ├── ontology.rs           # RDF ontology types
│   │   ├── poka_yoke.rs          # Type-safe RDF ops
│   │   ├── rdf_control.rs        # Control plane impl
│   │   ├── sparql.rs             # SPARQL executor
│   │   ├── sparql_queries.rs     # Query templates
│   │   ├── state_machine.rs      # Package state machine
│   │   └── turtle_config.rs      # TTL config loader
│   ├── rdf_mapper.rs             # Package <-> RDF mapper
│   ├── search.rs                 # Full-text search
│   ├── search_sparql.rs          # SPARQL search
│   ├── security.rs               # Cryptographic ops
│   ├── traits.rs                 # Core traits
│   ├── v3.rs                     # Production registry
│   └── validation.rs             # Package validation
├── tests/
│   └── unit/                     # Unit tests
└── benches/                      # Performance benchmarks
```

---

**Document Status**: Complete for Phase 1 & 2A
**Next Steps**: Phase 2B (Complete CRUD), Phase 2C (Integration Tests)
