# Code Organization & Implementation Structure

## Overview

This document defines the file structure, module hierarchy, and code organization for the marketplace v2 migration.

## Directory Structure

```
ggen/
├── crates/
│   ├── ggen-marketplace/           # V1 backend (existing)
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── search.rs           # Tantivy search engine
│   │   │   ├── registry.rs         # V1 package registry
│   │   │   ├── models.rs           # V1 data models
│   │   │   └── install.rs          # V1 installation logic
│   │   ├── tests/
│   │   └── Cargo.toml
│   │
│   ├── ggen-marketplace/        # V2 backend (new)
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── search/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── sparql_engine.rs    # SPARQL search engine
│   │   │   │   ├── query_builder.rs    # SPARQL query construction
│   │   │   │   └── ranking.rs          # Result ranking algorithms
│   │   │   ├── registry/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── rdf_registry.rs     # RDF-based package registry
│   │   │   │   ├── store.rs            # Oxigraph store wrapper
│   │   │   │   └── install.rs          # V2 installation logic
│   │   │   ├── crypto/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── ed25519.rs          # Ed25519 signing/verification
│   │   │   │   ├── keygen.rs           # Key generation utilities
│   │   │   │   └── keystore.rs         # Secure key storage
│   │   │   ├── models/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── package.rs          # V2 package models
│   │   │   │   ├── rdf.rs              # RDF triple models
│   │   │   │   └── signature.rs        # Signature models
│   │   │   ├── ontology/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── schema.rs           # RDF schema definitions
│   │   │   │   └── validation.rs       # SHACL validation
│   │   │   └── conversion/
│   │   │       ├── mod.rs
│   │   │       ├── rdf_builder.rs      # Build RDF from package data
│   │   │       └── rdf_parser.rs       # Parse package data from RDF
│   │   ├── schemas/
│   │   │   ├── package.ttl             # Package ontology
│   │   │   └── validation.shacl        # SHACL validation rules
│   │   ├── tests/
│   │   │   ├── sparql_search_tests.rs
│   │   │   ├── crypto_tests.rs
│   │   │   └── integration/
│   │   │       ├── mod.rs
│   │   │       └── e2e_tests.rs
│   │   └── Cargo.toml
│   │
│   ├── ggen-domain/                # Unified adapter layer
│   │   ├── src/
│   │   │   ├── marketplace/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── backend.rs          # MarketplaceBackend trait
│   │   │   │   ├── models.rs           # Unified data models
│   │   │   │   ├── v1_adapter.rs       # V1 backend adapter
│   │   │   │   ├── v2_adapter.rs       # V2 backend adapter
│   │   │   │   ├── dual_adapter.rs     # Dual backend (A/B testing)
│   │   │   │   ├── conversion/
│   │   │   │   │   ├── mod.rs
│   │   │   │   │   ├── v1_to_unified.rs
│   │   │   │   │   ├── v2_to_unified.rs
│   │   │   │   │   ├── unified_to_v1.rs
│   │   │   │   │   └── unified_to_v2.rs
│   │   │   │   ├── error.rs            # Unified error types
│   │   │   │   └── config.rs           # Backend configuration
│   │   │   └── ...
│   │   ├── tests/
│   │   │   ├── marketplace_adapter_tests.rs
│   │   │   ├── conversion_roundtrip_tests.rs
│   │   │   └── backend_compatibility_tests.rs
│   │   └── Cargo.toml
│   │
│   ├── ggen-cli/                   # CLI integration
│   │   ├── src/
│   │   │   ├── cmds/
│   │   │   │   ├── marketplace.rs      # Marketplace commands
│   │   │   │   └── marketplace/
│   │   │   │       ├── mod.rs
│   │   │   │       ├── search.rs
│   │   │   │       ├── install.rs
│   │   │   │       ├── publish.rs
│   │   │   │       ├── migrate.rs      # Migration utilities
│   │   │   │       └── keygen.rs       # Key generation
│   │   │   ├── config/
│   │   │   │   ├── mod.rs
│   │   │   │   └── marketplace.rs      # Marketplace config
│   │   │   └── ...
│   │   ├── tests/
│   │   │   ├── integration/
│   │   │   │   ├── marketplace_v1_tests.rs
│   │   │   │   ├── marketplace_v2_tests.rs
│   │   │   │   └── marketplace_migration_tests.rs
│   │   │   └── ...
│   │   └── Cargo.toml
│   │
│   └── ggen-core/                  # Core functionality
│       ├── src/
│       │   ├── marketplace/
│       │   │   ├── mod.rs              # Feature-gated exports
│       │   │   ├── v1/                 # V1 backend (conditional)
│       │   │   │   └── mod.rs
│       │   │   ├── v2/                 # V2 backend (conditional)
│       │   │   │   └── mod.rs
│       │   │   └── adapter/            # Adapter layer (conditional)
│       │   │       └── mod.rs
│       │   └── ...
│       └── Cargo.toml
│
├── docs/
│   └── architecture/
│       └── marketplace-v2-migration/
│           ├── 01-feature-gates.md
│           ├── 02-adapter-pattern.md
│           ├── 03-data-model-bridging.md
│           ├── 04-migration-phases.md
│           ├── 05-code-organization.md      # This file
│           ├── 06-error-handling.md
│           ├── 07-performance-strategy.md
│           ├── 08-testing-strategy.md
│           └── 09-deployment-rollout.md
│
└── tests/
    ├── integration/
    │   ├── marketplace_v1_e2e.rs
    │   ├── marketplace_v2_e2e.rs
    │   └── marketplace_migration_e2e.rs
    └── benchmarks/
        ├── marketplace_v1_benchmark.rs
        ├── marketplace_v2_benchmark.rs
        └── marketplace_comparison_benchmark.rs
```

## Module Hierarchy

### ggen-marketplace (New Crate)

```rust
// ggen-marketplace/src/lib.rs

pub mod search;
pub mod registry;
pub mod crypto;
pub mod models;
pub mod ontology;
pub mod conversion;

// Re-exports for ergonomics
pub use search::SparqlSearchEngine;
pub use registry::RdfRegistry;
pub use crypto::{Ed25519Signer, KeyGenerator};
pub use models::{V2Package, PackageMetadata, PackageUri};

// Prelude for convenience
pub mod prelude {
    pub use crate::search::SparqlSearchEngine;
    pub use crate::registry::RdfRegistry;
    pub use crate::crypto::Ed25519Signer;
    pub use crate::models::*;
}
```

### ggen-domain/src/marketplace

```rust
// ggen-domain/src/marketplace/mod.rs

pub mod backend;
pub mod models;
pub mod conversion;
pub mod error;
pub mod config;

// Feature-gated adapters
#[cfg(feature = "marketplace-v1")]
pub mod v1_adapter;

#[cfg(feature = "marketplace-v2")]
pub mod v2_adapter;

#[cfg(feature = "marketplace-parallel")]
pub mod dual_adapter;

// Re-exports
pub use backend::MarketplaceBackend;
pub use models::*;
pub use error::{MarketplaceError, Result};

// Adapter factory (feature-dependent)
pub fn create_backend(config: config::MarketplaceConfig) -> Result<Box<dyn MarketplaceBackend>> {
    #[cfg(all(feature = "marketplace-parallel"))]
    {
        dual_adapter::create_dual_backend(config)
    }

    #[cfg(all(feature = "marketplace-v2", not(feature = "marketplace-parallel")))]
    {
        v2_adapter::create_v2_backend(config)
    }

    #[cfg(all(feature = "marketplace-v1", not(feature = "marketplace-parallel")))]
    {
        v1_adapter::create_v1_backend(config)
    }

    #[cfg(not(any(
        feature = "marketplace-v1",
        feature = "marketplace-v2",
        feature = "marketplace-parallel"
    )))]
    {
        compile_error!("At least one marketplace feature must be enabled")
    }
}
```

### ggen-core/src/marketplace

```rust
// ggen-core/src/marketplace/mod.rs

// Feature-gated module structure

#[cfg(feature = "marketplace-v1")]
pub mod v1 {
    // Re-export ggen-marketplace types
    pub use ggen_marketplace::*;
}

#[cfg(feature = "marketplace-v2")]
pub mod v2 {
    // Re-export ggen-marketplace types
    pub use ggen_marketplace::*;
}

#[cfg(feature = "marketplace-parallel")]
pub mod adapter {
    // Re-export adapter types from ggen-domain
    pub use ggen_domain::marketplace::{
        MarketplaceBackend,
        DualBackendAdapter,
        BackendStrategy,
    };
}

// Default re-exports based on feature flags
#[cfg(all(feature = "marketplace-v1", not(feature = "marketplace-parallel")))]
pub use v1::*;

#[cfg(all(feature = "marketplace-v2", not(feature = "marketplace-parallel")))]
pub use v2::*;

#[cfg(feature = "marketplace-parallel")]
pub use adapter::*;
```

## Conditional Compilation Patterns

### Pattern 1: Feature-Gated Module Inclusion

```rust
// ggen-domain/src/marketplace/mod.rs

#[cfg(feature = "marketplace-v1")]
pub mod v1_adapter;

#[cfg(feature = "marketplace-v2")]
pub mod v2_adapter;

#[cfg(feature = "marketplace-parallel")]
pub mod dual_adapter;
```

### Pattern 2: Feature-Gated Implementations

```rust
// ggen-domain/src/marketplace/backend.rs

pub trait MarketplaceBackend: Send + Sync {
    // ... trait methods
}

// V1 implementation
#[cfg(feature = "marketplace-v1")]
impl MarketplaceBackend for v1_adapter::V1Adapter {
    // ... implementation
}

// V2 implementation
#[cfg(feature = "marketplace-v2")]
impl MarketplaceBackend for v2_adapter::V2Adapter {
    // ... implementation
}
```

### Pattern 3: Feature-Gated Factory Functions

```rust
// ggen-domain/src/marketplace/mod.rs

pub fn create_backend(config: Config) -> Result<Box<dyn MarketplaceBackend>> {
    #[cfg(feature = "marketplace-parallel")]
    {
        create_dual_backend(config)
    }

    #[cfg(all(feature = "marketplace-v2", not(feature = "marketplace-parallel")))]
    {
        create_v2_backend(config)
    }

    #[cfg(all(feature = "marketplace-v1", not(feature = "marketplace-parallel")))]
    {
        create_v1_backend(config)
    }
}

#[cfg(feature = "marketplace-v1")]
fn create_v1_backend(config: Config) -> Result<Box<dyn MarketplaceBackend>> {
    Ok(Box::new(V1Adapter::new(config.v1)?))
}

#[cfg(feature = "marketplace-v2")]
fn create_v2_backend(config: Config) -> Result<Box<dyn MarketplaceBackend>> {
    Ok(Box::new(V2Adapter::new(config.v2)?))
}

#[cfg(feature = "marketplace-parallel")]
fn create_dual_backend(config: Config) -> Result<Box<dyn MarketplaceBackend>> {
    Ok(Box::new(DualBackendAdapter::new(
        config.v1,
        config.v2,
        config.strategy,
    )?))
}
```

### Pattern 4: Feature-Gated Dependencies

```toml
# ggen-domain/Cargo.toml

[dependencies]
# V1 backend (optional)
ggen-marketplace = { workspace = true, optional = true }

# V2 backend (optional)
ggen-marketplace = { workspace = true, optional = true }

# ... other deps

[features]
marketplace-v1 = ["ggen-marketplace"]
marketplace-v2 = ["ggen-marketplace"]
marketplace-parallel = ["marketplace-v1", "marketplace-v2"]
```

## Test Organization

### Unit Tests (Per-Backend)

```rust
// ggen-domain/tests/marketplace_adapter_tests.rs

#[cfg(feature = "marketplace-v1")]
mod v1_adapter_tests {
    use ggen_domain::marketplace::{V1Adapter, MarketplaceBackend};

    #[tokio::test]
    async fn test_v1_search() {
        let adapter = V1Adapter::new(test_config()).unwrap();
        let results = adapter.search(&test_query()).await.unwrap();
        assert!(!results.packages.is_empty());
    }
}

#[cfg(feature = "marketplace-v2")]
mod v2_adapter_tests {
    use ggen_domain::marketplace::{V2Adapter, MarketplaceBackend};

    #[tokio::test]
    async fn test_v2_search() {
        let adapter = V2Adapter::new(test_config()).unwrap();
        let results = adapter.search(&test_query()).await.unwrap();
        assert!(!results.packages.is_empty());
    }
}

#[cfg(feature = "marketplace-parallel")]
mod dual_adapter_tests {
    use ggen_domain::marketplace::{DualBackendAdapter, BackendStrategy};

    #[tokio::test]
    async fn test_fallback() {
        let adapter = DualBackendAdapter::new(
            v1_config(),
            v2_config(),
            BackendStrategy::V2WithFallback,
        ).unwrap();

        let results = adapter.search(&test_query()).await.unwrap();
        assert!(!results.packages.is_empty());
    }
}
```

### Integration Tests (Cross-Backend)

```rust
// tests/integration/marketplace_compatibility_tests.rs

#![cfg(feature = "marketplace-parallel")]

use ggen_domain::marketplace::*;

#[tokio::test]
async fn test_same_results_from_both_backends() {
    let v1 = V1Adapter::new(test_config()).unwrap();
    let v2 = V2Adapter::new(test_config()).unwrap();

    let query = SearchQuery {
        query_text: "rust web framework".to_string(),
        limit: 10,
        ..Default::default()
    };

    let v1_results = v1.search(&query).await.unwrap();
    let v2_results = v2.search(&query).await.unwrap();

    // Results should be similar (not necessarily identical due to ranking differences)
    assert_eq!(v1_results.packages.len(), v2_results.packages.len());
}
```

### Benchmark Organization

```rust
// benches/marketplace_comparison_benchmark.rs

#![cfg(feature = "marketplace-parallel")]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_domain::marketplace::*;

fn benchmark_search_v1(c: &mut Criterion) {
    let adapter = V1Adapter::new(test_config()).unwrap();
    let query = test_query();

    c.bench_function("search_v1", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| adapter.search(black_box(&query)))
    });
}

fn benchmark_search_v2(c: &mut Criterion) {
    let adapter = V2Adapter::new(test_config()).unwrap();
    let query = test_query();

    c.bench_function("search_v2", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| adapter.search(black_box(&query)))
    });
}

criterion_group!(benches, benchmark_search_v1, benchmark_search_v2);
criterion_main!(benches);
```

## Configuration File Organization

```yaml
# ~/.config/ggen/config.yaml

marketplace:
  # Backend selection (v1, v2, auto, ab-test)
  backend: auto

  # V1 backend configuration
  v1:
    index_path: ~/.ggen/marketplace/v1/index
    registry_path: ~/.ggen/marketplace/v1/registry
    cache_ttl: 3600

  # V2 backend configuration
  v2:
    rdf_store_path: ~/.ggen/marketplace/v2/store
    keypair_path: ~/.ggen/keys/signing.key
    enable_signature_verification: true
    sparql_optimization: true

  # A/B testing configuration
  ab_testing:
    enabled: false
    v2_percentage: 10
    log_backend_selection: true
    compare_results: false

  # Fallback configuration
  fallback:
    enabled: true
    fallback_to_v1: true
    retry_attempts: 3

  # Performance tuning
  performance:
    search_timeout_ms: 5000
    install_timeout_ms: 30000
    enable_caching: true
    cache_size: 1000
```

## CLI Command Structure

```rust
// ggen-cli/src/cmds/marketplace.rs

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;

#[verb]
fn search(query: String, backend: Option<String>) -> Result<SearchOutput> {
    let backend = create_backend_from_config_or_flag(backend)?;
    // ... implementation
}

#[verb]
fn install(package: String, backend: Option<String>) -> Result<InstallOutput> {
    let backend = create_backend_from_config_or_flag(backend)?;
    // ... implementation
}

#[verb]
fn publish(sign: bool, key: Option<PathBuf>) -> Result<PublishOutput> {
    // Publishing requires V2 backend if signing is enabled
    let backend = if sign {
        create_v2_backend()?
    } else {
        create_backend_from_config()?
    };
    // ... implementation
}

#[cfg(feature = "marketplace-parallel")]
#[verb]
fn migrate_to_v2(dry_run: bool, backup: bool) -> Result<MigrationOutput> {
    // Migration utility (requires both backends)
    let v1 = create_v1_backend()?;
    let v2 = create_v2_backend()?;
    // ... implementation
}

// Helper: Create backend from config or CLI flag
fn create_backend_from_config_or_flag(
    backend_flag: Option<String>,
) -> Result<Box<dyn MarketplaceBackend>> {
    let config = load_marketplace_config()?;

    // CLI flag overrides config
    let backend_type = backend_flag
        .as_deref()
        .map(|s| s.parse())
        .transpose()?
        .unwrap_or(config.backend);

    match backend_type {
        BackendType::V1 => create_v1_backend(),
        BackendType::V2 => create_v2_backend(),
        BackendType::Auto => create_backend_from_config(),
        BackendType::ABTest => create_dual_backend(),
    }
}
```

## Error Handling Organization

```rust
// ggen-domain/src/marketplace/error.rs

use thiserror::Error;

#[derive(Error, Debug)]
pub enum MarketplaceError {
    #[error("V1 backend error: {0}")]
    V1Error(#[from] ggen_marketplace::Error),

    #[error("V2 backend error: {0}")]
    V2Error(#[from] ggen_marketplace::Error),

    #[error("Conversion error: {0}")]
    ConversionError(String),

    #[error("Backend not available: {0}")]
    BackendNotAvailable(String),

    #[error("Configuration error: {0}")]
    ConfigError(String),

    #[error("Signature verification failed")]
    SignatureVerificationFailed,

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, MarketplaceError>;
```

## Documentation Structure

```
docs/
└── marketplace-v2/
    ├── README.md                   # Overview
    ├── migration-guide.md          # For users
    ├── publishing-guide.md         # For publishers
    ├── cryptographic-signing.md    # Security docs
    ├── performance-tuning.md       # Optimization guide
    ├── troubleshooting.md          # Common issues
    └── architecture/               # Architecture docs (this folder)
        ├── 01-feature-gates.md
        ├── 02-adapter-pattern.md
        ├── 03-data-model-bridging.md
        ├── 04-migration-phases.md
        ├── 05-code-organization.md
        ├── 06-error-handling.md
        ├── 07-performance-strategy.md
        ├── 08-testing-strategy.md
        └── 09-deployment-rollout.md
```

## Build Script Organization

```rust
// ggen-marketplace/build.rs

fn main() {
    // Compile RDF schemas at build time
    let schema_path = "schemas/package.ttl";
    let shacl_path = "schemas/validation.shacl";

    // Validate schema files exist
    if !std::path::Path::new(schema_path).exists() {
        panic!("Schema file not found: {}", schema_path);
    }

    // Tell Cargo to rerun if schemas change
    println!("cargo:rerun-if-changed={}", schema_path);
    println!("cargo:rerun-if-changed={}", shacl_path);
}
```

## CI/CD Configuration

```yaml
# .github/workflows/marketplace-v2-ci.yml

name: Marketplace V2 CI

on: [push, pull_request]

jobs:
  test-feature-combinations:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        features:
          - "marketplace-v1"
          - "marketplace-v2"
          - "marketplace-parallel"
    steps:
      - uses: actions/checkout@v2
      - name: Test with ${{ matrix.features }}
        run: cargo test --no-default-features --features ${{ matrix.features }}

  benchmark-performance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run benchmarks
        run: cargo bench --features marketplace-parallel marketplace_comparison
```

## Summary

The code organization follows these principles:

1. **Separation of Concerns**: V1, V2, and adapter code are in separate modules
2. **Feature Gating**: Conditional compilation ensures minimal binary size
3. **Unified Interface**: All backends implement `MarketplaceBackend` trait
4. **Test Isolation**: Tests are organized by backend and feature combination
5. **Configuration Flexibility**: Runtime backend selection via config or CLI flags
6. **Documentation**: Comprehensive docs organized by audience (users, publishers, developers)
7. **CI/CD**: Automated testing of all feature combinations
