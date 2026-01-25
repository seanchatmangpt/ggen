<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Feature Gate Strategy - Marketplace V2 Migration](#feature-gate-strategy---marketplace-v2-migration)
  - [Overview](#overview)
  - [Feature Flag Design](#feature-flag-design)
    - [Cargo Features Hierarchy](#cargo-features-hierarchy)
    - [Conditional Compilation Strategy](#conditional-compilation-strategy)
      - [1. Backend Module Structure](#1-backend-module-structure)
      - [2. Trait-Based Backend Selection](#2-trait-based-backend-selection)
      - [3. Runtime Backend Selection](#3-runtime-backend-selection)
  - [Cargo.toml Changes](#cargotoml-changes)
    - [Root Workspace Cargo.toml](#root-workspace-cargotoml)
    - [ggen-core/Cargo.toml](#ggen-corecargotoml)
    - [ggen-cli/Cargo.toml](#ggen-clicargotoml)
  - [Feature Flag Testing Matrix](#feature-flag-testing-matrix)
  - [Build Commands](#build-commands)
  - [Configuration File](#configuration-file)
  - [Environment Variables](#environment-variables)
  - [Migration Phases and Feature Flags](#migration-phases-and-feature-flags)
    - [Phase 1: Feature Gates (Week 1)](#phase-1-feature-gates-week-1)
    - [Phase 2: Adapter Layer (Week 1)](#phase-2-adapter-layer-week-1)
    - [Phase 3: Search Migration (Week 2)](#phase-3-search-migration-week-2)
    - [Phase 4: Registry/Installation (Week 3)](#phase-4-registryinstallation-week-3)
    - [Phase 5: Publishing with Signing (Week 4)](#phase-5-publishing-with-signing-week-4)
    - [Phase 6: Cutover (Week 5)](#phase-6-cutover-week-5)
  - [Rollback Strategy](#rollback-strategy)
  - [Success Metrics](#success-metrics)
  - [Risk Mitigation](#risk-mitigation)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Feature Gate Strategy - Marketplace V2 Migration

## Overview

Feature gates enable gradual migration from marketplace-v1 to marketplace-v2 with zero breaking changes and full backward compatibility.

## Feature Flag Design

### Cargo Features Hierarchy

```toml
[features]
# Default: v1 backend (current production behavior)
default = ["marketplace-v1"]

# Feature flags for backend selection
marketplace-v1 = []           # Current tantivy-based marketplace (default)
marketplace-v2 = [            # Next-gen RDF-based marketplace
    "ggen-marketplace-v2",
    "rdf-backend",
    "crypto-signing"
]
marketplace-parallel = [      # Both backends enabled for A/B testing
    "marketplace-v1",
    "marketplace-v2",
    "dual-backend"
]

# Sub-features for v2 functionality
rdf-backend = []              # RDF/SPARQL search engine
crypto-signing = []           # Ed25519 cryptographic package signing
dual-backend = []             # Adapter layer for backend switching
```

### Conditional Compilation Strategy

#### 1. Backend Module Structure

```rust
// ggen-core/src/marketplace/mod.rs

#[cfg(feature = "marketplace-v1")]
pub mod v1;

#[cfg(feature = "marketplace-v2")]
pub mod v2;

#[cfg(feature = "marketplace-parallel")]
pub mod adapter;

// Re-export active backend
#[cfg(all(feature = "marketplace-v1", not(feature = "marketplace-parallel")))]
pub use v1::*;

#[cfg(all(feature = "marketplace-v2", not(feature = "marketplace-parallel")))]
pub use v2::*;

#[cfg(feature = "marketplace-parallel")]
pub use adapter::*;
```

#### 2. Trait-Based Backend Selection

```rust
// ggen-domain/src/marketplace/backend.rs

use async_trait::async_trait;

#[async_trait]
pub trait MarketplaceBackend: Send + Sync {
    async fn search(&self, query: &SearchQuery) -> Result<Vec<Package>>;
    async fn get_package(&self, id: &PackageId) -> Result<Package>;
    async fn install_package(&self, id: &PackageId, target: &Path) -> Result<()>;
    async fn publish_package(&self, pkg: &Package, manifest: &Manifest) -> Result<()>;
}

// V1 implementation
#[cfg(feature = "marketplace-v1")]
pub struct TantivyBackend {
    index: tantivy::Index,
    // ... existing v1 fields
}

// V2 implementation
#[cfg(feature = "marketplace-v2")]
pub struct RdfBackend {
    store: oxigraph::Store,
    sparql_engine: SparqlSearchEngine,
    crypto: Ed25519Signer,
}

// Adapter for dual-backend mode
#[cfg(feature = "marketplace-parallel")]
pub struct DualBackend {
    v1: TantivyBackend,
    v2: RdfBackend,
    strategy: BackendStrategy,
}
```

#### 3. Runtime Backend Selection

```rust
// ggen-cli/src/config.rs

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarketplaceConfig {
    /// Backend selection (only used when marketplace-parallel is enabled)
    #[serde(default = "default_backend")]
    pub backend: MarketplaceBackendType,

    /// Enable A/B testing (requires marketplace-parallel)
    #[serde(default)]
    pub enable_ab_testing: bool,

    /// Percentage of operations to route to v2 (0-100)
    #[serde(default = "default_v2_percentage")]
    pub v2_percentage: u8,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum MarketplaceBackendType {
    V1,      // Use tantivy backend
    V2,      // Use RDF backend
    Auto,    // Automatic based on feature flags
    ABTest,  // A/B testing mode
}

fn default_backend() -> MarketplaceBackendType {
    MarketplaceBackendType::Auto
}

fn default_v2_percentage() -> u8 {
    10  // Start with 10% traffic to v2
}
```

## Cargo.toml Changes

### Root Workspace Cargo.toml

```toml
[workspace]
members = [
    "crates/ggen-core",
    "crates/ggen-cli",
    "crates/ggen-domain",
    "crates/ggen-marketplace",      # V1 backend
    "crates/ggen-marketplace-v2",   # V2 backend
    # ... other crates
]

[workspace.dependencies]
ggen-marketplace = { path = "crates/ggen-marketplace", version = "3.2.0" }
ggen-marketplace-v2 = { path = "crates/ggen-marketplace-v2", version = "3.0.0" }
```

### ggen-core/Cargo.toml

```toml
[dependencies]
# V1 marketplace (default)
ggen-marketplace = { workspace = true, optional = true }

# V2 marketplace (opt-in)
ggen-marketplace-v2 = { workspace = true, optional = true }

# ... existing dependencies

[features]
default = ["marketplace-v1", "dx"]

# Backend selection features
marketplace-v1 = ["ggen-marketplace"]
marketplace-v2 = ["ggen-marketplace-v2", "rdf-backend", "crypto-signing"]
marketplace-parallel = ["marketplace-v1", "marketplace-v2", "dual-backend"]

# Sub-features
rdf-backend = []
crypto-signing = []
dual-backend = []

# Existing features
dx = []
live-llm-tests = []
proptest = []
docker = []
testcontainers = []
```

### ggen-cli/Cargo.toml

```toml
[dependencies]
ggen-core = { path = "../ggen-core", version = "3.2.0" }

# Optional marketplace backends
ggen-marketplace = { workspace = true, optional = true }
ggen-marketplace-v2 = { workspace = true, optional = true }

# ... existing dependencies

[features]
default = ["marketplace-v1"]

# Inherit from ggen-core
marketplace-v1 = ["ggen-core/marketplace-v1", "ggen-marketplace"]
marketplace-v2 = ["ggen-core/marketplace-v2", "ggen-marketplace-v2"]
marketplace-parallel = ["ggen-core/marketplace-parallel", "ggen-marketplace", "ggen-marketplace-v2"]
```

## Feature Flag Testing Matrix

| Feature Flag Combination | Backend Active | Use Case |
|-------------------------|----------------|----------|
| `default` | V1 only | Current production (baseline) |
| `marketplace-v1` | V1 only | Explicit V1 selection |
| `marketplace-v2` | V2 only | Pure V2 mode (future) |
| `marketplace-parallel` | Both (adapter) | A/B testing, gradual migration |
| `marketplace-parallel,marketplace-v2` | Prefer V2 | V2-first with V1 fallback |

## Build Commands

```bash
# Default build (v1 only)
cargo build

# V2-only build
cargo build --no-default-features --features marketplace-v2

# Dual-backend build (A/B testing)
cargo build --features marketplace-parallel

# Test all feature combinations
cargo test --all-features
cargo test --no-default-features --features marketplace-v1
cargo test --no-default-features --features marketplace-v2
cargo test --features marketplace-parallel
```

## Configuration File

```yaml
# ~/.config/ggen/config.yaml

marketplace:
  # Backend selection (v1, v2, auto, ab-test)
  backend: auto

  # A/B testing configuration
  enable_ab_testing: false
  v2_percentage: 10  # Route 10% of operations to v2

  # Fallback configuration
  fallback_on_error: true  # Fall back to v1 if v2 fails

  # Performance monitoring
  log_backend_selection: true
  track_performance_metrics: true
```

## Environment Variables

```bash
# Override backend selection at runtime
export GGEN_MARKETPLACE_BACKEND=v2

# Enable A/B testing
export GGEN_MARKETPLACE_AB_TEST=true
export GGEN_MARKETPLACE_V2_PERCENTAGE=25

# Enable debug logging for backend selection
export GGEN_MARKETPLACE_DEBUG=true
```

## Migration Phases and Feature Flags

### Phase 1: Feature Gates (Week 1)
- **Flags**: `marketplace-v1` (default), `marketplace-v2` (opt-in)
- **Risk**: Low (no code changes, just infrastructure)
- **Validation**: CI tests all feature combinations

### Phase 2: Adapter Layer (Week 1)
- **Flags**: `marketplace-parallel` (enables dual backend)
- **Risk**: Low (v1 still default, v2 only when explicitly enabled)
- **Validation**: Adapter passes all v1 tests

### Phase 3: Search Migration (Week 2)
- **Flags**: `marketplace-parallel` + `backend: ab-test` in config
- **Risk**: Medium (search results may differ)
- **Validation**: A/B test with 10% v2 traffic, compare results

### Phase 4: Registry/Installation (Week 3)
- **Flags**: `marketplace-parallel` + increase v2_percentage to 50%
- **Risk**: Medium (installation paths may differ)
- **Validation**: Monitor error rates, fallback to v1 on failure

### Phase 5: Publishing with Signing (Week 4)
- **Flags**: `marketplace-v2` becomes default, `marketplace-v1` deprecated
- **Risk**: High (new cryptographic features)
- **Validation**: Full regression testing, key management validation

### Phase 6: Cutover (Week 5)
- **Flags**: Remove `marketplace-v1`, `marketplace-v2` becomes `default`
- **Risk**: Low (proven stable in prior phases)
- **Validation**: Final regression suite, performance benchmarks

## Rollback Strategy

```rust
// Automatic rollback on error
impl DualBackend {
    async fn search_with_fallback(&self, query: &SearchQuery) -> Result<Vec<Package>> {
        match self.strategy {
            BackendStrategy::V2WithFallback => {
                match self.v2.search(query).await {
                    Ok(results) => {
                        tracing::info!("V2 search succeeded");
                        Ok(results)
                    }
                    Err(e) => {
                        tracing::warn!("V2 search failed, falling back to V1: {}", e);
                        self.v1.search(query).await
                    }
                }
            }
            BackendStrategy::V1 => self.v1.search(query).await,
            BackendStrategy::V2 => self.v2.search(query).await,
        }
    }
}
```

## Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| Feature flag coverage | 100% | All backends testable via flags |
| Build time impact | <5% increase | CI build time comparison |
| Zero breaking changes | 100% | All existing tests pass with default flags |
| A/B test readiness | Week 2 | Dual backend functional |
| Rollback time | <1 hour | Config change + restart |

## Risk Mitigation

1. **Compilation failures**: CI tests all feature combinations
2. **Runtime failures**: Automatic fallback to v1 in dual-backend mode
3. **Performance regression**: A/B testing with gradual rollout (10% → 50% → 100%)
4. **Data loss**: V1 remains active until v2 proven stable
5. **User impact**: Default behavior unchanged, v2 is opt-in

## Next Steps

1. Implement feature gates in Cargo.toml (Phase 1)
2. Create `MarketplaceBackend` trait (Phase 2)
3. Implement `DualBackend` adapter (Phase 2)
4. Add configuration file support (Phase 2)
5. Write feature flag tests (Phase 1)
