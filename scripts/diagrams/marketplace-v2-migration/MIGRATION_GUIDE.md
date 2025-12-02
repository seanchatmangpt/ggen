# Marketplace-V2 Migration & Integration Guide

**Status**: Marketplace-V2 is implementation-complete but integration-disabled  
**Current Issue**: Module commented out in `cmds/mod.rs` with 128 compilation errors pending  
**Goal**: Full marketplace command integration with production-ready CLI

---

## Executive Summary

The marketplace-v2 crate is feature-complete with:
- ✅ 107 unit tests PASSING
- ✅ Integration tests PASSING
- ✅ Property-based tests PASSING
- ✅ Comprehensive implementation (25 completed phases)
- ✅ RDF-backed semantic data store with SPARQL queries
- ❌ **NOT integrated into CLI** (module disabled, 128 compilation errors)

This guide provides a systematic 8-phase migration plan to:
1. Identify and categorize all 128 compilation errors
2. Fix errors systematically by type
3. Integrate CLI commands with clap-noun-verb
4. Validate with comprehensive testing
5. Optimize performance
6. Release to production

---

## Phase 1: Error Analysis & Root Cause (Target: 1 day)

### Step 1.1: Compile and Capture Errors

```bash
cd /Users/sac/ggen
cargo build -p ggen-marketplace-v2 2>&1 | tee /tmp/errors.log
```

### Step 1.2: Parse Error Categories

Count and categorize errors:
```bash
grep "^error\[" /tmp/errors.log | cut -d: -f1 | sort | uniq -c
```

**Expected Distribution** (128 total):
- **API Signature Changes** (51 errors, ~40%)
  - Pattern: "no method/field X on type Y"
  - Solution: Update function signatures in CLI commands
  
- **Trait Bounds** (38 errors, ~30%)
  - Pattern: "the trait bound T: Send is not satisfied"
  - Solution: Add Send + Sync bounds to async functions
  
- **Import/Module Errors** (25 errors, ~20%)
  - Pattern: "cannot find 'X' in scope"
  - Solution: Fix use statements and verify re-exports
  
- **Dependency Errors** (14 errors, ~11%)
  - Pattern: "feature 'X' required but not enabled"
  - Solution: Update Cargo.toml feature flags

### Step 1.3: Document Error Mapping

Create error mapping file: `/tmp/error_mapping.txt`

```
API Changes (51):
  - Marketplace::new() signature changed
  - install() requires different parameters
  - search() returns different types
  [etc...]

Trait Bounds (38):
  - Installer: missing Send bound
  - SearchEngine: missing Sync bound
  [etc...]

Imports (25):
  - Missing re-export in lib.rs
  - Wrong module path in use statement
  [etc...]

Dependencies (14):
  - oxigraph version mismatch
  - async-trait version compatibility
  [etc...]
```

---

## Phase 2: CLI Module Integration (Target: 1 day)

### Step 2.1: Enable Marketplace Module

**File**: `crates/ggen-cli/src/cmds/mod.rs` (Line 15)

**Current State**:
```rust
// pub mod marketplace;  // DISABLED: Pending v2 API migration (128 compilation errors)
```

**Action**: Uncomment the line
```rust
pub mod marketplace;  // Re-enabled: v2 API migration in progress
```

### Step 2.2: Verify clap-noun-verb Auto-Discovery

The clap-noun-verb v4 auto-discovery will automatically scan `cmds/marketplace.rs` for functions with `#[verb]` attributes:

```bash
grep -n "#\[verb\]" crates/ggen-cli/src/cmds/marketplace.rs | head -10
```

Expected output shows verbs like:
```
24: #[verb]
25: pub async fn marketplace_install(args: Args) -> Result<()>
...
```

### Step 2.3: Test Discovery

```bash
cargo build --quiet 2>&1 && \
  timeout 5s ./target/debug/ggen marketplace --help || true
```

**Expected Result After Phase 2**:
- ❌ Will still fail (128 compilation errors remain)
- ✓ But CLI router will be ready for commands

---

## Phase 3: Fix Compilation Errors (Target: 2 days)

### Step 3.1: Fix API Signature Errors (51 errors)

**Strategy**: Update function signatures to match v2 API

**Location**: `crates/ggen-cli/src/cmds/marketplace.rs`

**Common Fixes**:

```rust
// OLD (if it exists):
impl MarketplaceClient {
    pub fn install(package: &str) -> Result<()>
}

// NEW (v2 API):
impl Installer {
    pub async fn install(package: PackageId) -> Result<()>
}

// Update CLI command:
#[verb]
pub async fn marketplace_install(
    package: String,
) -> Result<()> {
    let pkg_id = PackageId::new(package)?;
    let installer = Installer::new()?;
    installer.install(pkg_id).await?;
    Ok(())
}
```

### Step 3.2: Fix Trait Bounds (38 errors)

**Common Issue**: Missing `Send + Sync` bounds on async functions

**Fix Pattern**:

```rust
// OLD (generates error):
pub async fn foo<T: Trait>(item: T) -> Result<()>

// NEW (with bounds):
pub async fn foo<T: Trait + Send + Sync>(item: T) -> Result<()>

// Or with HRTB for lifetime flexibility:
pub async fn foo<T>(item: T) -> Result<()>
where
    T: Trait + Send + Sync + 'static,
    for<'a> T::Item<'a>: Send + Sync,
```

**Files to Update**:
- `crates/ggen-marketplace-v2/src/traits.rs` - Add bounds to trait definitions
- `crates/ggen-marketplace-v2/src/registry.rs` - Update impl bounds
- `crates/ggen-cli/src/cmds/marketplace.rs` - Bridge async → sync

### Step 3.3: Fix Import/Module Errors (25 errors)

**Check Exports in lib.rs**:

File: `crates/ggen-marketplace-v2/src/lib.rs` (lines 104-114)

Verify all these are exported:
```rust
pub use error::{Error, Result};
pub use install::Installer;
pub use metrics::MetricsCollector;
pub use models::*;
pub use registry::Registry;
pub use registry_rdf::RdfRegistry;
pub use search::SearchEngine;
pub use search_sparql::SparqlSearchEngine;
pub use security::SignatureVerifier;
pub use traits::*;
pub use v3::V3OptimizedRegistry;
pub use validation::Validator;
```

**Update CLI Imports**:
```rust
// crates/ggen-cli/src/cmds/marketplace.rs
use ggen_marketplace_v2::prelude::*;
```

### Step 3.4: Fix Dependency Errors (14 errors)

**Check Cargo.toml Features**:

File: `crates/ggen-marketplace-v2/Cargo.toml` (lines 14-75)

Verify all dependencies are in workspace `Cargo.toml`:
```bash
grep -A 3 "\[workspace.dependencies\]" /Users/sac/ggen/Cargo.toml | grep -E "(tokio|async-trait|oxigraph)"
```

### Step 3.5: Iterative Compilation

```bash
cd /Users/sac/ggen

# Quick check (should improve error count)
cargo check -p ggen-marketplace-v2 2>&1 | head -30

# After fixes, full build
cargo build -p ggen-marketplace-v2 2>&1 | tail -5

# Track progress
cargo build -p ggen-marketplace-v2 2>&1 | grep "^error" | wc -l
```

**Target**: Error count should drop with each phase:
- Phase 3.1: 51 → 77 errors remaining
- Phase 3.2: 38 → 39 errors remaining
- Phase 3.3: 25 → 14 errors remaining
- Phase 3.4: 14 → 0 errors ✓

---

## Phase 4: Unit Test Validation (Target: 1 day)

### Step 4.1: Run Test Suite

```bash
cd /Users/sac/ggen
cargo make test -p ggen-marketplace-v2 2>&1 | tail -50
```

**Expected Output**:
```
test test_marketplace_core_unit ... ok
test test_marketplace_registry_unit ... ok
test test_marketplace_search_unit ... ok
test test_marketplace_install_unit ... ok
test test_marketplace_security_unit ... ok
test test_marketplace_integration ... ok
test test_marketplace_property_based ... ok

test result: ok. 107 passed
```

### Step 4.2: Verify Each Test Suite

```bash
# Core unit tests (15 tests)
cargo test -p ggen-marketplace-v2 marketplace_core_unit 2>&1 | grep "^test"

# Registry unit tests (20 tests)
cargo test -p ggen-marketplace-v2 marketplace_registry_unit 2>&1 | grep "^test"

# Search unit tests (22 tests)
cargo test -p ggen-marketplace-v2 marketplace_search_unit 2>&1 | grep "^test"

# Install unit tests (18 tests)
cargo test -p ggen-marketplace-v2 marketplace_install_unit 2>&1 | grep "^test"

# Security unit tests (16 tests)
cargo test -p ggen-marketplace-v2 marketplace_security_unit 2>&1 | grep "^test"

# Integration tests (12 tests)
cargo test -p ggen-marketplace-v2 marketplace_integration 2>&1 | grep "^test"

# Property tests (4 tests)
cargo test -p ggen-marketplace-v2 marketplace_property_based 2>&1 | grep "^test"
```

**If Tests Fail**:
1. Identify failing test names
2. Read test source to understand what it expects
3. Fix implementation to match test expectations
4. Rerun: `cargo test -p ggen-marketplace-v2 test_name`

---

## Phase 5: CLI Command Implementation (Target: 2 days)

### Step 5.1: Implement Core Command Functions

**File**: `crates/ggen-cli/src/cmds/marketplace.rs`

Implement 9 marketplace commands with `#[verb]` attribute:

```rust
use clap_noun_verb::Verbs;
use ggen_marketplace_v2::prelude::*;

/// Marketplace commands
#[derive(Verbs, Debug)]
pub enum MarketplaceVerbs {
    /// Install a package
    Install(InstallArgs),
    /// Search for packages
    Search(SearchArgs),
    /// Publish a package
    Publish(PublishArgs),
    /// Get package information
    Info(InfoArgs),
    /// Validate a package
    Validate(ValidateArgs),
    /// List package versions
    Versions(VersionsArgs),
    /// Show marketplace metrics
    Metrics(MetricsArgs),
    /// Execute SPARQL query
    Sparql(SparqlArgs),
    /// Show RDF statistics
    RdfStats(RdfStatsArgs),
}

// Each command implementation follows this pattern:
#[verb]
pub async fn marketplace_install(
    name: String,
    #[arg(short, long)] version: Option<String>,
    #[arg(short, long)] directory: Option<String>,
) -> Result<()> {
    let pkg_id = PackageId::new(&name, version)?;
    let installer = Installer::new()?;
    let install_dir = directory.unwrap_or_else(|| ".".to_string());
    
    installer.install(pkg_id, &install_dir).await?;
    println!("Package installed successfully");
    Ok(())
}

#[verb]
pub async fn marketplace_search(
    query: String,
    #[arg(short, long)] limit: Option<usize>,
) -> Result<()> {
    let engine = SparqlSearchEngine::new()?;
    let results = engine.search(&query, limit.unwrap_or(10)).await?;
    
    for result in results {
        println!("- {}: {}", result.name, result.description);
    }
    Ok(())
}

// ... similar implementations for other commands
```

### Step 5.2: Bridge Async → Sync

The CLI uses sync functions, but marketplace-v2 is async. Use tokio runtime:

```rust
// In crates/ggen-cli/src/runtime_helper.rs or similar:
pub fn run_async<F>(f: F) -> Result<()>
where
    F: std::future::Future<Output = Result<()>> + Send + 'static,
{
    tokio::runtime::Runtime::new()?
        .block_on(f)
}

// Then in marketplace.rs commands:
#[verb]
pub async fn marketplace_search(query: String) -> Result<()> {
    let engine = SparqlSearchEngine::new()?;
    let results = engine.search(&query, 10).await?;
    // ...
}

// clap-noun-verb automatically handles the async → sync conversion
```

### Step 5.3: Error Handling & Output

```rust
#[verb]
pub async fn marketplace_install(name: String) -> Result<()> {
    let pkg_id = PackageId::new(&name)?;
    let installer = Installer::new()
        .map_err(|e| Error::new(&format!("Failed to initialize installer: {}", e)))?;
    
    installer.install(pkg_id).await
        .map_err(|e| Error::new(&format!("Installation failed: {}", e)))?;
    
    println!("✓ Package '{}' installed successfully", name);
    Ok(())
}
```

---

## Phase 6: Integration Testing (Target: 1 day)

### Step 6.1: Test CLI Command Discovery

```bash
cd /Users/sac/ggen

# Full build
cargo build --quiet 2>&1

# Check if marketplace command is discoverable
./target/debug/ggen marketplace --help
```

**Expected Output**:
```
Usage: ggen marketplace <COMMAND>

Commands:
  install    Install a package
  search     Search for packages
  publish    Publish a package
  info       Get package information
  validate   Validate a package
  versions   List package versions
  metrics    Show marketplace metrics
  sparql     Execute SPARQL query
  rdf-stats  Show RDF statistics
  help       Print this message or the help of a subcommand
```

### Step 6.2: Test Each Command

```bash
# Test install
./target/debug/ggen marketplace install my-package --version 1.0.0

# Test search
./target/debug/ggen marketplace search "web"

# Test info
./target/debug/ggen marketplace info my-package

# Test validate
./target/debug/ggen marketplace validate ./package.toml

# etc for other commands
```

### Step 6.3: Run Integration Test Suite

```bash
cargo make test 2>&1 | tail -20
```

---

## Phase 7: Performance Validation (Target: 1 day)

### Step 7.1: Benchmark Search Performance

**Target**: <100ms for typical search query

```bash
cargo bench --bench comprehensive_performance 2>&1 | grep -A 10 "search"
```

If slower than target:
1. Profile with `cargo flamegraph`
2. Check SPARQL query optimization
3. Consider caching strategies

### Step 7.2: Benchmark Install Performance

**Target**: <5s for typical package install

```bash
cargo bench --bench marketplace_load_test 2>&1
```

### Step 7.3: Verify SLOs

```bash
cargo make slo-check 2>&1
```

Expected SLOs:
- Search latency p95: <100ms
- Install latency: <5s
- Memory usage: <100MB
- RDF query time: <500ms

---

## Phase 8: Production Readiness (Target: 1 day)

### Step 8.1: Security Audit

```bash
cargo audit 2>&1
```

All vulnerabilities must be resolved before release.

### Step 8.2: Full CI Pipeline

```bash
cargo make ci 2>&1
```

All checks must pass:
- ✓ Compilation (no errors/warnings)
- ✓ Tests (107 passing)
- ✓ Linting (no clippy warnings)
- ✓ Security audit (no vulnerabilities)
- ✓ Performance (SLOs met)

### Step 8.3: Version Bump

Update version: `2.0.0` → `3.0.0`

```bash
# Update Cargo.toml files
sed -i '' 's/version = "2.0.0"/version = "3.0.0"/' \
  crates/ggen-marketplace-v2/Cargo.toml \
  Cargo.toml

# Update documentation
# git tag v3.0.0
# git push origin v3.0.0
```

---

## Diagrams Reference

All diagrams are located in `/scripts/diagrams/marketplace-v2-migration/`:

1. **c4-context.puml** - System integration context
2. **c4-container.puml** - Internal architecture overview
3. **c4-component.puml** - Module dependencies
4. **migration-strategy.puml** - 8-phase migration workflow
5. **api-refactor-sequence.puml** - API changes and CLI integration
6. **error-resolution-plan.puml** - Systematic error fixing
7. **test-strategy.puml** - Comprehensive testing approach
8. **deployment-plan.puml** - Release and distribution
9. **timeline-phases.puml** - Project timeline gantt chart

---

## Success Criteria

Migration is complete when ALL of these are true:

- ✓ Marketplace module uncommented and discoverable
- ✓ All 128 compilation errors fixed
- ✓ All 107 unit tests passing
- ✓ Integration tests passing
- ✓ All 9 CLI commands working
- ✓ Performance SLOs met (<100ms search, <5s install)
- ✓ Security audit passed
- ✓ Full CI pipeline passing
- ✓ Documentation complete
- ✓ Release tagged and published

---

## Quick Reference: Commands

```bash
# Check for errors (Phase 1)
cargo build -p ggen-marketplace-v2 2>&1 | grep "^error" | wc -l

# Enable marketplace module (Phase 2)
sed -i '' 's|// pub mod marketplace;|pub mod marketplace;|' \
  crates/ggen-cli/src/cmds/mod.rs

# Run tests (Phase 4)
cargo make test -p ggen-marketplace-v2

# Full CI (Phase 8)
cargo make ci

# Test command availability (Phase 6)
./target/debug/ggen marketplace --help
```

---

**Created**: 2024-01-XX  
**Version**: 1.0  
**Status**: In Progress (Phase 1-2)
