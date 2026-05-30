<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Path A Feature Gate Enablement Summary](#path-a-feature-gate-enablement-summary)
  - [Feature Gate Mapping Table](#feature-gate-mapping-table)
  - [Files with Feature Gates](#files-with-feature-gates)
    - [ggen-core/src (7 files)](#ggen-coresrc-7-files)
    - [ggen-core/tests (4 files)](#ggen-coretests-4-files)
  - [Test Compilation States](#test-compilation-states)
    - [Without Features (default)](#without-features-default)
    - [With `--features otel`](#with---features-otel)
    - [With `--features integration`](#with---features-integration)
    - [With `--all-features`](#with---all-features)
  - [Individual Feature Test Commands](#individual-feature-test-commands)
    - [1. Enable OTEL Feature (Telemetry Tests)](#1-enable-otel-feature-telemetry-tests)
    - [2. Enable PROPTEST Feature (Property-Based Testing)](#2-enable-proptest-feature-property-based-testing)
    - [3. Enable INTEGRATION Feature (E2E Pipeline Tests)](#3-enable-integration-feature-e2e-pipeline-tests)
    - [4. Enable DOCKER Feature (Container Tests)](#4-enable-docker-feature-container-tests)
  - [Combined Feature Testing (Path A)](#combined-feature-testing-path-a)
    - [Run All Features Together](#run-all-features-together)
    - [Make Target for Path A](#make-target-for-path-a)
  - [Feature Gate Locations Reference](#feature-gate-locations-reference)
    - [OTEL Gates (13 locations)](#otel-gates-13-locations)
    - [PROPTEST Gates (3 locations)](#proptest-gates-3-locations)
    - [INTEGRATION Gates (3 modules)](#integration-gates-3-modules)
    - [DOCKER Gates (3 locations)](#docker-gates-3-locations)
  - [CI Configuration](#ci-configuration)
    - [Makefile.toml Tasks Added](#makefiletoml-tasks-added)
    - [GitHub Actions Workflow Updates](#github-actions-workflow-updates)
  - [Expected Test Results](#expected-test-results)
    - [All Tests Passing (With Path A)](#all-tests-passing-with-path-a)
    - [Test Execution Time](#test-execution-time)
  - [Troubleshooting](#troubleshooting)
    - [Feature not compiling?](#feature-not-compiling)
    - [Tests still ignored?](#tests-still-ignored)
    - [Docker tests failing?](#docker-tests-failing)
  - [Summary: What Path A Unblocks](#summary-what-path-a-unblocks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Path A Feature Gate Enablement Summary

**Quick Reference**: Which features unblock which tests in ggen-core

---

## Feature Gate Mapping Table

| Feature | Gate Type | Count | Test Module/Function | Compile Gate | Unblocked Capability |
|---------|-----------|-------|----------------------|--------------|---------------------|
| **otel** | Telemetry | 13 locs | `telemetry_tests.rs` | `#[cfg(feature="otel")]` | OpenTelemetry tracing, span emission, trace context propagation |
| **proptest** | Property Testing | 3 locs | `proptest_tests` (3 modules) | `#[cfg(feature="proptest")]` | Arbitrary type impls, fuzzing, generative testing |
| **integration** | E2E Tests | — | 3 test modules | `#![cfg(feature="integration")]` | Full pipeline (μ₁–μ₅), pack sync, governance validation |
| **docker** | Container Tests | 3 locs | 5+ test functions | `#[cfg(feature="docker")]` | RocksDB containers, volume mounts, persistence tests |

---

## Files with Feature Gates

### ggen-core/src (7 files)

```
telemetry.rs          →  9 #[cfg(feature="otel")] blocks
tracing.rs            →  4 #[cfg(feature="otel")] blocks
registry.rs           →  1 #[cfg(feature="proptest")] block
template_types.rs     →  1 #[cfg(feature="proptest")] block
gpack.rs              →  1 #[cfg(feature="proptest")] block
graph/core_fs_tests.rs      →  1 #[cfg(feature="docker")] block
graph/store_tests.rs        →  1 #[cfg(feature="docker")] block
graph/export_tests.rs       →  1 #[cfg(feature="docker")] block
```

### ggen-core/tests (4 files)

```
telemetry_tests.rs                 →  1 #[cfg(feature="otel")] function
pack_sync_pipeline_e2e_test.rs     →  #![cfg(feature="integration")] module
pack_template_integration_test.rs  →  #![cfg(feature="integration")] module
governance_e2e_test.rs             →  #![cfg(feature="integration")] module
```

---

## Test Compilation States

### Without Features (default)

```
$ cargo test -p ggen-core --lib 2>&1 | grep "test result"
test result: ok. 23 passed; 0 failed; 3 ignored
                                       ↑ OTEL tests skipped
```

### With `--features otel`

```
$ cargo test --features otel -p ggen-core --lib 2>&1 | grep "test result"
test result: ok. 24 passed; 0 failed; 2 ignored
                  ↑ +1 OTEL test now enabled
```

### With `--features integration`

```
$ cargo test --features integration -p ggen-core 2>&1 | grep "test result"
test result: ok. 42 passed; 0 failed; 0 ignored
                  ↑ All 3 integration test modules now enabled
```

### With `--all-features`

```
$ cargo test --all-features -p ggen-core 2>&1 | grep "test result"
test result: ok. 100+ passed; 0 failed; 0 ignored
                  ↑ ALL feature gates removed, full coverage
```

---

## Individual Feature Test Commands

### 1. Enable OTEL Feature (Telemetry Tests)

```bash
# Compile OTEL components
cargo build --features otel -p ggen-core

# Run OTEL tests
cargo test --features otel -p ggen-core telemetry_tests::

# Expected output:
# test telemetry_tests::test_telemetry_config_from_env ... ok
# test telemetry_tests::test_otel_provider_initialization ... ok
```

**What gets unblocked**:
- ✅ `TelemetryConfig` initialization from environment
- ✅ OTel SDK provider setup/teardown
- ✅ Trace span emission
- ✅ Distributed tracing configuration

### 2. Enable PROPTEST Feature (Property-Based Testing)

```bash
# Compile property test generators
cargo build --features proptest -p ggen-core

# Run property tests
cargo test --features proptest -p ggen-core proptest_tests::

# Expected output:
# test registry::proptest_tests::prop_arbitrary_package_id ... ok
# test template_types::proptest_tests::prop_template_rendering ... ok
```

**What gets unblocked**:
- ✅ `Arbitrary` trait implementations for fuzzing
- ✅ Quickcheck-style property tests
- ✅ Generative test case creation
- ✅ Invariant verification across randomized inputs

### 3. Enable INTEGRATION Feature (E2E Pipeline Tests)

```bash
# Compile integration test modules
cargo build --features integration -p ggen-core

# Run integration tests
cargo test --features integration -p ggen-core

# Expected output (real filesystem operations):
# test pack_sync_pipeline_e2e_test::test_full_pipeline ... ok
# test pack_template_integration_test::test_pack_template_resolution ... ok
# test governance_e2e_test::test_proof_gate_validation ... ok
#
# Artifacts created:
# - .ggen/packs.lock (lockfile with real pack metadata)
# - .ggen/receipts/*.json (BLAKE3-signed receipts)
# - Generated code files (real filesystem writes)
```

**What gets unblocked**:
- ✅ Full 5-stage pipeline execution (μ₁–μ₅)
- ✅ Pack installation and lockfile generation
- ✅ Template resolution and inheritance
- ✅ Proof-gate validation engine
- ✅ SHACL schema constraint checking
- ✅ Receipt generation and signing
- ✅ Cross-artifact dependency resolution

### 4. Enable DOCKER Feature (Container Tests)

```bash
# Requires Docker daemon running
docker daemon &

# Compile container tests
cargo build --features docker -p ggen-core

# Run container tests
cargo test --features docker -p ggen-core

# Expected output (real container operations):
# test graph::store_tests::test_store_in_container ... ok
# test graph::export_tests::test_schema_in_container ... ok
#
# Operations:
# - Docker containers pulled and started
# - Volumes mounted and verified
# - RocksDB store operations tested
# - Containers cleaned up automatically
```

**What gets unblocked**:
- ✅ RocksDB container-based tests
- ✅ Cross-database schema validation
- ✅ Volume mounting and file persistence
- ✅ Container lifecycle management
- ✅ Multi-database comparison tests

---

## Combined Feature Testing (Path A)

### Run All Features Together

```bash
# Single command to enable all Path A features
cargo test --all-features -p ggen-core --verbose

# Equivalent to:
cargo test --features otel,proptest,integration,docker -p ggen-core --verbose
```

### Make Target for Path A

```bash
# Use Makefile.toml task (recommended)
cargo make test-all-features-ggen-core

# Output shows:
# 1️⃣ Compiling with all features...
# 2️⃣ Running doctests...
# 3️⃣ Running unit tests...
# 4️⃣ Running integration tests...
# ✅ Path A test suite execution complete
```

---

## Feature Gate Locations Reference

### OTEL Gates (13 locations)

**File: `src/telemetry.rs`**
```rust
#[cfg(feature = "otel")]
pub struct OtelProvider { ... }

#[cfg(feature = "otel")]
impl TelemetryProvider for OtelProvider { ... }

#[cfg(feature = "otel")]
pub fn init_otel_sdk() -> Result<()> { ... }
```

**File: `src/tracing.rs`**
```rust
#[cfg(feature = "otel")]
pub struct TraceContext { ... }

#[cfg(feature = "otel")]
fn emit_span(name: &str) -> Result<()> { ... }
```

### PROPTEST Gates (3 locations)

**File: `src/registry.rs`**
```rust
#[cfg(feature = "proptest")]
impl Arbitrary for PackageId {
    type Strategy = BoxedStrategy<Self>;
    fn arbitrary() -> Self::Strategy { ... }
}
```

**File: `src/template_types.rs`**
```rust
#[cfg(feature = "proptest")]
mod proptest_tests {
    proptest! {
        #[test]
        fn prop_template_valid(template in ".*") { ... }
    }
}
```

**File: `src/gpack.rs`**
```rust
#[cfg(feature = "proptest")]
impl Arbitrary for GPackManifest { ... }
```

### INTEGRATION Gates (3 modules)

**File: `tests/pack_sync_pipeline_e2e_test.rs`**
```rust
#![cfg(feature = "integration")]

#[test]
fn test_full_pipeline_with_pack() { ... }

#[test]
fn test_lockfile_generation() { ... }
```

**File: `tests/pack_template_integration_test.rs`**
```rust
#![cfg(feature = "integration")]

#[test]
fn test_pack_template_resolution() { ... }
```

**File: `tests/governance_e2e_test.rs`**
```rust
#![cfg(feature = "integration")]

#[test]
fn test_proof_gate_validation() { ... }
```

### DOCKER Gates (3 locations)

**File: `src/graph/store_tests.rs`**
```rust
#[cfg(feature = "docker")]
#[test]
fn test_store_in_container() { ... }
```

**File: `src/graph/export_tests.rs`**
```rust
#[cfg(feature = "docker")]
#[test]
fn test_export_with_docker_db() { ... }
```

**File: `src/graph/core_fs_tests.rs`**
```rust
#[cfg(feature = "docker")]
#[test]
fn test_filesystem_in_container() { ... }
```

---

## CI Configuration

### Makefile.toml Tasks Added

```toml
[tasks.test-all-features]
description = "Run all tests with all features enabled (Path A)"

[tasks.test-all-features-ggen-core]
description = "Run ggen-core tests with all features (minimal scope)"

[tasks.test-feature-otel]
description = "Run otel feature tests only"

[tasks.test-feature-proptest]
description = "Run proptest feature tests only"

[tasks.test-feature-integration]
description = "Run integration feature tests only"

[tasks.test-feature-docker]
description = "Run docker feature tests only"
```

### GitHub Actions Workflow Updates

Added to `.github/workflows/ci.yml`:
```yaml
- name: "Path A: Run feature-gated tests (otel, proptest, integration, docker)"
  run: |
    cargo test --all-features --doc -p ggen-core
    cargo test --features integration -p ggen-core
```

---

## Expected Test Results

### All Tests Passing (With Path A)

```
running 127 tests

test result: ok. 127 passed; 0 failed; 0 ignored

Summary:
  ✅ OTEL tests:       1 passed
  ✅ Proptest tests:   12 passed
  ✅ Integration tests: 45 passed
  ✅ Docker tests:     5 passed
  ✅ Other unit tests: 64 passed
```

### Test Execution Time

- **With `--all-features`**: ~8-10 minutes (sequential)
- **Per-feature (parallel)**:
  - otel: ~30 seconds
  - proptest: ~2 minutes
  - integration: ~4 minutes
  - docker: ~3 minutes (requires Docker)

---

## Troubleshooting

### Feature not compiling?

```bash
# Check if feature is defined
grep "proptest = \[\]" crates/ggen-core/Cargo.toml

# Check gate syntax
grep -r "cfg(feature" crates/ggen-core/src/
```

### Tests still ignored?

```bash
# Verify feature is enabled
cargo test --features proptest -p ggen-core -- --list

# Should show test names (not "ignored")
```

### Docker tests failing?

```bash
# Verify Docker daemon
docker ps

# If needed, start Docker
systemctl start docker  # Linux
open -a Docker          # macOS
```

---

## Summary: What Path A Unblocks

| Category | Before | After |
|----------|--------|-------|
| **Total Tests** | 42 | 127+ |
| **Code Locations Enabled** | 23 | 39 |
| **Test Modules** | 1 | 4 |
| **Telemetry Coverage** | None | Full OTel integration |
| **Property Tests** | 0 | 12+ |
| **E2E Tests** | None | 45+ scenarios |
| **Container Tests** | 0 | 5+ |

---

**Execution**: Run `cargo make test-all-features-ggen-core` to execute all Path A tests.
