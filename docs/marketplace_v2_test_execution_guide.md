<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Marketplace V2 Test Execution Guide](#marketplace-v2-test-execution-guide)
  - [Overview](#overview)
  - [Test Suite Organization](#test-suite-organization)
    - [Directory Structure](#directory-structure)
  - [Running Tests](#running-tests)
    - [Quick Commands](#quick-commands)
    - [Feature Flag Matrix](#feature-flag-matrix)
    - [Run Examples](#run-examples)
      - [Development (Fast Feedback)](#development-fast-feedback)
      - [Pre-Commit (Comprehensive)](#pre-commit-comprehensive)
      - [CI/CD (Full Suite)](#cicd-full-suite)
  - [Test Categories Explained](#test-categories-explained)
    - [1. Unit Tests (40+ tests)](#1-unit-tests-40-tests)
    - [2. Integration Tests (50+ tests)](#2-integration-tests-50-tests)
    - [3. Performance Tests (15+ tests)](#3-performance-tests-15-tests)
    - [4. Security Tests (20+ tests)](#4-security-tests-20-tests)
  - [Test Fixtures](#test-fixtures)
    - [Package Fixtures](#package-fixtures)
  - [Interpreting Test Results](#interpreting-test-results)
    - [Success Criteria](#success-criteria)
    - [Common Failure Patterns](#common-failure-patterns)
      - [1. Feature Flag Missing](#1-feature-flag-missing)
      - [2. Compilation Error](#2-compilation-error)
      - [3. Performance SLO Violation](#3-performance-slo-violation)
      - [4. Signature Verification Failure](#4-signature-verification-failure)
  - [Coverage Analysis](#coverage-analysis)
    - [Generate Coverage Report](#generate-coverage-report)
    - [Coverage Targets](#coverage-targets)
  - [Continuous Integration](#continuous-integration)
    - [GitHub Actions Workflow](#github-actions-workflow)
  - [Troubleshooting](#troubleshooting)
    - [Tests Won't Compile](#tests-wont-compile)
    - [Tests Timeout](#tests-timeout)
    - [Tests Fail Intermittently](#tests-fail-intermittently)
  - [Success Metrics](#success-metrics)
    - [Test Execution Summary](#test-execution-summary)
    - [Quality Gates](#quality-gates)
  - [Next Steps](#next-steps)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Marketplace V2 Test Execution Guide

## Overview

This guide provides instructions for running the comprehensive marketplace v2 test suite, interpreting results, and ensuring quality standards are met.

---

## Test Suite Organization

### Directory Structure

```
crates/ggen-cli/tests/marketplace/
├── unit/                          # 40+ unit tests
│   ├── adapter_conversion_test.rs  (15 tests)
│   ├── rdf_mapping_test.rs        (14 tests)
│   ├── maturity_scoring_test.rs   (existing)
│   ├── search_ranking_test.rs     (existing)
│   └── package_filtering_test.rs  (existing)
│
├── integration/                   # 50+ integration tests
│   ├── backward_compat_test.rs    (20 tests)
│   ├── v2_workflows_test.rs       (18 tests)
│   ├── cross_backend_test.rs      (12 tests)
│   ├── cli_commands_test.rs       (existing)
│   └── edge_cases_test.rs         (existing)
│
├── performance/                   # 15+ performance tests
│   ├── latency_benchmark.rs       (15 tests)
│   └── benchmark_test.rs          (existing)
│
├── security/                      # 20+ security tests
│   ├── ed25519_signature_test.rs  (20 tests)
│   └── validation_test.rs         (existing)
│
└── fixtures/                      # Test data
    ├── package_fixtures.rs        (100+ package fixtures)
    └── mod.rs                     (helper functions)
```

**Total Test Count: ~125+ tests**

---

## Running Tests

### Quick Commands

```bash
# Run all marketplace tests
cargo test --package ggen-cli --test marketplace

# Run specific test categories
cargo test --package ggen-cli --test marketplace unit::
cargo test --package ggen-cli --test marketplace integration::
cargo test --package ggen-cli --test marketplace performance::
cargo test --package ggen-cli --test marketplace security::

# Run with specific feature flags
cargo test --package ggen-cli --features marketplace-v1
cargo test --package ggen-cli --features marketplace-v2
cargo test --package ggen-cli --features marketplace-parallel
```

### Feature Flag Matrix

| Feature Flag | Purpose | Tests Run |
|--------------|---------|-----------|
| `marketplace-v1` | V1 backend only | Backward compatibility (20 tests) |
| `marketplace-v2` | V2 RDF backend only | V2-specific (18 tests) + Security (20 tests) |
| `marketplace-parallel` | Both backends | Cross-backend comparison (12 tests) |
| `all-features` | All features | Full test suite (125+ tests) |

### Run Examples

#### Development (Fast Feedback)
```bash
# Unit tests only (fast, <5s)
cargo test --package ggen-cli --test marketplace unit::

# Specific test
cargo test --package ggen-cli --test marketplace unit::adapter_conversion_tests::test_round_trip_conversion_preserves_data
```

#### Pre-Commit (Comprehensive)
```bash
# All tests with output
cargo test --package ggen-cli --test marketplace -- --nocapture

# With test summary
cargo test --package ggen-cli --test marketplace 2>&1 | tee test-results.txt
```

#### CI/CD (Full Suite)
```bash
# All features + verbose output
cargo test --package ggen-cli --all-features --test marketplace -- --nocapture --test-threads=1

# With coverage
cargo tarpaulin --package ggen-cli --test marketplace --out Html --output-dir coverage/
```

---

## Test Categories Explained

### 1. Unit Tests (40+ tests)

**Purpose:** Validate individual components in isolation

**Key Test Files:**
- `adapter_conversion_test.rs` - V1↔V2 model conversion
- `rdf_mapping_test.rs` - Package→RDF triple mapping

**Example:**
```bash
# Run adapter conversion tests
cargo test --package ggen-cli --test marketplace unit::adapter_conversion_tests
```

**Expected Results:**
- ✅ All conversions preserve data
- ✅ Unicode handled correctly
- ✅ Round-trip conversion accurate
- ✅ Performance <1ms per conversion

---

### 2. Integration Tests (50+ tests)

**Purpose:** Validate end-to-end workflows across backends

**Key Test Files:**
- `backward_compat_test.rs` - V1 behavior unchanged
- `v2_workflows_test.rs` - V2 RDF/SPARQL workflows
- `cross_backend_test.rs` - V1 vs V2 consistency

**Example:**
```bash
# Run backward compatibility tests
cargo test --package ggen-cli --features marketplace-v1 --test marketplace integration::backward_compat_tests

# Run V2 workflow tests
cargo test --package ggen-cli --features marketplace-v2 --test marketplace integration::v2_workflows_tests

# Run cross-backend comparison
cargo test --package ggen-cli --features marketplace-parallel --test marketplace integration::cross_backend_tests
```

**Expected Results:**
- ✅ V1 commands work identically
- ✅ V2 produces same results as V1
- ✅ SPARQL queries correct
- ✅ No functional regression

---

### 3. Performance Tests (15+ tests)

**Purpose:** Validate SLO compliance and performance

**Key Test File:**
- `latency_benchmark.rs` - Latency percentiles and SLO validation

**Example:**
```bash
# Run performance benchmarks
cargo test --package ggen-cli --test marketplace performance::latency_benchmarks -- --nocapture
```

**Expected Results (SLOs):**
```
Lookup:  p50 <30ms,  p95 <100ms,  p99 <150ms ✅
Search:  p50 <80ms,  p95 <200ms,  p99 <300ms ✅
Install: p50 <200ms, p95 <500ms,  p99 <1000ms ✅
List:    p50 <50ms,  p95 <150ms,  p99 <250ms ✅

Cache hit rate: >80% ✅
Throughput: >10 ops/sec ✅
Memory: <500MB for 1000 packages ✅
```

---

### 4. Security Tests (20+ tests)

**Purpose:** Validate Ed25519 signatures and security

**Key Test File:**
- `ed25519_signature_test.rs` - Signature generation and verification

**Example:**
```bash
# Run security tests (requires marketplace-v2 feature)
cargo test --package ggen-cli --features marketplace-v2 --test marketplace security::ed25519_security_tests
```

**Expected Results:**
- ✅ Valid signatures verify correctly
- ✅ Tampered messages detected
- ✅ Invalid signatures rejected
- ✅ Key rotation handled correctly
- ✅ Supply chain attacks prevented

---

## Test Fixtures

### Package Fixtures

The test suite includes 100+ realistic package fixtures:

```rust
use ggen_cli::tests::marketplace::fixtures::package_fixtures::PackageFixtures;

let fixtures = PackageFixtures::new();

// 10 minimal packages
fixtures.minimal

// 50 standard packages (realistic)
fixtures.standard

// 20 complex packages (multi-version)
fixtures.complex

// 10 unicode packages (international)
fixtures.unicode

// 5 edge case packages (special chars, extremes)
fixtures.edge_cases

// All 95 packages
fixtures.all()
```

**Usage Example:**
```bash
# Tests using fixtures automatically load realistic data
cargo test --package ggen-cli --test marketplace unit::rdf_mapping_tests::test_rdf_performance
```

---

## Interpreting Test Results

### Success Criteria

✅ **PASS** - All tests passed
```
test result: ok. 125 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 3.21s
```

❌ **FAIL** - Some tests failed
```
test result: FAILED. 120 passed; 5 failed; 0 ignored; 0 measured; 0 filtered out; finished in 2.87s
```

### Common Failure Patterns

#### 1. Feature Flag Missing
```
error: no test target named `marketplace`
```
**Fix:** Add feature flag:
```bash
cargo test --features marketplace-v2 --test marketplace
```

#### 2. Compilation Error
```
error[E0432]: unresolved import `ggen_marketplace_v2`
```
**Fix:** Ensure dependencies are correct in `Cargo.toml`

#### 3. Performance SLO Violation
```
assertion failed: p95 < Duration::from_millis(100)
```
**Fix:** Optimize slow operation or adjust SLO

#### 4. Signature Verification Failure
```
assertion failed: is_valid, "Valid signature should verify successfully"
```
**Fix:** Check Ed25519 implementation or key handling

---

## Coverage Analysis

### Generate Coverage Report

```bash
# Install tarpaulin (one-time)
cargo install cargo-tarpaulin

# Generate HTML coverage report
cargo tarpaulin \
  --package ggen-cli \
  --test marketplace \
  --out Html \
  --output-dir coverage/ \
  --all-features

# Open coverage report
open coverage/index.html
```

### Coverage Targets

| Component | Target Coverage | Actual |
|-----------|----------------|--------|
| Adapter conversion | >95% | TBD |
| RDF mapping | >95% | TBD |
| V2 workflows | >90% | TBD |
| Security (Ed25519) | 100% | TBD |
| Overall new code | >90% | TBD |

---

## Continuous Integration

### GitHub Actions Workflow

```yaml
name: Marketplace V2 Tests

on: [push, pull_request]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo test --package ggen-cli --test marketplace unit::

  backward-compat:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo test --package ggen-cli --features marketplace-v1 --test marketplace integration::backward_compat_tests

  v2-workflows:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo test --package ggen-cli --features marketplace-v2 --test marketplace integration::v2_workflows_tests

  performance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo test --package ggen-cli --test marketplace performance::

  security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo test --package ggen-cli --features marketplace-v2 --test marketplace security::
      - run: cargo audit

  coverage:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo install cargo-tarpaulin
      - run: cargo tarpaulin --package ggen-cli --test marketplace --all-features --out Lcov
      - uses: coverallsapp/github-action@v2
```

---

## Troubleshooting

### Tests Won't Compile

**Problem:** Missing dependencies or feature flags

**Solution:**
```bash
# Check feature flags are enabled
cargo test --package ggen-cli --all-features --test marketplace

# Verify dependencies
cargo tree --package ggen-cli | grep marketplace
```

### Tests Timeout

**Problem:** Performance tests taking too long

**Solution:**
```bash
# Increase timeout
cargo test --package ggen-cli --test marketplace -- --test-threads=1 --nocapture

# Skip slow tests
cargo test --package ggen-cli --test marketplace --skip performance::
```

### Tests Fail Intermittently

**Problem:** Race conditions or timing issues

**Solution:**
```bash
# Run with single thread
cargo test --package ggen-cli --test marketplace -- --test-threads=1

# Run multiple times to identify flaky tests
for i in {1..10}; do cargo test --package ggen-cli --test marketplace || break; done
```

---

## Success Metrics

### Test Execution Summary

After running full suite, verify:

```
✅ Unit Tests:        40+ passed, 0 failed
✅ Integration Tests: 50+ passed, 0 failed
✅ Performance Tests: 15+ passed, 0 failed (all SLOs met)
✅ Security Tests:    20+ passed, 0 failed
✅ Total Coverage:    >90% for new code
✅ Execution Time:    <5 minutes for full suite
```

### Quality Gates

Before merging to main:

- [ ] All tests passing (100% pass rate)
- [ ] Coverage >90% for new code
- [ ] Performance SLOs met
- [ ] Security tests passing
- [ ] No clippy warnings
- [ ] No compilation warnings
- [ ] Documentation updated

---

## Next Steps

1. **Run Tests Locally:**
   ```bash
   cargo test --package ggen-cli --all-features --test marketplace
   ```

2. **Generate Coverage Report:**
   ```bash
   cargo tarpaulin --package ggen-cli --test marketplace --all-features --out Html
   ```

3. **Review Results:**
   - Check for failures
   - Verify coverage >90%
   - Confirm SLOs met

4. **Fix Any Issues:**
   - Update code to fix failures
   - Add tests for uncovered code
   - Optimize slow operations

5. **Commit and Push:**
   ```bash
   git add .
   git commit -m "Add comprehensive marketplace v2 test suite"
   git push
   ```

---

## References

- **Test Strategy:** `/docs/marketplace_v2_test_strategy.md`
- **SWE-Bench Methodology:** 84.8% solve rate
- **London School TDD:** Test behavior, not implementation
- **Performance SLOs:** <100ms lookup, <200ms search, >80% cache hit rate

---

**Document Version:** 1.0.0
**Last Updated:** 2025-01-18
**Owner:** QA & Testing Team
