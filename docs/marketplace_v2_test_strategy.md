# Marketplace V2 Migration - Comprehensive Test Strategy

## Executive Summary

This document outlines the comprehensive testing strategy for the marketplace v2 migration, ensuring quality, performance, and backward compatibility while introducing RDF-based architecture.

**Test Coverage Goals:**
- ✅ 100% unit test pass rate
- ✅ 100% integration test pass rate
- ✅ 100% backward compatibility maintained
- ✅ Performance meets SLOs (<100ms lookup, <200ms search)
- ✅ >90% code coverage for new code
- ✅ Security validated (Ed25519 verification)

---

## 1. Test Organization Structure

```
crates/ggen-cli/tests/marketplace/
├── unit/                          # Fast, isolated component tests
│   ├── adapter_conversion_test.rs  # V1↔V2 model conversion
│   ├── rdf_mapping_test.rs        # Package→RDF triples
│   ├── sparql_query_test.rs       # SPARQL query generation
│   ├── maturity_scoring_test.rs   # Existing maturity scoring
│   ├── search_ranking_test.rs     # Existing search ranking
│   └── package_filtering_test.rs  # Existing filtering logic
│
├── integration/                   # End-to-end workflow tests
│   ├── backward_compat_test.rs    # V1 feature flag tests
│   ├── v2_workflows_test.rs       # V2 feature flag tests
│   ├── cross_backend_test.rs      # V1 vs V2 comparison
│   ├── cli_commands_test.rs       # Existing CLI tests
│   └── edge_cases_test.rs         # Existing edge case tests
│
├── performance/                   # Performance & benchmarks
│   ├── latency_benchmark.rs       # Lookup/search latency
│   ├── cache_performance_test.rs  # V3 cache hit rates
│   ├── bulk_operations_test.rs    # Bulk install/search
│   └── benchmark_test.rs          # Existing benchmarks
│
├── security/                      # Security validation
│   ├── ed25519_signature_test.rs  # Signature gen/verify
│   ├── package_integrity_test.rs  # Tamper detection
│   ├── supply_chain_test.rs       # Dependency validation
│   └── validation_test.rs         # Existing validation
│
└── fixtures/                      # Test data & helpers
    ├── package_fixtures.rs        # 100-1000 package fixtures
    ├── test_helpers.rs            # Common test utilities
    └── mod.rs                     # Fixture exports
```

---

## 2. Test Coverage Map

### 2.1 Unit Tests (Critical 20%)

| Component | Test Cases | Priority | Coverage Target |
|-----------|-----------|----------|-----------------|
| **Adapter Conversion** | 15 tests | HIGH | 100% |
| - V1→V2 model conversion | 5 | HIGH | 100% |
| - V2→V1 model conversion | 5 | HIGH | 100% |
| - Edge cases (unicode, special chars) | 5 | HIGH | 100% |
| **RDF Mapping** | 12 tests | HIGH | 100% |
| - Package metadata→RDF triples | 4 | HIGH | 100% |
| - Version history→RDF | 3 | HIGH | 100% |
| - Dependencies→RDF relationships | 3 | HIGH | 100% |
| - Round-trip conversion accuracy | 2 | HIGH | 100% |
| **SPARQL Query Generation** | 10 tests | HIGH | 95% |
| - Search query generation | 3 | HIGH | 100% |
| - Filter query generation | 3 | HIGH | 100% |
| - Sort/pagination queries | 2 | HIGH | 90% |
| - Complex multi-criteria queries | 2 | MEDIUM | 90% |

**Unit Test Total: ~40 tests**

### 2.2 Integration Tests (Critical 20%)

| Workflow | Test Cases | Priority | Coverage Target |
|----------|-----------|----------|-----------------|
| **Backward Compatibility (V1)** | 20 tests | HIGH | 100% |
| - All 7 commands work identically | 7 | HIGH | 100% |
| - Output format matches v1 | 7 | HIGH | 100% |
| - Error messages match v1 | 3 | HIGH | 100% |
| - Performance equivalent to v1 | 3 | HIGH | 100% |
| **V2-Specific Workflows** | 15 tests | HIGH | 100% |
| - RDF-based search accuracy | 5 | HIGH | 100% |
| - SPARQL query correctness | 5 | HIGH | 100% |
| - Cache behavior (v3) | 3 | HIGH | 100% |
| - Signature verification | 2 | HIGH | 100% |
| **Cross-Backend Comparison** | 12 tests | HIGH | 100% |
| - Same search → same results | 4 | HIGH | 100% |
| - Same install → same behavior | 4 | HIGH | 100% |
| - Performance comparison | 2 | MEDIUM | 100% |
| - Error consistency | 2 | MEDIUM | 100% |

**Integration Test Total: ~50 tests**

### 2.3 Performance Tests (20% for benchmarking)

| Benchmark | Target SLO | Priority | Test Cases |
|-----------|-----------|----------|------------|
| **Search Latency** | <200ms p95 | HIGH | 5 tests |
| - 100 packages | <150ms p95 | HIGH | 1 |
| - 1000 packages | <200ms p95 | HIGH | 1 |
| - Complex filters | <250ms p95 | MEDIUM | 1 |
| - p50/p95/p99 distribution | - | MEDIUM | 2 |
| **Lookup Latency** | <100ms p95 | HIGH | 4 tests |
| - Single package lookup | <50ms p95 | HIGH | 1 |
| - Version history lookup | <100ms p95 | HIGH | 1 |
| - Dependency resolution | <100ms p95 | HIGH | 1 |
| - Cache cold vs warm | - | MEDIUM | 1 |
| **Cache Performance** | >80% hit rate | HIGH | 3 tests |
| - Hot query cache hit rate | >90% | HIGH | 1 |
| - Metadata cache hit rate | >80% | HIGH | 1 |
| - Cache invalidation accuracy | 100% | HIGH | 1 |
| **Bulk Operations** | - | MEDIUM | 3 tests |
| - Parallel install throughput | >10/sec | MEDIUM | 1 |
| - Bulk search performance | <500ms | MEDIUM | 1 |
| - Memory efficiency (<500MB) | - | MEDIUM | 1 |

**Performance Test Total: ~15 tests**

### 2.4 Security Tests (Critical for supply chain)

| Security Aspect | Test Cases | Priority | Coverage Target |
|-----------------|-----------|----------|-----------------|
| **Ed25519 Signatures** | 8 tests | HIGH | 100% |
| - Signature generation | 2 | HIGH | 100% |
| - Signature verification | 2 | HIGH | 100% |
| - Invalid signature detection | 2 | HIGH | 100% |
| - Key rotation handling | 2 | MEDIUM | 100% |
| **Package Integrity** | 6 tests | HIGH | 100% |
| - Tamper detection | 3 | HIGH | 100% |
| - Checksum validation | 2 | HIGH | 100% |
| - Version consistency | 1 | MEDIUM | 100% |
| **Supply Chain Security** | 6 tests | MEDIUM | 90% |
| - Dependency validation | 3 | MEDIUM | 90% |
| - Malicious package detection | 2 | MEDIUM | 90% |
| - Vulnerability scoring impact | 1 | MEDIUM | 90% |

**Security Test Total: ~20 tests**

---

## 3. Test Data & Fixtures

### 3.1 Package Fixtures

**Fixture Categories:**
1. **Minimal Packages (10)** - Bare minimum required fields
2. **Standard Packages (50)** - Typical real-world packages
3. **Complex Packages (20)** - Multi-version, many dependencies
4. **Edge Case Packages (20)** - Unicode, special chars, extremes

**Total: 100 realistic package fixtures**

### 3.2 Test Data Characteristics

```rust
// Example fixture design
pub struct PackageFixtures {
    // Normal cases
    pub minimal: Vec<Package>,           // 10 packages
    pub standard: Vec<Package>,          // 50 packages
    pub complex: Vec<Package>,           // 20 packages

    // Edge cases
    pub unicode_names: Vec<Package>,     // 5 packages
    pub special_chars: Vec<Package>,     // 5 packages
    pub large_descriptions: Vec<Package>, // 5 packages
    pub extreme_versions: Vec<Package>,  // 5 packages

    // Invalid/malformed (for error testing)
    pub invalid_ids: Vec<String>,        // 10 invalid IDs
    pub invalid_versions: Vec<String>,   // 10 invalid versions
    pub tampered_packages: Vec<Package>, // 5 tampered packages
}
```

---

## 4. Feature Flag Test Strategy

### 4.1 V1 Only (`marketplace-v1`)

**Goal:** Ensure zero regression from current behavior

```rust
#[cfg(feature = "marketplace-v1")]
#[tokio::test]
async fn test_v1_search_exact_match() {
    // V1 backend must produce identical results
}
```

**Test Coverage:**
- ✅ All 7 commands work identically
- ✅ Output format matches exactly
- ✅ Error messages unchanged
- ✅ Performance equivalent

### 4.2 V2 Only (`marketplace-v2`)

**Goal:** Validate RDF/SPARQL implementation

```rust
#[cfg(feature = "marketplace-v2")]
#[tokio::test]
async fn test_v2_sparql_search() {
    // V2 RDF backend with SPARQL queries
}
```

**Test Coverage:**
- ✅ SPARQL query correctness
- ✅ RDF data model validation
- ✅ Cache performance (v3)
- ✅ Signature verification

### 4.3 Parallel Comparison (`marketplace-parallel`)

**Goal:** Prove v1 and v2 produce identical results

```rust
#[cfg(feature = "marketplace-parallel")]
#[tokio::test]
async fn test_parallel_search_consistency() {
    let v1_results = v1_backend.search("query").await;
    let v2_results = v2_backend.search("query").await;
    assert_eq!(v1_results, v2_results);
}
```

**Test Coverage:**
- ✅ Same inputs → same outputs
- ✅ Same errors for same failures
- ✅ Performance comparison (v1 vs v2)

---

## 5. Performance SLOs & Validation

### 5.1 Latency SLOs

| Operation | p50 | p95 | p99 | Max |
|-----------|-----|-----|-----|-----|
| Lookup | <30ms | <100ms | <150ms | <500ms |
| Search | <80ms | <200ms | <300ms | <1000ms |
| Install | <200ms | <500ms | <1000ms | <5000ms |
| List | <50ms | <150ms | <250ms | <1000ms |

### 5.2 Cache SLOs

| Cache Type | Hit Rate | Invalidation Latency |
|------------|----------|---------------------|
| Hot Query Cache | >90% | <10ms |
| Metadata Cache | >80% | <50ms |
| Search Index | >95% | <100ms |

### 5.3 Resource SLOs

| Resource | Limit | Measurement |
|----------|-------|-------------|
| Memory | <500MB | Peak RSS for 1000 packages |
| CPU | <50% | Average during bulk operations |
| Disk | <100MB | RDF store size for 1000 packages |

---

## 6. Test Execution Plan

### 6.1 Development (Fast Feedback)

```bash
# Unit tests only (fast, <5s)
cargo test --test marketplace unit::

# Integration tests (moderate, <30s)
cargo test --test marketplace integration::

# Specific feature flag
cargo test --features marketplace-v1
cargo test --features marketplace-v2
```

### 6.2 Pre-Commit (Comprehensive)

```bash
# All tests
cargo test --test marketplace

# With coverage
cargo tarpaulin --test marketplace --out Html --output-dir coverage/
```

### 6.3 CI/CD (Full Suite + Benchmarks)

```bash
# All tests with all feature combinations
cargo test --all-features --test marketplace

# Performance benchmarks
cargo bench --bench marketplace_performance

# Security audit
cargo audit
cargo clippy -- -D warnings
```

---

## 7. Success Criteria & Metrics

### 7.1 Test Pass Rate
- ✅ **100% unit tests** passing
- ✅ **100% integration tests** passing
- ✅ **100% backward compatibility** tests passing
- ✅ **All performance benchmarks** meeting SLOs
- ✅ **All security tests** passing

### 7.2 Code Coverage
- ✅ **>90% coverage** for new v2 code
- ✅ **>95% coverage** for adapter/conversion logic
- ✅ **100% coverage** for security-critical paths

### 7.3 Performance Validation
- ✅ **p95 lookup latency <100ms**
- ✅ **p95 search latency <200ms**
- ✅ **Cache hit rate >80%**
- ✅ **No memory leaks** (valgrind/miri)

### 7.4 Security Validation
- ✅ **Ed25519 signatures** verified correctly
- ✅ **Tampered packages** detected 100%
- ✅ **No SQL/XSS injection** vulnerabilities
- ✅ **Supply chain** validated

---

## 8. Test Infrastructure

### 8.1 Test Helpers

```rust
// Common test utilities
pub mod test_helpers {
    pub fn create_test_package(id: &str, name: &str, version: &str) -> Package;
    pub fn create_multi_version_package(id: &str, versions: &[&str]) -> Package;
    pub fn create_invalid_package() -> Package;
    pub fn create_tampered_package() -> Package;

    pub fn setup_test_registry() -> RdfRegistry;
    pub fn setup_v1_registry() -> V1Registry;
    pub fn setup_parallel_registries() -> (V1Registry, RdfRegistry);

    pub fn assert_packages_equal(p1: &Package, p2: &Package);
    pub fn assert_latency_within_slo(duration: Duration, slo: Duration);
}
```

### 8.2 Benchmarking Infrastructure

```rust
use criterion::{Criterion, black_box};

pub fn benchmark_search(c: &mut Criterion) {
    let registry = setup_test_registry();
    let packages = load_1000_packages();

    c.bench_function("search_1000_packages", |b| {
        b.iter(|| {
            black_box(registry.search("database").await)
        })
    });
}
```

### 8.3 Property-Based Testing

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_package_conversion_roundtrip(
        id in "[a-z0-9-]{3,20}",
        version in "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2}"
    ) {
        let v1_pkg = create_v1_package(&id, &version);
        let v2_pkg = v1_to_v2(&v1_pkg);
        let roundtrip = v2_to_v1(&v2_pkg);

        assert_eq!(v1_pkg, roundtrip);
    }
}
```

---

## 9. Continuous Testing Pipeline

### 9.1 GitHub Actions Workflow

```yaml
name: Marketplace V2 Tests

on: [push, pull_request]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: cargo test --test marketplace unit::

  integration-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: cargo test --test marketplace integration::

  backward-compat:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: cargo test --features marketplace-v1

  v2-specific:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: cargo test --features marketplace-v2

  performance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: cargo bench --bench marketplace_performance

  security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: cargo audit
      - run: cargo test --test marketplace security::
```

---

## 10. Test Documentation Standards

### 10.1 Test Naming Convention

```rust
// Pattern: test_{component}_{scenario}_{expected_behavior}
#[test]
fn test_adapter_unicode_package_preserves_data() { }

#[test]
fn test_sparql_search_complex_filters_returns_accurate_results() { }

#[test]
fn test_ed25519_invalid_signature_detection_prevents_tampered_install() { }
```

### 10.2 Test Documentation

```rust
/// Test that V1→V2 adapter conversion preserves all package metadata
///
/// **Scenario:** Convert a package with unicode characters in name/description
/// **Expected:** All metadata preserved byte-for-byte, no data loss
/// **SLO:** Conversion <1ms
#[test]
fn test_adapter_unicode_package_preserves_data() {
    // Test implementation
}
```

---

## 11. Risk Mitigation

### 11.1 High-Risk Areas

| Risk | Mitigation | Test Coverage |
|------|-----------|---------------|
| **Backward incompatibility** | Parallel comparison tests | 100% |
| **Performance regression** | Continuous benchmarking | 100% |
| **Security vulnerabilities** | Security-specific test suite | 100% |
| **Data corruption** | Round-trip conversion tests | 100% |
| **Cache inconsistency** | Cache invalidation tests | 100% |

### 11.2 Rollback Plan

If tests fail:
1. **Unit test failure** → Fix immediately, block PR
2. **Integration failure** → Rollback feature flag, investigate
3. **Performance regression** → Profile, optimize, re-test
4. **Security failure** → Block release, security review

---

## 12. Timeline & Milestones

| Milestone | Tests | Target Date | Status |
|-----------|-------|-------------|--------|
| **Phase 1: Unit Tests** | 40 tests | Week 1 | ⏳ In Progress |
| **Phase 2: Integration Tests** | 50 tests | Week 2 | ⏳ Pending |
| **Phase 3: Performance Tests** | 15 tests | Week 3 | ⏳ Pending |
| **Phase 4: Security Tests** | 20 tests | Week 3 | ⏳ Pending |
| **Phase 5: CI/CD Integration** | All tests | Week 4 | ⏳ Pending |

**Total Test Count: ~125 tests**

---

## 13. References

- **SWE-Bench Methodology:** 84.8% solve rate with comprehensive testing
- **London School TDD:** Test behavior, not implementation
- **Property-Based Testing:** Use proptest for exhaustive coverage
- **Performance Testing:** Criterion for statistical benchmarking

---

**Document Version:** 1.0.0
**Last Updated:** 2025-01-18
**Owner:** QA & Testing Team
