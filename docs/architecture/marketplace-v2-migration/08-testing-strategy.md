# Comprehensive Testing Strategy for Migration

## Overview

The testing strategy ensures zero breaking changes, validates all feature combinations, and proves marketplace-v2 meets performance and security requirements.

## Test Pyramid Structure

```
                    ┌──────────────┐
                    │   E2E Tests  │ 5%
                    │  (20 tests)  │
                    └──────────────┘
                 ┌───────────────────┐
                 │Integration Tests  │ 15%
                 │   (80 tests)      │
                 └───────────────────┘
              ┌─────────────────────────┐
              │  Component Tests        │ 30%
              │   (200 tests)           │
              └─────────────────────────┘
          ┌────────────────────────────────┐
          │   Unit Tests                    │ 50%
          │    (400 tests)                  │
          └────────────────────────────────┘
```

## Test Categories

### 1. Unit Tests (400 tests, 50%)

#### Conversion Tests (80 tests)

```rust
// ggen-domain/tests/conversion_unit_tests.rs

mod v1_to_unified_tests {
    use ggen_domain::marketplace::conversion::*;

    #[test]
    fn test_basic_package_conversion() {
        let v1_pkg = create_test_v1_package();
        let unified: UnifiedPackage = v1_pkg.into();

        assert_eq!(unified.name, "test-package");
        assert_eq!(unified.version, "1.0.0");
        assert_eq!(unified.source_backend, BackendType::V1);
    }

    #[test]
    fn test_dependency_conversion() {
        let v1_dep = V1Dependency {
            name: "dep-pkg".to_string(),
            version: "^2.0.0".to_string(),
            optional: false,
        };

        let unified: UnifiedDependency = v1_dep.into();

        assert_eq!(unified.name, "dep-pkg");
        assert_eq!(unified.version, "^2.0.0");
        assert!(!unified.optional);
    }

    #[test]
    fn test_package_id_conversion() {
        let v1_id = "package@1.0.0";
        let pkg_id = PackageId::from_v1(v1_id);

        assert_eq!(pkg_id.to_v1(), v1_id);
    }
}

mod v2_to_unified_tests {
    #[test]
    fn test_rdf_package_conversion() {
        let v2_pkg = create_test_v2_package();
        let unified: UnifiedPackage = v2_pkg.into();

        assert_eq!(unified.source_backend, BackendType::V2);
        assert!(unified.rdf_graph_uri.is_some());
        assert!(unified.cryptographic_hash.is_some());
    }

    #[test]
    fn test_signature_preservation() {
        let v2_pkg = create_signed_v2_package();
        let unified: UnifiedPackage = v2_pkg.into();

        assert!(unified.signature.is_some());
        assert!(unified.signature_public_key.is_some());
    }
}

mod roundtrip_tests {
    #[test]
    fn test_v1_roundtrip() {
        let original = create_test_v1_package();
        let unified: UnifiedPackage = original.clone().into();
        let converted: V1Package = unified.try_into().unwrap();

        assert_packages_equal(&original, &converted);
    }

    #[test]
    fn test_v2_roundtrip() {
        let original = create_test_v2_package();
        let unified: UnifiedPackage = original.clone().into();
        let converted: V2Package = unified.try_into().unwrap();

        assert_v2_packages_equal(&original, &converted);
    }
}
```

#### Backend Adapter Tests (100 tests)

```rust
// ggen-domain/tests/backend_adapter_unit_tests.rs

mod v1_adapter_tests {
    #[tokio::test]
    async fn test_search() {
        let adapter = V1Adapter::new(test_config()).unwrap();
        let results = adapter.search(&test_query()).await.unwrap();

        assert!(!results.packages.is_empty());
        assert_eq!(results.backend_used, BackendType::V1);
    }

    #[tokio::test]
    async fn test_get_package() {
        let adapter = V1Adapter::new(test_config()).unwrap();
        let pkg = adapter.get_package(&test_package_id()).await.unwrap();

        assert_eq!(pkg.summary.name, "test-package");
    }

    #[tokio::test]
    async fn test_install() {
        let adapter = V1Adapter::new(test_config()).unwrap();
        let result = adapter.install_package(&test_install_request()).await.unwrap();

        assert!(result.installed_path.exists());
        assert_eq!(result.backend_used, BackendType::V1);
    }
}

mod v2_adapter_tests {
    #[tokio::test]
    async fn test_sparql_search() {
        let adapter = V2Adapter::new(test_config()).unwrap();
        let results = adapter.search(&test_query()).await.unwrap();

        assert!(!results.packages.is_empty());
        assert_eq!(results.backend_used, BackendType::V2);
    }

    #[tokio::test]
    async fn test_signature_verification() {
        let adapter = V2Adapter::new(test_config()).unwrap();
        let result = adapter.install_package(&test_signed_package_request()).await.unwrap();

        assert_eq!(result.cryptographic_verification, Some(true));
    }
}

mod dual_adapter_tests {
    #[tokio::test]
    async fn test_v2_with_fallback() {
        let adapter = DualBackendAdapter::new(
            v1_config(),
            v2_config(),
            BackendStrategy::V2WithFallback,
        ).unwrap();

        // V2 should succeed
        let result = adapter.search(&test_query()).await.unwrap();
        assert_eq!(result.backend_used, BackendType::V2);
    }

    #[tokio::test]
    async fn test_fallback_on_v2_failure() {
        let adapter = create_adapter_with_broken_v2();

        // Should fall back to V1
        let result = adapter.search(&test_query()).await.unwrap();
        assert_eq!(result.backend_used, BackendType::V1);
    }

    #[tokio::test]
    async fn test_ab_test_distribution() {
        let adapter = DualBackendAdapter::new(
            v1_config(),
            v2_config(),
            BackendStrategy::ABTest { v2_percentage: 50 },
        ).unwrap();

        let mut v1_count = 0;
        let mut v2_count = 0;

        for _ in 0..1000 {
            let result = adapter.search(&test_query()).await.unwrap();
            match result.backend_used {
                BackendType::V1 => v1_count += 1,
                BackendType::V2 => v2_count += 1,
                _ => {}
            }
        }

        // Should be roughly 50/50 (allow 10% variance)
        assert!(v1_count > 450 && v1_count < 550);
        assert!(v2_count > 450 && v2_count < 550);
    }
}
```

#### Error Handling Tests (60 tests)

```rust
// ggen-domain/tests/error_handling_unit_tests.rs

#[tokio::test]
async fn test_retry_with_backoff() {
    let mut attempts = 0;
    let result = retry_with_backoff(
        || async {
            attempts += 1;
            if attempts < 3 {
                Err(MarketplaceError::NetworkError("temp".to_string()))
            } else {
                Ok("success")
            }
        },
        5,
        10,
        100,
    ).await;

    assert!(result.is_ok());
    assert_eq!(attempts, 3);
}

#[tokio::test]
async fn test_circuit_breaker() {
    let breaker = CircuitBreaker::new(3, 1000);

    // Trigger circuit breaker
    for _ in 0..3 {
        let _ = breaker.execute(|| async {
            Err::<(), _>(MarketplaceError::Internal("fail".to_string()))
        }).await;
    }

    // Should be open now
    let result = breaker.execute(|| async { Ok(()) }).await;
    assert!(result.is_err());
}

#[test]
fn test_error_conversion() {
    let v1_error = ggen_marketplace::Error::PackageNotFound("test".to_string());
    let unified: MarketplaceError = v1_error.into();

    assert!(matches!(unified, MarketplaceError::PackageNotFound(_)));
}
```

### 2. Component Tests (200 tests, 30%)

#### V2 Search Component (50 tests)

```rust
// ggen-marketplace-v2/tests/search_component_tests.rs

#[tokio::test]
async fn test_sparql_query_builder() {
    let builder = SparqlQueryBuilder::new();
    let query = builder
        .with_text_search("rust framework")
        .with_category("backend")
        .with_limit(10)
        .build();

    assert!(query.contains("FILTER(CONTAINS"));
    assert!(query.contains("LIMIT 10"));
}

#[tokio::test]
async fn test_result_ranking() {
    let ranker = ResultRanker::new();
    let results = vec![
        create_package("pkg-a", 100, 50),  // downloads, stars
        create_package("pkg-b", 50, 100),
        create_package("pkg-c", 200, 10),
    ];

    let ranked = ranker.rank(&results, "test query");

    // pkg-c should rank highest (most downloads)
    assert_eq!(ranked[0].name, "pkg-c");
}
```

#### V2 Crypto Component (40 tests)

```rust
// ggen-marketplace-v2/tests/crypto_component_tests.rs

#[test]
fn test_keygen() {
    let (public_key, private_key) = KeyGenerator::generate_keypair();

    assert_eq!(public_key.as_bytes().len(), 32);
    assert_eq!(private_key.as_bytes().len(), 64);
}

#[test]
fn test_sign_and_verify() {
    let signer = Ed25519Signer::new(&test_keypair());
    let message = b"test message";

    let signature = signer.sign(message).unwrap();
    let valid = signer.verify(message, &signature).unwrap();

    assert!(valid);
}

#[test]
fn test_batch_verification() {
    let verifier = BatchSignatureVerifier::new();
    let packages = create_signed_packages(100);

    let results = verifier.verify_batch(&packages);

    assert_eq!(results.len(), 100);
    assert!(results.iter().all(|&r| r));
}
```

#### V2 RDF Component (60 tests)

```rust
// ggen-marketplace-v2/tests/rdf_component_tests.rs

#[test]
fn test_rdf_triple_generation() {
    let pkg = create_test_package();
    let triples = RdfTripleBuilder::build_package_triples(&pkg).unwrap();

    assert!(!triples.is_empty());
    assert!(triples.iter().any(|t| t.predicate == "pkg:name"));
    assert!(triples.iter().any(|t| t.predicate == "pkg:version"));
}

#[tokio::test]
async fn test_rdf_store_insert_and_query() {
    let store = RdfRegistry::new(test_store_path()).unwrap();
    let pkg = create_test_package();

    store.insert_package(&pkg).await.unwrap();

    let retrieved = store.get_package(&pkg.id).await.unwrap();
    assert_eq!(retrieved.name, pkg.name);
}

#[test]
fn test_ontology_validation() {
    let pkg = create_test_package();
    let validator = OntologyValidator::new(&test_schema());

    let result = validator.validate(&pkg);
    assert!(result.is_ok());
}
```

### 3. Integration Tests (80 tests, 15%)

#### Cross-Backend Integration (30 tests)

```rust
// tests/integration/cross_backend_integration_tests.rs

#[tokio::test]
async fn test_same_package_from_both_backends() {
    let v1 = V1Adapter::new(test_config()).unwrap();
    let v2 = V2Adapter::new(test_config()).unwrap();

    let pkg_id = test_package_id();

    let v1_pkg = v1.get_package(&pkg_id).await.unwrap();
    let v2_pkg = v2.get_package(&pkg_id).await.unwrap();

    // Should have same core data
    assert_eq!(v1_pkg.summary.name, v2_pkg.summary.name);
    assert_eq!(v1_pkg.summary.version, v2_pkg.summary.version);
}

#[tokio::test]
async fn test_search_result_comparison() {
    let v1 = V1Adapter::new(test_config()).unwrap();
    let v2 = V2Adapter::new(test_config()).unwrap();

    let query = SearchQuery {
        query_text: "rust web framework".to_string(),
        limit: 10,
        ..Default::default()
    };

    let v1_results = v1.search(&query).await.unwrap();
    let v2_results = v2.search(&query).await.unwrap();

    // Results should overlap significantly (>70%)
    let overlap = compute_result_overlap(&v1_results, &v2_results);
    assert!(overlap > 0.7);
}
```

#### Migration Integration (20 tests)

```rust
// tests/integration/migration_integration_tests.rs

#[tokio::test]
async fn test_migrate_all_packages() {
    let v1 = V1Adapter::new(test_config()).unwrap();
    let v2 = V2Adapter::new(test_config()).unwrap();

    let all_packages = v1.list_all().await.unwrap();

    for v1_pkg in all_packages {
        // Convert and publish to V2
        let unified: UnifiedPackage = v1_pkg.into();
        let v2_pkg: V2Package = unified.try_into().unwrap();

        v2.publish_package(&v2_pkg).await.unwrap();
    }

    // Verify all packages migrated
    let v2_count = v2.count_packages().await.unwrap();
    assert_eq!(v2_count, all_packages.len());
}
```

#### CLI Integration (30 tests)

```rust
// ggen-cli/tests/integration/cli_integration_tests.rs

#[test]
fn test_cli_search_with_v1() {
    let output = Command::new("ggen")
        .args(&["marketplace", "search", "--query", "rust", "--backend", "v1"])
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("packages"));
}

#[test]
fn test_cli_search_with_v2() {
    let output = Command::new("ggen")
        .args(&["marketplace", "search", "--query", "rust", "--backend", "v2"])
        .output()
        .unwrap();

    assert!(output.status.success());
}

#[test]
fn test_cli_install_with_signature_verification() {
    let output = Command::new("ggen")
        .args(&[
            "marketplace",
            "install",
            "signed-package",
            "--backend",
            "v2",
            "--verify-signature",
        ])
        .output()
        .unwrap();

    assert!(output.status.success());
}
```

### 4. End-to-End Tests (20 tests, 5%)

```rust
// tests/e2e/marketplace_e2e_tests.rs

#[tokio::test]
async fn test_complete_publish_search_install_workflow_v1() {
    // 1. Publish package
    let pkg = create_test_package();
    publish_package_v1(&pkg).await.unwrap();

    // 2. Search for package
    let results = search_v1("test-package").await.unwrap();
    assert!(results.iter().any(|p| p.name == "test-package"));

    // 3. Install package
    let installed = install_v1("test-package").await.unwrap();
    assert!(installed.path.exists());

    // 4. Verify installation
    assert!(verify_installation(&installed.path).is_ok());
}

#[tokio::test]
async fn test_complete_publish_search_install_workflow_v2_with_signing() {
    let keypair = generate_test_keypair();

    // 1. Publish signed package
    let pkg = create_test_package();
    publish_signed_package_v2(&pkg, &keypair).await.unwrap();

    // 2. Search for package
    let results = search_v2("test-package").await.unwrap();
    assert!(results.iter().any(|p| p.name == "test-package"));

    // 3. Install with signature verification
    let installed = install_v2_with_verification("test-package").await.unwrap();
    assert!(installed.signature_valid);

    // 4. Verify installation
    assert!(verify_installation(&installed.path).is_ok());
}

#[tokio::test]
async fn test_migration_and_continued_usage() {
    // 1. Migrate all V1 packages to V2
    migrate_v1_to_v2().await.unwrap();

    // 2. Publish new V2-only package (with signature)
    let new_pkg = create_v2_only_package();
    publish_signed_package_v2(&new_pkg, &test_keypair()).await.unwrap();

    // 3. Search should return both migrated and new packages
    let results = search_v2("").await.unwrap();
    assert!(results.len() > 100);  // Migrated packages
    assert!(results.iter().any(|p| p.name == new_pkg.name));

    // 4. Install mix of old and new packages
    install_v2("old-package").await.unwrap();
    install_v2("new-package").await.unwrap();
}
```

## Feature Flag Testing Matrix

```rust
// tests/feature_flags/matrix_tests.rs

// Test all feature combinations
#[cfg(all(feature = "marketplace-v1", not(feature = "marketplace-parallel")))]
mod v1_only_tests {
    #[test]
    fn test_v1_backend_available() {
        let backend = create_backend(test_config()).unwrap();
        assert!(matches!(backend.metadata().backend_type, BackendType::V1));
    }
}

#[cfg(all(feature = "marketplace-v2", not(feature = "marketplace-parallel")))]
mod v2_only_tests {
    #[test]
    fn test_v2_backend_available() {
        let backend = create_backend(test_config()).unwrap();
        assert!(matches!(backend.metadata().backend_type, BackendType::V2));
    }
}

#[cfg(feature = "marketplace-parallel")]
mod parallel_tests {
    #[test]
    fn test_dual_backend_available() {
        let backend = create_backend(test_config()).unwrap();
        // Dual backend can return V1, V2, or Dual depending on strategy
        assert!(backend.metadata().backend_type != BackendType::Unknown);
    }
}
```

## Performance Testing

### Benchmark Suite

```rust
// benches/comprehensive_benchmarks.rs

criterion_group!(
    benches,
    benchmark_search_latency,
    benchmark_install_latency,
    benchmark_signature_verification,
    benchmark_rdf_conversion,
    benchmark_sparql_queries,
);

fn benchmark_search_latency(c: &mut Criterion) {
    let mut group = c.benchmark_group("search_latency");

    group.bench_function("v1_search", |b| {
        b.to_async(Runtime::new().unwrap())
            .iter(|| v1_adapter.search(&test_query()))
    });

    group.bench_function("v2_search", |b| {
        b.to_async(Runtime::new().unwrap())
            .iter(|| v2_adapter.search(&test_query()))
    });

    group.finish();
}
```

### Load Testing

```bash
#!/bin/bash
# comprehensive_load_test.sh

echo "=== Load Testing Marketplace V2 ==="

# 1. Baseline load test (V1)
echo "Testing V1 baseline..."
wrk -t8 -c200 -d30s --latency http://localhost:8080/api/v1/search > v1_results.txt

# 2. V2 load test
echo "Testing V2..."
wrk -t8 -c200 -d30s --latency http://localhost:8080/api/v2/search > v2_results.txt

# 3. Compare results
python compare_load_tests.py v1_results.txt v2_results.txt
```

## Continuous Integration Testing

```yaml
# .github/workflows/comprehensive-test.yml

name: Comprehensive Test Suite

on: [push, pull_request]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run unit tests
        run: cargo test --lib

  component-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run component tests
        run: cargo test --package ggen-marketplace-v2

  integration-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run integration tests
        run: cargo test --test '*_integration_*'

  e2e-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run E2E tests
        run: cargo test --test '*_e2e_*'

  feature-matrix:
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

  benchmarks:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run benchmarks
        run: cargo bench --features marketplace-parallel

  backward-compatibility:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Test backward compatibility
        run: |
          cargo test --features marketplace-v1
          cargo test --features marketplace-v2
          # Ensure V2 can read V1 data
          cargo test backward_compat
```

## Test Coverage Requirements

| Category | Coverage Target | Current Status |
|----------|----------------|----------------|
| Unit Tests | ≥90% | Track via `cargo tarpaulin` |
| Component Tests | ≥85% | Track via `cargo tarpaulin` |
| Integration Tests | ≥75% | Track via `cargo tarpaulin` |
| E2E Tests | ≥60% | Track via E2E coverage tool |
| Feature Combinations | 100% | All combinations tested in CI |
| Backward Compatibility | 100% | All V1 tests pass with adapter |

## Success Criteria

| Criterion | Target | Validation |
|-----------|--------|------------|
| All tests passing | 100% | CI green |
| Feature flag coverage | 100% | All combinations tested |
| Backward compatibility | 100% | All V1 tests pass with adapter |
| Performance benchmarks | V2 ≥ V1 | Benchmark comparison |
| Code coverage | ≥85% | Tarpaulin report |
| Zero breaking changes | 100% | Existing tests unchanged |
