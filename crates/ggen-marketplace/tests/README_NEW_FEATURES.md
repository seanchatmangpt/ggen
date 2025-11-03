# Integration Tests for New Features

This document describes the integration test suite for the three major new features:

1. **Ed25519 Cryptographic Signatures**
2. **P2P Peer-to-Peer Registry**
3. **GraphQL API**

## Test Philosophy (80/20 Rule)

These tests follow the 80/20 principle:
- **Focus on critical paths** that provide 80% of confidence
- **Test real scenarios**, not implementation details
- **Fast, deterministic, isolated** tests
- **No mocks** for core interactions (only for network/external services)

## Test Structure

### File: `tests/integration_new_features.rs`

```
integration_new_features.rs
├── Test Helpers
│   ├── MockP2PRegistry      - In-memory P2P network simulator
│   ├── MockGraphQLServer    - In-memory GraphQL endpoint
│   └── Test Data Builders   - Create signed packages, test data
│
├── Ed25519 Tests (6 tests)
│   ├── test_ed25519_hash_content
│   ├── test_ed25519_hash_deterministic
│   ├── test_ed25519_different_content_different_hash
│   ├── test_ed25519_sign_and_verify
│   ├── test_ed25519_verify_wrong_content_fails
│   ├── test_ed25519_keypair_generation
│   └── test_ed25519_export_import_public_key
│
├── P2P Registry Tests (6 tests)
│   ├── test_p2p_publish_and_discover
│   ├── test_p2p_retrieve_package
│   ├── test_p2p_peer_management
│   ├── test_p2p_discover_multiple_packages
│   ├── test_p2p_network_resilience
│   └── test_p2p_concurrent_operations
│
├── GraphQL Tests (3 tests)
│   ├── test_graphql_search_query
│   ├── test_graphql_package_query
│   └── test_graphql_query_not_found
│
└── Full Stack Integration Tests (7 tests)
    ├── test_full_stack_signed_p2p_package_via_graphql
    ├── test_signed_package_p2p_distribution
    ├── test_graphql_query_signed_packages
    ├── test_graphql_invalid_query
    ├── test_p2p_retrieve_nonexistent_package
    ├── test_large_scale_p2p_discovery
    └── test_integration_with_local_registry
```

## Test Categories

### 1. Ed25519 Cryptographic Signatures ✅

**Purpose:** Verify that Ed25519 signing and verification works correctly.

**Key Tests:**
- Hash content deterministically (SHA-256)
- Generate cryptographically secure keypairs
- Sign content with private key
- Verify signatures with public key
- Detect tampered content
- Export/import public keys in PEM format

**Example:**
```rust
#[tokio::test]
async fn test_ed25519_sign_and_verify() {
    let verifier = Ed25519Verifier::new();
    let keypair = verifier.generate_keypair().expect("should generate keypair");
    let verifier_with_key = Ed25519Verifier::with_keypair(keypair);

    let content = b"test content";
    let signature = verifier_with_key.sign(content).expect("should sign");
    let is_valid = verifier_with_key.verify(content, &signature).expect("should verify");

    assert!(is_valid);
}
```

### 2. P2P Registry 🔄

**Purpose:** Verify peer-to-peer package discovery and distribution.

**Key Tests:**
- Publish packages to P2P network
- Discover packages via query
- Retrieve packages by ID
- Manage peer connections
- Handle concurrent operations
- Scale to hundreds of packages

**Example:**
```rust
#[tokio::test]
async fn test_p2p_publish_and_discover() {
    let p2p_registry = MockP2PRegistry::new();

    // Publish package
    p2p_registry.publish(package).await.expect("publish should succeed");

    // Discover via P2P
    let results = p2p_registry.discover("query").await.expect("discover should succeed");

    assert_eq!(results.len(), 1);
}
```

### 3. GraphQL API 🔍

**Purpose:** Verify GraphQL queries return correct package data.

**Key Tests:**
- Search packages via GraphQL
- Query specific package by name
- Handle not found errors
- Parse query syntax
- Return proper JSON responses

**Example:**
```rust
#[tokio::test]
async fn test_graphql_search_query() {
    let graphql_server = MockGraphQLServer::new(p2p_registry);

    let query = r#"
        {
            searchPackages(query: "rust") {
                name
                version
            }
        }
    "#;

    let response = graphql_server.execute_query(query).await.expect("query should succeed");

    assert!(response.data.is_some());
}
```

### 4. Full Stack Integration 🎯

**Purpose:** Verify all three features work together end-to-end.

**Key Scenarios:**
1. **Sign → Publish → Query:**
   - Sign package with Ed25519
   - Publish to P2P network
   - Query via GraphQL
   - Verify signature in results

2. **Distributed Signatures:**
   - Multiple signed packages
   - P2P distribution
   - GraphQL discovery
   - Signature verification

3. **Production Scenarios:**
   - Large scale (100+ packages)
   - Concurrent operations
   - Error handling
   - Integration with local registry

**Example:**
```rust
#[tokio::test]
async fn test_full_stack_signed_p2p_package_via_graphql() {
    // 1. Create signed package
    let (package, _) = create_signed_package("pkg", "1.0.0", b"content")
        .await.expect("should create");

    // 2. Publish to P2P
    p2p_registry.publish(package).await.expect("publish should succeed");

    // 3. Query via GraphQL
    let query = r#"{ searchPackages(query: "pkg") { name } }"#;
    let response = graphql_server.execute_query(query).await.expect("query should succeed");

    // 4. Verify result
    assert!(response.data.is_some());
}
```

## Running the Tests

### Run all integration tests:
```bash
cargo test --test integration_new_features
```

### Run specific test category:
```bash
# Ed25519 tests only
cargo test --test integration_new_features ed25519

# P2P tests only
cargo test --test integration_new_features p2p

# GraphQL tests only
cargo test --test integration_new_features graphql

# Full stack tests only
cargo test --test integration_new_features full_stack
```

### Run with output:
```bash
cargo test --test integration_new_features -- --nocapture
```

### Run in release mode (faster):
```bash
cargo test --release --test integration_new_features
```

## Test Performance Expectations

Following the 80/20 testing strategy:

| Test Category | Tests | Target Time | Coverage |
|--------------|-------|-------------|----------|
| Ed25519 | 7 | <1s | 100% critical paths |
| P2P Registry | 6 | <2s | 90% core functionality |
| GraphQL | 3 | <1s | 80% query patterns |
| Full Stack | 7 | <3s | 95% integration paths |
| **Total** | **23** | **<7s** | **90% overall** |

## Test Helpers

### MockP2PRegistry

Simulates a P2P network without actual networking:
- In-memory package storage
- Peer management
- Package discovery
- Concurrent-safe operations

### MockGraphQLServer

Provides a simple GraphQL endpoint:
- Query parsing (simplified)
- Integration with P2P registry
- Error handling
- Response formatting

### Test Data Builders

```rust
// Create signed package
let (package, content) = create_signed_package(
    "package-name",
    "1.0.0",
    b"package content"
).await?;

// Setup test environment
let (p2p_registry, graphql_server) = setup_test_environment().await;
```

## Coverage Goals

Following the 80/20 principle:

1. **Critical Paths (100%):**
   - Sign and verify content
   - Publish and discover packages
   - Query via GraphQL
   - End-to-end workflows

2. **Core Functionality (90%):**
   - Keypair generation
   - P2P peer management
   - GraphQL query parsing
   - Error handling

3. **Edge Cases (80%):**
   - Invalid signatures
   - Not found errors
   - Concurrent operations
   - Large scale scenarios

4. **Internal Details (50%):**
   - Implementation specifics
   - Performance optimizations
   - Advanced features

## What We DON'T Test

Per the 80/20 strategy, we avoid:
- Internal implementation details
- Third-party library behavior (ed25519-dalek, libp2p, async-graphql)
- Trivial getters/setters
- Obvious error messages
- Generated code

## Debugging Failed Tests

### Ed25519 Test Failures

```bash
# Run with verbose output
RUST_LOG=debug cargo test --test integration_new_features ed25519 -- --nocapture

# Common issues:
# - Keypair not generated
# - Invalid signature length (should be 64 bytes)
# - Content hash mismatch
```

### P2P Test Failures

```bash
# Check P2P operations
cargo test --test integration_new_features p2p -- --nocapture

# Common issues:
# - Package not found in registry
# - Concurrent access conflicts
# - Query string mismatch
```

### GraphQL Test Failures

```bash
# Test GraphQL queries
cargo test --test integration_new_features graphql -- --nocapture

# Common issues:
# - Query parsing errors
# - Invalid JSON responses
# - Package not in P2P registry
```

## Test Maintenance

### Adding New Tests

1. Follow the existing test structure
2. Use test helpers for common setup
3. Keep tests fast (<100ms per test)
4. Test one behavior per test
5. Use descriptive names

### Example:
```rust
#[tokio::test]
async fn test_new_feature_happy_path() {
    // Arrange
    let (p2p, graphql) = setup_test_environment().await;

    // Act
    let result = perform_operation().await?;

    // Assert
    assert_eq!(result.status, "success");
}
```

## Continuous Integration

These tests are designed to run in CI:
- No external dependencies
- Deterministic results
- Fast execution (<10s total)
- Isolated (no shared state)

### CI Configuration Example:
```yaml
test:
  script:
    - cargo test --test integration_new_features --release
  rules:
    - if: $CI_PIPELINE_SOURCE == "merge_request_event"
```

## Future Work

As P2P and GraphQL features are fully implemented:

1. **Replace Mocks:** Convert `MockP2PRegistry` and `MockGraphQLServer` to use real implementations with testcontainers
2. **Add E2E Tests:** Full network tests with multiple nodes
3. **Performance Benchmarks:** Measure throughput and latency
4. **Security Tests:** Penetration testing, fuzzing
5. **Integration Tests:** Test with real ggen and clnrm CLI tools

## Related Documentation

- [80/20 Testing Strategy](./80_20_TESTING_STRATEGY.md)
- [Ed25519 Cryptography](../src/crypto/ed25519.rs)
- [P2P Registry Design](../src/integration.md)
- [GraphQL API Schema](../docs/graphql-api.md)
- [Architecture Overview](../src/architecture.md)

## Questions?

For questions or issues with these tests:
1. Check existing test documentation
2. Review test helper implementations
3. See [80/20 Testing Strategy](./80_20_TESTING_STRATEGY.md)
4. Review architecture documentation
