# Ggen-Marketplace Test Suite

Comprehensive test coverage for the ggen-marketplace registry and search functionality.

## Test Structure

```
tests/
├── unit/                      # Unit tests for individual components
│   ├── registry_client.rs     # RegistryClient tests
│   ├── registry_index.rs      # RegistryIndex tests
│   ├── search_params.rs       # Search parameter tests
│   ├── version_resolution.rs  # Version comparison tests
│   ├── error_handling.rs      # Error handling paths
│   └── mock_impls.rs          # Mock implementations
│
├── integration/               # Integration tests
│   ├── end_to_end_flow.rs     # Complete package lifecycle
│   ├── search_integration.rs  # Search engine integration
│   ├── multi_node_scenario.rs # Multi-node registry tests
│   └── registry_api_integration.rs # API integration tests
│
├── property/                  # Property-based tests (proptest)
│   ├── package_properties.rs  # Package invariants
│   ├── search_properties.rs   # Search properties
│   ├── version_properties.rs  # Version comparison properties
│   └── serialization_properties.rs # Serialization roundtrip
│
└── security/                  # Security tests
    ├── signature_verification.rs # Cryptographic signature tests
    ├── input_validation.rs    # Input sanitization
    ├── dos_resistance.rs      # DoS prevention
    └── injection_prevention.rs # Injection attack prevention
```

## Running Tests

### All Tests
```bash
# Run all tests
cargo test

# Run with all features
cargo test --all-features

# Run with proptest
cargo test --features proptest
```

### Unit Tests Only
```bash
cargo test --test marketplace_tests_main unit::
```

### Integration Tests Only
```bash
cargo test --test marketplace_tests_main integration::
```

### Property-Based Tests
```bash
cargo test --features proptest --test marketplace_tests_main property::
```

### Security Tests
```bash
cargo test --test marketplace_tests_main security::
```

### Benchmarks
```bash
# Run benchmarks (requires nightly or criterion)
cargo bench --bench marketplace_benchmarks
```

## Test Categories

### 1. Unit Tests (>80% coverage target)

**registry_client.rs**
- Client creation and configuration
- Custom URL handling
- Index serialization/deserialization
- Field validation

**search_params.rs**
- Parameter construction
- Case sensitivity
- Unicode handling
- Boundary values

**version_resolution.rs**
- Version parsing
- Comparison operators
- Prerelease handling
- Sorting

**error_handling.rs**
- Missing package errors
- Invalid version errors
- JSON parsing errors
- Context preservation

**mock_impls.rs**
- Test data factories
- Mock registries
- Helper utilities

### 2. Integration Tests

**end_to_end_flow.rs**
- Complete package lifecycle (search → resolve → install)
- Package update flows
- Multi-package scenarios
- Error handling flows

**search_integration.rs**
- Basic search functionality
- Advanced filtering
- Relevance ranking
- Empty result handling

**multi_node_scenario.rs**
- Multiple registry instances
- Concurrent access
- Failover simulation
- Registry isolation

**registry_api_integration.rs**
- Category aggregation
- Keyword extraction
- Package listing
- Popularity metrics

### 3. Property-Based Tests (proptest)

**package_properties.rs**
- Serialization roundtrip (∀ pack: deserialize(serialize(pack)) == pack)
- ID consistency
- Version map integrity
- Tags/keywords preservation

**search_properties.rs**
- Results are subset (∀ query: |search(query)| ≤ |all_packages|)
- Case insensitivity
- Package integrity preservation

**version_properties.rs**
- Comparison transitivity (a < b ∧ b < c ⇒ a < c)
- Comparison antisymmetry (a < b ⇒ ¬(b < a))
- Equality reflexivity (a == a)
- Parsing roundtrip

**serialization_properties.rs**
- Idempotent serialization
- Special character preservation
- Unicode handling
- Optional field preservation

### 4. Security Tests

**signature_verification.rs**
- Valid signature verification
- Tampered message detection
- Tampered signature detection
- Wrong public key rejection
- SHA256 hash consistency

**input_validation.rs**
- XSS prevention
- SQL injection prevention
- Path traversal prevention
- Null byte handling
- Unicode normalization
- Control characters
- Homograph attacks

**dos_resistance.rs**
- Large registry handling
- Deeply nested structures
- Extremely long strings
- Many tags/keywords
- Serialization bomb prevention
- Hash collision resistance

**injection_prevention.rs**
- JSON injection
- HTML injection
- YAML injection
- Template injection
- LDAP injection
- NoSQL injection
- XML/XXE injection
- CSV formula injection
- Polyglot payloads

### 5. Benchmarks

Performance benchmarks using Criterion:
- Index serialization (10, 100, 1000 packages)
- Index deserialization
- Search performance (simple & filtered)
- Version comparison
- Package lookup
- Category aggregation
- Keyword extraction
- Sorting by downloads
- Memory usage

## Coverage Goals

- **Overall**: >80% code coverage
- **Critical paths**: 100% coverage
  - Signature verification
  - Version resolution
  - Search functionality
  - Error handling
- **Security tests**: Comprehensive attack vector coverage

## Running with Coverage

```bash
# Install tarpaulin
cargo install cargo-tarpaulin

# Generate coverage report
cargo tarpaulin --out Html --output-dir coverage --all-features
```

## CI/CD Integration

Tests are automatically run in CI with:
- Multiple Rust versions (stable, beta, nightly)
- Multiple platforms (Linux, macOS, Windows)
- Coverage reporting
- Benchmark regression detection

## Writing New Tests

### Unit Test Example
```rust
#[test]
fn test_package_id_validation() {
    let pack = create_mock_pack("test-id", "Test", "1.0.0");
    assert_eq!(pack.id, "test-id");
}
```

### Property Test Example
```rust
proptest! {
    #[test]
    fn package_roundtrip(
        id in "[a-z0-9\\-]{3,20}",
        version in "[0-9]\\.[0-9]\\.[0-9]",
    ) {
        let pack = create_pack(&id, &version);
        let json = serde_json::to_string(&pack).unwrap();
        let parsed: Pack = serde_json::from_str(&json).unwrap();
        prop_assert_eq!(&parsed.id, &id);
    }
}
```

### Integration Test Example
```rust
#[tokio::test]
async fn test_complete_search_flow() -> Result<()> {
    let client = create_test_client()?;
    let results = client.search("rust").await?;
    assert!(!results.is_empty());
    Ok(())
}
```

## Test Hooks Integration

All tests support hooks for coordination:

```bash
# Pre-test hook
npx claude-flow@alpha hooks pre-task --description "Test suite"

# Post-test hook
npx claude-flow@alpha hooks post-task --task-id "testing"

# Memory storage
npx claude-flow@alpha hooks post-edit --file "tests/integration/mod.rs" --memory-key "swarm/tester/coverage"
```

## Maintenance

- Run tests before every commit
- Update benchmarks monthly
- Review security tests quarterly
- Keep coverage above 80%
- Document new test patterns

## Resources

- [Proptest Book](https://proptest-rs.github.io/proptest/)
- [Criterion Docs](https://bheisler.github.io/criterion.rs/)
- [Rust Testing Guide](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Security Testing OWASP](https://owasp.org/www-project-web-security-testing-guide/)
