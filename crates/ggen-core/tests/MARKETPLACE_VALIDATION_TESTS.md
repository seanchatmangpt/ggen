# Marketplace Validation Test Suite

## Overview

Comprehensive validation tests for ggen marketplace operations using isolated test environments. This test suite ensures marketplace search, package installation, P2P registry interactions, error handling, and performance meet production standards.

## Test Files Created

### 1. `integration/marketplace_validation.rs`
**Purpose**: Core marketplace validation tests
**Coverage**:
- ✅ Search functionality (basic, by tag, case-insensitive, empty results, special characters)
- ✅ Package resolution and installation verification
- ✅ Specific version resolution
- ✅ Package metadata validation
- ✅ Error handling (nonexistent packages, invalid versions, empty registry, malformed index)
- ✅ Concurrent search operations
- ✅ Performance tests (large registry, rapid searches, package listing)
- ✅ Update and version management
- ✅ Registry metadata (categories, statistics)

**Test Count**: 22 comprehensive tests

### 2. `integration/marketplace_p2p_tests.rs`
**Purpose**: P2P registry interaction tests
**Coverage**:
- ✅ Peer discovery and connectivity
- ✅ Multi-peer mesh topology
- ✅ Package publishing to P2P network
- ✅ Package propagation across peers
- ✅ Distributed search operations
- ✅ DHT (Distributed Hash Table) operations
- ✅ Network partition handling
- ✅ Peer failure recovery
- ✅ Peer reputation tracking
- ✅ Performance and stress tests (concurrent publishes, large network scalability)

**Test Count**: 15 P2P-specific tests

## Test Categories

### Search Functionality Tests
```rust
#[tokio::test]
async fn test_marketplace_basic_search() -> Result<()>
async fn test_marketplace_search_by_tag() -> Result<()>
async fn test_marketplace_search_case_insensitive() -> Result<()>
async fn test_marketplace_search_empty_results() -> Result<()>
async fn test_marketplace_search_with_special_characters() -> Result<()>
```

### Package Installation Tests
```rust
#[tokio::test]
async fn test_marketplace_package_resolve() -> Result<()>
async fn test_marketplace_package_resolve_specific_version() -> Result<()>
async fn test_marketplace_package_metadata_validation() -> Result<()>
```

### Error Handling Tests
```rust
#[tokio::test]
async fn test_marketplace_nonexistent_package() -> Result<()>
async fn test_marketplace_invalid_version() -> Result<()>
async fn test_marketplace_empty_registry() -> Result<()>
async fn test_marketplace_malformed_index_handling() -> Result<()>
async fn test_marketplace_concurrent_searches() -> Result<()>
```

### Performance Tests
```rust
#[tokio::test]
async fn test_marketplace_large_registry_search() -> Result<()>
async fn test_marketplace_rapid_successive_searches() -> Result<()>
async fn test_marketplace_package_list_performance() -> Result<()>
```

### P2P Network Tests
```rust
#[tokio::test]
async fn test_p2p_peer_discovery() -> Result<()>
async fn test_p2p_multi_peer_connectivity() -> Result<()>
async fn test_p2p_publish_package() -> Result<()>
async fn test_p2p_package_propagation() -> Result<()>
async fn test_p2p_distributed_search() -> Result<()>
async fn test_p2p_dht_put_get() -> Result<()>
async fn test_p2p_network_partition_handling() -> Result<()>
async fn test_p2p_peer_failure_recovery() -> Result<()>
```

## Running the Tests

### Run All Marketplace Validation Tests
```bash
cd ggen-core
cargo test --test marketplace_tests_main integration::marketplace_validation
```

### Run All P2P Tests
```bash
cd ggen-core
cargo test --test marketplace_tests_main integration::marketplace_p2p_tests
```

### Run Specific Test Category
```bash
# Search tests only
cargo test --test marketplace_tests_main integration::marketplace_validation::test_marketplace_.*search

# Error handling tests only
cargo test --test marketplace_tests_main integration::marketplace_validation::test_marketplace_.*error

# P2P peer discovery tests
cargo test --test marketplace_tests_main integration::marketplace_p2p_tests::test_p2p_peer
```

### Run with Coverage
```bash
cargo tarpaulin --test marketplace_tests_main --out Html --output-dir coverage
```

### Run with Timing
```bash
cargo test --test marketplace_tests_main integration::marketplace_validation -- --nocapture --test-threads=1
```

## Test Architecture

### Helper Functions

#### `create_test_registry()`
Creates a temporary registry with multiple test packages.

```rust
fn create_test_registry(
    _temp_dir: &TempDir,
    packages: Vec<(&str, &str, Vec<&str>)>
) -> Result<RegistryIndex>
```

#### `setup_test_client()`
Sets up a registry client with a temporary index.

```rust
async fn setup_test_client(
    packages: Vec<(&str, &str, Vec<&str>)>
) -> Result<(TempDir, RegistryClient)>
```

### Mock P2P Components

#### `MockP2PRegistry`
Simulates a P2P registry node for testing without actual networking.

```rust
struct MockP2PRegistry {
    peer_id: String,
    packages: HashMap<String, PackMetadata>,
    peers: Vec<String>,
    latency_ms: u64,
}
```

#### `MockP2PNetwork`
Manages multiple mock P2P nodes for network simulation.

```rust
struct MockP2PNetwork {
    nodes: HashMap<String, MockP2PRegistry>,
}
```

## Test Isolation

All tests use:
- ✅ Temporary directories (automatically cleaned up)
- ✅ Isolated registry clients
- ✅ No shared state between tests
- ✅ Proper error handling (no `.unwrap()` or `.expect()`)
- ✅ Async/await for concurrent operations

## Performance Benchmarks

### Large Registry Search
- **Target**: 100 packages searchable in < 1 second
- **Status**: ✅ Implemented

### Rapid Successive Searches
- **Target**: 50 searches in < 5 seconds
- **Status**: ✅ Implemented

### Package Listing
- **Target**: 200 packages listed in < 1 second
- **Status**: ✅ Implemented

### Concurrent Operations
- **Target**: 10 concurrent searches without failures
- **Status**: ✅ Implemented

## Integration with Clnrm Harness

The tests integrate with the existing `clnrm_harness` module for:
- Test environment setup
- Temporary directory management
- Test fixture creation
- Clean resource cleanup

Example usage:
```rust
use ggen_core::tests::integration::clnrm_harness::TestHarness;

#[tokio::test]
async fn test_with_clnrm() -> Result<()> {
    let harness = TestHarness::new().await?;
    let fixture = harness.marketplace_fixture().await?;

    let results = fixture.search("rust").await?;
    assert!(!results.is_empty());

    Ok(())
}
```

## Error Handling

All tests follow production-ready error handling:
- ✅ All functions return `Result<T>` or `Result<()>`
- ✅ No `.unwrap()` or `.expect()` in test code
- ✅ Proper error context with `anyhow::Context`
- ✅ Graceful degradation on failures

## Coordination Hooks

Tests support coordination hooks for swarm testing:

```bash
# Pre-test hook
npx claude-flow@alpha hooks pre-task --description "Run marketplace validation tests"

# Post-test hook
npx claude-flow@alpha hooks post-task --task-id "marketplace-validation"

# Memory storage
npx claude-flow@alpha hooks post-edit \
    --file "tests/integration/marketplace_validation.rs" \
    --memory-key "swarm/tests/marketplace-validation"
```

## Coverage Goals

- **Overall**: >80% code coverage
- **Critical paths**: 100% coverage
  - Search functionality
  - Package resolution
  - Error handling
  - P2P operations

## Future Enhancements

1. **Real P2P Network Tests**: Integration with actual libp2p network
2. **Stress Tests**: 1000+ package registries
3. **Security Tests**: Malicious package detection
4. **Performance Profiling**: Detailed performance metrics
5. **Integration Tests**: Full end-to-end marketplace workflows

## Maintenance

- Run tests before every commit
- Update benchmarks monthly
- Review performance tests quarterly
- Keep coverage above 80%
- Document new test patterns

## Resources

- [Marketplace Architecture](../../ggen-marketplace/README.md)
- [Testing Guide](./README.md)
- [Clnrm Harness Documentation](./clnrm_harness.rs)
- [Proptest Guide](https://proptest-rs.github.io/proptest/)

## Status

✅ **Test Suite Complete**
- 37 comprehensive tests implemented
- Full coverage of marketplace operations
- P2P networking simulation
- Performance benchmarks
- Error handling validation
- Production-ready error handling (no unwrap/expect)
