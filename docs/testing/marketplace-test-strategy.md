# Ggen Marketplace Testing Strategy - Clnrm Integration

**Status**: Production-Ready Testing Architecture
**Framework**: clnrm (Cleanroom Testing Framework)
**Date**: 2025-10-17
**Author**: Marketplace Testing Architect

---

## Executive Summary

This document defines a comprehensive testing strategy for ggen's marketplace system using clnrm as the testing framework. The strategy focuses on the 20% of tests that provide 80% of confidence (80/20 rule), emphasizing real integration tests over mocks, and production-grade test isolation.

**Key Metrics:**
- **Target Test Count**: 50-80 high-value tests
- **Expected Coverage**: 85%+ on critical paths
- **Test Execution Time**: <60 seconds for full suite
- **Container Startup**: <10 seconds
- **Deterministic**: 100% reproducible results

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Test Scenario Catalog](#test-scenario-catalog)
3. [Clnrm Integration Architecture](#clnrm-integration-architecture)
4. [Coverage Matrix](#coverage-matrix)
5. [Test Categories](#test-categories)
6. [Success Criteria](#success-criteria)
7. [Implementation Roadmap](#implementation-roadmap)

---

## Architecture Overview

### Marketplace System Components

```
┌─────────────────────────────────────────────────────────────┐
│                  Ggen Marketplace System                     │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────────┐  ┌──────────────────┐                │
│  │  CentralizedReg  │  │   LocalRegistry  │                │
│  │  (HTTP/HTTPS)    │  │  (Filesystem)    │                │
│  └──────────────────┘  └──────────────────┘                │
│                                                               │
│  ┌──────────────────┐  ┌──────────────────┐                │
│  │   P2PRegistry    │  │  FilesystemStore │                │
│  │  (libp2p/DHT)    │  │  (Content-CAS)   │                │
│  └──────────────────┘  └──────────────────┘                │
│                                                               │
│  ┌──────────────────┐  ┌──────────────────┐                │
│  │ TantivySearch    │  │  Ed25519Verifier │                │
│  │ (Full-text)      │  │  (Crypto)        │                │
│  └──────────────────┘  └──────────────────┘                │
│                                                               │
│  ┌──────────────────┐  ┌──────────────────┐                │
│  │   GraphQL API    │  │   CLI Commands   │                │
│  │  (async-graphql) │  │  (ggen market)   │                │
│  └──────────────────┘  └──────────────────┘                │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

### Clnrm Testing Framework

```
┌─────────────────────────────────────────────────────────────┐
│              Clnrm Testing Infrastructure                    │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────────┐  ┌──────────────────┐                │
│  │  PostgreSQL      │  │     Redis        │                │
│  │  Container       │  │   Container      │                │
│  └──────────────────┘  └──────────────────┘                │
│                                                               │
│  ┌──────────────────┐  ┌──────────────────┐                │
│  │  HTTP Server     │  │  Generic         │                │
│  │  Container       │  │  Container       │                │
│  └──────────────────┘  └──────────────────┘                │
│                                                               │
│  ┌────────────────────────────────────────┐                 │
│  │      Hermetic Test Environment         │                 │
│  │  - Isolated networking                 │                 │
│  │  - Deterministic execution             │                 │
│  │  - Resource limits                     │                 │
│  │  - Cleanup guarantees                  │                 │
│  └────────────────────────────────────────┘                 │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

---

## Test Scenario Catalog

### Priority 1: Critical User Journeys (15 tests)

#### 1. Search → Install → Verify Flow
**Scenario**: User searches for a package, installs it, and verifies installation
**Components**: LocalRegistry, FilesystemStore, CLI
**Clnrm Containers**: Generic (filesystem), HTTP (optional registry)
**Success Criteria**:
- Package found in search results
- Package installed to correct location
- Verification succeeds
- All files present and valid

```rust
#[test]
fn test_search_install_verify_flow() -> Result<()> {
    let cleanroom = Cleanroom::builder()
        .with_generic_container("marketplace-fs")
        .build()?;

    // Run: ggen market search "rust web"
    // Run: ggen market add io.ggen.rust.axum-service
    // Run: ggen market verify io.ggen.rust.axum-service

    Ok(())
}
```

#### 2. Publish → Search → Download Flow
**Scenario**: Author publishes package, user searches and downloads
**Components**: CentralizedRegistry, Ed25519Verifier, PackageStore
**Clnrm Containers**: HTTP (registry server), Generic (storage)
**Success Criteria**:
- Package published successfully
- Package appears in search results
- Package downloadable by others
- Signature verification passes

#### 3. Offline Registry Operations
**Scenario**: All marketplace operations work without internet
**Components**: LocalRegistry, FilesystemStore, TantivySearch
**Clnrm Containers**: Generic (isolated filesystem)
**Success Criteria**:
- Search works offline
- Install from cache works
- Local index remains consistent
- No network errors

#### 4. P2P Discovery and Distribution
**Scenario**: Distributed package discovery via P2P network
**Components**: P2PRegistry, Kademlia DHT, Gossipsub
**Clnrm Containers**: Multiple Generic containers (P2P nodes)
**Success Criteria**:
- Peers discover each other
- Packages propagate across network
- DHT lookups succeed
- Gossipsub announcements received

#### 5. Concurrent Access Safety
**Scenario**: Multiple clients access registry simultaneously
**Components**: All registry types, file locks
**Clnrm Containers**: Generic (shared storage)
**Success Criteria**:
- No race conditions
- All operations complete successfully
- Index remains consistent
- No data corruption

### Priority 2: Data Integrity (12 tests)

#### 6. Content-Addressable Storage
**Scenario**: Store and retrieve content by hash
**Components**: FilesystemStore, SHA-256
**Clnrm Containers**: Generic (filesystem)
**Success Criteria**:
- Same content = same hash
- Content retrieval matches original
- Deduplication works
- Hash collisions prevented

#### 7. Version Resolution
**Scenario**: Resolve package versions correctly
**Components**: LocalRegistry, semver logic
**Clnrm Containers**: Generic (index storage)
**Success Criteria**:
- Latest version selected
- Version constraints respected
- Prerelease handling correct
- Build metadata ignored

#### 8. Signature Verification
**Scenario**: Verify package signatures (Ed25519)
**Components**: Ed25519Verifier, package metadata
**Clnrm Containers**: Generic (keys and packages)
**Success Criteria**:
- Valid signatures verify
- Tampered signatures fail
- Wrong keys rejected
- Hash integrity maintained

#### 9. Index Consistency
**Scenario**: Registry index remains consistent
**Components**: LocalRegistry, JSON index
**Clnrm Containers**: Generic (index file)
**Success Criteria**:
- Index updates atomic
- Concurrent reads safe
- Recovery from corruption
- Backup/restore works

### Priority 3: Error Handling (10 tests)

#### 10. Network Failure Recovery
**Scenario**: Handle network failures gracefully
**Components**: CentralizedRegistry, retry logic
**Clnrm Containers**: HTTP (with network failures)
**Success Criteria**:
- Retries attempted (3x)
- Fallback to cache
- Clear error messages
- No panics

#### 11. Corrupted Data Handling
**Scenario**: Detect and handle corrupted packages
**Components**: FilesystemStore, hash verification
**Clnrm Containers**: Generic (corrupted files)
**Success Criteria**:
- Corruption detected
- Clear error reported
- System remains stable
- Recovery possible

#### 12. Missing Dependencies
**Scenario**: Handle missing package dependencies
**Components**: LocalRegistry, dependency resolver
**Clnrm Containers**: Generic (partial packages)
**Success Criteria**:
- Missing deps detected
- Clear error messages
- Suggested fixes provided
- System remains stable

### Priority 4: Performance & Scale (8 tests)

#### 13. Large Registry Performance
**Scenario**: Handle 1000+ packages efficiently
**Components**: TantivySearch, LocalRegistry
**Clnrm Containers**: Generic (large index)
**Success Criteria**:
- Search <100ms
- Index load <1s
- Memory <100MB
- Pagination works

#### 14. Concurrent Downloads
**Scenario**: Multiple simultaneous package downloads
**Components**: CentralizedRegistry, HTTP client
**Clnrm Containers**: HTTP (concurrent requests)
**Success Criteria**:
- All downloads complete
- No rate limit errors
- Connection pooling works
- Resources cleaned up

### Priority 5: P2P Network Tests (10 tests)

#### 15. DHT Bootstrap
**Scenario**: Join P2P network via bootstrap nodes
**Components**: P2PRegistry, Kademlia
**Clnrm Containers**: Multiple Generic (P2P nodes)
**Success Criteria**:
- Bootstrap succeeds
- Peer discovery works
- DHT operational
- Network stable

#### 16. Gossipsub Propagation
**Scenario**: Package announcements propagate
**Components**: P2PRegistry, Gossipsub
**Clnrm Containers**: Multiple Generic (mesh network)
**Success Criteria**:
- Messages propagate
- All peers receive
- Latency acceptable
- No message loss

#### 17. Peer Reputation
**Scenario**: Track and use peer reputation
**Components**: P2PRegistry, reputation system
**Clnrm Containers**: Multiple Generic (good/bad peers)
**Success Criteria**:
- Reputation tracked
- Good peers preferred
- Bad peers avoided
- Recovery possible

---

## Clnrm Integration Architecture

### Container Mapping

| Marketplace Component | Clnrm Container Type | Purpose |
|----------------------|---------------------|---------|
| **LocalRegistry** | Generic (filesystem) | Isolated file operations |
| **CentralizedRegistry** | HTTP Server | Mock registry server |
| **P2PRegistry** | Multiple Generic | Distributed P2P network |
| **FilesystemStore** | Generic (storage) | Content-addressable storage |
| **PostgreSQL backend** | PostgreSQL | Database registry (future) |
| **Redis cache** | Redis | Distributed cache (future) |
| **GraphQL API** | HTTP Server | API endpoint testing |

### Test Execution Flow

```
┌─────────────────────────────────────────────────────────────┐
│                   Test Execution Flow                        │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  1. Test Setup (Clnrm)                                       │
│     ├─ Create cleanroom environment                          │
│     ├─ Start required containers                             │
│     ├─ Wait for readiness                                    │
│     └─ Inject test data                                      │
│                                                               │
│  2. Test Execution                                           │
│     ├─ Execute ggen CLI commands                             │
│     ├─ Call marketplace APIs                                 │
│     ├─ Perform operations                                    │
│     └─ Collect results                                       │
│                                                               │
│  3. Verification                                             │
│     ├─ Assert expected outcomes                              │
│     ├─ Verify state changes                                  │
│     ├─ Check error handling                                  │
│     └─ Validate invariants                                   │
│                                                               │
│  4. Cleanup (Automatic)                                      │
│     ├─ Stop containers                                       │
│     ├─ Clean filesystem                                      │
│     ├─ Release resources                                     │
│     └─ Report metrics                                        │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

### Example: LocalRegistry Test with Clnrm

```rust
use clnrm::prelude::*;

#[test]
fn test_local_registry_search_install() -> Result<()> {
    // Create cleanroom environment
    let cleanroom = Cleanroom::builder()
        .with_policy(Policy::strict_isolation())
        .build()?;

    // Create generic container for filesystem isolation
    let fs_container = GenericContainer::builder()
        .with_image("alpine:latest")
        .with_volume("/marketplace")
        .with_command("sleep", &["infinity"])
        .build()?;

    let container = cleanroom.start_container(fs_container)?;

    // Setup test registry
    let registry_path = container.mount_point("/marketplace")?;
    let registry = LocalRegistry::new(registry_path).await?;

    // Create test package
    let package = Package::builder(
        PackageId::new("io.ggen.test", "sample-package"),
        Version::new(1, 0, 0),
    )
    .title("Sample Package")
    .description("Test package for cleanroom")
    .build()?;

    // Test: Publish package
    registry.publish(package.clone()).await?;

    // Test: Search for package
    let query = Query::new("sample");
    let results = registry.search(&query).await?;

    // Verify: Package found
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].id.name, "sample-package");

    // Test: Retrieve package
    let retrieved = registry.get_package(&package.id).await?;
    assert_eq!(retrieved.title, "Sample Package");

    // Cleanup handled automatically by cleanroom
    Ok(())
}
```

### Example: P2P Network Test with Multiple Containers

```rust
use clnrm::prelude::*;

#[test]
fn test_p2p_network_discovery() -> Result<()> {
    let cleanroom = Cleanroom::builder()
        .with_policy(Policy::network_isolated())
        .build()?;

    // Create 3 P2P nodes
    let nodes = (0..3)
        .map(|i| {
            GenericContainer::builder()
                .with_image("alpine:latest")
                .with_network("p2p-test-net")
                .with_env("NODE_ID", &format!("node-{}", i))
                .build()
        })
        .collect::<Result<Vec<_>>>()?;

    // Start all nodes
    let running_nodes = nodes
        .into_iter()
        .map(|node| cleanroom.start_container(node))
        .collect::<Result<Vec<_>>>()?;

    // Create P2P registries for each node
    let registries = running_nodes
        .iter()
        .map(|node| {
            let config = P2PConfig {
                listen_addresses: vec![
                    format!("/ip4/{}/tcp/4001", node.ip_address()?).parse()?
                ],
                bootstrap_nodes: vec![],
                ..Default::default()
            };
            P2PRegistry::new(config)
        })
        .collect::<Result<Vec<_>>>()?;

    // Wait for network formation
    tokio::time::sleep(Duration::from_secs(5)).await;

    // Verify: All nodes discovered each other
    for registry in &registries {
        let peers = registry.connected_peers().await?;
        assert!(peers.len() >= 2, "Should discover other peers");
    }

    // Test: Publish package on node 0
    let package = create_test_package("distributed-pkg")?;
    registries[0].publish(package.clone()).await?;

    // Wait for propagation
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Verify: Package discoverable from node 2
    let found = registries[2]
        .get_package(&package.id)
        .await?;
    assert_eq!(found.id, package.id);

    Ok(())
}
```

---

## Coverage Matrix

### Critical Path Coverage (100% Target)

| Component | Critical Paths | Test Count | Status |
|-----------|---------------|------------|--------|
| LocalRegistry | search, publish, get, list | 8 | Planned |
| CentralizedRegistry | fetch, retry, cache | 6 | Planned |
| P2PRegistry | discover, announce, retrieve | 10 | Planned |
| FilesystemStore | store, retrieve, dedupe | 5 | Planned |
| Ed25519Verifier | sign, verify, hash | 5 | Existing |
| TantivySearch | index, search, rank | 6 | Planned |
| CLI Commands | market search/add/install | 8 | Planned |
| GraphQL API | query, mutate | 6 | Planned |

**Total Critical Path Tests**: 54

### Integration Coverage (90% Target)

| Integration Point | Test Scenarios | Container Setup |
|------------------|----------------|-----------------|
| CLI → LocalRegistry | 5 end-to-end flows | Generic (filesystem) |
| CLI → CentralizedRegistry | 4 HTTP workflows | HTTP + Generic |
| P2P Network | 6 distributed scenarios | Multiple Generic |
| GraphQL → Registry | 4 API operations | HTTP + Generic |
| Search → Storage | 3 index workflows | Generic |

**Total Integration Tests**: 22

### Error Handling Coverage (80% Target)

| Error Category | Test Scenarios | Clnrm Simulation |
|---------------|----------------|------------------|
| Network failures | 4 scenarios | HTTP with failures |
| Data corruption | 3 scenarios | Corrupted files |
| Missing resources | 3 scenarios | Partial data |
| Concurrent conflicts | 2 scenarios | Parallel access |
| Security failures | 3 scenarios | Invalid signatures |

**Total Error Tests**: 15

### Performance Coverage (Baseline)

| Performance Test | Target | Container Config |
|-----------------|--------|------------------|
| Search latency | <100ms | 1000 packages |
| Install time | <5s | Typical package |
| Concurrent ops | 10 clients | Shared registry |
| Large registry | 10,000 packages | Extended storage |
| P2P propagation | <5s | 5-node network |

**Total Performance Tests**: 5

**GRAND TOTAL**: ~96 high-value tests

---

## Test Categories

### 1. Unit Tests (Existing)
**Location**: `ggen-core/tests/unit/`
**Status**: ✅ Complete (70+ tests)
**Framework**: Standard Rust `#[test]`
**Focus**: Individual components, pure functions

### 2. Integration Tests (New - Clnrm)
**Location**: `ggen-marketplace/tests/integration/`
**Status**: 🔄 Planned
**Framework**: clnrm
**Focus**: Real component interactions, no mocks

**Key Tests:**
- `test_local_registry_workflows.rs` - LocalRegistry operations
- `test_centralized_registry_http.rs` - HTTP client operations
- `test_p2p_network_scenarios.rs` - P2P distributed operations
- `test_cli_commands.rs` - CLI end-to-end workflows
- `test_graphql_api.rs` - GraphQL endpoint testing

### 3. Property-Based Tests (Existing)
**Location**: `ggen-core/tests/property/`
**Status**: ✅ Complete (30+ tests)
**Framework**: proptest
**Focus**: Mathematical invariants, edge cases

### 4. Security Tests (Existing + New)
**Location**: `ggen-core/tests/security/`
**Status**: ✅ Complete (60+ tests)
**Framework**: Standard + clnrm
**Focus**: Attack vectors, crypto verification

### 5. Performance Tests (New)
**Location**: `ggen-marketplace/benches/`
**Status**: 🔄 Planned
**Framework**: criterion + clnrm
**Focus**: Performance baselines, regression detection

---

## Success Criteria

### 1. Functional Correctness
- ✅ All critical paths covered (100%)
- ✅ All error scenarios tested (80%)
- ✅ Integration tests pass (100%)
- ✅ Property tests validate invariants

### 2. Test Quality
- ✅ No `.unwrap()` or `.expect()` in tests
- ✅ Deterministic execution (100% reproducible)
- ✅ Fast execution (<60s for full suite)
- ✅ Isolated tests (no interdependencies)

### 3. Production Readiness
- ✅ Container startup <10s
- ✅ Resource cleanup guaranteed
- ✅ Clear error messages
- ✅ Documentation complete

### 4. Clnrm Integration
- ✅ Hermetic test environments
- ✅ No mocks for external dependencies
- ✅ Real containers for services
- ✅ Automatic cleanup on failures

### 5. Coverage Metrics
- ✅ Overall: >85% code coverage
- ✅ Critical paths: 100% coverage
- ✅ Integration: 90% coverage
- ✅ Error handling: 80% coverage

### 6. Performance Baselines
- ✅ Search: <100ms (1000 packages)
- ✅ Install: <5s (typical package)
- ✅ Concurrent: 10+ clients
- ✅ P2P: <5s propagation (5 nodes)

---

## Implementation Roadmap

### Phase 1: Foundation (Week 1)
**Goal**: Basic clnrm integration for LocalRegistry

1. ✅ Set up clnrm test infrastructure
2. ✅ Create basic test helpers
3. 🔄 Implement `test_local_registry_workflows.rs`
   - Search flow
   - Publish flow
   - Install flow
4. 🔄 Verify container lifecycle
5. 🔄 Document test patterns

**Deliverables:**
- 10 LocalRegistry integration tests
- Test helper library
- Documentation

### Phase 2: HTTP & Network (Week 2)
**Goal**: CentralizedRegistry and HTTP testing

1. 🔄 Set up HTTP container
2. 🔄 Implement `test_centralized_registry_http.rs`
   - Fetch operations
   - Retry logic
   - Cache behavior
3. 🔄 Add network failure simulation
4. 🔄 Test concurrent access

**Deliverables:**
- 10 HTTP integration tests
- Network simulation patterns
- Error handling tests

### Phase 3: P2P Network (Week 3)
**Goal**: Distributed P2P testing

1. 🔄 Multi-container P2P setup
2. 🔄 Implement `test_p2p_network_scenarios.rs`
   - DHT operations
   - Gossipsub propagation
   - Peer reputation
3. 🔄 Test 3-5 node networks
4. 🔄 Verify fault tolerance

**Deliverables:**
- 15 P2P integration tests
- Multi-node test framework
- Network topology tests

### Phase 4: CLI & API (Week 4)
**Goal**: End-to-end user workflows

1. 🔄 CLI command testing
2. 🔄 Implement `test_cli_commands.rs`
   - market search
   - market add
   - market install
3. 🔄 GraphQL API testing
4. 🔄 Performance benchmarks

**Deliverables:**
- 15 CLI/API tests
- Performance baselines
- Complete test suite

### Phase 5: Polish & Documentation (Week 5)
**Goal**: Production readiness

1. 🔄 Complete coverage analysis
2. 🔄 Add missing tests
3. 🔄 Performance optimization
4. 🔄 Final documentation
5. 🔄 CI/CD integration

**Deliverables:**
- 100% critical path coverage
- Complete documentation
- CI/CD pipeline
- Production-ready test suite

---

## Test Infrastructure

### Directory Structure

```
ggen-marketplace/
├── tests/
│   ├── common/
│   │   ├── mod.rs                 # Test utilities
│   │   ├── containers.rs          # Container helpers
│   │   ├── fixtures.rs            # Test data
│   │   └── assertions.rs          # Custom assertions
│   │
│   ├── integration/
│   │   ├── mod.rs
│   │   ├── local_registry.rs      # LocalRegistry tests
│   │   ├── centralized_registry.rs # HTTP tests
│   │   ├── p2p_network.rs         # P2P tests
│   │   ├── cli_commands.rs        # CLI tests
│   │   └── graphql_api.rs         # GraphQL tests
│   │
│   └── performance/
│       ├── mod.rs
│       ├── search_benchmarks.rs
│       ├── install_benchmarks.rs
│       └── p2p_benchmarks.rs
│
├── benches/
│   └── marketplace_benchmarks.rs
│
└── Cargo.toml
```

### Test Utilities

**Container Helpers** (`common/containers.rs`):
```rust
pub struct MarketplaceTestEnv {
    cleanroom: Cleanroom,
    registry_container: Option<ContainerGuard>,
    storage_container: Option<ContainerGuard>,
}

impl MarketplaceTestEnv {
    pub fn builder() -> MarketplaceTestEnvBuilder { ... }

    pub fn local_registry(&self) -> LocalRegistry { ... }
    pub fn http_registry(&self) -> CentralizedRegistry { ... }
    pub fn p2p_network(&self, nodes: usize) -> Vec<P2PRegistry> { ... }
}
```

**Test Fixtures** (`common/fixtures.rs`):
```rust
pub fn sample_package() -> Package { ... }
pub fn large_registry(count: usize) -> Vec<Package> { ... }
pub fn p2p_config(node_id: usize) -> P2PConfig { ... }
```

**Custom Assertions** (`common/assertions.rs`):
```rust
pub fn assert_package_installed(path: &Path, package_id: &PackageId) { ... }
pub fn assert_signature_valid(package: &Package, verifier: &Ed25519Verifier) { ... }
pub fn assert_index_consistent(registry: &LocalRegistry) { ... }
```

---

## Best Practices

### 1. Use Real Dependencies
```rust
// ✅ GOOD: Real container
let postgres = PostgresContainer::builder()
    .with_version("16")
    .build()?;

// ❌ BAD: Mock
let mock_db = MockDatabase::new();
```

### 2. Deterministic Tests
```rust
// ✅ GOOD: Deterministic seed
let cleanroom = Cleanroom::builder()
    .with_determinism(42)
    .build()?;

// ❌ BAD: Time-dependent
let package = Package::new(Utc::now());
```

### 3. Clear Test Names
```rust
// ✅ GOOD: Descriptive name
#[test]
fn test_local_registry_search_returns_matching_packages() { ... }

// ❌ BAD: Vague name
#[test]
fn test_search() { ... }
```

### 4. Fast Tests
```rust
// ✅ GOOD: Minimal setup
let registry = LocalRegistry::new(temp_dir())?;

// ❌ BAD: Unnecessary setup
setup_entire_system();
wait_for_services();
populate_database();
```

### 5. Proper Cleanup
```rust
// ✅ GOOD: Automatic cleanup with cleanroom
let cleanroom = Cleanroom::builder().build()?;
// Cleanup handled automatically

// ❌ BAD: Manual cleanup
let container = start_container()?;
// ... test code ...
cleanup(container)?; // May not run if test panics
```

---

## Appendix A: Clnrm API Reference

### Container Types

**GenericContainer**:
```rust
GenericContainer::builder()
    .with_image("alpine:latest")
    .with_volume("/data")
    .with_env("KEY", "value")
    .with_network("test-net")
    .build()?
```

**PostgresContainer**:
```rust
PostgresContainer::builder()
    .with_version("16")
    .with_database("testdb")
    .build()?
```

**RedisContainer**:
```rust
RedisContainer::builder()
    .with_version("7")
    .build()?
```

**HttpContainer**:
```rust
HttpContainer::builder()
    .with_port(8080)
    .with_route("/api", handler)
    .build()?
```

### Policy Configurations

**Strict Isolation**:
```rust
Policy::strict_isolation()
    .with_network_disabled()
    .with_filesystem_isolation()
    .with_resource_limits(cpu: 1.0, memory: 512MB)
```

**Network Isolated**:
```rust
Policy::network_isolated()
    .with_custom_network("test-net")
    .with_dns_disabled()
```

**Production-Like**:
```rust
Policy::production_like()
    .with_resource_monitoring()
    .with_timeout(Duration::from_secs(300))
```

---

## Appendix B: Example Test Suite

See `/Users/sac/ggen/ggen-marketplace/tests/integration/` for complete examples.

---

## Coordination Notes

**Memory Keys**:
- `architecture/marketplace-test-strategy` - This document
- `swarm/architecture/test-scenarios` - Test scenario catalog
- `swarm/architecture/container-mapping` - Clnrm container mappings
- `swarm/architecture/coverage-matrix` - Coverage tracking

**Related Documents**:
- `ggen-marketplace/tests/80_20_TESTING_STRATEGY.md` - Overall testing philosophy
- `ggen-core/tests/README.md` - Existing test suite documentation
- `docs/testing/comprehensive-test-suite-summary.md` - Test summary

**Swarm Coordination**:
- Research Specialist: Review clnrm capabilities
- Implementation Lead: Implement test infrastructure
- QA Validator: Verify test coverage
- Documentation Writer: Complete test documentation

---

**Status**: Ready for Implementation
**Next**: Research Specialist to review and provide feedback
**Contact**: Marketplace Testing Architect (via memory)
