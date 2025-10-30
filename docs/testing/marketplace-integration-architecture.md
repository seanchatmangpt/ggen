# Marketplace Testing Integration Architecture

**Visual companion to marketplace-test-strategy.md**

## System Architecture Diagrams

### 1. Overall System Architecture

```
┌────────────────────────────────────────────────────────────────────────┐
│                        Ggen Marketplace System                          │
│                                                                          │
│  ┌──────────────────────┐                                               │
│  │   CLI Interface      │  ggen market search "rust web"               │
│  │   (User Entry)       │  ggen market add io.ggen.rust.axum           │
│  └──────────┬───────────┘  ggen market install                         │
│             │                                                            │
│             ▼                                                            │
│  ┌──────────────────────────────────────────────────────────────┐      │
│  │              Marketplace Core Library                         │      │
│  │                  (ggen-marketplace)                           │      │
│  │                                                                │      │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐       │      │
│  │  │ Centralized  │  │    Local     │  │     P2P      │       │      │
│  │  │  Registry    │  │   Registry   │  │   Registry   │       │      │
│  │  │  (HTTP/S)    │  │ (Filesystem) │  │  (libp2p)    │       │      │
│  │  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘       │      │
│  │         │                  │                  │               │      │
│  │         └──────────────────┼──────────────────┘               │      │
│  │                            │                                  │      │
│  │  ┌─────────────────────────┴────────────────────────┐        │      │
│  │  │         Storage & Search Layer                    │        │      │
│  │  │                                                    │        │      │
│  │  │  ┌────────────┐  ┌────────────┐  ┌────────────┐ │        │      │
│  │  │  │ Filesystem │  │   Memory   │  │  Tantivy   │ │        │      │
│  │  │  │   Store    │  │   Store    │  │   Search   │ │        │      │
│  │  │  │   (CAS)    │  │  (Cache)   │  │  (Index)   │ │        │      │
│  │  │  └────────────┘  └────────────┘  └────────────┘ │        │      │
│  │  └───────────────────────────────────────────────────┘        │      │
│  │                                                                │      │
│  │  ┌────────────────────────────────────────────────────────┐  │      │
│  │  │         Security & Quality Layer                        │  │      │
│  │  │                                                          │  │      │
│  │  │  ┌────────────┐  ┌────────────┐  ┌────────────┐       │  │      │
│  │  │  │  Ed25519   │  │  Quality   │  │   Plugin   │       │  │      │
│  │  │  │  Verifier  │  │  Scoring   │  │   System   │       │  │      │
│  │  │  └────────────┘  └────────────┘  └────────────┘       │  │      │
│  │  └────────────────────────────────────────────────────────┘  │      │
│  │                                                                │      │
│  │  ┌────────────────────────────────────────────────────────┐  │      │
│  │  │            Advanced Features                            │  │      │
│  │  │                                                          │  │      │
│  │  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌────────┐│  │      │
│  │  │  │GraphQL   │  │  Cache   │  │Recommend │  │ Stats  ││  │      │
│  │  │  │   API    │  │ (Moka)   │  │  Engine  │  │        ││  │      │
│  │  │  └──────────┘  └──────────┘  └──────────┘  └────────┘│  │      │
│  │  └────────────────────────────────────────────────────────┘  │      │
│  └────────────────────────────────────────────────────────────────┘    │
└────────────────────────────────────────────────────────────────────────┘
```

### 2. Clnrm Testing Infrastructure

```
┌────────────────────────────────────────────────────────────────────────┐
│                   Clnrm Hermetic Test Environment                       │
│                                                                          │
│  ┌──────────────────────────────────────────────────────────────┐      │
│  │                  Test Orchestration Layer                     │      │
│  │                                                                │      │
│  │  ┌────────────────────────────────────────────────────────┐  │      │
│  │  │  Test Suite (Rust #[test])                             │  │      │
│  │  │  - integration/local_registry.rs                        │  │      │
│  │  │  - integration/centralized_registry.rs                  │  │      │
│  │  │  - integration/p2p_network.rs                           │  │      │
│  │  │  - integration/cli_commands.rs                          │  │      │
│  │  └────────────────────────────────────────────────────────┘  │      │
│  │                         │                                     │      │
│  │                         ▼                                     │      │
│  │  ┌────────────────────────────────────────────────────────┐  │      │
│  │  │  Cleanroom Builder API                                 │  │      │
│  │  │  - Cleanroom::builder()                                │  │      │
│  │  │  - with_policy(Policy::strict_isolation())             │  │      │
│  │  │  - with_determinism(42)                                │  │      │
│  │  └────────────────────────────────────────────────────────┘  │      │
│  └──────────────────┬───────────────────────────────────────────┘      │
│                     │                                                    │
│                     ▼                                                    │
│  ┌──────────────────────────────────────────────────────────────┐      │
│  │              Container Management Layer                       │      │
│  │                                                                │      │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐    │      │
│  │  │ Generic  │  │  HTTP    │  │PostgreSQL│  │  Redis   │    │      │
│  │  │Container │  │Container │  │Container │  │Container │    │      │
│  │  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘    │      │
│  │       │             │             │             │            │      │
│  │       └─────────────┴─────────────┴─────────────┘            │      │
│  │                     │                                         │      │
│  │                     ▼                                         │      │
│  │  ┌────────────────────────────────────────────────────────┐  │      │
│  │  │  Testcontainers-rs Backend                             │  │      │
│  │  │  - Docker daemon communication                          │  │      │
│  │  │  - Container lifecycle management                       │  │      │
│  │  │  - Network isolation                                    │  │      │
│  │  │  - Volume management                                    │  │      │
│  │  └────────────────────────────────────────────────────────┘  │      │
│  └──────────────────┬───────────────────────────────────────────┘      │
│                     │                                                    │
│                     ▼                                                    │
│  ┌──────────────────────────────────────────────────────────────┐      │
│  │                    Docker Engine                              │      │
│  │                                                                │      │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐    │      │
│  │  │Container │  │Container │  │Container │  │Container │    │      │
│  │  │  (Node1) │  │  (Node2) │  │  (HTTP)  │  │  (Store) │    │      │
│  │  │          │  │          │  │          │  │          │    │      │
│  │  │ P2P Node │  │ P2P Node │  │ Registry │  │ Storage  │    │      │
│  │  └──────────┘  └──────────┘  └──────────┘  └──────────┘    │      │
│  │                                                                │      │
│  │  ┌────────────────────────────────────────────────────────┐  │      │
│  │  │  Isolated Networks:                                    │  │      │
│  │  │  - p2p-test-net                                        │  │      │
│  │  │  - http-test-net                                       │  │      │
│  │  │  - storage-test-net                                    │  │      │
│  │  └────────────────────────────────────────────────────────┘  │      │
│  └──────────────────────────────────────────────────────────────┘      │
│                                                                          │
│  ┌──────────────────────────────────────────────────────────────┐      │
│  │              Cleanup & Resource Management                    │      │
│  │                                                                │      │
│  │  - Automatic container shutdown                               │      │
│  │  - Volume cleanup                                             │      │
│  │  - Network cleanup                                            │      │
│  │  - Resource limits enforcement                                │      │
│  │  - Panic-safe cleanup (Drop trait)                            │      │
│  └──────────────────────────────────────────────────────────────┘      │
└────────────────────────────────────────────────────────────────────────┘
```

### 3. Test Execution Flow

```
┌────────────────────────────────────────────────────────────────────────┐
│                      Test Execution Timeline                            │
└────────────────────────────────────────────────────────────────────────┘

Time: 0s
│
├─ Test Start
│  └─ Cleanroom::builder().build()
│     └─ Allocate resources
│        └─ Create isolated networks
│
Time: 1-2s
│
├─ Container Startup
│  ├─ Pull images (if not cached)
│  ├─ Start containers
│  │  ├─ GenericContainer (filesystem)
│  │  ├─ HttpContainer (registry server)
│  │  └─ Multiple GenericContainers (P2P nodes)
│  │
│  └─ Wait for readiness
│     ├─ Health checks
│     └─ Service availability
│
Time: 3-5s
│
├─ Test Setup
│  ├─ Inject test data
│  │  ├─ Create test packages
│  │  ├─ Initialize indexes
│  │  └─ Setup configurations
│  │
│  └─ Verify environment
│     ├─ Network connectivity
│     └─ Storage accessibility
│
Time: 5-10s
│
├─ Test Execution
│  ├─ Execute marketplace operations
│  │  ├─ CLI commands
│  │  │  └─ ggen market search "rust web"
│  │  │     ggen market add io.ggen.rust.axum
│  │  │     ggen market install
│  │  │
│  │  ├─ API calls
│  │  │  └─ registry.search(&query).await
│  │  │     registry.publish(package).await
│  │  │     registry.get_package(&id).await
│  │  │
│  │  └─ P2P operations
│  │     └─ DHT lookup
│  │        Gossipsub announce
│  │        Peer discovery
│  │
│  ├─ Collect results
│  │  ├─ Command output
│  │  ├─ API responses
│  │  └─ State changes
│  │
│  └─ Perform assertions
│     ├─ assert_eq!(results.len(), expected)
│     ├─ assert!(package_installed)
│     └─ verify_signature_valid()
│
Time: 10-15s
│
├─ Test Verification
│  ├─ Verify state changes
│  │  ├─ Filesystem state
│  │  ├─ Index consistency
│  │  └─ Network state
│  │
│  ├─ Check invariants
│  │  ├─ Data integrity
│  │  ├─ Security properties
│  │  └─ Performance metrics
│  │
│  └─ Validate error handling
│     ├─ Expected errors
│     └─ Recovery behavior
│
Time: 15-20s
│
├─ Cleanup (Automatic)
│  ├─ Stop containers
│  │  └─ Graceful shutdown
│  │
│  ├─ Remove volumes
│  │  └─ Delete temporary data
│  │
│  ├─ Remove networks
│  │  └─ Cleanup isolated networks
│  │
│  └─ Report metrics
│     ├─ Execution time
│     ├─ Resource usage
│     └─ Coverage data
│
Time: 20s
│
└─ Test Complete
   └─ Result: PASS/FAIL
```

### 4. Container Topology for P2P Tests

```
┌────────────────────────────────────────────────────────────────────────┐
│                    P2P Network Test Topology                            │
│                                                                          │
│                    (5-Node Mesh Network)                                │
│                                                                          │
│                                                                          │
│                          Node 0                                         │
│                      (Bootstrap Node)                                   │
│                   ┌──────────────────┐                                  │
│                   │  P2PRegistry     │                                  │
│                   │  Port: 4001      │                                  │
│                   │  ID: 12D3KooW... │                                  │
│                   └─────┬────────┬───┘                                  │
│                         │        │                                      │
│            ┌────────────┘        └────────────┐                         │
│            │                                   │                         │
│            ▼                                   ▼                         │
│      ┌──────────┐                        ┌──────────┐                   │
│      │  Node 1  │◄──────────────────────►│  Node 2  │                   │
│      │  Peer    │                        │  Peer    │                   │
│      └────┬─────┘                        └─────┬────┘                   │
│           │                                    │                         │
│           │          ┌──────────┐              │                         │
│           │          │  Node 3  │              │                         │
│           └─────────►│  Peer    │◄─────────────┘                         │
│                      └────┬─────┘                                        │
│                           │                                              │
│                           ▼                                              │
│                      ┌──────────┐                                        │
│                      │  Node 4  │                                        │
│                      │  Peer    │                                        │
│                      └──────────┘                                        │
│                                                                          │
│  Network: p2p-test-net                                                  │
│  Protocol: libp2p (Kademlia + Gossipsub)                                │
│  Isolation: Complete (no external network)                              │
│                                                                          │
│  Test Flow:                                                             │
│  1. Start Node 0 (bootstrap)                                            │
│  2. Start Nodes 1-4 (connect to Node 0)                                 │
│  3. Wait for mesh formation (5s)                                        │
│  4. Publish package on Node 0                                           │
│  5. Verify propagation to all nodes                                     │
│  6. Test DHT lookup from Node 4                                         │
│  7. Verify peer reputation tracking                                     │
│                                                                          │
└────────────────────────────────────────────────────────────────────────┘
```

### 5. Test Data Flow

```
┌────────────────────────────────────────────────────────────────────────┐
│                         Test Data Flow                                  │
│                                                                          │
│  1. Test Fixtures (Static)                                              │
│     ┌──────────────────────────────────────────────────┐               │
│     │  tests/common/fixtures.rs                        │               │
│     │                                                   │               │
│     │  fn sample_package() -> Package                  │               │
│     │  fn large_registry(n: usize) -> Vec<Package>    │               │
│     │  fn p2p_config(node_id: usize) -> P2PConfig     │               │
│     └──────────────────────────────────────────────────┘               │
│                            │                                             │
│                            ▼                                             │
│  2. Container Setup                                                     │
│     ┌──────────────────────────────────────────────────┐               │
│     │  Cleanroom                                        │               │
│     │  ├─ GenericContainer: /marketplace               │               │
│     │  │  └─ Inject: sample_package.json               │               │
│     │  │                                                │               │
│     │  ├─ HttpContainer: :8080                         │               │
│     │  │  └─ Serve: index.json (large_registry)        │               │
│     │  │                                                │               │
│     │  └─ P2P Nodes: [Node0..Node4]                    │               │
│     │     └─ Config: p2p_config(i)                     │               │
│     └──────────────────────────────────────────────────┘               │
│                            │                                             │
│                            ▼                                             │
│  3. Test Operations                                                     │
│     ┌──────────────────────────────────────────────────┐               │
│     │  Test Code                                        │               │
│     │                                                   │               │
│     │  let registry = env.local_registry();            │               │
│     │  let results = registry.search(&query).await?;   │               │
│     │  let package = registry.get_package(&id).await?; │               │
│     └──────────────────────────────────────────────────┘               │
│                            │                                             │
│                            ▼                                             │
│  4. Verification                                                        │
│     ┌──────────────────────────────────────────────────┐               │
│     │  Assertions                                       │               │
│     │                                                   │               │
│     │  assert_eq!(results.len(), expected);            │               │
│     │  assert_package_installed(&path, &id);           │               │
│     │  assert_signature_valid(&pkg, &verifier);        │               │
│     └──────────────────────────────────────────────────┘               │
│                            │                                             │
│                            ▼                                             │
│  5. Cleanup                                                             │
│     ┌──────────────────────────────────────────────────┐               │
│     │  Automatic by Cleanroom                          │               │
│     │  - Stop containers                                │               │
│     │  - Remove volumes                                 │               │
│     │  - Cleanup networks                               │               │
│     └──────────────────────────────────────────────────┘               │
│                                                                          │
└────────────────────────────────────────────────────────────────────────┘
```

## Component Interaction Matrix

### LocalRegistry Test Interactions

| Test | Components | Containers | Verification |
|------|-----------|-----------|--------------|
| Search Flow | LocalRegistry, TantivySearch, FilesystemStore | Generic (fs) | Results match query |
| Publish Flow | LocalRegistry, Ed25519Verifier, FilesystemStore | Generic (fs) | Package stored correctly |
| Install Flow | LocalRegistry, FilesystemStore, CLI | Generic (fs) | Files extracted properly |
| Concurrent Access | LocalRegistry, file locks | Generic (shared) | No corruption |

### CentralizedRegistry Test Interactions

| Test | Components | Containers | Verification |
|------|-----------|-----------|--------------|
| HTTP Fetch | CentralizedRegistry, HTTP client | HTTP server | Data fetched correctly |
| Retry Logic | CentralizedRegistry, retry policy | HTTP (flaky) | Retries work |
| Cache Behavior | CentralizedRegistry, Moka cache | HTTP + Generic | Cache hits work |
| Fallback | CentralizedRegistry, LocalRegistry | HTTP (down) + Generic | Falls back to local |

### P2PRegistry Test Interactions

| Test | Components | Containers | Verification |
|------|-----------|-----------|--------------|
| DHT Lookup | P2PRegistry, Kademlia | Multiple Generic | Package found |
| Gossipsub | P2PRegistry, Gossipsub | Multiple Generic | Messages propagate |
| Peer Discovery | P2PRegistry, identify | Multiple Generic | Peers discovered |
| Reputation | P2PRegistry, reputation tracker | Multiple Generic | Bad peers avoided |

## Test Isolation Levels

### Level 1: Unit Test (No Containers)
```rust
#[test]
fn test_package_id_validation() {
    let id = PackageId::new("io.ggen", "sample");
    assert_eq!(id.namespace, "io.ggen");
}
```
- **Isolation**: Process-level
- **Speed**: <1ms
- **Complexity**: Low
- **Use**: Pure logic, data structures

### Level 2: Integration Test (Single Container)
```rust
#[test]
fn test_local_registry_search() -> Result<()> {
    let cleanroom = Cleanroom::builder().build()?;
    let container = cleanroom.start_generic()?;
    // Test with real filesystem
    Ok(())
}
```
- **Isolation**: Container-level
- **Speed**: 1-5s
- **Complexity**: Medium
- **Use**: Single component, real storage

### Level 3: Integration Test (Multiple Containers)
```rust
#[test]
fn test_p2p_network() -> Result<()> {
    let cleanroom = Cleanroom::builder().build()?;
    let nodes = cleanroom.start_p2p_network(5)?;
    // Test distributed behavior
    Ok(())
}
```
- **Isolation**: Network-level
- **Speed**: 5-15s
- **Complexity**: High
- **Use**: Distributed systems, P2P

### Level 4: End-to-End Test (CLI + Containers)
```rust
#[test]
fn test_cli_complete_workflow() -> Result<()> {
    let cleanroom = Cleanroom::builder().build()?;
    let env = cleanroom.setup_marketplace()?;
    // Run actual CLI commands
    Command::new("ggen")
        .args(&["market", "search", "rust"])
        .output()?;
    Ok(())
}
```
- **Isolation**: Full system
- **Speed**: 10-30s
- **Complexity**: Very High
- **Use**: User workflows, acceptance tests

## Error Injection Scenarios

### Network Failures
```rust
let http = HttpContainer::builder()
    .with_failure_rate(0.5)  // 50% requests fail
    .with_latency(Duration::from_secs(5))
    .build()?;
```

### Corrupted Data
```rust
let fs = GenericContainer::builder()
    .with_corrupted_file("/marketplace/index.json")
    .build()?;
```

### Resource Exhaustion
```rust
let policy = Policy::strict_isolation()
    .with_memory_limit("100MB")
    .with_cpu_limit(0.5);
```

### Concurrent Conflicts
```rust
let registry = env.local_registry();
let handles: Vec<_> = (0..10)
    .map(|i| {
        tokio::spawn(async move {
            registry.publish(package.clone()).await
        })
    })
    .collect();
```

## Coverage Tracking

### Critical Path Coverage Map

```
┌─────────────────────────────────────────────────────────────┐
│              Coverage Map (Target: 100%)                     │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  LocalRegistry                  [████████████████] 100%      │
│  ├─ search()                    [████████████████] 100%      │
│  ├─ publish()                   [████████████████] 100%      │
│  ├─ get_package()               [████████████████] 100%      │
│  └─ list_versions()             [████████████████] 100%      │
│                                                               │
│  CentralizedRegistry            [████████████░░░░]  85%      │
│  ├─ fetch()                     [████████████████] 100%      │
│  ├─ retry_logic()               [████████████████] 100%      │
│  ├─ cache_behavior()            [████████████░░░░]  80%      │
│  └─ fallback()                  [██████████░░░░░░]  60%      │
│                                                               │
│  P2PRegistry                    [████████░░░░░░░░]  50%      │
│  ├─ dht_lookup()                [████████████░░░░]  75%      │
│  ├─ gossipsub_announce()        [██████░░░░░░░░░░]  40%      │
│  ├─ peer_discovery()            [████████████░░░░]  75%      │
│  └─ reputation_tracking()       [░░░░░░░░░░░░░░░░]   0%      │
│                                                               │
│  FilesystemStore                [████████████████] 100%      │
│  Ed25519Verifier                [████████████████] 100%      │
│  TantivySearch                  [██████████████░░]  90%      │
│  CLI Commands                   [████████░░░░░░░░]  50%      │
│  GraphQL API                    [░░░░░░░░░░░░░░░░]   0%      │
│                                                               │
│  Overall Coverage:              [████████████░░░░]  78%      │
│  Target:                        [████████████████]  85%      │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Next Steps

1. **Implement LocalRegistry Tests** (Week 1)
   - Setup test infrastructure
   - Create 10 integration tests
   - Achieve 100% critical path coverage

2. **Add HTTP Testing** (Week 2)
   - Setup HTTP containers
   - Implement retry/fallback tests
   - Add network failure simulation

3. **P2P Network Tests** (Week 3)
   - Multi-container setup
   - Distributed test scenarios
   - Performance baselines

4. **CLI & API Tests** (Week 4)
   - End-to-end workflows
   - GraphQL integration
   - Performance benchmarks

5. **Polish & CI/CD** (Week 5)
   - Complete documentation
   - CI/CD integration
   - Production readiness

---

**Related Documents**:
- `/Users/sac/ggen/docs/testing/marketplace-test-strategy.md` - Main strategy document
- `/Users/sac/ggen/ggen-marketplace/tests/80_20_TESTING_STRATEGY.md` - Testing philosophy
- `/Users/sac/ggen/ggen-core/tests/README.md` - Existing tests

**Status**: Ready for Implementation
