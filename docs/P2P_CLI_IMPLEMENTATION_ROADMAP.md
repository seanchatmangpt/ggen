# P2P CLI Implementation Roadmap

**Project:** ggen Marketplace P2P Integration v2.3.0
**Architect:** System Architecture Agent (Hive Mind)
**Date:** 2025-11-02
**Status:** READY FOR IMPLEMENTATION

---

## Executive Summary

This roadmap provides a phased implementation plan for integrating P2P marketplace functionality into the ggen CLI using clap-noun-verb v3.0.0 architecture. The implementation is structured into 7 phases over an estimated 4-6 weeks.

**Total Deliverables:**
- 📁 12+ new source files
- 🧪 50+ unit tests
- 🔬 20+ integration tests
- 📚 3 architecture documents
- 🎯 9 CLI commands
- ⚙️ 1 configuration system

---

## Phase Breakdown

### Phase 1: Foundation (Week 1, Days 1-2)

**Goal:** Set up domain layer structure and node manager

#### Tasks

1. **Create directory structure** ✅ PRIORITY: HIGH
   ```bash
   mkdir -p cli/src/domain/marketplace/p2p
   touch cli/src/domain/marketplace/p2p/mod.rs
   touch cli/src/domain/marketplace/p2p/node_manager.rs
   ```

2. **Implement NodeManager** ✅ PRIORITY: HIGH
   - File: `cli/src/domain/marketplace/p2p/node_manager.rs`
   - Features:
     - Global state management (`OnceCell + RwLock`)
     - `set_global_registry()`
     - `get_global_registry()`
     - `is_running()`
     - `stop_global_registry()`
   - Tests:
     - Unit tests for lifecycle
     - Thread safety tests
     - Error handling tests

3. **Create mod.rs exports** ✅ PRIORITY: HIGH
   - File: `cli/src/domain/marketplace/p2p/mod.rs`
   - Export all submodules
   - Export common types
   - Re-export node_manager

4. **Update parent modules** ✅ PRIORITY: HIGH
   - File: `cli/src/domain/marketplace/mod.rs`
   - Add `pub mod p2p;`
   - Export P2P types if needed

#### Deliverables
- ✅ Domain layer structure
- ✅ Node manager with tests
- ✅ Module exports configured

#### Acceptance Criteria
- [ ] `cargo build` succeeds
- [ ] `cargo test node_manager` passes
- [ ] Imports work from parent modules

---

### Phase 2: Configuration System (Week 1, Days 3-4)

**Goal:** Implement P2P configuration management

#### Tasks

1. **Implement config.rs** ✅ PRIORITY: HIGH
   - File: `cli/src/domain/marketplace/p2p/config.rs`
   - Types:
     - `P2PConfigFile` struct
     - `NetworkConfig` struct
     - `PerformanceConfig` struct
     - `StorageConfig` struct
     - `SecurityConfig` struct
   - Functions:
     - `load()` - Load from ~/.ggen/p2p-config.toml
     - `save()` - Save to disk
     - `validate()` - Validate configuration
     - `default()` - Sensible defaults
   - Commands:
     - `ConfigCmd::Show`
     - `ConfigCmd::Edit`
     - `ConfigCmd::Reset`
     - `ConfigCmd::Validate`

2. **Create default config** ✅ PRIORITY: MEDIUM
   - Bootstrap nodes list (use public IPFS nodes)
   - Default listen addresses
   - Reasonable performance limits
   - Security defaults

3. **Add config tests** ✅ PRIORITY: MEDIUM
   - Test TOML serialization/deserialization
   - Test validation logic
   - Test default values
   - Test error handling

#### Deliverables
- ✅ Configuration system
- ✅ TOML parser integration
- ✅ Config command handlers
- ✅ Unit tests

#### Acceptance Criteria
- [ ] Can create default config
- [ ] Can load and save config
- [ ] Validation catches errors
- [ ] `ggen marketplace p2p config show` works

---

### Phase 3: Core P2P Commands (Week 1, Days 5-7)

**Goal:** Implement start, status, and basic node operations

#### Tasks

1. **Implement start.rs** ✅ PRIORITY: HIGH
   - File: `cli/src/domain/marketplace/p2p/start.rs`
   - Types: `StartArgs`
   - Functions:
     - `run()` - Sync wrapper
     - `start_async()` - Async implementation
   - Features:
     - Parse command arguments
     - Load configuration
     - Create P2PRegistry
     - Start listening
     - Subscribe to topics
     - Bootstrap DHT
     - Store in NodeManager
     - Optional daemon mode

2. **Implement status.rs** ✅ PRIORITY: HIGH
   - File: `cli/src/domain/marketplace/p2p/status.rs`
   - Types: `StatusArgs`
   - Functions:
     - `run()` - Display status
     - Format peer info
     - Show network stats
   - Output:
     - Peer ID
     - Uptime
     - Connected peers
     - DHT status
     - Gossipsub topics
     - Activity metrics

3. **Implement disconnect.rs** ✅ PRIORITY: MEDIUM
   - File: `cli/src/domain/marketplace/p2p/disconnect.rs`
   - Types: `DisconnectArgs`
   - Functions:
     - Stop node gracefully
     - Clear global state
     - Cleanup resources

4. **Update CLI layer** ✅ PRIORITY: HIGH
   - File: `cli/src/cmds/marketplace.rs`
   - Add `P2pCmd` enum
   - Add command variants
   - Wire up routing

#### Deliverables
- ✅ Start command implementation
- ✅ Status command implementation
- ✅ Disconnect command
- ✅ CLI routing updated

#### Acceptance Criteria
- [ ] `ggen marketplace p2p start` works
- [ ] `ggen marketplace p2p status` displays info
- [ ] `ggen marketplace p2p disconnect` stops node
- [ ] Integration test passes

---

### Phase 4: Search & Discovery (Week 2, Days 1-3)

**Goal:** Implement P2P package search functionality

#### Tasks

1. **Implement search.rs** ✅ PRIORITY: HIGH
   - File: `cli/src/domain/marketplace/p2p/search.rs`
   - Types: `SearchArgs`
   - Functions:
     - `run()` - Execute search
     - `search_async()` - Query P2P network
     - Format results
   - Features:
     - Query local cache
     - Query DHT
     - Listen for gossipsub announcements
     - Filter and rank results
     - Display formatted output

2. **Update unified search** ✅ PRIORITY: HIGH
   - File: `cli/src/domain/marketplace/search.rs`
   - Add `--registry p2p|central|all` flag
   - Implement parallel search
   - Deduplicate results
   - Merge and rank

3. **Implement peers.rs** ✅ PRIORITY: MEDIUM
   - File: `cli/src/domain/marketplace/p2p/peers.rs`
   - Types: `PeersArgs`
   - Functions:
     - List connected peers
     - Show reputation scores
     - Display latency info

4. **Implement bootstrap.rs** ✅ PRIORITY: MEDIUM
   - File: `cli/src/domain/marketplace/p2p/bootstrap.rs`
   - Types: `BootstrapArgs`
   - Functions:
     - Manual DHT bootstrap
     - Add bootstrap nodes
     - Wait for completion

#### Deliverables
- ✅ P2P search command
- ✅ Unified marketplace search
- ✅ Peers command
- ✅ Bootstrap command

#### Acceptance Criteria
- [ ] `ggen marketplace p2p search "query"` returns results
- [ ] `ggen marketplace search --registry p2p` works
- [ ] Parallel search deduplicates correctly
- [ ] `ggen marketplace p2p peers` shows peer list

---

### Phase 5: Publishing (Week 2, Days 4-5)

**Goal:** Implement package publishing to P2P network

#### Tasks

1. **Implement publish.rs** ✅ PRIORITY: HIGH
   - File: `cli/src/domain/marketplace/p2p/publish.rs`
   - Types: `PublishArgs`
   - Functions:
     - `run()` - Publish package
     - `publish_async()` - P2P publishing
   - Features:
     - Read package manifest
     - Validate package
     - Calculate content hash
     - Store in DHT
     - Announce via gossipsub
     - Cache locally

2. **Package verification** ✅ PRIORITY: HIGH
   - Verify package structure
   - Check manifest validity
   - Validate dependencies
   - Sign metadata (future: GPG)

3. **Error handling** ✅ PRIORITY: MEDIUM
   - Graceful failure messages
   - Retry logic for network errors
   - Actionable suggestions

#### Deliverables
- ✅ Publish command
- ✅ Package validation
- ✅ DHT storage
- ✅ Gossipsub announcements

#### Acceptance Criteria
- [ ] `ggen marketplace p2p publish ./pkg` works
- [ ] Package stored in DHT
- [ ] Other peers receive announcement
- [ ] Published package appears in search

---

### Phase 6: Peer Management (Week 3, Days 1-2)

**Goal:** Implement direct peer connections and reputation

#### Tasks

1. **Implement connect.rs** ✅ PRIORITY: MEDIUM
   - File: `cli/src/domain/marketplace/p2p/connect.rs`
   - Types: `ConnectArgs`
   - Functions:
     - Connect to specific peer
     - Validate multiaddr
     - Persistent connections

2. **Reputation tracking** ✅ PRIORITY: MEDIUM
   - Extend `PeerReputation` in ggen-marketplace
   - Track success/failure rates
   - Ban malicious peers
   - Reputation scoring

3. **Enhanced peers command** ✅ PRIORITY: LOW
   - Show reputation details
   - Sort by various metrics
   - Filter by status

#### Deliverables
- ✅ Connect command
- ✅ Reputation system
- ✅ Peer filtering

#### Acceptance Criteria
- [ ] `ggen marketplace p2p connect <peer>` establishes connection
- [ ] Reputation scores update correctly
- [ ] Low-reputation peers avoided

---

### Phase 7: Testing & Documentation (Week 3, Days 3-5)

**Goal:** Comprehensive testing and final documentation

#### Tasks

1. **Unit tests** ✅ PRIORITY: HIGH
   - Test all domain functions
   - Mock P2P registry
   - Test configuration loading
   - Test error handling
   - Target: 80% code coverage

2. **Integration tests** ✅ PRIORITY: HIGH
   - File: `cli/tests/p2p_integration.rs`
   - Test scenarios:
     - Start/stop node
     - Publish and search
     - Two nodes communication
     - Peer discovery
     - Reputation system

3. **End-to-end tests** ✅ PRIORITY: MEDIUM
   - Multi-node scenarios
   - Network partition recovery
   - Performance under load
   - Security edge cases

4. **Documentation** ✅ PRIORITY: MEDIUM
   - User guide for P2P commands
   - Configuration reference
   - Troubleshooting guide
   - Examples and tutorials

5. **Performance benchmarks** ✅ PRIORITY: LOW
   - Startup time
   - Search latency
   - DHT query performance
   - Memory usage

#### Deliverables
- ✅ 50+ unit tests
- ✅ 20+ integration tests
- ✅ 5+ end-to-end tests
- ✅ User documentation
- ✅ Performance benchmarks

#### Acceptance Criteria
- [ ] All tests pass
- [ ] 80%+ code coverage
- [ ] Documentation complete
- [ ] Performance targets met

---

## Implementation Order

### Critical Path (Must implement first)

```
Phase 1: Foundation
   │
   ├─► NodeManager (BLOCKING ALL)
   └─► Directory structure
       │
       ▼
Phase 2: Configuration
   │
   ├─► Config types
   └─► Config loading
       │
       ▼
Phase 3: Core Commands
   │
   ├─► start.rs (BLOCKING search, publish)
   ├─► status.rs
   └─► disconnect.rs
       │
       ▼
Phase 4: Search (PARALLEL)    Phase 5: Publish (PARALLEL)
   │                              │
   ├─► search.rs                  ├─► publish.rs
   ├─► peers.rs                   └─► Validation
   └─► bootstrap.rs
       │                          │
       └──────────┬───────────────┘
                  │
                  ▼
Phase 6: Peer Management
   │
   ├─► connect.rs
   └─► Reputation
       │
       ▼
Phase 7: Testing & Docs
```

### Parallelization Opportunities

- **Phase 4 & 5 can run in parallel** (both depend on Phase 3)
- **Unit tests can be written alongside implementation**
- **Documentation can start after Phase 3**

---

## File Checklist

### New Files to Create

- [ ] `cli/src/domain/marketplace/p2p/mod.rs`
- [ ] `cli/src/domain/marketplace/p2p/node_manager.rs`
- [ ] `cli/src/domain/marketplace/p2p/config.rs`
- [ ] `cli/src/domain/marketplace/p2p/start.rs`
- [ ] `cli/src/domain/marketplace/p2p/status.rs`
- [ ] `cli/src/domain/marketplace/p2p/search.rs`
- [ ] `cli/src/domain/marketplace/p2p/publish.rs`
- [ ] `cli/src/domain/marketplace/p2p/connect.rs`
- [ ] `cli/src/domain/marketplace/p2p/disconnect.rs`
- [ ] `cli/src/domain/marketplace/p2p/peers.rs`
- [ ] `cli/src/domain/marketplace/p2p/bootstrap.rs`
- [ ] `cli/tests/p2p_integration.rs`

### Files to Modify

- [ ] `cli/src/cmds/marketplace.rs` (Add P2pCmd enum)
- [ ] `cli/src/domain/marketplace/mod.rs` (Export p2p module)
- [ ] `cli/src/domain/marketplace/search.rs` (Unified search)

---

## Testing Strategy

### Unit Tests (50+)

**Module:** `node_manager.rs`
- [ ] Test global state initialization
- [ ] Test set/get registry
- [ ] Test is_running
- [ ] Test concurrent access
- [ ] Test error when not started

**Module:** `config.rs`
- [ ] Test default config
- [ ] Test load/save
- [ ] Test TOML parsing
- [ ] Test validation rules
- [ ] Test invalid configs

**Module:** `start.rs`
- [ ] Test argument parsing
- [ ] Test config loading
- [ ] Test registry creation
- [ ] Test error handling

**Module:** `search.rs`
- [ ] Test query parsing
- [ ] Test result formatting
- [ ] Test filtering
- [ ] Test ranking

**Module:** `publish.rs`
- [ ] Test manifest parsing
- [ ] Test validation
- [ ] Test hash calculation
- [ ] Test error messages

### Integration Tests (20+)

**File:** `cli/tests/p2p_integration.rs`

```rust
#[tokio::test]
async fn test_start_and_status() {
    // Start node
    Command::cargo_bin("ggen")
        .arg("marketplace").arg("p2p").arg("start")
        .assert().success();

    // Check status
    Command::cargo_bin("ggen")
        .arg("marketplace").arg("p2p").arg("status")
        .assert().success()
        .stdout(contains("Peer ID"));
}

#[tokio::test]
async fn test_publish_and_search() {
    // Create test package
    // Publish to P2P
    // Search for package
    // Verify found
}

#[tokio::test]
async fn test_two_nodes_communication() {
    // Start node A
    // Start node B
    // A publishes package
    // B searches and finds package
}
```

### End-to-End Tests (5+)

- [ ] **Full workflow:** Start → Publish → Search → Install
- [ ] **Peer discovery:** Multiple nodes find each other
- [ ] **Reputation:** Track peer reliability
- [ ] **Network partition:** Recovery from disconnection
- [ ] **Performance:** Handle 100+ packages

---

## Performance Targets

| Metric | Target | Critical | Measurement |
|--------|--------|----------|-------------|
| Node startup | < 3s | < 5s | `time ggen marketplace p2p start` |
| Status check | < 100ms | < 500ms | `time ggen marketplace p2p status` |
| DHT query | < 5s | < 10s | Average of 100 queries |
| Package search | < 3s | < 7s | Search with 10 results |
| Package publish | < 10s | < 20s | Including DHT replication |
| Memory usage | < 50MB | < 100MB | RSS during operation |

---

## Risk Assessment

### High Risk

1. **libp2p integration complexity**
   - **Impact:** High (blocking all P2P functionality)
   - **Probability:** Medium
   - **Mitigation:** Extensive testing, reference examples, phased rollout

2. **DHT performance in real networks**
   - **Impact:** High (core search functionality)
   - **Probability:** Medium
   - **Mitigation:** Fallback to central registry, caching, timeouts

### Medium Risk

3. **Async/sync runtime issues**
   - **Impact:** Medium (architecture complexity)
   - **Probability:** Low
   - **Mitigation:** Runtime bridge pattern (proven in existing code)

4. **Peer reputation gaming**
   - **Impact:** Medium (trust system)
   - **Probability:** Medium
   - **Mitigation:** Configurable thresholds, banning, rate limiting

### Low Risk

5. **Configuration management**
   - **Impact:** Low (UX issue)
   - **Probability:** Low
   - **Mitigation:** Sensible defaults, validation, documentation

---

## Dependencies

### External Crates (Already in ggen-marketplace)
- ✅ `libp2p = "0.54"`
- ✅ `tokio = { version = "1", features = ["full"] }`
- ✅ `futures = "0.3"`
- ✅ `serde = { version = "1.0", features = ["derive"] }`
- ✅ `toml = "0.8"`
- ✅ `anyhow = "1.0"`

### Internal Dependencies
- ✅ `ggen-marketplace` (P2PRegistry already implemented)
- ✅ `ggen-utils` (error handling)
- ✅ `ggen-core` (template engine)

**No new external dependencies required!**

---

## Success Metrics

### Phase Completion Criteria

**Phase 1-3 Success:**
- [ ] Can start P2P node
- [ ] Can check status
- [ ] Can stop node gracefully
- [ ] All basic commands work

**Phase 4-5 Success:**
- [ ] Can search P2P network
- [ ] Can publish packages
- [ ] Search finds published packages
- [ ] Integration with central registry works

**Phase 6-7 Success:**
- [ ] All commands fully functional
- [ ] 80%+ test coverage
- [ ] Performance targets met
- [ ] Documentation complete
- [ ] Ready for production use

### Release Readiness Checklist

- [ ] All phases complete
- [ ] All tests passing
- [ ] Performance benchmarks pass
- [ ] Security audit complete
- [ ] Documentation reviewed
- [ ] User acceptance testing done
- [ ] Version bumped to 2.3.0
- [ ] Changelog updated
- [ ] Ready for crates.io publish

---

## Timeline

### Estimated Schedule (4-6 weeks)

**Week 1: Foundation & Core**
- Days 1-2: Phase 1 (Foundation)
- Days 3-4: Phase 2 (Configuration)
- Days 5-7: Phase 3 (Core Commands)

**Week 2: Features**
- Days 1-3: Phase 4 (Search)
- Days 4-5: Phase 5 (Publishing)

**Week 3: Advanced Features & Testing**
- Days 1-2: Phase 6 (Peer Management)
- Days 3-5: Phase 7 (Testing & Docs)

**Week 4-6: Buffer**
- Bug fixes
- Performance tuning
- Additional testing
- User feedback integration

### Velocity Assumptions

- **1 developer, full-time:** 4 weeks
- **1 developer, part-time (50%):** 8 weeks
- **2 developers, full-time:** 2-3 weeks

---

## Next Steps

### Immediate Actions (Start Today)

1. **Create branch:** `git checkout -b feature/p2p-cli-integration`

2. **Phase 1, Task 1:** Create directory structure
   ```bash
   cd cli/src/domain/marketplace
   mkdir p2p
   cd p2p
   touch mod.rs node_manager.rs
   ```

3. **Phase 1, Task 2:** Implement NodeManager skeleton
   ```rust
   // cli/src/domain/marketplace/p2p/node_manager.rs
   use once_cell::sync::OnceCell;
   // ... (see architecture doc)
   ```

4. **Verify build:** `cargo build`

5. **Create PR template** for review process

### Communication Plan

- **Daily:** Sync with hive mind via hooks
- **Weekly:** Review progress against roadmap
- **Blockers:** Escalate immediately
- **Phase completion:** Demo to stakeholders

---

## References

- 📚 **Main Architecture:** `/docs/P2P_CLI_ARCHITECTURE.md`
- 🎨 **Diagrams:** `/docs/P2P_CLI_INTEGRATION_DIAGRAM.md`
- 📋 **ADRs:** `/docs/P2P_CLI_ADR.md`
- 🔬 **P2P Backend:** `/ggen-marketplace/src/backend/p2p.rs`
- 🧪 **Integration Tests:** `/cli/tests/clap_noun_verb_integration.rs`

---

**Document Version:** 1.0.0
**Last Updated:** 2025-11-02
**Status:** APPROVED FOR IMPLEMENTATION
**Estimated Completion:** 4-6 weeks from start date
**Priority:** HIGH (Version 2.3.0 release blocker)
