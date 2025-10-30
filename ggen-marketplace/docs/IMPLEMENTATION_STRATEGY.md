# Implementation Strategy: Missing Features for ggen-marketplace

**Status**: Strategic Planning Complete ✅
**Queen Coordinator**: Active and Sovereign
**Methodology**: London TDD (Test-Driven Development, London School - mock-first)
**Date**: 2025-10-14

---

## 🎯 Executive Summary

This document outlines the strategic implementation plan for the remaining 2 critical features in ggen-marketplace (Ed25519 was already completed):

1. **Ed25519 Cryptographic Verification** ✅ **COMPLETED**
2. **P2P Registry with libp2p** ⏳ **PENDING**
3. **GraphQL API with async-graphql** ⏳ **PENDING**

---

## 📊 Current Status

### Feature 1: Ed25519 Cryptographic Verification ✅ COMPLETED

**Completion**: 100% (completed by user)

**Implemented**:
- ✅ `ed25519-dalek` dependency added to Cargo.toml
- ✅ `Ed25519Verifier::sign()` implementation
- ✅ `Ed25519Verifier::verify()` implementation
- ✅ `Ed25519Verifier::generate_keypair()` implementation
- ✅ PEM import/export for public keys
- ✅ Comprehensive unit tests (11 tests)
- ✅ Zero `.unwrap()`/`.expect()` in production code
- ✅ Full error handling with `Result<T>`

**Files**:
- `src/crypto/ed25519.rs` - Complete implementation
- Tests embedded in same file

---

### Feature 2: GraphQL API with async-graphql ⏳ PENDING

**Completion**: 0%
**Priority**: HIGH
**Estimated Time**: 3-4 hours

#### Dependencies (Already Added)
```toml
async-graphql = { version = "7.0", optional = true }
async-graphql-axum = { version = "7.0", optional = true }
axum = { version = "0.7", optional = true }
tower = { version = "0.4", optional = true }
tower-http = { version = "0.5", features = ["cors"], optional = true }
```

#### Tasks Breakdown

**Phase 1: Schema Definition** (1 hour)
- [ ] Create `src/graphql/mod.rs` with module structure
- [ ] Create `src/graphql/types.rs` with GraphQL object types
  - `PackageObject` (maps to `Package`)
  - `QueryObject` (maps to `Query`)
  - `RegistryMetadataObject`
- [ ] Create `src/graphql/schema.rs` with root schema
  - `QueryRoot` with search, getPackage, listVersions
  - `MutationRoot` with publish, delete
  - `SubscriptionRoot` (optional) for real-time updates

**Phase 2: Resolvers Implementation** (1.5 hours)
- [ ] Create `src/graphql/resolvers.rs` with resolver logic
- [ ] Implement query resolvers:
  - `search(query: String, limit: Int) -> [Package]`
  - `getPackage(id: String) -> Package`
  - `listVersions(id: String) -> [Package]`
- [ ] Implement mutation resolvers:
  - `publish(package: PackageInput!) -> Package`
  - `delete(id: String, version: String) -> Boolean`
- [ ] Add error handling for all resolvers

**Phase 3: Tests (London TDD)** (1 hour)
- [ ] Create `tests/graphql_api_test.rs`
- [ ] Write tests with mock registry (mockall)
- [ ] Test all queries with various inputs
- [ ] Test all mutations with success/failure cases
- [ ] Test error handling

**Phase 4: Example Server** (0.5 hours)
- [ ] Create `examples/graphql_server.rs`
- [ ] Set up Axum with async-graphql
- [ ] Add CORS support
- [ ] Add GraphQL playground UI
- [ ] Document usage

#### Success Criteria
- [ ] All GraphQL queries return correct data
- [ ] Mutations modify registry state correctly
- [ ] Error handling is comprehensive
- [ ] Mock-based tests cover all resolvers
- [ ] Example server runs and is accessible
- [ ] Zero `.unwrap()`/`.expect()` in production code

---

### Feature 3: P2P Registry with libp2p ⏳ PENDING

**Completion**: 0%
**Priority**: HIGH
**Estimated Time**: 4-5 hours

#### Dependencies (Already Added)
```toml
libp2p = { version = "0.56", features = ["tcp", "noise", "mplex", "yamux", "gossipsub", "kad", "identify", "tokio"] }
```

#### Tasks Breakdown

**Phase 1: Core P2P Infrastructure** (2 hours)
- [ ] Create `src/p2p/mod.rs` with module structure
- [ ] Create `src/p2p/behaviour.rs` with libp2p behaviour
  - Kademlia DHT for package discovery
  - Gossipsub for package announcements
  - Identify protocol
- [ ] Create `src/p2p/network.rs` with network management
  - Swarm setup with TCP transport
  - Noise encryption
  - Mplex/Yamux multiplexing
- [ ] Create `src/p2p/config.rs` for configuration

**Phase 2: P2P Registry Implementation** (1.5 hours)
- [ ] Create `src/p2p/registry.rs` implementing `Registry` trait
- [ ] Implement DHT-based package discovery:
  - `publish()` - Store package metadata in DHT
  - `search()` - Query DHT for packages
  - `get_package()` - Retrieve from DHT
- [ ] Implement gossipsub announcements:
  - Announce new packages to network
  - Subscribe to package updates
  - Handle incoming announcements
- [ ] Add content routing for package content

**Phase 3: WASM Plugin Interface** (1 hour)
- [ ] Create `src/plugins/p2p_plugin.rs`
- [ ] Define WASM-compatible P2P interface
- [ ] Implement plugin registration
- [ ] Add example WASM plugin

**Phase 4: Tests (London TDD)** (1.5 hours)
- [ ] Create `tests/p2p_registry_test.rs`
- [ ] Create mock network layer (in-memory transport)
- [ ] Test DHT discovery operations
- [ ] Test gossipsub announcements
- [ ] Test multi-node scenarios
- [ ] Test error handling and recovery

#### Success Criteria
- [ ] DHT-based discovery works correctly
- [ ] Gossipsub announcements propagate to peers
- [ ] WASM plugin interface is functional
- [ ] Mock network layer enables testing
- [ ] Multi-node test scenarios pass
- [ ] Zero `.unwrap()`/`.expect()` in production code

---

## 👥 Agent Coordination Plan

### 8 Specialized Agents

| Agent ID | Type | Responsibility | Priority |
|----------|------|----------------|----------|
| agent-1 | crypto-specialist | Ed25519 implementation | ✅ DONE |
| agent-2 | tester | Crypto tests | ✅ DONE |
| agent-3 | p2p-specialist | P2P Registry with libp2p | HIGH |
| agent-4 | tester | P2P tests (mock network) | HIGH |
| agent-5 | graphql-specialist | GraphQL API implementation | HIGH |
| agent-6 | tester | GraphQL tests (mock registry) | HIGH |
| agent-7 | integration-specialist | Cross-feature integration tests | MEDIUM |
| agent-8 | documentation-specialist | Examples and documentation | MEDIUM |

---

## 🔄 London TDD Protocol

**Philosophy**: Test-first with mock dependencies (London School)

### London TDD Steps
1. **Define interface/trait** - What should the component do?
2. **Write test with mocked dependencies** - How should it behave?
3. **Watch test fail (RED)** - Confirm test is valid
4. **Implement minimal code to pass (GREEN)** - Make it work
5. **Refactor for quality (REFACTOR)** - Make it clean
6. **Repeat** - Continue with next behavior

### Rules
- ✅ Always write tests BEFORE implementation
- ✅ Mock all external dependencies (network, filesystem, registry)
- ✅ Test behavior, not implementation details
- ✅ Each test should test ONE behavior
- ✅ Zero `.unwrap()`/`.expect()` in production code
- ✅ Use `mockall` crate for mocking traits

---

## 🔗 Integration Points

### Ed25519 → Registry
Registry uses `CryptoVerifier` trait for signature verification of packages.

### P2P → Storage
P2P registry uses `PackageStore` trait for content-addressable storage.

### GraphQL → Registry
GraphQL resolvers use `Registry` trait for all data access operations.

### All Features → WASM
All features can be exposed via WASM plugins for extensibility.

---

## ⏱️ Timeline

| Phase | Feature | Estimated Time |
|-------|---------|----------------|
| Phase 1 | ✅ Ed25519 (DONE) | 0 hours |
| Phase 2 | GraphQL API | 3-4 hours |
| Phase 3 | P2P Registry | 4-5 hours |
| Phase 4 | Integration & Docs | 1-2 hours |
| **TOTAL** | | **8-11 hours** |

---

## ✅ Quality Gates

### Code Quality
- [ ] Zero `.unwrap()`/`.expect()` in production code
- [ ] All errors properly handled with `Result<T>`
- [ ] All public APIs documented with examples
- [ ] All critical paths tested (80/20 strategy)

### Test Quality
- [ ] London TDD approach (mocks first)
- [ ] 80/20 strategy (focus on critical 20%)
- [ ] Property-based tests for invariants (where applicable)
- [ ] Integration tests for user journeys

### Production Readiness
- [ ] All features documented with examples
- [ ] No security vulnerabilities (cargo audit)
- [ ] Performance benchmarks pass
- [ ] Backward compatibility maintained

---

## 📋 Next Steps

1. **Update royal directives** - Mark Ed25519 as complete
2. **Spawn GraphQL agents** (agent-5, agent-6)
3. **Begin GraphQL implementation** using London TDD
4. **Spawn P2P agents** (agent-3, agent-4)
5. **Begin P2P implementation** using London TDD
6. **Spawn integration agents** (agent-7, agent-8)
7. **Complete integration testing and documentation**

---

## 🎯 Success Metrics

- [ ] All 3 features fully implemented
- [ ] All tests passing (>90% coverage on critical paths)
- [ ] All examples running successfully
- [ ] Documentation complete and accurate
- [ ] Zero production crashes or panics
- [ ] Performance meets or exceeds targets

---

**Prepared by**: Queen Coordinator
**Stored in**: `.swarm/memory.db` under key `implementation-strategy`
**Coordination Protocol**: Claude-Flow hooks with memory synchronization
