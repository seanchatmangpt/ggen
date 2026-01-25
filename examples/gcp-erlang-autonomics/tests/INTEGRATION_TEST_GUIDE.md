# Comprehensive Integration Test Suite

This directory contains **7 comprehensive integration test suites** for the GCP Erlang Autonomics system, following **Chicago TDD** methodology (state-based testing with real collaborators, no mocks).

## Test Suites Overview

### 1. **Entitlement FSM Integration** (`entitlement_fsm_integration.rs`)

Tests the **Entitlement state machine** with full lifecycle coverage.

**Coverage:**
- ✅ State transitions (pending → active → paused → active → expired)
- ✅ Timeout escalations (24-hour approval deadline)
- ✅ State persistence across restarts (serialization roundtrip)
- ✅ Quota enforcement boundaries (overage detection)
- ✅ FSM invariants (only valid transitions allowed)
- ✅ Concurrent quota updates (consistency under load)
- ✅ Complex escalation chains (multi-step workflows)
- ✅ Expired entitlement read-only behavior

**8 Tests | ~100 lines each | AAA Pattern**

**Run:**
```bash
cargo test --test entitlement_fsm_integration -- --nocapture
```

### 2. **Multi-Governor Orchestration** (`multi_governor_orchestration_advanced.rs`)

Tests coordination of **8+ governors** across complete customer lifecycle.

**Coverage:**
- ✅ Customer subscribe flow (6 governors: entitlement, billing, subscription, customer, quota, compliance)
- ✅ Concurrent operations (100+ concurrent customers, all succeed)
- ✅ Billing failure rollback (entitlement reverted when payment fails)
- ✅ Payment method failover (primary → secondary on failure)
- ✅ Quota exceeded notifications (customer + potential downgrade)
- ✅ Compliance gate blocking (KYC/AML failures block subscription)
- ✅ Subscription renewal coordination (3+ governors updated atomically)
- ✅ Idempotent event processing (same event ID → same result)
- ✅ Event deduplication (duplicate prevention)
- ✅ Conflict resolution (last-write-wins timestamp strategy)
- ✅ Compensating transactions (LIFO rollback on failure)
- ✅ State consistency (50+ concurrent events, no race conditions)
- ✅ Receipt generation (audit trail with all governor actions)

**13 Tests | Real integration scenarios**

**Run:**
```bash
cargo test --test multi_governor_orchestration_advanced -- --nocapture
```

### 3. **Property-Based FSM Invariants** (`property_based_fsm_invariants.rs`)

**PropEr-style** property-based tests using `proptest` for edge cases.

**Coverage:**
- ✅ Governor state idempotence (state = state repeatedly)
- ✅ Entitlement transition DAG (valid state graph, no invalid cycles)
- ✅ Quota monotonicity (usage only increases, never decreases)
- ✅ Fair-share proportional allocation (N customers get Q/N each)
- ✅ Timeout escalation strictly increasing (each level > previous)
- ✅ Idempotent event processing (E = E,E = E,E,E)
- ✅ Entitlement creation invariants (all fields present, valid)
- ✅ Resource usage percentage bounded (0-100%)
- ✅ State transition completeness (non-terminal states have exits)
- ✅ Concurrent linearizability (operations maintain ordering)
- ✅ Receipt hash determinism (same content → same hash)
- ✅ Event ordering causality (timestamps preserved)
- ✅ Quota overage consistency (U > Q ↔ overage > 0)

**13 Property Tests | Finds edge cases**

**Run:**
```bash
cargo test --test property_based_fsm_invariants -- --nocapture
```

### 4. **Load & Stress Testing** (`load_stress_integration.rs`)

High-concurrency performance tests with **SLO verification**.

**Coverage:**
- ✅ 1000 concurrent subscriptions within SLO (30 second limit, 33+ req/sec throughput)
- ✅ 100+ governors handling concurrent messages (routing correctness, <100ms avg latency)
- ✅ 1000 msgs/sec per governor throughput target
- ✅ Latency SLO verification (p99 < 50ms)
- ✅ Memory stability under sustained load (no memory leaks)
- ✅ Queue depth drain (events processed, queue returns to zero)
- ✅ High concurrency mixed workload (500 events, varied types)
- ✅ Orchestrator stats accuracy (metrics reflect reality)

**8 Tests | Real performance measurement**

**Run:**
```bash
cargo test --test load_stress_integration -- --nocapture
```

**Expected Output:**
```
Load Test: 1000 concurrent subscriptions completed in 25s (40.0 req/sec)
Message Throughput: 1250 messages/sec (1000 messages in 0.800s)
Latency SLOs: p50=2.34ms, p95=8.45ms, p99=12.10ms (SLO: p99<50ms)
```

### 5. **Hot Reload & Code Upgrade** (`hot_reload_upgrade_tests.rs`)

Tests **zero-downtime code upgrades** while governors are running.

**Coverage:**
- ✅ State preserved during hot reload (serialization → deserialization)
- ✅ Events processed before and after upgrade (zero downtime)
- ✅ Message buffering prevents loss during upgrade (buffer → process after)
- ✅ Automatic rollback on upgrade error (restore from snapshot)
- ✅ Version mismatch detection (incompatible versions blocked)
- ✅ Governor state preserved during individual upgrade (quota, state intact)
- ✅ Idempotent upgrade (same version twice is safe)
- ✅ Concurrent operations safe during upgrade (in-flight ops complete)
- ✅ Health check after upgrade succeeds (all 8 governors healthy)
- ✅ Pending event queue preserved after upgrade (ready to process)

**10 Tests | Production upgrade scenarios**

**Run:**
```bash
cargo test --test hot_reload_upgrade_tests -- --nocapture
```

### 6. **Edge Device (AtomVM) Integration** (`edge_atomvm_integration.rs`)

Tests **lightweight governors on edge devices** with cluster sync.

**Coverage:**
- ✅ Lightweight governor minimal memory footprint (< 100KB)
- ✅ Edge governor offline operation (cached decisions locally)
- ✅ Sync to cluster reconciles state (pending decisions flushed)
- ✅ Offline retry queue persists across restart (recovered from storage)
- ✅ Edge cluster replication (3 devices independent + synced)
- ✅ Network partition resilience (continues offline, queues decisions)
- ✅ Lightweight entitlement state (minimal fields for edge)
- ✅ Decision log for offline audit (local ledger for sync)
- ✅ Batch sync efficiency (100 decisions in single batch)
- ✅ Cache coherency during sync (merge strategy, last-write-wins)
- ✅ Recovery from partial sync failure (failed batch retried)

**11 Tests | IoT/edge computing scenarios**

**Run:**
```bash
cargo test --test edge_atomvm_integration -- --nocapture
```

### 7. **Clustering & Distributed** (`clustering_distributed.rs`)

Tests **multi-node cluster** with failover and partition handling.

**Coverage:**
- ✅ Join 3 nodes into cluster
- ✅ Distribute governors across nodes (load balancing)
- ✅ Node failure triggers migration (3 governors rehomed)
- ✅ Network partition split-brain detection (minority goes read-only)
- ✅ Quorum-based coordination (3/5 nodes needed for write)
- ✅ Node recovery rejoin and catch-up (missed updates replayed)
- ✅ Rolling upgrade across nodes (zero-downtime version update)
- ✅ Consistent hashing distribution (even spread, replicated)
- ✅ Cluster health metrics (latency, replication lag within SLOs)

**9 Tests | Distributed system challenges**

**Run:**
```bash
cargo test --test clustering_distributed -- --nocapture
```

---

## Test Execution Modes

### Run All Integration Tests
```bash
cargo test --tests -- --nocapture --test-threads=1
```

### Run Specific Test Suite
```bash
cargo test --test entitlement_fsm_integration
```

### Run Single Test
```bash
cargo test --test multi_governor_orchestration_advanced test_customer_subscribe_coordinates_six_governors
```

### Run with Output
```bash
cargo test --tests -- --nocapture
```

### Run Property-Based Tests (with more iterations)
```bash
PROPTEST_CASES=10000 cargo test --test property_based_fsm_invariants
```

### Run Load Tests (may take longer)
```bash
cargo test --test load_stress_integration -- --nocapture --ignored
```

---

## Test Statistics

| Suite | Tests | Lines | Focus |
|-------|-------|-------|-------|
| Entitlement FSM | 8 | ~700 | State machine, persistence, quota |
| Multi-Governor | 13 | ~600 | Orchestration, coordination, failure |
| Property-Based | 13 | ~500 | Invariants, edge cases, properties |
| Load & Stress | 8 | ~550 | Performance, throughput, SLOs |
| Hot Reload | 10 | ~650 | Zero-downtime upgrades, state preservation |
| Edge AtomVM | 11 | ~700 | Lightweight, offline, sync |
| Clustering | 9 | ~550 | Multi-node, failover, partitions |
| **TOTAL** | **72** | **~4200** | **Comprehensive coverage** |

---

## Chicago TDD Patterns Used

### Arrange-Act-Assert (AAA)
Every test follows strict AAA pattern:
```rust
#[test]
fn test_example() {
    // Arrange: Set up initial state
    let governor = Governor::new();

    // Act: Perform action
    let result = governor.transition(event);

    // Assert: Verify observable output/state
    assert_eq!(result.state, GovernorState::Active);
}
```

### Real Objects, No Mocks
Tests use actual `Orchestrator`, `Entitlement`, `Governor` objects:
- No mock objects
- No dependency injection tricks
- Real async/await behavior
- Real state changes verified

### State Verification Over Interaction Verification
Tests verify:
- Observable output values
- State after operation
- Side effects (e.g., receipts)

NOT:
- Method call counts
- Parameter matching
- Internal implementation details

---

## SLO Targets Verified

| SLO | Target | Test |
|-----|--------|------|
| Subscription latency | p99 < 50ms | `load_stress_integration` |
| Concurrent throughput | 1000 req/sec | `load_stress_integration` |
| 100 customers concurrent | Complete in 30s | `load_stress_integration` |
| Governor message routing | <100ms average | `load_stress_integration` |
| Cluster replication lag | <100ms | `clustering_distributed` |
| Inter-node latency | <50ms | `clustering_distributed` |

---

## Failure Scenarios Tested

### Permanent Failures
- Node failure → automatic governor migration
- Billing governor fails → entitlement revocation + rollback
- Compliance gate blocks subscription

### Transient Failures
- Network partition → split-brain detection
- Partial sync failure → automatic retry
- Upgrade error → rollback to previous state

### Concurrency Issues
- Race conditions in quota updates (prevented via ordering)
- Message loss during upgrade (buffering strategy)
- Idempotency violations (event deduplication)

---

## How to Add New Tests

1. **Identify the pattern:**
   - State machine test → use `entitlement_fsm_integration.rs`
   - Orchestration test → use `multi_governor_orchestration_advanced.rs`
   - Invariant test → use `property_based_fsm_invariants.rs`
   - Performance test → use `load_stress_integration.rs`
   - Distributed test → use `clustering_distributed.rs`

2. **Follow AAA pattern:**
   ```rust
   #[tokio::test]
   async fn test_new_scenario() {
       // Arrange
       // Act
       // Assert
   }
   ```

3. **Use real objects:**
   ```rust
   let mut orchestrator = MarketplaceOrchestrator::new();
   let _ = orchestrator.initialize().await;
   ```

4. **Verify observable outputs:**
   ```rust
   assert_eq!(orchestrator.current_state(), OrchestratorState::Idle);
   ```

---

## Performance Tips

- Use `--test-threads=1` for deterministic timing (especially load tests)
- Set `PROPTEST_CASES` environment variable for property test iterations
- Run load tests with `--nocapture` to see timing output
- Profile with `cargo test --release` for accurate performance

---

## Integration with CI/CD

```yaml
# Example GitHub Actions
- name: Run integration tests
  run: cargo test --tests -- --nocapture --test-threads=1

- name: Verify SLOs
  run: cargo test --test load_stress_integration -- --nocapture

- name: Check property invariants
  run: PROPTEST_CASES=10000 cargo test --test property_based_fsm_invariants
```

---

## Debugging Test Failures

### Enable debug output:
```bash
RUST_LOG=debug cargo test --test entitlement_fsm_integration -- --nocapture
```

### Run single test with backtrace:
```bash
RUST_BACKTRACE=1 cargo test --test multi_governor_orchestration_advanced test_customer_subscribe_coordinates_six_governors -- --nocapture
```

### Check test execution time:
```bash
cargo test --test load_stress_integration -- --nocapture --test-threads=1
```

---

## Key Assertions Used

| Assertion | Purpose |
|-----------|---------|
| `assert_eq!` | Exact state/value match |
| `assert!` | Boolean condition |
| `assert_ne!` | Not equal (state changes) |
| `unwrap()` / `expect()` | Test-only (OK in tests, NOT in production code) |
| `futures::future::join_all()` | Concurrent execution |
| `tokio::sync::RwLock` | Shared mutable state |

---

## Related Documentation

- See `/tests/README_CHICAGO_TDD.md` for Chicago TDD pattern guidelines
- See source code `/src/` for implementation details
- See `/docs/` for architecture documentation

---

**Last Updated**: 2026-01-25
**Test Framework**: `tokio` + `proptest`
**Pattern**: Chicago TDD (AAA, state-based, real objects)
**Coverage**: 72 tests, 4200+ lines, comprehensive scenarios
