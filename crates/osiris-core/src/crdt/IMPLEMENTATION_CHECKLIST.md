# Phase 1 Implementation Checklist
## CRDT Core Module - Ready for Development

---

## Pre-Implementation Review

- [ ] Design document reviewed and approved
- [ ] Team understands CRDT semantics (eventual consistency acceptable)
- [ ] Performance SLOs documented (see CRDT_DESIGN.md § Performance Characteristics)
- [ ] Replication strategy understood (Phase 2 planning)

---

## Core CRDT Types (Week 1)

### LwwMap<K, V>

**File**: `/crates/osiris-core/src/crdt/lww_map.rs`

- [ ] Struct definition: `entries: HashMap<K, (i64, V)>`
- [ ] `new()` - Create empty map
- [ ] `insert(key, value, timestamp)` - Insert with timestamp
- [ ] `insert_now(key, value)` - Insert with current time
- [ ] `get(&key) -> Option<&V>`
- [ ] `get_with_timestamp(&key) -> Option<(i64, &V)>`
- [ ] `merge(&mut self, other: &Self)` - CRDT merge
  - [ ] Verify: Commutative (order doesn't matter)
  - [ ] Verify: Idempotent (merge twice = merge once)
- [ ] `keys()`, `values()`, `len()`, `is_empty()`
- [ ] `snapshot() -> Vec<(K, i64, V)>` - For transmission
- [ ] `merge_snapshot(&mut self, snapshot)`
- [ ] `Default` impl
- [ ] `Clone` derive
- [ ] `Debug` derive
- [ ] Serde: `Serialize`, `Deserialize` derives

**Tests** (15+):
- [ ] Insert and retrieve
- [ ] LWW conflict (higher timestamp wins)
- [ ] Merge is commutative: `merge(A, B) == merge(B, A)`
- [ ] Merge is associative: `merge(merge(A, B), C) == merge(A, merge(B, C))`
- [ ] Merge is idempotent: `merge(A, A) == A`
- [ ] Empty map merges
- [ ] Snapshot/deserialization roundtrip
- [ ] Large map (1000s keys)

**Property Tests** (proptest):
- [ ] Commutative merge (5000 ops)
- [ ] No data loss (all keys present after merge)
- [ ] Deterministic output (same output regardless of order)

---

### Counter

**File**: `/crates/osiris-core/src/crdt/counter.rs`

- [ ] Struct: `value: i64`
- [ ] `new() -> Self`
- [ ] `increment(&mut self, delta: i64)`
- [ ] `value(&self) -> i64`
- [ ] `merge(&mut self, other: &Self)` - CRDT merge (addition)
- [ ] `Clone`, `Debug`, `Default` derives
- [ ] Serde: `Serialize`, `Deserialize`

**Tests** (8+):
- [ ] Increment positive
- [ ] Merge is addition
- [ ] Merge is commutative
- [ ] Large increment (i64::MAX boundary)

---

### PNCounter (Positive-Negative Counter)

**File**: `/crates/osiris-core/src/crdt/counter.rs` (same file)

- [ ] Struct: `increments: Counter, decrements: Counter`
- [ ] `new() -> Self`
- [ ] `increment(&mut self, delta: i64)` - Handle positive/negative
- [ ] `value(&self) -> i64` - Returns `increments.value() - decrements.value()`
- [ ] `merge(&mut self, other: &Self)` - Merge both counters
- [ ] `Clone`, `Debug`, `Default` derives

**Tests** (8+):
- [ ] Increment positive
- [ ] Increment negative (decrement)
- [ ] Value calculation
- [ ] Merge across replicas

---

### OrSet<T>

**File**: `/crates/osiris-core/src/crdt/set.rs`

- [ ] Struct: `entries: HashMap<T, HashSet<(String, u64)>>`
  - Map element -> set of (actor_id, unique_counter) pairs
- [ ] `new() -> Self`
- [ ] `add(&mut self, elem: T, actor_id: String, unique_id: u64)`
- [ ] `remove(&mut self, elem: &T)` - Removes ALL pairs (tombstone effect)
- [ ] `contains(&self, elem: &T) -> bool`
- [ ] `merge(&mut self, other: &Self)` - Union of all pairs
  - [ ] Verify: Commutative
  - [ ] Verify: Idempotent
- [ ] `len()`, `iter()`, `is_empty()`
- [ ] `Clone`, `Debug`, `Default` derives
- [ ] Serde: `Serialize`, `Deserialize`

**Tests** (12+):
- [ ] Add element
- [ ] Remove element
- [ ] Contains
- [ ] Merge creates union
- [ ] Merge is commutative
- [ ] Remove+add semantics (B adds after A removes: B's add wins)
- [ ] Multiple actors adding same element

**Property Tests**:
- [ ] Commutative merge
- [ ] Union property (all adds preserved)

---

### AppendOnlyLog<T>

**File**: `/crates/osiris-core/src/crdt/log.rs`

- [ ] Struct: `entries: Vec<(String, T)>` - (unique_id, entry)
- [ ] `new() -> Self`
- [ ] `append(&mut self, unique_id: String, entry: T)`
- [ ] `entries(&self) -> &[(String, T)]`
- [ ] `merge(&mut self, other: &Self)` - Union + sort by ID
  - [ ] Verify: Commutative
  - [ ] Verify: Deterministic order
- [ ] `len()`, `is_empty()`
- [ ] `Clone`, `Debug`, `Default` derives
- [ ] Serde: `Serialize`, `Deserialize`

**Tests** (10+):
- [ ] Append single entry
- [ ] Merge two logs (union)
- [ ] Merge maintains deterministic order
- [ ] Duplicate append (same ID) — should not duplicate
- [ ] Large logs (1000s entries)

---

## Module Structure

**File**: `/crates/osiris-core/src/crdt/mod.rs`

```rust
// Public exports
pub mod lww_map;
pub mod counter;
pub mod set;
pub mod log;

pub use lww_map::LwwMap;
pub use counter::{Counter, PNCounter};
pub use set::OrSet;
pub use log::AppendOnlyLog;
```

---

## Serialization & Network (Week 1)

- [ ] Add `serde` to Cargo.toml dependencies
- [ ] Implement `Serialize` for all CRDT types
- [ ] Implement `Deserialize` for all CRDT types
- [ ] Add `serde_json` tests (roundtrip)
- [ ] Add `bincode` tests (compact serialization)
- [ ] Document snapshot format (for Phase 2 replication)

**Tests**:
- [ ] JSON serialization roundtrip
- [ ] Bincode serialization (compact)
- [ ] LwwMap snapshot format documented

---

## Benchmarks (Week 1)

**File**: `/crates/osiris-core/benches/crdt_benchmarks.rs`

Using `criterion` crate:

- [ ] LwwMap insert (baseline vs Arc<RwLock<HashMap>>)
  - Expected: 100-500x faster
- [ ] LwwMap merge (1000 entries)
  - Expected: <1ms
- [ ] Counter increment
  - Expected: <100ns
- [ ] Counter merge (2 counters)
  - Expected: <10ns
- [ ] OrSet add/remove
  - Expected: <1µs
- [ ] OrSet merge (100 elements)
  - Expected: <10µs
- [ ] AppendOnlyLog merge (1000 entries)
  - Expected: <1ms

**Run**:
```bash
cargo bench --bench crdt_benchmarks
```

---

## Integration with Existing Code (Week 2-3)

### PerformanceMetrics Refactor

**File**: `/crates/osiris-core/src/performance_metrics.rs`

- [ ] Replace `Arc<RwLock<HashMap<String, MetricValue>>>` with `LwwMap<String, MetricValue>`
- [ ] Remove `.write().await` calls
- [ ] Update `record()` to call `insert_now()`
- [ ] Add merge capability
- [ ] Run existing tests (black-box)
  - [ ] `test_metrics_recording` passes
  - [ ] `test_average_calculation` passes
  - [ ] `test_monitoring_service` passes
- [ ] Add CRDT-specific test: merge semantics
- [ ] Verify no performance regression

---

### Health Monitor Refactor

**File**: `/crates/osiris-core/src/health.rs`

- [ ] Replace components `Arc<RwLock<HashMap>>` with `LwwMap<String, ComponentHealth>`
- [ ] Replace last_full_check `Arc<RwLock<Option>>` with `Option<i64>` (timestamp)
- [ ] Remove `.write().await` calls
- [ ] Update registration/update methods
- [ ] Run existing tests
- [ ] Add merge test

---

### Supervisor Refactor

**File**: `/crates/osiris-core/src/supervisor.rs`

- [ ] Replace `children: Arc<RwLock<HashMap>>` with `LwwMap<String, ChildSpec>`
- [ ] Keep `handles: Arc<RwLock<HashMap>>` (still needed for task management)
- [ ] Update start/stop/list methods
- [ ] Run existing tests
- [ ] Add child spec merge test

---

### A2A Service Refactor

**File**: `/crates/osiris-core/src/a2a_service.rs`

- [ ] Replace message_queue `Arc<RwLock<Vec>>` with `AppendOnlyLog<A2AMessage>`
- [ ] Replace subscriptions `Arc<RwLock<HashMap>>` with `OrSet<String>` (agent IDs)
- [ ] Keep message_handlers (can't replicate function pointers)
- [ ] Update send/subscribe/receive methods
- [ ] Run existing tests
- [ ] Add log merge test

---

### Recovery Orchestrator Refactor

**File**: `/crates/osiris-core/src/recovery_orchestrator.rs`

- [ ] Replace metrics `HashMap` with `LwwMap<String, RestartMetrics>`
- [ ] Replace states `HashMap` with `LwwMap<String, RecoveryState>`
- [ ] Replace decisions `Vec` with `AppendOnlyLog<RecoveryDecision>`
- [ ] Run existing tests
- [ ] Add merge tests

---

### Persistence Refactor

**File**: `/crates/osiris-core/src/persistence.rs`

- [ ] Replace cache `Arc<RwLock<HashMap>>` with `LwwMap<String, PersistedState>`
- [ ] Update load/save methods
- [ ] Run existing tests

---

## Testing Strategy

### Unit Tests (per CRDT type)
- Basic operations (insert, add, merge)
- CRDT properties (commutative, associative, idempotent)
- Edge cases (empty, single element, overflow)

### Property-Based Tests (proptest)
- 5000+ random operations
- Verify merge is commutative
- Verify no data loss

### Integration Tests
- Run all existing tests (should pass unchanged)
- New merge tests (cross-module)
- Black-box tests (public API unchanged)

### Performance Tests (criterion)
- Baseline: Arc<RwLock> vs LwwMap
- Target: 100-500x latency improvement
- Throughput: 1M ops/sec for high-contention paths

---

## Code Quality Gates

- [ ] All tests pass: `cargo make test`
- [ ] No warnings: `cargo make lint`
- [ ] Code compiles: `cargo make check`
- [ ] Coverage ≥ 80%: `cargo tarpaulin --out Html`
- [ ] Benchmarks document improvement: `cargo bench`
- [ ] No clippy warnings: `cargo clippy --all-targets`

---

## Documentation

- [ ] Module docs: `//! CRDT types for conflict-free state management`
- [ ] Type-level docs: `/// Last-Write-Wins Map...`
- [ ] Method docs: `/// Merges another map into this one (commutative)...`
- [ ] CRDT invariant examples (in tests)
- [ ] Serialization format documented (for Phase 2)

---

## Deliverable: PR Template

**Title**: `feat(osiris-core): implement CRDT core module (Phase 1)`

**Description**:
```markdown
## Summary
Implement conflict-free replicated data types (CRDTs) to replace lock-based state management.
This is Phase 1: model state as CRDTs with no API changes.

## Types Implemented
- LwwMap<K, V>: Last-Write-Wins Map (conflict resolution by timestamp)
- Counter: Increment-only counter
- PNCounter: Positive-Negative counter (supports increment/decrement)
- OrSet<T>: Observed-Remove Set (add/remove with proper semantics)
- AppendOnlyLog<T>: Immutable append-only log

## Performance Improvement
- Before: Arc<RwLock> latency p99 = 5000µs (contention)
- After: LwwMap latency p99 = 10µs (no contention)
- Improvement: 500x latency, 150x throughput

## Testing
- 80+ unit tests (CRDT operations)
- 10+ property-based tests (commutative, associative, idempotent merge)
- Benchmarks (criterion): 500x improvement confirmed
- All existing tests pass (black-box compatibility)

## Checklist
- [x] Design reviewed
- [x] All types implemented
- [x] Tests pass (80%+ coverage)
- [x] Benchmarks run
- [x] No API changes (internal only)
- [x] Documentation complete
```

---

## Success Criteria

✅ **Phase 1 Complete When**:
1. All CRDT types implemented and tested
2. All existing tests pass (no API breakage)
3. Benchmarks show 500x latency improvement
4. 80%+ code coverage
5. Zero clippy warnings
6. Documentation complete
7. Ready to start Phase 2 (replication layer)

**Estimated Time**: 1 engineer-week (5 days)

**Effort Breakdown**:
- Day 1: LwwMap + tests + benchmarks
- Day 2: Counter + PNCounter + tests
- Day 3: OrSet + tests
- Day 4: AppendOnlyLog + tests + serialization
- Day 5: Module integration + black-box testing + documentation

---

## Next Phase (Phase 2): Replication Layer

**When**: After Phase 1 PR merged

**Scope**:
- ReplicationService (handles state sync)
- PeerReplica (remote replica connection)
- Periodic state snapshots
- Network transmission (serde)
- Convergence monitoring

**Not in scope yet**: Design is in CRDT_DESIGN.md § Phase 2

---

## Questions & Clarifications

**Q: Why not async LwwMap?**
A: No await needed. Merge is CPU-only (no I/O), so sync is fine. Phase 2 adds async replication layer.

**Q: What about network failures?**
A: Handled in Phase 2. Phase 1 assumes local, in-process state only.

**Q: Can we test without Phase 2 replication?**
A: Yes! Single-region deployment works immediately (Phase 1 is drop-in replacement for locks). Phase 2 enables multi-region.

**Q: What's the memory overhead?**
A: Small—LwwMap adds one i64 per entry for timestamp. Counter adds nothing. OrSet adds (actor_id, counter) pairs. Accept for 500x latency gain.

**Q: What about version vectors (advanced)?**
A: Not needed for Phase 1. LWW (timestamp) is sufficient. Version vectors are optimization for Phase 3.

---

## References

- Design: `/crates/osiris-core/src/crdt/CRDT_DESIGN.md`
- CRDT Papers: https://crdt.tech/
- Kleppmann "Designing Data-Intensive Applications" Ch. 5
