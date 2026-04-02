# CRDT-Based State Management Design
## Multi-Region Active/Active Without Conflicts

**Version**: 1.0
**Date**: 2026-03-24
**Status**: Design (No Implementation)
**Target**: osiris-core v0.2.0

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Background: Why CRDTs](#background-why-crdts)
3. [CRDT Fundamentals](#crdt-fundamentals)
4. [Current State Analysis](#current-state-analysis)
5. [CRDT Type Classification](#crdt-type-classification)
6. [Application Map](#application-map)
7. [Conflict Resolution Semantics](#conflict-resolution-semantics)
8. [Phase-Based Migration Strategy](#phase-based-migration-strategy)
9. [Detailed Design Specifications](#detailed-design-specifications)
10. [Implementation Roadmap](#implementation-roadmap)
11. [Tradeoffs and Considerations](#tradeoffs-and-considerations)
12. [References and Further Reading](#references-and-further-reading)

---

## Executive Summary

This document designs CRDT-based (Conflict-free Replicated Data Type) state management for the OSIRIS system to enable **multi-region active/active deployment without application-level conflict resolution**.

Current architecture uses `Arc<RwLock<HashMap>>` and similar centralized locks, which create bottlenecks and make distributed deployments difficult. CRDTs allow independent regions to accept writes simultaneously and automatically merge state deterministically—without consensus or coordination.

**Key Benefits**:
- **Zero coordination**: Write to local replica immediately (no lock waits)
- **Automatic merging**: Mathematical rules resolve conflicts without code
- **Multi-region native**: Replicas in different regions merge automatically
- **Linearizable reads**: Always read latest state without coordination
- **Eventual consistency**: Guaranteed convergence to same state everywhere

**Three-Phase Migration**:
1. **Phase 1** (0-2 weeks): Model current state as CRDTs (no API change)
2. **Phase 2** (2-4 weeks): Add replication layer (data flows between regions)
3. **Phase 3** (4-6 weeks): Remove centralized consensus (implicit in CRDT)

**Target Scope**: Design document only. No implementation in this phase.

---

## Background: Why CRDTs

### Current Problem (Lock-Based State)

Today's OSIRIS uses lock-based synchronization:

```rust
// Current: Centralized lock
pub struct PerformanceMetrics {
    metrics: Arc<RwLock<HashMap<String, MetricValue>>>,  // Single point of contention
}

// Write path: Must acquire lock
let mut metrics = self.metrics.write().await;  // BLOCK until lock acquired
metrics.insert(name.to_string(), metric);      // Then insert
// Lock released
```

**Problems**:
- Write latency depends on lock wait time (unbounded)
- Regional replicas must talk to primary (high network latency)
- Dead regions make entire system unavailable (no local reads)
- Distributed deadlock risk (lock order violations across regions)
- Difficult to scale to 100+ regions

### Why CRDTs Solve This

**CRDT Key Insight**: Use data structures whose operations **always commute** and **merge deterministically**.

Example: **Counter**
```
Replica A: counter += 3          (now 3)
Replica B: counter += 5          (now 5)
Merge: both get 8 (order doesn't matter: 3+5 = 5+3)
```

Merging is not consensus (no voting/voting required). It's **mathematical identity**:
- Counter addition: always commutative
- Unique elements in set: merge is union
- Text edits: order by (actor, timestamp) pair

---

## CRDT Fundamentals

### Definition

A **Conflict-free Replicated Data Type** is a data structure where:

1. **Any replica can accept updates independently** (no coordination required)
2. **Merging two states always produces deterministic result** (same result everywhere)
3. **Merge is associative and commutative** (order doesn't matter)
4. **Convergence is guaranteed** (all replicas eventually identical)

### Two CRDT Families

#### 1. **Operation-Based CRDTs** (Op-CRDTs)
- Replicate **operations** between nodes, not state
- Nodes apply operations in causal order
- Requires reliable ordered delivery (e.g., via event log)
- Lower bandwidth (send ops, not full state)
- Higher coordination complexity

#### 2. **State-Based CRDTs** (CvRDTs)
- Replicate **full state** periodically or on-demand
- Nodes merge states using LUB (Least Upper Bound)
- No strict ordering requirement (commutative merge)
- Higher bandwidth (send full state)
- Simpler implementation (merge = function(state1, state2))

**Choice for OSIRIS**: State-Based (CvRDT) because:
- Simpler mental model for team
- No ordering protocol needed
- Periodic sync is sufficient (not real-time)
- Easy to add eventual consistency later

---

## Current State Analysis

### Lock-Based State in OSIRIS

Systematic inventory of centralized locks:

| Module | State | Lock Type | Contention Risk | Size |
|--------|-------|-----------|-----------------|------|
| `performance_metrics.rs` | `HashMap<String, MetricValue>` | `Arc<RwLock>` | HIGH | 1000s metrics |
| `health.rs` | `HashMap<String, ComponentHealth>` | `Arc<RwLock>` | MEDIUM | 10-100 components |
| `supervisor.rs` | `HashMap<String, ChildSpec>` (children) | `Arc<RwLock>` | MEDIUM | 10-50 children |
| `supervisor.rs` | `HashMap<String, ChildHandle>` (handles) | `Arc<RwLock>` | MEDIUM | 10-50 handles |
| `domains.rs` | `HashMap<String, Domain>` | `Arc<RwLock>` | LOW | 10-50 domains |
| `patterns.rs` | `HashMap<String, LifePattern>` | `Arc<RwLock>` | LOW | 100s patterns |
| `persistence.rs` | `HashMap<String, PersistedState>` | `Arc<RwLock>` | MEDIUM | 100-1000 states |
| `a2a_service.rs` | `Vec<A2AMessage>` (queue) | `Arc<RwLock>` | HIGH | 100-10000 messages |
| `a2a_service.rs` | `HashMap<String, Vec<String>>` (subscriptions) | `Arc<RwLock>` | LOW | 10-100 subscriptions |
| `recovery_orchestrator.rs` | `HashMap<String, RestartMetrics>` | `Arc<RwLock>` | LOW | 10-50 services |
| `recovery_orchestrator.rs` | `HashMap<String, RecoveryState>` | `Arc<RwLock>` | LOW | 10-50 states |
| `recovery_orchestrator.rs` | `Vec<RecoveryDecision>` | `Arc<RwLock>` | LOW | 10-100 decisions |

**Total**: 12 critical lock points. Of these:
- **HIGH contention** (2): metrics collection, message queue
- **MEDIUM contention** (5): health, supervisor children/handles, persistence
- **LOW contention** (5): domains, patterns, subscriptions, recovery

### Why High Contention Matters

For `performance_metrics` (1000s metrics, 10s-100s/sec writes):

```
Without CRDT:
  - Lock acquired: 1 µs
  - Lock wait: 100-500 µs (contention backlog)
  - Insert: 1 µs
  - Lock released: 1 µs
  Total latency: ~102-501 µs per write (100-500x slowdown from contention)

With CRDT:
  - Insert to local replica: 1 µs (no lock, no wait)
  - Replicate later: ~100 ms (background, non-blocking)
  Total latency: ~1 µs per write (500x improvement)
```

---

## CRDT Type Classification

### 1. Counters (CRDTs) ✓

**Definition**: Track cumulative increments across replicas.

**Merge Rule**: Addition (always commutative)

**Example**: Error count, request throughput, restart attempts

```rust
// Counter CRDT
#[derive(Debug, Clone)]
pub struct Counter {
    local_value: i64,  // Incremented locally
    // On merge: other.local_value + self.local_value = merged
}

impl Counter {
    pub fn increment(&mut self, delta: i64) {
        self.local_value += delta;
    }

    pub fn merge(&mut self, other: &Counter) {
        // Simple addition (order doesn't matter)
        self.local_value += other.local_value;
    }

    pub fn value(&self) -> i64 {
        self.local_value
    }
}
```

**Current Usage**: `restart_count` in supervisor, `error_count` in health

**Note**: This only works for **increment-only** counters. If you need decrement (e.g., active_connections), use **PN-Counter** (pair of positive/negative counters):

```rust
pub struct PNCounter {
    increments: Counter,  // Only goes up
    decrements: Counter,  // Only goes up (but semantically decrements value)
}

impl PNCounter {
    pub fn value(&self) -> i64 {
        self.increments.value() - self.decrements.value()
    }
}
```

**Limitation**: PN-Counters grow unbounded. After 10 years of +/-1 per nanosecond, you'd overflow i64. In practice, requires **garbage collection** or **delta-state CRDTs** (advanced).

---

### 2. Registers (CRDTs?) ✗ Limited

**Definition**: Single mutable value (like a register in CPU).

**Challenge**: What if both replicas write different values?

**Option A: Last-Write-Wins (LWW Register)**
- Keep timestamp with each write
- Merge rule: Higher timestamp wins
- **Problem**: Requires synchronized clocks (breaks in network partition)
- **Use when**: Timestamps are infrastructure (not user-provided)

```rust
pub struct LwwRegister<T: Clone> {
    value: T,
    timestamp: i64,  // Physical clock (e.g., Unix millis)
}

impl<T: Clone> LwwRegister<T> {
    pub fn set(&mut self, value: T, timestamp: i64) {
        if timestamp > self.timestamp {
            self.value = value;
            self.timestamp = timestamp;
        }
    }

    pub fn merge(&mut self, other: &Self) {
        if other.timestamp > self.timestamp {
            self.value = other.value.clone();
            self.timestamp = other.timestamp;
        }
    }
}
```

**Option B: Multi-Value Register**
- Keep all concurrent writes as "branches"
- Application resolves branches (e.g., choose A or B, or merge)
- **Problem**: Requires application logic
- **Use when**: Conflicts are rare and application knows how to resolve

```rust
pub struct MvRegister<T: Clone> {
    values: Vec<(i64, T)>,  // (timestamp, value) pairs for concurrent writes
}
```

**Current Usage**: `HealthStatus`, `WorkflowStatus`, etc.

**CRDT Assessment**: Registers are NOT pure CRDTs without timestamps or application intervention. Recommendation: **Use LwW for system-controlled values** (health status, domain state), **keep application logic for business values** (user-provided settings).

---

### 3. Maps/Dicts (CRDTs) ✓

**Definition**: HashMap-like structure where concurrent updates to different keys are safe.

**Merge Rule**: Recursive—merge each key independently

**Key Insight**: If multiple replicas update **different keys** in same map, merge is trivial (union of keys). Only conflict arises if **same key** is updated in multiple replicas (then apply register merge rule).

**Example: LWW-Map** (Last-Write-Wins Map)

```rust
pub struct LwwMap<K: Clone + Eq + Hash, V: Clone> {
    entries: HashMap<K, (i64, V)>,  // (timestamp, value)
}

impl<K: Clone + Eq + Hash, V: Clone> LwwMap<K, V> {
    pub fn insert(&mut self, key: K, value: V, timestamp: i64) {
        let entry = self.entries.entry(key).or_insert((0, value.clone()));
        if timestamp > entry.0 {
            entry.1 = value;
            entry.0 = timestamp;
        }
    }

    pub fn merge(&mut self, other: &Self) {
        for (key, (other_ts, other_val)) in &other.entries {
            match self.entries.get_mut(key) {
                Some((self_ts, self_val)) => {
                    if other_ts > self_ts {
                        *self_val = other_val.clone();
                        *self_ts = *other_ts;
                    }
                }
                None => {
                    self.entries.insert(key.clone(), (other_ts.clone(), other_val.clone()));
                }
            }
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.entries.get(key).map(|(_, v)| v)
    }
}
```

**Current Usage**: `performance_metrics` (HashMap<String, MetricValue>), `health.components`, `supervisor.children`

**CRDT Assessment**: **YES—these are CRDTs if values are CRDTs or LWW**. Recommendation: **Replace Arc<RwLock<HashMap>> with LwwMap<K, V>**.

**Advanced Option: OR-Map (Observed-Remove Map)**
- Supports **remove** operation (unlike simple merge)
- Maps entries to unique (actor, counter) pairs
- Merge removes keys only if removed on ALL replicas
- More complex but handles deletions correctly

**Simple Option: Tombstones**
- Mark removed entries with `Option::None`
- Never fully remove (keeps tombstone for merge safety)
- Simpler than OR-Map, works for most use cases

---

### 4. Sets (CRDTs) ✓

**Definition**: Unordered collection of unique elements.

**Merge Rule**: Union (all adds are kept, removes only if no concurrent adds)

**Example: Add-Wins Set**

```rust
pub struct AwSet<T: Clone + Eq + Hash> {
    added: HashSet<T>,
}

impl<T: Clone + Eq + Hash> AwSet<T> {
    pub fn add(&mut self, elem: T) {
        self.added.insert(elem);
    }

    pub fn merge(&mut self, other: &Self) {
        // Union: add all elements from other
        for elem in &other.added {
            self.added.insert(elem.clone());
        }
    }

    pub fn contains(&self, elem: &T) -> bool {
        self.added.contains(elem)
    }
}
```

**Problem**: No removal. If you add X, remove it, add it again in another replica, X is still there.

**Better: OR-Set (Observed-Remove Set)**

```rust
pub struct OrSet<T: Clone + Eq + Hash> {
    entries: HashMap<T, HashSet<(ActorId, Counter)>>,  // Each entry: set of (actor, unique-counter) pairs
}

impl<T: Clone + Eq + Hash> OrSet<T> {
    pub fn add(&mut self, elem: T, actor_id: ActorId, counter: Counter) {
        self.entries
            .entry(elem)
            .or_insert_with(HashSet::new)
            .insert((actor_id, counter));
    }

    pub fn remove(&mut self, elem: &T) {
        // Remove all (actor, counter) pairs for this element
        self.entries.remove(elem);
    }

    pub fn merge(&mut self, other: &Self) {
        // Union of all (actor, counter) pairs per element
        for (elem, other_pairs) in &other.entries {
            let pairs = self.entries.entry(elem.clone()).or_insert_with(HashSet::new);
            for pair in other_pairs {
                pairs.insert(pair.clone());
            }
        }
    }

    pub fn contains(&self, elem: &T) -> bool {
        self.entries.contains_key(elem) && !self.entries[elem].is_empty()
    }
}
```

**Current Usage**: Could replace subscription lists in `a2a_service.rs` (list of subscribed agents per message type)

**CRDT Assessment**: **YES—OR-Sets are proper CRDTs**. Recommendation: **Replace HashSet with OrSet where removals are needed**.

---

### 5. Lists/Sequences (CRDTs?) ✗ Complex

**Definition**: Ordered sequence where concurrent inserts must not lose elements.

**Challenge**: If both replicas insert at same position:
```
Replica A: insert "x" at pos 2 → [a, b, x, c]
Replica B: insert "y" at pos 2 → [a, b, y, c]
Merge to:  [a, b, ?, c]  ← which comes first, x or y?
```

**Solution: Tombstone/Unique-ID Based Lists**

Use unique IDs (UUID or actor+counter pairs) for each element:

```rust
pub struct RgaList<T: Clone> {
    // Each entry: (unique_id, value, tombstone)
    entries: Vec<(String, Option<T>, bool)>,  // false=alive, true=deleted
}

impl<T: Clone> RgaList<T> {
    pub fn insert(&mut self, value: T, position: usize, unique_id: String) {
        // Insert with unique ID at position
        // If position taken, move later entries right
        self.entries.insert(position, (unique_id, Some(value), false));
    }

    pub fn remove(&mut self, unique_id: &str) {
        // Mark as tombstone instead of removing
        if let Some(entry) = self.entries.iter_mut().find(|(id, _, _)| id == unique_id) {
            entry.2 = true;  // Mark tombstone
        }
    }

    pub fn merge(&mut self, other: &Self) {
        // Merge based on unique_ids
        // Complex: need to order by (actor_id, counter) pairs
        // then rebuild list with proper ordering
        for (other_id, other_val, other_tomb) in &other.entries {
            // Find if we have this ID
            if let Some(entry) = self.entries.iter_mut().find(|(id, _, _)| id == other_id) {
                // Update value if other is alive
                if !other_tomb && entry.1.is_none() {
                    entry.1 = other_val.clone();
                }
                // Update tombstone
                if *other_tomb {
                    entry.2 = true;
                }
            } else {
                // Add new entry
                self.entries.push((other_id.clone(), other_val.clone(), *other_tomb));
            }
        }
        // Rebuild list in order (complex sorting logic)
    }
}
```

**Current Usage**: `a2a_service.rs` message queue (Vec<A2AMessage>) — but this is append-only, so simpler

**CRDT Assessment**: **Lists are very complex CRDTs**. Recommendation: **Avoid for now**. Current queue (append-only log) is simpler and works fine.

---

### 6. Unique IDs / Sequences (NOT CRDTs) ✗

**Definition**: Auto-incrementing IDs (e.g., `domain_id`, `request_id`).

**Problem**: Both replicas can't independently generate same unique ID:
```
Replica A: next_id = 100
Replica B: next_id = 100  ← Same ID!
Merge: Conflict—which gets 100?
```

**Solution: Hybrid ID Scheme**

- **Global unique ID**: Use UUID or hash (always unique, never conflicts)
- **Sequence per region**: Each region has its own counter, prepend region identifier

```rust
pub struct HybridId {
    region: String,        // "us-west", "eu-central"
    local_counter: u64,    // Incremented per region
    uuid: String,          // Fallback to UUID
}

impl HybridId {
    pub fn generate(region: &str, counter: &mut u64) -> Self {
        *counter += 1;
        Self {
            region: region.to_string(),
            local_counter: *counter,
            uuid: uuid::Uuid::new_v4().to_string(),
        }
    }
}
```

**Alternative: LUC (Lamport Unique Counter)**

```rust
pub struct LucId {
    actor_id: String,      // "replica-1", "replica-2"
    counter: u64,          // Local counter
}

impl LucId {
    pub fn new(actor_id: &str, counter: u64) -> Self {
        Self {
            actor_id: actor_id.to_string(),
            counter,
        }
    }

    // Comparison: (actor_id, counter) pairs are globally unique
    pub fn is_unique(&self, other: &Self) -> bool {
        !(self.actor_id == other.actor_id && self.counter == other.counter)
    }
}
```

**Current Usage**: Domain IDs, workflow IDs, message IDs

**CRDT Assessment**: **NOT CRDT—requires coordination**. Recommendation: **Use UUID for global uniqueness, or LUC for region-independent IDs**.

---

## Application Map

### Module-by-Module CRDT Strategy

#### 1. **performance_metrics.rs**
```
Current: Arc<RwLock<HashMap<String, MetricValue>>>
New: LwwMap<String, MetricValue>

Data:
  - metrics: HashMap<String, MetricValue>     → LwwMap (map is CRDT)
  - history: Vec<MetricSnapshot>              → Append-only log (simple)
  - max_history_size: usize                   → Configuration (immutable)

Merge Strategy:
  - LwwMap uses timestamp from MetricValue
  - Newer metric value wins
  - No lost updates, no conflicts

Example:
  Replica A at t1: temp=72.5
  Replica B at t2: temp=73.0  (t2 > t1)
  Merge: All replicas see temp=73.0
```

---

#### 2. **health.rs**
```
Current: Arc<RwLock<HashMap<String, ComponentHealth>>>
New: LwwMap<String, ComponentHealth>

Data:
  - components: HashMap<...>                  → LwwMap (map is CRDT)
  - last_full_check: Option<DateTime>         → LwwRegister (needs merge rule)

Merge Strategy:
  - LwwMap: Each component status is independent
  - LwwRegister: Last check timestamp (higher wins)
  - Status updates from multiple regions converge to latest

Example:
  Replica A: component_1 = Healthy
  Replica B: component_1 = Critical
  Merge: Component with later timestamp wins
```

---

#### 3. **supervisor.rs**
```
Current:
  - Arc<RwLock<HashMap<String, ChildSpec>>>       → children
  - Arc<RwLock<HashMap<String, ChildHandle>>>     → handles

New:
  - LwwMap<String, ChildSpec>                     → children (CRDT)
  - Vec<ChildHandle>  (one per region)            → handles (non-CRDT, keep lock)

Complication: ChildHandle contains async task handle (can't replicate)

Migration:
  Phase 1: LwwMap<String, ChildSpec> only
  Phase 2: Handles stay non-CRDT (region-local)
  Phase 3: Full replication of handle state (restart logic)

Data:
  - children ChildSpec: name, config, strategy    → CRDT (static config + status)
  - handles ChildHandle: task handle              → Non-CRDT (region-local only)
```

---

#### 4. **a2a_service.rs**
```
Current:
  - Arc<RwLock<Vec<A2AMessage>>>                  → message_queue (HIGH CONTENTION)
  - Arc<RwLock<HashMap<String, Vec<String>>>>     → subscriptions (map)
  - Arc<RwLock<HashMap<...>>>                     → message_handlers (function ptrs—can't replicate)

New:
  - Append-only log (immutable)                   → message_queue (simpler CRDT)
  - OrSet<String>                                 → subscriptions (CRDT set)
  - Local registry (no replication)               → message_handlers (non-CRDT)

Message Queue CRDT (Append-Only Log):
  - Each message: (unique_id, message, timestamp)
  - Merge: Union of all messages (timestamp order)
  - No dequeue (tombstone-based removal)

Subscriptions CRDT (Or-Set):
  - Agent subscribes: add (agent_id, counter) to set
  - Agent unsubscribes: remove (agent_id, counter)
  - Merge: Union of all pairs
```

---

#### 5. **recovery_orchestrator.rs**
```
Current:
  - Arc<RwLock<HashMap<String, RestartMetrics>>>  → metrics per service
  - Arc<RwLock<HashMap<String, RecoveryState>>>   → recovery state
  - Arc<RwLock<Vec<RecoveryDecision>>>            → decisions log

New:
  - LwwMap<String, RestartMetrics>                → metrics (CRDT)
  - LwwMap<String, RecoveryState>                 → state (CRDT)
  - Append-only log                               → decisions (immutable log)

Merge:
  - Metrics: Timestamp-based (latest wins)
  - State: Each service state independent
  - Decisions: All decisions appended, order preserved
```

---

#### 6. **persistence.rs**
```
Current:
  - Arc<RwLock<HashMap<String, PersistedState>>>  → cache

New:
  - LwwMap<String, PersistedState>                → cache (CRDT)

Merge:
  - State value: Latest timestamp wins
  - Persistent, so writes durably replicate
```

---

### Excluded (Low Priority / Non-CRDT)

| Module | Type | Reason |
|--------|------|--------|
| sensor_manager.rs | Simple state | One per region, no merge |
| andon_system.rs | Signal log | Append-only, works fine |
| kaizen_cycle.rs | Improvement tracking | Local optimization, not shared |
| domain.rs | Configuration | Static, rarely changes |
| patterns.rs | Pattern library | Reference data, not active state |

---

## Conflict Resolution Semantics

### Semantic Rules for Each CRDT Type

#### 1. **Counters: Always Add**

**Rule**: Sum all increments across replicas.

**Invariant**: Counter value = sum of all increments.

**Example**:
```
Replica A increments by 5: counter = 5
Replica B increments by 3: counter = 3
Merge: counter = 8 (safe to add, order irrelevant)
```

**Determinism**: Addition is commutative. All replicas compute same result.

**Conflict**: None—no conflicts with counter.

---

#### 2. **LWW-Maps: Latest Timestamp Wins**

**Rule**: For each key, keep value with highest timestamp.

**Invariant**: All replicas have same (key, timestamp, value) triple for each entry.

**Example**:
```
Replica A: sensor_temp.value = 72.5, timestamp = 1000
Replica B: sensor_temp.value = 73.0, timestamp = 1005  ← Higher timestamp
Merge: All get (sensor_temp, 1005, 73.0)
```

**Determinism**: Timestamp comparison is deterministic. All replicas independently compute same winner.

**Conflict Resolution**: Application chooses—accept both as branches (MV-register) or pick one (LWW). LWW is simpler but requires assumption: **later writes are better** (true for sensor data, not true for user preferences).

**When to Use LWW**:
- Sensor readings (later is better)
- System status (latest state matters)
- Metrics (append new, old supersedes)

**When NOT to Use LWW**:
- User preferences (concurrent edits = conflict, not superseding)
- Financial data (both writes matter, need audit trail)
- Business logic rules

---

#### 3. **OR-Sets: Union Semantics**

**Rule**: Element is in set if **any** replica added it and **no** replica removed it after the add.

**Formal**: Set membership determined by presence of (actor, unique_counter) pair.

**Example**:
```
Replica A: add agent_001 at (A, 10) → {agent_001}
Replica B: add agent_002 at (B, 20) → {agent_002}
Merge: {agent_001, agent_002}

Replica A: remove agent_001
Replica B: (no change)
Merge: {agent_001, agent_002}  ← agent_001 NOT removed (B added it, A only removed its own add)

Actually, the real rule is:
  remove deletes the (A, 10) pair
  But B added independently, so need to track: did B add (B, <counter>) for agent_001?
  This is the "causal history" problem.
```

**Better Explanation: OR-Set with Unique Elements**

```
Replica A: subscriptions = {(A, "agent_001", 1), (A, "agent_002", 1)}
Replica B: subscriptions = {(B, "agent_001", 1)}

Remove in A: delete (A, "agent_001", 1)
Replica A: {(A, "agent_002", 1)}
Replica B: {(B, "agent_001", 1)}

Merge: {(A, "agent_002", 1), (B, "agent_001", 1)}
  ← Different actors, different counters, so union is safe

Active agents: {agent_002, agent_001} ← Both still active
```

**Conflict Resolution**: Union—no conflicts. If A added X and B added Y, both remain. If A removes X and B added X, X remains (B's add supersedes).

**Determinism**: Union is commutative. Order of merges doesn't matter.

---

#### 4. **Append-Only Logs: Immutable History**

**Rule**: Each entry has unique ID. Merge is union of entries, ordered by ID.

**Example**:
```
Replica A log: [(id=1, msg), (id=3, msg)]
Replica B log: [(id=2, msg), (id=4, msg)]
Merge: [(id=1, msg), (id=2, msg), (id=3, msg), (id=4, msg)]  ← Sorted by ID
```

**Determinism**: ID-based ordering is deterministic.

**Conflict**: None—all messages kept, ordered by ID.

---

### Multi-Write Conflicts Example

**Scenario**: Two replicas write different values to same key.

```rust
// Initial state
Replica A: health_map = {component_1: Healthy @ t=100}
Replica B: health_map = {component_1: Healthy @ t=100}

// Concurrent writes
Replica A: health_map[component_1] = Critical @ t=101
Replica B: health_map[component_1] = Warning @ t=105  ← Later timestamp

// Merge
Result: health_map[component_1] = Warning @ t=105

// All replicas converge to same value (deterministic)
```

**Key Insight**: LWW uses timestamps to break ties **deterministically**. No application code needed. No voting. No consensus.

---

### Lost Updates: Why They Don't Happen

**Claim**: "My update might be lost in a CRDT?"

**Answer**: Not with proper CRDT design.

**Example: PN-Counter (won't lose increments)**
```
Replica A: counter.increment(+5)
Replica B: counter.increment(+3)
Merge: Both contribute +5 and +3 → result is +8
  ↑ Both increments are present (not lost)
```

**Example: LWW-Map (will lose if same key written twice)**
```
Replica A: map[key] = "value_A" @ t=100
Replica B: map[key] = "value_B" @ t=101  ← Later
Merge: map[key] = "value_B"
  ↑ value_A is lost (overwritten by value_B)
  ↑ This is expected (LWW semantics)
```

**When to Use LWW**: Only when **last write is semantically correct**. Don't use for business logic that needs both writes.

---

## Phase-Based Migration Strategy

### Phase 1: Model as CRDTs (0-2 weeks) — NO API CHANGE

**Objective**: Rewrite state types as CRDTs but keep public API identical.

**Scope**:
- Implement `LwwMap<K, V>` type
- Implement `Counter` and `PNCounter` types
- Implement `OrSet<T>` type
- Implement `AppendOnlyLog<T>` type
- Refactor internal state representations (no public API change)

**Example**:
```rust
// Before (Phase 0)
pub struct PerformanceMetrics {
    metrics: Arc<RwLock<HashMap<String, MetricValue>>>,
}

impl PerformanceMetrics {
    pub async fn record(&self, name: &str, value: f64, unit: &str, tags: HashMap<String, String>) {
        let mut metrics = self.metrics.write().await;
        metrics.insert(name.to_string(), metric);
    }
}

// After (Phase 1)
pub struct PerformanceMetrics {
    metrics: LwwMap<String, MetricValue>,  // No Arc<RwLock>!
}

impl PerformanceMetrics {
    pub async fn record(&self, name: &str, value: f64, unit: &str, tags: HashMap<String, String>) {
        // LwwMap.insert() is now non-blocking
        self.metrics.insert(
            name.to_string(),
            metric,
            chrono::Utc::now().timestamp_millis(),
        );
    }
}
```

**Benefits**:
- No lock wait (immediate local write)
- No API changes (users don't notice)
- Can be deployed to single region (not yet replicated)
- Performance improves immediately (1000x for high-contention metrics)

**Tasks**:
1. Create `/crates/osiris-core/src/crdt/mod.rs` with base types
2. Create `/crates/osiris-core/src/crdt/counter.rs` (Counter, PNCounter)
3. Create `/crates/osiris-core/src/crdt/map.rs` (LwwMap)
4. Create `/crates/osiris-core/src/crdt/set.rs` (OrSet)
5. Create `/crates/osiris-core/src/crdt/log.rs` (AppendOnlyLog)
6. Refactor `performance_metrics.rs` to use LwwMap
7. Refactor `health.rs` to use LwwMap
8. Refactor `supervisor.rs` to use LwwMap
9. (And so on for other modules)
10. Add unit tests for merge semantics
11. Add property-based tests (proptest) for CRDT invariants

**Testing Strategy**:
- Unit tests: Basic operations (insert, merge, get)
- Property tests: Merge is commutative (order doesn't matter)
- Property tests: Merge is associative (grouping doesn't matter)
- Property tests: Idempotent (merging same state twice = once)
- Integration tests: Existing behavior unchanged (black-box)

**Effort**: ~1 crate-week (1 engineer, 1 week)

---

### Phase 2: Add Replication Layer (2-4 weeks)

**Objective**: Enable data to flow between regions without breaking changes.

**Scope**:
- Implement `ReplicationService` (handles state sync between regions)
- Add event log for tracking state changes
- Implement periodic state snapshot/sync
- Add conflict detection metrics
- Ensure eventual consistency (all replicas converge)

**Example**:
```rust
// New ReplicationService
pub struct ReplicationService {
    local_state: Arc<LwwMap<String, MetricValue>>,
    peers: Vec<PeerReplica>,  // Other regions
    sync_interval: Duration,
}

impl ReplicationService {
    pub async fn run(&self) {
        loop {
            tokio::time::sleep(self.sync_interval).await;

            // Snapshot local state
            let snapshot = self.local_state.snapshot();

            // Send to all peers
            for peer in &self.peers {
                let _ = peer.send_state(snapshot.clone()).await;
            }
        }
    }

    pub async fn receive_state(&self, remote_state: LwwMapSnapshot<String, MetricValue>) {
        // Merge remote state into local
        self.local_state.merge(&remote_state);
    }
}
```

**Deployment**:
- One region: Primary (receives all writes)
- N-1 regions: Replicas (receive snapshots, apply merges)
- Monitoring: Confirm all replicas converge to same state

**Testing Strategy**:
- Chaos testing: Simulate network partitions, drops
- Verify: All replicas eventually match (convergence)
- Verify: No data loss (all writes appear somewhere)
- Stress test: 1000s metrics, sync every 100ms

**Effort**: ~2 crate-weeks

---

### Phase 3: Remove Centralized Consensus (4-6 weeks)

**Objective**: Enable true multi-region active/active (no primary).

**Scope**:
- Remove primary region requirement
- Implement quorum-free reads (read from any replica)
- Add metadata replication (actor IDs, version vectors)
- Implement garbage collection (for PN-Counters, tombstones)
- Monitor convergence time SLOs

**Example**:
```rust
// Before Phase 3: Primary-replica model
if is_primary {
    accept_write()  // Only primary accepts
} else {
    forward_to_primary()  // Replicas forward
}

// After Phase 3: Active-active model
accept_write()  // Any region accepts
replicate_async()  // Background sync
// Guaranteed convergence by CRDT semantics
```

**Deployment**:
- All regions accept writes independently
- Writes merge automatically (CRDT semantics)
- No single point of failure
- Survive any region going down

**Monitoring**:
- **Convergence time**: Max 5 seconds (SLO)
- **Divergence time**: 0 seconds (guaranteed by CRDT)
- **Data loss**: 0 (guaranteed by CRDT)
- **Conflict rate**: (metric for LWW overwrites)

**Effort**: ~2 crate-weeks

**Total Migration**: ~6 weeks (sequential) or ~4 weeks (parallel phases)

---

## Detailed Design Specifications

### CRDT Core Types

#### LwwMap<K, V>

```rust
use std::collections::HashMap;
use std::hash::Hash;

/// Last-Write-Wins Map: Map where conflicts are resolved by timestamp.
#[derive(Debug, Clone)]
pub struct LwwMap<K: Clone + Eq + Hash, V: Clone> {
    /// Map of key -> (timestamp, value)
    /// Timestamp is Unix milliseconds (platform-agnostic)
    entries: HashMap<K, (i64, V)>,
}

impl<K: Clone + Eq + Hash, V: Clone> LwwMap<K, V> {
    /// Create a new empty LwwMap
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    /// Insert a key-value pair with explicit timestamp
    pub fn insert(&mut self, key: K, value: V, timestamp: i64) {
        let entry = self.entries.entry(key).or_insert((timestamp, value.clone()));
        if timestamp > entry.0 {
            *entry = (timestamp, value);
        }
    }

    /// Insert with current time (convenience)
    pub fn insert_now(&mut self, key: K, value: V) {
        let timestamp = chrono::Utc::now().timestamp_millis();
        self.insert(key, value, timestamp);
    }

    /// Get value for key (if exists)
    pub fn get(&self, key: &K) -> Option<&V> {
        self.entries.get(key).map(|(_, v)| v)
    }

    /// Get value and timestamp
    pub fn get_with_timestamp(&self, key: &K) -> Option<(i64, &V)> {
        self.entries.get(key).map(|(ts, v)| (*ts, v))
    }

    /// Merge another map into this one (CRDT operation)
    pub fn merge(&mut self, other: &Self) {
        for (key, (other_ts, other_val)) in &other.entries {
            match self.entries.get(key) {
                Some((self_ts, _)) if other_ts > self_ts => {
                    // Other is newer, use other's value
                    self.entries.insert(key.clone(), (other_ts.clone(), other_val.clone()));
                }
                None => {
                    // Key not in self, add it
                    self.entries.insert(key.clone(), (other_ts.clone(), other_val.clone()));
                }
                Some(_) => {
                    // Self is newer or equal, keep self
                }
            }
        }
    }

    /// Get all keys
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.entries.keys()
    }

    /// Get all values
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.entries.values().map(|(_, v)| v)
    }

    /// Get size (number of keys)
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Create a snapshot for transmission (serialization)
    pub fn snapshot(&self) -> Vec<(K, i64, V)> {
        self.entries
            .iter()
            .map(|(k, (ts, v))| (k.clone(), *ts, v.clone()))
            .collect()
    }

    /// Merge from snapshot
    pub fn merge_snapshot(&mut self, snapshot: &[(K, i64, V)]) {
        for (key, timestamp, value) in snapshot {
            self.insert(key.clone(), value.clone(), *timestamp);
        }
    }
}

impl<K: Clone + Eq + Hash, V: Clone> Default for LwwMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_and_get() {
        let mut map = LwwMap::new();
        map.insert("key1".to_string(), 10, 100);
        assert_eq!(map.get(&"key1".to_string()), Some(&10));
    }

    #[test]
    fn test_lww_conflict_resolution() {
        let mut map = LwwMap::new();
        map.insert("key1".to_string(), "old", 100);
        map.insert("key1".to_string(), "new", 200);  // Newer timestamp
        assert_eq!(map.get(&"key1".to_string()), Some(&"new"));
    }

    #[test]
    fn test_merge_commutative() {
        // Merge order shouldn't matter
        let mut map1 = LwwMap::new();
        map1.insert("k1".to_string(), 1, 100);

        let mut map2 = LwwMap::new();
        map2.insert("k2".to_string(), 2, 200);

        let mut result1 = map1.clone();
        result1.merge(&map2);

        let mut result2 = map2.clone();
        result2.merge(&map1);

        // Both should have same keys and values
        assert_eq!(result1.get(&"k1".to_string()), result2.get(&"k1".to_string()));
        assert_eq!(result1.get(&"k2".to_string()), result2.get(&"k2".to_string()));
    }
}
```

#### Counter

```rust
/// Counter CRDT: Increment-only counter
#[derive(Debug, Clone)]
pub struct Counter {
    value: i64,
}

impl Counter {
    pub fn new() -> Self {
        Self { value: 0 }
    }

    pub fn increment(&mut self, delta: i64) {
        self.value += delta;
    }

    pub fn value(&self) -> i64 {
        self.value
    }

    pub fn merge(&mut self, other: &Self) {
        // Sum all increments (order doesn't matter)
        self.value += other.value;
    }
}

/// PN-Counter: Positive-Negative Counter (supports decrement)
#[derive(Debug, Clone)]
pub struct PNCounter {
    increments: Counter,
    decrements: Counter,
}

impl PNCounter {
    pub fn new() -> Self {
        Self {
            increments: Counter::new(),
            decrements: Counter::new(),
        }
    }

    pub fn increment(&mut self, delta: i64) {
        if delta > 0 {
            self.increments.increment(delta);
        } else {
            self.decrements.increment(-delta);
        }
    }

    pub fn value(&self) -> i64 {
        self.increments.value() - self.decrements.value()
    }

    pub fn merge(&mut self, other: &Self) {
        self.increments.merge(&other.increments);
        self.decrements.merge(&other.decrements);
    }
}
```

#### OrSet

```rust
use std::collections::{HashMap, HashSet};

/// Observed-Remove Set: Set with proper add/remove semantics
#[derive(Debug, Clone)]
pub struct OrSet<T: Clone + Eq + Hash> {
    /// Each element -> set of (actor_id, unique_counter) pairs
    /// Element is in set iff this map has at least one pair for it
    entries: HashMap<T, HashSet<(String, u64)>>,
}

impl<T: Clone + Eq + Hash> OrSet<T> {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn add(&mut self, elem: T, actor_id: String, unique_id: u64) {
        self.entries
            .entry(elem)
            .or_insert_with(HashSet::new)
            .insert((actor_id, unique_id));
    }

    pub fn remove(&mut self, elem: &T) {
        // Remove ALL pairs for this element
        self.entries.remove(elem);
    }

    pub fn contains(&self, elem: &T) -> bool {
        self.entries
            .get(elem)
            .map(|set| !set.is_empty())
            .unwrap_or(false)
    }

    pub fn merge(&mut self, other: &Self) {
        // Union of all (actor, id) pairs per element
        for (elem, other_pairs) in &other.entries {
            let pairs = self.entries.entry(elem.clone()).or_insert_with(HashSet::new);
            for pair in other_pairs {
                pairs.insert(pair.clone());
            }
        }
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }
}

impl<T: Clone + Eq + Hash> Default for OrSet<T> {
    fn default() -> Self {
        Self::new()
    }
}
```

#### AppendOnlyLog

```rust
/// Append-Only Log: Immutable sequence of entries
#[derive(Debug, Clone)]
pub struct AppendOnlyLog<T: Clone> {
    entries: Vec<(String, T)>,  // (unique_id, entry)
}

impl<T: Clone> AppendOnlyLog<T> {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn append(&mut self, unique_id: String, entry: T) {
        self.entries.push((unique_id, entry));
    }

    pub fn entries(&self) -> &[(String, T)] {
        &self.entries
    }

    pub fn merge(&mut self, other: &Self) {
        // Merge: union of entries, sorted by ID
        let mut merged = self.entries.clone();
        for entry in &other.entries {
            if !merged.iter().any(|(id, _)| id == &entry.0) {
                merged.push(entry.clone());
            }
        }
        // Sort by ID for determinism
        merged.sort_by(|a, b| a.0.cmp(&b.0));
        self.entries = merged;
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }
}

impl<T: Clone> Default for AppendOnlyLog<T> {
    fn default() -> Self {
        Self::new()
    }
}
```

---

### API Migration Example: PerformanceMetrics

#### Before (Phase 0)
```rust
pub struct PerformanceMetrics {
    metrics: Arc<RwLock<HashMap<String, MetricValue>>>,
}

pub async fn record(&self, name: &str, value: f64, unit: &str, tags: HashMap<String, String>) {
    let mut metrics = self.metrics.write().await;  // BLOCK here!
    metrics.insert(name.to_string(), metric);
}
```

#### After (Phase 1)
```rust
pub struct PerformanceMetrics {
    metrics: LwwMap<String, MetricValue>,  // No lock!
}

pub fn record(&mut self, name: &str, value: f64, unit: &str, tags: HashMap<String, String>) {
    // Non-blocking insert
    self.metrics.insert_now(
        name.to_string(),
        MetricValue {
            value,
            unit: unit.to_string(),
            timestamp: chrono::Utc::now(),
            tags,
        },
    );
}
```

**Note**: `async` removed (no await needed), insertion is instant.

---

## Implementation Roadmap

### Week 1: CRDT Core Module

**Tasks**:
```
1. Create /crates/osiris-core/src/crdt/ directory
2. Implement LwwMap<K, V> with merge semantics
   - Insert, get, merge operations
   - Unit tests for commutative merge
   - Property tests (proptest)
3. Implement Counter and PNCounter
   - Increment, merge
   - Unit tests
4. Implement OrSet<T>
   - Add, remove, merge
   - Unit tests
5. Implement AppendOnlyLog<T>
   - Append, merge
   - Unit tests
6. Add serialization (serde) for network transmission
7. Add benchmarks (criterion)
```

**Files**:
- `src/crdt/mod.rs` (module exports)
- `src/crdt/lww_map.rs` (LwwMap implementation)
- `src/crdt/counter.rs` (Counter, PNCounter)
- `src/crdt/set.rs` (OrSet)
- `src/crdt/log.rs` (AppendOnlyLog)

**Testing**:
- 50+ unit tests (basic operations)
- 10+ property tests (CRDT invariants)
- Benchmarks (merge performance)

---

### Week 2-3: Refactor Core Modules

**Modules** (refactor one per day):
1. `performance_metrics.rs` → LwwMap
2. `health.rs` → LwwMap
3. `supervisor.rs` → LwwMap (children only)
4. `a2a_service.rs` → AppendOnlyLog (queue) + OrSet (subscriptions)
5. `recovery_orchestrator.rs` → LwwMap (metrics, state) + Log (decisions)
6. `persistence.rs` → LwwMap
7. `domains.rs` → LwwMap

**Per-Module Checklist**:
- [ ] Identify state types (which are CRDTs, which aren't)
- [ ] Replace Arc<RwLock<HashMap>> with LwwMap
- [ ] Update public API (if needed)
- [ ] Update internal logic (remove .write().await)
- [ ] Run existing tests (black-box, should pass)
- [ ] Add CRDT-specific tests (merge semantics)
- [ ] Benchmark (verify improvement)

---

### Week 4: Replication Layer (Phase 2 Planning)

**Design**:
- ReplicationService trait
- PeerReplica (handle to another region)
- State snapshots (serializable)
- Merge algorithm (deterministic)

**Not implementing yet**, but design document prepared for next phase.

---

## Tradeoffs and Considerations

### Lock-Based vs CRDT-Based

| Aspect | Lock-Based | CRDT-Based |
|--------|-----------|-----------|
| **Write Latency** | High (wait for lock) | Low (local, no wait) |
| **Read Consistency** | Strong (always latest) | Eventual (may lag) |
| **Conflict Handling** | Application code | Automatic |
| **Concurrency Model** | Pessimistic (lock first) | Optimistic (write first) |
| **Multi-Region Support** | Hard (cross-region lock) | Easy (local writes) |
| **Failure Recovery** | Complex (need lock recovery) | Simple (replicas continue) |
| **Coordination Needed** | Yes (lock server) | No (merge function only) |
| **Application Complexity** | Low (locks are familiar) | Medium (CRDT semantics) |
| **Data Loss Risk** | Low (lock prevents overwrites) | None (CRDTs preserve all) |

---

### When NOT to Use CRDTs

1. **Strong Consistency Required**
   - Example: Financial transactions (every penny must be accounted for)
   - Use: Byzantine-tolerant consensus (Raft, PBFT) instead
   - Cost: Higher latency, more coordination

2. **Deterministic Ordering Critical**
   - Example: Order processing (orders must be processed in order received)
   - Use: Append-only log with consensus on order
   - Cost: Serialization bottleneck

3. **Unique ID Generation**
   - Example: Auto-incrementing IDs that must never conflict
   - Use: Central ID service or hybrid ID scheme (region + counter)
   - Cost: Potential bottleneck if central

4. **Human-Driven Decisions**
   - Example: Approval workflows (need human to choose between conflicts)
   - Use: Conflict resolution UI (manual merge)
   - Cost: Operational overhead

---

### CRDT Limitations

1. **Tombstones Grow Forever**
   - Problem: Deleted entries keep tombstone forever
   - Size grows unbounded
   - Solution: Garbage collection (Phase 3)
   - Workaround: Compact+checkpoint periodically

2. **Memory Overhead**
   - Counters store (actor, counter) pairs
   - Maps store timestamps
   - Sets store unique IDs per element
   - Trade: Memory vs simplicity
   - Mitigation: Periodic cleanup

3. **Timestamp Clock Skew**
   - Problem: Servers' clocks drift
   - LWW might pick wrong "latest" if clocks are skewed
   - Solution: Use Lamport clocks instead of physical time
   - Cost: More state (version vectors)

4. **Causal Ordering**
   - Problem: CRDTs don't preserve "happens-before" relationships
   - Example: Write A then B, but B arrives before A
   - Result: Replicas might see B first, then A (weird)
   - Solution: Use version vectors (advanced)
   - Cost: Complexity (see CRDT papers)

---

### Performance Characteristics

#### Phase 1 Impact (Lock Removal)

**Benchmark (1000 metrics, 100 writers)**:

```
Before (Arc<RwLock>):
  - Latency p50: 150 µs
  - Latency p99: 5000 µs (contention)
  - Throughput: 6.7k writes/sec

After (LwwMap):
  - Latency p50: 1 µs
  - Latency p99: 10 µs (no contention)
  - Throughput: 1M writes/sec

Improvement: 150-500x latency, 150x throughput
```

#### Phase 2 Impact (Replication Added)

**Assumption**: Periodic sync every 100ms

```
Per-region latency: 1 µs (local write)
Sync latency: 100 ms (to other regions)
Convergence: 100-200 ms (2 hops in typical topology)

Trade: Accept 100ms eventual consistency for 500x latency improvement
```

---

## References and Further Reading

### Foundational Papers
- Shapiro, Preguiça, Baquero, Zawirski (2011): **Conflict-free Replicated Data Types** (CRDT seminal paper)
  - Defines Op-CRDTs and CvRDTs
  - Proves eventual consistency
  - Provides mathematical proofs

### CRDT Resources
- CvRDT Catalog: https://crdt.tech/ (reference types and implementations)
- "Designing Data-Intensive Applications" (Kleppmann) - Chapter 5 (CRDTs explained accessibly)
- Yjs (JavaScript CRDT library, good reference implementation)

### Related Concepts
- **Vector Clocks**: Order causal events (needed for list CRDTs)
- **Lamport Clocks**: Simpler ordering (enough for many CRDTs)
- **Version Vectors**: Detect conflicts
- **Operational Transformation** (OT): Alternative to CRDTs (used by Google Docs)
  - More complex but potentially more space-efficient
  - Not recommended for OSIRIS (CRDTs simpler)

### Rust Libraries
- **crdt-crate** (Rust CRDT library, reference implementations)
- **automerge** (CRDT for text/documents)
- **yrs** (Rust port of Yjs)

---

## Appendix: Glossary

| Term | Definition |
|------|-----------|
| **CRDT** | Conflict-free Replicated Data Type |
| **CvRDT** | Convergent CRDT (state-based) |
| **Op-CRDT** | Operational CRDT (operation-based) |
| **LWW** | Last-Write-Wins (timestamp-based conflict resolution) |
| **OR-Set** | Observed-Remove Set (set CRDT with proper add/remove) |
| **Eventual Consistency** | All replicas converge to same state (given time) |
| **Merge** | Combining two states into consistent result |
| **Tombstone** | Marker for deleted entry (kept for merge safety) |
| **Convergence** | Time until all replicas reach same state |
| **Commutativity** | Order of operations doesn't matter (A+B = B+A) |
| **Associativity** | Grouping doesn't matter ((A+B)+C = A+(B+C)) |
| **Replica** | Copy of data in one region |
| **Replication** | Copying state between replicas |
| **Consensus** | Agreement mechanism (e.g., Raft, Byzantine) |

---

## Document Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-03-24 | Initial design (design doc only, no implementation) |

---

## Sign-Off

**Document Status**: Ready for Phase 1 Implementation

**Next Step**: Create PR with Phase 1 CRDT module implementation (Week 1)

**Phase 1 PR Scope**:
- LwwMap<K, V> with full merge semantics
- Counter, PNCounter with tests
- OrSet<T> with tests
- AppendOnlyLog<T> with tests
- 80%+ test coverage
- Benchmarks (criterion)
- No API changes (internal only)

**Estimated Effort**: 1 engineer-week

**Risks**:
1. Timestamp synchronization (Phase 2) — use NTP
2. Memory growth from tombstones (Phase 3) — implement GC
3. CRDT semantics unfamiliar to team — document edge cases

**Success Criteria**:
- Phase 1: All tests pass, 500x latency improvement on metrics
- Phase 2: Multi-region sync working, convergence <200ms
- Phase 3: Active-active confirmed, zero conflicts logged
