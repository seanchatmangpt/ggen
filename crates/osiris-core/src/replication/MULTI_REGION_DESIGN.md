# Multi-Region Failover Design - OSIRIS ggen

**Purpose**: Specification for active/active replication across US-East, US-West, and EU regions with comprehensive failover strategies, conflict resolution, and recovery mechanisms.

**Version**: 1.0 | **Date**: 2026-03-24 | **Status**: Design Phase (No Implementation)

**Author**: ggen Architecture Team | **Stakeholders**: DevOps, Backend, SRE, Compliance

---

## Table of Contents

1. [Overview](#overview)
2. [System Context](#system-context)
3. [Replication Strategy](#replication-strategy)
4. [Consistency Model](#consistency-model)
5. [Failure Scenarios](#failure-scenarios)
6. [Conflict Resolution](#conflict-resolution)
7. [Recovery Mechanisms](#recovery-mechanisms)
8. [Data Flow Architecture](#data-flow-architecture)
9. [Implementation Roadmap](#implementation-roadmap)
10. [Appendices](#appendices)

---

## 1. Overview

### 1.1 Goals

- **Availability**: 99.99% uptime across 3 geographic regions
- **Consistency**: Eventual consistency with vector clock ordering + CRDT safety
- **Recovery**: Automatic rejoin within 5 minutes for region failure
- **Compliance**: Audit-ready with receipt replication across regions
- **Performance**: Sub-100ms write latency in active region + <500ms propagation to replicas

### 1.2 Scope

**Included**:
- Active/active replication across US-East (primary), US-West, EU (replicas)
- Vector clock-based causal ordering
- Last-write-wins and CRDT conflict resolution strategies
- Network partition handling (split-brain scenarios)
- Graceful degradation under cascading failures
- Receipt ledger replication with cryptographic verification

**Not Included** (Phase 2+):
- Byzantine fault tolerance (Phase 3)
- Geo-replication across >3 regions
- Hierarchical replication (primary→secondary→tertiary)
- Real-time consensus (Raft/Paxos) — design prefers eventual consistency

### 1.3 Key Assumptions

1. **Network**: Eventual connectivity between regions (not always-on)
2. **Clocks**: Loosely synchronized via NTP (±100ms drift acceptable)
3. **Storage**: Each region has independent persistent storage (Firestore, Cloud Logging)
4. **Operators**: Human intervention available for split-brain resolution
5. **Workload**: Read-heavy governance decisions + infrequent writes (policy updates)

---

## 2. System Context

### 2.1 Architecture Diagram

```
                           ┌─────────────────────────────────────┐
                           │     Global Load Balancer             │
                           │    (Anycast DNS + Cloud CDN)         │
                           └──────────┬──────────┬──────────────┘
                                      │          │
                    ┌─────────────────┴─┐    ┌──┴─────────────────┐
                    │                     │    │                     │
            ┌───────▼────────┐  ┌──────────────▼─┐  ┌───────────────▼──┐
            │   US-East      │  │   US-West      │  │      EU          │
            │   (Primary)    │  │   (Replica)    │  │   (Replica)      │
            └────┬──┬────────┘  └─┬───┬──────────┘  └────┬─────┬──────┘
                 │  │             │   │                  │     │
         ┌───────┴──┴─────┐ ┌────┴───┴────────┐ ┌───────┴──┬──┴──────┐
         │                 │ │                 │ │          │         │
      ┌──▼─────────────┐ ┌─▼────────────┐ ┌──▼──────────┐ ┌▼──────────▼┐
      │ Governance     │ │ Write Log    │ │ Governance  │ │ Write Log  │
      │ Service        │ │ (Event Store)│ │ Service     │ │(Event Store)
      │ (Autonomic)    │ │ (Replicating)│ │ (Read/Sync) │ │(Replicating)
      └──┬─────────────┘ └─┬────────────┘ └──┬──────────┘ └┬──────────┬┘
         │                 │                  │             │         │
         └────────────┬────┘                  │             │         │
                      │                       │             │         │
                      └───────────────────┬──┬┴─────┬───────┴──┬──────┘
                                          │ │       │          │
                        ┌─────────────────┴─┴────┬──┴──────────┴─────────┐
                        │                        │                       │
                    ┌───▼──────────────────┐ ┌──▼───────────────────────┐
                    │ Vector Clock Bus     │ │ Event Stream (Kafka/Pub) │
                    │ (Causal Ordering)    │ │ (Replication Channel)    │
                    └────────────────────────┴──────────────────────────┘
                                │                    │
                    ┌───────────┴────────────────────┴──────────┐
                    │                                            │
                    ├─ US-East: Leader election + local writes
                    ├─ US-West: Remote writes sync via VC-ordering
                    └─ EU: Remote writes sync via VC-ordering
```

### 2.2 Region Responsibilities

| Aspect | US-East (Primary) | US-West (Replica) | EU (Replica) |
|--------|-----------------|------------------|--------------|
| **Write Acceptance** | Direct writes + fast ack | Queue + remote write | Queue + remote write |
| **Leadership** | Elected leader | Follower | Follower |
| **Local Governance** | Full autonomy | Full autonomy | Full autonomy |
| **Conflict Resolution** | Arbiter (if split-brain) | Defer to arbiter | Defer to arbiter |
| **Evidence Ledger** | Primary ledger | Replicated copy | Replicated copy |
| **Backup Strategy** | GCS backup (daily) | GCS backup (daily) | GCS backup (daily) |

---

## 3. Replication Strategy

### 3.1 What Gets Replicated?

#### Critical (Replicates with durability guarantees)
- **Governance Decisions** (policy updates, domain changes)
  - Write: US-East (immediate) + queue for US-West/EU
  - Replication: Vector clock ordered, idempotent
  - Durability: 3-region receipt confirmation before returning to client

- **Evidence Ledger** (cryptographically signed receipts)
  - Write: Ledger entry in primary region
  - Replication: Hash-chain continuous replication to replicas
  - Durability: Merkle proof validates chain integrity across regions

- **Domain State** (workflow status, life patterns)
  - Write: Authoritative in US-East
  - Replication: Eventual consistency (VC ordering)
  - Durability: Last-write-wins with conflict logs

#### Semi-Critical (Replicates with best-effort)
- **Performance Metrics** (request latency, throughput, errors)
  - Replication: Asynchronous (next batch)
  - Durability: Lost metrics on region failure acceptable
  - TTL: 30 days (cost optimization)

- **Sensor Data** (health checks, deadlock detection)
  - Replication: Sampling (10% of events)
  - Durability: Real-time in local region, archived daily to GCS
  - TTL: 7 days

#### Non-Replicated (Local only)
- **In-Flight Transaction State** (temporary processing)
  - Scope: Request context, locks, semaphores
  - TTL: Session lifetime
  - Loss handling: Retry from client

### 3.2 Replication Channels

#### Primary Channel: Event Stream (Kafka/Google Pub/Sub)

```rust
// Conceptual: Event stream structure
pub struct ReplicationEvent {
    pub id: String,                    // Unique event ID
    pub region_id: String,             // Origin region
    pub vector_clock: VectorClock,     // Causal ordering
    pub event_type: ReplicationEventType,
    pub payload: serde_json::Value,
    pub timestamp: Instant,
    pub checksum: String,              // SHA-256 for validation
}

pub enum ReplicationEventType {
    PolicyUpdate { policy_id: String, version: u32 },
    DomainStateChange { domain_id: String },
    ReceiptEmitted { receipt_id: String, hash: String },
    RecoveryMarker { region_id: String, checkpoint: String },
    ConflictResolution { resolution_type: String },
}
```

**Delivery Semantics**: At-least-once (idempotent processing required)

**Ordering**: Vector clock causal ordering (not FIFO)

**Latency**: <500ms 99th percentile propagation to replicas

---

## 4. Consistency Model

### 4.1 Consistency Guarantees

**Model**: Eventual Consistency with Causal Ordering

```
┌─────────────────────────────────────────────────────────┐
│ Consistency Hierarchy (Weakest → Strongest)              │
├─────────────────────────────────────────────────────────┤
│ 1. Weak Consistency        ← Current network issues      │
│ 2. Eventual Consistency    ← Phase 1 goal                │
│ 3. Causal Consistency      ← Phase 1 + Vector Clocks     │
│ 4. Strong Consistency      ← Phase 2 (Consensus Raft)    │
│ 5. Linearizability         ← Phase 3 (Byzantine)         │
└─────────────────────────────────────────────────────────┘
```

### 4.2 Vector Clock Mechanism

**Definition**: A vector clock is a tuple `[ts_us_east, ts_us_west, ts_eu]` that orders causally dependent events.

**Invariant**: If event A causally happens before B, then `vc(A) < vc(B)` componentwise.

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VectorClock {
    pub us_east: u64,    // Lamport clock for US-East region
    pub us_west: u64,    // Lamport clock for US-West region
    pub eu: u64,         // Lamport clock for EU region
}

impl VectorClock {
    /// Increment clock for local region
    pub fn increment_local(&mut self, region: &str) {
        match region {
            "us-east" => self.us_east += 1,
            "us-west" => self.us_west += 1,
            "eu" => self.eu += 1,
            _ => {}
        }
    }

    /// Merge incoming clock (for replication events)
    pub fn merge(&mut self, other: &VectorClock) {
        self.us_east = self.us_east.max(other.us_east);
        self.us_west = self.us_west.max(other.us_west);
        self.eu = self.eu.max(other.eu);
    }

    /// Check if this clock is causally after another
    pub fn happens_after(&self, other: &VectorClock) -> bool {
        self.us_east >= other.us_east
            && self.us_west >= other.us_west
            && self.eu >= other.eu
            && self != other
    }

    /// Check if events are concurrent (neither happens before the other)
    pub fn concurrent_with(&self, other: &VectorClock) -> bool {
        !self.happens_after(other) && !other.happens_after(self)
    }
}
```

**Example Timeline**:

```
US-East Timeline:
  T0: Policy update (id=policy-1)     VC=[1,0,0]
  T1: Domain change (id=domain-1)     VC=[2,0,0]  (depends on T0)
  T2: Receives US-West policy-2       VC=[2,1,0]

US-West Timeline:
  T0': Policy update (id=policy-2)    VC=[0,1,0]
  T1': Receives US-East policy-1      VC=[1,1,0]
  T2': Domain change (id=domain-2)    VC=[1,2,0]  (depends on T1')

Observation: policy-1 → domain-1 → policy-2 is causal order.
BUT: policy-1 and policy-2 are concurrent (neither VC dominates).
     → Conflict resolution required!
```

### 4.3 Read Semantics

| Read Type | Where | Latency | Consistency |
|-----------|-------|---------|-------------|
| **Local Read** | Same region | <50ms | Strong (linearizable) |
| **Global Read** | Any region | <100ms | Causal (vector clock) |
| **Quorum Read** | 2/3 regions | <300ms | Eventual (after replication) |

**Default**: Local read (fast) + client can request Quorum if strict consistency needed

### 4.4 Write Semantics

| Write Type | Where | Latency | Durability |
|-----------|-------|---------|------------|
| **Local Write** | US-East | <50ms ack | Persisted locally |
| **Replicated Write** | All 3 regions | <500ms ack | 2 of 3 received |
| **Durable Write** | All 3 regions | <1000ms ack | All 3 durable |

**Default**: Replicated write (balance of speed + safety)

---

## 5. Failure Scenarios

### 5.1 Single Region Failure (US-West down)

**Timeline**:
1. **T0**: Load balancer detects US-West unhealthy (failed health checks)
2. **T1**: Traffic reroutes to US-East + EU (10-30 seconds)
3. **T2**: Replication queue accumulates US-West writes
4. **T3**: US-West rejoins network
5. **T4**: Recovery service syncs vector clocks + replays queued events

**Impact**:
- Read latency: +50ms (routing to US-East)
- Write latency: +200ms (higher replication latency)
- Throughput: -30% (fewer endpoints)
- Governance: Unchanged (2 regions sufficient for decisions)

**Mechanism**:

```rust
pub enum RegionStatus {
    Healthy {
        last_heartbeat: Instant,
        lag_events: usize,
    },
    Degraded {
        reason: String,
        queue_size: usize,
        retry_count: u32,
    },
    Failed {
        since: Instant,
        recovery_initiated: bool,
    },
}

pub struct RegionHealthMonitor {
    status: HashMap<String, RegionStatus>,
    heartbeat_interval: Duration,
    failure_threshold: u32,  // 3 missed heartbeats = failure
}

impl RegionHealthMonitor {
    pub async fn on_region_failure(&self, region: &str) -> Result<()> {
        // 1. Update status to Failed
        // 2. Stop accepting writes to failed region
        // 3. Queue replication events for retry
        // 4. Alert operator
        // 5. Start recovery timer (5 minute timeout)
        Ok(())
    }

    pub async fn on_region_recovery(&self, region: &str) -> Result<()> {
        // 1. Initiate vector clock sync
        // 2. Replay queued events in VC order
        // 3. Validate checksum of replayed state
        // 4. Mark region as Healthy when caught up
        Ok(())
    }
}
```

### 5.2 Network Partition (US-East ↔ EU split from US-West)

**Scenario**:
- US-East ↔ US-West: Connected
- US-West ↔ EU: Connected
- US-East ↔ EU: Lost (network partition)

**Timeline**:
1. **T0**: East-West bridge detects partition
2. **T1**: Vector clocks diverge (no merge possible)
3. **T2**: Clients in EU get stale reads (write lag)
4. **T3**: Partition heals
5. **T4**: Conflict resolution triggered

**Conflict Occurs When**:

```
US-East writes policy-A (VC=[5,3,3])
EU writes policy-A (VC=[5,3,4])  ← CONFLICT! Different VC but same ID

→ Last-write-wins: EU wins (VC[eu] = 4 > 3)
→ Evidence: Conflict log captured + receipt emitted
```

**Mechanism**:

```rust
pub struct PartitionDetector {
    ping_interval: Duration,      // 5 seconds
    failure_threshold: u32,         // 3 pings = partition
    quorum_required: usize,         // 2/3 regions required
}

impl PartitionDetector {
    pub async fn detect_partition(&self) -> Result<PartitionInfo> {
        let health = self.check_all_regions().await?;

        let healthy_count = health.values()
            .filter(|s| matches!(s, RegionStatus::Healthy { .. }))
            .count();

        if healthy_count < self.quorum_required {
            return Err(OSIRISError::PartitionDetected {
                healthy_regions: healthy_count,
                partition_majority: None,  // No majority
            });
        }

        Ok(PartitionInfo {
            isolated_regions: health.iter()
                .filter(|(_, s)| matches!(s, RegionStatus::Failed { .. }))
                .map(|(r, _)| r.clone())
                .collect(),
            majority_partition: vec!["us-east", "us-west"],  // Connected
        })
    }

    pub async fn apply_split_brain_rules(&self) {
        // Rule 1: Majority partition serves reads
        // Rule 2: Minority partition enters read-only mode
        // Rule 3: On reconciliation, last-write-wins wins
        // Rule 4: Conflict log emitted to evidence ledger
    }
}
```

### 5.3 Cascading Failures (All regions degraded)

**Scenario**: Network issues → latency spike → timeouts → circuit breaker opens → region isolation

**Timeline**:
1. **T0**: US-East replication latency spikes to 2000ms
2. **T1**: Timeout circuit breaker opens
3. **T2**: US-West + EU fall back to local decision-making
4. **T3**: Writes queue locally (no replication)
5. **T4**: Network recovers
6. **T5**: Recovery service merges state

**Graceful Degradation Policy**:

```rust
pub enum DegradationLevel {
    /// All regions healthy
    Nominal,

    /// One replication channel failing
    SingleRegionLag {
        lagging_region: String,
        queue_depth: usize,
    },

    /// Two regions isolated, one healthy
    PartitionedMinority {
        isolated: Vec<String>,
        healthy: Vec<String>,
    },

    /// All replication channels failing
    /// Fall back to local decision-making
    LocalOnly {
        reason: String,
        since: Instant,
    },
}

impl DegradationLevel {
    pub fn write_policy(&self) -> WritePolicy {
        match self {
            DegradationLevel::Nominal => WritePolicy::ReplicatedWrite,
            DegradationLevel::SingleRegionLag { .. } => WritePolicy::LocalWrite,
            DegradationLevel::PartitionedMinority { .. } => WritePolicy::LocalWrite,
            DegradationLevel::LocalOnly { .. } => WritePolicy::LocalWriteOnly,
        }
    }

    pub fn read_policy(&self) -> ReadPolicy {
        match self {
            DegradationLevel::Nominal => ReadPolicy::GlobalRead,
            DegradationLevel::SingleRegionLag { .. } => ReadPolicy::LocalRead,
            DegradationLevel::PartitionedMinority { .. } => ReadPolicy::LocalRead,
            DegradationLevel::LocalOnly { .. } => ReadPolicy::LocalReadOnly,
        }
    }
}
```

### 5.4 Split Brain (Both partitions claim leadership)

**Scenario**: Partition heals but both regions have committed writes

```
US-East partition (2 regions):       EU partition (1 region):
  - Write: policy-update-v1           - Write: policy-update-v1'
  - VC: [10, 9, 5]                    - VC: [5, 0, 10]
  - Committed to ledger                - Committed to ledger
  - Considers itself authoritative     - Considers itself authoritative
```

**Resolution**:

1. **Detection**: When partition heals, compare VCs
   - East-West have same VC[east], EU has different
   - If both updated the same policy → CONFLICT

2. **Strategy 1: Last-Write-Wins** (Phase 1)
   ```
   Winner: EU (VC[eu]=10 > 5)
   Loser: East (VC[eu]=5)
   Action: East replays EU's version
   ```

3. **Strategy 2: CRDT Merge** (Phase 2)
   ```
   If policy is CRDT-mergeable:
     result = policy_east.merge(policy_eu)
     new_version = max_version + 1
     new_vc = merge_clocks([10,9,5], [5,0,10]) = [10,9,10]
   ```

4. **Evidence**: Capture conflict resolution

   ```rust
   pub struct ConflictRecord {
       pub conflict_id: String,
       pub affected_entity: String,
       pub entity_id: String,
       pub winning_version: String,
       pub losing_versions: Vec<String>,
       pub resolution_strategy: ResolutionStrategy,
       pub timestamp: Instant,
       pub vector_clock_winning: VectorClock,
       pub receipt_hash: String,  // Evidence ledger
   }

   pub enum ResolutionStrategy {
       LastWriteWins { winner_region: String },
       CrdtMerge { merged_value: serde_json::Value },
       ManualIntervention { operator_id: String },
   }
   ```

---

## 6. Conflict Resolution

### 6.1 Strategy Comparison

| Strategy | Latency | Consistency | Merge Loss | Audit Trail | Complexity |
|----------|---------|-------------|-----------|------------|-----------|
| **Last-Write-Wins** | <10ms | Eventual | ~5-10% | Full | Low |
| **CRDT (Phase 2)** | <50ms | Strong | ~0% | Full | Medium |
| **Application Merge** | <100ms | Strong | 0% | Full | High |
| **Manual Intervention** | Hours | Strong | 0% | Full | Very High |

### 6.2 Last-Write-Wins (LWW) - Phase 1

**Algorithm**:

```rust
pub fn resolve_conflict_lww(
    entity_id: &str,
    local_version: &EntityVersion,
    remote_version: &EntityVersion,
    local_vc: &VectorClock,
    remote_vc: &VectorClock,
) -> (EntityVersion, ConflictRecord) {
    let (winner, loser) = if remote_vc.happens_after(local_vc) {
        (remote_version.clone(), local_version.clone())
    } else if local_vc.happens_after(remote_vc) {
        (local_version.clone(), remote_version.clone())
    } else {
        // Concurrent writes: use lexicographic tiebreaker (region ID)
        // "us-east" < "us-west" < "eu"
        if local_version.region_id < remote_version.region_id {
            (local_version.clone(), remote_version.clone())
        } else {
            (remote_version.clone(), local_version.clone())
        }
    };

    let record = ConflictRecord {
        conflict_id: format!("conflict-{}-{}", entity_id, Instant::now().elapsed().as_millis()),
        affected_entity: "policy".to_string(),
        entity_id: entity_id.to_string(),
        winning_version: winner.id.clone(),
        losing_versions: vec![loser.id.clone()],
        resolution_strategy: ResolutionStrategy::LastWriteWins {
            winner_region: winner.region_id.clone(),
        },
        timestamp: Instant::now(),
        vector_clock_winning: if winner.region_id == local_version.region_id {
            local_vc.clone()
        } else {
            remote_vc.clone()
        },
        receipt_hash: "".to_string(),  // Set by evidence layer
    };

    (winner, record)
}
```

**Trade-off**: Fast (deterministic) but loses loser's data

**Example Scenario**:

```
Time 0:
  US-East: policy-1 = "v1" (VC=[1,0,0])
  EU:      policy-1 = "v1" (VC=[1,0,0])

Time 1 (Partition):
  US-East: policy-1 = "v2" (VC=[2,0,0])  ← Autonomic update
  EU:      policy-1 = "v2'" (VC=[1,0,1]) ← Different decision

Time 2 (Partition Heals):
  Compare: VC=[2,0,0] vs VC=[1,0,1]
  Result: Neither dominates (concurrent)
  Tiebreaker: "us-east" < "eu" → US-East wins
  Action: EU replays "v2", discards "v2'"
  Loss: EU's autonomic decision lost (must audit!)
```

### 6.3 CRDT Strategy (Phase 2)

**Rationale**: For governance policies, use CRDT to avoid data loss

**Example: Policy Updates with CRDTs**

```rust
// Phase 2: CRDT-based policy (logical clock, not time-based)
pub struct CRDTPolicyUpdate {
    pub policy_id: String,
    pub operation: PolicyOperation,
    pub timestamp: u64,  // Logical timestamp (not wall-clock)
    pub actor_id: String,
    pub actor_region: String,
}

pub enum PolicyOperation {
    SetGoal { goal_name: String, target: String },
    AddDomain { domain_id: String },
    RemoveDomain { domain_id: String },  // With-Remove semantics
    SetMetric { metric_id: String, value: f64 },
}

impl CRDTPolicyUpdate {
    /// Merge two concurrent updates into a single consistent state
    pub fn merge(
        local_update: &CRDTPolicyUpdate,
        remote_update: &CRDTPolicyUpdate,
    ) -> (CRDTPolicyUpdate, ConflictRecord) {
        // If ops are commutative (e.g., Set vs Set), use LWW
        // If ops are non-commutative (e.g., Add vs Remove), use CRDT semantics:
        //   - Add-Remove: Remove wins (tombstone)
        //   - Set-Set: Later timestamp wins
        //   - Add-Add: Idempotent (set union)

        match (&local_update.operation, &remote_update.operation) {
            // Commutative: Set is idempotent
            (
                PolicyOperation::SetGoal { goal_name: ln, target: lt },
                PolicyOperation::SetGoal { goal_name: rn, target: rt },
            ) if ln == rn => {
                // Same goal, set to later value
                let winner = if remote_update.timestamp > local_update.timestamp {
                    remote_update.clone()
                } else {
                    local_update.clone()
                };
                (winner, ConflictRecord::default())  // No conflict
            }

            // Non-commutative: Add vs Remove → Remove wins
            (
                PolicyOperation::AddDomain { domain_id: domain_a },
                PolicyOperation::RemoveDomain { domain_id: domain_r },
            ) if domain_a == domain_r => {
                // Conflict: one adds, one removes
                let winner = CRDTPolicyUpdate {
                    policy_id: local_update.policy_id.clone(),
                    operation: PolicyOperation::RemoveDomain { domain_id: domain_a.clone() },
                    timestamp: remote_update.timestamp.max(local_update.timestamp) + 1,
                    actor_id: "merge".to_string(),
                    actor_region: "system".to_string(),
                };
                (winner, ConflictRecord::default())
            }

            // Default: Keep both (idempotent ops)
            _ => (local_update.clone(), ConflictRecord::default()),
        }
    }
}
```

**Advantage**: No data loss, commutative operations merge safely

**Complexity**: Requires policy authors to reason about CRDTs

### 6.4 Application-Level Merging (Phase 2)

**Use Case**: Custom merge logic for domain-specific policies

```rust
pub trait ConflictResolvable {
    fn merge_with(&self, other: &Self) -> Result<Self>;
    fn can_merge(&self, other: &Self) -> bool;
}

impl ConflictResolvable for GovernancePolicy {
    fn merge_with(&self, other: &GovernancePolicy) -> Result<GovernancePolicy> {
        // Example: Merge two workflow policies
        // - Union goals (if compatible)
        // - Keep stricter constraints
        // - Log rationale in evidence plane

        let merged_goals = self.goals.iter()
            .chain(&other.goals)
            .cloned()
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect();

        Ok(GovernancePolicy {
            id: self.id.clone(),
            goals: merged_goals,
            version: (self.version.max(other.version)) + 1,
            merged_at: Instant::now(),
        })
    }

    fn can_merge(&self, other: &GovernancePolicy) -> bool {
        // Policy A can merge with Policy B if goals are compatible
        self.goals.iter().zip(&other.goals)
            .all(|(ga, gb)| ga.compatible_with(gb))
    }
}
```

---

## 7. Recovery Mechanisms

### 7.1 Region Recovery Flow

```
Regional Failure Detected
         │
         ├─→ [Health Monitor] Mark region as Failed
         │
         ├─→ [Circuit Breaker] Stop sending writes
         │
         ├─→ [Replication Queue] Accumulate queued events
         │                        (max: 10k events / 5 minutes)
         │
         └─→ [Recovery Service] On region rejoining:
              │
              ├─→ Stage 1: Vector Clock Synchronization (30s)
              │   - Exchange VC from both sides
              │   - Determine recovery point
              │
              ├─→ Stage 2: State Snapshot (60s)
              │   - US-East sends full snapshot
              │   - EU applies locally
              │
              ├─→ Stage 3: Event Replay (60s)
              │   - Replay queued events in VC order
              │   - Apply conflict resolution
              │
              ├─→ Stage 4: Verification (30s)
              │   - Verify checksums match
              │   - Compare vector clocks
              │   - Validate evidence ledger
              │
              └─→ Stage 5: Handoff (10s)
                  - Mark region as Healthy
                  - Resume replication
                  - Emit recovery receipt
```

### 7.2 Recovery Algorithm

```rust
pub struct RecoveryService {
    pub failed_region: String,
    pub recovery_timeout: Duration,
    pub snapshot_path: PathBuf,
}

impl RecoveryService {
    pub async fn initiate_recovery(&self, failed_region: &str) -> Result<RecoveryReport> {
        // Stage 1: Vector clock sync
        let remote_vc = self.fetch_remote_vector_clock(failed_region).await?;
        let local_vc = self.get_local_vector_clock();

        let recovery_point = self.determine_recovery_point(&local_vc, &remote_vc)?;
        info!("Recovery point: VC={:?}", recovery_point);

        // Stage 2: Snapshot exchange
        let snapshot = self.create_state_snapshot(&recovery_point).await?;
        self.send_snapshot_to_region(failed_region, &snapshot).await?;

        // Stage 3: Event replay
        let queued_events = self.get_queued_events_since(&recovery_point).await?;
        let ordered_events = self.order_by_vector_clock(&queued_events)?;

        for event in ordered_events {
            self.apply_event_to_region(failed_region, &event).await?;
            self.update_vector_clock(&event.vector_clock)?;
        }

        // Stage 4: Verification
        let remote_final_vc = self.fetch_remote_vector_clock(failed_region).await?;
        let local_final_vc = self.get_local_vector_clock();

        if remote_final_vc != local_final_vc {
            return Err(OSIRISError::RecoveryVerificationFailed {
                expected: format!("{:?}", local_final_vc),
                actual: format!("{:?}", remote_final_vc),
            });
        }

        // Stage 5: Handoff
        self.mark_region_healthy(failed_region).await?;
        self.emit_recovery_receipt(failed_region, &recovery_point).await?;

        Ok(RecoveryReport {
            region: failed_region.to_string(),
            duration: self.recovery_timeout,
            events_replayed: queued_events.len(),
            checkpoint_vc: recovery_point,
            status: "success".to_string(),
        })
    }

    async fn determine_recovery_point(
        &self,
        local_vc: &VectorClock,
        remote_vc: &VectorClock,
    ) -> Result<VectorClock> {
        // Recovery point is where local and remote last agreed
        // (component-wise minimum)
        Ok(VectorClock {
            us_east: local_vc.us_east.min(remote_vc.us_east),
            us_west: local_vc.us_west.min(remote_vc.us_west),
            eu: local_vc.eu.min(remote_vc.eu),
        })
    }
}

pub struct RecoveryReport {
    pub region: String,
    pub duration: Duration,
    pub events_replayed: usize,
    pub checkpoint_vc: VectorClock,
    pub status: String,
}
```

### 7.3 Recovery Timeout & Abort

**Timeout**: 5 minutes per stage (20 minutes total)

**Abort Conditions**:
- Verification fails (checksums don't match)
- Vector clocks diverge after replay
- Region becomes unreachable during recovery
- Evidence ledger integrity check fails

**On Abort**:

```rust
pub async fn abort_recovery(&self, reason: &str) -> Result<()> {
    // 1. Alert operator (PagerDuty)
    // 2. Emit signal: ANDON_RECOVERY_FAILED
    // 3. Put region in "Requires Operator" state
    // 4. Preserve recovery logs for manual inspection
    // 5. Fallback to LocalOnly degradation mode
    Ok(())
}
```

---

## 8. Data Flow Architecture

### 8.1 Write Path (US-East Primary)

```
Client
  │
  └─→ [1. Ingress] Validate request
      └─→ [2. Governance Service] Evaluate policy
          └─→ [3. Decision] Accept/Reject
              └─→ [4. Execution] Apply change
                  └─→ [5. Persistence] Write to Firestore
                      └─→ [6. Vector Clock] Increment VC[us-east]
                          └─→ [7. Receipt] Emit signed receipt
                              └─→ [8. Replication Event] Emit to Pub/Sub
                                  ├─→ [US-West] Queue and apply
                                  └─→ [EU] Queue and apply
                                      └─→ [9. Ack] Return to client
                                          (after 2/3 received)
```

### 8.2 Replication Event Processing (Replica Regions)

```
Pub/Sub Topic
  │
  └─→ [Replication Worker] Receive event
      │
      ├─→ [1. Validate] Checksum + vector clock
      │
      ├─→ [2. Idempotence] Check if already applied
      │   (using event ID + region)
      │
      ├─→ [3. Order Check] Vector clock causal order
      │   - If dependencies missing, defer
      │   - If dependencies satisfied, proceed
      │
      ├─→ [4. Conflict Detection]
      │   - Read local version
      │   - Compare with incoming version
      │   - Trigger conflict resolver if needed
      │
      ├─→ [5. Apply] Write to Firestore
      │   (with event ID for idempotence)
      │
      ├─→ [6. Update VC] Merge vector clocks
      │
      └─→ [7. Emit Local Event]
          (for observability + evidence plane)
```

### 8.3 Read Path

```
Client (any region)
  │
  ├─→ [1. Check Locality] Same region as client?
  │   │
  │   ├─→ YES: [Local Read]
  │   │   └─→ Firestore local (US-East, US-West, or EU)
  │   │       └─→ <50ms
  │   │
  │   └─→ NO: [Global Read Options]
  │       │
  │       ├─→ [Option A: Eventual] Read from local region
  │       │   (may be stale by <500ms)
  │       │
  │       └─→ [Option B: Causal] Read from primary region
  │           + wait for VC ordering guarantee
  │           (<500ms typically)
```

### 8.4 Evidence Ledger Replication

```
Receipt Emission (US-East)
  │
  └─→ [1. Create Receipt]
      {
        "id": "receipt-2026-03-24-001",
        "action": "policy-update",
        "entity_id": "policy-xyz",
        "vector_clock": [42, 31, 28],
        "timestamp": "2026-03-24T14:30:45Z",
        "signature": "ed25519(payload)",
        "prior_hash": "sha256(receipt-previous)"
      }
      │
      ├─→ [2. Persist Ledger] Firestore (region-local)
      │   └─→ Document: receipts/receipt-2026-03-24-001
      │
      ├─→ [3. Emit to Evidence Replication Topic]
      │   ├─→ US-West subscribes
      │   └─→ EU subscribes
      │
      ├─→ [4. Hash-Chain Validation]
      │   (on each replica)
      │   - Verify prior_hash matches prior receipt
      │   - Verify signature
      │   - Append to local chain
      │
      └─→ [5. Archive to GCS] (daily batch)
          └─→ 7-year retention
```

---

## 9. Implementation Roadmap

### Phase 1: Active/Passive Replication (Months 1-3)

**Goal**: Basic replication with vector clocks + LWW conflict resolution

**Deliverables**:
1. Vector clock implementation (vecclock crate)
2. Replication event stream (Pub/Sub consumer)
3. Conflict detection (concurrent write detection)
4. Last-write-wins resolution algorithm
5. Recovery orchestrator (5-stage recovery)
6. Health monitoring + circuit breaker integration
7. Evidence ledger replication

**Metrics**:
- Replication latency: <500ms 99th percentile
- Recovery time: <5 minutes
- Conflict detection rate: 100% (all concurrent writes flagged)
- Conflict resolution: Deterministic (no manual intervention)

**Testing**:
- Unit: Vector clock arithmetic, conflict detection (80%+ coverage)
- Integration: 3-region simulated failure scenarios (Toxiproxy/Docker)
- Property: VC causal ordering invariants (proptest)
- Chaos: Netflix Chaos Monkey (network latency, packet loss)

**Checkpoints**:
- Week 2: Vector clock crate complete + tests passing
- Week 4: Replication consumer reading events from Pub/Sub
- Week 6: Conflict resolution algorithm implemented
- Week 8: Health monitoring detecting failures
- Week 10: Recovery orchestrator completing 5-stage flow
- Week 12: End-to-end test: 3-region failover + recovery

### Phase 2: Active/Active with CRDTs (Months 4-6)

**Goal**: CRDT-based merging for policy updates (no data loss)

**Deliverables**:
1. CRDT traits (Mergeable, Commutative)
2. Policy-specific CRDTs (GoalSet, DomainSet, etc.)
3. Merge algorithm with timestamp tiebreaker
4. Application-level conflict resolver interface
5. Evidence ledger captures merge rationale
6. Conflict resolution metrics + audit trail

**Metrics**:
- Data loss on conflict: 0% (CRDT guarantees)
- Merge latency: <50ms
- CRDT coverage: 95% of policy types
- Manual intervention: <1% of conflicts

**Testing**:
- Unit: CRDT merge correctness (80%+ coverage)
- Property: Associativity, commutativity, idempotence (proptest)
- Scenario: Concurrent policy updates across regions
- Equivalence: Compare LWW vs CRDT results

**Checkpoints**:
- Week 14: CRDT traits designed + policy types identified
- Week 16: 3 policy CRDTs implemented
- Week 18: Merge algorithm complete
- Week 20: Evidence ledger captures merge rationale
- Week 22: End-to-end CRDT merge test
- Week 24: Performance benchmarking + optimization

### Phase 3: Byzantine Fault Tolerance (Months 7-9)

**Goal**: Consensus-based ordering (strongest consistency)

**Deliverables**:
1. Raft consensus algorithm (3-region cluster)
2. Leader election + heartbeat
3. Log replication + commit semantics
4. Follower crash recovery
5. Network partition handling (majority rules)
6. Byzantine fault detector (Liskov/Castro algorithm)
7. Transition from eventually-consistent to strongly-consistent

**Metrics**:
- Consensus latency: <100ms (with 3 replicas)
- Byzantine node tolerance: Up to 1 of 3 faulty
- Linearizability: 100% of reads/writes ordered
- Split-brain recovery: Automatic (no manual intervention)

**Testing**:
- Jepsen-style testing: Distributed system verification
- Fault injection: Up to 3 simultaneous failures
- Network conditions: Partition, latency, packet loss
- Byzantine: Faulty node (wrong results, silence, corruption)

**Checkpoints**:
- Month 7: Raft algorithm implemented
- Month 8: Consensus working across 3 regions
- Month 8.5: Byzantine detector operational
- Month 9: End-to-end Byzantine test suite

---

## 10. Appendices

### A. Vector Clock Example Trace

```
Initial State (all regions at time 0):
  policy-1 = "allow_goal_setting"
  VC[east=0, west=0, eu=0]

--- Timeline ---

T1 (US-East): Update policy-1 = "require_goal_review"
  Event 1: policy-update (id=evt-1)
  VC after: [1, 0, 0]
  Store: Firestore (us-east region)

T2 (US-West): Receive event-1 from US-East
  Event 1 (remote): policy-update (id=evt-1, VC=[1,0,0])
  VC after merge: [1, 1, 0]
  Store: Firestore (us-west region)

T3 (EU): Receive event-1 from US-East
  Event 1 (remote): policy-update (id=evt-1, VC=[1,0,0])
  VC after merge: [1, 0, 1]
  Store: Firestore (eu region)

T4 (US-West): Update policy-1 = "require_stakeholder_approval"
  Event 2: policy-update (id=evt-2)
  VC before: [1, 1, 0]
  VC after: [1, 2, 0]  ← Depends on event-1 (same entity)
  Store: Firestore (us-west region)
  → CONFLICT! Two versions of policy-1

Conflict Resolution (LWW):
  Version A (East): "require_goal_review", VC=[1,0,0]
  Version B (West): "require_stakeholder_approval", VC=[1,2,0]
  Winner: Version B (VC[west]=2 > 0)
  → East will replay West's version

T5 (US-East): Receive event-2 from US-West
  Event 2 (remote): policy-update (id=evt-2, VC=[1,2,0])
  VC after merge: [1, 2, 0]
  Detect: policy-1 has both versions
  Conflict Resolution: LWW chooses event-2
  Store: Firestore (us-east region, updated)

Final State (all regions consistent):
  policy-1 = "require_stakeholder_approval"
  VC[east=1, west=2, eu=0]  ← EU lags (hasn't received evt-2 yet)

T6 (EU): Receive event-2 from replication queue
  Event 2 (remote): policy-update (id=evt-2, VC=[1,2,0])
  VC after merge: [1, 2, 1]
  Store: Firestore (eu region, updated)

Final State (all regions converged):
  policy-1 = "require_stakeholder_approval"
  VC[east=1, west=2, eu=1]
  → Causal consistency achieved!
```

### B. Configuration (Pseudocode)

```yaml
# Multi-Region Configuration
multi_region:
  regions:
    - name: us-east
      role: primary
      location: us-east1
      leadership_weight: 3

    - name: us-west
      role: replica
      location: us-west1
      leadership_weight: 1

    - name: eu
      role: replica
      location: europe-west1
      leadership_weight: 1

  replication:
    strategy: vector_clock_causal
    channel: google_pubsub
    topic: multi-region-replication-events
    batch_size: 100
    batch_timeout_ms: 500

  consistency:
    default_read: local_read
    default_write: replicated_write

  conflict_resolution:
    strategy: last_write_wins  # Phase 1, CRDT in Phase 2
    evidence_ledger: enabled

  recovery:
    timeout_secs: 300
    stages:
      - vc_sync: 30
      - snapshot: 60
      - event_replay: 60
      - verification: 30
      - handoff: 10

  degradation:
    single_region_lag_threshold: 2000  # ms
    circuit_breaker_timeout: 30  # seconds
    fallback_to_local_only_after: 5  # minutes
```

### C. Metrics & Observability

**Prometheus Metrics**:

```
# Replication latency
multi_region_replication_latency_ms{region_from, region_to}  # histogram
multi_region_replication_lag_events{region}                  # gauge

# Conflict rate
multi_region_conflict_detected_total{entity_type}  # counter
multi_region_conflict_resolved_total{strategy}  # counter (lww, crdt, manual)
multi_region_conflict_loss_total{entity_id}  # counter (data loss count)

# Recovery
multi_region_recovery_initiated_total{region}  # counter
multi_region_recovery_duration_seconds{region}  # histogram
multi_region_recovery_events_replayed{region}  # gauge

# Degradation
multi_region_degradation_level{current_level}  # gauge (nominal, lag, partition, local_only)
multi_region_circuit_breaker_status{region}  # gauge (open, closed)

# Evidence ledger
multi_region_receipt_replication_latency_ms  # histogram
multi_region_receipt_hash_chain_validated_total  # counter
multi_region_receipt_verification_failed_total  # counter
```

**Jaeger (Distributed Tracing)**:

- Trace ID: Includes region + vector clock + request ID
- Spans: Ingress → Service → Persistence → Replication → Ack
- Tags: region, entity_id, vector_clock, conflict_detected

**Logging**:

- Level: DEBUG for replication events, INFO for conflicts/recovery
- Structured: JSON with vector_clock, region, event_id, conflict_record
- Destination: Cloud Logging (tamper-proof audit trail)

### D. Testing Strategy

**Failure Injection**:

```rust
#[tokio::test]
async fn test_single_region_failure() {
    // Setup: 3-region cluster, simulate US-West failure
    let cluster = TestCluster::new(3).await;
    cluster.start().await;

    // Inject failure
    cluster.regions["us-west"].kill().await;

    // Assert: Traffic reroutes, replication queues
    assert_eq!(cluster.degradation_level(), DegradationLevel::SingleRegionLag);
    assert!(cluster.regions["us-west"].is_offline());

    // Recover region
    cluster.regions["us-west"].restart().await;

    // Assert: Vector clocks sync, events replayed
    assert_eq!(cluster.degradation_level(), DegradationLevel::Nominal);
    assert_consistency_across_regions(&cluster).await;
}

#[tokio::test]
async fn test_network_partition() {
    let cluster = TestCluster::new(3).await;
    cluster.start().await;

    // Partition: US-East/West connected, EU isolated
    cluster.partition(vec!["eu"], vec!["us-east", "us-west"]).await;

    // Assert: Majority rules, EU becomes read-only
    assert_eq!(cluster.regions["eu"].mode(), WriteMode::ReadOnly);
    assert_eq!(cluster.regions["us-east"].mode(), WriteMode::ReadWrite);

    // Heal partition
    cluster.heal_partition().await;

    // Assert: LWW conflict resolution triggered
    let conflicts = cluster.conflict_log().await;
    assert!(conflicts.len() > 0);
    assert_consistency_across_regions(&cluster).await;
}
```

### E. Glossary of Terms

| Term | Definition |
|------|-----------|
| **Vector Clock** | Tuple `[ts_east, ts_west, ts_eu]` tracking causal dependencies |
| **Causal Consistency** | If A → B causally, all processes see A before B |
| **Eventual Consistency** | Replicas converge given sufficient time (no ordering guarantee) |
| **Conflict** | Concurrent writes to same entity in different regions |
| **Partition** | Network disconnect preventing inter-region communication |
| **Split-Brain** | Both sides of partition commit conflicting writes |
| **Last-Write-Wins** | Winner determined by latest wall-clock timestamp (LWW strategy) |
| **CRDT** | Conflict-free Replicated Data Type (merger-friendly structure) |
| **Receipt** | Cryptographically signed proof of action (evidence ledger) |
| **Ledger** | Immutable append-only log (Firestore + Cloud Logging) |
| **Recovery** | Process of syncing failed region back to cluster state |
| **Degradation** | Fallback to weaker consistency mode (e.g., LocalOnly) |
| **Circuit Breaker** | Mechanism to stop requests to failing component |

---

## Summary

This design document specifies a **three-region active/active architecture** for ggen with:

1. **Vector Clock Replication**: Causal ordering of events across regions
2. **Last-Write-Wins + CRDT**: Two conflict resolution strategies (LWW in Phase 1, CRDT in Phase 2)
3. **Graceful Degradation**: Automatic fallback to local-only mode under cascading failures
4. **5-Stage Recovery**: Deterministic region rejoin process (20 min timeout)
5. **Evidence Ledger**: Cryptographic receipts replicated across regions for auditability

**Key Invariants**:
- No data loss in Phase 2 (CRDT guarantees idempotent merge)
- Deterministic conflict resolution (no manual intervention until Phase 3)
- Sub-100ms local latency maintained
- 99.99% availability target achievable in Phase 1

**Next Steps**: Start Phase 1 implementation with vector clock crate + Pub/Sub consumer.

---

**Document Version**: 1.0 | **Last Updated**: 2026-03-24 | **Status**: Ready for Implementation Planning
