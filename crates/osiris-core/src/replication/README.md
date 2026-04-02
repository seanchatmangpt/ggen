# Multi-Region Failover Design - ggen OSIRIS

**Complete Design Suite for Active/Active Replication Across US-East, US-West, EU**

**Version**: 1.0 | **Date**: 2026-03-24 | **Status**: Design Phase (No Implementation)

---

## Overview

This directory contains the complete design specification for ggen's multi-region failover architecture, enabling:

- **99.99% Availability**: Active/active across 3 geographic regions
- **Eventual Consistency**: Vector clock-based causal ordering
- **Automatic Recovery**: Region rejoin in <5 minutes
- **Zero Data Loss (Phase 2)**: CRDT-based conflict resolution
- **Audit Ready**: Cryptographically signed, immutable evidence ledger

---

## Documents in This Suite

### 1. **MULTI_REGION_DESIGN.md** (Primary Architecture)
**The master specification document** (1800 lines)

**Contains**:
- System context and architecture overview
- Replication strategy (what gets replicated, where, when)
- Consistency model (vector clocks, eventual consistency)
- 4 failure scenarios with detailed analysis
  - Single region failure
  - Network partition (split-brain)
  - Cascading failures
  - Both partitions claim leadership
- Recovery mechanisms (5-stage recovery process)
- Data flow architecture (write/read paths)
- 3-phase implementation roadmap (Phase 1: LWW, Phase 2: CRDTs, Phase 3: Byzantine)

**Best for**:
- Architects designing the overall system
- DevOps engineers understanding multi-region topology
- Compliance officers verifying recovery guarantees

---

### 2. **CONFLICT_RESOLUTION_STRATEGY.md** (Detailed Algorithms)
**Deep-dive on three conflict resolution approaches** (1200 lines)

**Contains**:
- **Strategy 1: Last-Write-Wins (LWW)** — Phase 1
  - Algorithm, Rust code patterns
  - Trade-offs: Fast (10ms) but loses data (~5%)
  - Example scenarios showing silent data loss

- **Strategy 2: CRDTs** — Phase 2
  - Commutative + idempotent operations
  - 3 CRDT types: LWW-Element-Set, OR-Set, G-Counter
  - Merge algorithms with proofs
  - No data loss, slightly slower (50ms)

- **Strategy 3: Application-Level Merging** — Phase 3
  - Custom domain-specific logic
  - Merge confidence scoring
  - Determinism vs flexibility trade-offs
  - Example: Workflow policy merge

- **Comparative Analysis**:
  - Data loss by strategy
  - Latency profiles
  - Complexity vs safety

- **Decision Framework**:
  - When to use each strategy
  - Entity classification (Goals, Policies, Workflows, etc.)
  - Recommended roadmap

**Best for**:
- Database engineers implementing conflict resolution
- Policy authors designing merge logic for their entities
- QA engineers writing conflict scenarios

---

### 3. **DATA_FLOW_DIAGRAMS.md** (Visual Specifications)
**Detailed sequence diagrams and flows** (1600 lines)

**Contains**:
- Architecture diagram (3-region deployment)
- **Write Path**: Local write → replication → global propagation
  - Happy path with latency breakdown
  - Replication worker processing (9 steps)
  - Idempotence checking

- **Read Path**: Local vs global consistency options
  - Local read (30ms, strong consistency)
  - Global read with causal wait (100ms, eventual consistency)

- **Replication Event Processing**: Step-by-step flowchart
  - Validation → Deduplication → VC ordering → Conflict detection → Apply

- **Conflict Detection & Resolution Flow**: Detailed scenario
  - Concurrent writes during partition
  - Conflict record generation
  - Evidence ledger capture

- **Evidence Ledger Replication**: Immutable audit trail
  - Receipt generation with hash-chain
  - Tamper detection
  - SPARQL query examples
  - GCS archival strategy

- **Region Recovery Flow**: 5-stage process
  - Stage 1: Vector clock sync (30s)
  - Stage 2: State snapshot (60s)
  - Stage 3: Event replay (60s)
  - Stage 4: Verification (30s)
  - Stage 5: Handoff (10s)

- **Failure Scenarios**: Network partitions + split-brain
- **Timing & Latency**: End-to-end breakdown

**Best for**:
- Implementation engineers building replication logic
- Testing engineers writing scenario-based tests
- SREs understanding operational flows

---

## How to Navigate

### By Role

| Role | Start Here | Then Read |
|------|-----------|-----------|
| **Architect** | Section 1 (Overview) → MULTI_REGION_DESIGN.md | CONFLICT_RESOLUTION_STRATEGY.md |
| **Backend Engineer** | DATA_FLOW_DIAGRAMS.md | MULTI_REGION_DESIGN.md (Sections 3-5) |
| **DevOps Engineer** | MULTI_REGION_DESIGN.md (Section 2) | DATA_FLOW_DIAGRAMS.md (Architecture) |
| **Database Engineer** | CONFLICT_RESOLUTION_STRATEGY.md | DATA_FLOW_DIAGRAMS.md (Sections 4-5) |
| **QA Engineer** | DATA_FLOW_DIAGRAMS.md | MULTI_REGION_DESIGN.md (Section 5) |
| **Compliance Officer** | MULTI_REGION_DESIGN.md (Section 6) | DATA_FLOW_DIAGRAMS.md (Evidence Ledger) |

### By Topic

| Topic | Primary | Secondary |
|-------|---------|-----------|
| **Replication** | MULTI_REGION_DESIGN.md (Section 3) | DATA_FLOW_DIAGRAMS.md (Section 4) |
| **Consistency** | MULTI_REGION_DESIGN.md (Section 4) | CONFLICT_RESOLUTION_STRATEGY.md |
| **Failure Handling** | MULTI_REGION_DESIGN.md (Section 5) | DATA_FLOW_DIAGRAMS.md (Section 8) |
| **Conflict Resolution** | CONFLICT_RESOLUTION_STRATEGY.md | DATA_FLOW_DIAGRAMS.md (Section 5) |
| **Recovery** | MULTI_REGION_DESIGN.md (Section 7) | DATA_FLOW_DIAGRAMS.md (Section 7) |
| **Evidence/Audit** | DATA_FLOW_DIAGRAMS.md (Section 6) | MULTI_REGION_DESIGN.md (Section 3.1) |
| **Implementation** | MULTI_REGION_DESIGN.md (Section 9) | All sections for Phase details |

---

## Key Concepts

### Vector Clock
A tuple `[ts_us_east, ts_us_west, ts_eu]` tracking causal dependencies between events. If event A causally happens before B, then `vc(A) < vc(B)` componentwise.

**Example**:
```
T0: Write policy-1 (US-East) → VC=[1,0,0]
T1: Receive in US-West → Merge VC → [1,1,0]
T2: Write policy-2 (US-West, depends on T0) → VC=[1,2,0]
```

### Causal Consistency
Replicas converge to the same state respecting causal dependencies. If write A → write B (A causally before B), all regions see A before B.

**Phase 1 (LWW)**: Deterministic but loses data
**Phase 2 (CRDT)**: No data loss, idempotent merge
**Phase 3 (Consensus)**: Linearizable, strongest guarantee

### Conflict
Concurrent writes to the same entity in different regions without clear ordering.

**Detection**: Neither region's vector clock dominates the other
**Resolution**: Last-write-wins (tiebreaker: region ID)
**Phase 2+**: CRDT merge if applicable

### Recovery Point
The vector clock position where a failed region should rejoin from. Computed as component-wise minimum of local and remote clocks.

### Evidence Ledger
Immutable, hash-chained append-only log of all actions (receipts). Stored in Firestore + Cloud Logging, archived to GCS (7-year retention).

---

## Architecture Layers

### 1. Data Layer (Mutable)
- **Storage**: Firestore (region-local, replicated via replication events)
- **Consistency**: Eventual (with causal ordering via VC)
- **Example**: Policies, domains, workflows

### 2. Control Layer (Ephemeral)
- **Storage**: In-memory, circuit breaker state, replication queue
- **Consistency**: Local region only
- **Example**: Request buffers, locks, semaphores

### 3. Evidence Layer (Immutable)
- **Storage**: Firestore receipts collection + Cloud Logging
- **Consistency**: Durable + hash-chained across regions
- **Example**: All actions with signatures + prior receipt hash

---

## Implementation Phases

### Phase 1: Active/Passive Replication (Q2 2026, 3 months)

**Goal**: Prove replication concept works

**Deliverables**:
- Vector clock implementation
- Event stream replication (Pub/Sub)
- Last-write-wins conflict resolution
- Health monitoring + circuit breaker
- 5-stage recovery orchestrator

**Metrics**:
- Replication latency: <500ms (P99)
- Recovery time: <5 minutes
- Conflict detection: 100%
- Zero data loss: N/A (acceptable to lose ~5%)

**Testing**: Unit + integration + chaos engineering

### Phase 2: Active/Active with CRDTs (Q3 2026, 3 months)

**Goal**: Zero data loss for commutative operations

**Deliverables**:
- CRDT traits (Mergeable, Commutative)
- 3 policy-specific CRDTs (Sets, Counters)
- Merge algorithms with proofs
- CRDT-aware conflict resolver
- Evidence ledger captures merge rationale

**Metrics**:
- Data loss: 0% (CRDT guarantees)
- Merge latency: <50ms
- CRDT coverage: 95% of entities

**Testing**: Property-based testing (commutativity, idempotence)

### Phase 3: Byzantine Fault Tolerance (Q4 2026, 3 months)

**Goal**: Strongest consistency model (linearizability)

**Deliverables**:
- Raft consensus (3-region cluster)
- Leader election + log replication
- Byzantine fault detector
- Transition from eventual → strong consistency
- Automatic split-brain resolution

**Metrics**:
- Linearizability: 100% of reads/writes
- Byzantine tolerance: 1 faulty node (of 3)
- Consensus latency: <100ms

**Testing**: Jepsen-style distributed system verification

---

## Key Decisions Made

### Replication Strategy
- **Decision**: Vector clock causal ordering (not wall-clock timestamps)
- **Rationale**: Clock skew immune, deterministic across regions
- **Trade-off**: Slightly more complex, but correct

### Conflict Resolution
- **Phase 1**: Last-Write-Wins (deterministic, 10ms, ~5% data loss)
- **Phase 2**: CRDTs (commutative ops, 50ms, zero loss)
- **Phase 3**: Application-level (domain-specific, 100ms+)
- **Rationale**: Staged approach allows shipping fast while improving over time

### Recovery Strategy
- **Decision**: 5-stage deterministic recovery (not consensus-based)
- **Rationale**: Faster (5 min vs 10+ min), proven in practice, manually auditable
- **Timeout**: 300s total, aborts if verification fails

### Evidence Ledger
- **Decision**: Immutable hash-chain receipts, 7-year GCS archive
- **Rationale**: Compliance + auditability, separates proof from mutable data
- **Tamper detection**: Any modification breaks hash chain immediately

---

## Testing Strategy

### Unit Tests
- Vector clock arithmetic, ordering, merge
- Conflict detection (concurrent write scenarios)
- LWW tiebreaker logic
- CRDT merge correctness (Phase 2)

### Integration Tests
- 3-region simulated cluster (Toxiproxy network simulation)
- Single region failure + recovery
- Network partition + split-brain
- Cascading failures
- Concurrent writes + conflict resolution

### Chaos Engineering
- Netflix Chaos Monkey style
- Random failures: region kill, network latency, packet loss
- Verify: Consistency after recovery, no data corruption

### Property-Based Tests (Phase 2+)
- CRDT idempotence: `merge(A, B) == merge(A, B)`
- CRDT commutativity: `merge(A, B) == merge(B, A)`
- VC ordering: If A → B causally, then `vc(A) < vc(B)`

### Jepsen-Style Tests (Phase 3)
- Distributed system verification
- Byzantine node injection (wrong results, silence, corruption)
- Network conditions: partition, latency, packet loss
- Verify linearizability of final state

---

## SLO Targets

| Metric | Target | Validation |
|--------|--------|-----------|
| **Write latency (P50)** | <50ms | Pre-commit test |
| **Write latency (P99)** | <150ms | Pre-commit test |
| **Read latency (P50)** | <50ms | Pre-commit test |
| **Replication lag (P99)** | <500ms | Integration test |
| **Recovery time** | <5 minutes | Chaos test |
| **Data consistency** | 100% eventually | Jepsen test |
| **Availability** | 99.99% | Fleet monitoring |
| **Evidence audit trail** | 100% (no loss) | Ledger verification |

---

## Glossary

| Term | Definition |
|------|-----------|
| **Vector Clock** | Tuple `[ts_east, ts_west, ts_eu]` tracking causal dependencies |
| **Causal Consistency** | If A → B causally, all regions see A before B |
| **Eventual Consistency** | Replicas converge given sufficient time |
| **Conflict** | Concurrent writes to same entity in different regions |
| **Partition** | Network disconnect preventing inter-region communication |
| **Split-Brain** | Both sides of partition commit conflicting writes |
| **LWW** | Last-Write-Wins: Winner determined by VC or timestamp |
| **CRDT** | Conflict-free Replicated Data Type (merger-friendly) |
| **Receipt** | Cryptographically signed proof of action |
| **Ledger** | Immutable append-only log (Firestore + Cloud Logging) |
| **Recovery** | Process of syncing failed region to cluster state |
| **Degradation** | Fallback to weaker consistency (e.g., LocalOnly) |
| **Circuit Breaker** | Stop requests to failing component |

---

## Document Statistics

| Document | Lines | Size | Key Sections |
|----------|-------|------|--------------|
| MULTI_REGION_DESIGN.md | 1800 | 68K | 10 sections, 3 phases |
| CONFLICT_RESOLUTION_STRATEGY.md | 1200 | 45K | 3 strategies, decision framework |
| DATA_FLOW_DIAGRAMS.md | 1600 | 60K | 9 flows, detailed sequences |
| **Total** | **4600** | **173K** | **20+ detailed sections** |

---

## Definition of Done

All documents complete when:

- [ ] ✅ 3 documents reviewed by architecture team
- [ ] ✅ All diagrams reviewed for accuracy
- [ ] ✅ Vector clock algorithm validated
- [ ] ✅ Conflict resolution strategies documented
- [ ] ✅ 5-stage recovery process specified
- [ ] ✅ Evidence ledger design complete
- [ ] ✅ 3-phase roadmap with checkpoints
- [ ] ✅ Latency SLOs specified (all operations)
- [ ] ✅ Test strategy documented
- [ ] ✅ Glossary complete
- [ ] ✅ Implementation guide ready
- [ ] ✅ Ready for Phase 1 implementation (Q2 2026)

---

## Next Steps

### Immediate (Week 1)
1. **Review & Approval**: Architecture team review of all 3 documents
2. **Feedback Loop**: Integrate feedback, iterate on designs
3. **Tech Spec**: Translate design into implementation tech specs

### Phase 1 (Q2 2026, 3 months)
1. **Crate Creation**: `crates/osiris-replication` with vector clock + event stream
2. **Replication Worker**: Pub/Sub consumer + idempotence
3. **Conflict Resolver**: LWW implementation
4. **Recovery Orchestrator**: 5-stage recovery state machine
5. **Health Monitoring**: Circuit breaker + degradation detection
6. **Integration Tests**: 3-region cluster simulation
7. **Deployment**: GCP multi-region setup

### Phase 2 (Q3 2026, 3 months)
1. **CRDT Traits**: Mergeable + Commutative interfaces
2. **Policy CRDTs**: LWW-Set, OR-Set, G-Counter implementations
3. **Merge Algorithm**: Policy-specific merge logic
4. **Evidence Capture**: Merge rationale in receipts
5. **Testing**: Property-based + CRDT merge correctness

### Phase 3 (Q4 2026, 3 months)
1. **Consensus**: Raft implementation (3-region)
2. **Leader Election**: Heartbeat + quorum
3. **Byzantine Detector**: Liskov/Castro algorithm
4. **Automatic Recovery**: Split-brain resolution
5. **Testing**: Jepsen-style distributed system verification

---

## References

**External**:
- [Vector Clocks](https://en.wikipedia.org/wiki/Vector_clock)
- [CRDTs](https://crdt.tech/)
- [Raft Consensus](https://raft.github.io/)
- [Jepsen Testing](https://jepsen.io/)
- [Google Cloud Firestore Replication](https://cloud.google.com/firestore/docs/regions)

**Internal**:
- ggen Architecture: `/docs/10-architecture/`
- OSIRIS Design: `/docs/30-autonomics/`
- Evidence Plane: `/docs/10-architecture/evidence-plane.md`

---

## Contact & Questions

**Document Owner**: ggen Architecture Team
**Last Updated**: 2026-03-24
**Status**: Design Phase (Ready for Implementation Planning)

For questions or clarifications on any design decision, refer to the specific document section or contact architecture team.

---

**Version**: 1.0 | **Status**: Complete Design Suite | **Ready for Phase 1 Planning**
