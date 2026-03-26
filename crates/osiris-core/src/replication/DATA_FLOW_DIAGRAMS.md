# Data Flow Diagrams - Multi-Region Architecture

**Purpose**: Visual specifications for write path, replication path, read path, and recovery flow with detailed sequence diagrams.

**Version**: 1.0 | **Date**: 2026-03-24 | **Status**: Design Phase

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Write Path: Local Write to Global Propagation](#write-path-local-write-to-global-propagation)
3. [Read Path: Local vs Global Consistency](#read-path-local-vs-global-consistency)
4. [Replication Event Processing](#replication-event-processing)
5. [Conflict Detection & Resolution Flow](#conflict-detection--resolution-flow)
6. [Evidence Ledger Replication](#evidence-ledger-replication)
7. [Region Recovery Flow](#region-recovery-flow)
8. [Failure Scenarios](#failure-scenarios)
9. [Timing & Latency Breakdown](#timing--latency-breakdown)

---

## 1. Architecture Overview

### 1.1 Three-Region Deployment Model

```
                          ┌─────────────────────────────────┐
                          │   Global Load Balancer           │
                          │   (Anycast DNS, Cloud CDN)       │
                          └──────────┬──────────┬────────────┘
                                     │          │
                    ┌────────────────┴─┐    ┌──┴──────────────────┐
                    │                   │    │                     │
        ┌───────────▼────────┐ ┌──────────────▼──┐ ┌──────────────▼────┐
        │    US-East         │ │   US-West       │ │       EU           │
        │   (Primary)        │ │   (Replica)     │ │     (Replica)      │
        │                    │ │                 │ │                    │
        │ ┌────────────────┐ │ │ ┌────────────┐ │ │ ┌─────────────┐   │
        │ │ Governance     │ │ │ │ Governance │ │ │ │ Governance  │   │
        │ │ Service        │ │ │ │ Service    │ │ │ │ Service     │   │
        │ │ (Autonomic)    │ │ │ │ (Sync Mode)│ │ │ │ (Sync Mode) │   │
        │ └─┬──────────────┘ │ │ └────┬───────┘ │ │ └─────┬───────┘   │
        │   │                │ │      │         │ │       │           │
        │ ┌─▼──────────────┐ │ │ ┌────▼─────┐  │ │ ┌──────▼─────┐   │
        │ │ Firestore      │ │ │ │ Firestore │ │ │ │ Firestore  │   │
        │ │ (Primary Copy) │ │ │ │ (Replica) │ │ │ │ (Replica)  │   │
        │ └─┬──────────────┘ │ │ └────┬──────┘ │ │ └──────┬─────┘   │
        │   │                │ │      │        │ │        │          │
        │ ┌─▼──────────────┐ │ │ ┌────▼─────┐ │ │ ┌───────▼────┐   │
        │ │ Cloud Logging  │ │ │ │Cloud Log │ │ │ │ Cloud Log  │   │
        │ │ (Evidence)     │ │ │ │(Evidence)│ │ │ │ (Evidence) │   │
        │ └────────────────┘ │ │ └──────────┘ │ │ └────────────┘   │
        │                    │ │              │ │                   │
        └────┬───┬───────────┘ └──┬──┬────────┘ └────┬──┬──────────┘
             │   │                │  │               │  │
             └─┬─┼────────────────┼──┼───────────────┼──┘
               │ │                │  │               │
        ┌──────▼─▼────────────┬───▼──▼───────────────▼──────┐
        │                     │                              │
        │  Vector Clock Bus   │   Event Stream (Pub/Sub)     │
        │  (Causal Ordering)  │   (Replication Channel)      │
        │                     │                              │
        └─────────────────────┴──────────────────────────────┘
                │                       │
        ┌───────▼───────┐      ┌────────▼────────┐
        │ Consensus     │      │ Replication     │
        │ (Phase 3)     │      │ Workers         │
        │ (Raft/BFT)    │      │ (Idempotent)    │
        └───────────────┘      └─────────────────┘
```

---

## 2. Write Path: Local Write to Global Propagation

### 2.1 Happy Path (Primary Region - US-East)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│ Write Path: Policy Update in US-East                                        │
└─────────────────────────────────────────────────────────────────────────────┘

Time  Actor                  Action                          VC Status
────  ──────────────────────────────────────────────────────────────────────
T0    Client Request
      │
      └─→ POST /policies/policy-1
          { "goals": ["improve-health", "reduce-cost"] }

T0.1  Ingress Service       Validate request
      │
      └─→ [✓] Check rate limits, auth, schema
          [×] Return 400 if invalid

T0.2  Governance Service    Evaluate policy
      │
      └─→ Read current: policy-1 = v1
          Run autonomic checks
          [✓] Check goal compatibility
          [✓] Check stakeholder alignment
          → Decision: ACCEPT

T0.3  Persistence Layer     Write to local Firestore
      │
      ├─→ Create document: policies/policy-1
      │   {
      │     "id": "policy-1",
      │     "version": 2,
      │     "goals": ["improve-health", "reduce-cost"],
      │     "created": 1711270245,
      │     "updated": 1711270245.0123,
      │     "vector_clock": [1, 0, 0],
      │     "region": "us-east"
      │   }
      │
      └─→ [✓] Firestore ACK (durability: disk)
          Time: ~5ms

T0.4  Vector Clock Update   Increment local VC
      │
      └─→ VC[us-east]++
          Before: [0, 0, 0]
          After:  [1, 0, 0]

T0.5  Receipt Emission      Create signed receipt
      │
      ├─→ {
      │     "id": "receipt-2026-03-24-001",
      │     "action": "policy-update",
      │     "entity_id": "policy-1",
      │     "vector_clock": [1, 0, 0],
      │     "timestamp": "2026-03-24T14:30:45.012Z",
      │     "region": "us-east",
      │     "signature": "ed25519(...)",
      │     "prior_receipt_hash": "sha256(...)"
      │   }
      │
      └─→ Persist to Firestore (receipts collection)
          Time: ~3ms

T0.6  Replication Event     Emit to Pub/Sub
      │
      ├─→ Topic: multi-region-replication-events
      │   Message: {
      │     "event_id": "evt-12345",
      │     "origin_region": "us-east",
      │     "entity_type": "policy",
      │     "entity_id": "policy-1",
      │     "operation": "update",
      │     "payload": { /* policy data */ },
      │     "vector_clock": [1, 0, 0],
      │     "timestamp": 1711270245012,
      │     "checksum": "sha256(...)"
      │   }
      │
      └─→ [✓] Pub/Sub ACK
          Time: ~10ms

T0.7  Replication Queue     Queue for US-West + EU
      │
      ├─→ Queue west: evt-12345
      ├─→ Queue eu:   evt-12345
      │
      └─→ Immediate queuing (async)

T0.8  Client Response       Return success
      │
      ├─→ HTTP 200 OK
      │   {
      │     "status": "success",
      │     "entity": policy-1,
      │     "receipt_id": "receipt-2026-03-24-001",
      │     "replicated_to": ["us-west", "eu"]
      │   }
      │
      └─→ Time: ~30ms total (P50)

T0.9  Replication Workers   (Async) Process in replicas
      │   (US-West + EU)
      │
      └─→ See Section 2.2 below

────────────────────────────────────────────────────────────────────────────

Total Latency to Client:
  - Validation:        2ms
  - Policy Eval:       8ms
  - Firestore Write:   5ms
  - VC Update:         1ms
  - Receipt Emit:      3ms
  - Pub/Sub Emit:     10ms
  - Response Build:    1ms
  ─────────────────────────
  TOTAL (P50):        30ms
  TOTAL (P99):        75ms
  TOTAL (P99.9):     150ms
```

### 2.2 Replication Path (US-West & EU)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│ Replication Path: Event Processing in US-West                              │
└─────────────────────────────────────────────────────────────────────────────┘

Time  Component             Action                          Effect
────  ──────────────────────────────────────────────────────────────────────

T1    Pub/Sub Subscriber    Receive replication event
      │
      ├─→ Topic: multi-region-replication-events
      ├─→ Message: evt-12345 (policy-1 update)
      │
      └─→ Latency from emit: ~50ms (network)

T1.1  Validation Layer      Verify message integrity
      │
      ├─→ [✓] Parse JSON
      ├─→ [✓] Verify checksum matches
      ├─→ [✓] Verify signature (ed25519)
      │
      └─→ [✗] Reject if tampered

T1.2  Idempotence Check     Detect duplicates
      │
      ├─→ Query: Has evt-12345 been applied?
      │   (Dedupication table: event_id → applied_at)
      │
      └─→ If yes: Skip steps 1.3-1.7 (idempotent)
          If no: Continue

T1.3  Vector Clock Order    Check causal dependencies
      │
      ├─→ Incoming VC: [1, 0, 0]
      ├─→ Local VC:    [0, 0, 0]
      │
      ├─→ Check: Are dependencies satisfied?
      │   - Does local.us_east >= incoming.us_east? [0 >= 1] → NO!
      │   - Dependency missing: Event depends on earlier US-East event
      │
      ├─→ Action: DEFER event to pending queue
      │   Reason: Waiting for evt-11344 (prior event)
      │
      └─→ Retry after 100ms or when prior event arrives

T1.4  (After dependency satisfied)
      │
      ├─→ VC Check passes: [1, 0, 0] can now apply
      │   (because local VC was updated by prior event)
      │
      └─→ Continue to conflict detection

T1.5  Conflict Detection    Check for concurrent writes
      │
      ├─→ Current local value: policy-1 = v1 (from T0 sync)
      │   Incoming value:     policy-1 = v2 (from US-East)
      │   Incoming VC:        [1, 0, 0]
      │   Local VC:           [1, 0, 0]  (after dependency)
      │
      ├─→ Are they same entity? YES (id = policy-1)
      │   Are they different versions? YES (v1 ≠ v2)
      │   Do VCs conflict? Compare: [1,0,0] vs [1,0,0]
      │     → Neither dominates (equal)
      │     → CHECK TIEBREAKER
      │
      ├─→ Tiebreaker: Region ID
      │     Local region: us-west
      │     Remote region: us-east
      │     "eu" < "us-east" < "us-west"
      │     → us-east wins (lexicographically smaller)
      │
      └─→ NO CONFLICT (just ordering by VC)
          If conflict detected: See Section 5

T1.6  Apply Update         Write to local Firestore
      │
      ├─→ Document: policies/policy-1
      │   {
      │     "id": "policy-1",
      │     "version": 2,
      │     "goals": ["improve-health", "reduce-cost"],
      │     "vector_clock": [1, 0, 0],
      │     "region": "us-east",
      │     "replicated_from": "us-east",
      │     "replicated_at": 1711270245050,
      │     "local_vc_after_apply": [1, 0, 0]
      │   }
      │
      └─→ [✓] Firestore ACK
          Time: ~5ms

T1.7  VC Merge             Update local vector clock
      │
      ├─→ Local VC before:  [1, 0, 0]
      ├─→ Incoming VC:      [1, 0, 0]
      │
      ├─→ Merge: component-wise MAX
      │   [max(1,1), max(0,0), max(0,0)] = [1, 0, 0]
      │
      └─→ Local VC after:   [1, 0, 0]
          (No change, but now this region has applied this version)

T1.8  Emit Local Event     Log for observability
      │
      ├─→ Log: "Policy policy-1 replicated from us-east"
      │   Metrics:
      │     - replication_latency: 50ms
      │     - conflict_detected: false
      │     - idempotent_duplicate: false
      │
      └─→ For Jaeger tracing + monitoring

T1.9  Mark as Applied      Deduplication cleanup
      │
      └─→ Dedupe table: evt-12345 → applied_at: T1.6

────────────────────────────────────────────────────────────────────────────

Total Replication Latency (T0.6 → T1.9):
  - Network propagation:    50ms
  - Validation:             2ms
  - Idempotence check:      1ms
  - VC order check:        10ms (assume no defer)
  - Conflict detection:     5ms
  - Firestore write:        5ms
  - VC merge:               1ms
  ─────────────────────────
  TOTAL (P50):             74ms
  TOTAL (P99):            150ms
  TOTAL (P99.9):          300ms
```

---

## 3. Read Path: Local vs Global Consistency

### 3.1 Local Read (Fast Path)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│ Read Path: Local Read (Same Region as Client)                              │
└─────────────────────────────────────────────────────────────────────────────┘

Client (EU) → GET /policies/policy-1

T0    Ingress              Route to EU region
      │
      └─→ Load balancer sees client in EU
          Route to EU data center

T0.1  Read Service         Query Firestore (EU region-local)
      │
      ├─→ Query: SELECT * FROM policies WHERE id = 'policy-1'
      │
      └─→ Result: {
          "id": "policy-1",
          "goals": ["improve-health", "reduce-cost"],
          "vector_clock": [1, 0, 0],
          "version": 2,
          "updated": 1711270245012
        }

T0.2  Response Builder     Serialize response
      │
      └─→ HTTP 200 OK + JSON

────────────────────────────────────────────────────────────────────────────

Total Latency: ~30ms (P50)
  - Network (client → EU): 10ms
  - Firestore read:        15ms
  - Response build:         2ms
  - Network (EU → client): 10ms (included above)
  ────────────────────────
  TOTAL:                   ~35ms

Consistency: STRONG (linearizable)
  - Client sees latest committed version
  - No stale reads possible (reading from local store)

Trade-off: May lag behind US-East by <500ms if update
           in US-East hasn't replicated yet
```

### 3.2 Global Read (Causal Consistency)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│ Read Path: Global Read (Wait for Causal Consistency)                        │
└─────────────────────────────────────────────────────────────────────────────┘

Client (anywhere) → GET /policies/policy-1?consistency=causal
                    &after_vc=[1,0,0]

T0    Ingress              Receive global read request
      │
      └─→ Query param: consistency=causal
          Query param: after_vc=[1,0,0]
                    (client has seen events up to this VC)

T0.1  Read Service         Check local VC
      │
      ├─→ Local VC: [0, 0, 0] (if EU lags behind)
      │
      └─→ Decision: Must wait for replication
          Because: after_vc=[1,0,0] requires us_east >= 1

T0.2  Causal Consistency   Wait for replication
      Monitor
      │
      ├─→ Poll replication status every 10ms
      │   "Has evt-12345 from us-east arrived?"
      │
      ├─→ T10: No
      ├─→ T20: No
      ├─→ T50: No
      ├─→ T75: YES! evt-12345 applied
      │
      └─→ Local VC now: [1, 0, 0] (matches requirement)

T0.3  Read Service         Now safe to serve
      │
      ├─→ Query: SELECT * FROM policies WHERE id = 'policy-1'
      │
      └─→ Result: {
          "id": "policy-1",
          "goals": ["improve-health", "reduce-cost"],
          "vector_clock": [1, 0, 0],
          "version": 2
        }

T0.4  Response Builder     Return with VC confirmation
      │
      └─→ HTTP 200 OK + JSON
          Header: X-Vector-Clock: [1,0,0]

────────────────────────────────────────────────────────────────────────────

Total Latency: ~100ms (P50)
  - Causal consistency wait: 50ms (P50)
                             300ms (P99)
  - Firestore read:          15ms
  - Response build:           2ms
  ────────────────────────
  TOTAL (P50):              ~67ms
  TOTAL (P99):             ~317ms

Consistency: CAUSAL (guaranteed ordering)
  - If client has seen VC=[1,0,0], all replicas with VC >= [1,0,0]
    will see same data
  - Prevents "write skew" and "read skew" anomalies
  - Weaker than linearizability, stronger than eventual
```

---

## 4. Replication Event Processing

### 4.1 Flowchart: Event Processing Pipeline

```
┌─────────────────────────────────────┐
│ Event arrives from Pub/Sub           │
│ (Multi-Region-Replication-Events)    │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│ [STEP 1] VALIDATION                 │
│ ✓ Parse JSON                        │
│ ✓ Verify checksum                   │
│ ✓ Verify signature                  │
└──────────┬──────────────────────────┘
           │
           ├─ FAIL → Log error, dead-letter queue
           │         Metrics: event_validation_failed
           │
           ▼ PASS
┌─────────────────────────────────────┐
│ [STEP 2] DEDUPLICATION              │
│ Query: event_id in dedup_table?     │
└──────────┬──────────────────────────┘
           │
           ├─ YES → Skip to STEP 9 (already applied)
           │
           ▼ NO
┌─────────────────────────────────────┐
│ [STEP 3] VC ORDER CHECK             │
│ Compare incoming VC vs local VC     │
│ Are all dependencies satisfied?     │
└──────────┬──────────────────────────┘
           │
           ├─ NO → Defer to pending queue
           │       Retry after 100ms
           │       Metrics: event_deferred_for_vc
           │
           ▼ YES
┌─────────────────────────────────────┐
│ [STEP 4] READ CURRENT VALUE         │
│ Query Firestore for entity_id       │
│ Get: local_version, local_vc        │
└──────────┬──────────────────────────┘
           │
           ▼
┌─────────────────────────────────────┐
│ [STEP 5] CONFLICT DETECTION         │
│ Compare local vs incoming           │
│ Are they same entity?               │
│ Are they different versions?        │
│ Do VCs conflict (concurrent)?       │
└──────────┬──────────────────────────┘
           │
           ├─ CONFLICT → Jump to STEP 6
           │
           ▼ NO CONFLICT
┌─────────────────────────────────────┐
│ [STEP 7] APPLY UPDATE               │
│ Write incoming version to Firestore │
└──────────┬──────────────────────────┘
           │
           ▼
┌─────────────────────────────────────┐
│ [STEP 6] CONFLICT RESOLUTION        │
│ Invoke conflict_resolver            │
│ - LWW: Latest VC wins               │
│ - CRDT: Merge                       │
│ - App-Level: Custom logic           │
│                                     │
│ Result: (winner, strategy)          │
└──────────┬──────────────────────────┘
           │
           ▼
│           Apply winner to Firestore │
│           Log conflict record       │
│
└──────────┬──────────────────────────┘
           │
           ▼
┌─────────────────────────────────────┐
│ [STEP 8] VC MERGE                   │
│ local_vc = MAX(local_vc, incoming)  │
└──────────┬──────────────────────────┘
           │
           ▼
┌─────────────────────────────────────┐
│ [STEP 9] MARK AS APPLIED            │
│ dedup_table[event_id] = now          │
│ Emit observability event            │
└──────────┬──────────────────────────┘
           │
           ▼
┌─────────────────────────────────────┐
│ [COMPLETE]                          │
│ Metrics: event_applied_successfully │
└─────────────────────────────────────┘
```

---

## 5. Conflict Detection & Resolution Flow

### 5.1 Conflict Scenario: Concurrent Updates

```
Timeline: US-East & US-West write same entity concurrently during partition

T0: Both regions have:
    policy-1 = { goals: ["improve-health"], version: 1, vc: [0, 0, 0] }

T1: PARTITION BEGINS

T1.1 (US-East): User updates policy-1
     → goals: ["improve-health", "reduce-cost"]
     → version: 2
     → vc: [1, 0, 0]

T1.2 (US-West): Autonomic system updates policy-1
     → goals: ["improve-health", "improve-safety"]
     → version: 2 (same version!)
     → vc: [0, 1, 0]

T2: PARTITION HEALS

T2.1 (US-East): Replication event arrives for policy-1 from US-West
     Message:
     {
       "event_id": "evt-west-001",
       "entity_id": "policy-1",
       "operation": "update",
       "payload": { goals: ["improve-health", "improve-safety"], version: 2 },
       "vector_clock": [0, 1, 0]
     }

┌─────────────────────────────────────────────────────────────────────────┐
│ CONFLICT DETECTION FLOW                                                 │
└─────────────────────────────────────────────────────────────────────────┘

Processing in US-East region:

T2.2 Current state (US-East):
     policy-1 = { goals: ["improve-health", "reduce-cost"], version: 2,
                  vc: [1, 0, 0] }

T2.3 Incoming state (from US-West):
     policy-1 = { goals: ["improve-health", "improve-safety"], version: 2,
                  vc: [0, 1, 0] }

T2.4 Conflict Check:
     ├─ Same entity? id="policy-1" ✓
     ├─ Different versions? v2 (reduce-cost) ≠ v2 (improve-safety) ✓
     ├─ Are VCs ordered?
     │  Local VC:    [1, 0, 0]
     │  Remote VC:   [0, 1, 0]
     │  Does local dominate? 1>=0 ✓, 0>=1 ✗ → NO
     │  Does remote dominate? 0>=1 ✗ → NO
     │  RESULT: Concurrent (neither dominates) → CONFLICT ✓
     │
     └─ ACTION: Invoke conflict resolver

T2.5 Conflict Resolution (LWW Strategy):
     ├─ Compare VCs: [1,0,0] vs [0,1,0]
     ├─ Neither dominates (concurrent)
     ├─ Use tiebreaker: Region ID
     │   Lexicographic: "eu" < "us-east" < "us-west"
     │   Local: us-east, Remote: us-west
     │   Winner: us-east (lexicographically smaller)
     │
     └─ WINNER: Local version
        { goals: ["improve-health", "reduce-cost"], version: 2, vc: [1,0,0] }
        LOSER: Remote version
        { goals: ["improve-health", "improve-safety"], version: 2, vc: [0,1,0] }

T2.6 Conflict Record:
     {
       "conflict_id": "conflict-policy-1-20260324T143045Z",
       "entity_id": "policy-1",
       "entity_type": "policy",
       "winning_version": "2-us-east",
       "losing_version": "2-us-west",
       "resolution_strategy": "last_write_wins_by_region",
       "winner_region": "us-east",
       "timestamp": 1711270245000,
       "vector_clock": [1, 0, 0],
       "receipt_hash": "sha256(...)"
     }

T2.7 Emit to Evidence Ledger:
     Store conflict record in Firestore (conflicts collection)
     Email operator: "Conflict resolved in policy-1"
     Metrics: conflict_resolved_lww

T2.8 Apply Winner:
     Firestore update: policy-1 = winner
     Log: "Conflict on policy-1 resolved, keeping reduce-cost goals"

────────────────────────────────────────────────────────────────────────

RESULT:
  - US-East policy-1 ← ["improve-health", "reduce-cost"]
  - US-West receives this via replication
  - EU receives this via replication
  - USER IMPACT: "improve-safety" goal was lost
  - AUDIT: Conflict record preserved for compliance review
```

---

## 6. Evidence Ledger Replication

### 6.1 Receipt Generation & Propagation

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Receipt Ledger: Immutable Audit Trail Across Regions                   │
└─────────────────────────────────────────────────────────────────────────┘

T0: Policy update written in US-East
    → Triggers receipt generation

T0.1 Receipt Emission (US-East Evidence Sidecar):

    CREATE receipt:
    {
      "id": "receipt-2026-03-24-14-30-45-001",
      "action": "policy-update",
      "entity_id": "policy-1",
      "vector_clock": [1, 0, 0],
      "timestamp": "2026-03-24T14:30:45.012Z",
      "region": "us-east",
      "operation_details": {
        "from": ["improve-health"],
        "to": ["improve-health", "reduce-cost"],
        "decision": "autonomic",
        "rationale": "User requested additional goal"
      },
      "actor": {
        "id": "user-123",
        "type": "human"
      },
      "prior_receipt_id": "receipt-2026-03-24-14-30-44-999",
      "prior_receipt_hash": "sha256:abc123...",
      "checksum": "sha256:payload",
      "signature": "ed25519:def456..."
    }

T0.2 Persist to Local Evidence (US-East):
    Collection: receipts
    Document ID: receipt-2026-03-24-14-30-45-001
    Durability: Firestore (durable)
    Also: Cloud Logging (tamper-proof mirror)

T0.3 Emit to Evidence Replication Topic:
    Topic: evidence-ledger-replication
    Message:
    {
      "receipt_id": "receipt-2026-03-24-14-30-45-001",
      "receipt_blob": { /* full receipt */ },
      "prior_receipt_hash": "sha256:abc123...",
      "checksum": "sha256:xyz789..."
    }

T1: US-West Receives Receipt:

T1.1 Validation:
     ✓ Verify signature (ed25519)
     ✓ Verify prior_receipt_hash matches chain
     ✓ Verify checksum

T1.2 Persist to Local Evidence (US-West):
     Collection: receipts
     Document ID: receipt-2026-03-24-14-30-45-001
     Durability: Firestore (durable) + Cloud Logging

T1.3 Update Hash Chain:
     receipts[current] → prior_hash = receipts[previous]
     receipts[current] → hash = sha256(current_receipt)

T2: EU Receives Receipt:

T2.1 Validation:
     ✓ Verify signature
     ✓ Verify prior_receipt_hash
     ✓ Verify checksum

T2.2 Persist to Local Evidence (EU):
     Collection: receipts
     Document ID: receipt-2026-03-24-14-30-45-001
     Durability: Firestore (durable) + Cloud Logging

────────────────────────────────────────────────────────────────────────

HASH CHAIN INTEGRITY:

receipt-N:     [sig: xyz] [hash_prior: ABC] [data: xyz]
               └─ hash_this: DEF

receipt-N+1:   [sig: abc] [hash_prior: DEF] [data: abc]
               └─ hash_this: GHI

receipt-N+2:   [sig: def] [hash_prior: GHI] [data: def]
               └─ hash_this: JKL

VERIFICATION ALGORITHM:

1. Start with receipt-N
2. Compute hash(receipt-N) → should be ABC (stored in receipt-N+1)
3. Get receipt-N+1, verify hash_prior = ABC ✓
4. Compute hash(receipt-N+1) → should be GHI (stored in receipt-N+2)
5. Get receipt-N+2, verify hash_prior = GHI ✓
... continue until latest receipt

TAMPER DETECTION:

If attacker modifies receipt-N:
  → hash(receipt-N) ≠ ABC (stored in receipt-N+1)
  → Hash chain breaks
  → Alert: "Ledger tampered"

────────────────────────────────────────────────────────────────────────

ARCHIVAL (Daily):

All receipts from 2026-03-24:
  → Batch into JSON-L file
  → Compute file hash
  → Sign with master key
  → Store in GCS bucket: evidence-archive-us-east/2026/03/24.jsonl.gz
  → Retention: 7 years (regulatory requirement)

SPARQL QUERY EXAMPLE:

SELECT ?receipt ?entity ?action ?timestamp
WHERE {
  ?receipt rdf:type osii:Receipt .
  ?receipt osii:entity_id ?entity .
  ?receipt osii:action ?action .
  ?receipt osii:timestamp ?timestamp .
  FILTER(?entity = "policy-1")
  FILTER(?timestamp >= "2026-03-24T00:00:00Z")
  FILTER(?timestamp <= "2026-03-25T00:00:00Z")
}
ORDER BY ?timestamp

RESULT: All actions taken on policy-1 during 2026-03-24
        Timestamped, signed, hash-chained
        → Audit-ready
```

---

## 7. Region Recovery Flow

### 7.1 Recovery State Machine

```
┌──────────────────────────────────────────────────────────────────────┐
│ Region Recovery: US-West rejoins after 5-minute outage              │
└──────────────────────────────────────────────────────────────────────┘

T0: US-West becomes OFFLINE
    Health check failures detected
    State: FAILED
    VC snapshot: [0, 4, 0]
    Last heartbeat: T0

T1-T5: US-West OFFLINE (queuing replication events)
       Replication events for policy changes accumulate
       Queue size: growing
       Circuit breaker: OPEN (stop writes to failed region)

T6: US-West comes back online
    Health check succeeds
    Recovery service: INITIATED
    Recovery timeout: 5 minutes (300 seconds)

T6.1-T6.5 [STAGE 1: Vector Clock Sync] (30s)

    US-East sends:
      Local VC: [10, 9, 8]
      Last applied receipt: receipt-001
      Checkpoint: block #12345

    US-West replies:
      Local VC: [0, 4, 0]  (behind on all components)
      Last applied receipt: receipt-000
      Checkpoint: block #12340

    Recovery service determines:
      Recovery point: VC = [0, 4, 0] (min of both)
      Behind: 10 us-east updates, 0 us-west updates, 8 eu updates
      Need to replay: 18 events

T6.6-T8.5 [STAGE 2: State Snapshot] (60s)

    US-East creates full snapshot:
      - All Firestore documents as of VC=[0,4,0]
      - All policy documents
      - All receipt documents up to recovery point
      - Total size: ~50MB

    US-East sends snapshot to US-West:
      Protocol: gRPC streaming (resume-able)
      Checksum: SHA-256 of entire snapshot
      Chunks: 1MB each

    US-West receives:
      Validates checksum
      Persists locally
      Acks receipt

T8.6-T10.5 [STAGE 3: Event Replay] (60s)

    Recovery service queries event log:
      FROM: Recovery point VC=[0,4,0]
      TO:   Current VC=[10,9,8]
      ORDER BY: Vector clock

    Events to replay (18 total):
      evt-001: policy-2 update (us-east) VC=[1,4,0]
      evt-002: policy-3 update (us-east) VC=[2,4,0]
      evt-003: metric update (eu) VC=[2,4,1]
      ...
      evt-018: policy-10 update (us-east) VC=[10,9,0]

    US-West applies each event:
      ├─ Validate event
      ├─ Check idempotence
      ├─ Apply to Firestore
      ├─ Update VC
      ├─ Emit local log
      └─ ACK to recovery service

    Checksum validation per batch:
      After every 3 events: Compute local state hash
      Compare with expected hash
      If mismatch: Stop, alert operator

T10.6-T11.5 [STAGE 4: Verification] (30s)

    Final checks:

    ✓ Vector clock match:
      US-East: [10, 9, 8]
      US-West: [10, 9, 8]  ✓ MATCH

    ✓ Receipt chain integrity:
      US-East: receipt-018 → sha256(receipt-017) ✓
      US-West: receipt-018 → sha256(receipt-017) ✓

    ✓ Document count:
      US-East: 523 docs
      US-West: 523 docs ✓ MATCH

    ✓ Checksum:
      US-East state hash: def456...
      US-West state hash: def456... ✓ MATCH

    Result: ALL VERIFICATION PASSED ✓

T11.6-T11.9 [STAGE 5: Handoff] (10s)

    ✓ Mark region as HEALTHY
    ✓ Close circuit breaker (resume accepting writes)
    ✓ Emit recovery receipt:
      {
        "id": "recovery-receipt-us-west-20260324T145xxx",
        "region": "us-west",
        "recovery_duration_seconds": 305,
        "events_replayed": 18,
        "from_vc": [0, 4, 0],
        "to_vc": [10, 9, 8],
        "status": "success",
        "signature": "ed25519:xyz"
      }
    ✓ Alert operator: "US-West recovered, 18 events replayed"

T11.10 RECOVERY COMPLETE
       State: HEALTHY
       VC: [10, 9, 8]
       Replication: RESUMED
       Write acceptance: ENABLED

────────────────────────────────────────────────────────────────────────

ABORT SCENARIOS:

If Stage 2 verification fails:
  ✗ Checksum mismatch
  ✗ Document count ≠
  ✗ Vector clock diverges
  → RECOVERY ABORTED
  → Alert operator
  → Region enters: REQUIRES_OPERATOR state
  → Manual investigation needed
```

---

## 8. Failure Scenarios

### 8.1 Network Partition Detection & Handling

```
Scenario: Network partition between US-East/West and EU

T0: Partition detected
    EU ↔ US-East: DISCONNECTED (no heartbeats for 10 seconds)
    EU ↔ US-West: DISCONNECTED

T0.1: Quorum analysis:
      Total regions: 3
      Connected:     2 (US-East + US-West)
      Isolated:      1 (EU)
      Majority:      2 ≥ 2 → MAJORITY PARTITION EXISTS

T0.2: Partition management:
      Majority partition (US-East + US-West):
        Action: CONTINUE (serve reads + writes)
        Reason: 2/3 regions can form consensus

      Minority partition (EU):
        Action: DOWNGRADE to READ_ONLY
        Reason: Cannot guarantee consistency
        Metrics: Writes rejected with error:
                 "Region in minority partition, writes rejected"

T0.3: Read consistency during partition:
      US-East write: policy-1 = "v1" VC=[5,0,0]
      US-West sync: policy-1 = "v1" VC=[5,1,0]
      EU local: policy-1 = "v0" VC=[4,0,0] (before partition)

      EU read attempt:
        Result: "v0" (local cached copy)
        Staleness: ~500ms
        Warning: "Data may be stale (not in majority partition)"

T1: Partition heals (network reconnected)
    EU ↔ US-East: CONNECTED ✓
    EU ↔ US-West: CONNECTED ✓

T1.1: Conflict detection:
      EU has: policy-1 = "v0" VC=[4,0,0]
      Majority has: policy-1 = "v1" VC=[5,1,0]

      Which is newer?
        EU VC:  [4, 0, 0]
        Main VC:[5, 1, 0]

        Does Main dominate? 5>=4 ✓, 1>=0 ✓, 0>=0 ✓ → YES

      Winner: Main partition version (v1)
      Action: EU replays majority's version
      Metric: conflict_resolved_partition_heal

T1.2: Status transitions:
      EU state: READ_ONLY → SYNCING → HEALTHY (3-4 minutes)

────────────────────────────────────────────────────────────────────────

Split-Brain Example (both sides have committed writes):

T0: Partition
T0.1 (US-East side):
     policy-1 = "v1-east"  VC=[5, 0, 0]
     policy-2 = "v2-east"  VC=[6, 0, 0]

T0.2 (EU side):
     policy-1 = "v1-eu"    VC=[5, 0, 1]
     policy-3 = "v3-eu"    VC=[5, 0, 2]

T1: Partition heals
    Conflict on policy-1:
      East: v1-east, VC=[5,0,0]
      EU:   v1-eu,   VC=[5,0,1]

      Compare: [5,0,0] vs [5,0,1]
      Neither dominates (concurrent)
      Tiebreaker: "eu" < "us-east" → EU wins

    Non-conflict on policy-2 & policy-3:
      East: policy-2 (unique to east)
      EU:   policy-3 (unique to eu)
      No conflict, both preserved

    Final state (all regions):
      policy-1 = "v1-eu"    (EU won tiebreaker)
      policy-2 = "v2-east"  (preserved)
      policy-3 = "v3-eu"    (preserved)

      Metric: split_brain_resolved, conflicts_total=1
```

---

## 9. Timing & Latency Breakdown

### 9.1 End-to-End Latency Profile

```
┌─────────────────────────────────────────────────────────────────────┐
│ Latency Breakdown by Operation                                     │
└─────────────────────────────────────────────────────────────────────┘

WRITE PATH (Client → US-East → Replicas):

  LOCAL WRITE (P50):
    ├─ Request routing:           3ms
    ├─ Ingress validation:        2ms
    ├─ Policy evaluation:         8ms
    ├─ Persistence:               5ms
    ├─ Vector clock update:       1ms
    ├─ Receipt emission:          3ms
    ├─ Pub/Sub emit:             10ms
    ├─ Response build:            1ms
    └─ Total:                    33ms (P50)
                                 75ms (P99)
                                150ms (P99.9)

REPLICATION LATENCY (Pub/Sub → Replica):
  (one-way, not including client RTT)

    ├─ Network propagation:      50ms (P50)
    ├─ Pub/Sub processing:        5ms
    ├─ Validation + dedup:        3ms
    ├─ VC order check:           10ms (or defer)
    ├─ Conflict detection:        5ms
    ├─ Firestore write:           5ms
    ├─ VC merge + logging:        3ms
    └─ Total:                    81ms (P50)
                                200ms (P99)

  Total client-perceived latency (write acked when replica received):
    33ms (write in primary) + 81ms (replication) = 114ms (P50)

READ PATH (Local):

    ├─ Request routing:           5ms
    ├─ Firestore query:          15ms
    ├─ Response build:            2ms
    └─ Total:                    22ms (P50)
                                 50ms (P99)

READ PATH (Causal Consistency Wait):

    ├─ VC wait (if behind):      50ms (P50)
                                300ms (P99)
    ├─ Firestore query:          15ms
    ├─ Response build:            2ms
    └─ Total:                    67ms (P50)
                                317ms (P99)

RECOVERY (Region Rejoin):

    ├─ Stage 1 (VC sync):        30s
    ├─ Stage 2 (snapshot):       60s
    ├─ Stage 3 (replay):         60s
    ├─ Stage 4 (verify):         30s
    ├─ Stage 5 (handoff):        10s
    └─ Total:                   300s (5 minutes, typical)
                                450s (worst case, with retries)

SLO TARGETS:

  ✓ Write latency (P50):         <50ms
  ✓ Write latency (P99):         <150ms
  ✓ Read latency (P50):          <50ms
  ✓ Replication lag (P99):       <500ms
  ✓ Recovery time (99th %ile):   <5 minutes
  ✓ Quorum write durability:     2/3 regions received
```

---

## Summary

This document specifies the complete data flow for:

1. **Write Path**: How writes flow from client → primary → replicas
2. **Read Path**: Local strong consistency vs global causal consistency
3. **Replication**: Event processing with VC ordering + conflict detection
4. **Conflict Resolution**: LWW + CRDT + application merging strategies
5. **Evidence Ledger**: Immutable receipt replication across regions
6. **Recovery**: 5-stage deterministic region rejoin
7. **Failures**: Network partitions + split-brain resolution

**Key Invariants**:
- Vector clock causal ordering maintained across all regions
- Conflict resolution is deterministic (same outcome everywhere)
- Evidence ledger is immutable + hash-chained
- Recovery is deterministic + verifiable
- Read consistency options trade latency for guarantees

---

**Document Version**: 1.0 | **Last Updated**: 2026-03-24 | **Status**: Ready for Implementation
