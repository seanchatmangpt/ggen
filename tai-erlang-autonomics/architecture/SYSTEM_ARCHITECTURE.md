# Enterprise Billing System Architecture
## Zero Billing Disputes at 1,000+ Customer Scale

**Last Updated**: 2026-01-25
**Target Scale**: 1,000+ customers | 10,000+ RPS | Zero billing disputes
**Quality SLA**: 99.99% uptime | <100ms billing latency | 1 in 1M error rate
**Enterprise Standards**: SOC2 Type II | GDPR/CCPA compliant | Multi-region failover

---

## Executive Summary

This document describes a production-grade billing system designed for enterprise scale with **zero tolerance for billing disputes**. The architecture employs:

- **Erlang/OTP** for fault-tolerant distributed computing
- **Event-sourcing ledger** for immutable billing records
- **Cryptographic receipts** for non-repudiation
- **Multi-region replication** for disaster recovery
- **Bounded concurrency** to prevent resource exhaustion
- **Deterministic state machines** for consistency guarantees

**Key Innovation**: Receipt-driven architecture where every billing event generates a cryptographic proof, enabling complete audit trails and dispute resolution without human intervention.

---

## Part 1: System Components Architecture

### 1.1 High-Level System Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           CUSTOMER LAYER                                     │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐                   │
│  │SaaS App  │  │Mobile    │  │API Client│  │Billing   │                   │
│  │  v1      │  │  App     │  │ (gRPC)   │  │Dashboard │                   │
│  └─────┬────┘  └─────┬────┘  └─────┬────┘  └─────┬────┘                   │
└────────┼─────────────┼─────────────┼─────────────┼────────────────────────┘
         │             │             │             │
    ┌────▼─────────────▼─────────────▼─────────────▼────┐
    │   INGESTION LAYER (Global Load Balancer)          │
    │   ├─ TLS Termination                               │
    │   ├─ Request Validation                            │
    │   ├─ Rate Limiting (global quota)                  │
    │   └─ Request Routing (by tenant affinity)          │
    └────┬──────────────────────────────────────────────┘
         │
    ┌────▼──────────────────────────────────────────────────────────────┐
    │   COMPUTE LAYER (Erlang/OTP Runtime - Auto-scaling Cloud Run)     │
    │   ┌────────────────────────────────────────────────────────────┐  │
    │   │ Instance Pool (1-N, min=3, max=100, target CPU=60%)        │  │
    │   │  ┌──────────────────────────────────────────────────────┐  │  │
    │   │  │ HTTP Server (Cowboy)                                 │  │  │
    │   │  │ ├─ /health - Readiness probe                        │  │  │
    │   │  │ ├─ /events - Billing event ingestion                │  │  │
    │   │  │ ├─ /usage - Real-time usage reporting               │  │  │
    │   │  │ └─ /invoices - Invoice query API                    │  │  │
    │   │  └──────────────────────────────────────────────────────┘  │  │
    │   │  ┌──────────────────────────────────────────────────────┐  │  │
    │   │  │ Governance Layer (gen_statem)                         │  │  │
    │   │  │ ├─ Tenant Governor (per-tenant isolation)            │  │  │
    │   │  │ ├─ Billing Governor (quota management)               │  │  │
    │   │  │ ├─ Compliance Governor (regulatory checks)           │  │  │
    │   │  │ └─ Entitlement Governor (access control)             │  │  │
    │   │  └──────────────────────────────────────────────────────┘  │  │
    │   │  ┌──────────────────────────────────────────────────────┐  │  │
    │   │  │ Action Executor (poolboy bounded concurrency)        │  │  │
    │   │  │ ├─ Worker pool (size=CPU*2, configurable)            │  │  │
    │   │  │ ├─ Timeout enforcement (5s default)                  │  │  │
    │   │  │ └─ Graceful degradation                              │  │  │
    │   │  └──────────────────────────────────────────────────────┘  │  │
    │   │  ┌──────────────────────────────────────────────────────┐  │  │
    │   │  │ Receipt Generator (sha256 hash chain)                │  │  │
    │   │  │ ├─ Cryptographic proof for each event               │  │  │
    │   │  │ ├─ Non-repudiation signatures (RS256)                │  │  │
    │   │  │ └─ Audit trail linkage                               │  │  │
    │   │  └──────────────────────────────────────────────────────┘  │  │
    │   │  ┌──────────────────────────────────────────────────────┐  │  │
    │   │  │ Observability (OTEL + Prometheus)                    │  │  │
    │   │  │ ├─ Distributed tracing (trace_id correlation)        │  │  │
    │   │  │ ├─ Metrics aggregation (per-tenant, per-entitlement)│  │  │
    │   │  │ └─ Structured logging (JSON, trace_id indexed)       │  │  │
    │   │  └──────────────────────────────────────────────────────┘  │  │
    │   └────────────────────────────────────────────────────────┘  │
    └────┬──────────────────────────────────────────────────────────┘
         │
    ┌────▼──────────────────────────────────────────────────────────────┐
    │   PERSISTENCE LAYER (Multi-Region)                               │
    │   ┌────────────────────────────────────────────────────────────┐ │
    │   │ PRIMARY REGION (us-central1)                               │ │
    │   │ ├─ Firestore (Billing Ledger)                              │ │
    │   │ │  └─ Collections: events, receipts, invoices, disputes    │ │
    │   │ ├─ Cloud Pub/Sub (Event Stream - ordered per customer)     │ │
    │   │ ├─ Cloud Spanner (Billing State - CA consistency)          │ │
    │   │ └─ Cloud Storage (Receipt Archive - immutable ledger)       │ │
    │   └────────────────────────────────────────────────────────────┘ │
    │   ┌────────────────────────────────────────────────────────────┐ │
    │   │ SECONDARY REGION (us-east1)                                │ │
    │   │ ├─ Firestore Read Replica (async replication, <5min lag)   │ │
    │   │ ├─ Cloud Spanner Replica (active-passive, 2s RTO)          │ │
    │   │ └─ Billing Stream Processing (failover support)            │ │
    │   └────────────────────────────────────────────────────────────┘ │
    │   ┌────────────────────────────────────────────────────────────┐ │
    │   │ TERTIARY REGION (eu-west1) [Optional: GDPR data residency]│ │
    │   │ ├─ Firestore Copy (EU data residency compliance)           │ │
    │   │ └─ Customer data only (billing computed in primary)        │ │
    │   └────────────────────────────────────────────────────────────┘ │
    └────┬──────────────────────────────────────────────────────────────┘
         │
    ┌────▼──────────────────────────────────────────────────────────────┐
    │   ANALYTICS & REPORTING LAYER                                     │
    │   ├─ BigQuery (Billing data warehouse)                            │
    │   │  └─ Scheduled ETL from Firestore (hourly incremental)        │
    │   ├─ Data Studio (Executive dashboards)                           │
    │   ├─ Revenue Analytics Engine (forecasting, churn prediction)    │
    │   └─ Dispute Resolution System (ML-assisted)                      │
    └────┬──────────────────────────────────────────────────────────────┘
         │
    ┌────▼──────────────────────────────────────────────────────────────┐
    │   CUSTOMER-FACING LAYER                                           │
    │   ├─ Billing Dashboard (React.js - read-only queries)            │
    │   ├─ Invoice System (PDF generation, email delivery)              │
    │   ├─ Payment Processing (Stripe/Adyen integration)                │
    │   └─ Dispute Management UI (ticket system + evidence viewer)      │
    └────────────────────────────────────────────────────────────────────┘
```

### 1.2 Component Inventory

| Component | Technology | Purpose | Scaling | Criticality |
|-----------|-----------|---------|---------|-------------|
| **Load Balancer** | GCP Cloud LB | Global request routing, TLS termination | Auto | Critical |
| **Compute** | Erlang/OTP on Cloud Run | Billing logic execution | 1-100 instances | Critical |
| **State Store** | Firestore | Event ledger, customer data | Auto-scale | Critical |
| **Event Stream** | Cloud Pub/Sub | Ordered event processing | Auto-scale | Critical |
| **Receipt Ledger** | Cloud Storage + Firestore | Immutable audit trail | Unlimited | Critical |
| **Consistency DB** | Cloud Spanner | Strong consistency for billing state | Manual scale | Critical |
| **Analytics** | BigQuery | Billing data warehouse | Auto-scale | High |
| **Cache** | Cloud Memorystore | Customer profile cache | Manual scale | Medium |
| **Secret Manager** | GCP Secret Manager | Credentials, keys | Manual | Critical |
| **Monitoring** | Cloud Monitoring + Prometheus | Observability | Auto | High |
| **Tracing** | Cloud Trace + OpenTelemetry | Distributed tracing | Auto | Medium |
| **Logging** | Cloud Logging | Structured logs, audit logs | Auto | High |

---

## Part 2: Data Flow Architecture

### 2.1 Billing Event Lifecycle (Happy Path)

```
STEP 1: EVENT INGESTION
┌─────────────────────┐
│ Customer system     │
│ (SaaS app usage)    │
└────────┬────────────┘
         │ HTTP POST /events
         │ {customer_id, entitlement_id, usage_amount, timestamp}
         ▼
┌─────────────────────────────────────────────────────────────┐
│ Global Load Balancer                                         │
│ ├─ Validate signature (JWT)                                 │
│ ├─ Rate limit (customer quota: 10K RPS / 100K per day)    │
│ ├─ Route to instance (tenant affinity hash)               │
│ └─ Add request trace_id                                     │
└────────┬────────────────────────────────────────────────────┘
         │
STEP 2: REQUEST HANDLER (Erlang HTTP Handler)
┌─────────────────────────────────────────────────────────────┐
│ tai_http_handler:handle_event/2                             │
│ ├─ Parse JSON payload                                       │
│ ├─ Verify customer exists (Redis cache lookup)             │
│ ├─ Check customer status (active/suspended/complying)      │
│ ├─ Validate entitlement_id belongs to customer             │
│ └─ Emit log: event.ingested {trace_id, customer_id}       │
└────────┬────────────────────────────────────────────────────┘
         │
STEP 3: GOVERNANCE LAYER (Bounded Concurrency)
┌─────────────────────────────────────────────────────────────┐
│ tai_governor:handle_event/3 (gen_statem)                    │
│ ├─ Lookup tenant governor (gproc registry)                 │
│ ├─ Check current quota usage                                │
│ ├─ Acquire worker from poolboy (max wait: 5s)             │
│ ├─ Calculate new usage: used + usage_amount                │
│ ├─ Verify: used <= quota (billing hard limit)             │
│ └─ State transition:                                        │
│    active → active (if used < quota)                       │
│    active → suspended (if used >= quota)                   │
└────────┬────────────────────────────────────────────────────┘
         │
STEP 4: ACTION EXECUTION (2 transactions)
┌─────────────────────────────────────────────────────────────┐
│ Transaction 1: Write to Ledger (Firestore)                  │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ Document: /billing-ledger/{timestamp}-{receipt_id}     │ │
│ │ {                                                        │ │
│ │   receipt_id: "r_2026_01_25_abc123xyz",                │ │
│ │   type: "event_recorded",                              │ │
│ │   customer_id: "cust_xyz",                             │ │
│ │   entitlement_id: "ent_abc",                           │ │
│ │   usage_amount: 150.00,                                │ │
│ │   previous_balance: 500.00,                            │ │
│ │   new_balance: 350.00,                                 │ │
│ │   currency: "USD",                                     │ │
│ │   timestamp: 1704067200000,                            │ │
│ │   previous_receipt_hash: "sha256_prev_hash",          │ │
│ │   status: "accepted",                                  │ │
│ │   region: "us-central1",                              │ │
│ │   trace_id: "4bf92f3577b34da6a3ce929d0e0e4736"       │ │
│ │ }                                                       │ │
│ │ [Firestore server timestamp ensures ordering]         │ │
│ └─────────────────────────────────────────────────────────┘ │
│                                                              │
│ Transaction 2: Update Customer State (Cloud Spanner)       │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ UPDATE customers SET                                    │ │
│ │   balance = 350.00,                                    │ │
│ │   last_update = now(),                                 │ │
│ │   event_count = event_count + 1,                       │ │
│ │   last_receipt_id = "r_2026_01_25_abc123xyz"         │ │
│ │ WHERE customer_id = "cust_xyz"                        │ │
│ │   AND balance = 500.00  -- Optimistic concurrency lock│ │
│ │ [CA consistency ensures atomic update]                │ │
│ └─────────────────────────────────────────────────────────┘ │
└────────┬────────────────────────────────────────────────────┘
         │
STEP 5: RECEIPT GENERATION (Cryptographic Proof)
┌─────────────────────────────────────────────────────────────┐
│ tai_receipts:emit_receipt/2                                 │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ receipt_hash = sha256({                                │ │
│ │   previous_hash: "sha256_prev_hash",                  │ │
│ │   customer_id: "cust_xyz",                            │ │
│ │   entitlement_id: "ent_abc",                          │ │
│ │   amount: 150.00,                                     │ │
│ │   timestamp: 1704067200000,                           │ │
│ │   action: "debit"                                     │ │
│ │ })                                                     │ │
│ │                                                        │ │
│ │ signature = rs256_sign({                              │ │
│ │   receipt_hash: result_of_above,                      │ │
│ │   issuer: "billing-system-prod",                      │ │
│ │   aud: "cust_xyz",                                    │ │
│ │   iat: now()                                          │ │
│ │ })                                                     │ │
│ │                                                        │ │
│ │ Store in Cloud Storage:                               │ │
│ │   gs://billing-receipts/2026/01/25/{receipt_id}.json │ │
│ │   (immutable, versioning disabled)                    │ │
│ └─────────────────────────────────────────────────────────┘ │
└────────┬────────────────────────────────────────────────────┘
         │
STEP 6: RESPONSE & ACKNOWLEDGMENT
┌─────────────────────────────────────────────────────────────┐
│ HTTP 202 Accepted (async confirmation)                      │
│ {                                                            │
│   receipt_id: "r_2026_01_25_abc123xyz",                     │
│   status: "processed",                                       │
│   balance_after: 350.00,                                     │
│   quota_remaining: 100.00,                                   │
│   next_billing_date: "2026-02-25",                          │
│   receipt_url: "https://receipts.billing.com/r_abc123xyz"  │
│ }                                                            │
│                                                              │
│ Emit metrics:                                                │
│ ├─ billing_events_processed_total (+1)                      │
│ ├─ billing_amount_total (+150.00 USD)                       │
│ ├─ event_processing_duration_ms (45 ms)                     │
│ └─ customer_balance_current_usd (350.00)                    │
└────────────────────────────────────────────────────────────┘

STEP 7: PUBLISH EVENT (Eventual Consistency)
┌─────────────────────────────────────────────────────────────┐
│ Publish to Pub/Sub (with deduplication ID for idempotency) │
│ Topic: projects/{project}/topics/billing-events             │
│ Message: {                                                   │
│   data: base64({...receipt_data...}),                       │
│   attributes: {                                              │
│     customer_id: "cust_xyz",                                │
│     receipt_id: "r_2026_01_25_abc123xyz",                   │
│     event_type: "debit",                                    │
│     deduplication_id: "r_2026_01_25_abc123xyz"  -- prevent │
│   }                                                          │
│ }                                                            │
│                                                              │
│ Subscribers:                                                │
│ ├─ Billing Dashboard (read new balance)                     │
│ ├─ Invoice Generator (accumulate for monthly invoice)       │
│ ├─ Compliance Logger (regulatory audit trail)               │
│ ├─ Customer Notification (email receipt)                    │
│ └─ BigQuery ETL (analytics warehouse)                       │
└────────────────────────────────────────────────────────────┘

STEP 8: EVENTUAL CONSISTENCY VERIFICATION
┌─────────────────────────────────────────────────────────────┐
│ Every 5 minutes: Reconciliation Job                          │
│ ├─ Read all receipts from Firestore (order by timestamp)    │
│ ├─ Replay ledger: start_balance → apply all debits/credits │
│ ├─ Compare computed_balance with Cloud Spanner state        │
│ ├─ Alert if mismatch detected (investigate & rollback)      │
│ └─ Emit metric: ledger_reconciliation_status                │
└────────────────────────────────────────────────────────────┘

TOTAL LATENCY: <100ms (p99)
- Request → HTTP handler: 5ms
- Validation: 10ms
- Governor state check: 5ms
- Firestore write: 30ms
- Spanner write: 40ms
- Receipt generation: 5ms
- Response: 5ms
```

### 2.2 Edge Cases & Failure Handling

**Case 1: Customer Balance Insufficient**
```
Event arrives: usage_amount=200, balance=150
├─ Governor checks: 150 + (-200) < 0 ❌
├─ Action: REJECT or QUEUE (depends on customer tier)
├─ Emit receipt: type="event_rejected", reason="insufficient_balance"
├─ Response: HTTP 402 Payment Required
└─ Customer notification: "Please top up balance before next usage"
```

**Case 2: Firestore Write Fails**
```
Scenario: Network error during ledger write
├─ Timeout after 5 seconds
├─ Emit receipt: type="event_pending", reason="ledger_unavailable"
├─ Buffer event in local queue (max 1000 events)
├─ Retry writes on exponential backoff
├─ Response: HTTP 202 Accepted (user sees success but uncommitted)
├─ Health check fails → trigger instance restart
└─ On recovery: retry all buffered events in original order
```

**Case 3: Spanner Write Conflict**
```
Scenario: Concurrent updates to same customer
├─ Transaction 1 reads balance=500
├─ Transaction 2 reads balance=500 (before T1 commits)
├─ T1 writes balance=350 (500 - 150) ✓
├─ T2 tries to write balance=350 (500 - 150) ❌ OCC violation
├─ Spanner aborts T2 with CONFLICT error
├─ T2 retried with exponential backoff (max 3 attempts)
├─ If all retries fail: reject event with "concurrent_update_conflict"
└─ Emit alert: "High contention on customer cust_xyz"
```

**Case 4: Pub/Sub Delivery Fails**
```
Scenario: Event accepted but Pub/Sub unavailable
├─ Firestore & Spanner writes succeeded (committed)
├─ Pub/Sub publish times out (5s)
├─ Response: HTTP 202 (billing recorded, notifications delayed)
├─ Retry Pub/Sub publish every 30s (queue in memory + async retry)
├─ Dashboard eventually consistent (<5min delay)
└─ Metrics alert: "Pub/Sub publish latency p99 > 1s"
```

---

## Part 3: Consistency Guarantees

### 3.1 Billing Consistency Model

```
GUARANTEE: Every billing event recorded exactly once
MECHANISM: Idempotent event processing with deduplication

┌─────────────────────────────────────────────────────────────┐
│ Deduplication Strategy                                      │
├─────────────────────────────────────────────────────────────┤
│ 1. Customer provides idempotent_key with request            │
│    POST /events                                              │
│    {                                                         │
│      idempotent_key: "uuid-generated-by-customer",         │
│      event_id: "evt_2026_01_25_abc",                       │
│      usage_amount: 150.00                                   │
│    }                                                         │
│                                                              │
│ 2. Server stores mapping:                                   │
│    idempotent_key → receipt_id (in Spanner)                │
│                                                              │
│ 3. On duplicate idempotent_key:                             │
│    ├─ Return cached receipt (idempotent)                    │
│    ├─ Don't apply charge twice                              │
│    └─ Response includes: "status": "duplicate"              │
│                                                              │
│ 4. Example: Network timeout scenario                        │
│    ├─ Customer sends event (idempotent_key=uuid_abc)       │
│    ├─ Server processes: balance 500 → 350                  │
│    ├─ Client never receives response (timeout)              │
│    ├─ Customer retries with SAME idempotent_key=uuid_abc   │
│    ├─ Server finds cached receipt                           │
│    ├─ Returns same receipt (balance already 350)            │
│    └─ No double charge ✓                                    │
│                                                              │
│ 5. Idempotent Key Storage (Spanner):                        │
│    CREATE TABLE idempotent_keys (                           │
│      customer_id STRING,                                    │
│      idempotent_key STRING,                                 │
│      receipt_id STRING,                                     │
│      created_at TIMESTAMP,                                  │
│      PRIMARY KEY (customer_id, idempotent_key)              │
│    );                                                        │
│    [Retention: 90 days, then deleted]                       │
└─────────────────────────────────────────────────────────────┘
```

### 3.2 Consistency Levels

| Level | Mechanism | Use Case | SLO |
|-------|-----------|----------|-----|
| **Strong** | Cloud Spanner (CA) | Customer balance, entitlements | Immediate |
| **Eventual** | Firestore + Pub/Sub | Ledger records, events | <5 min |
| **Read-Your-Writes** | Spanner read-after-write fence | Balance queries | 100% |
| **Causal** | Firestore document ordering | Chronological event replay | >99.9% |

### 3.3 Hash Chain Proof-of-Integrity

```
Receipt Ledger Verification Algorithm

Receipt N:
{
  "receipt_id": "r_2026_01_25_0001",
  "previous_hash": "0000000000000000000000000000000000000000000000000000000000000000" (genesis),
  "payload": {...billing data...},
  "computed_hash": sha256({previous_hash, payload}),
  "signature": RS256_sign(computed_hash)
}

Receipt N+1:
{
  "receipt_id": "r_2026_01_25_0002",
  "previous_hash": sha256_result_from_receipt_N,  ← Links to previous
  "payload": {...billing data...},
  "computed_hash": sha256({previous_hash, payload}),
  "signature": RS256_sign(computed_hash)
}

Verification Process:
├─ Load receipt R_i
├─ Hash(R_i.previous_hash, R_i.payload) == R_i.computed_hash? ✓
├─ RS256_verify(R_i.signature, R_i.computed_hash)? ✓
├─ R_{i+1}.previous_hash == R_i.computed_hash? ✓
├─ Continue for all N receipts
└─ If any check fails → LEDGER CORRUPTED (alert critical)

PROPERTY: Tampering any receipt invalidates all subsequent receipts
BENEFIT: Detects unauthorized modifications to billing history
```

---

## Part 4: Failure Modes & Recovery

### 4.1 Failure Mode Analysis

| Failure Mode | Probability | Impact | Detection | Recovery | RTO |
|--------------|-------------|--------|-----------|----------|-----|
| **Instance crash** | Medium | Service temporarily unavailable | Health check fails | Restart (auto) | <30s |
| **Firestore unavailable** | Low | Events buffered locally | Write timeout | Retry loop | <5min |
| **Cloud Spanner down** | Very Low | Strong consistency lost | Query timeout | Failover replica | <2min |
| **Pub/Sub delay** | Low | Analytics lag | Publish latency metric | Retry backoff | <5min |
| **Network partition** | Very Low | Requests timeout | Connection errors | Locality-based routing | <1min |
| **Billing data corruption** | Extremely Low | Dispute risk | Reconciliation job | Rollback to last good receipt | <30min |
| **Certificate expiry** | Very Low | TLS failure | Monitoring check | Manual rotation + automation | <24h |

### 4.2 Multi-Region Failover

```
PRIMARY REGION: us-central1 (Active)
├─ Cloud Run (compute)
├─ Firestore (primary writes)
├─ Cloud Spanner (primary reads)
└─ Pub/Sub (event stream)

SECONDARY REGION: us-east1 (Standby, <2min activation)
├─ Cloud Run (scaled down, 1 instance)
├─ Firestore read replica (async, ~5min lag behind primary)
├─ Cloud Spanner standby (active-passive, <5s failover)
└─ Pub/Sub read backup (catch-up on activation)

FAILURE SCENARIO: Primary region datacenter fire
├─ Primary region unavailable (all services down)
├─ Global LB health checks fail
├─ LB automatically routes to secondary region
├─ Secondary region receives full traffic
├─ If primary had uncommitted events → lost (max 30s worth)
├─ Firestore replication catches up (async)
└─ RTO: <2 minutes | RPO: ~30 seconds | Manual promotion required

RECOVERY STEPS:
1. Investigate root cause
2. Restore primary region
3. Sync Firestore: primary ← secondary (if primary corrupted)
4. Run reconciliation job (verify ledger integrity)
5. Manual promotion back to primary
6. Monitor for discrepancies
```

### 4.3 Data Loss Prevention

```
Scenario: How we prevent losing a day of billing data

ARCHITECTURE LAYERS:
┌─────────────────┐
│ Event ingested  │
├─────────────────┤
│ Local buffer    │ ← If Firestore down, buffer in-memory (1000 events)
├─────────────────┤
│ Firestore write │ ← Primary persistence
├─────────────────┤
│ Cloud Spanner   │ ← Strong consistency backup
├─────────────────┤
│ Cloud Storage   │ ← Immutable archive copy
├─────────────────┤
│ Pub/Sub         │ ← Eventual consistency copies
└─────────────────┘

GUARANTEES:
- If instance crashes: local buffer lost, but Firestore has data ✓
- If Firestore fails: Spanner has backup (eventual consistency) ✓
- If both fail: Cloud Storage + Pub/Sub have copies ✓
- If datacenter burns: secondary region has Firestore replica ✓

WORST-CASE DATA LOSS:
- In-memory events in failed instance: ~1000 events (~$50-500)
- Recovery: ask customer to resubmit (idempotent_key handles dupes)
```

---

## Part 5: Scalability Design

### 5.1 Horizontal Scaling Model

```
LOAD PROFILE ANALYSIS

Current State (Jan 2026):
├─ 10 customers paying
├─ Avg 100 events/sec
├─ Peak 500 events/sec
└─ Avg event value: $10

Target State (Jan 2027):
├─ 1,000 customers
├─ Avg 10,000 events/sec
├─ Peak 100,000 events/sec (Black Friday)
└─ Avg event value: varies ($0.01 - $1000)

COMPUTE SCALING (Cloud Run)

Minimum Instances (always running): 3
├─ Reason: HA across zones
├─ Cost: 3 × $0.00002472 per vCPU-sec ≈ $196/month idle

Maximum Instances: 100
├─ Hard limit: prevents runaway costs
├─ Soft limit: 75 (leaves 25% buffer)

Auto-scaling Triggers:
├─ CPU > 70% → scale up (add 5 instances)
├─ CPU < 30% for 5min → scale down (remove 1 instance)
├─ Request latency p99 > 100ms → scale up
├─ Request queue depth > 50 per instance → scale up

Scaling Math:
├─ Throughput per instance: 1000 RPS
├─ For 10,000 RPS: need 10 instances minimum
├─ Add 20% buffer: 12 instances nominal
├─ Peak capacity (100K RPS): 100 instances max

COST IMPACT:
├─ 12 instances × $0.00002472 vCPU-sec = $2,364/month
├─ 100 instances (peak) × $0.00002472 = $19,705/month
└─ Average: ~$5,000-8,000/month (with scaling)

DATABASE SCALING (Cloud Spanner)

Nodes (for strong consistency billing state):
├─ 1 node (100K ops/sec) ← startup
├─ 3 nodes (300K ops/sec, HA) ← 100 customers
├─ 10 nodes (1M ops/sec, HA) ← 1000 customers
└─ Auto-scaling: scale up if latency p99 > 50ms

Firestore (event ledger):
├─ Auto-scales on demand
├─ Estimate: 10,000 writes/sec = ~$6,000/month
└─ No pre-provisioning needed

Pub/Sub (event stream):
├─ Auto-scales on demand
├─ Estimate: 10,000 publishes/sec = ~$500/month
└─ No pre-provisioning needed

TOTAL INFRASTRUCTURE COST (1000 customers, 10K RPS):
├─ Cloud Run: $5,000/month
├─ Cloud Spanner: $8,000/month (3 nodes × $2,500 + storage)
├─ Firestore: $6,000/month
├─ Cloud Pub/Sub: $500/month
├─ Cloud Storage (receipts): $100/month
├─ Cloud Monitoring: $500/month
├─ Cloud Load Balancer: $1,000/month
├─ Data transfer: $1,000/month
└─ TOTAL: ~$22,100/month ≈ $265K/year

Cost per customer: $265 / 1000 = $0.265/customer/month
Cost per event: $265,000 / (10K RPS × 2.6B events/month) = $0.00001/event ✓ (profitable)
```

### 5.2 Connection Pool Sizing

```
Firestore Connection Pool:
├─ Min connections: 10
├─ Max connections: 100
├─ Connection timeout: 5s
├─ Idle connection TTL: 60s

Cloud Spanner Connection Pool:
├─ Min connections: 20 (for consistency)
├─ Max connections: 200
├─ Connection timeout: 5s
├─ Session pool size: 1000 (Spanner requirement)

Erlang Process Pool (poolboy):
├─ Worker pool size: min(CPU cores * 2, 32)
├─ On 16-vCPU Cloud Run: 32 workers
├─ Queue timeout: 5s (reject if worker unavailable)
├─ Overflow: queue additional requests (max queue size = 1000)

HTTP Keepalive:
├─ Client keepalive: 60s
├─ Server keepalive: 75s
├─ Max requests per connection: 1000
```

---

## Part 6: Disaster Recovery

### 6.1 RTO/RPO Targets

| Scenario | RTO | RPO | Strategy |
|----------|-----|-----|----------|
| **Single instance crash** | <30s | 0 min | Auto-restart |
| **Zone down** | <1min | 1 min | Cross-zone LB |
| **Region down** | <2min | 30s | Secondary region failover |
| **Firestore corruption** | <30min | 0 min | Rollback from backups |
| **Data theft/breach** | <24h | 24h | Restore from immutable archive |
| **Complete outage** | <4h | <1h | Full system recovery |

### 6.2 Backup Strategy

```
BACKUP MATRIX

Tier 1: Firestore Automated Backups
├─ Frequency: Hourly snapshots (automatic)
├─ Retention: 35 days (Google-managed)
├─ Recovery: Point-in-time restore available
├─ RTO: <30 minutes

Tier 2: Cloud Storage Archive
├─ Strategy: Copy receipts to GCS (immutable bucket)
├─ Frequency: Real-time (on every receipt emit)
├─ Retention: 7 years (regulatory requirement)
├─ Recovery: Restore individual files immediately
├─ Cost: $0.004/GB/month (archive storage)

Tier 3: Cloud Spanner Backup
├─ Frequency: Daily automated backups
├─ Retention: 7 days
├─ Recovery: On-demand restore to new instance
├─ RTO: <1 hour

Tier 4: BigQuery Snapshots
├─ Frequency: Weekly snapshots of billing data
├─ Retention: 1 year
├─ Recovery: Query historical snapshots
├─ Use: Analytics recovery, trend analysis

CROSS-REGION REPLICATION:
├─ Primary: us-central1 (hot)
├─ Replica 1: us-east1 (standby, <2 min failover)
├─ Replica 2: eu-west1 (cold, read-only, async)
├─ Replication lag: 30s - 5 min (acceptable for billing)
```

### 6.3 Incident Response Runbook

```
INCIDENT: Billing events rejected (HTTP 500 errors)

DETECTION (Automated):
├─ Alert triggered: error_rate > 1%
├─ Alert triggered: p99_latency > 500ms
├─ Alert triggered: instance restart count > 3 in 5min
└─ PagerDuty notification sent

IMMEDIATE INVESTIGATION (On-call SRE):
├─ Check Cloud Monitoring dashboard
│  └─ Look for: error distribution, affected instances, hot endpoints
├─ Check Cloud Logging (filter by trace_id of failed requests)
│  └─ Look for: Firestore errors, Spanner connection issues, timeout patterns
├─ Check instance CPU/memory
│  └─ Look for: resource exhaustion, goroutine leaks
└─ Check Firestore quota
   └─ Look for: write quota exceeded, database locked

RESPONSE ACTIONS (by root cause):

Case A: Firestore quota exceeded
├─ Action: Increase Firestore write capacity
├─ Timeline: 5 minutes
├─ Verify: Latency returns to normal
└─ Follow-up: Review traffic spike (DDoS? Marketing campaign?)

Case B: Cloud Spanner connection pool exhausted
├─ Action: Increase max connections to Spanner
├─ Timeline: 5 minutes
├─ Verify: Connection pool size < 80% utilization
└─ Follow-up: Analyze query performance (slow queries hogging connections)

Case C: Instance OOM (out of memory)
├─ Action: Restart instances
├─ Timeline: <30s (automatic failover)
├─ Verify: New instances healthy
└─ Follow-up: Memory profiling (leak? batch size too large?)

Case D: Network partition (single region)
├─ Action: Fail over to secondary region
├─ Timeline: <2 minutes (manual promotion)
├─ Verify: Events flowing into secondary region
└─ Follow-up: Investigate failed region, restore primary

POST-INCIDENT:
├─ Root cause analysis (24 hours)
├─ Timeline reconstruction (what happened when)
├─ Failure prevention (engineering fix)
├─ Monitoring improvement (catch earlier next time)
├─ Post-mortem document (share learnings)
└─ Stakeholder notification (customers, finance)
```

---

## Part 7: Multi-Tenancy & Isolation

### 7.1 Tenant Isolation Architecture

```
ISOLATION LEVELS

Network Isolation:
├─ Each tenant request tagged with tenant_id
├─ Load balancer routes by tenant_id hash (sticky)
├─ Instances can serve multiple tenants (co-locate for efficiency)
└─ No network cross-talk (all messages include tenant_id)

Process Isolation (Erlang):
├─ Per-tenant governor process (gen_statem)
├─ Per-tenant action queue (bounded concurrency)
├─ gproc registry namespaced by tenant
└─ Supervisor hierarchy isolates tenant processes

Data Isolation (Firestore):
├─ Collection path: /tenants/{tenant_id}/events/{event_id}
├─ Firestore security rules enforce access:
│  └─ request.auth.uid == resource.data.tenant_id
├─ No cross-tenant queries possible
└─ Separate document for each tenant partition

Quota Isolation:
├─ Per-tenant rate limit: configured in Cloud Spanner
├─ Per-entitlement quota: separate ledger per customer
├─ Shared Pub/Sub topic but tenant_id in attributes
└─ Metrics labeled by tenant_id

RESOURCE GUARANTEES:
├─ One tenant's failure (e.g., infinite loop) doesn't crash others
├─ One tenant's high load doesn't starve others
├─ One tenant's data corruption doesn't affect others
└─ Noisy neighbor problem mitigated via bounded concurrency
```

### 7.2 Firestore Security Rules

```
rules_version = '2';
service cloud.firestore {
  match /databases/{database}/documents {

    // Tenant data must be owned by authenticated customer
    match /tenants/{tenant_id}/events/{document=**} {
      allow read: if request.auth.uid == tenant_id
                  || request.auth.token.roles.contains("admin");
      allow write: if request.auth.uid == tenant_id
                   || request.auth.token.roles.contains("admin");
    }

    // Receipts can only be read by tenant or auditors
    match /tenants/{tenant_id}/receipts/{document=**} {
      allow read: if request.auth.uid == tenant_id
                  || request.auth.token.roles.contains("auditor");
      allow write: if request.resource == null
                   && resource == null; // Immutable (no writes)
    }

    // Deny all other access
    match /{document=**} {
      allow read, write: if false;
    }
  }
}
```

### 7.3 Customer Quota Management

```
TABLE: customer_quotas (Cloud Spanner)

CREATE TABLE customer_quotas (
  customer_id STRING NOT NULL,
  entitlement_id STRING NOT NULL,
  monthly_quota_usd NUMERIC(15, 2),
  usage_month DATE,
  current_usage_usd NUMERIC(15, 2),
  status ENUM('active', 'suspended', 'complying'),
  suspend_at_date DATE,  -- Auto-suspend if quota exceeded
  created_at TIMESTAMP,
  updated_at TIMESTAMP,

  PRIMARY KEY (customer_id, entitlement_id),

  CONSTRAINT quota_check CHECK (current_usage_usd <= monthly_quota_usd * 1.1)
    -- Allow 10% overage buffer for timing quirks
);

QUOTA ENFORCEMENT LOGIC:

On Event Ingestion:
├─ Check: current_usage + new_amount <= monthly_quota?
├─ If YES:
│  ├─ Update: current_usage += new_amount
│  └─ Accept event, emit receipt "accepted"
├─ If NO (over quota):
│  ├─ Check: customer tier allows overage?
│  ├─ If tier allows: accept with "warning", emit receipt "accepted_with_warning"
│  ├─ If tier disallows: reject, emit receipt "rejected_quota_exceeded"
│  └─ Set status = 'suspended' (no new events accepted)

Auto-Reset:
├─ Cron job runs daily (1am UTC)
├─ For each customer with usage_month < current_month:
│  ├─ Update: usage_month = current_month
│  ├─ Reset: current_usage_usd = 0
│  ├─ Update: status = 'active'
│  └─ Emit metric: "customer_quota_reset"

QUOTA TIERS (Configuration):

Tier 1 (Free):
├─ monthly_quota_usd: $0 (no billing)
├─ overage_allowed: false
├─ support_level: "community"

Tier 2 (Startup):
├─ monthly_quota_usd: $1,000
├─ overage_allowed: true (10% buffer)
├─ suspend_on_exceed: false

Tier 3 (Growth):
├─ monthly_quota_usd: $10,000
├─ overage_allowed: true (20% buffer)
├─ suspend_on_exceed: false

Tier 4 (Enterprise):
├─ monthly_quota_usd: $100,000+ (custom)
├─ overage_allowed: true (50% buffer)
├─ suspend_on_exceed: false
└─ dedicated_support: true
```

---

## Part 8: Regulatory & Compliance

### 8.1 Data Residency Requirements

```
GDPR (European Union)
├─ Personal data must reside in EU
├─ Solution: Tertiary region (eu-west1 - Ireland)
├─ What data lives in EU:
│  ├─ Customer profile (name, email, address)
│  ├─ Billing events for EU customers
│  ├─ Audit logs (14-month retention)
│  └─ Receipts (7 years)
├─ What data remains in US:
│  ├─ Aggregated analytics (anonymized)
│  ├─ System metrics (no PII)
│  └─ Non-EU customer data
└─ Implementation:
   ├─ Separate Firestore instance: eu-west1 (write-through)
   ├─ Separate Spanner instance: eu-west1 (sync replication)
   ├─ Data pipeline filters EU data before transfer
   └─ Encryption: AES-256 in transit + at rest

CCPA (California)
├─ Consumers have right to:
│  ├─ Know what data is collected
│  ├─ Delete personal data
│  ├─ Opt-out of data sale
│  └─ Data portability
├─ Billing data retention: 7 years (legal requirement)
├─ Implementation:
│  ├─ Consent manager (opt-in/out tracking)
│  ├─ Bulk export API (data portability)
│  ├─ Data deletion job (soft-delete + hard-delete after 90 days)
│  └─ Audit log of all deletions
└─ Non-compliance penalty: $2,500-7,500 per violation

SOC 2 Type II
├─ Security controls audited quarterly
├─ Required components:
│  ├─ Access controls (role-based, MFA)
│  ├─ Encryption (TLS 1.3, AES-256)
│  ├─ Audit logging (every action logged)
│  ├─ Change management (approval required)
│  ├─ Incident response (24h response SLA)
│  └─ Penetration testing (annual)
├─ Certification: Annual audit by Big 4 firm
└─ Cost: $50K-100K/year

HIPAA (Healthcare)
├─ If handling healthcare customer data:
│  ├─ Encryption: end-to-end
│  ├─ Access controls: strict RBAC
│  ├─ Audit logging: every data access logged
│  ├─ Business Associate Agreements (BAA) required
│  └─ Breach notification: 60 days
└─ Implementation: separate isolated environment

PCI DSS (Payment Card Data)
├─ If storing credit card data:
│  ├─ Never store CVV (not allowed)
│  ├─ Tokenize cards via Stripe/Adyen (not stored directly)
│  ├─ Encryption: AES-256 for any stored payment data
│  ├─ Access controls: MFA required
│  └─ Network segmentation: payment data isolated
└─ Implementation: pass-through payment processor (not stored locally)
```

### 8.2 Audit Trail Requirements

```
IMMUTABLE AUDIT LOG (Cloud Firestore)

Collection: /audit-logs/{year}/{month}/{day}/{log_id}

Document Schema:
{
  log_id: "auditlog_2026_01_25_000000001",
  action_type: "billing_event_accepted|rejected|refund_issued",
  timestamp: 1704067200000,  // Server timestamp (immutable)
  actor: {
    type: "customer|admin|system",
    id: "cust_xyz|adm_123|system",
    ip_address: "203.0.113.42",
    user_agent: "Mozilla/5.0..."
  },
  resource: {
    type: "entitlement|customer|invoice",
    id: "ent_abc|cust_xyz|inv_2026_01_25_001"
  },
  change: {
    before: {field1: old_value},
    after: {field1: new_value},
    diff: {field1: "old_value → new_value"}
  },
  result: {
    status: "success|failure",
    error_message: null | "error description",
    receipt_id: "r_2026_01_25_abc123xyz"
  },
  compliance: {
    gdpr_compliant: true,
    ccpa_compliant: true,
    pci_compliant: false,  // No card data in this event
    soc2_relevant: true
  }
}

RETENTION POLICY:

├─ Active Logs (1 month): Firestore (hot)
├─ Warm Logs (1-12 months): Cloud Storage (standard)
├─ Cold Logs (1-7 years): Cloud Storage (archive)
└─ Deletion: Automatic after 7 years (GDPR compliance)

AUDIT LOG QUERY EXAMPLES:

// Find all billing actions for customer X
db.collection('audit-logs').where('resource.id', '==', 'cust_xyz')
  .where('action_type', '==', 'billing_event_accepted')
  .orderBy('timestamp', 'desc')
  .limit(100)

// Find suspicious activity (high volume in short time)
db.collection('audit-logs').where('actor.id', '==', 'cust_xyz')
  .where('timestamp', '>', now - 3600000)  // last hour
  .orderBy('timestamp', 'desc')
  .count()  // alert if count > 1000

// Compliance report: who accessed what when
db.collection('audit-logs').where('action_type', '==', 'data_access')
  .where('timestamp', '>', now - 86400000)  // last 24h
  .orderBy('actor.id')
  .orderBy('timestamp')
```

---

## Part 9: Cost Analysis

### 9.1 Infrastructure Cost Breakdown

```
MONTHLY COSTS (for 1,000 customers, 10K RPS steady, peaks to 100K RPS)

COMPUTE & NETWORKING:
├─ Cloud Run (compute):
│  ├─ Baseline: 12 instances × 2 vCPU × $0.00002472/sec = $7,129/month
│  ├─ Peak spike (100 instances): $59,407/month (rare)
│  └─ Average: $8,000/month
├─ Cloud Load Balancer: $1,000/month (per forwarding rule)
├─ Network egress: $1,000/month (10TB/month @ $0.12/GB)
└─ Subtotal: ~$10,000/month

STORAGE & DATABASE:
├─ Cloud Spanner:
│  ├─ 3 nodes × $2,550/month = $7,650/month
│  ├─ Storage: 1TB @ $0.30/GB = $300/month
│  └─ Subtotal: $7,950/month
├─ Firestore (on-demand):
│  ├─ Writes: 10K/sec × 2.6B ops/month = $2,600/month @ $1/M
│  ├─ Reads: 5K/sec × 1.3B ops/month = $650/month @ $0.5/M
│  ├─ Storage: 100GB @ $0.18/GB = $18/month
│  └─ Subtotal: $3,268/month
├─ Cloud Storage:
│  ├─ Receipt archive: 1TB @ $0.004/GB/month = $4/month
│  ├─ Backups: 500GB @ $0.004/GB/month = $2/month
│  └─ Subtotal: $6/month
└─ Subtotal: ~$11,224/month

MESSAGING & STREAMING:
├─ Cloud Pub/Sub:
│  ├─ Publishes: 10K/sec × 2.6B ops/month = $500/month @ $0.2/M
│  ├─ Subscriptions: 5 subs @ $0.40/month = $2/month
│  └─ Subtotal: $502/month

OBSERVABILITY & MONITORING:
├─ Cloud Monitoring: $500/month
├─ Cloud Logging: $500/month (500GB ingestion)
├─ Cloud Trace: $100/month
└─ Subtotal: $1,100/month

SECURITY & COMPLIANCE:
├─ Cloud Key Management (HSM keys): $100/month
├─ Secret Manager: $50/month
└─ Subtotal: $150/month

DEVELOPERS & SUPPORT:
├─ Google Cloud Premium Support: $500/month
└─ Subtotal: $500/month

TOTAL MONTHLY COST: $23,076/month
ANNUAL COST: $276,912/year
COST PER CUSTOMER: $276,912 / 1,000 = $277/year = $0.23/month ✓

COST PER EVENT: $276,912 / (10K RPS × 2.6B events/year) = $0.00001/event ✓

UNIT ECONOMICS (per customer):
├─ Annual billing system cost: $277/year
├─ Minimum viable contract: $1,000/year
├─ Gross margin on billing: 72% (before support)
└─ Break-even customer MRR: $50/month
```

### 9.2 Cost Optimization Opportunities

```
SCENARIO: Reduce costs by 30%

Option 1: Firestore → Bigtable (write-heavy optimization)
├─ Firestore (current): $3,268/month
├─ Bigtable: $2,000/month (2 nodes, 10K ops/sec)
├─ Savings: $1,268/month (39% reduction)
├─ Trade-off: Less flexible queries, requires data migration

Option 2: Cloud Spanner → PostgreSQL (eventual consistency)
├─ Cloud Spanner (current): $7,950/month
├─ PostgreSQL (Cloud SQL HA): $3,000/month (High-Memory 32 vCPU)
├─ Savings: $4,950/month (62% reduction)
├─ Trade-off: Lose strong consistency guarantees, risk double-billing

Option 3: Reserved Capacity (1-year commitment)
├─ Cloud Run: 30% discount = $2,400/month savings
├─ Cloud Spanner: 25% discount = $1,988/month savings
├─ Firestore: 15% discount = $490/month savings
├─ Total savings: $4,878/month (21% reduction)
├─ Cost: $0 upfront (commitment-based)
└─ Recommendation: ✓ Do this (best risk/reward)

Option 4: Multi-region consolidation (us-central1 only)
├─ Remove secondary region: save $3,000/month
├─ Trade-off: RTO increases to 30min (manual recovery)
├─ Risk: Zone failure → complete outage (not acceptable)
└─ Recommendation: ✗ Don't do this (violates SLA)

RECOMMENDED OPTIMIZATIONS:
1. Reserve capacity (21% savings, no risk): $4,878/month
2. Bigtable for ledger (39% savings, medium risk): $1,268/month
3. Connection pooling optimization (5% savings, no risk): $1,154/month
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
TOTAL POTENTIAL SAVINGS: $7,300/month (31.6% reduction)
```

---

## Part 10: Migration Strategy

### 10.1 Customer Migration Path

```
MIGRATION TIMELINE: 6-month phased rollout

PHASE 1: FOUNDATION (Week 1-4)
├─ Set up new billing system
├─ Deploy to staging environment
├─ Load test: 1K RPS
├─ Security audit: penetration testing
└─ Customer comms: "New billing system rolling out Q1 2026"

PHASE 2: SOFT LAUNCH (Week 5-8)
├─ Beta customers (50 total):
│  ├─ Existing customers opt-in (10 customers)
│  ├─ New customers start on new system (40 customers)
│  └─ Parallel billing (both old + new system)
├─ Validation: verify receipts match within 0.01%
├─ Monitoring: track all metrics for anomalies
└─ Customer comms: "Early access available"

PHASE 3: EXPANSION (Week 9-16)
├─ Expand to 200 customers
├─ Include Tier 2 customers (higher volume)
├─ Load test: 5K RPS
├─ Run reconciliation job (verify no double-billing)
└─ Customer comms: "General availability available"

PHASE 4: GENERAL AVAILABILITY (Week 17-20)
├─ All new customers start on new system
├─ Existing customers given migration window (optional)
├─ Support team trained on new system
├─ Playbook: migration troubleshooting guide
└─ Customer comms: "All customers can migrate"

PHASE 5: SUNSET OLD SYSTEM (Week 21-26)
├─ Mandatory migration for remaining customers
├─ Gradual migration: 50 customers per week
├─ Parallel run: keep old system live (failback option)
├─ Verification: 100% of customers on new system
└─ Customer comms: "Old system will be retired [date]"

MIGRATION DATA TRANSFER:

For each customer:
├─ Export: 12 months of billing history
├─ Transform: normalize to new receipt format
├─ Validate: checksums match (md5_old == md5_new)
├─ Import: into Firestore + Cloud Spanner
├─ Verify: new system balances = old system balances
├─ Reconcile: catch any discrepancies
└─ Sign-off: customer approves migration

Migration Template:
```
Customer: Acme Inc (cust_xyz)
From: Legacy Billing System
To: TAI Billing System (new)

Historical data (12 months):
├─ Total events: 10,234
├─ Total billing amount: $123,456.78
├─ Disputes: 2 (both resolved)
├─ Refunds: 1 ($50.00)

Validation:
├─ Old system checksum: 3f4a8b2c1d9e7f5a6
├─ New system checksum: 3f4a8b2c1d9e7f5a6 ✓ MATCH
├─ Balance verification: $123,456.78 ✓ MATCH
└─ All receipts migrated: 10,234 ✓ OK

Sign-off:
├─ Date: 2026-02-01
├─ Customer: Jane Doe (jane@acme.com)
├─ Status: APPROVED
└─ Live date: 2026-02-15
```

ROLLBACK PROCEDURE (if issues arise):

If customer reports discrepancies:
├─ Compare old vs new receipts (sample check)
├─ Identify missing/duplicate events
├─ Root cause analysis (error in migration script?)
├─ Fix data: manual corrections in Firestore
├─ Revalidate: new checksums must match old
├─ Retry migration: if systematic issue found
└─ Escalate: if data cannot be reconciled

Fallback option:
├─ Keep customer on old system (temporary)
├─ Retry migration in next batch
├─ Dedicate engineer to resolve issue
└─ Target: migrate within 1 week
```

### 10.2 Behavioral Changes for Customers

```
OLD SYSTEM:
├─ Batch billing (monthly, aggregated)
├─ Invoices generated 5th of month
├─ Payment terms: Net 30
├─ Disputes resolved manually (1-2 weeks)
└─ No real-time usage visibility

NEW SYSTEM:
├─ Real-time billing (each event recorded)
├─ Usage visible in dashboard (live)
├─ Invoices generated 5th of month (same)
├─ Disputes resolved via AI + receipts (<24h)
└─ Notifications on quota warnings
└─ Automatic refunds (if data error detected)

REQUIRED CUSTOMER ACTIONS:

1. Migrate API client:
   ├─ Old: POST /api/billing/create-charge (monthly batch)
   ├─ New: POST /api/v2/events (per-event, real-time)
   └─ Migration time: 1-2 days

2. Update dashboards:
   ├─ Old: Monthly reports from email
   ├─ New: Real-time dashboard (live balance)
   └─ Training needed: 2 hours

3. Reconcile historical data:
   ├─ Old: Manual spreadsheet reconciliation
   ├─ New: Automatic reconciliation via receipts
   └─ No action required (automatic)

4. Dispute process:
   ├─ Old: Email support, 2-week resolution
   ├─ New: Dashboard ticket, <24h resolution
   └─ Training needed: 1 hour

CUSTOMER COMMUNICATIONS PLAN:

Email 1 (Week 1): "Introducing new billing system - no action needed"
├─ Explain benefits (real-time, faster disputes, lower costs)
├─ Share migration timeline
└─ Link to FAQ

Email 2 (Week 8): "Ready to migrate? Opt-in to new billing system"
├─ Explain how to enable
├─ Share onboarding guide
├─ Offer support during migration

Email 3 (Week 16): "New billing system now live for all customers"
├─ Explain auto-migration plan
├─ Share timeline: Week 17 onwards

Email 4 (Week 20): "Your billing account has been migrated"
├─ Verify migration was successful
├─ Show new dashboard
├─ Offer post-migration training call

Email 5 (Week 25): "Old billing system retiring [date]"
├─ Final deadline for migration
├─ Support available for last-minute issues
└─ Link to legacy system archival
```

---

## Part 11: Operational Excellence

### 11.1 SLO/SLI Framework

```
SERVICE LEVEL OBJECTIVES (SLOs)

Availability: 99.99% uptime (52.6 minutes downtime/year)
├─ SLI: (successful_requests / total_requests) > 99.99%
├─ Measurement: per endpoint, aggregated daily
├─ Alert: if availability drops below 99.9% for 5min

Latency: p99 < 100ms for billing events
├─ SLI: (requests with latency < 100ms) > 99%
├─ Measurement: per percentile (p50, p95, p99, p999)
├─ Alert: if p99 > 100ms sustained for 10min

Error Rate: < 0.01% of requests result in unhandled errors
├─ SLI: (error_count / total_requests) < 0.01%
├─ Measurement: by error type (4xx, 5xx)
├─ Alert: if error rate > 1% for 5min

Data Consistency: 100% of billing events eventually consistent
├─ SLI: (receipts_in_ledger / receipts_generated) > 99.99%
├─ Measurement: reconciliation job runs hourly
├─ Alert: if inconsistency detected (escalate to P1)

ALERTS:

P1 (Critical - page on-call):
├─ Availability < 99% (>5min of errors)
├─ Data loss detected (reconciliation failure)
├─ Billing calculation error > 0.1%
├─ Security incident (unauthorized access)
└─ Response SLA: 5 minutes

P2 (High - create ticket):
├─ Availability < 99.5% (but > 99%)
├─ Latency p99 > 100ms (sustained)
├─ Error rate > 0.5% (but < 1%)
├─ Resource exhaustion (CPU > 90%)
└─ Response SLA: 1 hour

P3 (Medium - next business day):
├─ Degraded performance (latency p95 > 100ms)
├─ Non-critical dependent service down
├─ Low error rate (< 0.5%)
└─ Response SLA: 8 hours

METRICS TO TRACK:

Throughput:
├─ events_processed_total (counter)
├─ events_per_second (gauge)
├─ billing_amount_total (counter)
└─ billing_amount_by_customer (histogram)

Latency:
├─ request_duration_ms (histogram: p50, p95, p99, p999)
├─ firestore_write_latency_ms (histogram)
├─ spanner_query_latency_ms (histogram)
└─ end_to_end_latency_ms (histogram)

Errors:
├─ errors_total (counter: by error_type)
├─ error_rate (gauge: percentage)
├─ timeouts_total (counter)
└─ rejected_events_total (counter: by reason)

Resource:
├─ cpu_utilization_percent (gauge)
├─ memory_utilization_percent (gauge)
├─ connection_pool_utilization (gauge)
└─ instance_count (gauge: current running)

Ledger Health:
├─ receipt_reconciliation_status (gauge: ok/failed)
├─ ledger_inconsistencies_total (counter)
├─ hash_chain_validation_failures (counter)
└─ backup_sync_lag_seconds (gauge)
```

### 11.2 Runbook: Common Issues

```
ISSUE 1: "Customer reports double charge"

Investigation:
├─ Check: Are there duplicate events with same idempotent_key?
│  └─ Query: SELECT * FROM events WHERE idempotent_key = 'X' AND customer = 'Y'
├─ Check: Are there duplicate receipt IDs?
│  └─ Query: SELECT * FROM receipts WHERE customer = 'Y' ORDER BY timestamp
└─ Check: What does customer's ledger show?
   └─ Query: SUM(amount) FROM receipts WHERE customer = 'Y' AND date = '2026-01-25'

Root Causes & Fixes:
├─ If duplicate idempotent_keys:
│  ├─ Root cause: Customer submitted same idempotent_key twice
│  ├─ Fix: Return cached receipt (user sees only one charge)
│  ├─ Verification: Confirm with customer via receipt_id
│  └─ Follow-up: Educate customer on idempotent_key usage
├─ If duplicate receipt_ids:
│  ├─ Root cause: System bug or data corruption
│  ├─ Fix: Mark older receipt as "voided", issue refund receipt
│  ├─ Verification: Reconciliation job detects, alerts
│  └─ Follow-up: Root cause analysis (P1 incident)
├─ If ledger shows duplicate amounts:
│  ├─ Root cause: Likely different transactions on same day
│  ├─ Fix: Verify transactions are different (different timestamps, event_ids)
│  ├─ Verification: Show customer detailed receipt history
│  └─ Follow-up: If confirmed duplicates → refund

Customer Communication:
├─ "Thanks for reporting. We've investigated and found: [explanation]"
├─ If our fault: "We've issued a refund of $[amount]. [Receipt link]"
├─ If customer error: "This appears to be a second submission of the same usage. The charge is correct and you've only been billed once."
└─ Escalation: Dispute closed (no refund) OR Refund issued (dispute resolved)

---

ISSUE 2: "Events are being rejected with 429 (rate limited)"

Investigation:
├─ Check: What's the customer's configured rate limit?
│  └─ Query: SELECT rate_limit_rps FROM customer_quotas WHERE customer_id = 'X'
├─ Check: How many events/sec are they currently sending?
│  └─ Query: SELECT COUNT(*) FROM events WHERE customer_id = 'X' AND timestamp > now() - 60s
├─ Check: Did their usage spike recently?
│  └─ Query: SELECT DATE, COUNT(*) FROM events WHERE customer_id = 'X' GROUP BY DATE
└─ Check: Are they using connection pooling?
   └─ Analyze: Are they creating new connections for each request?

Root Causes & Fixes:
├─ If legitimately over rate limit:
│  ├─ Root cause: Business growth (more events)
│  ├─ Fix: Increase rate_limit_rps in configuration
│  ├─ Verification: Confirm new rate limit sufficient for 24h peak
│  └─ Follow-up: Recommend batch endpoint (10x higher throughput)
├─ If using inefficient client:
│  ├─ Root cause: Not reusing connections (creating new TCP connection per request)
│  ├─ Fix: Educate on connection pooling (keep-alive)
│  ├─ Verification: Compare network connections before/after
│  └─ Follow-up: Provide code sample with connection pooling
├─ If rate limiter misconfigured:
│  ├─ Root cause: system bug
│  ├─ Fix: Correct rate limiter configuration
│  ├─ Verification: Rerun load test
│  └─ Follow-up: Prevent via load testing before deployment

Customer Communication:
├─ "Your account is sending [X] events/sec, but configured limit is [Y] events/sec."
├─ Option A: "We've increased your limit to [Z] events/sec (no cost change)."
├─ Option B: "Please batch events using our batch endpoint (supports up to [Z] events/sec)."
└─ Follow-up: "Send us a note if you need further increases."

---

ISSUE 3: "Reconciliation job detected inconsistency (P1 alert)"

Investigation:
├─ What's the inconsistency?
│  ├─ Missing receipts (Firestore has events not in Spanner)?
│  ├─ Balance mismatch (ledger sum != Spanner balance)?
│  └─ Timestamp ordering issue (events out of order)?
├─ Which customers affected?
│  └─ Query: SELECT DISTINCT customer_id FROM inconsistencies
├─ When did it start?
│  └─ Query: SELECT MIN(timestamp) FROM inconsistencies ORDER BY timestamp
└─ What's the impact (financially)?
   └─ Calc: SUM(balance_diff) FROM inconsistencies

Root Causes & Fixes:
├─ If Firestore write succeeded, Spanner failed:
│  ├─ Root cause: Network issue or Spanner overload
│  ├─ Fix: Retry all pending Spanner writes
│  ├─ Verification: Rerun reconciliation (should pass)
│  └─ Follow-up: Increase Spanner capacity if consistent pattern
├─ If Spanner write succeeded, Firestore failed:
│  ├─ Root cause: Firestore overloaded
│  ├─ Fix: Retry all pending Firestore writes
│  ├─ Verification: Rerun reconciliation (should pass)
│  └─ Follow-up: Investigate Firestore quota (increase if needed)
├─ If both systems have conflicting data:
│  ├─ Root cause: Data corruption (likely bug in application)
│  ├─ Fix: Determine source of truth (Firestore events are authoritative)
│  ├─ Action: Sync Spanner balances from Firestore
│  └─ Follow-up: Root cause analysis (what caused corruption?)

Recovery Procedure:
├─ If minor inconsistency (< $1000 affected):
│  ├─ Step 1: Determine correct balance from receipts
│  ├─ Step 2: Issue corrective receipt (debit or credit)
│  ├─ Step 3: Update Spanner (one-time manual update)
│  ├─ Step 4: Notify affected customers
│  └─ Step 5: Implement fix to prevent recurrence
├─ If major inconsistency (> $1000 affected):
│  ├─ Step 1: Activate incident response (page all on-call)
│  ├─ Step 2: Stop accepting new events (return 503)
│  ├─ Step 3: Identify all affected customers
│  ├─ Step 4: Halt billing until resolved
│  ├─ Step 5: Run full ledger reconciliation
│  ├─ Step 6: Issue corrective transactions
│  ├─ Step 7: Resume service once verified
│  └─ Step 8: Post-mortem analysis

Customer Communication:
├─ "We've detected a billing discrepancy affecting [N] customers."
├─ "We're resolving this immediately and no additional charges will be applied."
├─ "We're issuing corrective credits to affected accounts within 24h."
└─ "Full investigation and prevention plan will be shared within 72h."
```

---

## Part 12: Security Architecture

### 12.1 Defense-in-Depth

```
LAYER 1: Network Security
├─ Global HTTP(S) load balancer
├─ DDoS protection (Google Cloud Armor)
├─ TLS 1.3 only (no TLS 1.2)
├─ Certificate pinning (for mobile clients)
└─ WAF rules (SQL injection, XSS prevention)

LAYER 2: Authentication
├─ JWT signature verification (RS256)
├─ Asymmetric key rotation (quarterly)
├─ Customer API keys (stored in GCP Secret Manager)
├─ Admin MFA (TOTP or WebAuthn)
└─ Session tokens (short-lived: 15 min)

LAYER 3: Authorization
├─ Role-Based Access Control (RBAC)
├─ Customer can only access their data
├─ Admin roles: read-only, write, delete (separate)
├─ Principle of least privilege
└─ Regular access audits (quarterly)

LAYER 4: Data Protection
├─ Encryption at rest (AES-256)
├─ Encryption in transit (TLS 1.3)
├─ Field-level encryption (PII fields)
├─ Secrets rotation (monthly)
└─ No sensitive data in logs

LAYER 5: API Security
├─ Rate limiting (per customer, per IP)
├─ Input validation (schema validation)
├─ Output encoding (prevent XSS)
├─ CORS policy (only trusted origins)
└─ CSRF protection (token-based)

LAYER 6: Database Security
├─ Firestore encryption (Google-managed keys)
├─ Cloud Spanner encryption (Google-managed keys)
├─ Backup encryption (AES-256)
├─ Automated backups (encrypted)
└─ Point-in-time recovery (7 days)

LAYER 7: Monitoring & Detection
├─ Intrusion detection (Cloud IDS)
├─ Log analysis (anomaly detection)
├─ Security incident response (SOAR)
├─ Automated threat response (auto-block IPs)
└─ Compliance scanning (weekly)

LAYER 8: Incident Response
├─ Incident response plan (documented)
├─ War room setup (30 second callout)
├─ Forensics capture (preserve evidence)
├─ Stakeholder notification (timeline: 1h, 24h, 72h)
└─ Post-incident review (within 1 week)
```

### 12.2 Security Checklist

```
DEPLOYMENT SECURITY REQUIREMENTS (before prod)

Network:
  [ ] TLS certificate installed and valid
  [ ] Certificate pinning configured (mobile)
  [ ] DDoS protection enabled (Cloud Armor)
  [ ] WAF rules deployed (OWASP top 10)
  [ ] Network policies restrict inter-service communication

Authentication:
  [ ] JWT key stored in Secret Manager (not in code)
  [ ] Key rotation scheduled (quarterly)
  [ ] Admin MFA enforced
  [ ] API key management system operational
  [ ] Session timeouts configured (15 min idle)

Authorization:
  [ ] RBAC roles defined and tested
  [ ] Customer isolation verified (no cross-tenant access)
  [ ] Admin actions logged and monitored
  [ ] Service account permissions minimal
  [ ] Regular access audits scheduled (quarterly)

Data:
  [ ] Encryption at rest enabled (Firestore, Spanner, Storage)
  [ ] Encryption in transit enabled (TLS 1.3)
  [ ] PII field-level encryption enabled
  [ ] Backup encryption enabled
  [ ] Data retention policy documented

Code:
  [ ] No hardcoded secrets (bandit scan clean)
  [ ] No debug endpoints in production
  [ ] No stack traces in error responses
  [ ] Input validation on all endpoints
  [ ] SQL injection prevention (parameterized queries)

Monitoring:
  [ ] CloudTrail logging enabled (all actions)
  [ ] Cloud Logging configured (all events)
  [ ] Intrusion detection enabled
  [ ] Security Center alerts configured
  [ ] Incident response playbook ready

Compliance:
  [ ] Security audit completed (3rd party)
  [ ] Penetration testing passed
  [ ] OWASP ZAP scan passed
  [ ] SAST/DAST scans passed
  [ ] Dependency vulnerabilities scanned (snyk)

SIGN-OFF:
  Security Review: [Name, Date] _______
  Compliance Review: [Name, Date] _______
  CTO Approval: [Name, Date] _______
```

---

## Part 13: Deployment & Operations

### 13.1 Infrastructure-as-Code (Terraform)

```hcl
# terraform/main.tf
# Billing system infrastructure (production)

provider "google" {
  project = var.gcp_project
  region  = var.primary_region
}

# PRIMARY REGION: US Central 1
module "compute_primary" {
  source = "./modules/compute"

  environment    = "production"
  region         = "us-central1"
  min_instances  = 3
  max_instances  = 100
  target_cpu     = 60
  container_image = "gcr.io/my-project/billing-service:latest"

  env_vars = {
    GOOGLE_CLOUD_PROJECT = var.gcp_project
    FIRESTORE_DATABASE   = "billing-db"
    SPANNER_INSTANCE     = "billing-prod"
  }

  tags = ["billing-prod", "critical"]
}

module "database_primary" {
  source = "terraform-google-modules/spanner//modules/spanner"

  project_id = var.gcp_project
  instance_id = "billing-prod"
  instance_name = "Billing (Production)"
  config = "nam11"
  display_name = "Billing Database - Primary"
  node_count = 3

  databases = [{
    name = "billing"
    version_retention_period = "3600s"
    ddl = file("${path.module}/schema.sql")
  }]
}

module "firestore_primary" {
  source = "terraform-google-modules/firestore//modules/firestore"

  project_id = var.gcp_project
  database_id = "billing-db"
  type = "FIRESTORE_NATIVE"
  location_id = "us-central1"
  deletion_protection = true
}

# SECONDARY REGION: US East 1
module "compute_secondary" {
  source = "./modules/compute"

  environment    = "production"
  region         = "us-east1"
  min_instances  = 1  # Standby, scaled down
  max_instances  = 50
  target_cpu     = 60
  container_image = "gcr.io/my-project/billing-service:latest"

  # Secondary region config
}

module "database_secondary" {
  source = "terraform-google-modules/spanner//modules/spanner-backup"

  project_id = var.gcp_project
  instance_id = "billing-secondary"
  backup_id = "billing-prod-replica"
  version_time = timestamp()
}

# GLOBAL RESOURCES
resource "google_compute_global_address" "billing_lb" {
  name = "billing-lb-ip"
}

resource "google_compute_backend_service" "billing" {
  name            = "billing-backend"
  protocol        = "HTTPS"
  port_name       = "http"
  health_checks   = [google_compute_health_check.billing.id]

  backend {
    group           = module.compute_primary.instance_group_self_link
    balancing_mode  = "RATE"
    max_rate_per_endpoint = 100000
  }
}

resource "google_compute_url_map" "billing" {
  name            = "billing-url-map"
  default_service = google_compute_backend_service.billing.id
}

resource "google_compute_ssl_certificate" "billing" {
  name            = "billing-cert"
  certificate     = file("${path.module}/certs/billing.crt")
  private_key     = file("${path.module}/certs/billing.key")
}

resource "google_compute_target_https_proxy" "billing" {
  name             = "billing-https-proxy"
  url_map          = google_compute_url_map.billing.id
  ssl_certificates = [google_compute_ssl_certificate.billing.id]
}

resource "google_compute_forwarding_rule" "billing" {
  name                  = "billing-forwarding-rule"
  ip_protocol           = "TCP"
  load_balancing_scheme = "EXTERNAL"
  port_range            = "443"
  target                = google_compute_target_https_proxy.billing.id
  address               = google_compute_global_address.billing_lb.id
}

# MONITORING & ALERTING
resource "google_monitoring_alert_policy" "billing_errors" {
  display_name = "Billing Errors Alert"
  conditions {
    display_name = "Error rate > 1%"
    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/request_count\" AND resource.label.service_name=\"billing-service\""
      comparison      = "COMPARISON_GT"
      threshold_value = 0.01
      duration        = "60s"
    }
  }
  notification_channels = [google_monitoring_notification_channel.pagerduty.id]
}

# Outputs
output "billing_service_url" {
  value = "https://${google_compute_global_address.billing_lb.address}"
}
```

### 13.2 Deployment Checklist

```
PRE-DEPLOYMENT CHECKLIST

Code Quality:
  [ ] All tests passing (100%)
  [ ] Code review approved (2 reviewers)
  [ ] Security scan passed (no vulnerabilities)
  [ ] Performance benchmarks passed (latency SLO met)
  [ ] Load test passed (1.5x peak capacity)
  [ ] Integration test passed (end-to-end)

Infrastructure:
  [ ] Terraform plan reviewed
  [ ] Database migrations tested (rollback tested)
  [ ] Secrets configured correctly
  [ ] Backups configured and tested
  [ ] Monitoring and alerting working

Documentation:
  [ ] Runbooks updated
  [ ] Changelog updated
  [ ] API documentation updated
  [ ] Deployment guide updated
  [ ] Rollback procedure documented

Communication:
  [ ] Customer notification email drafted
  [ ] Support team briefed
  [ ] On-call team briefed
  [ ] Maintenance window scheduled (if needed)
  [ ] Status page message prepared

DEPLOYMENT STEPS:

1. Notify stakeholders
   └─ Send: "Deployment starting at [time]"
2. Run pre-flight checks
   └─ $ terraform plan -out=tfplan
   └─ $ ./scripts/pre-deploy-checks.sh
3. Deploy to staging (for validation)
   └─ $ terraform apply -target='module.compute_staging'
   └─ $ ./scripts/smoke-tests.sh
4. If staging OK, deploy to production
   └─ $ terraform apply -target='module.database_primary'
   └─ $ terraform apply -target='module.compute_primary'
5. Verify deployment
   └─ $ ./scripts/health-check.sh
   └─ $ ./scripts/validate-receipts.sh
6. Monitor for issues
   └─ Watch: Cloud Monitoring dashboard
   └─ Watch: Error logs in Cloud Logging
   └─ Duration: 1 hour of elevated monitoring
7. Notify completion
   └─ Send: "Deployment complete at [time]"
   └─ Send: "Monitoring green, no issues detected"

ROLLBACK PROCEDURE (if issues):

1. Immediate action (within 1 min):
   └─ Stop accepting new requests (return 503)
   └─ Page on-call engineers
2. Investigation (within 5 min):
   └─ Identify issue (logs, metrics, customer reports)
   └─ Determine if rollback is safe
3. Execute rollback:
   └─ $ terraform apply -var="billing_service_version=previous"
   └─ $ ./scripts/verify-rollback.sh
4. Verify (within 5 min):
   └─ Health checks passing
   └─ Error rate < 0.1%
   └─ Accept new requests
5. Communication:
   └─ Notify: "Issue detected, rolled back to previous version"
   └─ Notify: "Service fully operational"
   └─ Schedule: Post-mortem in 24h
```

---

## Conclusion

This architecture provides a **production-grade billing system** that can scale to 1,000+ customers with **zero billing disputes**. Key differentiators:

1. **Deterministic State Machines**: Erlang/OTP guarantees no edge cases slip through
2. **Cryptographic Receipts**: Every billing event is cryptographically signed and auditable
3. **Multi-Region Failover**: <2 minute RTO for complete zone failures
4. **Idempotent Processing**: No accidental double-billing even on network retries
5. **Eventual Consistency**: Automatic reconciliation prevents data corruption
6. **Enterprise Compliance**: GDPR, CCPA, SOC2, PCI-DSS ready

**Cost per customer**: $0.23/month in infrastructure
**Error rate target**: 1 in 1,000,000 events
**Dispute rate target**: <0.01% of transactions

This is enterprise-grade billing infrastructure, ready for production deployment.

---

**End of Document**
