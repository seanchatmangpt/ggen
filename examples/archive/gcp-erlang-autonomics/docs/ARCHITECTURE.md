# Architecture: Deep Dive

This document provides comprehensive architectural analysis of the GCP Erlang Autonomics system, including system context, component design, FSM state machines, signal processing pipelines, and receipt verification mechanisms.

---

## Table of Contents

1. [System Context (C4 Level 1)](#system-context-c4-level-1)
2. [Container Decomposition (C4 Level 2)](#container-decomposition-c4-level-2)
3. [Component Design (C4 Level 3)](#component-design-c4-level-3)
4. [Deployment Architecture (C4 Level 4)](#deployment-architecture-c4-level-4)
5. [FSM State Machines](#fsm-state-machines)
6. [Signal Processing Pipeline](#signal-processing-pipeline)
7. [Receipt Verification](#receipt-verification)
8. [MAPE-K Integration](#mape-k-integration)
9. [Data Flow](#data-flow)

---

## System Context (C4 Level 1)

**See**: [generated/c4-level1-context.mmd](../generated/c4-level1-context.mmd)

### Actors & External Systems

```
┌──────────────────────────────────────────────────────────────┐
│                                                              │
│                    GCP Erlang Autonomics                    │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │         Cloud Monitoring / Cloud Trace                │ │
│  │  (Metrics, logs, traces from running services)        │ │
│  └────────────┬─────────────────────────────────────────┘ │
│               │                                             │
│  ┌────────────▼─────────────────────────────────────────┐ │
│  │    Signal Ingest (Pub/Sub)                          │ │
│  │  (Normalize, deduplicate cloud events)              │ │
│  └────────────┬─────────────────────────────────────────┘ │
│               │                                             │
│  ┌────────────▼─────────────────────────────────────────┐ │
│  │    FSM Governor (Stateless Analysis)                │ │
│  │  (Analyze signals, plan actions)                    │ │
│  └────────────┬─────────────────────────────────────────┘ │
│               │                                             │
│  ┌────────────▼─────────────────────────────────────────┐ │
│  │    Actuator (Execution Layer)                       │ │
│  │  (Call GCP APIs)                                    │ │
│  └────────────┬─────────────────────────────────────────┘ │
│               │                                             │
│  ┌────────────▼─────────────────────────────────────────┐ │
│  │    Receipt Ledger (BigQuery)                        │ │
│  │  (Immutable audit trail)                           │ │
│  └────────────┬─────────────────────────────────────────┘ │
│               │                                             │
│  └────────────▼──────────────────────────────────────────┐
│  │                                                       │
│  │  Cloud Run, GKE, APIs (Target Systems)              │
│  │  (Scaled, rolled back, rate-limited)               │
│  │                                                       │
│  └───────────────────────────────────────────────────────┘
│
└──────────────────────────────────────────────────────────────┘
```

### Key Actors

| Actor | Role | Interaction |
|-------|------|-------------|
| **Cloud Operations Team** | Observes system behavior | Views receipts in BigQuery, receives alerts |
| **Cloud Monitoring** | Generates signals | Publishes metrics/logs to Pub/Sub |
| **Cloud Run Services** | Target systems | Scaled up/down by Actuator |
| **GKE Clusters** | Alternative target | Auto-scaled by Actuator |
| **BigQuery** | Receipt ledger | Receives audit trail entries |

---

## Container Decomposition (C4 Level 2)

**See**: [generated/c4-level2-containers.mmd](../generated/c4-level2-containers.mmd)

### Service Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                 GCP Erlang Autonomics System                    │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Signal Ingest Service (Rust, Tokio, Cloud Run)        │  │
│  │                                                         │  │
│  │  • Pub/Sub subscription listener                       │  │
│  │  • Signal validation + SPARQL enrichment              │  │
│  │  • Deduplication (Redis cache)                        │  │
│  │  • Rate limiting (token bucket)                       │  │
│  │  • Pub/Sub to Governor queue                          │  │
│  │                                                         │  │
│  │  Inputs: Cloud Events (Pub/Sub)                       │  │
│  │  Outputs: Validated signals (Pub/Sub)                 │  │
│  └──────────────────────┬──────────────────────────────────┘  │
│                         │                                      │
│  ┌──────────────────────▼──────────────────────────────────┐  │
│  │  Governor Service (Rust, Tokio, Cloud Run)            │  │
│  │                                                         │  │
│  │  • FSM state machine execution                        │  │
│  │  • SPARQL query evaluation (decision logic)           │  │
│  │  • State persistence (Firestore)                      │  │
│  │  • Action planning                                    │  │
│  │  • Pub/Sub to Actuator queue                          │  │
│  │                                                         │  │
│  │  Inputs: Validated signals (Pub/Sub)                  │  │
│  │  Outputs: Action plans (Pub/Sub)                      │  │
│  │  State: Firestore (governor state + signal history)   │  │
│  └──────────────────────┬──────────────────────────────────┘  │
│                         │                                      │
│  ┌──────────────────────▼──────────────────────────────────┐  │
│  │  Actuator Service (Rust, Tokio, Cloud Run)            │  │
│  │                                                         │  │
│  │  • GCP API calls (Cloud Run, GKE, etc.)              │  │
│  │  • Retry logic (exponential backoff)                 │  │
│  │  • Action verification (check outcome)               │  │
│  │  • Pub/Sub to Receipt queue                          │  │
│  │                                                         │  │
│  │  Inputs: Action plans (Pub/Sub)                       │  │
│  │  Outputs: Action results (Pub/Sub)                    │  │
│  │  External: GCP APIs (Cloud Run, GKE, etc.)           │  │
│  └──────────────────────┬──────────────────────────────────┘  │
│                         │                                      │
│  ┌──────────────────────▼──────────────────────────────────┐  │
│  │  Receipt Ledger Service (Rust, Tokio, Cloud Run)      │  │
│  │                                                         │  │
│  │  • Cryptographic receipt generation (SHA-256)        │  │
│  │  • BigQuery inserts (append-only audit log)           │  │
│  │  • Receipt verification + tamper detection            │  │
│  │  • GCS archive (historical records)                   │  │
│  │                                                         │  │
│  │  Inputs: Action results (Pub/Sub)                     │  │
│  │  Outputs: Receipt ledger (BigQuery)                   │  │
│  │  Storage: BigQuery (receipt_ledger), GCS (archive)    │  │
│  └─────────────────────────────────────────────────────────┘  │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Data Layer                                             │  │
│  │                                                         │  │
│  │  • Firestore: Governor state, signal history          │  │
│  │  • BigQuery: Receipt ledger, analytics                │  │
│  │  • Redis: Deduplication cache, TTL management         │  │
│  │  • Cloud Storage: Audit trails, exported data         │  │
│  │                                                         │  │
│  └─────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Container Details

| Container | Language | Runtime | Scale | Purpose |
|-----------|----------|---------|-------|---------|
| **Signal Ingest** | Rust | Tokio | Cloud Run | Normalize, deduplicate signals |
| **Governor** | Rust | Tokio | Cloud Run | FSM decision engine |
| **Actuator** | Rust | Tokio | Cloud Run | Execute GCP API calls |
| **Receipt Ledger** | Rust | Tokio | Cloud Run | Cryptographic proofs |
| **Firestore** | — | Managed | — | Governor state, history |
| **BigQuery** | — | Managed | — | Receipt audit trail |
| **Redis** | — | Memorystore | — | Deduplication cache |
| **Pub/Sub** | — | Managed | — | Event routing |

### Inter-Service Communication

```
Signal Ingest ──(validated_signals)──> Governor
                                           │
                                           ├─(state_update)──> Firestore
                                           │
                                           └─(actions)──> Actuator
                                                            │
                                                            ├─(execution_result)──> Receipt Ledger
                                                            │
                                                            └─(API calls)──> GCP APIs
```

---

## Component Design (C4 Level 3)

**See**: [generated/c4-level3-components.mmd](../generated/c4-level3-components.mmd)

### Governor FSM Components

The Governor Service is decomposed into components implementing a Finite State Machine:

```
┌─────────────────────────────────────────────────────────────┐
│              Governor Service (Rust, Tokio)                │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐  │
│  │         Input Buffer (Pub/Sub Subscription)         │  │
│  │  • Ack/Nack handling                               │  │
│  │  • Dead-letter queue on error                      │  │
│  └──────────────────────┬────────────────────────────┘  │
│                         │                                │
│  ┌──────────────────────▼────────────────────────────┐  │
│  │      Signal Deserializer (Serde JSON)            │  │
│  │  • Validate signal schema                        │  │
│  │  • Type-safe deserialization                     │  │
│  │  • Error handling (invalid signals)              │  │
│  └──────────────────────┬────────────────────────────┘  │
│                         │                                │
│  ┌──────────────────────▼────────────────────────────┐  │
│  │   FSM State Machine (Erlang-inspired)            │  │
│  │  • Current state (from Firestore)                │  │
│  │  • State invariants (type-safe)                  │  │
│  │  • Transition rules (SPARQL-driven)              │  │
│  │  • Guard conditions (thresholds, timers)         │  │
│  └──────────────────────┬────────────────────────────┘  │
│                         │                                │
│  ┌──────────────────────▼────────────────────────────┐  │
│  │    SPARQL Query Engine (Oxigraph)                │  │
│  │  • Decision logic (SELECT queries)               │  │
│  │  • Inference (OWL rules)                         │  │
│  │  • Ontology-driven rules                         │  │
│  └──────────────────────┬────────────────────────────┘  │
│                         │                                │
│  ┌──────────────────────▼────────────────────────────┐  │
│  │      Action Planner (Result<T,E>)                │  │
│  │  • Determine required actions                    │  │
│  │  • Validate preconditions                        │  │
│  │  • Estimate outcomes                             │  │
│  │  • Safe failure handling                         │  │
│  └──────────────────────┬────────────────────────────┘  │
│                         │                                │
│  ┌──────────────────────▼────────────────────────────┐  │
│  │   State Update (Firestore Transaction)           │  │
│  │  • Persist new state                             │  │
│  │  • Add signal to history                         │  │
│  │  • Timestamp all changes                         │  │
│  └──────────────────────┬────────────────────────────┘  │
│                         │                                │
│  ┌──────────────────────▼────────────────────────────┐  │
│  │   Output Buffer (Pub/Sub Publisher)              │  │
│  │  • Serialize actions to JSON                     │  │
│  │  • Publish to Actuator queue                     │  │
│  │  • Idempotent publishing (dedup by ID)           │  │
│  └─────────────────────────────────────────────────┘  │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### Signal Ingest Components

```
┌─────────────────────────────────────────────────────────┐
│         Signal Ingest Service (Rust, Tokio)            │
│                                                         │
│  ┌─────────────────────────────────────────────────┐  │
│  │    Event Receiver (Pub/Sub Subscription)       │  │
│  │  • Receive Cloud Events                        │  │
│  │  • Convert to internal format                  │  │
│  └──────────────────────┬──────────────────────┘  │
│                         │                          │
│  ┌──────────────────────▼──────────────────────┐  │
│  │      Signal Normalizer (SPARQL)            │  │
│  │  • Map external format to ontology          │  │
│  │  • Extract key fields (value, threshold)   │  │
│  │  • Apply unit conversions                   │  │
│  └──────────────────────┬──────────────────┘  │
│                         │                      │
│  ┌──────────────────────▼──────────────────┐  │
│  │  Deduplicator (Redis Cache)            │  │
│  │  • Check for duplicate signal ID       │  │
│  │  • Skip if seen in last N seconds      │  │
│  │  • Maintain TTL for cache entries      │  │
│  └──────────────────────┬──────────────────┘  │
│                         │                      │
│  ┌──────────────────────▼──────────────────┐  │
│  │   Rate Limiter (Token Bucket)           │  │
│  │  • Track signal rate per type           │  │
│  │  • Reject if rate exceeded              │  │
│  │  • Buffer accepted signals              │  │
│  └──────────────────────┬──────────────────┘  │
│                         │                      │
│  ┌──────────────────────▼──────────────────┐  │
│  │   Validator (JSON Schema)                │  │
│  │  • Check required fields                │  │
│  │  • Type validation (int, float, etc)    │  │
│  │  • Range validation (thresholds)        │  │
│  └──────────────────────┬──────────────────┘  │
│                         │                      │
│  ┌──────────────────────▼──────────────────┐  │
│  │   Output Queue (Pub/Sub Publisher)      │  │
│  │  • Publish to Governor queue            │  │
│  │  • Metadata: source, timestamp, hash    │  │
│  └──────────────────────────────────────┘  │
│                                             │
└─────────────────────────────────────────────┘
```

### Actuator Components

```
┌─────────────────────────────────────────────────────────┐
│          Actuator Service (Rust, Tokio)                │
│                                                         │
│  ┌─────────────────────────────────────────────────┐  │
│  │    Input Buffer (Pub/Sub Subscription)         │  │
│  │  • Receive action plans                        │  │
│  └──────────────────────┬──────────────────────┘  │
│                         │                          │
│  ┌──────────────────────▼──────────────────────┐  │
│  │     Action Router                           │  │
│  │  • Dispatch to handler by action type      │  │
│  │  • Load balance across handlers             │  │
│  │  • Circuit breaker (fail fast)              │  │
│  └──────────────────────┬──────────────────────┘  │
│                         │                          │
│   ┌─────────────────────┴──────────────────────┐  │
│   │                   │                        │  │
│  ┌▼──────────┐  ┌──────▼─────┐  ┌─────────▼──┐│
│  │Cloud Run  │  │GKE         │  │Cloud       ││
│  │Handler    │  │Handler     │  │Storage,   ││
│  │           │  │            │  │etc Handler││
│  │• Scale    │  │• Scale     │  │• Delete   ││
│  │• Rollback │  │• Update    │  │• Upload   ││
│  │• Route    │  │• Patch     │  │• Monitor  ││
│  └┬──────────┘  └──────┬─────┘  └─────────┬──┘│
│   │                   │                    │   │
│  ┌▼───────────────────▼────────────────────▼─┐ │
│  │    Retry Logic (Exponential Backoff)     │ │
│  │  • Max retries: 3                        │ │
│  │  • Backoff: 1s, 2s, 4s                  │ │
│  │  • Timeout: 30s per request              │ │
│  └──────────────────────┬────────────────────┘ │
│                         │                      │
│  ┌──────────────────────▼────────────────────┐ │
│  │  Verification (GET/DESCRIBE)              │ │
│  │  • Verify action outcome                 │ │
│  │  • Check new state matches expected      │ │
│  │  • Timeout if verification hangs         │ │
│  └──────────────────────┬────────────────────┘ │
│                         │                      │
│  ┌──────────────────────▼────────────────────┐ │
│  │   Output Queue (Pub/Sub Publisher)       │ │
│  │  • Publish result (success/failure)      │ │
│  │  • Include outcome details               │ │
│  │  • Timestamp + execution duration        │ │
│  └──────────────────────────────────────────┘ │
│                                               │
└───────────────────────────────────────────────┘
```

---

## Deployment Architecture (C4 Level 4)

**See**: [generated/c4-level4-deployment.mmd](../generated/c4-level4-deployment.mmd)

### GCP Infrastructure

```
┌────────────────────────────────────────────────────────────────┐
│                    GCP Project                                │
│                                                                │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │              us-central1 Region                          │ │
│  │                                                          │ │
│  │  ┌────────────────────────────────────────────────────┐ │ │
│  │  │         Cloud Run (Serverless Container)          │ │ │
│  │  │                                                    │ │ │
│  │  │  • signal-ingest-service:v1 (2-10 instances)     │ │ │
│  │  │  • governor-service:v1 (1-5 instances)           │ │ │
│  │  │  • actuator-service:v1 (1-5 instances)           │ │ │
│  │  │  • receipt-ledger-service:v1 (1-2 instances)     │ │ │
│  │  │                                                    │ │ │
│  │  │  Memory: 512Mi per instance                       │ │ │
│  │  │  CPU: 1 vCPU per instance                         │ │ │
│  │  │  Max concurrency: 80                              │ │ │
│  │  └────────────────────────────────────────────────────┘ │ │
│  │                                                          │ │
│  │  ┌────────────────────────────────────────────────────┐ │ │
│  │  │           Pub/Sub Message Queues                  │ │ │
│  │  │                                                    │ │ │
│  │  │  • cloud-events-topic (input signals)             │ │ │
│  │  │  • validated-signals (ingest→governor)            │ │ │
│  │  │  • action-plans (governor→actuator)               │ │ │
│  │  │  • action-results (actuator→ledger)               │ │ │
│  │  │  • dead-letter-queue (errors)                     │ │ │
│  │  │                                                    │ │ │
│  │  │  Retention: 7 days                                │ │ │
│  │  │  Ack deadline: 60s                                │ │ │
│  │  └────────────────────────────────────────────────────┘ │ │
│  │                                                          │ │
│  │  ┌────────────────────────────────────────────────────┐ │ │
│  │  │          Data Stores                              │ │ │
│  │  │                                                    │ │ │
│  │  │  • Firestore                                       │ │ │
│  │  │    - Collections: governor_state, signal_history  │ │ │
│  │  │    - TTL: 30 days for signal history              │ │ │
│  │  │                                                    │ │ │
│  │  │  • BigQuery                                        │ │ │
│  │  │    - Dataset: autonomic                           │ │ │
│  │  │    - Tables: receipt_ledger (append-only)         │ │ │
│  │  │    - Retention: unlimited                         │ │ │
│  │  │                                                    │ │ │
│  │  │  • Memorystore (Redis)                            │ │ │
│  │  │    - Size: 1GB                                    │ │ │
│  │  │    - TTL: 15min (dedup cache)                     │ │ │
│  │  │                                                    │ │ │
│  │  │  • Cloud Storage                                   │ │ │
│  │  │    - Bucket: autonomic-audit-trail                │ │ │
│  │  │    - Retention: 1 year                            │ │ │
│  │  │    - Versioning: enabled                          │ │ │
│  │  └────────────────────────────────────────────────────┘ │ │
│  │                                                          │ │
│  │  ┌────────────────────────────────────────────────────┐ │ │
│  │  │         Load Balancing & Networking              │ │ │
│  │  │                                                    │ │ │
│  │  │  • Cloud Load Balancer (HTTPS)                    │ │ │
│  │  │  • Cloud Armor (DDoS protection)                  │ │ │
│  │  │  • Cloud NAT (outbound traffic)                   │ │ │
│  │  │  • Service Networking (private APIs)              │ │ │
│  │  └────────────────────────────────────────────────────┘ │ │
│  │                                                          │ │
│  │  ┌────────────────────────────────────────────────────┐ │ │
│  │  │      Monitoring & Observability                  │ │ │
│  │  │                                                    │ │ │
│  │  │  • Cloud Monitoring (metrics, dashboards)         │ │ │
│  │  │  • Cloud Logging (structured logs)                │ │ │
│  │  │  • Cloud Trace (distributed tracing)              │ │ │
│  │  │  • Error Reporting (exceptions)                   │ │ │
│  │  │                                                    │ │ │
│  │  │  Retention: 30 days (logs), unlimited (metrics)   │ │ │
│  │  └────────────────────────────────────────────────────┘ │ │
│  │                                                          │ │
│  └──────────────────────────────────────────────────────────┘ │
│                                                                │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │        IAM & Security                                   │ │
│  │                                                          │ │
│  │  • Service Account: autonomic-governor@project.iam.gserviceaccount.com
│  │    Roles:                                                │ │
│  │    - roles/run.invoker (for Cloud Run)                  │ │
│  │    - roles/pubsub.subscriber (for Pub/Sub)              │ │
│  │    - roles/pubsub.publisher (for Pub/Sub)               │ │
│  │    - roles/bigquery.dataEditor (for BigQuery)           │ │
│  │    - roles/datastore.user (for Firestore)               │ │
│  │    - roles/redis.client (for Memorystore)               │ │
│  │    - roles/compute.osLogin (for GKE)                    │ │
│  │                                                          │ │
│  │  • Secret Manager                                        │ │
│  │    - API keys, OAuth tokens, credentials               │ │
│  │                                                          │ │
│  │  • VPC Service Controls                                  │ │
│  │    - Perimeter for sensitive services                   │ │
│  │                                                          │ │
│  └──────────────────────────────────────────────────────────┘ │
│                                                                │
└────────────────────────────────────────────────────────────────┘
```

### Network Diagram

```
┌──────────────────────────────────────────────────────────────┐
│                    External (Public)                         │
│                                                              │
│   Cloud Operations Team (monitoring dashboards, alerts)     │
│                   │                                          │
│                   │ (HTTPS)                                  │
│                   ▼                                          │
│  ┌─────────────────────────────────────────────────────┐  │
│  │          Cloud Load Balancer + Armor              │  │
│  │  (Rate limiting, DDoS protection, SSL/TLS)        │  │
│  └────────────────────┬────────────────────────────────┘  │
│                       │                                    │
└──────────────────────┼────────────────────────────────────┘
                       │
┌──────────────────────▼────────────────────────────────────┐
│              Cloud Run Services (VPC)                      │
│                                                            │
│  Signal Ingest ←→ Governor ←→ Actuator ←→ Receipt Ledger│
│       │                                                   │
│       └──(Pub/Sub)──────────────────────────────────────┘│
│                                                            │
└────────────┬──────────────────────────────────┬───────────┘
             │                                  │
   ┌─────────▼──────────┐          ┌────────────▼──────────┐
   │   Firestore        │          │   BigQuery            │
   │  (State storage)   │          │ (Audit ledger)        │
   └────────────────────┘          └───────────────────────┘
```

---

## FSM State Machines

### Cost Circuit Breaker FSM

```
States:
  • Nominal: Cost within baseline + 20% threshold
  • Alert: Cost exceeded threshold
  • Throttled: Scaling limited to reduce costs
  • Recovery: Cost returning to normal

Transitions:
  ┌──────────────┐
  │   Nominal    │ (cost <= $120/day)
  │              │
  └──────┬───────┘
         │ (cost spike > $150/day)
         ▼
  ┌──────────────┐
  │   Alert      │ (notify operations)
  │              │
  └──────┬───────┘
         │ (execute throttle action)
         ▼
  ┌──────────────┐
  │  Throttled   │ (max_instances=2)
  │              │
  └──────┬───────┘
         │ (cost decreases < $130/day)
         ▼
  ┌──────────────┐
  │  Recovery    │ (gradual scale-up)
  │              │
  └──────┬───────┘
         │ (cost normalized)
         ▼
  ┌──────────────┐
  │   Nominal    │
  └──────────────┘

Signals:
  - BillingSpike(value: f64)
  - CostNormalized()

Actions:
  - Throttle { max_instances: 2 }
  - ScaleUp { max_instances: 10 }
  - Alert { message: String }

Guards:
  - cost_baseline: 100.0
  - alert_threshold: 150% (150.0)
  - recovery_threshold: 130% (130.0)
  - sample_window: 1 hour
```

### Deploy Rollback Guard FSM

```
States:
  • Monitoring: Tracking deployment, normal operation
  • Degrading: Error rate elevated but below threshold
  • Critical: Error rate exceeds threshold
  • RollingBack: Executing rollback
  • Stable: Rollback complete, system stable

Transitions:
  ┌──────────────┐
  │ Monitoring   │ (error_rate <= 2%)
  │              │
  └──────┬───────┘
         │ (error_rate > 2% && < 5%)
         ▼
  ┌──────────────┐
  │ Degrading    │ (notify operator)
  │              │
  └──────┬───────┘
         │ (error_rate > 5%)
         ▼
  ┌──────────────┐
  │ Critical     │ (alert!)
  │              │
  └──────┬───────┘
         │ (execute rollback)
         ▼
  ┌──────────────┐
  │ RollingBack  │ (delete new revision)
  │              │
  └──────┬───────┘
         │ (route traffic to stable)
         ▼
  ┌──────────────┐
  │ Stable       │ (error_rate <= 1%)
  │              │
  └──────┬───────┘
         │ (monitor next deploy)
         ▼
  ┌──────────────┐
  │ Monitoring   │
  └──────────────┘

Signals:
  - ErrorRateSpike(value: f64)
  - ErrorRateRecovered()
  - RevisionHealthy()

Actions:
  - Alert { message: String }
  - Rollback { service: String, target_revision: String }
  - VerifyZeroDowntime()

Guards:
  - error_rate_warning: 2%
  - error_rate_critical: 5%
  - error_rate_recovered: 1%
  - monitoring_window: 5 minutes
```

---

## Signal Processing Pipeline

### Data Flow

```
Input: Cloud Event (JSON)
  ├─ source: "logging.googleapis.com"
  ├─ type: "billing.threshold.exceeded"
  ├─ data:
  │   ├─ resource_id: "projects/123456"
  │   ├─ metric_name: "cloudapis.googleapis.com/library/books"
  │   └─ metric_value: 156 (percent of baseline)
  └─ timestamp: "2026-01-25T10:30:45Z"

↓ (Signal Ingest Service)

Processing:
  1. Parse JSON → SignalEnvelope
  2. Normalize:
     - Extract signal_type: "cost_spike"
     - Extract threshold: 150
     - Extract value: 156
     - Extract baseline: 100
  3. Deduplicate (Redis):
     - Key: hash(signal_type, resource_id, value)
     - TTL: 5 minutes
     - Skip if duplicate
  4. Rate-limit (Token Bucket):
     - Rate: 100 signals/sec per type
     - Burst: 10 signals
     - Reject if exceeded
  5. Validate:
     - Required fields: signal_type, value
     - Type check: value is f64
     - Range check: value >= 0
  6. Enrich (SPARQL):
     - Look up SKU triggered by signal_type
     - Determine governance rules
     - Calculate expected action

↓ (Pub/Sub: validated-signals topic)

Output: ValidatedSignal (JSON)
  ├─ signal_id: "sig-uuid-1234"
  ├─ signal_type: "cost_spike"
  ├─ normalized_value: 156
  ├─ baseline: 100
  ├─ threshold: 150
  ├─ triggered_sku: "cost-circuit-breaker-p1"
  ├─ expected_action: "scale_down"
  ├─ confidence: 0.95
  ├─ received_at: "2026-01-25T10:30:45Z"
  └─ source_trace_id: "trace-1234"

↓ (Governor Service)

Processing:
  1. Receive signal
  2. Load governor state from Firestore
  3. Evaluate FSM:
     - Current state: Nominal
     - Input signal: cost_spike
     - Guard conditions: cost > $150
     - Transition rule: Nominal → Alert
  4. Execute SPARQL query:
     - "SELECT ?action WHERE governor_state=Nominal AND signal=cost_spike"
     - Result: scale_down action
  5. Plan action:
     - Action: ScaleCloudRun { service="api", max_instances=2 }
     - Preconditions: service exists, current_instances > 2
     - Expected outcome: max_instances updated
  6. Update state:
     - New state: Alert
     - Reason: "Billing spike detected"
     - Timestamp: now()
     - Signal history: append signal

↓ (Pub/Sub: action-plans topic)

Output: ActionPlan (JSON)
  ├─ action_id: "act-uuid-5678"
  ├─ action_type: "scale_cloud_run"
  ├─ service: "api"
  ├─ min_instances: 1
  ├─ max_instances: 2
  ├─ reason: "Cost circuit breaker activated"
  ├─ triggered_by_signal: "sig-uuid-1234"
  ├─ governor_state_before: "Nominal"
  ├─ governor_state_after: "Throttled"
  └─ planned_at: "2026-01-25T10:30:46Z"

↓ (Actuator Service)

Processing:
  1. Receive action plan
  2. Route by action type → CloudRunHandler
  3. Verify preconditions:
     - Service "api" exists: ✓
     - Current max_instances: 10 (> 2): ✓
  4. Prepare GCP API call:
     - Service: Cloud Run
     - Method: UpdateService
     - Request:
        - service_name: "projects/123/locations/us-central1/services/api"
        - spec.template.spec.container_concurrency: 1000
        - spec.max_instance_request_concurrency: 100
  5. Execute with retries:
     - Attempt 1: Success
  6. Verify outcome:
     - GET service → max_instances: 2: ✓
     - Verify matches expected: ✓

↓ (Pub/Sub: action-results topic)

Output: ActionResult (JSON)
  ├─ action_id: "act-uuid-5678"
  ├─ status: "success"
  ├─ result:
  │   ├─ service: "api"
  │   ├─ max_instances_before: 10
  │   ├─ max_instances_after: 2
  │   └─ verification: "GET returned spec.max_instances == 2"
  ├─ duration_ms: 1234
  ├─ executed_at: "2026-01-25T10:30:47Z"
  └─ trace_id: "trace-1234"

↓ (Receipt Ledger Service)

Processing:
  1. Receive action result
  2. Generate receipt:
     - execution_id: "exec-uuid-9999"
     - action: "scale_cloud_run"
     - result: "success"
     - content: action_result (JSON)
     - content_hash: SHA256(json_serialize(action_result))
  3. Create audit trail entry:
     - Timestamp: now()
     - Action: scale_cloud_run
     - Service: api
     - Change: max_instances 10 → 2
     - Reason: cost_circuit_breaker activated
     - Operator: autonomic-governor (service account)
     - Approval: N/A (autonomous action)
  4. Insert into BigQuery:
     - Table: autonomic.receipt_ledger
     - Columns: execution_id, timestamp, action, result, receipt_hash, audit_trail_path
     - Mode: append-only (no updates/deletes)
  5. Archive to Cloud Storage:
     - Path: gs://autonomic-audit-trail/2026/01/25/exec-uuid-9999.json
     - Metadata: CRC32C checksum, creation time

↓

Final Receipt:
  execution_id: "exec-uuid-9999"
  timestamp: "2026-01-25T10:30:47Z"
  signal_chain:
    - signal_id: "sig-uuid-1234"
    - action_id: "act-uuid-5678"
  action: "scale_cloud_run"
  result: "success"
  receipt_hash: "sha256:d1b2c3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0"
  audit_trail_path: "gs://autonomic-audit-trail/2026/01/25/exec-uuid-9999.json"
  verification: {
    original_hash: "sha256:...",
    current_hash: "sha256:...",
    tamper_detected: false
  }
```

---

## Receipt Verification

### Cryptographic Proof Chain

```
┌─────────────────────────────────────────────────────────┐
│              Receipt Verification Flow                  │
│                                                         │
│  1. Action Execution                                  │
│     └─ action_result.json (unencrypted)              │
│        └─ SHA256 hash = H1                           │
│                                                       │
│  2. Receipt Generation                               │
│     ├─ Include action_result.json                    │
│     ├─ Include H1 in receipt                         │
│     ├─ Receipt = {                                   │
│     │   execution_id: UUID,                          │
│     │   timestamp: ISO8601,                          │
│     │   action: String,                              │
│     │   result: {action_result},                     │
│     │   content_hash: H1,                            │
│     │   receipt_chain: [previous_receipt_hash]       │
│     │ }                                              │
│     ├─ SHA256 hash receipt = H2                      │
│     └─ Store in BigQuery + GCS                       │
│                                                       │
│  3. Audit Trail                                      │
│     ├─ Immutable log (append-only)                   │
│     ├─ Each entry includes:                          │
│     │   - previous_receipt_hash (chain)              │
│     │   - current_receipt_hash (this entry)          │
│     │   - timestamp                                  │
│     │   - action details                            │
│     └─ Enable forensic analysis                      │
│                                                       │
│  4. Verification (Later)                             │
│     ├─ Retrieve receipt from BigQuery               │
│     ├─ Retrieve archived JSON from GCS               │
│     ├─ Compute hash = SHA256(archived_json)          │
│     ├─ Compare: computed_hash == receipt.content_hash│
│     │   - MATCH: ✓ Receipt authentic                │
│     │   - MISMATCH: ✗ TAMPER DETECTED               │
│     └─ Verify chain: current_hash == previous.next  │
│                                                       │
│  5. Forensic Analysis                                │
│     ├─ Timeline: all receipts ordered by timestamp   │
│     ├─ Causality: signal → action → result           │
│     ├─ Anomalies: unexpected actions, large gaps    │
│     ├─ Root cause: which signal triggered action    │
│     └─ Compliance: audit trail for regulations      │
│                                                       │
└─────────────────────────────────────────────────────────┘
```

### BigQuery Schema

```sql
CREATE TABLE autonomic.receipt_ledger (
  execution_id STRING NOT NULL,                    -- Unique ID
  timestamp TIMESTAMP NOT NULL,                    -- When executed
  signal_id STRING,                                -- Which signal triggered
  action_id STRING,                                -- Which action planned
  action STRING NOT NULL,                          -- Action type
  service STRING,                                  -- Service affected
  change_summary STRING,                           -- What changed
  result STRING NOT NULL,                          -- success|failure
  error_message STRING,                            -- If failure
  duration_ms INT64,                               -- Execution time
  content_hash STRING NOT NULL,                    -- SHA256(action_result)
  receipt_hash STRING NOT NULL,                    -- SHA256(entire_receipt)
  previous_receipt_hash STRING,                    -- Chain link
  audit_trail_path STRING,                         -- GCS path
  verification_status STRING,                      -- verified|tampered|unknown
  tags ARRAY<STRING>,                              -- Labels (cost_control, deploy, etc)
  metadata JSON NOT NULL                           -- Full action result
)
PARTITION BY DATE(timestamp)
CLUSTER BY action, service
OPTIONS (
  description = "Immutable audit trail for autonomic actions",
  require_partition_filter = true,
  partition_expiration_ms = 31536000000  -- 1 year
)
```

---

## MAPE-K Integration

### MAPE-K Loops Mapped to Services

```
┌────────────────────────────────────────────────────────┐
│         MAPE-K Autonomic Loop                          │
│                                                        │
│  Monitor (M)                                          │
│  └─ Cloud Monitoring, Logging, Trace                 │
│     └─ Emits metrics, logs, events to Pub/Sub        │
│                                                        │
│  Analyze (A)                                          │
│  └─ Signal Ingest Service                            │
│     ├─ Normalize signals (SPARQL enrichment)         │
│     ├─ Deduplicate                                   │
│     ├─ Validate                                      │
│     └─ Produce: ValidatedSignal                      │
│                                                        │
│  Plan (P)                                             │
│  └─ Governor Service (FSM)                           │
│     ├─ Load current state                            │
│     ├─ Evaluate transition rules (SPARQL)            │
│     ├─ Decide on action                              │
│     ├─ Update state                                  │
│     └─ Produce: ActionPlan                           │
│                                                        │
│  Execute (E)                                          │
│  └─ Actuator Service                                 │
│     ├─ Call GCP APIs (Cloud Run, GKE, etc)           │
│     ├─ Verify outcome                                │
│     └─ Produce: ActionResult                         │
│                                                        │
│  Knowledge (K)                                        │
│  └─ Receipt Ledger + RDF Ontology                    │
│     ├─ Cryptographic proof (BigQuery, GCS)           │
│     ├─ Audit trail (immutable log)                   │
│     ├─ Ontology (RDF rules, SKU definitions)         │
│     └─ Historical data (for learning)                │
│                                                        │
│  └─ [Loop back to Monitor]                           │
│                                                        │
└────────────────────────────────────────────────────────┘
```

### Knowledge Base (RDF Ontology)

```
Ontology Layers:
├─ c4.ttl (System architecture model)
│  ├─ Actors, Systems, Containers, Components
│  ├─ Data flows, relationships
│  └─ Deployment mappings
│
├─ erlang-autonomics.ttl (FSM + rules)
│  ├─ FSM states (Nominal, Alert, etc)
│  ├─ Signal definitions (cost_spike, error_rate, etc)
│  ├─ Action definitions (scale, rollback, etc)
│  ├─ Transition rules (state1 + signal → state2)
│  ├─ Guard conditions (thresholds, timers)
│  └─ Inference rules (OWL, derived facts)
│
├─ gcp-infrastructure.ttl (Cloud resources)
│  ├─ Cloud Run services
│  ├─ GKE clusters
│  ├─ Pub/Sub topics
│  ├─ Firestore collections
│  ├─ BigQuery datasets
│  └─ Network topology
│
└─ skus.ttl (Governor definitions)
   ├─ Cost Circuit Breaker
   │  ├─ Signal: cost_spike
   │  ├─ Action: scale_down
   │  └─ Constraints: min_cost, max_cost
   │
   ├─ Deploy Rollback Guard
   │  ├─ Signal: error_rate_spike
   │  ├─ Action: rollback
   │  └─ Constraints: error_threshold, monitoring_window
   │
   └─ ... (more SKUs)

SPARQL Queries (Examples):
├─ Decision Logic:
│  "For current state + signal, find next state and actions"
│
├─ Transition Rules:
│  "What states can we transition to from current state?"
│
├─ Guard Evaluation:
│  "Are preconditions met for this action?"
│
└─ Inference:
   "Derive implicit facts from OWL rules and current state"
```

---

## Performance Characteristics

### Latency Budget (End-to-End)

```
Signal from Cloud Event → Receipt in BigQuery
├─ Cloud Monitoring emission: 1-2s (GCP latency)
├─ Signal Ingest (normalize, deduplicate): 100-200ms
├─ Governor (FSM, SPARQL): 200-500ms
├─ Actuator (GCP API call, verify): 2-5s
├─ Receipt Ledger (BigQuery insert, archive): 200-500ms
│
└─ Total: ~4-8 seconds (P95)

SLO: <10s for end-to-end (P99)
```

### Throughput

```
Signal Ingest: 1,000 signals/sec (Cloud Run max)
Governor: 100 concurrent FSM evaluations
Actuator: 50 concurrent GCP API calls
Receipt Ledger: 500 BigQuery inserts/sec

Bottleneck: Actuator (GCP API rate limits)
Solution: Batching, async verification
```

### Resource Usage

```
Memory per service:
├─ Signal Ingest: 256MB
├─ Governor: 512MB (FSM state + SPARQL engine)
├─ Actuator: 256MB
└─ Receipt Ledger: 128MB

Storage:
├─ Firestore (governor state): <1GB
├─ BigQuery (receipts, 1 year): ~100GB
├─ GCS (audit archive): ~50GB
└─ Memorystore (dedup cache): 1GB (TTL-managed)
```

---

## Conclusion

The GCP Erlang Autonomics system provides a production-grade MAPE-K architecture for autonomous cloud governance, with:

1. **Specification-Driven** design (RDF ontologies → generated code)
2. **Deterministic** state machines (FSM transitions)
3. **Type-Safe** implementations (Rust, Result<T,E>)
4. **Cryptographic** audit trails (immutable receipts)
5. **Chicago TDD** testing (real objects, no mocks)

See related documentation:
- [README.md](README.md) — Project overview
- [QUICKSTART.md](QUICKSTART.md) — 5-minute setup
- [API_REFERENCE.md](API_REFERENCE.md) — Service APIs
- [GCP_SETUP.md](GCP_SETUP.md) — Infrastructure guide
- [FAQ.md](FAQ.md) — Common questions

---

**Last Updated**: January 2026 | **Architecture**: v1.0 | **Status**: Production-Ready ✓
