# GCP Erlang Autonomics: Self-Healing Cloud Governors

**One-liner**: Erlang autonomic governors for GCP with signal→action→receipt loops enabling zero-touch cloud self-healing.

**Version**: 0.1.0 | **Status**: Production-Ready | **License**: Apache-2.0

---

## Overview

**gcp-erlang-autonomics** is a specification-driven system that transforms RDF ontologies into deterministic autonomic governors on Google Cloud Platform. These governors continuously monitor cloud infrastructure, detect anomalies via signal processing, and execute corrective actions autonomously—all with cryptographic proof of execution.

### Core Equation

```
Governor = μ(Ontology)
```

Where μ is a five-stage deterministic pipeline:
- **μ₁ (Normalize)**: RDF validation + SHACL shape checking
- **μ₂ (Extract)**: SPARQL queries → structured signal definitions
- **μ₃ (Emit)**: Tera templates → Governor implementations
- **μ₄ (Canonicalize)**: Deterministic formatting + syntax validation
- **μ₅ (Receipt)**: Cryptographic audit trail generation

### Signals → Actions → Receipts

```
Cloud Events
     ↓
Signal Ingest (deduplicate, normalize)
     ↓
FSM Governor (analyze, decide)
     ↓
Actuator (execute action)
     ↓
Receipt Ledger (prove execution)
```

---

## Quick Start (60 seconds)

### 1. Prerequisites
- Rust 1.91.1+ (`rustc --version`)
- GCP project with Cloud Run + Pub/Sub enabled
- `ggen` CLI installed (`cargo install ggen-cli`)

### 2. Clone & Configure
```bash
cd examples/gcp-erlang-autonomics
export GCP_PROJECT_ID="your-project-123"
export GCP_REGION="us-central1"
```

### 3. Generate from Ontology
```bash
# Preview changes (no files written)
ggen sync --dry_run true

# Generate all artifacts with audit trail
ggen sync --audit true
```

### 4. Deploy to Cloud Run
```bash
# Build container
docker build -t gcr.io/$GCP_PROJECT_ID/autonomic-governor:latest .

# Push to Artifact Registry
docker push gcr.io/$GCP_PROJECT_ID/autonomic-governor:latest

# Deploy to Cloud Run
gcloud run deploy autonomic-governor \
  --image gcr.io/$GCP_PROJECT_ID/autonomic-governor:latest \
  --platform managed \
  --region $GCP_REGION \
  --memory 512Mi \
  --cpu 1
```

### 5. Monitor in BigQuery
```bash
# View audit receipts
bq query --use_legacy_sql=false '
  SELECT timestamp, action, result, receipt_hash
  FROM `'$GCP_PROJECT_ID'.autonomic.receipt_ledger`
  ORDER BY timestamp DESC
  LIMIT 10
'
```

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                   Cloud Events                          │
│  (Billing spikes, Error rates, Deployment failures)    │
└─────────────────────┬───────────────────────────────────┘
                      │
                      ▼
    ┌─────────────────────────────────┐
    │   Signal Ingest (Pub/Sub)       │
    │  • Normalize signals             │
    │  • Deduplicate events           │
    │  • Rate-limit ingestion         │
    └─────────────┬───────────────────┘
                  │
                  ▼
    ┌──────────────────────────────────────┐
    │  FSM Governor (Stateful Logic)      │
    │  • Monitor state machine            │
    │  • Analyze signals                  │
    │  • Plan corrective actions          │
    │  • Decide (allow/throttle/scale)   │
    └─────────────┬──────────────────────┘
                  │
                  ▼
    ┌──────────────────────────────────────┐
    │   Actuator (Cloud APIs)              │
    │  • Scale Cloud Run services         │
    │  • Rollback deployments            │
    │  • Apply rate limits               │
    │  • Send alerts                     │
    └─────────────┬──────────────────────┘
                  │
                  ▼
    ┌──────────────────────────────────────┐
    │  Receipt Ledger (BigQuery)           │
    │  • Emit cryptographic proofs        │
    │  • Log action outcomes              │
    │  • Build audit trail (immutable)    │
    │  • Enable forensic analysis         │
    └──────────────────────────────────────┘
```

---

## Architecture Diagrams

Complete C4 architecture diagrams (System Context, Containers, Components, Deployment) are generated from RDF ontology:

- **[C4 Level 1: System Context](../generated/c4-level1-context.mmd)** — Actors, external systems, data flows
- **[C4 Level 2: Containers](../generated/c4-level2-containers.mmd)** — Microservices, databases, message queues
- **[C4 Level 3: Components](../generated/c4-level3-components.mmd)** — Governor FSM states, transitions, internal logic
- **[C4 Level 4: Deployment](../generated/c4-level4-deployment.mmd)** — GCP infrastructure, GKE, Cloud Run, Pub/Sub

See [ARCHITECTURE.md](ARCHITECTURE.md) for detailed analysis.

---

## SKU Catalog

The system provides pre-built "SKUs" (Stock Keeping Units) — reusable autonomic governors for common cloud problems:

### 1. Cost Circuit Breaker (P1: Critical)
**Signal**: Billing spike (>150% baseline)
**Action**: Throttle services, trigger cost optimization
**Receipt**: Cost saved + actions taken logged

```
Cost Baseline: $100/day → Alert at $150/day → Throttle scaling
```

**Example**: See [cost_circuit_breaker_example.rs](../examples/cost_circuit_breaker_example.rs)

### 2. Deploy Rollback Guard (P1: Critical)
**Signal**: Error rate spike (>5% on new revision)
**Action**: Auto-rollback to previous stable version
**Receipt**: Rollback verified with zero-downtime

```
Deploy Revision → Error Rate Alert → Automatic Rollback → Zero-downtime Verified
```

**Example**: See [deploy_rollback_guard_example.rs](../examples/deploy_rollback_guard_example.rs)

### 3. Resource Deadlock Detector (P2: High)
**Signal**: Pod OOM kills, CPU throttling, memory pressure
**Action**: Scale up, trigger garbage collection
**Receipt**: Resource freed + scale action verified

### 4. Traffic Spike Mitigator (P2: High)
**Signal**: Request latency spike (>2x baseline)
**Action**: Auto-scale horizontally, enable CDN caching
**Receipt**: Latency normalized + new instances verified

### 5. Database Connection Pool Exhaustion (P2: High)
**Signal**: Connection wait time >5s
**Action**: Scale read replicas, apply connection limits
**Receipt**: Pool returned to healthy state

See [generated/sku-catalog.md](../generated/sku-catalog.md) for complete SKU definitions with pricing tiers.

---

## Key Features

### ✅ Specification-Driven (RDF-First)
- All governance logic defined in RDF ontologies (`.specify/specs/*.ttl`)
- SPARQL queries extract domain knowledge
- Tera templates generate idiomatic code
- Same spec → identical output every time (deterministic)

### ✅ MAPE-K Architecture
- **Monitor**: Signal ingest from Cloud Events, metrics
- **Analyze**: FSM state machine evaluates conditions
- **Plan**: SPARQL-driven decision logic
- **Execute**: Cloud APIs (Cloud Run, GKE, Pub/Sub)
- **Knowledge**: Cryptographic receipt ledger

### ✅ Chicago TDD (Real Objects, No Mocks)
- All examples tested with real GCP services (Firestore, BigQuery, Pub/Sub)
- Integration tests with testcontainers
- Property-based testing with proptest
- State-based verification (AAA pattern)

### ✅ Production-Ready
- Zero `unwrap()/expect()` in production code (all `Result<T,E>`)
- Timeout enforcement (SLO: <5s for signal processing)
- Poka-Yoke error prevention (SHACL validation pre-flight)
- Andon signals: compiler errors, test failures, warnings trigger auto-halt

### ✅ Cryptographic Proof (Immutable Receipts)
- Every action execution generates tamper-proof receipt
- SHA-256 content hashing for output validation
- Event sourcing log in BigQuery
- Forensic-ready audit trail

---

## File Organization

```
examples/gcp-erlang-autonomics/
├── docs/
│   ├── README.md                    # This file
│   ├── QUICKSTART.md                # 5-minute setup guide
│   ├── ARCHITECTURE.md              # Deep dive (C4, MAPE-K, FSM)
│   ├── API_REFERENCE.md             # Service APIs
│   ├── GCP_SETUP.md                 # Infrastructure guide
│   └── FAQ.md                       # Common questions
│
├── examples/
│   ├── cost_circuit_breaker_example.rs       # Example 1
│   └── deploy_rollback_guard_example.rs      # Example 2
│
├── .specify/                        # RDF SPECIFICATIONS (source of truth)
│   ├── specs/                       # Feature specifications
│   │   ├── 001-signal-ingest/
│   │   ├── 002-fsm-governor/
│   │   ├── 003-actuator/
│   │   └── 004-receipt-ledger/
│   └── ontologies/
│       ├── c4.ttl                   # C4 model ontology
│       ├── erlang-autonomics.ttl    # FSM + state definitions
│       ├── gcp-infrastructure.ttl   # GCP resources
│       └── skus.ttl                 # Governor SKU catalog
│
├── templates/                       # Tera templates → code generation
│   ├── c4-level1.tera              # System context diagram
│   ├── c4-level2.tera              # Containers diagram
│   ├── c4-level3.tera              # Components diagram
│   ├── c4-level4.tera              # Deployment diagram
│   ├── sku-catalog.tera            # SKU documentation
│   └── deployment-gke.tera         # Kubernetes manifests
│
├── generated/                       # Generated artifacts (DO NOT EDIT)
│   ├── c4-level1-context.mmd       # System context diagram
│   ├── c4-level2-containers.mmd    # Containers diagram
│   ├── c4-level3-components.mmd    # Components diagram
│   ├── c4-level4-deployment.mmd    # Deployment diagram
│   ├── sku-catalog.md              # SKU documentation
│   └── deployment-gke.yaml         # Kubernetes manifests
│
├── ggen.toml                        # Generation manifest
├── Dockerfile                       # Container image
└── Cargo.toml                       # Example crate (if applicable)
```

**Key Principle**: Edit `.specify/specs/*.ttl` (source of truth). Never edit generated files in `generated/` — they're regenerated by `ggen sync`.

---

## Technology Stack

| Component | Version | Purpose |
|-----------|---------|---------|
| **Rust** | 1.91.1+ | Core language (type-safe governors) |
| **Tokio** | 1.47+ | Async runtime (event processing) |
| **Oxigraph** | 0.5.1+ | RDF store + SPARQL engine |
| **Tera** | 1.20+ | Template engine (code generation) |
| **Serde** | 1.0+ | Serialization (JSON/YAML) |
| **google-cloud-bigquery** | Latest | Receipt ledger |
| **google-cloud-pubsub** | Latest | Signal ingest |
| **google-cloud-run** | API | Service orchestration |
| **proptest** | 1.8+ | Property-based testing |
| **testcontainers** | 0.25+ | Integration testing |

---

## Getting Started

### Option 1: 5-Minute Quick Start
→ [QUICKSTART.md](QUICKSTART.md)

### Option 2: Deploy to GCP
→ [GCP_SETUP.md](GCP_SETUP.md)

### Option 3: Understand Architecture
→ [ARCHITECTURE.md](ARCHITECTURE.md)

### Option 4: API Integration
→ [API_REFERENCE.md](API_REFERENCE.md)

### Option 5: Common Questions
→ [FAQ.md](FAQ.md)

---

## Example Projects

### Cost Circuit Breaker
Autonomically prevents cloud runaway costs:
```bash
cd examples/gcp-erlang-autonomics/examples
cargo run --example cost_circuit_breaker_example
```

**What happens**:
1. Initialize Cost Circuit Breaker SKU
2. Simulate billing spike signal
3. Observe governor throttle action
4. Verify receipt emitted to BigQuery

→ See [cost_circuit_breaker_example.rs](../examples/cost_circuit_breaker_example.rs) for code

### Deploy Rollback Guard
Autonomous zero-downtime rollback on deploy failures:
```bash
cd examples/gcp-erlang-autonomics/examples
cargo run --example deploy_rollback_guard_example
```

**What happens**:
1. Deploy new service revision
2. Monitor error rate (5% threshold)
3. Automatic rollback on error spike
4. Zero-downtime verified + receipt logged

→ See [deploy_rollback_guard_example.rs](../examples/deploy_rollback_guard_example.rs) for code

---

## Core Concepts

### Signal
An event from GCP infrastructure indicating a potential problem:
- Billing spike: `signal_type=cost_threshold, value=150, baseline=100`
- Error rate spike: `signal_type=error_rate, value=5.2, threshold=5.0`
- Deployment failure: `signal_type=revision_error_rate, value=8.5`

### Action
A corrective measure executed autonomously:
- Scale service: `action=scale_cloud_run, service=api, min_instances=2, max_instances=10`
- Rollback deployment: `action=rollback, service=api, target_revision=rev-5`
- Throttle traffic: `action=rate_limit, service=api, requests_per_second=1000`

### Receipt
Cryptographic proof of action execution:
```json
{
  "execution_id": "exec-uuid-1234",
  "timestamp": "2026-01-25T10:30:45Z",
  "action": "scale_cloud_run",
  "result": "success",
  "receipt_hash": "sha256:abc123...",
  "audit_trail": "s3://bucket/audits/2026-01-25.json"
}
```

### Governor (FSM)
A deterministic state machine that:
1. Listens for signals
2. Evaluates current state
3. Applies transition rules
4. Executes actions
5. Emits receipts

Example FSM for Cost Circuit Breaker:
```
[Nominal] ---(billing_spike)---> [Throttled]
   ^                                  |
   |----------(cost_normalized)-------+
```

---

## Production Checklist

Before deploying to production:

- [ ] All RDF specifications validated (SHACL checks pass)
- [ ] Generated code reviewed for type safety (zero `unwrap()` in production)
- [ ] Integration tests pass with real GCP services
- [ ] SLO targets met (signal processing <5s, action execution <10s)
- [ ] Receipt ledger schema created in BigQuery
- [ ] Cloud Run service configured with min/max instances
- [ ] Pub/Sub topics + subscriptions created
- [ ] IAM roles applied (minimal privilege principle)
- [ ] Secrets stored in Secret Manager (not in code/config)
- [ ] Monitoring + alerts configured in Cloud Monitoring
- [ ] Runbooks documented for on-call engineers

---

## Support & Resources

- **GitHub**: [ggen Repository](https://github.com/seanchatmangpt/ggen)
- **Issues**: [Report bugs or request features](https://github.com/seanchatmangpt/ggen/issues)
- **Documentation**: [ggen Docs](https://docs.ggen.io)
- **Community**: [ggen Discussions](https://github.com/seanchatmangpt/ggen/discussions)

---

## License

Apache License 2.0 — See LICENSE file for details.

---

**Last Updated**: January 2026 | **ggen Version**: 6.0.0+ | **Status**: Production-Ready ✓
