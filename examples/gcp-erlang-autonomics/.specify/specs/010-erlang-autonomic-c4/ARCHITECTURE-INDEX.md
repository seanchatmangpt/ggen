# Erlang Autonomic C4 Architecture - Complete Index

## Quick Navigation

### Core Architecture Files (in reading order)

| Level | File | Description | Triples | Key Entities |
|-------|------|-------------|---------|--------------|
| **1** | `c4-system.ttl` | System context & actors | ~50 | System boundary, CTO, Marketplace, GCP Platform |
| **2** | `c4-containers.ttl` | Services & data sources | ~200 | 5 services, 8 data sources, 5 protocols |
| **3** | `c4-components.ttl` | Governor Engine internals | ~300 | 6 components, 5-state FSM, RETE engine |
| **4** | `c4-deployment.ttl` | GCP infrastructure | ~400 | Cloud Run, BigQuery, Cloud Spanner, multi-region |
| **Market** | `sku-mapping.ttl` | Product definitions | ~350 | 3 SKUs, 12 policies, pricing, SLOs |
| **Validation** | `shapes.ttl` | SHACL constraints | ~200 | 20 validation shapes |
| **Ontology** | `ontology.ttl` | RDF vocabulary | ~600 | 60+ classes, 100+ properties |

---

## Architecture Overview

### C4 Level 1: System Context
```
┌─────────────────────────────────────────────────────────────┐
│                    Autonomic Governor System                │
│                                                              │
│  Manages self-healing, self-scaling, self-optimizing cloud  │
│  infrastructure through intelligent governance policies     │
└─────────────────────────────────────────────────────────────┘
         ▲                                              │
         │ install/configure                           │ observe/actuate
         │                                              ▼
    ┌────┴────┐                                   ┌──────────┐
    │   CTO   │                                   │    GCP   │
    │         │                                   │ Platform │
    └────┬────┘                                   └──────────┘
         │ pay
         ▼
    ┌──────────────┐
    │  Marketplace │
    │   (publish   │
    │     SKUs)    │
    └──────────────┘
```

### C4 Level 2: Container Architecture
```
┌─────────────────────────────────────────────────────────────────────┐
│                                                                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐               │
│  │   Signal     │  │   Governor   │  │   Actuator   │               │
│  │  Ingestion   │─→│    Engine    │─→│   Service    │─→ GCP         │
│  │ (Cloud Run)  │  │ (Cloud Run)  │  │ (Cloud Run)  │               │
│  └──────────────┘  └──────────────┘  └──────────────┘               │
│         ▲               │                                            │
│         │               ▼                                            │
│         │          ┌──────────────┐                                  │
│         │          │ Invariant    │                                  │
│         │          │  Checker     │                                  │
│         │          └──────────────┘                                  │
│         │                                                            │
│         └────────────────────────────────────────────┐               │
│                                                      ▼               │
│                                               ┌──────────────┐      │
│                                               │   Receipt    │      │
│         ┌────────────────────────────────────→│   Ledger     │      │
│         │                                     │ (Cloud Run)  │      │
│         │                                     └──────────────┘      │
│         │                                            │               │
│    ┌────┴──────────────┐                           ▼               │
│    │  Cloud Pub/Sub    │                      ┌──────────────┐      │
│    │  Topics/Topics    │                      │  BigQuery    │      │
│    │  (messaging)      │                      │   (ledger)   │      │
│    └───────────────────┘                      └──────────────┘      │
│                                                                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐               │
│  │ Entitlement  │  │  Firestore   │  │ Cloud SQL    │               │
│  │  Manager     │  │  (policies)  │  │  (analytics) │               │
│  └──────────────┘  └──────────────┘  └──────────────┘               │
│                                                                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐               │
│  │ Cloud Spanner│  │   Redis      │  │    Cloud     │               │
│  │  (state)     │  │   (cache)    │  │  Storage     │               │
│  └──────────────┘  └──────────────┘  └──────────────┘               │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### C4 Level 3: Governor Engine Components
```
┌─────────────────────────────────────────────────────────────┐
│         Governor Engine (gen_statem FSM)                    │
│                                                              │
│  ┌──────────────┐                                           │
│  │Event Router  │ (demultiplexes signals by tenant)        │
│  └──────┬───────┘                                           │
│         │                                                   │
│  ┌──────▼──────────────────────────────────┐               │
│  │ Tenant Registry (per-tenant state)      │               │
│  └──────┬─────────────────────────────────┬┘                │
│         │                                 │                │
│  ┌──────▼──────┐          ┌──────────────┴──────┐           │
│  │gen_statem   │          │ Invariant Checker   │           │
│  │  FSM (5x)   │──────────│ (pre/post-action)   │           │
│  └──────┬──────┘          └─────────────────────┘           │
│         │                                                   │
│         ▼                                                   │
│  ┌─────────────────────────────────────────┐               │
│  │  Planner (RETE + Policy Evaluation)    │               │
│  │  - Match rules against state            │               │
│  │  - Generate action plans                │               │
│  │  - Document rationale                   │               │
│  └──────┬──────────────────────────────────┘               │
│         │                                                   │
│         ▼                                                   │
│  ┌─────────────────────────────────────────┐               │
│  │  Receipt Emitter (Crypto Signing)       │               │
│  │  - SHA-256 hashing                      │               │
│  │  - Immutable record                     │               │
│  │  - Audit trail                          │               │
│  └─────────────────────────────────────────┘               │
│                                                              │
└─────────────────────────────────────────────────────────────┘

FSM States: stable → warn → intervene → degrade → refuse
Latency: <5s signal-to-receipt
Throughput: 10k+ signals/sec
```

### C4 Level 4: GCP Deployment (Multi-Region HA)
```
┌──────────────────────────────────────────────────────────────────┐
│ Primary Region (us-central1)    │ Secondary Region (us-east1)   │
│                                 │                                │
│ ┌───────────────────────────┐   │ ┌───────────────────────────┐  │
│ │  Cloud Run Services (5)   │   │ │ Read-only Replicas        │  │
│ │  - Signal Ingest          │   │ │ (Failover)                │  │
│ │  - Governor Engine        │   │ │                           │  │
│ │  - Actuator               │   │ │ Auto-promote on           │  │
│ │  - Entitlement Mgr        │   │ │ primary failure           │  │
│ │  - Receipt Ledger         │   │ │                           │  │
│ └───────────────────────────┘   │ └───────────────────────────┘  │
│                                 │                                │
│ ┌───────────────────────────┐   │ ┌───────────────────────────┐  │
│ │ Persistent Data (Cloud    │   │ │ Replicated Data          │  │
│ │ Spanner, Firestore)       │   │ │ (Spanner replicas,       │  │
│ │                           │   │ │  Firestore multi-region) │  │
│ └───────────────────────────┘   │ └───────────────────────────┘  │
│                                 │                                │
│ ┌───────────────────────────┐   │ ┌───────────────────────────┐  │
│ │ Analytics & Archive       │   │ │ Archive Mirror            │  │
│ │ - BigQuery (US multi-reg) │   │ │ - GCS versioning          │  │
│ │ - Cloud Storage           │   │ │ - Backup exports          │  │
│ └───────────────────────────┘   │ └───────────────────────────┘  │
│                                 │                                │
└──────────────────────────────────────────────────────────────────┘
         │                            │
         └────────────────────────────┘
              Cloud Pub/Sub
         (global event streaming)
```

---

## Key Entities & Relationships

### Services (Container Level)

| Service | Technology | Replicas | Latency | Throughput |
|---------|-----------|----------|---------|-----------|
| Signal Ingest | Erlang + Cloud Run | 3-100 | <100ms | 10k+ sig/sec |
| Governor Engine | Erlang + gen_statem | 5-50 | <5s | 10k+ sig/sec |
| Actuator | Erlang + gRPC | 2-20 | <300s | 100+ actions/sec |
| Entitlement | Erlang + Redis | 3-50 | <1ms | 200+ concurrent |
| Receipt Ledger | Erlang + BigQuery | 3-30 | <300s | 1k+ receipts/sec |

### Data Sources (Persistent Layer)

| Source | Type | Purpose | Consistency |
|--------|------|---------|-------------|
| Firestore | Document DB | Policy definitions, tenant config | Strong eventual |
| Cloud Spanner | Relational | Governor state, transactions | Strong ACID |
| Redis Cache | In-memory | Quota cache, sessions | Ephemeral |
| Cloud SQL | Relational | Analytics, audit logs | Eventually consistent |
| BigQuery | Data warehouse | Receipt ledger, analytics | Append-only |
| Cloud Storage | Object store | Policy backups, archives | Versioned |

### FSM State Machine (Component Level)

```
        ┌─────────┐
    ┌───┤ STABLE  ├───┐
    │   └────┬────┘   │
    │        │        │
    │    threshold    │
    │   exceeded      │
    │        │        │
    │        ▼        │
    │   ┌────────┐    │
    └──→│  WARN  │◄───┘
        └───┬────┘
            │
        breached
            │
            ▼
        ┌──────────┐
        │INTERVENE │
        └───┬──────┘
            │
       failed mitigation
            │
            ▼
        ┌────────┐
        │ DEGRADE│
        └───┬────┘
            │
        critical
            │
            ▼
        ┌────────┐
        │ REFUSE │
        └────────┘
```

**Transitions**:
- **stable → warn**: Threshold exceeded, alert admin
- **warn → intervene**: Threshold breached, execute mitigation
- **intervene → degrade**: Intervention failed, reduce load
- **degrade → refuse**: System critical, deny new requests
- **refuse → degrade**: Manual recovery initiated
- **degrade → intervene**: Load normalized, restore services
- **intervene → warn**: Metrics improving, continue monitoring
- **warn → stable**: All healthy, resume normal

---

## SKU Catalog

### SKU 1: Cost Circuit Breaker (CCB-001)
```
Governance: Cost Optimizer
Signals: Daily cost, budget limits, idle resources, commitments
Actions: Scale down, degrade features, throttle deployments
Pricing: $499/month + $0.00001/signal
SLO: <5s latency, 99.5% uptime
Guarantee: 15% cost reduction or partial refund
```

### SKU 2: Deploy Rollback Guard (DRG-002)
```
Governance: Deployment Health Monitor
Signals: Error rates, latency, health checks, canary traffic
Actions: Pause rollout, auto-rollback, lock deployments
Pricing: $799/month + $50/deployment
SLO: <2m rollback latency, 99.9% uptime
Guarantee: 40% incident reduction or refund
```

### SKU 3: Backlog Pressure Valve (BPV-003)
```
Governance: Queue Health Regulator
Signals: Queue depth, growth rate, consumer health, priority
Actions: Load shed, throttle, priority queuing, circuit break
Pricing: $399/month + $0.00005/message
SLO: <500ms p99, 99.5% uptime
Guarantee: 99.9% cascade prevention confidence
```

---

## Policy Examples

### Policy 1: Monitor Daily Cost (Cost Circuit Breaker)
```
Trigger: Daily 00:00 UTC
Condition: Fetch GCP billing data for current month
Action: IF projected_monthly_cost > budget_limit THEN transition(warn)
Rationale: Early warning when spending exceeds budget
```

### Policy 2: Auto-Rollback on Error (Deploy Rollback Guard)
```
Trigger: Every 10 seconds during rollout
Condition: error_rate_delta > 5.0% AND latency_p99_delta > 20%
Action: transition(intervene) AND trigger_rollback() AND alert_team()
Rationale: Automatically rollback clearly broken deployments
```

### Policy 3: Load Shedding (Backlog Pressure Valve)
```
Trigger: Per message when queue at limit
Condition: queue_depth >= max AND message.priority < HIGH
Action: transition(degrade) AND shed_low_priority_messages()
Rationale: Prevent complete system failure by dropping non-critical work
```

---

## Validation Summary

### SHACL Shapes Validation

| Shape | Type | Rules | Coverage |
|-------|------|-------|----------|
| System Context | Completeness | 3 properties | C4 Level 1 |
| Actor | Completeness | 2 properties | All actors |
| Relationship | Completeness | 4 properties | All interactions |
| Container | Completeness | 3 properties | All services |
| Component | Completeness | 2 properties | All components |
| DataSource | Metadata | 2 properties | All data sources |
| FSM State | Completeness | 2 properties | All states |
| SKU | Completeness | 8 properties | All SKUs |
| Policy | Completeness | 4 properties | All policies |
| Database | Completeness | 3 properties | All databases |

### Deterministic Triples Validation
- ✅ No random UUIDs (all semantic URLs)
- ✅ All identifiers deterministic
- ✅ Reproducible across runs
- ✅ Same input → Same output

---

## File Statistics

| File | Size | Triples | Classes | Properties | Lines |
|------|------|---------|---------|-----------|-------|
| c4-system.ttl | 8.8K | ~50 | 2 | 8 | 200 |
| c4-containers.ttl | 12K | ~200 | 8 | 25 | 280 |
| c4-components.ttl | 14K | ~300 | 7 | 30 | 320 |
| c4-deployment.ttl | 19K | ~400 | 15 | 40 | 450 |
| sku-mapping.ttl | 17K | ~350 | 5 | 20 | 380 |
| shapes.ttl | 18K | ~200 | 20 | - | 420 |
| ontology.ttl | 28K | ~600 | 60+ | 100+ | 650 |
| **TOTAL** | **128K** | **2,100+** | **60+** | **100+** | **2,700** |

---

## Integration Points

### Code Generation (ggen sync)
1. **SPARQL Queries** → Extract specific entities
2. **Tera Templates** → Transform to code/config
3. **Deterministic Hashing** → SHA-256 of output
4. **Receipt Generation** → Audit trail per sync

### Documentation Generation
1. **Parse TTL** → Extract triples
2. **Render Markdown** → Architecture docs
3. **Generate Diagrams** → C4 visual representation
4. **Track Changes** → Git history of specs

### Compliance Reporting
1. **SHACL Validation** → Quality gates
2. **Audit Trail** → All decisions
3. **Coverage Report** → What's documented
4. **Gap Analysis** → Missing entities

---

## Related Files

```
examples/gcp-erlang-autonomics/
├── .specify/specs/010-erlang-autonomic-c4/   (← YOU ARE HERE)
│   ├── c4-system.ttl
│   ├── c4-containers.ttl
│   ├── c4-components.ttl
│   ├── c4-deployment.ttl
│   ├── sku-mapping.ttl
│   ├── shapes.ttl
│   ├── ontology.ttl
│   ├── README.md (detailed guide)
│   └── ARCHITECTURE-INDEX.md (this file)
├── .specify/templates/        (rendering templates)
├── templates/                 (code generation)
└── ggen.toml                  (project config)
```

---

## Next Steps

1. **Validate**: `cargo make speckit-validate`
2. **Render**: `cargo make speckit-render` (generates markdown)
3. **Query**: Use SPARQL to explore the ontology
4. **Extend**: Add domain-specific components
5. **Generate**: Use ggen sync to create code/config

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-01-25 | Initial release with 7 TTL files, 60+ classes, 100+ properties |

---

**Status**: Production-Ready
**Maintainer**: ggen-autonomics@ggen.org
**License**: Apache 2.0 (same as ggen project)

