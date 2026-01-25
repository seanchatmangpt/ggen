# Autonomic Reconciliation Engine - C4 Architecture Documentation

**Purpose**: Government-buyer-focused C4 architecture diagrams (Levels 1-5) for the autonomic reconciliation engine running on GCP Marketplace.

**Document Suite Version**: 1.0 | **Date**: 2026-01-25

---

## Architecture Levels

### [Level 1: C4 Context](c4-context.md) - System Perspective

**For**: Government CTOs, CISOs, PMs understanding the whole system

**Contains**:
- System context diagram (5 personas, 6 systems, 11 relationships)
- 5 government pain points + autonomic solutions
- TAI 2030 compliance alignment (sections 1.0, 2.3, 3.2, 4.1, 5.0)
- Signal → Governor → Action → Receipt flow
- Regional consistency model
- Jidoka enforcement rules
- Entitlement drift, intervention audit, storm handling receipts

**Key Diagrams**: 1 C4Context diagram | **Key Sections**: 5 pain points, data flow, receipt examples

---

### [Level 2: C4 Containers](c4-containers.md) - Deployment Architecture

**For**: DevOps engineers, SREs deploying on GCP

**Contains**:
- 4 containers: Ingress Sidecar, Governor Service, Evidence Sidecar, Catalog Controller
- Cloud Run, Pub/Sub, Firestore, Cloud Storage integration
- gen_statem FSM, policy evaluation, intervention handling
- Receipt generation + hash-chain building
- Regional receipt replication strategy
- Policy pack deployment workflow

**Key Diagrams**: 1 C4Container diagram | **Key Sections**: Container definitions, data storage strategy, Pub/Sub topics

---

### [Level 3: C4 Components](c4-components.md) - Governor Service Internals

**For**: Backend engineers implementing the autonomic engine

**Contains**:
- 6 core components: HTTP ingress, JSON decoder, rate limiter, gen_statem FSM, policy evaluator, action router
- Receipt emission, metrics collection, OpenTelemetry tracing
- Happy path, failure path, intervention path flows
- gen_statem FSM states (idle, evaluating, deciding, executing, emitting)
- SPARQL policy evaluation examples
- Invariant checking, jidoka enforcement
- Prometheus metrics + optional OTEL tracing

**Key Diagrams**: 4 C4Component diagrams (ingress, FSM, intervention, full sequence) | **Key Sections**: Component interactions, FSM states, performance latencies

---

### [Level 4: C4 Runtime](c4-runtime.md) - Behavior & Sequences

**For**: QA engineers, architects designing test strategies

**Contains**:
- 4 runtime sequences:
  1. Normal execution (happy path)
  2. Intervention (warn → human approval → execute)
  3. Storm handling (rate limit, postponement, bounded actions)
  4. Refusal (policy block, scope violation)
- gen_statem FSM state diagram (6 states)
- Event handlers (deterministic state transitions)
- Sequence diagrams with timelines
- Receipt structure for each sequence type
- Storm detection logic (threshold, postponement, drop reasons)
- Intervention consent flow (timeout, approval method)
- Refusal types (5 categories)

**Key Diagrams**: 4 sequence diagrams + 1 FSM state diagram | **Key Sections**: Timelines, receipt examples, SLOs

---

### [Level 5: Evidence Plane](evidence-plane.md) - Immutable Audit Trail

**For**: Compliance officers, auditors, security teams

**Contains**:
- Three-plane architecture: Control (ephemeral), Data (mutable), Evidence (immutable)
- Receipt generation, hash-chain building, signature validation
- Firestore ledger schema + Cloud Logging mirror
- SPARQL evidence queries (3 examples)
- Receipt verification algorithm (signature + hash-chain validation)
- Jidoka enforcement: "No receipt, no action" rule
- Tamper detection, timeline reconstruction
- GCS archive strategy (7-year retention)

**Key Diagrams**: 1 evidence plane architecture diagram | **Key Sections**: Receipt structure, verification algorithm, SPARQL queries, jidoka enforcement

---

## Quick Navigation

### By Role

| Role | Start Here | Then Read |
|------|-----------|-----------|
| **Government CIO/CISO** | [Level 1: Context](c4-context.md) | [Level 5: Evidence Plane](evidence-plane.md) |
| **DevOps Engineer** | [Level 2: Containers](c4-containers.md) | [Level 3: Components](c4-components.md) |
| **Backend Engineer** | [Level 3: Components](c4-components.md) | [Level 2: Containers](c4-containers.md) |
| **QA Engineer** | [Level 4: Runtime](c4-runtime.md) | [Level 3: Components](c4-components.md) |
| **Compliance Officer** | [Level 5: Evidence Plane](evidence-plane.md) | [Level 1: Context](c4-context.md) |
| **Security Auditor** | [Level 5: Evidence Plane](evidence-plane.md) | [Level 4: Runtime](c4-runtime.md) |

### By Topic

| Topic | Primary | Secondary |
|-------|---------|-----------|
| **Entitlement Management** | [Level 1](c4-context.md#pain-point-1-entitlement-drift) | [Level 5](evidence-plane.md#receipt-contract-evidence-plane) |
| **Policy Evaluation** | [Level 3](c4-components.md#policy-evaluator) | [Level 4](c4-runtime.md#happy-path-all-checks-pass) |
| **Storm Handling** | [Level 4](c4-runtime.md#sequence-diagram-3-storm-handling) | [Level 2](c4-containers.md#rate-limiter-token-bucket) |
| **Human Intervention** | [Level 4](c4-runtime.md#sequence-diagram-2-intervention) | [Level 3](c4-components.md#intervention-handler) |
| **Receipt Generation** | [Level 5](evidence-plane.md#receipt-generator-evidence-sidecar) | [Level 2](c4-containers.md#3-evidence-sidecar-cloud-run-sidecar-process) |
| **Hash-Chain Proof** | [Level 5](evidence-plane.md#2-hash-chain-builder-merkle-linked-proof) | [Level 5](evidence-plane.md#receipt-verification-algorithm-auditor) |
| **SPARQL Queries** | [Level 5](evidence-plane.md#sparql-evidence-queries) | [Level 3](c4-components.md#sparql-executor) |
| **Regional Consistency** | [Level 1](c4-context.md#pain-point-5-regional-compliance-fragmentation) | [Level 2](c4-containers.md#firestore-ledger-region-local) |

---

## Key Concepts

### Government Pain Points (5)

From [Level 1: Context](c4-context.md#government-pain-points--solutions):

1. **Entitlement Drift** → Solved by: Jidoka + Receipt embedding JWT hash
2. **Slow Compliance Verification** → Solved by: SPARQL evidence queries + Firestore mirror
3. **Missing Intervention Audit** → Solved by: Receipt captures consent decision + timeline
4. **Storm Mitigation Opaqueness** → Solved by: Storm receipt shows signal batching, drops, reasons
5. **Regional Compliance Fragmentation** → Solved by: Deterministic FSM + Policy pack versioning + Firestore replication

### Core Architectural Principles

From [Level 5: Evidence Plane](evidence-plane.md#separation-principle-no-receipt-no-action):

- **No Receipt, No Action** (Jidoka): If receipt generation fails, action is refused
- **Immutable Evidence Plane**: Append-only ledger (Firestore + Cloud Logging)
- **Hash-Chain Proof**: Each receipt links to prior (Merkle-linked, tamper-proof)
- **SPARQL Queries**: Reconstruct timeline from RDF receipts (TAI 2030 compliant)
- **Cryptographic Receipts**: Ed25519 signatures prove authenticity
- **Separated Planes**: Control (ephemeral), Data (mutable), Evidence (immutable)

### Government Compliance (TAI 2030)

From [Level 1: Context](c4-context.md#government-requirements):

- **Section 1.0**: Autonomic governance + jidoka enforcement
- **Section 2.3**: Regional compliance consistency (deterministic rules)
- **Section 3.2**: Intervention transparency (consent + decision path)
- **Section 4.1**: Real-time evidence (cryptographic receipts)
- **Section 5.0**: Audit-ready ledger (queryable timeline)

---

## Glossary Cross-References

All documents cross-reference the [project glossary](../../sync-patterns/src/glossary.md) for terminology:

- **gen_statem**: Erlang OTP finite state machine
- **Policy Pack**: RDF ontology defining governance rules
- **Receipt**: Cryptographically signed action proof
- **Hash-Chain**: Merkle-linked receipts (immutable proof)
- **Jidoka**: Stop-the-line enforcement (fail-safe)
- **Evidence Plane**: Immutable audit trail (Firestore + Cloud Logging)
- **SPARQL**: RDF query language (SELECT, CONSTRUCT)
- **Firestore**: Managed document database (region-local ledger)
- **Cloud Logging**: Managed audit trail (tamper-proof mirror)

---

## File Organization

```
docs/10-architecture/
├── README.md                    ← You are here
├── c4-context.md               ← Level 1: Government buyer perspective
├── c4-containers.md            ← Level 2: GCP deployment architecture
├── c4-components.md            ← Level 3: Governor service internals
├── c4-runtime.md               ← Level 4: Runtime sequences & FSM behavior
├── evidence-plane.md           ← Level 5: Evidence ledger & verification
└── security-architecture.md    ← Supplementary: Security threat model
```

---

## Document Statistics

| File | Lines | Size | Diagrams | Key Sections |
|------|-------|------|----------|--------------|
| c4-context.md | 369 | 17K | 1 C4Context | 5 pain points, data flow, TAI 2030 alignment |
| c4-containers.md | 430 | 17K | 1 C4Container | 4 containers, Pub/Sub topics, Firestore schema |
| c4-components.md | 411 | 18K | 4 C4Component | 6 components, gen_statem FSM, SPARQL evaluation |
| c4-runtime.md | 450 | 20K | 5 diagrams | 4 sequences, FSM states, performance SLOs |
| evidence-plane.md | 659 | 22K | 1 architecture | Receipt structure, SPARQL queries, verification |
| **Total** | **2,319** | **94K** | **12 diagrams** | **50+ sections** |

---

## How to Use This Documentation

### For System Design

1. Start with [Level 1: Context](c4-context.md) — understand government requirements
2. Review [Level 5: Evidence Plane](evidence-plane.md) — understand immutable ledger
3. Then [Level 2: Containers](c4-containers.md) — understand deployment
4. Finally [Level 3: Components](c4-components.md) + [Level 4: Runtime](c4-runtime.md) — understand implementation

### For Implementation

1. Start with [Level 2: Containers](c4-containers.md) — understand GCP resources
2. Deep-dive [Level 3: Components](c4-components.md) — implement each component
3. Reference [Level 4: Runtime](c4-runtime.md) — verify behavior against sequences
4. Validate against [Level 5: Evidence Plane](evidence-plane.md) — ensure receipt generation works

### For Auditing/Compliance

1. Start with [Level 5: Evidence Plane](evidence-plane.md) — understand proof mechanism
2. Review [Level 4: Runtime](c4-runtime.md) — understand action sequences
3. Check [Level 1: Context](c4-context.md) — verify pain points addressed
4. Validate SPARQL queries work against Firestore data

### For Testing/QA

1. Start with [Level 4: Runtime](c4-runtime.md) — understand all 4 sequences
2. Reference [Level 3: Components](c4-components.md) — understand component interactions
3. Use performance SLOs from [Level 4](c4-runtime.md#performance-requirements) — validate timing
4. Check jidoka enforcement from [Level 5](evidence-plane.md#jidoka-enforcement) — verify fail-safety

---

## Definition of Done (Entire Suite)

All 5 documents complete with:

- [ ] ✅ 12 Mermaid diagrams, all renderable
- [ ] ✅ 5 government pain points documented + solutions
- [ ] ✅ TAI 2030 compliance sections mapped (1.0, 2.3, 3.2, 4.1, 5.0)
- [ ] ✅ gen_statem FSM: 6 states, deterministic transitions
- [ ] ✅ 4 runtime sequences: normal, intervention, storm, refusal
- [ ] ✅ Receipt structure: JSON schema defined
- [ ] ✅ Hash-chain: Merkle linking explained
- [ ] ✅ SPARQL queries: 3 evidence query examples
- [ ] ✅ Jidoka enforcement: "No receipt, no action" rule
- [ ] ✅ Firestore schema: Collections, fields, indexes
- [ ] ✅ Cloud Logging mirror: Tamper-proof backup
- [ ] ✅ Performance SLOs: All latencies specified
- [ ] ✅ Verification algorithm: Signature + hash-chain validation
- [ ] ✅ Glossary cross-references: All technical terms defined

---

## Next Steps

1. **Implementation**: Start with [Level 2: Containers](c4-containers.md) to understand GCP deployment
2. **Testing**: Use [Level 4: Runtime](c4-runtime.md) sequences to write test cases
3. **Validation**: Implement verification algorithm from [Level 5: Evidence Plane](evidence-plane.md)
4. **Deployment**: Follow container definitions in [Level 2](c4-containers.md) for production setup
5. **Compliance**: Run SPARQL queries from [Level 5](evidence-plane.md) for audit proof

---

**Created**: 2026-01-25 | **Version**: 1.0 (Government-Focused C4 Suite)

See also: [docs/c4/](../c4/) for broader context (46-diagram universe).
