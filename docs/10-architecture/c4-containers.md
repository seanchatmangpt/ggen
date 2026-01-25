# C4 Level 2: Container Architecture - Cloud Run, Pub/Sub, Firestore

**Document Purpose**: Define the deployment containers and runtime infrastructure for the Autonomic Reconciliation Engine on GCP.

**Version**: 1.0 | **Date**: 2026-01-25

---

## Container Context: Bounded Systems

The autonomic reconciliation engine is decomposed into 4 **containers** running on GCP:

| Container | Service | Purpose | Storage |
|-----------|---------|---------|---------|
| **Governor Service** | Cloud Run | gen_statem FSM, policy evaluation, decision routing | Memory (state), Firestore (receipts) |
| **Ingress Sidecar** | Cloud Run | HTTP server, JWT verification, signal aggregation | Cloud Pub/Sub (buffering) |
| **Evidence Sidecar** | Cloud Run (sidecar) | Receipt generation, hash-chain building, Firestore writes | Firestore (region-local ledger) |
| **Catalog Controller** | Cloud Run (Job) | Policy pack deployment, schema validation, global sync | Cloud Storage (policy artifacts) |

---

## C4 Diagram: Container Architecture

```mermaid
C4Container
    title Autonomic Reconciliation Engine - Container Deployment (GCP)

    Container(ingress, "Ingress Sidecar", "Cloud Run", "HTTP server (Cowboy), JWT verifier, rate limiter (Erlang token bucket), signal aggregator")
    Container(governor, "Governor Service", "Cloud Run", "gen_statem FSM (state machine), policy evaluator (SPARQL), action router, intervention handler")
    Container(evidence, "Evidence Sidecar", "Cloud Run", "Receipt generator, hash-chain builder, Firestore writer, Cloud Logging emitter")
    Container(controller, "Catalog Controller", "Cloud Run Job", "Policy pack loader, SHACL validator, ontology deployer, global sync coordinator")

    ContainerDb(firestore, "Firestore Ledger", "Firestore", "Region-local receipts (document store), hash-chain indices (composite), audit log (streaming export)")
    ContainerDb(pubsub, "Pub/Sub Topics", "Cloud Pub/Sub", "signal-ingest (push subscription), action-emit (pull subscription), evidence-stream (fanout to logging)")
    ContainerDb(gcs, "Policy Artifacts", "Cloud Storage", "policy-pack/*.ttl (RDF ontologies), governance-schemas/*.shacl (shape constraints), execution-logs/*.json (audit)")

    Rel(ingress, pubsub, "Publish: aggregated signals → signal-ingest topic")
    Rel(pubsub, governor, "Consume: signal-ingest (push subscription)")
    Rel(governor, evidence, "Emit: action result (local message queue)")
    Rel(evidence, firestore, "Write: receipt document (Firestore collection)")
    Rel(evidence, pubsub, "Publish: evidence-stream (fanout to logging)")
    Rel(governor, pubsub, "Publish: action-emit (remediator subscription)")
    Rel(controller, gcs, "Deploy: policy-pack.ttl, governance.shacl")
    Rel(governor, gcs, "Load: current policy-pack.ttl (cached)")
    Rel(firestore, pubsub, "Export: Firestore streaming export → Cloud Logging")

    UpdateElementStyle(governor, $bgColor=#FF6B6B, $fontColor=#FFFFFF, $borderColor=#C92A2A)
    UpdateElementStyle(evidence, $bgColor=#45B7D1, $fontColor=#FFFFFF)
    UpdateElementStyle(firestore, $bgColor=#FFD700, $fontColor=#000000)
    UpdateElementStyle(pubsub, $bgColor=#4ECDC4, $fontColor=#FFFFFF)
```

---

## Container Definitions

### 1. Ingress Sidecar (Cloud Run)

**Responsibility**: HTTP entry point, authentication, signal aggregation, rate limiting

**Components**:
- **Cowboy HTTP Server**: Lightweight Erlang HTTP framework
  - Port 8080: Listens for signal webhooks
  - Validates JWT entitlement tokens (issuer: GCP Marketplace)
  - Rejects if token outside SKU scope or expired

- **JWT Verifier**: OAuth2 token validation
  - Extracts claims: `iss`, `sub`, `aud`, `scope`, `iat`, `exp`
  - Verifies signature against GCP public keys (cached)
  - Enriches signal with principal context (who triggered this?)

- **Rate Limiter**: Token bucket (Erlang `rate_limiter` module)
  - Per-principal quota (e.g., 100 signals/minute per SKU)
  - Storm detection: If >50 signals/10s, trigger postponement
  - Jidoka enforcement: Refuse signals beyond quota (respond 429)

- **Signal Aggregator**: Dedupplication + windowing
  - Dedup by (signal_type, resource_id, timestamp_bucket)
  - Window: 5s aggregation window (configurable per policy)
  - Batches deduplicated signals → Pub/Sub publish

**Data Flow**:
```
HTTP POST /signal
  ↓ (JWT validation)
  ↓ (Rate limit check)
  ↓ (Deduplication)
  ↓ (Aggregation window)
Pub/Sub: signal-ingest topic
```

**SLO**: P99 latency <100ms, P99.9 availability 99.99%

---

### 2. Governor Service (Cloud Run)

**Responsibility**: Policy evaluation (gen_statem FSM), decision making, action routing

**Components**:
- **gen_statem FSM**: Finite state machine (Erlang OTP pattern)
  - States: idle → evaluating → deciding → executing → emitting → idle
  - Event handlers per state (signals cause transitions)
  - Deterministic transitions (same signal + state → same output)

- **Policy Evaluator**: SPARQL query executor
  - Receives aggregated signal batch
  - Evaluates policy rules (loaded from GCS policy-pack.ttl)
  - Generates decision (action_type, parameters, capability_required)

- **Invariant Checker**: Type safety + business rules
  - Verifies action within entitlement scope
  - Checks resource availability (quota, limits)
  - Prevents contradictory actions (e.g., can't update and delete simultaneously)

- **Action Router**: Capability enforcement
  - Maps (action_type, parameters) → remediator capability
  - Checks if action allowed by policy AND entitlement
  - Refuses action if policy veto or scope violation (receipt: "refused")

- **Intervention Handler**: Human consent flow
  - Some actions require "warn → human approval → execute" flow
  - Blocks execution until approval received (Pub/Sub wait)
  - On approval: Continues to evidence sidecar
  - On timeout/rejection: Cancels action (receipt: "cancelled")

**FSM Diagram**:
```
┌─────────┐
│  idle   │ ← Start state
└────┬────┘
     │ signal_arrived
     ↓
┌──────────────┐
│  evaluating  │ ← Policy rules evaluated
└────┬─────────┘
     │ policy_result
     ↓
┌──────────────┐
│  deciding    │ ← Determine action + consent requirement
└────┬─────────┘
     │ decision_made
     ↓
┌──────────────────────────┐
│   consent_required?      │
└────┬─────────────────────┘
     │
     ├─ No → deciding
     │
     └─ Yes → ┌──────────────────┐
              │ await_consent    │ ← Wait for human approval
              └────┬─────────────┘
                   │ consent_received
                   ↓
           ┌──────────────┐
           │  executing   │ ← Execute remediator action
           └────┬─────────┘
                │ action_result
                ↓
           ┌──────────────┐
           │  emitting    │ ← Emit receipt
           └────┬─────────┘
                │ receipt_sent
                ↓
           ┌─────────┐
           │  idle   │ ← Ready for next signal
           └─────────┘
```

**SLO**: P95 decision latency <2s, P99 <5s

---

### 3. Evidence Sidecar (Cloud Run sidecar process)

**Responsibility**: Receipt generation, hash-chain building, immutable ledger writes

**Components**:
- **Receipt Generator**: Structure action outcome
  - Timestamp: ISO 8601 UTC
  - Action ID: UUID4 (generated by Governor)
  - Signal trace: List of signal IDs that triggered action
  - Decision path: Policy rules evaluated + results
  - Action executed: Type and parameters
  - Result: success | failure | refused | postponed
  - Entitlement context: Token hash, principal, scope

- **Hash-Chain Builder**: Link to prior receipt
  - Loads previous receipt from Firestore
  - Computes `previous_receipt_hash = SHA256(json.stringify(prev_receipt))`
  - Sets `hash_chain_link = previous_receipt_hash` in current receipt
  - Forms immutable Merkle-linked chain

- **Signature Generator**: Cryptographic proof
  - Serializes receipt (canonical JSON format)
  - Signs with Ed25519 private key (stored in Secret Manager)
  - Signature proves: receipt was generated by system, not tampered after

- **Firestore Writer**: Persist receipt
  - Collection: `evidence/{region}/receipts`
  - Document ID: `action-{action_id}`
  - Fields: All receipt JSON fields + Firestore metadata
  - Indexes: Composite index on (timestamp, action_type, result)

- **Cloud Logging Emitter**: Audit trail mirror
  - Publishes receipt to Pub/Sub `evidence-stream` topic
  - Pub/Sub → Cloud Logging export (via subscription)
  - GCS archive (Cloud Logging long-term storage)

**Receipt JSON Format**:
```json
{
  "action_id": "uuid-2026-01-25-001",
  "region": "us-central1",
  "timestamp": "2026-01-25T14:32:45.123Z",
  "signal_trace": [
    "sig-monitoring-spike-001",
    "sig-alert-threshold-exceeded"
  ],
  "decision_path": [
    {
      "rule": "policy/security/threat-detection",
      "evaluated": true,
      "result": true
    },
    {
      "rule": "policy/scope/within-sku",
      "evaluated": true,
      "result": true
    }
  ],
  "action_executed": "iam_binding_update",
  "action_parameters": {
    "binding_update": "remove_service_account_from_editor_role",
    "resource": "projects/gov-buyer-project-123"
  },
  "result": "success",
  "entitlement_context": {
    "token_hash": "sha256(jwt_payload)",
    "principal": "service-autonomics@gov-buyer-project-123.iam.gserviceaccount.com",
    "sku_scope": "security-autonomics-premium"
  },
  "hash_chain_link": "sha256(previous_receipt_json)",
  "signature": "ed25519-hex-encoded-signature",
  "intervention_type": "none",
  "intervention_result": null
}
```

**SLO**: Receipt generation <50ms, Firestore write ≤1s

---

### 4. Catalog Controller (Cloud Run Job)

**Responsibility**: Policy pack deployment, schema validation, global coordination

**Components**:
- **Policy Pack Loader**: Fetch from GCS, validate, deploy to all regions
  - Reads `policy-pack/{version}.ttl` from GCS
  - Parses Turtle RDF (using Oxigraph)
  - Extracts governance rules, schema constraints
  - Validates against SHACL shapes (governance.shacl)
  - Deploys to all region-local Cloud Run instances (via Pub/Sub announcement)

- **SHACL Validator**: Ensure schema conformance
  - Validates policy-pack RDF against governance.shacl shapes
  - Checks: Required properties, type constraints, cardinality
  - Rejects policy if validation fails (Andon signal)
  - Logs validation errors to Cloud Logging

- **Ontology Deployer**: Version control + rollback
  - Stores policy version metadata in Firestore
  - Maintains version history (for rollback if needed)
  - Synchronizes policy to all Governors (cache invalidation signal)
  - Emits policy-deployed event to Pub/Sub (announces new version)

- **Global Sync Coordinator**: Cross-region consistency
  - Monitors Firestore replication lag
  - Waits for all regions to acknowledge new policy before marking "deployed"
  - Falls back to last-known-good if deployment fails in any region
  - Logs sync status to Cloud Logging

**Deployment Flow**:
```
1. Admin uploads new policy-pack.ttl to GCS
2. Catalog Controller (Cloud Run Job) triggered on GCS event
3. Controller validates policy (SHACL shapes)
4. Controller deploys to region-local Governors:
   a. Pub/Sub policy-updated announcement
   b. Each Governor: Invalidates cache, reloads policy
5. Waits for all regions to ack
6. Marks policy as "deployed" in Firestore version table
7. Logs success/failure to Cloud Logging
```

**SLO**: Policy deployment latency <5s per region, eventual consistency <30s

---

## Data Storage Strategy

### Firestore Ledger (Region-Local)

**Collection Structure**:
```
evidence/{region}/receipts/{action_id}
  - action_id (String, document ID)
  - timestamp (Timestamp)
  - signal_trace (Array[String])
  - decision_path (Array[Struct])
  - action_executed (String)
  - result (String)
  - hash_chain_link (String)
  - entitlement_context (Struct)
  - signature (String)

Indexes:
  - (region, timestamp) ASC → Enable "all receipts on date X"
  - (region, result) → "all failures", "all refusals"
  - (region, action_executed) → "all IAM updates", etc.
```

**Replication Strategy**:
- Region-local writes: Cloud Run (us-central1) writes to regional Firestore instance
- Global replication: Firestore standard enables multi-region reads
- Eventual consistency: Typically <5s cross-region latency
- Fallback: If regional Firestore unavailable, queue to Cloud Storage (local cache)

### Cloud Pub/Sub Topics

**signal-ingest**:
- Push subscription → Governor service
- Message schema: `{timestamp, signal_type, resource_id, severity, enriched_context}`
- TTL: 7 days (messages auto-deleted)
- DLQ: Dead-letter topic for failed deliveries

**action-emit**:
- Pull subscription by remediator service
- Message schema: `{action_id, action_type, parameters, resource_ids}`
- Delivery guarantee: At-least-once (remediator must be idempotent)
- Ack timeout: 60s

**evidence-stream**:
- Fanout subscription → Cloud Logging export
- Message schema: `{receipt}` (full receipt JSON)
- TTL: 1 day (short; logged immediately to Cloud Logging)
- Archive: Cloud Logging → Cloud Storage (long-term retention)

**policy-updated**:
- Pub/Sub announcement: New policy version deployed
- Message schema: `{policy_version, deployment_timestamp, regions_affected}`
- Broadcast to all Governors (fan-out pattern)

### Cloud Storage (Policy Artifacts)

**policy-pack/{version}.ttl**:
- Version-controlled RDF ontologies
- Immutable (new version = new file, never overwrite)
- Path: `gs://autonomics-policy-bucket/policy-pack/v2.1.3.ttl`
- Retention: 7 years (TAI 2030 compliance)

**governance.shacl**:
- SHACL shapes (RDF Schema constraints)
- Defines valid policy structures
- Updated when governance rules change

**execution-logs/{date}.json**:
- Compressed JSON lines (one receipt per line)
- Created nightly (Cloud Logging → Cloud Storage export)
- Retention: 7 years (cryptographic evidence archive)

---

## Glossary Cross-Reference

- **gen_statem**: Erlang OTP finite state machine for deterministic governor transitions
- **Cloud Run**: Serverless container orchestration (stateless, auto-scaling)
- **Cloud Pub/Sub**: Managed message broker (push/pull subscriptions, fanout)
- **Firestore**: Managed document database (region-local, global replication available)
- **Evidence Plane**: Separate ledger (Firestore + Cloud Logging) for immutable action records
- **Jidoka**: Stop-the-line enforcement; refuse action if receipt generation fails
- **Hash-Chain**: Merkle-linked receipts; each receipt includes SHA256 of prior receipt

---

## Receipt Contract (Evidence Plane)

**Storage Guarantee**: Every action generates a receipt immediately after execution.

**Receipt Lifecycle**:
1. **Generation** (Evidence Sidecar): Action executed → Receipt structure created
2. **Hashing** (Hash-Chain Builder): Previous receipt loaded → Hash computed → Linked
3. **Signing** (Signature Generator): Receipt serialized → Ed25519 signature computed
4. **Persistence** (Firestore Writer): Receipt document written to `evidence/{region}/receipts`
5. **Mirroring** (Cloud Logging Emitter): Receipt published to `evidence-stream` topic
6. **Archival** (Cloud Logging Export): Receipt exported to Cloud Storage nightly

**Verification Path** (Auditor):
```
Auditor Query: "Show all actions on 2026-01-25"
  ↓
SPARQL SELECT: SELECT ?action_id ?result FROM evidence WHERE timestamp = "2026-01-25"
  ↓ (Query Firestore)
Firestore composite index (timestamp): Returns 347 receipts
  ↓
Receipt Verifier: Load each receipt, rebuild hash-chain, verify signatures
  ↓
Result: Timeline of 347 actions, verified tamper-free
```

---

## Definition of Done

- [ ] C4 container diagram renders in Mermaid (4 containers, 4 databases, 8 relationships)
- [ ] Ingress Sidecar: HTTP server, JWT verifier, rate limiter, aggregator all documented
- [ ] Governor Service: gen_statem FSM diagram shows all 8 states and transitions
- [ ] Evidence Sidecar: Receipt JSON schema defined with all required fields
- [ ] Catalog Controller: Policy deployment flow documented
- [ ] Firestore schema: Collection structure and indexes specified
- [ ] Pub/Sub topics: All 4 topics defined with schemas and retention
- [ ] Hash-chain linking: Merkle-linked proof mechanism explained
- [ ] Regional consistency: Firestore replication strategy shown
- [ ] Jidoka enforcement: Receipt generation mandatory before action completion
- [ ] SLOs for all 4 containers documented (latency, availability targets)

---

**Next**: See [c4-components.md](c4-components.md) for Level 3 component architecture (inside Governor service).
