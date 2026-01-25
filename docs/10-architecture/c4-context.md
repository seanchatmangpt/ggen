<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [C4 Level 1: System Context - Government Autonomic Reconciliation](#c4-level-1-system-context---government-autonomic-reconciliation)
  - [Executive Context: The Government Buyer Problem](#executive-context-the-government-buyer-problem)
  - [C4 Diagram: Government Buyer Context](#c4-diagram-government-buyer-context)
  - [Context Narrative: TAI 2030 Alignment](#context-narrative-tai-2030-alignment)
    - [Key Government Requirements](#key-government-requirements)
  - [Data Flow: Signal → Governor → Action → Receipt](#data-flow-signal-%E2%86%92-governor-%E2%86%92-action-%E2%86%92-receipt)
  - [Government Pain Points & Solutions](#government-pain-points--solutions)
    - [Pain Point 1: Entitlement Drift](#pain-point-1-entitlement-drift)
    - [Pain Point 2: Slow Compliance Verification](#pain-point-2-slow-compliance-verification)
    - [Pain Point 3: Missing Intervention Audit](#pain-point-3-missing-intervention-audit)
    - [Pain Point 4: Storm Mitigation Opaqueness](#pain-point-4-storm-mitigation-opaqueness)
    - [Pain Point 5: Regional Compliance Fragmentation](#pain-point-5-regional-compliance-fragmentation)
  - [Glossary Cross-Reference](#glossary-cross-reference)
  - [Receipt Contract (Evidence Plane)](#receipt-contract-evidence-plane)
  - [Definition of Done](#definition-of-done)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# C4 Level 1: System Context - Government Autonomic Reconciliation

**Document Purpose**: Define the government buyer perspective for the Autonomic Reconciliation Engine (ggen-powered governance) within TAI 2030 compliance and GCP Marketplace distribution.

**Version**: 1.0 (Government-Focused) | **Date**: 2026-01-25

---

## Executive Context: The Government Buyer Problem

The ggen autonomic reconciliation engine solves **5 critical government pain points**:

| Pain Point | Current State | Autonomic Solution |
|------------|---------------|-------------------|
| **Entitlement Drift** | Manual audit trails, human error, compliance gaps | Real-time receipt emission, cryptographic proof, "no receipt = no action" rule (jidoka) |
| **Slow Compliance Verification** | 60-90 day audit cycles, paper trails | SPARQL-based evidence queries, instant timeline reconstruction from Firestore ledger |
| **Missing Intervention Audit** | No record of who authorized what | Every intervention (warn → attempt → result) generates signed receipt with decision path |
| **Storm Mitigation Opaqueness** | Signal postponement loses context | Hash-chain receipts preserve signal sequence, bounded action proof, storm handling log |
| **Regional Compliance Fragmentation** | Separate systems per region, no global consistency | Region-local receipt generation + global Firestore replication + unified verification |

---

## C4 Diagram: Government Buyer Context

```mermaid
C4Context
    title TAI 2030 Autonomic Reconciliation - Government Buyer Perspective

    Person(gov_cio, "Government CIO", "Enterprise cloud governance, TAI 2030 compliance")
    Person(gov_ciso, "Government CISO", "Security posture, incident response, audit readiness")
    Person(gov_pm, "Program Manager", "SKU entitlement tracking, cost optimization, SLA enforcement")
    Person(auditor, "External Auditor", "3rd-party compliance verification, receipt validation")

    System(ggen_autonomics, "Autonomic Reconciliation Engine", "Signal→Governor→Action→Receipt (deterministic, auditable)")
    System(gcp_marketplace, "GCP Marketplace", "SKU provisioning, entitlement tokens, billing integration")
    System(gcp_infra, "GCP Infrastructure", "Cloud Run, Pub/Sub, Firestore, Cloud Logging, Cloud Trace")

    System_Ext(signal_sources, "Signal Sources", "Monitoring (Cloud Monitoring), Logs (Cloud Logging), Alerts (GCP native)")
    System_Ext(evidence_ledger, "Evidence Ledger", "Firestore (region-local) + Cloud Logging (audit mirror)")
    System_Ext(remediation_actuators, "Actuators", "IAM bindings, Firewall rules, resource limits, quotas")

    Rel(gov_cio, gcp_marketplace, "Purchase: autonomic governance SKU")
    Rel(gov_ciso, ggen_autonomics, "Audit: intervention log, storm handling, consent flow")
    Rel(gov_pm, ggen_autonomics, "Monitor: entitlement compliance, receipt counts, action latency")
    Rel(auditor, evidence_ledger, "Verify: cryptographic proof, event timeline, tamper-free ledger")

    Rel(gcp_marketplace, ggen_autonomics, "Issue: entitlement token (JWT)")
    Rel(ggen_autonomics, gcp_infra, "Execute: governor FSM, policy evaluation, action routing")
    Rel(signal_sources, ggen_autonomics, "Emit: all signals (filtered by policy pack)")
    Rel(ggen_autonomics, remediation_actuators, "Execute: stop-the-line actions (jidoka enforcement)")
    Rel(ggen_autonomics, evidence_ledger, "Emit: signed receipt + hash-chain proof")
    Rel(auditor, ggen_autonomics, "Query: SPARQL evidence extraction, timeline reconstruction")

    UpdateElementStyle(ggen_autonomics, $bgColor=#FF6B6B, $fontColor=#FFFFFF, $borderColor=#C92A2A)
    UpdateElementStyle(evidence_ledger, $bgColor=#45B7D1, $fontColor=#FFFFFF)
    UpdateElementStyle(remediation_actuators, $bgColor=#96CEB4, $fontColor=#FFFFFF)
    UpdateElementStyle(signal_sources, $bgColor=#4ECDC4, $fontColor=#FFFFFF)
```

---

## Context Narrative: TAI 2030 Alignment

### Key Government Requirements

**1. Real-Time Compliance Evidence** (TAI 2030 Section 4.1)
- Every autonomic action must generate a **cryptographic receipt** with:
  - Action ID, timestamp (ISO 8601), signal trace
  - Governance policy reference (RDF ontology hash)
  - Authorization context (entitlement token, principal)
  - Result (success/failure), side effects

**2. Intervention Transparency** (TAI 2030 Section 3.2 - Consent)
- Government must prove every intervention followed consent rules:
  - **Warn path**: System emits alert, human can refuse → receipt shows "refused"
  - **Action path**: System executes decision → receipt shows "executed" with authorization
  - **Storm path**: Multiple signals trigger postponement → receipt shows "postponed + reason"

**3. Audit-Ready Ledger** (TAI 2030 Section 5.0)
- Evidence must be **queryable by arbitrary timeline** (e.g., "All actions on 2026-01-15"):
  - Firestore ledger holds region-local receipts (Cloud Run sidecar)
  - Cloud Logging mirrors audit events (tailing to GCS archive)
  - SPARQL queries reconstruct decision tree (evidence plane)
  - Hash-chain validation proves no tampering

**4. Regional Compliance Consistency** (TAI 2030 Section 2.3)
- Same autonomic rules must apply across US regions:
  - gen_statem FSM is region-agnostic (deterministic state transitions)
  - Policy pack loaded from GCP Marketplace (same for all regions)
  - Firestore replication ensures eventual consistency of evidence
  - Receipt timestamps are canonical (UTC, cryptographically signed)

**5. Jidoka Enforcement** (TAI 2030 Section 1.0 - Autonomic Governance)
- System must stop automatically on policy violation:
  - "No receipt, no action" rule: If receipt generation fails, action is refused
  - Entitlement boundary: Autonomic actions only within SKU scope
  - Policy boundary: Governor refuses actions outside policy pack
  - Evidence boundary: All state changes auditable via Firestore hash-chain

---

## Data Flow: Signal → Governor → Action → Receipt

```
┌─────────────────────────────────────────────────────────────────┐
│ Government Buyer Autonomic Reconciliation Flow                  │
└─────────────────────────────────────────────────────────────────┘

1. ENTITLEMENT (GCP Marketplace)
   Buyer purchase SKU → Marketplace issues JWT token → Cloud Run sidecar validates

2. SIGNAL INGEST (Cloud Monitoring, Cloud Logging, Alerts)
   Signal sources emit events → Pub/Sub aggregator → Policy-based filter

3. GOVERNOR EVALUATION (gen_statem FSM + Policy Pack)
   Signal arrival triggers state transition → Evaluate policy rules → Determine action

4. ACTION EXECUTION (Remediator Actuator)
   Governor output → Capability router → Execute remediation
   Examples: Update IAM, change firewall rule, enforce quota

5. RECEIPT GENERATION (Evidence Plane)
   Action result → Hash-chain builder → Firestore write → Cloud Logging mirror
   Receipt contains: action ID, signal trace, decision path, authorization context

6. AUDIT QUERY (SPARQL Evidence Extractor)
   Auditor query: "Show all actions on date X" → SPARQL SELECT + reconstruction → Timeline
```

---

## Government Pain Points & Solutions

### Pain Point 1: Entitlement Drift

**Problem**: How do we prove the system only took actions within the purchased SKU scope?

**Traditional Approach**:
- Manual audit logs (Excel spreadsheets, error-prone)
- Post-facto investigation (60-90 days)
- No cryptographic proof of authorization

**Autonomic Solution**:
- **Jidoka Boundary Enforcement**: Each receipt includes entitlement token (JWT) embedded in hash
- **Firestore Hash-Chain**: Every receipt links to prior receipt, forming immutable ledger
- **SPARQL Evidence Query**: "Show all actions outside my SKU scope" → Instant false alarms

**Receipt Format** (Evidence Plane):
```json
{
  "action_id": "recv-2026-01-25-001",
  "timestamp": "2026-01-25T14:32:45.123Z",
  "entitlement_token_hash": "sha256(jwt-payload)",
  "signal_trace": ["signal_1", "signal_2"],
  "decision_path": [
    {"rule": "policy_pack/security/threat_detect", "result": true},
    {"rule": "policy_pack/scope/within_sku", "result": true}
  ],
  "action_executed": "iam_binding_update",
  "result": "success",
  "hash_chain_link": "sha256(prev_receipt)",
  "signature": "ed25519(...)"
}
```

---

### Pain Point 2: Slow Compliance Verification

**Problem**: How do we instantly verify compliance posture without manual investigation?

**Traditional Approach**:
- Compliance team runs scripts to audit logs
- Results reported 30-60 days later
- No way to verify data wasn't tampered with

**Autonomic Solution**:
- **Receipt Verifier (SPARQL Backend)**: Query Firestore → Rebuild hash-chain → Prove integrity
- **Cloud Logging Mirror**: Audit events streamed to GCS archive (tamper-proof)
- **Instant Timeline**: "Show all remediation actions in Q4" → SPARQL SELECT → Results in <1s

**Evidence Plane Query Example**:
```sparql
PREFIX receipt: <http://autonomics.ggen/receipt/>
PREFIX evidence: <http://autonomics.ggen/evidence/>

SELECT ?action_id ?timestamp ?policy_rule ?result
WHERE {
  ?receipt a receipt:AutonomicReceipt ;
    receipt:actionId ?action_id ;
    receipt:timestamp ?timestamp ;
    receipt:decisionPath ?decision ;
    receipt:result ?result .
  ?decision evidence:evaluatedRule ?policy_rule ;
    evidence:ruleResult true .
  FILTER(?timestamp >= "2025-10-01T00:00:00Z"^^xsd:dateTime)
  FILTER(?timestamp <= "2025-12-31T23:59:59Z"^^xsd:dateTime)
}
ORDER BY ?timestamp
```

---

### Pain Point 3: Missing Intervention Audit

**Problem**: When humans must approve autonomic actions, how do we record that decision?

**Traditional Approach**:
- Slack message (not auditable)
- Ticket system (no cryptographic proof)
- Email chain (loses context)

**Autonomic Solution**:
- **Intervention Receipt**: When human says "warn → action", receipt captures full chain
- **Decision Path Embedding**: Policy rule that required consent + actual consent decision
- **No Hands Requirement**: System enforces consent automatically, no human touch after approval

**Intervention Receipt Example**:
```json
{
  "action_id": "recv-2026-01-25-storm-001",
  "intervention_type": "warn_then_action",
  "signal_trace": ["signal_1", "signal_2", "signal_3"],
  "policy_decision": "storm_detected",
  "required_consent": true,
  "consent_method": "jidoka_human_approval",
  "consent_result": "approved_by_oncall_engineer",
  "consent_timestamp": "2026-01-25T14:35:00Z",
  "action_executed_after_consent": "firewall_rule_update",
  "action_result": "success",
  "action_timestamp": "2026-01-25T14:35:02Z",
  "signature": "ed25519(...)"
}
```

---

### Pain Point 4: Storm Mitigation Opaqueness

**Problem**: When signals arrive too fast (storm), how do we prove the system didn't lose context?

**Traditional Approach**:
- System drops signals silently (data loss)
- No record of what was postponed
- Compliance team can't explain why action was delayed

**Autonomic Solution**:
- **Signal Postpone Receipt**: When storm triggers postponement, receipt captures full signal sequence
- **Bounded Action Proof**: Receipt shows which signals were batched, which were dropped, why
- **Hash-Chain Storm Log**: Each postponement linked to prior receipts, forming audit trail

**Storm Receipt Example**:
```json
{
  "action_id": "recv-2026-01-25-storm-batch-001",
  "event_type": "storm_mitigation",
  "signal_count": 47,
  "signals_batched": 35,
  "signals_dropped": 12,
  "drop_reason": "bounded_action_limit_100_per_minute",
  "postponement_duration_ms": 5000,
  "reprocessing_timestamp": "2026-01-25T14:36:00Z",
  "final_action_executed": "rate_limit_enforcement",
  "storm_log": [
    {"signal_id": "sig_001", "action": "batched"},
    {"signal_id": "sig_002", "action": "batched"},
    {"signal_id": "sig_051", "action": "dropped", "reason": "quota_exceeded"}
  ],
  "signature": "ed25519(...)"
}
```

---

### Pain Point 5: Regional Compliance Fragmentation

**Problem**: How do we prove the same rules applied in us-central1, us-east1, and us-west1?

**Traditional Approach**:
- Separate systems per region (impossible to compare)
- Different audit trails (hard to correlate)
- No global compliance view

**Autonomic Solution**:
- **Deterministic gen_statem FSM**: Same state machine runs in all regions
- **Policy Pack Versioning**: Same policy pack (RDF ontology) deployed globally
- **Firestore Replication**: Region-local receipts replicated to global Firestore, queryable by timestamp
- **Unified Evidence Plane**: SPARQL queries cross regions (eventual consistency)

**Regional Consistency Model**:
```
┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐
│  us-central1     │  │  us-east1        │  │  us-west1        │
│  ─────────────   │  │  ────────────    │  │  ─────────────   │
│  Cloud Run       │  │  Cloud Run       │  │  Cloud Run       │
│  (Autonomics)    │  │  (Autonomics)    │  │  (Autonomics)    │
│  ↓ Firestore     │  │  ↓ Firestore     │  │  ↓ Firestore     │
│  (region-local)  │  │  (region-local)  │  │  (region-local)  │
└────────┬─────────┘  └────────┬─────────┘  └────────┬─────────┘
         │                     │                     │
         └─────────────────────┼─────────────────────┘
                               ↓
                    Firestore Global Replication
                               ↓
                    ┌──────────────────────┐
                    │ Global Evidence      │
                    │ Ledger (Firestore)   │
                    │ + Cloud Logging      │
                    │ Archive (GCS)        │
                    └──────────────────────┘
                               ↓
                    SPARQL Evidence Extractor
                    (Instant cross-region queries)
```

---

## Glossary Cross-Reference

This context diagram uses terminology defined in [glossary.md](../../sync-patterns/src/glossary.md):

- **gen_statem**: Finite state machine (Erlang OTP pattern) for deterministic governor transitions
- **Policy Pack**: RDF ontology (`.ttl`) defining governance rules, constraints, capabilities
- **Entitlement Token**: JWT issued by GCP Marketplace, embedded in every receipt
- **Hash-Chain**: Linked receipts where each includes hash of previous (Merkle-linked proof)
- **Jidoka Enforcement**: Stop-the-line principle; system halts on policy violation or evidence failure
- **Evidence Plane**: Separate ledger (Firestore + Cloud Logging) for immutable action records
- **Receipt**: Cryptographically signed record of autonomic action, decision path, and result

---

## Receipt Contract (Evidence Plane)

Every system interaction generates a **Receipt** — cryptographically signed proof of action.

**Receipt Structure** (JSON):
- `action_id` (UUID): Unique identifier
- `timestamp` (ISO 8601, UTC): When action occurred
- `signal_trace` (List[String]): Signals that triggered action
- `decision_path` (List[RuleEvaluation]): Policy rules evaluated, results
- `action_executed` (String): Type of action (e.g., iam_binding_update)
- `result` (enum): success | failure | refused | postponed
- `hash_chain_link` (SHA256): Hash of previous receipt (or null if first)
- `entitlement_token_hash` (SHA256): Hash of JWT (proves authorization)
- `signature` (Ed25519): Cryptographic proof (tamper detection)

**Storage**:
- Primary: Firestore (region-local, real-time queries)
- Mirror: Cloud Logging (audit trail, long-term archive)
- Verification: Receipt Verifier rebuilds hash-chain, validates signatures

---

## Definition of Done

- [ ] C4 context diagram renders in Mermaid (5 personas, 6 systems, 11 relationships)
- [ ] All 5 government pain points addressed in narrative
- [ ] Receipt Contract section defines JSON schema for evidence
- [ ] Cross-references to glossary.md for all technical terms
- [ ] Data flow diagram shows signal → governor → action → receipt
- [ ] Regional consistency model explains Firestore replication strategy
- [ ] SPARQL evidence query example shows timeline reconstruction
- [ ] Entitlement drift, intervention audit, storm handling receipts all documented
- [ ] Jidoka enforcement rules clearly stated
- [ ] TAI 2030 compliance sections (4.1, 3.2, 5.0, 2.3, 1.0) mapped to solutions

---

**Next**: See [c4-containers.md](c4-containers.md) for Level 2 container architecture (Cloud Run, Pub/Sub, Firestore).
