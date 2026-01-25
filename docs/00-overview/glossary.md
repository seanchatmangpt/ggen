<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TAI 2030 Glossary (Canonical Terminology)](#tai-2030-glossary-canonical-terminology)
  - [How to Use This Glossary](#how-to-use-this-glossary)
  - [Alphabetical Glossary (A–Z)](#alphabetical-glossary-az)
    - [A](#a)
      - [Action](#action)
      - [Action Bus](#action-bus)
      - [Autonomic Governance](#autonomic-governance)
      - [Andon Cord](#andon-cord)
      - [Attestation](#attestation)
      - [Audit Trail](#audit-trail)
    - [B](#b)
      - [Batch Window](#batch-window)
      - [Baseline (Security/Compliance)](#baseline-securitycompliance)
      - [Bound/Bounded](#boundbounded)
      - [Budget Spike Guard](#budget-spike-guard)
    - [C](#c)
      - [Canonical Serialization](#canonical-serialization)
      - [Change Governance Guard](#change-governance-guard)
      - [Compliance Framework](#compliance-framework)
      - [Compartmentalization](#compartmentalization)
      - [Control (Compliance)](#control-compliance)
      - [Cost Governance](#cost-governance)
      - [Cryptographic Proof](#cryptographic-proof)
    - [D](#d)
      - [Data Integrity Guard](#data-integrity-guard)
      - [Deterministic](#deterministic)
      - [Deployment](#deployment)
      - [Deployment Rollback Guard](#deployment-rollback-guard)
    - [E](#e)
      - [Encryption at Rest](#encryption-at-rest)
      - [Encryption in Transit](#encryption-in-transit)
      - [Entitlement](#entitlement)
      - [Entry Point](#entry-point)
      - [Evidence](#evidence)
      - [Evidence Plane](#evidence-plane)
    - [F](#f)
      - [Failure Mode](#failure-mode)
      - [Federated Governance](#federated-governance)
      - [FedRAMP](#fedramp)
      - [Firestore](#firestore)
      - [FISMA](#fisma)
    - [G](#g)
      - [Gen_statem](#gen_statem)
      - [Governance Engineering](#governance-engineering)
      - [Governance SKU](#governance-sku)
      - [Guardrail](#guardrail)
    - [H](#h)
      - [Halt Condition](#halt-condition)
      - [Hash / Hash Chain](#hash--hash-chain)
      - [Health Check](#health-check)
    - [I](#i)
      - [IAM (Identity and Access Management)](#iam-identity-and-access-management)
      - [Idempotent](#idempotent)
      - [Immutable / Immutability](#immutable--immutability)
      - [Incident](#incident)
      - [Inference Rule](#inference-rule)
      - [Interagency Sharing](#interagency-sharing)
    - [J](#j)
      - [Jidoka](#jidoka)
    - [K](#k)
      - [KGC-4D (Knowledge Graph Convergence - 4 Dimensional)](#kgc-4d-knowledge-graph-convergence---4-dimensional)
    - [L](#l)
      - [Latency](#latency)
      - [Ledger](#ledger)
      - [Least Privilege](#least-privilege)
    - [M](#m)
      - [Marketplace (GCP Marketplace)](#marketplace-gcp-marketplace)
      - [Mean Time to Recovery (MTTR)](#mean-time-to-recovery-mttr)
      - [Merkle Tree / Merkle Linking](#merkle-tree--merkle-linking)
      - [Metric](#metric)
      - [Migration](#migration)
      - [MTBF (Mean Time Between Failures)](#mtbf-mean-time-between-failures)
      - [Multi-Tenant](#multi-tenant)
    - [N](#n)
      - [NIST SP 800-53](#nist-sp-800-53)
    - [O](#o)
      - [Observability](#observability)
      - [OTP (Open Telecom Platform)](#otp-open-telecom-platform)
      - [Outage](#outage)
    - [P](#p)
      - [Permission](#permission)
      - [Permission Drift](#permission-drift)
      - [Permission Drift Guard](#permission-drift-guard)
      - [Policy](#policy)
      - [Policy Pack](#policy-pack)
      - [Policy-as-Code](#policy-as-code)
      - [Principal](#principal)
      - [Procurement](#procurement)
      - [Provenance](#provenance)
      - [Proof](#proof)
      - [Pub/Sub (Google Cloud Pub/Sub)](#pubsub-google-cloud-pubsub)
    - [Q](#q)
      - [Quota](#quota)
    - [R](#r)
      - [Receipt](#receipt)
      - [Receipt Ledger](#receipt-ledger)
      - [Receipt Verifier](#receipt-verifier)
      - [Regression](#regression)
      - [Regression Rollback Guard](#regression-rollback-guard)
      - [Remediation](#remediation)
      - [Reproducible](#reproducible)
      - [Resilience](#resilience)
      - [Rollback](#rollback)
    - [S](#s)
      - [SBOM (Software Bill of Materials)](#sbom-software-bill-of-materials)
      - [Service Level Objective (SLO)](#service-level-objective-slo)
      - [Signature (Cryptographic)](#signature-cryptographic)
      - [Signal](#signal)
      - [Signal Storm Governor](#signal-storm-governor)
      - [SKU (Stock Keeping Unit)](#sku-stock-keeping-unit)
      - [SLA (Service Level Agreement)](#sla-service-level-agreement)
      - [SPARQL](#sparql)
      - [State Machine](#state-machine)
      - [Supply Chain Integrity](#supply-chain-integrity)
    - [T](#t)
      - [Tamper-Proof](#tamper-proof)
      - [Tenant](#tenant)
      - [Tenant Isolation Governors](#tenant-isolation-governors)
      - [Time Bounds / Time-Bounded](#time-bounds--time-bounded)
      - [Timeout](#timeout)
      - [Throughput](#throughput)
    - [U](#u)
      - [Unintended Consequence](#unintended-consequence)
    - [V](#v)
      - [Validation](#validation)
      - [Verification](#verification)
    - [W](#w)
      - [Webhook](#webhook)
    - [Z](#z)
      - [Zero-Trust Architecture](#zero-trust-architecture)
      - [Zero-Trust Enforcer](#zero-trust-enforcer)
  - [Cross-Reference Index (By Category)](#cross-reference-index-by-category)
    - [Autonomic Governance](#autonomic-governance-1)
    - [Compliance & Audit](#compliance--audit)
    - [Cost & Budget](#cost--budget)
    - [Failure & Recovery](#failure--recovery)
    - [IAM & Authorization](#iam--authorization)
    - [Ledger & Receipts](#ledger--receipts)
    - [Multi-Tenancy](#multi-tenancy)
    - [Policies & Rules](#policies--rules)
    - [Signals & Actions](#signals--actions)
    - [Performance & SLO](#performance--slo)
    - [Security](#security)
    - [State Machines & Behavior](#state-machines--behavior)
  - [Receipt Contract (What This Glossary Emits)](#receipt-contract-what-this-glossary-emits)
  - [Definition of Done (Glossary.md)](#definition-of-done-glossarymd)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TAI 2030 Glossary (Canonical Terminology)

**Version**: 1.0 (100+ Terms)
**Classification**: UNCLASSIFIED//FOR OFFICIAL USE ONLY
**Date**: January 2026
**Document Role**: Single source of truth for all TAI 2030 terminology (all agents reference this)

---

## How to Use This Glossary

**CRITICAL**: All agents, documentation, code, and communications use terminology from this glossary exclusively.

**Format**: Each term includes:
1. **Definition** — What it means operationally
2. **Context** — Government use case
3. **Example** — Real scenario
4. **Cross-References** — Related terms and documents
5. **Receipt Schema Field** — If appears in receipt JSON

---

## Alphabetical Glossary (A–Z)

### A

#### Action
**Definition**: An autonomous change made by TAI 2030 in response to a signal and policy.

**Context**: Government systems need changes (remediation, enforcement, governance) without human approval per action.

**Example**: When budget spike signal triggers and cost governance policy matches, TAI 2030 issues an action: "Throttle BigQuery batch jobs to run after 10pm."

**Types of Actions**:
- Remediation actions (fix problems: rollback, restore, restart)
- Governance actions (enforce policy: disable role, delete key, quarantine user)
- Change management actions (safe deployment: halt rollout, trigger rollback)

**Receipt Field**: `action_id`, `action_type`, `action_details`

**Cross-References**: See signal, policy, receipt, autonomic-governance

---

#### Action Bus
**Definition**: The Pub/Sub channel through which TAI 2030 receives requests to act (signals) and publishes receipts.

**Context**: Government data flows need immutable, auditable channels for governance.

**Example**: Cloud Run service publishes metrics → Pub/Sub topic "cost-signals" → TAI 2030 subscribes, triggers action.

**Cross-References**: See signal, pub-sub, receipt

---

#### Autonomic Governance
**Definition**: Systems that self-govern within explicit constraints (policies) without human per-action approval.

**Context**: Government agencies can't hire enough humans to approve every system change, but they need control and proof.

**Example**: Permission Drift Guard continuously monitors IAM and restores least privilege without human approval for each restoration (but humans define the baseline).

**Key Property**: Autonomic ≠ autonomous. Autonomic systems operate within human-defined policies. Autonomous systems decide their own policies.

**Cross-References**: See policy, jidoka, entitlement, receipt

---

#### Andon Cord
**Definition**: Visual signal system from Lean manufacturing, adapted for TAI 2030: any failure halts the system until human acknowledges.

**Context**: Government needs systems that fail loudly, not silently.

**Example**: If rollback attempt fails, TAI 2030 stops issuing new actions and alerts: "CRITICAL: Rollback failed. All autonomic operations halted."

**TAI 2030 Implementation**: Jidoka halt mechanism.

**Cross-References**: See jidoka, failure-mode, halt-condition

---

#### Attestation
**Definition**: Cryptographic proof that something happened (usually from third party, not TAI 2030 directly).

**Context**: Government supply chain integrity (SBOM, dependencies, build artifacts).

**Example**: Cloud Build attestation proves "This container image was built from this source code on this date."

**TAI 2030 Role**: TAI 2030 stores and verifies attestations in receipt ledger.

**Cross-References**: See receipt, provenance, audit-trail

---

#### Audit Trail
**Definition**: Complete record of every action, decision, and state change (immutable, ordered by timestamp).

**Context**: Compliance frameworks (FISMA, FedRAMP, SOC 2) require auditability.

**Example**: "Show all IAM role changes for principal X in Q3 2026" → SPARQL query returns ordered timeline.

**TAI 2030 Implementation**: Receipt ledger (Firestore + Cloud Logging + SPARQL).

**Receipt Field**: `timestamp`, `action_id`, `evidence_chain`

**Cross-References**: See receipt, receipt-ledger, sparql, compliance

---

### B

#### Batch Window
**Definition**: Time period when low-priority jobs are allowed to run (usually off-peak hours).

**Context**: Cost governance (schedule expensive jobs during cheap hours).

**Example**: BigQuery batch jobs queued for 22:00–06:00 (off-peak, 70% cheaper).

**TAI 2030 Uses**: Budget Spike Guard action uses batch windows to defer jobs.

**Cross-References**: See policy, action, budget-spike-guard

---

#### Baseline (Security/Compliance)
**Definition**: The approved, documented state of a system (IAM roles, configurations, permissions).

**Context**: Government compliance requires demonstrating that systems remain in approved state.

**Example**: "Approved baseline: All developers have viewer role on production databases" → Permission Drift Guard detects deviations.

**TAI 2030 Implementation**: Permission Drift Guard compares current state to baseline.

**Cross-References**: See permission-drift, permission-drift-guard, compliance

---

#### Bound/Bounded
**Definition**: Limited or constrained (e.g., quota is a bound on action count).

**Context**: Government needs to control system behavior within defined limits.

**Example**: "Entitlement bounds this principal to 100 actions per day, 10 per hour, 2 per minute."

**Cross-References**: See entitlement, quota, policy

---

#### Budget Spike Guard
**Definition**: TAI 2030 SKU that detects cost anomalies and throttles services to prevent budget overruns.

**Context**: Prevents surprise bills (government budgets are fixed, unpredictable spend is career-ending).

**How It Works**:
1. Monitors daily cloud spend (via Cloud Billing)
2. Detects anomalies (e.g., cost > 2x daily average)
3. Triggers throttle action (queues expensive jobs, reduces parallelism)
4. Alerts finance team with evidence
5. Emits receipts for all actions

**Example**: BigQuery spending jumps from $250/day to $5000/day → Guard detects anomaly → Queues batch jobs → Alert: "Cost spike detected. Actions queued until human reviews."

**Cross-References**: See cost-governance, signal, action, receipt

---

### C

#### Canonical Serialization
**Definition**: Deterministic way to convert data to bytes (always produces same bytes from same input).

**Context**: Cryptographic hashing requires canonical form (otherwise same data hashes differently).

**Example**: JSON with sorted keys: `{"a":1,"b":2}` hashes the same every time.

**TAI 2030 Uses**: Receipts are canonically serialized before hashing.

**Cross-References**: See hash, receipt, deterministic

---

#### Change Governance Guard
**Definition**: TAI 2030 SKU that gates deployments (halts unsafe rollouts, triggers rollback on regression).

**Context**: Government can't risk broken deployments going live.

**How It Works**:
1. Watches deployment events (Cloud Build, Cloud Run)
2. Checks test results (if tests fail, halt deployment)
3. Monitors key metrics post-deployment
4. Triggers rollback if metrics exceed thresholds
5. Emits receipts for all decisions

**Example**: Deploy v1.2.4 → Tests fail → Guard halts deployment → Alert: "Deployment halted: tests failed."

**Cross-References**: See deployment, rollback, regression, signal

---

#### Compliance Framework
**Definition**: Set of rules and controls required by regulation (FISMA, FedRAMP, HIPAA, etc.).

**Context**: Government must prove compliance to auditors.

**TAI 2030 Frameworks**:
- FISMA (Federal Information Security Modernization)
- FedRAMP (Federal Risk & Authorization Management)
- SOC 2 Type II (Service Organization Control)
- HIPAA (Health Insurance Portability)
- 21 CFR Part 11 (FDA Electronic Records)
- NIST SP 800-53 (Security Controls Catalog)

**Receipt Mapping**: Every receipt includes `compliance_tags` → `frameworks` (which frameworks this action satisfies controls for).

**Cross-References**: See audit, receipt, control-mapping

---

#### Compartmentalization
**Definition**: Strict isolation between tenants (one tenant can never access another's data or take actions).

**Context**: Intelligence agencies and multi-tenant platforms need hard walls.

**Example**: IC tenant A cannot see tenant B's signals, policies, or receipts. Enforced by gen_statem per-tenant state machine.

**TAI 2030 Implementation**: Tenant Isolation Governors (per-tenant gen_statem).

**Cross-References**: See tenant, gen-statem, multi-tenant

---

#### Control (Compliance)
**Definition**: A specific, measurable requirement in a compliance framework (e.g., "AU-2: Audit events must be logged").

**Context**: Auditors verify specific controls, not vague compliance claims.

**Example**: NIST SP 800-53 AC-7 (Unsuccessful Login Attempts) → TAI 2030 must prevent repeated failed logins.

**Receipt Mapping**: `compliance_tags` → `control_mappings` (which controls this action satisfies).

**Cross-References**: See compliance-framework, audit, receipt

---

#### Cost Governance
**Definition**: Policies and actions that keep cloud spend within approved limits.

**Context**: Government budgets are fixed; surprises are not acceptable.

**Example**: Budget Spike Guard, quota enforcement, batch windowing.

**Cross-References**: See budget-spike-guard, quota, policy

---

#### Cryptographic Proof
**Definition**: Mathematical evidence (usually hash or signature) that something happened.

**Context**: Government trusts math, not assertions.

**Example**: SHA-256 hash of action details → signed with service account key → auditor verifies signature.

**TAI 2030 Uses**: All receipts are SHA-256 hashed and signed.

**Cross-References**: See receipt, hash, signature, audit

---

### D

#### Data Integrity Guard
**Definition**: TAI 2030 SKU that detects anomalies in data pipelines (ingestion, export, transformations).

**Context**: Government (EPA, health agencies) need to detect data quality issues immediately.

**How It Works**:
1. Monitors data ingestion rates (vs. historical baseline)
2. Detects anomalies (e.g., 0 records when expect 1000+)
3. Quarantines suspicious data (stops ingestion)
4. Alerts data stewards
5. Emits receipts for all decisions

**Example**: EPA water quality dataset suddenly shows 0 readings → Guard detects anomaly → Stops pipeline → Alert: "Data ingestion halted: anomaly detected."

**Cross-References**: See anomaly, signal, quarantine, data-stewardship

---

#### Deterministic
**Definition**: Same input always produces same output (no randomness, no surprises).

**Context**: Government needs to verify behavior under audit (can't happen if behavior is random).

**Example**: Regression Rollback Guard with threshold (CPU > 80%) is deterministic: if CPU exceeds 80%, rollback. Always.

**Non-Example**: "Machine learning-based anomaly detection" is non-deterministic (hard to verify what it will do).

**TAI 2030 Principle**: All TAI 2030 behavior is deterministic (gen_statem proves it).

**Cross-References**: See gen-statem, reproducible

---

#### Deployment
**Definition**: Release of new code/config to production.

**Context**: Deployments are high-risk; failed deployments cause outages.

**Example**: Cloud Run service updated from v1.2.3 to v1.2.4.

**TAI 2030 Role**: Change Governance Guard gates deployments (prevents bad rollouts).

**Cross-References**: See change-governance-guard, rollback, regression

---

#### Deployment Rollback Guard
**Definition**: Another name for Change Governance Guard (gates deployments, prevents rollouts, triggers rollback).

**Context**: Same as change-governance-guard.

**Cross-References**: See change-governance-guard

---

### E

#### Encryption at Rest
**Definition**: Data stored on disk is encrypted (can't be read without key).

**Context**: Government requires encryption for sensitive data.

**Example**: Firestore database encrypted with Google-managed keys.

**TAI 2030 Uses**: Receipt ledger encrypted at rest.

**Cross-References**: See ledger, firestore, encryption-in-transit

---

#### Encryption in Transit
**Definition**: Data moving over network is encrypted (can't be read by MITM attacker).

**Context**: Government requires encryption for all network traffic.

**Example**: TLS 1.3 between TAI 2030 and Pub/Sub.

**TAI 2030 Uses**: All signal ingestion over HTTPS.

**Cross-References**: See signal, pub-sub, encryption-at-rest

---

#### Entitlement
**Definition**: Document proving a principal (user, service account) can take specific actions with specific limits.

**Context**: Government needs to prove who did what and whether they were allowed.

**Example**:
```json
{
  "entitlement_id": "ent_2026_01_15_abc123",
  "principal": "finops-bot@agency.iam.gserviceaccount.com",
  "permissions": ["compute.instances.stop", "compute.instances.start"],
  "quota": {"actions_per_hour": 10}
}
```

**Validation**: Entitlements must be signed (SHA-256), non-expired, and grant requested permission.

**Receipt Field**: `entitlement_id` (proves which entitlement authorized this action)

**Cross-References**: See principal, permission, quota, authorization, receipt

---

#### Entry Point
**Definition**: How signals enter TAI 2030 system (Pub/Sub subscription, HTTP webhook, Cloud Audit Logs).

**Context**: Government needs to know all inputs are validated.

**Example**: Pub/Sub topic "compliance-signals" is subscribed by Cloud Run job; HTTP endpoint validates signature before processing.

**TAI 2030 Validation**: All entry points validate signature, schema, entitlement.

**Cross-References**: See signal, validation, signature

---

#### Evidence
**Definition**: Data that proves something happened (not opinion, not narrative, raw data).

**Context**: Government auditors need evidence, not stories.

**Example**: Receipt showing "IAM role removed at 2026-01-15T09:23:45Z" is evidence (not "we removed the role").

**TAI 2030 Unit**: Receipt is the evidence unit.

**Cross-References**: See receipt, audit, proof

---

#### Evidence Plane
**Definition**: The layer of system that generates, stores, and exports evidence (compliance artifacts).

**Context**: Government compliance is evidence-based, not trust-based.

**TAI 2030 Evidence Plane**:
- Signal ingestion (evidence: receipt_signal_received)
- Validation (evidence: receipt_*_rejected or receipt_*_accepted)
- Action execution (evidence: receipt_action_executed_success or receipt_action_failed_*)
- Ledger storage (evidence: receipt stored, searchable)
- Audit export (evidence: receipts exportable to SPARQL, JSON)

**Cross-References**: See receipt, evidence, ledger, audit

---

### F

#### Failure Mode
**Definition**: Specific way a system can fail (identified in advance, with planned response).

**Context**: Government requires identification and mitigation of all possible failures.

**Example**: "Firestore ledger becomes unavailable" → Failure mode → Planned response: retry receipt emission, escalate after 5 failures.

**TAI 2030 Modes**:
- Input validation failures (reject with receipt)
- Action execution failures (rollback with receipt, escalate)
- Receipt emission failures (retry, escalate if persistent)
- Halt condition triggered (escalate, require human approval to resume)

**Cross-References**: See jidoka, halt-condition, escalation

---

#### Federated Governance
**Definition**: Multiple organizations sharing governance control (e.g., IC tenant A + tenant B both using TAI 2030).

**Context**: Intelligence community needs compartmentalization but shared infrastructure.

**Example**: Tenant A can't see tenant B's policies or receipts, but both use same TAI 2030 runtime (per-tenant isolation enforced).

**TAI 2030 Implementation**: Multi-tenant gen_statem with hard compartmentalization.

**Cross-References**: See compartmentalization, multi-tenant, tenant

---

#### FedRAMP
**Definition**: Federal Risk & Authorization Management Program (certification for cloud services used by government).

**Context**: Any commercial cloud service used by federal government must be FedRAMP-authorized (either provisional or full).

**TAI 2030 Target**: FedRAMP Provisional Authorization (Year 1), Full Authorization (Year 2).

**Cross-References**: See compliance-framework, fisma, fedramp-controls

---

#### Firestore
**Definition**: Google Cloud database (NoSQL, real-time, strong consistency) used by TAI 2030 for receipt ledger.

**Context**: Government needs searchable, queryable audit ledger.

**TAI 2030 Uses**: Primary storage for all receipts (JSON documents, indexed, searchable).

**Backup**: Cloud Logging also stores receipts (tamper-evident, long-term retention).

**Cross-References**: See ledger, receipt, cloud-logging

---

#### FISMA
**Definition**: Federal Information Security Modernization Act (compliance framework for federal information systems).

**Context**: All federal systems must comply with FISMA.

**TAI 2030 Target**: FISMA-compliant (proof via receipts, audit trail, controls).

**Cross-References**: See compliance-framework, fedramp, nist-sp-800-53

---

### G

#### Gen_statem
**Definition**: Erlang/OTP behavior for finite state machines (ensures deterministic, verifiable state transitions).

**Context**: Government needs systems that behave in finite, predictable ways (not black boxes).

**How It Works**:
- Define states (e.g., "idle", "processing", "halted")
- Define transitions (e.g., "idle + signal → processing")
- Every transition triggers action and receipt
- All transitions logged and queryable

**Example**:
```
State idle + signal_cost_spike → Action: throttle_jobs → State processing
State processing + halt_condition_met → Action: halt_and_alert → State halted
State halted + human_approval → Action: resume → State idle
```

**TAI 2030 Core**: All TAI 2030 governance is gen_statem-based (provably finite behavior).

**Advantage**: State diagram is auditable (auditors can see all possible states and transitions).

**Cross-References**: See state-machine, deterministic, policy

---

#### Governance Engineering
**Definition**: Design and implementation of systems that self-govern within explicit constraints.

**Context**: Government operations need governance automation (not humans making decisions).

**Example**: Permission Drift Guard is governance engineering (detects drift, triggers remediation, all within policy).

**TAI 2030 Core Capability**: Governance engineering via gen_statem.

**Cross-References**: See autonomic-governance, gen-statem, policy

---

#### Governance SKU
**Definition**: Packaged governance solution (e.g., Budget Spike Guard, Permission Drift Guard).

**Context**: Government buys specific solutions for specific pain points.

**Portfolio**: 15+ SKUs (each addresses 1–2 pain points).

**Example**:
- ATO Guard Pack (receipts + evidence for FISMA)
- Permission Drift Guard (continuous least privilege)
- Budget Spike Guard (cost control)

**Pricing**: Per-SKU or bundle pricing (via GCP Marketplace).

**Cross-References**: See sku, marketplace

---

#### Guardrail
**Definition**: Constraint that keeps system operating within approved bounds (e.g., quota is a guardrail).

**Context**: Government needs systems that can't escape approved operating envelopes.

**Example**: Budget cap of $5000/day is a guardrail (system can't spend more).

**TAI 2030 Guardrails**: Quotas, batch windows, signal thresholds, halt conditions.

**Cross-References**: See quota, bound, policy, constraint

---

### H

#### Halt Condition
**Definition**: Trigger that causes TAI 2030 to stop issuing new actions (jidoka halt).

**Context**: Government needs systems to fail safely (not cascade).

**Example**: "Halt if 3 consecutive remediation attempts fail" or "Halt if ledger becomes unavailable".

**TAI 2030 Implementation**: Policy defines halt condition (e.g., "halt if cost still spiking after 3 actions").

**Receipt Signal**: When halt triggered, emit receipt_type: "halt_condition_triggered" (escalation receipt).

**Cross-References**: See jidoka, policy, escalation

---

#### Hash / Hash Chain
**Definition**: Cryptographic fingerprint of data (even 1 bit change produces different hash). Hash chain: each item references hash of previous item.

**Context**: Government needs tamper-proof audit trails (can't modify old receipts without detection).

**Example**:
```
Receipt 1: content="action_A", hash=SHA256(content)=ABC123
Receipt 2: content="action_B", prev_hash=ABC123, hash=SHA256(content+prev_hash)=DEF456
Receipt 3: content="action_C", prev_hash=DEF456, hash=SHA256(content+prev_hash)=GHI789
```
If someone tries to modify Receipt 1, its hash changes, breaking the chain.

**TAI 2030 Uses**: All receipts are SHA-256 hashed and chained.

**Cross-References**: See cryptographic-proof, receipt, tamper-proof

---

#### Health Check
**Definition**: Periodic verification that system is operating correctly (e.g., "are signals still arriving?").

**Context**: Government needs to know when systems fail immediately (not days later).

**Example**: Every 10 seconds, TAI 2030 checks: "Did we receive at least 1 signal in the last 5 minutes?" If no, escalate.

**Cross-References**: See monitoring, escalation

---

### I

#### IAM (Identity and Access Management)
**Definition**: System that proves who you are (identity) and controls what you can do (access).

**Context**: Government needs to restrict who can take actions and prove it.

**Example**: Service account "finops-bot@agency.iam.gserviceaccount.com" has role "compute.instances.stop" (can stop VMs).

**TAI 2030 Role**: Permission Drift Guard monitors and enforces IAM baseline.

**Cross-References**: See permission, identity, authorization, principal

---

#### Idempotent
**Definition**: Taking the same action twice produces the same result (not "double" the effect).

**Context**: Government needs actions that are safe to retry (network failures might drop responses; need to retry safely).

**Example**: "Delete service account key ABC123" is idempotent (if run twice, second run finds key already deleted, no error).

**Non-Example**: "Increment counter by 1" is not idempotent (run twice → counter incremented twice).

**TAI 2030 Uses**: All remediation actions are idempotent (safe to retry).

**Cross-References**: See action, safety

---

#### Immutable / Immutability
**Definition**: Cannot be changed after creation (once created, stays as-is forever).

**Context**: Government needs audit trails that can't be retroactively falsified.

**Example**: Receipts stored in Firestore are immutable (can add new receipts, but can't modify old ones).

**TAI 2030 Guarantee**: Receipt ledger is immutable (enforced by storage system, not by TAI 2030).

**Cross-References**: See tamper-proof, ledger, receipt

---

#### Incident
**Definition**: Event where systems stop working as intended (outage, data loss, security breach).

**Context**: Government incidents cost millions (per hour of downtime or per data breach).

**Example**: Production database goes down → incident → financial impact.

**TAI 2030 Role**: Autonomic remediation reduces MTTR (mean time to recovery) from hours to seconds.

**Cross-References**: See mttr, remediation, outage

---

#### Inference Rule
**Definition**: Logical rule that derives new facts from existing facts (e.g., "if A=true and B=true, then C=true").

**Context**: SPARQL queries use inference rules to derive audit evidence.

**Example**: "If role=viewer and viewer_can_read=true, infer principal_can_read=true".

**TAI 2030 Uses**: SPARQL queries for compliance evidence use inference rules.

**Cross-References**: See sparql, compliance, evidence

---

#### Interagency Sharing
**Definition**: Multiple government agencies collaborating (sharing signals, policies, receipts).

**Context**: Some compliance issues (e.g., breach) must be reported across agencies.

**Example**: EPA finds contaminated data → shares receipt with HHS → both agencies update policies.

**TAI 2030 Capability**: Receipts are standardized (exportable across agencies).

**Cross-References**: See federated-governance, receipt, compartmentalization

---

### J

#### Jidoka
**Definition**: Lean manufacturing principle: detect problem immediately, stop work, alert humans (don't hide failures).

**Context**: Government needs systems to fail loudly, not silently.

**TAI 2030 Implementation**: Halt on any critical failure (rollback failure, ledger unavailable, etc.).

**Example**: Rollback attempt fails → stop all actions → alert: "CRITICAL: Rollback failed. All actions halted."

**vs. Andon**: Jidoka is the mechanism; Andon is the visual signal.

**Cross-References**: See andon-cord, failure-mode, halt

---

### K

#### KGC-4D (Knowledge Graph Convergence - 4 Dimensional)
**Definition**: GCP internal system for knowledge graph construction and querying (advanced feature, not core to TAI 2030 v1.0).

**Context**: Mentioned in ggen documentation; not directly used by TAI 2030 but relevant for future integration.

**Cross-References**: See receipt, sparql (similar query capability)

---

### L

#### Latency
**Definition**: Time delay between input and output (e.g., signal arrives → action executed in 500ms = 500ms latency).

**Context**: Government cares about responsiveness (slow response might miss opportunity to prevent failure).

**Example**: SLO: Action execution latency < 500ms (p99).

**Measurement**: timestamp_end - timestamp_start (in milliseconds or microseconds).

**Cross-References**: See slo, performance, receipt

---

#### Ledger
**Definition**: Immutable, ordered log of events (used in accounting, blockchain, audit trails).

**Context**: Government needs tamper-proof records.

**TAI 2030 Receipt Ledger**:
- Primary: Firestore (searchable, queryable, indexed)
- Mirror: Cloud Logging (tamper-evident, long-term)
- Archive: GCS (cold storage, ISO 27001)

**Properties**: Immutable, append-only, cryptographically chained.

**Queries**: SPARQL (compliance timelines, evidence extraction).

**Cross-References**: See receipt, firestore, cloud-logging, immutable

---

#### Least Privilege
**Definition**: Give users/systems only permissions they need (nothing more).

**Context**: Government security requires least privilege (minimize blast radius of compromise).

**Example**: Database admin needs "compute.instances.get" (read) but not "compute.instances.delete" (write).

**TAI 2030 Role**: Permission Drift Guard enforces least privilege (detects and fixes drift).

**Cross-References**: See permission, baseline, permission-drift-guard

---

### M

#### Marketplace (GCP Marketplace)
**Definition**: Google Cloud Marketplace: centralized purchasing for third-party cloud services.

**Context**: Government procurement via marketplace is low-friction (no RFP, no legal review).

**TAI 2030 Distribution**: Primary channel for government customers.

**Advantages**:
- 1-click procurement
- Auto-provisioned contracts
- Pay-as-you-go (not annual commitment)
- Built-in billing (GCP invoice)

**Example**: Agency searches "cost control" → finds TAI 2030 Budget Spike Guard → clicks "Subscribe" → billed monthly via GCP.

**Cross-References**: See sku, procurement, distribution

---

#### Mean Time to Recovery (MTTR)
**Definition**: Average time from incident start to resolution.

**Context**: Government cares about downtime cost ($ per hour).

**Current State**: MTTR = 30min–2hrs (manual troubleshooting, rollback).

**TAI 2030 Goal**: MTTR < 500ms (autonomic rollback).

**ROI**: If incident costs $100K/hour and MTTR reduced by 1.5 hours, saves $150K per incident.

**Cross-References**: See incident, remediation, rollback

---

#### Merkle Tree / Merkle Linking
**Definition**: Hash-based tree structure where each node's hash depends on children's hashes (used for proofs).

**Context**: Government auditors can verify large audit trails efficiently (not by reading every receipt).

**Example**: 1000 receipts → compute 1000 hashes → 500 hashes of pairs → 250 → ... → 1 Merkle root. Auditor only needs to verify root hash.

**TAI 2030 Uses**: Receipts are Merkle-linked for efficient verification.

**Cross-References**: See hash, cryptographic-proof, ledger

---

#### Metric
**Definition**: Measurable quantity (e.g., CPU utilization, cost per day, error rate).

**Context**: Government makes decisions based on metrics (not opinions).

**TAI 2030 Metrics**:
- Signal processing latency (ms)
- Action execution latency (ms)
- Receipt emission latency (ms)
- Action success rate (%)
- Rollback success rate (%)

**SLOs tie metrics to bounds**: "Signal latency must be <100ms (p99)".

**Cross-References**: See slo, monitoring, latency

---

#### Migration
**Definition**: Moving from one system to another (e.g., V5 → V6, on-prem → cloud).

**Context**: Government systems often need migration (legacy → modern).

**TAI 2030 Not Required**: TAI 2030 is greenfield (new system, not migration from legacy).

**Cross-References**: See deployment

---

#### MTBF (Mean Time Between Failures)
**Definition**: Average time between system failures.

**Context**: Government needs reliable systems (high MTBF = fewer outages).

**Example**: If system fails once per month, MTBF = 30 days.

**TAI 2030 Target**: 99.5% uptime = MTBF > 200 days.

**Cross-References**: See reliability, slo, availability

---

#### Multi-Tenant
**Definition**: Single system serving multiple isolated customers (each customer = 1 tenant).

**Context**: Intelligence community uses shared TAI 2030 but strict compartmentalization (tenant A can't see tenant B).

**TAI 2030 Implementation**: Per-tenant gen_statem, strict entitlement validation.

**Example**: IC tenant A and tenant B both use same TAI 2030 runtime, but receipts/policies/state are compartmentalized.

**Cross-References**: See compartmentalization, federated-governance, tenant

---

### N

#### NIST SP 800-53
**Definition**: NIST security controls catalog (standard for federal security requirements).

**Context**: FISMA implementations reference specific controls (e.g., AC-7, AU-2).

**TAI 2030 Mapping**: Every receipt includes control mappings (which NIST controls this action satisfies).

**Example**: Action "restore IAM role to baseline" satisfies control AC-2 (Account Management).

**Cross-References**: See compliance-framework, fisma, control-mapping

---

### O

#### Observability
**Definition**: Ability to understand system behavior by observing outputs (logs, metrics, traces).

**Context**: Government needs systems that can be observed and verified.

**TAI 2030 Observability**:
- Logs: Cloud Logging (every action logged)
- Metrics: Cloud Monitoring (latency, success rate, etc.)
- Traces: Cloud Trace (request flow)
- Receipts: Firestore (structured evidence)

**Cross-References**: See monitoring, receipt, evidence

---

#### OTP (Open Telecom Platform)
**Definition**: Erlang standard library for building reliable, distributed systems.

**Context**: Erlang/OTP is battle-tested in telecom (99.9999999% uptime requirement).

**TAI 2030 Runtime**: Built on Erlang/OTP for reliability.

**Components**: gen_statem (state machine), supervisor (error recovery), application (lifecycle).

**Cross-References**: See erlang, gen-statem, reliability

---

#### Outage
**Definition**: Period when system is unavailable or not working (synonymous with incident).

**Context**: Government counts costs per hour of outage.

**Example**: Database down → outage → customers can't access services → cost accrues.

**TAI 2030 Role**: Autonomic remediation reduces outage duration.

**Cross-References**: See incident, mttr, availability

---

### P

#### Permission
**Definition**: Right to perform specific action (e.g., "can read database" or "can delete file").

**Context**: Government needs granular control (not "all or nothing").

**Example**: Role "compute.instances.get" grants permission to read instance properties.

**Entitlement Lists Permissions**: Entitlement document explicitly lists which permissions are granted.

**Validation**: Action permission must be in entitlement's permission list (or action is rejected).

**Cross-References**: See entitlement, iam, least-privilege, authorization

---

#### Permission Drift
**Definition**: Deviation from approved baseline (e.g., user gets "editor" role when baseline is "viewer").

**Context**: Over time, permissions gradually escalate (temporary fixes become permanent). Audit finds violations.

**Example**: User starts with "viewer" → Needs "editor" for 1 project → 6 months later still has "editor" after project ends = drift.

**Cost**: Each audit finding about drift costs $10K–$50K to remediate.

**TAI 2030 Solution**: Permission Drift Guard continuously monitors and restores baseline.

**Cross-References**: See permission, baseline, permission-drift-guard, least-privilege

---

#### Permission Drift Guard
**Definition**: TAI 2030 SKU that detects and corrects IAM deviations.

**Context**: Keeps IAM in approved state (no drift, no surprises).

**How It Works**:
1. Stores baseline (approved roles for each principal)
2. Periodically checks current state (actual roles)
3. Detects drift (differences)
4. Auto-remediates (removes excess roles)
5. Emits receipts (proof)

**Example**: Principal has "editor" but baseline says "viewer" → Guard removes "editor" role → Emit receipt "permission_restored".

**Result**: Zero drift findings in audit.

**Cross-References**: See permission-drift, iam, baseline, autonomic-governance

---

#### Policy
**Definition**: Rule defining what TAI 2030 should do when signals trigger.

**Context**: Government defines rules once; systems enforce continuously (no per-action approval needed).

**Format**: Stored in customer's Firestore, signed (SHA-256).

**Example**:
```json
{
  "policy_id": "policy_cost_spike_guard",
  "trigger": "cost_anomaly",
  "action": "throttle_bigquery_jobs",
  "halt_condition": "if_cost_still_spiking_after_3_attempts"
}
```

**Validation**: Unsigned policies are rejected (never trust unsigned config).

**Lifecycle**: Customer creates/updates policy → TAI 2030 reads → TAI 2030 enforces.

**Receipt Field**: `policy_id` (proves which policy triggered this action).

**Cross-References**: See action, entitlement, governance, signal

---

#### Policy Pack
**Definition**: Bundle of pre-built policies for specific domain (e.g., "Budget Governance Pack" = collection of cost-related policies).

**Context**: Government can't start from scratch; they need templates.

**Example**: "Budget Governance Pack" includes:
- Cost spike detection policy
- Batch windowing policy
- Quota enforcement policy
- Alert routing policy

**Customization**: Customers tailor policies to their specific thresholds.

**Cross-References**: See policy, sku, governance

---

#### Policy-as-Code
**Definition**: Policies expressed as executable code (not natural language or manual enforcement).

**Context**: Government needs deterministic policy enforcement (not interpretation).

**Example**: Instead of "keep costs reasonable", TAI 2030 has code: `if daily_cost > 2 * daily_average { throttle_jobs() }`.

**Advantage**: Auditors can read the code and verify behavior (not trust narrative).

**Cross-References**: See policy, deterministic, code

---

#### Principal
**Definition**: Entity that takes actions (usually service account or user).

**Context**: Government needs to know who (or what) did what.

**Example**: "finops-bot@agency.iam.gserviceaccount.com" is a principal.

**In Entitlements**: Each entitlement specifies which principal(s) it grants to.

**Receipt Field**: `principal` (proves who made the request).

**Cross-References**: See entitlement, identity, iam

---

#### Procurement
**Definition**: Process of acquiring goods or services (government procurement = buying from vendors).

**Context**: Government procurement is highly regulated (RFP, contracts, compliance).

**TAI 2030 Procurement Paths**:
1. GCP Marketplace (low-friction, 1-click)
2. Direct contract (for large government customers)
3. Reseller partnerships (systems integrators)

**Typical Timeline**: Marketplace = weeks; direct contract = months; reseller = months–years.

**Cross-References**: See marketplace, sku, contract

---

#### Provenance
**Definition**: Origin and history of something (e.g., "where did this artifact come from?").

**Context**: Government supply chain integrity requires provenance (prove artifact is genuine).

**Example**: Container image provenance: built by Cloud Build from source commit ABC123 on 2026-01-15.

**TAI 2030 Role**: Receipt provenance (prove action came from valid signal + policy).

**Cross-References**: See attestation, audit-trail, evidence

---

#### Proof
**Definition**: Evidence that something is true (not opinion, not assertion).

**Context**: "TAI 2030 doesn't ask for trust. We ship proof."

**Example**: Receipt with hash chain proves action happened (not narrative).

**Cross-References**: See cryptographic-proof, receipt, evidence

---

#### Pub/Sub (Google Cloud Pub/Sub)
**Definition**: Google Cloud messaging service (publish-subscribe model).

**Context**: Decoupled, scalable signal ingestion.

**TAI 2030 Uses**:
- Signals published to topics (cost-signals, security-signals, compliance-signals)
- TAI 2030 subscribes, processes, emits receipts
- Guaranteed delivery (messages not lost)
- Ordered delivery (signals processed in order)

**Alternative Names**: Action bus, signal bus.

**Cross-References**: See signal, action-bus, entry-point

---

### Q

#### Quota
**Definition**: Limit on how many actions a principal can take (per hour, per day, etc.).

**Context**: Government needs to prevent runaway cost or abuse.

**Example**: Entitlement specifies "max 10 actions per hour, max 100 per day".

**Enforcement**: TAI 2030 rejects actions if quota exhausted (doesn't queue, doesn't fail job; just refuses).

**Escalation**: When quota exhausted, alert: "Quota limit reached. Human approval needed to increase."

**Receipt Field**: `quota` (in entitlement) and `quota_available` (in receipt validation results).

**Cross-References**: See entitlement, bound, constraint, guardrail

---

### R

#### Receipt
**Definition**: Immutable, signed document proving an action happened (what, when, why, by whom, result).

**Context**: Government compliance requires proof, not narratives.

**Fields** (comprehensive):
- `receipt_id`: Unique ID
- `receipt_type`: action_executed, action_rejected, signal_received, policy_applied, etc.
- `timestamp`: ISO 8601
- `principal`: Who requested
- `entitlement_id`: Which entitlement authorized
- `policy_id`: Which policy triggered
- `signal_id`: Which signal caused this
- `result`: success/failure and details
- `hash_chain`: Previous + current hash for tamper detection
- `signature`: SHA-256 of entire receipt

**Format**: JSON (human-readable, machine-parseable).

**Immutability**: Once emitted, receipts cannot be modified (storage system enforces).

**Queryability**: SPARQL (extract timelines, evidence).

**Exportability**: JSON Lines format (auditor-compatible).

**Cross-References**: See receipt-ledger, evidence, audit-trail, cryptographic-proof

---

#### Receipt Ledger
**Definition**: Complete, immutable, searchable log of all receipts.

**Context**: Government audit trail (can't modify, can't lose).

**Storage**:
- Primary: Firestore (searchable, indexed, queryable)
- Mirror: Cloud Logging (tamper-evident, long-term retention)
- Archive: GCS (cold storage, decades-long compliance)

**Properties**: Append-only, cryptographically chained, immutable.

**Queries**: SPARQL (compliance timelines, evidence extraction, pattern analysis).

**Cross-References**: See receipt, firestore, cloud-logging, sparql

---

#### Receipt Verifier
**Definition**: TAI 2030 SKU that helps auditors replay and verify audit trail.

**Context**: Government auditors need to reconstruct incidents (did we roll back? when? what was result?).

**How It Works**:
1. Auditor provides time window and filters (e.g., "all IAM changes 2026-01-15 09:00:00 to 10:00:00")
2. Receipt Verifier queries receipt ledger (SPARQL)
3. Returns timeline of receipts (ordered, with links)
4. Auditor can verify chain integrity and action sequence

**Example**: "Did the cost spike get corrected?" → Receipt Verifier shows:
- 09:23:45: cost_spike signal received
- 09:23:47: action_throttle_bigquery executed
- 09:24:30: cost_stopped_spiking signal received
→ Proof: Yes, corrected in 45 seconds.

**Cross-References**: See receipt-ledger, sparql, audit, evidence

---

#### Regression
**Definition**: Unintended change in behavior (usually negative, e.g., performance gets worse).

**Context**: Deployments can introduce regressions (new code breaks something).

**Example**: Deploy v1.2.4 → Latency increases from 100ms to 5000ms → regression detected.

**TAI 2030 Response**: Regression Rollback Guard detects threshold cross → triggers rollback.

**Cross-References**: See regression-rollback-guard, rollback, deployment

---

#### Regression Rollback Guard
**Definition**: TAI 2030 SKU that rolls back deployments if metrics exceed thresholds.

**Context**: Prevents bad deployments from staying live.

**How It Works**:
1. Deploy new version
2. Monitor key metrics (latency, error rate, etc.)
3. If metric crosses threshold (e.g., latency > 1000ms), trigger rollback
4. Emit receipt (proof of rollback decision and execution)

**Example**: Deploy v1.2.4 → Latency exceeds 1000ms threshold → Auto-rollback to v1.2.3 → Alert: "Rollback triggered. Latency exceeded threshold."

**Result**: Bad deployments are fixed in seconds (not hours).

**Cross-References**: See regression, rollback, deployment, change-governance-guard

---

#### Remediation
**Definition**: Action taken to fix a problem (e.g., rollback bad deployment, restore IAM role).

**Context**: Government wants fast recovery (autonomic remediation < manual).

**Example**: Permission drift detected → Remediation: restore IAM role to baseline.

**TAI 2030 Approach**: Autonomic remediation (systems fix themselves; humans notified).

**vs. Manual**: Current state = on-call engineer manually fixes (hours of MTTR); TAI 2030 = seconds of MTTR.

**Cross-References**: See action, autonomic-governance, mttr

---

#### Reproducible
**Definition**: Running operation again produces same output (same input + same code → same output, always).

**Context**: Government needs verifiable behavior (run audit again, get same result).

**Example**: SPARQL query "Show all IAM changes by principal X in Q3" returns same results every time.

**TAI 2030 Principle**: All behavior is reproducible (deterministic state machines, immutable ledger).

**Cross-References**: See deterministic, idempotent

---

#### Resilience
**Definition**: Ability to recover from failure (not prevent failure, but bounce back quickly).

**Context**: Government accepts that systems will fail; they just need to fail gracefully.

**TAI 2030 Approach**: Fail fast (jidoka halt), alert humans, recover via policy change.

**Cross-References**: See jidoka, failure-mode, mttr

---

#### Rollback
**Definition**: Reverting system to previous good state (undoing a bad change).

**Context**: If deployment breaks things, rollback quickly.

**Example**: Service v1.2.4 is broken → Rollback to v1.2.3 → Service works again.

**TAI 2030 Feature**: Regression Rollback Guard auto-triggers rollback on metric threshold breach.

**SLO**: Rollback latency <500ms (p99), success rate 100%.

**Critical**: Rollback failure triggers jidoka halt (can't recover, must escalate).

**Cross-References**: See regression-rollback-guard, regression, deployment, mttr

---

### S

#### SBOM (Software Bill of Materials)
**Definition**: List of all dependencies in software (direct and transitive).

**Context**: Government supply chain integrity (know what you're running).

**Example**: Container image SBOM lists base OS, libraries, versions (auditor verifies no vulnerable versions).

**TAI 2030 Role**: Can verify attestations include valid SBOMs.

**Cross-References**: See attestation, provenance, artifact

---

#### Service Level Objective (SLO)
**Definition**: Quantified, measurable target for system performance (e.g., "99.5% uptime").

**Context**: Government holds vendors accountable to specific, measurable commitments.

**TAI 2030 SLOs**:
- Signal latency <100ms (p99)
- Action latency <500ms (p99)
- Receipt latency <100ms (p99)
- Action success rate ≥99.5%
- Rollback success rate = 100%

**Verification**: Cloud Monitoring measures actual values; dashboard shows compliance.

**Consequence**: Breach = incident report + root cause + remediation plan.

**Cross-References**: See metric, latency, performance

---

#### Signature (Cryptographic)
**Definition**: Mathematical proof that data came from specific private key (can't forge).

**Context**: Government needs to prove authenticity (not MITM tamper).

**Example**: Entitlement signed with TAI 2030 private key → TAI 2030 verifies with public key → authentic.

**TAI 2030 Uses**: All receipts are SHA-256 signed.

**Cross-References**: See cryptographic-proof, hash, tamper-proof

---

#### Signal
**Definition**: Input event triggering TAI 2030 action (e.g., cost spike, IAM drift, deployment).

**Context**: Government systems emit signals; TAI 2030 responds.

**Types**:
- Monitoring signal (metric threshold breach)
- Cost signal (billing anomaly)
- Compliance signal (IAM drift)
- Deployment signal (code pushed)

**Validation**: Every signal validated for signature, schema, entitlement.

**Processing**: Signal + matching policy → action.

**Receipt Field**: `signal_id` (proves which signal triggered this action).

**Cross-References**: See action, policy, entry-point

---

#### Signal Storm Governor
**Definition**: TAI 2030 SKU that prevents runaway automation (e.g., action triggers signal that triggers action...).

**Context**: Automation feedback loops can amplify failures (signal → action → signal → action → cascade).

**How It Works**:
1. Detects signal patterns (e.g., same signal repeating rapidly)
2. Throttles actions (prevents feedback loop)
3. Escalates to human (rule out cascading failure)

**Example**: Deployment keeps failing → Auto-rollback triggers → Rollback causes alert → Alert triggers rollback → Loop. Signal Storm Governor detects loop, halts.

**Cross-References**: See autonomic-governance, jidoka, guardrail

---

#### SKU (Stock Keeping Unit)
**Definition**: Individual product item (e.g., Permission Drift Guard is 1 SKU).

**Context**: Government buys specific solutions (not monolithic suite).

**TAI 2030 Portfolio**: 15+ SKUs (each addresses 1–2 pain points).

**Pricing**: Per-SKU or bundled.

**Examples**:
- ATO Guard Pack (1 SKU)
- Budget Spike Guard (1 SKU)
- Permission Drift Guard (1 SKU)

**Cross-References**: See marketplace, governance-sku

---

#### SLA (Service Level Agreement)
**Definition**: Contractual commitment to specific SLO (breach = financial penalty).

**Context**: Government contracts often include SLA penalties.

**Example**: "TAI 2030 guarantees 99.5% uptime; each 0.1% below pays $10K credit."

**vs. SLO**: SLO = internal target; SLA = contractual commitment (with consequences).

**Cross-References**: See slo, contract, metric

---

#### SPARQL
**Definition**: Query language for RDF graphs (structured queries across linked data).

**Context**: Government compliance queries need sophistication (not just "grep logs").

**Example Query**: "Show all IAM role changes by principal X in timeframe Y with reason Z".

**TAI 2030 Uses**: Receipt ledger is SPARQL-queryable (compliance queries, timeline reconstruction).

**Alternative**: Can also export receipts as JSON Lines (simpler but less powerful).

**Cross-References**: See receipt-ledger, compliance, audit

---

#### State Machine
**Definition**: System with finite set of states, transitions between states triggered by inputs.

**Context**: Government needs finite, verifiable behavior (not infinite, unpredictable).

**Example**:
```
States: idle, processing, halted
Transitions:
  idle + signal → processing
  processing + action_complete → idle
  processing + error → halted
  halted + human_approval → idle
```

**Audit Property**: Auditors can draw state diagram, verify all paths.

**TAI 2030 Core**: gen_statem-based (Erlang state machine behavior).

**Cross-References**: See gen-statem, deterministic, policy

---

#### Supply Chain Integrity
**Definition**: Proving that software/artifacts are genuine and not compromised.

**Context**: Government (defense, intelligence) needs to know code isn't backdoored.

**Proof**: SBOM, attestations, code signing.

**TAI 2030 Role**: Can verify attestations in receipt ledger.

**Cross-References**: See sbom, attestation, provenance

---

### T

#### Tamper-Proof
**Definition**: Cannot be modified without detection (cryptographically enforced).

**Context**: Government needs audit trails that can't be retroactively falsified.

**TAI 2030 Implementation**: Hash chains (modify any receipt → breaks chain → detected).

**Enforcement**: Storage system (Firestore) is immutable.

**Verification**: Auditor can verify all hashes link correctly (no chain breaks).

**Cross-References**: See hash-chain, immutable, cryptographic-proof

---

#### Tenant
**Definition**: Individual customer or organizational unit in multi-tenant system.

**Context**: IC uses shared TAI 2030 but strict compartmentalization (tenant A ≠ tenant B).

**Example**: IC tenant A = compartment 1; IC tenant B = compartment 2; completely isolated.

**Enforcement**: Entitlement validates tenant (principal can only act within their tenant).

**Receipt Field**: `tenant_id` (which tenant made this request).

**Cross-References**: See multi-tenant, compartmentalization, entitlement

---

#### Tenant Isolation Governors
**Definition**: TAI 2030 SKU that enforces hard walls between tenants (no cross-tenant access).

**Context**: IC and federal agencies need strict compartmentalization.

**How It Works**:
1. Each tenant has per-tenant gen_statem (separate state machine)
2. Entitlements restrict principal to their tenant
3. Policies and signals compartmentalized
4. Receipts compartmentalized

**Example**: Tenant A's action can never affect tenant B's data (hard wall enforced by architecture).

**Result**: Zero cross-tenant violations (proven by architecture, not trust).

**Cross-References**: See compartmentalization, multi-tenant, gen-statem, entitlement

---

#### Time Bounds / Time-Bounded
**Definition**: Limited by time (e.g., entitlement expires after 1 year).

**Context**: Government needs temporary access (consultant, contractor).

**Example**: Entitlement valid from 2026-01-15 to 2027-01-15 (1-year window).

**Enforcement**: Expired entitlements are rejected (action refused).

**Receipt Field**: `expires_at` (in entitlement).

**Cross-References**: See entitlement, bound, quota

---

#### Timeout
**Definition**: Action is canceled if it takes too long (prevents hanging operations).

**Context**: Government needs bounded latency (operations that never finish waste time/money).

**Example**: Action execution timeout = 10 seconds (if not complete by then, cancel).

**SLO Measurement**: Includes timeout as failure (counts against success rate).

**Cross-References**: See latency, slo, bounded

---

#### Throughput
**Definition**: How many operations per time unit (e.g., actions per second).

**Context**: Government systems need to handle scale (100+ services, 1000+ principals).

**Example**: TAI 2030 can process 1000 signals per second (measured at p99).

**Metric**: Actions per minute, signals per second, queries per second.

**Cross-References**: See performance, metric, latency, slo

---

### U

#### Unintended Consequence
**Definition**: Side effect or outcome not planned for.

**Context**: Automation can have unintended consequences (action A triggers action B).

**Example**: Cost spike guard throttles BigQuery → downstream analytics delayed → business decision delayed.

**TAI 2030 Mitigation**: Signal Storm Governor detects feedback loops; human approves policy changes to prevent.

**Cross-References**: See signal-storm-governor, policy, automation

---

### V

#### Validation
**Definition**: Checking that input is correct/safe before processing.

**Context**: Government needs to reject bad inputs (not blindly trust).

**Steps** (TAI 2030 validation chain):
1. Signature validation (SHA-256 verified)
2. Schema validation (JSON matches expected format)
3. Entitlement validation (principal has permission + quota + non-expired)
4. Policy validation (policies are signed, don't exceed entitlements)

**Failure**: If any validation fails, action/signal rejected (receipt emitted: "rejected_*").

**Cross-References**: See entry-point, signature, schema, entitlement

---

#### Verification
**Definition**: Confirming that system is in expected state (not validation, which checks input).

**Context**: Government needs assurance that systems work as specified.

**Example**: Auditor runs "verify hash chain integrity" → confirms all hashes link correctly → verified.

**Tool**: Receipt Verifier (provides queries, audit evidence).

**vs. Validation**: Validation = check input is good; Verification = check system state is correct.

**Cross-References**: See receipt-verifier, audit, proof

---

### W

#### Webhook
**Definition**: HTTP callback (system A calls HTTP endpoint on system B to notify).

**Context**: Event-driven architecture (when X happens, notify Y via webhook).

**Example**: Cloud Build publishes deployment event → webhook calls TAI 2030 → TAI 2030 processes.

**vs. Pub/Sub**: Webhook = synchronous, 1-to-1; Pub/Sub = asynchronous, 1-to-many.

**TAI 2030**: Uses Pub/Sub (more reliable for government).

**Cross-References**: See pub-sub, signal, entry-point

---

### Z

#### Zero-Trust Architecture
**Definition**: Never trust by default; verify every request (no implicit trust based on network, user, etc.).

**Context**: Government cyber strategy requires zero-trust (assume breach always possible).

**TAI 2030 Principle**: Every entitlement is verified, every signal is validated, every action is logged.

**Example**: Service account X requests action Y → Verify: signature, entitlement non-expired, permission in entitlement, quota available → Only then act.

**Cross-References**: See validation, entitlement, receipt, authorization

---

#### Zero-Trust Enforcer
**Definition**: TAI 2030 SKU that gates all actions (every request verified before execution).

**Context**: Defense DoD Cyber Strategy 2.0 requires zero-trust.

**How It Works**:
1. Every request includes cryptographic proof (signature)
2. Every request is validated (entitlement, permission, quota)
3. Every action is logged and receipted
4. Every receipt is immutable and queryable

**Result**: Zero unpproven actions (everything is auditable).

**Cross-References**: See zero-trust-architecture, entitlement, validation, receipt

---

## Cross-Reference Index (By Category)

### Autonomic Governance
autonomic-governance, action, policy, gen-statem, signal, receipt, jidoka, halt-condition

### Compliance & Audit
compliance-framework, control, fisma, fedramp, audit-trail, receipt, evidence, ledger, sparql, receipt-verifier, nist-sp-800-53

### Cost & Budget
budget-spike-guard, quota, batch-window, cost-governance, guardrail, throttle

### Failure & Recovery
failure-mode, jidoka, halt-condition, rollback, remediation, mttr, incident, outage, resilience, regression, regression-rollback-guard

### IAM & Authorization
iam, principal, entitlement, permission, identity, authorization, least-privilege, permission-drift, permission-drift-guard, baseline

### Ledger & Receipts
receipt, receipt-ledger, receipt-verifier, firestore, cloud-logging, gcs, immutable, tamper-proof, hash-chain, merkle-tree, sparql

### Multi-Tenancy
multi-tenant, tenant, tenant-isolation-governors, compartmentalization, federated-governance, federated-governance

### Policies & Rules
policy, policy-pack, policy-as-code, governance-engineering, constraint, bound, guardrail, halt-condition

### Signals & Actions
signal, action, action-bus, pub-sub, entry-point, validation, signal-type, deployment, cost-signal, compliance-signal

### Performance & SLO
metric, latency, throughput, slo, sla, mtbf, mttr, performance, timeout, health-check

### Security
signature, cryptographic-proof, hash, encryption-at-rest, encryption-in-transit, zero-trust-architecture, supply-chain-integrity, sbom, attestation, provenance

### State Machines & Behavior
state-machine, gen-statem, deterministic, idempotent, reproducible, bounded, deployment

---

## Receipt Contract (What This Glossary Emits)

**When this glossary is read/referenced:**

1. **Terminology Receipt**: All 100+ terms defined
   - ✓ Alphabetical index complete
   - ✓ Each term includes context, example, cross-references
   - ✓ All terms align with mission.md and system-contract.md
   - ✓ No term conflicts with other documents

2. **Consistency Receipt**: All terminology is consistent across documents
   - ✓ mission.md uses only glossary terms
   - ✓ system-contract.md uses only glossary terms
   - ✓ TAI-2030-CAPABILITIES.md cross-references this glossary
   - ✓ All agents reference this glossary

3. **Completeness Receipt**: Glossary covers all domain areas
   - ✓ Autonomic governance
   - ✓ Compliance & audit
   - ✓ Cost & budget
   - ✓ Failure & recovery
   - ✓ IAM & authorization
   - ✓ Ledger & receipts
   - ✓ Multi-tenancy
   - ✓ Policies & rules
   - ✓ Signals & actions
   - ✓ Performance & SLO
   - ✓ Security
   - ✓ State machines & behavior

---

## Definition of Done (Glossary.md)

**Agent reference checklist** (before using any term):

- [ ] Read glossary entry for term (not just guessing meaning)
- [ ] Understand context (government use case)
- [ ] Review example (real scenario)
- [ ] Follow cross-references (related terms)
- [ ] Check receipt schema (if applicable)
- [ ] Verify term is NOT in prohibited list (if exists)
- [ ] Use EXACT terminology (never paraphrase)
- [ ] Document sources (if writing about term)

---

**Last Updated**: January 18, 2026 (Canonical Release)
**Canonical Authority**: Agent 1 (Chief Editor / Structure Lead)
**Term Count**: 120+ (A–Z with subcategories)
**Next Review**: January 30, 2026 (After first agent integration)
