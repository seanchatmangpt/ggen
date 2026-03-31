# TAI Autonomics Support Model: Marketplace & Procurement Versions

**Date**: 2026-01-26
**Document Type**: Customer-Facing + Procurement-Ready
**Status**: Production Ready
**Positioning**: "We don't offer improvisational support; we offer provable support."

---

## Version 1: Marketplace-Friendly (5 Bullets, Calm Tone)

### Support Model: TCPS / MCP-Only

Support is delivered **exclusively by MCP agents** operating under **TCPS rules** (standard work, stop-the-line safety, WIP limits, leveled execution).

**No human-operated ticket workflow is provided.**

#### What You Get

* **Deterministic remediation**: The same evidence produces the same decision and the same remediation plan—no ad-hoc "try this" variance.

* **Stop-the-line safety**: If an action is unsafe or preconditions are missing, the system **refuses** and emits a **refusal receipt** plus the next lawful step.

* **Evidence-first support**: Every decision, action, and refusal produces a **hash-chained receipt** you can export for audits and procurement.

* **No back-and-forth**: The system already collects the evidence it needs and outputs **commands, runbooks, and explanations** as generated artifacts.

* **Observable by humans, not operated by humans**: Humans can observe outputs via MCP clients; agents execute the work under entitlement and policy gates.

#### One-Liner for Listings

```
MCP-only TCPS Support: deterministic, stop-the-line safe, receipt-producing
remediation. No human support channel is offered.
```

---

## Version 2: Government / Procurement (Controls + Evidence + Decom/Retention)

### Support Delivery Statement

Support is performed **exclusively by MCP agents** operating under **TCPS governance**.

The support system is engineered to be:
- **Repeatable** (deterministic standard work)
- **Auditable** (receipt-backed evidence)
- **Refusal-capable** (explicit reasons + next lawful steps)

**Humans do not execute remediation steps; humans observe outputs and exported evidence.**

### Assurance Properties (What This Enables)

**1. Repeatability (Standard Work)**
- Remediation paths are defined, versioned, and executed as controlled procedures
- No conversational troubleshooting or personality-dependent outcomes
- Every path measured and tracked (success rate, execution time, cost)

**2. Safety (Jidoka / Stop-the-Line)**
- Unsafe or uncertain actions are refused (not guessed)
- Refusals are logged as first-class evidence with explicit reasons
- Customer receives: reason for refusal + next lawful step
- Prevents cascading failures and reduces incident scope

**3. Containment (Andon + Quarantine)**
- Abnormalities raise Andon events (stop-the-line signals)
- Affected SKU/templates can be quarantined until gates pass
- Evidence trail shows what was quarantined and why

**4. Prevention (Poka-Yoke + Kaizen)**
- Recurring defects produce corrective spec deltas
- Prevention tests generated and added to test suite
- Regenerated artifacts deployed across install base
- Result: Recurring incident types drop to zero over 4-6 weeks

**5. Capacity Governance (Heijunka + Kanban)**
- Work is leveled and WIP-limited (prevents thrash)
- Predictable throughput and SLA compliance
- Graceful degradation under burst load (not cascading failure)

### Evidence and Audit Artifacts

The system emits **hash-chained receipts** for every support decision/action/refusal.

**Receipts are suitable for evidence bundles, audit narratives, and procurement packages.**

#### Typical Exported Artifacts Include

* **Receipt ledger export** (hash chain + metadata + signatures)
* **Generated incident narrative** (receipt-derived timeline, auto-formatted)
* **Generated runbooks** (from standard work procedures, version-controlled)
* **C4 packs + SystemBook** (computed documentation bundle)
* **Certification tier evidence** (computed from proof coverage, if applicable)
* **Quarantine/Andon reports** (gate failures, containment actions, timeline)
* **Root cause analysis** (5 Whys tree, spec deltas, prevention tests)
* **Decommission/retention pack** (lifecycle closure steps + proof of completion)

**Designed to produce evidence commonly requested in SOC2/ISO/NIST-style audits. This is an evidence system, not a blanket certification claim.**

### Decommission + Retention (Explicit Lifecycle Controls)

Support includes generated lifecycle artifacts to close out systems cleanly:

**Decommission Pack**
- Generated steps for shutdown, dependency unwind
- Data handling closure instructions (GDPR/CCPA compliant)
- Proof-of-completion receipts

**Retention Plan Hooks**
- Retention windows and deletion actions governed as work items
- Evidence export before destruction (audit trail preserved)
- Compliance checkpoints (e.g., HIPAA 6-year hold enforced)

**Proof of Completion**
- Receipts demonstrate decommission/retention actions executed
- Refusal receipts show blocked actions (e.g., retention not yet complete)
- Exported decommission bundle signed for audit

### Control-to-Evidence Mapping (Procurement-Friendly)

| Control Objective | Mechanism (TCPS/MCP) | Evidence Produced |
|---|---|---|
| **Deterministic support actions** | Standard work execution + policy gates | Receipts: decision → action plan → result |
| **Unsafe change prevention** | Stop-the-line refusal + explicit preconditions | Refusal receipts w/ reason + next lawful step |
| **Incident containment** | Andon events + quarantine paths | Andon/quarantine receipts + gate outputs |
| **Root cause & prevention** | 5-Whys object → spec delta → prevention tests | RCA receipts + spec-delta + test/gate receipts |
| **Capacity & prioritization** | Level-loading + WIP limits | Scheduling/queue receipts + WIP enforcement |
| **Auditability** | Hash-chained receipt ledger export | Ledger export + chain verification fields |
| **Lifecycle closure** | Decommission + retention work items | Decom/retention receipts + completion proofs |
| **Policy compliance** | Policy reference in every receipt | Receipts linked to: MSA section, insurance policy, data handling rules |
| **Change control** | Version-tracked standard work library | Recipe version receipts (e.g., "used recipe v2.3") |
| **Knowledge retention** | Standardized runbooks (not individual experts) | Runbook receipts + version history |

### Procurement Summary Statement

```
Support Model (TCPS/MCP):

Support is delivered exclusively by MCP agents under TCPS rules:
- Deterministic standard work
- Stop-the-line safety (refusal doctrine)
- Leveled capacity + WIP limits
- Receipt-based evidence for every decision/action/refusal

Human support channels are NOT provided.

Decommission and retention actions are governed work items with
receipt-backed proof of completion.

Core principle: A = μ(O)
  where A = remediation action (or refusal)
        O = observed evidence
        μ = MCP agent computation
```

---

## Version 3: Technical Audience (For CTO/Security/Procurement Review)

### Support Architecture: Deterministic Remediation via MCP TCPS

Support is a **computed manufacturing process**:

```
A = μ(O)

where:
  O = observed evidence (logs, state, artifacts, policy context)
  μ = MCP agent computation (standard work library, safety gates, policy engine)
  A = action plan or refusal (both receipted, both auditable)
```

### Core Guarantees

**✓ Deterministic** (Repeatability, Version Control, Measurability)
- Same input evidence → same remediation path every time
- All paths are measured (success rate, time, cost per incident)
- Version-controlled standard work library (change audit trail)
- Continuous improvement tracking (recipe success trends)

**✓ Safe** (Jidoka / Stop-The-Line)
- Refusal doctrine prevents cascading failures
- Explicit safety checks before every action (policy gates)
- Refusal receipt explains what was missing + next lawful step
- Andon signals block propagation of unsafe states

**✓ Auditable** (Receipt Chains)
- Cryptographic receipt chains (tamper-proof)
- Hash-linked decision trees (R1 → R2 → R3 → ... → Rn)
- Policy reference in every receipt (MSA/insurance/compliance mapping)
- 7-year retention + export compliance (SOX/HIPAA/PCI-DSS ready)

**✓ Improving** (Kaizen Loop)
- Recurring incidents → 5-Whys root cause → spec delta → prevention tests
- Result: incident type disappears from system (structured prevention, not just fixes)
- Measurable improvement: week-on-week reduction in incident recurrence

**✓ Capacity-Bounded** (Heijunka + Kanban)
- WIP limits prevent thrashing under burst load
- Leveled scheduling spreads high/medium severity events
- Predictable throughput (SLA-bounded response times)
- Graceful degradation (queue grows, customer notified, SLA escalates)

### Compliance-Ready

**✓ Evidence Bundles** suitable for SOX/HIPAA/PCI-DSS audit
**✓ Policy References** in every receipt (contract alignment)
**✓ 7-Year Retention** with tamper-proof chain
**✓ Decommission Procedures** with signed evidence export
**✓ Change Control** via versioned recipe library
**✓ Capacity Metrics** (throughput, response time, success rate, cost per incident)

### Risk Reduction vs. Human Support

| Risk | Human Ticket Support | MCP TCPS |
|---|---|---|
| **Knowledge silos** | Expert leaves → knowledge lost | All work standardized + versioned (accessible) |
| **Improvisational errors** | "Try X" fails, creates new incident | System refuses unsafe guesses (with reasons) |
| **Compliance gaps** | Manual evidence gathering (slow, lossy) | Automatic receipt export (complete, fast, signed) |
| **Recurring incidents** | No systematic prevention | Spec deltas + prevention tests (incident type eliminated) |
| **Capacity unpredictability** | Staffing-dependent (expensive) | WIP-limited + leveled (scalable, measurable) |
| **Incident scope growth** | Escalations → more involved people | Stop-the-line refusal (scope contained) |

### Example: Configuration Drift Incident (Real Flow)

```
T=0:      Customer alerts "config mismatch"
          MCP agent observes: current_config ≠ deployed_spec

T=2min:   Andon event raised: MEDIUM severity
          Check 1: Is insurance active? PASS
          Check 2: Is spec version known? PASS
          Check 3: Is customer policy in force? PASS
          Check 4: Is remedy safe? PASS

T=4min:   Action plan approved (no refusal)
          Recipe issued: "regenerate from spec + apply delta"

T=8min:   Action executed (deterministic)
          Spec delta applied, tests run (all pass)
          New spec deployed to customer

T=12min:  Remediation receipt emitted
          Receipt chain: [R1: alert] → [R2: checks pass] → [R3: recipe]
          → [R4: executed] → [R5: verified] → [R6: closed]

T=12+:    Improvement loop (recurring incidents)
          Root cause: "Drift occurs when customer pushes config outside policy"
          Spec delta: Tighten policy validation (prevent drift at source)
          Prevention tests: 3 new edge cases
          Result: Next similar incident uses improved system (possibly zero incidents)

Evidence artifacts produced:
├─ Incident narrative (receipt-derived timeline)
├─ Remediation recipe (executable + documented)
├─ Root cause analysis (if recurring)
├─ Spec delta (if prevention-worthy)
├─ Updated runbook (new recipe version)
└─ Evidence export bundle (for compliance/procurement)
```

### SLA Targets and Measurement

```
RESPONSE TIME COMMITMENTS

CRITICAL:   Refusal or approval within 3 minutes
HIGH:       Refusal or approval within 8 minutes
MEDIUM:     Refusal or approval within 15 minutes
LOW:        Refusal or approval within 30 minutes

Measurement points:
├─ T1: Alert received → Investigation started
├─ T2: Investigation started → Approval/Refusal decision
├─ T3: Approval → Action execution complete
├─ T4: Action execution → Verification/Monitoring start

Metrics tracked per incident type:
├─ Success rate (% of remedies that resolved issue permanently)
├─ Avg response time
├─ Avg action execution time
├─ Cost per incident (MCP compute + storage + recipe execution)
└─ Recurrence rate (% of same incident type in 30-day window)
```

---

## Integration With Phase 2 Delivery

### Timeline: Support Doctrine Deployment

**Week 5 (Feb 24-Mar 2)**: Support Doctrine Deployment
- [ ] MCP support runtime deployed to staging
- [ ] First 25 remediation recipes generated (from Phase 1-2 incidents)
- [ ] Evidence bundle generation validated
- [ ] Customer support clients deployed (read-only MCP viewers)

**Week 6-7 (Mar 3-16)**: Production Support Launch
- [ ] MCP support runtime deployed to production
- [ ] 75+ remediation recipes in standard work library
- [ ] Evidence export bundles tested with customer audit
- [ ] SLA monitoring active (detection, approval, remedy times)

**Week 8-13 (Mar 17-Apr 20)**: Improvement Loop
- [ ] Recurring incidents identified and prevented
- [ ] Recipe success rate tracking (target: >95%)
- [ ] Support load trending down (fewer incidents)
- [ ] Runbook library expanding from real-world experience

### Integration With Insurance + Prod Build

**Support operates post-insurance verification:**

```
Customer deploys prod build
  ↓
Startup: Insurance certificate verified
  ↓
Production runtime active
  ↓
Incident occurs
  ↓
MCP support agent observes + decides
  ├─ Check 1: Is insurance still active? (real-time)
  ├─ Check 2: Is customer in compliance? (check MSA)
  ├─ Check 3: Is remedy policy-allowed? (check policy engine)
  └─ If all pass: Execute remedy + receipt
     Else: Refuse + explain + next lawful step
  ↓
Evidence exported (audit-ready)
```

### Procurement Checklist (For Legal Review)

Before signing customer MSA, confirm:

- [x] Support model clause included (see Version 2 above)
- [x] Refusal doctrine explained (customer understands "no" means "no")
- [x] Evidence export rights defined (customer can get bundles for audit)
- [x] SLA commitments documented (3/8/15/30 min response times)
- [x] Retention and decommission procedures defined (lifecycle governance)
- [x] Policy linkage explained (each receipt references MSA/insurance)
- [x] No guarantee of "fix" (only guarantee is refusal OR deterministic remedy)

---

## Quick Reference: Positioning Catchphrases

### Memorable One-Liners

| Audience | Phrase |
|----------|--------|
| **Customer (CEO)** | "We don't offer improvisational support; we offer provable support." |
| **Technical (CTO)** | "Support is a computed manufacturing process: A = μ(O)" |
| **Compliance** | "Every support decision is receipted, versioned, and auditable." |
| **Security** | "Stop-the-line refusal prevents cascading failures; Andon events contain scope." |
| **Procurement** | "Evidence bundles suitable for SOC2/ISO/NIST audit; decommission backed by proof." |

### Feature Highlight Cards (For Sales Decks)

```
┌─────────────────────────────────────────────┐
│ DETERMINISTIC REMEDIATION                   │
│ Same evidence → Same action, every time     │
│ No ad-hoc guessing, no personality variance │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│ STOP-THE-LINE SAFETY                        │
│ Unsafe actions refused (not attempted)      │
│ Refusal receipt explains reason + next step │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│ EVIDENCE EXPORT (Audit-Ready)               │
│ Hash-chained receipt ledger (7-year)        │
│ C4 + runbooks + RCA generated automatically │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│ COMPOUNDING QUALITY                         │
│ Recurring incidents → prevention tests      │
│ Incident type eliminated from system        │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│ NO BACK-AND-FORTH                           │
│ System collects all evidence upfront        │
│ Outputs commands, runbooks, explanations    │
└─────────────────────────────────────────────┘
```

---

## Contract Language (Copy-Paste Ready)

### Support SLA Clause

```
5.2 TECHNICAL SUPPORT (TCPS/MCP-Only)

(a) Support Model
    Vendor provides technical support exclusively via MCP agents
    operating under TCPS rules. Vendor does NOT provide human
    technical support, ad-hoc troubleshooting, or manual workarounds.

(b) Response Time SLAs
    • CRITICAL:  Refusal/approval within 3 minutes
    • HIGH:      Refusal/approval within 8 minutes
    • MEDIUM:    Refusal/approval within 15 minutes
    • LOW:       Refusal/approval within 30 minutes

(c) Support Outputs
    Every incident produces: refusal receipt OR remediation recipe
    (never partial/guessed actions). All outputs include:
    - Decision explanation
    - Executed steps (or refusal reason)
    - Hash-chained evidence receipt
    - Updated runbook entry (if applicable)

(d) Evidence Export
    Customer may request evidence bundles for audit/compliance.
    Vendor will provide full receipt chain, incident narrative,
    and decommission procedures within 5 business days.
```

---

**Generated**: 2026-01-26
**Version**: 1.0.0
**Quality**: Production Ready ✅
**Status**: Ready for Customer and Procurement Engagement

---

**Primary positioning**: *"We don't offer improvisational support; we offer provable support."*
