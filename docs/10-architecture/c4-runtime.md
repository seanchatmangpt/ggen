<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [C4 Level 4: Runtime Sequences - Signals, State Transitions, and Receipts](#c4-level-4-runtime-sequences---signals-state-transitions-and-receipts)
  - [Runtime Overview](#runtime-overview)
  - [Sequence Diagram 1: Normal Execution (Happy Path)](#sequence-diagram-1-normal-execution-happy-path)
  - [Sequence Diagram 2: Intervention (Warn → Approval → Execute)](#sequence-diagram-2-intervention-warn-%E2%86%92-approval-%E2%86%92-execute)
  - [Sequence Diagram 3: Storm Handling (Rate Limit Postponement)](#sequence-diagram-3-storm-handling-rate-limit-postponement)
  - [Sequence Diagram 4: Refusal (Policy Block + Scope Violation)](#sequence-diagram-4-refusal-policy-block--scope-violation)
  - [Runtime State Diagram (gen_statem FSM)](#runtime-state-diagram-gen_statem-fsm)
  - [Glossary Cross-Reference](#glossary-cross-reference)
  - [Receipt Contract (Evidence Plane)](#receipt-contract-evidence-plane)
  - [Performance Requirements](#performance-requirements)
  - [Definition of Done](#definition-of-done)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# C4 Level 4: Runtime Sequences - Signals, State Transitions, and Receipts

**Document Purpose**: Define runtime behavior through sequence diagrams showing signal→decision→action→receipt flows, including interventions, storms, and refusals.

**Version**: 1.0 | **Date**: 2026-01-25

---

## Runtime Overview

The autonomic reconciliation engine's behavior is defined by **4 core sequences**:

| Sequence | Trigger | Flow | Receipt |
|----------|---------|------|---------|
| **Normal Execution** | Signal arrives → Policy allows → Action executes | Ingress → FSM → Router → Remediator | success |
| **Intervention** | Signal arrives → Policy requires consent | Ingress → FSM → Intervention handler → Approval → Executor | approved |
| **Storm Handling** | >50 signals/10s OR action quota exceeded | Ingress → Rate limiter → Postponement | postponed |
| **Refusal** | Signal arrives → Policy blocks OR scope violation | Ingress → FSM → Router rejects | refused |

---

## Sequence Diagram 1: Normal Execution (Happy Path)

```mermaid
sequenceDiagram
    actor Signal as Monitoring<br/>(Signal Source)
    participant HTTP as Cowboy<br/>(HTTP Ingress)
    participant FSM as gen_statem<br/>(FSM)
    participant Policy as Policy<br/>(SPARQL)
    participant Router as Action Router
    participant Evidence as Evidence<br/>(Receipt)
    participant Remediator as Remediator<br/>(Actuator)
    participant Firestore as Firestore<br/>(Ledger)

    Signal->>HTTP: POST /signal {signal_type, resource_id, severity}
    HTTP->>HTTP: Validate JWT ✓
    HTTP->>HTTP: Decode JSON ✓
    HTTP->>HTTP: Rate limit check ✓
    HTTP->>HTTP: Aggregate (5s window)
    HTTP-->>Signal: 202 Accepted

    Note over HTTP,FSM: Aggregation window closes after 5s
    HTTP->>FSM: signal_batch_arrived event

    FSM->>FSM: Current state = idle
    FSM->>FSM: Event handler: handle_signal_batch

    FSM->>Policy: Load policy-pack.ttl, SPARQL SELECT
    Policy->>Policy: Evaluate rules sequentially
    Policy-->>FSM: {action_type="iam_binding_update", consent_required=false}

    FSM->>Router: Proposed action + signal context
    Router->>Router: Invariant check (type safety, quota, scope)
    Router->>Router: All constraints satisfied ✓
    Router-->>FSM: Action approved

    FSM->>FSM: State transition: evaluating → executing
    FSM->>Remediator: action_emit: {action_id, action_type, parameters}

    Remediator->>Remediator: Execute: Update IAM binding
    Remediator-->>FSM: action_result: {status=success, affected_resources=1}

    FSM->>Evidence: action_result event
    Evidence->>Evidence: Generate receipt (action_id, timestamp, signal_trace, decision_path)
    Evidence->>Evidence: Load previous receipt, compute hash
    Evidence->>Evidence: Sign receipt (Ed25519)
    Evidence->>Firestore: Write receipt document
    Firestore-->>Evidence: Write acknowledged

    FSM->>FSM: State transition: executing → emitting
    Evidence->>Firestore: Emit receipt_written event
    Evidence-->>FSM: receipt_sent event

    FSM->>FSM: State transition: emitting → idle

    Note over FSM,Firestore: Receipt stored: <br/>action_id=uuid, result=success<br/>hash_chain_link=sha256(prev)<br/>signature=ed25519(receipt)
```

**Timeline**:
- T+0: Signal arrives at HTTP ingress
- T+5s: Aggregation window closes, batch sent to FSM
- T+5.2s: FSM evaluates policy (50ms)
- T+5.25s: Router approves action (10ms)
- T+5.26s: Action sent to remediator
- T+5.5s: Remediator executes, result returned (240ms)
- T+5.55s: Receipt generated and signed (50ms)
- T+5.6s: Firestore write completes (50ms)
- **Total latency**: P99 < 2s (from signal to receipt generation)

---

## Sequence Diagram 2: Intervention (Warn → Approval → Execute)

```mermaid
sequenceDiagram
    actor Signal as Monitoring<br/>(Signal Source)
    participant HTTP as Cowboy<br/>(HTTP Ingress)
    participant FSM as gen_statem<br/>(FSM)
    participant Policy as Policy<br/>(SPARQL)
    participant IntHandler as Intervention<br/>(Handler)
    participant Approval as Approval<br/>(Pub/Sub)
    participant Human as Oncall<br/>(Human)
    participant Executor as Remediator
    participant Evidence as Evidence<br/>(Receipt)
    participant Firestore as Firestore<br/>(Ledger)

    Signal->>HTTP: POST /signal {signal_type, resource_id}
    HTTP->>HTTP: Validate JWT ✓
    HTTP->>HTTP: Aggregate (5s window)
    HTTP-->>Signal: 202 Accepted

    HTTP->>FSM: signal_batch_arrived event
    FSM->>Policy: SPARQL SELECT → policy rules
    Policy-->>FSM: {action_type="security_group_update", consent_required=true}

    FSM->>IntHandler: Action requires consent

    IntHandler->>IntHandler: Emit alert to oncall
    IntHandler->>Approval: Waiting for approval (5min timeout)

    Approval-->>IntHandler: (waiting...)

    Note over Human,Approval: Human reviews alert asynchronously
    Human->>Approval: POST /approve {action_id, approver_id}
    Approval-->>IntHandler: approval_received event

    IntHandler->>FSM: consent_approved event
    FSM->>Executor: action_emit: {action_type, parameters}

    Executor->>Executor: Execute: Update security group
    Executor-->>FSM: action_result: {status=success}

    FSM->>Evidence: action_result event
    Evidence->>Evidence: Generate receipt (intervention_type=warn, consent_result=approved)
    Evidence->>Evidence: Compute hash_chain_link
    Evidence->>Evidence: Sign receipt (Ed25519)
    Evidence->>Firestore: Write receipt document

    Note over Firestore,Evidence: Receipt stored:<br/>action_id=uuid<br/>intervention_type="warn"<br/>consent_result="approved"<br/>consent_timestamp=(human approval time)<br/>signature=ed25519(receipt)
```

**Timeline**:
- T+0: Signal arrives
- T+5s: Aggregation window closes
- T+5.2s: Policy indicates consent required
- T+5.3s: Intervention handler activates, alert sent to oncall
- T+30s (example): Human approves via Pub/Sub
- T+30.1s: FSM transitions to executing
- T+30.5s: Remediator executes action
- T+30.6s: Receipt generated (includes human approval timestamp)
- **Key metric**: Consent latency (human approval time) recorded in receipt

---

## Sequence Diagram 3: Storm Handling (Rate Limit Postponement)

```mermaid
sequenceDiagram
    actor Signals as 100+ Signals<br/>(Signal Storm)
    participant HTTP as Cowboy<br/>(HTTP Ingress)
    participant Limiter as Rate Limiter
    participant Buffer as Signal<br/>(Buffer)
    participant Postpone as Postponement<br/>(Handler)
    participant FSM as gen_statem<br/>(FSM)
    participant PubSub as Pub/Sub<br/>(Topic)
    participant Evidence as Evidence<br/>(Receipt)
    participant Firestore as Firestore<br/>(Ledger)

    loop Multiple Signals in <10s
        Signals->>HTTP: POST /signal (rapid fire)
        HTTP->>Limiter: Rate limit check
    end

    Note over Limiter,Buffer: Token bucket depleted after 50 signals/10s

    Signals->>HTTP: POST /signal #51 (exceeds quota)
    HTTP->>Limiter: Rate limit check FAILED
    Limiter->>Postpone: Storm detected (>50 signals/10s)

    Postpone->>Postpone: Extract pending signals (47 accepted, 3 refused)
    Postpone->>Postpone: Compute bounded action set (limit=100 actions/min)
    Postpone->>Buffer: Preserve signal queue state
    Postpone->>Postpone: Calculate postponement duration (5s until quota refills)

    Postpone->>PubSub: Publish storm_detected event
    Note over Postpone,PubSub: Alert: 50+ signals in 10s,<br/>postponing execution for 5s

    HTTP-->>Signals: 503 Service Unavailable (retry after 5s)

    Note over Limiter,Buffer: Tokens refill over 5s (1 token/sec)

    Postpone->>Buffer: Reprocess buffered signals
    Buffer->>FSM: signal_batch_arrived (consolidated batch)

    FSM->>FSM: Evaluate consolidated batch
    FSM->>FSM: Apply bounded action limit (100 actions/min max)
    FSM->>FSM: Determine final action set

    FSM->>Evidence: Storm mitigation receipt generation
    Evidence->>Evidence: Serialize storm context<br/>{signals_accepted=47, signals_refused=3,<br/>postponement_duration_ms=5000,<br/>drop_reason="quota_exceeded"}
    Evidence->>Evidence: Compute hash_chain_link
    Evidence->>Evidence: Sign receipt (Ed25519)
    Evidence->>Firestore: Write storm receipt

    Note over Firestore,Evidence: Storm Receipt stored:<br/>action_id=uuid<br/>event_type="storm_mitigation"<br/>signal_count=50<br/>signals_batched=47<br/>signals_dropped=3<br/>drop_reason="bounded_action_limit"<br/>postponement_duration_ms=5000
```

**Key Points**:
- **Storm Detection**: Triggered when >50 signals arrive in <10s (configurable threshold)
- **Bounded Action**: Limit to 100 actions/minute (prevents resource exhaustion)
- **Signal Preservation**: Dropped signals logged in receipt (auditable)
- **Postponement Duration**: 5s (tokens refill at 1/sec, quota = 100)
- **Receipt Includes**: Drop reason, signal counts, postponement duration, reprocessing time

**Storm Metrics** (visible in Prometheus):
- Counter: `gov_storm_detections_total{reason}` (incremented per storm)
- Counter: `gov_signals_dropped_total{reason}` (count of dropped signals)
- Gauge: `gov_signal_queue_depth` (current backlog)
- Histogram: `gov_postponement_duration_seconds` (distribution of postponement times)

---

## Sequence Diagram 4: Refusal (Policy Block + Scope Violation)

```mermaid
sequenceDiagram
    actor Signal as Monitoring<br/>(Signal Source)
    participant HTTP as Cowboy<br/>(HTTP Ingress)
    participant FSM as gen_statem<br/>(FSM)
    participant Policy as Policy<br/>(SPARQL)
    participant Router as Action Router
    participant Invariant as Invariant<br/>(Checker)
    participant Evidence as Evidence<br/>(Receipt)
    participant Firestore as Firestore<br/>(Ledger)

    Signal->>HTTP: POST /signal {action=update_iam, sku_requested="admin_tier"}
    HTTP->>HTTP: Validate JWT (entitlement: sku="standard_tier") ✓
    HTTP->>HTTP: Aggregate (5s window)
    HTTP-->>Signal: 202 Accepted

    HTTP->>FSM: signal_batch_arrived event
    FSM->>Policy: SPARQL SELECT → policy rules
    Policy-->>FSM: {action_type="iam_binding_update"}

    FSM->>Router: Proposed action + signal context

    Router->>Invariant: Type and constraint check
    Invariant->>Invariant: Check: Is action within sku_scope?
    Invariant->>Invariant: Entitlement.scope="standard_tier"<br/>Action.requires="admin_tier" capability
    Invariant-->>Router: FAILED (scope violation)

    Router->>Router: Action REFUSED
    Router-->>FSM: Refusal (reason="outside_sku_scope")

    FSM->>Evidence: Refusal event

    Evidence->>Evidence: Generate refusal receipt<br/>{action_type="iam_binding_update",<br/>result="refused",<br/>refusal_reason="action_requires_admin_tier_sku",<br/>entitlement_scope="standard_tier"}
    Evidence->>Evidence: Compute hash_chain_link (no action taken)
    Evidence->>Evidence: Sign receipt (Ed25519)
    Evidence->>Firestore: Write refusal receipt

    FSM->>FSM: State transition: deciding → idle (no execution)

    Note over Firestore,Evidence: Refusal Receipt stored:<br/>action_id=uuid<br/>result="refused"<br/>refusal_reason="outside_sku_scope"<br/>entitlement_token_hash=sha256(jwt)<br/>signature=ed25519(receipt)
```

**Refusal Types**:

| Refusal Reason | Root Cause | Example |
|----------------|-----------|---------|
| `policy_blocked` | Policy rule returns deny | Security policy: cannot update IAM binding when event_type="compliance_override" |
| `outside_sku_scope` | Action requires capability not in SKU | Action requires "admin_tier", entitlement is "standard_tier" |
| `quota_exceeded` | Resource limit reached | Action would exceed 1000 rules/resource limit |
| `missing_required_field` | Signal missing required field | signal_type missing, cannot determine action |
| `permission_denied` | Principal unauthorized | Service account lacks compute.instances.update permission |

**Receipt Structure for Refusal**:
```json
{
  "action_id": "uuid-refusal-001",
  "timestamp": "2026-01-25T14:35:00Z",
  "signal_trace": ["sig-001"],
  "decision_path": [
    {"rule": "policy/scope/within-sku", "result": false, "matched": true}
  ],
  "action_proposed": "iam_binding_update",
  "result": "refused",
  "refusal_reason": "action_requires_admin_tier_sku",
  "entitlement_context": {
    "sku_scope": "standard_tier",
    "required_scope": "admin_tier"
  },
  "hash_chain_link": "sha256(previous_receipt)",
  "signature": "ed25519(...)"
}
```

---

## Runtime State Diagram (gen_statem FSM)

```
┌─────────────────────────────────────────────────────────────┐
│ gen_statem FSM - Deterministic State Transitions             │
└─────────────────────────────────────────────────────────────┘

                        ┌─────────┐
                        │  START  │
                        └────┬────┘
                             │
                             ↓
                        ┌─────────┐
                        │  idle   │◄──────┐ (state=init)
                        └────┬────┘       │
                             │           │
                   (signal_batch_arrived) (receipt_sent)
                             │           │
                             ↓           │
                      ┌──────────────┐   │
                      │  evaluating  │   │
                      └────┬─────────┘   │
                           │             │
                 (policy_evaluated)      │
                           │             │
                           ↓             │
                      ┌──────────────┐   │
                      │  deciding    │   │
                      └────┬─────────┘   │
                           │             │
                    (decision_made)      │
                           │             │
            ┌──────────────┴──────────────┐
            │                             │
            ↓                             ↓
      ┌──────────────┐            ┌──────────────┐
      │  executing   │            │ refused      │
      │  (or await)  │            │ (terminal)   │
      └────┬─────────┘            └──────┬───────┘
           │                             │
     (action_result)                     │
           │                             │
           ↓                             │
      ┌──────────────┐                   │
      │  emitting    │                   │
      └────┬─────────┘                   │
           │                             │
      (receipt_sent)                     │
           │                             │
           └─────────────┬───────────────┘
                         │
                         ↓
                    ┌─────────┐
                    │  idle   │ (ready for next signal)
                    └─────────┘
```

**States**:
- **idle**: Waiting for signal_batch_arrived event
- **evaluating**: Loading policy, executing SPARQL queries
- **deciding**: Evaluating rules, making decision (approve/refuse/intervention)
- **executing**: Routing action to remediator (or waiting for consent if intervention)
- **emitting**: Generating receipt, writing to Firestore
- **refused**: Terminal state (no action taken, receipt generated)

**Event Handlers** (deterministic):
- `signal_batch_arrived`: idle → evaluating (always)
- `policy_evaluated`: evaluating → deciding (always)
- `decision_made`: deciding → executing (or to refused if policy blocks)
- `action_result`: executing → emitting (always)
- `receipt_sent`: emitting → idle (always)

---

## Glossary Cross-Reference

- **gen_statem**: Erlang OTP finite state machine behavior (deterministic transitions)
- **Signal Aggregation**: Windowing (5s) + deduplication to reduce noise
- **Token Bucket**: Rate limiter data structure (capacity tokens, refill 1/sec)
- **Storm**: >50 signals in <10s (triggers postponement + jidoka enforcement)
- **Bounded Action**: Limit to 100 actions/minute (resource protection)
- **Intervention**: Warn → human approval → execute flow
- **Jidoka**: Stop-the-line enforcement (refuse action if policy blocks or evidence fails)
- **Hash-Chain**: Merkle-linked receipts (each includes SHA256 of prior)
- **Receipt**: Cryptographically signed proof of action or refusal

---

## Receipt Contract (Evidence Plane)

**Receipts Generated**:

| Sequence | Result | Receipt.result | Receipt.intervention_type |
|----------|--------|---------------|-----------------------------|
| Normal Execution | Action executed | success | none |
| Intervention | Approval given, action executed | success | warn |
| Intervention | Approval refused/timeout | cancelled | warn |
| Storm Handling | Signals postponed | postponed | storm |
| Refusal | Policy blocks | refused | none |

**Receipt Metadata Always Includes**:
- action_id (UUID)
- timestamp (ISO 8601 UTC)
- signal_trace (List[String])
- decision_path (List[Rule Evaluation])
- result (success | failure | refused | postponed | cancelled)
- hash_chain_link (SHA256 of previous receipt, or null)
- signature (Ed25519, cryptographic proof)
- entitlement_context (token_hash, principal, sku_scope)

---

## Performance Requirements

| Metric | SLO |
|--------|-----|
| Signal ingestion latency (HTTP to 202) | <100ms, P99 |
| Aggregation window | 5s (fixed) |
| Policy evaluation (SPARQL) | <50ms, P99 |
| Router invariant check | <10ms, P99 |
| Receipt generation + signing | <50ms, P99 |
| Firestore write | <1s, P99 |
| **Total signal-to-receipt** | **<2s**, P95 |
| Storm detection threshold | >50 signals/10s |
| Bounded action limit | 100 actions/minute |
| Intervention consent timeout | 5 minutes |

---

## Definition of Done

- [ ] Sequence diagram 1: Normal execution (9 actors, 20 messages)
- [ ] Sequence diagram 2: Intervention flow (10 actors, 18 messages)
- [ ] Sequence diagram 3: Storm handling (8 actors, 19 messages)
- [ ] Sequence diagram 4: Refusal path (7 actors, 13 messages)
- [ ] All 4 diagrams render in Mermaid without syntax errors
- [ ] gen_statem FSM state diagram shows all 6 states + transitions
- [ ] Event handlers defined (deterministic mapping of event → state transition)
- [ ] Receipt structure for each sequence type documented
- [ ] Timeline with latency milestones for all 4 sequences
- [ ] Storm detection logic: threshold, postponement duration, bounded actions
- [ ] Intervention consent: timeout (5min), approval method (Pub/Sub), alert mechanism
- [ ] Refusal reasons: 5 types listed with examples
- [ ] Performance SLOs: All latencies and thresholds specified
- [ ] Jidoka enforcement: Stop-the-line rules articulated
- [ ] Hash-chain: Merkle linking explained in all sequences
- [ ] Receipt generation: Signing, Firestore write, Cloud Logging mirror documented

---

**Next**: See [evidence-plane.md](evidence-plane.md) for Level 5 evidence plane architecture (separation from data/control planes).
