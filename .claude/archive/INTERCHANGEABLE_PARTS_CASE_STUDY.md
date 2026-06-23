# Interchangeable Parts for Enterprise Operations: OCPQ Case Study

**Date**: 2026-05-27  
**Strategic Insight**: WASM + AtomVM + Construct Law = Industrial-Grade Interchangeable Execution Parts  
**Case Study**: OCPQ Order Management — "Third Reminder → No Later Confirmation"

---

## The Thesis

### Before Interchangeable Parts

Enterprise systems are **vertically integrated artisanal systems**:
- Every workflow handcrafted
- Every integration custom
- Every runtime mutable
- Every change requires human reconfiguration

### After Interchangeable Parts

Enterprise operations are **modular, sealed, swappable execution components**:
- Sealed WASM execution cells (deterministic, sandboxed, portable)
- Coordinated by AtomVM assembly line (actor isolation, supervision, self-healing)
- Governed by Construct Law (admission/refusal, proof, receipt)

---

## The Technical Stack

### Layer 1: WASM (Precision Machining)

| Property | Interchangeable-Parts Effect |
|----------|------------------------------|
| Deterministic execution envelope | Predictable operational tolerances |
| Sandboxed runtime | Component isolation |
| Portable binary format | Replaceable operational cells |
| Compilable from many languages | Manufacturing flexibility |
| Small sealed artifacts | Swappable execution units |
| Capability-style interfaces | Controlled boundary surfaces |

### Layer 2: AtomVM (Assembly Line)

| Property | Industrial Effect |
|----------|-------------------|
| Actor isolation | Failure containment |
| Message passing | Standardized coupling geometry |
| Supervision trees | Automated operational maintenance |
| Lightweight concurrency | Massive cell scaling |
| Distributed execution | Factory-scale orchestration |
| Fault recovery | Self-healing assembly lines |

### Layer 3: Construct Law (Legal Framework)

| Property | Governance Effect |
|----------|-------------------|
| Genesis kernel (pure computation) | Deterministic consequence |
| Admission/refusal protocol | Only lawful operations executed |
| Receipt + proof | Cryptographic evidence trail |
| Replay + determinism | Verification and auditability |
| Object-centric event logs (OCEL) | Multi-object causal consistency |
| BLAKE3 chains | Tamper-proof artifact binding |

---

## Case Study: OCPQ (Order, Customer, Payment, Quote)

### The Constraint

**After a third payment reminder for an order is sent to a customer, no other order by that customer should be confirmed.**

In formal terms:
```
third_reminder(customer, order_a, t1)
AND
order_confirmed(customer, order_b, t2)
AND
t2 > t1
⇒ NON-CONSTRUCTIBLE (violation of law)
```

### Industrial Implementation: Sealed Execution Cells

Each operation becomes a **sealed WASM cell supervised by an AtomVM actor**:

| Operation | WASM Cell | AtomVM Supervisor | Bound Objects |
|-----------|-----------|-------------------|---------------|
| Send payment reminder | `SendPaymentReminder` | `ReminderSupervisor` | customer, order_a, reminder |
| Check customer risk | `CustomerRiskGate` | `CustomerLifecycleSupervisor` | customer, source order, third reminder |
| Confirm order | `ConfirmOrder` | `OrderSupervisor` | customer, order_b |
| Emit evidence | `ReceiptEmitter` | `EvidenceSupervisor` | all objects, hook, artifact, epoch |

### OCEL Evidence Trail

Each cell emits **object-centric event log entries**:

```json
{
  "ocel:eventID": "reminder_sent_001",
  "ocel:eventType": "payment_reminder_sent",
  "ocel:timestamp": "2026-05-27T10:15:00Z",
  "ocel:objectIDs": {
    "customer": "cust_789",
    "order": "order_a_123",
    "reminder": "reminder_3_of_3"
  },
  "ocel:attributes": {
    "ggen.source_cell": "SendPaymentReminder",
    "construct.epoch": "v26.5.25-CONVERGED",
    "construct.law": "K-P09",
    "blake3.hash": "hash_of_reminder_event"
  }
}
```

And when a later confirmation is attempted:

```json
{
  "ocel:eventID": "order_confirm_attempt_001",
  "ocel:eventType": "order_confirmation_attempted",
  "ocel:timestamp": "2026-05-27T10:20:00Z",
  "ocel:objectIDs": {
    "customer": "cust_789",
    "order": "order_b_456"
  },
  "ocel:attributes": {
    "ggen.source_cell": "ConfirmOrder",
    "construct.epoch": "v26.5.25-CONVERGED",
    "construct.law": "K-P09",
    "causal_predecessor": "reminder_sent_001",
    "constraint_check": "VIOLATED — third reminder already sent for this customer"
  }
}
```

### The Impossibility Layer: Why Forging Is Infeasible

To forge a valid `order_confirmed` after the third reminder, an attacker cannot simply forge one log entry. They must simultaneously counterfeit:

1. **The reminder event** (from SendPaymentReminder cell)
   - Hash of event itself
   - Object binding to customer + order_a + reminder_3
   - Timestamp consistency

2. **The customer object state**
   - Customer record before third reminder
   - Customer record after third reminder
   - Causal link to reminder event

3. **The source order object state**
   - Order_a state at reminder time
   - Immutable BLAKE3 binding

4. **The other order object state**
   - Order_b state at confirmation time
   - Temporal consistency (can't predate reminder)

5. **The ConfirmOrder WASM cell artifact**
   - Binary hash
   - Signature proof
   - Ed25519 verification

6. **The AtomVM actor envelope**
   - Supervision message history
   - Actor state at decision time
   - Restart recovery (if any)

7. **The Construct epoch and law version**
   - K-P09 rule set
   - Admission criterion
   - Genesis kernel version

8. **The entire OCEL object-centric path**
   - All object state transitions
   - All event causality links
   - All BLAKE3 bindings

9. **All OTEL observability traces**
   - Span IDs connecting across cells
   - Attribute consistency (customer ID, order IDs match)
   - Latency plausibility

And **all forged components must still satisfy the OCPQ constraint**:

```text
third_reminder(customer, order_a, t1)
AND
order_confirmed(customer, order_b, t2)
AND
t2 > t1
⇒ must NOT construct under law

Consequence: REFUSAL (not acceptance)
```

### Why This Kills Incumbent SaaS

**Incumbent systems** (Salesforce, NetSuite, etc.) are:
```
ticket row + order row + audit log + maybe a signature
```

**Construct systems** with interchangeable parts are:
```
independent WASM cells
+ AtomVM actor envelopes
+ OCEL object bindings
+ BLAKE3 cryptographic chains
+ Ed25519 signatures on cells
+ Construct law enforcement
+ Epoch + law-version tracking
+ Causal consistency proof
```

To forge a payment confirmation in Salesforce: corrupt one record in one table.

To forge it in Construct with sealed cells: counterfeit 9 independent systems AND preserve object-level consistency AND satisfy law constraints.

**The attacker surface expanded from 1 target (database) to 9+ systems (WASM, actors, OCEL, crypto, law, epoch, traces).**

---

## Implementation Roadmap: OCPQ as Phase 5+ Blueprint

### Phase 5 Wave 3: Core Cells (6 weeks)

**Implement the 4 sealed cells**:

1. **SendPaymentReminder cell** (WASM + Erlang template)
   - Input: customer ID, order ID, reminder count
   - Output: reminder event with timestamp + hash
   - Tests: Reminder count increments, timestamp monotonic, OCEL event emitted

2. **CustomerRiskGate cell** (WASM + Erlang)
   - Input: customer ID, source order ID, reminder count
   - Output: risk verdict (allow/restrict)
   - Tests: After 3rd reminder, returns RESTRICT for that customer

3. **ConfirmOrder cell** (WASM + Erlang)
   - Input: customer ID, order ID
   - Output: confirmation receipt (or REFUSAL)
   - Tests: Refuses if customer has 3+ reminders on other orders

4. **ReceiptEmitter cell** (WASM + Erlang)
   - Input: event objects, hook result, artifact hash
   - Output: OCEL event + cryptographic receipt
   - Tests: BLAKE3 hash, Ed25519 signature, object binding

### Phase 5 Wave 4: Supervision & Orchestration (4 weeks)

**Wire cells into AtomVM supervision tree**:

1. **ReminderSupervisor actor**
   - Spawns SendPaymentReminder cells
   - Tracks reminder count per customer
   - Supervises with `:one_for_one` restart strategy

2. **CustomerLifecycleSupervisor actor**
   - Monitors customer risk state
   - Invokes CustomerRiskGate for decisions
   - Maintains causal links to reminder history

3. **OrderSupervisor actor**
   - Spawns ConfirmOrder cells
   - Validates against CustomerRiskGate
   - Emits receipts via ReceiptEmitter

4. **EvidenceSupervisor actor**
   - Collects OCEL events from all cells
   - Maintains BLAKE3 chain of receipts
   - Exposes for external audit (process mining)

### Phase 5 Wave 5: Law & Proof (3 weeks)

**Encode OCPQ constraint in Construct Law**:

1. **Admission rule in K-P09**:
   ```
   order_confirmed(customer, order_b, t2)
   :-
   NOT exists (
     third_reminder(customer, order_a, t1)
     AND t2 > t1
   )
   ```

2. **Receipt validation**:
   - Proof gates verify OCEL object consistency
   - Causal chain proves reminder → decision → confirmation
   - Ed25519 signatures validate cell authenticity

3. **Replay & determinism**:
   - Same customer + reminder history → always same gate verdict
   - BLAKE3 hash of event log proves immutability
   - OTEL traces show no hidden mutations

---

## Why This Matters for the Charter

The OCPQ case study **proves the thesis at operational scale**:

1. **Interchangeable parts work** — Each cell is independent, swappable, testable
2. **Sealed execution is feasible** — WASM gives determinism + isolation
3. **Law-based governance scales** — K-P09 admission control handles complex multi-object constraints
4. **Forgery becomes infeasible** — 9-layer attack surface is harder than 1-layer database

This transforms enterprise software from:

```text
"mutable configuration 
+ human judgment 
+ audit trail"
```

To:

```text
"sealed cells 
+ deterministic law 
+ cryptographic proof"
```

**That is the charter's endgame**: Construct Law as the operational substrate for sealed, interchangeable execution parts.

---

## Next Steps

**Phase 5 Wave 2 Planning** should consider:

1. **Prioritize OCPQ as proof-of-concept** (6-week implementation)
2. **Map existing dormant cells** to OCPQ operations (SendPaymentReminder, CustomerRiskGate, etc.)
3. **Design reusable cell templates** (WASM scaffolds, Erlang supervisor patterns)
4. **Build law-checking infrastructure** (OCPQ constraint as example, generalize)

**This turns the capability inventory (44 operations) into interchangeable-parts catalog** — each capability becomes a sealed, supervisable, law-governed execution component.

The thesis: **WASM + AtomVM + Construct Law = industrial-grade enterprise operations**. OCPQ proves it works.
