# ggen v26.7.31 — Fully Automatic and Autonomic Operations

## Status

`UNKNOWN` until the dedicated execution crown completes on the exact candidate SHA.

This document defines the next Gall checkpoint above the RWR Level-5 and Fortune-5 foundations introduced by PR #507. It does not broaden authority. It mechanizes unattended operation over the existing exact-grant, atomic-actuation, receipt, and replay rail.

## Operating equation

```text
O → O* → I → G → A → R → O'
```

- `O`: partial external observation or trigger.
- `O*`: bounded admitted trigger.
- `I`: deterministically manufactured intent.
- `G`: authority derived for exactly one action and payload.
- `A`: atomic artifact-plus-receipt actuation.
- `R`: causal automatic-operation receipt.
- `O'`: re-observed consequence.

An operation is not successful because a command returned zero. It is successful only when the consequence is re-observed and bound into a verifying receipt.

## Automatic versus autonomic

**Automatic operation** executes a predetermined transition after an admitted trigger.

**Autonomic operation** monitors current state, compares it with desired state, manufactures a bounded repair trigger, executes through the same automatic rail, re-observes state, accumulates knowledge, and either converges or returns a typed refusal.

Neither path may create authority, mutate policy, or bypass the actuator.

## Crown contract

The release contract is conjunctive across 15 capabilities:

1. Trigger admission.
2. Deterministic routing.
3. Intent manufacturing.
4. Exact grant derivation.
5. Atomic actuation.
6. Consequence verification.
7. Idempotency.
8. Bounded retry.
9. Circuit breaking.
10. Error-budget gating.
11. Andon stop-the-line.
12. Lawful rollback.
13. Bounded autonomic repair.
14. Causal receipt and replay protection.
15. Knowledge-hook emission.

Every capability requires three independent surfaces:

- positive execution;
- named negative refusal;
- receipt verification and duplicate-replay refusal.

```text
15 capabilities × 3 surfaces = 45 crown obligations
```

No weighted average is permitted. One open capability prevents `ALIVE`.

## Best-practice constraints

### Zero unreceipted actuation

All normal actions, repairs, and lawful inverses use `FoundationMachine` grants and `FilesystemActuator` atomic commits. Recovery has no alternate side-effect lane.

### Deterministic control plane

Routes are finite and unique by trigger kind. Intents are derived from trigger identity, source sequence, route identity, desired state, and optional inverse state. Retry backoff is expressed in logical ticks; the verifier does not depend on wall-clock sleeps.

### Fail-closed admission

Unsafe identities, changed payloads, unknown trigger kinds, duplicate routes, zero sequences, payload overflow, and sequence overflow are typed refusals.

### Explicit postconditions

Actuation receipts prove that bytes were committed. A consequence observer independently reads the committed artifact and compares its digest with the intended state. Promotion requires both proofs.

### Idempotency before execution

An admitted trigger digest is consumed before actuation. The same trigger cannot repeat after success, uncertainty, or rollback. A new attempt requires a new trigger identity or sequence.

### Monotonic Andon

Severity can move only from `GREEN` to `YELLOW` to `RED`. `RED` refuses new work. De-escalation requires a future separately authorized recovery mechanism and is deliberately unavailable through the execution API.

### Bounded retry and circuit breaking

Transient observations retry only within an explicit attempt budget. Repeated postcondition failures increment a circuit breaker. An open circuit fails closed and escalates Andon to red.

### Error-budget gating

Each successful risky operation consumes explicit budget units. Operations without sufficient budget are refused before trigger admission or actuation.

### Lawful rollback

Rollback is not filesystem deletion or an untracked compensating command. When an inverse exists, the inverse becomes a new exact action with its own grant, atomic commit, and receipt.

### Bounded autonomics

The autonomic controller has a hard cycle bound. It never invents a transition. It emits repair triggers already represented in the deterministic router and terminates with convergence or a typed refusal.

### Causal receipts and hooks

Automatic receipts bind trigger, route, intent, grant, actuation, postcondition, retry history, predecessor, and knowledge hook. Hooks carry meaning but cannot actuate.

## Failure states

The implementation refuses, among other cases:

- malformed or changed trigger payloads;
- unsupported trigger kinds;
- duplicate trigger identities;
- duplicate routes;
- changed action payloads after grant derivation;
- duplicate action identities;
- exhausted retries;
- permanent observation failure;
- open circuits;
- depleted error budgets;
- execution under Andon red;
- Andon de-escalation;
- failed postconditions without a verified consequence;
- zero autonomic cycle bounds;
- tampered automatic or autonomic receipts;
- duplicate receipt replay;
- malformed or causally detached knowledge hooks.

## Ontology-first manufacturing

`packs/automatic-autonomic-operations-pack` defines the 15 canonical capability identities, order, category, outcome, implementation artifact, verifier command, named falsifier, and all 45 capability/surface edges.

`ggen sync run` manufactures an independent consumer containing:

- a canonical capability registry;
- a real automatic trigger-to-consequence verifier;
- a real autonomic repair verifier;
- positive and negative tests;
- generated capability documentation;
- a non-self-promoting release declaration;
- a verifier script;
- independent CI.

The release declaration remains `UNKNOWN`. Only executed tests and the generated verifier report may produce `ALIVE`.

## Required verification ladder

1. Strict workspace formatting.
2. Direct automatic/autonomic integration fixtures.
3. Preservation of the RWR Level-5 crown.
4. Preservation of the Fortune-5 crown.
5. Build of the real `ggen` actuator.
6. Ontology validation gates.
7. Two byte-identical generation passes.
8. Generated consumer tests.
9. `ggen receipt verify`.
10. Generated `ALIVE / 15 / 45` report.

## Scope boundary

This release proves the bounded filesystem reference implementation. It does not claim universal production autonomy, distributed consensus, remote deployment authority, or production-load SLO evidence. Those require later Gall checkpoints with their own real boundaries and receipts.
