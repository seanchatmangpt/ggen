# Crown Formal

This standalone Lean 4 and Mathlib project contains the formal trace kernel,
its operational preservation theorem, decisive countermodels, and an
observation-factored behavioral adequacy boundary.

## 1. Exact abstract crown

The canonical quotient classifier satisfies

```text
classify I left = classify I right ↔ TraceEq I left right
```

without assuming the result. For an arbitrary compiler, the same kernel
statement is equivalent to two separately named algorithm-correctness
obligations:

1. **trace soundness** erases accidental serialization;
2. **trace reflection** refuses to merge distinct trace classes.

This is an exact theorem about action words and the selected independence
relation. It is not automatically a theorem about a richer native behavior
record.

## 2. Operational and proof-relevant bridge

`DiamondCertificate` is stated over the deterministic partial replay function.
`run_trace_eq` lifts a two-step operational diamond through every constructor
of Mazurkiewicz trace equivalence.

`Execution` retains each transition equality as a proof-relevant receipt.
`Execution.of_run_eq` reifies every successful replay, and
`transport_execution` manufactures a receipt derivation for every admitted
serialization while retaining the exact final state.

Both manufacturers are definitions because they return `Execution ... : Type`.
They are not theorem declarations: Lean reserves `theorem` for proposition-valued
results. A clean build of the merged #466 source exposed and this branch repairs
that declaration-sort defect.

## 3. Abstract semantic preservation and admitted standing

`SemanticIndependenceCertificate` transports the abstract word-level
lawfulness conjunction:

- successful transition replay;
- final goal satisfaction;
- precondition preservation;
- invariant preservation;
- numeric-flow compatibility;
- temporal-word preservation;
- trajectory-word preservation.

The last two fields are deliberately described as word-level projections. They
are not a direct native schedule or state-trajectory model.

The mathematical preservation proof is not itself sufficient for admitted
standing. `AdmittedSemanticCrown` additionally requires:

- `SemanticBoundaryReceipt`, including goal standing and all five word-indexed
  semantic layers;
- `OperationalBoundaryReceipt`, carrying a successful concrete independent
  pair and both replay receipts.

The additive model activates every semantic component with acceptance and
rejection witnesses and transports both lawfulness and a proof-relevant
execution.

## 4. Adequacy boundary: why the event-only behavioral statement fails

`Adequacy.lean` proves two independent exclusions.

### Timestamp countermodel

Two behavior records may have identical event lists and therefore be
event-trace-equivalent while carrying different timestamps. A temporal
lawfulness predicate can accept one and reject the other. Therefore a theorem
quantified over two arbitrary behavior records cannot preserve full lawfulness
when its premise relates only their event projections.

The repaired timed model keys timestamps by unique event identity instead of
list position. An admitted commutation therefore reorders event identities
without silently reassigning their timestamps.

### Trajectory countermodel

The additive actions `[1, 3]` and `[3, 1]` reach the same final state but induce
different state traces:

```text
[0, 1, 4]
[0, 3, 4]
```

A trajectory observation can distinguish them. Final-state diamond equality is
therefore insufficient for arbitrary native trajectory constraints.
`AdditiveTrajectoryRelated` includes equality of the complete observed state
trace and correctly refuses this swap.

## 5. Observation-factored behavioral crown

The previous behavioral boundary carried a field equivalent to the theorem it
claimed to prove:

```text
related left right → (lawful left ↔ lawful right)
```

That made the trace projection logically decorative. The refactored
`CrownObservationAdmission` removes that field.

Native lawfulness must now factor pointwise through:

```text
(classify I (events behavior), observe behavior)
```

A related pair must prove:

1. event trace equivalence;
2. equality of the admitted observation;
3. equivalence-relation laws for the native relation.

The derived `lawfulIff` theorem consumes `traceSound`, `classify_eq_iff`,
`observationEq`, and `lawfulFactorization`. It no longer assumes pairwise
lawfulness preservation.

`AdmittedBehavioralCrown` additionally carries a related pair with distinct
event serializations. This excludes an empty relation and an equality-only
relation from receiving non-vacuous crown standing.

The concrete timed instance includes the genuine swap:

```text
[false, true] ↔ [true, false]
```

with the same identity-keyed timestamp map. `timed_repaired_crown` derives
lawfulness preservation from trace classification and observation equality.

## 6. Exact standing

- **Abstract quotient crown:** proved in source.
- **Abstract replay and word-level lawfulness preservation:** proved in source.
- **Proof-relevant execution declaration sort:** repaired from invalid
  proposition-only `theorem` syntax to `def`.
- **Proof-relevant execution transport:** implemented in source.
- **Abstract non-vacuity receipts:** load-bearing through
  `AdmittedSemanticCrown`.
- **Event-only behavioral crown over arbitrary timestamps:** refuted by a
  kernel-visible countermodel.
- **Pairwise-lawfulness admission schema:** removed as circular.
- **Observation-factored behavioral crown:** implemented in source.
- **Nontrivial behavioral relation receipt:** implemented in source.
- **Direct adapter to the external MFW `BehaviorTrace` and PDDL 3.1 model:** not
  claimed by this standalone project; it still requires explicit schedule and
  state-trajectory observations in the owning theory.
- **Executable canonical normal form for trace classes:** not claimed; the
  current canonical classifier is the mathematical quotient.
- **Lean kernel standing for the current head:** determined only by the latest
  workflow receipt, never inferred from source inspection.

## 7. Verification commands

```bash
python3 scripts/audit.py
lake update
lake exe cache get
lake build
```

The project is pinned to Lean 4.30.0 and Mathlib v4.30.0.