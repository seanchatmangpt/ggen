# Crown Formal

This standalone Lean 4 and Mathlib project contains the formal trace kernel,
its operational preservation theorem, decisive countermodels, and the repaired
behavioral admission boundary.

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

## 3. Semantic preservation and admitted standing

`SemanticIndependenceCertificate` transports the abstract lawfulness
conjunction:

- successful transition replay;
- final goal satisfaction;
- precondition preservation;
- invariant preservation;
- numeric-flow compatibility;
- temporal preservation;
- trajectory preservation.

The mathematical preservation proof is not itself sufficient for admitted
standing. `AdmittedSemanticCrown` additionally requires:

- `SemanticBoundaryReceipt`, including goal standing and all five word-indexed
  semantic layers;
- `OperationalBoundaryReceipt`, carrying a successful concrete independent
  pair and both replay receipts.

The additive model activates every semantic component with acceptance and
rejection witnesses and transports both lawfulness and a proof-relevant
execution.

## 4. Adequacy boundary: why the original event-only behavioral statement fails

`Adequacy.lean` proves two independent exclusions.

### Timestamp countermodel

Two behavior records may have identical event lists and therefore be
trace-equivalent while carrying different timestamps. A temporal lawfulness
predicate can accept one and reject the other. Therefore a theorem quantified
over two arbitrary behavior records cannot preserve full lawfulness when its
premise relates only their event projections.

The repaired `TimedTraceEq` relation includes both event trace equivalence and
timestamp alignment. `timed_repaired_crown` then transports temporal
lawfulness.

### Trajectory countermodel

The additive actions `[1, 3]` and `[3, 1]` reach the same final state but induce
different state traces:

```text
[0, 1, 4]
[0, 3, 4]
```

A trajectory observation can distinguish them. Final-state diamond equality is
therefore insufficient for arbitrary native trajectory constraints; a concrete
adapter must expose and prove the required trajectory relation.

`CrownObservationAdmission` is the repaired behavioral boundary. It requires a
native admitted relation that:

1. projects to event trace equivalence;
2. preserves the complete native lawfulness predicate bidirectionally.

## 5. Exact standing

- **Abstract quotient crown:** proved in source.
- **Abstract replay and lawfulness preservation:** proved in source.
- **Proof-relevant execution transport:** proved in source.
- **Non-vacuity receipts:** load-bearing through `AdmittedSemanticCrown`.
- **Event-only behavioral crown over arbitrary timestamps:** refuted by a
  kernel-visible countermodel.
- **Repaired observation-admitted behavioral crown:** proved in source.
- **Direct adapter to the external MFW `BehaviorTrace` and PDDL 3.1 model:** not
  claimed by this standalone project; it requires an explicit schedule and
  trajectory morphism rather than name-level correspondence.
- **Executable canonical normal form for trace classes:** not claimed; the
  current canonical classifier is the mathematical quotient.

## 6. Verification commands

```bash
python3 scripts/audit.py
lake update
lake exe cache get
lake build
```

The project is pinned to Lean 4.30.0 and Mathlib v4.30.0.
