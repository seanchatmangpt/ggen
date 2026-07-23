# Adversarial Review of PR #466

## Review target

PR #466 was reviewed as a proof artifact after merge into `main`, not as a
trusted implementation. The preserved kernel was the Mazurkiewicz quotient
classification theorem and its operational replay transport. The review then
attempted to falsify every stronger behavioral claim.

## Findings

| Severity | Finding | Consequence | Repair |
| --- | --- | --- | --- |
| P0 | `CrownObservationAdmission.lawfulIff` was the exact pairwise theorem the interface claimed to derive. | The repaired crown was circular. `preservesLawful` projected the assumed theorem and did not consume `traceSound`. | Replaced pairwise lawfulness admission with pointwise factorization through `(TraceClass I, Observation)`. The derived theorem consumes `traceSound`, `classify_eq_iff`, `observationEq`, and `lawfulFactorization`. |
| P0 | The behavioral relation could be empty or equality-only. | The theorem could receive vacuous standing without one admitted serialization change. | Added `AdmittedBehavioralCrown` with a related witness pair whose event serializations are distinct. |
| P1 | Timed behavior stored timestamps by list position. | Swapping events could silently reassign timestamps to different occurrences while raw timestamp-list equality still held. | Timed behavior now uses unique event identities and a timestamp map keyed by identity. |
| P1 | The trajectory falsifier was explanatory but not connected to a refusal relation. | A downstream adapter still lacked a kernel-visible statement that the additive swap must be rejected when full state traces are observed. | Added `AdditiveTrajectoryRelated` and proved that it refuses `[1, 3] ↔ [3, 1]`. |
| P1 | The source audit checked declaration names but not the circular interface shape. | The old pairwise field could be reintroduced while the audit remained green. | Added anti-regression checks for the pairwise `lawfulIff` field, positional timestamps, and missing theorem dependencies. |
| P2 | README language called abstract word predicates native temporal and trajectory preservation. | The documentation blurred abstract projection semantics with native schedules and state trajectories. | Reclassified those fields as word-level projections and documented the remaining MFW adapter boundary. |

## Preserved results

The review did not invalidate:

- `classify I left = classify I right ↔ TraceEq I left right`;
- `CrownLaw I transform ↔ TraceSound I transform ∧ TraceReflecting I transform`;
- deterministic replay preservation under a valid `DiamondCertificate`;
- proof-relevant execution reification and transport;
- the timestamp and intermediate-state countermodels.

## Refactored calculus

For a behavior `b`, native lawfulness must now satisfy a pointwise equation:

```text
lawful(b)
↔
admittedLaw(classify(I, events(b)), observe(b))
```

For related behaviors `left` and `right`, the adapter proves:

```text
TraceEq(I, events(left), events(right))
observe(left) = observe(right)
```

The quotient theorem converts the first receipt into equality of trace classes.
Rewriting both coordinates then derives:

```text
lawful(left) ↔ lawful(right)
```

The conclusion is no longer stored as an input field.

## Exclusions

This refactor still does not claim:

- a direct adapter to the external MFW `BehaviorTrace`;
- PDDL 3.1 schedule or trajectory adequacy;
- an executable canonical normal form for trace classes;
- Lean kernel execution standing for this branch until a build receipt exists.

## Source standing

- Abstract trace kernel: preserved.
- Circular behavioral admission: refuted and removed.
- Observation-factored behavioral theorem: source complete.
- Nontrivial behavioral receipt: source complete.
- Identity-aligned timed model: source complete.
- Complete-trajectory refusal: source complete.
- Lean build: unknown at review time.