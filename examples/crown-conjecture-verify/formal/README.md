# Crown Formal

This standalone Lean 4 and Mathlib project replaces the generated obligation-only skeleton with a kernel-checkable, non-vacuous formal core.

## Exact crown

The canonical quotient classifier satisfies

```text
classify I left = classify I right ↔ TraceEq I left right
```

without assuming the result. For an arbitrary compiler, the same kernel statement is equivalent to two separately named algorithm-correctness obligations:

1. trace soundness, which erases accidental serialization;
2. trace reflection, which refuses to merge distinct trace classes.

## Operational bridge

`DiamondCertificate` is stated over the actual deterministic replay function. `run_trace_eq` lifts a two-step operational diamond through every constructor of Mazurkiewicz trace equivalence.

`SemanticIndependenceCertificate` then transports the complete lawfulness conjunction:

- successful transition replay;
- final goal satisfaction;
- precondition preservation;
- invariant preservation;
- numeric-flow compatibility;
- temporal preservation;
- trajectory preservation.

`SemanticBoundaryReceipt` prevents an all-accepting predicate from being treated as experimental evidence. Every component is explicitly either neutral or active with an accepted and rejected witness.

## Falsifiers

The library proves:

- a constant transformation does not satisfy the unrestricted crown;
- an order-sensitive transition system cannot carry a false operational diamond;
- a concrete additive system carries a real diamond and lawfulness-transport proof.

## Verification

```bash
python3 scripts/audit.py
lake update
lake exe cache get
lake build
```

The project is pinned to Lean 4.30.0 and Mathlib v4.30.0.
