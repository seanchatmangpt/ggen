# TCPS lifecycle reference implementation

This example is the reference implementation used to derive `packs/tcps-pack`.
It is deliberately implemented before the unified pack: the pack must reproduce
observed working artifacts rather than invent a second, template-only design.

The example assigns distinct responsibilities:

- **TCPS** controls flow, abnormality handling, standard work, pull, restart, and 1000x phase-shift classification.
- **Praxis** admits observations, independently judges autonomic proposals, checks Lean, and receipts law.
- **cargo-cicd** observes and executes repository workcells and emits process evidence; it does not adjudicate itself.
- **ggen** projects this admitted production system into canonical repository paths.
- **Charon/Aeneas** independently functionalize the isolated safe Rust kernel for Lean refinement.

The autonomic reference is documented in `AUTONOMICS.md`. Its exact boundary is:

```text
999.9x -> continuous standard work
1000x  -> bounded production-system recomposition
```

At 1000x the controller partitions demand, enables pull shards, requires an
independent oracle and proof-carrying receipts, and stages canary rollback. The
loop is typestate-constrained and can only finish as `Receipted` or `Stopped`.

Run the local verification rail:

```bash
bash scripts/verify.sh
```

Run the complete external-oracle rail when `cargo-cicd`, `my-conforming-project`,
`praxis-l4`, `charon`, and `aeneas` are installed:

```bash
bash scripts/verify.sh --crown
```

Authorization, verification evidence, autonomic evidence, autonomic plans, and
autonomic authorization are affine capabilities. No projected artifact is placed
under a `generated/` directory. Rust, Lean, Aeneas, release, deployment,
autonomics, and evidence surfaces are canonical project artifacts.
