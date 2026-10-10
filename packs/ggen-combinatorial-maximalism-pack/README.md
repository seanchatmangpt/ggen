# ggen-combinatorial-maximalism-pack

This pack turns **Design for Combinatorial Maximalism (CMD)** into executable repository law.

It preserves the graph domain long enough to manufacture and verify bounded combinations, then permits irreversible filesystem mutation only through a generated broker that atomically publishes an output together with its BLAKE3 receipt. The pack enforces the distinctions:

```text
candidate != verified != authorized != actuated
planning != actuation
inferred concurrency != proven independence
serialization != partial-order preservation
intent hook != broker
```

The consumer supplies only `ggen.toml` and an RDF design space. ggen manufactures:

- a Rust verifier for exhaustive or pairwise candidate coverage;
- an atomic filesystem broker;
- BLAKE3 receipt chaining and exact-output replay;
- a public-argv CLI;
- a real process/filesystem integration test;
- a machine-readable construction plan;
- a human verifier report.

The included consumer at `examples/combinatorial-maximalism/` is the executable specimen. Run:

```bash
bash scripts/verify-combinatorial-maximalism-pack.sh
```

The verifier reports `ALIVE` only after the manufactured cell crosses generation, compilation, process, filesystem, receipt, replay, refusal, sabotage, and second-sync boundaries. Before that receipt exists, the implementation remains `PARTIAL_ALIVE`; an unobserved or unsupported boundary is never promoted by assertion.

A green run proves this pack and specimen are alive at the declared boundary. It does not claim that every possible CMD consumer is correct.
