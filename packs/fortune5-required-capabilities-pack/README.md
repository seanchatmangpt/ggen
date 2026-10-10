# Fortune-5 Required Capabilities Pack

This pack manufactures an independent verifier for the bounded repository-defined Fortune-5 contract.

## Contract

- **6 release truths:** install, compiler, conflict, rendering, trust, proof
- **2 supporting systems:** atomic pack taxonomy, bundle/profile system
- **5 performance governors:** CLI startup, rendering, RDF query, memory, concurrency
- **6 operational controls:** Andon, Poka-Yoke, tracing, chaos resilience, golden signals, error budgets
- **3 proof surfaces each:** positive execution, negative refusal, receipt/replay

The crown is therefore **19 capabilities × 3 surfaces = 57 obligations**.

## Execute

```bash
ggen sync run
cargo test --manifest-path consumer/fortune5-required-foundation/Cargo.toml
bash consumer/fortune5-required-foundation/scripts/verify.sh
```

`RELEASE_STANDING.json` intentionally remains `UNKNOWN`. Only the executed verifier may emit `ALIVE`.
