# ggen Enterprise Architecture Runtime State

This directory is the first executable architecture repository for the Chatman Ecosystem.
It is intentionally small enough to audit and complete enough to drive machinery.

## Authoritative inputs

- `ggen-enterprise.json` — registered architecture assets, realization dependencies, lifecycle, standing, capacity policy, and autonomic policy.
- `ontology/ggen-enterprise-architecture.ttl` — semantic metamodel and SHACL obligations.
- `stimuli/sample-cycle.json` — bounded synthetic stimuli used to prove the autonomic loop.
- `run-autonomics.sh` — deterministic local automation that writes doctor, capacity, impact, and autonomic-cycle receipts.

## Commands

```bash
cargo run --manifest-path tools/ggen-architecture/Cargo.toml -- \
  doctor --state architecture/ggen-enterprise.json --json

cargo run --manifest-path tools/ggen-architecture/Cargo.toml -- \
  impact --state architecture/ggen-enterprise.json \
  --asset enterprise-architecture-ontology --json

cargo run --manifest-path tools/ggen-architecture/Cargo.toml -- \
  cycle --state architecture/ggen-enterprise.json \
  --stimuli architecture/stimuli/sample-cycle.json \
  --observed-at synthetic-proof-v1 --json
```

## Constitutional boundary

The controller implements Monitor, Analyze, and Plan. It does **not** execute.
Every autonomic output is an `ArchitectureIntent` with preconditions, required
capabilities, expected evidence, and a deterministic BLAKE3 identity. A broker
such as BRCE must independently admit an intent before any external actuation.

The sample capacity stimulus is explicitly synthetic. It proves policy and
intent behavior; it is not represented as observed production performance.
Real stress receipts should be appended to `capacity_samples` only after the
workload, environment, phase timings, and memory observations are captured.
