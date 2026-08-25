# Epistemic consumer replication

This repository is an independent execution root for the marketplace `epistemic-sensor-factory-pack`.

## Contract

The admitted subject is `.ggen/epistemic-replication/r52-subject.json`. It binds the exact consumer base, producer head, marketplace pack and target token. The contract grants observation, construction and verification only; it grants no ambient consequential DO.

## Qualification

Run:

```bash
python3 scripts/verify-epistemic-consumer-r52.py
cargo check -p ggen-cli --locked
```

The first command validates exact-subject and authority invariants. The second proves that the real ggen CLI substrate still compiles on the consumer subject. Hosted execution supplements, rather than replaces, exact-subject evidence.

## Evidence return

A consumer receipt must bind consumer base/head, producer head/pack, an independent evidence root, the executed checks and standing. `ALIVE` requires observed successful execution on the exact consumer head. Source presence alone is `UNKNOWN`.

## Failure semantics

Identity drift is `REFUSED[FOREIGN_SUBJECT]`. Compilation failure is `BUILD_BROKEN`. Missing execution evidence is `UNKNOWN`. A lower court passing without the requested crown is `PARTIAL_ALIVE`.
