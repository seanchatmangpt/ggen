# ggen-architecture

Executable enterprise architecture machinery for ggen.

## Capabilities

- governed architecture asset registry;
- lawful lifecycle transition state machine;
- dependency closure and cycle refusal;
- transitive change-impact calculation;
- dependency-safe transition planning;
- multidimensional capacity policies and observed knee detection;
- architecture doctor diagnostics;
- MAPE-K-style autonomic diagnosis and intent planning;
- deterministic BLAKE3 receipts;
- a strict zero-direct-actuation boundary.

This tool is a standalone Cargo workspace so the machinery can land without
expanding the main ggen workspace dependency graph. Promotion into the primary
`ggen` noun/verb CLI is a later architecture transition, not a hidden coupling.
