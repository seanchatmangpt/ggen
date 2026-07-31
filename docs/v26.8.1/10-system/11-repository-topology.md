# Repository topology

The active repository is a Rust workspace whose root package publishes the `ggen` library while the complete CLI is built from `ggen-cli-lib`. The current workspace contains the root plus specialized crates for configuration, marketplace, CLI, graph, LSP, engine, Praxis law and graph execution, POWL decomposition, Chicago-TDD tooling, planning IR, Genesis kernels, CPMP, and structural cheat scanning.

## Research obligations

The v26.8.1 observer must enumerate:

- workspace members and excluded directories;
- package names, versions, publishability, features, binaries, examples, benches, and build scripts;
- direct and transitive dependencies relevant to runtime, evidence, and licensing;
- generated source and ontology ownership;
- command and protocol entrypoints;
- filesystem state under `.ggen`, `.ggen-v2`, caches, lockfiles, receipts, and keys;
- CI workflows and release-law actuators;
- legacy directories retained only in history or archival form.

## Sunset rule

A package or directory cannot be considered obsolete solely because no active Cargo edge reaches it. Its former commands, formats, recovery value, and historical consumers must be dispositioned first.
