# Workspace crate map

The baseline workspace exposes 17 packages including the root. The research program must treat each package as a distinct semantic owner and reject undocumented overlap.

## Active ownership map

- `ggen-engine`: production sync and manufacturing pipeline.
- `praxis-core`: law objects, obligations, receipt records, and lifecycle primitives.
- `praxis-graphlaw`: graph-law execution, SPARQL, N3/Datalog, validation, and planning integrations.
- `ggen-cli`: user command routing and binary surface.
- `ggen-config`: one project configuration domain and manifest parser.
- `ggen-marketplace`: packs, registries, acquisition, and composition support.
- `ggen-graph`: deterministic RDF graph operations, deltas, validation hooks, and transition receipts.
- `ggen-lsp`: diagnostics, checking, intelligence, repair, and optional protocol modules.
- `ggen-cheat-scanner`: structural test-quality enforcement.
- `powl2-decompose`, `bcinr-pddl`, `bcinr-mfw-ir`: planning and workflow decomposition path.
- `chicago-tdd-tools`: verification utilities, not production runtime authority.
- `genesis-types-v2`, `genesis-core-v2`: workflow kernel types and execution.
- `cpmp`: project mapping, capability classification, projections, and receipts.
- `ggen`: public root library package.

## Required validation

The final map must be generated from Cargo metadata and compared against documented owners. Duplicate semantic ownership, unreachable active code, unpublished local-only dependencies, and license boundaries must produce typed findings.
