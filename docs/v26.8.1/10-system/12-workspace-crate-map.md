# Workspace crate map

Real count as of 2026-07-31 (G4 governance/system evidence pass): `Cargo.toml`'s
`[workspace] members = [...]` array holds **17** `crates/*` entries (`grep -c '^  "crates/'
Cargo.toml`) plus the root `ggen` package = **18** total, not 17. `crates/ggen-cheat-scanner`
was already counted in an earlier pass; the 18th is `crates/openapi-cnv-reflect`
(`packs/clap-noun-verb-*-pack`'s OpenAPI-to-`cnv:Cli` reflector, described in that crate's own
`Cargo.toml`).

**Real, confirmed divergence found and fixed this pass:** `crates/openapi-cnv-reflect` was
present in `Cargo.toml` workspace members but had **no** corresponding `rf:Crate` individual in
`.specify/repo-facts.ttl` — meaning the CLAUDE.md/`.claude/rules/architecture.md` crate-map
tables generated from that TTL silently omitted a real, building workspace member. Fixed by
adding `rf:crate_openapi_cnv_reflect` to `.specify/repo-facts.ttl` and correcting
`rf:memberCount`/`rf:crateCount` from `"16"`/`"17"` to `"17"`/`"18"`. This is now a checked fact,
not just a claim: `crates/ggen-config/tests/system_crate_map_parity_test.rs` parses both
`Cargo.toml` and `.specify/repo-facts.ttl` directly off disk on every test run and fails loudly
if they diverge again (verified by a local sabotage-and-revert: renaming the `rf:dir` fact away
from `"openapi-cnv-reflect"` makes the test fail with the exact missing-crate name; reverting
makes it pass again).

The research program must treat each package as a distinct semantic owner and reject undocumented overlap.

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
