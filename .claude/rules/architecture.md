---
auto_load: false
category: reference
priority: normal
version: 26.5.4
---

# Architecture Reference

GENERATED from `.specify/repo-facts.ttl` — edit the RDF, never this file (compressed 2026-09-28 for budget). `LSP workspaceSymbol` for live discovery.

**14 crates**: `ggen-engine` (live pipeline behind `ggen sync`, publish=true; default graph backend =
sibling `graphlaw` crate at `../graphlaw`) · `ggen-cli` · `ggen-config`
(one of two `ggen.toml` schemas) · `ggen-marketplace` · `ggen-graph` (RDF hashing/receipts) ·
`powl2-decompose`/`bcinr-pddl`/`bcinr-mfw-ir` (vendored planning) · `ggen-cheat-scanner` ·
`ggen-lsp` · `ggen-mcp` · `ggen-abb-sbb` (ABB/SBB SELECT-or-MANUFACTURE admission kernel,
v26.10.9; ggen-engine's Stage 2c gate) · `pm4pytest-cli` (`cargo` wrapper around external
`pm4pytest` for OCEL/conformance evidence) · `ggen` (root). `ggen-core` fully deleted
2026-07-17; `praxis-core`/`praxis-graphlaw` retired 2026-10-10 (SJIRA-15).
`crates/ggen-architecture/` is a nested workspace, excluded.

`ggen.toml` has two incompatible schemas by raw-text pre-parse — no drift guard.

Removed (ERRC 2026-08-12): `cpmp`, `openapi-cnv-reflect`, `genesis-{types,core}-v2`.

**~90 packs** (`packs/*/pack.toml`): agent/workflow codegen, TCPS production, constitutional/
DfCM, Fortune-5, domain catalogs, release/CI, diagrams. Full table: `ggen sync run`.

**Patterns**: `Result<T,E>` via `thiserror`; builder+typestate+newtype in `ggen-marketplace`;
RDF/SPARQL everywhere; chained-BLAKE3 receipts via `graphlaw`/`ggen-graph`.

Navigation: `workspaceSymbol`→`goToDefinition`→`findReferences`→`goToImplementation`
([[rust/lsp]]).
