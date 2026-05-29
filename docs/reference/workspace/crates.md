# Workspace Crates (Reference)

> Reference. Factual lookup. This table is the **real** workspace, derived from
> `Cargo.toml` `members = [...]`. If it disagrees with any other doc, this is correct and
> the other doc is drift. ggen v26.5.28.

The ggen workspace has **15 library/binary crates** plus one example crate that is a
workspace member for CI. Older architecture docs once described ~68 crates; those were
phantom or future bodies and have been removed. **Trust `Cargo.toml`, not prose.**

## Code-generation core

| Crate | Purpose |
|-------|---------|
| `ggen-core` | Core graph-aware code-generation engine; the μ₁–μ₅ deterministic pipeline |
| `ggen-cli` | CLI interface (binary + library `ggen-cli-lib`) |
| `ggen-config` | Parser and validator for `ggen.toml`; telemetry config schema |
| `ggen-graph` | Deterministic RDF graph — Oxigraph wrapper with deterministic hashing, state-change deltas, validation hooks, transition receipts |
| `ggen-marketplace` | Marketplace / pack management (typestate `Draft`/`Published`, profile enforcement) |

## Agent & LSP surface

| Crate | Purpose |
|-------|---------|
| `ggen-a2a-mcp` | A2A protocol + MCP server for agent-to-agent communication |
| `ggen-lsp` | Language server for ggen surfaces (analyzers, check, intel/mining, pack, route/repair); also a library API. **Behind the `lsp` feature.** |
| `ggen-lsp-mcp` | MCP server exposing `ggen-lsp` repair routes as a tool (leaf crate — avoids the `ggen-core`↔`ggen-a2a-mcp` cycle) |
| `ggen-lsp-a2a` | A2A bridge exposing the `ggen-lsp-mcp` route engine as an A2A agent (leaf crate, cycle-free) |

## Genesis / KNHK V2 kernel

| Crate | Purpose |
|-------|---------|
| `genesis-core` | Pure mathematical foundation for interchangeable-parts architecture (A = μ(O)); no_std variant for wasm32 targets |
| `genesis-types-v2` | KNHK V2 type system — workflow/pattern definitions, execution state/events, error and config types |
| `genesis-schema-v2` | KNHK V2 schema system — OpenAPI specs, RDF ontology, 43 YAWL pattern definitions, schema validation |
| `genesis-core-v2` | KNHK V2 core — `Pattern` trait system, registry, composition, zero-copy/zero-alloc execution |

## Mapping & stewardship

| Crate | Purpose |
|-------|---------|
| `cpmp` | Computer Project Mapping Protocol (Open Ontologies Catalog) — scanner, capability classification, projection, receipts |
| `stpnt` | Stewards of the Pentecost — Canonical Stewardship Cell (canon, cells, governance, membrane, projections, proof) |

## Example member

| Member | Purpose |
|--------|---------|
| `examples/7-agent-validation` | Workspace-member example used in CI |

## Dormant (on disk, NOT workspace members — do not compile)

`genesis-construct8`, `genesis-lockchain`, `genesis-wasm-shell`, `ggen-membrane`,
`ggen-projection`. These directories exist under `crates/` but are excluded from
`Cargo.toml` `members`. Treat as non-compiled reference material until activated.

## See also

- [Feature flags](feature-flags.md) — which crates are gated, and behind what
- [Command-proof matrix](../cli/command-proof-matrix.md) — what each CLI noun delivers
- [v26.5.28 boundary](../release/v26-5-28-boundary.md) — the release boundary
