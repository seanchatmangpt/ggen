---
auto_load: false
category: reference
priority: normal
version: 26.5.4
---

# Architecture Reference (LSP-Surveyed)

Full public API surface derived from LSP `documentSymbol` sweep of all workspace crates.

Use LSP for navigation -- this file is orientation, not a substitute for `LSP workspaceSymbol`.

## Crate Map (12 workspace members)

Re-verified against `Cargo.toml` `members = [...]` on 2026-07-16 (12 members, counted directly
from the array). The 2026-07-02 count of 10 packages / 9 disk dirs (2026-07 crate-consolidation
pass, trimmed from 17 packages / 24 disk dirs) is superseded: the workspace gained three more
members — `ggen-engine`, `praxis-core`, `praxis-graphlaw` — vendored in for the ggen-core
replacement migration (`docs/jira/v26.7.16/`, `2026-ggen-core-replacement` branch). Use `LSP
workspaceSymbol` for live symbol discovery — this table is orientation only. See
`CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` for the 2026-07 pass's evidence base and
phase-by-phase history.

### Code generation core

`ggen-core` is being disconnected in place, not deleted: its dependents are ported off it crate
by crate (fix-forward/non-deletion doctrine — code stays on disk after a dependent stops calling
it). As of 2026-07-16, `ggen-cli`'s default `ggen sync`/`doctor`/`graph`/`receipt` commands
already route to `ggen-engine`'s clap-noun-verb nouns instead of `ggen-core` (see
`crates/ggen-cli/src/lib.rs`'s `inject_default_verbs`/`cli_match`, and `cmds/mod.rs`'s
`// pub mod sync;` archival note). `ggen-engine` is the growing, actively-developed half of this
split; `ggen-core` still compiles and is still called directly by other, not-yet-ported paths
(e.g. the experimental, default-off `ggen wizard` command's initial-sync step).

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `ggen-core` | Legacy graph-aware code generation engine; μ₁–μ₅ deterministic pipeline. Being disconnected as dependents are ported to `ggen-engine` — see note above. |
| `ggen-engine` | SPARQL-in-Tera code generation engine (vendored, renamed from `~/praxis/crates/ggen`; `docs/jira/v26.7.16/`). Now the live pipeline behind `ggen sync`: five stages (Resolve → Enrich → Extract → Render → Write) in `src/sync.rs`, GENERATED clap-noun-verb CLI routing in `src/verbs/` (hand-written logic in `verbs::handlers` only), plus `config`/`graph`/`template`/`write`/`watch`/`pack`/`lint`/`law_engine`/`repl` modules. `publish = false`. |
| `ggen-cli` | CLI interface for ggen (binary + library `ggen-cli-lib`); routes `sync`/`doctor`/`graph`/`receipt` to `ggen-engine`'s nouns |
| `ggen-config` | Configuration parser and validator for `ggen.toml` files (depends on the published `star-toml` crate, not an embedded copy) |
| `ggen-graph` | Deterministic RDF graph module — Oxigraph wrapper with deterministic hashing, state-change deltas, validation hooks, and cryptographic transition receipts |
| `ggen-marketplace` | Marketplace / package management system for ggen |

### Praxis kernel (ggen-engine's direct dependencies)

Vendored alongside `ggen-engine` in the same migration pass — renamed copies of
`~/praxis/crates/{praxis-core,praxis-graphlaw}`, not live path dependencies back to `~/praxis`.
Both `publish = false`.

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `praxis-core` | Fused Law Object abstraction: obligation + lifecycle + receipt + OCEL (`LawObject`, `Obligation`, `Andon`, `ReceiptRecord`, `ArazzoProjectionReceipt`) |
| `praxis-graphlaw` | Praxis GraphLaw law-state engine: native N3, Datalog, SPARQL 1.1, SHACL, ShEx (fork of `pbonte/roxi`) — `ggen-engine`'s default `EngineKind::GraphLaw` graph backend |

### Agent & LSP surface

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `ggen-lsp` | Language server for ggen surfaces — analyzers, `check`, `init`, `intel`/mining, `pack`, `route`/repair, handlers; also a library API. Absorbed `ggen-lsp-mcp` (MCP server exposing repair routes, feature `mcp`), `ggen-a2a-mcp` (A2A protocol + MCP server for agent-to-agent communication, module `a2a_mcp`), and `ggen-lsp-a2a` (A2A bridge over the mcp tools, feature `a2a`) as feature-gated modules |

### Genesis / KNHK V2 kernel

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `genesis-types-v2` | KNHK V2 type system — foundational data structures (workflow/pattern definitions, execution state/events, error and config types). Absorbed `genesis-schema-v2` as its `schema` module (OpenAPI specs, RDF ontology, 43 YAWL pattern definitions, workflow schema validation) |
| `genesis-core-v2` | KNHK V2 core — `Pattern` trait system, pattern registry, composition, zero-copy/zero-alloc execution paths |

### Mapping & stewardship

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `cpmp` | Computer Project Mapping Protocol (Open Ontologies Catalog) — scanner, capability classification, projection, receipts, symbol/db modules |

### Removed in the 2026-07 consolidation pass

`ggen-a2a-mcp`, `ggen-lsp-mcp`, `ggen-lsp-a2a` (absorbed into `ggen-lsp`), `genesis-schema-v2` (absorbed into `genesis-types-v2`), `star-toml` (removed from the workspace — now an external published dependency), `stpnt` and `genesis-core` (dead code, zero dependents). The prior dormant non-member directories (`genesis-construct8`, `genesis-lockchain`, `genesis-wasm-shell`, `ggen-daemon`, `ggen-membrane`, `ggen-projection`, `ggen-pack-clap-noun-verb`, `ggen-pack-lsp-max`) were deleted earlier in the same pass. See `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` and git history for the removed code.

## Cross-Cutting Patterns

| Pattern | Where Used | Details |
|---------|-----------|---------|
| `pub type Result<T> = std::result::Result<T, CrateError>` | Most crates | Each crate has its own error enum via `thiserror` |
| Builder pattern | `ggen-core`, `ggen-marketplace` | `with_*()` chain methods for optional config |
| Typestate | `ggen-marketplace` (`Draft`/`Published`) | Compile-time state transitions |
| Newtype wrappers | `ggen-core`, `ggen-marketplace` | Invariant encoding in types (e.g., `PackageId`) |
| Async traits | `ggen-lsp` (`a2a_mcp` module, feature `mcp`/`a2a`) | `#[async_trait]` with `Result` returns |
| RDF/SPARQL foundation | `ggen-core`, `ggen-graph`, `ggen-marketplace`, `ggen-engine` (via `praxis-graphlaw`/`oxigraph`) | Built on `oxigraph` triplestores (or `praxis-graphlaw`'s N3/Datalog/SHACL/ShEx engine, `ggen-engine`'s default) |
| Pipeline architecture | `ggen-core` (μ₁–μ₅, legacy); `ggen-engine` (Resolve → Enrich → Extract → Render → Write, live) | Multi-stage deterministic transformation — see `ggen-engine/src/sync.rs` module doc for the current pipeline |
| Deterministic hashing + transition receipts | `ggen-graph`; `ggen-engine` (via `praxis-core::ReceiptRecord`, chained BLAKE3 over `{graph_hash, outputs}`) | State-change detection (deltas) and cryptographic receipts |
| Feature-gated module absorption | `ggen-lsp` (`mcp`/`a2a` features), `genesis-types-v2` (`schema` module) | Former sibling crates folded in behind Cargo features/modules, cycle-free |
| Pattern trait / registry | `genesis-core-v2`, `genesis-types-v2::schema` | 43 YAWL workflow patterns, zero-copy execution |

## Navigation

This file is orientation only. For any symbol lookup use the LSP tool:

1. `LSP workspaceSymbol` — find a symbol position (use any `.rs` file as `filePath`).
2. `LSP goToDefinition` — jump to the definition (works across re-exports).
3. `LSP findReferences` — enumerate all usages across the workspace.
4. `LSP goToImplementation` — find all implementers of a project-defined trait.

See `.claude/rules/rust/lsp.md` for the full LSP-first navigation contract.
