---
auto_load: false
category: reference
priority: normal
version: 26.5.4
---

# Architecture Reference (LSP-Surveyed)

Full public API surface derived from LSP `documentSymbol` sweep of all workspace crates.

Use LSP for navigation -- this file is orientation, not a substitute for `LSP workspaceSymbol`.

## Crate Map (15 workspace members)

Verified against `Cargo.toml` `members = [...]` on 2026-05-28. These are the only crates compiled in the workspace. Use `LSP workspaceSymbol` for live symbol discovery — this table is orientation only.

### Code generation core

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `ggen-core` | Core graph-aware code generation engine; μ₁–μ₅ deterministic pipeline |
| `ggen-cli` | CLI interface for ggen (binary + library `ggen-cli-lib`) |
| `ggen-config` | Configuration parser and validator for `ggen.toml` files |
| `ggen-graph` | Deterministic RDF graph module — Oxigraph wrapper with deterministic hashing, state-change deltas, validation hooks, and cryptographic transition receipts |
| `ggen-marketplace` | Marketplace / package management system for ggen |

### Agent & LSP surface

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `ggen-a2a-mcp` | A2A protocol and MCP server for agent-to-agent communication |
| `ggen-lsp` | Language server for ggen surfaces — analyzers, `check`, `init`, `intel`/mining, `pack`, `route`/repair, handlers; also a library API |
| `ggen-lsp-mcp` | MCP server exposing `ggen-lsp` repair routes as a tool (leaf crate — avoids the `ggen-core`↔`ggen-a2a-mcp` cycle) |
| `ggen-lsp-a2a` | A2A bridge exposing the `ggen-lsp-mcp` route engine as an A2A agent (leaf crate, cycle-free) |

### Genesis / KNHK V2 kernel

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `genesis-core` | Pure mathematical foundation for the Genesis interchangeable-parts architecture (A = μ(O)); no_std variant for wasm32 targets |
| `genesis-types-v2` | KNHK V2 type system — foundational data structures (workflow/pattern definitions, execution state/events, error and config types) |
| `genesis-schema-v2` | KNHK V2 schema system — OpenAPI specs, RDF ontology, 43 YAWL pattern definitions, workflow schema validation |
| `genesis-core-v2` | KNHK V2 core — `Pattern` trait system, pattern registry, composition, zero-copy/zero-alloc execution paths |

### Mapping & stewardship

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `cpmp` | Computer Project Mapping Protocol (Open Ontologies Catalog) — scanner, capability classification, projection, receipts, symbol/db modules |
| `stpnt` | Stewards of the Pentecost — Canonical Stewardship Cell implementation (canon, cells, governance, membrane, projections, proof) |

### Dormant

None. The prior dormant non-member directories (`genesis-construct8`, `genesis-lockchain`, `genesis-wasm-shell`, `ggen-daemon`, `ggen-membrane`, `ggen-projection`, `ggen-pack-clap-noun-verb`, `ggen-pack-lsp-max`) were deleted in the 2026-07 crate-consolidation pass — see `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` and git history for the removed code.

## Cross-Cutting Patterns

| Pattern | Where Used | Details |
|---------|-----------|---------|
| `pub type Result<T> = std::result::Result<T, CrateError>` | Most crates | Each crate has its own error enum via `thiserror` |
| Builder pattern | `ggen-core`, `ggen-marketplace` | `with_*()` chain methods for optional config |
| Typestate | `ggen-marketplace` (`Draft`/`Published`) | Compile-time state transitions |
| Newtype wrappers | `ggen-core`, `ggen-marketplace` | Invariant encoding in types (e.g., `PackageId`) |
| Async traits | `ggen-a2a-mcp` | `#[async_trait]` with `Result` returns |
| RDF/SPARQL foundation | `ggen-core`, `ggen-graph`, `ggen-marketplace` | Built on `oxigraph` triplestores |
| Pipeline architecture | `ggen-core` (μ₁–μ₅) | Multi-stage deterministic transformation |
| Deterministic hashing + transition receipts | `ggen-graph` | State-change detection (deltas) and cryptographic receipts |
| Leaf-crate cycle avoidance | `ggen-lsp-mcp`, `ggen-lsp-a2a` | Bridge crates kept dependency-cycle-free |
| Pattern trait / registry | `genesis-core-v2`, `genesis-schema-v2` | 43 YAWL workflow patterns, zero-copy execution |

## Navigation

This file is orientation only. For any symbol lookup use the LSP tool:

1. `LSP workspaceSymbol` — find a symbol position (use any `.rs` file as `filePath`).
2. `LSP goToDefinition` — jump to the definition (works across re-exports).
3. `LSP findReferences` — enumerate all usages across the workspace.
4. `LSP goToImplementation` — find all implementers of a project-defined trait.

See `.claude/rules/rust/lsp.md` for the full LSP-first navigation contract.
