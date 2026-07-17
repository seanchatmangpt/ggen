<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Workspace Crates (Reference)](#workspace-crates-reference)
  - [Code-generation core](#code-generation-core)
  - [Praxis kernel](#praxis-kernel)
  - [Testing infrastructure](#testing-infrastructure)
  - [Agent & LSP surface](#agent--lsp-surface)
  - [Genesis / KNHK V2 kernel](#genesis--knhk-v2-kernel)
  - [Mapping & stewardship](#mapping--stewardship)
  - [Disconnected (on disk, excluded from workspace members)](#disconnected-on-disk-excluded-from-workspace-members)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Workspace Crates (Reference)

> Reference. Factual lookup. This table is the **real** workspace, derived from
> `Cargo.toml` `members = [...]`. If it disagrees with any other doc, this is correct and
> the other doc is drift. ggen v26.7.17-track, post `2026-ggen-core-replacement` migration
> (PR #255, merged to `main`).

The ggen workspace has **16 members** (15 crates under `crates/` + the root `ggen` package).
`ggen-core` was retired from the default pipeline in this migration: it moved from
`members` to `exclude` in root `Cargo.toml`, no longer compiles standalone, and has zero
in-workspace dependents. It is kept on disk (fix-forward/non-deletion doctrine), not deleted.
**Trust `Cargo.toml`, not prose.**

## Code-generation core

| Crate | Purpose |
|-------|---------|
| `ggen-engine` | The **live** code-generation pipeline behind `ggen sync`/`doctor`/`graph`/`receipt` (vendored from `~/praxis/crates/ggen`). Five stages in `src/sync.rs`: Resolve → Enrich → Extract → Render → Write (OTEL spans `pipeline.load`/`extract`/`validate`/`generate`/`emit`). GENERATED clap-noun-verb CLI routing in `src/verbs/`. `publish = false` |
| `ggen-cli` | CLI interface (binary + library `ggen-cli-lib`); routes `sync`/`doctor`/`graph`/`receipt` to `ggen-engine`'s nouns |
| `ggen-config` | One of `ggen.toml`'s two schemas — the "declarative-rules" `GgenManifest` (see [ggen.toml has two schemas](#see-also)) |
| `ggen-graph` | Deterministic RDF graph — Oxigraph wrapper with deterministic hashing, state-change deltas, validation hooks, transition receipts |
| `ggen-marketplace` | Marketplace / pack management (typestate `Draft`/`Published`, profile enforcement) |

## Praxis kernel

`ggen-engine`'s direct dependencies, vendored from `~/praxis/crates/*`. All `publish = false`.

| Crate | Purpose |
|-------|---------|
| `praxis-core` | Fused Law Object abstraction: obligation + lifecycle + receipt + OCEL (`LawObject`, `Obligation`, `Andon`, `ReceiptRecord`) |
| `praxis-graphlaw` | Native N3/Datalog/SPARQL 1.1/SHACL/ShEx law-state engine (fork of `pbonte/roxi`) — `ggen-engine`'s default graph backend |
| `powl2-decompose` | Vendored Kourani et al. Stage-1 WF-net → POWL 2.0 decomposition; `praxis-graphlaw`'s dependency |
| `bcinr-pddl` | Vendored PDDL8 → POWL tape → Prolog8 admission → OCEL → BLAKE3 receipt planner |
| `bcinr-mfw-ir` | Shared IR types/trait contracts for the multifractal-workflow planner (`bcinr-pddl`'s dependency) |

## Testing infrastructure

| Crate | Purpose |
|-------|---------|
| `chicago-tdd-tools` | Dev/test-only Chicago-TDD utilities (property/snapshot/parameterized/mutation/concurrency testing, `cli-proof`) for `ggen-engine`/`praxis-graphlaw`'s own test suites. `publish = false`. Its optional `wasm4pm-cognition` re-export path carries a BUSL-1.1 (non-OSI) license — optional and off by default |

## Agent & LSP surface

| Crate | Purpose |
|-------|---------|
| `ggen-lsp` | Language server for ggen surfaces (analyzers, check, intel/mining, pack, route/repair); also a library API. Absorbed `ggen-lsp-mcp`, `ggen-a2a-mcp`, and `ggen-lsp-a2a` as feature-gated modules (`mcp`, `a2a`) — those three crates no longer exist standalone |

## Genesis / KNHK V2 kernel

| Crate | Purpose |
|-------|---------|
| `genesis-types-v2` | KNHK V2 type system — workflow/pattern definitions, execution state/events, error and config types. Absorbed `genesis-schema-v2` as its `schema` module (OpenAPI specs, RDF ontology, 43 YAWL pattern definitions) |
| `genesis-core-v2` | KNHK V2 core — `Pattern` trait system, registry, composition, zero-copy/zero-alloc execution |

## Mapping & stewardship

| Crate | Purpose |
|-------|---------|
| `cpmp` | Computer Project Mapping Protocol (Open Ontologies Catalog) — scanner, capability classification, projection, receipts |

## Disconnected (on disk, excluded from workspace members)

| Path | Status |
|------|--------|
| `crates/ggen-core` | Excluded via root `Cargo.toml`'s `exclude = [...]`. Does not compile standalone (inherits `workspace = true` fields with no workspace to inherit from). Zero in-workspace dependents. Kept on disk, not deleted |

Dormant, non-member directories from earlier eras (`genesis-construct8`, `genesis-lockchain`,
`genesis-wasm-shell`, `ggen-daemon`, `ggen-membrane`, `ggen-projection`,
`ggen-pack-clap-noun-verb`, `ggen-pack-lsp-max`, `stpnt`, `genesis-core`) were deleted in the
2026-07 crate-consolidation pass — see git history, not this doc, for their content.

## See also

- [Feature flags](feature-flags.md) — which crates are gated, and behind what
- [Command-proof matrix](../cli/command-proof-matrix.md) — what each CLI noun delivers
- `CLAUDE.md` and `.claude/rules/architecture.md` — the actively-maintained sources this table
  is derived from; ggen.toml's two-schema split is documented there
