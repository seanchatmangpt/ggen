---
auto_load: false
category: reference
priority: normal
version: 26.5.4
---

# Architecture Reference (LSP-Surveyed)

Full public API surface derived from LSP `documentSymbol` sweep of all workspace crates.

Use LSP for navigation -- this file is orientation, not a substitute for `LSP workspaceSymbol`.

## Crate Map (17 workspace members)

Re-verified against `Cargo.toml` `members = [...]` on 2026-07-17 (16 array entries + the root
`ggen` package = 17 total; `grep -c '^  "crates/' Cargo.toml` → 16 — this exact count was
previously mis-stated in this file as 15/16 total; the miscount traced to `ggen-cheat-scanner`,
added by PR #257, never having been added to any table here — it now has its own row in the
Testing infrastructure section below). The prior 12-member count (2026-07-16) is superseded: PR
#255 (absolute-local-path Cargo dependency cleanup) added 4 more members — `powl2-decompose`,
`chicago-tdd-tools` (+ its `chicago-tdd-tools/proc_macros` path-dependency, not itself a
top-level workspace member), `bcinr-pddl`, `bcinr-mfw-ir` — all `publish = false`, vendored to
eliminate `path = "/Users/sac/..."` dependencies that only resolved on one machine and broke CI
(`cargo build --workspace` failing with "No such file or directory" for anyone else). See the
Praxis kernel and Testing infrastructure sections below. Use `LSP workspaceSymbol` for live
symbol discovery — this table is orientation only. See `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md`
for the 2026-07 pass's evidence base and phase-by-phase history.

### Code generation core

`ggen-core` is **fully deleted, not merely disconnected.** PR #255 (2026-07-16/17) first ported
every dependent off it and moved it from `[workspace] members` to root `Cargo.toml`'s `exclude`
as an interim step; PR #259 (2026-07-17, "remove ggen-core, rewrite README from first
principles") then deleted `crates/ggen-core/` outright. That interim "excluded but present on
disk" state — described at length in an earlier version of this note, including a 3-way
standalone-compile-failure demonstration — no longer exists and cannot be reproduced:
`crates/ggen-core/` is absent from disk (`find crates -maxdepth 1 -iname 'ggen-core*'` → empty),
and root `Cargo.toml`'s `exclude` list contains only `examples/7-agent-validation`. The
experimental, default-off `ggen wizard`/`sigma` commands (and `inverse_sync`), which used to
import now-nonexistent `ggen_core::` symbols, were deleted in the same pass rather than
re-pointed — see `crates/ggen-cli/src/cmds/mod.rs`'s "REMOVED" comments and
`docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md` (marked superseded/executed). `ggen-engine`
is the sole live half of this split.

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `ggen-engine` | SPARQL-in-Tera code generation engine (vendored, renamed from `~/praxis/crates/ggen`; `docs/jira/v26.7.16/`). Now the live pipeline behind `ggen sync`: five stages (Resolve → Enrich → Extract → Render → Write) in `src/sync.rs`, GENERATED clap-noun-verb CLI routing in `src/verbs/` (hand-written logic in `verbs::handlers` only), plus `config`/`graph`/`template`/`write`/`watch`/`pack`/`lint`/`law_engine`/`repl` modules. `publish = false`. |
| `ggen-cli` | CLI interface for ggen (binary + library `ggen-cli-lib`); routes `sync`/`doctor`/`graph`/`receipt` to `ggen-engine`'s nouns |
| `ggen-config` | Defines and validates ONE of `ggen.toml`'s two schemas — the "declarative-rules" `GgenManifest` (depends on the published `star-toml` crate, not an embedded copy). Not sole authority over `ggen.toml` parsing; see "ggen.toml has two schemas" below |
| `ggen-graph` | Deterministic RDF graph module — Oxigraph wrapper with deterministic hashing, state-change deltas, validation hooks, and cryptographic transition receipts |
| `ggen-marketplace` | Marketplace / package management system for ggen |

### ggen.toml has two schemas

`ggen.toml` is parsed by one of two independently-defined, incompatible struct hierarchies,
chosen by a raw-text pre-parse before any typed parse runs: `ggen_engine::generation_rules::
has_generation_rules` (`crates/ggen-engine/src/generation_rules.rs:108`) checks the raw TOML for
a non-empty `[[generation.rules]]` array. If present, `sync()`'s Stage 0 dispatch
(`crates/ggen-engine/src/sync.rs:155`) parses the file as `ggen_config::manifest::GgenManifest`
(`crates/ggen-config/src/manifest/types.rs:160`, the "declarative-rules" schema) — `[[packs]]` is
an array-of-tables of a flat `PackRef { name, registry, path, version }`. If absent, it falls
through to `ggen_engine::config::GgenConfig` (`crates/ggen-engine/src/config.rs:45`, the
"frontmatter" schema) — `[packs]` is instead a table-of-tables (`BTreeMap<String, PackRef>`) of
an untagged `PackRef` enum, `Path { path, .. } | Git { .. }`. Same table names throughout
(`[project]`, `[ontology]`, `[packs]`, `[templates]`, `[law]`), genuinely divergent shapes, and
no automated cross-drift guard between the two.

### Praxis kernel (ggen-engine's direct dependencies)

Vendored alongside `ggen-engine` in the same migration pass — renamed copies of
`~/praxis/crates/{praxis-core,praxis-graphlaw}`. Both `publish = false`.

**Correction (2026-07-17, PR #255, verified live) — supersedes the prior "assume it reaches
`~/praxis`" correction below:** every absolute `path = "/Users/sac/..."` Cargo dependency in this
workspace was found and removed. `praxis-core`'s `arazzo.rs` module (the sole consumer of the
`powl2-decompose`/`wasm4pm-arazzo` path deps this note used to describe) was deleted outright —
confirmed unused by any downstream crate (`ggen-engine`, `ggen-cli`) before removal; its dead
`chatman-common`-gated `signing` feature/module went with it. `ArazzoProjectionReceipt` is no
longer part of `praxis-core`'s public API — the table row below is corrected accordingly.
`praxis-graphlaw`'s own `powl2-decompose` dependency (genuinely load-bearing, reachable from
`ggen-engine` via `chatman/{closure,abi,powl_projection,engine}.rs`) is now satisfied by the new
`crates/powl2-decompose` workspace member (vendored, not a live path back to `~/praxis`) rather
than an absolute path — see that row below. `cargo fmt --all`'s crash (the consequence this note
used to warn about) is resolved as a side effect: nothing in this workspace's manifest graph
reaches outside `/Users/sac/ggen` anymore. Verify via `grep -rn 'path.*=.*"/Users/sac'
--include="Cargo.toml" .` → empty.

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `praxis-core` | Fused Law Object abstraction: obligation + lifecycle + receipt + OCEL (`LawObject`, `Obligation`, `Andon`, `ReceiptRecord`). No longer exposes `ArazzoProjectionReceipt`/`arazzo` (removed 2026-07-17, PR #255 — dead weight, zero downstream consumers) |
| `praxis-graphlaw` | Praxis GraphLaw law-state engine: native N3, Datalog, SPARQL 1.1, SHACL, ShEx (fork of `pbonte/roxi`) — `ggen-engine`'s default `EngineKind::GraphLaw` graph backend |
| `powl2-decompose` | Vendored from `~/praxis/crates/powl2-decompose` (2026-07-17, PR #255): Kourani et al. Stage-1 WF-net → POWL 2.0 decomposition. `praxis-graphlaw`'s dependency (not published on crates.io). `publish = false` |
| `bcinr-pddl` | Vendored from `~/bcinr/crates/bcinr-pddl` (2026-07-17, PR #255) — **not** the crates.io release: the published 26.6.26 lacks `Pddl8Error::PlanningFailed`/`.into_result()` that `praxis-graphlaw`'s code needs (version-tag/content divergence, confirmed by a failed build against the registry version before vendoring). PDDL8 → POWL tape → Prolog8 admission → OCEL → BLAKE3 receipt. `publish = false` |
| `bcinr-mfw-ir` | Vendored alongside `bcinr-pddl` (its own hard dependency, same divergence reasoning) — shared IR types/trait contracts for the multifractal-workflow planner. `publish = false` |

### Testing infrastructure

| Crate | Purpose (from Cargo.toml / lib.rs) |
|-------|-------------------------------------|
| `chicago-tdd-tools` | Dev/test-only Chicago-TDD utilities (property-testing, snapshot-testing, parameterized-testing, mutation-testing, concurrency-testing, `cli-proof`) for `ggen-engine`/`praxis-graphlaw`'s own test suites — never a runtime dependency of any shipped binary. A trimmed vendor (source only — `src/`, `build.rs`, `proc_macros/`; no `tests/`/`examples/`/`benches/`/docs) of the ~106MB `~/chicago-tdd-tools` project, copied 2026-07-17 (PR #255) because its `cli-proof` feature isn't published to crates.io yet. `publish = false`. **License note:** its own optional `wasm4pm-cognition` re-export path is unrelated to `praxis-graphlaw`'s separate `wasm4pm-cognition` dependency, which carries a BUSL-1.1 (non-OSI) license on crates.io — see that dependency's Cargo.toml comment; mitigated only because it's optional and off by default |
| `ggen-cheat-scanner` | `syn`-based AST scanner (PR #257, modeled on `~/bcinr/tools/bcinr-cheat-scanner`) detecting test-quality anti-patterns across the workspace: CHEAT-T01 (vacuous-assert), CHEAT-T02 (tautological-result-check), CHEAT-T03 (no-assertion-test), CHEAT-T04 (mock-import). Wired into `just pre-commit` via the `guard-cheat-scan` recipe; currently fails on ~464 pre-existing findings (tracked, not blocking new work — see `docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md`'s TECH-DEBT-001). `publish = false` |

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
| Builder pattern | `ggen-marketplace` | `with_*()` chain methods for optional config |
| Typestate | `ggen-marketplace` (`Draft`/`Published`) | Compile-time state transitions |
| Newtype wrappers | `ggen-marketplace` | Invariant encoding in types (e.g., `PackageId`) |
| Async traits | `ggen-lsp` (`a2a_mcp` module, feature `mcp`/`a2a`) | `#[async_trait]` with `Result` returns |
| RDF/SPARQL foundation | `ggen-graph`, `ggen-marketplace`, `ggen-engine` (via `praxis-graphlaw`/`oxigraph`) | Built on `oxigraph` triplestores (or `praxis-graphlaw`'s N3/Datalog/SHACL/ShEx engine, `ggen-engine`'s default) |
| Pipeline architecture | `ggen-engine` (Resolve → Enrich → Extract → Render → Write, live) | Multi-stage deterministic transformation — see `ggen-engine/src/sync.rs` module doc for the current pipeline |
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
