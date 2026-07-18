# ggen — 2-Day Change Audit (Hyperdimensional Reference)

**Scope:** 13 commits, `cbf173f8..6967f38f` (PR #255–#268), 2026-07-16 → 2026-07-18.
**Method:** 10 independent agents each analyzed the same commit range through one orthogonal
dimension (dataflow, control flow, architecture, types, errors, performance, concurrency,
testing, interfaces, invariants). Every claim below cites a file, line, and/or commit SHA.
Claims that could not be verified against actual code/diffs are marked `unverified`.
Machine-readable form: [`coordinates.json`](./coordinates.json).

---

## Executive Summary

The dominant event of this window is **the retirement of `ggen-core`** (PR #255 excludes it,
PR #259 deletes it outright) and its replacement by a vendored-from-`~/praxis` pipeline:
`ggen-engine` + `praxis-core` + `praxis-graphlaw`. Everything else in the 13 commits is either
downstream of that migration (routing fixes, dependency repointing, doc drift) or independent
hardening work (test-quality cleanup, publish-safety narrowing, a new claims ledger).

| # | What | Dimensions | Impact |
|---|------|-----------|--------|
| 1 | `ggen-core` deleted; `ggen-engine`/`praxis-core`/`praxis-graphlaw` are now the live pipeline | D1,D3,D4,D5,D7,D9 | **Breaking** |
| 2 | New Stage-0 schema-dispatch layer unifies 6 previously-inconsistent `ggen.toml`-schema call sites | D1,D2,D4,D5 | Authority-deepening |
| 3 | 5 fail-open bugs fixed (policy crash, init `--force` silently ignored, doctor misclassification, generic template errors) | D5 | Improvement |
| 4 | `receiptctl` example unions 6 packs, fixing a real cross-pack ontology collision | D1,D9 | Improvement |
| 5 | `wizard`/`sigma`/`inverse_sync` CLI commands deleted; `agent install`/`capability inspect` disabled with typed refusal | D2,D3,D9 | **Breaking** (low/medium severity) |
| 6 | Publish targets narrowed 7→4 real crates.io packages | D3 | Improvement (publish-safety) |
| 7 | New 180s receipt-chain SLO gate (`just slo-check`) — new infrastructure, not a changed baseline | D6,D10 | New capability |
| 8 | Confirmed: the 5-stage pipeline is **synchronous**, not async (corrects this audit's own initial assumption) | D7 | Clarification |
| 9 | London-TDD mock removal: 308 files, +1210/−14826 lines; new `ggen-cheat-scanner` gates `pre-commit` | D8 | Improvement |
| 10 | `ggen.construct` MCP tool response schema silently dropped `duration_ms`/`content_hash` | D9 | **Breaking**, undocumented |

**7 gaps found** (see [Gaps & Drift](#gaps--drift) below) — none block current work, two are
medium-severity and worth near-term follow-up (`C04`, `C21`, `C22`).

---

## D1 — Dataflow

**How data moves RDF → generated code, now that `ggen-core` is gone.**

- **Stage-0 dispatch inserted before the pipeline** (`crates/ggen-engine/src/sync.rs:147-169`,
  `627f84c5`). `sync()` now calls `schema_dispatch::load(root)` instead of hand-rolled TOML
  pre-parsing. Same dispatcher is now shared by `sync`, `handle_doctor`, `handle_graph_validate`,
  `build_law_engine` — closes a "6 call sites, 6 ad-hoc decisions" drift class.
- **Receipt payload hardened**: `ReceiptPayload` gained `#[serde(deny_unknown_fields)]`
  (`sync.rs:117-133`, `627f84c5`) — a tampered stored payload with extra fields now fails to
  deserialize instead of loading silently.
- **`receiptctl` example** (`271f8184`) unions 6 packs from 3 upstream projects through the real
  `[packs]` mechanism, surfacing and fixing a genuine duplicate-`wasmExport`-name bug shared
  between `wasm4pm-algorithms-pack` and `wasm4pm-facts-pack` ontologies — the fix is at the pack
  level, so it benefits every downstream consumer, not just this example.
- **Gap (C04, medium severity):** `crates/ggen-cli/src/cmds/{sync,graph,template,doctor}.rs`
  still contain **live, uncommented** `use ggen_core::...` imports against a crate that no longer
  exists on disk. They don't break the build only because their `pub mod` lines are commented
  out in `mod.rs:46,63,69`. Unlike `wizard.rs`/`sigma.rs`/`inverse_sync.rs` (deleted outright in
  PR #259), these four files were missed by that cleanup sweep — classic **Legacy Path
  Contamination** (`.claude/rules/coding-agent-mistakes.md` class 4).

## D2 — Control Flow

**CLI command routing: what exists, what was removed, what's disabled.**

| Command | Change | Evidence |
|---|---|---|
| `ggen wizard` | Deleted (1744 lines) | `9cef6e40`, `cmds/wizard.rs` |
| `ggen sigma` | Deleted (49 lines) | `9cef6e40`, `cmds/sigma.rs` |
| `ggen inverse_sync` | Deleted (359 lines) | `9cef6e40`, `cmds/inverse_sync.rs` |
| `ggen sync`/`doctor`/`graph`/`receipt` (flat verbs) | Unwired (commented `pub mod`), routed instead through `ggen-engine`'s noun/verb registry | `cmds/mod.rs:39-82` |
| `ggen agent install` | Disabled — always `Err(...)`, citing BUG-004 | `cmds/agent.rs:132-165`, `627f84c5` |
| `ggen capability inspect` | Disabled — always `Err(...)`, citing GAP-001 | `cmds/capability.rs:58-165`, `627f84c5` |

New dispatch machinery: `inject_default_verbs()` (`ggen-cli/src/lib.rs`, `cbf173f8`) rewrites bare
`ggen sync`→`ggen sync run` etc. before handing off to `clap_noun_verb::CommandRegistry`; a
`use ggen_engine as _;` force-link shim ensures `ggen-engine`'s `#[linkme::distributed_slice]`
noun/verb registrations actually link into the binary.

**Gap:** `a2a`/`framework`/`mcp` remain `#[cfg(feature="experimental")]`-gated — same disposition
as the removed `wizard`/`sigma` but not removed in this pass; unverified why the treatment is
inconsistent.

## D3 — Architecture

**Workspace membership, dependency edges, publish safety.**

- `cbf173f8`: **+7 workspace members** (`ggen-engine`, `praxis-core`, `praxis-graphlaw`,
  `powl2-decompose`, `chicago-tdd-tools`, `bcinr-pddl`, `bcinr-mfw-ir`), `ggen-core` removed from
  `members` (moved to `exclude` first, then `9cef6e40` deletes it from disk entirely).
  `f39b7972` adds an 8th member, `ggen-cheat-scanner`.
- Root `ggen` package's `[[bin]]` removed, `autobins = false` — the crate becomes library-only;
  the canonical `ggen` binary is now built from `ggen-cli-lib`.
- **Publish-flag narrowing**: `d0792d19` flips `publish = false` on `cpmp`, `genesis-core-v2`,
  `genesis-types-v2`. Real crates.io-publishable targets go from 7 to **4**: `ggen`,
  `ggen-config`, `ggen-graph`, `ggen-marketplace` (confirmed by scanning every `Cargo.toml`).
- Verified clean: no publish-safe crate has a dependency edge on a now-`publish=false` crate; the
  process-intelligence boundary (CLAUDE.md table) holds — zero bare `wasm4pm` deps found, only
  `wasm4pm-compat`.

## D4 — Type System

**What structural types changed.**

- `ReceiptRecord`, `GgenManifest`/`PackRef`, `GgenConfig`/`PackRef` were all introduced
  **whole-file** by `cbf173f8` (vendored), not incrementally modified — no later commit in this
  window touched their bodies.
- **New**: `ConfigSchemaClassification` (5 variants: `DeclarativeRules`, `Frontmatter`,
  `Ambiguous{matched}`, `Unsupported{observed_markers}`, `Malformed{diagnostic}`,
  `crates/ggen-config/src/config_schema.rs:177-198`) and `ParsedGgenToml`
  (`crates/ggen-engine/src/schema_dispatch.rs:60-67`), both from `627f84c5`. These are the
  single shared decision point backing the D1/D2 dispatch unification above.
- **Confirmed still absent**: `ArazzoProjectionReceipt`/`arazzo.rs` — already removed upstream
  before vendoring; only a dead `.tera` template referencing it survives
  (`crates/praxis-core/templates/arazzo_projection.tera`).
- **Gap (C12, low severity, documented):** `ggen_config::manifest::PackRef` (flat struct) and
  `ggen_engine::config::PackRef` (untagged `Path | Git` enum) remain two independently-defined,
  incompatible types for the same `[packs]` table name. Each type's own doc comment
  cross-references the other as "not the same type" — the new dispatch machinery manages the
  ambiguity rather than eliminating the duplication.

## D5 — Error Handling

**Fail-open → fail-closed fixes, all from `627f84c5` (PR #265).**

| Bug ID | File | Before | After |
|---|---|---|---|
| BUG-001 | `cmds/policy.rs:87-249` | One malformed lockfile entry aborted the whole `policy check` via bare `?` | Malformed entries collected separately (`LoadedPackContexts{contexts, malformed}`), valid packs still scored |
| BUG-002/003 | `cmds/init.rs:444-500` | `--force garbage` silently became `false`; error JSON bodies returned exit 0 | New `parse_bool_flag()` rejects non-`true`/`false`; error payloads now propagate as `Err` |
| BUG-004 | `cmds/agent.rs:132-165` | False-positive "already installed" checks | Disabled with typed refusal (see D2) |
| GAP-001 | `cmds/capability.rs:58-165` | Unknown-surface errors silently swallowed via `.unwrap_or_default()` | Disabled with typed refusal (see D2) |
| BUG-005 | `verbs/handlers.rs:759-799` | `handle_doctor` unconditionally used the frontmatter-only schema | Routes through the new `schema_dispatch::load` (see D1) |
| FM-GEN-008 | `crates/ggen-engine/src/error.rs:38-118` | One generic Tera-render error message | 10 typed `TemplateFailureCause` variants with distinct codes |

**Known gap (C14, by design):** `E0011`/`E0013` (`crates/ggen-config/src/manifest/validation.rs`,
unchanged since `cbf173f8`) stay warnings unless `strict_mode=true` — non-strict projects still
get non-deterministic `ORDER BY` silently logged, not blocked.

## D6 — Performance

**No benchmarks were run this session; all findings are static-diff-based.**

- The **180s receipt-chain SLO gate is brand-new infrastructure** (`justfile`'s `slo-check`
  recipe + `crates/ggen-engine/tests/receipt_chain_e2e.rs`, both introduced whole by `cbf173f8`)
  — there is no "before" to compare against; this is not a changed baseline.
- `cli_startup_performance` bench (root `Cargo.toml:775-776`) has **zero diff** across all 13
  commits.
- `.claude/rules/rust/performance.md`'s claim that `slo-check` only automates these two SLOs
  (startup bench + 180s receipt-chain wall clock) is re-confirmed accurate for this window.

## D7 — Concurrency

- **Correction to this audit's own premise**: the five-stage pipeline (`sync.rs`, `watch.rs`) is
  **fully synchronous** — no `tokio::spawn`, `async fn`, or `.await` anywhere in either file.
  `watch.rs:73,139,189,191` uses `std::sync::mpsc::channel` + `thread::spawn` (a blocking OS
  thread, not tokio) for filesystem-watch debouncing.
- Primitives in use: `Arc<dyn GraphEngine>` (immutable, no `Mutex`, `sync.rs:194,454,1172,1175`),
  `std::sync::Mutex<LawState>` (`graph.rs:760`), `static OnceLock<Mutex<()>>` test lock
  (`keys.rs:232-234`).
- Concurrency-heavy code (mock servers with `tokio::spawn`, `Arc<RwLock<...>>` caches) was
  deleted alongside `ggen-core` (`9cef6e40`) but PR #259's own commit message confirms it had
  zero live dependency edges at deletion time — no new race conditions introduced this window.
- **Unverified gaps**: no dedicated race/timeout test found for `watch.rs`'s debounce loop or
  `graph.rs`'s `Mutex<LawState>` poisoning/panic-recovery behavior.

## D8 — Testing

- `f39b7972` (PR #257): **308 files changed, +1210/−14826 lines.** Deleted whole mock-heavy
  files/dirs (`tests/mcp_a2a/` — `mock_a2a_server.rs` 1758 lines, `mock_mcp_server.rs` 1477
  lines; `crates/ggen-cli/tests/mcp_error_handling.rs` 1249 lines) plus partial edits (`tests/
  cli.rs` — `MockRegistryClient` struct + 3 tests removed, ~15 real `assert_cmd` tests kept).
  No `tests-archive/` directory exists on disk — mock tests were deleted outright, not archived
  as CLAUDE.md's migration path prescribes (only docs were archived, via `git mv`).
- **New**: `ggen-cheat-scanner` crate (480-line `syn`-based AST scanner, 4 rules — vacuous
  assert, tautological result check, no-assertion test, mock import). Wired into `just
  pre-commit` as `guard-cheat-scan`; it's a real, currently-failing gate, not decorative.
- **Gap (C21, medium severity, "Contract Drift"):** the commit message, `docs/jira/2026-07-17-
  JTBD-VERIFICATION-DISCOVERED-BUGS.md:156`, and `justfile:363`'s comment all cite **515**
  pre-existing cheat-scan findings. `CLAUDE.md` (introduced by the later doc-drift-fix pass,
  `f27a6483`) cites **~464** — with no scanner re-run evidence reconciling the two numbers. A
  plausible explanation (findings inside `ggen-core/src/*` disappearing when that crate was
  deleted in `9cef6e40`) is inference, not confirmed. This audit could not independently re-run
  the scanner — the pinned nightly toolchain in this sandbox has `rustc`/`rustfmt` but no `cargo`
  binary despite `rustup` reporting it installed.

## D9 — External Interfaces

- **`receiptctl`** (`271f8184`) and **`clap-noun-verb-cli`** (`627f84c5`) are new canonical
  examples; ~84 prior examples were moved to `examples/archive*` in the same commit (count
  unverified against current disk).
- **Breaking, undocumented (C22, medium severity):** the `ggen.construct` MCP tool
  (`crates/ggen-lsp/src/a2a_mcp/mcp_server.rs`, `cbf173f8`) switched its backing call from
  `ggen_core::codegen::pipeline::GenerationPipeline` to `ggen_engine::sync::sync(...)`. Response
  payload changed: **dropped** `duration_ms` and per-file `size_bytes`/`content_hash`; **added**
  `graph_hash_hex`; `files_count` now sourced from `report.written.len()` instead of
  `state.generated_files.len()`. No CHANGELOG or MCP schema doc found updated alongside this —
  external MCP clients reading the dropped fields will get missing keys, not an error.
- `ggen agent install`/`ggen capability inspect` remain registered (visible in `--help`) but
  always fail (see D2/D5) — a breaking behavior change for any automation on the same command
  name.

## D10 — Invariants & Proofs

- `ReceiptRecord` (`crates/praxis-core/src/receipt_record.rs`, 190 lines, new in `cbf173f8`):
  `recompute_chain_hash()` rebuilds the BLAKE3 frame independently — doc comment states it "can
  never silently diverge from the live emission path"; 4 unit tests cover tamper-detection and
  malformed-hex hard-failure.
- `keys.rs` (369 lines, new): Ed25519 key resolution with `O_CREAT|O_EXCL` file creation —
  genuine filesystem-level never-overwrite guarantee under races, not check-then-write.
- The full chain `verbs/receipt.rs → handlers::handle_receipt_verify →
  praxis-core::ReceiptRecord::recompute_chain_hash` is confirmed structurally intact post-
  `ggen-core`-removal, with zero `ggen-core` references anywhere in it. Not runtime-re-executed
  this session (sandbox toolchain limitation, see D8).
- **`docs/aps/claims.toml`** (new, `27ed0bf9`): a machine-readable claims/standing ledger.
  `release.package-contents-clean` is **self-flagged, admitted drift** (C24, low severity) — its
  `method` field says "7 real publish targets" while its own `notes` field (added the same
  commit that made the change) says the real count is now 4. The ledger's own README states
  evidence should only be updated when its falsifier is actually re-run — deliberate, not hidden.
- **Gap (C25, low severity):** `release.rustsec-quick-xml` stays `BLOCKED` with no
  `exception_admitted_by`. `just pre-commit`'s `guard-claims-schema` only runs `--schema-only`,
  so this doesn't currently gate commits — only a real `cargo publish` would hit it.

---

## Gaps & Drift

| ID | Class | Severity | Summary |
|---|---|---|---|
| C04 | Legacy Path Contamination | **Medium** | 4 CLI cmd files retain live imports of deleted `ggen-core`, only inert because their `mod` lines are commented out |
| C21 | Contract Drift (unreconciled) | **Medium** | Cheat-scan finding count: 515 (commit/jira/justfile) vs. 464 (CLAUDE.md) — no reconciling re-run found |
| C22 | Breaking Change (undocumented) | **Medium** | `ggen.construct` MCP response schema silently dropped 2 fields, added 1 |
| C12 | Type Duplication (documented) | Low | Two incompatible `PackRef` types for the same `ggen.toml` table |
| C14 | Fail-Open Behavior (by design) | Low | `E0011`/`E0013` remain warnings outside `strict_mode` |
| C24 | Contract Drift (admitted) | Low | `claims.toml`'s own publish-target-count note is self-flagged stale |
| C25 | Fail-Open Behavior (gated at publish only) | Low | `rustsec-quick-xml` BLOCKED, no exception recorded, doesn't gate `pre-commit` |

## Change × Crate Matrix

| Crate | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D10 |
|---|---|---|---|---|---|---|---|---|---|---|
| `ggen-core` (deleted) | ✓ | ✓ | ✓ | ✓ | ✓ | | ✓ | | ✓ | |
| `ggen-engine` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | | | ✓ |
| `ggen-cli` | ✓ | ✓ | ✓ | | ✓ | | | ✓ | ✓ | |
| `ggen-config` | ✓ | | | ✓ | ✓ | | | | | |
| `praxis-core` | | | ✓ | ✓ | | | | | | ✓ |
| `praxis-graphlaw` | | | ✓ | | | | | | | |
| `ggen-cheat-scanner` | | | ✓ | | | | | ✓ | | |
| `ggen-lsp` | | | | | | | | | ✓ | |
| `cpmp`/`genesis-*-v2` | | | ✓ | | | | | | | |

## Migration Guide (breaking changes)

1. **`ggen-core` symbols** (`ggen_core::codegen::*`, `ggen_core::domain::*`) → use `ggen-engine`'s
   `sync`, `graph`, `template`, `doctor` verb modules instead. If you maintain code that imports
   `ggen_core::*` directly, it will not compile against this workspace — the crate is gone.
2. **`ggen wizard`/`ggen sigma`/`ggen inverse_sync`** → no replacement; these were experimental
   and abandoned, not ported.
3. **`ggen agent install`/`ggen capability inspect`** → both always fail with a typed error
   pending a future fix (tracked as BUG-004/GAP-001). Do not script against them.
4. **`ggen.construct` MCP tool consumers** → stop relying on `duration_ms`/`content_hash` in the
   response; read `graph_hash_hex` instead for content-addressing.

---

*Generated by a 10-agent hyperdimensional analysis of `/home/user/ggen` on 2026-07-18.
Every claim above traces to a file path, line range, and/or commit SHA verified by the
originating agent. See [`coordinates.json`](./coordinates.json) for the structured form and
the interactive explorer artifact for a filterable view.*
