<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GALL-CHECKPOINT-001B — Pre-Inventory (Phase 0)](#gall-checkpoint-001b--pre-inventory-phase-0)
  - [Phase 0 gate — PASS](#phase-0-gate--pass)
  - [The 10 confirmations](#the-10-confirmations)
  - [Wiring seams (for agents)](#wiring-seams-for-agents)
  - [Project-root resolution note](#project-root-resolution-note)
  - [Scope lock (this checkpoint)](#scope-lock-this-checkpoint)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GALL-CHECKPOINT-001B — Pre-Inventory (Phase 0)

**Mission:** GALL-CHECKPOINT-001B — LIVE WIRING FOR GGEN-TPL-001
**Branch:** `feat/ggen-tpl-001-living-lsp` (continues from Checkpoint 001/001A)
**commit_before:** `9a9fb646` (work uncommitted on branch)
**Date:** 2026-05-29
**Verified by:** orchestrator (direct reads + gate run)

## Phase 0 gate — PASS
`cargo test -p ggen-lsp` → **0 failed** (grep for `FAILED`/`error[`/non-`0 failed` is empty). Suite is GREEN before any 001B change. Agents cleared to launch.

## The 10 confirmations
| # | Claim | Verdict | Evidence |
|---|---|---|---|
| 1 | GGEN-TPL-001 detector tests pass | ✅ | `tests/ggen_tpl_001.rs` 5 passed / 1 ignored |
| 2 | full ggen-lsp suite passes pre-change | ✅ | Phase 0 gate above |
| 3 | server.rs owns didOpen/didChange | ✅ | `server.rs` `analyze_and_publish` (single-file `build_analyzer`); `did_open:75`, `did_change:81` |
| 4 | check.rs owns headless gate | ✅ | `check_files_in_root(root,paths)`, `check_files_with_routes(root,paths,with_routes)`; `CheckReport{error_count,has_errors()}` |
| 5 | state.rs owns observe_diagnostics | ✅ | `state.rs:127` `observe_diagnostics` → DiagnosticRaised…ReceiptEmitted |
| 6 | detect_tpl_001 callable without changing detector | ✅ | `analyzers::detect_tpl_001(&ProjectIndex)` (pure, mod.rs:32) |
| 7 | no emitted artifact writes needed | ✅ | analysis path is read-only by construction |
| 8 | GGEN-OUT-001 inactive | ✅ | integration test ignored; no detector |
| 9 | GGEN-HARNESS-001 metadata only | ✅ | `route/diagnostic_species.rs` detector_active=false |
| 10 | no child-LSP federation | ✅ | none present; out of scope |

## Wiring seams (for agents)
- **server.rs (Agent 1):** `analyze_and_publish(uri, text)` currently does single-file `build_analyzer(&path,text).diagnostics()` → `observe_diagnostics` → `publish_diagnostics`. EXTEND: when `path` is a rule-referenced `.tera`/`.rq`/`ggen.toml`, also build/refresh `ProjectIndex::from_root(<project root>)`, run `analyzers::detect_tpl_001(&proj)`, and MERGE the GGEN-TPL-001 diagnostics into what's published **for the affected template URI** — through the same `observe_diagnostics`→`publish_diagnostics` path (do not bypass the lifecycle). Read-only; no file writes.
- **check.rs (Agent 2):** the root-aware gates (`check_files_in_root`/`check_files_with_routes`) should additionally build `ProjectIndex::from_root(root)`, run `detect_tpl_001`, and fold each `(template_path, diags)` into the per-file `CheckReport` so GGEN-TPL-001 ERROR increments `error_count`. Don't weaken existing single-file diagnostics or error-count semantics.
- **state.rs:** NOT owned by any agent this wave. If Agent 1 finds it truly cannot route through `observe_diagnostics` without a state.rs change, it STOPS and reports in handoff (orchestrator decides).

## Project-root resolution note
`ProjectIndex::from_root` expects the dir containing `ggen.toml`. From a file URI, agents walk up parent dirs to the nearest `ggen.toml` (bounded). `ServerState` already carries a `root: PathBuf` (state.rs:72) usable as a default/fallback.

## Scope lock (this checkpoint)
ONLY live + headless wiring of the EXISTING GGEN-TPL-001. No OUT-001, no HARNESS-001 detector, no PROOF-001, no sync artifact-guard, no Claude Code hooks, no `.lsp.json`/plugin wiring, no child LSP, no `RepairFamily::SourceLaw` rename (DanglingReference mapping accepted; cleanup = known gap only), no sync change, no tag/push.
