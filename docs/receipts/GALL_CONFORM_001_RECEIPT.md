---
receipt: GALL_CONFORM_001_RECEIPT
date: 2026-06-02
status: STAGE_0_COMPLETE
gate: Inspection Gate (prerequisite)
authority: GAP_FIRMAMENT_002_LIVING_LSP_GALL_CODEMANUFACTORY (GAP_003 remediation)
pre_inventory: GALL_CONFORM_001_PRE_INVENTORY.md (2026-05-30)
---

# GALL-CONFORM-001 Migration Receipt

## Purpose

This receipt documents the OCEL-retirement migration defined in
`GALL_CONFORM_001_PRE_INVENTORY.md`: retiring ggen's internal PM algorithms in favor
of wasm4pm as the execution oracle, and migrating ggen's internal OCEL types to
`ocel-core` as the canonical OCEL 2.0 type authority.

Doctrine: ggen **emits** OCEL; wasm4pm **parses, mines, conforms, and IO's** it.

---

## Stage 0 — Pin the Dependency (COMPLETE)

**Date completed:** 2026-06-02
**Evidence:** `/Users/sac/ggen/crates/ggen-graph/Cargo.toml`

```toml
# GALL-CONFORM-001 Stage 0: pin ocel-core from wasm4pm as the canonical OCEL type authority.
# ggen is an OCEL *producer*; wasm4pm is the PM authority (per GGEN-NEEDS.md §0).
# This dependency gates Stages 1-4 of the PM-retirement migration.
ocel-core = { path = "/Users/sac/wasm4pm/crates/ocel-core" }
```

**ocel-core verified:**
- Location: `/Users/sac/wasm4pm/crates/ocel-core/`
- Version: 26.5.30
- Minimal deps: `serde`, `serde_json`, `chrono` only
- OCEL 2.0 types: `OCEL`, `OCELEvent` (serde `"type"`), `OCELObject` (serde `"type"`),
  `OCELRelationship` (`objectId`), `OCELEventAttribute`, `OCELObjectAttribute`,
  `OCELType`, `OCELAttributeValue`
- Intake: `ocel-core::intake::NDJsonStream<R: BufRead>` iterator available

**Acceptance:** `cargo make check` passes with `ocel-core` in scope.

---

## wpm CLI Oracle Contract — VERIFIED

**Date verified:** 2026-06-02
**Source:** `/Users/sac/wasm4pm/crates/wasm4pm-cli/src/main.rs` and
`/Users/sac/wasm4pm/crates/wasm4pm-cli/src/commands/mining.rs`

The `wpm` CLI provides the following subcommand contract, verified by reading source:

```
wpm mining discover <input> [--algo heuristic] [--activity-key concept:name]
wpm mining conformance <log> <model> [--activity-key concept:name]
```

- `wpm mining discover` calls `wasm4pm_algos::heuristic::discover_heuristic`
- `wpm mining conformance` calls
  `wasm4pm_algos::conformance::check_conformance_token_replay`
- Both accept `.xes` or `.json` event logs

**Constraint for Stage 3:** Stage 3 (`mine()` externalization) must shell out to
`wpm mining discover` and `wpm mining conformance`. The DFG/conformance invocation
must produce output ggen can parse without importing `wasm4pm-algos` as a linked dep.

---

## NDJSON Truncated-Line Tolerance Gap — DOCUMENTED

**Date:** 2026-06-02
**Gap source:** `GALL_CONFORM_001_PRE_INVENTORY.md` §4/§6

**Current state:**
- `IntelLog::read` (ggen) silently skips truncated trailing lines (`continue` on parse
  error) — required crash-safety invariant, proven by
  `truncated_trailing_line_is_skipped` test in `ggen-lsp/src/intel/log.rs`.
- `ocel-core::intake::NDJsonStream::next` (wasm4pm) returns `Err` on a bad line
  (line 99-101 of `intake.rs`).

**Resolution path (Stage 2 prerequisite):**
Option A (preferred): Modify `ocel-core::intake::NDJsonStream` to detect a truncated
*final* line (EOF after partial content without newline) and yield `None` instead of
`Err`. This requires distinguishing between a mid-file corrupt line (should remain
`Err`) and a truncated trailing line (should yield `None`/EOF).

Option B (caller-side): In ggen, wrap the `NDJsonStream` iterator so that a trailing
`Err` after successful reads is converted to `None`. This is less clean but avoids
touching ocel-core.

**Acceptance for Stage 2:** The crash-safety invariant from
`truncated_trailing_line_is_skipped` must pass against the new intake path.

---

## Stage 1 — Type Swap (PENDING)

**Status:** Not started
**Prerequisite:** Stage 0 complete (DONE)
**Target files:**
- `crates/ggen-lsp/src/intel/events.rs` — repoint builders to `ocel_core::OCELEvent`/
  `OCELObject`/`OCELRelationship`
- Preserve all `activity::*` and `obj_type::*` value strings (serialized as values,
  not keys — substring-mineable constraint satisfied per §3.2 analysis)
- Audit `ggen_tpl_001_living_loop.rs` and `ggen_tpl_001_stale_clear.rs` for
  key-bearing substring assertions; co-update in same commit if found

**Acceptance:** `ggen_tpl_001_*` and `ggen_harness_001_living_loop` pass;
6-link chain remains substring-mineable.

---

## Stage 2 — Swap the Reader (PENDING)

**Status:** Not started
**Prerequisite:** Stage 1 complete + truncated-line tolerance gap resolved
**Target files:**
- `crates/ggen-lsp/src/intel/log.rs` — replace `IntelLog::read` with
  `ocel-core::intake::NDJsonStream` fold
- `crates/ggen-lsp/src/intel/metrics.rs` — repoint `OcelLog` type
- `crates/ggen-lsp/src/intel/replay.rs` — repoint `IntelLog::read` calls
- `crates/ggen-graph/src/ocel/projection.rs` — delete `extract_ocel`
- `crates/ggen-graph/src/ocel/gall_projection.rs` — delete `extract_self_audit`/
  `query_relationship`

**Acceptance:** metrics/replay tests pass.

---

## Stage 3 — Externalize Discovery and Conformance (PENDING)

**Status:** Not started
**Prerequisite:** Stage 2 complete + `wpm` CLI oracle verified (DONE above)
**Target files:**
- `crates/ggen-lsp/src/intel/mine.rs` — shell out to `wpm mining discover` and
  `wpm mining conformance` instead of `EvidenceProjector::project_ocel` + `discover_dfg`
  + `check_lifecycle_order`; keep promotion/receipt/history glue
- Delete `crates/ggen-graph/src/ocel/dfg.rs`
- Delete `crates/ggen-graph/src/ocel/conformance.rs`
- Remove `lib.rs:34` re-exports of `check_guard`, `check_lifecycle_order`,
  `discover_dfg`, `DfgEdge`

**Acceptance:** `mine_*` tests pass against real `wpm`.

---

## Stage 4 — Retire ocel_types.rs (PENDING)

**Status:** Not started
**Prerequisite:** Stages 1-3 complete
**Target files:**
- Delete `crates/ggen-graph/src/ocel/ocel_types.rs` once all symbol references
  to `ggen_graph::ocel::Ocel*` are gone
- Prune `crates/ggen-graph/src/ocel/mod.rs` re-exports
- Decide final home for `self_audit.rs` and `coverage.rs`
  (KEEP as ggen-domain audit, migrated to `ocel-core` types)

**Acceptance:** Full `cargo make test` + `clippy -D warnings` green.
Issue an updated version of this receipt with `status: COMPLETE` upon Stage 4 passing.

---

## Evidence Ledger

| Claim | Source |
|-------|--------|
| Stage 0 dep pinned in ggen-graph/Cargo.toml | File read: ggen/crates/ggen-graph/Cargo.toml |
| ocel-core version 26.5.30 ships at wasm4pm | File read: wasm4pm/crates/ocel-core/Cargo.toml |
| ocel-core OCEL 2.0 serde names | File read: wasm4pm/crates/ocel-core/src/lib.rs |
| NDJsonStream iterator exists in intake.rs | File read: wasm4pm/crates/ocel-core/src/intake.rs |
| NDJsonStream returns Err on bad line (line 99-101) | File read: wasm4pm/crates/ocel-core/src/intake.rs |
| IntelLog::read skips truncated line (crash-safe) | File read: ggen/crates/ggen-lsp/src/intel/log.rs |
| wpm mining discover + conformance subcommands exist | File read: wasm4pm/crates/wasm4pm-cli/src/main.rs |
| wpm mining subcommand implementation | File read: wasm4pm/crates/wasm4pm-cli/src/commands/mining.rs |
| Proof tests grep JSONL value substrings (resilient to key rename) | File read: ggen/crates/ggen-lsp/tests/ggen_tpl_001_stale_clear.rs:84-91, ggen_tpl_001_living_loop.rs:175-181 |
| Pre-inventory analysis | GALL_CONFORM_001_PRE_INVENTORY.md (2026-05-30) |

---

## Relationship to ALIVE Receipt

This receipt (Stage 0 complete, Stages 1-4 pending) is cited by
`LIVING_LSP_ALIVE_001.yaml` as the CodeManufactory migration receipt. The ALIVE
receipt documents the current gate state: the CodeManufactory manufacturing pipeline
exists and is ALIVE at the GALL-CHECKPOINT-002 boundary; the GALL-CONFORM-001
migration is the next-phase work that further aligns ggen's internal types with the
wasm4pm authority boundary.

The ALIVE verdict for Firmament Inspection Gate admission does NOT require Stages 1-4
to be complete — it requires the manufacturing pipeline to be operational and the
OCEL dependency path to be established. Both conditions are met at Stage 0.
