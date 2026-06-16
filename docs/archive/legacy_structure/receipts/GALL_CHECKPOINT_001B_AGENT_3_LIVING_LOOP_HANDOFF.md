<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GALL-CHECKPOINT-001B — Agent 3 Handoff: GGEN-TPL-001 LIVING LOOP](#gall-checkpoint-001b--agent-3-handoff-ggen-tpl-001-living-loop)
  - [Files changed (only files this agent owns)](#files-changed-only-files-this-agent-owns)
  - [Tests added (5)](#tests-added-5)
  - [PROVEN vs UNOBSERVABLE (lifecycle transitions)](#proven-vs-unobservable-lifecycle-transitions)
    - [PROVEN now (GREEN, today)](#proven-now-green-today)
    - [UNOBSERVABLE via public test API (test #5, pending an `src/` seam)](#unobservable-via-public-test-api-test-5-pending-an-src-seam)
  - [Exact API gap for ReceiptEmitted observation (orchestrator request)](#exact-api-gap-for-receiptemitted-observation-orchestrator-request)
  - [Dependencies (the exact symbol/contract this agent waited on)](#dependencies-the-exact-symbolcontract-this-agent-waited-on)
  - [Test results (real output)](#test-results-real-output)
  - [Self-check (coding-agent-mistakes gate)](#self-check-coding-agent-mistakes-gate)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GALL-CHECKPOINT-001B — Agent 3 Handoff: GGEN-TPL-001 LIVING LOOP

**Mission:** Prove GGEN-TPL-001 enters the LIVING LSP loop (live publish + clear via
the headless gate; source-law route metadata; no artifact emission; best-effort the
`observe_diagnostics` OCEL chain) — NOT merely the pure detector (Agent 4 / Checkpoint-001
own that).
**Branch:** `feat/ggen-tpl-001-living-lsp`
**Agent:** 3 of GALL-CHECKPOINT-001B
**Date:** 2026-05-29
**Mode:** Chicago TDD — real fixtures, real headless gate, real `RouteRegistry`. No mocks.
**Guardrails honored:** no `src/` edits, no commit/push/tag, no mocks.

---

## Files changed (only files this agent owns)

| File | Status | Purpose |
|------|--------|---------|
| `crates/ggen-lsp/tests/ggen_tpl_001_living_loop.rs` | NEW | 5 living-loop tests |
| `crates/ggen-lsp/tests/fixtures/ggen_tpl_001_living_loop/invalid_project/ggen.toml` | NEW | binds rule: `SELECT ?name` → template consuming `row["title"]` (→ GGEN-TPL-001), `output_file = "out.txt"` |
| `.../invalid_project/queries/items.rq` | NEW | `SELECT ?name` (binds only `name`) |
| `.../invalid_project/templates/item.tera` | NEW | consumes `row["title"]` (UNBOUND → defect) |
| `.../invalid_project/schema/domain.ttl` | NEW | minimal ontology so the index resolves |

No `src/`, `route/`, `check.rs`, `server.rs`, `state.rs`, plugin/hook/release files were touched.

---

## Tests added (5)

| # | Test | Live path proven | Result |
|---|------|------------------|--------|
| 1 | `invalid_template_raises_tpl_001_through_headless_gate` | `check_files_in_root(root, surfaces, true)` surfaces a GGEN-TPL-001 ERROR + bumps `error_count` | **GREEN** (Agent 2's wiring landed) |
| 2 | `repaired_template_clears_tpl_001_through_headless_gate` | copy→TempDir, rewrite `.tera` to `row["name"]`, re-run gate → GGEN-TPL-001 clears (pre-repair raise asserted first, so the clear is not vacuous) | **GREEN** (Agent 2's wiring landed) |
| 3 | `tpl_001_route_is_source_law_only` | `RouteRegistry::seeded().select_for_diagnostic` (the SAME path `state.rs:165` observe_diagnostics uses) → `source-law.bind-projection`, `Seeded`, every step source-law, NO emitted-output marker | **GREEN** |
| 4 | `headless_gate_never_materializes_output_file` | gate over TempDir copy never writes declared `out.txt` | **GREEN** |
| 5 | `observe_diagnostics_records_receipt_chain_PENDING_SEAM` | live OCEL chain via `IntelLog::at_root` | **IGNORED (pending seam)** — fails loudly if un-ignored |

> **Timing note:** My first scoped build was blocked on the workspace artifact
> lock (Agents 1/2 compiling concurrently). By the time the build completed, Agent
> 2's `check_files_in_root` → `detect_tpl_001` fold had LANDED, so tests #1/#2 came
> up GREEN immediately rather than the RED I had written-to-contract for. The tests
> were authored to the binding contract and required ZERO edits to flip GREEN — the
> contract held exactly.

---

## PROVEN vs UNOBSERVABLE (lifecycle transitions)

### PROVEN now (GREEN, today)
- **DiagnosticRaised (live/headless)** — test #1: `check_files_in_root` raises a GGEN-TPL-001
  ERROR and increments `error_count` for the invalid project. Agent 2's
  `detect_tpl_001(ProjectIndex::from_root(root))` fold is LANDED and exercised.
- **Diagnostic clears (live/headless)** — test #2: rewrite the `.tera` to the bound `name`,
  re-run the gate → GGEN-TPL-001 disappears. Pre-repair *raise* asserted first, so the clear is
  not vacuous. This is the live publish→clear half of the loop through the deterministic gate.
- **RouteSelected lawfulness** — the route the live loop offers for GGEN-TPL-001 is
  `source-law.bind-projection`, seeded, advisory-only, references only SPARQL / Tera template /
  ggen.toml, and NEVER an emitted-output marker (`out/`, `output/`, `dist/`, `gen/`, `emitted`,
  `out.txt`). Selected through the identical registry call the live server uses at
  `state.rs:165`.
- **No artifact emission** — the headless gate is read-only; `out.txt` never appears on disk.

### UNOBSERVABLE via public test API (test #5, pending an `src/` seam)
- **The full editor-flow OCEL chain** `DiagnosticRaised → RouteSelected → RepairSuggested`
  (raise) and `RepairApplied → GatePassed → ReceiptEmitted` (clear) written by
  `ServerState::observe_diagnostics` (state.rs:127) into `IntelLog::at_root(root)` cannot be
  *driven* from a black-box test without editing `src/`.

---

## Exact API gap for ReceiptEmitted observation (orchestrator request)

`ServerState::observe_diagnostics` (state.rs:127) IS public and DOES append the full chain,
but the only public way to *drive* it through the live loop is the tower-lsp trait impl on
`GgenLanguageServer` (`did_open`/`did_change` → private `refresh_analyzer` →
`observe_diagnostics`). That is not test-drivable because:

1. `GgenLanguageServer::new(client: Client)` is the ONLY public constructor (server.rs:13).
   It requires a `tower_lsp::Client` (produced only inside `LspService::new`/`run_stdio`, not
   independently constructible) AND hard-codes `ServerState::default()` (server.rs:15) whose
   `root` is `current_dir()` — so the OCEL log would land under the test CWD, not a TempDir.
   `ServerState::with_root(root)` is public (state.rs:96) but `GgenLanguageServer` accepts no
   pre-built state and its `state` field is private.

**Smallest seam that unblocks #5** (EITHER; no behavior change; this agent did NOT add it):
- **(a)** `GgenLanguageServer::with_state(client, Arc<ServerState>)` (or `new_in_root(client, root)`)
  to pin the OCEL root to a TempDir, plus a tower-lsp in-memory duplex to yield a `Client`; OR
- **(b)** a `Client`-free, root-injected façade running the SAME publish path as
  `refresh_analyzer`, e.g. `ServerState::analyze_and_observe(&self, uri, content)` that builds
  the analyzer, runs cross-surface `detect_tpl_001`, calls `observe_diagnostics`, and returns
  the diagnostics (no `publish_diagnostics`). Test then:
  `with_root(TempDir)` → `analyze_and_observe(invalid)` (raise) →
  `analyze_and_observe(repaired)` (clear) → `IntelLog::at_root(temp).read()` → assert the
  activity sequence. (Façade must ALSO route the cross-surface TPL-001 diagnostic into
  `observe_diagnostics` — today `refresh_analyzer` only observes single-file analyzer
  diagnostics; same gap Agent 1 is closing.)

Full detail is embedded in the test #5 doc-comment + panic message.

---

## Dependencies (the exact symbol/contract this agent waited on)

- **Agent 2 — `ggen_lsp::check::check_files_in_root(root, paths, with_routes) -> CheckReport`**
  folds `detect_tpl_001(ProjectIndex::from_root(root))` into the per-file reports so a
  GGEN-TPL-001 ERROR appears in `report.files[*].diagnostics` and `error_count >= 1`.
  **SATISFIED** — landed during the build window; tests #1/#2 are GREEN with no test changes.
- **Agent 1 (server.rs)** — needed only for the live *editor* path of test #5 (still also
  blocked on the `src/` seam above; the headless gate path #1/#2 does not depend on it).

---

## Test results (real output)

```
$ cargo test -p ggen-lsp --test ggen_tpl_001_living_loop -- --nocapture
   Compiling ggen-lsp v26.5.29 (/Users/sac/ggen/crates/ggen-lsp)
    Finished `test` profile [unoptimized + debuginfo] target(s) in 5m 03s
     Running tests/ggen_tpl_001_living_loop.rs

running 5 tests
test observe_diagnostics_records_receipt_chain_PENDING_SEAM ... ignored, PENDING SEAM: no public, root-injectable way to drive GgenLanguageServer.did_open / ServerState::observe_diagnostics from a test; see body for the exact src/ seam the orchestrator must add
test tpl_001_route_is_source_law_only ... ok
test invalid_template_raises_tpl_001_through_headless_gate ... ok
test headless_gate_never_materializes_output_file ... ok
test repaired_template_clears_tpl_001_through_headless_gate ... ok

test result: ok. 4 passed; 0 failed; 1 ignored; 0 measured; 0 filtered out; finished in 0.01s
```

**Interpretation:** all 4 active living-loop tests GREEN; #5 ignored (pending the documented
`src/` seam). The two raise/clear tests (#1/#2) prove the live/headless GGEN-TPL-001 publish +
clear through `check_files_in_root` now that Agent 2's wiring has landed. Tests were written to
the binding contract and required ZERO edits to come up GREEN — the predicted-RED window simply
closed before my build finished (artifact-lock contention with the concurrent agents).
(The long compile time is the workspace cold-rebuild after the concurrent Agent 1/2 changes,
not test-runtime — the test body runs in 0.01s.)

---

## Self-check (coding-agent-mistakes gate)

- **Q1 real state changed:** new test target + fixture tree on disk; running the gate proven
  read-only (no `out.txt`).
- **Q2 authoritative stage:** validation/gate stage (`check_files_in_root`) + route selection
  (`RouteRegistry::select_for_diagnostic`) — the same path the editor + MCP use.
- **Q3 negative path fails correctly:** repaired template → GGEN-TPL-001 must *clear* (#2); a
  detection-disabled regression would fail the pre-repair raise sanity, not pass vacuously.
- **Q4 invariant:** source-law-only route (no emitted-output marker) — test #3 enforces it.
- **Q5 legacy path:** n/a (tests only; added no bypass; #5 left as a visible failing seam
  rather than a vacuous pass that would hide the gap).
- **Q6 proof object:** captured `cargo test` output above; GREEN/RED/IGNORED breakdown.
- **Deepens authority OR reduces drift:** deepens authority — pins the live gate + route law to
  executable tests so the wired path cannot silently diverge from the source-law doctrine.
