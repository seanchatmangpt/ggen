# GALL-CHECKPOINT-001B — Coordination Receipt

**mission:** GALL-CHECKPOINT-001B — live/headless wiring for the existing GGEN-TPL-001 nerve
**branch:** feat/ggen-tpl-001-living-lsp (off main; NOT committed)
**commit_before:** 9a9fb646
**date:** 2026-05-29
**verdict:** ALIVE (not fake-live) — verified against a stable whole-graph source state.

## What was wired
- **server.rs (Agent 1):** `refresh_analyzer` extended — on didOpen/didChange of a
  rule surface (.tera/.rq/.sparql/ggen.toml) it builds ProjectIndex::from_root,
  runs detect_tpl_001, and publishes per affected template URI through
  `publish_observed` (→ observe_diagnostics → publish_diagnostics). Merge-once-per-URI
  for the edited template (E0024 + GGEN-TPL-001 in one publish).
- **check.rs (Agent 2):** `check_files_in_root` folds detect_tpl_001 into the
  CheckReport; GGEN-TPL-001 ERROR increments error_count → headless gate fails.
  Single-file path unchanged; missing template stays an index issue (not reclassified).
- **STALE-CLEAR (orchestrator fix):** the original Agent 1 wiring missed the
  cross-surface clear — a query-side repair drops the now-lawful template from
  detect_tpl_001's result, so its URI was never re-published and the squiggle rotted
  ("fake-live"). Fixed in state.rs (`tpl_flagged` set + `tpl_clears_for` keyed
  subtraction) + server.rs (`residual_single_file_diags`). The clear republishes the
  template's RESIDUAL single-file diagnostics (NOT empty) so observe_diagnostics'
  per-key diff drops only the GGEN-TPL-001 key while preserving unrelated law (E0024).

## The two bugs the gate caught (successful failures)
1. **Missed clear (fake-live):** lawful repair made the diagnostic disappear from the
   analyzer result, but disappearance-as-absence meant observe_diagnostics never saw
   the clear → no RepairApplied/ReceiptEmitted. Fixed by previously-flagged −
   currently-flagged reconciliation.
2. **Over-clear (false cleanliness):** the first fix published Vec::new(), which would
   erase a co-located E0024. Fixed by publishing residual single-file diagnostics.
   Law: relation repair ≠ document cleanliness; clear = keyed subtraction + residual
   preservation.

## Tests added (orchestrator) — tests/ggen_tpl_001_stale_clear.rs
- query_side_repair_clears_template_through_living_loop — 3-link chain
  (DiagnosticRaised → RepairApplied → ReceiptEmitted) proven from the EXTERNAL OCEL
  log at .ggen/ocel/agent-edit-events.ocel.jsonl. ✅
- clear_preserves_unrelated_diagnostic_on_same_template — residual preservation
  (E0024 survives query-side TPL-001 repair). ✅
- raise_without_clear_has_no_repair_receipt — negative control (receipt is
  load-bearing, not vacuous). ✅

## changed_files_by_agent
- agent_1: crates/ggen-lsp/src/server.rs (+ orchestrator stale-clear fix)
- agent_2: crates/ggen-lsp/src/check.rs
- agent_3: crates/ggen-lsp/tests/ggen_tpl_001_living_loop.rs + fixtures
- agent_4: crates/ggen-lsp/tests/ggen_tpl_001_regression.rs + fixtures
- orchestrator: crates/ggen-lsp/src/state.rs (tpl_flagged + tpl_clears_for),
  crates/ggen-lsp/src/server.rs (residual_single_file_diags),
  crates/ggen-lsp/tests/ggen_tpl_001_stale_clear.rs + the pre-inventory + this receipt

## test_results (whole-graph, stable source state)
- ggen-core dependency: 0 errors (stable — verified after a concurrent-author flap settled)
- ggen-lsp full suite: 160 passed / 0 failed
- stale-clear proof: 3 passed
- fmt: CLEAN ; own-crate clippy (--no-deps -D warnings): 0 issues

## non_overlap_audit
PASS — Agents 1-4 disjoint. Orchestrator edits (state.rs, server.rs stale-clear,
stale_clear test) are the reserved integration seam; no agent owned state.rs.

## artifact_write_audit
PASS — analysis path is read-only; analysis_never_materializes_output_file +
headless_gate_never_materializes_output_file assert no output_file on disk.

## scope_creep_audit
PASS — OUT-001 inactive, HARNESS-001 metadata-only, no PROOF-001, no child LSP,
no hooks, no .lsp.json/plugin wiring, no sync change, no RepairFamily::SourceLaw.

## known_gaps
- ggen-core full-tree `cargo clippy` still fails on pre-existing debt (orthogonal).
- ProjectIndex rebuilt uncached per keystroke (MVP-acceptable perf note).
- .rq edits re-publish ALL affected templates (RuleIndexEntry lacks resolved query path).
- Agent 3's #[ignore]'d observe_diagnostics seam test is now SUPERSEDED by
  ggen_tpl_001_stale_clear.rs (which proves the chain via ServerState directly).
- did_close does not proactively clear standing cross-file template diagnostics.

## next_lawful_action
Phase 1.5: GGEN-OUT-001 (unbound output-path var) + consolidate duplicated SELECT
extractor + RepairFamily::SourceLaw. Phase 2: GGEN-HARNESS-001 detector.

**HARD STOP honored.** No tag, no push, no next checkpoint. Uncommitted on branch.
