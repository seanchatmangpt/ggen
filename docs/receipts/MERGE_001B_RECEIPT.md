# MERGE 001B → main — Receipt

**Date:** 2026-05-30
**Merge commit:** 00befed2 (--no-ff)
**Branch merged:** feat/ggen-tpl-001-living-lsp (tip 61253195)
**main before:** 9a9fb646 → **main after:** 00befed2
**origin/main was:** d3118392 (ancestor of local main — local main was AHEAD by the ark-covenant/ProofPack lead)

## What this merge contains (honest disclosure)
- **001/001B ggen-lsp living-LSP deliverables:** GGEN-TPL-001 cross-surface detector
  (`analyzers/{out_,}mod`, `project_index.rs`, `rule_index.rs`,
  `route/diagnostic_species.rs`), live `server.rs` + headless `check.rs` wiring,
  stale-clear via keyed subtraction + residual preservation (`state.rs::tpl_clears_for`,
  `server.rs::residual_single_file_diags`), 4 test suites incl.
  `ggen_tpl_001_stale_clear.rs` (external-OCEL-log living-loop proof), + receipts.
- **PLUS bundled ggen-core clippy/refactor changes (109 files)** from a concurrent
  session (same git identity, commit c79b3d94: "resolve all clippy suppressions +
  Cargo.toml hygiene"). Intentionally carried; not hidden.

## Scratch artifacts removed (fix-forward, no history rewrite)
clippy_errors.txt (355KB), clippy_errors2.txt (98KB), parse_errors.py — removed in
commit 61253195 before the merge. Verified absent from `git ls-files` on main.

## Verification
- Pre-merge whole-graph gate (on feature branch): ggen-core 0 errors; ggen-lsp 197 passed / 0 failed.
- Post-merge build (main): ggen-core 0 errors.
- Merge strategy: `--no-ff` (preserves the 001/001B boundary commit). Local main was
  fast-forwardable from feature; --no-ff forces the marker commit. No rebase.
- Deliverables confirmed present on main; scratch files confirmed removed.

## Guardrails honored
Never rebase; force-push to main BLOCKED; canonical remote = origin (seanchatmangpt);
not pushed to jmanhype/upstream.

## Next
Push main to origin (pre-push cargo-test hook gates, ~300s), then branch
feat/ggen-out-harness-powl for the POWL v2 mission (001C GGEN-OUT-001 → 002A GGEN-HARNESS-001).
