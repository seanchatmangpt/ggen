<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GALL-CHECKPOINT-001B — Agent 2 (Check Gate) Handoff](#gall-checkpoint-001b--agent-2-check-gate-handoff)
  - [Files changed](#files-changed)
  - [Entry points touched](#entry-points-touched)
  - [New private helpers added to check.rs](#new-private-helpers-added-to-checkrs)
  - [Exact error-count behavior (how GGEN-TPL-001 increments it)](#exact-error-count-behavior-how-ggen-tpl-001-increments-it)
  - [Acceptance behavior wired (vs. task spec)](#acceptance-behavior-wired-vs-task-spec)
  - [Dedup verification (CONFIRMED — no double-counting)](#dedup-verification-confirmed--no-double-counting)
  - [Inline tests added (match existing check.rs `#[cfg(test)]` patterns)](#inline-tests-added-match-existing-checkrs-cfgtest-patterns)
  - [Tests run + results](#tests-run--results)
  - [Known gaps](#known-gaps)
  - [Guardrails honored](#guardrails-honored)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GALL-CHECKPOINT-001B — Agent 2 (Check Gate) Handoff

**Mission:** Live wiring of GGEN-TPL-001 into the headless `ggen lsp check` gate.
**Owned file:** `crates/ggen-lsp/src/check.rs` (only file written).
**Branch:** `feat/ggen-tpl-001-living-lsp`
**Date:** 2026-05-29
**Status:** IMPLEMENTED, COMPILES CLEAN, ALL TESTS GREEN (verified).

---

## Files changed
- `crates/ggen-lsp/src/check.rs` — `git diff --stat` = **1 file changed, 254 insertions(+)** (purely additive; no deletions, no rewrites of existing functions). File is 771 lines (518 baseline + 253 net additions). `build_analyzer` import and its use at `check_content` (line 282) untouched.

No other files were created or modified. No stray files left in the tree.

---

## Entry points touched
- **`check_files_in_root(root, paths, with_routes)`** (line 315) — the single root-aware gate. After the existing per-file single-file loop and before computing `route_summary`, added:
  ```rust
  error_count += fold_tpl_001(root, &mut files, registry.as_ref());   // line 360
  ```
- **`check_files_with_routes(paths, with_routes)`** — unchanged source, but it delegates to `check_files_in_root(Path::new("."), paths, with_routes)`, so the TPL-001 fold covers the `--with-routes` path too (both required paths) via the single insertion.
- **`check_content` (single-file, no root)** — **untouched**. Single-file behavior and its E0024-only Tera diagnostics are unchanged, as required.

## New private helpers added to check.rs
- **`fn fold_tpl_001(root, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>) -> usize`** (line 375)
  1. Builds `crate::project_index::ProjectIndex::from_root(root)`. On `Err` (no/invalid `ggen.toml`, including the cwd `"."` default) returns `0` — best-effort; never disturbs the single-file reports.
  2. Runs the existing pure detector `crate::analyzers::detect_tpl_001(&project)` (no duplicated extraction).
  3. For each `(template_path, diags)`: appends the diagnostics to the matching `FileReport` (matched by `paths_match`), or pushes a new `FileReport` for that template when it is not already among `files`.
  4. Returns the count of newly-added ERROR diagnostics so the caller keeps `error_count` exact.
  5. When `registry` is `Some` (`--with-routes`), resolves a `RoutePlan` per diagnostic via `crate::route::route_plan_for_diagnostic(reg, d, &template_content)`, using the **template file content** (read best-effort with `read_to_string(...).unwrap_or_default()`) as the route edit-site context — because the TPL-001 edit site lives in the template, not the manifest. Same route engine as every other channel.
- **`fn paths_match(a, b) -> bool`** (line 411) — exact string equality first; falls back to `std::fs::canonicalize` comparison so a relative gate path matches the index-resolved absolute `template_path`. Best-effort (canonicalize failure ⇒ only exact-string holds).

---

## Exact error-count behavior (how GGEN-TPL-001 increments it)
- Single-file pass: unchanged — `error_count` is incremented per ERROR diagnostic produced by `check_content`, exactly as before.
- TPL-001 fold: `fold_tpl_001` returns the number of GGEN-TPL-001 **ERROR** diagnostics it added, and the caller does `error_count += <that count>`. Each diagnostic from `unbound_projection_diagnostics` is `DiagnosticSeverity::ERROR` with code `GGEN-TPL-001` (confirmed in `analyzers/tera_analyzer.rs:419-440`).
- Net effect: a project with rule `SELECT ?name` + template `row["title"]` yields ≥1 GGEN-TPL-001 ERROR ⇒ `error_count >= 1` ⇒ `has_errors() == true` ⇒ `exit_code() == 1` (gate fails). Repairing the template to `row["name"]` ⇒ detector returns empty ⇒ no increment ⇒ gate passes.

## Acceptance behavior wired (vs. task spec)
1. `SELECT ?name` + `row["title"]` ⇒ gate fails. ✅
2. Repair to `row["name"]` ⇒ gate passes. ✅
3. Missing template ⇒ stays a `RuleIndexEntry.issues` index problem, NOT GGEN-TPL-001 — `detect_tpl_001` already `continue`s on `template_content: None`; `fold_tpl_001` never reclassifies. ✅
4. No output-file materialization; `ggen-lsp` stays READ-ONLY (the only filesystem touch is `read_to_string` of the already-resolved template purely for route edit-site context). ✅
5. `--with-routes` preserved; TPL-001 routes resolve through the existing seeded route (Checkpoint-001 registration maps `GGEN-TPL-001` → `RepairFamily::DanglingReference`, confirmed in `route/registry.rs:130`). ✅

---

## Dedup verification (CONFIRMED — no double-counting)
In the headless path, `build_analyzer(path, content)` constructs the Tera analyzer via `TeraAnalyzer::new_from_content(content, "")` — i.e. **empty** `available_vars`. `TeraAnalyzer::diagnostics()` only calls `unbound_projection_diagnostics(...)` when `!self.available_vars.is_empty()` (`tera_analyzer.rs:131`). So the single-file Tera analyzer in the gate emits **E0024 (syntax) ONLY — never GGEN-TPL-001**. Appending `detect_tpl_001` results in `fold_tpl_001` therefore does not duplicate any diagnostic. If a `.tera` is already in `paths` (e.g. via `discover_law_surfaces`), its single-file report holds only E0024 and `fold_tpl_001` appends the TPL-001 diagnostics to that same report — additive, not duplicative.

---

## Inline tests added (match existing check.rs `#[cfg(test)]` patterns)
Added to the `mod tests` block in check.rs:
- `root_aware_gate_folds_tpl_001_and_fails` (line ~603) — `SELECT ?name` + `row["title"]` ⇒ `has_errors()`, `error_count >= 1`, template report carries a `GGEN-TPL-001` diagnostic.
- `repaired_template_has_no_tpl_001` — template `row["name"]` ⇒ `!has_errors()`, no TPL-001 remains.
- `missing_template_is_not_reclassified_as_tpl_001` — template file absent ⇒ no synthesized TPL-001 (stays an index issue).
- `tpl_001_resolves_a_route_with_routes` — with routes on, the TPL-001 diagnostic resolves a non-empty route.

All four use real `tempfile::TempDir` + real `ggen.toml`/`.tera` on disk (Chicago TDD; no mocks). No `unwrap`/`expect` in non-test code; the one fallible read in `fold_tpl_001` uses `unwrap_or_default()`.

---

## Tests run + results
- **`cargo check -p ggen-lsp`** → **PASS**: `Finished dev profile target(s) in 4m 50s`, `Checking ggen-lsp`, zero `error[...]` lines.
- **`cargo test -p ggen-lsp --lib`** → **PASS**: `test result: ok. 275 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out` (271 baseline + 4 new = 275). No existing test weakened or broken.
- The four new check-gate tests all pass:
  - `check::tests::root_aware_gate_folds_tpl_001_and_fails ... ok`
  - `check::tests::repaired_template_has_no_tpl_001 ... ok`
  - `check::tests::missing_template_is_not_reclassified_as_tpl_001 ... ok`
  - `check::tests::tpl_001_resolves_a_route_with_routes ... ok`

---

## Known gaps
- `paths_match` relies on `canonicalize` for relative↔absolute matching; when the index resolves `template_content: Some(...)`, the `template_path` is an existing resolved path, so matching is reliable. A path form that cannot canonicalize falls back to exact-string only (acceptable).

---

## Guardrails honored
No commit/push/tag. No edits to any file other than the owned `crates/ggen-lsp/src/check.rs`. No artifact materialization (read-only LSP preserved). No destructive git operations. No stray files left behind.
