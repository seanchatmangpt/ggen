<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GALL-CHECKPOINT-001B — Agent 4 (Regression / No-Scope-Creep) Handoff](#gall-checkpoint-001b--agent-4-regression--no-scope-creep-handoff)
  - [Files changed (only files in this agent's ownership)](#files-changed-only-files-in-this-agents-ownership)
  - [Each boundary assertion + what it locks](#each-boundary-assertion--what-it-locks)
  - [Tests run + results](#tests-run--results)
    - [One self-inflicted RED, fixed (test &#035;7)](#one-self-inflicted-red-fixed-test-7)
  - [API surface verified against landed source](#api-surface-verified-against-landed-source)
  - [Known gaps](#known-gaps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GALL-CHECKPOINT-001B — Agent 4 (Regression / No-Scope-Creep) Handoff

**Mission:** GALL-CHECKPOINT-001B — LIVE WIRING FOR GGEN-TPL-001
**Agent:** 4 of 001B — lock the checkpoint boundaries against scope-creep and regression
**Branch:** `feat/ggen-tpl-001-living-lsp`
**Date:** 2026-05-29
**Adds behavior:** NO. Boundary-asserting tests + hermetic fixtures only.

## Files changed (only files in this agent's ownership)

New test file:
- `crates/ggen-lsp/tests/ggen_tpl_001_regression.rs`

New hermetic fixtures (real files, no mocks):
- `crates/ggen-lsp/tests/fixtures/ggen_tpl_001_regression/valid/` — `ggen.toml`, `schema/domain.ttl`, `queries/items.rq`, `templates/item.tera` (SELECT `?name`; template consumes `name` / `row["name"]`)
- `crates/ggen-lsp/tests/fixtures/ggen_tpl_001_regression/invalid/` — same shape but template reads `row["title"]` (unbound)
- `crates/ggen-lsp/tests/fixtures/ggen_tpl_001_regression/unbound_output_path/` — `output_file = "out/{{ slug }}.txt"` (`slug` unbound), template BODY clean

NO `src/`, `route/`, `check.rs`, `server.rs`, `state.rs`, plugin/hook/release files were edited. No commit/push/tag.

## Each boundary assertion + what it locks

| # | Test | What it locks |
|---|------|---------------|
| 1a | `out_001_remains_inactive_in_species_registry` | `species_for("GGEN-OUT-001")` is `None` OR `detector_active == false`. Proves OUT-001 was not accidentally activated. (Currently absent from registry → inactive by absence.) |
| 1b | `out_001_not_emitted_for_unbound_output_path` | `detect_tpl_001` over the unbound-OUTPUT-PATH fixture emits no `GGEN-OUT-001` (and no `GGEN-TPL-001`, since the body is clean). Proves OUT-001 detection did not secretly start. |
| 2 | `harness_001_remains_metadata_only` | `species_for("GGEN-HARNESS-001")` exists with `detector_active == false`. Proves no harness-mismatch detector was wired. |
| 3 | `detect_tpl_001_runs_without_any_child_lsp` | Pure in-process call (no subprocess/socket/env) emits `GGEN-TPL-001` on the invalid fixture. Proves no child-LSP dependency crept into detection. |
| 4 | `valid_fixture_stays_clean` | Valid fixture (SELECT `?name`, template `name`) → no `GGEN-TPL-001`. |
| 5 | `invalid_fixture_emits_only_tpl_001` | Invalid fixture → `GGEN-TPL-001` at ERROR severity and NO other `GGEN-*` species (`GGEN-OUT-001`/`GGEN-HARNESS-001`/`GGEN-PROOF-001`); any future `GGEN-` code other than TPL-001 also fails. Locks the blast radius. |
| 6 | `analysis_writes_no_emitted_output_files` | TempDir copy of invalid fixture: `ProjectIndex::from_root` + `detect_tpl_001` + `check_files_in_root(root, paths, true)` observe the ERROR yet materialize NO `out.txt` and NO `out/` dir. Locks read-only analysis (no emit-during-analysis). |
| 7 | `tpl_001_route_targets_only_source_law` | `RouteRegistry::seeded().select_for_diagnostic(GGEN-TPL-001)`: every step is advisory (`NoOp`), every step title references source law (SPARQL / Tera template / ggen.toml), and NO step title or route description references an emitted-output marker (`out/`, `output/`, `dist/`, `gen/`, `out.txt`, `emitted`). Locks: emitted-output path is never a repair target. |

## Tests run + results

Command:
```
cargo test -p ggen-lsp --test ggen_tpl_001_regression -- --nocapture
```

Real output (final GREEN run, tail):
```
running 8 tests
test harness_001_remains_metadata_only ... ok
test out_001_remains_inactive_in_species_registry ... ok
test tpl_001_route_targets_only_source_law ... ok
test out_001_not_emitted_for_unbound_output_path ... ok
test valid_fixture_stays_clean ... ok
test detect_tpl_001_runs_without_any_child_lsp ... ok
test invalid_fixture_emits_only_tpl_001 ... ok
test analysis_writes_no_emitted_output_files ... ok

test result: ok. 8 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.01s
```

**8 passed / 0 failed / 0 ignored.** No transient RED from Agent 2 wiring: test #6's `check_files_in_root` path is already landed on the branch and the headless gate correctly observes the GGEN-TPL-001 ERROR while writing nothing.

### One self-inflicted RED, fixed (test #7)

First run of test #7 (`tpl_001_route_targets_only_source_law`) failed: my forbidden-marker list included the bare word `"emitted"`, which substring-matched the route *description* — `"...Advisory only; never edits emitted output."`. That phrase is a **disclaimer** of output editing (the invariant being LOCKED), not a violation. Fix: narrowed the forbidden set to concrete emitted-output **path** fragments (`out/`, `output/`, `dist/`, `gen/`, `out.txt`); dropped the bare `"emitted"` token. No `src/` change — test-only correction. Re-run: GREEN.

## API surface verified against landed source

- `ggen_lsp::project_index::ProjectIndex::from_root(&Path) -> Result<ProjectIndex, IndexError>`
- `ggen_lsp::analyzers::detect_tpl_001(&ProjectIndex) -> Vec<(PathBuf, Vec<Diagnostic>)>` (pure; skips rules with no resolved template content)
- `ggen_lsp::route::species_for(&str) -> Option<&'static DiagnosticSpecies>` with `.detector_active`
- `ggen_lsp::route::RouteRegistry::seeded().select_for_diagnostic(&Diagnostic) -> Option<&RepairRoute>`
- `ggen_lsp::route::{EditTemplate, RepairRoute}` — steps are `EditTemplate::NoOp` (advisory)
- `ggen_lsp::check::check_files_in_root(&Path, &[PathBuf], bool) -> CheckReport` (already folds GGEN-TPL-001 via the project index; read-only)

GGEN-TPL-001 maps to `RepairFamily::DanglingReference`, owning that family exclusively; the seeded route is `source-law.bind-projection` (3 advisory NoOp steps: SPARQL SELECT, Tera template, ggen.toml rule).

## Known gaps

- **GGEN-OUT-001 not yet a registered species.** Test 1a is satisfied by *absence*; when OUT-001 is later added as metadata it must land with `detector_active == false` or this guard fires (intended).
- **GGEN-PROOF-001 is not in the registry at all** — test 5 only asserts it does not appear in emitted diagnostics (correct for this checkpoint); there is no metadata guard for it because it is out of 001B scope.
- These tests assert *behavioral boundaries* only; they add no detection logic. The active GGEN-TPL-001 detector behavior is covered by the sibling suite `tests/ggen_tpl_001.rs` (Agent 4 of Checkpoint-001).
