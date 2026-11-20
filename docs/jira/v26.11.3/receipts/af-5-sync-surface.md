# AF-5 Receipt: sync_surface_test FM-LAW-015 fixture remediation

Standing: ALIVE

Lane: AF-5 (implementation). Date: 2026-11-03. Repo: /Users/sac/ggen.
Owner files touched (only): `crates/ggen-cli/tests/sync_surface_test.rs` + this receipt.

## Finding (SX-1 disclosure)

Four `sync_surface_test` failures were reported as the same FM-LAW-015
missing-fixture class as the 44 remediated files (K2's skip-flag/empty-EA-graph
routes), surfaced post-census and never given the remediation pass.

## What was done

`sync_surface_test.rs` is a CLI-harness test (assert_cmd against the real `ggen`
binary, self-contained `scaffold()`), so route 2 applies: the valid-but-empty
`ea.graph.json` fixture, mirroring exactly the JSON fixture already used in
`crates/ggen-engine/tests/support/mod.rs` (`scaffold_pack`, `scaffold_multi_pack`,
`write_single_pack_project`, `write_two_pack_project`) and its K2 disclosure
comment shape. One fixture write added to `scaffold()`; no other file touched.
Route 1 (`skip_ea_admission: true`) does not apply — this file drives the real
binary, not `ggen_engine::sync::sync()` directly, so there is no `SyncOptions`
seam here.

## Per-test before/after

Baseline run `cargo test -p ggen-cli-lib --test sync_surface_test` (2026-11-03,
pre-fix): 3 passed / 3 FAILED. (Task said 4 failures; the suite observed on disk
this session carries 6 tests and 3 failed — recorded as-disk, not as-briefed.)

| test | before | after | fix |
|---|---|---|---|
| bare_sync_defaults_to_run | FAILED (FM-LAW-015, ea.graph.json absent) | pass | fixture |
| dry_run_exits_zero_and_writes_nothing | FAILED (FM-LAW-015) | pass | fixture |
| real_run_writes_outputs_and_receipt | FAILED (FM-LAW-015) | pass | fixture |
| bare_sync_without_manifest_is_run_verb_config_error | pass | pass | (FM-CONFIG-001 path unaffected) |
| watch_starts_and_runs_until_killed | pass | pass | — |
| unknown_audit_flag_is_refused_by_clap | pass | pass | — |

## Gates (exact commands, exits)

1. `cargo test -p ggen-cli-lib --test sync_surface_test` → after fix:
   `test result: ok. 6 passed; 0 failed` (finished 7.13s). ALIVE.
2. `cargo test -p ggen-cli-lib --test performance` (SX-1's gate) →
   `test result: ok. 10 passed; 0 failed; 6 ignored` (finished 5.02s). ALIVE.
   Note: first two attempts failed on environment issues, not the diff — a
   missing `tracing_attributes` dylib (concurrent-lane target/ contention) and a
   dead sccache server; clean run used `RUSTC_WRAPPER=""`. No git commands run.

## Standing

ALIVE — both gates ran this session with real output cited above.
