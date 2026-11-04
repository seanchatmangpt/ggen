# Receipt: cl-q-k2-recovery (K2 orphan recovery, ggen-engine FM-LAW-015 fixtures)

- Subject: `/Users/sac/ggen` canonical checkout, `crates/ggen-engine/tests/` failing fixtures (K2 lane, never reported).
- Authority: CL-Q lane contract (orphan recovery for K2; no git operations; exclusions G1/F2/K8/src respected).
- Date: 2026-11-03.

## Before census (cargo test -p ggen-engine --no-fail-fast, /tmp/k2_census2.log)

1000 passed / 98 failed. Per-target:
cli_boundary 32, cli_read_only_invariant_matrix 13, framework_packs_e2e 12, composition_plan_feed_test 6,
doctor_e2e 5, watch_module_test 4, fortune5_architecture_pack_e2e 4, config_schema_dispatch_e2e 4,
fortune5_testing_bblock_pack_e2e 3, cross_pack_matrix 3, ggen_release_pack_workflow_e2e 2, custom_behavior_e2e 2,
composed_packs_e2e 2, book_gap_closure_e2e 2, product_mirror_conformance 1, gall_core_pack_e2e 1,
envelope_undeclared_cap_e2e 1, economics_measured_evidence_transitive 1.

## Root causes (3, disjunctive)

1. **Missing workspace `ggen` binary (90 of 98)**: CLI-driving tests resolve `target/debug/ggen`; absent, they fail
   closed ("could not resolve the ggen binary") or fall back to a stale ambient `~/.local/bin/ggen` 26.10.11 vs
   workspace 26.10.12 (cli_boundary's version guard). NOT a test defect — environment defect. Fixed by
   `cargo build -p ggen-cli-lib --bin ggen` (230 MB binary at target/debug/ggen). NOTE: the binary was deleted
   at least once mid-session by concurrent-lane cargo activity on the shared `target/`; the final census builds
   it in the same command as the run.
2. **Product-mirror drift (1)**: FM-PACK-022/023 landed with WO-01's composition-plan feed (sync.rs:2294/2345
   call sites) but were never mirrored into `.specify/ggen-product.ttl`. Fixed by appending
   `prod:fm_PACK_22`/`prod:fm_PACK_23` individuals (citations sync.rs:2294, sync.rs:2345). TTL-only drift
   guard respected (no hand-added individual without a real call site; test re-scans source).
3. **watch_module_test fixture (4)**: `seed_fixture` lacked `ea.graph.json`, so the fail-closed FM-LAW-015 gate
   refused watch()'s initial sync. `watch()` takes no `SyncOptions` (no skip flag exists), so the honest fix is
   the same valid-but-empty EA-graph stub used by tests/support/mod.rs scaffolds, with citation comment.

## After census (same 22 targets, build+run in one command, /tmp/k2_final.log)

**155 passed / 0 failed / 22 targets ok** (`cargo test -p ggen-engine --no-fail-fast --test <22 owned targets>`,
exit 0; zero FAILED lines; `ls target/debug/ggen` non-empty). Files edited: `.specify/ggen-product.ttl`,
`crates/ggen-engine/tests/watch_module_test.rs` (2 files, both in-lane; no G1/F2/K8/src files touched; no git).

## Attribution

- G1's (sync_e2e, abb_sbb_datalog_admission_e2e, abb_sbb_stage2c_wiring_e2e), F2's (declarative_ea_admission_test),
  and K8's files untouched. F2's declarative-path admission landing is what introduced FM-PACK-022/023, whose
  missing TTL mirror this lane repaired (cross-lane dependency, disclosed here).
- Pre-existing, NOT remediated (not FM-LAW-015 class): `--doc` target fails compiling
  `blast_radius.rs:162` E0433 `ggen_scip` (src/** — not mine); `economics_measured_evidence_test` is transitive
  on receipt_chain_e2e (passed once the binary existed — verified passing in after-census).
