# Integration Close — ggen companion receipt (LANE-CLOSE)

Companion to
`/Users/sac/claude-code-config-lsp/docs/jira/v26.11.3/closure/integration-close.md`
(three-repo gate table + verdicts live there). Census-only — no fixes, no git.
Date: 2026-11-03.

## Commands and exits (this repo)

- `cargo test --no-fail-fast -p ggen-abb-sbb -p ggen-engine -p ggen-sjira -p
  ggen-cli-lib -p ggen-config` — 126 test binaries, **1306 passed / 11 failed**
  (non-zero exit; full per-binary census in the parent doc).
- `cargo clippy --workspace` — **FAILED**: `could not compile ggen-cli-lib`;
  `clippy::too_long_first_doc_paragraph` at `crates/ggen-cli/src/cmds/sjira/mod.rs:1`,
  escalated by `#![deny(warnings)]` at `crates/ggen-cli/src/lib.rs:51`. Workspace clippy
  aborted at this crate; later crates unchecked. ggen-engine lib carried 35 non-blocking
  warnings.

## Failure attribution (owning lanes to claim)

| binary | file | failing tests | count |
|---|---|---|---|
| performance | crates/ggen-cli/tests/performance.rs | perf_cold_start_with_config | 1 |
| sync_surface_test | crates/ggen-cli/tests/sync_surface_test.rs | bare_sync_defaults_to_run, real_run_writes_outputs_and_receipt, watch_starts_and_runs_until_killed, dry_run_exits_zero_and_writes_nothing | 4 |
| governance_precommit_gate_count_test | crates/ggen-config/tests/governance_precommit_gate_count_test.rs | no_governance_doc_disagrees_with_real_workspace_crate_count | 1 |
| system_crate_map_parity_test | crates/ggen-config/tests/system_crate_map_parity_test.rs | repo_facts_ttl_crate_map_matches_cargo_toml_workspace_members | 1 |
| fortune5_architecture_pack_e2e | crates/ggen-engine/tests/fortune5_architecture_pack_e2e.rs | tier0_reference_fixture_generates_all_four_artifacts_and_is_idempotent | 1 |
| fortune5_testing_bblock_pack_e2e | crates/ggen-engine/tests/fortune5_testing_bblock_pack_e2e.rs | testing_bblock_pack_generates_verifier_and_suite_scripts_with_real_content; generated_suite_scripts_are_syntactically_valid_bash; generated_verifier_self_test_actually_passes_as_a_real_subprocess | 3 |

## Known-failure delta vs contract

Contract-known failures **not observed** this run: FM-LAW-015 fixture fallout (~100),
`doctor_e2e`, `envelope_caps_e2e`. `envelope_courts` 4/4 ok, `doctor_surface_test` 5/5 ok.
Read: K2's FM-LAW-015 fix appears landed; remaining 11 failures are a different, smaller
failure class (sync-surface CLI behavior, governance/parity doc-count drift, fortune5
pack e2e fixtures).

## Verdict

ggen: **GREEN-WITH-DEBT** — test suite substantially green (1306/11), clippy workspace
gate RED on one denied doc-comment lint; both recorded, nothing fixed.
