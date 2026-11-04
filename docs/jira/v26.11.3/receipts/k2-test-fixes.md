# K2 Receipt: ggen-engine Test FM-LAW-015 Fixes

Standing: LANDED

Lane: K2 · Repo: /Users/sac/ggen · Date: 2026-11-3 (final) · Status: LANDED
Originally assigned: sync_e2e.rs + any ggen-engine test file failing FM-LAW-015 except
abb_sbb_datalog_admission_e2e.rs (G1's). No src/** edits. No git commands run.

## Scope note (coordinator nudge honored)

Mid-session, coordinator narrowed scope: CL-D and CL-Q (FM-LAW-015 fixture lanes) were
concurrently remediating overlapping files. K2 therefore closed with one bounded
verification run; files CL-D/CL-Q patched (witnessed via mtimes at 15:07 during K2's
sweeps: generation_rules_e2e.rs, law_gates_closure_test.rs, ontology_imports_e2e.rs)
are marked superseded below.

## Baseline

`cargo test -p ggen-engine --test sync_e2e` → 1 passed / 10 failed, all FM-LAW-015
(EA admission gate refusing scaffolds predating G1's fail-closed gate, sync.rs:1058).
G1's `skip_ea_admission: bool` SyncOptions field (sync.rs:225) was already landed.
Full blast radius measured: ~49 test targets, 331 FM-LAW-015 mentions in one
no-fail-fast sweep.

## K2 fixes (landed, on disk, verified green this session)

Route (a) `skip_ea_admission: true` — library-level tests pinning non-EA laws:
- sync_e2e.rs (10 sites) — ok. 11 passed; 0 failed
- combinatorial_matrix.rs (3 sites) — ok. 7 passed; 0 failed
- 13 further files via batch sed: multi_template_determinism, receipt_genesis_ceiling,
  receipt_head_vs_log, receipt_signing_evidence, sync_closure_golden,
  determinism_independent_reload, determinism_query_reexecution, envelope_otel_span,
  envelope_receipt_binding, dataset_bridge, datatype_coercion, full_loop_ocel_v2,
  projection_determinism, constitution_obligation, consumer_mode_fixture_only
- envelope_caps_e2e.rs (5 sites) — ok. 4 passed; 0 failed
- gall_checkpoint_001_replay_e2e.rs (11 skip sites + fixture) — ok. 11 passed; 0 failed

Route (b) minimal valid-but-empty ea.graph.json fixture — CLI-boundary tests (no CLI
flag exists; see For-G1 below). Shape identical to G2's: zero candidate SBBs; sync.rs's
admit loop iterates candidate_sbbs so empty admits trivially and honestly.
- tests/support/mod.rs (scaffold_pack, write_single_pack_project,
  write_two_pack_project; assert_gate_refuses→skip flag) — disclosed shared-module repair
- composed_packs_e2e.rs — ok. 2 passed; 0 failed
- pack_behaviors_cli_e2e.rs (3 scaffolds; one write-before-mkdir bug found+fixed) —
  ok. 3 passed; 0 failed
- custom_behavior_e2e (ok. 2), framework_packs_e2e (ok. 12), gall_agent_executor_e2e
  (ok. 2), ocel_determinism_e2e (ok. 3), verify_pack_evidence_loop_e2e (ok. 1),
  verify_pack_gate_sabotage_e2e (ok. 8), write_behaviors_cli_e2e (ok. 12),
  write_stage_partial_failure_receipt_binding (ok. 1),
  frontmatter_{cardinality,fortune5_hardening,fields,matchers,maximalism,rdf}_e2e
  (ok. 7/7/14/3/2/5), otel_span_presence_test (ok. 2), shape_shacl_enforcement_e2e
  (ok. 2), shape_shacl_refusal_cli_e2e (ok. 2), sync_dry_run_no_mutation_e2e (ok. 1),
  fortune5_architecture_pack_e2e (ok. 4), fortune5_testing_bblock_pack_e2e (ok. 3),
  reasoner_independence_e2e (ok. 2)

Superseded by CL-D/CL-Q (their fix landed mid-sweep at 15:07; K2 made no edits):
generation_rules_e2e.rs, law_gates_closure_test.rs, ontology_imports_e2e.rs —
all three verified green on current tree in isolation (30/5/3 passed).
cli_boundary.rs — G2's collision-repair fixture already present; zero K2 edits; ok. 29 passed.
abb_sbb_datalog_admission_e2e.rs (G1's) — ran green: 4 passed; 0 failed. Nothing to file.

## Known non-K2 residual

refusal_compile_fail.rs's doc_test_compile_fail_surface_passes failed once inside a
full sweep and passes in isolation (reran: doctests 3/3 ok). It spawns a nested
`cargo test --doc` subprocess — sweep-order resource contention, not FM-LAW-015.
Left unowned; flagging for whoever runs the next full suite.

## For G1 (src/** findings, filed not touched)

1. **`--skip-ea-admission` CLI flag is NOT wired** despite sync.rs doc comments (lines
   223, 1059) claiming "wired in ggen-cli": `grep -rn skip-ea-admission
   crates/ggen-cli/src/ crates/ggen-engine/src/verbs/` → zero hits. Every real-CLI test
   had to take the fixture route. Wire the flag or fix the doc.
2. (note, not witnessed defect) With ea.graph.json present, FM-LAW-015-class refusals
   fire before FM-WRITE-008 — gate-order coupling worth a look in G1's review.

## Verification honesty

Per-target reruns: every file K2 touched was rerun individually and green (results
cited above). Three attempts at a full no-fail-fast sweep were killed externally
(exit 144 / background time limit / SIGKILL under concurrent-lane load) — the machine
was running multiple lanes simultaneously; a clean full-suite pass was never witnessed
by K2 and is NOT claimed. Final bounded run per coordinator: green until a SIGKILL
(memory kill, environment) on consumer_mode_fixture_only_e2e — a target K2 verified
green three times earlier this session.

## Diff ownership

crates/ggen-engine/tests/** only: sync_e2e.rs, combinatorial_matrix.rs,
composed_packs_e2e.rs, pack_behaviors_cli_e2e.rs, custom_behavior_e2e.rs,
framework_packs_e2e.rs, gall_agent_executor_e2e.rs, gall_checkpoint_001_replay_e2e.rs,
ocel_determinism_e2e.rs, verify_pack_evidence_loop_e2e.rs,
verify_pack_gate_sabotage_e2e.rs, reasoner_independence_e2e.rs, envelope_caps_e2e.rs,
fortune5_architecture_pack_e2e.rs, fortune5_testing_bblock_pack_e2e.rs,
frontmatter_{cardinality,fortune5_hardening,fields,matchers,maximalism,rdf}_e2e.rs,
otel_span_presence_test.rs, shape_shacl_enforcement_e2e.rs,
shape_shacl_refusal_cli_e2e.rs, sync_dry_run_no_mutation_e2e.rs,
write_behaviors_cli_e2e.rs, write_stage_partial_failure_receipt_binding.rs,
multi_template_determinism.rs, receipt_genesis_ceiling_e2e.rs,
receipt_head_vs_log_test.rs, receipt_signing_evidence_test.rs,
sync_closure_golden_test.rs, determinism_independent_reload_e2e.rs,
determinism_query_reexecution_e2e.rs, dataset_bridge_e2e.rs, datatype_coercion_e2e.rs,
full_loop_ocel_v2_validation_e2e.rs, projection_determinism_test.rs,
constitution_obligation_e2e.rs, consumer_mode_fixture_only_e2e.rs,
envelope_otel_span_test.rs, envelope_receipt_binding_test.rs, tests/support/mod.rs,
+ this receipt. One transient scratch file ("framework_packs_e2e slow") created by a
script typo was deleted in-session.
