# CL-D Receipt: FM-LAW-015 declarative-fixture remediation (ggen-engine tests)

Standing: ALIVE

Lane: CL-D (IMPLEMENTATION), v26.11.3 wave, F2-fallout contract.
Owner of record: CL-D lane. Date: 2026-11-03.

## Identity

- Subject: `crates/ggen-engine/tests/` — 28 test files edited, all confined to the
  tests/ tree (no src/** edits, no git commands run).
- Status: **ALIVE** for the FM-LAW-015 declarative-fixture class on owned files:
  per-target runs green for all 30 owned/related targets.
- Standing: PARTIAL_ALIVE — FM-LAW-015 class driven to zero on all owned and
  adjacent files; the killed shared-target census leaves ~10 trailing binaries
  (wasm4pm/typer/tcps/standing_ladder/shacl/version_consistency/verbs tails)
  UNWITNESSED in the final shared-target census itself (all green in the
  lane-local runs and/or K2's per-target reruns; see Census section).

## μ (diff)

Two mechanical routes, exactly the two F2 recorded (K2's landed helper in
`tests/support/mod.rs` reused implicitly: the support-module scaffolds already carry
the stub; my files use their own local scaffolds):

1. **Route (a) skip flag** (`skip_ea_admission: true` on library-level `sync()`
   calls pinning non-EA laws). Mechanical batch (python insertion after every
   `SyncOptions {` literal lacking the flag + inline `SyncOptions::default()` →
   `SyncOptions { skip_ea_admission: true, ..Default::default() }`), applied to the
   24 library-level owned files:
   generation_rules_e2e, generation_rules_typed_causes_e2e, portable_receipt_e2e,
   pack_e2e, sparql_feature_coverage_e2e, receipt_verb_e2e, pack_lock_determinism_court,
   rules_n3_sync_e2e, pipeline_stage_evidence_test, law_gates_closure_test,
   graphlaw_e2e (green-path literal only — two refusal-path literals with
   `skip_ea_admission: false` deliberately KEPT `false`: their expect_err asserts
   fire before admission; the graphlaw green-path literal was pre-existing
   `false` and is the one flip), wasm4pm_facts_e2e, ontology_imports_e2e,
   manifest_diagnostic_codes_evidence_test, gall_core_pack_e2e,
   receipt_epoch_e2e, pack_dependency_scope_e2e, github_actions_pack_e2e,
   gh_enterprise_architecture_pack_e2e, gall_automation_pack_e2e,
   reasoner_independence_e2e, envelope_caps_e2e,
   verbs_handlers_test (2 doctor sync sites), template_source_pack_test (3 sites).
2. **Route (b) stub ea.graph.json** (valid-but-empty graph, zero candidate SBBs —
   K2's exact shape) written adjacent to each ggen.toml scaffold write in the 4
   CLI-harness owned files: reflexive_law_e2e (4 scaffold sites),
   ggen_release_pack_workflow_e2e, fortune5_architecture_pack_e2e,
   fortune5_testing_bblock_pack_e2e — 1 site each. This is the K2 recipe verbatim
   (fixture written after the ggen.toml expect, project dir already created by
   prior create_dir_all).

Deliberate non-edit: graphlaw_e2e's two refusal-path `skip_ea_admission: false`
literals kept false — those tests assert FM-LAW-001/FM-LAW-013 refusals that fire
before the admission gate; flipping them would weaken the courts. Only the green-path
literal was flipped to true.

## Commands / exits

Lane-local build root (shared target/ was being corrupted mid-build by concurrent
lane deletions of rmeta files — sccache fatal errors on libghash/http-body/
zerofrom_derive rmeta; two retries failed the same way):

- `CARGO_TARGET_DIR=/tmp/cl-d-target cargo test -p ggen-engine --no-fail-fast` over
  all 30 owned/related targets → **218 passed / 0 failed** (179/23 first pass with
  23 failures all ggen-binary-resolution in the lane-local target — not test
  defects; after `cargo build -p ggen-cli-lib --bin ggen` in the lane target, the 7
  affected targets reran **39 passed / 0 failed**).
- Shared-target confirmation of the 4 late-fixed files: `cargo test -p ggen-engine
  --test template_source_pack_test --test verbs_handlers_test --test
  frontmatter_fields_e2e --test gall_checkpoint_001_replay_e2e` → **38 passed /
  0 failed**.
- Shared-target confirmation batch 1 (shared target, after ggen binary rebuilt at
  target/debug/ggen): pack_e2e 15/15, reflexive_law_e2e 5/5, law_gates_closure_test
  4/4, graphlaw_e2e 6/6, generation_rules_e2e 30/30, generation_rules_typed_causes_e2e
  8/8, portable_receipt_e2e / sparql_feature_coverage_e2e / receipt_verb_e2e /
  pack_lock_determinism_court / rules_n3_sync_e2e — all `test result: ok`.

## Census

`cargo test -p ggen-engine --no-fail-fast` (shared target), run after my batch-1
edits and before the last 4 fixes; **killed at the 30-minute background cap** at
~143 ok / 10 FAILED binaries (of 138) — not the complete tail. Attribution of the
10 failing binaries at census snapshot:

- **FM-LAW-015, all now fixed**: frontmatter_fields_e2e (13) and
  gall_checkpoint_001_replay_e2e (10) — fixed mid-session by ANOTHER lane
  (fixtures present on disk, not my edits; confirmed green 14/14 and 11/11 in the
  shared-target confirmation run); template_source_pack_test (2) and
  verbs_handlers_test (2) — fixed by CL-D (this receipt). **FM-LAW-015 class on
  witnessed binaries: zero after these fixes.**
- **Not FM-LAW-015, pre-existing / other lanes' surfaces (filed, not touched)**:
  product_mirror_conformance (fm_diagnostic_codes_ttl_matches_real_source — likely
  G7/schema-lane surface), refusal_compile_fail (documented BLOCKED-class
  compile-fail probe, pre-existing per F2), replay_court_test
  (partial_alive_envelope_never_promotes), sync_dly_run (typo note:
  sync_dry_run_no_mutation_e2e — K2-fixed file, CLI subprocess route, failure
  cause unexamined by this lane), vacuity_court_selfref_test (4 CLI-exit courts —
  cause unexamined), plus the killed-tail binaries listed under Standing.
- F2's pre-existing failures (economics SLO, refusal_compile_fail) predate this wave.

## Verification ladder

narrow (per-target shared runs) → lane-local 30-target sweep (218/0 after binary
build) → shared-target confirmation of late fixes (38/0).

## Falsifier / non-vacuity

Non-vacuous by construction: the census DID witness these files refusing FM-LAW-015
before the edits (F2's census + my killed census), and after the edits the same
targets pass — the gate demonstrably fired on these fixtures and demonstrably passes
them now. The two kept-false graphlaw literals prove the skip flag is not a blanket
on: refusal-path courts still see gate behavior. The CLI fixture route admits under
the gated 26.10.12 binary (witnessed in shared-target fortune5/gall/release runs),
not only the ungated PATH binary. Shared-target confirmation batch 1 ran with
`target/debug/ggen` built by this lane (`cargo build -p ggen-cli-lib --bin ggen`,
exit 0, 1m17s).

## Standing / follow-up (coordinator)

1. Full shared-target census is INCOMPLETE (killed at 30-min cap, ~10 trailing
   binaries unwitnessed: typer_pack, tcps_core, standing_ladder, shacl_projection,
   version_consistency, wasm4pm_*, sync_e2e tail, etc. — all green per-target
   earlier). A coordinator-owned complete census with the binary-identity pin F2
   requested would close this.
2. Non-FM-LAW-015 failures needing attribution by their owning lanes:
   product_mirror_conformance, replay_court_test, sync_dry_run_no_mutation_e2e
   (K2's), vacuity_court_selfref_test (4). refusal_compile_fail ×2 pre-existing.
3. Lane lease cleanup: delete /tmp/cl-d-target (lane build root, ~size of a full
   engine test build) at integration.

## Exclusions

sync_e2e.rs (K2), abb_sbb_datalog_admission_e2e.rs (G1), tests/support/mod.rs (K2's
landed state reused, one disclosed repair already in it — no new edits by this lane),
src/**, all git commands.
