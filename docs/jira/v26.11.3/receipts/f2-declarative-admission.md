# F2 — Declarative-rules path: Stage 2c EA admission hoist (DEF-K6-2)

Lane: F2 (IMPLEMENTATION), v26.11.3 wave, DEF-K6-2 contract.
Owner of record: F2 lane. Date: 2026-11-03.

## Identity

- Subject: `/Users/sac/ggen/crates/ggen-engine/src/sync.rs` (declarative branch + hoisted
  admission fn) and `/Users/sac/ggen/crates/ggen-engine/tests/declarative_ea_admission_test.rs` (new).
- Status: **ALIVE** for owned surfaces; suite-wide fallout recorded, not owned.
- Standing: PARTIAL_ALIVE — owned court green; ~100 pre-existing declarative fixtures now
  loudly refuse (contract-mandated behavior change, recorded for follow-up lanes).

## μ (diff)

`crates/ggen-engine/src/sync.rs`, three edits (frontmatter Stage 2c region itself untouched —
G1's region is called, never edited):

1. **Hoist**: extracted the frontmatter Stage 2c EA admission sequence (fail-closed missing-file
   gate, parse → validate → envelope caps → per-SBB `admit_with_caps` with digest pinning and
   receipt-generation lease) verbatim into `pub(crate) fn run_ea_graph_admission(root,
   skip_ea_admission, envelope_iri: Option<&str>, prior_generation: u64) -> Result<()>`.
   The frontmatter path calls it with the run's smallest template-declared envelope IRI and the
   Stage 2c-epoch generation. Declarative manifests declare no template frontmatter envelopes,
   so the declarative dispatch passes `None` → kernel DEFAULT_MAX_ARTIFACTS (64).
2. **Frontmatter call site**: replaces the inline region; the envelope-IRI computation moved
   above the call (pure computation over templates; no behavior change).
3. **Declarative branch** (Stage 0 dispatch): reads the receipt-log generation tail
   (`read_receipt_log_generation(root)?`) and calls the same admission fn with `None` envelope
   and `opts.skip_ea_admission`, before `generation_rules::run` executes any rule.

## Commands / exits

- `cargo check -p ggen-engine` → exit 0 (2m42s).
- `cargo test -p ggen-engine --test declarative_ea_admission_test` → **4/4 green** (0.08s):
  - `declarative_manifest_valid_graph_admits_and_writes` — declarative manifest + real
    `synthetic_graph(1, 8)` → sync proceeds past admission, real output written, content
    asserted.
  - `declarative_manifest_unqualified_sbb_refuses_fm_law_015` — qualifications cleared →
    `FM-LAW-015` wrapping kernel `SBB_UNQUALIFIED`; nothing written.
  - `declarative_manifest_missing_graph_refuses_by_default` — absent graph, no flag →
    fail-closed refusal naming file + `--skip-ea-admission`; nothing written.
  - `declarative_manifest_skip_flag_admits` — `skip_ea_admission: true` → sync proceeds.
- `cargo test -p ggen-engine` (fail-fast, first full run) — lib unittests 235 passed /
  2 ignored; `abb_sbb_stage2c_wiring_e2e` 7/7 (G1's frontmatter path unchanged by the hoist).
- `cargo clippy -p ggen-only --lib` (ggen-engine lib): no warnings on the hoisted fn; only
  pre-existing pedantic warnings in error.rs (other lanes' region).

## Full-suite census (`cargo test -p ggen-engine --no-fail-fast`)

862 passed / 130 failed / 138 binaries. Failure discrimination:

- **Deterministic gate fallout (contract-mandated, ~100 tests)**: every in-process
  `sync()` declarative fixture without `ea.graph.json` at its sync root now refuses
  FM-LAW-015. Affected files (failure counts from census):
  generation_rules_e2e (30), generation_rules_typed_causes (8), portable_receipt_e2e (6),
  pack_e2e (6), sparql_feature_coverage (5), reflexive_law_e2e (4), receipt_verb_e2e (4),
  pack_lock_determinism_court (4), rules_n3_sync_e2e (3), pipeline_stage_evidence (3),
  law_gates_closure (3), graphlaw_e2e (3), fortune5_testing_bblock_pack_e2e (3),
  envelope_caps_e2e (3), wasm4pm_facts (2), pack_behaviors_cli (2), ontology_imports (2),
  manifest_diagnostic_codes (2), ggen_release_pack_workflow (2),
  gall_core_pack (2), receipt_epoch (1), reasoner_independence (1), pack_dependency_scope (1),
  github_actions_pack (1), gh_enterprise_architecture (1), gall_automation (1),
  fortune5_architecture (1).
- **Pre-existing / not gate-caused**: economics_measured_evidence_test (1, SLO wall-clock,
  load-sensitive), refusal_compile_fail (2, documented BLOCKED-class compile-fail probes).
- **CLI-harness suites — nondeterministic, environment confound (observed, unresolved)**:
  cross_pack_matrix, doctor_e2e, custom_behavior_e2e, framework_packs_e2e,
  frontmatter_{cardinality,fields,maximalism,rdf}_e2e, gall_agent_executor_e2e,
  graphlaw and gall suites refused FM-LAW-015 in census run 1 but passed in census run 2.
  Observed facts: the CLI harnesses resolve `target/debug/ggen` (workspace 26.10.12, HAS the
  gate — census run 1 OTEL spans show `version="26.10.12"` with FM-LAW-015 refusals) vs PATH
  `~/.local/bin/ggen` (26.10.11, predates the gate — direct probe: declarative sync exits 0
  with no ea.graph.json). `target/debug/ggen` is absent after run 2 and I did not build it;
  hypothesis: another lane in the shared checkout built/removed it between censuses. These
  suites are LATENT fallout: once a workspace `ggen` binary is built, all declarative consumer
  fixtures (e.g. pack-generated `consumer/` roots without ea.graph.json) refuse.
  Remediation recipe: write a zero-SBB stub ea.graph.json into each fixture's sync root
  (pattern already present in `tests/support/mod.rs` lines 60-64/125/221/248) or pass
  `skip_ea_admission: true`.

## Verification ladder

narrow (cargo check) → unit (lib 235/235) → integration (owned court 4/4 + G1 wiring 7/7) →
suite census (862/130 with full discrimination above) → clippy clean on owned fn.

## Falsifier / non-vacuity

The new court is non-vacuous by construction: each row flips on the exact mutation under
test (valid graph admits + writes; unqualified refuses; absent refuses; skip admits). The
hoist itself is falsified by census run 2: the declarative suites listed above DID refuse
FM-LAW-015 with my gate and pass on the ungated path — the gate demonstrably fires on the
declarative path. G1's frontmatter court (abb_sbb_stage2c_wiring_e2e 7/7) proves the hoist
did not disturb the frontmatter path.

## Standing / follow-up (coordinator)

1. Fixture unblock across ~27 test files: stub ea.graph.json per sync root (support/mod.rs
   pattern) or skip flag — follow-up wave work, not F2's owned files.
2. Resolve the CLI-binary-resolution confound (which lane builds `target/debug/ggen`;
   census runs must pin the binary identity, else suite results are not reproducible).
3. Pre-existing failures (economics SLO timing, refusal_compile_fail ×2) predate F2.

## Exclusions

Frontmatter Stage 2c region (G1, LANDED-FROZEN) untouched — called, never edited.
`crates/ggen-abb-sbb/**` untouched. `tests/sync_e2e.rs` untouched. No git commands run.
