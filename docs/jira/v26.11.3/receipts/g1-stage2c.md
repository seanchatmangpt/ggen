# Receipt: g1 — WO-01 Stage 2c Admission Wiring

Standing: PARTIAL

- **Date**: 2026-11-03
- **Lane**: IMPLEMENTATION (g1) — `crates/ggen-engine/src/sync.rs` Stage 2c region + `crates/ggen-engine/src/envelope.rs`
- **Work order**: `docs/jira/v26.11.3/WO-01-stage-2c-wiring.md`
- **Standing**: PARTIAL — contract surface (a)-(d) landed and court-witnessed; 9 CLI-driven tests red on a cross-lane dependency (CLI flag pending, see Blocker).

## Diff (what landed)

1. `crates/ggen-engine/src/envelope.rs` — new `abb_envelope_caps(root, envelope) -> Option<ggen_abb_sbb::EnvelopeCaps>`:
   SPARQL over `.specify/envelope-shapes.ttl`, maps the declared envelope's `res:maxArtifacts` literal into the kernel `EnvelopeCaps{max_artifacts}`,
   clamped to `ggen_abb_sbb::DEFAULT_MAX_ARTIFACTS` (64). Absent reticle / undeclared envelope / no literal → `None` = kernel default — no value invented.
   (Distinct from `extract_caps`, which folds "no grant" into `CapTable::default()`; `abb_envelope_caps` preserves the None-vs-default distinction.)
2. `crates/ggen-engine/src/sync.rs` Stage 2c region:
   - Missing `ea.graph.json` now REFUSES with `FM-LAW-015` naming the absent file and the `--skip-ea-admission` opt-out (defect (e), fail-closed default).
   - `admit(&ea_graph, &req)` → `admit_with_caps(&ea_graph, &req, abb_caps.as_ref())`, caps = `abb_envelope_caps(root, stage2c_envelope_iri)` where the IRI is derived
     from the run's templates' `frontmatter.envelope` (deterministically smallest), same derivation as Stage 2c-caps (defect (a)).
   - `expected_graph_digest: Some(ea_graph.digest())` — request pinned to the exact parsed subject (defect (c)).
   - `receipt_generation: prior_generation.unwrap_or(0)` — Stage 2c-epoch read (sync.rs ~983). Note: this was already wired at the contract's baseline; verified, kept.
   - `artifact_baseline_generation: 0` retained with a comment citing WO-01 follow-up — no per-artifact baseline source exists.
   - `SyncOptions.skip_ea_admission: bool` added (default `false`, disclosed: refuse is the default).
3. `SyncOptions` literals in existing green-path tests that sync fixtures without an EA graph patched to `skip_ea_admission: true` (minimal unblock, each marked
   with a `WO-01 fail-closed EA gate` comment in-file): `graphlaw_e2e.rs`, `reasoner_independence_e2e.rs`, `annotated_pack_sync_smoke.rs` (4),
   `autofde_execution_profile_pack_e2e.rs` (2 + `tests/support/mod.rs` second-sync), `book_gap_closure_e2e.rs` (empty-graph fixture, CLI-driven),
   `build_provenance_test.rs`, `capability_topology_exp.rs` (green path only).
4. `abb_sbb_datalog_admission_e2e.rs::unbound_pack_port_refuses_sync` — fixture port id `port:missing-event-stream` → `missing-event-stream`.
   Root cause was NOT the EA gate: ggen-abb-sbb (v26.10.12) now enforces canonical port ids (a-z/0-9/dash), so `extract_pack_manifest` refuses the old id and the
   depgraph check was silently skipped (`if let Ok` fail-open in Stage 2c block 1 — pre-existing fail-open, unchanged here, flagged for WO follow-up).

## Commands + exits (real, this session)

- `cargo build -p ggen-engine` → exit 0 (`Finished dev profile`).
- `cargo test -p ggen-engine --test abb_sbb_stage2c_wiring_e2e` → exit 0, **7 passed; 0 failed**.
- `cargo test -p ggen-engine` → **PARTIAL**: 232 lib + all integration binaries ok except `cli_boundary.rs` (9 tests) — every red test's stderr names
  `[FM-LAW-015] EA admission gate ... --skip-ea-admission`, root cause = the CLI verb has no flag to pass (see Blocker).
- Mutation courts (each: mutate → named test flips → restore → passes):
  - Court 1 (defects a+c: revert to 2-arg `admit`, digest `None`): `envelope_cap_tighter_than_contract_refuses_admission` FAILED under mutation; passes restored.
  - Court 2 (defect e: gate condition `if false && ...`): `missing_ea_graph_refuses_by_default` FAILED under mutation; passes restored.
  - Court 3 (defect c digest): `pinned_digest_refuses_mutated_graph` witnesses GRAPH_DIGEST_MISMATCH through the real kernel seam
    (real graph parsed from disk, digest taken, subject mutated, re-admitted via `admit_with_caps` → refuses; unmutated control admits, digest equality asserted).
    Pipeline-level pinning is structural (same parsed graph at the call site); `grep -n "expected_graph_digest: Some(ea_graph.digest())" src/sync.rs` is the source gate.

## Tests landed (crates/ggen-engine/tests/abb_sbb_stage2c_wiring_e2e.rs — owned)

1. `envelope_cap_tighter_than_contract_refuses_admission` — reticle `res:maxArtifacts 2` vs contract 8 → EXCEEDS_CONTRACT; nothing written.
2. `envelope_cap_within_contract_admits` — ceiling 16 (clamped to 64) vs contract 8 → admits, writes output.
3. `translation_maps_max_artifacts_literal` / `translation_absent_reticle_yields_none` — translation fn contract.
4. `missing_ea_graph_refuses_by_default` / `missing_ea_graph_admits_with_opt_out_flag` — fail-closed gate + explicit opt-out.
5. `pinned_digest_refuses_mutated_graph` — GRAPH_DIGEST_MISMATCH through real `admit_with_caps` + positive control.

## Blocker (cross-lane, coordinator action)

The 9 `cli_boundary.rs` failures share ONE root cause: the `ggen sync run` verb (generated from `crates/ggen-cli/src/conventions/templates/rdf/ggen_cli_nouns.rdf`)
exposes no `--skip-ea-admission` flag, so CLI-driven green-path tests refuse at the new gate. Both files are outside my owned surface (ggen-cli forbidden by lane
contract; the verb is RDF-generated, hand-editing generated output banned by repo rules). Remedy: regenerate the verb surface with a `--skip-ea-admission` bool
mapping to `SyncOptions.skip_ea_admission` (field already landed, default false). Until then `cargo test -p ggen-engine` cannot go fully green.
