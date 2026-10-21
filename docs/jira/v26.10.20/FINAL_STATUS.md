# FINAL STATUS — ggen v26.10.20 (G20 finisher lane)

Assembled 2026-10-20 by G20 from on-disk evidence only (cheap checks: ls, grep,
stat — no builds, no test executions). **Refreshed by D08 (docs lane) the same
day** with coordinator-verified outcomes G20 marked UNVERIFIED: every status
change below cites its receipt (RECEIPTS.md row / on-disk artifact). UNVERIFIED
remains only where nothing ran. Repos: `/Users/sac/ggen` (ggen),
`/Users/sac/graphlaw` (graphlaw).

## 1. PRD capability matrix

| PRD objective | Status | Evidence (file; what G20 checked) |
|---|---|---|
| Dialect layering (dialect dispatch across packs/laws/receipts) | ALIVE (receipted) | Court sources on disk (`crates/ggen-engine/tests/dialect_dispatch_test.rs`, `pack_dialects_test.rs`, `receipt_dialect_test.rs`, `law_engine_parse_errors_test.rs`); **ran GREEN** per `RECEIPTS.md` "dialect dispatch" row (V01/V03/V15/V16: 7+16+10+4 tests) — receipt: RECEIPTS.md pattern-courts table |
| RDFC-1.0 universal hashing (cross-dialect identity, blank-node stable) | ALIVE (receipted) | `/Users/sac/graphlaw/tests/canonical_cross_dialect.rs` (L219: "blank-node relabeling must not move identity"); **6/6 court ran** per `RECEIPTS.md` slice-1 (V05), non-vacuity caught a real no-op mutant — receipt: RECEIPTS.md slice-1 |
| Pack dialect gates | ALIVE (receipted) | `crates/ggen-engine/tests/pack_dialects_test.rs`; **"dialect court 6" GREEN** per `RECEIPTS.md` typed-refusal row (V02/V13) — receipt: RECEIPTS.md pattern-courts table |
| Quad-store slice 2 (dataset seam) | ALIVE (receipted) | `crates/ggen-graph/tests/dataset_intent_court.rs` + `receipt_v2_domain_separation_test.rs`; **43 suites GREEN** per `RECEIPTS.md` "dataset seam" row (V17) — receipt: RECEIPTS.md pattern-courts table |
| JSON-LD reduced (no new parser deps; remote contexts fail-closed) | ALIVE (receipted) | Probe source: `/Users/sac/graphlaw/tests/parser_capability_probe.rs` L107-146 (probe 7 = remote `@context` at `https://example.invalid/...` must NOT fetch); **probe ran 7/7** per `RECEIPTS.md` slice-1 (V04): purrdf 2.0.2 parses all probed formats, oxrdfio REJECTED, remote contexts fail-closed with typed refusal, zero network |
| Resource hardening (wasm) | ALIVE (receipted) | `/Users/sac/graphlaw/tests/abi_bounds_court.rs`, `resource_limits.rs`, `parse_refusal_court.rs`, `shacl_depth_guard_court.rs`, `tier_court.rs`; **8/8 boundary courts ran, 3 gaps closed** per `RECEIPTS.md` slice-1 (V09); plus differential harness 35 cases ZERO divergences (V12) and ABI manifest + 10-test court (V11) |
| Size budget (wasm ≤ 25 MiB) | ALIVE — receipted re-pin (G01) | Artifact re-pinned at 26.10.13 source state: `/Users/sac/graphlaw/registry/ARTIFACTS.blake3` line 2 `digest=cbc80f07346f025d… size=6654496 pin-source=26.10.13`, byte-verified against `/Users/sac/graphlaw/priv/graphlaw.wasm.blake3`; 6,654,496 B = 6.34 MiB, under the 25 MiB (`26_214_400`) ceiling in `/Users/sac/graphlaw/tests/wasm_size_court.rs` L9; fresh-build differential zero-divergence per coordinator receipt (G01) — closes former follow-up 2 |
| praxis-graphlaw subsumption | ALIVE — dependency edge verified on disk | ggen-engine consumes sibling `graphlaw` crate (see `.claude/rules/architecture.md`; `crates/ggen-engine/src/law_engine.rs` header: "backed by `graphlaw`"); graphlaw retired praxis-core/praxis-graphlaw 2026-10-10 (SJIRA-15) |

## 2. Court inventory (wave test files, mtime 2026-10-20)

### ggen — 32 files

- `crates/ggen-abb-sbb/tests/`: `bench_bound.rs`, `envelope_courts.rs`, `falsifiers.rs`, `marketplace_fixture.rs`, `property_admission_test.rs` (5)
- `crates/ggen-config/tests/`: `canonical_identity_courts_test.rs`, `generated_mutation_court.rs` (+`crates/ggen-config/tests/generated/` dir) (2)
- `crates/ggen-engine/tests/`: `canonical_identity_codegen_test.rs`, `capability_topology_exp.rs`, `cli_boundary.rs`, `combinatorial_matrix.rs`, `court_compiler_kat_test.rs`, `court_runner_fail_closed.rs`, `court_witness_ordering.rs`, `dialect_dispatch_test.rs`, `envelope_caps_e2e.rs`, `envelope_receipt_binding_test.rs`, `envelope_undeclared_cap_e2e.rs`, `fresh_epoch_lease_court_test.rs`, `fresh_lease_write_test.rs`, `frontmatter_canonical_schema_test.rs`, `generated_shell_safety_test.rs`, `law_engine_parse_errors_test.rs`, `pack_dialects_test.rs`, `receipt_chain_differential_test.rs`, `receipt_chain_kat_test.rs`, `receipt_dialect_test.rs`, `receipt_epoch_e2e.rs`, `refusal_compile_fail.rs`, `vacuity_court_selfref_test.rs` (23)
- `crates/ggen-graph/tests/`: `dataset_intent_court.rs`, `receipt_v2_domain_separation_test.rs` (2)

### graphlaw — 13 wave-touched test files (of 61 total in tests/)

`abi_bounds_court.rs`, `abi_manifest_court.rs`, `canonical_abi.rs`,
`canonical_cross_dialect.rs`, `capabilities_digest.rs`, `corpus_conformance.rs`,
`insa_law_order.rs`, `parse_refusal_court.rs`, `parser_capability_probe.rs`,
`pn64_abi_conformance.rs`, `tier_court.rs`, `wasm_native_differential.rs`,
`wasm_size_court.rs` (+ `w3c_conformance.rs`, `w3c_manifest.rs`, `temprun_framing.rs`,
`refusal_crosswalk_differential.rs`, `strata_gate_differential.rs`, `receipt_chain_golden_test.rs`
also carry 2026-10-20 mtimes — counted conservatively as wave files: **19**)

**Total: 32 (ggen) + 19 (graphlaw) = 51 wave test files** (method: `stat` mtime = 2026-10-20; file list captured by G20 from disk).

## 3. Out-of-scope honored

### N3-not-codegen (no generation path keyed on N3)

Grep receipt (G20, `grep -rniE '\bn3\b' crates/ggen-engine/src`): N3 references
confined to the law-evaluation seam (`law_engine.rs` — Eyeron N3 parse,
`Dialect::N3` display, `E-PARSE-N3` typed refusals) and doc comments. In
graphlaw, `Step::DeriveN3` is a law-runtime step (`/Users/sac/graphlaw/src/law.rs` L57/103/120), a
court-execution surface, not a ggen template/codegen path. No template
rendering or code emission keyed on N3 was found in either repo's src. Caveat:
this is a grep-level receipt, not a compiler-level proof of absence —
UNVERIFIED beyond grep.

### Remote contexts disabled (V04)

`/Users/sac/graphlaw/tests/parser_capability_probe.rs` probe 7 (L125-146):
JSON-LD document with remote `@context` pointing at
`https://example.invalid/never-served-context.jsonld` must not trigger network
access — parse-or-refuse offline, typed refusal, never silent term-drop.
`RECEIPTS.md` slice-1: probe ran 7/7 with fail-closed remote contexts. G20
verified the probe source on disk; did not re-run it.

## 4. Known follow-ups (updated from RECEIPTS.md)

0. RESOLVED — FENCED (G05): `fresh_epoch_lease_court_test::backdated_receipt_refused_stale_lease` flake closed with a structural fence. The staleness signal in `backdated_refuse_once` (`crates/ggen-engine/tests/fresh_epoch_lease_court_test.rs` L194-215, mtime 2026-10-20 14:44) is structural — `check_lease_generation` in `crates/ggen-engine/src/write.rs` is a pure `u64` comparison; no mtime/wall-clock/SystemTime participates in the gate. A shared 20× in-process stress modifier fences load-order dependence. Fence note documents the analysis in-test so it cannot drift from the court.
1. OPEN: `Step::DeriveN3` still takes raw rule strings (V08 RuleSet refactor needs its own slice).
2. CLOSED (G01): wasm artifact re-pin landed — new pins minted at 26.10.13 source state, digest `cbc80f07…`, 6,654,496 B, fresh-artifact differential zero-divergence (see size-budget row above).
3. OPEN: dialects-schema registration in ggen-pack-spec-pack TTL (V02).
4. OPEN: `ggen receipt verify` head-vs-log comparison (A19: checks internally only).

## 5. Dry-run publish status (coordinator, real exits — RECEIPTS.md "Dry-run publish")

- `.ggen-v2/receipt.json`: EXISTS, mtime 2026-10-20 14:44:04, chain-hash record present (`payload_hash_hex 0bd41f…`, `chain_hash_hex bd4fc9…`, `andon: Green`) — read directly on disk.
- graphlaw `cargo publish --dry-run --allow-dirty -p graphlaw` → **clean**: packaging + verification passed, "aborting upload due to dry run". Version 26.10.13. (G20's earlier "UNVERIFIED / NOT FOUND in /tmp" note is superseded — the dry-runs ran for real; receipt: RECEIPTS.md.)
- ggen-engine `cargo publish --dry-run --allow-dirty -p ggen-engine` → **blocked on publish order**: requires `ggen-abb-sbb ^26.10.11`, crates.io has 26.10.10. Workspace version is 26.10.12 (abb-sbb inherits). Ladder: publish ggen-abb-sbb 26.10.12 first, then ggen-engine, then dependents. Not a wave defect — the standard publish ladder for this repo.

## 6. Coordinator verify-ladder results (receipted 2026-10-20)

- `just courts` (ggen): all court suites GREEN except the one flake in follow-up 0 — now fenced (G05, structural staleness + 20× stress modifier).
- graphlaw `cargo test`: exit 0.
- clippy sweeps: 0 errors (HEAD commit 2bedde840 "fix(engine): clippy -D warnings clean in receipt_chain/replay_court tests" is the corresponding fix-forward receipt).
