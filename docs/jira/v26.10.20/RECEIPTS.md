# v26.10.20 Integration Receipt (wave 2 + dialect wave)

**RELEASED (2026-10-20/21):**
- Pushed: graphlaw main 2798b61; ggen wave landed via PR #819 (MERGED),
  main = b6c170b98 (direct main pushes are ruleset-blocked — PR path only).
- Published to crates.io: graphlaw 26.10.13, ggen-abb-sbb 26.10.12,
  ggen-config 26.10.12, ggen-marketplace 26.10.12, ggen-graph 26.10.12,
  ggen-engine 26.10.12, ggen 26.10.12. Ladder complete, in dependency order.
- Late packaging finds (fixed + committed in-wave): caret req resolved
  registry ggen-graph 26.15.2 (stale train) → exact-pin =26.10.12;
  templates/ missing from engine tarball (court include_str!) → include
  added; generated/ modules untracked in VCS → force-added (fresh-clone
  E0583 closed at the git layer, not just packaging).


**FINAL STATE (all lanes landed):** `just pre-commit` exit 0 — 20 gates GREEN,
1723 tests passed, 194s hot (H07 run 7). TECH-DEBT-001 (cheat-scan ~464
findings) and TECH-DEBT-002 (workspace clippy errors) both CLOSED during
closure. graphlaw: full suite 540/540 + clippy -D warnings 0 (G02/H04).
Residual watch-item: environments holding pre-26.10.13 consumer receipts fail
guard-pack-proofs until reset (graphlaw chain-rule rename; H07 sanctions the
regen path). Untracked-and-must-commit-together: graphlaw
contracts/abi-manifest.json + src/abi_manifest.rs (CI court reads both).

Coordinator-verified gates. Lanes: A01-A20 (pattern impl), I1-I3 (seam
completion), V01-V20 (dialect slices 1-3 + wasm prep). Per-lane receipts in
their completion reports; this file records the integration-level gates.

## Pattern courts (ggen)

| Court | Gate | Result |
|---|---|---|
| envelope | ggen-abb-sbb/envelope_courts 4, ggen-engine envelope_* e2e 5, receipt_binding 2 | GREEN (I1/I2; A03 seam pinned) |
| canonical | ggen-config canonical courts 9, ggen-engine codegen 6, schema parity 5 | GREEN (A10/A09/I3) |
| fresh-epoch | write tests 8, epoch court 4, cli_boundary 29, shim-court BLOCKED-exit-2 + restore sha-verified | GREEN (A06/A17/I1) |
| vacuity | self-ref 9 (V14 reconcile), KAT/fail-closed/witness 10 (A12) | GREEN |
| typed-refusal | compile-fail 3, census 11 codes 0-drift, shell-safety 4, fail-open 4, dialect court 6 | GREEN (A15/A16/V02/V13) |
| dialect dispatch | pack_dialects 7, dialect_dispatch 16, receipt_dialect 10, law_engine parse errors 4 | GREEN (V01/V03/V15/V16) |
| dataset seam | ggen-graph 43 suites (incl. dataset_intent 6) | GREEN (V17) |

## Dialect slice 1 verdict (recorded)

- purrdf 2.0.2 parses ALL probed RDF formats natively (V04 probe 7/7);
  oxrdfio REJECTED (no new parser deps); remote JSON-LD contexts fail-closed
  offline (typed refusal, zero network).
- Cross-dialect RDFC-1.0 identity: 6/6 court (V05), blank-node stable across
  dialects; N3 typed-Unsupported; non-vacuity caught a real no-op mutant.
- W3C RDF 1.1 corpus: 325 entries, 9 engine divergences registered loudly
  (V06); generator made self-contained (coordinator fix).
- graphlaw wasm: 3 resource-bound gaps closed (V09, 8/8 boundary courts);
  ABI manifest + 10-test court (V11); differential harness 35 cases
  ZERO divergences native-vs-wasm (V12); size court 6.34MiB / 25MiB (V10);
  version skew 26.10.5/26.10.13 closed + pin verified b3sum byte-exact (V19);
  CI wasm-court job wired skip-loud (V20); docs runbooks (V18).

## Publish ladder (H09/H10, real exits)

- ggen-abb-sbb `cargo publish --dry-run` → **exit 0** (11 files, 109.5KiB).
  Real blocker found: repo `.gitignore` `generated/` pattern excluded
  `src/generated/{mod,refusals}.rs` from the tarball (E0583); fixed via explicit
  `[package] include`. Same pattern threatens `crates/ggen-engine/src/generated/`
  — H10 to verify.
- Ladder (docs/jira/v26.10.20/PUBLISH_LADDER.md): abb-sbb → ggen-config →
  ggen-marketplace → ggen-graph → ggen-engine → root ggen. No mandatory
  version-req bumps (caret reqs resolve 26.10.12).
- Cleanup: H09's isolated build root moved to /tmp/target-h09-pubcheck-cleanup
  (rm denied in session; operator may delete).

## Dry-run publish (coordinator, real exits)

- graphlaw `cargo publish --dry-run --allow-dirty -p graphlaw` → **clean**:
  packaging + verification passed, "aborting upload due to dry run". Version
  26.10.13.
- ggen-engine `cargo publish --dry-run --allow-dirty -p ggen-engine` →
  **blocked on publish order**: requires `ggen-abb-sbb ^26.10.11`, crates.io
  has 26.10.10. Workspace version is 26.10.12 (abb-sbb inherits). Ladder:
  publish ggen-abb-sbb 26.10.12 first, then ggen-engine, then dependents.
  Not a wave defect — the standard publish ladder for this repo.

## Known follow-ups (not blocking release)

0. FLAKE CANDIDATE → FENCED (G05): root-cause corrected — the court was already
   fully structural (write.rs:393 pure u64 generation compare; no clock in path);
   the single ladder failure was compile churn under concurrent lane edits, not a
   time race. Fence: structural-no-clock property documented on a shared helper +
   20× stress modifier test; 20/20 sequential + parallel-load green.

1. Step::DeriveN3 still takes raw rule strings (V08: RuleSet refactor ripples
   through public Step surface — needs its own slice).
2. wasm artifact re-pin pending rebuild (V19 documented; pins minted at
   26.10.5 source state, valid until rebuild).
3. dialects-schema registration in ggen-pack-spec-pack TTL (V02 noted).
4. receipt verify head-vs-log comparison (A19 note: ggen receipt verify
   checks internally only).

## Disclosures index (cross-lane unblocks)

Recorded in lane reports; notable: V10/V05 abi.rs duplicate-fragment removals
(V09's file, mid-edit), V17 sync.rs import dedupe, V06 dialect.rs/tier.rs
format-string fixes, V14 sha2 import hoist + STATUS-guard assertion.
