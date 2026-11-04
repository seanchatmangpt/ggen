# ZD-11: sync.rs:431 artifact_baseline_generation hardcode

Status: **RESOLVED-DISCLOSED** (pinned-by-law; no code change)
Date: 2026-11-04
Lane: zd-11 (sync.rs:431 area only)

## Subject

`crates/ggen-engine/src/sync.rs:428` sets
`artifact_baseline_generation: 0` in the `ggen_abb_sbb::Request` built for
each EA SBB admission (FM-LAW-015 path, declarative EA-graph branch).

## Root cause (from history + code)

- The field and the kernel's stale-lease gate landed in commit `a2867ea97`
  (v26.11.3-wave lineage: "five lithographic pattern courts + dialect
  slices 1-3 + receipt identity", sole `-S artifact_baseline_generation`
  hit in both crates).
- The kernel gate (`crates/ggen-abb-sbb/src/lib.rs:827-836`) refuses
  `StaleLease` when `receipt_generation <= artifact_baseline_generation
  && artifact_baseline_generation > 0`.
- ggen-engine's only generation source is the **global per-run**
  `prior_generation` (`read_receipt_log_generation`, sync.rs:2435 — tail
  of `.ggen-v2/receipt-log.jsonl`). The receipt log tracks **no
  per-artifact baseline**; the in-code comment at sync.rs:423-427 says
  exactly this and defers to a WO-01 follow-up ("wire a real baseline when
  a baseline source lands").

## Ruling: hardcode is load-bearing

There is no generic mechanism to swap in. Wiring the only available
generic source (the global per-run generation) would set
`receipt_generation == artifact_baseline_generation` on every subsequent
run → `Refusal::StaleLease` on **every** EA SBB admission — a functional
regression. `0` is the kernel's own documented "untracked" sentinel that
keeps the stale-lease gate dormant for generation-less callers
(lib.rs:829-831 comment). Ledger item closed as RESOLVED-DISCLOSED:
pinned-by-law until a per-artifact baseline source exists (WO-01
follow-up).

No test pins the sync.rs value: the only test touching
`artifact_baseline_generation: 0`
(`crates/ggen-engine/tests/abb_sbb_stage2c_wiring_e2e.rs:205`) constructs
a kernel `Request` directly to prove GRAPH_DIGEST_MISMATCH, which is
correct kernel semantics, not a pin of the engine-side hardcode. Code
comment already documents the defer; left unchanged.

## Verification (commands + exits, this session)

- `cargo test -p ggen-engine --lib` → 236 passed, 0 failed (pass 1).
- `cargo build -p ggen-cli-lib --bin ggen` → exit 0 (1m35s).
- `./target/debug/ggen sync run` → exit 0 (4.14s; wrote
  `crates/ggen-abb-sbb/src/generated/refusals.rs`).
- `./target/debug/ggen receipt verify` → exit 0, `"valid": true`,
  `"signed": true`.
- `cargo test -p ggen-engine` full pass 1 → 8 suites ok; 2 failures in
  `book_gap_closure_e2e` (both panicked at line 59: "could not resolve
  the ggen binary") — environmental, fixed by the build above.
- `cargo test -p ggen-engine` full pass 2 (after binary built):
  all suites green EXCEPT one: `economics_receipt_chain_wall_clock_
  measured_under_slo_threshold` FAILED — its inner `receipt_chain_e2e`
  subprocess fails on 2 tests pinning the **committed** TCPS receipt
  chain (`receipt_chain_e2e.rs:945` hash mismatch,
  `:1002` 68 vs 67): shared-checkout working-tree drift, not
  attributable to this lane (zero code edits; tree carries other lanes'
  uncommitted regenerated outputs incl. `self-host/ontology.ttl`,
  `.ggen-v2/receipt*`). `receipt verify` passes against the current
  self-consistent tree. Pre-existing class on a shared non-clean tree;
  NOT a regression from this task.

## Standing

ALIVE for the ruling (code read at HEAD, kernel gate read, history
traced, tests + dogfood run this session). Receipt-file-only landing;
coordinator owns ledger update + git transitions.
