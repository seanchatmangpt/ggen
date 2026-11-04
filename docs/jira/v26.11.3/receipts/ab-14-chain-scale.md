# Receipt: ab-14-chain-scale

Standing: ALIVE (both scale courts ran green this session)

Lane AB-14 (STRESS) — chain-scale stress over the two EXISTING chain
planes, both read-only subjects. No production code changed on either
plane; the diff is two new test files + this receipt.

## Subject

- ggen: `.ggen-v2/receipt-log.jsonl` ledger + its REAL full-chain
  verifier `handle_receipt_history_in`
  (`crates/ggen-engine/src/verbs/handlers.rs:712`) — genesis all-zeros
  anchor, raw-bytes payload-hash binding, rule-aware recompute
  (`ReceiptRecord::recompute_chain_hash`, the emission-path fold), strict
  adjacent-link check, head-pointer consistency.
- cgap: `RingReceiptChain` (`crates/cgap-ffi/src/receipt_chain.rs`) —
  per-transition BLAKE3 Merkle receipts, genesis `GENESIS` all-zeros,
  one opaque `Spoliation` refusal class, license verify gate + sealer
  cross-check at append, strict position invariant in verify.

## Files (owned by this lane)

- `/Users/sac/ggen/crates/ggen-engine/tests/receipt_chain_scale_stress.rs` (new, 2 tests)
- `/Users/sac/cgap/crates/cgap-ffi/tests/receipt_chain_scale.rs` (new, 3 tests)
- this receipt

## Commands + exits (this session)

- `cargo test -p cgap-ffi --test receipt_chain_scale -- --nocapture` →
  exit 0, `3 passed; 0 failed` (51.67s)
- `cargo test -p ggen-engine --test receipt_chain_scale_stress -- --nocapture` →
  exit 0, `2 passed; 0 failed` (42.83s)

## Measured cost curves (min-of-3, debug profile, Apple silicon)

ggen ledger (100k records):
- mint+append: 9352.0 ms (flat per-1k buckets: 0.0826 → 0.0872 ms, x1.06)
- full `handle_receipt_history_in`: 4967.1 ms
- census per-1k bucket: 199 → 132 allocs, 16581 → 8531 B (BufWriter
  amortization; flat-to-down, no growth channel; rerun census:
  90 allocs / 6809 B per bucket, flat x0.86-0.92 — stable across runs)

cgap `RingReceiptChain` (100k links):
- append: 4174.5 ms min-of-3; per-1k buckets flat x1.00
  (0.0378 ms/1k, 2 allocs / 266 B per bucket window)
- verify_all: 4540.8 ms (O(n): full verify >= 5x a 10k prefix span on
  every run)
- span curve (test-local span helper, 20 checkpoints, min-of-3):
  1k span 24.33 ms | 10k span 253.21 ms | 50k span 1276.01 ms —
  52x cost for 50x span: cost tracks the SPAN, not the chain
  (O(span) measured, not asserted); 1k-span cost position-independent
  (head vs tail, <5x ceiling)
- tamper-at-50k: flipping receipt[50_000].hash[0] flips verify_all to
  `Spoliation`, the minimal 2-link covering span (49_999..50_001)
  refuses, and every span BEFORE the tamper (0..50_000, 40_000..50_000)
  verifies clean — prefix-integrity law witnessed at scale.

ggen tamper-at-50k: flipping one hex nibble of record 50_000's
payload_hash_hex ON THE WIRE bytes → `handle_receipt_history_in` refuses
with `history invalid at index 50000` (payload-hash binding); the
pristine 50k prefix, re-verified as its own ledger, verifies with its
genesis anchor intact — prefix-integrity witnessed on the ggen plane too.
Genesis law intact on both planes (record 0 / link 0 anchor all-zeros,
asserted on wire bytes / stored links).

## Disclosures

1. cgap impl has NO `verify_span`. The O(span) curve is measured through
   a TEST-LOCAL span-recompute helper running the SAME link law over
   `receipts[s..e]` — it measures what a checkpointed fast path costs,
   it adds no production API.
2. ggen `write_receipt` is `pub(crate)` — no pub writer. Ledger lines are
   minted through the pub seam types (`ReceiptRecord` +
   `recompute_chain_hash`, the exact emission-path hash) in the emission
   wire format, then verified by the REAL pub verifier. 100 real
   `ggen sync run` calls would mint only ~100 records — wrong tool for a
   100k court.
3. cgap block payloads cycle over a 256-block pool of real sealed blocks
   (100k distinct 64 KiB leaked buffers would be ~6.4 GB; per-link hash
   cost identical).
4. cgap alloc tracking via the real `cgap-audit` `CountingAlloc`; ggen
   via a hand-rolled counting global allocator in the test binary.
5. Both crates share one canonical checkout with other lanes: two
   mid-run compile freezes in `cgap-crypto` (aead.rs, fips203.rs — other
   lanes' in-flight edits) were polled out per the compile-freeze SLA
   (~90 s and ~2 min), never touched.

## Gates (unregression)

- ggen-engine full suite: `cargo test -p ggen-engine` — lib 235 passed
  0 failed; integration targets green EXCEPT one: `receipt_verb_e2e::
  resync_after_tamper_produces_verifiable_new_head` (`test result:
  FAILED. 4 passed; 1 failed`) — root-caused as a PRE-EXISTING
  deterministic hex-prefix lottery, NOT session-introduced: the test
  asserts `assert_ne!(new_chain.chars().next(), '0')` on a DETERMINISTIC
  fixture, so the re-synced head either starts with '0' or it doesn't;
  for the current binary/content it deterministically does
  (witnessed twice: suite run + isolated rerun, same `left: '0'`).
  Files predate this lane's session (test 15:07, sync.rs 16:07 vs lane
  start ~22:00); the AB-14 diff is additive test-only and that test
  binary does not link it. Needs a fix-forward by the receipt-verb
  owner (a 1/16-of-hex-space assertion cannot be a stable court).
- cgap-ffi suite (`cargo test -p cgap-ffi --no-fail-fast`): ALL 9
  integration targets green, including `receipt_chain_scale` 3 passed
  (67.03s — the ×2 green; first green run 51.67s). Lib courts 54 passed
  / 4 failed, ALL FOUR failures are the AOQ lane's IN-FLIGHT
  sandbox-archiving courts (`status_court_reachability_ledger`,
  `wave9_a07_zero_string_gate_over_all_built_crates`,
  `xip_t1_riscv_elf_size_court`, `r2_production_link_court`): their /tmp
  sandbox copies archive `src/` from a manifest predating the new
  `src/aoq` module (`E0583 file not found for module aoq`, later
  `E0433` inside `~/.cache/tmp/cgap-a19-xip-t1-*`) — pre-existing /
  other-lane, root-caused and NOT session-introduced by AB-14 (this
  lane's diff is additive test-only; the real lib compiles and 54
  courts pass). Also one doctest env failure (`can't find crate
  cgap_p28`), likewise unrelated to AB-14 (no doctests in this lane's
  files).
- New tests green x2: cgap `receipt_chain_scale` 3 passed twice
  (51.67s, 67.03s); ggen `receipt_chain_scale_stress` 2 passed three
  times (42.83s, 61.89s, 56.25s).

## Status

ALIVE (both scale courts ran green this session against the exact
subjects; both crates' suites exercised; the only failures are
other-lane in-flight surfaces, root-caused above).
