# F1 Chain-Rule Compat Window Receipt — DEF-K6-1 closed

Standing: ALIVE

- **Date**: 2026-11-03
- **Repo**: /Users/sac/ggen
- **Lane**: F1 IMPLEMENTATION (receipt-chain read/verify path, DEF-K6-1)
- **Standing**: ALIVE — every claim below carries a command + real exit from
  this session.

## Fold-algorithm analysis (the decision)

**`praxis-chain/v2-fold` and `graphlaw-chain/v2-fold` are the SAME fold
under different names.** Witnessed empirically, twice:

1. Source-level: graphlaw's
   `/Users/sac/graphlaw/src/receipt_chain/record.rs:461` defines
   `CHAIN_RULE_V2_FOLD = "graphlaw-chain/v2-fold"` as "the F1 fix: the
   base admission-frame chain hash, then `schema` and the full `v2`
   payload folded in by `fold_in_v2_epoch`". The retired praxis source on
   disk (`~/praxis/crates/praxis-core`) carries no `CHAIN_RULE_*` constant
   (grepped: zero hits), and git is forbidden in-lane, so name-history
   evidence was unavailable; the empirical probe is the authority.
2. Empirical: all 16 legacy-rule records from the real repo-root ledger
   (`.ggen-v2/receipt-log.jsonl` records 199-214, copied VERBATIM as
   `crates/ggen-engine/tests/fixtures/receipt_chain_legacy_tail.jsonl`)
   recompute their stored chain hashes exactly under
   `ChainRule::V2Fold` once the discriminator is aliased
   (`every_legacy_ledger_entry_recomputes_under_the_v2_fold` — green).

Therefore: migration (option b) was unnecessary AND unlawful to attempt
blindly (rewriting historical entries would break their stored hashes'
provenance); the compat window (option a) is the honest fix. No ledger
bytes were modified.

## Change (compat window, read-side only)

- `crates/ggen-engine/src/receipt_chain_seam.rs`: added
  `CHAIN_RULE_V2_FOLD_LEGACY = "praxis-chain/v2-fold"` and
  `verify_chain_compat()` — identical to `ReceiptRecord::verify_chain`
  except a legacy discriminator is aliased to `ChainRule::V2Fold` before
  recompute. Fail-closed preserved: an unrecognized rule, or a legacy
  record whose hash does NOT recompute under the fold, refuses exactly as
  before.
- Call sites switched to the compat verify (read/verify path only):
  - `sync.rs` previous-head check (FM-CHAIN-009 site).
  - `verbs/handlers.rs` `handle_receipt_verify_in` (FM-CHAIN-014 site) and
    `handle_receipt_history_in` (FM-CHAIN-007 site). handlers.rs is
    outside the lane's named files but IS the receipt read/verify path;
    edits are two one-line call-site swaps, disclosed here.
- Writes untouched: sync.rs:4310 still emits `CHAIN_RULE_V2_FOLD`
  exclusively (pinned by test).

## Test (crates/ggen-engine/tests/receipt_chain_rule_test.rs, NEW)

4 tests, all green: `cargo test -p ggen-engine --test
receipt_chain_rule_test` → 4 passed; 0 failed (0.73s).

1. `legacy_discriminator_is_refused_by_the_raw_verifier` — pins the
   DEF-K6-1 defect: raw `verify_chain` refuses the legacy string.
2. `every_legacy_ledger_entry_recomputes_under_the_v2_fold` — the
   fold-identity witness over the verbatim fixture bytes.
3. `tampered_legacy_entry_refuses_under_compat` — mutating a v2 admission
   reason flips Verified → Mismatch (window widens the accepted
   discriminator, never the verification).
4. `legacy_ledger_tail_extends_and_the_whole_chain_verifies` — end-to-end
   over the real write path: tempdir seeded with the verbatim FULL ledger
   (`fixtures/receipt_chain_full_ledger.jsonl`, records 1-214) + matching
   head; real `sync` extends the legacy head (was FM-CHAIN-009), the new
   record declares `graphlaw-chain/v2-fold`, adjacent links hold across
   the legacy→current boundary, and the real CLI `ggen receipt history`
   verifies the whole 215-record ledger. Fresh-project first record also
   asserts current-rule emission.

Fixtures are verbatim ledger bytes and cannot be trimmed (payload binding
hashes raw line bytes).

## Repo-root witness (the DEF-K6-1 unblock itself)

- `./target/debug/ggen receipt history` → exit 0, `"valid": true`
  (K6 recorded FM-CHAIN-007 invalid at index 198).
- `./target/debug/ggen receipt verify` → exit 0, `"valid": true`
  (K6 recorded FM-CHAIN-014).
- Repo-root sync additionally blocked by DEF-K6-2/3 — other lanes' files,
  not addressed here.

## Verification ladder (this session, real exits)

- `cargo build -p ggen-cli-lib --bin ggen` → Finished, exit 0.
- `cargo test -p ggen-engine --test receipt_chain_rule_test` → 4 passed,
  0 failed.
- `cargo test -p ggen-engine receipt` (filtered) → 32 suites ok.
- `cargo test -p ggen-engine` (full) → all suites ok EXCEPT
  `doctor_e2e`: 4 failures, ALL the same pre-existing `FM-LAW-015` EA
  admission-gate refusal at sync SETUP (before any chain verification
  runs; the tests predate the gate and lack `skip_ea_admission`).
  Pre-existing relative to this lane; doctor_e2e is not a lane file.
- Bounded retries for lane contention: repeated runs above were stable;
  no compile-freeze encountered.

## Files authored

- crates/ggen-engine/src/receipt_chain_seam.rs (compat window added)
- crates/ggen-engine/src/sync.rs (one call-site swap in the
  previous-head verify)
- crates/ggen-engine/src/verbs/handlers.rs (two call-site swaps,
  disclosed above)
- crates/ggen-engine/tests/receipt_chain_rule_test.rs (NEW)
- crates/ggen-engine/tests/fixtures/receipt_chain_legacy_tail.jsonl (NEW,
  verbatim ledger records 199-214)
- crates/ggen-engine/tests/fixtures/receipt_chain_full_ledger.jsonl (NEW,
  verbatim ledger records 1-214)
- docs/jira/v26.11.3/receipts/f1-chain-rule.md (this file)
