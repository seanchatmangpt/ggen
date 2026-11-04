# ZD-60: DS-7 registered-vacuous criteria tightened (CARRY-06)

Standing: battery gate PASS, exit 0 — vacuous count 0

Date: 2026-11-04 · Lane: CARRY-06 · Repo: /Users/sac/ggen (branch v26-11-3-wave) ·
Scope: crates/ggen-sjira court files + scripts/sjira_mutation_battery.py

## Charter

Close the three registered defects from the DS-7 mutation battery receipt
(`ds-7-mutation.md`, DS-7-D1/D2/D3): each vacuous criterion must pin its law with an
anti-vacuity mutation that flips it.

## What changed

- `crates/ggen-sjira/tests/execute_test.rs` — two new tests:
  - `exhaustion_refuses_at_bound_with_partial_progress_recorded` (D1): a depth-3 chain
    (OA→OB→OC) with `max_iterations: 2` must refuse typed `exhausted` naming "pass 2 of 2",
    with BOTH executed receipts (OA, OB) on disk. Collapsing the loop bound to a single pass
    leaves only OA's receipt and flips the assertion.
  - `replay_guard_counts_a_replayed_candidate_once` (D2): the same order twice in one
    frontier pass — the first candidate appends, the second replays to AlreadyRecorded.
    Asserts `plans.len() == 1`, `events.len() == 1`, one on-disk event; then a fresh run is
    standing-blocked appending nothing. Accepting AlreadyRecorded as appended (`if true` shim)
    double-counts and flips.
- `crates/ggen-sjira/tests/lease_epoch_test.rs` — `granted_at_field_change_breaks_verification`
  (D3): tampering the granted_at VALUE after signing must refuse `SignatureInvalid`. The lease
  binds the exact recorded timestamp, not merely granted_at's presence.
- `scripts/sjira_mutation_battery.py` — three rows retargeted:
  - EXEC-C02: shim switched from the `if !progressed` branch to the loop bound
    (`1..=opts.max_iterations` → `1..=1`), pinned by the new bound test.
  - EXEC-C02b probe row retired (superseded by the retargeted C02; its old filter could not
    flip under a 1-pass budget).
  - EXEC-C03 filter → `replay_guard_counts_a_replayed_candidate_once`.
  - LEAS-C08 filter → `granted_at_field_change_breaks_verification`.
  - PROM-C17 shim find-string repaired to the current (reformatted) promote.rs gate text —
    the old one-liner no longer matched after another lane's rustfmt pass (disclosed below).

## Reclassification: DS-7-D1's original guard is UNREACHABLE (disclosed)

The battery's original EXEC-C02 shim (`if !progressed` disabled) was provably unflippable by
any black-box test: the branch requires an eligible candidate whose append returns
AlreadyRecorded, but eligibility requires chain-tip standing UNKNOWN (frontier.rs
`standing_str != "UNKNOWN"` → blocked), and an event already recorded for the identity implies
the ledger projects that order at PARTIAL_ALIVE — contradiction. Within one pass, the first
candidate per distinct identity always Appends, so `progressed` is always true when the pass
did any work. The branch is defensive dead code under the current frontier standing law; the
reachable exhaustion law is the loop bound at the budget ceiling, which the new test pins.

## Concurrent-lane events (disclosed, not session-introduced)

- PROM-C17: another lane reformatted `src/promote.rs` (the `UnwitnessedCourt` return is now a
  multi-line literal), so the battery's find-string went UNAPPLICABLE mid-run. Repaired in the
  battery script only; promote.rs untouched by this lane.
- Mid-session build races: sccache (RUSTC_WRAPPER, ZD-39's in-flight infra) failed with exit
  254 and transient E0583 "file not found" errors on pristine registry sources (memchr/
  regex-automata). Both cleared on retry with `RUSTC_WRAPPER=""`; no tree state changed. A
  concurrent lane's `ggen-lsp --release` stress run held the shared target lock; my full-suite
  run queued behind it.

## Verification ladder (this session, all ALIVE)

- Tightened suites green ×2:
  `cargo test -p ggen-sjira --test execute_test --test lease_epoch_test` → execute_test 8
  passed/0 failed, lease_epoch_test 23 passed/0 failed (run 1 and run 2, exit 0 both).
- Full crate: `cargo test -p ggen-sjira` → exit 0, 21 suites all `test result: ok`,
  **219 passed / 0 failed** (211+ unregressed; kernel_stress alone 2170.64s).
- Full battery: `python3 scripts/sjira_mutation_battery.py` → 20 criteria run, **20/20 FLIPPED,
  gate PASS, exit 0** — vacuous count 0. Zero RESTORE-FAILED lines; zero VACUOUS/COMPILE-ABORT.
- Mutation witnesses for the four retargeted rows (each: baseline pass → shim exit 101):
  EXEC-C02, EXEC-C02c, EXEC-C03, LEAS-C08, LEAS-C08b, PROM-C17.

## DS-7 register rows updated

In `ds-7-mutation.md`: DS-7-D1/D2/D3 marked CLOSED-ZD-60 with their replacement shims/tests;
EXEC-C02/EXEC-C03/LEAS-C08 verdict rows updated; LEAS-C08's "pinned only indirectly" caveat
resolved (C08 now flips directly on the granted_at VALUE shim; C08b remains a supplemental
probe).

result: all 18 DS-7 criteria (20 battery rows incl. probes) now flip under their pinned
mutations — battery gate PASS exit 0, full crate suite 219/0, tree byte-restored by the
battery (0 restore failures). Receipt at
/Users/sac/ggen/docs/jira/v26.11.3/receipts/zd-60-ds7-tighten.md
