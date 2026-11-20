# DS-7: sjira mutation battery (C05: mutation-testing the backlog)

Standing: UNVERIFIED

Date: 2026-11-03 · Lane: DS-7 · Repo: /Users/sac/ggen · Scope: crates/ggen-sjira
Battery: `scripts/sjira_mutation_battery.py` (in-place shim protocol, sha256-exact restores,
git never invoked)

## Charter

Prove sjira's acceptance criteria are non-vacuous: mutating the acceptance-relevant code must
make the criterion's pinned test flip. Gate: battery exits 0 iff all criteria flipped.

## Deviation from charter (disclosed)

The charter named a /tmp sandbox. The repo is 93 GB with workspace/path dependencies
(ggen-scip path dep, ggen-graph workspace dep) that do not resolve outside the tree, so the
battery applies shims IN PLACE on the canonical checkout with byte-exact restore, mirroring the
smalt `scripts/shim_court/lib.sh` protocol (BASELINE → SHIM → RESTORE, fail-closed). git was
never invoked.

## Method

Per criterion: BASELINE (pinned test passes on pristine bytes) → SHIM (one minimal mutation,
each find-string verified unique in-source) → pinned test must FLIP (exit non-zero) → RESTORE
(sha256 compared; 0 restore failures across all runs). Compile-abort is classified separately
from flip (census law). The harness originally ran every cargo filter with `-- --exact`; for the
`--lib` target that silently matched 0 tests and read as exit 0 — a harness-level vacuity trap,
fixed to substring filters before any result was trusted.

## Per-criterion results

Format: `id — criterion — pinned test — shim — verdict`.

- EXEC-C01 — ceiling law (only UNKNOWN→PARTIAL_ALIVE writable) —
  `promotion_ceiling_is_typed` — relax FROM-side law in `check_promotion` — FLIP.
- EXEC-C02 — exhaustion: a budget smaller than the chain depth refuses typed `exhausted` at
  the bound, with partial progress recorded on disk (ZD-60) —
  `exhaustion_refuses_at_bound_with_partial_progress_recorded` — collapse the loop bound to
  one pass — FLIP. (Originally: no-progress-branch shim, VACUOUS DS-7-D1 — the `if !progressed`
  guard is UNREACHABLE under the frontier standing law; see ZD-60.)
- EXEC-C02c (probe) — what the pinned test DOES pin: typed exhaustion shape —
  `max_iterations_refuses_typed_exhaustion` — mutate the `exhausted` reason string — FLIP.
- EXEC-C03 — idempotent replay: AlreadyRecorded ≠ second execution —
  `replay_guard_counts_a_replayed_candidate_once` (ZD-60: the duplicated-candidate-in-one-pass
  pin at the guard site) — accept AlreadyRecorded (`if true`) — FLIP. (Originally:
  `rerun_is_idempotent_already_recorded`, VACUOUS DS-7-D2 — that test still stands as the
  user-visible replay law; the guard site itself is now pinned.)
- LEAS-C04 — multi-party: ≥2 sigs from ≥2 distinct signers —
  `two_signatures_same_signer_refuses_insufficient` — drop distinct-signer check — FLIP.
- LEAS-C05 — minimum-count: <2 records refuse —
  `one_signature_refuses_insufficient` — accept 1 record — FLIP.
- LEAS-C06 — non-monotonic: unchanged ontology digest refuses —
  `unchanged_ontology_digest_refuses_non_monotonic` — neutralize `digest_delta` — FLIP.
- LEAS-C07 — Ed25519 verification: tampered/unknown signer refuses —
  `tampered_signature_refuses_invalid` — disable `verify_all` — FLIP.
- LEAS-C08 — signatures bind every field incl. granted_at —
  `granted_at_field_change_breaks_verification` (ZD-60: tampering the granted_at VALUE after
  signing refuses SignatureInvalid) — remove granted_at from canonical bytes — FLIP.
  (Originally: `unsigned_field_change_breaks_verification`, VACUOUS DS-7-D3 — the
  subject_iri-only tamper is now supplemented by the granted_at-value tamper; the
  indirect-only C08b caveat is resolved, C08b remains a supplemental probe.)
- LEAS-C08b (probe) — which test DOES pin granted_at coverage —
  `canonical_json_is_sorted_key_compact_minus_signatures` — same shim —
  FLIP only via the canonical-shape test.
- EPOC-C09 — watermark fail-closed — `missing_watermark_fails_closed` —
  missing watermark → Ok — FLIP.
- EPOC-C10 — legacy-edit refusal — `legacy_edit_refuses` — `LegacyEdit` Err→Ok — FLIP.
- LEDG-C11 — tamper detection on fetch —
  `dir_mode_tampered_event_refuses_on_fetch_and_append` — drop verify at the fetch site — FLIP.
- LEDG-C12 — vc_concurrent law — `vc_concurrent_law` — invert vc_concurrent — FLIP.
- LEDG-C13 — epoch regression refused before any write —
  `epoch_regression_refused_before_any_write` — disable regression check in `stamp` — FLIP.
- RECP-C14 — receipts chain tamper detection at depth —
  `mid_chain_tamper_detected_at_any_depth` — disable hash recomputation check — FLIP.
- CTRL-C15 — authority fail-closed gate —
  `controller_missing_origin_authority_field_refuses_fail_closed` — disable the
  Stage::Authority gate — FLIP.
- CTRL-C16 — critical-path gate halts before admission —
  `controller_critical_path_refusal_halts_before_sjira_admission` — disable the
  violations gate — FLIP.
- PROM-C17 — courts-satisfied: every required court witnessed —
  `unwitnessed_court_refuses` (lib) — disable the `!passed` court gate — FLIP.
- DESC-C18 — blocked-order refusal with reason —
  `blocked_order_refuses_not_on_frontier_with_reason` — NotOnFrontier → not-blocked — FLIP.

Probe rows are supplemental (they witness what the pinned tests DO pin), not independent
criteria.

## Flip summary

- 16/18 criteria FLIPPED with witnessed runs (baseline pass → shim exit non-zero).
- The gate FAILED overall by design: 3 criteria did not flip.

## Defect findings (recorded 2026-11-03; CLOSED-ZD-60 2026-11-04)

- **DS-7-D1** — `execute.rs` no-progress exhaustion branch (the `if !progressed` refusal) is
  unpinned: `max_iterations_refuses_typed_exhaustion` uses max_iterations=1, so it exercises the
  post-loop budget path; disabling the no-progress branch leaves the test green. Typed shape of
  the budget path IS pinned (C02c flip). Filed for the owning lane.
  **CLOSED-ZD-60**: the guard is UNREACHABLE under the frontier standing law (eligible ⇒
  standing UNKNOWN ⇒ first append per identity Appends) — reclassified defensive dead code;
  EXEC-C02 retargeted to the loop-bound shim, pinned by
  `exhaustion_refuses_at_bound_with_partial_progress_recorded` (flips).
- **DS-7-D2** — `execute.rs`'s AlreadyRecorded replay guard (`if appended.status ==
  AppendStatus::Appended`) is unpinned: `rerun_is_idempotent_already_recorded` reaches the append
  site only in run 1; in run 2 the frontier blocks the order on standing before the append, so
  accepting AlreadyRecorded does not flip the test. The user-visible no-double-append law IS
  pinned, but through the frontier standing gate, not the replay guard itself.
  **CLOSED-ZD-60**: the guard site is now pinned directly by
  `replay_guard_counts_a_replayed_candidate_once` (duplicated candidate in one pass counts
  once; `if true` shim flips it).
- **DS-7-D3** — `lease.rs` `unsigned_field_change_breaks_verification` tampers subject_iri only;
  removing granted_at from the canonical bytes leaves it green. granted_at's signature coverage
  is pinned only indirectly by `canonical_json_is_sorted_key_compact_minus_signatures` (C08b
  flip). Filed for the owning lane.
  **CLOSED-ZD-60**: `granted_at_field_change_breaks_verification` now tampers the granted_at
  VALUE itself; removing granted_at from the canonical bytes flips it directly (C08b remains a
  supplemental probe).

## Concurrent-lane events (disclosed, not session-introduced failures)

- Mid-battery, another lane reworked `execute.rs` (added `ExecuteOpts::epoch_extensions`) and
  briefly left `tests/execute_test.rs` non-compiling; the first EXEC rows were reclassified
  COMPILE-ABORT (not flips) and rerun after the lane landed its fix. Files NOT owned by this
  lane (complete.rs, generated.rs, pack_health.rs, reconcile.rs, execute.rs) differ from my
  pre-battery snapshot because of that lane's landed edits — all seven files this lane mutated
  were restored sha256-exact (script-verified per run).

## Verification ladder (this session, all ALIVE)

- Full battery: `python3 scripts/sjira_mutation_battery.py` → 18 rows, exit 1 (gate FAIL = 3
  non-flipped criteria found).
- Reruns with corrected harness: `--only EXEC-C01 EXEC-C02 EXEC-C03 LEAS-C08 LEAS-C08b PROM-C17`
  → C01/C08b/C17 flipped; C02/C03/C08 vacuous; EXEC baselines compile-aborted (lane).
- Post-fix EXEC rerun: `--only EXEC-C01 EXEC-C02 EXEC-C03` → C01 FLIP, C02/C03 vacuous.
- Probes: EXEC-C02b (loop-bound mutation) did NOT flip — semantically a no-op under a 1-pass
  budget; recorded as a probe artifact, not a criterion verdict. EXEC-C02c flipped.
- Post-battery sanity: `cargo test -p ggen-sjira` (6 integration suites) → 65 passed; `--lib` →
  38 passed. Zero restore-failure lines in logs; shim-target find-strings all still present in
  current sources (no residue).

result: DS-7 sjira mutation battery ran 18 criteria — 16 flipped (non-vacuous), 3 genuine
unpinned-branch findings recorded (DS-7-D1/D2/D3), tree restored byte-exact; receipt at
/Users/sac/ggen/docs/jira/v26.11.3/receipts/ds-7-mutation.md

UPDATE 2026-11-04 (ZD-60, lane CARRY-06): all three findings closed — battery re-run 20/20
FLIPPED, gate PASS exit 0, vacuous count 0; full `cargo test -p ggen-sjira` 219/0. See
docs/jira/v26.11.3/receipts/zd-60-ds7-tighten.md
