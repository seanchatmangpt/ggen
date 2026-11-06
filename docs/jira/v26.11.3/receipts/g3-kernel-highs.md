# Receipt: g3-kernel-highs (v26.11.3, lane G3)

Standing: ALIVE

**Lane**: G3 (IMPLEMENTATION — kernel HIGH defects)
**Subject**: `/Users/sac/ggen/crates/ggen-abb-sbb` (canonical checkout, no worktree)
**Date**: 2026-11-03
**Gate**: `cargo test -p ggen-abb-sbb` — ALIVE, exit 0, this session.
**Result**: 10 test binaries, **106 tests passed, 0 failed, 0 ignored**
(binary split: 11 + 4 + 13 + 4 + 43 + 8 + 1 + 16 + 3 + 3; includes the new
8-test `kernel_highs_courts` binary).

## Owned-scope edits (all landed on disk)

### H1 — ceiling semantics inverted (envelope_court.rs)
- `Some(&declared) if declared == measured => Pass` → `Some(&declared) if measured <= declared => Pass`,
  with a HIGH-1 comment citing the v26.11.3 fix and the old-law inversion.
- `Crates/ggen-abb-sbb/src/envelope_court.rs` (law arm + module docs + `ReplayVerdict::Pass` doc in lib.rs).

### H2 — envelope-vs-receipt graph_digest binding (envelope_court.rs)
- `envelope_court` now refuses `ENVELOPE_GRAPH_MISMATCH` (exit 2) when
  `env.graph_digest != env.receipt.graph_digest`, before any cap evaluation.
- Same file, top of `envelope_court`; module docs updated.

### H3 — absent packs list disabled dangling-pack check (lib.rs:580)
- Removed the `!self.packs.is_empty() &&` guard in `EaGraph::validate`:
  an unresolvable `sbb.pack` now refuses `DanglingReference` whether or not the
  graph carries a `packs` list, with a HIGH-3 comment. Empty/element-colliding
  packs still refuse `PackConflation` ahead of this check (unchanged).
- Pre-flight safety: both fixtures and `synthetic_graph` carry `packs`; no test
  or fixture relied on the absent-list bypass (grepped before removing).

### L1 — remediation sentences (lib.rs Display strings)
- `SBB_UNQUALIFIED`: "— qualify the SBB against its contract, then re-admit"
- `STALE_QUALIFICATION`: "— re-qualify the SBB against its current digest, then re-admit"
- `EXCEEDS_CONTRACT`: "— shrink the realization below the contract ceiling, or re-qualify against the current contract digest"

## Court tests
- New file `crates/ggen-abb-sbb/tests/kernel_highs_coverage` — actual name:
  **`tests/kernel_highs_courts.rs`** (7 tests): H1 declared-64/measured-5 Pass;
  H1 declared-4/measured-5 `CAP_CEILING_MISMATCH` exit 2; H1 boundary
  declared==measured Pass; H2 `ENVELOPE_GRAPH_MISMATCH` typed refusal + digest
  movement; H3 resolvable-pack control admits; H3 absent-list unresolvable-pack
  refuses; H3 present-list unresolvable-pack refuses; L1 remediation presence
  over real `admit` refusals (all built from real kernel receipts via
  `synthetic_graph` + real `admit`/`manufacture` — no mocks).
- Updated `tests/envelope_courts.rs` Court 2 with receipt note: the x10-raise
  mutant encoded the OLD equality law; under the corrected ceiling law an x10
  raise is lawful headroom (now PASSES by design), so the mutant was inverted to
  clamp-below-measured (`saturating_sub(1)`), preserving the court's intent.
- Updated `tests/falsifiers.rs` DoD-9 positive control with receipt note: it
  previously cleared `packs` and relied on the absent-list bypass; under H3 the
  control now registers `pack:fresh` in the packs list.

## Minimal unblock disclosure (compile-freeze SLA)
- `examples/admit_demo.rs`: added `requires: BTreeSet::new()` to the
  `ConsumerEdges` initializer (WO-07 lane added the field to depgraph.rs without
  updating this initializer; empty set preserves prior behavior). Disclosed in
  both lanes' receipts per SLA. `tests/property_admission_test.rs` had already
  been unblocked by the WO-07 lane itself before my edit (their disclosure
  comment at line 372) — my edit there was a no-op (string-not-found).

## Test-law updates (old-law encodings, with receipt notes in-file)
- `tests/envelope_courts.rs` Court 2 (x10 mutant → clamp mutant) — receipt note in doc comment.
- `tests/falsifiers.rs` DoD-9 positive control (absent-list bypass → registered pack) — receipt note in comment.

## Non-session failures observed and ruled out
- `refusal_crosswalk_test::court_every_wire_code_has_ctor` failed transiently
  mid-session: concurrent lane G5/WO-05 was mid-edit on
  `src/generated/refusals.rs` (`E-ENVELOPE-UNDECLARED` ctor). The ctor has since
  landed; the court passes in the final run. Pre-existing/concurrent, not introduced here.
- `property_admission_test` / `admit_demo` compile errors: WO-07's additive
  `requires` field; unblocked per SLA (above). Not session-introduced.

## Verification ladder
1. Read-back of all edited files (Edit tool verified each diff in-session).
2. `cargo test -p ggen-abb-sbb` — 106 passed / 0 failed (exit 0, final run).
3. All pre-existing falsifier/property/bench-bound/marketplace-fixture suites green.
4. Compile-fail doctests still enforced (`#![forbid(unsafe_code)]` intact, sealed
   `Admitted` untouched).

## Standing
ALIVE — gate run this session, exit 0, counts re-read from the final run output.
