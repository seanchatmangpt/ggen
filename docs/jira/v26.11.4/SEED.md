# SEED

Next-wave frontier carried from v26.11.3. Originally compiled by ZP-4 (2026-11-04);
re-verified against disk receipts by ZD-90 (2026-11-04, this wave's seed-refresh
lane). Standing vocabulary per the receipt-cited-counts law: every status below is
re-read from the cited receipt on disk, not carried from the prior SEED.

## A. Carried ggen descope items — CLOSED (re-verified 2026-11-04)

- CARRY-01 CLOSED (zd-50-wo-drift.md): WO-01/03/05/07/09 standing promoted
  UNVERIFIED → ALIVE, WO-08 → PARTIAL-ALIVE, each equal to its cited receipt
  body; verification sweep exit 0, 0 UNVERIFIED in Standing lines.
- CARRY-02 CLOSED (zd-45-receipt-seam.md): stale `#![allow(dead_code)]` removed
  from `crates/ggen-engine/src/receipt_chain_seam.rs`; workspace check 0
  dead_code warnings; seam pin court 4/0; focused tests 6/0.
- CARRY-04 CLOSED (zd-44-skip-flag.md): `--skip-ea-admission` confirmed already
  wired CLI→gate (verbs/sync.rs:18 → handlers.rs:62,74 → sync.rs:225/354-371);
  gap closed this lane = bypass-marked receipt: `ReceiptPayload` gains
  `ea_admission_skipped: Option<String>` (skip-when-None, legacy fixtures
  byte-identical) + fail-closed court that an admitted run carries no mark.
  **MKT-01 unblock dependency satisfied.**
- CARRY-05 CLOSED (zd-59-carry05.md): FM-LAW-015 descope row →
  RESOLVED-DISCLOSED; `sync_surface_test` 6/0 ×2 under ambient 26.11.3; closed
  by xd-11-fmlaw015.md + fixture commit e9829bead.
- CARRY-07 CLOSED (zd-46-zero-verb-audit.md): phantom noun `"mod"` root-caused
  (clap-noun-verb derives noun from `file!()` stem; `mod.rs` ⇒ "mod", all bare
  `#[verb]` fns collapsed onto it); 23 dead verbs rewired across sbb (6),
  maximalism (7), vision2030 (10); 5 live ea/ verbs untouched.
- CARRY-08 CLOSED (zd-47-relay-probe.md): probe lives at
  `/Users/sac/lsp-max/crates/lsp-max-compositor/tests/diagnostic_relay_probe.rs`
  (not in ggen); hard-coded child bin replaced by `resolve_child_bin()` —
  env override → known-location search → loud typed SKIP, never a fake pass.
- CARRY-09 CLOSED (zd-48-fm-codes.md): tested_by:none census 52 → 45 (8 closed
  by new Chicago courts: FM-CONFIG-014, FM-TPL-001/006/012/013, FM-GEN-009/010,
  FM-WRITE-001; courts green ×2 + parity court 3/0 after FM-KEY-013 row added);
  typed rulings for all remaining 45 (12 U untestable / 21 P crate-private /
  12 T future-court candidates) in FM_REGISTRY.md.
- CARRY-12 (zd-51-r80.md): R80-12 CLOSED — `pub mod diagnostics_rules` already
  in HEAD (lib.rs:19) with production caller server.rs:54-60; diagnostics_rules
  tests 15/0. R80-11 PARTIAL_ALIVE — acceptance commands exit 0 (edits_bridge
  11/0 + 4/0); SCIP witness landed. R80-6 (SCIP/write-path wiring) confirmed
  L-effort, still open.
- CARRY-13/14 UNCHANGED: parked-no-consumer (D-1) not dispatched; lsp-types-max
  pin re-opens only when crates.io > 26.6.24.

## B. Publish chain (strictly ordered, Phases 0-3) — OPEN

- CARRY-15: PR #824 green-or-waived rollup → merge = tag anchor.
- CARRY-19: mint 3 tags (ggen at merge commit; cgap main; marketplace main)
  + tag-advance falsifier sweep (nothing at/beneath v26.15.2).
- CARRY-17: cgap crates.io ladder (see
  cgap docs/jira/v26.11.3/plans/crates-io-ladder-plan.md; cgap-mcp and
  wire-shm now publish=false — resolved).
- CARRY-16: verify ggen-engine version.workspace flip resolved (XD-10
  landed; re-check at open).
- CARRY-18: ggen crates.io ladder, ggen-graph first (exact =26.11.3 pin).

## C. AoQ Wave-2 state

- W2-A ALIVE (cgap zd-6-w2a-session-ffi.md), W2-C ALIVE
  (cgap zd-13-w2c-sidecar.md).
- W2-B ALIVE (arcus zd-29-w2b-twin.md).
- W2-D CLOSED (arcus zd-42-w2d-admission.md): `admit.go` admission chain
  (LookupFlightOp → LookupTranscoder → RequireBoundary → Seal/Unseal → one
  AOQ1 UDS round trip), FlightRefusal gains StatusWord/StatusName mirroring
  cgap's GENERATED status_maps.rs frozen band, 6 courts, ALIVE.
- ZD-56 rung-8 CLOSED (cgap zd-56-sidecar-e2e.md): sidecar sequential
  serve loop (accept per connection, fresh Session each, stdin-EOF stop,
  self-unlinking socket), rung-8 E2E court — decrypted == pre-seal bytes,
  tag gate fires on flipped byte. Also closes the zd-42 stale-socket class
  at the source.
- W2-E: status UNVERIFIED at this refresh — no receipt located on disk under
  either repo's docs/jira/v26.11.3/receipts/; re-verify at wave open before
  dispatch.

## D. Marketplace carried — CLOSED

- MKT-01 CLOSED (ggen-marketplace zd-62-mkt01-adoption.md): ambient ggen
  26.11.3 adopted corpus-wide as qualification court identity via the
  disclosed `--skip-ea-admission` opt-out (ZB-5/ZB-10) in
  scripts/qualify_packs.py; baseline re-projected `ggen 26.10.11` →
  `ggen 26.11.3`.
- MKT-02 CLOSED (zd-49-gac-registry.md + zd-28-regression-packs.md +
  zd-65-smalt-gates.md): gac:Registry individual landed (ZD-20 falsifier
  satisfied); 7 zd-12 regression packs repaired to ALIVE; smalt/pplan
  inverted positive-style gates rewritten violation-style (6 rewrites).
  Final corpus (zd-65): 347 packs, 347/347 qualified, zero REFUSED for the
  first time in the v26.11.3 window, full-corpus exit 0 under ambient 26.11.3.
- MKT-03 CLOSED (ggen-marketplace zd-52-warn-typing.md): 10 WARN + 1 SKIPPED
  typed — 9 ACCEPTED-WARN ratchet-residue, 1 SKIPPED harness-limitation, 1
  REPAIR-LANE real-signal (clap-noun-verb-schema-pack composition-closure
  claim unwitnessed — that one repair item survives into the open set below).
- MKT-04: pack-side standing re-verify at the v26.11.3 tag — still gated on
  CARRY-19 (no tag minted yet).

## E. ggen specific

- E-1: 2 quick-xml CVEs — upstream-blocked by process_mining; monitor
  crates.io for ≥0.38, then cargo update closes both (xc-8-cve-bump.md took
  the rest of the audit surface 18 → 2). OPEN.
- E-2: self-host observation re-pin discipline — mechanicalize as a
  gate/hook on every landing (currently prose-only). OPEN. Binding insight
  from ZB-21 (coordinator dispatch; **no ZB-21 receipt located on disk** —
  UNVERIFIED until one lands): a `--check` gate must compare against the
  observation commit, not literal HEAD — C21-class out-of-subject receipt
  binding (a commit cannot contain its own receipt).
- E-3 CLOSED by ZD-15; re-verify fmt-check ×2 + sha256 identity across two
  syncs at wave open.

## F. Open set (rewritten 2026-11-04, ZD-90)

Single-lane / larger:

- CARRY-03 (L, code): blast_radius/transactional write-path wiring (GAP-1)
  — ≥1 courted production caller, or typed parked-no-consumer.
- CARRY-06 (M, court): DS-7 D1/D2/D3 vacuous sjira criteria — tighten in
  shared court files. Coordinator reported ZD-60 dispatched for this; **no
  zd-60 receipt exists on disk** (searched ggen + cgap + arcus +
  ggen-marketplace docs/jira) — treat as IN-FLIGHT/UNRECEIPTED, do not
  dispatch a second lane over it; re-check at wave open.
- CARRY-10 (M, court): aoq-pack baseline consequence_sha256 completion.
- CARRY-11 (L, court): ~100 F2 declarative-fixture refusals triage.
- MKT-03 residue (S): clap-noun-verb-schema-pack composition-closure repair
  lane (the one REPAIR-LANE row from zd-52).
- MKT-04 (S): pack-side standing re-verify at the v26.11.3 tag (post-CARRY-19).
- E-2 (M, infra): mechanicalize the self-host observation re-pin gate,
  binding the check to the observation commit (ZB-21 insight) rather than
  literal HEAD.
- E-1 (S, monitor): quick-xml ≥0.38 watch.

Publish chain (ordered, unchanged):

1. CARRY-15 → CARRY-19 → CARRY-17 → CARRY-16 → CARRY-18.
