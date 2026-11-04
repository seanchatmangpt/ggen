# Next-Wave Seeds — v26.11.4 pre-drain

as-of 2026-11-04T21:41Z (updated post-fill by the INTEGRATION lane:
10 rows). 8 seed rows at fill: 3 carried from the v26.11.4 plan
set + 5 hunt-surfaced this session; S-11/S-12 added from O3's
mechanical-gates-spec. Session-context rows carry explicit
reconstruct-or-demote markers. Lines <=100 chars.

## A. Carried (3)

| id | seed | size | falsifier |
|---|---|---|---|
| S-1 | OI-1 follow-up: ggen tag-push publish rehearsal now that
  engine pins are =26.11.4 (see open-items-register OI-1 correction); the
  zp-16 receipt's BLOCKED claim is stale | M | pre-fix-gate still refuses on a
  rehearsal run |
| S-2 | OI-7: cgap workspace version bump 26.11.3 → 26.11.4 + tier-DAG
  consistency pass | S | version skew breaks a tier job's `version` guard |
| S-3 | OI-6: ZP-21/ZP-14 dispute — coordinator decision + retire the
  loser's surface | S | two `cbmqs-audit` homes ship in one tree |

## B. Hunt-surfaced (5)

| id | seed | size | falsifier |
|---|---|---|---|
| S-4 | ZP-15 foreign-subject projection still UNVERIFIED (no receipt in
  ggen/cgap/arcus trees, PLANS.md:15) | L | projection court renders P1-P8
  rows with `projection_class` on a real subject |
| S-5 | ZP-19 slab/TLS WindowBuffer integration UNVERIFIED (PLANS.md:22-23);
  G7-chain unfenced row stays open (PLANS.md:137) | L | 0 B/op CTQ-03-sidecar
  row flips on a real soak |
| S-6 | ZB-65 ubuntu half: verify_v26.10.31.sh:80 BSD `sed -i ''` still
  open (PLANS.md:171-174) | S | verify script exits 0 on ubuntu runner |
| S-7 | E-2 mechanicalize self-host observation re-pin gate bound to the
  observation commit, not literal HEAD (SEED.md:101-106) | M | gate flips on
  an observation-commit-shifted replay |
| S-8 | MKT-04 pack-side standing re-verify at the v26.11.3 tag, gated on
  CARRY-19 (SEED.md:93-94) | S | a qualified pack fails at-tag re-verify |

## Session-context rows (reconstruct-or-demote)

- S-1 carried text comes from the session coordination ledger, not disk:
  reconstruct in-writing at drain or demote.
- `_impl14` row (open-items OI-8): no on-disk referent found; reconstruct
  in-writing at drain or demote.
- **S-9 (NEW, gate-family harness boot path)** — `gates/common.py` spawn_substrate boots BEAM-only and times out on `wait_for_port(arm)` post-WO-32 cordon (posh owns 9443); none of the 7 invariant gates can write fresh receipts until the harness gains a BEAM+posh boot path. Witness: D33 BLOCKED-BOOT (4 attempts, warm-compile) + E-z3 corroboration. Size M. Falsifier: all 7 gates write fresh same-day receipts after the fix.
- **S-10 (NEW, receipt-lint vocabulary extension)** — accept Standing|Status|verdict|
  Exit-code synonyms (P-p root-cause: vocabulary drift, not missing status). Size S.
  STATUS update (post-fill): EXISTS in its backfill half — D108 ran the lint's
  --backfill mode across the ~60 flagged receipts and inserted canonical
  `Standing:` lines (see mechanical-gates-spec.md Gate 3 backfill note). Remaining
  leg: land `scripts/receipt_lint.py` + the census standing-map extension as a CI
  job (`just lint-receipts`) — carried as S-12 below.
- **S-11 (NEW, lane-registry gate)** — O3 mechanical-gates-spec Gate 1:
  `docs/jira/<milestone>/lane-registry.json` (coordinator-minted, single writer,
  disjoint owned_paths enforced at mint) + pre-commit hook
  `scripts/hooks/lane-lease-check` (GGEN_LANE_ID lookup; foreign staged path
  refuses with the owning lane's ID; stale lease refuses; GGEN_LANE_ID=COORD
  audits orphans) + slot-map projection `scripts/lane_slot_map.sh`
  (projection-not-copy, hand edits prohibited). Would have prevented D25/D32's
  staged-deletion sweep and C6/U4's a11f072b attribution split; mechanically
  enforces same-checkout-fanout law 3 (coordinator owns transitions). Size S-M.
  Falsifier: a commit staging a path outside the lane's owned globs lands
  without exit-1 or an explicit disclosed adopt.
- **S-12 (NEW, receipt-lint vocabulary extension)** — O3 mechanical-gates-spec
  Gate 3 (= S-10's remaining leg; O3's own table numbers this row S-13, this
  file's numbering governs): `scripts/receipt_lint.py` with the accepted
  synonym vocabulary (Standing | Status | verdict | Result | Exit-code |
  Exit code | Gate; line-start keys, backtick/parens stripped) and the ALIVE
  standing value set; `--backfill` emits diffs, never writes; the census
  consumes the lint's parsed standing-map (file -> standing, mtime+hash
  pinned) so once a file parses it never re-flags. Size S. Falsifier: a
  receipt whose standing line uses an accepted synonym still FAILs the
  census, or a no-standing-line receipt passes.
- **S-11/S-12 provenance + doctrine-conflict note (INTEGRATION lane)** — both
  rows are spec'd in `mechanical-gates-spec.md` with data shapes, hook
  scripts, and effort estimates; adopt them, do not re-spec. Runbook-vs-
  doctrine conflicts named, none resolved unilaterally: (a) runbook §1.7
  sizes the fleet to observed 429 behavior ("start smaller, never stacked
  retries") vs fanout-first's default 10-lane opening move — sizing principle
  differs; (b) runbook §2 GIT clause permits contract-granted scoped lane
  commit rights vs same-checkout-fanout's "coordinator owns transitions,
  commits per lane" — the registry gate (S-11) is written for the
  coordinator-commits reading; if scoped lane commits are ever granted, the
  hook must key on the committing lane's own lease.
