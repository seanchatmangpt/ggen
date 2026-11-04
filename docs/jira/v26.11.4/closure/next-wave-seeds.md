# Next-Wave Seeds — v26.11.4 pre-drain

as-of 2026-11-04T21:41Z. 8 seed rows: 3 carried from the v26.11.4 plan
set + 5 hunt-surfaced this session. Session-context rows carry explicit
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
