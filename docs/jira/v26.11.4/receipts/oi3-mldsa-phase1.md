# OI-3 / ZP-22 Phase 1 — FIPS-204 Parameter Scaffold + Witness Corpus (ggen)

Standing: PASS

## Scope

Phase 1 ONLY of the ZP-22 5-phase ML-DSA-65 signer build (spec: ggen
`docs/jira/v26.11.4/PLANS.md:135-136` + cgap PLANS.md ZP-22 section).
No signing implementation attempted. Falsifier addressed: no FIPS-204
witness existed in this tree → a witness corpus now exists and a loader
court validates all 3 vectors.

## Files owned (all NEW, path-owned by lane OI-3)

- `/Users/sac/ggen/docs/jira/v26.11.4/ml-dsa/fips204_parameters.json` —
  ML-DSA-44/65/87 Table-2 parameter sets + shared constants, source cited
  (NIST FIPS 204, doi:10.6028/NIST.FIPS.204).
- `/Users/sac/ggen/docs/jira/v26.11.4/ml-dsa/witness_fixtures.json` —
  3 ML-DSA-65 keygen witness vectors. Vector source (cited in-file):
  NIST ACVP `ML-DSA-keyGen-FIPS204` (25-vector set), consumed via the
  BoringSSL mirror, fetched 2026-11-04 into cgap
  `crates/cgap-crypto/tests/mldsa65_kat_courts.rs`; seeds + pk[0:32] rho
  extracted byte-exact from that file. sigma/K embedded at full 64/32-byte
  length, derived by the pinned expansion law below (validated 3/3 against
  the published ACVP rho — the derivation is witness-anchored, not invented).
- `/Users/sac/ggen/docs/jira/v26.11.4/ml-dsa/loader_court.py` — loader court,
  pure stdlib (`hashlib.shake_256` = real FIPS 202 SHAKE-256; no mocks).

## Key law pinned (empirically, this session)

Seed expansion: `(rho, sigma, K) = SHAKE-256(xi || k || l, 128)` with
k=0x06, l=0x05 for ML-DSA-65 (FIPS 204 final keygen derivation). Pinned by
grid-search over hash/concat variants against the 3 published ACVP rho
values — 3/3 byte-exact match (H(xi,128), H(xi||6||4,128), H(xi,64) all
REFUTED for ML-DSA-65).

## Gate (GATE: loader court exit 0 on real vectors)

```
$ cd /Users/sac/ggen/docs/jira/v26.11.4/ml-dsa && python3 loader_court.py; echo "exit=$?"
COURT-PASS: 3/3 ML-DSA-65 keygen vectors witness-validated (SHAKE-256 seed
expansion byte-exact vs NIST ACVP rho); parameter scaffold OK; anti-vacuity
mutation caught
exit=0
```

Court gates: G1 parameter-scaffold completeness + ML-DSA-65 pinned
constants; G2 fixture hex hygiene + pk_prefix==rho cross-check;
G3 expansion law on all vectors (>=1 = falsifier gate); G4 anti-vacuity
(one-byte rho mutation must flip the court; it does).

## Corrections made during the lane (disclosed)

- First draft of `fips204_parameters.json` carried a mangled prose key and
  Python-style numeric literals (invalid JSON) — fixed, `python3 -m json.tool`
  validates clean.
- First draft of `witness_fixtures.json` embedded a HAND-FABRICATED
  continuation of keygen-0's sigma beyond the witnessed 16-hex prefix —
  caught and replaced before the court ran; all sigma/K fields are now the
  real full-length expansion output.

## Registered gaps (not hidden)

- Full pk (1952 B) / sk (4032 B) byte-exact fixtures not yet embedded;
  witnessed surface this phase = seed-expansion law vs published ACVP rho.
- sigGen vectors: Phase 2.
- Note: cgap's own keygen KAT court registers its t1/t0 (A*s1+s2 row
  product) region UNVERIFIED pending a differential row-product court —
  that risk mass is cgap's lane, not closed here.

## Commands + exits

| Command | Exit |
|---|---|
| `python3 -m json.tool fips204_parameters.json` | 0 |
| `python3 loader_court.py` | 0 |

Git: no commits made (lane law).
