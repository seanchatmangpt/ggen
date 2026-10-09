# Two-Tier Attestation Spec — `affidavit.architecture-qualification.v2` (normative extension)

Status: NORMATIVE (extends the shipped v2 receipt)
Version: 1.0 (2026-10-08)
Grounding: affidavit @ ee57f9d, ggen @ fcfd6349, bcinr @ 3fe4ba75

## 0. Grounding Receipt

Every element of this spec is grounded in the real affidavit implementation:

| Element | Real anchor |
|---|---|
| Schema id `affidavit.architecture-qualification.v2` | affidavit `src/architecture.rs:43` `ARCHITECTURE_RECEIPT_SCHEMA` |
| Receipt struct | `src/architecture.rs:140` `ArchitectureQualificationReceipt` (`#[serde(deny_unknown_fields)]`) |
| Standing enum | `src/architecture.rs:51` `ArchitectureStanding` |
| BLAKE3 receipt digest | `src/architecture.rs:227` `blake3_digest`; reseal at `:379` — digest over the receipt with the digest field cleared |
| Supersession / chain | `:496` `supersede`, `:534` `verify_chain`, `:576` `verify` |
| sj_record faces | `src/sj_record.rs:271` `SjRecordDocument` (identity, authority, consequence, replay(+replay_binding), standing — the five R faces; plus work_order_id, origin_authority, provider); full verify at `:576` |
| sj_record tamper refusals | `src/sj_record.rs:302` `SjRefusal`: `ChainTamper` (:352), `DigestMismatch` (:355), `ChainHeadMismatch` (:362), `EventClaimMismatch` |
| Standing vocabulary | `src/sj_record.rs:81` `StandingValue` (`UNKNOWN|PARTIAL_ALIVE|ALIVE|BLOCKED|BUILD_BROKEN|UNSUPPORTED|REFUSED`), schema pattern at `schemas/dfcm-receipt.schema.json:115` |
| Crypto providers | Ed25519 (`src/ed25519_witness.rs:85` `verify_witness`, `ed25519_dalek`), ES256K (BIP-340 + RFC 6979 ECDSA, `src/secp256k1_witness.rs`), ML-DSA-65 (FIPS 204, `src/crypto_trust_pqc.rs:26` `ML_DSA_65_SPEC_REF`, keygen/sign/verify at `:78/:98/:115`, hybrid ES256+ML-DSA-65 at `:201`) — ML-DSA-65 is **live**, not integration-in-progress (CHANGELOG.md:272: "placeholders were removed") |
| KAT suites | `src/crypto_trust_kat.rs:59` `KAT_ALGORITHM_REGISTRY` = `ED25519|Ed25519@@ES256|Es256@@ES256+ML-DSA-65|HybridEs256MlDsa65@@ES256K|Es256k@@ML-DSA-65|MlDsa65@@SLH-DSA-SHA2-128s|SlhDsa128s`; golden literals `:479-501`; multi-surface fixture `fixtures/crypto_trust_kat_vectors.json` (schema `CTP-KATVECTORS-v1`, `tests/crypto_trust_kat_vectors.rs:76`); Rust KAT `cargo test --features crypto-trust --lib crypto_trust_kat` = 15 passed, 0 failed; python cross-runtime `tools/verify_crypto_trust_kat.py` = 27 passed, 0 failed, 5 skipped (skips are rust-only PQ signature checks — the honest cross-runtime gap documented at `tests/crypto_trust_kat_vectors.rs:33-38`) |

## 1. Subject Model: the sj_record Five Faces

The qualified subject of an architecture-qualification receipt is an
execution record carrying the five R faces (identity, authority,
consequence, replay, standing), already sealed as an sj_record:

- **identity**: `SjRecordDocument.identity` (sj_record.rs:271) with the
  non-commit digest face `SubjectDigest` (:183, `{algorithm: "blake3", value: <64 hex>}`),
  re-derived, never read back (`SjRecord::subject_digest` :563,
  `subject_digest_of(&self.base)`).
- **authority**: the ceiling actually executed under (:199), always at or
  below the origin ceiling; affidavit never confers DO
  (architecture.rs:163 `confers_do_authority` always false).
- **consequence**: `Consequence` (:210).
- **replay**: `Replay` (:238) + causal edges `ReplayBinding` (:248) with the
  chain-head binding checked in verify (:592-595).
- **standing**: `Standing` (:259) carrying the receipt standing vocabulary
  (`StandingValue`, :81).

Tamper law: `SjRecord::verify()` (:576-599) re-derives events, chain, head
binding, and subject digest; any divergence is a typed refusal
(`ChainTamper`, `EventClaimMismatch`, `ChainHeadMismatch`, `DigestMismatch`).
A qualification receipt never attaches to an sj_record that fails
`SjRecord::verify()`.

## 2. Two-Tier Partition

A qualified receipt binds **two independent signature tiers** over the same
receipt digest:

**Tier 1 — classical (live).** Ed25519 (or ES256K) over
`T1_DOMAIN || receipt_digest`, where `T1_DOMAIN = "affidavit.aq.v2/tier1"`.
Provider: `ed25519_witness.rs` (`ed25519_dalek`); alternative ES256K per
`secp256k1_witness.rs`. This tier is required for a receipt to be admitted
today.

**Tier 2 — post-quantum (live).** ML-DSA-65 per FIPS 204 over
`T2_DOMAIN || receipt_digest`, `T2_DOMAIN = "affidavit.aq.v2/tier2"`.
Provider: `crypto_trust_pqc.rs:78/98/115` (RustCrypto `ml-dsa`). Hybrid
ES256+ML-DSA-65 is available per `crypto_trust_pqc.rs:201` and is the
KAT-registered composition (`HybridEs256MlDsa65` in
`KAT_ALGORITHM_REGISTRY`). Tier 2 is required for SUPERSEDED chain-head
receipts and recommended everywhere; a Tier-1-only receipt is structurally
valid but flagged `tier2: false`.

Wire mapping: the ARW/1 TIER enum (wire-protocol.md Section 3.2) carries
0x01 TIER1_ED25519, 0x02 TIER2_MLDSA, 0x03 TIER_DUAL. (Note: the ARW/1
header has no room for signature bytes; signatures travel in the JSON
envelope beside the packet, keyed by receipt_digest — the same envelope
pattern as `SjRecordWire`, sj_record.rs:549.)

## 3. Qualification Poset (normative)

The spec poset over qualification standing:

```
UNQUALIFIED < PROVISIONAL < QUALIFIED < SUPERSEDED
```

- UNQUALIFIED: no admitted evidence. Never certifiable.
- PROVISIONAL: evidence admitted, qualified against a contract but not yet
  against the exact subject triple.
- QUALIFIED: qualified against the exact ABB/contract/SBB subject
  (architecture.rs:56-57).
- SUPERSEDED: terminal. Retirement record of a replaced receipt, chained to
  both the retired receipt and its successor (architecture.rs:60-62,
  `Supersession` at :167).

**Mapping to the shipped enum (honest note).** The real
`ArchitectureStanding` is `Unknown | Candidate | Qualified | Refused |
Superseded` (architecture.rs:51-62, deriving `Ord`). The spec poset maps:
UNQUALIFIED := Unknown, PROVISIONAL := Candidate, QUALIFIED := Qualified,
SUPERSEDED := Superseded. `Refused` is **outside the poset** — it is not a
rank but a verdict; it never participates in monotonicity. The enum already
derives `PartialOrd/Ord`, and the derived order coincides with the spec
poset order, so non-regression can be enforced with the existing derive; a
`Refused` receipt must be refused in any monotonicity check.

**Non-regression enforcement.** For a fixed subject key
`(abb_digest, contract_digest, sbb_digest)` (fields at architecture.rs:144-148),
the standing rank observed at any time t2 must be >= the rank at t1 < t2,
except via the lawful transition QUALIFIED -> SUPERSEDED executed by
`supersede` (:496), which produces a `Supersession` (successor QUALIFIED +
retired SUPERSEDED, verified by `verify_supersession`-shaped law at :576).
Any other rank decrease for the same subject key is refused
E_STANDING_REGRESSION (0xE0AD). This is the machine form of the existing
regression-bound test law (`tests/architecture_qualification.rs:367`
`certify_and_replay_stay_within_regression_bound`).

## 4. Receipt Schema (v2 fields, normative)

The receipt is the shipped struct (architecture.rs:140-165), unchanged:

`schema` (= `affidavit.architecture-qualification.v2`), `abb_digest`,
`contract_digest`, `sbb_digest`, `exact_subject_digest`,
`qualification_evidence_digests` (canonical: sorted, deduplicated),
`producer_digest`, `artifact_digests` (canonical), `standing`,
`confers_do_authority` (always false), `prior_receipt_digest`,
`superseded_by_receipt_digest`, `receipt_digest`.

**Hash-chain link derivation (normative formula).**

```
receipt_digest = BLAKE3-32( JCS(receipt with receipt_digest cleared) )   -- architecture.rs:379 reseal, :160
chain_link(prior, successor) =
    BLAKE3-32( "affidavit.aq.v2/chain" || hex_decode(prior.receipt_digest)
                                     || hex_decode(successor.receipt_digest) )
```

The domain string follows the domain-tag law of the sj_record
(`CAMPAIGN_DOMAIN`, sj_record.rs:39-40: "a change to the canonical layout is
a new domain, never a silent rewrite") and the journal entry_hash law
(crypto_trust_journal.rs:12-19: BLAKE3 over `digest(JOURNAL_DOMAIN, ...)`).
Chain verification: `verify_chain` (architecture.rs:534-556) requires the
successor to point at an intact QUALIFIED prior of the same ABB/contract
with a changed SBB; the ARW/1 stream layer additionally binds CHAIN_PREV per
wire-protocol.md Section 4.5.

**Two-tier extension fields** (v3 candidates, additive; v2 consumers reject
unknown fields per `deny_unknown_fields`, so tier fields ride the envelope
until a v3 schema revision):

```
"tier1": {"algorithm": "ED25519" | "ES256K", "sig": <base64>, "pubkey": <base64>},
"tier2": {"algorithm": "ML-DSA-65", "sig": <base64>, "pubkey": <base64>} | null
```

Both signatures are over `TIER_DOMAIN || receipt_digest` (Section 2).

## 5. Anti-Vacuity Gate — `E_VACUOUS_QUALIFICATION_REJECTED` (0xE0AC)

Fail-closed on an empty qualification domain. Concretely, certification is
refused when any of:

1. `qualification_evidence_digests` is empty — the existing law
   (`certify_from_evidence` refuses empty evidence bytes as `MissingEvidence`,
   architecture.rs:106-109 / :350; `verify_bytes` refuses empty bytes,
   :121-124);
2. `artifact_digests` is empty while standing = QUALIFIED (an artifact-free
   qualification is vacuous);
3. the evidence set is non-empty but every member fails
   `Evidence::verify_bytes` — an evidence set that cannot falsify anything
   carries no bits and is refused, not ignored.

Gate id: `E_VACUOUS_QUALIFICATION_REJECTED`, code 0xE0AC, shared with the
ARW/1 error table (wire-protocol.md Section 6). Fail-closed: the gate has no
override, no waiver path; an unparseable evidence set is treated as empty.

## 6. Verification Algorithm (sub-millisecond)

```
verify_receipt(receipt, envelope):
  1. parse JSON with deny_unknown_fields                     -- constant
  2. schema == ARCHITECTURE_RECEIPT_SCHEMA                    -- string cmp
  3. recompute receipt_digest: clear field, JCS, BLAKE3-32    -- 1 hash
     mismatch -> ReplayMismatch (architecture.rs:386 verify_integrity)
  4. validate digest fields are `algorithm:value` well-formed (:243)
  5. tier check: if envelope.tier1 present, Ed25519 verify over
     T1_DOMAIN || receipt_digest          -- ~50-100 us (ed25519_dalek)
     if envelope.tier2 present, ML-DSA-65 verify over
     T2_DOMAIN || receipt_digest          -- RustCrypto ml-dsa verify
  6. if prior chain claimed: verify_chain(prior)             -- 1 hash + field cmps
```

Steps 1-4 + 6 are pure hashing and comparison — microseconds. The dominant
cost is at most two signature verifications (Ed25519 + ML-DSA-65), each
sub-millisecond, so full two-tier verification stays under ~1 ms — the
sub-ms budget holds for the Tier-1-only path and near-1 ms for dual-tier.
No network, no allocation beyond the canonical JSON buffer; the same
real-KAT discipline as `crypto_trust_kat.rs` gates any provider swap.

## 7. Tests and Falsifiers

- Anti-vacuity: certifying with zero evidence must yield the typed gate
  refusal, and the existing corpus already holds the positive cases
  (`tests/architecture_qualification.rs`: duplicate evidence idempotent :80,
  evidence stripped after certification refused :230, empty subject is
  missing-not-malformed :162).
- Poset: any standing decrease for a fixed subject key must refuse
  E_STANDING_REGRESSION; `supersede` is the only QUALIFIED exit.
- Mutation law: reverting the gate must make a vacuous certification pass —
  if it still fails, the gate is vacuous itself.
- Cross-runtime: KAT registry vectors must keep passing
  (15/15 Rust, 27/27 python non-skipped) after any tier change.
