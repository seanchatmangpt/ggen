# PILLARS.md — five-pillar invariant mapping, industry-pharma-21cfr

Consumer example `/Users/sac/ggen/examples/industry/pharma-21cfr/` for pack
`/Users/sac/ggen-marketplace/packs/industry-pharma-21cfr/` (v26.10.0, IND-3
pillar extension). Maps the operator's five-pillar invariants onto the pack's
witnessed refusal gates. Gate text and witnesses live in the pack; this file is
the consumer-facing mapping. Grants no execution authority.

## Pillar-to-gate map

| pillar | law | gate (pack `gates/`) | pass/fail witness stem | p11: faces |
|---|---|---|---|---|
| P1 matter/data isolation | every BatchRecord's audit trail rides an isolated per-batch storage scope (per-batch journal path law) | `050_journal_scope_missing.rq` | `050_journal_scope_missing` | `IsolatedStorageScope`, `hasJournalScope`, `journalPath` |
| P2 BYOK/SKR attestation | every e-signature activity binds a `sha256:` attestation measurement fingerprint over key material (AZ-2 SKR shape) | `060_esig_attestation_missing.rq` | `060_esig_attestation_missing` | `attestationFingerprint` |
| P3 no-model-training proposer-judge | AI-derived quality annotations are proposals; JudgeActivity adjudication with a verdict precedes any BatchRecord mutation | `070_ai_annotation_unadjudicated.rq` | `070_ai_annotation_unadjudicated` | `QualityAnnotation`, `aiDerived`, `proposesMutationOf`, `adjudicatedBy`, `JudgeActivity`, `verdict` |
| P4 Entra/ACP identity | identity is a first-class prov:Agent with attribution on every record and judge activity | no new gate — pre-existing `010_signature_missing.rq` (signature bound to `prov:agent`) + `wasAttributedTo` already witness both ways | `010_signature_missing` | `prov:Agent`, `wasAttributedTo`, `eSignature` |
| P5 Merkle self-authentication | every completed BatchRecord references a `MerkleSeal` carrying `sealedRoot` (`sha256:`) and a 320-byte `receiptDigest` | `080_merkle_seal_missing.rq` | `080_merkle_seal_missing` | `MerkleSeal`, `hasMerkleSeal`, `sealedRoot`, `receiptDigest` |

## Court method

- **Exactly-one-law**: each fail witness flips its OWN gate and is proven zero-row
  on the other seven (`qualification/verify.py` + `runners/semantic_runner.py`,
  the dflss-exemplar runner contract: exit 0 observed / 2 REFUSED / 3 structural).
- Non-vacuity was probed live: swapped expectation → `REFUSED_GATE_ROWS_ON_PASS`
  (exit 2); cross-stem pairing → `REFUSED_EXPECTED_GATE_DID_NOT_FIRE` (exit 2);
  a foreign gate path → `REFUSED_STRUCTURAL` (exit 3).
- Pack graph itself is gate-clean: the ontology carries a fully compliant
  individual chain (batch-101: signed, attested, trailed, scoped, adjudicated
  proposal chain, completed + Merkle-sealed).

## Consumer replay

```bash
cp /Users/sac/ggen-marketplace/packs/industry-pharma-21cfr/ontology.ttl pack-vocabulary.ttl
cd /Users/sac/ggen/examples/industry/pharma-21cfr && ggen sync run   # twice
# two runs byte-identical (verified 2026-10-22); generated SHACL now 126 triples
```

Pillar extension receipt: `/Users/sac/ggen-marketplace/packs/industry-pharma-21cfr/RECEIPT.md`
