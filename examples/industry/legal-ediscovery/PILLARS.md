# PILLARS — legal-ediscovery pillar→gate mapping

Pack: `ggen-marketplace/packs/industry-legal-ediscovery` — pillars encoded as
ASK-shaped SPARQL gates in `gates/`, each with pass+fail witnesses under
`witnesses/{pass,fail}/` (fail witness breaks exactly one law; the court is
`gate-court.toml` → `runners/semantic_runner.py`, the dflss-exemplar runner
ported into this pack).

| Pillar | Gate | Law (refusal condition) | Ontology terms | smalt face |
|---|---|---|---|---|
| P1 sovereignty / matter isolation | `gates/040_matter_isolation.rq` | Every EvidenceDocument's custody path must reference a MatterStore whose `journalPath` matches `^/data/smalt/journal/matter_[^/]+/`; a cross-matter path is a refusal. | `lex:MatterStore`, `lex:journalPath`, `lex:custodyStoredIn` | smalt append-only journal layout; custody ETag-CAS in Cosmos (`lib/smalt/data_plane/cosmos.ex:133`) |
| P2 BYOK/SKR | `gates/050_skr_attestation.rq` | A SealedRecord must carry `prov:wasGeneratedBy` an activity bound to a valid attestation measurement fingerprint (AZ-2 SKR shape: token `measurement_digest` == expected fingerprint). | `lex:SealedRecord`, `lex:attestationFingerprint` | `lib/smalt/attestation/maa.ex` `secure_key_release/3` (AZ-2, BK-128 / KD-168) |
| P3 no-model-training / proposer-judge | `gates/060_proposer_judge.rq` | Every AI-derived annotation must carry `prov:wasGeneratedBy` a `lex:JudgeActivity` (human- or sieve-adjudicated); unadjudicated AI proposals refuse. | `lex:AIAnnotation`, `lex:JudgeActivity` | smalt proposer-judge law (`docs/jira/v26.10.22`, BK-134 pillar audit — spec file pending; worked from operator pillar spec) |
| P4 Entra/ACP walls | (pre-existing) `gates/030_ethical_wall.rq` | A custodian walled away from a matter (`lex:conflictedMatter`) must not hold evidence in it. | `lex:EthicalWall`, `lex:conflictedMatter` | — |
| P5 retention | `gates/070_retention_policy.rq` | Every matter MUST have a retention/deletion policy individual (`lex:retentionPolicy`). | `lex:retentionPolicy` | smalt WORM hold + ledger disposition chain |

Two of the five pillars (matter isolation, SKR) encode smalt storage-law
surfaces; the third (proposer-judge) encodes the operator's AI-governance law;
the fourth (Entra/ACP walls) was already encoded as the ethical-wall gate and
is retained as the P4 wall analog; the fifth (retention) encodes the
deletion-policy law. Merkle self-authentication rides the existing custody
chain: `lex:kvSealThumbprint` (KV PS256 seal, `lib/smalt/data_plane/kv_pss.ex`,
KD-105/KD-119) over ETag-CAS'd custody records — the seal chain is the Merkle
self-authentication surface and is cited but not a new gate in this lane.

## Witness court summary (2026-10-22)

All 7 gates × pass+fail = 14 legs green via
`runners/semantic_runner.py` (exit 0 observed, exit 2 on expectation
violation, exit 3 structural). Fail witnesses break exactly one law each.

## Consumer negative controls (union graph, rdflib)

- as-is consumer graph: no gate fires.
- injected cross-matter journal path → 040 fires (alongside 010/020 because
  the injected doc also lacks bates/custodian — disclosed join, not leakage).
- unattested SealedRecord → 050 fires alone.
- AIAnnotation with no JudgeActivity provenance → 060 fires alone.
- Matter without retention policy → 070 fires alone.

## Validation receipts

See pack `RECEIPT.md` section 6 (appended) for the full execution transcript:
witness court 14/14, marketplace check + validate, isolated court-structure
check (7 cases ALIVE), consumer sync ×2 byte-identical.
