# PILLARS — industry-financial-ledger (IND-2)

5-pillar spec mapping: gate -> pillar -> pack surface -> consumer surface.
Receipt: both courts green (see Court receipt below), 2026-10-22.

## Mapping table

| Pillar | Pack gate | Law | Consumer surface (ggen examples/industry/financial-ledger) |
|---|---|---|---|
| P1 isolation | `040_ledger_isolation` | every `fin:JournalEntry` MUST reference an isolated `fin:LedgerStore` carrying a per-account `fin:journalPath` | consumer ledger facts bind each entry to `fin:store-5001` (per-account journal path `journals/5001/journal.ndjson`) |
| P2 BYOK/SKR | `050_skr_attestation` | settlement-stage completions MUST bind `fin:attestationFingerprint` (AZ-2 shape: token measurement_digest == expected fingerprint; smalt `lib/smalt/attestation/maa.ex` secure_key_release/3) | settlement faces `fin:Capture/Enrich/Settle/Audit`; completions are admitted only under a bound fingerprint |
| P3 proposer-judge | `060_proposer_judge` | AI-categorized entries MUST carry `prov:wasGeneratedBy` a `fin:JudgeActivity` before commit | AI-categorized entries in consumer data admit only post-adjudication |
| P5 Merkle self-auth | `070_merkle_seal_missing` | every `fin:SettledBatch` MUST reference a `fin:MerkleSeal` with sha256 `fin:sealedRoot` + `fin:receiptDigest` | settled batches project the seal law into the generated SHACL/walkthrough |
| (P4 identity walls) | — | not in this lane's IND-2 scope (no KV identity surface in this pack) | — |

## Court receipt

```
$ python3 qualification/verify.py   # packs/industry-financial-ledger
standing=ALIVE, cells=14 (7 gates x pass/fail), exit=0
$ python3 scripts/marketplace.py check industry-financial-ledger
check ok packs=1 turtle=ok qualify=ok   [exit=0]
```

Witnesses: exactly-one-law fail witnesses (runner refuses
`REFUSED_MORE_THAN_ONE_GATE_FIRED`); pass witnesses silent under all 7
gates. Consumer fixture `qualification/consumer.ttl` updated to bind
`fin:isolatedStore` (P1) and stays silent under all gates.
