# PILLARS — industry-aero-defense (IND-4)

5-pillar spec mapping: gate -> pillar -> pack surface -> consumer surface.
Receipt: court green (see Court receipt below), 2026-10-22.

## Mapping table

| Pillar | Pack gate | Law | Consumer surface (ggen examples/industry/aero-defense) |
|---|---|---|---|
| P1 classification isolation (storage segregation) | `040_storage_segregation` | artifacts of different classifications MUST sit in distinct `ad:Store` individuals | consumer `data.ttl` facts project via `rendered-shacl.ttl.tera`; the segregation law admits only single-level stores |
| P2 attestation | `050_execution_attestation` | every `ad:VerifiableExecutionProof` MUST bind the air-gap attestor's `ad:measurementFingerprint` (AZ-2 shape; smalt `lib/smalt/attestation/maa.ex` secure_key_release/3) | proofs admitted only with bound measurement |
| P5 Merkle self-auth | `060_merkle_seal_missing` | every `ad:ClassifiedArtifact` MUST reference an `ad:Seal` with sha256 `ad:sealedRoot` | artifacts project the seal law into `generated/rendered-shacl.ttl` |
| (P3) | — | no AI-proposer surface in IND-4 scope (no AI-derived facts in this pack) | — |
| (P4 identity walls) | — | not in this lane's IND-4 scope (identity walls live in the egress/identity pack family) | — |

## Court receipt

```
$ python3 qualification/verify.py   # packs/industry-aero-defense
standing=ALIVE, cells=12 (6 gates x pass/fail), exit=0
$ python3 scripts/marketplace.py check industry-aero-defense
check ok packs=1 turtle=ok qualify=ok   [exit=0]
```

Witnesses: exactly-one-law fail witnesses (runner refuses
`REFUSED_MORE_THAN_ONE_GATE_FIRED`); pass witnesses silent under all 6
gates. Existing 010/020 witnesses updated to carry `ad:hasSeal` (P5) so the
new seal law stays silent on their pass/fail cells.