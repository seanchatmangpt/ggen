# PILLARS — IND-6 pillar-gate lane receipt (v26.10.22)

Five-pillar spec for `industry-semantic-cad-bim`: the pack already carried
G1/G2/G3 (gates 010/020/030). This lane added the four missing pillar gates,
each with a pass/fail witness that fires exactly-one-law, wired through the
pack's existing strict court (`gate-court.toml` → `qualification/verify.py` →
`runners/semantic_runner.py`, schema `ggen.semantic-gate-witness-court/1`).

## Pillar → gate mapping

| Pillar | Law | Gate | Pass witness | Fail witness | Standing |
|---|---|---|---|---|---|
| P1 design isolation (per-project store, mirrors legal matter-scoping) | every `cad:DesignArtifact` is `cad:storedIn` a `cad:ProjectStore` whose `cad:journalPath` matches `^/data/smalt/journal/project_[^/]+/` | `gates/040_design_isolation.rq` | `witnesses/pass/040_design_isolation.ttl` (0 rows) | `witnesses/fail/040_design_isolation.ttl` (1 refusal row, only G4 fires) | ALIVE |
| P2 BYOK/SKR attestation (AZ-2 SKR shape) | every `cad:DesignRevision` is `prov:wasGeneratedBy` an activity carrying `cad:attestationFingerprint` | `gates/050_attestation.rq` | `witnesses/pass/050_attestation.ttl` (0 rows) | `witnesses/fail/050_attestation.ttl` (1 refusal row, only G5 fires) | ALIVE |
| P3 proposer-judge no-training | every `cad:AIContribution` that `cad:mutatesElement` is `cad:adjudicatedBy` a `cad:JudgeActivity` before mutation | `gates/060_proposer_judge.rq` | `witnesses/pass/060_proposer_judge.ttl` (0 rows) | `witnesses/fail/060_proposer_judge.ttl` (1 refusal row, only G6 fires) | ALIVE |
| P5 Merkle self-authentication (320-byte receipt digest) | every `cad:RootRevision` is `cad:sealedRoot` a `cad:MerkleSeal` whose `cad:receiptDigest` matches `^[0-9a-f]{640}$` | `gates/070_seal.rq` | `witnesses/pass/070_seal.ttl` (0 rows) | `witnesses/fail/070_seal.ttl` (1 refusal row, only G7 fires) | ALIVE |
| P4 Entra/ACP identity walls | NOT COVERED by this lane (identity-wall gates live in the Entra/ACP surface lane, not the pack) | — | — | — | NOT-COVERED |

P4 is declared NOT-COVERED here, not silently omitted.

## What changed

- `ontology.ttl` — new terms: `cad:ProjectStore`, `cad:storedIn`,
  `cad:journalPath`, `cad:AttestedActivity`, `cad:attestationFingerprint`,
  `cad:AIContribution`, `cad:JudgeActivity`, `cad:mutatesElement`,
  `cad:adjudicatedBy`, `cad:MerkleSeal`, `cad:sealedRoot`,
  `cad:receiptDigest`; refusal-law list extended G4–G7.
- `ontology/shapes.ttl` — SHACL twins `G4`–`G7` (incl. SPARQLTarget for G6,
  digest pattern shape for G7), mirroring the SPARQL gates.
- `gates/040…070` — four new SPARQL refusal gates.
- `witnesses/pass|fail/040…070` — eight new witnesses; `020` pair updated for
  exactly-one-law cross-compliance (root now sealed + attested).
- `templates/pillar-gates.md.tmpl` — SELECT-only pillar-manifest projection
  (also satisfies the consumer-sync `.tmpl` requirement).
- `examples/industry/semantic-cad-bim/` consumer:
  `ggen.toml` path-pins the pack, `ontology.ttl` is a pillar-lawful design
  snapshot silent under all seven gates + SHACL-conformant; `ggen sync run`
  is byte-identical across runs.

## Court receipt

- `python3 qualification/verify.py` (pack root) → standing ALIVE, 14/14 cells
  (7 gates × pass+fail), exit 0.
- `python3 scripts/check_gate_witness_courts.py` qualify(industry-semantic-cad-bim)
  → standing ALIVE, case_count 7, exit 0.
- `python3 scripts/marketplace.py validate` → 339 packs / 339 manifests /
  557 ontologies / 1925 templates / 1966 native_gates, exit 0.
- Consumer sync determinism: two `ggen sync run` passes byte-identical,
  sha256 `992ec6d1f5509fcaa3b337a506305f7500fe227d57d6d71f75401020f258d55e`;
  lock pins pack content `blake3:c22c7dbebd60288be2f0870f501d14c3a405fa37846f8742a7f5e508561621a0`
  (re-lock by deleting `ggen.lock` is the intended flow after any pack edit).

## Non-vacuity (shim court discipline)

Each new gate's fail witness is an anti-vacuity probe: reverting its law (the
gate returning 0 rows on a violating graph) is exactly the
`REFUSED_EXPECTED_GATE_DID_NOT_FIRE` refusal the runner already types; the
exactly-one-law fail witnesses prove the new laws do not cross-fire on the
pre-existing G1–G3 witnesses (the `020` pair was repaired for this, not the
laws loosened).

## See Also

- [gate-court.toml](gate-court.toml) · [qualification/verify.py](qualification/verify.py) ·
  [consumer example](../../examples/industry/semantic-cad-bim/README.md)
