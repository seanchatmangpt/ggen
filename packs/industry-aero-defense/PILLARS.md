# PILLARS — BK-143r aero-defense P3 proposer-judge lane (v26.10.22)

Pillar cell ledger for `industry-aero-defense`. Gates 010-060 already
carried the P1/P2/P5-class laws. This lane closed the last open pillar
cell — P3 proposer-judge — with gate 070, its pass/fail witnesses, and
the new proposer-judge vocabulary.

## Pillar → gate mapping

| Pillar | Law | Gate | Pass witness | Fail witness | Standing |
|---|---|---|---|---|---|
| P1 classification completeness | artifact carries classification, caveat, need-to-know, store | `gates/010_artifact_classification_complete.rq` | `witnesses/pass/010_...ttl` (0 rows) | `witnesses/fail/010_...ttl` (only G1 fires) | ALIVE |
| P1b cross-level flow | no artifact flows below a principal's clearance | `gates/020_cross_level_flow.rq` | `witnesses/pass/020_...ttl` (0 rows) | `witnesses/fail/020_...ttl` (only G2 fires) | ALIVE |
| P2 execution attestation | artifact is prov:wasGeneratedBy an attested activity | `gates/050_execution_attestation.rq` | `witnesses/pass/050_...ttl` (0 rows) | `witnesses/fail/050_...ttl` (only G5 fires) | ALIVE |
| P3 proposer-judge | every ad:AIContribution proposing a mutation is ad:adjudicatedBy an ad:JudgeActivity before commit | `gates/070_proposer_judge.rq` | `witnesses/pass/070_proposer_judge.ttl` (0 rows) | `witnesses/fail/070_proposer_judge.ttl` (only G7 fires) | ALIVE |
| P4 Entra/ACP identity walls | NOT COVERED by this lane (identity-wall gates live in the Entra/ACP surface lane, not the pack) | — | — | — | NOT-COVERED |
| P5 Merkle self-authentication | sealed artifact carries ad:hasSeal to an ad:Seal with sha256 sealedRoot | `gates/060_merkle_seal_missing.rq` | `witnesses/pass/060_...ttl` (0 rows) | `witnesses/fail/060_...ttl` (only G6 fires) | ALIVE |

P4 is declared NOT-COVERED, not silently omitted.

## What changed (BK-143r)

- `ontology.ttl` — new P3 vocabulary: `ad:AIContribution` and
  `ad:JudgeActivity` (prov:Entity / prov:Activity subclasses),
  `ad:proposes`, `ad:adjudicatedBy`, domains/ranges pinned. Extends the
  pack's execution-proof surface with the proposer-judge terms (mirror
  of IND-2's gate 060 pattern, `lex:JudgeActivity`).
- `gates/070_proposer_judge.rq` — new ASK refusal gate: an
  `ad:AIContribution` with `ad:proposes` and no adjudication by an
  `ad:JudgeActivity` refuses.
- `witnesses/pass|fail/070_proposer_judge.ttl` — pass: adjudicated
  contribution over a fully-lawful classified scene (0 rows under all
  gates); fail: adjudication by a plain prov:Activity (only G7 fires;
  the proposed artifact is otherwise complete, so gates 010-060 stay
  silent).
- `templates/pillar-gates.md.tmpl` — SELECT-only pillar-manifest
  projection of the P3 law.
- `examples/industry/aero-defense/consumer` — path-pins the pack;
  ontology is pillar-lawful and silent under all gates; `ggen sync run`
  is byte-identical across runs.

## Court receipt

`python3 qualification/verify.py` — 14/14 cells PASS, standing ALIVE,
exit 0. The runner's strict contract fires REFUSED on any fail witness
that trips more than its own gate or any pass witness with rows.

## Falsifiers

- gate 070 ASK-false on the fail witness — court must REFUSE.
- gate 070 ASK-true on the pass witness — court must REFUSE.
- any of gates 010-060 firing on the 070 fail witness — exactly-one-law
  violated; court must REFUSE.
