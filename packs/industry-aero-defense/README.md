# industry-aero-defense

Aerospace / defense / government-intelligence pack for **air-gapped,
zero-egress, PQC-positioned** work. Lane 4/6 of the azure-industry-matrix
family (see smalt `docs/jira/v26.10.22/azure-industry-matrix.md`, defense row).

## Ontology

PROV-O (`http://www.w3.org/ns/prov#`) + a minimal classified-artifact
vocabulary (`ad:` namespace):

| Term | Grounding |
|---|---|
| `ad:ClassifiedArtifact` | subclass of `prov:Entity` |
| `ad:Principal` | subclass of `prov:Agent` (need-to-know principal) |
| `ad:ClassificationLevel` | ordered tier (`ad:levelOrder` xsd:integer) |
| `ad:caveat` | handling caveat (e.g. NOFORN) |
| `ad:AirGapBoundary` | zero-egress boundary; loopback endpoints only |
| `ad:VerifiableExecutionProof` | digest + PROV-O provenance, replayable |

## Gates (the air-gap IS the gate)

| Gate | Pillar | Law | Refusal |
|---|---|---|---|
| `010_artifact_classification_complete` | — | artifact MUST carry classification + caveat + cleared principal | row = refusal |
| `020_cross_level_flow` | — | flow target more restrictive than source | row = refusal |
| `030_nonloopback_endpoint` | — | any non-loopback `ad:endpoint` | row = refusal |
| `040_storage_segregation` | P1 | artifacts of different classifications MUST sit in distinct `ad:Store` individuals | row = refusal |
| `050_execution_attestation` | P2 | `ad:VerifiableExecutionProof` MUST bind `ad:measurementFingerprint` (AZ-2 shape) | ASK true = refusal |
| `060_merkle_seal_missing` | P5 | every artifact MUST reference an `ad:Seal` with a sha256 `ad:sealedRoot` | row = refusal |

Negative witnesses live in `witnesses/fail/`; the exact-stem court is
`qualification/verify.py`:

```bash
python3 qualification/verify.py
# {"failures": [], "gate_count": 6, "standing": "ALIVE", ...}
```

Pillar mapping receipt (gate -> pillar -> consumer): `PILLARS.md` in the
consumer (`ggen examples/industry/aero-defense`).

## Templates

- `templates/rendered-shacl.ttl.tera` — SHACL/shape projection per admitted artifact
- `templates/client-walkthrough.md.tera` — air-gapped client walkthrough:
  zero-egress sovereign console -> local Blob WORM -> local Confidential
  Ledger; network faces deliberately ABSENT.

## PQC position (typed honestly)

PQC primitives exist **off-wire only**. No ML-DSA/ML-KEM `kty` on the KV
create/sign wire face — **GAP(kv-pqc-keyface)** (smalt
`docs/jira/v26.10.22/azure-industry-matrix.md:186`; `KvRoutes.ex:28,147-207`
accepts RSA/EC/oct only; primitives at `lib/smalt/tls.ex`,
`lib/smalt/trust_profile/trust_profile.ex`, `lib/smalt/hw/frame.ex`).
This pack does not claim on-wire PQC.

## Standing

KernelPack. Grants no execution authority. All classification facts are
synthetic admitted inputs, not real program data.
