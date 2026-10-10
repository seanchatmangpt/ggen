# Generated MFW proof-carrying-plan verifier

This consumer tree is generated from `ontology.ttl`. Delete it and run `ggen sync run` to rebuild it.

## Level-5 closure capabilities

| Order | Capability | Required evidence artifact |
|---:|---|---|
| 1 | Authoritative semantic source | `ontology.ttl` |
| 2 | Complete generation surface | `ggen.toml` |
| 3 | Deterministic regeneration | `evidence/repeat-run.json` |
| 4 | Fixed-point convergence | `evidence/erase-rebuild.json` |
| 5 | Generated verification | `templates/tests/generated_proof.rs.tmpl` |
| 6 | Generated negative witnesses | `templates/tests/generated_negative.rs.tmpl` |
| 7 | Generated documentation | `templates/README.md.tmpl` |
| 8 | Generated provenance | `templates/PROVENANCE.ttl.tmpl` |
| 9 | Generated receipts | `evidence/ggen-receipt.json` |
| 10 | Generated release surface | `templates/ci.yml.tmpl` |
| 11 | Generated evolution path | `evidence/semantic-diff.json` |
| 12 | Consumer replacement | `evidence/consumer-replacement.json` |


The presence of this generated file does not itself grant Level-5 standing. Standing requires all twelve evidence artifacts to be independently verified.
