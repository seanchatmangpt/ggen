# AutoFDE Semantic Registry Pack

This pack installs the full federated semantic inventory supplied for AutoFDE into `ggen` without collapsing ontologies, provider schemas, knowledge bases, and protocols into one false type.

## Canonical surfaces

- `source-inventory.md` — admitted source snapshot; SHA-256 `8a4a75675ad9db9597767bd8086b2a5a85b1a1e7c98c0d34f90bc4b935c99d1a`.
- `ontology/schema.ttl` — registry vocabulary, authority concepts, standing model, and integration patterns.
- `ontology/00-foundation.ttl` … `ontology/15-industry.ttl` — the 16 canonical ontology modules.
- `ontology.ttl` — generated aggregate consumed by the pack loader. Never edit it directly.
- `shapes/source-registry.shacl.ttl` — executable source-record and ALIVE-claim constraints.
- `queries/source-inventory.rq` — deterministic projection query.
- `gates/build_aggregate.py` and `gates/verify_registry.py` — drift, syntax, coverage, P0, provider, standing, and receipt gates.

## Standing model

The registry records themselves are `ALIVE` when their structure and inventory coverage verify. External source materialization and generated projections remain independently `UNKNOWN` until a canonical location, version, license, retrieval timestamp, source digest, transformation result, projection digest, and validation result are bound.

This prevents a catalog entry from masquerading as a retrieved ontology or a generated provider projection.

## Replay

```bash
python3 packs/autofde-semantic-registry-pack/gates/build_aggregate.py --check
python3 packs/autofde-semantic-registry-pack/gates/verify_registry.py \
  --receipt /tmp/autofde-semantic-registry-receipt.json
```

The verifier requires Python 3 and `rdflib`. It executes the same invariants expressed by the included SHACL shapes and refuses generated drift or ungrounded `ALIVE` source claims.
