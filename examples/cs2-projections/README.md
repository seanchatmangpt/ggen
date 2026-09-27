# CS2 projections

Deterministic value projections and reusable schema manufacture for `RFC-CS2-001`.

```
canonical.ttl -----------------> queries/consumers.rq
      |                                  |
      |                                  +--> generated/consumers.json
      |                                  +--> generated/consumers.exs
      |
      +-------------------------> semantic-jira.rq
      |                                  |
      |                                  +--> generated/semantic-jira.json
      |
projection-spec.ttl
      |
      +--> packs/multi-projection-pack/queries/fields.rq
              |
              +--> Rust / Python / TypeScript
              +--> SQL / Protobuf / GraphQL
              +--> JSON Schema / JSON-LD context / manifest
```

All generated values remain projections of the same exact subject and authority
ceiling. Generated output is not truth, authority, DO, admission, or standing.

The semantic Jira projection is schema-bound by
`schema/semantic-jira.schema.json` (issue contract) and
`schema/semantic-jira-batch.schema.json` (generated batch contract). The
generated batch carries its canonical schema URI and preserves the CS2 work ID,
consumer IRI, campaign, and exact subject used by downstream Jira adapters.

The generic multi-projection pack owns repeated target-schema manufacture.
`projection-spec.ttl` is the CS2-specific semantic map; Rust, Python,
TypeScript, SQL, Protocol Buffers, GraphQL, JSON Schema, JSON-LD context, and
the projection manifest are generated from that single field map rather than
maintained as parallel handwritten schemas.

## Manufacture

From the repository root:

```bash
cargo run --quiet -p ggen-cli -- sync \
  --manifest examples/cs2-projections/ggen.toml
```

If `ggen` is already installed:

```bash
ggen sync --manifest examples/cs2-projections/ggen.toml
```

## Qualification courts

```bash
python3 examples/cs2-projections/tests/verify.py
python3 packs/multi-projection-pack/tests/verify_pack.py
```

The CS2 court checks exact-subject value projection and replay. The generic pack
court checks complete target manufacture plus byte-identical second generation.
These courts are authored separately from manufacture and may be executed by a
verification lane.
