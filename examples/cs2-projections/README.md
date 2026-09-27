# CS2 projections

Deterministic multi-projection witness for `RFC-CS2-001`.

```
canonical.ttl
  -> queries/consumers.rq
  -> templates/consumers.json.tera -> generated/consumers.json
  -> templates/consumers.exs.tera  -> generated/consumers.exs
  -> semantic-jira.rq
  -> templates/semantic-jira.json.tera -> generated/semantic-jira.json
```

All outputs are projections of the same exact subject and authority ceiling.
Generated output is not truth, authority, DO, admission, or standing.

The semantic Jira projection is schema-bound by
`schema/semantic-jira.schema.json` (issue contract) and
`schema/semantic-jira-batch.schema.json` (generated batch contract). The
generated batch carries its canonical schema URI and preserves the CS2 work ID,
consumer IRI, campaign, and exact subject used by downstream Jira adapters.

## Manufacture

From the repository root:

```bash
cargo run --quiet -p ggen-cli -- sync \
  --manifest examples/cs2-projections/ggen.toml
```

If `ggen` is already installed, the equivalent is:

```bash
ggen sync --manifest examples/cs2-projections/ggen.toml
```

## Qualification court

```bash
python3 examples/cs2-projections/tests/verify.py
```

The court requires:

1. JSON and Elixir projections are both manufactured.
2. every row binds the exact `RFC-CS2-001` subject.
3. every row preserves the `CONSTRUCT` authority ceiling.
4. projection rows remain deterministically ordered and unique.
5. a second sync is byte-identical for both artifacts.
6. the divergent-subject fixture cannot mint a canonical projection.
7. JSON and Elixir contain the same source semantic values.

The semantic Jira code surface additionally projects the same canonical TTL
through `semantic-jira.rq` into a schema-linked batch consumed by the
repository's deterministic Python Jira adapter.

The successful court prints a content-addressed JSON receipt using SHA-256 over
the canonical Turtle input and both generated artifacts.
