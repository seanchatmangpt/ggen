# Multi-Projection Pack

One RDF `mp:ProjectionSpec` manufactures a family of implementation/schema
surfaces without repeating field semantics in each language.

```
ProjectionSpec + ProjectionField
          |
          v
   queries/fields.rq
          |
          +--> Rust struct
          +--> Python dataclass
          +--> TypeScript interface
          +--> SQL table
          +--> Protocol Buffers message
          +--> GraphQL type
          +--> JSON Schema
          +--> JSON-LD context
          +--> projection manifest
```

## Contract

The semantic source owns field identity, order, documentation, requiredness,
and each target's explicit type spelling. The pack does not infer authority,
business rules, persistence ownership, or runtime dispatch.

Every generated artifact carries or is paired with the exact
`mp:ProjectionSpec` IRI. Target-language type strings are explicit semantic
inputs, which makes type-policy changes ontology diffs rather than repeated
template edits.

## Standalone example

```bash
ggen sync --manifest packs/multi-projection-pack/ggen.toml
```

The generated directory is disposable. `tests/verify_pack.py` is an authored
qualification harness for a verification lane: it checks complete projection
manufacture and byte-identical second generation.

## Consumer composition

A consumer should:

1. import `ontology.ttl` plus its domain-specific ProjectionSpec Turtle;
2. reuse `queries/fields.rq`;
3. copy or reference only the generation rules for target surfaces it owns;
4. keep generated targets overwrite-only and out of handwritten ownership.

`examples/cs2-projections` is the first concrete consumer.
