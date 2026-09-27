# Multi-projection pack

A reusable RDF-first manufacturing pack. One `mp:ProjectionSpec` and ordered field relation drives JSON, JSON-LD, Rust, Python, TypeScript, SQL, Protocol Buffers, and GraphQL projections.

The semantic source owns subject identity, authority ceiling, field order, source predicates, target types, and requiredness. Templates are projections only; generated files are not authority or standing.

## Adapt a domain

1. Import or reuse the `mp:` vocabulary.
2. Declare one `mp:ProjectionSpec`.
3. Bind ordered `mp:field` nodes.
4. Map target-language type strings in the domain RDF.
5. Reuse or override only the irreducible target template.

CS2 is a consumer/fixture candidate rather than the pack's ontology.
