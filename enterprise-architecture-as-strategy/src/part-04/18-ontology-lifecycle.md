# 18. The Ontology Lifecycle

An ontology file is not an ontology lifecycle.

The lifecycle requires explicit states:

```text
discovered
-> acquired
-> identified
-> qualified
-> admitted
-> composed
-> materialized
-> projected
-> authorized
-> active
-> deprecated
-> retired
-> archived
```

Each state has distinct evidence.

Discovery records a candidate source. Acquisition records exact bytes. Identification establishes canonical IRI, version, hash, source, and aliases. Qualification checks syntax, metadata, license, ownership, and minimum quality. Admission accepts the ontology into an observation boundary. Composition resolves dependencies, alignments, and conflicts. Materialization evaluates rules. Projection manufactures consequences. Authorization permits use in an operating context. Active status records real consumers. Deprecation names a successor and migration. Retirement refuses new use. Archival preserves replay.

The descriptor should include identity, authority, provenance, semantics, composition, policy, quality, capacity, and lifecycle.

Capacity is part of ontology architecture. A version should publish representative document count, triples, rule complexity, expected memory, latency, and supported profiles.

Compatibility requires semantic diff, not only content hash. The system should identify:

- added and removed terms;
- changed domains and ranges;
- changed cardinalities;
- shape changes;
- rule-consequence changes;
- query impact;
- template impact;
- generated-artifact impact.

Version recommendation follows from observed compatibility, not arbitrary convention.

Retirement is essential. Without retirement, semantic systems accumulate every historical ontology into startup, reasoning, validation, and human comprehension. The performance cliff becomes an architectural inevitability.

The ontology lifecycle therefore answers not only "can this file load?" but "why is this semantic asset present, who owns it, who consumes it, what does it cost, what replaces it, and when may it leave?"
