# ggen v26.8.1 Unified Ontology

This directory is the semantic authority for the v26.8.1 release program and the ggen-legacy sunset decision.

## Files

- `ontology.ttl` — canonical classes, properties, subsystem instances, pipeline stages, boundaries, capabilities, gates, standing vocabulary, and sunset law.
- `shapes.ttl` — fail-closed SHACL constraints for subsystem mapping, stages, gates, releases, standing, legacy disposition, and sunset predicates.
- `queries/subsystem-inventory.rq` — deterministic subsystem/source/capability projection.
- `queries/unresolved-legacy.rq` — crown query returning any legacy capability whose standing or disposition remains unknown.

## Authority law

The ontology is authority. Markdown, CSV coverage tables, verifier JSON, generated diagrams, and release notes are projections or evidence. They may not introduce a subsystem, capability, gate, standing, or disposition absent from this graph.

## Release predicate

`v26.8.1` is admitted only when all required gates pass against one exact source head and no required coverage mapping is unknown.

## Sunset predicate

`ggen-legacy` sunset is admitted only when:

1. every historical capability is represented as a `ggen:LegacyCapability`;
2. every legacy capability has exactly one disposition;
3. every legacy capability has non-UNKNOWN evidence standing;
4. receipt and replay compatibility gates pass;
5. the external crown verifier promotes the decision.

The ontology currently records the release as `PARTIAL_ALIVE` and the sunset decision as `UNKNOWN`. Those values are deliberate and may only be promoted by executable evidence.
