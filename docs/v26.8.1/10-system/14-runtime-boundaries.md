# Runtime boundaries

The runtime architecture separates pure observation and construction from bounded mutation and external actuation.

## Internal runtime

- load admitted project, pack, ontology, template, and policy inputs;
- evaluate graph law and validation;
- extract deterministic bindings;
- render artifacts in memory;
- plan filesystem consequences;
- emit receipts, telemetry, and OCEL evidence.

## Mutation boundary

Filesystem mutation is admitted only through declared write semantics with path safety, ownership checks, dry-run equivalence, and result receipts. Lockfile, cache, key, receipt, and generated-output mutation must be independently identifiable.

## External boundary

Cloud, network, deployment, release, infrastructure, and other external changes remain inert intents until BRCE admits and actuates them. Local pack acquisition that crosses a network boundary must still emit real provenance and integrity evidence.

## Verification

The integrated verifier must identify every subprocess, filesystem, state, network, telemetry, and protocol boundary actually crossed. A source-level call graph alone is insufficient.
