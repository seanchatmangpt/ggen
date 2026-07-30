# 16. BRCE, wasm4pm, and cargo-cicd

BRCE, wasm4pm, and cargo-cicd govern the final movement from constructed possibility to operating consequence.

## BRCE

BRCE enforces the invariant of zero unreceipted actuation. It is the only DO boundary. An intent must identify the requested capability, scope, target, preconditions, policy, and expected evidence. The broker admits or refuses the request and emits an execution receipt after observation.

## wasm4pm

wasm4pm treats process behavior as evidence. It can consume events, evaluate conformance, identify deviations, and support replay across portable execution boundaries. OCEL provides an object-centric event representation that can connect one event to multiple business objects.

## cargo-cicd

cargo-cicd turns build and release into governed law. Release is not an afterthought to generated product code. CI matrices, package metadata, signing, provenance, SBOMs, archive construction, promotion gates, and rollback procedures are part of the product lifecycle.

Together they close the architecture loop:

```text
manufactured artifact or intent
-> release and promotion gate
-> execution authorization
-> observed process
-> consequence receipt
-> architecture change input
```

The architecture repository should link every deployed product version to:

- source architecture state;
- pack and ontology closure;
- build and test evidence;
- release policy;
- execution grants;
- runtime observations;
- incidents;
- deprecation state.

This creates a traversable evidence lineage from strategy to operation.

The major refusal boundary is retrospective fabrication. A release receipt cannot be inferred from a successful deployment after the fact if the required evidence was not captured. A process-conformance claim cannot be reconstructed from missing events. An architecture state should remain UNKNOWN rather than receive invented standing.
