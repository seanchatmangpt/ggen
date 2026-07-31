# Process intelligence boundary

ggen emits process evidence. It does not own process discovery, conformance, fitness, precision, or variant analysis.

## ggen-owned surfaces

- OCEL event emission from real generation and pack operations;
- lifecycle ordering needed to emit valid evidence;
- OTel spans and metrics from actual runtime execution;
- receipt bindings linking source, operation, output, and evidence.

## Externally owned analysis

- DFG and process-model discovery;
- conformance, fitness, and precision;
- process variants and mining algorithms;
- analytical interpretation of emitted OCEL.

These belong to `wasm4pm-compat` and `wasm4pm`.

## v26.8.1 verifier

A structural guard must fail when analysis symbols or forbidden dependencies enter production ggen code. A real integration fixture must also prove that emitted evidence is consumable externally without importing analysis implementation into ggen.

## Legacy disposition

Any historical local analysis implementation is post-split residue. It may be removed only after its external replacement, compatibility format, and migration path are demonstrated.
