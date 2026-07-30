# 8. Technology Architecture, Capacity, and the Ontology Cliff

Technology Architecture defines the platforms, runtimes, protocols, deployment patterns, security boundaries, and operating envelopes required by the target system.

The ontology-import performance cliff is a useful example because it shows how a local defect can reveal an architectural omission.

A loader that slowed sharply around a certain number of ontology documents appeared to be an implementation problem. The immediate cause was repeated graph-store work. Batching the documents reduced the cost. But the deeper issue was that the ecosystem had no capacity model for ontology composition.

A complete Technology Architecture must model workload as more than file count:

$$
W = (D, Q, B, I, R, S, T, E)
$$

where:

- $D$ is document count;
- $Q$ is quad count;
- $B$ is blank-node density;
- $I$ is import depth;
- $R$ is rule complexity;
- $S$ is validation complexity;
- $T$ is projection volume;
- $E$ is execution environment.

The architecture must measure cold and warm latency, memory, throughput, materialization time, validation time, projection time, p95 and p99 behavior, growth slope, and first budget breach.

This produces an evidence-based capacity envelope rather than a superstition such as "do not load more than 36 files."

Technology standards should also declare operating profiles. A developer workstation, CI runner, edge device, WASM runtime, and enterprise service may admit different ontology and projection profiles. The same semantic architecture can have multiple technical realizations.

The capacity model becomes a planning input. A proposed ontology import can be evaluated before startup degrades. A new rule pack can be assessed for materialization cost. A distribution pack can publish minimum and recommended envelopes.

This is an example of architecture change management working before failure. Performance evidence updates the technology landscape, the repository stores the observed envelope, planning uses it, and runtime warnings explain the dominant cost.

Capacity is therefore not a benchmark appendix. It is an architectural constraint with lifecycle, ownership, and standing.
