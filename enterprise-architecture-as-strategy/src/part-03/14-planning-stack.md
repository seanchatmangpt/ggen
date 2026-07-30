# 14. CMD, MFW, POWL, PDDL, BCINR, and CMCA

The planning stack separates option construction from execution.

CMD, Design for Combinatorial Maximalism, expands the reversible candidate space. It asks which lawful combinations of knowledge, operators, environments, consumers, versions, and artifact surfaces should be considered.

MFW represents recursive and multifractal workflow manufacture. It allows workflows to expand locally while unresolved obligation rank descends globally. This supports plans that become structurally larger while becoming semantically closer to completion.

POWL represents partial-order workflows. It captures concurrency and dependency without forcing an arbitrary total sequence.

PDDL provides a standard planning projection. Domains encode actions, preconditions, effects, resources, and goals. Problems encode the admitted state and desired state.

BCINR and CMCA provide optimization and allocation lenses. They can rank or select candidates under bounded measures such as cost, risk, latency, capability fit, and proof burden.

The planning sequence is:

```text
architecture requirement
-> candidate construction
-> admissibility gates
-> partial order
-> optimization lens
-> selected plan
-> plan certificate
```

The selected plan remains declarative. It must be converted into bounded intents and execution grants before actuation.

Planning must also preserve refusals. If no plan exists under the current constraints, the system should expose the minimal unresolved obligations or conflicting requirements. It should not silently relax policy.

The architecture repository should record:

- planner identity and version;
- domain and problem hashes;
- eligible candidate set;
- rejected candidates and reasons;
- measure map;
- selected plan;
- assumptions;
- expected effects;
- rollback plan;
- certificate and replay data.

This makes planning an architecture service rather than an opaque algorithmic step.

The strategic benefit is that roadmaps become computable transition architectures. Dependencies, parallelism, risk, and evidence requirements can be recalculated when the architecture changes.
