# 10. The Chatman Ecosystem

The Chatman Ecosystem is a set of cooperating architecture capabilities rather than one monolithic platform.

```text
Observation and configuration: star-toml
Semantic law and admission: Graphlaw
Manufacturing: ggen
Candidate construction: CMD
Planning and workflows: MFW, POWL, PDDL
Optimization and allocation: BCINR, CMCA
Formal assurance: mfact, procint, Lean
Actuation boundary: BRCE
Process evidence: wasm4pm
Release governance: cargo-cicd
Evidence: receipts, PROV-O, OCEL
```

The separation is constitutional.

Graphlaw may infer and validate semantic consequences, but it should not mutate the filesystem. ggen may manufacture artifacts and intents, but it should not receive ambient execution authority. A planner may determine a valid sequence, but a plan is not an execution grant. Lean may prove a mathematical model, but the theorem does not automatically prove the deployed binary corresponds to it. wasm4pm may observe process behavior, but observation does not retroactively authorize an action.

This division creates a lawful pipeline:

```text
O -> O* -> K -> C -> P -> A -> G -> X -> E
```

where observation becomes admitted knowledge, candidates, plans, artifacts, grants, executions, and evidence through explicit boundaries.

The ecosystem should be modeled in the enterprise architecture graph as capabilities realized by products and components. Repositories are implementation locations, not the capabilities themselves.

For every ecosystem component, the architecture should record:

- capability realized;
- inputs and outputs;
- authority;
- dependencies;
- consumers;
- performance envelope;
- evidence model;
- lifecycle state;
- replacement constraints;
- target roadmap.

This removes a recurring ambiguity in fast-moving research systems. A repository can be ALIVE while the broader capability remains PARTIAL_ALIVE. A theorem rail can be ALIVE while implementation correspondence remains UNKNOWN. A prototype can demonstrate a mechanism without possessing production standing.

The ecosystem is strongest when every component preserves those distinctions.
