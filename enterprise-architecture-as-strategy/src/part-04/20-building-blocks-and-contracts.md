# 20. Architecture Building Blocks, Solution Building Blocks, and Contracts

Architecture Building Blocks define required capabilities and constraints. Solution Building Blocks realize them.

Examples:

| Architecture Building Block | Possible Solution Building Blocks |
|---|---|
| Semantic admission | Graphlaw, SHACL engine, policy service |
| Bounded actuation | BRCE broker, capability grants, adapters |
| Formal assurance | Lean models, theorem packages, correspondence tests |
| Process evidence | wasm4pm, OCEL event adapters, conformance engine |
| Artifact projection | ggen engine, packs, templates, validators |

The distinction supports substitution. If the organization replaces Oxigraph, the ontology-admission capability should remain conceptually stable. If a new planner replaces an old planner, transition-planning requirements should remain testable.

An architecture contract binds an Architecture Building Block to an implementation initiative. It declares:

- required behavior;
- quality attributes;
- interfaces;
- authority;
- standards;
- evidence;
- capacity;
- lifecycle;
- exceptions;
- acceptance criteria.

Contracts should be represented as graphs and projected into human-readable review packets, CI gates, test suites, and receipts.

The contract must include negative space. It should state what the solution may not do, which claims remain outside scope, and which escape hatches are quarantined.

This is especially important for external packs. Their contract must be sufficient for independent admission by a consumer. No consumer should need private repository knowledge to understand the pack's meaning, authority, dependencies, or outputs.

Architecture contracts convert governance from interpretation into executable obligation while preserving the need for judgment where policy permits alternatives.
