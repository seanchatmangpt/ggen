# 2. Architecture as Executable Knowledge

Executable architecture does not mean that a diagram directly deploys infrastructure. It means that architectural knowledge can participate lawfully in computation.

An ontology may play at least six roles:

1. **Descriptive** - represent what exists.
2. **Normative** - represent what must or must not be true.
3. **Generative** - manufacture artifacts.
4. **Analytical** - support impact, capacity, cost, and risk calculations.
5. **Evidentiary** - connect claims to observations and receipts.
6. **Evolutionary** - describe migration, deprecation, replacement, and retirement.

A service dependency, for example, is not merely a line on a diagram. It may entail API contracts, deployment ordering, trace edges, failure propagation, incident routing, resilience tests, cost allocation, and migration constraints.

The ontology does not execute those consequences directly. It is transformed through bounded operators:

```text
admitted knowledge
-> query or derivation
-> candidate consequence
-> validation
-> authorization
-> artifact or intent
-> observation
-> receipt
```

This sequence protects the difference between knowledge and power. A fact may imply that a deployment should move, but the fact does not possess authority to move it. A pack may manufacture a repair intent, but the intent must still cross an admitted execution boundary.

The result is a more useful architecture repository. Instead of serving primarily as a record of diagrams and standards, the repository becomes a semantic control plane for manufacturing.

The word **canonical** must be used carefully. The enterprise graph is canonical for architectural meaning, not omniscient reality. Runtime observations remain external evidence. Human decisions remain decisions. Local systems may contain facts that have not yet been admitted. The graph represents the bounded state on which the enterprise is currently willing to act.

This supports a core equation:

$$
A = \mu(O^*)
$$

where $O^*$ is admitted observation, $\mu$ is a lawful manufacturing operator, and $A$ is an artifact with standing.

The equation prohibits two common errors. First, the manufacturer may not silently use unadmitted context. Second, the artifact may not claim more than the operator and admitted knowledge support.

Executable architecture is therefore not maximal automation. It is maximal lawful reuse of admitted meaning.
