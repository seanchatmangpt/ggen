# 12. Graphlaw and star-toml: Admission Before Consequence

Graphlaw and star-toml protect the boundary between available information and admitted state.

star-toml carries bounded configuration. It makes configuration parseable, typed, and receiptable. It should preserve source identity, schema identity, validation outcome, and the exact value set that downstream computation consumed.

Graphlaw provides semantic admission and derivation. It can evaluate constraints, materialize rules, inspect denials, and produce derived facts. Its authority is epistemic: it determines what follows from the admitted graph under declared law.

The pair supports a disciplined sequence:

```text
raw configuration
-> typed configuration
-> admitted configuration
-> semantic graph
-> constraints and derivation
-> admitted consequence
```

Several distinctions must remain explicit.

- Parsing is not validation.
- Validation is not derivation.
- Derivation is not authorization.
- Authorization is not actuation.
- Successful actuation is not proof of correctness.

Graphlaw should produce typed refusals when constraints fail. A failure must name the document, rule, shape, focus node, and remediation where possible. Silent omission is especially dangerous because it changes the architecture while appearing successful.

The architecture repository should record which ontology and rule versions produced each derived fact. This allows impact analysis when a rule changes and prevents an inferred fact from becoming an unexplained permanent assertion.

The same architecture graph may contain descriptive facts, normative constraints, derivation rules, and evidence links. Their types must remain visible. Otherwise, the system cannot distinguish what was observed from what was inferred or required.

The strategic value of Graphlaw and star-toml is not only correctness. They make semantic authority portable. A pack can declare the configuration and graph conditions it requires, allowing a consumer to admit or refuse the pack under its own sovereignty.
