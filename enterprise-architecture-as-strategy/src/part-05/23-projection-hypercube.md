# 23. Tera and the Projection Hypercube

Tera projects finite text, not merely source files.

A projection includes:

$$
Projection =
Path + Content + Metadata + Intent + ValidationObligation
$$

Path determines repository topology. Content determines the artifact. Metadata determines ownership, version, provenance, and lifecycle. Intent describes a bounded requested action. Validation obligations define the proof burden.

This enables projections across:

- languages;
- APIs;
- data schemas;
- infrastructure as code;
- policy languages;
- CI and release systems;
- observability;
- planners;
- user interfaces;
- documentation;
- diagrams;
- receipts;
- migration assets.

Tera can also project packs and templates that project later artifacts. This creates progressive binding:

```text
domain knowledge
-> domain pack
+ organization policy
-> organization pack
+ environment state
-> deployment-specific artifacts
```

Each stage adds context while preserving earlier invariants.

Recursive generation must remain bounded. A generated pack receives no standing until it is independently admitted. Receipt chains should form a DAG from the final artifact back to each input and transformation.

The projection hypercube makes the architecture repository economically significant. One admitted fact can be reused across lifecycle phases and consumers without becoming one universal artifact.

The important refusal is speculative evidence. A template can generate an expected receipt schema, but it cannot claim an observed digest before materialization. The lawful sequence is artifact, observation, validation, receipt.

Projection is powerful precisely because it remains downstream of knowledge and upstream of evidence.
