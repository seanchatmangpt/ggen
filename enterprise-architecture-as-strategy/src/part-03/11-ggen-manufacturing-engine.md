# 11. ggen: The Manufacturing Engine

ggen transforms admitted semantic knowledge into deterministic artifact families.

Its core pipeline is:

```text
resolve
-> load
-> enrich
-> extract
-> render
-> write
-> verify
-> receipt
```

SPARQL provides typed selection and derivation over the graph. Tera provides finite textual projection. The filesystem projection determines paths, content, metadata, intent descriptions, and validation obligations.

The decisive design claim is that Tera is not limited to code generation. Any finite textual infrastructure representation can be projected:

- source code;
- APIs;
- schemas;
- infrastructure;
- policies;
- tests;
- proof obligations;
- workflows;
- documentation;
- diagrams;
- observability;
- migration plans;
- receipt schemas.

Indirectly, those textual artifacts can be consumed by native tools to produce binaries, containers, indexes, databases, or PDFs.

ggen must maintain three fences.

First, the graph is authority; generated files are projections. Hand-editing generated output does not change the source law.

Second, construction is distinct from actuation. ggen may create a deployment manifest or execution intent, but another admitted tool performs the consequence.

Third, receipts follow observation. A template cannot truthfully predict the final digest of an artifact that has not yet been materialized and measured.

The manufacturing engine should expose architecture-aware planning before writes. Given a project, it should report:

- ontology closure;
- pack closure;
- output ownership;
- conflicts;
- projected artifact family;
- required validators;
- predicted capacity;
- affected consumers;
- lifecycle implications.

This is the role of `ggen architecture plan` and `ggen architecture doctor`.

ggen becomes strategically significant when it eliminates independent semantic maintenance. The metric is not generated line count. It is the number of downstream representations that no longer need to rediscover the same architectural fact.
