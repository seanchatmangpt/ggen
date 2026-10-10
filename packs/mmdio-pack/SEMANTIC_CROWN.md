# mmdio Pack Semantic Crown Contract

## Separation of claims

`mer:pythonSupport true` means that the mmdio repository contains a Python grammar/parser surface for a Mermaid diagram type. It does **not** mean that the type has executed a canonical semantic roundtrip.

A `mer:SemanticCrown` is stronger and must bind an exact external source subject:

```text
profile identity
+ repository
+ commit
+ source path
+ Git blob
+ claim ceiling
+ non-actuation authority
```

## Current crown

Only this bounded profile is crowned by the present contract:

```text
profile:       mmdio.flowchart.rectangle-solid/1
repository:    seanchatmangpt/mmdio
commit:        360c575becf941743131e51e8cb06e8b6f0eeef9
path:          src/mmdio/flowchart_crown.py
Git blob:      d114944c95392adf14a22fb2bdfcd26651dc39a0
claim ceiling: BOUNDED_FLOWCHART_SEMANTIC_ROUNDTRIP_AND_MERMAID_RENDER_ONLY
actuation:     false
```

This is the exact executed head of draft `mmdio#8`. Its focused workflow run `30878284501` passed nine positive/refusal/mutation tests, two byte-identical crown executions, independent receipt verification, and pinned Mermaid CLI rendering. The bounded receipt is `b6226754690ed6ec23cfdf77af5ddc7837152eeeee7f402ee681d2de6d5fecbb`.

The other Python-supported diagram types remain grammar/parser inventory. Their standing is unchanged until each receives its own executable profile, exact-source identity, negative controls, receipt, and replay.

## Authority graph

`semantic-crown.ttl` owns the cross-repository provenance facts. `ontology.ttl` continues to own the Python support and grammar-path inventory. Keeping them separate prevents grammar presence from being silently promoted to semantic equivalence.

## Gate

`gates/030_semantic_crown_requires_exact_source.rq` returns violations when:

- a crown lacks profile, repository, commit, path, Git blob, ceiling, or authority metadata;
- a crown has actuation authority;
- the v1 crown is attached to any type other than `mer:Type_flowchart`;
- the flowchart profile identity drifts.

## Exact-head integration

The workflow checks out the pinned mmdio commit, recomputes the source Git blob, parses both RDF graphs, executes the gate, checks the exact facts, and reruns the mmdio crown tests and deterministic replay.

A successful integration run proves the cross-repository contract for the pinned source only. It does not certify every Mermaid diagram type, the full generated mmdio registry, ggen release standing, or any actuation path.
