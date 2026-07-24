# TICKET-004: Public-ontology-to-projection mapping table

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/*.ttl`
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Produce one authoritative table mapping each public-ontology class/predicate actually used in the corpus to the TypeScript/template construct it will project into, so template authors in workstreams C-G have a single reference instead of re-deriving the mapping per-ticket.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

A `packs/wasm4pm-interview-assist-pack/docs/projection-mapping.md` table: e.g. `schema:Action` (capability/* subset) -> capability registry entry type; `skos:Concept` (phase/* scheme) -> phase union type + transition table row; `odrl:Set`/`Permission`/`Prohibition` -> policy-check function signature; `prov:Entity`/`Activity` -> transition receipt type; `dcat:Dataset`/`Distribution` -> pack manifest type.

## Projection classification

- Template: 100%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 0%
- Expected ratio: 100/0
- Custom-code justification: NONE — this is documentation, not runtime code.

## Inputs

- packs/wasm4pm-interview-assist-pack/ontology/*.ttl
- the public-vocabulary audit prefix table from this session's report

## Outputs

- packs/wasm4pm-interview-assist-pack/docs/projection-mapping.md

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The mapping table itself is the reusable contract template authors reference; it is not tied to any single template file.

## Domain-data responsibility

Which specific IRIs exist under each class remains in the RDF; the table maps CLASSES/PREDICATES to projection SHAPES, not individual instances to individual code lines.

## Custom-code boundary

NONE.

## Exclusions

- no invented implementation semantics absent from the admitted graph
- no domain constants copied into the mapping table beyond illustrative single examples

## Implementation steps

1. Walk the 14-prefix whitelist from the prior audit report.
2. For each class actually instantiated in the corpus (schema:Action, schema:DigitalDocument, skos:Concept, skos:ConceptScheme, odrl:Set/Permission/Prohibition, prov:Entity/Activity, dcat:Dataset/Distribution/DataService, spdx:Checksum, hydra:Operation), state its target TypeScript/template projection shape.
3. Cross-reference each mapping to the workstream ticket(s) (D-G) that will implement it.
4. Review for internal consistency — no class maps to two contradictory shapes across different tickets.

## Admission gates

- TICKET-003 (query catalog) exists, since the mapping table should cite which .rq file selects each class.

## Acceptance criteria

- Given a template author reads projection-mapping.md, when they look up skos:Concept (phase scheme), then they find the exact target type shape and the .rq file that selects it, with no ambiguity requiring re-derivation.

## Negative tests

- Attempt to map a class NOT present in the admitted corpus (e.g. owl:Class) and confirm the table explicitly excludes it as 'not used in this corpus' rather than inventing a mapping for an absent concept.

## Verification ladder

- Unit: N/A with reason — documentation artifact
- Integration: cross-checked against TICKET-003's query catalog for consistency
- End-to-end: N/A with reason — no runtime surface
- Chaos: N/A with reason — static document
- Stress: N/A with reason — static document
- Benchmark: N/A with reason — static document
- Verifier report: manual consistency review recorded as a checklist in the doc's own changelog section

## Receipts

- projection-mapping.md content hash

## Dependencies

- TICKET-003

## Falsifier

If any workstream D-G ticket's Template responsibility section contradicts this mapping table's stated shape for the same class, this ticket is not complete.

## Handoff

Workstreams C through G cite this table directly in their Template responsibility sections instead of re-deriving mappings ad hoc.

## Definition of done

- projection-mapping.md exists and covers every class/predicate in the 14-prefix whitelist that the corpus actually instantiates
- no contradictions with downstream ticket definitions
