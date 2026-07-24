# TICKET-003: Establish the SPARQL query contract catalog

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

Define, as a catalog of named `.rq` files with documented expected result shapes, every SPARQL query later templates will run against the admitted graph — so query authorship is centralized and auditable rather than ad hoc per-template.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

A `packs/wasm4pm-interview-assist-pack/queries/` directory (mirroring the `gates/*.rq` convention already used by ggen packs in this repo) with one `.rq` file per projection need identified in workstreams D-G, each with a header comment stating its expected variable bindings.

## Projection classification

- Template: 100%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 0%
- Expected ratio: 100/0
- Custom-code justification: NONE — SPARQL query text is, by this repo's own convention (see architecture.md's gates/*.rq pattern), template-classified structural machinery, not custom runtime code.

## Inputs

- packs/wasm4pm-interview-assist-pack/ontology/*.ttl (to know what's queryable)
- workstream D-G ticket objectives (to know what each needs selected)

## Outputs

- packs/wasm4pm-interview-assist-pack/queries/capabilities.rq
- packs/wasm4pm-interview-assist-pack/queries/phases.rq
- packs/wasm4pm-interview-assist-pack/queries/policies.rq
- packs/wasm4pm-interview-assist-pack/queries/event-families.rq
- packs/wasm4pm-interview-assist-pack/queries/receipts.rq
- packs/wasm4pm-interview-assist-pack/queries/acceptance-steps.rq
- (additional .rq files as workstream D-G tickets identify concrete selection needs)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Every .rq file is reusable query structure — parameterizable SELECT/CONSTRUCT shapes that any future pack's ontology-consuming template could reuse with a different graph.

## Domain-data responsibility

The query TEXT may reference InterviewAssist-specific IRI prefixes (e.g. `capability/`, `phase/`) since it's selecting FROM that graph — this is necessarily domain-aware at the query level, but the query itself produces generic tabular/graph bindings, not hardcoded value lists.

## Custom-code boundary

NONE.

## Exclusions

- no domain constants (e.g. a literal list of phase names) hardcoded outside the queries themselves
- no private ontology vocabulary in the queries

## Implementation steps

1. List every distinct selection need from workstreams D (state types), E (transitions), F (capabilities/policies), G (UI labels).
2. Author one .rq file per need, using only the public predicates cataloged in the prior report's prefix table.
3. Header-comment each .rq file with its expected SELECT variables or CONSTRUCT shape.
4. Run each query against the admitted corpus and record actual result counts (e.g. capabilities.rq -> 98 rows) as a smoke check.

## Admission gates

- TICKET-002 (SHACL gate) passes.

## Acceptance criteria

- Given capabilities.rq runs against the admitted corpus, when executed, then it returns exactly 98 rows (one per capability/* schema:Action).
- Given phases.rq runs, then it returns exactly 14 rows in lifecycle order (via skos:broader/narrower chase).

## Negative tests

- Run capabilities.rq against a corpus with one capability/* resource deleted and confirm the row count drops to 97, proving the query genuinely selects from the graph rather than returning a cached/hardcoded count.

## Verification ladder

- Unit: each .rq file's expected-row-count smoke test
- Integration: queries run together against the full corpus in sequence
- End-to-end: N/A with reason — queries have no runtime surface yet, that's workstream D-G
- Chaos: N/A with reason — read-only queries, no concurrency hazard
- Stress: N/A with reason — 1789-triple graph, no stress profile needed
- Benchmark: N/A with reason — sub-second query execution, no target
- Verifier report: per-query row-count smoke test results, aggregated

## Receipts

- query text hash per .rq file
- smoke-test row counts per query

## Dependencies

- TICKET-002

## Falsifier

If any .rq file's actual result set, when run against the real corpus, doesn't match its documented header comment, this ticket is not complete for that query.

## Handoff

Every generation ticket in workstreams D-G cites the specific .rq file(s) it consumes as its Source/Inputs.

## Definition of done

- queries/ directory exists with header-documented .rq files
- each file's smoke-test row count verified against the real corpus
- no query invents a resource not present in the admitted graph
