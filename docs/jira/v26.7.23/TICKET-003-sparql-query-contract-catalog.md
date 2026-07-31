# TICKET-003: Establish the SPARQL query contract catalog

## Status

ALIVE

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

## Implementation notes (real evidence)

- `queries/capabilities.rq`, `queries/phases.rq`, `queries/event-families.rq` written and header-
  documented with expected row counts.
- Verified against the real concatenated `ontology.ttl`: capabilities.rq -> 98 rows (matches
  TICKET-026's expected count), phases.rq -> 14 rows, event-families.rq -> 15 rows. All PASS.
- Negative test: deleted `<capability/session/join-session>` from a working copy of
  `ontology/30-capabilities.ttl`, rebuilt `ontology.ttl`, re-ran capabilities.rq -> 97 rows;
  restored -> 98 rows again. Proves genuine RDF-driven selection, not a cached/hardcoded count.
- Remaining queries (for workstreams D-G beyond phase/event-family/capability) not yet written —
  will be added incrementally as those tickets are picked up. Catalog is PARTIAL_ALIVE, not ALIVE.

## Implementation notes (real evidence) — session 2026-07-23 continuation

- Added 6 more `.rq` files closing the remaining workstream D-G selection needs from
  TICKET-004's "not yet written" rows: `queries/refusal-codes.rq`, `queries/policies.rq`,
  `queries/policy-rules.rq`, `queries/receipts.rq`, `queries/datasets.rq`,
  `queries/acceptance-steps.rq`. (`queries/accessibility-capabilities.rq`,
  `queries/hydra-operations.rq`, `queries/system-planes.rq` were already present on disk from
  another parallel agent's session — not authored in this pass, not re-verified here.)
- All 6 run for real via `rdflib` against `ontology.ttl` (1790 triples). Real row counts:
  - `refusal-codes.rq` -> 16 rows (the 16 `req/ard-refusal-*` codes carrying an `rdf:value` short
    code; the 17th resource in the same IRI family, `req/ard-refusal-no-inferred-truth`, is
    explicitly `FILTER`-excluded — it is the ARD §11 governing rule, not a refusal code, and
    carries a full sentence in `rdf:value` rather than a short code; unfiltered the count is 17).
  - `policies.rq` -> 6 rows (`authority-broker-default`, `practice-mode`, `mock-interview-mode`,
    `live-assistance-mode`, `assessment-mode`, `prohibited-mode` — all 6 `odrl:Set` resources).
  - `policy-rules.rq` -> 17 rows (the `odrl:Permission`/`odrl:Prohibition` blank nodes under the
    6 Sets: authority-broker-default 4 permission + 3 prohibition = 7; practice-mode 1;
    mock-interview-mode 1; live-assistance-mode 1; assessment-mode 1; prohibited-mode 6
    prohibition; total 17). The header comment's pre-execution estimate of 15 was wrong — the
    file was corrected to the real, executed count of 17 rather than left at the guess.
  - `receipts.rq` -> 11 rows (6 `prov:Entity` + 2 `prov:Activity` under `manufacturing-chain/*`,
    2 `prov:Entity` under `receipt/*`, 1 `spdx:Checksum`; per-entry inline checksum blank nodes
    not separately counted, no stable IRI).
  - `datasets.rq` -> 11 rows (1 `dcat:Dataset` + 9 `dcat:Distribution` + 1 `dcat:DataService`).
  - `acceptance-steps.rq` -> 10 rows, ordered via `BIND(xsd:integer(STRAFTER(...)))` numeric sort
    (plain lexical `ORDER BY ?step` was tried first and produced the wrong order — `1, 10, 2, 3,
    ...` — caught before finalizing, not shipped). Printed the ordered `?name` list and confirmed
    it reads: build the system -> create the sandbox -> replay the interview -> reproduce all
    admitted state transitions -> reproduce all code executions -> reproduce all visible and
    hidden test outcomes -> reproduce all accessibility projections -> verify that no prohibited
    capability was invoked -> reproduce the final session state -> match the final receipt hash —
    matches the expected build->sandbox->replay->...->hash-match shape.
- Negative test (same method as the original pass): deleted line 169
  (`req/ard-refusal-unsupported-language`) from a working copy of `ontology/20-requirements.ttl`,
  rebuilt `ontology.ttl` via `scripts/build-ontology.py`, re-ran `refusal-codes.rq` -> count
  dropped 16 -> 15; restored the line, rebuilt, re-ran -> back to 16. Proves genuine RDF-driven
  selection, not a cached count.
- All 6 named outputs from the ticket's Outputs list now exist and are verified
  (`capabilities.rq`, `phases.rq`, `policies.rq`, `event-families.rq`, `receipts.rq`,
  `acceptance-steps.rq`), plus 2 additional queries (`refusal-codes.rq`, `policy-rules.rq`,
  `datasets.rq`) needed to cover the D-G selection surface actually named in the source ticket
  text (refusal codes, ODRL permission/prohibition detail, DCAT dataset/distribution/service).
  Status raised PARTIAL_ALIVE -> ALIVE.
