# TICKET-D003: 5 of 14 SHACL shapes have no SPARQL-gate equivalent enforced at sync time

## Status

DEFERRED — permanent structural limitation, disclosed by the originating ticket

## Priority

P1 — governance/validation coverage gap: 5 domain constraints are advisory only; real hardening needed before broader ontology authorship or scaling the corpus

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-009-sparql-gate-integration.md`

## Source

- Research report: Disclosed Gaps Catalog, section (a) Permanent structural limitations
- Citation: "9 enforceable SHACL shapes (of the 14 total; 5 are advisory `minCount 0` shapes with no gate equivalent, honestly noted, not fabricated) translated into `gates/010`..`090_*.rq`." — `docs/jira/v26.7.23/TICKET-009-sparql-gate-integration.md:128-129`

## Objective

Determine, for each of the 5 advisory `minCount 0` SHACL shapes not covered by a `ggen sync run`-time gate, whether it should gain an enforceable SPARQL gate or remain permanently advisory by documented design.

## Current state

TICKET-009 is ALIVE for the 9 shapes it did translate into gates; 5 of the original 14 SHACL shapes are `minCount 0` (advisory, non-enforcing by SHACL's own semantics) and have no corresponding `gates/0NN_*.rq` file, so a graph violating one of the 5 would not fail `ggen sync run`.

## Target state

Each of the 5 advisory shapes has either (a) an explicit new gate if the domain actually requires enforcement, or (b) a documented rationale in the pack for why `minCount 0` (advisory-only) is the correct, permanent design for that specific shape.

## Projection classification

- Template: 100% if new gates are added — gates are pure SPARQL, structurally identical to the existing 9
- Domain data: the 5 shapes themselves are already admitted RDF (SHACL is domain data, not code)
- Custom code: 0% — gate authorship is itself SPARQL/template work, not custom runtime code

## Inputs

- `packs/wasm4pm-interview-assist-pack/shapes/interview-assist.shacl.ttl` (14 NodeShapes)
- the existing `gates/010`..`090_*.rq` (9 files)

## Outputs

- either up to 5 new `gates/*.rq` files, or a documented decision note in the pack listing each advisory shape and its rationale for staying advisory

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

Any new gate reuses the exact SPARQL-gate pattern TICKET-009 already established for the 9 enforced shapes.

## Domain-data responsibility

The 5 shapes' semantics (what they check) are already fully specified in the admitted SHACL graph; this ticket only decides enforcement policy, not new domain meaning.

## Custom-code boundary

None — gate SPARQL is template/query work, not custom code.

## Exclusions

- no silent enforcement of a shape without updating the pack's gate-coverage documentation
- no removal of the existing 9 gates

## Implementation steps

1. Enumerate the 5 `minCount 0` shapes by name from `interview-assist.shacl.ttl`.
2. For each, assess whether the underlying domain concept is load-bearing enough to warrant a hard sync-time gate versus remaining genuinely optional/advisory.
3. Author new `gates/1NN_*.rq` files for any shape reclassified as enforceable, following TICKET-009's existing SPARQL-gate pattern exactly.
4. Document the final coverage (9+N of 14, or 9 of 14 with rationale) in the pack's own README or a `gates/COVERAGE.md`.

## Admission gates

- TICKET-009 (predecessor)

## Acceptance criteria

Given the pack's SHACL shapes and its `gates/` directory, when compared, then every one of the 14 shapes is accounted for as either an enforced gate or a documented advisory decision — none silently uncovered.

## Negative tests

Construct a graph violating one of the 5 previously-advisory shapes; if it was reclassified as enforceable, confirm `ggen sync run` now fails on it; if left advisory, confirm the documented rationale explicitly covers this exact violation case.

## Verification ladder

- Unit: per-gate SPARQL query unit test against a real violating and a real conforming graph fixture
- Integration: `ggen sync run` against the full pack with a real gate-violating fixture, confirming refusal
- End-to-end: N/A with reason — gate enforcement is a sync-time concern, not an application-runtime concern
- Chaos: N/A with reason
- Stress: N/A with reason — no volume target
- Benchmark: N/A with reason
- Verifier report: gate-coverage documentation cross-checked against the 14-shape SHACL file

## Receipts

- updated gate-coverage documentation
- new gate SPARQL files (if any)

## Dependencies

- TICKET-009

## Falsifier

If any of the 14 SHACL shapes is neither enforced by a gate nor explicitly documented as an accepted advisory decision, this ticket is not complete.

## Handoff

Feeds TICKET-009's own future re-verification; no other downstream ticket depends on this directly.

## Definition of done

- all 5 previously-uncovered shapes triaged
- any newly-enforceable shapes have real gates with passing/failing fixture tests
- gate-coverage documented for all 14 shapes
