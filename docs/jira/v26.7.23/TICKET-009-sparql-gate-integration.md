# TICKET-009: SPARQL gate integration (replacing SHACL for ggen sync run consumption)

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources: (none)
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes:
  - `packs/wasm4pm-interview-assist-pack/shapes/interview-assist.shacl.ttl (source of truth to translate into gates)`

## Objective

Translate the intent of the 14 SHACL NodeShapes into `.rq`-based SPARQL ASK/SELECT gates, since this repo's own convention (confirmed: wasm4pm-facts-pack already migrated off SHACL) is that `ggen sync run` only executes SPARQL gates, not SHACL.

## Current state

BLOCKED — SHACL validation currently only runs via the manual pyshacl path (TICKET-002); `ggen sync run` will not enforce these constraints until this ticket lands.

## Target state

packs/wasm4pm-interview-assist-pack/gates/*.rq — one gate per SHACL NodeShape's core constraint (e.g. every schema:Action has schema:name), auto-discovered and run by `ggen sync run` per this repo's existing gate convention.

## Projection classification

- Template: 100%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 0%
- Expected ratio: 100/0
- Custom-code justification: NONE — gates are SPARQL text, already this repo's own convention for reusable structural admission checks.

## Inputs

- packs/wasm4pm-interview-assist-pack/shapes/interview-assist.shacl.ttl
- an existing pack's gates/*.rq as a convention reference

## Outputs

- packs/wasm4pm-interview-assist-pack/gates/action-has-name.rq
- packs/wasm4pm-interview-assist-pack/gates/policy-set-has-permission-or-prohibition.rq
- packs/wasm4pm-interview-assist-pack/gates/concept-scheme-has-top-concepts.rq
- (one gate per NodeShape, 14 total)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Every gate is reusable ASK-query structure; the SHACL-to-SPARQL translation pattern itself is reusable for future packs migrating off SHACL.

## Domain-data responsibility

Gate query text references InterviewAssist classes/predicates since it's checking that specific graph, same domain-awareness-at-query-level noted in TICKET-003.

## Custom-code boundary

NONE.

## Exclusions

- no fail-open gates (an ASK gate that can't determine an answer must be treated as a failure, not a pass, per this repo's Mistake Class 3)

## Implementation steps

1. For each of the 14 SHACL NodeShapes, write an equivalent SPARQL ASK or SELECT-count gate.
2. Verify each gate, run standalone against the admitted corpus, agrees with the corresponding pyshacl NodeShape result (both pass on the current clean corpus).
3. Verify each gate, run against a deliberately broken fixture (e.g. transition-plan/* missing schema:name, the real bug found this session), fails and pyshacl agrees.
4. Place gates in the pack's gates/ directory per the auto-discovery convention.

## Admission gates

- TICKET-002 (SHACL baseline to translate from).
- TICKET-008 (directory scaffold).

## Acceptance criteria

- Given the admitted corpus, when all 14 gates run, then all pass, matching pyshacl's CONFORMS: True.
- Given the same broken fixture pyshacl caught (transition-plan/* missing schema:name), when the corresponding gate runs, then it fails, matching pyshacl's violation.

## Negative tests

- The transition-plan/* missing-schema:name fixture is the primary negative test — real, not synthetic, since it's the actual bug this session found and fixed.

## Verification ladder

- Unit: per-gate ASK query correctness against fixtures
- Integration: all 14 gates run together against the real corpus
- End-to-end: gates run via a real `ggen sync run --dry-run` invocation once TICKET-006's layout resolution lands
- Chaos: N/A with reason — deterministic read-only queries
- Stress: N/A with reason — small corpus
- Benchmark: N/A with reason — no perf target
- Verifier report: 14-gate pass/fail matrix compared against pyshacl's 14-shape results

## Receipts

- per-gate pass/fail results
- cross-check against pyshacl results

## Dependencies

- TICKET-002
- TICKET-006
- TICKET-008

## Falsifier

If any gate disagrees with its corresponding SHACL NodeShape's verdict on either the clean corpus or the broken fixture, this ticket is not complete.

## Handoff

TICKET-010 (idempotent sync) runs these gates as part of `ggen sync run`.

## Definition of done

- 14 gates exist, one per NodeShape
- all agree with pyshacl on both the clean corpus and the known broken fixture
