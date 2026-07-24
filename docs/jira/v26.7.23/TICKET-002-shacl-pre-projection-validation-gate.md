# TICKET-002: SHACL pre-projection validation gate (pyshacl, admission check before any query runs)

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/*.ttl`
- SHACL shapes:
  - `packs/wasm4pm-interview-assist-pack/shapes/interview-assist.shacl.ttl (14 NodeShapes: DigitalDocumentShape, ActionShape, ConceptSchemeShape, ConceptShape, PolicySetShape, PermissionShape, ProhibitionShape, ProvenanceEntityShape, ReceiptEntityShape, ProvenanceActivityShape, DatasetShape, DistributionShape, DataServiceShape, RequirementShape)`
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)

## Objective

Turn the manual pyshacl.validate() run from this session's report into a repeatable, scriptable gate that every projection ticket must pass before it queries the graph.

## Current state

PARTIAL_ALIVE — pyshacl.validate() was run manually once this session and returned CONFORMS: True (after fixing the 13 transition-plan/* missing schema:name violations); not yet wired into a repeatable script or CI step.

## Target state

A script (`scripts/validate-interview-assist-ontology.sh` or similar) runs `pyshacl.validate()` against the corpus and exits non-zero on any violation, callable by every downstream ticket's admission gate.

## Projection classification

- Template: 100%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 0%
- Expected ratio: 100/0
- Custom-code justification: NONE — this is a pure invocation of an existing library (pyshacl) against existing artifacts; no irreducible runtime behavior.

## Inputs

- packs/wasm4pm-interview-assist-pack/ontology/*.ttl
- packs/wasm4pm-interview-assist-pack/shapes/interview-assist.shacl.ttl
- TICKET-001 corpus-manifest.json

## Outputs

- scripts/validate-interview-assist-ontology.sh
- SHACL validation report artifact (pyshacl results_text) written per run

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Generic 'run pyshacl against data+shapes, fail loudly on non-conformance' script — reusable for any future admitted-ontology pack, not InterviewAssist-specific logic.

## Domain-data responsibility

None — the shapes file already encodes what 'valid InterviewAssist graph' means; this ticket only automates invoking that check.

## Custom-code boundary

NONE.

## Exclusions

- no domain constants in templates
- no `generated/` directory
- no unreceipted actuation

## Implementation steps

1. Wrap the manual pyshacl.validate() call used in this session's verification into a standalone script.
2. Script takes the corpus file list and shapes file as arguments (not hardcoded paths) so it is reusable.
3. Script writes the full results_text to a report file, and exits 1 on CONFORMS: False.
4. Wire the script to also verify against TICKET-001's corpus-manifest.json hash to ensure gate and hash always agree on which corpus was checked.

## Admission gates

- TICKET-001 complete (corpus-manifest.json exists).

## Acceptance criteria

- Given the current admitted corpus, when the gate script runs, then it exits 0 and reports CONFORMS: True.
- Given a deliberately introduced SHACL violation (e.g. a schema:Action missing schema:name), when the gate script runs, then it exits non-zero and names the violating resource.

## Negative tests

- Remove schema:name from one capability/* resource and confirm the gate fails with a MinCountConstraintComponent violation naming that exact resource (mirrors the real transition-plan/* bug found and fixed this session).

## Verification ladder

- Unit: N/A with reason — this ticket wraps an existing verified library call, no new logic to unit test beyond the wrapper's exit-code behavior
- Integration: run against the real corpus and a deliberately-broken fixture copy
- End-to-end: N/A with reason — no runtime surface, this is a build-time gate
- Chaos: N/A with reason — single-process, single-invocation script
- Stress: N/A with reason — corpus is small
- Benchmark: N/A with reason — no perf target for a <2s validation script
- Verifier report: pyshacl results_text is the verifier report

## Receipts

- pyshacl results_text artifact per invocation
- exit code

## Dependencies

- TICKET-001

## Falsifier

If the gate script exits 0 against a corpus that actually violates a shape (verified independently by running pyshacl.validate() by hand), this ticket is not complete.

## Handoff

Every subsequent projection ticket (011+) invokes this gate as a precondition before running its SPARQL queries.

## Definition of done

- script exists, is executable, and is committed
- passes against the real current corpus
- fails against a deliberately broken fixture
