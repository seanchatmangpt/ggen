# TICKET-017: Admission/refusal outcome types projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/20-requirements.ttl (req/ard-refusal-* resources, 16 refusal codes per prior report)`
  - `packs/wasm4pm-interview-assist-pack/ontology/60-provenance-receipts.ttl (<manufacturing-chain/admitted-observation>, <manufacturing-chain/refusal>)`
- ARD components:
  - `ARD §11 Failure and refusal taxonomy (doc/ard#refusal-taxonomy)`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate a discriminated union of the 16 refusal codes plus the two-outcome (admitted | refused) result type, mirroring the prov:Entity alternate-outcome pattern already modeled in 60-provenance-receipts.ttl.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 90%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 10%
- Expected ratio: 90/10
- Custom-code justification: the Result<Admitted, Refused>-style wrapper type has some generic-programming boilerplate beyond pure enumeration.

## Inputs

- queries/refusal-codes.rq (new)

## Outputs

- examples/interview-assist/lib/domain/refusal.ts (16-member union + AdmissionResult<T> generic wrapper)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Generic 'RDF-enumerated code list -> discriminated union + typed Result wrapper' projection.

## Domain-data responsibility

The 16 refusal codes and their meanings live in 20-requirements.ttl's req/ard-refusal-* resources.

## Custom-code boundary

NONE.

## Exclusions

- no refusal code hardcoded outside the RDF-bound generation
- refusal is a first-class outcome type here, never represented as a thrown exception that swallows the code

## Implementation steps

1. Query req/ard-refusal-* resources with their rdf:value/dcterms:title text.
2. Generate the 16-member union.
3. Generate a generic AdmissionResult<T> = {status:'admitted', value:T} | {status:'refused', code: RefusalCode, reason:string}.
4. Verify member count equals 16 exactly.

## Admission gates

- TICKET-004.

## Acceptance criteria

- Given 16 refusal resources in RDF, when generation runs, then refusal.ts's union has exactly 16 members.

## Negative tests

- Remove one req/ard-refusal-* resource from a fixture and confirm the generated union drops to 15 members.

## Verification ladder

- Unit: generated output structure / type-check test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- refusal.ts hash
- member-count confirmation (16)

## Dependencies

- TICKET-004

## Falsifier

If refusal.ts's member count doesn't match the RDF-selected refusal-code count, this ticket is not complete.

## Handoff

TICKET-025 (refusal transitions) and TICKET-032 (refusal UI presentation) both import this type.

## Definition of done

- refusal.ts generated with 16 members
- AdmissionResult<T> wrapper generated and type-checks
