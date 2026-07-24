# TICKET-032: Test result/refusal/replay-failure presentation projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability/verification/run-visible-test>, <capability/verification/run-hidden-test>)`
  - `packs/wasm4pm-interview-assist-pack/ontology/20-requirements.ttl (req/ard-refusal-* resources)`
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the test-result display, refusal-presentation, and replay-failure-presentation components, with refusal messages sourced from TICKET-017's refusal.ts (which carries the RDF-sourced refusal-code text), never a UI-layer-hardcoded message string.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 75%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 25%
- Expected ratio: 75/25
- Custom-code justification: same JSX-authoring classification basis as TICKET-030/031.

## Inputs

- TICKET-017 refusal.ts
- TICKET-019 verification-state.ts

## Outputs

- examples/interview-assist/components/test-result-view.tsx
- examples/interview-assist/components/refusal-presentation.tsx
- examples/interview-assist/components/replay-failure-presentation.tsx

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Presentation components whose message content is entirely prop-driven from imported domain types.

## Domain-data responsibility

All 16 refusal-code display strings live in 20-requirements.ttl's req/ard-refusal-* resources (via TICKET-017's refusal.ts), never duplicated as UI-layer literals.

## Custom-code boundary

NONE beyond ordinary JSX authoring.

## Exclusions

- no refusal-code message string duplicated in the UI layer — must reference refusal.ts's RDF-sourced text, not restate it

## Implementation steps

1. Author test-result-view.tsx accepting VerificationState.
2. Author refusal-presentation.tsx accepting a RefusalCode and rendering its RDF-sourced message via a lookup, not a re-typed string.
3. Author replay-failure-presentation.tsx accepting a replay-mismatch result (TICKET-025's replay.ts output).

## Admission gates

- TICKET-017.
- TICKET-019.
- TICKET-025.

## Acceptance criteria

- Given a RefusalCode, when refusal-presentation.tsx renders, then the displayed message text matches the corresponding req/ard-refusal-* resource's rdf:value exactly, sourced via lookup.

## Negative tests

- Pass an unknown/malformed RefusalCode value at the type level and confirm `tsc` rejects it (the union type from TICKET-017 makes this a compile-time impossibility, verified here as a real test, not assumed).

## Verification ladder

- Unit: per-refusal-code render test against all 16 real codes
- Integration: composed with a real replay-mismatch fixture from TICKET-025
- End-to-end: exercised in TICKET-047 (timeout/refusal) and TICKET-049 (tamper detection)
- Chaos: N/A with reason — presentational only
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: 16-refusal-code render test matrix

## Receipts

- component file hashes
- 16-refusal-code render matrix result

## Dependencies

- TICKET-017
- TICKET-019
- TICKET-025

## Falsifier

If any of the 16 refusal codes renders a message that doesn't match its RDF source text, this ticket is not complete.

## Handoff

TICKET-047 and TICKET-049 (workstream I) exercise these components end-to-end.

## Definition of done

- three components generated
- all 16 refusal codes render correctly from RDF-sourced text
- type-level enforcement of valid RefusalCode confirmed
