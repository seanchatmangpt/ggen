# TICKET-019: Verification/accessibility/authority state projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability-category/verification> 13, <capability-category/accessibility> 16 resources)`
  - `packs/wasm4pm-interview-assist-pack/ontology/50-policy.ttl (8 authority-action/* resources)`
- ARD components:
  - `ARD §3.15 Authority Broker (doc/ard#components)`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate VerificationState, AccessibilityState, and AuthorityState types using the same capability-to-field derivation macro established in TICKET-018.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 85%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 15%
- Expected ratio: 85/15
- Custom-code justification: reuses TICKET-018's derivation macro; the 15% accounts for authority-state's additional need to reference the odrl:Set policy resources (a second RDF class), requiring the macro to compose two query results rather than one.

## Inputs

- queries/verification-capabilities.rq, accessibility-capabilities.rq (TICKET-015 reuse), authority-actions.rq (new)

## Outputs

- examples/interview-assist/lib/domain/verification-state.ts
- examples/interview-assist/lib/domain/accessibility-state.ts
- examples/interview-assist/lib/domain/authority-state.ts

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Reuses TICKET-018's capability-to-field derivation macro across three more capability categories plus the authority-action resources.

## Domain-data responsibility

Field sets driven entirely by capability-category/verification (13), capability-category/accessibility (16, already queried in TICKET-015 — reuse the same query, don't re-derive), and authority-action/* (8).

## Custom-code boundary

NONE.

## Exclusions

- no re-implementation of TICKET-018's derivation macro — import and reuse it
- no duplicate accessibility-capability query — reuse TICKET-015's

## Implementation steps

1. Reuse TICKET-018's derivation macro against capability-category/verification and capability-category/accessibility.
2. Query authority-action/* (8 resources) and derive AuthorityState fields similarly.
3. Verify field counts: 13 verification, 16 accessibility, 8 authority.

## Admission gates

- TICKET-018.

## Acceptance criteria

- Given 8 authority-action/* resources, when generation runs, then AuthorityState has exactly 8 derived fields.

## Negative tests

- Same pattern as TICKET-018: remove one authority-action/* resource from a fixture and confirm the field count drops by one.

## Verification ladder

- Unit: generated output structure / type-check test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- three state-type hashes
- field-count confirmations (13/16/8)

## Dependencies

- TICKET-018

## Falsifier

If any of the three state types' field counts don't match their RDF source counts, this ticket is not complete.

## Handoff

TICKET-028 (precondition/policy-target/authority-check projection) consumes AuthorityState.

## Definition of done

- three state types generated with correct field counts
- TICKET-018's macro reused, not duplicated
