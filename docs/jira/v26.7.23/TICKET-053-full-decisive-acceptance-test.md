# TICKET-053: Vertical scenario: Full decisive acceptance test

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources: (none)
- Acceptance-test steps:
  - `acceptance-step/1 through acceptance-step/10, in order, per <acceptance-test-scheme>`
- PRD requirements: (none)
- ARD components: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Compose scenarios 040-052's proven real paths into one single end-to-end run reproducing all 10 acceptance-step/* resources in their documented dcterms:requires order: build -> create sandbox -> replay -> reproduce transitions -> reproduce executions -> reproduce test outcomes -> reproduce accessibility projections -> verify no prohibited capability invoked -> reproduce final session state -> match the final receipt hash.

## Current state

UNKNOWN — no implementation exists yet; this ticket verifies the composed system once workstreams B-H land.

## Target state

A passing Playwright/Chicago-TDD test exercising the real composed system for this scenario, using real collaborators: every real collaborator from scenarios 040-052, composed in one run. No mocked core collaborator.

## Projection classification

- Template: 60%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 40%
- Expected ratio: 60/40
- Custom-code justification: verification tickets are mostly test AUTHORING (a form of template-adjacent reusable test-harness code) composed with real system components; classified with a higher custom fraction than pure projection tickets because test scenario logic requires human design judgment about what to assert, though it contains zero domain data duplication — assertions reference generated types/RDF-sourced values, never restate them.

## Inputs

- the full composed system as of this scenario's dependencies

## Outputs

- examples/interview-assist/tests/e2e/full-decisive-acceptance-test.spec.ts

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Shared Playwright harness (TICKET-039) and shared fixture-building utilities, reused across all 14 scenarios.

## Domain-data responsibility

Assertions reference RDF-sourced generated types/values (e.g. specific phase names, refusal codes, capability ids) rather than restating them as new literals.

## Custom-code boundary

The test itself, and any real external process it must drive (Ollama, subprocess, browser) via the workstream H adapters — no new production custom code introduced by this ticket, only test code exercising existing adapters.

## Exclusions

- no mocked core collaborator per this repo's Chicago TDD policy
- no assertion that merely counts generated files or checks a generated string for non-emptiness — must assert on real observable state
- no test skipped/marked #[ignore] to force a green run

## Implementation steps

1. Execute acceptance-step/1 (real build) through acceptance-step/10 (real hash match) in the exact dcterms:requires-declared order from 80-acceptance.ttl.
2. For acceptance-step/8 (verify no prohibited capability was invoked), assert the full event log contains zero prohibited-action/* dispatches (TICKET-028's policy check log).
3. For acceptance-step/10, assert the freshly-computed final receipt hash equals the hash computed at original session completion, byte-for-byte.
4. This ticket is the ONLY one authorized to claim the system is ALIVE per this backlog's Falsifier discipline — no earlier ticket may claim it.

## Admission gates

- All ticket IDs in Dependencies complete.

## Acceptance criteria

- Given a full real interview session run through this composed scenario, when all 10 acceptance steps execute in order, then step 10's freshly-computed hash matches the original session's receipt hash exactly, and step 8 confirms zero prohibited-capability dispatches occurred.

## Negative tests

- Deliberately invoke a prohibited-action/* capability mid-session (bypassing normal UI flow, calling the dispatch table directly) and confirm acceptance-step/8 fails the run, proving the check is real and load-bearing, not decorative.

## Verification ladder

- Unit: N/A with reason — this ticket IS the integration/e2e layer, not a unit-test ticket
- Integration: composed real system, no mocks
- End-to-end: the primary layer this ticket delivers, via Playwright against a real running dev server
- Chaos: where relevant to the scenario, noted in steps above
- Stress: N/A with reason — single-scenario correctness test, not a load test
- Benchmark: N/A with reason — no perf target for this scenario
- Verifier report: Playwright test run transcript + screenshots/traces on failure

## Receipts

- test spec hash
- Playwright run transcript
- pass/fail result

## Dependencies

- TICKET-040
- TICKET-041
- TICKET-042
- TICKET-043
- TICKET-044
- TICKET-045
- TICKET-046
- TICKET-047
- TICKET-048
- TICKET-049
- TICKET-050
- TICKET-051
- TICKET-052

## Falsifier

If this scenario's test passes while any real collaborator is mocked or stubbed, the test is not a valid completion signal for this ticket — re-author with real collaborators before claiming done.

## Handoff

TICKET-053 (full decisive acceptance test) composes all 14 scenarios' proven paths into one end-to-end run.

## Definition of done

- test authored against real collaborators
- test passes against the real composed system
- no mocked core collaborator
- negative case included
