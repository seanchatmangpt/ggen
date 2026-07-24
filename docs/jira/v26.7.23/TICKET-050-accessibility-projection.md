# TICKET-050: Vertical scenario: Accessibility projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (16 capability/accessibility/* resources)`
- Acceptance-test steps:
  - `acceptance-step/7 (reproduce all accessibility projections)`
- ARD components:
  - `ARD §9 Accessibility requirements`
- PRD requirements: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Prove, via a real Playwright accessibility audit (axe-core or equivalent, run against a real rendered page, not a static lint), that the session UI satisfies real ARIA-landmark and keyboard-only-operation requirements corresponding to a sample of the 16 accessibility capabilities, and that this is REPRODUCIBLE (acceptance-step/7) across two independent renders of the same session state.

## Current state

UNKNOWN — no implementation exists yet; this ticket verifies the composed system once workstreams B-H land.

## Target state

A passing Playwright/Chicago-TDD test exercising the real composed system for this scenario, using real collaborators: real Playwright browser + real accessibility audit tooling. No mocked core collaborator.

## Projection classification

- Template: 60%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 40%
- Expected ratio: 60/40
- Custom-code justification: verification tickets are mostly test AUTHORING (a form of template-adjacent reusable test-harness code) composed with real system components; classified with a higher custom fraction than pure projection tickets because test scenario logic requires human design judgment about what to assert, though it contains zero domain data duplication — assertions reference generated types/RDF-sourced values, never restate them.

## Inputs

- the full composed system as of this scenario's dependencies

## Outputs

- examples/interview-assist/tests/e2e/accessibility-projection.spec.ts

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

1. Render a real completed-session page.
2. Run a real automated accessibility audit against the live DOM.
3. Assert zero critical violations for the landmark/keyboard-operation checks corresponding to the audited capabilities.
4. Re-render the same session state a second time and assert the audit result is IDENTICAL (reproducibility, per acceptance-step/7).

## Admission gates

- All ticket IDs in Dependencies complete.

## Acceptance criteria

- Given a real rendered session page, when audited, then zero critical accessibility violations are reported for the capabilities under test, and re-auditing an identical render produces an identical result.

## Negative tests

- Deliberately remove an ARIA landmark role from a component (fixture regression) and confirm the audit catches it — proving the audit is a real, sensitive check, not a rubber stamp.

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

- TICKET-033
- TICKET-039

## Falsifier

If this scenario's test passes while any real collaborator is mocked or stubbed, the test is not a valid completion signal for this ticket — re-author with real collaborators before claiming done.

## Handoff

TICKET-053 (full decisive acceptance test) composes all 14 scenarios' proven paths into one end-to-end run.

## Definition of done

- test authored against real collaborators
- test passes against the real composed system
- no mocked core collaborator
- negative case included
