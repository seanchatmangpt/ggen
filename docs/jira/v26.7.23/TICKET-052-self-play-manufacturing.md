# TICKET-052: Vertical scenario: Self-play manufacturing

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/70-packs-datasets.ttl (<service/self-play-factory>)`
- Acceptance-test steps: (none)
- PRD requirements:
  - `PRD §10 Self-play requirements`
- ARD components:
  - `ARD §3.12 Self-Play Factory`
- Policies: (none)
- SHACL shapes: (none)

## Objective

Prove the real Ollama-backed self-play worker (TICKET-037) produces a real candidate pack delta OUTSIDE the live session's critical path (per Architecture Decision 9: admission depends on executable evidence, not the model's say-so) — the delta must pass the SAME real execution/test admission path as a human-authored submission (TICKET-043/45/46), not a separate trusted path.

## Current state

UNKNOWN — no implementation exists yet; this ticket verifies the composed system once workstreams B-H land.

## Target state

A passing Playwright/Chicago-TDD test exercising the real composed system for this scenario, using real collaborators: real local Ollama model invocation, real subprocess execution of model-generated code. No mocked core collaborator.

## Projection classification

- Template: 60%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 40%
- Expected ratio: 60/40
- Custom-code justification: verification tickets are mostly test AUTHORING (a form of template-adjacent reusable test-harness code) composed with real system components; classified with a higher custom fraction than pure projection tickets because test scenario logic requires human design judgment about what to assert, though it contains zero domain data duplication — assertions reference generated types/RDF-sourced values, never restate them.

## Inputs

- the full composed system as of this scenario's dependencies

## Outputs

- examples/interview-assist/tests/e2e/self-play-manufacturing.spec.ts

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

1. Invoke the real self-play worker to generate a candidate solution + test.
2. Route the generated candidate through the SAME real compile/execute/test dispatch path a human submission would use (no privileged bypass).
3. Assert admission is based on the real test outcome, not the model's own claim of correctness.

## Admission gates

- All ticket IDs in Dependencies complete.

## Acceptance criteria

- Given a real self-play-generated candidate that actually fails its own generated test when really executed, when submitted for admission, then it is refused — proving admission depends on real executable evidence, not model self-report.

## Negative tests

- Given a real self-play-generated candidate that genuinely passes, confirm it IS admitted — the positive-path companion, proving the gate isn't fail-closed-always.

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

- TICKET-037
- TICKET-045
- TICKET-046

## Falsifier

If this scenario's test passes while any real collaborator is mocked or stubbed, the test is not a valid completion signal for this ticket — re-author with real collaborators before claiming done.

## Handoff

TICKET-053 (full decisive acceptance test) composes all 14 scenarios' proven paths into one end-to-end run.

## Definition of done

- test authored against real collaborators
- test passes against the real composed system
- no mocked core collaborator
- negative case included
