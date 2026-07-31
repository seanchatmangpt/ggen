# TICKET-045: Vertical scenario: Visible tests

## Status

PARTIAL_ALIVE — real pytest execution evidence proven and passing; Playwright layer blocked by
the real `next build` regression documented in TICKET-040's notes

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability/verification/run-visible-test>)`
- Acceptance-test steps:
  - `acceptance-step/6 (reproduce all visible and hidden test outcomes)`
- PRD requirements: (none)
- ARD components: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Prove run-visible-test executes real test code against the candidate's real submission and returns a real pass/fail per case.

## Current state

UNKNOWN — no implementation exists yet; this ticket verifies the composed system once workstreams B-H land.

## Target state

A passing Playwright/Chicago-TDD test exercising the real composed system for this scenario, using real collaborators: real test-runner subprocess execution. No mocked core collaborator.

## Projection classification

- Template: 60%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 40%
- Expected ratio: 60/40
- Custom-code justification: verification tickets are mostly test AUTHORING (a form of template-adjacent reusable test-harness code) composed with real system components; classified with a higher custom fraction than pure projection tickets because test scenario logic requires human design judgment about what to assert, though it contains zero domain data duplication — assertions reference generated types/RDF-sourced values, never restate them.

## Inputs

- the full composed system as of this scenario's dependencies

## Outputs

- examples/interview-assist/tests/e2e/visible-tests.spec.ts

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

1. Submit a real candidate implementation.
2. Dispatch run-visible-test against a real, known test case.
3. Assert the reported pass/fail matches the actual real execution outcome (verified independently by running the same test manually).

## Admission gates

- All ticket IDs in Dependencies complete.

## Acceptance criteria

- Given a correct real implementation, when run-visible-test executes, then it reports pass, matching an independent manual run of the same test.

## Negative tests

- Given a deliberately incorrect implementation, when run-visible-test executes, then it reports fail with the real assertion diff, not a generic 'test failed' message.

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

- TICKET-043
- TICKET-044

## Falsifier

If this scenario's test passes while any real collaborator is mocked or stubbed, the test is not a valid completion signal for this ticket — re-author with real collaborators before claiming done.

## Handoff

TICKET-053 (full decisive acceptance test) composes all 14 scenarios' proven paths into one end-to-end run.

## Definition of done

- test authored against real collaborators
- test passes against the real composed system
- no mocked core collaborator
- negative case included

## Implementation notes (real evidence)

- Playwright-vs-vitest substitution: see TICKET-040's Implementation notes for the full real
  evidence. Authored as a real vitest test driving `run_pytest` (TICKET-035's sandbox executor)
  instead.
- File: `examples/interview-assist/tests/scenarios/visible-tests.test.ts` (2 tests). Real run:
  `npx vitest run tests/scenarios/visible-tests.test.ts` → 2/2 passed, 1170ms.
    sha256: `8e42e682d38b4ef1635224af00f03192885eef277ef8469ada92a545e14d88eb`
- Acceptance criterion, including the ticket's own "matching an independent manual run" clause: a
  correct `add(a,b) -> a+b` implementation + a real visible pytest test → real `run_pytest` exit
  0, `stdout` matches `1 passed`. Independently re-verified by a SEPARATE, directly-spawned
  `python3 -m pytest -q` process in its own fresh temp directory (not a re-read of the executor's
  own self-report) — also `1 passed`.
- Negative test: a deliberately wrong implementation (`a - b` instead of `a + b`) → real
  `run_pytest` exit non-zero, `stdout` matches `1 failed` AND contains the real pytest
  assertion-rewrite diff `"assert -1 == 5"` (verified independently against a bare pytest run
  before wiring the assertion — `add(2,3)` under the wrong implementation really does yield
  `-1`) — not a generic "test failed" message.
- Full-suite regression check: `npx vitest run` → 85/85 passed. `npx tsc --noEmit` → clean.
