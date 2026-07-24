# TICKET-046: Vertical scenario: Hidden tests

## Status

PARTIAL_ALIVE — real pytest hidden-test execution evidence proven and passing; Playwright layer
blocked by the real `next build` regression documented in TICKET-040's notes; ADDITIONALLY a
real, verified, disclosed gap was found in this ticket's own no-leak acceptance criterion (see
notes) — the pass-path does not leak, but the fail-path currently does

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability/verification/run-hidden-test>)`
- Acceptance-test steps:
  - `acceptance-step/6`
- PRD requirements: (none)
- ARD components: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Prove run-hidden-test executes real hidden test cases WITHOUT exposing their content to the candidate-facing state (verified by inspecting the real client-visible state, not trusting a claim), matching PRD's visible/hidden distinction.

## Current state

UNKNOWN — no implementation exists yet; this ticket verifies the composed system once workstreams B-H land.

## Target state

A passing Playwright/Chicago-TDD test exercising the real composed system for this scenario, using real collaborators: real test-runner subprocess execution + real client-state inspection. No mocked core collaborator.

## Projection classification

- Template: 60%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 40%
- Expected ratio: 60/40
- Custom-code justification: verification tickets are mostly test AUTHORING (a form of template-adjacent reusable test-harness code) composed with real system components; classified with a higher custom fraction than pure projection tickets because test scenario logic requires human design judgment about what to assert, though it contains zero domain data duplication — assertions reference generated types/RDF-sourced values, never restate them.

## Inputs

- the full composed system as of this scenario's dependencies

## Outputs

- examples/interview-assist/tests/e2e/hidden-tests.spec.ts

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

1. Dispatch run-hidden-test.
2. Assert the pass/fail result is real (matches an independent manual run).
3. Inspect the real client-visible SessionState/WorkspaceState and assert the hidden test's source/assertions are NOT present anywhere in it.

## Admission gates

- All ticket IDs in Dependencies complete.

## Acceptance criteria

- Given a real hidden test suite, when run-hidden-test executes, then the reported result matches an independent manual run, AND the hidden test source is absent from all client-visible state.

## Negative tests

- Attempt to read hidden test content via any capability NOT explicitly authorized for it and confirm it's refused (policy check, TICKET-028).

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

- TICKET-045
- TICKET-028

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
- "Client-visible state" reasoning (the ticket leaves this open, stated here rather than
  assumed): `sandbox-executor.ts` has no distinct `run_hidden_test` capability — its
  `CapabilityId` union only has `run_pytest`/`run_cargo_test` (verified by reading the file). The
  visible/hidden distinction the RDF layer models
  (`capability/verification/run-visible-test` vs `run-hidden-test` in `capability.ts`) is not
  enforced by a separate code path today, only by which files the caller includes. The only real
  client-visible surface hidden-test content could leak through is
  `ExecutionReceipt.{stdout,stderr}`, because `app/api/run/route.ts` forwards those fields
  verbatim to the browser (`NextResponse.json({ receipt: result })`), and `app/page.tsx` stores
  them into `AppState.stdout`/`stderr`, rendered by `<ConsolePanel>`. The test inspects that real
  value.
- File: `examples/interview-assist/tests/scenarios/hidden-tests.test.ts` (3 tests). Real run:
  `npx vitest run tests/scenarios/hidden-tests.test.ts` → 3/3 passed, 862ms.
    sha256: `37bc2bcb897ce37a8d748479d8253bed5bf8d5158a5d1fd06ce8fc8fb0e6abfb`
- **Positive path (holds):** a passing hidden test (unique marker
  `HIDDEN_TEST_MAGIC_MARKER_9f3c` embedded in the hidden test's function name and assertion
  message) → real `run_pytest` exit 0, `stdout` matches `1 passed`, matching an independent
  manual pytest run. Real inspection of `result.stdout`/`result.stderr`: the marker does **not**
  appear anywhere — pytest's `-q` passing-case output is just `.` + the summary line, no test
  names.
- **DISCLOSED FINDING, real and verified, NOT fixed by this ticket (out of its stated
  Custom-code boundary — a production fix, not test authoring):** the SAME hidden test, run
  against a deliberately wrong implementation so it FAILS, DOES leak. Independently verified
  against a bare `python3 -m pytest -q` run before wiring the assertion:
  ```
  F                                                                        [100%]
  =================================== FAILURES ===================================
  ________________________ test_hidden_MAGIC_MARKER_9f3c _________________________
      def test_hidden_MAGIC_MARKER_9f3c():
          from solution import add
  >       assert add(7, 6) == 13, "MAGIC_MARKER_9f3c leak check"
  E       AssertionError: MAGIC_MARKER_9f3c leak check
  ...
  FAILED test_hidden.py::test_hidden_MAGIC_MARKER_9f3c - AssertionError: MAGIC_...
  ```
  `-q` (quiet) only suppresses per-test progress verbosity, not the `FAILURES` detail section —
  the hidden test's fully-qualified name and assertion message both appear in `stdout`, which is
  exactly the field `app/api/run` forwards to the client. The test file asserts this REAL,
  reproduced behavior (`result.stdout` DOES contain the marker on the failure path) rather than
  asserting the ticket's stated criterion falsely. **This means TICKET-046's acceptance criterion
  ("the hidden test source is absent from all client-visible state") does not currently hold on
  the failure path** with `sandbox-executor.ts`'s hardcoded `["-m", "pytest", "-q"]` invocation.
  A real fix (e.g., a custom pytest reporter/`--tb=no` combined with a separate pass/fail-only
  summary line, or running hidden tests with output redacted before it reaches
  `ExecutionReceipt`) is a follow-up production task, out of this ticket's own test-authoring
  scope, and is reported here rather than silently omitted or silently patched.
- Negative test (capability/policy layer): confirmed independently against the real generated
  `checkPolicy` (`lib/domain/policy-check.ts`, TICKET-028) that
  `policy/authority-broker-default` really does deny `authority-action/execute-code` before
  relying on it; then dispatched `run_pytest` under that mode — real refusal,
  `kind === "policy_denied"`, and a `ps ax` check confirms no subprocess bearing a unique marker
  was ever spawned.
- Full-suite regression check: `npx vitest run` → 85/85 passed. `npx tsc --noEmit` → clean.
