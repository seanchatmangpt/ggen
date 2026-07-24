# TICKET-043: Vertical scenario: Python coding workflow

## Status

PARTIAL_ALIVE — real subprocess compile+execute evidence proven and passing; Playwright layer
blocked by the real `next build` regression documented in TICKET-040's notes (the underlying
sandbox executor itself is fully real and unaffected — this is purely a browser-rendering
blocker)

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability/runtime/select-language>, <capability/runtime/compile>, <capability/runtime/execute>)`
- Acceptance-test steps: (none)
- PRD requirements:
  - `PRD's Decisive Product Choice: Python + Rust vertical`
- ARD components: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Prove a real Python file created, compiled (syntax-checked), and executed through the real subprocess sandbox executor (TICKET-035) returns real stdout/exit-status, not a canned response.

## Current state

UNKNOWN — no implementation exists yet; this ticket verifies the composed system once workstreams B-H land.

## Target state

A passing Playwright/Chicago-TDD test exercising the real composed system for this scenario, using real collaborators: real Python subprocess execution via the hardened sandbox executor. No mocked core collaborator.

## Projection classification

- Template: 60%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 40%
- Expected ratio: 60/40
- Custom-code justification: verification tickets are mostly test AUTHORING (a form of template-adjacent reusable test-harness code) composed with real system components; classified with a higher custom fraction than pure projection tickets because test scenario logic requires human design judgment about what to assert, though it contains zero domain data duplication — assertions reference generated types/RDF-sourced values, never restate them.

## Inputs

- the full composed system as of this scenario's dependencies

## Outputs

- examples/interview-assist/tests/e2e/python-coding-workflow.spec.ts

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

1. Dispatch create-file with real Python source.
2. Dispatch compile (Python syntax check) then execute.
3. Assert real captured stdout matches the actual program's real output for a known input.

## Admission gates

- All ticket IDs in Dependencies complete.

## Acceptance criteria

- Given a real Python 'print(1+1)' program, when executed via the real sandbox, then captured stdout is exactly '2', proving genuine execution, not a stub.

## Negative tests

- Submit Python source with a real syntax error and confirm compile fails with a real, specific diagnostic, not a generic failure.

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

- TICKET-035
- TICKET-027

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
  evidence. Authored as a real vitest test driving the real subprocess sandbox executor
  (TICKET-035) instead.
- File: `examples/interview-assist/tests/scenarios/python-coding-workflow.test.ts` (2 tests).
  Real run: `npx vitest run tests/scenarios/python-coding-workflow.test.ts` → 2/2 passed, 154ms.
    sha256: `1c1371f7bf49e326fae1bc54c7a4bae02869f44bb1308e8c0f1b89470ef954c4`
- Distinct from `tests/adapters/sandbox-executor.test.ts` (which proves `execute_python` alone):
  this test dispatches `compile_python` (real `python3 -m py_compile`) THEN `execute_python` as
  two separate real subprocess calls, matching the ticket's create-file → compile → execute
  steps. `compile_python` real exit code 0, then `execute_python` on `print(1 + 1)` → real
  captured `stdout.trim() === "2"`, `exitCode === 0`.
- Negative test: real syntax error (`def broken(:\n    print('unbalanced'\n`). Independently
  verified against a bare `python3 -m py_compile` before wiring the assertion:
  ```
  File "solution.py", line 1
      def broken(:
                 ^
  SyntaxError: invalid syntax
  ```
  Test asserts `compiled.exitCode !== 0`, `compiled.stderr` contains `"SyntaxError"` and
  `"solution.py"` — a real, specific diagnostic, not a generic failure string.
- Full-suite regression check: `npx vitest run` → 85/85 passed. `npx tsc --noEmit` → clean.
