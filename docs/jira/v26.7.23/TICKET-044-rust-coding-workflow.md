# TICKET-044: Vertical scenario: Rust coding workflow

## Status

PARTIAL_ALIVE — real rustc compile+execute evidence proven and passing; Playwright layer blocked
by the real `next build` regression documented in TICKET-040's notes (the underlying sandbox
executor itself is fully real and unaffected — this is purely a browser-rendering blocker)

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (same runtime capabilities as Python, different select-language-version)`
- Acceptance-test steps: (none)
- PRD requirements:
  - `PRD's Decisive Product Choice: Python + Rust vertical`
- ARD components: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Prove the same real-execution path works for Rust (compile step is genuinely load-bearing here, unlike Python's syntax-check-only compile), proving the sandbox executor is language-parameterized, not Python-only.

## Current state

UNKNOWN — no implementation exists yet; this ticket verifies the composed system once workstreams B-H land.

## Target state

A passing Playwright/Chicago-TDD test exercising the real composed system for this scenario, using real collaborators: real rustc invocation via the sandbox executor. No mocked core collaborator.

## Projection classification

- Template: 60%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 40%
- Expected ratio: 60/40
- Custom-code justification: verification tickets are mostly test AUTHORING (a form of template-adjacent reusable test-harness code) composed with real system components; classified with a higher custom fraction than pure projection tickets because test scenario logic requires human design judgment about what to assert, though it contains zero domain data duplication — assertions reference generated types/RDF-sourced values, never restate them.

## Inputs

- the full composed system as of this scenario's dependencies

## Outputs

- examples/interview-assist/tests/e2e/rust-coding-workflow.spec.ts

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

1. Dispatch create-file with real Rust source.
2. Dispatch compile (real rustc invocation) then execute.
3. Assert real captured stdout/exit-status for a known program.

## Admission gates

- All ticket IDs in Dependencies complete.

## Acceptance criteria

- Given a real Rust program printing '2', when compiled and executed via the real sandbox, then captured stdout is exactly '2' and exit status is 0.

## Negative tests

- Submit Rust source with a real type error and confirm compile fails with rustc's real diagnostic text captured, not a fabricated message.

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
- TICKET-043

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
- File: `examples/interview-assist/tests/scenarios/rust-coding-workflow.test.ts` (3 tests). Real
  run: `npx vitest run tests/scenarios/rust-coding-workflow.test.ts` → 3/3 passed, 844ms
  (compile+execute test 658ms).
    sha256: `254361e157c73f5c493029469e5bdc3e8adf5e6bde6bdd912a8bbb42fc92a233`
- Distinct from `tests/adapters/sandbox-executor.test.ts` (which proves `execute_rust`, a
  combined compile+run, alone): this test dispatches `compile_rust` and `execute_rust` as two
  SEPARATE real `rustc`-then-binary subprocess calls, matching the ticket's own compile-then-
  execute steps. `fn main() { println!("{}", 1 + 1); }` → real `compile_rust` exit 0, real
  `execute_rust` `stdout.trim() === "2"`, `exitCode === 0`.
- Negative test: real Rust type error (`let x: i32 = "not a number";`). Independently verified
  against a bare `rustc` invocation before wiring the assertion:
  ```
  error[E0308]: mismatched types
   --> main.rs:1:26
    |
  1 | fn main() { let x: i32 = "not a number"; println!("{}", x); }
    |                    ---   ^^^^^^^^^^^^^^ expected `i32`, found `&str`
  ```
  Test asserts `compiled.exitCode !== 0`, `compiled.stderr` contains `"error[E0308]"` and
  `"mismatched types"` — rustc's real diagnostic text, not fabricated.
- Language-parameterization proof (third test): the same `getSandboxExecutor()` instance handles
  both `compile_python` and `compile_rust` in the same test, both succeeding — proves the sandbox
  is not Python-only.
- Full-suite regression check: `npx vitest run` → 85/85 passed. `npx tsc --noEmit` → clean.
