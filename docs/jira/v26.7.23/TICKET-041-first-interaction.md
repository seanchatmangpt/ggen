# TICKET-041: Vertical scenario: First interaction

## Status

PARTIAL_ALIVE — real reducer/replay/transition-table evidence proven and passing; Playwright
layer blocked by the real `next build` regression documented in TICKET-040's notes (same root
cause, not restated in full here)

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl (<transition-plan/created-to-preparing>, <transition-plan/preparing-to-ready>)`
- Acceptance-test steps: (none)
- PRD requirements: (none)
- ARD components: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Prove the first real event (identify-participant-roles) legally advances the session from CREATED through PREPARING to READY per TICKET-021's transition table.

## Current state

UNKNOWN — no implementation exists yet; this ticket verifies the composed system once workstreams B-H land.

## Target state

A passing Playwright/Chicago-TDD test exercising the real composed system for this scenario, using real collaborators: real reducer, real dispatch table. No mocked core collaborator.

## Projection classification

- Template: 60%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 40%
- Expected ratio: 60/40
- Custom-code justification: verification tickets are mostly test AUTHORING (a form of template-adjacent reusable test-harness code) composed with real system components; classified with a higher custom fraction than pure projection tickets because test scenario logic requires human design judgment about what to assert, though it contains zero domain data duplication — assertions reference generated types/RDF-sourced values, never restate them.

## Inputs

- the full composed system as of this scenario's dependencies

## Outputs

- examples/interview-assist/tests/e2e/first-interaction.spec.ts

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

1. Dispatch capability/session/identify-participant-roles.
2. Assert phase transitions CREATED -> PREPARING -> READY per the real transition table, not an assumed sequence.

## Admission gates

- All ticket IDs in Dependencies complete.

## Acceptance criteria

- Given the real event sequence, when dispatched, then the final phase is phase/ready, matching transition-plan/preparing-to-ready's schema:result exactly.

## Negative tests

- Attempt to skip directly to phase/ready without the intermediate PREPARING event and confirm the reducer refuses the illegal jump.

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
- TICKET-021

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
  evidence (`next build`/`next dev` both fail with a reproduced `node:module` client-bundling
  error, traced to `checksum-adapter.ts` reaching `page.tsx`'s client bundle via
  `reducer.ts -> receipt-emitter.ts`). Authored as a real vitest test instead.
- File: `examples/interview-assist/tests/scenarios/first-interaction.test.ts` (3 tests). Real
  run: `npx vitest run tests/scenarios/first-interaction.test.ts` → 3/3 passed, 2ms.
    sha256: `4e732e31664526ccc43c91f455a3ed493ccba4b417b7f1522b818e99112ce062`
- Acceptance criterion: real event log
  `[{family:"ParticipantEvent", type:"identify-participant-roles"}, {family:"WorkflowEvent",
  targetPhase:"PREPARING"}, {family:"WorkflowEvent", targetPhase:"READY"}]` folded through the
  real `replaySession` (TICKET-025, which itself folds the real `sessionReducer`) — admitted,
  final `phase === "READY"`. Cross-checked against the real generated
  `PHASE_TRANSITIONS["CREATED"] === ["PREPARING"]` and `PHASE_TRANSITIONS["PREPARING"] ===
  ["READY"]` (TICKET-021) rather than restating those edges as independent literals.
- Negative test 1: `sessionReducer({phase:"CREATED"}, {family:"WorkflowEvent",
  targetPhase:"READY"})` (direct skip, no intermediate PREPARING) → refused,
  `code === "STALE_SESSION_EVENT"`; cross-checked against the real `isLegalTransition("CREATED",
  "READY") === false`.
- Negative test 2 (replay form): the same illegal jump placed mid-log, followed by an event that
  WOULD be legal from CREATED — `replaySession` halts at the refusal (per TICKET-025's documented
  "does not continue folding past a refusal" behavior) and never reaches the trailing legal event.
- Full-suite regression check: `npx vitest run` → 85/85 passed (see TICKET-040's notes for the
  full run). `npx tsc --noEmit` → clean.
