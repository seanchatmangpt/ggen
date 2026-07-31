# TICKET-040: Vertical scenario: Bootstrap

## Status

PARTIAL_ALIVE — real reducer/RDF-generated-table evidence proven and passing; the ticket's own
stated primary verification layer (Playwright against a real running dev server / real `next
build`) is currently blocked by a real, reproduced production regression outside this ticket's
scope (see notes)

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl (<phase/created>)`
- Acceptance-test steps:
  - `acceptance-step/1 (build the system)`
  - `acceptance-step/2 (create the sandbox)`
- PRD requirements: (none)
- ARD components: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Prove the application builds and a fresh session reaches phase/created via a real `next build` + real sandbox-creation capability call, corresponding to acceptance-step/1 and acceptance-step/2.

## Current state

UNKNOWN — no implementation exists yet; this ticket verifies the composed system once workstreams B-H land.

## Target state

A passing Playwright/Chicago-TDD test exercising the real composed system for this scenario, using real collaborators: real `next build` process, real capability/session/create-session dispatch. No mocked core collaborator.

## Projection classification

- Template: 60%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 40%
- Expected ratio: 60/40
- Custom-code justification: verification tickets are mostly test AUTHORING (a form of template-adjacent reusable test-harness code) composed with real system components; classified with a higher custom fraction than pure projection tickets because test scenario logic requires human design judgment about what to assert, though it contains zero domain data duplication — assertions reference generated types/RDF-sourced values, never restate them.

## Inputs

- the full composed system as of this scenario's dependencies

## Outputs

- examples/interview-assist/tests/e2e/bootstrap.spec.ts

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

1. Run a real `next build` (or `next dev` for the test target) against the generated application.
2. Call capability/session/create-session through the real dispatch table.
3. Assert the resulting SessionState.phase equals phase/created.

## Admission gates

- All ticket IDs in Dependencies complete.

## Acceptance criteria

- Given a fresh build, when create-session is dispatched, then SessionState.phase is exactly phase/created, sourced from the real reducer, not a hand-set fixture.

## Negative tests

- Attempt to dispatch any capability before create-session and confirm it's refused (no session exists yet) rather than silently creating implicit state.

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

- TICKET-011
- TICKET-023
- TICKET-035

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

- **Playwright-vs-vitest substitution, honestly disclosed (applies to TICKET-040 through
  TICKET-047 uniformly, restated once here in full, cross-referenced by the other 7 tickets'
  notes):** a real `npx next build` against `examples/interview-assist/` (run 2026-07-23) FAILS:
  ```
  ▲ Next.js 16.2.11 (Turbopack)
    Creating an optimized production build ...
  Build error occurred
  Error: Turbopack build failed with 1 errors:
  ./app/page.tsx
  Code generation for chunk item errored
  ...the chunking context (unknown) does not support external modules (request: node:module)
  ```
  Root cause, traced by reading the real import graph: `app/page.tsx` (`"use client"`) imports
  `lib/domain/reducer.ts`, which unconditionally imports `lib/domain/receipt-emitter.ts`
  (TICKET-055), which imports `lib/adapters/checksum-adapter.ts`, whose top-level
  `import { createRequire } from "node:module"` is a real Node builtin that cannot be bundled
  into a browser chunk. `checksum-adapter.ts`'s own module doc only ever audited this import
  against SERVER route bundling (`app/api/receipt/route.ts`); TICKET-055's later wiring of
  `reducer.ts -> receipt-emitter.ts -> checksum-adapter.ts` is what newly drags it into
  `page.tsx`'s CLIENT bundle. `npx next dev` reproduces the same failure at request time (`GET /
  500`, identical `node:module` chunking error, captured in the real dev-server log). This is a
  real, reproduced regression — **not** an environment/installation gap: Playwright itself is
  installed and working (`npx playwright --version` → `1.61.1`; `chromium-1181` present under
  `~/Library/Caches/ms-playwright`). Fixing this bundling bug is outside every one of these 8
  tickets' own stated Custom-code boundary ("no new production custom code introduced by this
  ticket, only test code exercising existing adapters"), so it is reported here unfixed rather
  than silently patched or silently ignored. This corrects an earlier claim (passed into this
  session) that `app/page.tsx` was already "next-build-clean" — it is `tsc --noEmit`-clean
  (reverified this session, zero errors) but **not** `next build`-clean; the two are not the same
  claim and should not have been conflated.
  Per this workflow's explicit fallback clause, all 8 scenarios below are authored as real
  vitest tests against real collaborators (real reducer, real RDF-generated tables, real
  subprocess execution) instead of browser-driven Playwright specs.
- File: `examples/interview-assist/tests/scenarios/bootstrap.test.ts` (5 tests). Real run:
  `npx vitest run tests/scenarios/bootstrap.test.ts` → 5/5 passed, 3ms.
    sha256: `0c06db98b57949e9a2c7437da03d448c7a390aadffd7c6f236571afc842c5dc1`
- Acceptance criterion: dispatched a real `{family: "SessionEvent", type: "create-session"}`
  event through the real `sessionReducer` (TICKET-023) from a fresh `{phase: "CREATED"}` state —
  admitted, `result.value.phase === "CREATED"`, sourced from the reducer's own return value.
- Also verified `capability/session/create-session` is a real, present key in the RDF-generated
  `CAPABILITY_DISPATCH` table (TICKET-027, `HTTP_CAPABILITY_COUNT === 9`) — the real dispatch slot
  the ticket names exists, even though (disclosed, not in this ticket's scope) no adapter handler
  is wired into it yet.
- Negative tests (both real, both passing): `checkPreconditions("capability/session/start-interview",
  new Set())` and `checkPreconditions("capability/session/join-session", new Set())` — both real
  TICKET-028 `dcterms:requires`-derived edges — return `met: false`, `missing` containing
  `"capability/session/create-session"`. A positive control (`checkPreconditions(..., new
  Set(["capability/session/create-session"]))` → `met: true`) proves the check is a real gate,
  not an always-false stub.
- Full-suite regression check: `npx vitest run` (whole `examples/interview-assist/` suite,
  including this file) → **85/85 passed**, 24 test files, 11.78s wall clock. `npx tsc --noEmit` →
  clean, zero errors.
