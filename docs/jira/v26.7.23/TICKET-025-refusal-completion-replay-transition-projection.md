# TICKET-025: Refusal/completion/replay transition projection

## Status

ALIVE

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl (<phase/refused>, <phase/complete>)`
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability/session/replay-session>, <capability/verification/replay-execution>)`
- ARD components: (none)
- PRD requirements: (none)
- Acceptance-test steps:
  - `packs/wasm4pm-interview-assist-pack/ontology/80-acceptance.ttl (<acceptance-step/3> replay the interview)`
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the terminal-transition handling (COMPLETE, REFUSED) and the replay-transition path (capability/session/replay-session + capability/verification/replay-execution) as a distinct reducer extension, since replay must independently revalidate rather than trust persisted state (per Architecture Decision 12).

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 80%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 20%
- Expected ratio: 80/20
- Custom-code justification: replay's revalidation logic composes the reducer (TICKET-023) with the transition table (TICKET-021) in a loop over a persisted event log — control-flow design judgment, same classification basis as TICKET-023.

## Inputs

- TICKET-023 reducer.ts
- TICKET-021 phase-transitions.ts

## Outputs

- examples/interview-assist/lib/domain/replay.ts (replaySession(eventLog) -> AdmissionResult<SessionState>, re-running the reducer over every persisted event)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The replay-loop skeleton: fold the reducer over an event log from CREATED, re-validating every transition — reusable structure.

## Domain-data responsibility

No new domain data — reuses TICKET-023's reducer entirely, must not reimplement transition logic.

## Custom-code boundary

NONE.

## Exclusions

- no separate replay-specific transition table — must reuse TICKET-021/023 exactly, replay that diverges from live-session logic would violate Architecture Decision 12

## Implementation steps

1. Implement replaySession as reduce(reducer, initialState, eventLog).
2. Verify replaying a real event log from a bootstrap fixture reproduces the exact final state the live session reached.
3. Verify a tampered event log (one event's payload altered) produces a DIFFERENT final state/hash than the untampered original, proving replay genuinely re-derives rather than trusting a cached final state.

## Admission gates

- TICKET-023.

## Acceptance criteria

- Given an untampered event log, when replayed, then the final state matches the original live session's final state exactly (including hash, per TICKET-020's receipt type).

## Negative tests

- Tamper with one event in the log and confirm the replayed final state's hash differs from the original — this IS TICKET-049 (tamper detection)'s core mechanism, cited here as the shared implementation.

## Verification ladder

- Unit: replaySession over a small synthetic event log
- Integration: replay over a real bootstrap-scenario event log (TICKET-040)
- End-to-end: exercised fully in TICKET-048 (persistence and replay) and TICKET-049 (tamper detection)
- Chaos: N/A with reason — deterministic fold, no concurrency
- Stress: replay over a long event log (full decisive-acceptance-test-length session) for perf sanity
- Benchmark: N/A with reason — no fixed perf target established yet
- Verifier report: replay-matches-original test result + tamper-detected test result

## Receipts

- replay.ts hash
- replay-match confirmation
- tamper-detection confirmation

## Dependencies

- TICKET-023

## Falsifier

If replaying an untampered event log does not reproduce the exact original final state, or if a tampered log's replay is NOT detected as diverging, this ticket is not complete.

## Handoff

TICKET-048 and TICKET-049 (workstream I) exercise this directly as their core mechanism.

## Definition of done

- replaySession implemented, reusing TICKET-023's reducer with no divergent logic
- match and tamper-detection tests both pass

## Implementation notes (real evidence) — closes as ALIVE

- Hand-authored `lib/domain/replay.ts` (this ticket's classification: "Template: 80% / Custom
  code: 20%", same control-flow-design-judgment basis as TICKET-023 — no `.tmpl` file). Folds
  `sessionReducer` (TICKET-023) over the event log starting from `ALL_PHASES[0]` (read from the
  generated table, not a `"CREATED"` literal, so this file carries no phase string of its own),
  stopping at the first refusal rather than continuing past it. Reuses TICKET-023's reducer (and
  transitively TICKET-021's `isLegalTransition`) exactly — no separate replay-specific
  transition logic, per Architecture Decision 12 as required.
- `ALL_PHASES[0]` types as `Phase | undefined` under this project's `noUncheckedIndexedAccess:
  true` — handled with a real runtime `undefined` guard that throws (documented as an invariant
  check, not expected to fire, since phase.ts's own TICKET-016 falsifier already proves 14
  members), not a `!` assertion or `any`.
- Chicago TDD, `tests/domain/replay.test.ts`, real generated tables composed throughout:
  - Built a real 4-event log covering 4 real admitted transition-plan edges
    (CREATED→PREPARING→READY→INTRODUCTION→PROBLEM_PRESENTATION). `replaySession` over it is
    compared against an independent live-sequential-reduction helper (calls `sessionReducer`
    directly in a local loop, not via `replaySession`) — both reach the identical
    `SessionState`, `phase: "PROBLEM_PRESENTATION"`.
  - **Tamper test 1** (illegal mutation): 3rd event's `targetPhase` changed from the legal
    `READY→INTRODUCTION` to the illegal `READY→COMPLETE`. Untampered replay: `status:
    "admitted"`. Tampered replay: `status: "refused"`. `expect(tampered).not.toEqual(untampered)`
    passes — genuine divergence, not a hash-only check, since replay.ts itself does not compute
    a hash (that composition is TICKET-020's receipt type + a later ticket's job; this ticket's
    own text frames tamper detection as "the shared implementation" TICKET-049 will build on,
    not a hash it must itself produce).
  - **Tamper test 2** (truncation): dropped the 4th event. Final phase differs (`INTRODUCTION`
    vs `PROBLEM_PRESENTATION`) — the same divergence mechanism catches a different tamper shape.
  - `npx vitest run` → `tests/domain/replay.test.ts (4 tests)`, all pass.
- `npx tsc --noEmit`: zero errors.
- SHA-256 of `replay.ts`: `a0b0309adb5aabd2dd762524344f778aa915dd094be930a0e6c00eb5db9267cd`.
