# TICKET-D026: Tamper detection: real coverage exists, but narrower than TICKET-049's BLAKE3 hash-divergence target

## Status

DEFERRED — verification is real but narrower than the property its ticket's prose implies

## Priority

P1 — verification gap for a security-relevant claim (Architecture Decision 12, independent revalidation on replay); no hash-based detection exists anywhere in the codebase yet

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-025-... (replay.test.ts), target ticket TICKET-049 (tamper detection scenario, PLANNED)`

## Source

- Research report: Thin Verification Report, finding 1
- Citation: `tests/domain/replay.test.ts` (TICKET-025) has two real tamper cases — a single-field mutation of one event's `targetPhase` to an illegal transition, and a truncation tamper (dropping the last event). Both pass and assert on genuinely different replay outcomes, not vacuous (Thin Verification Report, finding 1).
- Citation: It's a narrower slice of TICKET-049's actual target: TICKET-049 wants tampering with a persisted event log (including captured stdout/execution artifacts) detected via BLAKE3 hash divergence on replay, surfaced through `replay-failure-presentation.tsx`. `replay.test.ts` never touches a checksum — it asserts only on reducer-level status/phase.
- Citation: No fuzzing, no multi-field/simultaneous mutation, no mutation of `family` itself, no hash-based detection anywhere in the codebase (`grep tamper` across `lib/` and `tests/` turns up only `replay.ts`/`replay.test.ts`).

## Objective

Widen tamper-detection coverage from `replay.test.ts`'s reducer-level status/phase assertions to TICKET-049's actual target: BLAKE3 hash divergence on a tampered persisted event log, surfaced through `replay-failure-presentation.tsx`, once TICKET-038/032/048 exist to support it.

## Current state

`replay.test.ts` has 2 real, passing tamper cases (single-field mutation, truncation), but both assert only on reducer-level `status`/`phase` — no checksum is ever computed or compared, and no fuzzing/multi-field mutation exists.

## Target state

A real test persists an event log, computes its real BLAKE3 hash (TICKET-038), tampers one field in the persisted (not in-memory) log, replays it, recomputes the hash, and asserts divergence — with the divergence surfaced via `replay-failure-presentation.tsx`, per TICKET-049's actual acceptance criteria.

## Projection classification

- Template: N/A — this is a test-coverage-widening ticket, following the same test-AUTHORING classification workstream I's own tickets already use
- Domain data: none new — widened assertions still reference generated types/RDF-sourced values, never restate them as new literals, per the existing tickets' own Domain-data responsibility pattern
- Custom code: matches the relevant workstream I ticket's own 60/40 classification, since this is the same kind of real-collaborator test authoring

## Inputs

- the existing real, passing test file this ticket widens (see Source)
- the real collaborator(s) already used by that test

## Outputs

- an updated or additional test file widening coverage to the specific narrower-than-target gap named in Source

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

Reuses the existing shared test-harness/fixture patterns already established by the relevant workstream I ticket.

## Domain-data responsibility

Assertions continue to reference RDF-sourced generated types/values rather than introducing new domain literals.

## Custom-code boundary

The test itself, and any real external process it drives — no new production custom code introduced by this ticket, only wider test coverage of existing adapters/logic.

## Exclusions

- no mocked core collaborator, per this repo's Chicago TDD policy
- no assertion that merely counts generated files or checks a generated string for non-emptiness — must assert on real observable state
- no claiming the underlying claim is now fully verified when only the specific narrower gap named in Source has been closed — other narrower-than-target gaps in the same ticket may remain

## Implementation steps

1. Confirm TICKET-038 (BLAKE3 checksum adapter) and TICKET-032 (replay-failure-presentation) exist and are real.
2. Author a test that persists a real event log via TICKET-036's persistence adapter, computes its real BLAKE3 hash.
3. Directly mutate a persisted field (e.g. a captured stdout value) bypassing the application, simulating real tampering of durable state.
4. Replay via TICKET-025's reducer, recompute the hash via TICKET-038, and assert divergence, with `replay-failure-presentation.tsx` rendering the explicit tamper state.
5. Add a fuzzed/multi-field mutation variant and a mutation of the `family` field itself, extending beyond the current single-field/truncation cases.

## Admission gates

- TICKET-025-...

## Acceptance criteria

Given a tampered persisted event log, when replayed, then the recomputed BLAKE3 hash differs from the original, and `replay-failure-presentation.tsx` displays an explicit tamper-detected state — matching TICKET-049's actual acceptance criteria, not merely a reducer-level status difference.

## Negative tests

Given an untampered persisted log, when replayed, then the recomputed hash matches exactly — the companion positive test proving the detector isn't just always reporting tamper.

## Verification ladder

- Unit: widened real-collaborator unit test, per the specific gap named in Source
- Integration: composed with the real system components the existing test already uses
- End-to-end: N/A with reason — matches the existing test's own layer, typically unit/integration not e2e
- Chaos: N/A with reason — not the gap this ticket targets
- Stress: N/A with reason — not the gap this ticket targets unless explicitly named in Source
- Benchmark: N/A with reason
- Verifier report: widened test transcript, real collaborator, no mocks

## Receipts

- widened test file hash
- real-collaborator test transcript

## Dependencies

- TICKET-025-... (replay.test.ts), target ticket TICKET-049 (tamper detection scenario, PLANNED)

## Falsifier

If the widened test still exercises only the same single instance/case as before, or introduces a mock in place of the real collaborator, this ticket is not complete.

## Handoff

Feeds the relevant workstream I vertical scenario's own eventual closure, once it is picked up.

## Definition of done

- test widened per the specific gap named in Source
- real collaborator used throughout, no mocks
- negative case included where applicable
