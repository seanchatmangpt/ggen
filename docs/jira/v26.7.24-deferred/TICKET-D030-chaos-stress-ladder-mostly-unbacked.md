# TICKET-D030: Chaos/Stress verification-ladder rows are mostly N/A, and several non-N/A rows have no corresponding real test

## Status

DEFERRED — verification is real but narrower than the property its ticket's prose implies

## Priority

P2 — test-ladder accuracy/honesty gap — documentation debt about what verification actually exists, not itself a runtime risk

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/multiple tickets (TICKET-029, 034-039, 040-053, 023, 025)`

## Source

- Research report: Thin Verification Report, finding 5
- Citation: Chaos: 36 of 57 tickets say "N/A with reason." Of the remaining 21: 7 (TICKET-029, 034-039) claim the identical line "kill/interrupt the real collaborator mid-operation and confirm graceful, receipted refusal"; 14 (workstream-I scenarios, 040-053) say only "where relevant to the scenario, noted in steps above" — a deferral, not a commitment, and none of those 14 are authored yet (Thin Verification Report, finding 5).
- Citation: Of the 7 adapter tickets claiming a real chaos rung, a genuinely corresponding test was confirmed for only 2: TICKET-029 (`timeout-wrapper.test.mjs`) and TICKET-035 (the sandbox orphan-kill test). The other 4 (034 monaco, 036 persistence, 037 ollama, 038 checksum) have zero chaos-relevant keywords anywhere in their test files.
- Citation: Stress: 55 of 57 say "N/A with reason." Only 2 claim a real stress rung — TICKET-023 and TICKET-025. Neither exists: `reducer.test.ts` has 6 tests, none a loop/perf test; `replay.test.ts` operates only on the same 4-event log throughout. The honest count for Stress is 0 of 57 backed by a real test, not 2 — the 2 non-N/A rows are currently just prose.

## Objective

Correct the Chaos/Stress verification-ladder rows across the affected tickets (TICKET-023, 025, 034, 036, 037, 038) to honestly reflect no backing test exists yet, OR author the real tests those rows currently only assert in prose.

## Current state

4 of 7 tickets claiming a real Chaos rung (034, 036, 037, 038) have zero chaos-relevant test coverage despite the identical prose line. Both tickets claiming a real Stress rung (023, 025) have zero corresponding loop/perf test — the honest count is 0 of 57, not 2.

## Target state

Either (a) each of the 6 affected tickets' verification-ladder Chaos/Stress rows is corrected to 'N/A with reason — not yet authored' matching reality, or (b) the real tests are authored, closing the gap between the ladder's claim and the actual test file for each.

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

1. For TICKET-034/036/037/038: either author a real chaos test (kill/interrupt the real collaborator mid-operation, per the shared prose line) or correct the ladder row to state honestly that none exists yet.
2. For TICKET-023: either author a real stress test (reducer called in a tight loop across all 14x14 phase pairs, per the ticket's own prose) or correct the row.
3. For TICKET-025: either author a real stress test (replay over a long event log for perf sanity, per the ticket's own prose) or correct the row.
4. Re-audit the 21 non-N/A Chaos rows and 2 non-N/A Stress rows as a batch, since this is a systemic pattern, not 6 isolated incidents.

## Admission gates

- multiple

## Acceptance criteria

Given each ticket's verification-ladder Chaos/Stress row, when compared against its actual test file, then the row's claim matches reality — either a real corresponding test exists, or the row is honestly marked N/A with the actual reason (not yet authored), never asserting a test that doesn't exist.

## Negative tests

Grep each of the 6 affected tickets' test files for chaos/stress-relevant keywords (kill/SIGKILL/SIGTERM/abort/interrupt/crash for Chaos; loop/perf/timing for Stress) and confirm the presence or honest absence matches the corrected ladder row.

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

- multiple tickets (TICKET-029, 034-039, 040-053, 023, 025)

## Falsifier

If the widened test still exercises only the same single instance/case as before, or introduces a mock in place of the real collaborator, this ticket is not complete.

## Handoff

Feeds the relevant workstream I vertical scenario's own eventual closure, once it is picked up.

## Definition of done

- test widened per the specific gap named in Source
- real collaborator used throughout, no mocks
- negative case included where applicable
