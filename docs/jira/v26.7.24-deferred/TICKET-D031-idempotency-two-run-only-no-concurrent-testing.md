# TICKET-D031: Idempotency is verified via exactly 2 consecutive syncs, repeated per-ticket, but never a 3rd+ run and never concurrent writers

## Status

DEFERRED — verification is real but narrower than the property its ticket's prose implies

## Priority

P2 — hardening opportunity, not an active risk today — TICKET-001's own Chaos row already names 'no concurrent writers to this manifest yet' as an explicit, disclosed scope cut

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-010/017/018/030-... (projection idempotency checks)`

## Source

- Research report: Thin Verification Report, finding 6
- Citation: Every idempotency claim found (TICKET-010, 017, 018, 030) uses the identical method: run `ggen sync run` twice and compare whole-tree (or single-file) SHA-256/output hashes for byte-identity (Thin Verification Report, finding 6).
- Citation: TICKET-010 states this directly and marks itself PARTIAL_ALIVE, not ALIVE, precisely because "idempotency has only been exercised for 1 of the eventual N templates this pack will carry... re-verify as each lands" — the method is reapplied at each subsequent ticket landing, but always as a fresh 2-run pair, never accumulated into a 3+-run sequence in one sitting.
- Citation: No test runs a 3rd, 4th, or Nth consecutive sync to check for slow drift. No test simulates a concurrent or interrupted sync — and TICKET-001's own Chaos row states this is out of scope by name: "N/A with reason — no concurrent writers to this manifest yet," an explicit, not merely accidental, gap.

## Objective

Once D012 (full-pack idempotency re-verification) lands, extend at least one representative idempotency check to a 3rd/4th/Nth consecutive sync (checking for slow drift the 2-run method cannot catch) and add a real concurrent-or-interrupted-sync test, closing the gap TICKET-001's own Chaos row already names.

## Current state

Every idempotency check in the corpus (TICKET-010/017/018/030) runs exactly 2 consecutive syncs and diffs the result. No test has ever run a 3rd+ consecutive sync, and no test simulates two concurrent `ggen sync run` processes or an interrupted-then-resumed sync.

## Target state

At least one representative template's idempotency is verified across N (N ≥ 4) consecutive syncs with no observed drift, and a real test exercises either a concurrent-sync race or a kill-mid-write-then-resync scenario, with the observed outcome (safe/unsafe) documented honestly.

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

1. Select a representative template already covered by the 2-run check (e.g. TICKET-010's original 1 template).
2. Run `ggen sync run` 4+ times consecutively, diffing each run against the first, to check for slow drift the 2-run method structurally cannot detect.
3. Design and run a real concurrent-sync test: launch two `ggen sync run` processes against the same pack simultaneously and observe the actual outcome (race condition, lock contention, or safe serialization).
4. Design and run a real interrupted-sync test: kill a `ggen sync run` process mid-write and confirm a subsequent re-run either completes cleanly or fails loudly, never leaving silently-corrupted output.
5. Update TICKET-001's Chaos row from 'N/A with reason — no concurrent writers to this manifest yet' to reflect the new real finding, once this ticket's concurrent-sync test exists.

## Admission gates

- TICKET-010/017/018/030-...

## Acceptance criteria

Given N ≥ 4 consecutive `ggen sync run` invocations against the same template, when each output is diffed against the first, then no drift is observed across any run — not merely the first 2. Given two concurrent `ggen sync run` processes, when both complete, then the observed outcome (safe serialization, detected race, or documented failure mode) is recorded honestly.

## Negative tests

If the concurrent-sync test reveals an actual race condition or silent corruption, that is itself the finding this ticket exists to surface — it must be documented and tracked as a new ticket, not silently patched away without a record.

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

- TICKET-010/017/018/030-... (projection idempotency checks)

## Falsifier

If the widened test still exercises only the same single instance/case as before, or introduces a mock in place of the real collaborator, this ticket is not complete.

## Handoff

Feeds the relevant workstream I vertical scenario's own eventual closure, once it is picked up.

## Definition of done

- test widened per the specific gap named in Source
- real collaborator used throughout, no mocks
- negative case included where applicable
