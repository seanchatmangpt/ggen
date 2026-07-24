# TICKET-D010: Persistence adapter's real browser IndexedDB/localStorage behavior is never exercised (Node filesystem substitute only)

## Status

DEFERRED — environment-dependent, blocked on D008 (Playwright harness)

## Priority

P1 — persistence semantics (quota limits, eviction policy, storage-permission prompts) differ materially between filesystem and real browser storage; session-data durability claims are unverified against the actual target runtime

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-036-local-persistence-adapter.md`

## Source

- Research report: Disclosed Gaps Catalog, section (b) Environment-dependent gaps
- Citation: "PARTIAL_ALIVE — real filesystem-backed persistence implemented and tested; real browser IndexedDB/localStorage NOT exercised (documented Node-side substitution)" — `docs/jira/v26.7.23/TICKET-036-local-persistence-adapter.md:5`
- Citation: "HONEST SUBSTITUTION, stated up front in the source and here: this runs in a Node/Vitest test context, not a browser, so there is no real `window.indexedDB`. Implemented against the real filesystem (`node:fs/promises`... against a real temp directory created with `mkdtemp`) as the documented Node-side stand-in — not a claim of real IndexedDB." — `docs/jira/v26.7.23/TICKET-036-local-persistence-adapter.md:129-132`

## Objective

Once D008's Playwright harness runs, exercise the persistence adapter's real `window.indexedDB`/`localStorage` code path in a real browser, replacing (or supplementing alongside) the documented Node-filesystem substitute.

## Current state

TICKET-036's persistence logic is tested against a real temp directory via `node:fs/promises` as an explicitly documented Node-side stand-in for `window.indexedDB`. No test has run against a real browser's actual storage APIs.

## Target state

A real Playwright test exercises the adapter against a real browser's `window.indexedDB` (or `localStorage`, per the adapter's actual implementation choice), confirming write/read/eviction behavior matches what the Node-filesystem tests assumed.

## Projection classification

- Template: N/A — verification execution only
- Domain data: none new
- Custom code: N/A — no new production code unless the browser-specific code path needs adjustment once tested for real

## Inputs

- `examples/interview-assist/lib/adapters/persistence-adapter.ts` (already authored)
- D008's working Playwright harness

## Outputs

- a passing real-browser persistence test
- updated TICKET-036 Status header if the real-browser behavior diverges from the Node-substitute assumption and requires a fix

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

None — adapter logic already authored by TICKET-036.

## Domain-data responsibility

None.

## Custom-code boundary

None new, unless real-browser testing surfaces a divergence requiring an adapter fix — in which case the fix stays scoped to the adapter's existing custom-code boundary (irreducible browser-API interaction).

## Exclusions

- no claiming real-browser persistence 'works' based on the filesystem-substitute tests alone
- no silent removal of the honest Node-substitute disclosure from TICKET-036's text until this ticket actually supersedes it with real evidence

## Implementation steps

1. Confirm D008's Playwright harness is running.
2. Write a Playwright test that writes a session record via the real browser's `window.indexedDB`/`localStorage`, reloads the page, and confirms the record persists.
3. Test a storage-quota-adjacent edge case if reasonably reachable (e.g. writing a large-but-legal payload) to confirm no silent truncation.
4. Compare observed real-browser behavior against what the Node-filesystem substitute tests assumed; document any divergence and fix the adapter if warranted.

## Admission gates

- D008
- TICKET-036

## Acceptance criteria

Given a real browser tab, when a session record is persisted via the adapter and the page reloads, then the record is read back correctly from real browser storage — not the Node-filesystem substitute.

## Negative tests

Clear the browser's storage for the origin and confirm the adapter correctly reports 'no persisted session' rather than a stale in-memory fallback masking the clear.

## Verification ladder

- Unit: N/A with reason — already covered by TICKET-036's Node-substitute unit tests
- Integration: persistence adapter composed with the real app shell in a real browser
- End-to-end: the primary layer this ticket delivers — real browser storage round-trip
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: real Playwright transcript showing write/reload/read round-trip

## Receipts

- Playwright transcript
- documented divergence notes (if any) between Node-substitute and real-browser behavior

## Dependencies

- D008
- TICKET-036

## Falsifier

If TICKET-036 is marked ALIVE for real-browser persistence without a transcript from an actual `window.indexedDB`/`localStorage` round-trip in a real browser, this ticket is not complete.

## Handoff

Feeds D026 (tamper detection) and TICKET-048/049's persistence-and-replay vertical scenarios, which assume durable real-browser storage.

## Definition of done

- real browser write/reload/read round-trip test passes
- any divergence from the Node-substitute assumption documented and fixed
- TICKET-036 Status header updated if warranted
