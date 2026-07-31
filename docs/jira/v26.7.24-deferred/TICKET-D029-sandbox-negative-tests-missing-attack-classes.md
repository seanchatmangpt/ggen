# TICKET-D029: Subprocess sandbox negative tests cover 2 real attack classes; fork bombs, disk-fill, memory exhaustion, and network egress are untested because no defense exists yet

## Status

DEFERRED — verification is real but narrower than the property its ticket's prose implies

## Re-verification (2026-07-24)

**Still true, unchanged.** Read `tests/adapters/sandbox-executor.test.ts` directly, current
version: exactly 6 tests (this ticket's guess that TICKET-035's "5" was stale is now confirmed —
6 is correct and has not grown). Re-ran this ticket's own verification method: `grep -rniE
"fork.?bomb|disk.?fill|memory.?exhaust|network.?egress|ulimit|cgroup"` across
`examples/interview-assist/tests/` — 0 hits. The 6 tests remain: real python3 execution, real
rustc compile+execute, timeout-kill with a real `ps ax` orphan check, policy-denial-without-spawn
(now against the real TICKET-028 checker rather than the stub — see D005 — but still exercising
policy logic, not sandbox-escape resistance), `no_source_provided` refusal, and one
path-traversal workspace-escape refusal. Fork bombs, disk-fill, memory exhaustion, and network
egress remain completely untested, for the same reason this ticket already states: D001/D015's
isolation boundary still does not exist for a test to exercise (this ticket's own step 1,
"Confirm D001/D015's isolation boundary has landed," is not met — see those tickets'
re-verifications above).

## Priority

P0 — directly tied to D001/D015/D022 — these are exactly the attacks the missing isolation boundary would need to defend against; no test exists because no isolation mechanism exists to test

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-035-subprocess-sandbox-executor.md (sandbox-executor.test.ts)`

## Source

- Research report: Thin Verification Report, finding 4
- Citation: `tests/adapters/sandbox-executor.test.ts` has 6 tests (TICKET-035's notes say "5," likely stale relative to the current file): real python3/rustc execution, a real timeout-kill with a genuine `ps ax` orphan-process check, real policy-denial-without-spawn, `no_source_provided` refusal, and workspace-escape refusal via exactly one path-traversal pattern (Thin Verification Report, finding 4).
- Citation: TICKET-035's own "Implementation notes" discloses the gap directly, unprompted: "no network or filesystem isolation beyond the write-side workspace-escape check — a submitted program still runs as the same OS user as the host process."
- Citation: Fork bombs, disk-fill, memory exhaustion, and network egress from inside the sandboxed subprocess are not merely untested — the ticket states outright that no isolation mechanism exists for them yet, so a test couldn't currently pass against a hardened boundary even if written.

## Objective

Once D001/D015's real isolation boundary exists, add real negative tests for the 4 named-but-untested attack classes (fork bomb, disk-fill, memory exhaustion, network egress) to `sandbox-executor.test.ts`, closing the gap this same ticket already discloses honestly.

## Current state

`sandbox-executor.test.ts` has 6 real tests covering timeout-kill, policy-denial, empty-files, and single-path-traversal workspace-escape. Fork bombs, disk-fill, memory exhaustion, and network egress have zero test coverage, because — per TICKET-035's own disclosure — no isolation mechanism exists yet to contain them.

## Target state

4 new real negative tests exist: (1) a real fork-bomb program is contained without exhausting host process table resources, (2) a real disk-fill attempt is capped/denied, (3) a real memory-exhaustion attempt is capped/denied, (4) a real network-egress attempt from inside the sandbox is denied — each verifiable via real observation (process count, disk usage, memory usage, network connection attempt), not a code-review claim.

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

1. Confirm D001/D015's isolation boundary has landed — testing these attacks against the current unisolated boundary would only re-confirm the already-known, already-disclosed gap.
2. Write a real fork-bomb test program and confirm the sandbox's isolation boundary contains it (e.g. via a real process-count or cgroup-limit check), not merely a timeout.
3. Write a real disk-fill test program and confirm it is capped before exhausting host disk space.
4. Write a real memory-exhaustion test program and confirm it is capped before exhausting host memory.
5. Write a real network-egress attempt (e.g. opening a socket to an external address) and confirm it is denied at the isolation boundary, verifiable via a real connection-attempt failure, not a mocked network layer.

## Admission gates

- TICKET-035-subprocess-sandbox-executor.md

## Acceptance criteria

Given each of the 4 named attack-class programs, when executed inside the sandbox, then the isolation boundary contains or denies the attack, verified via a real observable signal (process count, disk usage, memory usage, or connection failure) specific to that attack class.

## Negative tests

Run each attack-class program WITHOUT the isolation boundary active (e.g. against a deliberately-reverted config) first, to confirm the test would actually fail without the defense — proving these are real regression-catching tests, not tests that pass regardless of the isolation boundary's presence.

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

- TICKET-035-subprocess-sandbox-executor.md (sandbox-executor.test.ts)

## Falsifier

If the widened test still exercises only the same single instance/case as before, or introduces a mock in place of the real collaborator, this ticket is not complete.

## Handoff

Feeds the relevant workstream I vertical scenario's own eventual closure, once it is picked up.

## Definition of done

- test widened per the specific gap named in Source
- real collaborator used throughout, no mocks
- negative case included where applicable
