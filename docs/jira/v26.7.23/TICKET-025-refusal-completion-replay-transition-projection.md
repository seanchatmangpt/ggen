# TICKET-025: Refusal/completion/replay transition projection

## Status

PLANNED

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
