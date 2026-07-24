# TICKET-056: Final session receipt + deterministic replay

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/60-provenance-receipts.ttl (<receipt/entry-2-final>)`
- Acceptance-test steps:
  - `acceptance-step/9 (reproduce the final session state)`
  - `acceptance-step/10 (match the final receipt hash)`
- PRD requirements: (none)
- ARD components: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Wire TICKET-025's replaySession + TICKET-055's receipt chain into the final-session-receipt computation, directly implementing acceptance-step/9 and acceptance-step/10.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 70%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 30%
- Expected ratio: 70/30
- Custom-code justification: this ticket composes multiple prior custom/template pieces (replay, checksum adapter, receipt emitter) into one final computation — the composition logic itself is the irreducible design work, everything it calls is already generated/adapter code.

## Inputs

- TICKET-025 replay.ts
- TICKET-055 receipt-emitter.ts
- TICKET-038 checksum-adapter.ts

## Outputs

- examples/interview-assist/lib/domain/final-receipt.ts (computeFinalReceipt(sessionOrReplayedState) -> TransitionReceipt)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The composition wiring (call replay, call checksum adapter, emit final receipt) is generic structural glue.

## Domain-data responsibility

None new — reuses TICKET-020's receipt shape and TICKET-025's replay logic exactly.

## Custom-code boundary

Composition-only; no new irreducible platform binding introduced.

## Exclusions

- no separate final-hash computation path — must reuse TICKET-038's checksum adapter exactly, matching Architecture Decision 12's independent-revalidation requirement

## Implementation steps

1. Implement computeFinalReceipt calling replaySession then hashing the resulting state via TICKET-038.
2. Verify against a real completed session: computeFinalReceipt(liveState) === computeFinalReceipt(replaySession(persistedLog)).

## Admission gates

- TICKET-025.
- TICKET-055.
- TICKET-038.

## Acceptance criteria

- Given a real completed session, when computeFinalReceipt runs on both the live final state and the replayed state, then both produce identical receipt hashes — directly satisfying acceptance-step/9 and acceptance-step/10.

## Negative tests

- Same tamper-detection negative test as TICKET-049, re-verified here at the final-receipt composition level specifically.

## Verification ladder

- Unit: computeFinalReceipt against a fixture
- Integration: live-vs-replayed hash match on a real session
- End-to-end: exercised as acceptance-step/9 and /10 within TICKET-053
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: live-vs-replayed hash match result

## Receipts

- final-receipt.ts hash
- live-vs-replayed match confirmation

## Dependencies

- TICKET-025
- TICKET-055
- TICKET-038

## Falsifier

If computeFinalReceipt(live) != computeFinalReceipt(replayed) for an untampered real session, this ticket is not complete — this is acceptance-step/10 itself.

## Handoff

TICKET-057 (final verifier report) and TICKET-053 (decisive acceptance test) both depend directly on this.

## Definition of done

- computeFinalReceipt implemented
- live-vs-replayed match verified on a real session
- tamper negative test passes
