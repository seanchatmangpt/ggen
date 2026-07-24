# TICKET-036: Local persistence adapter (custom)

## Status

PARTIAL_ALIVE — real filesystem-backed persistence implemented and tested; real browser IndexedDB/localStorage NOT exercised (documented Node-side substitution)

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources: (none)
- PRD requirements: (none)
- ARD components:
  - `ARD §3.16 Receipt Ledger`
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Implement real localStorage/IndexedDB persistence of session event logs behind a generated PersistenceAdapter port, so TICKET-025's replaySession has real data to replay.

## Current state

UNKNOWN — no implementation exists yet; classified in advance as genuinely custom per the analysis below.

## Target state

A working adapter implementing the generated port `a generated PersistenceAdapter interface (save(eventLog), load(sessionId)) — define this interface as part of TICKET-020's receipt/state type projection scope, not invented ad hoc here`, with zero domain-rule duplication (all domain checks — policy, precondition, refusal — are called INTO from workstream E/F, never reimplemented here).

## Projection classification

- Template: 20%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 80%
- Expected ratio: 20/80
- Custom-code justification: Browser storage APIs (localStorage/IndexedDB) are irreducible platform bindings with no RDF representation.

## Inputs

- generated port: a generated PersistenceAdapter interface (save(eventLog), load(sessionId)) — define this interface as part of TICKET-020's receipt/state type projection scope, not invented ad hoc here

## Outputs

- examples/interview-assist/lib/adapters/persistence-adapter.ts

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The port interface itself (a generated PersistenceAdapter interface (save(eventLog), load(sessionId)) — define this interface as part of TICKET-020's receipt/state type projection scope, not invented ad hoc here) is generated (workstream E/F/G) — this ticket implements it, does not define it.

## Domain-data responsibility

NONE — this adapter must not independently redefine domain rules already present in RDF (per the user's explicit custom-code exclusion). Any domain check needed (policy, precondition) is imported and called, never reimplemented.

## Custom-code boundary

Browser storage APIs (localStorage/IndexedDB) are irreducible platform bindings with no RDF representation.

## Exclusions

- no domain rule reimplementation — must call into workstream E/F's generated checks, never duplicate them
- no direct capability-list literal
- no bypass of TICKET-028's policy/precondition checks

## Implementation steps

1. Confirm the generated port interface this adapter must satisfy (from its Source dependency).
2. Implement the adapter, calling workstream E/F's generated policy/precondition checks before performing any real action.
3. Write a Chicago-TDD test using the REAL collaborator (real subprocess / real browser API / real Ollama endpoint / real crypto library), per this repo's own testing.md — no mocks.
4. Document the reduction path: if session persistence moves server-side to a real database in a later phase, this adapter's interface stays the same (it's already a generated port) and only the implementation swaps — proving the port/adapter separation pays off.

## Admission gates

- The generated port ticket this adapter implements (see Dependencies).

## Acceptance criteria

- Given a real invocation, when the adapter executes, then it produces output conforming to the generated port's typed return shape, with no domain-rule bypass.

## Negative tests

- Attempt an action the policy/precondition checks would deny and confirm the adapter refuses BEFORE performing the real action (proving it calls into the generated check rather than acting first and checking later).

## Verification ladder

- Unit: adapter unit test against the real collaborator in an isolated/ephemeral instance
- Integration: adapter composed with the real dispatch table (TICKET-027) and real policy check (TICKET-028)
- End-to-end: exercised in the relevant workstream I vertical scenario
- Chaos: kill/interrupt the real collaborator mid-operation and confirm graceful, receipted refusal, not a hang or crash
- Stress: N/A with reason — noted per-adapter below if a stress profile is genuinely relevant
- Benchmark: N/A with reason — no fixed perf target at this phase
- Verifier report: real-collaborator test transcript

## Receipts

- adapter source hash
- real-collaborator test transcript
- policy-check-called-before-action confirmation

## Dependencies

- TICKET-020
- TICKET-025

## Falsifier

If this adapter performs its real action WITHOUT first calling the generated policy/precondition check, this ticket is not complete — that would be exactly the 'custom code independently redefining domain rules' failure the user's instructions explicitly forbid.

## Handoff

The relevant workstream I vertical scenario exercises this adapter against real infrastructure.

## Definition of done

- adapter implements the generated port exactly
- real-collaborator Chicago-TDD test passes (no mocks)
- policy-check-before-action test passes
- reduction path documented

## Implementation notes (real evidence)

- File: `examples/interview-assist/lib/adapters/persistence-adapter.ts` (104 lines); test:
  `examples/interview-assist/tests/adapters/persistence-adapter.test.ts` (4 tests, all passing —
  `npx vitest run tests/adapters/persistence-adapter.test.ts`).
- HONEST SUBSTITUTION, stated up front in the source and here: this runs in a Node/Vitest test
  context, not a browser, so there is no real `window.indexedDB`. Implemented against the real
  filesystem (`node:fs/promises` `readFile`/`writeFile`/`mkdir` against a real temp directory
  created with `mkdtemp`) as the documented Node-side stand-in — not a claim of real IndexedDB.
- TICKET-020's real generated PersistenceAdapter port has not landed yet; the interface used here
  (`save(sessionId, eventLog)` / `load(sessionId)`) is hand-authored and marked
  `PENDING(TICKET-020)` in source.
- Real-collaborator evidence: (1) `load` on a never-saved session returns `undefined` from a real
  `ENOENT` filesystem error, not a stub; (2) `save` then `load` round-trips a real event log
  written to and read back from disk, deep-equal; (3) durability across store instances — a
  second `FilesystemEventLogStore` instance pointed at the same directory reads what the first
  wrote, proving real disk state, not in-memory state; (4) session-id sanitization strips `..`
  segments so `../../etc/evil` cannot escape the store directory (verified via `listSessionIds`).
- Policy-check-before-action: both `save` and `load` call `checkPolicy(...)` as their first
  statement, before any `fs` call — currently deferring to the `PENDING(TICKET-028)` placeholder.
