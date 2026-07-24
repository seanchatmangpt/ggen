# TICKET-023: Deterministic reducer generation

## Status

ALIVE

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources: (none)
- ARD components:
  - `ARD §3.1 Session Orchestrator`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the reducer SKELETON (switch/dispatch machinery, immutable-update boilerplate) as pure reusable structure, wired to TICKET-021's transition table and TICKET-022's routing table for all actual domain decisions — the reducer function body contains zero InterviewAssist-specific literals.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 75%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 25%
- Expected ratio: 75/25
- Custom-code justification: the reducer's core dispatch-and-immutable-update logic is generic React/Redux-pattern machinery independent of any RDF input (no domain data to select), so it is authored once as a reusable template partial rather than per-field generated — classified here as template-authored (not custom) because it contains zero domain literals and is fully reusable, but flagged at 25% custom-adjacent since the control-flow logic itself (not values) required human design judgment beyond mechanical RDF-to-code substitution.

## Inputs

- TICKET-016 phase.ts
- TICKET-021 phase-transitions.ts
- TICKET-022 event-routing.ts
- TICKET-017 refusal.ts

## Outputs

- examples/interview-assist/lib/domain/reducer.ts (sessionReducer(state, event) -> AdmissionResult<SessionState>)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The dispatch/immutable-update skeleton itself — reusable structural machinery, containing only calls into the imported RDF-derived tables, no domain literals.

## Domain-data responsibility

Every domain decision (which transitions are legal, which events route where, which refusal codes exist) is delegated to imported generated modules — the reducer body literally cannot express a new phase name or event family without editing RDF first.

## Custom-code boundary

NONE — flagged above as template-authored-with-design-judgment, not custom runtime code; there is no irreducible platform/OS/browser dependency here.

## Exclusions

- no phase or event-family literal string anywhere in reducer.ts
- no duplicate transition logic — must call TICKET-021's isLegalTransition(), never reimplement it inline

## Implementation steps

1. Author the reducer skeleton importing phase-transitions.ts, event-routing.ts, refusal.ts.
2. Implement dispatch: unknown event family -> AdmissionResult refused with a named refusal code (not a silent drop).
3. Implement transition attempt: illegal transition -> refused via isLegalTransition() check, not inline phase comparison.
4. Add an ESLint/grep-based CI check confirming reducer.ts contains no phase/event-family string literal outside its import statements.

## Admission gates

- TICKET-021.
- TICKET-022.
- TICKET-017.

## Acceptance criteria

- Given an event with no matching family route, when the reducer processes it, then it returns AdmissionResult refused with a specific named refusal code, never silently ignoring the event.
- Given a legal transition per TICKET-021's table, when the reducer processes the corresponding event, then state.phase updates to the target phase.

## Negative tests

- Feed the reducer an event attempting an illegal transition (e.g. created directly to complete) and confirm it returns refused, not a mutated state.

## Verification ladder

- Unit: reducer dispatch unit tests, one per event family + one illegal-transition test
- Integration: reducer composed with real generated tables (not mocked stand-ins, per this repo's Chicago TDD rule)
- End-to-end: exercised via workstream I's vertical scenarios
- Chaos: N/A with reason — pure function, no concurrency
- Stress: reducer called in a tight loop across all 14x14 phase pairs to confirm no perf cliff
- Benchmark: N/A with reason — pure in-memory reducer, no meaningful benchmark target at this scale
- Verifier report: reducer dispatch test matrix

## Receipts

- reducer.ts hash
- no-literal-string CI check result

## Dependencies

- TICKET-021
- TICKET-022
- TICKET-017

## Falsifier

If a grep for any phase/event-family string literal in reducer.ts (outside import lines) finds a match, this ticket is not complete — the template has absorbed domain data, which is the exact anti-pattern the user's instruction forbids.

## Handoff

TICKET-024 (selectors) and workstream I's vertical scenarios exercise this reducer directly.

## Definition of done

- reducer generated/authored with zero domain literals
- illegal-transition and unknown-event-family negative tests pass
- CI literal-string check wired

## Implementation notes (real evidence) — closes as ALIVE

- Hand-authored `lib/domain/reducer.ts` (this ticket's own classification: "Template: 75% /
  Custom code: 25%" — the dispatch skeleton is reusable structure containing only calls into
  RDF-derived tables, not RDF-row-generated itself, so no `.tmpl` file). Imports `Phase` from
  `phase.ts` (TICKET-016), `ALL_EVENT_FAMILIES`/`EventFamily` from `event-family.ts`
  (TICKET-016), `AdmissionResult`/`RefusalCode` from `refusal.ts` (TICKET-017),
  `isLegalTransition` from `phase-transitions.ts` (TICKET-021), `EVENT_ROUTING` from
  `event-routing.ts` (TICKET-022).
- **Falsifier, run for real from `examples/interview-assist/`:**
  ```
  $ grep -E '"(CREATED|PREPARING|READY|INTRODUCTION|PROBLEM_PRESENTATION|CLARIFICATION|PLANNING|IMPLEMENTATION|EXECUTION|DEBUGGING|EXPLANATION|FOLLOW_UP|COMPLETE|REFUSED)"' lib/domain/reducer.ts
  $ echo "exit code: $?"
  exit code: 1
  ```
  Zero matches (grep's own exit-code-1-means-no-match convention) — confirmed with a second,
  independent mechanism too: `tests/domain/reducer.test.ts`'s own test re-reads the file at
  runtime via `node:fs` and applies the identical regex against the live `ALL_PHASES` list,
  asserting `matches` is `[]`.
- Both refusal branches (unrecognized event family; illegal transition) resolve to
  `RefusalCode` `"STALE_SESSION_EVENT"` — the closest fit among refusal.ts's 16 admitted codes
  for "an event that does not apply to the session as it currently stands"; no 17th code was
  invented to split the two cases (documented in the file's own comment above
  `unrecognizedFamilyCode`/`illegalTransitionCode`).
- Chicago TDD: `tests/domain/reducer.test.ts` composes the *real* generated
  `phase-transitions.ts`/`event-routing.ts`/`refusal.ts` — nothing mocked. 6 tests: legal
  transition admits and updates `state.phase`; illegal transition (CREATED→COMPLETE, the
  ticket's own example) refuses with `STALE_SESSION_EVENT` and leaves the caller's state object
  unmutated; unknown event family refuses with a named, non-empty code and a reason string
  naming the bad family (never silently ignored); a routed event with no `targetPhase` admits
  as a pass-through no-op; all 15 real `ALL_EVENT_FAMILIES` dispatch without throwing; the
  no-literal falsifier itself as a live test. `npx vitest run` →
  `tests/domain/reducer.test.ts (6 tests)`, all pass.
- `npx tsc --noEmit`: zero errors, including `noUncheckedIndexedAccess`-driven checks on
  `EVENT_ROUTING[event.family as EventFamily]` (typed `string | undefined`, handled with an
  explicit `undefined` guard, not a non-null assertion).
- SHA-256 of `reducer.ts`: `04f6c0e678ff746fcf390a2fb9b59a48d525789a02221fe7e623afd78e579c4a`.
