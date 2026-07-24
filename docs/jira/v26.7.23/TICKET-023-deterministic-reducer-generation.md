# TICKET-023: Deterministic reducer generation

## Status

PLANNED

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
