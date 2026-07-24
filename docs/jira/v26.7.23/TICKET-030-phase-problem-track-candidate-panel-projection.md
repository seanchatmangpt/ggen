# TICKET-030: Phase/problem/track-candidate panel projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl (<phase-scheme>)`
  - `packs/wasm4pm-interview-assist-pack/ontology/20-requirements.ttl (problem-related req/* resources)`
- PRD requirements:
  - `PRD §12 User-interface requirements (doc/prd#ui)`
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate React component scaffolds for the phase indicator, problem panel, and track-candidate panel, with all displayed labels selected from TICKET-016's phase.ts / TICKET-018's problem-state.ts / TICKET-024's selectors, never hardcoded JSX text.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 75%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 25%
- Expected ratio: 75/25
- Custom-code justification: React component JSX structure requires human UI-design judgment (layout, accessibility markup per PRD §12) beyond mechanical RDF substitution — the label CONTENT remains 100% RDF-sourced, but the component SHAPE is template-authored, consistent with the reducer's classification basis.

## Inputs

- TICKET-016 phase.ts
- TICKET-018 problem-state.ts, track-candidate.ts
- TICKET-024 selectors.ts
- TICKET-015 accessibility defaults.ts

## Outputs

- examples/interview-assist/components/phase-indicator.tsx
- examples/interview-assist/components/problem-panel.tsx
- examples/interview-assist/components/track-candidate-panel.tsx

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Component scaffolds (props typed against imported domain types, rendering structure, ARIA landmark roles per accessibility defaults) — reusable component-generation pattern.

## Domain-data responsibility

Every displayed phase name, problem field label, and track-candidate label is a prop sourced from the imported domain types — never a JSX string literal.

## Custom-code boundary

NONE beyond ordinary React/JSX authoring, which is template-classified per this ticket's own justification, not custom runtime code.

## Exclusions

- no phase name, problem-field label, or track-candidate label hardcoded as JSX text outside prop bindings

## Implementation steps

1. Author phase-indicator.tsx accepting a Phase prop (from phase.ts) and rendering it via a generated phase-to-display-label map (not inline switch-case string literals).
2. Author problem-panel.tsx and track-candidate-panel.tsx similarly, props typed against TICKET-018's state types.
3. Add accessibility landmark roles sourced from TICKET-015's defaults.ts.
4. Snapshot-test each component against a real fixture SessionState (Chicago TDD — real typed fixture, not a mock component).

## Admission gates

- TICKET-016.
- TICKET-018.
- TICKET-024.
- TICKET-015.

## Acceptance criteria

- Given a SessionState fixture with phase/debugging active, when phase-indicator.tsx renders, then its displayed label matches phase/debugging's schema... skos:prefLabel exactly, sourced via the generated phase-to-label map, not a JSX literal.

## Negative tests

- Render with a fixture SessionState missing a required problem-state field and confirm the panel renders a documented empty/placeholder state rather than crashing or silently omitting the field with no indication.

## Verification ladder

- Unit: component render test against real typed fixtures (React Testing Library, real DOM, no shallow-render mocking per Chicago TDD)
- Integration: components composed together in a real page render
- End-to-end: exercised in workstream I's vertical scenarios via Playwright
- Chaos: N/A with reason — pure presentational components, no concurrency hazard
- Stress: N/A with reason — no perf-sensitive rendering at this scale
- Benchmark: N/A with reason — no perf target defined for panel rendering
- Verifier report: component render test matrix

## Receipts

- component file hashes

## Dependencies

- TICKET-016
- TICKET-018
- TICKET-024
- TICKET-015

## Falsifier

If a grep for phase-name or problem-field-label string literals finds a match inside JSX (outside prop-binding expressions) in these components, this ticket is not complete.

## Handoff

TICKET-013's app shell composes these panels into the session page.

## Definition of done

- three components generated
- real-fixture render tests pass
- no hardcoded domain labels in JSX
