# TICKET-033: Accessibility projection controls + session completion summary projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (16 capability/accessibility/* resources)`
  - `packs/wasm4pm-interview-assist-pack/ontology/60-provenance-receipts.ttl (<receipt/entry-2-final>)`
- ARD components:
  - `ARD §9 Accessibility requirements`
- Acceptance-test steps:
  - `packs/wasm4pm-interview-assist-pack/ontology/80-acceptance.ttl (<impl-phase/4>)`
- PRD requirements: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the accessibility-controls panel (one toggle/control per capability/accessibility/* resource, wired to TICKET-015's defaults.ts) and the session-completion-summary component displaying the final TransitionReceipt (TICKET-020).

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 75%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 25%
- Expected ratio: 75/25
- Custom-code justification: same JSX-authoring classification basis as prior UI tickets; actual screen-reader/Braille/TTS platform integration is explicitly out of scope here (TICKET-039).

## Inputs

- TICKET-015 accessibility defaults.ts
- TICKET-020 receipt.ts

## Outputs

- examples/interview-assist/components/accessibility-controls.tsx (16 controls)
- examples/interview-assist/components/session-summary.tsx

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Controls panel generated 1:1 with the 16 accessibility capabilities; summary component rendering the typed TransitionReceipt.

## Domain-data responsibility

The 16 accessibility capability names/toggles come from 30-capabilities.ttl via TICKET-015's defaults.ts; the receipt fields displayed come from TICKET-020's receipt.ts shape.

## Custom-code boundary

Explicitly NONE here — actual platform accessibility API integration (screen reader semantics, Braille output, TTS) is TICKET-039's custom-code boundary; this ticket produces the typed controls contract only.

## Exclusions

- no accessibility-feature list hardcoded outside TICKET-015's generated defaults
- no platform-specific accessibility API call in this ticket's own code

## Implementation steps

1. Generate one control per accessibility default key from TICKET-015.
2. Verify 16 controls generated.
3. Generate session-summary.tsx rendering TICKET-020's TransitionReceipt fields.
4. Verify the summary displays the checksum value in a copyable/verifiable format, per ARD §14's decisive-acceptance-test step 10 (match the final receipt hash).

## Admission gates

- TICKET-015.
- TICKET-020.

## Acceptance criteria

- Given 16 accessibility defaults, when generation runs, then accessibility-controls.tsx has exactly 16 controls.
- Given a final TransitionReceipt fixture, when session-summary.tsx renders, then the displayed checksum value matches the receipt's spdx:checksumValue exactly, character for character.

## Negative tests

- Render session-summary.tsx with a receipt fixture missing a checksum and confirm it displays an explicit 'receipt incomplete' state rather than a blank or fabricated hash.

## Verification ladder

- Unit: 16-control render test + receipt-display exact-match test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- component file hashes
- 16-control count confirmation
- checksum-display exact-match test result

## Dependencies

- TICKET-015
- TICKET-020

## Falsifier

If session-summary.tsx ever displays a checksum that doesn't exactly match the underlying TransitionReceipt's spdx:checksumValue, this ticket is not complete — this directly supports acceptance-step/10's 'match the final receipt hash' requirement.

## Handoff

TICKET-053 (full decisive acceptance test) verifies this component displays the real final receipt hash matching acceptance-step/10.

## Definition of done

- accessibility-controls.tsx generated with 16 controls
- session-summary.tsx generated with exact checksum display
- missing-checksum negative test passes
