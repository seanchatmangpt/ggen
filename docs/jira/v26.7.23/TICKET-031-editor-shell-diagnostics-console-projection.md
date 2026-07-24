# TICKET-031: Editor shell/diagnostics/console projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability-category/editor> 12, <capability/editor/display-diagnostics>, <capability/editor/display-diff>)`
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the editor-shell component's PROPS interface and diagnostics/console panel scaffolds from the 12 capability/editor/* resources; the Monaco runtime itself is out of scope here (TICKET-034 custom adapter) — this ticket generates only the typed wrapper/props contract Monaco must satisfy.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 75%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 25%
- Expected ratio: 75/25
- Custom-code justification: same JSX-authoring classification basis as TICKET-030; the Monaco runtime itself is entirely out of scope for this ticket (lives in TICKET-034).

## Inputs

- TICKET-018 workspace-state.ts
- queries/editor-capabilities.rq (new)

## Outputs

- examples/interview-assist/components/editor-shell.tsx (props interface + non-Monaco chrome)
- examples/interview-assist/components/diagnostics-panel.tsx
- examples/interview-assist/components/console-panel.tsx

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Props interface generated from the 12 editor capabilities (one prop per capability implying a UI affordance: onCreateFile, onModifyFile, onDisplayDiagnostics, etc.); non-Monaco chrome (panel borders, tab bar) is template-authored structure.

## Domain-data responsibility

Which 12 editor operations exist, and thus which callback props the shell requires, comes from 30-capabilities.ttl.

## Custom-code boundary

Explicitly NONE in this ticket — the Monaco runtime integration itself is TICKET-034's custom-code boundary; this ticket produces only the typed contract (a generated port) TICKET-034 must implement.

## Exclusions

- no Monaco-specific runtime code in this ticket
- no editor-capability list hardcoded outside the RDF-bound generation

## Implementation steps

1. Query capability-category/editor's 12 resources.
2. Generate editor-shell.tsx's props interface with one callback per capability.
3. Generate diagnostics-panel.tsx and console-panel.tsx scaffolds.
4. Verify prop count equals 12.

## Admission gates

- TICKET-018.

## Acceptance criteria

- Given 12 editor capabilities, when generation runs, then editor-shell.tsx's props interface has exactly 12 capability-derived callback props.

## Negative tests

- Instantiate editor-shell.tsx without providing one of the 12 required callback props and confirm `tsc` rejects it — proving the generated interface is a real, enforced port, not documentation.

## Verification ladder

- Unit: props-interface completeness test (tsc rejects a partial implementation)
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- component file hashes
- 12-prop count confirmation

## Dependencies

- TICKET-018

## Falsifier

If editor-shell.tsx's props interface can be satisfied while omitting a capability-derived callback, this ticket's generated port is incomplete.

## Handoff

TICKET-034 (Monaco runtime adapter) implements this generated props interface.

## Definition of done

- editor-shell.tsx props interface generated with 12 capability-derived props
- diagnostics/console panel scaffolds generated
- tsc enforces prop completeness
