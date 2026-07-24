# TICKET-012: App layout + module boundaries generation

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/10-product.ttl (<role/*> resources)`
  - `packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl (<phase-scheme>)`
- PRD requirements: (none)
- ARD components:
  - `ARD §2 System planes (doc/ard#planes)`
  - `ARD §3 Core components (doc/ard#components)`
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the Next.js app/ directory's root layout.tsx and top-level module boundary folders (one per ARD §2 system plane: observation, admission, manufacturing, execution, receipt) as structural scaffolding, with no InterviewAssist domain lists embedded in the layout component itself.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 85%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 15%
- Expected ratio: 85/15
- Custom-code justification: React Server Component boilerplate (providers, error boundaries) has some irreducible Next.js-framework-specific wiring not expressible as pure template output without framework knowledge baked into the template — kept minimal and documented.

## Inputs

- queries/system-planes.rq (new)
- packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl

## Outputs

- examples/interview-assist/app/layout.tsx
- examples/interview-assist/lib/{observation,admission,manufacturing,execution,receipt}/ (module boundary scaffolds)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

layout.tsx structural shell + one module-boundary folder per system plane, generated from the ARD §2 planes list in RDF.

## Domain-data responsibility

Which planes exist and their names live in ARD components resources; the template iterates over whatever the query returns, not a hardcoded 5-item array.

## Custom-code boundary

Minimal Next.js App Router lifecycle glue (root layout's <html>/<body> wrapper) that has no domain content.

## Exclusions

- no hardcoded plane-name array in layout.tsx
- no business logic in layout.tsx beyond composition

## Implementation steps

1. Add system-planes.rq selecting ARD §2 plane resources.
2. Author layout.tsx.tmpl iterating planes only for module-boundary folder scaffolding, not runtime logic.
3. Generate one empty index.ts per plane folder as a placeholder module boundary.
4. Verify folder count matches the RDF-selected plane count exactly.

## Admission gates

- TICKET-011.
- TICKET-005 refusal macro.

## Acceptance criteria

- Given N planes in ARD §2, when generation runs, then exactly N module-boundary folders exist under lib/.

## Negative tests

- Remove one plane resource from a fixture copy and confirm the folder count drops by exactly one, proving genuine RDF-driven generation.

## Verification ladder

- Unit: generated output structure / type-check test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- layout.tsx hash
- module-boundary folder listing

## Dependencies

- TICKET-011

## Falsifier

If the module-boundary folder count doesn't match the RDF-selected plane count, this ticket is not complete.

## Handoff

TICKET-013 (routes) and workstream D (state types) populate these module boundaries.

## Definition of done

- layout.tsx generated
- module-boundary folders generated 1:1 with ARD §2 planes
- no hardcoded plane list in the template
