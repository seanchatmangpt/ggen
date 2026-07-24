# TICKET-015: Accessibility defaults generation

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (16 capability/accessibility/* resources)`
- PRD requirements: (none)
- ARD components:
  - `ARD §9 Accessibility requirements (doc/ard#accessibility-req, 14 linked resources)`
- Acceptance-test steps:
  - `packs/wasm4pm-interview-assist-pack/ontology/80-acceptance.ttl (<impl-phase/4> Accessibility crown)`
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate a default accessibility configuration object (one field per capability/accessibility/* resource, default-enabled per ARD §9) so accessibility is a first-class generated config surface, not an afterthought bolted onto the UI later.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 90%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 10%
- Expected ratio: 90/10
- Custom-code justification: wiring the generated config object into Next.js's metadata/viewport APIs involves minimal framework-specific glue not expressible as pure data.

## Inputs

- queries/accessibility-capabilities.rq (new)
- packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl

## Outputs

- examples/interview-assist/lib/accessibility/defaults.ts (generated config object, one key per capability/accessibility/* resource)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The config-object generation structure: one boolean/enum field per accessibility capability, keyed by the capability's local IRI segment.

## Domain-data responsibility

Which 16 accessibility capabilities exist, and their names, live in 30-capabilities.ttl — the generated object's key set is exactly this set, never hand-curated.

## Custom-code boundary

Minimal Next.js metadata API wiring (colorScheme, viewport) — framework-specific, not domain-specific.

## Exclusions

- no hardcoded accessibility-feature list in the template outside the SPARQL-bound iteration

## Implementation steps

1. Add accessibility-capabilities.rq selecting all capability/accessibility/* resources with schema:name.
2. Author defaults.ts.tmpl generating one config key per row.
3. Verify generated key count equals 16 exactly, matching the capability-category/accessibility count established in the prior TTL report.

## Admission gates

- TICKET-012.

## Acceptance criteria

- Given 16 accessibility capabilities in RDF, when generation runs, then defaults.ts has exactly 16 keys, one per capability.

## Negative tests

- Remove one capability/accessibility/* resource from a fixture and confirm the generated key count drops to 15.

## Verification ladder

- Unit: generated output structure / type-check test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- defaults.ts hash
- key-count confirmation (16)

## Dependencies

- TICKET-012

## Falsifier

If defaults.ts's key count doesn't match the RDF-selected accessibility capability count, this ticket is not complete.

## Handoff

TICKET-033 (accessibility UI projection) consumes this generated config.

## Definition of done

- defaults.ts generated 1:1 with the 16 accessibility capabilities
- no hardcoded feature list
