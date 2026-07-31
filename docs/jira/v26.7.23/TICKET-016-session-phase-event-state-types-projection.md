# TICKET-016: Session/phase/event state types projection

## Status

PARTIAL_ALIVE

## Implementation notes (real evidence)

- `examples/interview-assist/lib/domain/phase.ts` — 14-member `Phase` union, order taken from a
  real rdflib SPARQL walk of `skos:broader` chains in `ontology.ttl` (not hand-guessed; see
  transcript below), matching `queries/phases.rq`'s existing verified 14-row count.
- `examples/interview-assist/lib/domain/event-family.ts` — 15-member `EventFamily` union, from a
  real run of `queries/event-families.rq` against `ontology.ttl` (15 rows).
- Real negative tests (scratch copies only — `ontology/40-events-workflow.ttl` was copied to
  `/tmp`, never the real pack file was mutated): deleting `phase/debugging` from a scratch copy
  and rebuilding a scratch full-ontology graph in `/tmp/scratch-ontology-dir` dropped
  `phases.rq`'s row count from 14 to 13; deleting `event-family/compiler` the same way dropped
  `event-families.rq`'s row count from 15 to 14. Both confirmed by direct rdflib query output,
  not asserted.
- **Gap**: this ticket's title also covers "session ... state types" (session/workspace state) —
  that slice was not done in this pass; see TICKET-018 (still PLANNED). No `session-state.ts` was
  written here. Status left PARTIAL_ALIVE, not ALIVE, for that reason.

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl (<phase-scheme> 14 skos:Concept, <event-family-scheme> 15 skos:Concept)`
- ARD components:
  - `ARD §3.1 Session Orchestrator (doc/ard#components)`
  - `ARD §7 Event model (doc/ard#event-model)`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate a TypeScript discriminated union type for the 14-state session lifecycle and a separate union for the 15 event families, directly from the two skos:ConceptScheme resources — this is the canonical example of workstream D's rule: domain values come from RDF, the union-type SHAPE is template machinery.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 95%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 5%
- Expected ratio: 95/5
- Custom-code justification: TypeScript's own type-system quirks (e.g. const-assertion boilerplate) require a tiny amount of hand-verified template logic beyond pure string substitution.

## Inputs

- queries/phases.rq, queries/event-families.rq (from TICKET-003)

## Outputs

- examples/interview-assist/lib/domain/phase.ts (union type + as-const array, 14 members)
- examples/interview-assist/lib/domain/event-family.ts (union type + as-const array, 15 members)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Generic 'skos:ConceptScheme -> TypeScript discriminated union' projection macro, reusable for any future skos:ConceptScheme in any future pack.

## Domain-data responsibility

The 14 phase names and 15 event-family names live exclusively in 40-events-workflow.ttl; changing a phase name requires an RDF edit, never a TypeScript edit.

## Custom-code boundary

NONE.

## Exclusions

- no phase/event-family name typed directly into phase.ts/event-family.ts outside the SPARQL-bound generation
- no duplicate state machine — this IS the one canonical phase type, workstream E's transition table references it, does not redefine it

## Implementation steps

1. Query phase-scheme's skos:hasTopConcept in skos:broader/narrower order.
2. Query event-family-scheme's skos:hasTopConcept.
3. Generate a TS union type + as-const readonly array for each.
4. Verify generated union member count: 14 for phase, 15 for event-family, exactly matching the prior TTL report's counts.

## Admission gates

- TICKET-004 (mapping table states this exact projection shape).

## Acceptance criteria

- Given the phase-scheme skos:Concept count is 14, when generation runs, then phase.ts's union type has exactly 14 members in lifecycle order (CREATED first, COMPLETE/REFUSED terminal).

## Negative tests

- Add a 15th phase concept to a fixture without a skos:broader/narrower link and confirm generation either orders it deterministically at the end with a warning, or fails loudly — behavior decided and documented in this ticket, not left ambiguous.

## Verification ladder

- Unit: generated union type member count + order test against tsc's own type narrowing
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- phase.ts/event-family.ts hashes
- member-count confirmation (14/15)

## Dependencies

- TICKET-004

## Falsifier

If phase.ts's member count or order doesn't match the RDF skos:Concept chain exactly, this ticket is not complete.

## Handoff

TICKET-021 (transition table) and TICKET-022 (event routing) both import phase.ts/event-family.ts rather than redefining phase/event lists.

## Definition of done

- phase.ts generated with 14 members in correct order
- event-family.ts generated with 15 members
- both consumed (not duplicated) by later workstreams
