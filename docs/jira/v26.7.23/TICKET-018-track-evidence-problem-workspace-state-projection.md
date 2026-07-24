# TICKET-018: Track/evidence/problem/workspace state projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl (<observed-input/*> 7 prov:Entity resources)`
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability-category/problem> 11 capability/problem/* resources)`
- PRD requirements:
  - `PRD §13 Zero-input behavior (doc/prd#zero-input)`
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the ProblemState and WorkspaceState TypeScript types from the capability/problem/* resources (load/parse/extract/identify operations imply state fields) and the TrackCandidate/Evidence types from ARD's zero-input cognition model.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 80%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 20%
- Expected ratio: 80/20
- Custom-code justification: deriving a state SHAPE from a set of capability/action resources (rather than from an explicit state-schema resource) requires a documented, human-reviewed derivation convention per field — more interpretive than the pure enum projections in TICKET-016/017, hence a higher acknowledged custom fraction for the derivation script itself, though the output remains generated TypeScript, not hand-written.

## Inputs

- queries/problem-capabilities.rq (new)
- queries/observed-inputs.rq (new)

## Outputs

- examples/interview-assist/lib/domain/problem-state.ts
- examples/interview-assist/lib/domain/workspace-state.ts
- examples/interview-assist/lib/domain/track-candidate.ts

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The capability-to-state-field derivation macro (e.g. capability/problem/identify-input-format -> ProblemState.inputFormat?: string field), reusable for any future capability category with an analogous 'identify/extract/parse X' naming pattern.

## Domain-data responsibility

Which fields exist is entirely a function of which capability/problem/* resources exist — adding a 12th problem capability like 'identify-time-complexity' automatically adds a field, no template edit.

## Custom-code boundary

NONE in the generated output; the field-name derivation heuristic (capability local-name -> camelCase field name) is documented and unit-tested as part of the template macro.

## Exclusions

- no problem-state field hand-typed outside the capability-driven derivation
- no duplicate list of problem capabilities — this ticket's query is the only place that enumerates them for this purpose

## Implementation steps

1. Query capability/problem/* resources.
2. Apply the documented local-name -> camelCase field-name derivation.
3. Generate ProblemState with one optional field per capability.
4. Similarly derive WorkspaceState from editor/* and TrackCandidate from the interview-cognition capabilities (hypothesis, evidence retrieval).
5. Verify field counts against the known capability counts (11 problem, 12 editor, 13 cognition).

## Admission gates

- TICKET-004.

## Acceptance criteria

- Given 11 capability/problem/* resources, when generation runs, then ProblemState has exactly 11 derived fields.

## Negative tests

- Add a capability/problem/* resource with a name that doesn't cleanly camelCase (e.g. containing a digit-leading segment) and confirm the derivation macro either handles it deterministically or fails loudly with a named resource, not silently mangling the field name.

## Verification ladder

- Unit: field-name derivation heuristic unit test against known-tricky IRI segments
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- problem-state.ts/workspace-state.ts/track-candidate.ts hashes
- field-count confirmations

## Dependencies

- TICKET-004

## Falsifier

If a state type's field count doesn't match its source capability category's resource count, this ticket is not complete.

## Handoff

TICKET-023 (reducer generation) operates over these state types.

## Definition of done

- three state types generated with field counts matching their capability-category sources
- field-name derivation heuristic documented and tested
