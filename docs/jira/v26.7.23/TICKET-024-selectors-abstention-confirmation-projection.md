# TICKET-024: Selectors + abstention/confirmation projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability/cognition/abstain-under-insufficient-evidence>)`
- ARD components: (none)
- PRD requirements:
  - `PRD §13 Zero-input behavior`
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate selector functions (deriving UI-facing view state from SessionState) and the abstention/confirmation-handling logic implied by capability/cognition/abstain-under-insufficient-evidence.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 85%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 15%
- Expected ratio: 85/15
- Custom-code justification: selector composition logic (which fields combine into which view) requires human design judgment analogous to TICKET-023's reducer, not pure RDF substitution.

## Inputs

- TICKET-016-020 state types
- queries/cognition-capabilities.rq (new)

## Outputs

- examples/interview-assist/lib/domain/selectors.ts

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Selector-function skeleton pattern (memoized derivation from state), reusable structure.

## Domain-data responsibility

Which cognition capability triggers abstention (capability/cognition/abstain-under-insufficient-evidence) is looked up from RDF, not hardcoded as a magic threshold.

## Custom-code boundary

NONE.

## Exclusions

- no hardcoded confidence-threshold values not traceable to an RDF resource

## Implementation steps

1. Query capability-category/interview-cognition's 13 resources.
2. Generate one selector per capability implying a derivable view (e.g. selectRankedSolutionFamilies from capability/cognition/rank-solution-families).
3. Generate the abstain-when-insufficient-evidence selector explicitly named after its source capability.

## Admission gates

- TICKET-018.
- TICKET-019.

## Acceptance criteria

- Given insufficient evidence (simulated fixture state), when selectAbstention runs, then it returns true, matching capability/cognition/abstain-under-insufficient-evidence's intent.

## Negative tests

- Feed a state with strong evidence and confirm selectAbstention returns false, proving the selector isn't a constant.

## Verification ladder

- Unit: per-selector fixture test, both positive and negative evidence cases
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- selectors.ts hash

## Dependencies

- TICKET-018
- TICKET-019

## Falsifier

If selectAbstention returns a constant value regardless of input state, this ticket is not complete.

## Handoff

Workstream G (UI projection) consumes these selectors for panel rendering.

## Definition of done

- selectors generated, one per relevant cognition capability
- abstention logic tested both directions
