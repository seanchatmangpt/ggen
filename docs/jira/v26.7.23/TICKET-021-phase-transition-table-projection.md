# TICKET-021: Phase transition table projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl (13 transition-plan/* schema:Action+prov:Plan resources, <phase-scheme>)`
- ARD components:
  - `ARD §3.1 Session Orchestrator`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate a TypeScript transition table (Record<Phase, Phase[]>, legal-next-phases per phase) directly from the 13 transition-plan/* resources' schema:object/schema:result pairs, plus the debugging<->implementation bidirectional edge and the REFUSED reachable-from-any-non-terminal-phase rule from phase/refused's skos:related set.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 90%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 10%
- Expected ratio: 90/10
- Custom-code justification: encoding the REFUSED wildcard-reachability rule (documented in RDF as skos:related to 12 phases rather than 12 separate transition-plan resources, a deliberate brevity choice made in the TTL phase) requires a small amount of template logic beyond pure schema:object/result pair enumeration.

## Inputs

- queries/transition-plans.rq (new)
- TICKET-016 phase.ts

## Outputs

- examples/interview-assist/lib/domain/phase-transitions.ts (transition table + isLegalTransition() function)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Generic 'prov:Plan schema:object/schema:result pairs -> adjacency table' projection.

## Domain-data responsibility

The 13 legal edges (plus REFUSED's wildcard) live in 40-events-workflow.ttl; this is the SAME phase-scheme already imported from TICKET-016, not redefined.

## Custom-code boundary

NONE.

## Exclusions

- no duplicate phase enumeration — imports phase.ts from TICKET-016
- no hardcoded transition edge outside the RDF-bound generation

## Implementation steps

1. Query transition-plan/* resources' schema:object/schema:result pairs.
2. Query phase/refused's skos:related set for the wildcard-refusal rule.
3. Generate the adjacency table + isLegalTransition(from, to) function.
4. Verify: 12 forward edges + 1 backward edge (debugging->implementation) + refusal wildcard = matches the 13 transition-plan resources plus the documented refusal exception.

## Admission gates

- TICKET-016.

## Acceptance criteria

- Given phase/debugging, when isLegalTransition is queried, then both phase/explanation (forward) and phase/implementation (backward, per transition-plan/debugging-to-implementation) return true, and any other phase not in the RDF-declared edge set returns false.

## Negative tests

- Query isLegalTransition(phase/created, phase/complete) (a non-adjacent jump) and confirm it returns false, proving the table isn't a permissive any-to-any pass-through.

## Verification ladder

- Unit: isLegalTransition() exhaustive truth-table test against all 14x14 phase pairs
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- phase-transitions.ts hash
- 14x14 truth-table test result

## Dependencies

- TICKET-016

## Falsifier

If isLegalTransition() returns true for any pair not backed by a transition-plan/* resource or the documented refusal wildcard, this ticket is not complete.

## Handoff

TICKET-023 (reducer generation) enforces transitions via this table.

## Definition of done

- transition table generated
- exhaustive 14x14 truth-table test passes
- no permissive fallback
