# TICKET-024: Selectors + abstention/confirmation projection

## Status

ALIVE

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

## Implementation notes (real evidence) — closes as ALIVE

- Reused `queries/cognition-capabilities.rq` verbatim (already verified 13 rows, TICKET-018).
  Real Tera template `templates/024_selectors_ts.tmpl`, starts with `---`.
- All 13 capabilities got a generated selector (one per capability, per Definition of Done),
  not a hand-picked subset. 11 of the 13 follow one uniform, mechanically-generated pattern
  (`select{PascalCase(id)}Evidence(candidates): string[]` — truthy-evidence-key filter, no
  per-capability special-casing). Two get bespoke composition logic, matching this ticket's own
  85%/15% template/custom split and its "Custom-code boundary: NONE / composition logic
  requires human judgment analogous to TICKET-023" framing:
  - `selectAbstention(evidence): boolean` for `capability/cognition/
    abstain-under-insufficient-evidence` — the literal name the ticket's Implementation
    steps/Acceptance criteria require; boolean shape, not the list shape the other 12 share,
    since abstention is a session-level yes/no, not a per-candidate filter.
  - `selectRankedSolutionFamilies(candidates): string[]` for `capability/cognition/
    rank-solution-families` — the ticket's other named example; the one selector composing
    `TrackCandidate.rank` (sorted ascending), matching the capability's own "ranked" semantics.
  - No confidence threshold is hardcoded anywhere in the file — every boolean read
    (`evidence["cognition/..."]`) is written by that capability's own handler elsewhere, out of
    this ticket's scope, satisfying the Exclusions clause verbatim.
- Real (non-dry) `ggen sync run` wrote `lib/domain/selectors.ts` for the first time —
  `"lib/domain/selectors.ts": "written"` in the sync summary.
- **Real test, both directions** (`tests/domain/selectors.test.ts`): `selectAbstention` on a
  fixture with `"cognition/abstain-under-insufficient-evidence": true` → `true`; on a fixture
  with strong evidence (other capability keys truthy, this one absent) → `false`; and on a
  fixture with this key explicitly `false` → `false` — proving it is not a constant, per the
  ticket's own falsifier. Also: `selectRankedSolutionFamilies` orders 3 candidates by
  ascending `rank` and excludes a 4th with no evidence at that key; a generic mechanical
  selector (`selectMarkConceptCoveredEvidence`) cross-checked as a sanity probe on the uniform
  pattern; `COGNITION_CAPABILITY_COUNT === 13`. `npx vitest run` →
  `tests/domain/selectors.test.ts (6 tests)`, all pass.
- `npx tsc --noEmit`: zero errors.
- Idempotency: second real sync reported
  `"lib/domain/selectors.ts": "skipped: unchanged: content identical"`.
- SHA-256: `12df90ede1cd354a09d6c2ceba79fad5fcf507d907519d2a3af1a33964b7e5fd`.
