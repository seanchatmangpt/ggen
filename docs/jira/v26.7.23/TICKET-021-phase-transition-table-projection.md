# TICKET-021: Phase transition table projection

## Status

ALIVE

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

## Implementation notes (real evidence) — closes as ALIVE

- Real queries: `queries/transition-plans.rq` (13 rows, transition-plan/* schema:object/
  schema:result pairs joined to skos:prefLabel) and `queries/refusal-wildcard.rq` (12 rows,
  phase/refused's skos:related set). Verified via a real rdflib run against
  `packs/wasm4pm-interview-assist-pack/ontology.ttl`:
  `python3 -c "...g.query(open('queries/transition-plans.rq').read())..."` → 13;
  same pattern against `refusal-wildcard.rq` → 12.
- Real Tera template `templates/021_phase_transitions_ts.tmpl` (two named `sparql:` queries,
  `edges` + `wildcard`), starts with a literal `---` (checked). Real (non-dry) `ggen sync run`
  from `examples/interview-assist/` (after `rm ggen.lock`, per this session's own template-change
  rule) wrote `lib/domain/phase-transitions.ts` for the first time via the actual engine —
  `"lib/domain/phase-transitions.ts": "written"` in the sync summary.
- `PHASE_TRANSITIONS` is typed `Partial<Record<Phase, readonly Phase[]>>` (not a bare
  `Record<Phase, Phase[]>`) — matches this pack's own precedented adjacency-table pattern
  (`028a_preconditions_ts.tmpl`'s `DIRECT_REQUIRES: Partial<Record<CapabilityId, ...>>`); the
  two terminal phases (COMPLETE, REFUSED) have zero transition-plan edges and are simply absent
  from the object rather than present with `[]`, and `isLegalTransition` handles the missing-key
  case via `?? []`. `noUncheckedIndexedAccess: true` in `tsconfig.json` requires this — a bare
  `Record` would still type-check the literal but `PHASE_TRANSITIONS[from]` reads would carry a
  spurious `| undefined` either way, so `Partial` is the honest type.
- **Real exhaustive 14x14 truth-table test**, `tests/domain/phase-transitions.test.ts`: an
  independently-transcribed second encoding of the 13 edges + 12-member wildcard set (not a
  mirror of the generated file's own logic) checked against `isLegalTransition` for all 196
  pairs. `npx vitest run` → `tests/domain/phase-transitions.test.ts (4 tests)` all pass,
  including the acceptance-criteria case (DEBUGGING↔EXPLANATION forward,
  DEBUGGING↔IMPLEMENTATION backward, both true) and the ticket's own negative test
  (`isLegalTransition("CREATED", "COMPLETE")` → `false`).
- **Real negative test on the RDF layer itself** (in-memory only, real `ontology.ttl` file on
  disk untouched): removed the `<transition-plan/created-to-preparing>` triple from the parsed
  graph in a python3/rdflib session and re-ran `queries/transition-plans.rq` — row count dropped
  13 → 12; `queries/refusal-wildcard.rq` count unaffected (still 12), confirming the two query
  files are independent, not accidentally coupled.
- `npx tsc --noEmit` from `examples/interview-assist/`: zero errors (whole project, including
  this file and its consumers).
- Idempotency: a second real `ggen sync run` (lock file present, no template changes) reported
  `"lib/domain/phase-transitions.ts": "skipped: unchanged: content identical"`.
- SHA-256 of the generated file: `de589f0dea8ecfe230dbf78ade833936b3817d7eed3b58c357de6f067a20f4f7`.
