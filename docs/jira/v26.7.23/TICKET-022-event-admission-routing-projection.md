# TICKET-022: Event admission routing projection

## Status

ALIVE

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl (<event-family-scheme>, 7 observed-input/* prov:Entity resources)`
- ARD components:
  - `ARD §7 Event model`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate an event-family -> admission-handler-name routing table from the 15 event-family/* concepts and the 7 observed-input/* examples of how raw events map to families via dcterms:type.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 90%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 10%
- Expected ratio: 90/10
- Custom-code justification: handler-name derivation follows the same camelCase-from-IRI heuristic already established and unit-tested in TICKET-018.

## Inputs

- queries/event-families.rq (TICKET-016 reuse)
- queries/observed-inputs.rq (TICKET-018 reuse)

## Outputs

- examples/interview-assist/lib/domain/event-routing.ts (family -> handler-name routing table)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Generic 'skos:Concept family -> named handler slot' routing table generator, reusing the field-name derivation heuristic from TICKET-018.

## Domain-data responsibility

The 15 families and their example instances live in 40-events-workflow.ttl.

## Custom-code boundary

NONE — this ticket generates the routing TABLE; the actual handler implementations are workstream E's reducer (TICKET-023).

## Exclusions

- no duplicate event-family enumeration — imports event-family.ts from TICKET-016

## Implementation steps

1. Reuse TICKET-016's event-family query.
2. Generate one routing-table entry per family, keyed by family local name.
3. Verify 15 entries.

## Admission gates

- TICKET-016.

## Acceptance criteria

- Given 15 event-family/* concepts, when generation runs, then event-routing.ts has exactly 15 routing entries.

## Negative tests

- Route an event of a family with no registered handler slot (simulated) and confirm the reducer (TICKET-023) treats it as abstain/refuse, not a silent no-op — cross-verified in TICKET-023's own tests, cited here as the dependency contract.

## Verification ladder

- Unit: generated output structure / type-check test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- event-routing.ts hash
- entry-count confirmation (15)

## Dependencies

- TICKET-016

## Falsifier

If the routing table's entry count doesn't match the 15 event families, this ticket is not complete.

## Handoff

TICKET-023 (reducer) dispatches through this routing table.

## Definition of done

- routing table generated with 15 entries

## Implementation notes (real evidence) — closes as ALIVE

- Reused `queries/event-families.rq` verbatim (already verified 15 rows, TICKET-016). Also
  created `queries/observed-inputs.rq` — TICKET-022's own Inputs list cites it as "TICKET-018
  reuse", but grep of `packs/wasm4pm-interview-assist-pack/queries/` before this ticket showed
  it was never actually created by TICKET-018 (its own file lists it as "(new)", not delivered);
  created it now. First version over-matched (11 rows, not 7) because
  `?input a prov:Entity ; dcterms:type ?family` also matches unrelated `manufacturing-chain/*`
  resources elsewhere in the merged ontology (real bug found via live rdflib run, not assumed) —
  fixed with a `FILTER(CONTAINS(STR(?input), "/observed-input/"))` guard; re-verified 7 rows.
- Real Tera template `templates/022_event_routing_ts.tmpl`, starts with `---`. Handler-name
  derivation is plain string concatenation (`"handle" + label`), not a camelCase-from-IRI
  transform — the ticket text's premise ("already established and unit-tested in TICKET-018")
  does not hold: `problem-state.ts`'s own header comment says it explicitly avoided inventing
  such a heuristic ("avoids inventing a camelCase-derivation heuristic not present in the
  admitted graph"). No such heuristic existed to reuse; string concatenation on the
  already-PascalCase `skos:prefLabel` values (e.g. `"SessionEvent"` → `"handleSessionEvent"`) is
  simpler and needs no heuristic at all.
- Real (non-dry) `ggen sync run` wrote `lib/domain/event-routing.ts` for the first time —
  `"lib/domain/event-routing.ts": "written"` in the sync summary.
- **Real test**, `tests/domain/event-routing.test.ts`, matching this pack's own established
  Chicago-TDD pattern (`lib/domain/__tests__/capability.count.test.mjs`'s real-rdflib-subprocess
  re-derivation, not a hardcoded constant): spawns `python3` running the live
  `queries/event-families.rq` against the live `ontology.ttl` and asserts the generated module's
  `EVENT_ROUTING_ENTRY_COUNT`/`Object.keys(EVENT_ROUTING).length` match that live count (15), plus
  a per-family presence check and the `SessionEvent → "handleSessionEvent"` acceptance case.
  `npx vitest run` → `tests/domain/event-routing.test.ts (3 tests)`, all pass.
- `npx tsc --noEmit`: zero errors.
- Idempotency: second real sync reported
  `"lib/domain/event-routing.ts": "skipped: unchanged: content identical"`.
- SHA-256: `3ca1df5831b5211a7e4abfcd032e499f5dea61987be330b004a9f5c82252eee8`.
