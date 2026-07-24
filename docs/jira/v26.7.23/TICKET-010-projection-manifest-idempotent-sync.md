# TICKET-010: Projection manifest + idempotent synchronization

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources: (none)
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Establish the projection manifest (per-ticket outputs, source graph hash, query hash, template hash) that later receipt tickets (054-057) depend on, and verify `ggen sync run` is idempotent against this pack once templates exist (re-running with no graph change produces zero diffs).

## Current state

BLOCKED — no templates exist yet to synchronize; this ticket's idempotency test is exercised progressively as workstream C-G tickets land, not completable in isolation.

## Target state

packs/wasm4pm-interview-assist-pack/projection-manifest.json (or equivalent) tracks, per output file, which source graph hash / query / template produced it; a documented idempotent-sync test proves re-running `ggen sync run` with an unchanged graph produces a zero-diff result.

## Projection classification

- Template: 85%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 15%
- Expected ratio: 85/15
- Custom-code justification: manifest emission piggybacks on ggen-engine's own existing receipt-writing machinery (BLAKE3 chain, already used elsewhere in this repo) rather than reinventing it; the 15% is wiring this pack's specific outputs into that existing mechanism.

## Inputs

- TICKET-001 corpus-manifest.json
- every template ticket's declared Outputs (accumulates as workstreams C-G land)

## Outputs

- packs/wasm4pm-interview-assist-pack/projection-manifest.json

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The manifest schema and idempotency test harness are reusable across any future pack.

## Domain-data responsibility

None — this tracks provenance of files, not domain meaning.

## Custom-code boundary

Wiring into ggen-engine's existing `write_receipt`/BLAKE3 chain machinery (crates/ggen-engine/src/sync.rs) — reuse, not reimplementation.

## Exclusions

- no `generated/` directory
- no duplicate hashing logic beyond what ggen-engine already provides

## Implementation steps

1. Once at least one template exists (from workstream C+), run `ggen sync run` twice with no source changes between runs.
2. Confirm the second run produces zero file diffs and the receipt chain hash is stable (or advances only due to timestamp fields the receipt format itself defines, not content).
3. Record the manifest linking each output file to its source graph hash, query hash, and template hash.
4. Repeat the idempotency check incrementally as each workstream C-G ticket adds new templates, not only once at the end.

## Admission gates

- TICKET-006 (resolved single-file ontology.ttil layout).
- TICKET-009 (gates).

## Acceptance criteria

- Given an unchanged source graph, when `ggen sync run` executes twice in a row, then the second run's output is byte-identical to the first.

## Negative tests

- Change one triple in the source graph, re-run sync, and confirm the affected output file(s) — and only those — change, proving the projection genuinely depends on the graph rather than being cached/static.

## Verification ladder

- Unit: N/A with reason — this is an integration-level property (idempotency), not a unit-testable function
- Integration: double-run diff test, incrementally exercised as templates land
- End-to-end: full `ggen sync run` against the real pack once workstream C+ exists
- Chaos: N/A with reason — single-process sequential sync runs
- Stress: N/A with reason — small corpus
- Benchmark: N/A with reason — no perf target at this phase
- Verifier report: double-run diff result + single-triple-change diff result

## Receipts

- projection-manifest.json
- double-run zero-diff confirmation

## Dependencies

- TICKET-006
- TICKET-009

## Falsifier

If two consecutive `ggen sync run` invocations against an unchanged graph produce different output content, this ticket is not complete.

## Handoff

TICKET-054 (projection receipt) consumes this manifest directly.

## Definition of done

- projection-manifest.json schema defined
- idempotency verified for at least the first template that lands, with a plan to re-verify per subsequent workstream
