# TICKET-020: Transition receipt types projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/60-provenance-receipts.ttl (<receipt/entry-1>, <receipt/entry-2-final>, <receipt/checksum-algorithm-blake3>)`
- ARD components:
  - `ARD §3.16 Receipt Ledger (doc/ard#components)`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the TransitionReceipt TypeScript type directly from the PROV-O + spdx:Checksum shape modeled in 60-provenance-receipts.ttl, so the runtime receipt format matches the admitted RDF receipt model field-for-field.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 90%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 10%
- Expected ratio: 90/10
- Custom-code justification: the actual BLAKE3 computation at runtime is custom code (TICKET-038); this ticket only generates the TYPE shape the custom code must satisfy.

## Inputs

- queries/receipt-shape.rq (new, selecting the predicates actually used on receipt/* resources: prov:used, prov:generated, prov:wasDerivedFrom, spdx:checksum/algorithm/checksumValue)

## Outputs

- examples/interview-assist/lib/domain/receipt.ts (TransitionReceipt interface + Checksum interface)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Generic 'PROV-O entity shape -> TypeScript interface' projection, matching predicates actually observed on receipt/* instances, not an idealized PROV-O schema.

## Domain-data responsibility

Which fields a receipt carries (used/generated/wasDerivedFrom/checksum) is fixed by the admitted 60-provenance-receipts.ttl shape.

## Custom-code boundary

NONE in this ticket; TICKET-038 (crypto adapter) and TICKET-054-057 (receipt tickets) implement runtime values conforming to this generated type.

## Exclusions

- no BLAKE3 computation logic in this ticket — type only
- no receipt field invented beyond what 60-provenance-receipts.ttl actually models

## Implementation steps

1. Query the predicate set actually used on receipt/entry-1 and receipt/entry-2-final.
2. Generate TransitionReceipt/Checksum interfaces matching exactly those predicates.
3. Verify the generated interface's field set has no extra/missing fields versus the RDF-observed predicate set.

## Admission gates

- TICKET-004.

## Acceptance criteria

- Given the receipt/* predicate set in RDF, when generation runs, then TransitionReceipt's TypeScript fields are a 1:1 match (prov:used -> used, prov:generated -> generated, prov:wasDerivedFrom -> derivedFrom, spdx:checksum -> checksum: Checksum).

## Negative tests

- Add a new predicate to receipt/entry-2-final in a fixture (e.g. a new prov:qualifiedGeneration) and confirm the generated type picks it up as a new optional field, not silently ignored.

## Verification ladder

- Unit: generated output structure / type-check test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- receipt.ts hash

## Dependencies

- TICKET-004

## Falsifier

If TransitionReceipt's fields don't match the RDF-observed predicate set on receipt/* resources, this ticket is not complete.

## Handoff

TICKET-054-057 (receipt/replay workstream J) implement/consume this type at runtime.

## Definition of done

- receipt.ts generated matching the RDF receipt shape exactly
