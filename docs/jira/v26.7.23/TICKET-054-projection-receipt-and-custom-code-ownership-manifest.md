# TICKET-054: Projection receipt + custom-code ownership manifest

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/60-provenance-receipts.ttl`
- ARD components:
  - `ARD §3.16 Receipt Ledger`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Extend TICKET-010's projection manifest into the full projection receipt required by the epic: source graph hash (TICKET-001), query hash (TICKET-003), template hash (per-template), output manifest, AND a custom-code ownership manifest explicitly listing every file from workstream H with its justification and reduction path, so 'what is generated vs custom' is a receipted fact, not a claim.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 90%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 10%
- Expected ratio: 90/10
- Custom-code justification: reuses ggen-engine's existing receipt-writing machinery; 10% is wiring this pack's specific custom-code manifest into that mechanism.

## Inputs

- TICKET-001 corpus-manifest.json
- TICKET-003 query catalog
- TICKET-010 projection-manifest.json
- every workstream H ticket's Custom-code boundary + reduction path text

## Outputs

- packs/wasm4pm-interview-assist-pack/projection-receipt.json (source graph hash, query hashes, template hashes, output manifest)
- packs/wasm4pm-interview-assist-pack/custom-code-ownership-manifest.json (one entry per workstream H adapter: file, justification, generated-port-it-implements, reduction-path)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Manifest schema and emission, reusing ggen-engine's existing BLAKE3 receipt chain.

## Domain-data responsibility

None — this tracks build provenance, not domain meaning.

## Custom-code boundary

Wiring into ggen-engine's existing receipt machinery — reuse, not reimplementation.

## Exclusions

- no custom-code file omitted from the ownership manifest
- no `generated/` directory

## Implementation steps

1. Aggregate TICKET-001/003/010's hashes into one projection-receipt.json.
2. Enumerate every workstream H output file into custom-code-ownership-manifest.json with its justification/reduction-path text verbatim from its ticket.
3. Verify every file under lib/adapters/ has a corresponding manifest entry — no orphaned custom file.

## Admission gates

- TICKET-010.
- All workstream H tickets (034-039) complete.

## Acceptance criteria

- Given the composed system, when custom-code-ownership-manifest.json is checked against a real `find lib/adapters -type f`, then every file is accounted for with a justification, and no manifest entry references a non-existent file.

## Negative tests

- Add an undocumented file to lib/adapters/ without a manifest entry and confirm a CI check (grep/diff between find output and manifest) fails.

## Verification ladder

- Unit: manifest-completeness check against a real file listing
- Integration: N/A with reason — this is itself an aggregation/reporting ticket
- End-to-end: N/A with reason
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: manifest-completeness check result

## Receipts

- projection-receipt.json
- custom-code-ownership-manifest.json

## Dependencies

- TICKET-010
- TICKET-034
- TICKET-035
- TICKET-036
- TICKET-037
- TICKET-038
- TICKET-039

## Falsifier

If any file under lib/adapters/ has no corresponding ownership-manifest entry, this ticket is not complete.

## Handoff

TICKET-056/057 (final receipt/verifier report) fold this manifest in.

## Definition of done

- both manifests generated
- completeness check against real filesystem passes
