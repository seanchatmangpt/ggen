# TICKET-054: Projection receipt + custom-code ownership manifest

## Status

PARTIAL_ALIVE

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

## Implementation notes (real evidence)

- `packs/wasm4pm-interview-assist-pack/scripts/hash-tree.py` written (real BLAKE3 tree hasher,
  same algorithm/style as `scripts/hash-corpus.py`). Run for real against `queries/` and
  `templates/`: `queries/` -> 16 files, combined hash
  `b97054570d591f6c6661de012e0b8a28ecbb9db1e6e51e75e431deec020ab077`; `templates/` -> 10 files,
  combined hash `1aa11354551077777eea2ff468503dc903c3244c0deac12ff7def6334c1c12cc`.
- `projection-receipt.json` aggregates: TICKET-001's `corpus-manifest.json` `combined_hash`
  (`0c7e5242...`, read verbatim not recomputed) + the two tree hashes above. TICKET-010's
  `projection-manifest.json` (a listed Input) was searched for and NOT found on disk
  (`find . -maxdepth 1 -iname "projection-manifest.json"` -> empty) -- the `output_manifest`
  field is honestly `null` rather than fabricated, and `missing_inputs` documents this. This is
  why the ticket is PARTIAL_ALIVE, not ALIVE: TICKET-054 depends on TICKET-010, which has not
  landed.
- `custom-code-ownership-manifest.json`: ran `find examples/interview-assist/lib/adapters -type
  f` twice during this session (~5 minutes apart, to give parallel workstream-H agents time, per
  the task instruction) -- both runs returned the same 7 files: `monaco-adapter.ts`,
  `sandbox-executor.ts`, `persistence-adapter.ts`, `ollama-adapter.ts`, `checksum-adapter.ts`,
  `accessibility-platform-adapter.ts`, `policy-check-stub.ts`. 6 of 7 matched to their owning
  TICKET-034..039 by that ticket's real Outputs list, with justification/reduction-path text
  copied verbatim from each ticket (not paraphrased). 1 file, `policy-check-stub.ts`, does not
  appear in any of TICKET-034..039's Outputs lists as read in this session -- flagged as an
  `orphan_files` entry rather than given an invented justification.
  Cross-checked TICKET-034..039's own Status headers: all 6 are still `PLANNED` in the tracker
  despite their output files already existing on disk -- noted in the manifest's `status` field
  ("workstream H ticket statuses not confirmed ALIVE") since this ticket's own Admission gates
  require "All workstream H tickets (034-039) complete," which is not yet true.
- Negative-test / completeness check (per the ticket's own falsifier): every file returned by the
  real `find` has a manifest entry (7 files, 7 entries) -- the check passes in the sense that
  nothing is silently omitted, but 1 entry is marked orphan/incomplete rather than a full pass,
  which is why the manifest's own `status` field says "SNAPSHOT -- INCOMPLETE" and this ticket is
  PARTIAL_ALIVE, not ALIVE.
- Both manifest files are explicitly marked as snapshots requiring re-generation once TICKET-010
  and all of TICKET-034-039 close, per this ticket's own Admission gates and the task
  instruction.
