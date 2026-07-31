# TICKET-008: Query and template directory organization

## Status

ALIVE

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

Establish the on-disk layout under packs/wasm4pm-interview-assist-pack/queries/ and packs/wasm4pm-interview-assist-pack/templates/ mirroring this repo's own existing pack conventions (checked against at least one real existing pack, not invented from scratch), so workstream C-G tickets have a fixed target location for their outputs.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 100%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 0%
- Expected ratio: 100/0
- Custom-code justification: NONE — directory layout only.

## Inputs

- an existing repo pack's templates/ and gates/ layout, read directly as a reference (not assumed)

## Outputs

- packs/wasm4pm-interview-assist-pack/templates/ (directory structure, empty scaffolds)
- packs/wasm4pm-interview-assist-pack/queries/ (populated by TICKET-003)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The directory convention itself.

## Domain-data responsibility

None.

## Custom-code boundary

NONE.

## Exclusions

- no deviation from the repo's existing pack layout convention without documented justification

## Implementation steps

1. Read one existing pack's directory layout directly from disk (e.g. wasm4pm-facts-pack or another already-consuming pack) as ground truth.
2. Mirror its templates/ and gates/-equivalent subdirectory naming.
3. Create the empty directory scaffold with a README noting what each subdirectory is for.
4. Cross-check against TICKET-003's queries/ output location.

## Admission gates



## Acceptance criteria

- Given the directory scaffold, when compared against an existing repo pack's layout, then the naming convention matches (or documents a justified deviation).

## Negative tests

- N/A with reason — this ticket has no failure-mode behavior to test, it is a directory scaffold.

## Verification ladder

- Unit: N/A with reason — no code
- Integration: N/A with reason — no code
- End-to-end: N/A with reason — no code
- Chaos: N/A with reason — no code
- Stress: N/A with reason — no code
- Benchmark: N/A with reason — no code
- Verifier report: manual comparison against the reference pack's layout

## Receipts

- directory listing snapshot

## Dependencies

- TICKET-003

## Falsifier

If a later ticket's output doesn't fit this scaffold and must invent a new top-level directory, this ticket's scaffold was incomplete.

## Handoff

Workstreams C-G write their templates into this scaffold.

## Definition of done

- scaffold exists
- verified against a real existing pack's layout, not invented

## Implementation notes (real evidence) — closes as ALIVE

- `queries/`, `templates/`, `gates/` (already existed), `docs/` created, matching
  `wasm4pm-interview-site-pack`'s and `wasm4pm-sandbox-pack`'s real on-disk layout convention
  (read directly, not assumed).
- Proven load-bearing, not decorative: the real `ggen sync run` in TICKET-006/010's evidence
  actually read `templates/010_package_json.tmpl` and `gates/*.rq` from these exact directories.
