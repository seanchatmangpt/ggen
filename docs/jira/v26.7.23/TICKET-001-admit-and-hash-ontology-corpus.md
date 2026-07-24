# TICKET-001: Admit and hash the InterviewAssist ontology corpus

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/00-document.ttl … 80-acceptance.ttl (9 files, 1789 triples union)`
  - `packs/wasm4pm-interview-assist-pack/shapes/interview-assist.shacl.ttl`
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes:
  - `all 14 sh:NodeShape in interview-assist.shacl.ttl`

## Objective

Establish the admitted ontology corpus as a hashed, versioned input to every downstream projection ticket, so every later ticket can cite a concrete graph-hash rather than 'the current files'.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

A `corpus-manifest.json` (or equivalent RDF resource) lists the 9 ontology files + 1 shapes file with a BLAKE3 hash per file and one combined corpus hash. No projection ticket may proceed without citing this hash.

## Projection classification

- Template: 90%
- Domain data: the corpus being hashed is admitted domain data; the hashing mechanism is template/tooling
- Custom code: 10%
- Expected ratio: 90/10
- Custom-code justification: hashing itself is a generic reusable script/template; the only custom part is invoking a BLAKE3 library binding.

## Inputs

- packs/wasm4pm-interview-assist-pack/ontology/*.ttl
- packs/wasm4pm-interview-assist-pack/shapes/interview-assist.shacl.ttl

## Outputs

- packs/wasm4pm-interview-assist-pack/corpus-manifest.json (or .ttl using spdx:Checksum, matching the pattern already used in 60-provenance-receipts.ttl)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

A generic 'hash every file in a directory tree, emit a manifest' script — reusable for any future pack, not InterviewAssist-specific.

## Domain-data responsibility

None — this ticket touches no domain semantics, only file identity.

## Custom-code boundary

A BLAKE3 hashing call (already used elsewhere in this repo's receipt chain — reuse, don't reinvent).

## Exclusions

- no private ontology vocabulary
- no domain constants in templates
- no unreceipted actuation

## Implementation steps

1. Enumerate the 9 ontology files + 1 shapes file in a fixed, sorted order.
2. Compute a BLAKE3 hash per file.
3. Compute one combined corpus hash (hash of concatenated per-file hashes, in the same fixed order).
4. Emit corpus-manifest.json with file paths, per-file hashes, combined hash, and a timestamp.
5. Verify the manifest is reproducible: re-running step 1-4 without file changes yields an identical combined hash.

## Admission gates

- All 9 ontology files parse (rdflib) — verified prior report shows this holds.
- SHACL conforms — verified prior report shows this holds.

## Acceptance criteria

- Given the corpus is unchanged, when the hash script runs twice, then both runs produce an identical combined hash.
- Given any single byte in any ontology file changes, when the hash script re-runs, then the combined hash changes.

## Negative tests

- Corrupt one ontology file (introduce a syntax error) and confirm the manifest step refuses to hash a non-parsing corpus rather than silently hashing broken Turtle.

## Verification ladder

- Unit: hash-function determinism test (same input twice -> same output)
- Integration: run against the real 9-file corpus, not a fixture subset
- End-to-end: N/A with reason — no runtime surface yet, this ticket only establishes the manifest
- Chaos: N/A with reason — no concurrent writers to this manifest yet
- Stress: N/A with reason — corpus is small (1789 triples), no stress profile needed
- Benchmark: N/A with reason — hashing 9 small files has no meaningful perf target
- Verifier report: manifest is itself the verifier artifact for this ticket

## Receipts

- corpus-manifest.json combined hash
- per-file hashes

## Dependencies

(none — entry point of the dependency graph)

## Falsifier

If two runs of the hash script against an unchanged corpus produce different combined hashes, this ticket is not complete.

## Handoff

TICKET-002 consumes corpus-manifest.json as its input for the pre-projection validation gate.

## Definition of done

- corpus-manifest.json exists and is committed
- combined hash reproducible across two runs
- no domain data duplicated into the manifest beyond file paths/hashes
