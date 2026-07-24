# TICKET-011: Next.js package metadata + root configuration generation

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/10-product.ttl (<product/interview-assist> a schema:SoftwareApplication, doap:Project)`
- PRD requirements: (none)
- ARD components:
  - `ARD §5 Initial technology choices (doc/ard#tech-choices)`
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate package.json name/version/description fields and root Next.js config (next.config, tsconfig base) from <product/interview-assist>'s schema:name/schema:version/doap:programming-language triples, so product identity lives in one place (RDF) rather than being duplicated by hand into package.json.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 90%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 10%
- Expected ratio: 90/10
- Custom-code justification: package.json's dependency version pins are external ecosystem facts, not derivable from the graph — recorded as a small hand-maintained dependency list within an otherwise-generated file.

## Inputs

- queries/product-metadata.rq (new, added to TICKET-003's catalog)
- packs/wasm4pm-interview-assist-pack/ontology/10-product.ttl

## Outputs

- examples/interview-assist/package.json (name/version/description sections)
- examples/interview-assist/next.config.ts
- examples/interview-assist/tsconfig.json (base compilerOptions)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Tera template producing package.json/next.config.ts/tsconfig.json structure, with product name/version/description as Tera variables bound from the SPARQL query, not hardcoded strings.

## Domain-data responsibility

Product name, version, description remain in <product/interview-assist> in 10-product.ttl; changing the product's displayed name requires editing RDF, not the template.

## Custom-code boundary

Dependency version list (react, next, typescript, etc.) — external ecosystem facts not representable as InterviewAssist domain RDF; hand-maintained within the generated file's dependency block.

## Exclusions

- no product name/version/description literal hardcoded in the template outside the SPARQL-bound variable
- no `generated/` directory — output is examples/interview-assist/ directly, first-class source

## Implementation steps

1. Add product-metadata.rq querying <product/interview-assist>'s schema:name, schema:version, dcterms:description, doap:programming-language.
2. Author package.json.tmpl binding those variables into the name/version/description fields.
3. Author next.config.ts.tmpl and tsconfig.json.tmpl with structural (non-domain) TypeScript/Next.js configuration.
4. Run `ggen sync run` (once TICKET-006/009/010 land) and verify the emitted package.json's name/version match the RDF source exactly.

## Admission gates

- TICKET-004 (mapping table).
- TICKET-005 (refusal macro) wraps the product-metadata query as required.

## Acceptance criteria

- Given <product/interview-assist> has schema:name X, when package.json is generated, then package.json's "name" field equals X exactly (normalized to npm package-name rules, documented in the template).

## Negative tests

- Delete schema:version from <product/interview-assist> in a fixture copy and confirm generation fails via the TICKET-005 refusal macro rather than emitting package.json with an empty version string.

## Verification ladder

- Unit: generated output structure / type-check test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- package.json/next.config.ts/tsconfig.json content hashes
- source query hash

## Dependencies

- TICKET-004
- TICKET-005
- TICKET-008

## Falsifier

If package.json's name/version ever diverges from <product/interview-assist>'s RDF triples after a sync run, this ticket is not complete.

## Handoff

TICKET-012 builds the app layout on top of this root config.

## Definition of done

- package.json/next.config.ts/tsconfig.json generated and verified against RDF source
- negative test (missing required field) passes
