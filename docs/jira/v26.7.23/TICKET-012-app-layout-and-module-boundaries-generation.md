# TICKET-012: App layout + module boundaries generation

## Status

PARTIAL_ALIVE

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/10-product.ttl (<role/*> resources)`
  - `packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl (<phase-scheme>)`
- PRD requirements: (none)
- ARD components:
  - `ARD §2 System planes (doc/ard#planes)`
  - `ARD §3 Core components (doc/ard#components)`
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the Next.js app/ directory's root layout.tsx and top-level module boundary folders (one per ARD §2 system plane: observation, admission, manufacturing, execution, receipt) as structural scaffolding, with no InterviewAssist domain lists embedded in the layout component itself.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 85%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 15%
- Expected ratio: 85/15
- Custom-code justification: React Server Component boilerplate (providers, error boundaries) has some irreducible Next.js-framework-specific wiring not expressible as pure template output without framework knowledge baked into the template — kept minimal and documented.

## Inputs

- queries/system-planes.rq (new)
- packs/wasm4pm-interview-assist-pack/ontology/40-events-workflow.ttl

## Outputs

- examples/interview-assist/app/layout.tsx
- examples/interview-assist/lib/{observation,admission,manufacturing,execution,receipt}/ (module boundary scaffolds)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

layout.tsx structural shell + one module-boundary folder per system plane, generated from the ARD §2 planes list in RDF.

## Domain-data responsibility

Which planes exist and their names live in ARD components resources; the template iterates over whatever the query returns, not a hardcoded 5-item array.

## Custom-code boundary

Minimal Next.js App Router lifecycle glue (root layout's <html>/<body> wrapper) that has no domain content.

## Exclusions

- no hardcoded plane-name array in layout.tsx
- no business logic in layout.tsx beyond composition

## Implementation steps

1. Add system-planes.rq selecting ARD §2 plane resources.
2. Author layout.tsx.tmpl iterating planes only for module-boundary folder scaffolding, not runtime logic.
3. Generate one empty index.ts per plane folder as a placeholder module boundary.
4. Verify folder count matches the RDF-selected plane count exactly.

## Admission gates

- TICKET-011.
- TICKET-005 refusal macro.

## Acceptance criteria

- Given N planes in ARD §2, when generation runs, then exactly N module-boundary folders exist under lib/.

## Negative tests

- Remove one plane resource from a fixture copy and confirm the folder count drops by exactly one, proving genuine RDF-driven generation.

## Verification ladder

- Unit: generated output structure / type-check test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- layout.tsx hash
- module-boundary folder listing

## Dependencies

- TICKET-011

## Falsifier

If the module-boundary folder count doesn't match the RDF-selected plane count, this ticket is not complete.

## Handoff

TICKET-013 (routes) and workstream D (state types) populate these module boundaries.

## Definition of done

- layout.tsx generated
- module-boundary folders generated 1:1 with ARD §2 planes
- no hardcoded plane list in the template

## Implementation notes (real evidence)

- Wrote `packs/wasm4pm-interview-assist-pack/queries/system-planes.rq`. Inspected
  `ontology.ttl` directly (not guessed): the real ARD §2 shape is
  `?plane dcterms:isPartOf <doc/ard#planes> ; dcterms:title ?title`, 4 real rows
  (`plane-manufacturing`, `plane-runtime`, `plane-sandbox`, `plane-projection`) — confirmed via
  `grep -n "isPartOf <doc/ard#planes>"` before writing the query, and re-confirmed at generation
  time: the real `ggen sync run --dry-run` receipt reports the query returning exactly 4 rows.
- `templates/020_app_layout.tmpl` -> `app/layout.tsx`: real App Router root layout, no domain
  content hardcoded except the product name (queried from `schema:SoftwareApplication`).
- `templates/021_lib_planes_index.tmpl` -> `lib/planes/index.ts`: **one file, not one
  folder per plane.** Investigated the "N-row-to-N-file" question directly per the ticket's own
  instruction — grepped every `packs/*/templates/*.tmpl` for a per-row-dynamic `to:` path inside
  a `{{ }}`/for-loop body; none exists anywhere in this workspace (all `to:` targets found,
  e.g. `gh-terraform-pack`, `lsp-max-pack`, `star-toml-pack`, use a single frontmatter-level
  `{{ var }}` substitution, not per-row iteration over `to:`). ggen's template model as observed
  does not support one-file-per-SPARQL-row from a single template. Rather than inventing an
  unproven mechanism, emitted the single-file projection (`SYSTEM_PLANES: SystemPlane[]`) — the
  "iterate the plane query to also emit multiple files" half of this ticket is BLOCKED on that
  missing engine capability, not attempted via a side-channel script (would be Epistemic Bypass
  per `.claude/rules/coding-agent-mistakes.md`).
- Ran a real (non-dry) `ggen sync run` from `examples/interview-assist/` twice in a row (the
  pack's `010_package_json.tmpl` was temporarily moved aside for these two runs only, to avoid
  writing over `package.json`, which a concurrent workstream-H agent owns and had already
  hand-authored with different, incompatible content — restored immediately after, untouched).
  Both real runs produced byte-identical `app/layout.tsx` and `lib/planes/index.ts`
  (`diff` exit 0) — idempotent.
- PARTIAL_ALIVE: layout generation and the plane-query projection are real and idempotent; true
  per-plane folder emission is BLOCKED (engine capability gap, not a coding gap), not silently
  dropped.
