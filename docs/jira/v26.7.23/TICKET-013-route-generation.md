# TICKET-013: Route generation from capability HTTP operations

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (7 hydra:Operation-typed capability/* resources)`
- PRD requirements: (none)
- ARD components:
  - `ARD §6 Sandbox API boundary (doc/ard#sandbox-api)`
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate one Next.js API route file per hydra:Operation-typed capability (create-session, create-file, open-file, modify-file, compile, execute, run-* tests), with hydra:method determining the exported HTTP verb handler, and route logic delegating to a generated capability-dispatch function (workstream F) rather than containing domain logic inline.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 80%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 20%
- Expected ratio: 80/20
- Custom-code justification: each route handler's actual body ultimately calls a thin custom adapter (workstream H) for real subprocess/sandbox execution; the route file itself is 100% generated, but its runtime behavior bottoms out in custom code by design — the 20% here anticipates that composition, not custom code inside this ticket's own output.

## Inputs

- queries/hydra-operations.rq (new)
- packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl

## Outputs

- examples/interview-assist/app/api/sandbox/[capability]/route.ts (single dynamic route dispatching by capability id, OR one route.ts per hydra:Operation — decision recorded)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Route-handler wiring: HTTP verb -> capability id -> dispatch call, generated per the 7 hydra:Operation resources, method taken from hydra:method exactly.

## Domain-data responsibility

Which capabilities are HTTP-shaped, and their HTTP methods, live in 30-capabilities.ttl's hydra:Operation/hydra:method triples — never hardcoded in the route template.

## Custom-code boundary

NONE in this ticket's own output; the generated route calls into workstream F's generated dispatch table, which in turn calls workstream H's custom adapters. This ticket produces zero custom code itself.

## Exclusions

- no capability-specific business logic inline in route.ts
- no hardcoded capability-id-to-method mapping outside the hydra:method-bound template variable

## Implementation steps

1. Add hydra-operations.rq selecting all schema:Action, hydra:Operation-typed resources with their hydra:method.
2. Verify exactly 7 rows returned (create-session, create-file, open-file, modify-file, compile, execute, and one run-* test — cross-check against 30-capabilities.ttl's comment claiming 7).
3. Author route.ts.tmpl generating a handler per HTTP method that dispatches to the workstream F capability registry by capability id.
4. Verify generated route methods (GET/POST) match hydra:method exactly for each capability.

## Admission gates

- TICKET-012.
- TICKET-026 (capability registry) for the dispatch target — may be built in parallel and wired last.

## Acceptance criteria

- Given 7 hydra:Operation capabilities, when routes are generated, then exactly 7 HTTP method handlers exist, each with the HTTP verb matching its capability's hydra:method exactly.

## Negative tests

- Add an 8th hydra:Operation to a fixture copy and confirm route generation picks it up automatically (no template edit required) — proving genuine RDF-drivenness, not a hardcoded 7-item list.

## Verification ladder

- Unit: generated output structure / type-check test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- route.ts hash(es)
- hydra:Operation row count confirmation (7)

## Dependencies

- TICKET-012

## Falsifier

If a route's HTTP method doesn't match its capability's hydra:method triple, this ticket is not complete.

## Handoff

TICKET-026-029 (capability registry/API projection) provide the dispatch target these routes call.

## Definition of done

- routes generated 1:1 with hydra:Operation capabilities
- HTTP methods verified against hydra:method
- no domain logic inline
