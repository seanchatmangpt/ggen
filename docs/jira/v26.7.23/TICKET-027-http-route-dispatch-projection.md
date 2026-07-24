# TICKET-027: HTTP route dispatch projection

## Status

PARTIAL_ALIVE

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (7 hydra:Operation capabilities)`
- PRD requirements: (none)
- ARD components:
  - `ARD §6 Sandbox API boundary`
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the capability-id -> handler-function dispatch table that TICKET-013's routes call into, completing the route-to-capability wiring.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 85%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 15%
- Expected ratio: 85/15
- Custom-code justification: the dispatch table's handler slots are generated, but each slot ultimately resolves to a workstream H custom adapter call — the dispatch TABLE itself is 100% generated, 15% reflects the immediate adjacency to custom code, consistent with TICKET-013's classification.

## Inputs

- TICKET-026 capability.ts
- queries/hydra-operations.rq (TICKET-013 reuse)

## Outputs

- examples/interview-assist/lib/domain/capability-dispatch.ts (CapabilityId -> handler function slot map)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Generic dispatch-table generation, one slot per hydra:Operation capability, typed against TICKET-026's Request/Response types.

## Domain-data responsibility

Which 7 capabilities are dispatchable via HTTP is fixed by the hydra:Operation typing in RDF.

## Custom-code boundary

NONE in this ticket's output — the dispatch table's slots are typed function references; TICKET-035 (subprocess sandbox executor) and others provide the actual implementations wired in at integration time.

## Exclusions

- no capability-specific logic inline in the dispatch table generation, only typed slot declarations

## Implementation steps

1. Generate one dispatch-table slot per hydra:Operation capability, typed to accept/return TICKET-026's Request/Response types.
2. Leave slots as typed function-reference placeholders pending workstream H wiring.
3. Verify 7 slots, matching TICKET-013's route count exactly.

## Admission gates

- TICKET-026.
- TICKET-013.

## Acceptance criteria

- Given 7 hydra:Operation capabilities, when generation runs, then capability-dispatch.ts has exactly 7 typed slots, matching TICKET-013's 7 routes 1:1.

## Negative tests

- Attempt to call a dispatch slot for a capability NOT typed hydra:Operation (e.g. capability/session/join-session) at the type level and confirm `tsc` rejects it — proving the HTTP-shaped subset is enforced at compile time, not just convention.

## Verification ladder

- Unit: generated output structure / type-check test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- capability-dispatch.ts hash
- 7-slot count confirmation, cross-checked against TICKET-013

## Dependencies

- TICKET-026
- TICKET-013

## Falsifier

If the dispatch table's slot count doesn't match TICKET-013's route count exactly, this ticket is not complete.

## Handoff

TICKET-034-039 (workstream H) wire their custom adapters into these typed slots.

## Definition of done

- dispatch table generated with 7 typed slots matching TICKET-013's routes
- type-level enforcement of the HTTP-shaped capability subset verified

## Implementation notes (real evidence) — closes as PARTIAL_ALIVE

- Reused `queries/hydra-operations.rq` unmodified against `ontology/30-capabilities.ttl` via
  rdflib. **Discrepancy found and documented, not silently forced to match:** the ticket's own
  Source line and comment in `30-capabilities.ttl` (line 14) both assert 7 hydra:Operation
  capabilities (the conceptual ARD §6 `InterviewSandbox` trait method count, collapsing
  create/open/modify-file into one `apply_file_delta` and run-visible/hidden/complete-test-suite
  into one `run_tests`). The live ontology actually types **9 discrete resources**
  `hydra:Operation`: `create-session`, `create-file`, `open-file`, `modify-file`, `compile`,
  `execute`, `run-visible-test`, `run-hidden-test`, `run-complete-test-suite` — re-verified live
  via `hydra-operations.rq`, not assumed from the comment. This is a real ontology/ticket-text
  divergence (TICKET-013's route count would need the same correction), reported here honestly
  rather than truncated to 7 to match stale ticket prose.
- Wrote `examples/interview-assist/lib/domain/capability-dispatch.ts` (9 typed
  `HttpCapabilityId` slots in `CAPABILITY_DISPATCH`, each `undefined` pending workstream H wiring)
  and the reusable template `packs/wasm4pm-interview-assist-pack/templates/027_capability_dispatch_ts.tmpl`.
  Real check: `node --experimental-strip-types -e "import('../capability-dispatch.ts')..."` ->
  `9 9` (HTTP_CAPABILITY_COUNT and Object.keys(CAPABILITY_DISPATCH).length both 9, matching the
  live query, not TICKET-013's stated 7).
- Type-level enforcement of the HTTP-shaped subset (the ticket's negative test) is real by
  construction — `HttpCapabilityId` is a narrower union than `CapabilityId`; a slot for
  `capability/session/join-session` (not hydra:Operation) would be a `tsc` type error since it is
  not assignable to `HttpCapabilityId`. Not separately compiled with `tsc` this round (no
  `tsconfig.json`/`node_modules` exist yet in `examples/interview-assist/` — app-shell workstream
  C's responsibility), so this is a structural/by-construction claim, not a captured `tsc` error
  transcript — noted as the gap, not glossed over.
- Why PARTIAL_ALIVE not ALIVE: the 7-vs-9 discrepancy against TICKET-013 needs reconciliation
  (out of this round's scope — TICKET-013 is not one of the assigned tickets), and the `tsc`
  negative-test transcript is not yet captured.
