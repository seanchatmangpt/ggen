# TICKET-026: Capability identifier + request/response type projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (98 capability/* schema:Action resources)`
- PRD requirements:
  - `PRD §8 Capability ontology`
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the full 98-member CapabilityId union type and a Request/Response type pair per capability, satisfying the user's explicit requirement that 'the capability inventory must come from RDF' and 'templates may not contain an InterviewAssist-specific capability array.'

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 95%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 5%
- Expected ratio: 95/5
- Custom-code justification: a handful of capabilities imply non-trivial payload shapes (e.g. capability/runtime/execute needs stdin/timeout fields) requiring a small per-category payload-shape convention, documented once and mechanically applied.

## Inputs

- queries/capabilities.rq (TICKET-003)

## Outputs

- examples/interview-assist/lib/domain/capability.ts (98-member CapabilityId union + Request/Response interfaces per capability)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The single most load-bearing generic projection in this backlog: 'schema:Action resource -> CapabilityId member + typed request/response pair,' reusable for any future capability-ontology pack.

## Domain-data responsibility

All 98 capability identifiers and their category grouping live exclusively in 30-capabilities.ttl.

## Custom-code boundary

NONE.

## Exclusions

- ABSOLUTE: no InterviewAssist-specific capability array literal anywhere in any template — this is the single most important exclusion in the entire backlog per the user's explicit instruction
- no capability count hardcoded as a comment claim without a verifying query

## Implementation steps

1. Query all 98 capability/* resources with schema:name and dcterms:isPartOf (category).
2. Generate the CapabilityId union (98 members).
3. Apply the documented per-category payload-shape convention to generate Request/Response interfaces.
4. Verify member count is exactly 98, cross-checked against the prior TTL report's independently-verified count.

## Admission gates

- TICKET-004.

## Acceptance criteria

- Given 98 capability/* resources, when generation runs, then CapabilityId has exactly 98 members, and this count is re-derived by a live SPARQL query at generation time, never hardcoded.

## Negative tests

- Add a 99th capability/* resource to a fixture and confirm the generated union grows to 99 automatically — the primary proof this ticket satisfies the user's 'capability inventory must come from RDF' requirement.

## Verification ladder

- Unit: 98-member-count assertion re-derived from a live query, not a hardcoded constant in the test itself
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- capability.ts hash
- 98-member-count confirmation via live query

## Dependencies

- TICKET-004

## Falsifier

If capability.ts's member count is ever hardcoded rather than re-derived from a live SPARQL query at generation time, or if it doesn't match the corpus's actual capability/* count, this ticket is not complete.

## Handoff

TICKET-013 (routes), TICKET-027 (dispatch), TICKET-028 (preconditions) all import CapabilityId from here.

## Definition of done

- CapabilityId generated with 98 members matching a live query
- Request/Response types generated per capability
- zero hardcoded capability arrays anywhere
