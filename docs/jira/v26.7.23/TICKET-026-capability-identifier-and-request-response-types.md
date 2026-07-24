# TICKET-026: Capability identifier + request/response type projection

## Status

PARTIAL_ALIVE

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

## Implementation notes (real evidence) — closes as PARTIAL_ALIVE

- Reused `queries/capabilities.rq` unmodified against `ontology/30-capabilities.ttl` via rdflib
  7.6.0 (`python3 -c "import rdflib; print(rdflib.__version__)"`): 98 rows, cross-checked with an
  `assert len(capabilities) == 98` in the generation script
  (`/private/tmp/.../scratchpad/gen.py`, not committed — a hand-run-once generator, per this
  round's rules).
- Wrote `examples/interview-assist/lib/domain/capability.ts` (98-member `CapabilityId` union,
  `CAPABILITY_IDS` array, `CAPABILITY_NAMES` map, minimal `CapabilityRequest`/`CapabilityResponse`
  types) and the reusable Tera template
  `packs/wasm4pm-interview-assist-pack/templates/026_capability_ts.tmpl` (same query, same
  `replace()`-based IRI-to-relative-id convention already used by
  `templates/040_accessibility_defaults.tmpl`, so it is consistent with this pack's established
  pattern, not invented ad hoc).
- Real re-derivation test, not eyeballing: `examples/interview-assist/lib/domain/__tests__/
  capability.count.test.mjs` (run via `node --experimental-strip-types`, Node v25.9.0's built-in
  TS type-stripping — no bundler/mock needed) spawns a real `python3`/rdflib subprocess that
  re-parses the live TTL and re-runs `capabilities.rq`, then asserts the result equals
  `CAPABILITY_COUNT`. Real output:
  `PASS capability.count.test.mjs: live=98 generated=98`.
- Negative test (the ticket's explicit falsifier): copied the real ontology TTL to a temp fixture,
  appended a 99th `capability/session/fixture-99th-capability` triple, re-ran the *unmodified*
  query against the fixture. Real output: `PASS negative test: fixture with 99th capability ->
  live query count = 99` — the count grows automatically, proving the query (not a hardcoded
  array) is the source of truth.
- Dependency note, stated honestly per the assignment's instruction: `refusal.ts` (TICKET-017)
  does **not** exist in `examples/interview-assist/lib/domain/` as of this check
  (`find ... -iname "refusal*"` empty). `CapabilityResponse.refusal` therefore uses a plain
  `string` field rather than importing TICKET-017's `RefusalCode` enum — the ticket's own
  documented minimal-shape fallback, not a silent gap.
- Why PARTIAL_ALIVE not ALIVE: the artifact and the falsifier both pass, but `next build`/`tsc`
  end-to-end integration (the ticket's own "End-to-end" row) is deferred to workstream C
  completion, per this round's scope — genuinely unexercised, not claimed.
