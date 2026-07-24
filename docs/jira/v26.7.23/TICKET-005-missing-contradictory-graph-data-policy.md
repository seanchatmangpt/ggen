# TICKET-005: Missing, contradictory, or unsupported graph-data handling policy

## Status

PARTIAL_ALIVE

## Implementation notes (real evidence)

- Policy doc: `packs/wasm4pm-interview-assist-pack/docs/graph-data-refusal-policy.md` (new).
- Macro: `packs/wasm4pm-interview-assist-pack/templates/_partials/require-nonempty.tera` (new) —
  `require_nonempty(rows, resource)` and `require_single(rows, field, resource)`, both generic
  (no domain constants), triggering a Tera undefined-variable render error on violation.
- Fixture query: `packs/wasm4pm-interview-assist-pack/queries/product-metadata-required.rq`
  (new, marked `# required`).
- Negative test run for real via rdflib (not fabricated): copied `ontology.ttl` to
  `/tmp/scratch-ontology.ttl`, removed `<product/interview-assist> schema:name "InterviewAssist"`,
  re-serialized, re-parsed, ran the fixture query — **0 rows** on the mutilated copy, **1 row** on
  the real `ontology.ttl`. Transcript in the policy doc.
- **Gap, honestly flagged**: the macro's actual fail-loudly behavior inside a real Tera render
  (does the undefined-variable trick really propagate to a non-zero `ggen sync run` exit code?)
  was NOT verified end-to-end against `ggen-engine`'s renderer this session — no template in this
  pack invokes the macro yet, and per the task's CRITICAL RULES this session must not run
  `ggen sync run` (non-dry) against `examples/interview-assist/`. This is why status is
  PARTIAL_ALIVE, not ALIVE: the SPARQL/rdflib-level refusal precondition is proven; the
  Tera-render-level propagation is asserted from Tera's documented default undefined-variable
  behavior, not independently executed.
- Not yet done: TICKET-003's query catalog headers have not been swept to mark every query
  required/optional (rule 4) — only the one new fixture query carries the `# required` marker.

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/*.ttl`
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Define, before any template is written, the exact behavior when a template's SPARQL query returns zero rows, contradictory rows, or a shape not covered by an existing SHACL NodeShape — so this failure mode is a designed refusal, not an accidental empty-string emission discovered later.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

A written policy (and where mechanically enforceable, a shared template macro/partial) stating: zero rows on a required query -> projection MUST fail loudly (non-zero exit, named missing resource), never silently emit an empty file or placeholder; contradictory rows (e.g. two schema:name for one resource) -> projection MUST fail and name both conflicting values; a class/predicate combination with no SHACL coverage -> flagged as UNSUPPORTED in the projection manifest (TICKET-010), not silently accepted.

## Projection classification

- Template: 90%
- Domain data: the specific failure conditions reference domain classes but the refusal MECHANISM is generic
- Custom code: 10%
- Expected ratio: 90/10
- Custom-code justification: the policy is documentation + a shared Tera macro for the fail-loudly behavior; the 10% is the actual process-exit-code wiring in the ggen template runner's own harness (already exists, not to be reimplemented — see verification of usage).

## Inputs

- packs/wasm4pm-interview-assist-pack/docs/projection-mapping.md (TICKET-004)
- TICKET-003 query catalog

## Outputs

- packs/wasm4pm-interview-assist-pack/docs/graph-data-refusal-policy.md
- packs/wasm4pm-interview-assist-pack/templates/_partials/require-nonempty.tera (or equivalent Tera macro, if the templating layer supports it)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

A shared 'assert non-empty / assert non-contradictory' Tera macro/partial, reusable across every template in workstreams C-G.

## Domain-data responsibility

What counts as a 'required' vs 'optional' query result is a domain-level judgment call recorded per query in the query catalog (TICKET-003) headers, not invented here.

## Custom-code boundary

Whatever minimal glue is needed to make a Tera template render failure actually propagate as a non-zero `ggen sync run` exit code — verify this already works via the engine's existing error propagation rather than building new plumbing.

## Exclusions

- no fail-open behavior (per this repo's own coding-agent-mistakes.md Mistake Class 3)
- no silent empty-file emission
- no domain constants in the refusal macro itself

## Implementation steps

1. Enumerate every query in TICKET-003's catalog as 'required' (projection cannot proceed without ≥1 row) or 'optional' (empty result is a valid domain state, e.g. no refusals recorded yet).
2. Write the fail-loudly Tera macro/partial.
3. Verify, using a deliberately empty-result query against a fixture graph, that a template invoking the macro actually causes `ggen sync run` (once wired in a later ticket) to exit non-zero rather than emit a blank file.
4. Document the contradictory-data case (two conflicting bindings for what should be a single value) and how the macro detects it.

## Admission gates

- TICKET-003 complete.

## Acceptance criteria

- Given a required query returns zero rows, when the template renders, then the build fails with a message naming the missing query/resource, not a blank output file.
- Given a query returns two conflicting bindings for a single-valued field, when the template renders, then the build fails naming both conflicting values.

## Negative tests

- Point a 'required' query at a corpus copy with the target resource deleted and confirm the build fails rather than emitting an empty TypeScript file — this is itself the primary test for this ticket, not a secondary one.

## Verification ladder

- Unit: macro unit test against a fixture graph with a deliberately missing resource
- Integration: macro invoked from a real template against the real corpus (should pass silently since real corpus is complete)
- End-to-end: N/A with reason — no full runtime surface exists yet at this point in the dependency chain
- Chaos: N/A with reason — single deterministic build, no concurrency
- Stress: N/A with reason — small corpus
- Benchmark: N/A with reason — no perf target
- Verifier report: fail-loudly test transcript (both the failing and passing case) as the verifier artifact

## Receipts

- macro source hash
- fail-loudly test transcript

## Dependencies

- TICKET-003
- TICKET-004

## Falsifier

If a template using this macro against a corpus missing a required resource still emits a non-empty output file, this ticket is not complete — that is exactly the Decorative Completion failure class this repo's coding-agent-mistakes.md forbids.

## Handoff

Every workstream C-G template ticket wraps its required-query invocations in this macro.

## Definition of done

- policy doc exists
- macro exists and is exercised by at least one passing and one failing fixture test
- referenced from projection-mapping.md
