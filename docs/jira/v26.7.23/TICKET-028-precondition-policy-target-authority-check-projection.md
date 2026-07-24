# TICKET-028: Precondition/policy-target/authority-check projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (dcterms:requires chains)`
  - `packs/wasm4pm-interview-assist-pack/ontology/50-policy.ttl (6 odrl:Set policies, 8 authority-action/*)`
- ARD components:
  - `ARD §3.15 Authority Broker`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies:
  - `policy/authority-broker-default`
  - `policy/practice-mode`
  - `policy/mock-interview-mode`
  - `policy/live-assistance-mode`
  - `policy/assessment-mode`
  - `policy/prohibited-mode`
- SHACL shapes: (none)

## Objective

Generate the precondition-check function (walking each capability's dcterms:requires chain) and the policy-check function (matching odrl:Permission/Prohibition against the active operating-mode policy set) that gate every capability dispatch before execution.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 85%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 15%
- Expected ratio: 85/15
- Custom-code justification: graph-walk (transitive dcterms:requires closure) and ODRL permission/prohibition matching are generic algorithms but require careful design of the traversal/matching logic itself, same classification basis as TICKET-023/025.

## Inputs

- TICKET-026 capability.ts
- TICKET-019 authority-state.ts
- queries/policy-permissions.rq, queries/capability-preconditions.rq (new)

## Outputs

- examples/interview-assist/lib/domain/preconditions.ts (checkPreconditions(capabilityId, state))
- examples/interview-assist/lib/domain/policy-check.ts (checkPolicy(capabilityId, activeMode))

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Generic transitive-closure precondition walker + generic ODRL permission/prohibition matcher, both reusable structural algorithms parameterized entirely by RDF-selected data.

## Domain-data responsibility

The actual precondition chains and policy permission/prohibition sets live in 30-capabilities.ttl and 50-policy.ttl.

## Custom-code boundary

NONE.

## Exclusions

- no capability-specific precondition literal (e.g. 'execute requires compile') hardcoded outside the dcterms:requires-driven traversal — this is the exact case flagged UNSUPPORTED-as-a-stretch in the prior TTL report, now consumed here honestly, not silently treated as certain

## Implementation steps

1. Query dcterms:requires chains per capability, compute transitive closure.
2. Query odrl:Permission/Prohibition sets per policy/* resource.
3. Generate checkPreconditions walking the closure against current state.
4. Generate checkPolicy matching capability against the active mode's odrl:Set.
5. Test against policy/prohibited-mode's 6 prohibited-action/* resources explicitly.

## Admission gates

- TICKET-026.
- TICKET-019.

## Acceptance criteria

- Given policy/prohibited-mode active, when checkPolicy is called for prohibited-action/hidden-overlay, then it returns denied.
- Given capability/runtime/execute's precondition (requires compile), when checkPreconditions is called without a prior compile, then it returns unmet.

## Negative tests

- Call checkPolicy for a permitted action under policy/practice-mode and confirm it returns allowed — the positive-path test proving the checker isn't fail-closed-always (a different, equally wrong failure mode).

## Verification ladder

- Unit: precondition-closure and policy-match unit tests against real RDF-derived data, both positive and negative cases
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- preconditions.ts/policy-check.ts hashes

## Dependencies

- TICKET-026
- TICKET-019

## Falsifier

If checkPolicy allows a prohibited-action/* capability under policy/prohibited-mode, this ticket is not complete — this is a safety-critical negative test, not optional.

## Handoff

TICKET-029 (timeout/result/refusal handling) and TICKET-035 (sandbox executor) call these checks before every dispatch.

## Definition of done

- both checkers generated and RDF-driven
- prohibited-mode denial test passes as a hard safety gate
- practice-mode allowance test passes
