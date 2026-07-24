# TICKET-029: Timeout/result/refusal handling projection

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability/runtime/enforce-timeout>, <capability/runtime/terminate-process>)`
- ARD components:
  - `ARD §10 Performance requirements (doc/ard#performance, 7 linked targets)`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the timeout-wrapper and result/refusal-normalization logic that every capability dispatch passes through, deriving the actual timeout DURATION values from ARD §10's performance-target resources rather than a magic number.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 80%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 20%
- Expected ratio: 80/20
- Custom-code justification: the actual timer/AbortController mechanism is JS-runtime-specific and composes with TICKET-035's real subprocess kill logic — generated wrapper structure, but its termination effect is only real once composed with custom code, mirroring TICKET-013/027's classification pattern.

## Inputs

- TICKET-017 refusal.ts
- TICKET-026 capability.ts
- queries/performance-targets.rq (new)

## Outputs

- examples/interview-assist/lib/domain/timeout-wrapper.ts (withTimeout(capabilityId, fn) -> AdmissionResult)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Generic timeout-wrapping higher-order function, parameterized by per-capability timeout duration selected from ARD §10's performance-target resources where declared, with a documented default otherwise.

## Domain-data responsibility

Timeout durations, where the PRD/ARD specifies them, come from ARD §10 resources; capabilities without an explicit target use a documented, reviewable default, not a silently-invented magic number.

## Custom-code boundary

The actual JS timer/AbortSignal composes with TICKET-035's real process-kill; this ticket's own output is 100% generated wrapper logic.

## Exclusions

- no magic-number timeout hardcoded without either an RDF source or an explicitly documented and reviewed default

## Implementation steps

1. Query ARD §10 performance-target resources for any explicit timeout/latency figures.
2. Generate withTimeout wrapping any capability dispatch, on timeout returning AdmissionResult refused with the enforce-timeout-sourced refusal code.
3. Document defaults for capabilities with no explicit RDF-sourced target.

## Admission gates

- TICKET-017.
- TICKET-026.

## Acceptance criteria

- Given a capability with an ARD §10 explicit timeout target, when its execution exceeds that duration, then withTimeout returns refused with the timeout refusal code, never hanging indefinitely.

## Negative tests

- Simulate a capability handler that never resolves and confirm withTimeout still returns within the configured duration, proving the wrapper genuinely bounds execution rather than being decorative.

## Verification ladder

- Unit: withTimeout against a never-resolving fixture handler, asserting bounded return time
- Integration: composed with a real (not mocked) slow subprocess in TICKET-047 (timeout and refusal scenario)
- End-to-end: exercised in TICKET-047
- Chaos: kill the wrapped process externally mid-execution and confirm graceful refusal, not a hang
- Stress: N/A with reason — single-capability timeout test, no concurrent-load profile defined yet
- Benchmark: timeout wrapper overhead measured against a trivial always-fast handler to confirm negligible added latency
- Verifier report: bounded-return-time test result

## Receipts

- timeout-wrapper.ts hash
- bounded-return-time test result

## Dependencies

- TICKET-017
- TICKET-026

## Falsifier

If a never-resolving handler wrapped by withTimeout does not return within its configured bound, this ticket is not complete — this is the primary, non-negotiable test.

## Handoff

TICKET-035 (subprocess sandbox executor) is wrapped by this timeout logic at integration time; TICKET-047 exercises the composed real behavior.

## Definition of done

- withTimeout generated and RDF-sourced where ARD §10 specifies a target
- never-resolving-handler test passes
- chaos kill test passes
