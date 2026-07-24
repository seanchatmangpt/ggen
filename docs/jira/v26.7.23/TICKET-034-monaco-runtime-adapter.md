# TICKET-034: Monaco runtime adapter (custom)

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources: (none)
- ARD components:
  - `ARD §5 Initial technology choices`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Implement the real Monaco Editor integration behind TICKET-031's generated editor-shell.tsx props interface — Monaco's own runtime (web worker bundling, language-service wiring) cannot be projected from RDF.

## Current state

UNKNOWN — no implementation exists yet; classified in advance as genuinely custom per the analysis below.

## Target state

A working adapter implementing the generated port `TICKET-031's editor-shell.tsx props interface`, with zero domain-rule duplication (all domain checks — policy, precondition, refusal — are called INTO from workstream E/F, never reimplemented here).

## Projection classification

- Template: 20%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 80%
- Expected ratio: 20/80
- Custom-code justification: Monaco Editor is a third-party browser-runtime library with its own web-worker bundling and language-service lifecycle; representing its internal wiring as generated template output would mean re-deriving Monaco's own architecture from RDF, which is neither safe nor the pack's job.

## Inputs

- generated port: TICKET-031's editor-shell.tsx props interface

## Outputs

- examples/interview-assist/lib/adapters/monaco-adapter.ts

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The port interface itself (TICKET-031's editor-shell.tsx props interface) is generated (workstream E/F/G) — this ticket implements it, does not define it.

## Domain-data responsibility

NONE — this adapter must not independently redefine domain rules already present in RDF (per the user's explicit custom-code exclusion). Any domain check needed (policy, precondition) is imported and called, never reimplemented.

## Custom-code boundary

Monaco Editor is a third-party browser-runtime library with its own web-worker bundling and language-service lifecycle; representing its internal wiring as generated template output would mean re-deriving Monaco's own architecture from RDF, which is neither safe nor the pack's job.

## Exclusions

- no domain rule reimplementation — must call into workstream E/F's generated checks, never duplicate them
- no direct capability-list literal
- no bypass of TICKET-028's policy/precondition checks

## Implementation steps

1. Confirm the generated port interface this adapter must satisfy (from its Source dependency).
2. Implement the adapter, calling workstream E/F's generated policy/precondition checks before performing any real action.
3. Write a Chicago-TDD test using the REAL collaborator (real subprocess / real browser API / real Ollama endpoint / real crypto library), per this repo's own testing.md — no mocks.
4. Document the reduction path: if a future Monaco version exposes a fully declarative/headless API, more of this adapter's wiring (currently worker registration, language config) could migrate to generated configuration rather than imperative setup code.

## Admission gates

- The generated port ticket this adapter implements (see Dependencies).

## Acceptance criteria

- Given a real invocation, when the adapter executes, then it produces output conforming to the generated port's typed return shape, with no domain-rule bypass.

## Negative tests

- Attempt an action the policy/precondition checks would deny and confirm the adapter refuses BEFORE performing the real action (proving it calls into the generated check rather than acting first and checking later).

## Verification ladder

- Unit: adapter unit test against the real collaborator in an isolated/ephemeral instance
- Integration: adapter composed with the real dispatch table (TICKET-027) and real policy check (TICKET-028)
- End-to-end: exercised in the relevant workstream I vertical scenario
- Chaos: kill/interrupt the real collaborator mid-operation and confirm graceful, receipted refusal, not a hang or crash
- Stress: N/A with reason — noted per-adapter below if a stress profile is genuinely relevant
- Benchmark: N/A with reason — no fixed perf target at this phase
- Verifier report: real-collaborator test transcript

## Receipts

- adapter source hash
- real-collaborator test transcript
- policy-check-called-before-action confirmation

## Dependencies

- TICKET-031

## Falsifier

If this adapter performs its real action WITHOUT first calling the generated policy/precondition check, this ticket is not complete — that would be exactly the 'custom code independently redefining domain rules' failure the user's instructions explicitly forbid.

## Handoff

The relevant workstream I vertical scenario exercises this adapter against real infrastructure.

## Definition of done

- adapter implements the generated port exactly
- real-collaborator Chicago-TDD test passes (no mocks)
- policy-check-before-action test passes
- reduction path documented
