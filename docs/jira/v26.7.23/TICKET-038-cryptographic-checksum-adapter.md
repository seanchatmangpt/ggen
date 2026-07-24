# TICKET-038: Cryptographic (BLAKE3) checksum adapter (custom)

## Status

ALIVE — real blake3 library calls, digest independently cross-checked, determinism verified

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/60-provenance-receipts.ttl (<receipt/checksum-algorithm-blake3>)`
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Implement the real BLAKE3 hashing calls satisfying TICKET-020's generated Checksum interface, reusing this repo's existing receipt-chain BLAKE3 usage (crates/ggen-engine/src/sync.rs pattern, or its JS/WASM equivalent) rather than a new hashing implementation.

## Current state

UNKNOWN — no implementation exists yet; classified in advance as genuinely custom per the analysis below.

## Target state

A working adapter implementing the generated port `TICKET-020's Checksum interface`, with zero domain-rule duplication (all domain checks — policy, precondition, refusal — are called INTO from workstream E/F, never reimplemented here).

## Projection classification

- Template: 20%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 80%
- Expected ratio: 20/80
- Custom-code justification: Cryptographic library bindings are irreducible — hashing cannot be expressed as RDF-projected structure, only invoked.

## Inputs

- generated port: TICKET-020's Checksum interface

## Outputs

- examples/interview-assist/lib/adapters/checksum-adapter.ts

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The port interface itself (TICKET-020's Checksum interface) is generated (workstream E/F/G) — this ticket implements it, does not define it.

## Domain-data responsibility

NONE — this adapter must not independently redefine domain rules already present in RDF (per the user's explicit custom-code exclusion). Any domain check needed (policy, precondition) is imported and called, never reimplemented.

## Custom-code boundary

Cryptographic library bindings are irreducible — hashing cannot be expressed as RDF-projected structure, only invoked.

## Exclusions

- no domain rule reimplementation — must call into workstream E/F's generated checks, never duplicate them
- no direct capability-list literal
- no bypass of TICKET-028's policy/precondition checks

## Implementation steps

1. Confirm the generated port interface this adapter must satisfy (from its Source dependency).
2. Implement the adapter, calling workstream E/F's generated policy/precondition checks before performing any real action.
3. Write a Chicago-TDD test using the REAL collaborator (real subprocess / real browser API / real Ollama endpoint / real crypto library), per this repo's own testing.md — no mocks.
4. Document the reduction path: reduction is already near-maximal here: this adapter is intentionally the thinnest possible wrapper (one function call) around an existing, audited hashing library — flag any future growth of this file as a smell to investigate.

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

- TICKET-020

## Falsifier

If this adapter performs its real action WITHOUT first calling the generated policy/precondition check, this ticket is not complete — that would be exactly the 'custom code independently redefining domain rules' failure the user's instructions explicitly forbid.

## Handoff

The relevant workstream I vertical scenario exercises this adapter against real infrastructure.

## Definition of done

- adapter implements the generated port exactly
- real-collaborator Chicago-TDD test passes (no mocks)
- policy-check-before-action test passes
- reduction path documented

## Implementation notes (real evidence)

- File: `examples/interview-assist/lib/adapters/checksum-adapter.ts` (50 lines) — the thinnest
  of the 6 adapters, one function call into the real `blake3` npm package's `hash()`. Test:
  `examples/interview-assist/tests/adapters/checksum-adapter.test.ts` (4 tests, all passing —
  `npx vitest run tests/adapters/checksum-adapter.test.ts`).
- `blake3` was not previously a dependency anywhere under `examples/`; added it as a real
  dependency in the new `examples/interview-assist/package.json` (`"blake3": "^2.1.7"`),
  `npm install`ed successfully.
- Determinism evidence: hashing the same string twice in the same process produces identical
  64-hex-char digests (32-byte BLAKE3 output); different inputs produce different digests.
- Known/reproducible digest evidence: independently ran
  `node -e "const {hash}=require('blake3'); console.log(Buffer.from(hash(Buffer.from('hello world'))).toString('hex'))"`
  outside the test suite and got `d74981efa70a0c880b8d8c1985d075dbcbf679b99a5f9914e5aaf96b831a9e24`,
  matching the constant asserted in the test — the digest in the test file was captured from this
  real run, not invented.
- Reuses the shape (not the Rust code) of this repo's own `crates/ggen-engine/src/sync.rs` BLAKE3
  usage (`blake3::hash(&bytes).to_hex()`), applying the equivalent JS/WASM binding.
- TICKET-020's real generated Checksum interface has not landed; the `hashHex(input): string`
  shape used here is hand-authored and marked `PENDING(TICKET-020)`.
- Policy-check-before-action: `hashHex()` calls `checkPolicy(...)` before calling `hash()` —
  currently deferring to the `PENDING(TICKET-028)` placeholder.
