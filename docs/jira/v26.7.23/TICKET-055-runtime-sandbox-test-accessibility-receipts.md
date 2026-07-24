# TICKET-055: Runtime transition/sandbox execution/test/accessibility projection receipts

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/60-provenance-receipts.ttl (<manufacturing-chain/*> activities/entities)`
- ARD components: (none)
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Wire TICKET-020's TransitionReceipt type to real emission at each of the manufacturing-chain steps already modeled in RDF (admission, sandbox execution, test result, accessibility projection), so every real session run produces real per-step receipts, not just a final one.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 75%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 25%
- Expected ratio: 75/25
- Custom-code justification: receipt emission is triggered from within the real adapters (workstream H) at the moment of real action — the emission call-sites require the same design-judgment classification as the reducer/adapters they're embedded in.

## Inputs

- TICKET-020 receipt.ts
- TICKET-038 checksum-adapter.ts
- TICKET-023 reducer.ts
- TICKET-035 sandbox-executor.ts

## Outputs

- examples/interview-assist/lib/domain/receipt-emitter.ts (emits a TransitionReceipt at each manufacturing-chain step)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The emission-point wiring pattern (call emitReceipt(step, data) at each of the 4 manufacturing-chain steps) is generic structure.

## Domain-data responsibility

Which 4 steps require receipts (admission, sandbox execution, test result, accessibility projection) is fixed by 60-provenance-receipts.ttl's manufacturing-chain/* resources.

## Custom-code boundary

Emission call-sites live inside workstream H's real adapters (sandbox-executor.ts, etc.) — this ticket wires the calls, doesn't duplicate adapter logic.

## Exclusions

- no receipt data fabricated — every emitted receipt must reflect a REAL action that actually occurred

## Implementation steps

1. Add emitReceipt calls at the 4 manufacturing-chain step locations in the real reducer/adapters.
2. Verify each real session run produces exactly 4+ receipts (one per real manufacturing-chain step traversed).
3. Verify receipt chaining (each receipt's prov:wasDerivedFrom-equivalent field points to the prior receipt, matching 60-provenance-receipts.ttl's receipt/entry-1 -> receipt/entry-2-final chain pattern).

## Admission gates

- TICKET-020.
- TICKET-038.
- TICKET-035.

## Acceptance criteria

- Given a real session traversing all 4 manufacturing-chain steps, when it completes, then exactly 4 (or more, if steps repeat) receipts are emitted, correctly chained.

## Negative tests

- Simulate a step that fails (e.g. sandbox execution errors) and confirm a receipt is STILL emitted recording the failure, not silently dropped — receipts must cover failure paths too, not just success.

## Verification ladder

- Unit: emission call-site unit tests
- Integration: real session run producing a real receipt chain
- End-to-end: exercised in TICKET-053
- Chaos: N/A with reason — deterministic emission points
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: receipt chain from one real full session run

## Receipts

- receipt-emitter.ts hash
- sample real receipt chain from a test run

## Dependencies

- TICKET-020
- TICKET-038
- TICKET-035

## Falsifier

If a real session traverses a manufacturing-chain step but no corresponding receipt is emitted, this ticket is not complete.

## Handoff

TICKET-056 (final session receipt) is the last link in this chain.

## Definition of done

- emission wired at all 4 manufacturing-chain steps
- chaining verified
- failure-path receipt emission verified
