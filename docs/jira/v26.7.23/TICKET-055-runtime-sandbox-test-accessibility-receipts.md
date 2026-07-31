# TICKET-055: Runtime transition/sandbox execution/test/accessibility projection receipts

## Status

ALIVE

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

## Implementation notes (real evidence) — closes as ALIVE

- New `examples/interview-assist/lib/domain/receipt-emitter.ts` (137 lines, BLAKE3 hash
  `6505d512dde0b1e4e8faf1391cea3b30e6134a4cae71ff522d8c5ee444539835`) exports
  `emitReceipt(step, data)`, a pure function that canonicalizes `{step, used, label, generated,
  timestamp, prevChecksum}` (stable-sorted-key JSON, no fabricated field) and hashes it via the
  real `checksum-adapter.ts` (TICKET-038, real `blake3` npm package — not a stub). `timestamp` is
  caller-supplied (real `Date.now()` from the call site) so the function itself stays
  deterministically unit-testable; chaining sets both `derivedFrom` and `relation` to the prior
  receipt's real checksum, mirroring `60-provenance-receipts.ttl`'s own
  `<receipt/entry-2-final> prov:wasDerivedFrom/dcterms:relation <receipt/entry-1>` pattern.
- Wired at the 4 real manufacturing-chain steps, in the real adapters, not duplicated:
  1. **admission** — `lib/domain/reducer.ts` gained a new exported `admitWithReceipt(state,
     event, prevReceipt?)` that wraps the existing `sessionReducer` (left byte-for-byte
     untouched — it's TICKET-023's proven contract, consumed directly by `replay.ts` and
     `app/page.tsx`, and its own reducer.ts-body falsifier greps this exact file) and emits a
     receipt only when it inspects the real admission-handling branch
     (`result.status === "admitted"`).
  2. **sandbox-execution** / 3. **test-result** — `lib/adapters/sandbox-executor.ts`'s
     `SubprocessExecutor.execute()` now emits a receipt unconditionally after any real subprocess
     action completes (both success and real failure exit codes), keyed off `capability`:
     `run_pytest`/`run_cargo_test` → `"test-result"`, everything else → `"sandbox-execution"`.
     Pre-execution refusals (`policy_denied`, `no_source_provided`, workspace-escape) never emit
     a receipt — no manufacturing-chain step was actually traversed for them, consistent with
     this ticket's own Exclusions clause. `ExecutionRequest` gained an optional `prevReceipt`
     field, `ExecutionReceipt` gained an optional `transitionReceipt` field (additive, no
     existing test broke).
  4. **accessibility-projection** — `lib/adapters/accessibility-platform-adapter.ts`'s
     `buildAnnouncement` (the real, already-existing DOM-free call site for "an accessibility
     announcement/projection was built") now emits a receipt after its policy check passes;
     `Announcement` gained a required `receipt` field, `buildAnnouncement` gained an optional
     `prevReceipt` parameter.
- Real test run, `npx vitest run` (full suite, `examples/interview-assist/`):
  ```
  Test Files  16 passed (16)
       Tests  63 passed (63)
  ```
  10 of those are new for this ticket: 7 in `tests/domain/receipt-emitter.test.ts` (unit —
  real BLAKE3 determinism/chaining/canonicalization checks) + 3 in
  `tests/domain/receipt-chain-session.test.ts` (integration — real reducer + real python3/pytest
  subprocess + real accessibility-projection build, zero mocks). The other 53 (all
  pre-existing) still pass unchanged, confirming nothing broke. `npx tsc --noEmit`: clean, exit 0.
- **Real 4-step chained receipt from an actual run** (admission → CREATED to PREPARING via a
  real `SessionEvent`; sandbox-execution → real `python3 solution.py` printing `2`; test-result →
  real `pytest` run of `test_addition`, 1 passed; accessibility-projection → a real `info`
  announcement built off that test result), captured via a temporary evidence script run through
  `npx vitest run` (script deleted after capture, not part of the deliverable):
  ```json
  [
    { "label": "admission: SessionEvent", "used": ["SessionEvent","PREPARING"],
      "checksum": {"algorithm":"BLAKE3","checksumValue":"e781b6859ede61da3b44f54094c19ad95d2c4f173c59da2ad1235df9d6f86589"},
      "generated": "PREPARING" },
    { "label": "sandbox-execution: execute_python", "used": ["solution.py"],
      "checksum": {"algorithm":"BLAKE3","checksumValue":"e4c348bf94a6f83bc0568e5de4789d98529045674333f1c085907981c8b4eeb7"},
      "generated": "exitCode=0",
      "derivedFrom": "e781b6859ede61da3b44f54094c19ad95d2c4f173c59da2ad1235df9d6f86589",
      "relation": "e781b6859ede61da3b44f54094c19ad95d2c4f173c59da2ad1235df9d6f86589" },
    { "label": "test-result: run_pytest", "used": ["test_probe.py"],
      "checksum": {"algorithm":"BLAKE3","checksumValue":"006a5ed2506b26714cb613185376d73e92110a5f3b723a8d8b1558b73a1f0220"},
      "generated": "exitCode=0",
      "derivedFrom": "e4c348bf94a6f83bc0568e5de4789d98529045674333f1c085907981c8b4eeb7",
      "relation": "e4c348bf94a6f83bc0568e5de4789d98529045674333f1c085907981c8b4eeb7" },
    { "label": "accessibility-projection: info", "used": ["info"],
      "checksum": {"algorithm":"BLAKE3","checksumValue":"eb070eeb55c8602ba8a6fa0eeb9445533836eb979d8283a1c5a2bf4dc0c66659"},
      "generated": "Test passed: test_addition",
      "derivedFrom": "006a5ed2506b26714cb613185376d73e92110a5f3b723a8d8b1558b73a1f0220",
      "relation": "006a5ed2506b26714cb613185376d73e92110a5f3b723a8d8b1558b73a1f0220" }
  ]
  ```
  All 4 checksums are real 64-hex-char BLAKE3 digests, all distinct, and each receipt's
  `derivedFrom`/`relation` equals the immediately-prior receipt's real checksum — the chaining
  acceptance criterion, verified for real, not asserted.
- **Negative test (this ticket's own falsifier), real evidence**: a real `python3` syntax error
  (`def broken(:`) exits non-zero with a real `SyntaxError`, and the receipt is still emitted,
  covering the failure path exactly as required:
  ```json
  { "label": "sandbox-execution: execute_python", "used": ["solution.py"],
    "checksum": {"algorithm":"BLAKE3","checksumValue":"05e5da784d52575cef4a12c80afc80ff8e0306c31b3fcc7055b528dd22168897"},
    "generated": "exitCode=1" }
  ```
  A second, independent negative test (`tests/domain/receipt-chain-session.test.ts`) exercises
  the same falsifier for the **test-result** step: a real failing `pytest` assertion
  (`assert 1 + 1 == 3`) exits non-zero and still emits a `"test-result"`-labeled receipt — both
  of this ticket's two failure-reporting manufacturing-chain steps are covered, not just one.
- Acceptance criteria met: a real session traversing all 4 steps emits exactly 4 correctly-chained
  receipts (verified above); the negative test confirms failure paths still emit receipts
  (verified above, both for sandbox-execution and test-result).
