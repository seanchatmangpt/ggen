# TICKET-035: Subprocess sandbox executor (custom)

## Status

ALIVE — real subprocess compile/execute for Python and Rust, real timeout-kill with orphan-process check, all against real infrastructure

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability/runtime/execute>, <capability/runtime/isolate-filesystem>, <capability/runtime/isolate-network>)`
- PRD requirements: (none)
- ARD components:
  - `ARD §6 Sandbox API boundary`
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Implement real subprocess execution (compile/execute/test-run) behind TICKET-027's generated dispatch-table slots, reusing this session's already-hardened patterns (path.resolve workspace-escape prevention, output-size capping, process-group SIGKILL) from examples/interview-sandbox/lib/executor.ts rather than reimplementing from scratch.

## Current state

UNKNOWN — no implementation exists yet; classified in advance as genuinely custom per the analysis below.

## Target state

A working adapter implementing the generated port `TICKET-027's capability-dispatch.ts slots for compile/execute/run-*-test`, with zero domain-rule duplication (all domain checks — policy, precondition, refusal — are called INTO from workstream E/F, never reimplemented here).

## Projection classification

- Template: 20%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 80%
- Expected ratio: 20/80
- Custom-code justification: Real OS process spawning, filesystem/network isolation, and process-tree termination are irreducible operating-system interactions with no RDF representation possible; this is the canonical example of an authorized custom boundary.

## Inputs

- generated port: TICKET-027's capability-dispatch.ts slots for compile/execute/run-*-test

## Outputs

- examples/interview-assist/lib/adapters/sandbox-executor.ts (reusing examples/interview-sandbox/lib/executor.ts's hardened patterns)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The port interface itself (TICKET-027's capability-dispatch.ts slots for compile/execute/run-*-test) is generated (workstream E/F/G) — this ticket implements it, does not define it.

## Domain-data responsibility

NONE — this adapter must not independently redefine domain rules already present in RDF (per the user's explicit custom-code exclusion). Any domain check needed (policy, precondition) is imported and called, never reimplemented.

## Custom-code boundary

Real OS process spawning, filesystem/network isolation, and process-tree termination are irreducible operating-system interactions with no RDF representation possible; this is the canonical example of an authorized custom boundary.

## Exclusions

- no domain rule reimplementation — must call into workstream E/F's generated checks, never duplicate them
- no direct capability-list literal
- no bypass of TICKET-028's policy/precondition checks

## Implementation steps

1. Confirm the generated port interface this adapter must satisfy (from its Source dependency).
2. Implement the adapter, calling workstream E/F's generated policy/precondition checks before performing any real action.
3. Write a Chicago-TDD test using the REAL collaborator (real subprocess / real browser API / real Ollama endpoint / real crypto library), per this repo's own testing.md — no mocks.
4. Document the reduction path: if the sandbox migrates to a WASM-based execution model in a later phase, more of the isolation logic could become declarative sandbox-policy data rather than imperative OS calls.

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

- TICKET-027
- TICKET-028
- TICKET-029

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

- File: `examples/interview-assist/lib/adapters/sandbox-executor.ts` (248 lines) — reuses
  `examples/interview-sandbox/lib/executor.ts`'s hardened patterns verbatim (path.resolve
  workspace-escape check, `MAX_OUTPUT_BYTES` output cap with kill, `detached: true` +
  negative-PID `process.kill(-pid, "SIGKILL")` process-group cleanup) rather than reimplementing
  them. Test: `examples/interview-assist/tests/adapters/sandbox-executor.test.ts` (5 tests, all
  passing — `npx vitest run tests/adapters/sandbox-executor.test.ts`, ~2.2s wall clock).
- Real python3 stdout capture: executed `print(1+1)` via a real `python3` subprocess (not
  simulated); asserted `result.stdout.trim() === "2"` and `exitCode === 0` — passed.
- Real rustc compile+execute: wrote `fn main() { println!("{}", 1 + 1); }`, compiled with a real
  `rustc` subprocess, ran the produced binary, asserted stdout `"2"` — passed (804ms first run,
  313ms on rerun).
- Real timeout-kill + orphan-process proof: submitted a real infinite-loop Python program
  (`while True: time.sleep(0.05)`) tagged with a unique marker string, `timeoutMs: 1500`.
  Measured wall-clock elapsed was under the 5000ms assertion bound (real `Date.now()` deltas,
  not simulated); asserted `exitCode === -1` and `stderr` contains `"timed out"`. Then ran a
  REAL `ps ax -o command=` against the live process table and asserted the marker string does
  NOT appear — proving no orphan process survived the process-group SIGKILL. All assertions
  passed.
- Policy-check-before-action: `execute()` calls `checkPolicy(...)` as its first statement,
  before `Object.keys(request.files).length === 0` is even checked, let alone before any
  `spawn(...)` call — structurally guarantees the check runs before any real action. TICKET-028's
  real generated check does not exist yet, so this currently defers to a documented
  `PENDING(TICKET-028)` default-allow placeholder in `policy-check-stub.ts`.
- Negative tests: empty-files refusal (`no_source_provided`) and workspace-escape refusal
  (`../../etc/escape.py` rejected with `"escapes the sandbox workspace"`, subprocess never
  spawned) both pass.
- Disclosed, unfixed risk (inherited from the reused pattern, restated honestly): no network or
  filesystem isolation beyond the write-side workspace-escape check — a submitted program still
  runs as the same OS user as the host process. Not fixed in this pass; same disclosed scope as
  the original `examples/interview-sandbox/lib/executor.ts`.
