# TICKET-D001: Subprocess sandbox has no OS-level isolation beyond workspace-escape check

## Status

DEFERRED — permanent structural limitation, disclosed by the originating ticket

## Re-verification (2026-07-24)

**Still true, unchanged.** Read `examples/interview-assist/lib/adapters/sandbox-executor.ts`
directly (current version, post this session's cognition-loop/UX work). Line 101:
`spawn(cmd, args, { cwd, detached: true })` — a bare Node `child_process.spawn`, same OS user as
the host process, no container/gVisor/microVM/namespace/seccomp wrapper of any kind. The only
boundary is still the write-side `resolveWithinWorkspace` path-traversal check (lines 160-166),
which gates only which paths this session's own `writeFiles()` call may write to before spawning
— it does nothing to constrain what the spawned subprocess itself can `open()`, connect to, or
fork once running. Confirmed via `grep -rniE
"gvisor|firecracker|microvm|docker-in-docker|kata container|nsjail|bubblewrap|seccomp|namespaces"`
across `examples/interview-assist/app/` and `lib/`: 0 hits. This session's real, verified work
(second cognition track, real `run_pytest` UI wiring, interactive drawer, accessibility CSS
effects, loading states, WASM-failure handling, timeout/retry) touched none of this file's
isolation surface — only the unrelated `checkPolicy` import at line 25 changed (now the real
TICKET-028 policy check instead of the stub; see D005's own re-verification — a permission-layer
change, not an OS-isolation change, and orthogonal to this ticket's gap).

## Priority

P0 — any real exposure of code execution to untrusted input requires this closed first; see also D015/D022/D029 for the isolation/pentest/negative-test picture this same gap implies

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-035-subprocess-sandbox-executor.md`

## Source

- Research report: Disclosed Gaps Catalog, section (a) Permanent structural limitations
- Citation: "Disclosed, unfixed risk (inherited from the reused pattern, restated honestly): no network or filesystem isolation beyond the write-side workspace-escape check — a submitted program still runs as the same OS user as the host process. Not fixed in this pass; same disclosed scope as the original `examples/interview-sandbox/lib/executor.ts`." — `docs/jira/v26.7.23/TICKET-035-subprocess-sandbox-executor.md:154-157`

## Objective

Close the disclosed gap between TICKET-035's actual isolation boundary (a workspace-escape path check only) and real OS-level isolation (container, gVisor, microVM, or namespace-based sandboxing) for submitted candidate code.

## Current state

TICKET-035 is ALIVE for its own scope (real subprocess compile/execute, timeout-kill, workspace-escape refusal), but by its own disclosure a submitted program runs as the same OS user as the host process with no network or filesystem isolation beyond that one check.

## Target state

Submitted candidate code executes inside a real isolation boundary (container, gVisor, microVM, or equivalent) with no shared filesystem, network egress denied by default, and no ability to affect the host process or other sandboxed executions.

## Projection classification

- Template: N/A — this ticket catalogs a disclosed gap, not a projection ratio
- Domain data: the RDF corpus already states this requirement — see D015 (Out-of-Scope report) for the exact uncited `req/ard-sec-network-denied` / `req/ard-sec-no-host-fs` resources
- Custom code: closing this gap is irreducible OS/infrastructure work, same category TICKET-035 itself classified 80% custom

## Inputs

- TICKET-035's `examples/interview-assist/lib/adapters/sandbox-executor.ts`
- the ARD §8 security requirements TICKET-035 did not cite (see D015/D022)

## Outputs

- a real isolation boundary (container/gVisor/microVM) wrapping `sandbox-executor.ts`'s spawn call
- an updated disclosed-risk paragraph in TICKET-035 reflecting the closed gap

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

None — this is infrastructure/runtime work, not a ggen-projected artifact.

## Domain-data responsibility

None directly, though closing this gap is how `req/ard-sec-network-denied`/`req/ard-sec-no-host-fs` (ARD §8) would actually become enforced rather than merely admitted as ontology text.

## Custom-code boundary

Real OS/container/microVM integration is irreducible infrastructure work with no RDF representation possible — same boundary class TICKET-035 itself already names.

## Exclusions

- no partial mitigation presented as full isolation (e.g. a resource-limit ulimit alone does not satisfy this ticket)
- no removal of the existing workspace-escape check — this is additive hardening, not a replacement

## Implementation steps

1. Select a concrete isolation mechanism (container, gVisor, microVM, or namespace-based sandbox) appropriate to the deployment target.
2. Wrap the existing `spawn(...)` call in `sandbox-executor.ts` with the chosen isolation boundary, preserving the existing workspace-escape and output-cap logic.
3. Deny network egress by default from inside the isolation boundary; require an explicit allowlist if any network access is later needed.
4. Re-run TICKET-035's existing Chicago-TDD test suite (real python3/rustc execution, timeout-kill, workspace-escape) against the isolated executor to confirm no regression.
5. Add new real-collaborator tests proving the isolation itself: a program attempting host filesystem access outside the isolation boundary is denied; a program attempting network egress is denied.

## Admission gates

- TICKET-035 (must remain ALIVE after this change)

## Acceptance criteria

Given a submitted program attempting to read a host file outside its workspace or open a network socket, when executed inside the new isolation boundary, then the attempt fails at the OS/isolation layer (not merely the pre-execution workspace-escape string check), and this is provable via a real negative test, not a code-review claim.

## Negative tests

Submit a real program that attempts `open('/etc/passwd')` and a real program that attempts an outbound HTTP request; confirm both fail at the isolation boundary, with the failure distinguishable from a normal program crash.

## Verification ladder

- Unit: isolation-boundary unit test against a real container/microVM/gVisor instance (not simulated)
- Integration: composed with TICKET-035's existing dispatch-table wiring
- End-to-end: exercised via the relevant workstream I vertical scenario once workstream H's isolation lands
- Chaos: kill the isolated process mid-execution and confirm no host-process or other-sandbox impact
- Stress: N/A with reason — no concurrent-execution load target defined yet (see D025)
- Benchmark: N/A with reason — no isolation-overhead perf target defined yet
- Verifier report: real-collaborator test transcript showing both the filesystem-escape and network-egress negative tests failing as expected

## Receipts

- isolation-boundary configuration hash
- real-collaborator negative-test transcript

## Dependencies

- TICKET-035 (predecessor)

## Falsifier

If a submitted program can read any host file outside its workspace or reach any network endpoint from inside the sandbox after this ticket claims done, this ticket is not complete.

## Handoff

D022 (security review/pentest) exercises this isolation boundary adversarially once it exists; D029 (missing sandbox negative tests) supplies the specific attack classes (fork bomb, disk-fill, memory exhaustion) to test against it.

## Definition of done

- real isolation boundary in place
- existing TICKET-035 test suite still passes
- new filesystem-escape and network-egress negative tests pass
- disclosed-risk paragraph in TICKET-035 updated to reflect the closed gap
