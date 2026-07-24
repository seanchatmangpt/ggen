# TICKET-D015: Real container/microVM sandbox isolation for candidate code execution

## Status

DEFERRED — production-readiness work never scoped by the v26.7.23 epic

## Re-verification (2026-07-24)

**Still true, unchanged.** Same underlying code as D001 (`lib/adapters/sandbox-executor.ts`),
re-checked directly: still a bare `spawn(..., { cwd, detached: true })`, no isolation mechanism.
Re-ran this ticket's own verification method: `grep -rniE
"gvisor|firecracker|microvm|docker-in-docker|kata container|nsjail|bubblewrap|seccomp|namespaces"`
across `examples/interview-assist/app/` and `lib/` — 0 hits. No `security-requirements.rq` query
file exists (`find . -iname "security-requirements.rq"` — 0 results, workspace-wide). The
`req/ard-sec-network-denied` / `req/ard-sec-no-host-fs` resources are still present only in the
ontology (`packs/wasm4pm-interview-assist-pack/ontology/20-requirements.ttl:115,117`, mirrored in
the compiled `ontology.ttl:440,442`) and still cited by zero implementing ticket outside this
backlog's own D001/D015 text — `grep -rn "ard-sec-network-denied\|ard-sec-no-host-fs"
--include="*.md"` across the repo matches only this backlog's own files. This session's real work
(cognition loop, second track, accessibility CSS effects, drawer, loading states, WASM-failure
handling, timeout/retry) did not touch sandbox isolation at all — it is orthogonal application/UX
work, not infrastructure work.

## Priority

P0 — any real exposure of code execution to untrusted input requires this; the epic's own admitted ontology already states this requirement and it is uncited by any ticket

## Parent

EPIC: InterviewAssist v26.7.24-deferred

## Source

- Research report: Out-of-Scope Production Work Report, item 1
- Citation: `grep -rniE "gvisor|firecracker|microvm|docker-in-docker|kata container|nsjail|bubblewrap|seccomp|namespaces"` across all 57 tickets + EPIC + README: 0 hits (Out-of-Scope Production Work Report, item 1).
- Citation: TICKET-035's only named future reduction path is "if the sandbox migrates to a WASM-based execution model" — never container/microVM.
- Citation: The reused source pattern `examples/interview-sandbox/lib/executor.ts` carries this header warning: "Do not expose this beyond localhost / a trusted single operator until real OS-level sandboxing (container, gVisor, microVM) replaces this." TICKET-035 copied the isolation code but dropped this warning sentence from its own disclosed-risk paragraph.
- Citation: The admitted ontology `ontology/20-requirements.ttl` (ARD §8) already contains `req/ard-sec-network-denied` and `req/ard-sec-no-host-fs` as domain requirements, cited by zero of the 57 tickets, with no `security-requirements.rq` query file among the 33 that exist (compare `performance-targets.rq`, which does exist and is used by TICKET-029).

## Objective

Implement real OS-level isolation (container, gVisor, microVM, or namespace-based sandboxing) around TICKET-035's subprocess executor. This ticket is the production-scope companion of D001, which tracks the same gap as disclosed by TICKET-035 itself.

## Current state

TICKET-035's sandbox runs submitted code as the same OS user as the host process, with isolation limited to a write-side workspace-escape path check. No container/microVM/namespace mechanism exists anywhere in the codebase or CI.

## Target state

Submitted candidate code executes inside a real, adversarially-tested isolation boundary; `req/ard-sec-network-denied` and `req/ard-sec-no-host-fs` are cited by an implementing ticket and enforced, not merely admitted as unused ontology text.

## Projection classification

- Template: N/A — no template exists; this is net-new production-hardening scope never authored as a ggen projection
- Domain data: N/A — see this ticket's Source citation for whether the admitted RDF graph already states a related requirement that was never wired to any ticket
- Custom code: N/A until scoped — production-hardening work of this kind is typically irreducible infrastructure/runtime code, not template-projectable, but the exact ratio depends on the implementation approach chosen

## Inputs

- the relevant portion of `examples/interview-assist/` as it exists today
- the admitted RDF graph's related requirement, if any (see Source)

## Outputs

- to be determined at implementation time — this ticket is a scoping/backlog entry, not a completed design

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

Not yet determined — depends on the implementation approach chosen when this ticket is picked up.

## Domain-data responsibility

Not yet determined; if the admitted RDF graph already states a related requirement (see Source), the implementation should query it rather than hardcode a parallel definition.

## Custom-code boundary

Not yet determined; likely irreducible infrastructure/runtime work per the pattern of TICKET-035's existing custom-code classification, but this is not asserted as fact until scoped.

## Exclusions

- no implementation without first confirming this gap is still real (re-verify against current `examples/interview-assist/` state, since a concurrent workflow may have addressed related work)
- no domain rule invented in custom code where the admitted RDF graph should instead be extended and queried

## Implementation steps

1. Author a `security-requirements.rq` query (parallel to the existing `performance-targets.rq` pattern) that surfaces ARD §8's security requirements so they become a live, queryable gate rather than dead ontology text.
2. Select and implement a concrete isolation mechanism (see D001 for the shared implementation work — this ticket is the production-readiness scoping and requirement-traceability half).
3. Restore the dropped warning sentence's intent in whatever documentation replaces TICKET-035's disclosed-risk paragraph, tracking it as CLOSED rather than removed once real isolation exists.
4. Cite `req/ard-sec-network-denied`/`req/ard-sec-no-host-fs` explicitly in the implementing ticket's `## Source` section once this work is picked up.

## Admission gates

- re-verification that the gap is still current (grep-based, per this ticket's own Source method)

## Acceptance criteria

Given the ARD §8 security requirements, when this ticket closes, then at least one ticket in the corpus (this one, or D001) cites `req/ard-sec-network-denied` and `req/ard-sec-no-host-fs`, and a real negative test proves network/filesystem isolation holds against a real attack attempt.

## Negative tests

See D001's negative test (real filesystem-escape and network-egress attempts against the isolation boundary) — this ticket is the requirement-traceability and production-hardening scope wrapper around that same implementation.

## Verification ladder

- Unit: N/A with reason — this capability does not exist yet; no unit to test
- Integration: N/A with reason — no implementation exists yet
- End-to-end: the acceptance criteria below define the first end-to-end check once implemented
- Chaos: N/A with reason — not applicable until the capability exists
- Stress: N/A with reason — not applicable until the capability exists
- Benchmark: N/A with reason — no perf target defined yet
- Verifier report: the real grep/read evidence cited in ## Source, re-verified at implementation time

## Receipts

- implementation evidence once scoped and built — none exist yet

## Dependencies

- none within this backlog — independent production-hardening work

## Falsifier

If this ticket is claimed complete without a real, run artifact (test transcript, live grep confirming the gap is closed) — narration alone is not sufficient.

## Handoff

Downstream of nothing in this backlog; upstream of any real production deployment of InterviewAssist.

## Definition of done

- gap re-verified as still current
- implementation scoped and built
- real test/verification artifact produced, not merely code review
