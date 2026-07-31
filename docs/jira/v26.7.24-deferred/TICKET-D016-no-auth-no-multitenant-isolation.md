# TICKET-D016: Multi-tenant session isolation, authentication, and per-tenant resource quotas

## Status

DEFERRED — production-readiness work never scoped by the v26.7.23 epic

## Re-verification (2026-07-24)

**Still true, unchanged.** Read `examples/interview-assist/app/api/run/route.ts` directly
(current version): the `POST` handler parses the JSON body, validates only
`capability`/`files` shape, and calls `getSandboxExecutor().execute(...)` — no auth header check,
no session/tenant identifier read or attached, no per-client concurrency gate anywhere in the
file. Re-ran this ticket's own verification method: `grep -rniE
"\bauth\b|authenticat|login|session.?cookie|jwt"` across `examples/interview-assist/app/` and
`lib/` — 0 hits. `grep -rniE "multi-tenant|multitenant|tenant isolation"` across the same tree —
0 hits. This session added 4 new API routes (`app/api/cognition`, `app/api/receipt`,
`app/api/run` pre-existing, `app/api/test`) — none of them introduce any auth/session concept
either; all are equally reachable by any caller with no identity attached.

## Priority

P0 — the subprocess-executing endpoint has zero auth/session concept and no per-client concurrency limit; this is a critical gap for any exposure beyond a single trusted local operator

## Parent

EPIC: InterviewAssist v26.7.24-deferred

## Source

- Research report: Out-of-Scope Production Work Report, item 2
- Citation: `grep -rniE "multi-tenant|multitenant|tenant isolation|concurrent user|resource quota|per-tenant"`: 0 hits (Out-of-Scope Production Work Report, item 2).
- Citation: `examples/interview-assist/app/api/run/route.ts` — the real subprocess-dispatching endpoint — has no session/tenant identifier, no auth check, no per-client concurrency limit, confirmed by reading the route source directly.
- Citation: `req/ard-sec-authenticated-participants` (ARD §8, "Authenticated participant identities") is never cited by any ticket, and `grep -rniE "\bauth\b|authenticat|login|session.?cookie|jwt"` across all of `examples/interview-assist/**/*.{ts,tsx}` returns 0 hits.

## Objective

Add authentication and per-tenant/per-session isolation to `app/api/run/route.ts` (and any other API surface that dispatches real actions), so multiple concurrent users cannot interfere with, exhaust, or observe each other's sandboxed sessions.

## Current state

`app/api/run/route.ts` has no auth check, no session/tenant identifier, and no per-client concurrency limit. Any caller who can reach the endpoint can dispatch subprocess execution with no identity attached.

## Target state

Every dispatch to `app/api/run/route.ts` is attributable to an authenticated session/tenant; concurrent sessions are isolated from each other's sandbox workspace and subject to a per-tenant concurrency/resource quota.

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

1. Cite `req/ard-sec-authenticated-participants` (ARD §8) explicitly in whatever ticket implements this, since the admitted graph already states this requirement.
2. Add an authentication layer (even a minimal session-token scheme appropriate to the deployment target) in front of `app/api/run/route.ts`.
3. Attach a session/tenant identifier to every sandbox invocation and scope its workspace directory to that identifier.
4. Add a per-tenant concurrency limit (e.g. one in-flight execution per session) to prevent a single caller from exhausting shared resources.

## Admission gates

- re-verification that the gap is still current (grep-based, per this ticket's own Source method)

## Acceptance criteria

Given two concurrent authenticated sessions, when both submit code for execution, then each session's sandbox workspace and results are isolated from the other, and an unauthenticated request to `app/api/run/route.ts` is rejected.

## Negative tests

Submit a request to `app/api/run/route.ts` with no auth token and confirm it is rejected before any subprocess is spawned; submit two concurrent authenticated sessions attempting to read each other's workspace and confirm isolation holds.

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
