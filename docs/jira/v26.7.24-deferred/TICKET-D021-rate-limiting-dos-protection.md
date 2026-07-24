# TICKET-D021: Rate limiting and DoS protection on the subprocess-dispatching API route

## Status

DEFERRED — production-readiness work never scoped by the v26.7.23 epic

## Re-verification (2026-07-24)

**Still true, unchanged.** `examples/interview-assist/app/api/run/route.ts`, read directly: 42
lines total, no rate limiter, no concurrency counter, no request-size cap beyond the executor's
own `MAX_OUTPUT_BYTES` (which caps *output*, not input request rate or size). Re-ran this
ticket's own verification method: `grep -rniE
"rate.?limit|throttl|concurrency.?(limit|cap)|per-tenant|per-session.?limit"` across
`examples/interview-assist/app/` and `lib/` — 0 hits. Combined with D016 (still no auth), the
endpoint remains reachable by any caller with no per-caller throttle of any kind. None of this
session's 3 new API routes (`app/api/cognition`, `app/api/receipt`, `app/api/test`) add rate
limiting either — same gap, wider surface than when this ticket was written.

## Priority

P0 — app/api/run/route.ts has no throttling of any kind on an endpoint that spawns real subprocesses; combined with D016's missing auth, this is a trivial resource-exhaustion vector

## Parent

EPIC: InterviewAssist v26.7.24-deferred

## Source

- Research report: Out-of-Scope Production Work Report, item 7
- Citation: `examples/interview-assist/app/api/run/route.ts` has no throttling, no per-IP/per-session limiter, no request-size cap beyond the executor's own output-byte cap; confirmed by reading the route source directly (Out-of-Scope Production Work Report, item 7). `grep` for concurrency/queue/rate-limit terms across the whole `examples/interview-assist` tree returns 0 real hits.
- Citation: The predecessor `examples/interview-sandbox/lib/executor.ts`'s own header comment names "NO PER-CLIENT AUTH OR PERSISTENT RATE LIMITING beyond the in-memory limiter in the API route" as a disclosed risk, implying some minimal limiter existed in that predecessor's API layer — UNVERIFIED, since `examples/interview-sandbox` retains no `app/api/` directory on disk to confirm what that limiter actually did.
- Citation: What is confirmed: TICKET-035, when it restates this same disclosed-risk paragraph for `interview-assist`, mentions only network/filesystem isolation and drops the rate-limiting sentence entirely.

## Objective

Add a real rate limiter (per-session or per-IP) to `app/api/run/route.ts`, restoring at minimum the level of protection the predecessor `interview-sandbox`'s disclosed-risk comment implied once existed, and going further given this endpoint spawns real subprocesses.

## Current state

`app/api/run/route.ts` has zero throttling of any kind — no per-client request-rate limit, no concurrency cap. The predecessor project's disclosed-risk language implying an in-memory limiter was dropped when TICKET-035 restated that same paragraph for `interview-assist`.

## Target state

`app/api/run/route.ts` enforces a real per-session/per-IP rate limit and concurrency cap; a client exceeding it receives an explicit rate-limited refusal (consistent with Architecture Decision 13, refusal as a first-class outcome) rather than an unbounded queue of real subprocess spawns.

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

1. Determine the appropriate rate-limit shape (requests per minute, concurrent-execution cap, or both) for the chosen deployment target.
2. Implement the limiter at the API route layer, returning a typed refusal (not a bare HTTP 429 with no body) when exceeded, consistent with this codebase's refusal-as-first-class-outcome pattern.
3. Wire the limiter to the per-tenant/per-session identifier introduced in D016, since rate limiting without identity is easily bypassed.
4. Document the final limiter design in place of the dropped disclosed-risk sentence.

## Admission gates

- re-verification that the gap is still current (grep-based, per this ticket's own Source method)

## Acceptance criteria

Given a client exceeding the configured rate limit, when it submits another request to `app/api/run/route.ts`, then the request is refused with an explicit, typed rate-limited response before any subprocess is spawned.

## Negative tests

Submit requests in a tight loop exceeding the configured limit and confirm the Nth-and-beyond requests are refused without a subprocess being spawned for them (verifiable via a real `ps ax` check showing no corresponding process, similar to TICKET-035's existing orphan-check pattern).

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
