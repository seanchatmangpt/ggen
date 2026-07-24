# TICKET-D025: Load/stress testing at realistic concurrent-user scale

## Status

DEFERRED — production-readiness work never scoped by the v26.7.23 epic

## Priority

P1 — real hardening needed before scaling past a single-operator demo; the backlog's own N/A pattern across every Stress rung confirms this was never closed, only deferred

## Parent

EPIC: InterviewAssist v26.7.24-deferred

## Source

- Research report: Out-of-Scope Production Work Report, item 11
- Citation: Every ticket carrying a "Stress" verification-ladder rung (TICKET-029, 040-053) reads "Stress: N/A with reason" — confirmed pattern-matched across the full backlog, no exceptions found (Out-of-Scope Production Work Report, item 11).
- Citation: TICKET-029 is the one ticket that actually queried ARD §10 ("Performance requirements") via a real `performance-targets.rq` SPARQL query; its own implementation notes document all 7 resulting requirements are "latency budgets for pipeline stages... not a candidate-code wall-clock cap" — none is a concurrent-user throughput target, and the ticket deliberately declines to repurpose them as one.
- Citation: No ticket among 001-057 exists to define a concurrent-user performance target or to build a load/stress test against one; the gap is visible in the backlog's own language but never assigned a follow-up ticket — until this one.

## Objective

Define a concurrent-user load/throughput target (distinct from TICKET-029's per-stage latency budgets) and build a real load test against it, once D016's multi-tenant isolation and D021's rate limiting exist to test against.

## Current state

Zero of the 57 tickets define a concurrent-user throughput target or build a load test. TICKET-029's 7 performance requirements are explicitly latency budgets for individual pipeline stages, not a concurrency target, and its own text declines to repurpose them as one.

## Target state

A documented concurrent-user target (e.g. N simultaneous sandbox executions, M requests/minute to `app/api/run/route.ts`) exists, cited from a real ARD/PRD section or a newly-added one, and a real load test exercises the system against it, reporting actual observed throughput/latency degradation.

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

1. Confirm whether the admitted ontology should gain a new concurrent-user throughput requirement (per Architecture Decision 1) distinct from TICKET-029's per-stage latency budgets, or whether this is deliberately left as an operational SLO outside the RDF graph.
2. Once D016 (multi-tenant isolation) and D021 (rate limiting) exist, build a real load-testing harness driving N concurrent sessions against `app/api/run/route.ts`.
3. Run the load test and record real observed throughput, latency degradation under load, and any failure mode (e.g. resource exhaustion, queueing behavior).
4. Compare observed behavior against the defined target and document pass/fail.

## Admission gates

- re-verification that the gap is still current (grep-based, per this ticket's own Source method)

## Acceptance criteria

Given a defined concurrent-user target, when the real load test runs N simultaneous sessions, then observed throughput/latency is recorded and compared against the target, with the result (pass or documented shortfall) reported — not merely 'N/A with reason' as every current Stress rung reads.

## Negative tests

Run the load test at 2x the defined target's concurrency and confirm the system either degrades gracefully (rate-limited refusals, per D021) rather than crashing, or the failure mode is documented as a known limit.

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
