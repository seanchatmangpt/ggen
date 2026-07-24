# TICKET-D018: CI/CD pipeline wiring for examples/interview-assist (currently zero workflow coverage)

## Status

DEFERRED — production-readiness work never scoped by the v26.7.23 epic

## Priority

P1 — needed before scaling past manual local runs; regressions are currently caught by nobody automatically

## Parent

EPIC: InterviewAssist v26.7.24-deferred

## Source

- Research report: Out-of-Scope Production Work Report, item 4
- Citation: `grep -rl "interview-assist" .github/workflows/*.yml`: 0 hits (Out-of-Scope Production Work Report, item 4). `.github/workflows/` has 25 workflow files; none reference `examples/interview-assist` or sweep `examples/*` generically.
- Citation: `ci.yml` itself is Rust/cargo-focused end to end (no `npm`/`node`/`vitest`/`playwright` steps at all — confirmed by grepping the file for those tokens). Every "Implementation notes" section across the 57 tickets describes manual local runs (`npx vitest run ...`, `npx tsc --noEmit`) — nothing is wired to run on push/PR.

## Objective

Add a GitHub Actions workflow (or extend an existing one) that runs `examples/interview-assist`'s test suite (`npx vitest run`, `npx tsc --noEmit`, and once D008 lands, `npx playwright test`) on every push/PR touching that directory.

## Current state

None of the 25 existing `.github/workflows/*.yml` files reference `examples/interview-assist`; `ci.yml` is entirely Rust/cargo-focused. Every test run documented in the 57 tickets was a manual local invocation.

## Target state

A CI workflow triggers on changes under `examples/interview-assist/` or `packs/wasm4pm-interview-assist-pack/`, runs the Node/TypeScript test suite and type-check, and fails the build on any regression — no longer relying on a human remembering to run tests locally.

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

1. Author a new workflow file (or a job within an existing one) scoped to `examples/interview-assist/` path changes.
2. Wire `npm ci`, `npx vitest run`, and `npx tsc --noEmit` as required steps.
3. Once D008/D011 land, add `npx playwright test` as a further required step.
4. Confirm the workflow actually triggers on a real PR touching `examples/interview-assist/` (not just a manual `workflow_dispatch`).

## Admission gates

- re-verification that the gap is still current (grep-based, per this ticket's own Source method)

## Acceptance criteria

Given a PR that introduces a real test failure in `examples/interview-assist/`, when the new CI workflow runs, then the PR check fails, blocking merge without the failure being caught only by a human running tests locally.

## Negative tests

Introduce a deliberately failing test in a scratch branch and confirm the new CI workflow reports a real failure, not a silent pass.

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
