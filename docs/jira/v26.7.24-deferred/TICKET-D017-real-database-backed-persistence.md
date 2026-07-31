# TICKET-D017: Real server-side database-backed persistence (beyond the documented Node-filesystem/browser-storage substitute)

## Status

DEFERRED — production-readiness work never scoped by the v26.7.23 epic

## Priority

P2 — explicitly deferred by design in TICKET-036, whose adapter interface is already stated to stay stable across this swap; not urgent for single-operator/demo scale

## Parent

EPIC: InterviewAssist v26.7.24-deferred

## Source

- Research report: Out-of-Scope Production Work Report, item 3
- Citation: TICKET-036 implements `examples/interview-assist/lib/adapters/persistence-adapter.ts` against the real Node filesystem, explicitly labeled in-source as an "HONEST SUBSTITUTION" for `window.indexedDB` since it runs in a Node/Vitest context, not a browser (Out-of-Scope Production Work Report, item 3).
- Citation: TICKET-036's own step 4 states the deferral outright: "if session persistence moves server-side to a real database in a later phase, this adapter's interface stays the same... and only the implementation swaps." No server-side database, connection pooling, or multi-session store is scoped anywhere in 001-057.

## Objective

Implement a real server-side database-backed persistence layer (e.g. PostgreSQL, SQLite-with-connection-pooling) behind the same adapter interface TICKET-036 already established, for deployments that need durability beyond a single browser's local storage.

## Current state

Session persistence is either the browser's real `window.indexedDB`/`localStorage` (once D010 verifies it) or, in test contexts, a real filesystem substitute. No server-side multi-session database store exists.

## Target state

A server-side database adapter implements the exact same interface TICKET-036 defined, swappable without changing any calling code, per TICKET-036's own stated design intent.

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

1. Confirm TICKET-036's adapter interface (port) is stable and does not need to change to support a server-side implementation.
2. Select a database technology appropriate to the deployment target chosen in D019.
3. Implement the adapter against a real database instance (per this repo's Chicago TDD policy — no mocked DB), with real connection pooling.
4. Verify the swap requires no changes to any calling code, confirming TICKET-036's design promise held.

## Admission gates

- re-verification that the gap is still current (grep-based, per this ticket's own Source method)

## Acceptance criteria

Given a real database instance, when the server-side adapter is used in place of the browser/filesystem adapter, then session read/write/list operations behave identically from the caller's perspective, with real data durability across process restarts.

## Negative tests

Kill and restart the server process mid-session and confirm session data persisted to the real database survives, unlike the browser-local or Node-filesystem-temp-directory substitutes.

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
