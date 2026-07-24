# TICKET-D023: Accessibility certification by real assistive-technology users (screen readers, Braille devices)

## Status

DEFERRED — production-readiness work never scoped by the v26.7.23 epic

## Priority

P1 — real user validation is needed before claiming accessibility compliance; the automated Playwright/axe-core layer this depends on is itself still BLOCKED (D008)

## Parent

EPIC: InterviewAssist v26.7.24-deferred

## Source

- Research report: Out-of-Scope Production Work Report, item 9
- Citation: TICKET-039 implements ARIA-live-region and Web Speech API code, but its own "Implementation notes" state the Playwright harness is BLOCKED and that the DOM-dependent pieces are "present in source but NOT exercised this pass" (Out-of-Scope Production Work Report, item 9).
- Citation: `grep -rniE "screen reader user|braille|assistive technology|nvda|jaws|voiceover|usability test"` across all 57 tickets returns only TICKET-033/039's own text disclaiming that real platform integration is "explicitly out of scope here (TICKET-039)" — the deepest layer is disclaimed forward to a ticket that itself never reaches it.

## Objective

Once D008 (Playwright harness) and TICKET-050 (axe-core automated audit) land, arrange a real assistive-technology-user certification pass (screen reader, Braille device) against the live application — the layer no ticket in 001-057 currently reaches.

## Current state

No ticket in the 57-ticket backlog reaches real assistive-technology-user testing; the deepest layer (TICKET-033/039) explicitly disclaims it forward to TICKET-039, which itself only implements the DOM-free logic and defers browser/platform exercise to a still-BLOCKED harness.

## Target state

A real screen-reader or Braille-device user (or a qualified accessibility auditor using such tools) has exercised the live application and confirmed the ARIA-live-region announcements and Web Speech API integration function as intended for their actual assistive technology, with findings documented.

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

1. Wait for D008 (Playwright harness) and TICKET-050 (axe-core automated audit) to land as prerequisites — automated checks should pass before spending real user-testing time.
2. Recruit or arrange access to a real assistive-technology user or qualified accessibility auditor.
3. Conduct a structured session exercising the primary interview-assist workflows (bootstrap, coding, test running, session completion) with real assistive technology.
4. Document findings, including any gap between the automated axe-core pass and real user experience.

## Admission gates

- re-verification that the gap is still current (grep-based, per this ticket's own Source method)

## Acceptance criteria

Given a real assistive-technology user session, when the primary interview-assist workflows are exercised, then the user can complete them using their actual assistive technology, and any friction points are documented as follow-up tickets.

## Negative tests

N/A in the traditional automated sense — the negative-test equivalent here is: if the automated axe-core pass (TICKET-050) is green but the real user session surfaces a usability blocker, that is itself the finding this ticket exists to catch, and must be documented rather than dismissed as 'axe-core already passed.'

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
