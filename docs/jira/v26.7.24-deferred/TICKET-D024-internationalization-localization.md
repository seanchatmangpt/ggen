# TICKET-D024: Internationalization / localization (currently entirely unstated, not merely deferred)

## Status

DEFERRED — production-readiness work never scoped by the v26.7.23 epic

## Priority

P2 — no PRD/ARD section addresses this at all — a genuinely unstated gap rather than a disclosed scope cut; nice-to-have completeness, not a demo blocker

## Parent

EPIC: InterviewAssist v26.7.24-deferred

## Source

- Research report: Out-of-Scope Production Work Report, item 10
- Citation: `grep -rniE "i18n|l10n|internationaliz|localiz|translat|english.only|multi.language"` across EPIC/README/57 tickets: 0 real hits (the only "translate" hits are TICKET-009's unrelated "SHACL-to-SPARQL translation") (Out-of-Scope Production Work Report, item 10).
- Citation: Every PRD (18) and ARD (14 + Decisive Product Choice) section title was listed directly from `ontology/00-document.ttl`'s `dcterms:title` triples — no section addresses internationalization or localization at all. Unlike accessibility (an explicit ARD §9 section, 16 `capability/accessibility/*` resources), i18n has no corresponding section anywhere in the admitted graph.

## Objective

Decide, as a deliberate product decision (not a silent omission), whether InterviewAssist should support internationalization/localization, and if so, add a PRD/ARD section to the admitted ontology before any template work begins — per this backlog's own domain-data-first architecture.

## Current state

No PRD or ARD section addresses i18n/l10n at all — this is unlike every other production-readiness gap in this workstream, which at least has an ARD §8/§10 section stating the requirement even if uncited. i18n has zero presence in the admitted graph.

## Target state

Either (a) a new ARD/PRD section explicitly states InterviewAssist is English-only by product decision (closing the gap honestly, not silently), or (b) a new section defines the i18n/l10n requirement and downstream tickets are scoped to implement it as a projection, per this epic's own architecture rule that domain meaning must live in RDF first.

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

1. Make an explicit product decision: English-only by design, or i18n/l10n in scope for a future phase.
2. If English-only: add a short ARD section stating this explicitly, closing the gap as a documented decision rather than an absence.
3. If i18n/l10n is in scope: add a new PRD/ARD section to the admitted ontology (per Architecture Decision 1, domain data must live in RDF first) before any template work targeting it begins.
4. Only after the ontology is updated should any template or component work reference locale-specific projection.

## Admission gates

- re-verification that the gap is still current (grep-based, per this ticket's own Source method)

## Acceptance criteria

Given the admitted ontology, when it is inspected for an i18n/l10n-related section, then one exists — either stating the English-only decision explicitly or defining the l10n requirement — closing the current zero-section gap either way.

## Negative tests

N/A — this is a product-decision/scoping ticket; the relevant check is whether the ontology contains an i18n-related section at all, not a runtime test.

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
