# TICKET-D008: Playwright + accessibility harness is BLOCKED — no app-shell dev server exists to drive it

## Status

DEFERRED — environment-dependent, may resolve once workstream C/H's app shell lands

## Priority

P1 — the primary browser-based verification harness for the whole app doesn't run yet; blocks confident UI/accessibility verification before any real user exposure

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-039-playwright-and-accessibility-platform-harness.md`

## Source

- Research report: Disclosed Gaps Catalog, section (b) Environment-dependent gaps
- Citation: "PARTIAL_ALIVE — accessibility-adapter DOM-free logic implemented and tested; Playwright harness BLOCKED (no app-shell dev server exists yet to drive)" — `docs/jira/v26.7.23/TICKET-039-playwright-and-accessibility-platform-harness.md:5`
- Citation: "Playwright harness half: BLOCKED as instructed. `examples/interview-assist/playwright.config.ts` does not exist... the app-shell/TICKET-014 workstream this depends on has not generated yet. Delivered `tests/harness/playwright-setup.ts` as a real, runnable base config... but it has NOT been run against a live dev server in this pass — no such server exists." — `docs/jira/v26.7.23/TICKET-039-playwright-and-accessibility-platform-harness.md:139-146`
- Citation: "`announceToLiveRegion`... and `speak`... are present in source but NOT exercised this pass — jsdom has no `SpeechSynthesis` polyfill... both are documented in-source as requiring a real browser (Playwright), not mocked to fake a pass." — `docs/jira/v26.7.23/TICKET-039-playwright-and-accessibility-platform-harness.md:132-138`

## Objective

Once the app-shell dev server exists (workstream C/D14 lands), run TICKET-039's already-authored `tests/harness/playwright-setup.ts` against it for the first time and confirm it actually drives a real browser.

## Current state

`tests/harness/playwright-setup.ts` is real, authored, and self-documented as never having been run against a live dev server — no such server exists yet in the tree. `announceToLiveRegion`/`speak` are present in source but unexercised because jsdom has no `SpeechSynthesis` polyfill.

## Target state

`playwright.config.ts` exists, `next dev` (or equivalent) serves the app shell, and the Playwright harness runs at least one real test against a real browser, exercising `announceToLiveRegion` and `speak` against a real DOM.

## Projection classification

- Template: N/A — this is a verification-execution ticket, the harness itself is already authored by TICKET-039
- Domain data: none new
- Custom code: N/A — no new production code, this is running an existing test harness for the first time

## Inputs

- `examples/interview-assist/tests/harness/playwright-setup.ts` (already authored by TICKET-039)
- the app-shell dev server (workstream C/D011/D014's dependency)

## Outputs

- `examples/interview-assist/playwright.config.ts`
- a passing first real Playwright run transcript

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

None — harness authorship already happened in TICKET-039; this ticket only executes it for the first time.

## Domain-data responsibility

None.

## Custom-code boundary

None new.

## Exclusions

- no claiming the harness 'works' based on the file existing — per TICKET-039's own words, 'file exists' is not 'harness verified end-to-end'
- no jsdom/SpeechSynthesis mock substituted for the real browser run

## Implementation steps

1. Confirm the app-shell dev server (workstream C, gated on D011/D014's TypeScript config resolution) is running and reachable.
2. Author `playwright.config.ts` pointing at the real dev server.
3. Run the existing harness for the first time; capture the full transcript, including any failures.
4. Specifically exercise `announceToLiveRegion` and `speak` against the real DOM/Web Speech API and assert on observable results (e.g. `[aria-live]` node content, or a speech-synthesis call being made).

## Admission gates

- TICKET-039
- TICKET-014
- D011 (package.json/tsconfig cross-workstream conflict)

## Acceptance criteria

Given a running dev server, when the Playwright harness executes, then it produces a real pass/fail transcript against a real browser — not a jsdom substitute — and `announceToLiveRegion`/`speak` are exercised at least once each with an observable assertion.

## Negative tests

Deliberately break the dev server (stop it) and confirm the harness fails with a real connection error, proving the test genuinely depends on a live server rather than silently passing regardless.

## Verification ladder

- Unit: N/A with reason — this is an end-to-end execution ticket, not a unit-test ticket
- Integration: Playwright harness against the real composed app shell
- End-to-end: the primary layer this ticket delivers — first real browser run
- Chaos: N/A with reason — noted per TICKET-039's own ladder as future work once base execution succeeds
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: first real Playwright run transcript, pass/fail, screenshots/traces on failure

## Receipts

- playwright.config.ts
- first real run transcript

## Dependencies

- TICKET-039
- TICKET-014
- D011

## Falsifier

If this ticket is claimed done without a real transcript from a run against a live dev server (i.e., still relying on the unexercised jsdom path), it is not complete.

## Handoff

D009 (Monaco DOM mount) and D023 (real AT-user certification) both depend on this harness actually running.

## Definition of done

- playwright.config.ts authored
- dev server confirmed reachable
- at least one real browser test run with a captured transcript
- announceToLiveRegion/speak exercised against real DOM
