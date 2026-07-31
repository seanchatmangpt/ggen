# TICKET-D028: Component tests are SSR-string-only; zero interaction, keyboard, or ARIA live-region behavior testing

## Status

DEFERRED — verification is real but narrower than the property its ticket's prose implies

## Priority

P1 — relates directly to D023 (real AT-user certification) — without interaction testing, the components most relevant to accessibility have never had their actual interactive behavior exercised

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-030/031/032/033-... (accessibility-controls.test.tsx, refusal-presentation.test.tsx, session-summary.test.tsx)`

## Source

- Research report: Thin Verification Report, finding 3
- Citation: Grep across `tests/components/*.tsx` confirms all three files exclusively import and call `renderToStaticMarkup` from `react-dom/server`. No `fireEvent`, `userEvent`, `@testing-library`, or jsdom-based interaction anywhere (Thin Verification Report, finding 3).
- Citation: What they prove is real: `accessibility-controls.test.tsx` checks the rendered HTML string contains `data-control-count="16"` and a `data-testid` for each of the 16 real `AccessibilityDefaults` keys; `refusal-presentation.test.tsx` checks all 16 refusal codes render distinct markup.
- Citation: What they don't touch at all: click handlers, `onChange` firing with correct values, keyboard focus order, ARIA live-region behavior — anything an assistive-technology user's actual interaction would exercise. `tests/harness/playwright-setup.ts` is explicitly self-documented as unexercised.

## Objective

Add real interaction-level component tests (jsdom + `@testing-library`/`fireEvent`, or once D008 lands, Playwright) exercising click handlers, `onChange` firing, and keyboard focus order for `accessibility-controls.tsx`, `refusal-presentation.tsx`, and `session-summary.tsx` — beyond the current SSR-string-only checks.

## Current state

All three component test files verify only static SSR HTML output (`renderToStaticMarkup`) — presence of `data-testid`/`data-control-count` attributes and distinct markup per refusal code. Zero interaction is tested: no click, no `onChange`, no keyboard focus order, no ARIA live-region firing.

## Target state

Each of the 3 components has real interaction tests: `accessibility-controls.tsx`'s toggles fire `onChange` with the correct value when clicked; `refusal-presentation.tsx`'s and `session-summary.tsx`'s interactive elements respond to real keyboard focus/activation, exercised via a real DOM environment (jsdom or a real browser via D008), not merely SSR string presence checks.

## Projection classification

- Template: N/A — this is a test-coverage-widening ticket, following the same test-AUTHORING classification workstream I's own tickets already use
- Domain data: none new — widened assertions still reference generated types/RDF-sourced values, never restate them as new literals, per the existing tickets' own Domain-data responsibility pattern
- Custom code: matches the relevant workstream I ticket's own 60/40 classification, since this is the same kind of real-collaborator test authoring

## Inputs

- the existing real, passing test file this ticket widens (see Source)
- the real collaborator(s) already used by that test

## Outputs

- an updated or additional test file widening coverage to the specific narrower-than-target gap named in Source

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

Reuses the existing shared test-harness/fixture patterns already established by the relevant workstream I ticket.

## Domain-data responsibility

Assertions continue to reference RDF-sourced generated types/values rather than introducing new domain literals.

## Custom-code boundary

The test itself, and any real external process it drives — no new production custom code introduced by this ticket, only wider test coverage of existing adapters/logic.

## Exclusions

- no mocked core collaborator, per this repo's Chicago TDD policy
- no assertion that merely counts generated files or checks a generated string for non-emptiness — must assert on real observable state
- no claiming the underlying claim is now fully verified when only the specific narrower gap named in Source has been closed — other narrower-than-target gaps in the same ticket may remain

## Implementation steps

1. Add a jsdom-based test environment (or extend the existing one) supporting `@testing-library/react` and `fireEvent`/`userEvent` for these 3 component files.
2. For `accessibility-controls.tsx`: click each of the 16 real toggle controls and assert the correct `onChange` value fires.
3. For `refusal-presentation.tsx`: verify keyboard focus reaches the relevant interactive elements and ARIA attributes update as expected.
4. For `session-summary.tsx`: verify any interactive summary elements respond correctly to real click/keyboard events.
5. Once D008's Playwright harness lands, add at least one real-browser interaction test per component as the deepest layer, per D023's real-AT-user-certification dependency.

## Admission gates

- TICKET-030/031/032/033-...

## Acceptance criteria

Given a real click or keyboard event on an interactive element in any of the 3 components, when fired via `fireEvent`/`userEvent` (or a real browser via Playwright), then the correct handler fires with the correct value — not merely 'the SSR markup contains this attribute.'

## Negative tests

Fire a click on a disabled/refused-state control and confirm the handler does NOT fire (or fires a documented no-op), proving the interaction test would catch a broken disabled-state implementation, not just a working happy path.

## Verification ladder

- Unit: widened real-collaborator unit test, per the specific gap named in Source
- Integration: composed with the real system components the existing test already uses
- End-to-end: N/A with reason — matches the existing test's own layer, typically unit/integration not e2e
- Chaos: N/A with reason — not the gap this ticket targets
- Stress: N/A with reason — not the gap this ticket targets unless explicitly named in Source
- Benchmark: N/A with reason
- Verifier report: widened test transcript, real collaborator, no mocks

## Receipts

- widened test file hash
- real-collaborator test transcript

## Dependencies

- TICKET-030/031/032/033-... (accessibility-controls.test.tsx, refusal-presentation.test.tsx, session-summary.test.tsx)

## Falsifier

If the widened test still exercises only the same single instance/case as before, or introduces a mock in place of the real collaborator, this ticket is not complete.

## Handoff

Feeds the relevant workstream I vertical scenario's own eventual closure, once it is picked up.

## Definition of done

- test widened per the specific gap named in Source
- real collaborator used throughout, no mocks
- negative case included where applicable
