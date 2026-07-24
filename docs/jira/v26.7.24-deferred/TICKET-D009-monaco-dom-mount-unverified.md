# TICKET-D009: Monaco editor DOM mount is UNVERIFIED — never run against a real browser

## Status

DEFERRED — environment-dependent, blocked on D008 (Playwright harness)

## Priority

P1 — the core code-editing surface of the entire app (Monaco) has never been verified to actually mount in a browser; a demo-blocking risk once real usage is attempted

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-034-monaco-runtime-adapter.md`

## Source

- Research report: Disclosed Gaps Catalog, section (b) Environment-dependent gaps
- Citation: "PARTIAL_ALIVE — DOM-free logic implemented and tested; Monaco DOM mount is UNVERIFIED (no browser test run this pass)" — `docs/jira/v26.7.23/TICKET-034-monaco-runtime-adapter.md:5`
- Citation: "What's UNVERIFIED: actually mounting `monaco.editor.create(...)` against a real DOM node plus its web-worker language service. Monaco requires a browser DOM + worker environment; this was not exercised via Playwright in this pass (TICKET-039's harness itself is BLOCKED — see that ticket's notes) — stated honestly rather than mocked or claimed working." — `docs/jira/v26.7.23/TICKET-034-monaco-runtime-adapter.md:135-138`

## Objective

Once D008's Playwright harness runs, mount `monaco.editor.create(...)` against a real DOM node plus its web-worker language service and confirm it actually initializes.

## Current state

TICKET-034's DOM-free config-building logic (`buildMonacoConfig`) is implemented and unit-tested; the actual `monaco.editor.create(...)` call against a real DOM node has never been exercised, because it requires a browser environment that D008's harness cannot yet drive.

## Target state

A real Playwright test mounts the Monaco editor in a real browser tab, confirms the editor instance initializes (e.g. `editor.getValue()` returns the seeded content, a keystroke updates the model), and the web-worker language service starts without error.

## Projection classification

- Template: N/A — this is verification execution, the adapter logic is already authored by TICKET-034
- Domain data: none new
- Custom code: N/A — no new production code

## Inputs

- `examples/interview-assist/lib/adapters/monaco-adapter.ts` (already authored)
- D008's working Playwright harness

## Outputs

- a passing real-browser Monaco-mount test
- updated TICKET-034 Status header from PARTIAL_ALIVE to ALIVE if the mount succeeds cleanly

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

None — TICKET-034's port/config logic is already generated/authored.

## Domain-data responsibility

None.

## Custom-code boundary

None new.

## Exclusions

- no claiming Monaco 'works' based on the DOM-free config-builder tests alone
- no substitute stub replacing the real `monaco.editor.create(...)` call

## Implementation steps

1. Confirm D008's Playwright harness is running against a real dev server.
2. Write a Playwright test that navigates to a page hosting the Monaco editor, waits for `monaco.editor.create(...)` to resolve, and asserts on the editor's initial content.
3. Type a keystroke via Playwright's real input simulation and assert the Monaco model updates.
4. Confirm the web-worker language service starts without a console error (read via `read_console_messages`-equivalent or Playwright's own console listener).

## Admission gates

- D008
- TICKET-034

## Acceptance criteria

Given a real browser tab with the app shell loaded, when the Monaco editor mounts, then `editor.getValue()` returns the expected seeded content, a real keystroke updates the model, and no console error is raised by the web-worker language service.

## Negative tests

Deliberately misconfigure the Monaco worker path and confirm the test fails with an observable console error, proving the test would actually catch a broken worker setup rather than passing regardless.

## Verification ladder

- Unit: N/A with reason — already covered by TICKET-034's DOM-free unit tests
- Integration: Monaco adapter composed with the real app shell
- End-to-end: the primary layer this ticket delivers — real browser mount
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: real Playwright transcript showing successful mount and keystroke update

## Receipts

- Playwright transcript
- console-log capture showing no worker error

## Dependencies

- D008
- TICKET-034

## Falsifier

If TICKET-034 is marked ALIVE without a real browser transcript showing `monaco.editor.create(...)` actually mounting and responding to input, this ticket is not complete.

## Handoff

Feeds the workstream I vertical scenarios that depend on a working editor (TICKET-043/044 Python/Rust coding workflows).

## Definition of done

- real browser mount test passes
- keystroke-to-model-update confirmed
- no worker console error
- TICKET-034 Status header updated if warranted
