# TICKET-039: Playwright browser harness + accessibility platform adapters (custom)

## Status

PARTIAL_ALIVE — accessibility-adapter DOM-free logic implemented and tested; Playwright harness BLOCKED (no app-shell dev server exists yet to drive)

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability/accessibility/screen-reader-semantic-regions>, <capability/accessibility/braille-display-output>, <capability/accessibility/text-to-speech-projection>)`
- ARD components: (none)
- PRD requirements: (none)
- Acceptance-test steps:
  - `packs/wasm4pm-interview-assist-pack/ontology/80-acceptance.ttl (<impl-phase/4>)`
- Policies: (none)
- SHACL shapes: (none)

## Objective

Implement (a) the Playwright test harness used by every workstream I scenario, reusing examples/nextjs-ai-sdk's proven Playwright config (TICKET-014) as its base, and (b) the real platform accessibility API bindings (ARIA live-region wiring, Web Speech API for TTS) behind TICKET-033's generated accessibility-controls port.

## Current state

UNKNOWN — no implementation exists yet; classified in advance as genuinely custom per the analysis below.

## Target state

A working adapter implementing the generated port `TICKET-033's accessibility-controls.tsx port + TICKET-014's Playwright config`, with zero domain-rule duplication (all domain checks — policy, precondition, refusal — are called INTO from workstream E/F, never reimplemented here).

## Projection classification

- Template: 20%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 80%
- Expected ratio: 20/80
- Custom-code justification: Browser accessibility platform APIs (screen reader semantics via native ARIA, Web Speech API, Braille-compatible output) are irreducible browser/OS bindings with no RDF representation; the Playwright harness itself drives a real browser, which is by definition outside what a template can generate.

## Inputs

- generated port: TICKET-033's accessibility-controls.tsx port + TICKET-014's Playwright config

## Outputs

- examples/interview-assist/lib/adapters/accessibility-platform-adapter.ts
- examples/interview-assist/tests/harness/playwright-setup.ts

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The port interface itself (TICKET-033's accessibility-controls.tsx port + TICKET-014's Playwright config) is generated (workstream E/F/G) — this ticket implements it, does not define it.

## Domain-data responsibility

NONE — this adapter must not independently redefine domain rules already present in RDF (per the user's explicit custom-code exclusion). Any domain check needed (policy, precondition) is imported and called, never reimplemented.

## Custom-code boundary

Browser accessibility platform APIs (screen reader semantics via native ARIA, Web Speech API, Braille-compatible output) are irreducible browser/OS bindings with no RDF representation; the Playwright harness itself drives a real browser, which is by definition outside what a template can generate.

## Exclusions

- no domain rule reimplementation — must call into workstream E/F's generated checks, never duplicate them
- no direct capability-list literal
- no bypass of TICKET-028's policy/precondition checks

## Implementation steps

1. Confirm the generated port interface this adapter must satisfy (from its Source dependency).
2. Implement the adapter, calling workstream E/F's generated policy/precondition checks before performing any real action.
3. Write a Chicago-TDD test using the REAL collaborator (real subprocess / real browser API / real Ollama endpoint / real crypto library), per this repo's own testing.md — no mocks.
4. Document the reduction path: if a future W3C standard exposes a more declarative accessibility-preference API, more of the platform-binding half could shrink; the harness half (driving a real browser) remains irreducibly custom by nature.

## Admission gates

- The generated port ticket this adapter implements (see Dependencies).

## Acceptance criteria

- Given a real invocation, when the adapter executes, then it produces output conforming to the generated port's typed return shape, with no domain-rule bypass.

## Negative tests

- Attempt an action the policy/precondition checks would deny and confirm the adapter refuses BEFORE performing the real action (proving it calls into the generated check rather than acting first and checking later).

## Verification ladder

- Unit: adapter unit test against the real collaborator in an isolated/ephemeral instance
- Integration: adapter composed with the real dispatch table (TICKET-027) and real policy check (TICKET-028)
- End-to-end: exercised in the relevant workstream I vertical scenario
- Chaos: kill/interrupt the real collaborator mid-operation and confirm graceful, receipted refusal, not a hang or crash
- Stress: N/A with reason — noted per-adapter below if a stress profile is genuinely relevant
- Benchmark: N/A with reason — no fixed perf target at this phase
- Verifier report: real-collaborator test transcript

## Receipts

- adapter source hash
- real-collaborator test transcript
- policy-check-called-before-action confirmation

## Dependencies

- TICKET-033
- TICKET-014

## Falsifier

If this adapter performs its real action WITHOUT first calling the generated policy/precondition check, this ticket is not complete — that would be exactly the 'custom code independently redefining domain rules' failure the user's instructions explicitly forbid.

## Handoff

The relevant workstream I vertical scenario exercises this adapter against real infrastructure.

## Definition of done

- adapter implements the generated port exactly
- real-collaborator Chicago-TDD test passes (no mocks)
- policy-check-before-action test passes
- reduction path documented

## Implementation notes (real evidence)

- Files: `examples/interview-assist/lib/adapters/accessibility-platform-adapter.ts` (97 lines),
  `examples/interview-assist/tests/harness/playwright-setup.ts` (34 lines). Test:
  `examples/interview-assist/tests/adapters/accessibility-platform-adapter.test.ts` (4 tests,
  all passing — `npx vitest run tests/adapters/accessibility-platform-adapter.test.ts`).
- Accessibility-adapter half: `ariaLiveForSeverity`/`buildAnnouncement` (DOM-free severity ->
  ARIA `aria-live` politeness mapping + announcement construction, calling the policy check
  first) are real, unit-tested logic. `announceToLiveRegion` (writes into a real `[aria-live]`
  DOM node) and `speak` (real Web Speech API `speechSynthesis`/`SpeechSynthesisUtterance`) are
  present in source but NOT exercised this pass — jsdom has no `SpeechSynthesis` polyfill, and
  this package's toolchain has no jsdom dependency either; both are documented in-source as
  requiring a real browser (Playwright), not mocked to fake a pass.
- Playwright harness half: BLOCKED as instructed. `examples/interview-assist/playwright.config.ts`
  does not exist — in fact NOTHING existed under `examples/interview-assist/` before this
  ticket set ran (`find examples/interview-assist` returned empty); the app-shell/TICKET-014
  workstream this depends on has not generated yet. Delivered
  `tests/harness/playwright-setup.ts` as a real, runnable base config (reusing
  examples/nextjs-ai-sdk's config shape) that TICKET-014's future app-shell generation can point
  at, but it has NOT been run against a live dev server in this pass — no such server exists.
  Stated as BLOCKED rather than claimed working.
- TICKET-033's real generated accessibility-controls.tsx port has not landed; the
  `AccessibilityControlsProps` interface is hand-authored and marked `PENDING(TICKET-033)`.
- Policy-check-before-action: `buildAnnouncement` calls `checkPolicy(...)` before returning —
  currently deferring to the `PENDING(TICKET-028)` placeholder.
