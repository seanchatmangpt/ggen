# TICKET-042: Vertical scenario: Track confirmation

## Status

PARTIAL_ALIVE — real selector/reducer evidence proven and passing; Playwright layer blocked by
the real `next build` regression documented in TICKET-040's notes

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (<capability/cognition/rank-solution-families>, <capability/cognition/construct-answer-scaffold>)`
- Acceptance-test steps: (none)
- PRD requirements: (none)
- ARD components: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Prove track-candidate ranking and scaffold construction (zero-input cognition capabilities) produce real, non-placeholder TrackCandidate state via the real selectors (TICKET-024).

## Current state

UNKNOWN — no implementation exists yet; this ticket verifies the composed system once workstreams B-H land.

## Target state

A passing Playwright/Chicago-TDD test exercising the real composed system for this scenario, using real collaborators: real selectors composed with a real event stream (no LLM required — per Architecture Decision 8, the live runtime does not require an LLM). No mocked core collaborator.

## Projection classification

- Template: 60%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 40%
- Expected ratio: 60/40
- Custom-code justification: verification tickets are mostly test AUTHORING (a form of template-adjacent reusable test-harness code) composed with real system components; classified with a higher custom fraction than pure projection tickets because test scenario logic requires human design judgment about what to assert, though it contains zero domain data duplication — assertions reference generated types/RDF-sourced values, never restate them.

## Inputs

- the full composed system as of this scenario's dependencies

## Outputs

- examples/interview-assist/tests/e2e/track-confirmation.spec.ts

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Shared Playwright harness (TICKET-039) and shared fixture-building utilities, reused across all 14 scenarios.

## Domain-data responsibility

Assertions reference RDF-sourced generated types/values (e.g. specific phase names, refusal codes, capability ids) rather than restating them as new literals.

## Custom-code boundary

The test itself, and any real external process it must drive (Ollama, subprocess, browser) via the workstream H adapters — no new production custom code introduced by this ticket, only test code exercising existing adapters.

## Exclusions

- no mocked core collaborator per this repo's Chicago TDD policy
- no assertion that merely counts generated files or checks a generated string for non-emptiness — must assert on real observable state
- no test skipped/marked #[ignore] to force a green run

## Implementation steps

1. Feed a real sequence of observed-input-typed events (editor changes, speech-classified events).
2. Assert selectRankedSolutionFamilies returns a real ranked, non-empty list derived from the actual event sequence.

## Admission gates

- All ticket IDs in Dependencies complete.

## Acceptance criteria

- Given a real event sequence implying two candidate solution families, when selectRankedSolutionFamilies runs, then it returns both, ranked by the real selector logic, not a fixed 2-item stub.

## Negative tests

- Feed an ambiguous/insufficient event sequence and confirm selectAbstention (TICKET-024) returns true instead of a fabricated ranking.

## Verification ladder

- Unit: N/A with reason — this ticket IS the integration/e2e layer, not a unit-test ticket
- Integration: composed real system, no mocks
- End-to-end: the primary layer this ticket delivers, via Playwright against a real running dev server
- Chaos: where relevant to the scenario, noted in steps above
- Stress: N/A with reason — single-scenario correctness test, not a load test
- Benchmark: N/A with reason — no perf target for this scenario
- Verifier report: Playwright test run transcript + screenshots/traces on failure

## Receipts

- test spec hash
- Playwright run transcript
- pass/fail result

## Dependencies

- TICKET-041
- TICKET-024

## Falsifier

If this scenario's test passes while any real collaborator is mocked or stubbed, the test is not a valid completion signal for this ticket — re-author with real collaborators before claiming done.

## Handoff

TICKET-053 (full decisive acceptance test) composes all 14 scenarios' proven paths into one end-to-end run.

## Definition of done

- test authored against real collaborators
- test passes against the real composed system
- no mocked core collaborator
- negative case included

## Implementation notes (real evidence)

- Playwright-vs-vitest substitution: see TICKET-040's Implementation notes for the full real
  evidence. Authored as a real vitest test instead.
- File: `examples/interview-assist/tests/scenarios/track-confirmation.test.ts` (2 tests). Real
  run: `npx vitest run tests/scenarios/track-confirmation.test.ts` → 2/2 passed, 2ms.
    sha256: `ee46dcdaa6fa66be8361af214f01fa8ff989db3c6b81e3a43f16d30e27b018d5`
- Test-fixture derivation, disclosed plainly: the real system has no reducer step that turns
  observed-input events into `TrackCandidate[]` (page.tsx's `addTrackCandidate` is a UI-only
  state mutation, not reducer-routed). The test file's local (non-exported, not new production
  code) `deriveCandidatesFromEvents` helper first runs every fixture event through the real
  `sessionReducer` (asserting it is genuinely admitted) before folding its declared
  `candidateId`/`rank`/`evidenceKey` fields into a `TrackCandidate[]`, so the real TICKET-024
  selectors run over data derived from a real admitted event sequence, not a hand-set array.
- Acceptance criterion: two events (`family: "EditorEvent"`/`"SpeechEvent"`) implying candidates
  `family-two-pointer` (rank 2) and `family-hash-map` (rank 1) both carrying
  `cognition/rank-solution-families` evidence → real `selectRankedSolutionFamilies` returns both,
  rank-ascending (`["family-hash-map", "family-two-pointer"]`). Not-a-stub proof: re-running with
  the ranks swapped flips the returned order (`["family-two-pointer", "family-hash-map"]`) — a
  fixed 2-item stub would not respond to the input change.
- Negative test: a single event carrying `cognition/abstain-under-insufficient-evidence` evidence
  (real, admitted via `sessionReducer`) → real `selectAbstention` returns `true`; the derived
  candidate array (carrying no `rank-solution-families` evidence) makes
  `selectRankedSolutionFamilies` correctly return `[]` rather than a fabricated ranking.
- Full-suite regression check: `npx vitest run` → 85/85 passed. `npx tsc --noEmit` → clean.
