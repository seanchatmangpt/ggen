# TICKET-D027: Self-play admission verified via exactly one live Ollama call; TICKET-052's repeated-trial claim has zero coverage

## Status

DEFERRED — verification is real but narrower than the property its ticket's prose implies

## Priority

P2 — verification-depth gap, not an immediate runtime risk given the execution-gated design; TICKET-052's actual scenario is still PLANNED (not yet authored) so this widens future authorship rather than fixing an existing shallow test

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-037-... (ollama-adapter.test.ts), target ticket TICKET-052 (self-play manufacturing scenario, PLANNED)`

## Source

- Research report: Thin Verification Report, finding 2
- Citation: `tests/adapters/ollama-adapter.test.ts` (TICKET-037, which TICKET-052 depends on) makes exactly one real live call, gated by `it.runIf(reachable)`, asserting only that `content`/`model` are non-empty strings (Thin Verification Report, finding 2).
- Citation: TICKET-037's own "Implementation notes" confirm this: "3 tests, all passing... live-call test took 9.1–14.2s" — one live trial. There is no loop, no statistical sample, and no test anywhere touching self-play/selfPlay in the test tree.
- Citation: TICKET-052's actual claim — that execution-gated admission holds across many generations (a self-play candidate that fails its own test is refused; one that passes is admitted) — has zero coverage at any repetition count; the underlying scenario test doesn't exist yet (PLANNED).

## Objective

When TICKET-052 (currently PLANNED) is authored, ensure it tests execution-gated self-play admission across a real repeated-trial sample, not a single live Ollama call — closing the gap between TICKET-037's single-trial connectivity check and TICKET-052's actual multi-generation admission claim.

## Current state

TICKET-037's Ollama adapter test makes one real live call and checks basic response shape. TICKET-052, the ticket that actually needs to prove self-play admission holds across many generations, does not exist yet — there is zero coverage of the repeated-trial claim at any repetition count.

## Target state

TICKET-052, once authored, includes a real test running the self-play loop across N real generations (N > 1, ideally covering both a failing and a passing candidate), asserting execution-gated admission/refusal holds for each — not merely that one Ollama call returns non-empty content.

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

1. When TICKET-052 is picked up, design its test to run the self-play generate-execute-admit/refuse loop across multiple real generations, not a single call.
2. Include at least one real generation that fails its own execution-gated test (expect refusal) and one that passes (expect admission), per TICKET-052's actual claim.
3. Use real Ollama calls throughout (per Chicago TDD), accepting the wall-clock cost TICKET-037 already documented (9.1-14.2s per call) as a known, disclosed cost of real coverage.
4. Record the repetition count actually achieved and whether it constitutes a statistically meaningful sample, honestly, rather than claiming full coverage from a small N.

## Admission gates

- TICKET-037-...

## Acceptance criteria

Given N real self-play generations (N > 1), when each is executed and evaluated, then execution-gated admission/refusal holds correctly for both a real passing and a real failing candidate — not asserted from a single live call's non-empty response.

## Negative tests

Include at least one real generation deliberately expected to fail its own test and confirm the system refuses admission for it, proving the gate isn't a rubber stamp that admits everything the model returns.

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

- TICKET-037-... (ollama-adapter.test.ts), target ticket TICKET-052 (self-play manufacturing scenario, PLANNED)

## Falsifier

If the widened test still exercises only the same single instance/case as before, or introduces a mock in place of the real collaborator, this ticket is not complete.

## Handoff

Feeds the relevant workstream I vertical scenario's own eventual closure, once it is picked up.

## Definition of done

- test widened per the specific gap named in Source
- real collaborator used throughout, no mocks
- negative case included where applicable
