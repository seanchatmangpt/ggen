# TICKET-037: Ollama client adapter (custom)

## Status

ALIVE — real chat-completion call against a live local Ollama server succeeded this pass

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/70-packs-datasets.ttl (<service/self-play-factory>)`
- PRD requirements:
  - `PRD §10 Self-play requirements`
- ARD components:
  - `ARD §3.12 Self-Play Factory`
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Implement the real Ollama OpenAI-compatible HTTP transport behind a generated SelfPlayWorker port, reusing this session's already-proven local-model integration pattern (@ai-sdk/openai-compatible against http://localhost:11434/v1) from examples/nextjs-ai-sdk rather than reinventing the transport.

## Current state

UNKNOWN — no implementation exists yet; classified in advance as genuinely custom per the analysis below.

## Target state

A working adapter implementing the generated port `a generated SelfPlayWorker port (interviewer/candidate/test-generator/critic roles per ARD §3.12)`, with zero domain-rule duplication (all domain checks — policy, precondition, refusal — are called INTO from workstream E/F, never reimplemented here).

## Projection classification

- Template: 20%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 80%
- Expected ratio: 20/80
- Custom-code justification: Network transport to a local model server is an irreducible external-process integration; this adapter must run OUTSIDE the live runtime's critical path per ARD's own architecture decision that the live runtime does not require an LLM.

## Inputs

- generated port: a generated SelfPlayWorker port (interviewer/candidate/test-generator/critic roles per ARD §3.12)

## Outputs

- examples/interview-assist/lib/adapters/ollama-adapter.ts (reusing examples/nextjs-ai-sdk's proven transport pattern)

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

The port interface itself (a generated SelfPlayWorker port (interviewer/candidate/test-generator/critic roles per ARD §3.12)) is generated (workstream E/F/G) — this ticket implements it, does not define it.

## Domain-data responsibility

NONE — this adapter must not independently redefine domain rules already present in RDF (per the user's explicit custom-code exclusion). Any domain check needed (policy, precondition) is imported and called, never reimplemented.

## Custom-code boundary

Network transport to a local model server is an irreducible external-process integration; this adapter must run OUTSIDE the live runtime's critical path per ARD's own architecture decision that the live runtime does not require an LLM.

## Exclusions

- no domain rule reimplementation — must call into workstream E/F's generated checks, never duplicate them
- no direct capability-list literal
- no bypass of TICKET-028's policy/precondition checks

## Implementation steps

1. Confirm the generated port interface this adapter must satisfy (from its Source dependency).
2. Implement the adapter, calling workstream E/F's generated policy/precondition checks before performing any real action.
3. Write a Chicago-TDD test using the REAL collaborator (real subprocess / real browser API / real Ollama endpoint / real crypto library), per this repo's own testing.md — no mocks.
4. Document the reduction path: if self-play migrates to an in-process embedding model, the network-transport half of this adapter shrinks; the worker-role logic stays.

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

- TICKET-018

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

- File: `examples/interview-assist/lib/adapters/ollama-adapter.ts` (130 lines); test:
  `examples/interview-assist/tests/adapters/ollama-adapter.test.ts` (3 tests, all passing —
  `npx vitest run tests/adapters/ollama-adapter.test.ts`, live-call test took 9.1–14.2s).
- Reachability check (mirrors the assignment's suggested `curl -s http://localhost:11434/api/tags`):
  a real local Ollama server WAS reachable this pass, reporting model `qwen3.5:0.8b` (873M
  params, Q8_0) among its `/api/tags` response — not simulated.
  `isOllamaReachable()` performs the identical real HTTP GET and is real, not stubbed.
- Real live-call evidence: sent a real POST to `http://localhost:11434/v1/chat/completions`
  with role `interviewer`, prompt `"Reply with exactly the word: PONG"`. Response returned a
  non-empty `content` string and a non-empty `model` string from the real Ollama HTTP server —
  test asserts both are non-empty (content is model-dependent free text, not asserted
  byte-exact).
- Transport reuses examples/nextjs-ai-sdk's proven local-model integration shape (OpenAI-compatible
  endpoint against `http://localhost:11434/v1`) — via a direct `fetch` call rather than the
  `@ai-sdk/openai-compatible` package, since that SDK is not a dependency of this thin-adapter
  package; the wire protocol being reused is the proven part, documented explicitly in-source.
- TICKET-018's real generated SelfPlayWorker port has not landed; `SelfPlayRequest`/
  `SelfPlayResponse`/`SelfPlayWorker` are hand-authored and marked `PENDING(TICKET-018)`.
- Policy-check-before-action: `run()` calls `checkPolicy(...)` before constructing the
  `AbortController`/`fetch` call — currently deferring to the `PENDING(TICKET-028)` placeholder.
- A compile-time correctness check against a deliberately unreachable port (`localhost:1`)
  confirms the adapter reaches the network layer and fails there (not from a type/shape bug),
  independent of whether a real Ollama server happens to be running.
