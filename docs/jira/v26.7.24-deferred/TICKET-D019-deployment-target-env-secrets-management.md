# TICKET-D019: Production deployment target, environment-variable management, and secrets handling

## Status

DEFERRED — production-readiness work never scoped by the v26.7.23 epic

## Priority

P1 — blocks any real hosted deployment; the Ollama endpoint is currently a hardcoded localhost literal with zero process.env indirection anywhere, even as a stub

## Parent

EPIC: InterviewAssist v26.7.24-deferred

## Source

- Research report: Out-of-Scope Production Work Report, item 5
- Citation: `grep -rniE "vercel|deploy|deployment target|hosting|environment variable|\.env|secrets manager"` across EPIC/README/57 tickets: 0 hits (Out-of-Scope Production Work Report, item 5).
- Citation: Reading `examples/interview-assist/lib/adapters/ollama-adapter.ts` directly: the endpoint is a hardcoded literal, `baseUrl: "http://localhost:11434/v1"`, with zero `process.env` lookups anywhere in the file and no `apiKey` field in its config type at all. No `.env`/`.env.example` file is present in the directory listing.

## Objective

Choose a deployment target and introduce environment-variable indirection (starting with the Ollama endpoint URL and any future hosted-LLM API key) plus a secrets-handling approach appropriate to that target.

## Current state

The Ollama base URL is a hardcoded `http://localhost:11434/v1` literal with no `process.env` indirection and no config field for an API key. No `.env`/`.env.example` file exists. No deployment target is named anywhere in the corpus.

## Target state

The Ollama (or any future hosted-LLM) endpoint and any credentials are read from environment variables with a documented `.env.example`, and a chosen deployment target's secrets-handling mechanism (e.g. platform-native secrets manager) is wired in.

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

1. Add `process.env.OLLAMA_BASE_URL` (with the current hardcoded value as a documented default) to `ollama-adapter.ts`'s config resolution.
2. Add an optional `apiKey` config field, sourced from an environment variable, for any future hosted-LLM provider.
3. Author a `.env.example` documenting every environment variable the app consumes.
4. Select and document a deployment target, and wire its native secrets-handling mechanism (e.g. platform environment-variable injection) rather than committing secrets to the repo.

## Admission gates

- re-verification that the gap is still current (grep-based, per this ticket's own Source method)

## Acceptance criteria

Given a deployment environment with `OLLAMA_BASE_URL` set to a non-default value, when the Ollama adapter initializes, then it uses the environment value, not the hardcoded literal; given no `.env` file committed, when the repo is inspected, then no secret value is present in source.

## Negative tests

Set `OLLAMA_BASE_URL` to an unreachable address and confirm the adapter's real connection attempt fails against that address, proving the environment variable is actually consulted rather than the hardcoded literal silently winning.

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
