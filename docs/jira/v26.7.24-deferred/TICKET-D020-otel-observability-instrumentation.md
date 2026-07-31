# TICKET-D020: OpenTelemetry observability/instrumentation for InterviewAssist's TypeScript runtime

## Status

DEFERRED — production-readiness work never scoped by the v26.7.23 epic

## Priority

P1 — needed for production debugging; this repo's own otel-validation.md mandates spans for exactly the categories TICKET-035/036/037 implement, and none exist

## Parent

EPIC: InterviewAssist v26.7.24-deferred

## Source

- Research report: Out-of-Scope Production Work Report, item 6
- Citation: `grep -rniE "otel|opentelemetry|telemetry|tracer|span|instrumentation"` across all 57 tickets: 0 hits (Out-of-Scope Production Work Report, item 6).
- Citation: `examples/interview-assist/package.json` declares zero direct `@opentelemetry/*` dependency; the one `@opentelemetry/api` hit inside `package-lock.json` is a nested transitive entry (most likely pulled in by Next.js itself), not something application code imports — grep for OTEL usage in `.ts`/`.tsx` source returns only an unrelated `DiagnosticsPanel` component-name false-positive, zero real spans/tracer calls.
- Citation: This is a genuine gap against `.claude/rules/otel-validation.md`'s mandatory-span requirement for exactly the categories TICKET-035 (subprocess execution), TICKET-036 (persistence), and TICKET-037 (Ollama HTTP transport) implement.

## Objective

Add real OpenTelemetry spans to TICKET-035/036/037's adapters (subprocess execution, persistence, Ollama transport), matching this repo's own `.claude/rules/otel-validation.md` mandate for exactly these categories.

## Current state

No direct `@opentelemetry/*` dependency exists in `examples/interview-assist/package.json`; zero real spans or tracer calls exist anywhere in the TypeScript source, despite this repo's own rule requiring OTEL spans for LLM/external-service/subprocess operations.

## Target state

`sandbox-executor.ts`, `persistence-adapter.ts`, and `ollama-adapter.ts` each emit real spans (e.g. `sandbox.execute`, `persistence.write`, `llm.complete`) with populated attributes (duration, model, token counts where applicable), verifiable via `RUST_LOG`-equivalent trace capture for a Node/TS runtime.

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

1. Add a direct `@opentelemetry/api` (and an appropriate SDK/exporter) dependency to `examples/interview-assist/package.json`.
2. Instrument `sandbox-executor.ts`'s execute/compile/timeout-kill paths with real spans and duration attributes.
3. Instrument `persistence-adapter.ts`'s write/read operations with real spans.
4. Instrument `ollama-adapter.ts`'s `llm.complete`-equivalent call with model/token/duration attributes, mirroring this repo's existing Rust-side OTEL convention for LLM calls.
5. Capture a real trace output and grep for the expected span names, per this repo's own `otel-validation.md` verification procedure.

## Admission gates

- re-verification that the gap is still current (grep-based, per this ticket's own Source method)

## Acceptance criteria

Given a real sandbox execution, persistence write, or Ollama call, when trace output is captured, then the corresponding span exists with non-placeholder attributes (real duration, real token counts where applicable) — not zero/empty values.

## Negative tests

Run an adapter operation with tracing enabled and confirm the span's duration attribute reflects real elapsed wall-clock time (not a synthetic/zero value), and that a deliberately-failed operation produces an error-flagged span rather than no span at all.

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
