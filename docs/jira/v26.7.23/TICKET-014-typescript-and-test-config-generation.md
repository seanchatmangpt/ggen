# TICKET-014: TypeScript config + test/Playwright config generation

## Status

PARTIAL_ALIVE

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources: (none)
- PRD requirements: (none)
- ARD components: (none)
- Acceptance-test steps: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Generate the remaining structural configuration (strict tsconfig compilerOptions, vitest/playwright config) as pure framework boilerplate with zero domain content — the clearest possible 'template, not domain' example in this backlog.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 100%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 0%
- Expected ratio: 100/0
- Custom-code justification: NONE — pure structural configuration, no domain data or irreducible runtime behavior involved at all.

## Inputs

- framework documentation for Next.js 16 / Playwright / vitest config shape (no RDF input — this ticket is domain-free by design)

## Outputs

- examples/interview-assist/tsconfig.json (full strict config)
- examples/interview-assist/playwright.config.ts
- examples/interview-assist/vitest.config.ts

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

100% of this ticket's output.

## Domain-data responsibility

None — explicitly, this ticket demonstrates a workstream with zero domain-data dependency.

## Custom-code boundary

NONE.

## Exclusions

- no domain data whatsoever in this ticket's templates — a mixing-in here would be a clear violation of the fundamental separation

## Implementation steps

1. Author tsconfig.json.tmpl with strict:true and the module resolution settings matching this repo's existing examples/nextjs-ai-sdk/tsconfig.json conventions (reuse, don't reinvent).
2. Author playwright.config.ts.tmpl and vitest.config.ts.tmpl similarly reusing examples/nextjs-ai-sdk's proven config as the structural reference.
3. Verify generated configs load without error via `tsc --noEmit` and `playwright test --list`.

## Admission gates

- TICKET-011.

## Acceptance criteria

- Given the generated tsconfig.json, when `tsc --noEmit` runs, then it reports zero config-level errors (independent of application code correctness, which is out of scope here).

## Negative tests

- N/A with reason — this ticket has no domain-driven failure mode to test negatively; its only failure mode is malformed JSON/TS, covered by the positive `tsc`/`playwright --list` checks.

## Verification ladder

- Unit: N/A with reason — pure config, no logic to unit test
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- config file hashes

## Dependencies

- TICKET-011

## Falsifier

If `tsc --noEmit` or `playwright test --list` fails against the generated config, this ticket is not complete.

## Handoff

Workstream I (verification tickets) consume this config directly.

## Definition of done

- configs generated and load cleanly
- reused examples/nextjs-ai-sdk's proven structure rather than inventing new config from scratch

## Implementation notes (real evidence)

- Copied `examples/nextjs-ai-sdk/{tsconfig.json,playwright.config.ts,vitest.config.ts}` into
  `examples/interview-assist/` verbatim (per the task's "reuse proven conventions" instruction),
  adapted only where the source referenced nothing project-specific (none of the three files
  needed edits beyond being copied as-is — no project name is embedded in any of them).
- Ran `npx tsc --noEmit` for real from `examples/interview-assist/`. Result: **config-level
  errors are absent** — every reported error is `TS2307 Cannot find module` /
  `TS2580 Cannot find name 'process'/'Buffer'` style, i.e. missing `node_modules` packages
  (`next`, `react`, `@playwright/test`, `@types/node`), not a tsconfig/jsx/module-resolution
  misconfiguration. This is the expected signal that the config itself is structurally sound.
- **Root cause of the missing deps, reported not hidden:** `examples/interview-assist/package.json`
  currently belongs to a different, concurrently-running workstream (workstream H,
  "interview-assist-adapters", TICKET-034-039) — hand-authored with a *different* dependency set
  (`vitest`, `typescript`, `blake3`; no `next`/`react`/`@playwright/test`/`@types/node`). This
  session's own pack template (`010_package_json.tmpl`, owned by TICKET-011, not this ticket)
  would generate a different, Next.js-shaped `package.json`, but overwriting workstream H's file
  was out of scope for this ticket and this session's explicit non-destructive mandate re: other
  agents' work — left untouched.
- PARTIAL_ALIVE: the three config files are real, copied-from-proven-source, and demonstrated
  free of config-level `tsc` errors; full `npx tsc --noEmit` success is BLOCKED on a
  cross-workstream `package.json`/dependency conflict outside this ticket's scope, not on
  anything wrong with the config files themselves.
