# TICKET-D004: next.config.ts / tsconfig.json project templates not yet written

## Status

DEFERRED — permanent structural limitation as of the reports' read time (an authoring gap, not an engine gap)

## Priority

P1 — build/deploy config completeness; needed before a real Next.js build/deploy target exists (feeds D019)

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-011-nextjs-package-metadata-and-config-generation.md`

## Source

- Research report: Disclosed Gaps Catalog, section (a) Permanent structural limitations
- Citation: "`next.config.ts`/`tsconfig.json` templates not yet written." — `docs/jira/v26.7.23/TICKET-011-nextjs-package-metadata-and-config-generation.md:141`
- Citation: "Still PARTIAL_ALIVE, not ALIVE: `next.config.ts` is not yet generated (TICKET-012's scope)." — `docs/jira/v26.7.23/TICKET-011-nextjs-package-metadata-and-config-generation.md:167`

## Objective

Author the missing `next.config.ts`/`tsconfig.json` Tera templates so the generated Next.js application has a complete, real build configuration, not just `package.json` metadata.

## Current state

TICKET-011 generates real `package.json` (including a resolved `schema:version` triple, per that ticket's own later update) but explicitly defers `next.config.ts`/`tsconfig.json` template authorship to TICKET-012's scope; as of the report's read time these templates do not exist.

## Target state

`next.config.ts` and `tsconfig.json` are projected from the admitted graph (or from a reasonable static baseline where no domain data applies) exactly like `package.json`, with the same idempotent-sync guarantee TICKET-010 already applies to other templates.

## Projection classification

- Template: ~90% — config-file shape is reusable Next.js/TypeScript boilerplate; only project-specific paths/aliases would come from domain data if any
- Domain data: minimal — most Next.js/tsconfig options are framework convention, not InterviewAssist-specific facts
- Custom code: ~10% — any config option requiring runtime logic beyond static JSON/TS literals

## Inputs

- TICKET-011's existing `package.json` template as a pattern reference
- TICKET-014's TypeScript config work (related but distinct — TICKET-014 covers test/Playwright config, not the root `tsconfig.json`/`next.config.ts`)

## Outputs

- `packs/wasm4pm-interview-assist-pack/templates/0NN_next_config.tmpl` -> `examples/interview-assist/next.config.ts`
- `packs/wasm4pm-interview-assist-pack/templates/0NN_tsconfig.tmpl` -> `examples/interview-assist/tsconfig.json`

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

Both files are pure structural templates — Next.js/TypeScript configuration conventions, not InterviewAssist domain content.

## Domain-data responsibility

None expected — build configuration is not domain meaning.

## Custom-code boundary

None — this is template authorship, not custom runtime code.

## Exclusions

- no InterviewAssist-specific literal baked into a config file that should instead come from `package.json`'s already-generated fields
- no bypass of TICKET-014's separately-scoped test/Playwright config work

## Implementation steps

1. Author `next.config.ts` template using standard Next.js App Router configuration, referencing `package.json`'s generated values where relevant (e.g. no duplicated version literal).
2. Author `tsconfig.json` template using standard strict-TypeScript configuration for a Next.js project.
3. Wire both into the pack's `pack.toml` and existing sync gates.
4. Run `ggen sync run` twice consecutively and confirm byte-identical output (TICKET-010's idempotency bar) for both new files.
5. Confirm `npx tsc --noEmit` recognizes the new `tsconfig.json` (subject to D011's separate package.json cross-workstream conflict being resolved first, or noting that dependency explicitly).

## Admission gates

- TICKET-011
- TICKET-012

## Acceptance criteria

Given the pack's templates directory, when `ggen sync run` executes, then `next.config.ts` and `tsconfig.json` exist in `examples/interview-assist/`, are non-empty, valid syntax, and byte-identical across two consecutive syncs.

## Negative tests

Run `ggen sync run` twice and diff both generated files; any difference is a failure of the idempotency bar this pack already holds every other template to.

## Verification ladder

- Unit: template-render unit test for each config file against the real pack graph
- Integration: `ggen sync run` end-to-end producing both files
- End-to-end: `npx next build` (or at minimum `npx tsc --noEmit`) against the generated config, noting D011's cross-workstream package.json dependency if still open
- Chaos: N/A with reason — static config generation has no chaos-relevant runtime behavior
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: two-consecutive-sync byte-identity diff, plus a real `tsc`/`next build` transcript

## Receipts

- template source hash
- two-run idempotency diff
- tsc/next build transcript

## Dependencies

- TICKET-011
- TICKET-012
- D011 (TypeScript/test config cross-workstream conflict, may share root cause)

## Falsifier

If `next.config.ts` or `tsconfig.json` is generated but differs across two consecutive `ggen sync run` invocations, or contains an InterviewAssist-specific literal not derived from the graph, this ticket is not complete.

## Handoff

Feeds D011's TypeScript config resolution and D019's deployment-target work, both of which need a real build configuration to proceed.

## Definition of done

- both templates authored and wired into pack.toml
- idempotent across 2 consecutive syncs
- recognized by tsc/next tooling
