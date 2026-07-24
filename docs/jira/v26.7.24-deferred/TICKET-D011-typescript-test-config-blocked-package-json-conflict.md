# TICKET-D011: TypeScript/test config generation is BLOCKED on a cross-workstream package.json conflict

## Status

DEFERRED — environment-dependent, blocked on reconciling workstream C's generated package.json against workstream H's hand-authored one

## Priority

P1 — full type-checking (tsc --noEmit) doesn't succeed end-to-end yet; blocks a clean CI type-check gate (feeds D018)

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-014-typescript-and-test-config-generation.md`

## Source

- Research report: Disclosed Gaps Catalog, section (b) Environment-dependent gaps
- Citation: "Root cause of the missing deps, reported not hidden: `examples/interview-assist/package.json` currently belongs to a different, concurrently-running workstream (workstream H... hand-authored with a *different* dependency set... This session's own pack template... would generate a different, Next.js-shaped `package.json`, but overwriting workstream H's file was out of scope... left untouched." — `docs/jira/v26.7.23/TICKET-014-typescript-and-test-config-generation.md:130-141`
- Citation: "PARTIAL_ALIVE: the three config files are real... full `npx tsc --noEmit` success is BLOCKED on a cross-workstream `package.json`/dependency conflict outside this ticket's scope, not on anything wrong with the config files themselves." — `docs/jira/v26.7.23/TICKET-014-typescript-and-test-config-generation.md:130-141`

## Objective

Reconcile the two competing `package.json` sources — TICKET-011's pack-generated Next.js-shaped manifest and workstream H's hand-authored one with a different dependency set — into a single authoritative file, then re-verify `npx tsc --noEmit` succeeds.

## Current state

`examples/interview-assist/package.json` belongs to workstream H's hand-authored version; TICKET-011's own template would generate a different one. TICKET-014's three config files (tsconfig/test/Playwright config) are real, but full `tsc --noEmit` success is blocked purely by this unresolved dependency-set conflict, not by anything wrong with the config files.

## Target state

A single, reconciled `package.json` exists (either the generated one wins with workstream H's necessary hand-added deps merged in, or vice versa with the generated deps merged in), and `npx tsc --noEmit` succeeds with zero errors.

## Projection classification

- Template: N/A — this is a reconciliation ticket between two already-produced artifacts
- Domain data: none
- Custom code: N/A — reconciliation work, not new logic

## Inputs

- TICKET-011's generated `package.json` template output
- workstream H's hand-authored `package.json` currently on disk

## Outputs

- a single reconciled `package.json`
- a passing `npx tsc --noEmit` transcript

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

TICKET-011's template remains the source of truth for the Next.js-shaped baseline; any workstream H additions should be merged as explicit, documented additions rather than silently overwriting the generated baseline going forward.

## Domain-data responsibility

None — dependency management is not domain meaning.

## Custom-code boundary

None.

## Exclusions

- no silent overwrite of either file without documenting which dependencies from the losing file were preserved and why
- no claiming tsc success without a captured real transcript

## Implementation steps

1. Diff TICKET-011's generated `package.json` against workstream H's hand-authored one to enumerate every dependency-set difference.
2. Decide and document the reconciliation strategy (generated-wins-plus-merge vs. hand-authored-wins-plus-merge) with an explicit rationale.
3. Produce the single reconciled file.
4. Run `npx tsc --noEmit` and capture the transcript; fix any remaining real type errors surfaced (not merely errors caused by the now-resolved dependency conflict).

## Admission gates

- TICKET-011
- TICKET-014
- workstream H's package.json owner (concurrent workflow)

## Acceptance criteria

Given the reconciled `package.json`, when `npx tsc --noEmit` runs, then it exits 0 with no missing-dependency-caused errors.

## Negative tests

Revert to the pre-reconciliation `package.json` and confirm `tsc --noEmit` fails with the same missing-dependency errors TICKET-014 originally reported, proving the reconciliation is the actual fix, not a coincidental pass.

## Verification ladder

- Unit: N/A with reason — this is a config reconciliation ticket
- Integration: `npx tsc --noEmit` against the full reconciled project
- End-to-end: N/A with reason — covered by D008/D009's browser-level verification instead
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: captured tsc transcript, before/after diff

## Receipts

- reconciled package.json
- tsc --noEmit transcript

## Dependencies

- TICKET-011
- TICKET-014

## Falsifier

If `npx tsc --noEmit` still fails due to a missing/conflicting dependency after this ticket claims done, it is not complete.

## Handoff

Feeds D004 (next.config.ts/tsconfig.json completeness) and D018 (CI/CD wiring), both of which need a clean, reconciled build baseline.

## Definition of done

- single reconciled package.json exists
- tsc --noEmit passes with a captured transcript
- reconciliation rationale documented
