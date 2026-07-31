# TICKET-D012: Projection idempotency has only been exercised for 1 of N eventual templates

## Status

DEFERRED — environment-dependent, self-resolving as remaining templates land

## Priority

P2 — self-resolving verification-completeness gap; the originating ticket's own instructions already schedule re-verification as each template lands

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-010-projection-manifest-idempotent-sync.md`

## Source

- Research report: Disclosed Gaps Catalog, section (b) Environment-dependent gaps
- Citation: "PARTIAL_ALIVE, not ALIVE: idempotency has only been exercised for 1 of the eventual N templates this pack will carry (workstream C-G still mostly unwritten) — re-verify as each lands, per the ticket's own step 4 instruction." — `docs/jira/v26.7.23/TICKET-010-projection-manifest-idempotent-sync.md:132-133`

## Objective

Re-run the two-consecutive-sync byte-identity check against every template as workstreams C-G land, until all templates in the pack are confirmed idempotent, not just the 1 exercised at TICKET-010's original authoring time.

## Current state

TICKET-010 verified idempotency (two consecutive `ggen sync run`, byte-identical output) for exactly 1 template. Workstreams C-G were mostly unwritten at that time; this coverage has not been re-run against the fuller template set.

## Target state

Every template in `packs/wasm4pm-interview-assist-pack/templates/` has been confirmed idempotent via the same two-consecutive-sync byte-identity check, with a per-template result recorded.

## Projection classification

- Template: N/A — this is a re-verification ticket, not new template authorship
- Domain data: none
- Custom code: N/A

## Inputs

- the full set of templates in `packs/wasm4pm-interview-assist-pack/templates/` as they land
- TICKET-010's existing idempotency-check method

## Outputs

- a per-template idempotency verification log covering all templates, not just the original 1

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

None — this only re-applies TICKET-010's existing check method to more templates.

## Domain-data responsibility

None.

## Custom-code boundary

None.

## Exclusions

- no claiming full-pack idempotency based on the original single-template check alone
- no skipping a template because it 'looks similar' to an already-verified one

## Implementation steps

1. Enumerate all templates currently present in the pack.
2. For each template not yet covered by TICKET-010's original check, run `ggen sync run` twice consecutively and diff the relevant output file(s).
3. Record pass/fail per template in an idempotency verification log.
4. Repeat this ticket (or re-open it) each time a new template lands, until full coverage is reached.

## Admission gates

- TICKET-010

## Acceptance criteria

Given the current full set of templates in the pack, when each is checked via two consecutive `ggen sync run` invocations, then every one produces byte-identical output across both runs, and this is recorded per-template, not asserted in aggregate.

## Negative tests

Deliberately introduce a non-deterministic element into one template (e.g. an unsorted SPARQL result) and confirm the two-run diff check catches it, proving the check is a real gate, not a rubber stamp.

## Verification ladder

- Unit: N/A with reason — this is a re-verification sweep, not unit-level
- Integration: two-consecutive-sync diff per template
- End-to-end: N/A with reason
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: per-template idempotency log

## Receipts

- per-template idempotency verification log

## Dependencies

- TICKET-010

## Falsifier

If any template in the pack has never been through the two-consecutive-sync check when this ticket is claimed complete, it is not complete.

## Handoff

Feeds D004 (next.config.ts/tsconfig.json) and D013 (projection receipt) — both depend on a fuller idempotency picture.

## Definition of done

- idempotency log covers 100% of current templates
- log re-run as new templates land until parity with the pack's full template count
