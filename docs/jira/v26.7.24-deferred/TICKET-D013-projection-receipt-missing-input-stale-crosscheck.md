# TICKET-D013: Projection receipt is missing TICKET-010's output_manifest input and its workstream-H cross-check is stale

## Status

DEFERRED — environment-dependent, self-resolving once TICKET-010 lands and the manifest is regenerated

## Priority

P2 — receipt-freshness/bookkeeping gap; self-resolving once TICKET-010 lands and the manifest is regenerated against current ticket statuses

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-054-projection-receipt-and-custom-code-ownership-manifest.md`

## Source

- Research report: Disclosed Gaps Catalog, section (b) Environment-dependent gaps
- Citation: "TICKET-010's `projection-manifest.json` (a listed Input) was searched for and NOT found on disk... the `output_manifest` field is honestly `null` rather than fabricated, and `missing_inputs` documents this. This is why the ticket is PARTIAL_ALIVE, not ALIVE: TICKET-054 depends on TICKET-010, which has not landed." — `docs/jira/v26.7.23/TICKET-054-projection-receipt-and-custom-code-ownership-manifest.md:139-143`
- Citation: "Cross-checked TICKET-034..039's own Status headers: all 6 are still `PLANNED` in the tracker despite their output files already existing on disk." — `docs/jira/v26.7.23/TICKET-054-projection-receipt-and-custom-code-ownership-manifest.md:153-154`
- Citation: "This is no longer accurate as of this read: TICKET-034/036/039 now show `PARTIAL_ALIVE` and TICKET-035/037/038 now show `ALIVE` at their live Status headers... TICKET-054 and its `custom-code-ownership-manifest.json`/`projection-receipt.json` artifacts have not been regenerated to reflect that progress — both files self-label as a snapshot (`\"status\": \"SNAPSHOT -- INCOMPLETE...\"`, `custom-code-ownership-manifest.json:61`)." — Disclosed Gaps Catalog, section (b)

## Objective

Regenerate `projection-receipt.json` once TICKET-010's `projection-manifest.json` exists (filling the currently-null `output_manifest` field), and refresh the workstream-H status cross-check to match the six tickets' current live Status headers instead of the stale PLANNED snapshot.

## Current state

`projection-receipt.json`'s `output_manifest` field is honestly `null` because TICKET-010's `projection-manifest.json` was not found on disk at TICKET-054's authoring time. Separately, TICKET-054's workstream-H cross-check text says all 6 of TICKET-034-039 were still `PLANNED`, which is now stale — several have since progressed to `PARTIAL_ALIVE`/`ALIVE`.

## Target state

`projection-receipt.json`'s `output_manifest` field is populated from a real, on-disk `projection-manifest.json`, and the workstream-H cross-check reflects each ticket's current live Status header, not a stale snapshot.

## Projection classification

- Template: N/A — regenerating an existing receipt format, not authoring new structure
- Domain data: none
- Custom code: N/A

## Inputs

- TICKET-010's `projection-manifest.json` (once it exists)
- TICKET-034..039's current live Status headers

## Outputs

- a regenerated `packs/wasm4pm-interview-assist-pack/projection-receipt.json` with `output_manifest` populated
- a regenerated `custom-code-ownership-manifest.json` with the workstream-H status cross-check refreshed and the `SNAPSHOT -- INCOMPLETE` marker updated or removed if warranted

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

Reuses TICKET-054's existing manifest-generation logic; this ticket only re-triggers it once inputs are fresh.

## Domain-data responsibility

None.

## Custom-code boundary

None — reuse of ggen-engine's existing receipt machinery, per TICKET-054's own classification.

## Exclusions

- no fabricated `output_manifest` value in place of the real TICKET-010 output
- no stale status text left uncorrected once the real current statuses are known

## Implementation steps

1. Confirm TICKET-010's `projection-manifest.json` now exists on disk (per D012's completion).
2. Re-run TICKET-054's aggregation script to populate `output_manifest` from the real file.
3. Re-read TICKET-034..039's current Status headers directly and update the cross-check text accordingly.
4. Update or remove the `SNAPSHOT -- INCOMPLETE` status marker based on whether all admission gates (workstream H tickets complete) are now actually met.

## Admission gates

- TICKET-010
- TICKET-054
- D006 (orphan-file resolution, since the manifest also carries that entry)

## Acceptance criteria

Given TICKET-010's real `projection-manifest.json` and the current live Status headers of TICKET-034..039, when `projection-receipt.json`/`custom-code-ownership-manifest.json` are regenerated, then `output_manifest` is non-null and the status cross-check matches reality at regeneration time.

## Negative tests

Compare the regenerated manifest's workstream-H status list against a fresh direct read of TICKET-034..039's Status headers; any mismatch is a failure of this ticket's freshness guarantee.

## Verification ladder

- Unit: manifest-field-population check against real TICKET-010 output
- Integration: N/A with reason — aggregation/reporting only
- End-to-end: N/A with reason
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: regenerated manifest diffed against the prior stale snapshot

## Receipts

- regenerated projection-receipt.json
- regenerated custom-code-ownership-manifest.json

## Dependencies

- TICKET-010
- TICKET-054
- D012

## Falsifier

If `output_manifest` remains null after TICKET-010's manifest exists, or if the workstream-H status cross-check still lists a ticket as PLANNED after it has demonstrably progressed, this ticket is not complete.

## Handoff

Feeds TICKET-056/057's final receipt/verifier report, which fold this manifest in.

## Definition of done

- output_manifest populated from real TICKET-010 data
- workstream-H status cross-check refreshed
- SNAPSHOT-INCOMPLETE marker updated appropriately
