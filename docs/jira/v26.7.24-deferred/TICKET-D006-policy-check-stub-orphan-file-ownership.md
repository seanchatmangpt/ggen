# TICKET-D006: policy-check-stub.ts is an orphan file with no owning ticket in the custom-code manifest

## Status

DEFERRED — permanent structural gap in TICKET-054's manifest until D005 resolves

## Priority

P2 — bookkeeping/manifest-completeness issue; the runtime-risk half of this same file is tracked separately as D005 (P0) — this ticket is about receipt accuracy, not the runtime default-allow behavior itself

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-054-projection-receipt-and-custom-code-ownership-manifest.md`

## Source

- Research report: Disclosed Gaps Catalog, section (a) Permanent structural limitations
- Citation: "1 file, `policy-check-stub.ts`, does not appear in any of TICKET-034..039's Outputs lists as read in this session — flagged as an `orphan_files` entry rather than given an invented justification." — `docs/jira/v26.7.23/TICKET-054-projection-receipt-and-custom-code-ownership-manifest.md:150-152`
- Citation: "ORPHAN: present on disk but not accounted for by any workstream H ticket's Outputs list as read in this session." — `packs/wasm4pm-interview-assist-pack/custom-code-ownership-manifest.json:52-53` (confirmed still true at read time)

## Objective

Either retroactively assign `policy-check-stub.ts` an owning ticket in the custom-code-ownership-manifest (documenting it as an explicit, temporary placeholder with a stated removal date/condition), or remove it once D005 rewires all 5 call sites off it, whichever happens first.

## Current state

`policy-check-stub.ts` exists on disk, is imported by 5 adapters (TICKET-034/035/036/037/039), but appears in none of their Outputs lists and has no ownership-manifest entry beyond an `ORPHAN` flag.

## Target state

Either the file has a real owning ticket entry in `custom-code-ownership-manifest.json` (with justification: 'temporary placeholder pending TICKET-028, removal condition: D005 lands') until it is deleted, or it no longer exists on disk because D005 has already removed it.

## Projection classification

- Template: N/A — this is a manifest-accuracy/bookkeeping ticket
- Domain data: none
- Custom code: N/A — no new code, only manifest entries

## Inputs

- `packs/wasm4pm-interview-assist-pack/custom-code-ownership-manifest.json`
- D005's rewire status

## Outputs

- an updated `custom-code-ownership-manifest.json` entry for `policy-check-stub.ts` (interim), or its removal from the manifest once the file is deleted by D005

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

None — manifest is a receipted artifact, not a template.

## Domain-data responsibility

None.

## Custom-code boundary

None — this ticket only corrects bookkeeping, introduces no new custom code.

## Exclusions

- no invented justification pretending the stub was always intentionally scoped by one of TICKET-034..039 — the orphan status must stay honestly recorded until actually resolved

## Implementation steps

1. Check whether D005 has landed and deleted `policy-check-stub.ts`; if so, simply remove its entry from the manifest and confirm zero remaining references.
2. If D005 has not yet landed, add an explicit interim manifest entry: file, justification ('temporary default-allow placeholder pending TICKET-028'), generated-port-it-implements (none — it is itself the missing piece), and removal-condition ('D005 lands').
3. Re-run TICKET-054's completeness check (`find lib/adapters -type f` vs. manifest entries) and confirm zero unaccounted files remain.

## Admission gates

- TICKET-054

## Acceptance criteria

Given a real `find examples/interview-assist/lib/adapters -type f` listing, when compared against `custom-code-ownership-manifest.json`, then every file — including `policy-check-stub.ts` if still present — has either a real entry or is absent because it was actually deleted, with no `ORPHAN` flags remaining.

## Negative tests

Add any new file to `lib/adapters/` without a manifest entry and confirm TICKET-054's own completeness check still catches it (this ticket must not weaken that check while fixing the one known orphan).

## Verification ladder

- Unit: manifest-completeness check against a real file listing
- Integration: N/A with reason — aggregation/reporting only
- End-to-end: N/A with reason
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: manifest-completeness check result, zero orphans

## Receipts

- updated custom-code-ownership-manifest.json

## Dependencies

- TICKET-054
- D005

## Falsifier

If `custom-code-ownership-manifest.json` still shows an unexplained ORPHAN entry after this ticket claims done, or if a fabricated justification is added instead of the honest interim/removal framing, this ticket is not complete.

## Handoff

Resolves automatically once D005 deletes the file; until then, keeps the manifest honest.

## Definition of done

- manifest entry added or file removal reflected
- completeness check passes with zero true orphans
