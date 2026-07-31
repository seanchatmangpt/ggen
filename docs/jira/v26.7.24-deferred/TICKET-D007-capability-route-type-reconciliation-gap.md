# TICKET-D007: Capability/route projection has an unreconciled 7-vs-9 count discrepancy and refusal.ts is not wired into CapabilityResponse

## Status

DEFERRED — permanent structural limitation until TICKET-013/TICKET-017 land and are reconciled

## Priority

P1 — type-safety gap: refusal reasons are unconstrained strings rather than a closed enum, weakening the 'refusal is a first-class outcome' guarantee (Architecture Decision 13) at the type level; also an unresolved route-count discrepancy against TICKET-013

## Parent

EPIC: InterviewAssist v26.7.24-deferred
Relates to: `docs/jira/v26.7.23/TICKET-026-capability-identifier-and-request-response-types.md, TICKET-027-http-route-dispatch-projection.md`

## Source

- Research report: Disclosed Gaps Catalog, section (a) Permanent structural limitations
- Citation: "Why PARTIAL_ALIVE not ALIVE: the 7-vs-9 discrepancy against TICKET-013 needs reconciliation (out of this round's scope — TICKET-013 is not one of the assigned tickets), and the `tsc` negative-test transcript is not yet captured." — `docs/jira/v26.7.23/TICKET-027-http-route-dispatch-projection.md:147-148`
- Citation: "`refusal.ts` (TICKET-017) does **not** exist in `examples/interview-assist/lib/domain/`... `CapabilityResponse.refusal` therefore uses a plain `string` field rather than importing TICKET-017's `RefusalCode` enum." — `docs/jira/v26.7.23/TICKET-026-capability-identifier-and-request-response-types.md:146-149`

## Objective

Reconcile TICKET-027's projected route count against TICKET-013's expected count, and rewire TICKET-026's `CapabilityResponse.refusal` field from a plain `string` to TICKET-017's `RefusalCode` enum once `refusal.ts` exists.

## Current state

TICKET-027 is PARTIAL_ALIVE with an open 7-vs-9 route-count discrepancy against TICKET-013 that was out of scope for the session that authored it. TICKET-026's `CapabilityResponse.refusal` field is a plain `string` because TICKET-017's `refusal.ts` does not yet exist in `lib/domain/`.

## Target state

TICKET-027's route count matches TICKET-013's expected count with the discrepancy explicitly explained (not just closed by re-running until numbers match), and `CapabilityResponse.refusal` imports and uses TICKET-017's real `RefusalCode` enum, making refusal reasons a closed, type-checked set rather than an arbitrary string.

## Projection classification

- Template: 85% — matches TICKET-026's own classification; the enum-wiring change is a template-output correction, not new custom logic
- Domain data: `RefusalCode` values are already admitted RDF (refusal taxonomy), consumed once `refusal.ts` exists
- Custom code: 15% — type import wiring only

## Inputs

- TICKET-013's expected route count and derivation
- TICKET-017's `refusal.ts` (once it lands)
- TICKET-026's `CapabilityResponse` type
- TICKET-027's route dispatch table

## Outputs

- TICKET-027's route dispatch table reconciled to TICKET-013's count with the discrepancy documented
- TICKET-026's `CapabilityResponse.refusal` field retyped to `RefusalCode`
- a captured `tsc` negative-test transcript for TICKET-027

Projected/authored files, once this ticket is picked up, are canonical source and must not be placed under `generated/`.

## Template responsibility

Both fixes are corrections to already-generated template output, re-deriving from the same SPARQL sources, not hand-patching literals.

## Domain-data responsibility

None new — both the route count and the refusal taxonomy are already fully specified in the admitted graph; this ticket makes the generated TypeScript actually agree with it.

## Custom-code boundary

None — this is template/type correction work.

## Exclusions

- no hand-patched literal route count that isn't re-derived from SPARQL
- no fallback `string` type retained 'just in case' once `RefusalCode` exists — the field must be fully retyped

## Implementation steps

1. Read TICKET-013's route-generation ticket to determine the authoritative expected route count and how it was derived.
2. Re-run TICKET-027's SPARQL query and dispatch-table generation, comparing actual vs. TICKET-013's expected count; document the root cause of any remaining discrepancy (e.g. a capability with multiple HTTP verbs counted once vs. twice).
3. Once TICKET-017's `refusal.ts` exists, rewire `CapabilityResponse.refusal` in TICKET-026's generated types to import `RefusalCode` instead of using `string`.
4. Run `npx tsc --noEmit` and capture the transcript as TICKET-027's still-missing negative-test evidence.

## Admission gates

- TICKET-013
- TICKET-017
- TICKET-026
- TICKET-027

## Acceptance criteria

Given TICKET-013's expected route count, when TICKET-027's dispatch table is generated, then the counts match or the discrepancy is explicitly explained in the ticket text; given TICKET-017's `RefusalCode` enum, when `CapabilityResponse.refusal` is type-checked, then it accepts only enum members, not arbitrary strings.

## Negative tests

Attempt to assign an arbitrary string not in `RefusalCode` to `CapabilityResponse.refusal` and confirm `tsc` rejects it (proving the retype is real, not cosmetic).

## Verification ladder

- Unit: type-check test asserting `CapabilityResponse.refusal` rejects non-enum strings
- Integration: route-count reconciliation compared against TICKET-013's real query output
- End-to-end: N/A with reason — this is a type/count correctness ticket, not an application-behavior ticket
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: captured `tsc --noEmit` transcript, route-count reconciliation note

## Receipts

- tsc transcript
- route-count reconciliation documentation

## Dependencies

- TICKET-013
- TICKET-017
- TICKET-026
- TICKET-027

## Falsifier

If `CapabilityResponse.refusal` still accepts an arbitrary string after `refusal.ts` exists, or if the route-count discrepancy is closed by silently changing TICKET-013's expected number rather than explaining the actual cause, this ticket is not complete.

## Handoff

None further — this closes a loose end between workstreams D/F.

## Definition of done

- route count reconciled or discrepancy explained
- CapabilityResponse.refusal retyped to RefusalCode
- tsc transcript captured
