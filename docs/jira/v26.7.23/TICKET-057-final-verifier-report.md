# TICKET-057: Final verifier report

## Status

PLANNED

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/*.ttl (entire admitted corpus)`
- Acceptance-test steps:
  - `all 10 acceptance-step/* resources`
- PRD requirements: (none)
- ARD components: (none)
- Policies: (none)
- SHACL shapes: (none)

## Objective

Produce the final, single verifier report aggregating every prior ticket's receipts (TICKET-001 corpus hash through TICKET-056 final session receipt) into one document, using this backlog's own PARTIAL_ALIVE/ALIVE/BLOCKED/BUILD_BROKEN/UNKNOWN/UNSUPPORTED vocabulary, stating explicitly whether the system is ALIVE (only permissible if TICKET-053's decisive acceptance test passed with a real hash match).

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 95%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 5%
- Expected ratio: 95/5
- Custom-code justification: report aggregation is templated; 5% is the final human-reviewed judgment call on the overall ALIVE/PARTIAL_ALIVE verdict itself.

## Inputs

- every prior ticket's receipts
- TICKET-053's full decisive acceptance test result
- TICKET-054's projection + custom-code manifests

## Outputs

- docs/jira/v26.7.23/FINAL-VERIFIER-REPORT.md

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Report structure/aggregation template.

## Domain-data responsibility

None — this is a report about the system, not domain data itself.

## Custom-code boundary

NONE beyond the human verdict judgment noted above.

## Exclusions

- no ALIVE claim without TICKET-053's real, passing hash-match result cited directly
- no averaging-away of any individual ticket's BLOCKED/UNKNOWN status — every non-ALIVE ticket must be listed explicitly, not summarized into a rounded-up aggregate

## Implementation steps

1. Collect every ticket's Current-state/receipts.
2. Compute the real aggregate template/custom ratio from individual ticket percentages (mean weighted by ticket count per workstream, documented method, not an assumed flat 80/20).
3. State the overall system status using the required vocabulary, citing TICKET-053's actual result as the deciding evidence for ALIVE vs PARTIAL_ALIVE.
4. List every UNSUPPORTED/BLOCKED/UNKNOWN item across the whole backlog in one place (not scattered, so a reader doesn't have to open all 57 tickets to find them).

## Admission gates

- All 57 tickets have at least a Current-state entry (even if UNKNOWN, since none are implemented at backlog-authoring time).

## Acceptance criteria

- Given every ticket's Current-state, when the report is assembled, then the aggregate ratio is computed (not asserted) from individual values, and the overall status is never ALIVE unless TICKET-053 is cited as passing.

## Negative tests

- Attempt to mark the report ALIVE while TICKET-053 shows anything other than a passing decisive acceptance test and confirm this is flagged as a contradiction requiring correction before the report can be finalized.

## Verification ladder

- Unit: N/A with reason — aggregation/reporting ticket, not code
- Integration: N/A with reason
- End-to-end: N/A with reason
- Chaos: N/A with reason
- Stress: N/A with reason
- Benchmark: N/A with reason
- Verifier report: this IS the verifier report — self-referential terminal ticket

## Receipts

- FINAL-VERIFIER-REPORT.md
- aggregate ratio computation
- full UNSUPPORTED/BLOCKED/UNKNOWN list

## Dependencies

- TICKET-053
- TICKET-054
- TICKET-056

## Falsifier

If the report claims ALIVE without TICKET-053's passing decisive-acceptance-test result cited as direct evidence, this ticket is not complete — this is the terminal instance of the entire backlog's no-overclaiming discipline.

## Handoff

This is the terminal ticket of the epic; no further handoff.

## Definition of done

- report assembled from real per-ticket receipts
- aggregate ratio computed, not asserted
- ALIVE claim (if made) directly cites TICKET-053's passing result
- full UNSUPPORTED/BLOCKED/UNKNOWN list present
