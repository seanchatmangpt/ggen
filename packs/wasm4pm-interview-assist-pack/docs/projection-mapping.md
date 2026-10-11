# Public-ontology-to-projection mapping (TICKET-004)

One row per class actually instantiated in the admitted corpus (verified via `queries/*.rq` and
`gates/*.rq`, not asserted). "Not used" classes from the original 14-prefix palette are omitted
here on purpose — see the PR #489 report's audit for the full whitelist.

| Class | Instances (real count) | Target projection shape | Selecting query |
|---|---|---|---|
| `schema:Action` (capability/* subset) | 98 | `CapabilityId` union + per-capability Request/Response (TICKET-026) | `queries/capabilities.rq` |
| `schema:Action` (transition-plan/*, authority-action/*, prohibited-action/*, acceptance-step/*) | 37 | typed constants consumed by TICKET-021/028/029/053, not part of `CapabilityId` | gates/020 covers all of `schema:Action` uniformly |
| `schema:DigitalDocument` | 149 | requirement/document reference resources; not directly projected to runtime types (documentation-only) | n/a |
| `skos:Concept` (phase-scheme) | 14 | `Phase` discriminated union (TICKET-016) + transition table (TICKET-021) | `queries/phases.rq` |
| `skos:Concept` (event-family-scheme) | 15 | `EventFamily` discriminated union (TICKET-016) + routing table (TICKET-022) | `queries/event-families.rq` |
| `skos:ConceptScheme` | 5 | source of the `skos:hasTopConcept` ordering used by the two unions above | n/a (queried inline via the two queries above) |
| `odrl:Set` | 6 | policy-check function inputs (TICKET-028) | `queries/policies.rq` (6 rows, verified) |
| `odrl:Permission`/`odrl:Prohibition` | 17 (blank nodes under the 6 Sets) | permission/prohibition matcher input (TICKET-028) | `queries/policy-rules.rq` (17 rows, verified) |
| `prov:Entity`/`prov:Activity` | 11 (manufacturing-chain + receipt resources; see below) | `TransitionReceipt` type (TICKET-020) | `queries/receipts.rq` (11 rows, verified) |
| `dcat:Dataset`/`Distribution`/`DataService` | 1/9/1 | pack manifest type (out of scope for the live runtime; documentation only) | `queries/datasets.rq` (11 rows, verified) |
| `spdx:Checksum` | 3 (1 named + 2 blank) | `Checksum` interface field (TICKET-020) | `queries/receipts.rq` selects the 1 named (`receipt/checksum-algorithm-blake3`); the 2 per-receipt-entry inline checksums are blank nodes with no stable IRI, not separately selectable |
| `schema:DigitalDocument` (`req/ard-refusal-*`) | 16 refusal codes + 1 governing rule | `RefusalCode` union (TICKET-021/029) | `queries/refusal-codes.rq` (16 rows, verified; excludes the governing-rule resource by design) |
| `skos:Concept`+`schema:Action` (`acceptance-step/*`) | 10 | Decisive Acceptance Test step sequence (documentation/test-harness only) | `queries/acceptance-steps.rq` (10 rows in `dcterms:requires` order, verified) |
| `hydra:Operation` | 7 (subset of capability/* `schema:Action`) | HTTP-shaped capability subset → route generation (TICKET-013/027) | not yet written — TICKET-013 scope |

## Excluded from this mapping (confirmed not instantiated in the current corpus)

`owl:*`, `dqv`/`duv`, `sosa`/`ssn`, `qudt`/`unit`, `org`, `vann`, `c4o`/`deo`/`fabio`/`frbr`, `oa`,
`ldp`, `as`, `csvw`, `locn`, `time` — none appear as a class or predicate namespace anywhere in
`ontology.ttl` (confirmed via the public-vocabulary audit script from the PR #489 phase, re-run
against the concatenated `ontology.ttl` — same 14-prefix result, 0 violations).

## Status

Rows for TICKET-016/021/022/026 (state types, transitions, capability registry) and, as of this
session (2026-07-23, TICKET-003 continuation), the TICKET-020/028 rows (receipts, DCAT datasets,
ODRL policy/policy-rule detail) plus refusal codes and acceptance steps now all have real,
verified queries with row counts confirmed against the actual `ontology.ttl` corpus via `rdflib`
(see `queries/*.rq` header comments and TICKET-003's Implementation notes for the run evidence).
The `hydra:Operation` row (TICKET-013 scope) still says "not yet written" honestly — a
`queries/hydra-operations.rq` file exists on disk from another parallel agent's session but was
not authored or independently re-verified in this pass, so this table's TICKET-013 row is left
unchanged rather than claimed without direct verification.
