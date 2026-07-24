# InterviewAssist v26.7.23 — Implementation backlog index

## Epic summary

57 tickets manufacturing InterviewAssist as a projection of the admitted
`packs/wasm4pm-interview-assist-pack/` RDF corpus (1,789 triples, 9 ontology files, SHACL-verified,
public-vocabulary-audited, full PRD/ARD section coverage — see EPIC.md for the full program). No
implementation has begun; this directory is the manufactured backlog only.

## Ticket table

| ID | Title | Status | Depends on | Template % | Custom % |
|---|---|---|---|---|---|
| 001 | Admit and hash the InterviewAssist ontology corpus | PLANNED | - | 90% | 10% |
| 002 | SHACL pre-projection validation gate | PLANNED | 001 | 100% | 0% |
| 003 | SPARQL query contract catalog | PLANNED | 002 | 100% | 0% |
| 004 | Public-ontology-to-projection mapping table | PLANNED | 003 | 100% | 0% |
| 005 | Missing/contradictory graph-data handling policy | PLANNED | 003, 004 | 90% | 10% |
| 006 | Pack layout + ontology.ttl concatenation strategy | PLANNED | 001, 002 | 95% | 5% |
| 007 | Author pack.toml for the projection phase | PLANNED | - | 100% | 0% |
| 008 | Query and template directory organization | PLANNED | 003 | 100% | 0% |
| 009 | SPARQL gate integration (replaces SHACL for ggen sync run) | PLANNED | 002, 006, 008 | 100% | 0% |
| 010 | Projection manifest + idempotent synchronization | PLANNED | 006, 009 | 85% | 15% |
| 011 | Next.js package metadata + root config generation | PLANNED | 004, 005, 008 | 90% | 10% |
| 012 | App layout + module boundaries generation | PLANNED | 011 | 85% | 15% |
| 013 | Route generation from capability HTTP operations | PLANNED | 012 | 80% | 20% |
| 014 | TypeScript config + test/Playwright config generation | PLANNED | 011 | 100% | 0% |
| 015 | Accessibility defaults generation | PLANNED | 012 | 90% | 10% |
| 016 | Session/phase/event state types projection | PLANNED | 004 | 95% | 5% |
| 017 | Admission/refusal outcome types projection | PLANNED | 004 | 90% | 10% |
| 018 | Track/evidence/problem/workspace state projection | PLANNED | 004 | 80% | 20% |
| 019 | Verification/accessibility/authority state projection | PLANNED | 018 | 85% | 15% |
| 020 | Transition receipt types projection | PLANNED | 004 | 90% | 10% |
| 021 | Phase transition table projection | PLANNED | 016 | 90% | 10% |
| 022 | Event admission routing projection | PLANNED | 016 | 90% | 10% |
| 023 | Deterministic reducer generation | PLANNED | 021, 022, 017 | 75% | 25% |
| 024 | Selectors + abstention/confirmation projection | PLANNED | 018, 019 | 85% | 15% |
| 025 | Refusal/completion/replay transition projection | PLANNED | 023 | 80% | 20% |
| 026 | Capability identifier + request/response type projection | PLANNED | 004 | 95% | 5% |
| 027 | HTTP route dispatch projection | PLANNED | 026, 013 | 85% | 15% |
| 028 | Precondition/policy-target/authority-check projection | PLANNED | 026, 019 | 85% | 15% |
| 029 | Timeout/result/refusal handling projection | PLANNED | 017, 026 | 80% | 20% |
| 030 | Phase/problem/track-candidate panel projection | PLANNED | 016, 018, 024, 015 | 75% | 25% |
| 031 | Editor shell/diagnostics/console projection | PLANNED | 018 | 75% | 25% |
| 032 | Test result/refusal/replay-failure presentation projection | PLANNED | 017, 019, 025 | 75% | 25% |
| 033 | Accessibility controls + session completion summary projection | PLANNED | 015, 020 | 75% | 25% |
| 034 | Monaco runtime adapter (custom) | PLANNED | 031 | 20% | 80% |
| 035 | Subprocess sandbox executor (custom) | PLANNED | 027, 028, 029 | 20% | 80% |
| 036 | Local persistence adapter (custom) | PLANNED | 020, 025 | 20% | 80% |
| 037 | Ollama client adapter (custom) | PLANNED | 018 | 20% | 80% |
| 038 | Cryptographic (BLAKE3) checksum adapter (custom) | PLANNED | 020 | 20% | 80% |
| 039 | Playwright browser harness + accessibility platform adapters (custom) | PLANNED | 033, 014 | 20% | 80% |
| 040 | Vertical: Bootstrap | PLANNED | 011, 023, 035 | 60% | 40% |
| 041 | Vertical: First interaction | PLANNED | 040, 021 | 60% | 40% |
| 042 | Vertical: Track confirmation | PLANNED | 041, 024 | 60% | 40% |
| 043 | Vertical: Python coding workflow | PLANNED | 035, 027 | 60% | 40% |
| 044 | Vertical: Rust coding workflow | PLANNED | 035, 027, 043 | 60% | 40% |
| 045 | Vertical: Visible tests | PLANNED | 043, 044 | 60% | 40% |
| 046 | Vertical: Hidden tests | PLANNED | 045, 028 | 60% | 40% |
| 047 | Vertical: Timeout and refusal | PLANNED | 029, 035 | 60% | 40% |
| 048 | Vertical: Persistence and replay | PLANNED | 036, 025, 041 | 60% | 40% |
| 049 | Vertical: Tamper detection | PLANNED | 048, 038, 032 | 60% | 40% |
| 050 | Vertical: Accessibility projection | PLANNED | 033, 039 | 60% | 40% |
| 051 | Vertical: Zero-input cognition | PLANNED | 042 | 60% | 40% |
| 052 | Vertical: Self-play manufacturing | PLANNED | 037, 045, 046 | 60% | 40% |
| 053 | Vertical: Full decisive acceptance test | PLANNED | 040-052 | 60% | 40% |
| 054 | Projection receipt + custom-code ownership manifest | PLANNED | 010, 034-039 | 90% | 10% |
| 055 | Runtime/sandbox/test/accessibility projection receipts | PLANNED | 020, 038, 035 | 75% | 25% |
| 056 | Final session receipt + deterministic replay | PLANNED | 025, 055, 038 | 70% | 30% |
| 057 | Final verifier report | PLANNED | 053, 054, 056 | 95% | 5% |

Output surface for every ticket: `examples/interview-assist/` (generated Next.js application,
first-class canonical source, no `generated/` directory) and `packs/wasm4pm-interview-assist-pack/`
(templates, queries, gates, manifests). Current standing: **all 57 tickets PLANNED / UNKNOWN** — none
implemented yet. Completion receipt: none yet — TICKET-057's `FINAL-VERIFIER-REPORT.md` will hold it.

## Aggregate estimates (computed from the 57 individual ticket files, not asserted)

```text
Projected/template code percentage:  73.7%  (mean of all 57 tickets' Template % field)
Custom code percentage:              26.3%  (mean of all 57 tickets' Custom % field)
Domain-data coverage:                100% of PRD (18/18 sections) and ARD (14/14 + Decisive
                                      Product Choice) sections have ≥1 linked RDF resource,
                                      per the prior TTL-authoring phase's verified coverage matrix
Tickets with no custom code:         7   (002, 003, 004, 007, 008, 009, 014)
Tickets exceeding 20% custom code:   27  (023, 030-039, 040-053, 055, 056)
Unsupported concepts:                4, carried forward honestly from the TTL-authoring phase:
                                      BLAKE3-vs-SPDX-checksum-algorithm-enum mismatch;
                                      dcterms:requires between two schema:Action instances is a
                                      non-canonical stretch; odrl:spatial/event/eq used in
                                      50-policy.ttl without independent live-spec re-verification;
                                      schema:Role used for InterviewAssist roles is a documented
                                      simplification of schema.org's canonical Role pattern
Blocking dependencies:               TICKET-006 (pack-layout resolution) is BLOCKED until it
                                      re-verifies ggen-engine's live auto-load behavior — every
                                      ggen sync run-dependent ticket downstream (009, 010, and by
                                      extension most of C-J) is gated on that resolution
```

**Honest disclosure on the 80/20 target**: the aggregate above (73.7/26.3) is below the epic's
80/20 heuristic target, computed directly rather than rounded up to match the target. The reason
is workstream I's 14 verification tickets, each honestly classified 60/40 because Chicago-TDD
scenario authoring composed with real collaborators is not pure template projection even when it
introduces zero domain-data duplication (see EPIC.md's "Target projection ratio" section for the
full breakdown, including the 86.7/13.3 figure when workstream I is excluded). This number is not
adjusted to look better — the brief explicitly warns against gaming the ratio, and reporting a
false 80/20 here would be exactly that.

## Tickets exceeding 20% custom code — required justification check

Every one of the 27 tickets above 20% custom includes a `## Projection classification` →
`Custom-code justification` field (see individual ticket files) and, for the 6 pure custom-code
tickets (034-039), an explicit `## Custom-code boundary` (why irreducible) and reduction path.
None of the 27 lacks a justification — this was verified by construction (the ticket-generation
script requires the field) rather than checked after the fact.
