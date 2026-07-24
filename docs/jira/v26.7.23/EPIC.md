# EPIC: InterviewAssist v26.7.23

## Status

PLANNED

## Product objective

Manufacture InterviewAssist — a Next.js coding-interview practice/assessment application — as a
**projection of admitted RDF**, not as hand-written application code. The PRD/ARD's domain
meaning (phases, capabilities, policies, event families, requirements, acceptance criteria) is
already admitted as public-ontology Turtle. This epic converts that admitted graph into running
software through SPARQL selection → ggen templates → projected canonical source → thin custom
runtime code → integration → verification → receipts.

## Admitted source graphs

`packs/wasm4pm-interview-assist-pack/`:

- `ontology/00-document.ttl` … `80-acceptance.ttl` (9 files, 1,789 triples union, verified parsed)
- `shapes/interview-assist.shacl.ttl` (14 SHACL NodeShapes, verified `CONFORMS: True` via pyshacl)
- Public-vocabulary audit: **PASS, 0 violations** (14-prefix whitelist: rdf, rdfs, xsd, dcterms,
  dcat, prov, skos, sh, odrl, schema, foaf, doap, spdx, hydra)
- Full PRD (18 sections) + ARD (14 sections + Decisive Product Choice) coverage confirmed —
  every section has ≥1 linked resource, none at zero.

This graph is the **only** place InterviewAssist domain meaning is allowed to live. No ticket in
this backlog may introduce a second, competing definition of a phase, capability, policy, or
requirement.

## Architectural boundaries

1. **Domain data** (RDF/Turtle, already admitted) — phases, capabilities, problem types,
   policies, authority decisions, accessibility profiles, workflow definitions, requirement
   statements, acceptance criteria. Lives exclusively in `packs/wasm4pm-interview-assist-pack/ontology/*.ttl`.
2. **Reusable templates** (Tera + SPARQL, workstreams B, C, D, E, F, G) — structural projection
   logic only. A template queries the graph and projects its result; it never contains an
   InterviewAssist-specific literal list, label, or capability inventory.
3. **Custom code** (workstream H) — irreducible runtime actuation only: Monaco integration,
   subprocess sandbox execution, browser storage, Ollama transport, cryptographic calls,
   Playwright/accessibility platform bindings. Every custom-code ticket implements a generated
   interface/port and documents a future reduction path; none may independently redefine a
   domain rule already present in RDF.

## Target projection ratio — computed, not asserted

The epic targets "template-projected implementation over custom implementation," interpreted per
the user's own correction: **RDF domain data is neither template nor custom code — it is admitted
input.** The ratio below is the mean of each ticket's own `## Projection classification` section,
computed directly from the 57 ticket files, not hand-set to 80/20:

```text
Aggregate template-projected %:  73.7%
Aggregate custom %:              26.3%
```

This is honestly below the 80/20 heuristic named in the brief. The reason is visible in the
per-workstream breakdown (see README.md's ticket table): the 14 vertical Chicago-TDD scenarios
(workstream I) are classified 60/40 because verification-ticket work is mostly test *authoring*
composed with real collaborators, which — per the brief's own guidance — is not counted as pure
template projection even though it duplicates no domain data. Workstream H (thin custom
boundaries) is honestly 20/80 by design; these are the irreducible adapters. If workstream I were
excluded from the average (since test-authoring ratio is a different kind of quantity than
generation-ticket ratio), the remaining 43 generation/architecture tickets average **86.7%
template / 13.3% custom** — closer to the target. Both numbers are reported in README.md; neither
is hidden to make the headline figure look better.

## Definition of template code

Reusable structural projection logic: Tera templates, SPARQL `.rq` queries, generated TypeScript
types/interfaces/dispatch tables/reducer skeletons/component scaffolds, generated tests,
generated manifests. A template's *shape* is authored once; its *content* comes entirely from
what SPARQL selects out of the admitted graph.

## Definition of custom code

Irreducible runtime actuation that cannot be projected: real subprocess execution, browser APIs,
Monaco editor runtime, Ollama HTTP transport, cryptographic library calls, process
timeout/cancellation, platform accessibility APIs. Every instance in this backlog (workstream H,
tickets 034-039) implements a generated port and states why it cannot be generated further.

## Domain-data exclusion from templates — the single most load-bearing rule in this backlog

No template may contain an InterviewAssist-specific literal array. The clearest example:
TICKET-026 generates the 98-member `CapabilityId` union type by querying `capability/*` resources
at generation time — the count is **re-derived from a live SPARQL query**, never hardcoded. Every
generation ticket's `## Falsifier` section states the specific test that would catch a template
that absorbed domain data to inflate its apparent generation percentage.

## Major workstreams

| Workstream | Tickets | Purpose |
|---|---|---|
| A. Ontology consumption | 001-005 | Admit, hash, validate, catalog queries against the graph |
| B. Pack architecture | 006-010 | Resolve the 9-file → single-`ontology.ttl` gap, gates, idempotent sync |
| C. Next.js application shell | 011-015 | Generated package/config/routes/shell |
| D. Canonical application state | 016-020 | Generated TypeScript domain types |
| E. Workflow and reducer projection | 021-025 | Generated transition table, routing, reducer, replay |
| F. Capability registry and API projection | 026-029 | Generated capability types, dispatch, policy checks |
| G. UI projection | 030-033 | Generated component scaffolds, domain-driven labels only |
| H. Thin custom boundaries | 034-039 | Monaco, subprocess, persistence, Ollama, crypto, Playwright/a11y adapters |
| I. Verification / Chicago TDD | 040-053 | 14 vertical real-collaborator scenarios |
| J. Receipt and replay | 054-057 | Projection receipt, custom-code manifest, final receipt, verifier report |

## Dependency graph (workstream level)

```text
A (001-005) ──► B (006-010) ──┬──► C (011-015) ──► D (016-020) ──┬──► E (021-025) ─┐
                               │                                  ├──► F (026-029) ─┤
                               └──────────────────────────────────┘                 │
                                                                                     ▼
                                                          G (030-033) ◄─────── E+F outputs
                                                                │
                                                                ▼
                                          H (034-039, parallel-eligible once G's ports exist)
                                                                │
                                                                ▼
                                     I (040-053, sequential vertical scenarios, 053 is terminal)
                                                                │
                                                                ▼
                                                     J (054-057, receipts + final verifier report)
```

Full per-ticket dependencies are in each ticket's `## Dependencies` section (ticket IDs), not
restated here at line-level granularity.

## Decisive acceptance test

TICKET-053 composes all 14 vertical scenarios (040-052) into one run reproducing the 10
`acceptance-step/*` resources from `80-acceptance.ttl`'s `<acceptance-test-scheme>`, in their
documented `dcterms:requires` order: build → create sandbox → replay → reproduce transitions →
reproduce executions → reproduce test outcomes → reproduce accessibility projections → verify no
prohibited capability invoked → reproduce final session state → match the final receipt hash.
TICKET-053 is the **only** ticket in this backlog authorized to conclude the system is ALIVE.

## Completion state criteria

The epic is complete when TICKET-057's Final Verifier Report states ALIVE, which is permitted
**only** if it cites TICKET-053's passing decisive-acceptance-test result directly — not test
coverage, not "the code compiles," not "most tickets are done." Any ticket short of this remains
PARTIAL_ALIVE, BLOCKED, or UNKNOWN in the report, listed explicitly, never averaged away.

## Rollback and refusal rules

- Refusal is a first-class outcome (Architecture Decision 13, TICKET-017's `AdmissionResult`
  type) — a capability dispatch that cannot be admitted returns a typed refusal, never a thrown
  exception that discards the reason.
- Every actuation is authorized and receipted (Architecture Decision 11) — TICKET-028's
  precondition/policy checks run before every dispatch; TICKET-055 emits a receipt at every
  manufacturing-chain step, including failure paths.
- Persisted state is independently revalidated on replay, never trusted (Architecture Decision
  12) — TICKET-025/048/049/056 recompute rather than read a cached "final" flag.
- No `git reset --hard` or destructive rollback of committed work; per this repo's own
  `CLAUDE.md`, fix issues forward with new commits.

## Expected final receipt

TICKET-057's `FINAL-VERIFIER-REPORT.md`, citing: TICKET-001's corpus hash, TICKET-054's
projection receipt and custom-code ownership manifest, TICKET-056's live-vs-replayed final-hash
match, and TICKET-053's full decisive-acceptance-test transcript (all 10 acceptance steps,
including the acceptance-step/10 hash match and the acceptance-step/8 zero-prohibited-capability
confirmation).

## Architecture decisions carried through this backlog

1. The PRD/ARD RDF graph is the source of domain truth.
2. Public ontology terms are used; no private vocabulary is introduced (verified: 0 violations).
3. ggen projects canonical application source.
4. **Projected code is canonical first-class source.**
5. No `generated/` directory is used anywhere in this backlog's outputs.
6. Templates contain reusable structure, not InterviewAssist domain — enforced per-ticket via
   explicit `## Exclusions` and `## Falsifier` sections.
7. Runtime custom code implements generated ports (workstream H).
8. The live runtime does not require an LLM (self-play, workstream H's Ollama adapter, runs
   outside the live critical path — see TICKET-037, TICKET-052).
9. Self-play may use local models but admission depends on executable evidence, never the model's
   self-report (TICKET-052's core negative test).
10. Undisclosed interview assistance is prohibited (TICKET-028's policy check, TICKET-053's
    acceptance-step/8).
11. Every actuation is authorized and receipted.
12. Persisted state is independently revalidated on replay (TICKET-049, TICKET-056).
13. Refusal is a first-class outcome.
14. Accessibility is a projection of the same admitted state, not a parallel application
    (TICKET-033/039/050 all project from the same 16 `capability/accessibility/*` resources the
    rest of the system uses, not a separate accessibility-only data model).
15. The final architecture is not ALIVE until TICKET-053's decisive acceptance test reproduces
    the final receipt hash.

## What this epic does not do

No implementation begins under this epic's own scope — writing the 57 tickets to disk is this
epic's only completed action. Each ticket remains `PLANNED`/`UNKNOWN` until separately picked up
and closed with a real receipt.
