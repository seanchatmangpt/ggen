# ggen Documentation Index

Specification-driven code generation from RDF ontologies. Formula: A = μ(O) — code precipitates
from RDF via `ggen-engine`'s five-stage pipeline (Resolve → Enrich → Extract → Render → Write).

## Quick Navigation

| Need | Go to |
|------|-------|
| Get started fast | [tutorials/01-getting-started.md](tutorials/01-getting-started.md) |
| Architecture overview | `CLAUDE.md` / `.claude/rules/architecture.md` (repo root — actively maintained; the old `docs/architecture/COMPRESSED_REFERENCE.md` is archived, see below) |
| Current crate map | [reference/workspace/crates.md](reference/workspace/crates.md) |
| `sync` command reference | [reference/ggen_sync_manual.md](reference/ggen_sync_manual.md) |
| Agent implementation guides | [agent/README.md](agent/README.md) |
| Marketplace / packs | [marketplace/ARCHITECTURE.md](marketplace/ARCHITECTURE.md) |
| Interop contracts | [archive/2026_docs_pre_ggen_engine/interop/00_INDEX.md](archive/2026_docs_pre_ggen_engine/interop/00_INDEX.md) *(archived — pre-migration content)* |

---

## Directory Map

This map now covers all 46 top-level directories under `docs/` (expanded 2026-07-18; an earlier
version covered only ~14, with no disclosure it was a curated subset). Directories are grouped by
how current/actionable they are, not alphabetically — see the note after each group.

```
docs/
├── INDEX.md                         ← you are here
├── GETTING_STARTED.md
│
├── architecture/                    ← C4, LSP-ARD-PRD (COMPRESSED_REFERENCE.md and
│   │                                   C4_GGEN_PAAS_ARCHITECTURE.md archived — stale crate
│   │                                   topology, see docs/archive/2026_docs_pre_ggen_engine/)
│   ├── LSP-ARD-PRD.md
│   └── ...
│
├── reference/
│   ├── workspace/crates.md          ← the real, current crate map (17 members)
│   ├── ggen_sync_manual.md          ← `ggen sync` command reference
│   └── commands/                    ← per-noun CLI reference
│
├── agent/                           ← agent implementation guides
│   ├── README.md
│   ├── rdf-sparql-guide.md
│   ├── template-system.md
│   └── testing-guide.md
│
├── tutorials/                       ← learning paths (start with 01-)
│   ├── 01-getting-started.md        ← START HERE (beginner)
│   ├── 02-first-project.md
│   └── ...
│
├── diataxis/                        ← Diataxis-structured reference
│   ├── tutorials/
│   ├── how-to/
│   ├── explanation/
│   └── reference/
│
├── aps/                              ← machine-readable claims/standing ledger (claims.toml) —
│                                        see docs/aps/README.md; cross-linked from repo README's
│                                        Known Limitations section
├── features/                        ← per-feature specs + OTEL checklists
├── marketplace/                     ← pack system docs
├── security/                        ← security policy, checklist, incident response
├── testing/                         ← testing guides (CONTRACT_TESTING_GUIDE.md, TEST_SUITE_MANIFEST.md)
├── validation/                      ← validation gates policy
├── thesis/                          ← research & thesis materials
├── jira/v26.7.16/                   ← the ggen-core → ggen-engine migration's planning record
├── superpowers/                     ← implementation roadmaps and specs
│
├── how-to/                          ← 23 task-oriented guides (e.g. run-process-conformance.md)
├── how-to-guides/                   ← smaller, older sibling of how-to/ (e.g. use-rdf-ontologies.md)
├── explanation/                     ← 17 concept docs (e.g. oracle-gaps.md) — Diataxis "explanation" quadrant
├── explanations/                    ← 11 concept docs (e.g. rdf-for-beginners.md) — likely predates
│                                        (or duplicates) explanation/ above; not deduplicated
├── getting-started/                 ← quick-start-mcp-a2a.md — narrower sibling of GETTING_STARTED.md
├── mcp/                              ← MCP implementation docs (DELTA.md: plan vs. reality)
├── mcpp/                             ← mcpp.toml spec (Machine-Checkable Project Protocol)
├── cli/                              ← CONSTRUCT_COMMANDS.md (LLM-construct CLI commands)
├── api/                              ← rate-limiting.md and API-surface notes
├── adr/                              ← Architecture Decision Records (e.g. ADR-002-Firestore)
├── performance/                      ← README.md hub + performance docs
├── performance-dashboard/            ← index.html static dashboard (not markdown)
├── observability/                    ← OTEL_DEFINITION_OF_DONE.json (not markdown)
├── metrics/                          ← Kaizen metrics deliverables
├── dflss/                            ← Design-for-Lean-Six-Sigma validation docs
├── automation/                       ← process-automation Definition of Done
├── dx/                               ← developer-experience notes (`just` as entry point)
├── audits/                           ← ACCEPTED_ADVISORIES.md and other audit records
├── research/                         ← grounded research notes (post-chatman AGI, etc.)
├── open-ontologies/                  ← CPMP/OpenOntologies governance-mesh integration plan
├── registry/                         ← index.json (marketplace registry snapshot, not markdown)
├── gall/                             ← "W8" contradiction-supersession and related process notes
├── swarm/                            ← capability-map test plan for agent-swarm work
├── rust_swarm_doc_plan/              ← RFC: next-gen documentation system (cargo-ggen-doc)
├── templates/                        ← doc/case-study templates (not code templates)
├── textbook/                         ← "Ontology-Native Enterprise Construction" syllabus
├── ggen-v6-thesis/                   ← earlier version's planning docs (v6.1.0-perfected-plan.md)
├── troubleshooting/                  ← TROUBLESHOOTING_GUIDE.md
├── ark-covenant/                     ← historical release receipts (e.g. v26.5.29)
├── post-chatman/                     ← Post-Chatman Phase 1 verification runbook
├── proof-cycles/                     ← JSON receipt snapshots, not markdown
│
├── archive/                         ← historical/stale content, never deleted (fix-forward doctrine)
│   ├── 2025_docs/
│   ├── 2026_docs_pre_ggen_engine/   ← content stale relative to the ggen-core→ggen-engine migration
│   └── legacy_structure/
└── preserved/                       ← RECOVERY.md — looks like archive-adjacent content that was
                                         never merged into archive/; flagging for the user to
                                         decide whether to fold into archive/ or delete
```

**Flagged for a maintainer decision, not indexed as current:** `explanations/` (likely a
duplicate/predecessor of `explanation/`), `ggen-v6-thesis/` (superseded planning docs, name
suggests an older version number scheme), and `preserved/` (its own content calls it a
"recovery guide," suggesting archive-adjacent material that was never actually moved into
`archive/`). None were deleted or merged in this pass — flagging only, per the fix-forward /
non-deletion doctrine.

---

## Architecture Context

`ggen-core` is fully deleted (PR #255 retired it from the default pipeline,
`2026-ggen-core-replacement` migration; PR #259 deleted the crate outright, 2026-07-17) — it no
longer exists on disk. `ggen-engine` (backed by `praxis-core`/`praxis-graphlaw`) is now the
live pipeline behind `sync`/`doctor`/`graph`/`receipt`. See
[reference/workspace/crates.md](reference/workspace/crates.md) for the current 17-crate map and
`CLAUDE.md` / `.claude/rules/architecture.md` for the actively-maintained authoritative
reference this doc defers to.

---

## Spec Source of Truth

All specifications live in `.specify/specs/*/feature.ttl`.
**Never edit generated .md files in .specify/ — edit the .ttl source.**

```bash
ggen graph validate --files .specify/specs/NNN-feature/feature.ttl  # bare `ggen validate` no longer exists
ggen sync run --dry-run   # preview
ggen sync run             # generate
```

---

*Last verified against `main` post-PR #255 (2026-07-17). See CLAUDE.md for project rules.*
