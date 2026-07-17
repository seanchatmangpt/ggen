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
│   ├── workspace/crates.md          ← the real, current crate map (16 members)
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
├── features/                        ← per-feature specs + OTEL checklists
├── marketplace/                     ← pack system docs
├── security/                        ← security policy, checklist, incident response
├── testing/                         ← testing guides (CONTRACT_TESTING_GUIDE.md, TEST_SUITE_MANIFEST.md)
├── validation/                      ← validation gates policy
├── thesis/                          ← research & thesis materials
├── jira/v26.7.16/                   ← the ggen-core → ggen-engine migration's planning record
├── superpowers/                     ← implementation roadmaps and specs
│
└── archive/                         ← historical/stale content, never deleted (fix-forward doctrine)
    ├── 2025_docs/
    ├── 2026_docs_pre_ggen_engine/   ← content stale relative to the ggen-core→ggen-engine migration
    └── legacy_structure/
```

---

## Architecture Context

`ggen-core` was retired from the default pipeline (PR #255, `2026-ggen-core-replacement`
migration): it's excluded from the workspace, doesn't compile standalone, and has zero
in-workspace dependents. `ggen-engine` (backed by `praxis-core`/`praxis-graphlaw`) is now the
live pipeline behind `sync`/`doctor`/`graph`/`receipt`. See
[reference/workspace/crates.md](reference/workspace/crates.md) for the current 16-crate map and
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
