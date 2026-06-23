# ggen Documentation Index

Specification-driven code generation from RDF ontologies. Formula: A = μ(O).

## Quick Navigation

| Need | Go to |
|------|-------|
| Get started fast | [tutorials/01-getting-started.md](tutorials/01-getting-started.md) |
| MCP server generation | [mcp-rdf/README.md](mcp-rdf/README.md) |
| Architecture overview | [architecture/COMPRESSED_REFERENCE.md](architecture/COMPRESSED_REFERENCE.md) |
| Workspace health | [crate-audits/AUDIT_DASHBOARD.md](crate-audits/AUDIT_DASHBOARD.md) |
| Agent implementation guides | [agent/README.md](agent/README.md) |
| Marketplace / packs | [marketplace/ARCHITECTURE.md](marketplace/ARCHITECTURE.md) |
| OTEL validation | [observability/OTEL_DEFINITION_OF_DONE.json](observability/OTEL_DEFINITION_OF_DONE.json) |
| Thesis / research | [thesis/FUSION_THESIS.md](thesis/FUSION_THESIS.md) |
| Interop contracts | [interop/00_INDEX.md](interop/00_INDEX.md) |

---

## Directory Map

```
docs/
├── INDEX.md                         ← you are here
├── MANIFESTO.md                     ← v30.1.1 CalVer manifesto
│
├── architecture/                    ← C4, compressed reference, LSP-ARD-PRD
│   ├── COMPRESSED_REFERENCE.md      ← PRIMARY: load before modifying code
│   ├── LSP-ARD-PRD.md
│   └── ...
│
├── crate-audits/                    ← workspace health
│   └── AUDIT_DASHBOARD.md           ← 54 stubs, 8,900 LOC dead code, 4 P0 blockers
│
├── agent/                           ← 10 agent implementation guides
│   ├── README.md
│   ├── rdf-sparql-guide.md
│   ├── template-system.md
│   ├── testing-guide.md
│   └── ...
│
├── mcp-rdf/                         ← MCP server generation via RDF
│   ├── README.md
│   ├── 01-quick-start/
│   ├── 02-rdf-schema/
│   ├── 03-code-generation/
│   ├── 04-template-customization/
│   ├── 05-sparql-guide/
│   └── 06-examples/
│
├── tutorials/                       ← learning paths (start with 01-)
│   ├── 01-getting-started.md        ← START HERE (beginner)
│   ├── 02-first-project.md
│   ├── core/01-first-mcp-tool.md
│   ├── packs-getting-started.md
│   └── ...
│
├── diataxis/                        ← Diataxis-structured reference
│   ├── tutorials/
│   ├── how-to/
│   ├── explanation/
│   └── reference/
│
├── explanation/                     ← conceptual deep-dives
│   ├── rdf-for-beginners.md
│   ├── poke-yoke.md
│   └── ...
│
├── features/                        ← per-feature specs + OTEL checklists
│   ├── COMPLETENESS_MATRIX.md
│   └── ...
│
├── marketplace/                     ← pack system docs
│   ├── ARCHITECTURE.md
│   ├── CLI_REFERENCE.md
│   └── ...
│
├── interop/                         ← architecture contracts (numbered 00-26)
│   └── 00_INDEX.md
│
├── thesis/                          ← research & thesis materials
│   ├── FUSION_THESIS.md             ← executive manifesto
│   ├── phd-thesis/                  ← LaTeX PhD thesis (canonical)
│   ├── fusion-thesis/               ← LaTeX Fusion thesis
│   └── research/
│
├── performance/                     ← SLO targets, benchmarks, optimization
├── observability/                   ← OTEL definition of done
├── automation/                      ← CI/CD definition of done
├── textbook/                        ← 14-week graduate course
├── gall/                            ← evidence gates W0-W9
├── superpowers/plans/               ← implementation roadmaps
├── jira/                            ← active defect reports
│
└── archive/                         ← historical/stale content
```

---

## Crate README Status

| Crate | README | Quality |
|-------|--------|---------|
| ggen-lsp | ✅ | 184 lines — comprehensive |
| ggen-core | ✅ | 88 lines — functional |
| ggen-cli | ❌ | **MISSING** — primary user entry point |
| ggen-marketplace | ⚠️ | 2 lines — stub only |
| ggen-config | ⚠️ | 2 lines — stub only |
| ggen-a2a-mcp | ⚠️ | 2 lines — stub only |
| genesis-core-v2 | ❌ | **MISSING** |
| genesis-schema-v2 | ❌ | **MISSING** |
| genesis-types-v2 | ❌ | **MISSING** |
| cpmp | ❌ | **MISSING** |
| stpnt | ❌ | **MISSING** |
| ggen-lsp-mcp | ❌ | **MISSING** |
| ggen-lsp-a2a | ❌ | **MISSING** |

---

## Spec Source of Truth

All specifications live in `.specify/specs/*/feature.ttl`.
**Never edit generated .md files in .specify/ — edit the .ttl source.**

```bash
ggen validate .specify/specs/NNN-feature/feature.ttl
just sync-dry   # preview
just sync       # generate
```

---

*Generated 2026-06-23. See CLAUDE.md for project rules.*
