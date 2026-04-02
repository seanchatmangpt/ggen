# MCP, A2A, and Enterprise Delivery (Diataxis)

This documentation bundle explains how to use `ggen` for enterprise delivery using:
- MCP as the execution interface for ggen capabilities
- A2A as multi-agent orchestration for role-based workflows

It is anchored in a synthetic Fortune 500-style case study: **Meridian Global Industries (MGI)**.

All scenarios are illustrative and non-production examples.

## Why this exists

`ggen` was built to complete these jobs:
- transform ontology + SPARQL + templates into deterministic artifacts
- validate quality gates before merge
- expose the same operations to automation
- support coordinated multi-agent workflows without forking core logic

This bundle organizes those jobs using the Diataxis framework from [`docs/src/DATAXIS_GUIDE.md`](../src/DATAXIS_GUIDE.md).

## Diataxis map

- Tutorials (learn by doing):
  - [`tutorials/01-first-end-to-end-with-mcp.md`](tutorials/01-first-end-to-end-with-mcp.md)
  - [`tutorials/02-introduce-a2a-handoff.md`](tutorials/02-introduce-a2a-handoff.md)
- How-to guides (solve specific problems):
  - [`how-to/run-quality-gates-before-merge.md`](how-to/run-quality-gates-before-merge.md)
  - [`how-to/use-mcp-for-ci-gates.md`](how-to/use-mcp-for-ci-gates.md)
  - [`how-to/coordinate-multi-agent-with-a2a.md`](how-to/coordinate-multi-agent-with-a2a.md)
  - [`how-to/resolve-ontology-import-cycles.md`](how-to/resolve-ontology-import-cycles.md)
- Explanation (understand why):
  - [`explanation/jtbds-ggen-mcp-a2a.md`](explanation/jtbds-ggen-mcp-a2a.md)
  - [`explanation/case-study-meridian-global-industries.md`](explanation/case-study-meridian-global-industries.md)
  - [`explanation/observability-and-trust.md`](explanation/observability-and-trust.md)
- Reference (look things up):
  - [`reference/glossary.md`](reference/glossary.md)
  - [`reference/mcp-tool-intents.md`](reference/mcp-tool-intents.md)

## Start here

- I am learning from scratch:
  - Start with [`tutorials/01-first-end-to-end-with-mcp.md`](tutorials/01-first-end-to-end-with-mcp.md)
- I have a delivery problem now:
  - Start with [`how-to/run-quality-gates-before-merge.md`](how-to/run-quality-gates-before-merge.md)
- I need terms and mapping quickly:
  - Start with [`reference/glossary.md`](reference/glossary.md)
- I need architecture rationale:
  - Start with [`explanation/jtbds-ggen-mcp-a2a.md`](explanation/jtbds-ggen-mcp-a2a.md)

## Related docs

- [`docs/mcp-a2a-best-practices-summary.md`](../mcp-a2a-best-practices-summary.md)
- [`docs/multi-agent-a2a-best-practices.md`](../multi-agent-a2a-best-practices.md)
- [`docs/mcp-usage.md`](../mcp-usage.md)
- [`crates/ggen-a2a-mcp/src/ggen_server.rs`](../../crates/ggen-a2a-mcp/src/ggen_server.rs)
