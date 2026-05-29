# Playground Proof Fixtures

Each fixture here is a **foundation pier** — a minimal real authored surface used by the
Gall integration tests (`crates/ggen-lsp-a2a/tests/gall_foundation_lsp_mcp_a2a.rs`) to
prove the LSP / MCP / A2A delivery plane routes identically with no drift.

| Fixture | Pier it tests |
|---------|---------------|
| `broken-construct.rq` | An unconstrained `CONSTRUCT` (no `ORDER BY`). Under strict mode it raises diagnostic **E0011** and the route engine attaches the seeded repair route `template.values-inline`. Proves the diagnostic→route path fires identically through LSP (`ggen lsp check`), MCP (`ggen-lsp-mcp` repair_route), and the A2A bridge. Its content is byte-identical to the bridge's `E0011` test constant, so the proof composes across three independently-written surfaces. |

The clean-surface pier uses the sibling `../thesis-ontology.ttl` (valid Turtle — a
recognized law surface that must produce zero ERROR diagnostics and no repair route).

> Keep these fixtures minimal and pure. `broken-construct.rq` in particular must stay
> byte-identical to the bridge test constant — do not add comments inside it.
