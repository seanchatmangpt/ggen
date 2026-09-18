# 02 — Ontology IRI Defect: Unexpanded `mcp:` Prefix Baked Into IRIs

Part of [00-OVERVIEW](00-OVERVIEW.md). The checked-in generated files are currently
correct; this defect fires on the **next regeneration**, so it must land before anyone
runs a full `unrdf` sync.

## Symptom

Regenerating the `pydantic-models` rule emits
`class Mcp:mcptool(BaseModel):` at `src/ostar/ontology_models.py:175` — a SyntaxError.
The generator ([fixed in the sweep](01-COMMIT-BOUNDARY.md)) is not at fault; it faithfully
renders what the ontology contains.

## Root cause (verified live 2026-08-16)

A `mcp:` CURIE was never expanded when these triples were authored — the prefix is baked
into the IRI text itself:

```text
$ grep -rn "chatmangpt.com/ontologies/o-star/mcp:" ontology/ | head -3
ontology/core/powl.nt:13:<...powl#ToolPm4pyDiscoverPowl>
  <https://chatmangpt.com/ontologies/o-star/mcp:mcpServer> "pm4py-mcp" .
ontology/core/powl.nt:14:<...>
  <https://chatmangpt.com/ontologies/o-star/mcp:toolName> "pm4py_discover_powl" .
ontology/core/powl.nt:16:<...> a <https://chatmangpt.com/ontologies/o-star/mcp:McpTool> .
```

The sweep's namespace census counted the class-IRI population across `ontology/**/*.nt`:
`https://chatmangpt.com/ontologies/o-star/mcp:McpTool` **54**, `.../o-star/McpTool` 32,
`.../protocols/mcp#McpTool` 10 (declared as a Class but nothing typed with it), plus 3
`urn:` stragglers (one in a `.bak`). So the *majority* population is the mangled form —
`unrdf.toml`'s current mcp-tools rule was pointed at it during the sweep to make Phase 4
honest, which works but enshrines the defect. Class-name derivation in templates
camel-cases the IRI local name, and `mcp:mcptool` contains a colon → invalid Python.

## Fix

1. Pick the canonical IRI. Recommendation: `https://chatmangpt.com/ontologies/protocols/
   mcp#McpTool` and `#mcpServer`/`#toolName`/... — it is the form already *declared* as a
   class in `ontology/core/o_star_protocols.nt:65-67` and matches the repo namespace
   convention (per `CLAUDE.md`, all O* ontologies under `https://chatmangpt.com/
   ontologies/...`; a bare `mcp:` mid-IRI matches nothing).
2. Rewrite all 54+ mangled triples (and the 3 `urn:` stragglers) to the canonical form.
   Use the `onto_*` MCP tools per `.claude/rules/ontology-mcp.md`: `onto_version` snapshot
   first, then load → SPARQL to enumerate → rewrite → `onto_validate` + `onto_lint`.
3. Repoint `unrdf.toml`'s mcp-tools query at the canonical IRI.
4. Regenerate **all** rules and re-run the acceptance gate below.

## Acceptance

- `grep -rn "o-star/mcp:" ontology/` → zero hits outside `.bak`/archive.
- Full regeneration, then: import sweep 466 OK / 0 FAIL;
  `from ostar.mcp_tool_registry import list_tools; len(list_tools())` ≥ 17;
  `vision_2030_e2e_proof.py` Phase 4 PASS. All fresh runs with pasted output.

## See Also

- [01-COMMIT-BOUNDARY](01-COMMIT-BOUNDARY.md) — land the generator fix first so regen
  results are attributable
- `/Users/sac/chatmangpt/ostar/.claude/rules/ontology-mcp.md` — version-before-bulk-change
  workflow this ticket must follow
