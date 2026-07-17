# ggen-lsp Migration

Part of [00-OVERVIEW](00-OVERVIEW.md) — Phase 4, depends on
[05-MANIFEST-CONFIG-PORT](05-MANIFEST-CONFIG-PORT.md) landing first, and on
[08-GGEN-CLI-MIGRATION](08-GGEN-CLI-MIGRATION.md)'s agent-facade step (9) for the
`a2a_mcp/mcp_packs.rs` file specifically.

## File reference table (all 6 files, full paths, exact LOC)

All paths under `/Users/sac/ggen/crates/ggen-lsp/src/` unless noted. Confirmed via
`grep -rln "ggen_core::" crates/ggen-lsp/src` → exactly 6 files; `grep -rn "ggen_core::"
crates/ggen-lsp/src | wc -l` → exactly 17 hits (both re-verified this session).

| File | LOC | Blocking dependency |
|---|---:|---|
| `project_index.rs` | 302 | manifest module landing in `ggen-config` (net-new there today — see [05](05-MANIFEST-CONFIG-PORT.md)) |
| `rule_index.rs` | 400 | manifest module landing in `ggen-config` |
| `analyzers/sparql_analyzer.rs` | 349 | manifest's `validation` submodule specifically |
| `analyzers/tera_analyzer.rs` | 1,160 | none — the one hit (line 567) is a doc comment, zero functional risk |
| `a2a_mcp/mcp_packs.rs` | 417 | the native `agent` facade — cross-phase dependency on [08](08-GGEN-CLI-MIGRATION.md)'s agent-facade step |
| `a2a_mcp/mcp_server.rs` | 301 | manifest **and** a 4th distinct pipeline mechanism (`codegen::pipeline::GenerationPipeline` at lines 44-45 + `get_llm_service()` at line 80), no counterpart among ggen-cli's three — most cross-cutting file in ggen-lsp |

`/Users/sac/ggen/crates/ggen-lsp/Cargo.toml:32-35`:
```
32: # Reuse from ggen workspace
33: ggen-core = { path = "../ggen-core", version = "26.7.1" }
34: ggen-config = { path = "../ggen-config", version = "26.7.1" }
35: ggen-graph = { path = "../ggen-graph", version = "26.7.1" }
```
Confirmed: `ggen-lsp` already depends directly on both `ggen-core` (line 33) and
`ggen-config` (line 34), plus `ggen-graph` (line 35).

## Recommended order

0. **Land manifest parsing in `ggen-config` first** (see
   [05-MANIFEST-CONFIG-PORT](05-MANIFEST-CONFIG-PORT.md)) — blocking prerequisite, net-new
   there, not a ggen-lsp file.
1-2. **`project_index.rs`/`rule_index.rs`** together, trivial re-point once step 0 lands.
3. **`analyzers/sparql_analyzer.rs`**, needs the `validation` submodule specifically.
4. **`analyzers/tera_analyzer.rs`** — doc-comment fix only (line 567), any time.
5. **`a2a_mcp/mcp_packs.rs`** — gated on
   [08-GGEN-CLI-MIGRATION](08-GGEN-CLI-MIGRATION.md)'s agent-facade step (9).
6. **`a2a_mcp/mcp_server.rs` last** — needs manifest, a `codegen::pipeline` equivalent, and
   the agent contract simultaneously.

## Definition of done for this ticket

- All 17 references across the 6 files above re-pointed to `ggen_config::manifest::*` and
  the new engine's `agent`/`codegen::pipeline` equivalents.
- `GGEN-TPL-001`/`GGEN-OUT-001`/`GGEN-YIELD-001`/`GGEN-RULE-001`/`GGEN-QUERY-002`/`E0011`/
  `E0013`/`E0015` diagnostics re-verified firing correctly against the new manifest types
  (detector/orchestration files themselves —
  `/Users/sac/ggen/crates/ggen-lsp/src/route/diagnostic_species.rs` (299 lines),
  `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/mod.rs` (587 lines),
  `/Users/sac/ggen/crates/ggen-lsp/src/analyzers/{harness_analyzer.rs (175),
  source_law_analyzer.rs (143), toml_analyzer.rs (356)}` — need no changes, per
  [05-MANIFEST-CONFIG-PORT](05-MANIFEST-CONFIG-PORT.md)).
- Confirm whether `ggen-lsp`'s `ggen-core` dependency
  (`/Users/sac/ggen/crates/ggen-lsp/Cargo.toml:33`) can be dropped entirely (check for any
  remaining need beyond the 6 files above).
