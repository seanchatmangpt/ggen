# ggen-lsp-mcp — audit (generation-first, 2026-07)

**Purpose:** "MCP server exposing ggen-lsp repair routes as a tool (leaf crate — avoids the ggen-core↔ggen-a2a-mcp cycle)." (`crates/ggen-lsp-mcp/Cargo.toml`).

## LOC

`tokei crates/ggen-lsp-mcp/src --output json` (tokei Rust code lines):

| File | Code LOC |
|---|---|
| src/lib.rs | 297 |
| src/main.rs | 13 |
| **Total** | **310** |

## Consolidation status

`CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` §2 (line 28) + §3 (line 37): CONSOLIDATE into `ggen-lsp` behind an `mcp` feature — "Thin adapter… single dependency (ggen-lsp), no independent logic." 2 dependents (`ggen-cli`, `ggen-lsp-a2a`). Not DEAD.

## Class breakdown

| Class | LOC (est.) | Basis |
|---|---|---|
| GENERATABLE-WITH-SPEC | ~270 (~87%) | whole-file read of lib.rs: three schemars-derived param structs (`RepairRouteParams`, `ReplayCaseParams`, `MetricsParams`), `make_tool` + tool registration (name/description/schema triples), parse/validate helpers (`parse_repair_params`, `validate_root`, `validate_case_id` — bound checks fully determined by the param spec), the three `*_result` functions that deserialize params and delegate to `ggen_lsp::{replay_case,verify_promotion,compute_metrics}`, and the `ServerHandler` impl (get_info/list_tools/call_tool dispatch). All of it is determined by a tool schema: (tool name, description, params with max-length bounds, target ggen-lsp function). main.rs (13) is stdio-server launch wiring, same class. |
| IRREDUCIBLY-CUSTOM | ~40 (~13%) | `build_repair_routes_in` (lib.rs lines 300–336): analyzer construction, seeded registry + promoted-pack loading, per-diagnostic envelope-vs-refusal loop, pack-hash provenance binding — thin but semantic orchestration over the route engine (the "never read an empty list as all-clear" refusal policy), not schema-derivable. |

## Matching template/spec

No emitting template or TTL tool-spec exists. Nearest family: the generated MCP/A2A surface in `crates/ggen-a2a-mcp/src/a2a_generated/` (the workspace's only GENERATED tree per METHODOLOGY baseline) proves the pipeline already emits protocol adapters; a `ggen-lsp-route-tools.ttl` (shared with ggen-lsp-a2a, which dispatches the identical three tools) plus an rmcp-server template would cover the ~270 LOC. The doc comment itself declares the param structs the "single source of truth" for the advertised schema — that source of truth belongs in TTL.
