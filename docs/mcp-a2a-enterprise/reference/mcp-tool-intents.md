# MCP tool intents

This table maps enterprise intents to MCP operations exposed by ggen.

Tool naming aligns with module documentation in:
- [`crates/ggen-a2a-mcp/src/ggen_server.rs`](../../crates/ggen-a2a-mcp/src/ggen_server.rs)

| Intent | Primary MCP tool(s) | Required inputs | Typical output |
|---|---|---|---|
| Generate code artifacts from ontology | `generate`, `sync` | `ontology_path`, optional `queries_dir`, `output_dir`, `language` | Files generated, elapsed time, receipt info |
| Validate raw TTL content | `validate` | `ttl` | Valid/invalid summary with parse details |
| Validate complete project gates | `validate_pipeline` | `project_path` | Pass/fail summary across checkpoints |
| Validate SPARQL syntax | `validate_sparql` | `query_path` | Syntax validation result |
| Validate template syntax | `validate_templates` | `template_path` | Template validation result |
| Detect/remediate import cycles | `fix_cycles` | `project_path`, `strategy`, optional `dry_run` | Cycle report and applied strategy result |
| Inspect runnable examples | `list_examples`, `get_example` | optional filter/name | Example metadata and project details |
| Scaffold from known example | `scaffold_from_example` | `example_name`, `target_dir` | Copy result and file list |
| Query ontology semantics quickly | `query_ontology` | `ttl`, `sparql` | SELECT rows as structured output |

## Notes

- Keep `limit` values bounded where tools support pagination/filtering.
- Prefer explicit paths over implicit defaults in CI and agent automation.
- Do not create private wrappers that bypass these tool contracts.
