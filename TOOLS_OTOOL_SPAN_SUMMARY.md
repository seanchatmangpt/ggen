# MCP Tool OTEL Span Summary

| # | Tool | OTEL Span (`mcp.tool_name`) | Additional Attributes | Source Line | Status |
|---|------|----------------------------|----------------------|-------------|--------|
| 1 | `generate` | ✅ Yes | `mcp.ontology_path` | ggen_server.rs:355 | ✅ PASS |
| 2 | `validate` | ✅ Yes | `mcp.ttl_length` | ggen_server.rs:431 | ✅ PASS |
| 3 | `sync` | ✅ Yes | `mcp.ontology_path` | ggen_server.rs:489 | ✅ PASS |
| 4 | `list_generators` | ✅ Yes | - | ggen_server.rs:567 | ✅ PASS |
| 5 | `list_examples` | ✅ Yes | - | ggen_server.rs:593 | ✅ PASS |
| 6 | `get_example` | ✅ Yes | - | ggen_server.rs:636 | ✅ PASS |
| 7 | `search` | ✅ Yes | - | ggen_server.rs:689 | ✅ PASS |
| 8 | `scaffold_from_example` | ✅ Yes | `mcp.project_path` | ggen_server.rs:751 | ✅ PASS |
| 9 | `query_ontology` | ✅ Yes | `mcp.sparql_query_length`, `mcp.ttl_length` | ggen_server.rs:805 | ✅ PASS |
| 10 | `validate_pipeline` | ✅ Yes | `mcp.project_path` | ggen_server.rs:912 | ✅ PASS |
| 11 | `validate_sparql` | ✅ Yes | `mcp.query_path` | ggen_server.rs:1015 | ✅ PASS |
| 12 | `validate_templates` | ✅ Yes | `mcp.project_path`, `mcp.template_path` | ggen_server.rs:1066 | ✅ PASS |
| 13 | `fix_cycles` | ✅ Yes | `mcp.project_path` | ggen_server.rs:1139 | ✅ PASS |

## Evidence

### Source Code Pattern (all 13 tools)
```rust
#[tracing::instrument(name = "ggen.mcp.tool_call", skip(self), fields(...))]
async fn tool_name(...) -> Result<CallToolResult, McpError> {
    tracing::Span::current().record(otel_attrs::MCP_TOOL_NAME, "tool_name");
    // ...
}
```

### Runtime Evidence (from test logs)
```
INFO ggen.mcp.tool_call{...} validate_pipeline tool called project_path=/Users/sac/ggen
```

## Conclusion

✅ **13/13 tools (100%) emit proper OTEL spans with `mcp.tool_name` attribute**

All tools comply with OTEL validation requirements per `.claude/rules/otel-validation.md`.
