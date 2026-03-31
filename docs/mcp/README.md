# ggen MCP Server Generation

Generate spec-pinned MCP 2025-11-25 servers from RDF ontology.

## Architecture

```
Ontology (.ttl) -> SPARQL CONSTRUCT -> Normalized JSON Context -> Capability Templates -> Rust Server
```

- **Ontology**: Pure MCP 2025-11-25 classes (`specify/ontologies/mcp/mcp.ttl`)
- **Context**: Normalized JSON via SPARQL CONSTRUCT (`crates/ggen-core/queries/mcp/extract-mcp-context.rq`)
- **Templates**: Capability-specific (`crates/ggen-core/templates/mcp/`)
- **SDK**: RMCP 1.3.0 (official Rust SDK)

## Quick Start

1. Define your MCP server in RDF/Turtle:

```turtle
@prefix mcp: <http://ggen.dev/mcp#> .

:MyServer a mcp:McpServer ;
    mcp:serverName "MyServer" ;
    mcp:hasCapabilitySet [
        a mcp:CapabilitySet ;
        mcp:tools true ;
        mcp:resources true ;
    ] ;
    mcp:hasTransport mcp:StdioTransport ;
    mcp:hasTool [
        a mcp:Tool ;
        mcp:name "hello" ;
        mcp:description "Say hello" ;
        mcp:hasArgument [
            a mcp:ToolArgument ;
            mcp:argumentName "name" ;
            mcp:argumentType "String" ;
            mcp:isRequired true ;
        ] ;
    ] .
```

2. Generate the server:

```bash
ggen mcp generate --ontology server.ttl --output ./src
```

3. Run your server:

```bash
cargo run
```

## Supported Capabilities

| Capability | Template | Status |
|-------------|----------|--------|
| Tools | `tools.rs.tera` | Implemented |
| Resources | `resources.rs.tera` | Implemented |
| Resource Templates | `resource_templates.rs.tera` | Implemented |
| Prompts | `prompts.rs.tera` | Implemented |
| Completions | `completions.rs.tera` | Implemented |
| Logging | `logging.rs.tera` | Implemented |
| Subscriptions | - | Planned |

## Templates

| File | Purpose |
|------|---------|
| `server.rs.tera` | Server struct, lifecycle, transport setup |
| `get_info.rs.tera` | Capability declaration (get_info handler) |
| `tools.rs.tera` | Tool router + handlers (#[tool_router]) |
| `resources.rs.tera` | Resource list + read |
| `resource_templates.rs.tera` | Resource template list |
| `prompts.rs.tera` | Prompt list + get |
| `completions.rs.tera` | Argument autocomplete |
| `logging.rs.tera` | MCP logging protocol |
| `main.rs.tera` | Server bootstrap |

## Validation

The compile gate ensures generated code compiles:

```bash
ggen mcp generate --ontology server.ttl --output ./src
# Pre-commit hook runs cargo check automatically
```

## OTEL Integration

Generated tools emit structured OTEL spans:

```
mcp.tool.call (span)
  mcp.tool.name = "validate_pipeline"
Tool invocation completed
```
