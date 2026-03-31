# What is MCP?

## Overview

**MCP (Model Context Protocol)** is an open protocol that enables AI assistants (like Claude, ChatGPT) to connect to external data sources and tools. In ggen, MCP provides a standardized interface for LLMs to interact with the code generation pipeline.

## How MCP Works in ggen

```
┌─────────────┐     JSON-RPC 2.0      ┌──────────────────┐
│   LLM       │ ◄────────────────────► │  GgenMcpServer   │
│  (Claude)   │                        │   (16 tools)     │
└─────────────┘                        └────────┬─────────┘
                                                 │
                                                 ▼
                                        ┌─────────────────┐
                                        │  Code Gen Core  │
                                        │   (μ₁-μ₅)       │
                                        └─────────────────┘
```

### Key Components

1. **MCP Client**: LLM application (Claude Desktop, Cursor IDE)
2. **MCP Server**: `GgenMcpServer` exposes 16 tools via stdio/http transport
3. **A2A Bridge**: Bidirectional translation between LLM and A2A protocols
4. **Core Services**: Code generation, validation, SPARQL queries

## Benefits

| Benefit | Description |
|---------|-------------|
| **Standardized** | Uses JSON-RPC 2.0, compatible with any MCP client |
| **Secure** | Origin validation, schema validation, rate limiting |
| **Extensible** | Easy to add new tools via `#[tool]` attribute |
| **Observable** | OpenTelemetry spans for all tool calls |
| **Streaming** | Supports long-running operations with progress updates |

## Available Tools

The ggen MCP server exposes 16 tools organized by category:

### Pipeline Tools
- `generate` - Generate code from RDF ontology (μ₁-μ₅ pipeline)
- `sync` - Full sync with dry-run and audit support
- `validate` - Validate Turtle (.ttl) ontology syntax

### Query Tools
- `query_ontology` - Execute SPARQL SELECT queries
- `list_generators` - List available code generators
- `search` - Search marketplace (deprecated)

### Example Tools
- `list_examples` - List bundled example projects
- `get_example` - Get example details (TTL, config, README)
- `scaffold_from_example` - Copy example to new directory

### Validation Tools
- `validate_pipeline` - Run all 6 quality gates
- `validate_sparql` - Validate SPARQL query syntax
- `validate_templates` - Validate template syntax
- `validate_project` - Full project validation
- `validate_incremental` - Validate only changed files
- `validate_dependency_graph` - Analyze cross-input dependencies

### Orchestration Tools
- `fix_cycles` - Detect and fix circular dependencies

## Transport Options

### Stdio (Default)
```bash
ggen mcp start-server --transport stdio
```
Used by Claude Desktop and other local MCP clients.

### HTTP
```bash
ggen mcp start-server --transport http --port 8080
```
Used for remote connections and web-based clients.

## Next Steps

- [Quick Start Guide](./quick-start.md) - Get up and running in 5 minutes
- [Installation](./installation.md) - Detailed installation instructions
- [Tool Reference](../02-user-guide/tools/) - Complete tool documentation
