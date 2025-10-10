# ggen-mcp

MCP (Model Context Protocol) server for ggen - exposes template generation, graph operations, and project management through a standardized API.

## Installation

```bash
# Install as MCP server
claude mcp add ggen npx -y ggen-mcp

# Or build from source
cd ggen-mcp
cargo build --release
```

## Usage

The ggen MCP server runs via stdio transport and exposes the following tool categories:

### Project Tools

- **project_gen** - Generate files from template
- **project_plan** - Create execution plan without applying
- **project_apply** - Apply execution plan
- **project_diff** - Show differences between template and existing files

### Market Tools

- **market_list** - List available marketplace templates
- **market_search** - Search templates by query
- **market_install** - Install template from marketplace

### Graph Tools

- **graph_query** - Execute SPARQL queries
- **graph_load** - Load RDF data from file
- **graph_export** - Export graph to file

### Template Tools

- **template_create** - Create new template
- **template_validate** - Validate template syntax

### Hook Tools

- **hook_register** - Register lifecycle hooks

## Architecture

```
ggen-mcp/
├── src/
│   ├── main.rs          # MCP server entrypoint
│   ├── server.rs        # ServerHandler implementation
│   ├── schema.rs        # JSON schemas for tools
│   ├── error.rs         # Error types and helpers
│   └── tools/           # Tool implementations
│       ├── project.rs
│       ├── market.rs
│       ├── graph.rs
│       ├── template.rs
│       └── hook.rs
```

## Development

```bash
# Run tests
cargo test

# Run with logging
RUST_LOG=info cargo run

# Test MCP connection
npx @modelcontextprotocol/inspector cargo run
```

## License

MIT
