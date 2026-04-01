# Quick Start: Generate Your First MCP Server

Get started with ggen's RDF-driven MCP server generation in 5 minutes.

## Prerequisites

- **Rust 1.91.1+** - Install via `rustup`
- **ggen CLI** - Install via `cargo install ggen-cli`
- **Basic RDF knowledge** - Turtle syntax is simple to learn

## Step 1: Define Your Server

Create a file `my-server.ttl`:

```turtle
@prefix mcp: <http://ggen.dev/mcp#> .
@prefix ex:  <http://example.com/my-server#> .

# Server definition
ex:MyServer a mcp:McpServer ;
    mcp:serverName "my-first-mcp-server" ;
    mcp:serverVersion "1.0.0" ;
    mcp:serverDescription "My first MCP server" ;
    mcp:hasProtocolVersion "2025-11-25" ;
    mcp:hasTransport mcp:StdioTransport ;
    mcp:hasCapabilitySet ex:MyCapabilities ;
    mcp:hasTool ex:HelloTool .

# Capabilities (which MCP features to enable)
ex:MyCapabilities a mcp:CapabilitySet ;
    mcp:tools true ;
    mcp:resources false ;
    mcp:prompts false .

# Tool: Say hello
ex:HelloTool a mcp:Tool ;
    mcp:name "hello" ;
    mcp:description "Say hello to someone" ;
    mcp:hasArgument ex:NameArgument .

ex:NameArgument a mcp:ToolArgument ;
    mcp:argumentName "name" ;
    mcp:argumentDescription "The name to say hello to" ;
    mcp:argumentType "string" ;
    mcp:isRequired true .
```

## Step 2: Generate the Server

```bash
ggen mcp generate \
  --ontology my-server.ttl \
  --output ./my-mcp-server/src
```

**Output:**
```
μ₁: Loading ontology... 12 triples loaded
μ₂: Extracting context... 1 tool
μ₃: Rendering templates... 3 files generated
μ₄: Compile gate... ✅ PASSED (cargo check in 1.2s)
μ₅: Emitting files... receipt: 8f3a2c1b7d9e

Generated files:
  - server.rs (87 lines)
  - tools.rs (45 lines)
  - main.rs (32 lines)
```

## Step 3: Create Cargo.toml

Create `Cargo.toml` in `my-mcp-server/`:

```toml
[package]
name = "my-first-mcp-server"
version = "1.0.0"
edition = "2021"

[dependencies]
rmcp = "1.3.0"
tokio = { version = "1.42", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["env-filter"] }
```

## Step 4: Run Your Server

```bash
cd my-mcp-server
cargo run
```

Your server is now running on stdio, ready to receive MCP requests!

## Step 5: Test with Claude Desktop

Add to your Claude Desktop config (`~/Library/Application Support/Claude/claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "my-first-server": {
      "command": "/path/to/my-mcp-server/target/release/my-first-mcp-server"
    }
  }
}
```

Restart Claude Desktop. Your `hello` tool is now available!

## What Happened?

The `ggen mcp generate` command:

1. **Loaded** your RDF ontology into Oxigraph
2. **Extracted** server metadata via SPARQL CONSTRUCT
3. **Rendered** 3 Tera templates with extracted context:
   - `server.rs` - Server struct and `serve_stdio()` method
   - `tools.rs` - `#[tool]` handler for `hello`
   - `main.rs` - Tokio bootstrap
4. **Validated** generated code compiles (cargo check)
5. **Emitted** files with cryptographic receipt

## Next Steps

- **Add more tools** - Define additional `mcp:Tool` instances
- **Enable resources** - Set `mcp:resources true` and define `mcp:Resource`
- **Add prompts** - Define `mcp:Prompt` for reusable LLM prompts
- **Customize templates** - See [Template Customization Guide](../04-template-customization/)
- **Explore RDF schema** - See [RDF Schema Reference](../02-rdf-schema/)

## Common Issues

### "cargo check failed"

The compile gate found errors. Check:
- Rust version is 1.91.1+
- RMCP dependency is 1.3.0
- No syntax errors in generated code

### "No tools found"

Verify your ontology has:
- `mcp:McpServer` instance
- `mcp:hasTool` property linking to tool definitions
- Tool has `mcp:name` property

### "Template not found"

Ensure ggen is installed correctly:
```bash
cargo install ggen-cli --force
```

## See Also

- [RDF Schema Reference](../02-rdf-schema/) - All MCP classes and properties
- [Examples](../06-examples/) - Sample server definitions
- [Code Generation Guide](../03-code-generation/) - Pipeline internals
