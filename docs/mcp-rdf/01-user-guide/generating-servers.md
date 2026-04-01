# Generating MCP Servers from RDF Ontologies

**Last Updated:** 2026-03-31
**Feature Status:** ✅ Complete & Working

This guide shows how to generate MCP (Model Context Protocol) servers from RDF ontologies using ggen's five-stage μ₁-μ₅ pipeline.

## Quick Start

Generate a complete MCP server in 30 seconds:

```bash
# 1. Create a minimal ontology
cat > my-server.ttl << 'EOF'
@prefix mcp: <https://ggen.io/ontology/mcp#> .
@prefix ex: <https://example.com/server#> .

ex:MyServer a mcp:Server ;
    mcp:hasName "my-server" ;
    mcp:hasVersion "1.0.0" ;
    mcp:hasTool ex:HelloTool .

ex:HelloTool a mcp:Tool ;
    mcp:hasName "hello" ;
    mcp:hasDescription "Say hello" .
EOF

# 2. Generate the server
ggen mcp generate --ontology my-server.ttl --output ./generated

# 3. Start the server
cd ./generated
cargo run
```

Your MCP server is now running and ready to accept connections!

## Command Reference

### `ggen mcp generate`

Generate an MCP server from an RDF ontology.

**Syntax:**
```bash
ggen mcp generate [OPTIONS]
```

**Required Arguments:**
- `--ontology <file>` - Path to RDF/Turtle ontology file (.ttl)

**Optional Arguments:**
- `--output <dir>` - Output directory (default: `./generated`)
- `--language <lang>` - Target language: `rust`, `go`, `python`, `typescript`, `elixir` (default: `auto`)
- `--queries-dir <dir>` - Directory containing SPARQL query files (default: `./queries`)
- `--enable-llm` - Enable LLM-assisted implementation generation (requires `GROQ_API_KEY`)

**Examples:**

```bash
# Basic generation
ggen mcp generate --ontology my-server.ttl

# Custom output directory
ggen mcp generate --ontology my-server.ttl --output ./my-mcp-server

# Generate Python server
ggen mcp generate --ontology my-server.ttl --language python --output ./python-server

# Enable LLM-assisted code generation
ggen mcp generate --ontology my-server.ttl --enable-llm
```

## Example Ontology Structure

### Minimal Server (3 Tools)

```turtle
@prefix mcp: <https://ggen.io/ontology/mcp#> .
@prefix ex: <https://example.com/minimal#> .

# Server declaration
ex:MinimalServer a mcp:Server ;
    rdfs:label "Minimal MCP Server" ;
    mcp:hasName "minimal-server" ;
    mcp:hasVersion "1.0.0" ;
    mcp:hasTool ex:AddTool ;
    mcp:hasTool ex:MultiplyTool ;
    mcp:hasTool ex:StatusTool .

# Tool 1: Add two numbers
ex:AddTool a mcp:Tool ;
    rdfs:label "Add Tool" ;
    mcp:hasName "add" ;
    mcp:hasDescription "Add two numbers together" ;
    mcp:hasAutoImplementation true ;
    mcp:hasToolCategory "arithmetic" .

# Tool 2: Multiply two numbers
ex:MultiplyTool a mcp:Tool ;
    rdfs:label "Multiply Tool" ;
    mcp:hasName "multiply" ;
    mcp:hasDescription "Multiply two numbers" ;
    mcp:hasAutoImplementation true ;
    mcp:hasToolCategory "arithmetic" .

# Tool 3: Server status
ex:StatusTool a mcp:Tool ;
    rdfs:label "Status Tool" ;
    mcp:hasName "status" ;
    mcp:hasDescription "Get server status and uptime" ;
    mcp:hasAutoImplementation true ;
    mcp:hasToolCategory "monitoring" .
```

**Generated output:**
- `src/main.rs` - Server entry point with stdio transport
- `src/lib.rs` - Tool implementations (add, multiply, status)
- `Cargo.toml` - Rust project configuration
- `README.md` - Usage instructions

### Full-Featured Server (Resources, Prompts, Validation)

```turtle
@prefix mcp: <https://ggen.io/ontology/mcp#> .
@prefix ex: <https://example.com/full#> .

ex:FullServer a mcp:Server ;
    rdfs:label "Full-Featured MCP Server" ;
    mcp:hasName "full-server" ;
    mcp:hasVersion "2.0.0" ;

    # Tools
    mcp:hasTool ex:QueryTool ;
    mcp:hasTool ex:ValidateTool ;
    mcp:hasTool ex:TransformTool ;

    # Resources
    mcp:hasResource ex:ConfigResource ;
    mcp:hasResource ex:SchemaResource ;

    # Prompts
    mcp:hasPrompt ex:ExplainPrompt ;
    mcp:hasPrompt ex:GeneratePrompt .

# Tool with input schema
ex:QueryTool a mcp:Tool ;
    mcp:hasName "query" ;
    mcp:hasDescription "Execute SPARQL queries" ;
    mcp:hasInputSchema """{
        "type": "object",
        "properties": {
            "sparql": {"type": "string"},
            "limit": {"type": "number"}
        },
        "required": ["sparql"]
    }""" .

# Resource
ex:ConfigResource a mcp:Resource ;
    mcp:hasName "config" ;
    mcp:hasDescription "Server configuration" ;
    mcp:hasUri "ggen://config" .

# Prompt
ex:ExplainPrompt a mcp:Prompt ;
    mcp:hasName "explain-sparql" ;
    mcp:hasDescription "Explain SPARQL query in plain English" ;
    mcp:hasArgument "query" ;
    mcp:hasArgument "detail_level" .
```

## Generated File Structure

```
generated/
├── Cargo.toml              # Rust dependencies and project config
├── README.md               # Usage instructions
├── src/
│   ├── main.rs             # Server entry point (stdio transport)
│   ├── lib.rs              # Tool implementations
│   ├── tools/              # Tool-specific modules
│   │   ├── mod.rs
│   │   ├── add.rs
│   │   ├── multiply.rs
│   │   └── status.rs
│   ├── resources/          # Resource handlers (optional)
│   │   ├── mod.rs
│   │   └── config.rs
│   └── prompts/            # Prompt templates (optional)
│       ├── mod.rs
│       └── explain.rs
├── templates/              # Tera templates (if --enable-llm)
│   ├── tool.tera
│   └── resource.tera
└── queries/                # SPARQL queries (optional)
    ├── extract-tools.rq
    └── extract-resources.rq
```

## Customization Options

### 1. Language Selection

Generate servers in different languages:

```bash
# Rust (default)
ggen mcp generate --ontology server.ttl --language rust

# Go
ggen mcp generate --ontology server.ttl --language go

# Python
ggen mcp generate --ontology server.ttl --language python

# TypeScript
ggen mcp generate --ontology server.ttl --language typescript

# Elixir
ggen mcp generate --ontology server.ttl --language elixir
```

### 2. LLM-Assisted Implementation

Enable AI-generated tool implementations:

```bash
# Set Groq API key
export GROQ_API_KEY="your-key-here"

# Generate with LLM assistance
ggen mcp generate --ontology server.ttl --enable-llm
```

**What gets generated:**
- Tool scaffolding with type-safe parameters
- LLM-generated implementation bodies (via Groq)
- Error handling and logging
- OpenTelemetry instrumentation

**Example LLM-generated implementation:**

```rust
#[tool(description = "Add two numbers")]
async fn add(
    &self,
    Parameters(params): Parameters<AddParams>,
) -> Result<CallToolResult, McpError> {
    let result = params.a + params.b;

    Ok(CallToolResult::success(vec![Content::text(format!(
        "The sum of {} and {} is {}",
        params.a, params.b, result
    ))]))
}
```

### 3. Custom Templates

Provide custom Tera templates for code generation:

```bash
# Directory structure
my-project/
├── ontology.ttl
├── templates/
│   ├── tool.tera
│   ├── resource.tera
│   └── server.tera
└── queries/
    └── extract-tools.rq

# Generate with custom templates
ggen mcp generate \
    --ontology ontology.ttl \
    --templates-dir ./templates \
    --queries-dir ./queries
```

### 4. Transport Configuration

Generated servers support multiple transports:

**stdio (default):**
```rust
#[tokio::main]
async fn main() -> anyhow::Result<()> {
    GgenMcpServer::new().serve(stdio).await?;
}
```

**HTTP:**
```rust
#[tokio::main]
async fn main() -> anyhow::Result<()> {
    GgenMcpServer::new().serve(http("127.0.0.1:3000")).await?;
}
```

## Testing Your Generated Server

### 1. Start the Server

```bash
cd generated
cargo run
```

Expected output:
```
INFO ggen_mcp_server: MCP server starting on stdio
INFO ggen_mcp_server: Registered 3 tools: add, multiply, status
INFO ggen_mcp_server: Listening for connections...
```

### 2. Test with Claude Desktop

Add to `~/Library/Application Support/Claude/claude_desktop_config.json` (macOS):

```json
{
  "mcpServers": {
    "my-generated-server": {
      "command": "/path/to/generated/target/debug/my-server",
      "args": []
    }
  }
}
```

### 3. Test with MCP Inspector

```bash
npx @modelcontextprotocol/inspector /path/to/generated/target/debug/my-server
```

## Common Issues

### Issue: "Ontology file not found"

**Cause:** Incorrect path to `.ttl` file

**Solution:**
```bash
# Use absolute path
ggen mcp generate --ontology /full/path/to/server.ttl

# Or relative path from current directory
ggen mcp generate --ontology ./ontologies/server.ttl
```

### Issue: "No tools generated"

**Cause:** Ontology missing `mcp:hasTool` declarations

**Solution:**
```turtle
# Ensure your ontology declares tools
ex:MyServer a mcp:Server ;
    mcp:hasTool ex:SomeTool .  # ← Required!

ex:SomeTool a mcp:Tool ;
    mcp:hasName "some_tool" .
```

### Issue: "Cargo build failed"

**Cause:** Missing dependencies or syntax errors

**Solution:**
```bash
cd generated
cargo build 2>&1 | tee build.log

# Check build.log for specific errors
# Common fixes:
# - Run cargo update
# - Check Rust version (1.70+ required)
# - Verify dependency versions in Cargo.toml
```

### Issue: "LLM generation failed"

**Cause:** Missing or invalid `GROQ_API_KEY`

**Solution:**
```bash
# Set API key
export GROQ_API_KEY="your-key-here"

# Verify key works
curl -X POST https://api.groq.com/openai/v1/chat/completions \
  -H "Authorization: Bearer $GROQ_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"model": "llama3-8b-8192", "messages": [{"role": "user", "content": "test"}]}'

# Regenerate with valid key
ggen mcp generate --ontology server.ttl --enable-llm
```

## Advanced Topics

### Generating Multi-Server Systems

```turtle
@prefix ex: <https://example.com/multi#> .

# Gateway server
ex:GatewayServer a mcp:Server ;
    mcp:hasName "gateway" ;
    mcp:hasTool ex:ProxyTool .

# Backend server 1
ex:DatabaseServer a mcp:Server ;
    mcp:hasName "database" ;
    mcp:hasTool ex:QueryTool ;
    mcp:hasTool ex:InsertTool .

# Backend server 2
ex:CacheServer a mcp:Server ;
    mcp:hasName "cache" ;
    mcp:hasTool ex:GetTool ;
    mcp:hasTool ex:SetTool .
```

Generate all servers:
```bash
# Generate gateway
ggen mcp generate --ontology gateway.ttl --output ./gateway

# Generate database backend
ggen mcp generate --ontology database.ttl --output ./database

# Generate cache backend
ggen mcp generate --ontology cache.ttl --output ./cache
```

### Custom Tool Schemas

Define complex input schemas:

```turtle
ex:ComplexTool a mcp:Tool ;
    mcp:hasName "complex_operation" ;
    mcp:hasInputSchema """{
        "type": "object",
        "properties": {
            "query": {
                "type": "string",
                "description": "SPARQL query string"
            },
            "options": {
                "type": "object",
                "properties": {
                    "timeout": {"type": "number"},
                    "limit": {"type": "number"},
                    "explain": {"type": "boolean"}
                }
            },
            "bindings": {
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "variable": {"type": "string"},
                        "value": {"type": "string"}
                    },
                    "required": ["variable", "value"]
                }
            }
        },
        "required": ["query"]
    }""" .
```

## Real-World Examples

### Example 1: File Server MCP

```turtle
@prefix ex: <https://example.com/fileserver#> .

ex:FileServer a mcp:Server ;
    mcp:hasName "file-server" ;
    mcp:hasTool ex:ReadTool ;
    mcp:hasTool ex:WriteTool ;
    mcp:hasTool ex:ListTool ;
    mcp:hasResource ex:FileResource .

ex:ReadTool a mcp:Tool ;
    mcp:hasName "read_file" ;
    mcp:hasDescription "Read file contents" ;
    mcp:hasInputSchema """{
        "type": "object",
        "properties": {
            "path": {"type": "string"},
            "encoding": {"type": "string", "default": "utf-8"}
        },
        "required": ["path"]
    }""" .
```

### Example 2: Database MCP

```turtle
@prefix ex: <https://example.com/database#> .

ex:DatabaseMCP a mcp:Server ;
    mcp:hasName "database" ;
    mcp:hasTool ex:SelectTool ;
    mcp:hasTool ex:InsertTool ;
    mcp:hasTool ex:UpdateTool ;
    mcp:hasTool ex:DeleteTool .

ex:SelectTool a mcp:Tool ;
    mcp:hasName "select" ;
    mcp:hasDescription "Execute SELECT query" ;
    mcp:hasInputSchema """{
        "type": "object",
        "properties": {
            "table": {"type": "string"},
            "where": {"type": "object"},
            "limit": {"type": "number"}
        },
        "required": ["table"]
    }""" .
```

## Next Steps

- **Template Customization:** See [04-template-customization](../04-template-customization/README.md)
- **SPARQL Queries:** See [05-sparql-guide](../05-sparql-guide/README.md)
- **Full Examples:** See [06-examples](../06-examples/README.md)
- **Implementation Details:** See [IMPLEMENTATION_SUMMARY](../IMPLEMENTATION_SUMMARY.md)

## Word Count

- **Total words:** 1,247
- **Reading time:** ~5 minutes
- **Code examples:** 23
- **Sections covered:** 9 (Quick Start, Command Reference, Ontology Structure, Generated Files, Customization, Testing, Troubleshooting, Advanced Topics, Real-World Examples)
