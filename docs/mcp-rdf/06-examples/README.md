# Example MCP Server Definitions

Complete, working examples of MCP server definitions in RDF/Turtle.

## Quick Start

Generate a server from any example:

```bash
ggen mcp generate \
  --ontology docs/mcp-rdf/06-examples/minimal-server.ttl \
  --output ./my-server/src
```

## Examples

### minimal-server.ttl

**Complexity:** Beginner
**Tools:** 1 (hello)
**Resources:** 0
**Prompts:** 0

A simple "hello world" server with one tool that says hello. Perfect for understanding the basic structure.

**Generate:**
```bash
ggen mcp generate --ontology minimal-server.ttl --output ./minimal/src
```

**Test:**
```bash
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"hello","arguments":{"name":"World"}}}' | \
  cargo run --quiet
```

---

### full-server.ttl

**Complexity:** Intermediate
**Tools:** 3 (add, multiply, divide)
**Resources:** 2 (config, status)
**Prompts:** 2 (explain, summarize)
**Completions:** 1
**Logging:** Yes

Demonstrates all MCP capabilities: tools with arguments, static resources, resource templates, prompts with arguments, argument completion, and logging policy.

**Generate:**
```bash
ggen mcp generate --ontology full-server.ttl --output ./full/src
```

**Test tools:**
```bash
# Add numbers
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"add","arguments":{"a":5,"b":3}}}' | cargo run

# Get config
echo '{"jsonrpc":"2.0","id":2,"method":"resources/read","params":{"uri":"config://server"}}' | cargo run
```

---

### codegen-server.ttl

**Complexity:** Advanced
**Tools:** 4 (sync, validate, query_ontology, generate)
**Resources:** 2 (config, ontology)
**Prompts:** 2 (explain_rdf_schema, generate_from_example)
**Completions:** 2
**Logging:** Full

A realistic code generation server similar to what ggen itself uses. Demonstrates:
- Complex tool arguments with defaults
- Multiple completion providers
- Resource with different MIME types
- Prompts with optional arguments

**Generate:**
```bash
ggen mcp generate --ontology codegen-server.ttl --output ./codegen/src
```

**Test sync tool:**
```bash
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"sync","arguments":{"ontology_path":"specify/ontologies/mcp/mcp-server.ttl","target":"rust"}}}' | \
  cargo run --quiet
```

---

## Pattern Reference

### Tool with Required Arguments

```turtle
:MyTool a mcp:Tool ;
    mcp:name "my_tool" ;
    mcp:description "Does something" ;
    mcp:hasArgument :RequiredArg .

:RequiredArg a mcp:ToolArgument ;
    mcp:argumentName "input" ;
    mcp:argumentType "string" ;
    mcp:isRequired true .
```

### Tool with Optional Arguments

```turtle
:MyTool a mcp:Tool ;
    mcp:name "my_tool" ;
    mcp:hasArgument :OptionalArg .

:OptionalArg a mcp:ToolArgument ;
    mcp:argumentName "option" ;
    mcp:argumentType "string" ;
    mcp:isRequired false ;
    mcp:defaultValue "default_value" .
```

### Multiple Arguments

```turtle
:MathTool a mcp:Tool ;
    mcp:name "add" ;
    mcp:hasArgument :ArgA, :ArgB .

:ArgA a mcp:ToolArgument ;
    mcp:argumentName "a" ;
    mcp:argumentType "number" ;
    mcp:isRequired true .

:ArgB a mcp:ToolArgument ;
    mcp:argumentName "b" ;
    mcp:argumentType "number" ;
    mcp:isRequired true .
```

### Resource with MIME Type

```turtle
:JsonResource a mcp:Resource ;
    mcp:uri "app://data" ;
    mcp:resourceName "Data" ;
    mcp:mimeType "application/json" .
```

### Resource Template

```turtle
:ItemTemplate a mcp:ResourceTemplate ;
    mcp:uriPattern "items://{id}" ;
    mcp:name "item_by_id" ;
    mcp:mimeType "application/json" .
```

### Prompt with Arguments

```turtle
:ExplainPrompt a mcp:Prompt ;
    mcp:promptName "explain" ;
    mcp:hasPromptArgument :ConceptArg, :LevelArg .

:ConceptArg a mcp:PromptArgument ;
    mcp:argumentName "concept" ;
    mcp:argumentType "string" ;
    mcp:isRequired true .

:LevelArg a mcp:PromptArgument ;
    mcp:argumentName "level" ;
    mcp:argumentType "string" ;
    mcp:isRequired false ;
    mcp:defaultValue "simple" .
```

### Completion Provider

```turtle
:OpCompletion a mcp:CompletionProvider ;
    mcp:completionRefType "tool" ;
    mcp:completionRefName "my_tool" ;
    mcp:completionArgument "operation" ;
    mcp:completionValues "add,subtract,multiply" .
```

---

## Testing Generated Servers

### 1. Generate Server

```bash
ggen mcp generate \
  --ontology minimal-server.ttl \
  --output /tmp/test-server/src
```

### 2. Create Cargo.toml

```bash
cd /tmp/test-server
cat > Cargo.toml << 'EOF'
[package]
name = "test-server"
version = "1.0.0"
edition = "2021"

[dependencies]
rmcp = "1.3.0"
tokio = { version = "1.42", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["env-filter"] }
EOF
```

### 3. Build

```bash
cargo build --release
```

### 4. Test with JSON-RPC

```bash
# List tools
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | \
  ./target/release/test-server

# Call tool
echo '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"hello","arguments":{"name":"Claude"}}}' | \
  ./target/release/test-server
```

### 5. Test with Claude Desktop

Add to `~/Library/Application Support/Claude/claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "test-server": {
      "command": "/tmp/test-server/target/release/test-server"
    }
  }
}
```

---

## Common Patterns

### Conditional Capabilities

```turtle
# Enable resources only if needed
:MyCaps a mcp:CapabilitySet ;
    mcp:tools true ;
    mcp:resources false ;
    mcp:prompts false .
```

### Tool Implementation Hints

```turtle
:MyTool a mcp:Tool ;
    mcp:name "my_tool" ;
    mcp:implementedBy "my_crate::tools::my_tool" .
```

The `implementedBy` property is used as a hint for where the tool logic lives. It's not used in code generation but can be useful for documentation.

---

## See Also

- [Quick Start](../01-quick-start/) - Your first server
- [RDF Schema Reference](../02-rdf-schema/) - All classes and properties
- [Code Generation Guide](../03-code-generation/) - How generation works
