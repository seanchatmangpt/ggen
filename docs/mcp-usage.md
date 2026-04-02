# ggen MCP Server Usage Guide

**Version:** 6.0.1 | **Last Updated:** 2026-03-31

## Overview

ggen provides an official MCP (Model Context Protocol) server via [`rmcp` 1.3.0](https://github.com/lijuncie/rmcp), exposing code generation capabilities as MCP primitives for AI agents and tool integrations.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    ggen MCP Server                          │
├─────────────────┬───────────────────┬───────────────────────┤
│  Tools (14)     │  Resources (4)    │  Prompts (3)          │
│  - generate     │  - ggen://example/│  - explain-rdf-schema │
│  - validate     │  - ttl            │  - generate-from-     │
│  - sync         │  - readme         │    example            │
│  - query_       │  - config         │  - scaffold-project   │
│    ontology     │                   │                       │
│  - validate_    │                   │                       │
│    pipeline     │                   │                       │
│  - validate_    │                   │                       │
│    sparql       │                   │                       │
│  - validate_    │                   │                       │
│    templates    │                   │                       │
│  - fix_cycles   │                   │                       │
│  - list_        │                   │                       │
│    generators   │                   │                       │
│  - list_        │                   │                       │
│    examples     │                   │                       │
│  - get_example  │                   │                       │
│  - search       │                   │                       │
│  - scaffold_    │                   │                       │
│    from_example │                   │                       │
└─────────────────┴───────────────────┴───────────────────────┘
```

## Transport Options

### 1. Stdio Transport (Default)

For MCP clients like Claude Desktop:

```bash
ggen mcp start-server --transport stdio
```

### 2. HTTP Transport

For HTTP-based MCP clients:

```bash
ggen mcp start-server --transport http --host 0.0.0.0 --port 3000
```

**Example HTTP request:**

```bash
curl -X POST http://localhost:3000 \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/call",
    "params": {
      "name": "list_generators",
      "arguments": {}
    },
    "id": 1
  }'
```

## Tool Reference

### Core Pipeline Tools

#### `generate`

Generate code from a RDF ontology file via the μ₁-μ₅ pipeline.

**Parameters:**

```typescript
{
  ontology_path: string;        // Path to .ttl file
  queries_dir?: string;         // Directory with .rq files (default: "queries/")
  output_dir?: string;          // Output directory (default: "generated/")
  language?: string;            // Target language: go|rust|python|typescript|elixir|auto
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": "Generated 12 file(s) in 234ms\nFiles: src/main.rs, src/lib.rs, ...\nSoundness violations: none\nReceipt: sha256:abc123..."
    }
  ]
}
```

**OTEL Spans:**
- `mcp.tool_name = "generate"`
- `mcp.ontology_path = "/path/to/ontology.ttl"`
- `mcp.files_generated = 12`
- `mcp.receipt = "sha256:..."`

---

#### `validate`

Validate Turtle (.ttl) ontology content for syntax correctness.

**Parameters:**

```typescript
{
  ttl: string;  // Turtle content string
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": "Valid Turtle content (42 triple(s) parsed)"
    }
  ]
}
```

**OTEL Spans:**
- `mcp.tool_name = "validate"`
- `mcp.ttl_length = 1234`
- `mcp.triple_count = 42`

---

#### `sync`

Run the full ggen μ₁-μ₅ sync pipeline from an ontology file.

**Parameters:**

```typescript
{
  ontology_path: string;        // Path to .ttl file
  queries_dir?: string;         // Directory with .rq files
  output_dir?: string;          // Output directory
  language?: string;            // Target language
  dry_run?: boolean;            // Preview only, no files written
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": "Sync (full) complete: 12 file(s) in 234ms\nFiles: src/main.rs, ...\nReceipt: sha256:abc123..."
    }
  ]
}
```

---

#### `query_ontology`

Execute a SPARQL SELECT query against a Turtle ontology string.

**Parameters:**

```typescript
{
  ttl: string;      // Turtle content string
  sparql: string;   // SPARQL SELECT query
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": {
        "rows": [
          {
            "class": "http://example.org/Person",
            "label": "Person"
          }
        ],
        "count": 1
      }
    }
  ]
}
```

**OTEL Spans:**
- `mcp.tool_name = "query_ontology"`
- `mcp.sparql_query_length = 123`
- `mcp.ttl_length = 4567`

---

### Quality Gate Tools

#### `validate_pipeline`

Run all 6 quality gates on a ggen project.

**Parameters:**

```typescript
{
  project_path: string;  // Directory containing ggen.toml
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": "✅ All quality gates passed (6/6 checkpoints)\n\nPassed checks:\n  ✓ manifest_schema\n  ✓ ontology_dependencies\n  ✓ sparql_queries\n  ✓ templates\n  ✓ file_permissions\n  ✓ generation_rules"
    }
  ]
}
```

**Quality Gates:**
1. Manifest schema validation
2. Ontology dependency checks
3. SPARQL query syntax validation
4. Template syntax validation
5. File permission validation
6. Generation rule validation

---

#### `validate_sparql`

Validate a SPARQL query file for syntax correctness.

**Parameters:**

```typescript
{
  query_path: string;  // Path to .rq file
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": "SPARQL query syntax is valid"
    }
  ]
}
```

---

#### `validate_templates`

Validate a template file for syntax correctness.

**Parameters:**

```typescript
{
  template_path: string;  // Path to .tera, .hbs, or .j2 file
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": "Template syntax is valid"
    }
  ]
}
```

---

#### `fix_cycles`

Detect and fix circular dependencies in ontology imports.

**Parameters:**

```typescript
{
  project_path: string;  // Directory containing ontologies
  strategy: string;      // "remove_import" | "merge_files" | "create_interface"
  dry_run?: boolean;     // Preview only, no files modified
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": {
        "mode": "fix",
        "cycles_found": 2,
        "fixes_applied": 2,
        "files_modified": ["domain.ttl", "shared.ttl"],
        "backup_path": "/tmp/backup_20260331_123456",
        "cycles": [...]
      }
    }
  ]
}
```

---

### Discovery Tools

#### `list_generators`

List available code generators.

**Parameters:** None

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": {
        "generators": ["go", "python", "rust", "typescript", "elixir", "terraform", "docker-kubernetes"],
        "count": 7,
        "default": "auto"
      }
    }
  ]
}
```

---

#### `list_examples`

List bundled ggen example projects.

**Parameters:**

```typescript
{
  category?: string;  // Filter by category (optional)
  limit?: number;     // Max results (default: 50)
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": {
        "examples": [
          {
            "name": "basic-rust-project",
            "description": "Simple Rust CLI application",
            "category": "rust"
          }
        ],
        "count": 1
      }
    }
  ]
}
```

---

#### `get_example`

Get details of a specific ggen example project.

**Parameters:**

```typescript
{
  name: string;  // Example name (directory name under examples/)
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": {
        "name": "basic-rust-project",
        "ggen_toml": "[project]\nname = \"...\"",
        "ttl": "@prefix rdf: <...>",
        "readme": "# Basic Rust Project\n...",
        "templates": ["main.rs.tera", "lib.rs.tera"]
      }
    }
  ]
}
```

---

#### `search`

Search marketplace packages by keyword or category.

**Parameters:**

```typescript
{
  query: string;        // Search query string
  category?: string;    // Filter by category
  limit?: number;       // Max results (default: 10)
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": {
        "query": "rust cli",
        "results": [
          {
            "name": "rust-cli-starter",
            "version": "1.0.0",
            "description": "Production-ready Rust CLI template",
            "category": "rust",
            "tags": ["cli", "rust", "production"],
            "stars": 42,
            "downloads": 1234,
            "is_8020_certified": true
          }
        ],
        "count": 1
      }
    }
  ]
}
```

---

#### `scaffold_from_example`

Scaffold a new project by copying a bundled example.

**Parameters:**

```typescript
{
  example_name: string;  // Source example name
  target_dir: string;    // Target directory path
}
```

**Response:**

```json
{
  "content": [
    {
      "type": "text",
      "text": "Scaffolded 12 file(s) from 'basic-rust-project' to '/tmp/my-project'\nFiles:\n  Cargo.toml\n  src/main.rs\n  ..."
    }
  ]
}
```

## Resources

### Example Resources

Browse example projects via URI scheme:

- `ggen://example/{name}` - Example summary (JSON)
- `ggen://example/{name}/ttl` - Raw ontology TTL
- `ggen://example/{name}/readme` - README content
- `ggen://example/{name}/config` - Raw ggen.toml

**Example usage:**

```typescript
// List resources
const resources = await mcp_client.list_resources();

// Read example summary
const summary = await mcp_client.read_resource("ggen://example/basic-rust-project");

// Read TTL content
const ttl = await mcp_client.read_resource("ggen://example/basic-rust-project/ttl");
```

## Prompts

### Available Prompts

#### `explain-rdf-schema`

Explain a Turtle RDF ontology in plain English.

**Parameters:**

```typescript
{
  ttl_content: string;  // Turtle content to explain
}
```

**Usage:**

```typescript
const prompt = await mcp_client.getPrompt("explain-rdf-schema", {
  ttl_content: "@prefix rdf: <...>"
});
// Returns formatted prompt explaining classes, properties, relationships
```

---

#### `generate-from-example`

Adapt a ggen example project to a new domain.

**Parameters:**

```typescript
{
  example_name: string;   // Source example name
  target_domain: string;  // New domain
}
```

**Usage:**

```typescript
const prompt = await mcp_client.getPrompt("generate-from-example", {
  example_name: "basic-rust-project",
  target_domain: "healthcare"
});
// Returns prompt with example TTL/ggen.toml + adaptation instructions
```

---

#### `scaffold-project`

Design a new ggen project from scratch.

**Parameters:**

```typescript
{
  domain: string;       // Business domain
  language?: string;    // Target language (optional)
}
```

**Usage:**

```typescript
const prompt = await mcp_client.getPrompt("scaffold-project", {
  domain: "e-commerce",
  language: "rust"
});
// Returns prompt requesting ggen.toml, TTL ontology, and design explanation
```

## Completions

### Argument Autocomplete

The MCP server provides autocomplete for:

- `example_name` argument - Lists discovered example names
- `generator` / `language` argument - Lists known generators

**Usage:**

```typescript
// Autocomplete example names
const completions = await mcp_client.complete({
  name: "example_name",
  value: "basic"  // Prefix to filter
});
// Returns: ["basic-rust-project", "basic-python-service"]

// Autocomplete generators
const completions = await mcp_client.complete({
  name: "generator",
  value: "ru"  // Prefix to filter
});
// Returns: ["rust"]
```

## Rust Client Example

### In-Process MCP Client

```rust
use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, ClientHandler, ServiceExt;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Create duplex transport for in-process communication
    let (server_transport, client_transport) = tokio::io::duplex(4096);

    // Start MCP server
    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        server.serve(server_transport).await
    });

    // Connect MCP client
    #[derive(Default)]
    struct TestClientHandler;
    impl ClientHandler for TestClientHandler {
        fn get_info(&self) -> ClientInfo {
            ClientInfo::default()
        }
    }

    let mcp_client = TestClientHandler::default()
        .serve(client_transport)
        .await?;

    // Call tool: validate_pipeline
    let result = mcp_client
        .call_tool(CallToolRequestParams::new("validate_pipeline")
            .with_arguments(serde_json::json!({
                "project_path": "/Users/sac/ggen"
            }).as_object().unwrap().clone()))
        .await?;

    println!("Result: {:?}", result.content);
    Ok(())
}
```

### HTTP Client Example

```rust
use reqwest::Client;
use serde_json::json;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let client = Client::new();
    let response = client
        .post("http://localhost:3000")
        .header("Content-Type", "application/json")
        .json(&json!({
            "jsonrpc": "2.0",
            "method": "tools/call",
            "params": {
                "name": "list_generators",
                "arguments": {}
            },
            "id": 1
        }))
        .send()
        .await?;

    let result: serde_json::Value = response.json().await?;
    println!("Result: {}", result);
    Ok(())
}
```

## OTEL Trace Validation

### Required Spans

All MCP tool calls emit OpenTelemetry spans with these attributes:

**Common Attributes:**
- `service.name = "ggen-mcp-server"`
- `service.version = "6.0.1"`
- `mcp.tool_name = "<tool_name>"`

**Tool-Specific Attributes:**

| Tool | Additional Attributes |
|------|----------------------|
| `generate` | `mcp.ontology_path`, `mcp.files_generated`, `mcp.receipt` |
| `validate` | `mcp.ttl_length`, `mcp.triple_count`, `mcp.error_count` |
| `sync` | `mcp.ontology_path`, `mcp.files_generated`, `mcp.receipt` |
| `query_ontology` | `mcp.sparql_query_length`, `mcp.ttl_length` |
| `validate_pipeline` | `mcp.project_path` |
| `validate_sparql` | `mcp.query_path` |
| `validate_templates` | `mcp.template_path` |
| `fix_cycles` | `mcp.project_path`, strategy`, `dry_run` |

### Verification

Enable trace logging:

```bash
export RUST_LOG=trace,ggen_a2a_mcp=trace
cargo test -p ggen-a2a-mcp --test llm_mcp_a2a_chain -- --nocapture 2>&1 | tee otel_output.txt
```

Verify spans exist:

```bash
grep -E "mcp.tool_name|ggen.mcp.tool_call" otel_output.txt
grep -E "mcp.ontology_path|mcp.files_generated" otel_output.txt
```

## Error Handling

### Error Response Format

```json
{
  "content": [
    {
      "type": "text",
      "text": "Ontology file not found: /path/to/file.ttl"
    }
  ],
  "isError": true
}
```

### Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `Ontology file not found` | Invalid path | Check file exists |
| `Invalid TTL` | Syntax error | Fix Turtle syntax |
| `SPARQL parse error` | Invalid query | Check SPARQL syntax |
| `Template validation failed` | Template error | Fix template syntax |
| `ggen.toml not found` | Not a ggen project | Run in project directory |

## Best Practices

### 1. Use `dry_run` for Preview

Before running full sync:

```typescript
const result = await mcp_client.call_tool({
  name: "sync",
  arguments: {
    ontology_path: "domain.ttl",
    dry_run: true  // Preview only
  }
});
```

### 2. Validate Before Generate

```typescript
// Step 1: Validate ontology
await mcp_client.call_tool({
  name: "validate",
  arguments: { ttl: ontology_content }
});

// Step 2: Validate SPARQL queries
await mcp_client.call_tool({
  name: "validate_sparql",
  arguments: { query_path: "queries/extract.rq" }
});

// Step 3: Validate templates
await mcp_client.call_tool({
  name: "validate_templates",
  arguments: { template_path: "templates/main.rs.tera" }
});

// Step 4: Run quality gates
await mcp_client.call_tool({
  name: "validate_pipeline",
  arguments: { project_path: "." }
});

// Step 5: Generate code
await mcp_client.call_tool({
  name: "generate",
  arguments: { ontology_path: "domain.ttl" }
});
```

### 3. Use Resources for Discovery

```typescript
// List examples
const resources = await mcp_client.list_resources();

// Get example details
const example = await mcp_client.read_resource(
  "ggen://example/basic-rust-project"
);

// Read TTL content
const ttl = await mcp_client.read_resource(
  "ggen://example/basic-rust-project/ttl"
);
```

### 4. Leverage Prompts for LLM Workflows

```typescript
// Explain schema
const explain_prompt = await mcp_client.getPrompt("explain-rdf-schema", {
  ttl_content: ontology_content
});

// Adapt example
const adapt_prompt = await mcp_client.getPrompt("generate-from-example", {
  example_name: "basic-rust-project",
  target_domain: "healthcare"
});

// Scaffold new project
const scaffold_prompt = await mcp_client.getPrompt("scaffold-project", {
  domain: "fintech",
  language: "rust"
});
```

## Integration Examples

### Claude Desktop

Configure in Claude Desktop settings:

```json
{
  "mcpServers": {
    "ggen": {
      "command": "/path/to/ggen",
      "args": ["mcp", "start-server", "--transport", "stdio"]
    }
  }
}
```

### Continue.dev

```javascript
// .continuerc.js
module.exports = {
  tools: [
    {
      name: "ggen",
      command: "/path/to/ggen",
      args: ["mcp", "start-server", "--transport", "stdio"]
    }
  ]
};
```

### Custom MCP Client

```rust
use ggen_a2a_mcp::ggen_server::GgenMcpServer;

async fn custom_workflow() -> anyhow::Result<()> {
    let server = GgenMcpServer::new();

    // Call multiple tools in sequence
    let validate_result = server.call_tool(/* ... */).await?;
    let generate_result = server.call_tool(/* ... */).await?;

    Ok(())
}
```

## Performance Considerations

- **Sync Pipeline**: ~2-5 seconds for typical projects
- **Validation**: <500ms for single file
- **Query Execution**: <200ms for simple SPARQL SELECT queries
- **Example Listing**: <100ms

## Troubleshooting

### Server Not Starting

```bash
# Check if port is in use
lsof -i :3000

# Use different port
ggen mcp start-server --transport http --port 3001
```

### Tool Not Found

Verify server initialization:

```bash
# Check server info
curl -X POST http://localhost:3000 \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'
```

### OTEL Spans Missing

Enable trace logging:

```bash
export RUST_LOG=trace,ggen_a2a_mcp=trace
ggen mcp start-server --transport stdio
```

## References

- **rmcp Documentation**: https://github.com/lijuncie/rmcp
- **MCP Protocol**: https://modelcontextprotocol.io/
- **ggen Repository**: https://github.com/seanchatmangpt/ggen
- **Example Binaries**: `examples/mcp-a2a-self-hosting/`

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 6.0.1 | 2026-03-31 | Initial documentation |
| 6.0.0 | 2026-03-30 | rmcp 1.3.0 migration |
