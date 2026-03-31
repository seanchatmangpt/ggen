# MCP Client Examples for ggen

This directory contains working examples demonstrating how to call ggen MCP tools via the rmcp protocol.

## Examples

### 1. mcp_generate_example.rs

Demonstrates calling the `generate` tool via MCP protocol.

**What it does:**
- Shows JSON-RPC request format for the `generate` tool
- Displays expected response format (success and error cases)
- Provides usage instructions for MCP server and direct CLI

**Usage:**
```bash
cargo run --example mcp_generate_example
```

**JSON-RPC Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "generate",
    "arguments": {
      "ontology_path": "/Users/sac/ggen/.specify/specs/016-self-play/ggen-meta.ttl",
      "output_dir": "/tmp/mcp_generate_test_output",
      "language": "rust"
    }
  }
}
```

**Expected Response (Success):**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Generated N file(s) in Xms\nFiles: path1.rs, path2.rs, ...\nReceipt: sha256hash..."
      }
    ]
  },
  "id": 1
}
```

### 2. mcp_sync_example.rs

Demonstrates calling the `sync` tool via MCP protocol.

**What it does:**
- Shows JSON-RPC request format for the `sync` tool
- Displays expected response format (full sync and dry-run modes)
- Provides usage instructions for MCP server and direct CLI
- Explains key differences between `generate` and `sync` tools

**Usage:**
```bash
cargo run --example mcp_sync_example
```

**JSON-RPC Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "sync",
    "arguments": {
      "ontology_path": "/Users/sac/ggen/.specify/specs/016-self-play/ggen-meta.ttl",
      "output_dir": "/tmp/mcp_sync_test_output",
      "language": "rust",
      "dry_run": false
    }
  }
}
```

**Expected Response (Success):**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Sync (full) complete: N file(s) in Xms\nFiles: path1.rs, path2.rs, ...\nReceipt: sha256hash..."
      }
    ]
  },
  "id": 1
}
```

## How to Use with Actual MCP Server

### Step 1: Start the ggen MCP Server

In one terminal, start the MCP server with stdio transport:

```bash
cd /Users/sac/ggen
cargo run -p ggen-a2a-mcp -- mcp start-server --transport stdio
```

### Step 2: Call MCP Tools via rmcp Client

In your Rust code, use the rmcp client library:

```rust
use rmcp::{
    client::{ClientBuilder, ClientError},
    model::CallToolRequestParams,
};
use serde_json::json;

#[tokio::main]
async fn main() -> Result<(), ClientError> {
    // Connect to MCP server via stdio
    let (client, _) = ClientBuilder::new()
        .connect(stdio())
        .await?;

    // Call the generate tool
    let params = CallToolRequestParams {
        name: "generate".to_string(),
        arguments: Some(json!({
            "ontology_path": "/path/to/ontology.ttl",
            "output_dir": "/path/to/output",
            "language": "rust"
        })),
    };

    let result = client.call_tool(params).await?;

    // Process result
    if let Some(content) = result.content.first() {
        println!("{}", content.text);
    }

    Ok(())
}
```

## Alternative: Direct ggen CLI

You can also call the tools directly via the ggen CLI:

```bash
# Generate code from ontology
cargo run --bin ggen -- sync \
  --ontology /Users/sac/ggen/.specify/specs/016-self-play/ggen-meta.ttl \
  --output /tmp/ggen_output \
  --language rust

# Sync with dry-run (preview mode)
cargo run --bin ggen -- sync \
  --ontology /Users/sac/ggen/.specify/specs/016-self-play/ggen-meta.ttl \
  --output /tmp/ggen_output \
  --dry-run
```

## Tool Parameters

### generate Tool

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `ontology_path` | string | Yes | Path to RDF/Turtle ontology file (.ttl) |
| `queries_dir` | string | No | Directory containing SPARQL queries (default: `queries/` beside ontology) |
| `output_dir` | string | No | Output directory (default: `generated/` beside ontology) |
| `language` | string | No | Target language: rust, go, python, typescript, elixir, auto (default: auto) |

### sync Tool

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `ontology_path` | string | Yes | Path to RDF/Turtle ontology file (.ttl) |
| `queries_dir` | string | No | Directory containing SPARQL queries (default: `queries/` beside ontology) |
| `output_dir` | string | No | Output directory (default: `generated/` beside ontology) |
| `language` | string | No | Target language: rust, go, python, typescript, elixir, auto (default: auto) |
| `dry_run` | boolean | No | Preview mode - no files written (default: false) |

## Key Differences: generate vs sync

### generate
- Runs μ₁-μ₅ pipeline with validation enabled
- Forces regeneration of all files
- **Use when:** Initial generation or complete rebuild needed

### sync
- Runs μ₁-μ₅ pipeline with validation enabled
- Detects changes and regenerates only affected files
- Supports dry-run mode for preview
- **Use when:** Iterative development with incremental updates

## Test Ontology

The examples use a small test ontology located at:
```
/Users/sac/ggen/.specify/specs/016-self-play/ggen-meta.ttl
```

This ontology defines ggen as both an A2A Agent and MCP Server, including:
- Agent skills (ontology validation, code generation, template rendering, quality validation)
- MCP tools (generate, sync, validate, fix_cycles)
- Schema types for generation requests and results

## Output Examples

When the tools run successfully, they return:

1. **Files generated:** List of generated file paths
2. **Receipt:** SHA256 hash for reproducibility
3. **Timing:** Elapsed time in milliseconds
4. **Quality report:** Number of quality gates passed

Example output:
```
Generated 3 file(s) in 1234ms
Files: /tmp/output/src/lib.rs, /tmp/output/src/main.rs, /tmp/output/Cargo.toml
Receipt: abc123def456...
Soundness violations: none
```

## Error Handling

Both tools return structured error responses:

```json
{
  "jsonrpc": "2.0",
  "error": {
    "code": -32602,
    "message": "Ontology file not found: /path/to/file.ttl"
  },
  "id": 1
}
```

Common errors:
- **File not found:** Check ontology_path exists
- **Invalid TTL:** Check ontology syntax with `validate` tool first
- **Template errors:** Check templates are valid Tera syntax
- **Permission errors:** Ensure output directory is writable

## Further Reading

- [ggen MCP Server Documentation](/Users/sac/ggen/crates/ggen-a2a-mcp/README.md)
- [rmcp Protocol Documentation](https://docs.rs/rmcp/)
- [A2A Protocol Specification](/Users/sac/ggen/crates/a2a-generated/README.md)
