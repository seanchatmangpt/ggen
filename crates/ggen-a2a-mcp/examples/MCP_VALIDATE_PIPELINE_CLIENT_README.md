# MCP validate_pipeline Tool Client Example

This example demonstrates how to call the `validate_pipeline` MCP tool via the rmcp protocol.

## Location

- **Source**: `/Users/sac/ggen/crates/ggen-a2a-mcp/examples/mcp_validate_pipeline_client.rs`
- **Binary**: `/Users/sac/ggen/crates/ggen-a2a-mcp/examples/mcp_validate_pipeline_bin.rs`

## What It Does

1. Creates a `GgenMcpServer` instance
2. Calls the `validate_pipeline` tool with `project_path="."` (current ggen project)
3. Prints the 6 quality gate validation results

## Key Components

### 1. GgenMcpServer

The `GgenMcpServer` is the official rmcp 1.3.0 MCP server for ggen tooling. It exposes 14 tools including `validate_pipeline`.

```rust
use ggen_a2a_mcp::ggen_server::GgenMcpServer;

let server = Arc::new(GgenMcpServer::new());
```

### 2. CallToolRequest

MCP tools are called using `CallToolRequest` with tool name and parameters:

```rust
use rmcp::model::CallToolRequest;

let params = serde_json::json!({
    "project_path": "."  // Path to ggen project directory
});

let request = CallToolRequest::new(
    "validate_pipeline".to_string(),
    Some(params)
);
```

### 3. Direct Tool Call

For in-memory testing, tools can be called directly without transport layer:

```rust
use ggen_a2a_mcp::ggen_server::{ValidatePipelineParams};
use rmcp::handler::server::wrapper::Parameters;

let params = ValidatePipelineParams {
    project_path: ".".to_string(),
};

let result = server
    .validate_pipeline(Parameters(params))
    .await?;
```

### 4. Quality Gate Results

The `validate_pipeline` tool runs 6 quality gates:

1. **Manifest Schema Validation** - Validates `ggen.toml` schema
2. **Ontology Dependency Validation** - Checks TTL file imports
3. **SPARQL Query Validation** - Validates `.rq` query syntax
4. **Template Validation** - Validates `.tera` template syntax
5. **File Permissions Check** - Verifies file access permissions
6. **Generation Rules Validation** - Checks generation rule definitions

## Example Output

### Successful Validation

```
🔧 MCP Client Example: validate_pipeline Tool Call
==================================================

✅ Created GgenMcpServer instance
✅ Server info: ggen v0.1.0

📂 Project path: /Users/sac/ggen

🔨 Calling validate_pipeline tool...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
QUALITY GATE VALIDATION RESULTS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✅ VALIDATION PASSED

✅ All quality gates passed (6/6 checkpoints)

Passed checks:
  ✓ manifest_schema_validation
  ✓ ontology_dependency_validation
  ✓ sparql_query_validation
  ✓ template_validation
  ✓ file_permissions_check
  ✓ generation_rules_validation

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✅ Example complete!
```

### Failed Validation

```
🔧 MCP Client Example: validate_pipeline Tool Call
==================================================

✅ Created GgenMcpServer instance
✅ Server info: ggen v0.1.0

📂 Project path: /Users/sac/ggen

🔨 Calling validate_pipeline tool...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
QUALITY GATE VALIDATION RESULTS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

❌ VALIDATION FAILED

Error details:
Quality gate validation failed:
- Template validation failed: templates/hello.tera:3: unexpected end of template
- SPARQL syntax error: queries/extract.rq:5: syntax error at 'SELECT'

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

## How to Run

### Method 1: Run as Example (Recommended)

```bash
cd /Users/sac/ggen
cargo run -p ggen-a2a-mcp --example mcp_validate_pipeline_client
```

### Method 2: Run as Binary

```bash
cd /Users/sac/ggen/crates/ggen-a2a-mcp
cargo run --example mcp_validate_pipeline_bin
```

### Method 3: Build and Run

```bash
# Build the example
cargo build -p ggen-a2a-mcp --example mcp_validate_pipeline_bin

# Run the compiled binary
./target/debug/examples/mcp_validate_pipeline_bin
```

## Code Structure

### Main Components

1. **Server Creation** - Creates `GgenMcpServer` instance
2. **Tool Call** - Calls `validate_pipeline` with project path
3. **Result Parsing** - Extracts validation results from `CallToolResult`
4. **Output Formatting** - Prints formatted quality gate results

### Error Handling

The example handles several error cases:

- Invalid project path
- Missing `ggen.toml`
- Parse errors in manifest
- Quality gate failures

## Integration with rmcp Protocol

The example demonstrates two ways to call MCP tools:

### 1. Direct Method Call (In-Memory)

Best for testing and integration:

```rust
let params = ValidatePipelineParams {
    project_path: ".".to_string(),
};

let result = server
    .validate_pipeline(Parameters(params))
    .await?;
```

### 2. CallToolRequest (Protocol-Level)

Best for MCP client implementations:

```rust
let request = CallToolRequest::new(
    "validate_pipeline".to_string(),
    Some(serde_json::json!({"project_path": "."}))
);

// Route through tool router
let result = server.call_tool(request, context).await?;
```

## Dependencies

```toml
[dependencies]
# RMCP client for calling MCP tools
rmcp = { version = "1.3.0", features = ["client", "transport-io"] }

# ggen-a2a-mcp server (for GgenMcpServer)
ggen-a2a-mcp = { path = "../../crates/ggen-a2a-mcp" }

# Async runtime
tokio = { workspace = true }

# Error handling
anyhow = { workspace = true }

# Serialization
serde_json = { workspace = true }

# Logging
tracing = { workspace = true }
tracing-subscriber = { version = "0.3", features = ["env-filter", "fmt"] }
```

## Testing

To test the example manually:

1. **Test on valid project**:
   ```bash
   cargo run --example mcp_validate_pipeline_bin
   # Expected: ✅ VALIDATION PASSED
   ```

2. **Test on invalid project**:
   ```bash
   cd /tmp
   mkdir invalid_project
   cd invalid_project
   echo "invalid toml content" > ggen.toml
   cargo run -p ggen-a2a-mcp --example mcp_validate_pipeline_bin
   # Expected: ❌ VALIDATION FAILED
   ```

3. **Test on missing project**:
   ```bash
   cargo run -p ggen-a2a-mcp --example mcp_validate_pipeline_bin -- /nonexistent
   # Expected: Error about missing directory
   ```

## Related Examples

- `examples/query_ontology_direct.rs` - Query ontology MCP tool
- `examples/test_validate_direct.rs` - Validate TTL content tool
- `src/bin/test_validate_pipeline.rs` - Binary for validate_pipeline

## See Also

- [GgenMcpServer Documentation](../../src/ggen_server.rs)
- [MCP Tools Reference](../../README.md#tools)
- [Quality Gates Documentation](../../../ggen-core/src/poka_yoke/quality_gates.rs)
