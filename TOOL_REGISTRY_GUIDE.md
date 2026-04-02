# Tool Registry Implementation Guide

## Overview

The Tool Registry provides centralized discovery and management of agent tools in ggen. It enables agents (DSPy, MCP, GenAI) to discover and invoke tools through a unified interface.

**Key Components:**
1. `Tool` - Complete tool definition with metadata and validation
2. `ToolRegistry` - Thread-safe registry for tool discovery
3. `REGISTRY` - Global singleton for runtime tool management
4. Export formats - OpenAPI and MCP for multi-agent integration

## Architecture

```
┌─────────────────────────────────────────────────┐
│ Agent Layer (DSPy, MCP, HTTP, Direct Rust API)│
└────────────────┬────────────────────────────────┘
                 │
                 ▼
         ┌──────────────────┐
         │  REGISTRY        │
         │ (Arc<RwLock>)    │
         └────────┬─────────┘
                  │
         ┌────────▼─────────┐
         │  ToolRegistry    │
         │  (thread-safe)   │
         └────────┬─────────┘
                  │
    ┌─────────────┼─────────────┐
    │             │             │
    ▼             ▼             ▼
  Tool1        Tool2         ToolN
  (Signature)  (Signature)  (Signature)
```

## Core Types

### Tool

A complete tool definition combining a DSPy Signature with operational metadata.

```rust
pub struct Tool {
    pub id: String,                      // Unique identifier
    pub name: String,                    // Human-readable name
    pub version: String,                 // Semantic version (e.g., "1.0.0")
    pub description: String,             // Detailed description
    pub signature: Signature,            // Input/output contract
    pub tags: Vec<ToolTag>,             // Discovery tags
    pub author: Option<String>,          // Tool author
    pub documentation_url: Option<String>,
    pub slo: ToolSlo,                   // Service level objectives
    pub auth_scope: AuthScope,          // Authorization requirements
    pub metadata: HashMap<String, String>,
    pub examples: Vec<ToolExample>,     // Usage examples
}
```

### ToolRegistry

Thread-safe registry for tool management.

```rust
pub struct ToolRegistry {
    tools: HashMap<String, Tool>,
}
```

### Global REGISTRY

Accessible via `REGISTRY: once_cell::sync::Lazy<Arc<RwLock<ToolRegistry>>>`.

## Usage Patterns

### 1. Basic Tool Registration

```rust
use ggen_ai::{
    dspy::{field::{InputField, OutputField}, Signature},
    tool::{Tool, ToolTag},
    tool_registry::REGISTRY,
};

fn register_financial_tool() -> Result<()> {
    // Define signature
    let sig = Signature::new("ValidateTransaction", "Validates transactions")
        .with_input(InputField::new("amount", "Transaction amount", "String"))
        .with_output(OutputField::new("valid", "Is valid", "bool"));

    // Create tool
    let tool = Tool::new("validate_tx", "Validator", "1.0.0", "Validates transactions", sig)
        .with_tag(ToolTag::Validation)
        .with_tag(ToolTag::Financial);

    // Register
    let mut registry = REGISTRY.write()?;
    registry.register("validate_tx", tool)?;

    Ok(())
}
```

### 2. Discovering Tools by Domain

```rust
// Find all financial tools
let registry = REGISTRY.read()?;
let financial_tools = registry.list_by_tag(&ToolTag::Financial);
for tool in financial_tools {
    println!("Tool: {} ({})", tool.id, tool.description);
}
```

### 3. Input Validation

```rust
let registry = REGISTRY.read()?;
let input = serde_json::json!({
    "amount": "1500.50",
    "currency": "USD"
});

// Validates against tool's signature
registry.validate_input("validate_tx", &input)?;
```

### 4. Export for OpenAI Function Calls

```rust
let registry = REGISTRY.read()?;
let tools = registry.export_openapi();
// Returns JSON array suitable for OpenAI API
// [
//   {
//     "type": "function",
//     "function": {
//       "name": "validate_tx",
//       "description": "Validates transactions",
//       "parameters": {...}
//     }
//   }
// ]
```

### 5. Export for MCP

```rust
let registry = REGISTRY.read()?;
let mcp_tools = registry.export_mcp_tools();
// Returns JSON array for Model Context Protocol
// [
//   {
//     "name": "validate_tx",
//     "description": "Validates transactions",
//     "inputSchema": {...}
//   }
// ]
```

## Tool Metadata

### ToolTag

Categorizes tools for discovery:
- `CodeGeneration` - Code generation
- `Validation` - Data validation
- `QueryGeneration` - SPARQL/SQL queries
- `Financial`, `Banking`, `Insurance` - Domain-specific
- `Custom(String)` - User-defined tags

### ToolSlo

Service level objectives:
```rust
pub struct ToolSlo {
    pub timeout_ms: u64,        // Execution timeout
    pub max_retries: u32,       // Retry limit
    pub cacheable: bool,        // Can cache results
}
```

### AuthScope

Authorization requirements:
- `Public` - No authentication
- `Authenticated` - Valid credentials required
- `Admin` - Admin credentials required
- `Custom(String)` - Custom scope

## Error Handling

All operations return `Result<T>`:

```rust
match registry.register("my_tool", tool) {
    Ok(()) => println!("Registered"),
    Err(e) => eprintln!("Failed: {}", e),
}
```

**Common Errors:**
- `ValidationError` - Tool validation failed
- `ValidationError` - Tool not found
- `ValidationError` - Input validation failed

## Thread Safety

The registry uses `Arc<RwLock<>>` for safe concurrent access:

```rust
// Multiple readers
{
    let registry = REGISTRY.read()?;
    // Can read simultaneously
}

// Exclusive writer
{
    let mut registry = REGISTRY.write()?;
    // Exclusive access for registration
}
```

## Real-World Example: Financial Domain

See `examples/financial_tool_registry.rs` for a complete example including:
- Transaction validation tool
- Risk analysis tool
- Portfolio optimization tool
- Compliance checking tool

Run with:
```bash
cargo run --example financial_tool_registry --features openai-integration
```

## Testing

Chicago TDD pattern with real objects (no mocks):

```rust
#[test]
fn test_tool_registration() {
    let mut registry = ToolRegistry::new();
    let tool = create_test_tool("test_tool");

    assert!(registry.register("test_tool", tool).is_ok());
    assert!(registry.contains("test_tool"));
}
```

Run tests:
```bash
cargo test --lib tool::tests -- --nocapture
cargo test --lib tool_registry::tests -- --nocapture
```

## Integration Points

### Direct Rust API
```rust
let registry = REGISTRY.read()?;
let tool = registry.get("my_tool")?;
```

### HTTP Endpoints (Future)
```
GET /tools              # List all tools
GET /tools/:id          # Get specific tool
POST /tools/:id/invoke  # Execute tool
GET /tools/openapi      # Export for OpenAI
GET /tools/mcp          # Export for MCP
```

### MCP Server (Future)
```rust
impl MCPServer for ToolRegistry {
    fn list_resources(&self) -> Vec<Resource> {
        // Return tools as MCP resources
    }
}
```

### DSPy Integration
```rust
impl Into<DSPyModule> for Tool {
    fn into(self) -> DSPyModule {
        // Wrap tool as DSPy module
    }
}
```

## Design Principles

1. **Type Safety** - Constraints in types, compiler verifies invariants
2. **Thread Safe** - Arc<RwLock<>> prevents data races
3. **Error Handling** - Result<T, E> throughout, no unwrap/expect
4. **Zero Cost** - No runtime overhead for abstractions
5. **Deterministic** - Same input always produces same output
6. **Composable** - Tools easily combined in pipelines

## Performance

- **Lookup** - O(1) hash map lookup
- **Registration** - O(1) amortized
- **Discovery** - O(n) where n = tool count
- **Memory** - ~1KB per tool + signature
- **Concurrency** - Multiple readers, exclusive writers

## Future Enhancements

1. **Tool Versioning** - Support multiple versions of same tool
2. **Lazy Loading** - Load tools on-demand from external sources
3. **Caching** - Cache tool lookup results
4. **Metrics** - Track tool invocation counts and latencies
5. **Rate Limiting** - Per-tool rate limiting support
6. **Webhook Hooks** - On registration/deregistration events
7. **Dependency Graph** - Track tool dependencies
8. **Cost Tracking** - Monitor tool execution costs

## Troubleshooting

### Tool not found
```rust
// Make sure tool is registered before access
let mut reg = REGISTRY.write()?;
reg.register("tool_id", tool)?;

let reg = REGISTRY.read()?;
let tool = reg.get("tool_id")?; // Now succeeds
```

### Validation errors
```rust
// Tool must have at least one input and output
let sig = Signature::new("Test", "Test")
    .with_input(InputField::new("in", "Input", "String"))
    .with_output(OutputField::new("out", "Output", "String"));
```

### Lock contention
```rust
// Keep locks scoped to minimize contention
{
    let mut reg = REGISTRY.write()?;
    reg.register("tool", tool)?;
} // Lock released here

// Now other threads can access
```

## References

- DSPy: https://github.com/stanfordnlp/dspy
- Model Context Protocol: https://modelcontextprotocol.io/
- OpenAI Functions: https://platform.openai.com/docs/guides/function-calling
- JSON Schema: https://json-schema.org/
