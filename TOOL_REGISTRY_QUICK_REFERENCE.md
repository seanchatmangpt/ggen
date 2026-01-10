# Tool Registry - Quick Reference

## Import

```rust
use ggen_ai::{
    Tool, ToolTag, ToolSlo, AuthScope,
    ToolRegistry, REGISTRY,
    Signature,
    dspy::field::{InputField, OutputField},
};
```

## Create a Tool

```rust
// Define signature
let sig = Signature::new("MyTool", "Does something useful")
    .with_input(InputField::new("param", "Description", "String"))
    .with_output(OutputField::new("result", "Output", "String"));

// Create tool
let tool = Tool::new("my_tool", "My Tool", "1.0.0", "Description", sig)
    .with_tag(ToolTag::CodeGeneration)
    .with_tag(ToolTag::Financial)
    .with_author("Team")
    .with_slo(ToolSlo {
        timeout_ms: 5_000,
        max_retries: 3,
        cacheable: true,
    })
    .with_auth_scope(AuthScope::Authenticated);
```

## Register a Tool

```rust
// Write lock for registration
let mut registry = REGISTRY.write()?;
registry.register("my_tool", tool)?;
```

## Find a Tool

```rust
// Read lock for access
let registry = REGISTRY.read()?;

// By ID
let tool = registry.get("my_tool")?;

// Check existence
if registry.contains("my_tool") { ... }

// List all
let all_tools = registry.list();

// By tag
let codegen = registry.list_by_tag(&ToolTag::CodeGeneration);

// By domain
let financial = registry.list_by_domain("financial");

// Count
let count = registry.count();
```

## Validate Input

```rust
let registry = REGISTRY.read()?;
let input = serde_json::json!({
    "param": "value"
});
registry.validate_input("my_tool", &input)?;
```

## Export Tools

```rust
let registry = REGISTRY.read()?;

// For OpenAI
let openai_tools = registry.export_openapi();

// For MCP
let mcp_tools = registry.export_mcp_tools();

// As JSON
let json = registry.export_json()?;
```

## Tool Tags

```
CodeGeneration    - Code generation
Validation        - Data validation
QueryGeneration   - SQL/SPARQL queries
Ontology          - Ontology operations
Template          - Template operations
Caching           - Caching operations
Summarization     - Text summarization
Analysis          - Text analysis
Generation        - Text generation
Translation       - Translation
Financial         - Financial domain
Banking           - Banking domain
Insurance         - Insurance domain
Custom(String)    - Custom tag
```

## Authorization Scopes

```
Public            - No authentication
Authenticated     - Valid credentials
Admin             - Admin credentials
Custom(String)    - Custom scope
```

## SLO Defaults

```rust
ToolSlo {
    timeout_ms: 30_000,     // 30 seconds
    max_retries: 3,
    cacheable: true,
}
```

## Error Handling

```rust
// Validation errors
match registry.register("id", tool) {
    Ok(()) => println!("Registered"),
    Err(e) => eprintln!("Error: {}", e),
}

// Tool not found
match registry.get("nonexistent") {
    Ok(tool) => println!("Found: {}", tool.name),
    Err(e) => eprintln!("Not found: {}", e),
}

// Input validation
match registry.validate_input("tool_id", &input) {
    Ok(()) => println!("Valid"),
    Err(e) => eprintln!("Invalid: {}", e),
}
```

## Thread Safety

```rust
// Multiple readers (concurrent)
{
    let r1 = REGISTRY.read()?;
    let r2 = REGISTRY.read()?;
    // Both can read simultaneously
}

// Exclusive writer
{
    let mut registry = REGISTRY.write()?;
    registry.register("tool", tool)?;
} // Lock released after block
```

## Common Patterns

### Startup Initialization
```rust
pub fn init_tools() -> Result<()> {
    let mut registry = REGISTRY.write()?;

    for tool in create_all_tools() {
        registry.register(&tool.id, tool)?;
    }

    Ok(())
}
```

### Service Endpoint
```rust
pub fn get_openai_tools() -> Result<serde_json::Value> {
    let registry = REGISTRY.read()?;
    Ok(registry.export_openapi())
}
```

### Tool Discovery
```rust
pub fn find_financial_tools() -> Result<Vec<Tool>> {
    let registry = REGISTRY.read()?;
    Ok(registry.list_by_tag(&ToolTag::Financial)
        .into_iter()
        .cloned()
        .collect())
}
```

### Input Pre-validation
```rust
pub fn execute_tool(id: &str, input: &serde_json::Value) -> Result<()> {
    let registry = REGISTRY.read()?;
    registry.validate_input(id, input)?;
    // Execute tool...
    Ok(())
}
```

## Testing

```rust
#[test]
fn test_my_tool() {
    // Create test tool
    let sig = Signature::new("Test", "Test")
        .with_input(InputField::new("in", "Input", "String"))
        .with_output(OutputField::new("out", "Output", "String"));

    let tool = Tool::new("test", "Test", "1.0.0", "Test", sig);

    // Create registry
    let mut registry = ToolRegistry::new();

    // Register
    assert!(registry.register("test", tool).is_ok());

    // Verify
    assert!(registry.contains("test"));
    assert_eq!(registry.count(), 1);
}
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "Tool not found" | Make sure to register before accessing |
| "ID mismatch" | Tool.id must match registry key |
| "Validation failed" | Tool must have inputs/outputs and version with dots |
| "Lock poisoned" | Very rare; indicates panic in lock holder |
| "Input invalid" | Check that input JSON matches required fields |

## Files

| File | Purpose |
|------|---------|
| `/crates/ggen-ai/src/tool.rs` | Tool type definition |
| `/crates/ggen-ai/src/tool_registry.rs` | Registry implementation |
| `/crates/ggen-ai/src/lib.rs` | Library exports |
| `/crates/ggen-ai/examples/financial_tool_registry.rs` | Full example |
| `/TOOL_REGISTRY_GUIDE.md` | Detailed guide |
| `/IMPLEMENTATION_SUMMARY.md` | Implementation details |

## See Also

- TOOL_REGISTRY_GUIDE.md - Full documentation
- examples/financial_tool_registry.rs - Complete example
- CLAUDE.md - Project conventions
