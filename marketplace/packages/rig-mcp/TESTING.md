# Testing Strategy - No LLM APIs Required

This package can be fully tested WITHOUT spending money on LLM API calls.

## Philosophy

✅ **Test What We Control**: Configuration, tool adaptation, schema conversion
❌ **Don't Test External APIs**: LLM providers, actual MCP servers

## Test Structure

```
tests/
├── config_test.rs        # Configuration loading and parsing
├── adaptor_test.rs       # MCP tool schema conversion
├── mock_mcp_server.rs    # Mock MCP server for testing
└── integration_test.rs   # Full workflow without real APIs
```

## Running Tests

```bash
# Run all tests (no API keys needed!)
cargo test

# Run specific test file
cargo test --test config_test

# Run with output
cargo test -- --nocapture

# Run a specific test
cargo test test_config_loads_from_file
```

## What We Test

### 1. Configuration Loading ✅

```rust
#[tokio::test]
async fn test_config_loads_from_file() {
    let config = Config::retrieve("test_config.toml").await.unwrap();
    assert_eq!(config.mcp.servers.len(), 1);
}
```

**Tests**:
- TOML parsing
- Optional fields (API keys)
- Multiple MCP servers
- Environment variables
- Invalid TOML handling

### 2. MCP Tool Schema Conversion ✅

```rust
#[test]
fn test_mcp_tool_schema_conversion() {
    let mcp_tool = McpTool { /* ... */ };
    let schema = mcp_tool.schema_as_json_value();
    assert!(schema.is_object());
}
```

**Tests**:
- JSON schema conversion
- Missing descriptions
- Complex nested schemas
- Enum values
- Required fields

### 3. Mock MCP Server ✅

```rust
let tools = MockMcpServer::mock_tools_list();
let response = MockMcpServer::mock_tool_call_response("read_file", args);
```

**Tests**:
- Tool list responses
- Tool call responses
- Error handling
- Multiple tools

### 4. Transport Configuration ✅

```rust
#[tokio::test]
async fn test_config_validates_transport_types() {
    // Test stdio, SSE, and HTTP transports
}
```

**Tests**:
- Stdio transport config
- SSE transport config
- HTTP streamable config
- Transport-specific parameters

## What We DON'T Test

### ❌ LLM API Calls
- Actual OpenAI/Anthropic/Cohere requests
- Real token usage
- Model responses

**Reason**: Costs money, requires API keys

### ❌ Real MCP Servers
- Actual server processes
- Network connections
- Tool execution

**Reason**: Requires external dependencies, can be flaky

### ❌ Embeddings Generation
- Vector store creation
- Similarity search

**Reason**: Requires API calls or large models

## Mock Testing Pattern

Instead of testing with real APIs:

```rust
// ❌ DON'T: Real API call
let agent = deepseek_client.agent("deepseek-chat").build();
let response = agent.prompt("test").await?;

// ✅ DO: Test the setup
let config = Config::retrieve("config.toml").await?;
let mcp_manager = config.mcp.create_manager().await?;
// Stop here - don't call LLM APIs
```

## Adding New Tests

When adding features:

1. **Test configuration changes** in `config_test.rs`
2. **Test schema conversions** in `adaptor_test.rs`
3. **Add mock responses** in `mock_mcp_server.rs`
4. **Test workflows** in `integration_test.rs`

## CI/CD Integration

These tests are perfect for CI/CD because:
- ✅ No API keys required
- ✅ Fast execution (< 1 second)
- ✅ Deterministic results
- ✅ No external dependencies
- ✅ No rate limiting

## Example: Testing a New Feature

If adding a new transport type:

```rust
// 1. Add config test
#[tokio::test]
async fn test_websocket_transport_config() {
    let config_content = r#"
    [mcp]
    [[mcp.server]]
    name = "ws-server"
    protocol = "websocket"
    url = "ws://localhost:8080"
    "#;

    let config = load_config(config_content).await.unwrap();
    assert!(matches!(
        config.mcp.servers[0].transport,
        McpServerTransportConfig::WebSocket { .. }
    ));
}

// 2. Add schema test
#[test]
fn test_websocket_connection_params() {
    // Test parameter validation
}

// 3. Add mock
impl MockMcpServer {
    pub fn mock_websocket_connection() -> /* ... */ {
        // Return mock connection
    }
}
```

## Test Coverage Goals

- **Configuration**: 100% (all TOML parsing paths)
- **Schema Conversion**: 100% (all JSON types)
- **Mock Server**: 100% (all tool types)
- **Integration**: 80% (stop before LLM calls)

## Running Tests in Watch Mode

```bash
cargo watch -x 'test --manifest-path marketplace/packages/rig-mcp/Cargo.toml'
```

## Debugging Tests

```bash
# Show all output
cargo test -- --nocapture

# Show failed test output only
cargo test -- --show-output

# Run single test with full output
cargo test test_config_loads_from_file -- --nocapture --exact
```

## Summary

✅ **Complete test coverage WITHOUT LLM APIs**
✅ **Fast, deterministic, CI-friendly**
✅ **Tests what matters: our code, not external APIs**
✅ **Mock external dependencies**

**Result**: Confidence in the package without spending money! 💰
