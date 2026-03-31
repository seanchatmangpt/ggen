# Development Guide: Getting Started

Contributor guide for developing ggen MCP server features.

## Prerequisites

- Rust 1.91.1+
- Familiarity with async/await and Tokio
- Understanding of MCP protocol basics
- RDF/SPARQL knowledge (for ontology work)

## Development Environment Setup

### 1. Clone Repository

```bash
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen
```

### 2. Install Dependencies

```bash
# Install Rust toolchain
rustup install stable
rustup component add rust-analyzer

# Install development tools
cargo install cargo-make
cargo install cargo-watch
```

### 3. Build Project

```bash
# Full build
cargo make build

# Development build (faster)
cargo make check
```

### 4. Run Tests

```bash
# All tests
cargo make test

# Unit tests only
cargo make test-unit

# With output
cargo test -- --nocapture
```

### 5. Run Linter

```bash
cargo make lint
```

## Project Structure

```
ggen/
├── crates/
│   ├── ggen-a2a-mcp/         # MCP server (16 tools)
│   ├── ggen-transport/       # Transport layer
│   ├── ggen-a2a-registry/    # Agent registry
│   ├── a2a-generated/        # A2A protocol types
│   └── ggen-integration/     # Integration layer
├── examples/                 # Example projects
├── docs/                     # Documentation
└── tests/                    # Integration tests
```

## Development Workflow

### Adding a New Tool

1. **Define Tool Parameters**

```rust
// crates/ggen-a2a-mcp/src/ggen_server.rs

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct MyToolParams {
    pub input: String,
    pub option: Option<u32>,
}
```

2. **Implement Tool Handler**

```rust
#[tool(description = "My tool description")]
pub async fn my_tool(
    &self,
    #[arg(description = "Input value")] input: String,
    #[arg(description = "Optional number")] option: Option<u32>,
) -> Result<String, A2aMcpError> {
    // Implementation
    Ok(format!("Processed: {}", input))
}
```

3. **Add Tests**

```rust
// crates/ggen-a2a-mcp/tests/my_tool_test.rs

#[tokio::test]
async fn test_my_tool() {
    let server = GgenMcpServer::new();
    let result = server.my_tool("test".to_string(), Some(42)).await;
    assert!(result.is_ok());
}
```

4. **Update Documentation**

```markdown
<!-- docs/mcp/02-user-guide/tools/my_tool.md -->
# my_tool Tool

Description of my tool...

## Parameters
...
```

### Running MCP Server Locally

```bash
# Stdio transport (for Claude Desktop)
cargo run -- mcp start-server --transport stdio

# HTTP transport (for testing)
cargo run -- mcp start-server --transport http --port 8080
```

### Testing with MCP Inspector

```bash
npx @modelcontextprotocol/inspector ggen
```

## Code Organization

### File Naming Conventions

| Type | Pattern | Example |
|------|---------|---------|
| Module | `snake_case.rs` | `message_router.rs` |
| Test | `{module}_test.rs` | `message_router_test.rs` |
| Integration | `integration_test.rs` | `a2a_integration_test.rs` |

### Module Structure

```rust
// lib.rs - Public API
pub mod server;
pub mod handlers;
pub mod adapter;

// Re-exports
pub use server::{GgenMcpServer, serve_stdio, serve_http};
```

## Common Patterns

### Error Handling

```rust
// Use Result<T, A2aMcpError>
pub async fn my_function(&self) -> Result<String, A2aMcpError> {
    // Use bail! for early returns
    let value = self.get_value().await?;

    // Use ensure! for validation
    ensure!(value.len() > 0, A2aMcpError::InvalidInput("empty value"));

    Ok(value)
}
```

### Async Context

```rust
// Always propagate cancellation tokens
pub async fn process_with_cancel(
    &self,
    input: &str,
    cancel: CancellationToken,
) -> Result<String, A2aMcpError> {
    tokio::select! {
        result = self.process(input) => { result }
        _ = cancel.cancelled() => {
            Err(A2aMcpError::Cancelled)
        }
    }
}
```

### OTEL Tracing

```rust
use opentelemetry::trace::{Tracer, TraceContextExt};

#[tracing::instrument(skip(self))]
pub async fn traced_function(&self, input: &str) -> Result<String, A2aMcpError> {
    let span = tracing::span!(Level::INFO, "traced_function");

    // Add attributes
    span.record("input_length", input.len());

    // Do work
    let result = self.do_work(input).await?;

    Ok(result)
}
```

## Testing Guidelines

### Unit Tests

```rust
#[tokio::test]
async fn test_unit_function() {
    // Arrange
    let input = "test";

    // Act
    let result = function_under_test(input).await;

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "expected");
}
```

### Integration Tests

```rust
#[tokio::test]
async fn test_tool_call() {
    let server = GgenMcpServer::new();

    // Call tool via MCP protocol
    let request = json!({
        "jsonrpc": "2.0",
        "method": "tools/call",
        "params": {
            "name": "my_tool",
            "arguments": {"input": "test"}
        },
        "id": 1
    });

    let response = server.handle_request(request).await;
    assert!(response.is_ok());
}
```

### Property-Based Tests

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn fn prop_roundtrip(input in "[a-zA-Z0-9]+") {
        let result = roundtrip_function(&input).unwrap();
        assert_eq!(result, input);
    }
}
```

## Debugging

### Enable Trace Logging

```bash
RUST_LOG=trace,ggen_a2a_mcp=trace cargo test -- --nocapture
```

### Debug OTEL Spans

```bash
# Enable OTEL console exporter
RUST_LOG=trace OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317 cargo run
```

### Use Rust Analyzer

```bash
# Install rust-analyzer
rustup component add rust-analyzer

# Open in VS Code
code .
```

## Performance

### Profiling

```bash
# CPU profiling
cargo install flamegraph
cargo flamegraph --bin ggen -- mcp start-server --transport stdio

# Memory profiling
valgrind --tool=massif ./target/release/ggen mcp start-server --transport stdio
```

### Benchmarks

```rust
// benches/tool_bench.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_generate(c: &mut Criterion) {
    c.bench_function("generate_small", |b| {
        b.iter(|| {
            generate_tool(black_box(small_input))
        })
    });
}

criterion_group!(benches, bench_generate);
criterion_main!(benches);
```

## Contributing

### Code Review Checklist

- [ ] Tests pass (`cargo make test`)
- [ ] Lint passes (`cargo make lint`)
- [ ] Documentation updated
- [ ] OTEL spans added (if applicable)
- [ ] Error handling uses `Result<T, E>`

### Commit Message Format

```
feat(mcp): add new tool for X

Add new tool that does Y.

- Implements Z functionality
- Adds tests for edge cases
- Updates documentation

Closes #123
```

## See Also

- [Adding Tools Guide](./adding-tools.md)
- [Testing Guide](./testing/unit-tests.md)
- [Benchmarking Guide](./testing/benchmarks.md)
