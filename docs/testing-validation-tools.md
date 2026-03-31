# Testing Validation Tools — Chicago TDD Guide

Test strategy and inventory for the 18 integration tests in `crates/ggen-a2a-mcp/tests/validation_e2e.rs`.

## Test Philosophy

All tests follow **Chicago TDD** principles:

- **Real collaborators**: Actual MCP transport (via `tokio::io::duplex`), real file I/O (`tempfile::TempDir`), real Oxigraph parser, real Tera parser, real SPARQL parser
- **No mocks**: No `mockall`, no test doubles, no behavior verification
- **State-based assertions**: Assert on observable results (`is_valid`, `error_count`, response content)
- **AAA pattern**: Arrange / Act / Assert

## Test Infrastructure

### Server Setup
Each test starts a real MCP server connected via in-memory duplex transport:

```rust
async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(4096);
    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    let client = TestClientHandler::default().serve(client_transport).await?;
    Ok(client)
}
```

This ensures tests exercise the full MCP protocol stack, not just function calls.

### Test Data
Tests create real files in `TempDir`:

```rust
let temp_dir = TempDir::new()?;
let manifest_path = temp_dir.path().join("ggen.toml");
std::fs::write(&manifest_path, valid_toml_content)?;
```

## Test Inventory (18 tests)

### Manifest Validation (5 tests)

| Test | What it validates | Key assertion |
|------|-------------------|---------------|
| `test_validate_manifest_parse_success` | Valid ggen.toml with all required fields | `is_valid == true`, no errors |
| `test_validate_manifest_parse_missing_required_field` | ggen.toml missing `project.name` | `is_valid == false`, specific error message |
| `test_validate_manifest_dependencies_success` | Valid manifest with existing referenced files | All dependencies resolve |
| `test_validate_manifest_dependencies_missing_file` | Manifest referencing non-existent ontology file | Dependency failure reported |
| `test_validate_manifest_quality_gates_success` | Manifest passing all quality gates | All gates pass |

### Turtle Validation (4 tests)

| Test | What it validates | Key assertion |
|------|-------------------|---------------|
| `test_validate_ttl_syntax_valid` | Well-formed Turtle with prefixes and triples | `is_valid == true` |
| `test_validate_ttl_syntax_invalid` | Malformed Turtle (missing semicolons, bad URIs) | `is_valid == false`, `error_count > 0` |
| `test_validate_ttl_structure_prefixes` | TTL with multiple @prefix declarations | All prefixes recognized |
| `test_validate_ttl_shacl_conforms` | TTL with SHACL shapes that conform | SHACL validation passes |

### SPARQL Validation (2 tests)

| Test | What it validates | Key assertion |
|------|-------------------|---------------|
| `test_validate_sparql_syntax_valid` | Valid SPARQL SELECT with WHERE clause | `is_valid == true` |
| `test_validate_sparql_syntax_invalid` | Malformed SPARQL (unclosed braces) | `is_valid == false` |

### Template Validation (2 tests)

| Test | What it validates | Key assertion |
|------|-------------------|---------------|
| `test_validate_template_syntax_valid` | Valid Tera template with variables and blocks | `is_valid == true` |
| `test_validate_template_syntax_invalid` | Malformed Tera (unclosed blocks) | `is_valid == false` |

### Pipeline Validation (1 test)

| Test | What it validates | Key assertion |
|------|-------------------|---------------|
| `test_validate_pipeline_full` | Full pipeline with valid manifest, TTL, SPARQL, templates | All 6 gates pass, `overall_status == "pass"` |

### Edge Cases (3 tests)

| Test | What it validates | Key assertion |
|------|-------------------|---------------|
| `test_validate_empty_manifest` | Empty ggen.toml file | Graceful failure, specific error |
| `test_validate_missing_required_params` | Tool called without required params | Error response |
| `test_validate_nonexistent_file_path` | Path to file that doesn't exist | File-not-found error |

### Performance (1 test)

| Test | What it validates | Key assertion |
|------|-------------------|---------------|
| `test_validate_large_ttl_performance` | 1000-triple TTL file | Completes in < 5 seconds |

## OTEL Assertion Pattern

Tests verify OTEL spans are emitted by checking trace output:

```bash
# Run with trace output
RUST_LOG=trace,ggen_a2a_mcp=trace \
  cargo test -p ggen-a2a-mcp --test validation_e2e -- --nocapture \
  2>&1 | tee otel_output.txt

# Verify spans
grep 'mcp.tool.name' otel_output.txt       # Tool was called
grep 'service.name' otel_output.txt        # Server identified
grep 'validation.tools_executed_count' otel_output.txt  # Orchestration ran
```

## Running Tests

```bash
# All validation tests
cargo test -p ggen-a2a-mcp --test validation_e2e

# Single test
cargo test -p ggen-a2a-mcp --test validation_e2e test_validate_ttl_syntax_valid

# With OTEL output
RUST_LOG=trace,ggen_a2a_mcp=trace \
  cargo test -p ggen-a2a-mcp --test validation_e2e -- --nocapture

# Performance test only
cargo test -p ggen-a2a-mcp --test validation_e2e test_validate_large_ttl_performance -- --nocapture
```

## Coverage Targets

| Category | Tests | Coverage |
|----------|-------|----------|
| Manifest validation | 5 | Happy path + missing fields + dependencies + quality gates |
| Turtle validation | 4 | Valid + invalid + prefixes + SHACL |
| SPARQL validation | 2 | Valid + invalid |
| Template validation | 2 | Valid + invalid |
| Pipeline integration | 1 | Full end-to-end |
| Edge cases | 3 | Empty input + missing params + bad paths |
| Performance | 1 | Large input SLO |
| **Total** | **18** | **All tools covered** |

## Adding New Tests

When adding a new validation tool, follow this pattern:

```rust
#[tokio::test]
async fn test_new_tool_happy_path() {
    // Arrange: set up real files
    let mut client = start_server().await.unwrap();
    let temp_dir = TempDir::new().unwrap();
    // ... create test data ...

    // Act: call via MCP protocol
    let result = client
        .call_tool(CallToolRequestParams::new("new_tool".to_string())
            .with_arguments(serde_json::json!({ "param": "value" })))
        .await;

    // Assert: state-based verification
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(!response.is_error);
    // ... verify specific fields ...
}
```
