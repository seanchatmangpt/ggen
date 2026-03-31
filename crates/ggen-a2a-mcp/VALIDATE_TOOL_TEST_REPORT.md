# Validate MCP Tool Test Report

**Date:** 2026-03-31
**Component:** ggen-a2a-mcp
**Tool:** `validate`
**Test Method:** Direct invocation via MCP protocol (not cargo test)

---

## Test Summary

The `validate` MCP tool has been successfully tested directly through the MCP protocol using in-process duplex transport. All tests passed successfully.

---

## Test Infrastructure

### Files Created

1. **`crates/ggen-a2a-mcp/examples/test_validate_direct.rs`**
   - Basic test with valid Turtle content
   - Tests successful validation path

2. **`crates/ggen-a2a-mcp/examples/test_validate_direct_error.rs`**
   - Comprehensive error handling tests
   - Tests 3 scenarios: missing prefix, malformed triple, valid TTL (control)

### Test Architecture

```rust
// MCP Client-Server Pattern
let (server_transport, client_transport) = tokio::io::duplex(65536);
let server = GgenMcpServer::new();
tokio::spawn(async move {
    server.serve(server_transport).await;
});
let client = TestClientHandler::default().serve(client_transport).await?;

// Tool invocation
let result = client
    .call_tool(
        CallToolRequestParams::new("validate")
            .with_arguments(args(serde_json::json!({
                "ttl": ttl_content
            })))
    )
    .await?;
```

---

## Test Results

### Test 1: Valid Turtle Content

**Input:**
```turtle
@prefix ex: <http://example.org/ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Subject a ex:Resource .
ex:Subject ex:name "Test Subject" .
```

**Output:**
```
Is Error: false
Valid Turtle content (2 triple(s) parsed)
```

**Result:** ✅ PASS

---

### Test 2: Missing Prefix (Error Case)

**Input:**
```turtle
ex:Subject a ex:Resource .
```

**Output:**
```
Is Error: true
Invalid TTL (1 error(s)): Parser error at line 2 between columns 1 and 11:
The prefix ex: has not been declared
```

**Result:** ✅ PASS (correct error detection)

---

### Test 3: Malformed Triple (Error Case)

**Input:**
```turtle
@prefix ex: <http://example.org/ns#> .
ex:Subject INVALID TRIPLE HERE
```

**Output:**
```
Is Error: true
Invalid TTL (1 error(s)): Parser error at line 2 between columns 12 and 19:
INVALID is not a valid predicate
```

**Result:** ✅ PASS (correct error detection)

---

### Test 4: Valid TTL Control Test

**Input:**
```turtle
@prefix ex: <http://example.org/ns#> .
ex:Subject a ex:Resource .
```

**Output:**
```
Is Error: false
Valid Turtle content (1 triple(s) parsed)
```

**Result:** ✅ PASS

---

## Implementation Details

### Tool Signature

```rust
#[tool(
    description = "Validate a Turtle (.ttl) ontology string for syntax correctness. Returns 'Valid' or a list of parse errors."
)]
async fn validate(
    &self,
    Parameters(params): Parameters<ValidateParams>,
) -> Result<CallToolResult, McpError>
```

### Parameters

```rust
pub struct ValidateParams {
    /// Turtle (TTL) content string to validate
    pub ttl: String,
}
```

### Response Format

**Success:**
```json
{
  "content": [
    {
      "type": "text",
      "text": "Valid Turtle content (N triple(s) parsed)"
    }
  ],
  "isError": false
}
```

**Error:**
```json
{
  "content": [
    {
      "type": "text",
      "text": "Invalid TTL (N error(s)): <error details>"
    }
  ],
  "isError": true
}
```

---

## OTEL Spans

The validate tool emits the following OpenTelemetry spans:

1. **Span Name:** `ggen.mcp.tool_call`
2. **Attributes:**
   - `operation.name`: "mcp.validate"
   - `mcp.tool_name`: "validate"
   - `mcp.ttl_length`: <length of TTL string>
   - `mcp.triple_count`: <number of triples parsed> (success case)
   - `mcp.error_count`: <number of errors> (failure case)

**Example Trace:**
```
INFO ggen.mcp.tool_call{operation.name="mcp.validate" ttl_len=163}: validate tool called ttl_len=163
INFO ggen.mcp.tool_call{operation.name="mcp.validate" ttl_len=163}: validate tool complete: valid triple_count=2
```

---

## How to Run Tests

### Run Basic Test (Valid TTL)
```bash
cargo run -p ggen-a2a-mcp --example test_validate_direct
```

### Run Error Handling Tests
```bash
cargo run -p ggen-a2a-mcp --example test_validate_direct_error
```

### Expected Output
Both tests should complete in <1 second and show:
- ✅ MCP server and client connected
- ✅ Tool invoked successfully
- ✅ Correct validation results
- ✅ OTEL spans emitted

---

## Conclusion

The `validate` MCP tool is **fully functional** and correctly:

1. ✅ Parses valid Turtle content
2. ✅ Reports accurate triple counts
3. ✅ Detects and reports syntax errors
4. ✅ Provides clear error messages
5. ✅ Emits OTEL spans for observability
6. ✅ Handles edge cases (missing prefixes, malformed triples)

**Status:** Ready for production use

---

## References

- **Tool Implementation:** `/Users/sac/ggen/crates/ggen-a2a-mcp/src/ggen_server.rs:418-474`
- **Test Files:**
  - `/Users/sac/ggen/crates/ggen-a2a-mcp/examples/test_validate_direct.rs`
  - `/Users/sac/ggen/crates/ggen-a2a-mcp/examples/test_validate_direct_error.rs`
- **MCP Protocol:** rmcp 1.3.0
- **Parser:** oxigraph RdfParser (Turtle format)
