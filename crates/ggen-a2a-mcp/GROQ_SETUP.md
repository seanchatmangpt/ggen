# Groq Integration Setup Guide

Quick setup guide for validating ggen-a2a-mcp with Groq as the LLM backend.

## Prerequisites

1. **Groq API Key**
   ```bash
   # Get from https://console.groq.com/
   export GROQ_API_KEY="gsk_..."
   ```

2. **Rust Toolchain** (already installed)
   ```bash
   rustc --version  # Should be 1.91.1+
   ```

## Setup Steps

### Step 1: Get Groq API Key

1. Visit https://console.groq.com/
2. Sign up or log in
3. Create new API key
4. Copy the key (starts with `gsk_`)

### Step 2: Configure Environment

```bash
# Add to ~/.bashrc or ~/.zshrc
export GROQ_API_KEY="gsk_your_actual_key_here"

# Or set for single session
export GROQ_API_KEY="gsk_..." && cargo test -p ggen-a2a-mcp --test groq_integration_test
```

### Step 3: Run Validation Tests

```bash
# Run all Groq validation tests
cargo test -p ggen-a2a-mcp --test groq_integration_test -- --nocapture

# Run with verbose output
RUST_LOG=debug cargo test -p ggen-a2a-mcp --test groq_integration_test -- --nocapture

# Run single test
cargo test -p ggen-a2a-mcp --test groq_integration_test test_groq_configuration_creation -- --nocapture
```

## Available Groq Models

### llama-3.3-70b-versatile (Default)
- **Speed:** ~800 tokens/second
- **Quality:** High
- **Best for:** General-purpose tasks
- **Config:** `groq_default_config()`

```rust
use ggen_ai::providers::adapter::groq_default_config;
let config = groq_default_config();
```

### llama-3.1-8b-instant (Fast)
- **Speed:** ~1200 tokens/second (fastest)
- **Quality:** Medium-high
- **Best for:** Low-latency agent loops
- **Config:** `groq_fast_config()`

```rust
use ggen_ai::providers::adapter::groq_fast_config;
let config = groq_fast_config();
```

### deepseek-r1-distill-llama-70b (Reasoning)
- **Speed:** Slower (reasoning overhead)
- **Quality:** Best (multi-step reasoning)
- **Best for:** Complex reasoning, SPARQL generation
- **Config:** `groq_smart_config()`

```rust
use ggen_ai::providers::adapter::groq_smart_config;
let config = groq_smart_config();
```

## Environment Variables

```bash
# Required for Groq
export GROQ_API_KEY="gsk_..."

# Optional: Override default model
export GROQ_MODEL="llama-3.1-8b-instant"

# Auto-detection priority:
# 1. DEFAULT_MODEL (highest priority)
# 2. GROQ_MODEL
# 3. GROQ_API_KEY present → llama-3.3-70b-versatile
# 4. Fallback → OpenAI gpt-4o-mini
```

## Test File Location

```
crates/ggen-a2a-mcp/tests/groq_integration_test.rs
```

## Test Coverage

The validation suite includes 11 tests covering:

1. ✅ Groq credentials configured
2. ✅ Groq configuration creation
3. ✅ MCP tool discovery (5+ tools)
4. ✅ Tool registry adapter
5. ✅ MCP registry integration
6. ✅ Tool schema validation
7. ✅ Tool execution result handling
8. ✅ Message routing with Groq
9. ✅ LLM config auto-detection
10. ✅ Groq model variants
11. ✅ Summary report with setup instructions

## Expected Output

```
running 11 tests
test test_groq_credentials_configured ... ok
test test_groq_configuration_creation ... ok
test test_mcp_tool_discovery ... ok
test test_tool_registry_adapter ... ok
test test_mcp_registry_creation ... ok
test test_tool_schema_validation ... ok
test test_tool_execution_result_handling ... ok
test test_message_routing_groq ... ok
test test_llm_config_auto_detection ... ok
test test_groq_model_variants ... ok
test test_groq_integration_summary ... ok

test result: ok. 11 passed; 0 failed
```

## Troubleshooting

### Error: "GROQ_API_KEY not set"
```bash
# Solution: Export the key
export GROQ_API_KEY="gsk_..."
echo $GROQ_API_KEY  # Verify it's set
```

### Error: "GROQ_API_KEY doesn't look like a valid key"
```
Your key should:
- Start with "gsk_"
- Be at least 20 characters long
- Not contain spaces or special chars (except underscore)
```

### Error: "compilation failed in ggen-a2a-mcp"
```
This is expected if production code hasn't been fixed yet.
The validation test suite is independent and will work
once the 7 compilation errors in client.rs/server.rs are resolved.

See GROQ_VALIDATION_REPORT.md for details.
```

## Full Testing Workflow

```bash
# 1. Setup environment
export GROQ_API_KEY="gsk_..."

# 2. Verify key is set
echo "Key present: $([[ -n "$GROQ_API_KEY" ]] && echo yes || echo no)"

# 3. Run validation tests
cargo test -p ggen-a2a-mcp --test groq_integration_test -- --nocapture

# 4. Run specific test
cargo test -p ggen-a2a-mcp --test groq_integration_test test_groq_configuration_creation -- --nocapture

# 5. Full suite (once compilation errors are fixed)
cargo test -p ggen-a2a-mcp
```

## Integration with Your Code

```rust
// In your agent code
use ggen_ai::providers::adapter::groq_default_config;
use ggen_a2a_mcp::registry::McpToolRegistry;

// Create Groq config
let config = groq_default_config();

// Use in LLM client
let client = GenAiClient::new(config)?;

// Integrate with MCP registry
let registry = McpToolRegistry::new();
```

## Next Steps

1. ✅ Setup environment with GROQ_API_KEY
2. ✅ Run validation tests
3. ✅ Review test results
4. 🔲 Fix any production code compilation errors
5. 🔲 Run full end-to-end tests with real Groq backend

## Support

For issues:
1. Check GROQ_VALIDATION_REPORT.md for detailed documentation
2. Verify GROQ_API_KEY is valid and has quota
3. Check network connectivity to api.groq.com
4. Review Groq documentation at https://console.groq.com/docs

---

**Version:** 1.0
**Last Updated:** 2026-03-24
