# Test Helpers - SHOULD DO Documentation

**File**: `ggen-ai/src/test_helpers.rs`
**Purpose**: Test utilities for Ollama integration testing
**Last Updated**: 2025-10-10

---

## 🎯 Core Purpose

**WHAT IT SHOULD DO**: Provide ergonomic, reusable utilities for integration tests that require real LLM interaction via Ollama, with automatic availability checks, sensible defaults, and conditional test execution.

**NOT**: A runtime utility or production helper. This is exclusively for testing code.

---

## 🏗️ Architectural Intent

### Design Principles

1. **Ergonomic**: Should make test setup trivial (1-2 lines)
2. **Safe**: Should skip tests gracefully when Ollama unavailable
3. **Fast**: Should check availability quickly (<10s timeout)
4. **Consistent**: Should use same configuration across all tests
5. **Explicit**: Should make it clear when using real LLM vs mocks
6. **Isolated**: Should not affect other tests or production code

---

## 📋 Function Contracts

### check_ollama_availability()

**SHOULD DO**:
1. Create test LLM config with qwen3-coder:30b model
2. Attempt to create OllamaClient
3. If client creation succeeds:
   - Send minimal test prompt ("Hello")
   - Set short timeout (10 seconds)
   - Wait for response
   - Return true if successful
4. If client creation or completion fails:
   - Return false (not available)
5. Never panic or throw errors
6. Complete within 10 seconds maximum

**RETURN VALUE**:
- `true`: Ollama is running and qwen3-coder:30b is available
- `false`: Ollama not available or model not loaded

**SHOULD NOT**:
- Take longer than 10 seconds
- Panic on connection failure
- Leave hanging connections
- Require user input
- Print error messages (silent check)

**USE CASES**:
```rust
// Manual check
if !check_ollama_availability().await {
    println!("Skipping test: Ollama not available");
    return;
}

// Via macro
skip_if_ollama_unavailable!();
```

---

### skip_if_ollama_unavailable! Macro

**SHOULD DO**:
1. Call check_ollama_availability().await
2. If false:
   - Print user-friendly skip message
   - Return early from test function
3. If true:
   - Continue test execution silently

**SKIP MESSAGE SHOULD**:
- Be informative ("⏭️  Skipping test: Ollama not available")
- Use Unicode symbols for visibility
- Explain why test was skipped
- Not alarm developers (this is expected behavior)

**SHOULD NOT**:
- Fail the test (skip, don't error)
- Be verbose when Ollama IS available
- Require arguments
- Work outside of test functions

**USAGE**:
```rust
#[tokio::test]
async fn test_ollama_generation() {
    skip_if_ollama_unavailable!();

    // Test code runs only if Ollama available
    let client = create_test_ollama_client().unwrap();
    // ...
}
```

---

### create_test_ollama_client()

**SHOULD RETURN**:
- `Result<OllamaClient, GgenAiError>`
- Configured for qwen3-coder:30b
- Using create_test_llm_config() defaults

**CONFIGURATION**:
- Model: qwen3-coder:30b (30B parameter code model)
- Max tokens: 100 (short responses for tests)
- Temperature: 0.1 (low randomness, deterministic)
- Top-p: None (use temperature only)
- Stop: None (no custom stop sequences)

**SHOULD USE**:
- Standard Ollama connection (default host/port)
- Minimal resource configuration
- Fast inference settings

**SHOULD NOT**:
- Require environment variables
- Use different model than other test helpers
- Create client if Ollama not available
- Leave hanging connections

---

### create_test_llm_config()

**SHOULD RETURN**:
- LlmConfig struct with test-appropriate settings

**CONFIGURATION**:
- `model`: "qwen3-coder:30b" (consistent across all tests)
- `max_tokens`: Some(100) (prevent long responses in tests)
- `temperature`: Some(0.1) (mostly deterministic)
- `top_p`: None (not needed with low temperature)
- `stop`: None (no custom stop sequences)
- `extra`: Empty HashMap (no custom parameters)

**WHY THESE VALUES**:
- **qwen3-coder:30b**: Good code generation capability, reasonable size
- **100 tokens**: Enough for meaningful test responses, not wasteful
- **0.1 temperature**: Mostly deterministic while allowing some variation
- **No top_p**: Simpler configuration, temperature is sufficient

**SHOULD NOT**:
- Use production settings (longer max_tokens, higher temp)
- Require configuration file
- Use hardcoded API keys
- Change model between test runs

---

## 🧪 Testing Contract

### Test Coverage SHOULD Include

**For check_ollama_availability**:
- ✅ Returns true when Ollama running
- ✅ Returns false when Ollama not running
- ✅ Completes within timeout
- ✅ Doesn't panic on connection failure

**For skip_if_ollama_unavailable**:
- ✅ Skips test when Ollama unavailable
- ✅ Continues test when Ollama available
- ✅ Prints appropriate message

**For create_test_ollama_client**:
- ✅ Returns valid client
- ✅ Client can complete simple prompts
- ✅ Uses correct configuration

---

## 📝 Usage Patterns

### Pattern 1: Simple Integration Test

```rust
#[tokio::test]
async fn test_generate_code() {
    skip_if_ollama_unavailable!();

    let client = create_test_ollama_client().unwrap();
    let result = client.complete("Write hello world in Rust").await;

    assert!(result.is_ok());
    assert!(result.unwrap().content.contains("fn main"));
}
```

**SHOULD DO**:
- Check availability first
- Use helper to create client
- Make minimal assertions
- Complete quickly

---

### Pattern 2: Manual Availability Check

```rust
#[tokio::test]
async fn test_conditional_behavior() {
    let ollama_available = check_ollama_availability().await;

    if ollama_available {
        // Use real Ollama
        let client = create_test_ollama_client().unwrap();
        // ... real integration test
    } else {
        // Use mock client
        let client = MockClient::new();
        // ... mock-based test
    }
}
```

**SHOULD DO**:
- Provide fallback behavior
- Test both paths (real and mock)
- Document why dual path needed

---

### Pattern 3: Custom Configuration

```rust
#[tokio::test]
async fn test_with_custom_config() {
    skip_if_ollama_unavailable!();

    let mut config = create_test_llm_config();
    config.max_tokens = Some(500); // Longer response needed
    config.temperature = Some(0.0); // Fully deterministic

    let client = OllamaClient::new(config).unwrap();
    // ... test with custom settings
}
```

**SHOULD DO**:
- Start with standard config
- Modify only what's needed
- Document why customization needed

---

## 🚀 Future Evolution Intent

### Phase 1: Basic Helpers (Current)
- Availability check
- Skip macro
- Standard client creation

### Phase 2: Enhanced Helpers (Next)
**SHOULD ADD**:
- Multiple model support (different sizes)
- Response caching (avoid duplicate LLM calls)
- Custom timeout configuration
- Retry logic for transient failures
- Performance benchmarking utilities

### Phase 3: Advanced Testing (Future)
**SHOULD ADD**:
- Mock response recording/playback
- Test data generation utilities
- Performance regression detection
- Cost tracking for LLM usage
- Parallel test execution helpers

### Phase 4: CI/CD Integration (Long-term)
**SHOULD ADD**:
- GitHub Actions setup helpers
- Docker container management
- Model pre-loading utilities
- Test result analytics
- Automated model version updates

---

## 📊 Best Practices

### DO:
✅ Use skip_if_ollama_unavailable!() at start of integration tests
✅ Use create_test_ollama_client() for consistent configuration
✅ Keep test timeouts short (prefer <30s total)
✅ Use deterministic settings (low temperature)
✅ Check availability before creating clients
✅ Print informative messages when skipping
✅ Document why test requires real LLM

### DON'T:
❌ Use in production code
❌ Create clients without availability check
❌ Use high temperature in tests (non-deterministic)
❌ Set long timeouts (>1 minute)
❌ Fail tests when Ollama unavailable
❌ Use different models across tests
❌ Hardcode Ollama connection details

---

## 🔧 Refactoring Guidance

When refactoring this file, preserve these key behaviors:

1. **Automatic skipping** - Tests should skip gracefully when Ollama unavailable
2. **Fast checks** - Availability check should complete in <10s
3. **Consistent config** - All tests should use same model and settings
4. **Simple API** - Creating test client should be 1 line
5. **No panics** - Availability check should never panic

Improve these areas:

1. **Add caching** - Cache availability check result for 5 minutes
2. **Add retries** - Retry availability check once on timeout
3. **Add logging** - Optionally log when tests are skipped
4. **Add metrics** - Track how often tests are skipped
5. **Support multiple models** - Add helpers for different model sizes
6. **Add response mocking** - Record/replay responses for faster tests
7. **Add setup verification** - Helper to verify Ollama is properly configured
8. **Add documentation** - Example usage in module doc comments

---

## 🎯 Success Criteria

Well-implemented test helpers SHOULD:

✅ **Ergonomic**: Setting up test client takes 1 line
✅ **Reliable**: Availability check is accurate 100% of time
✅ **Fast**: Availability check completes in <10 seconds
✅ **Safe**: Never panic or fail tests due to missing Ollama
✅ **Consistent**: All tests use same configuration
✅ **Documented**: Clear examples in module docs
✅ **Tested**: Helpers themselves have test coverage

---

## 📝 Code Quality Standards

**Functions SHOULD**:
- Be <30 lines
- Have clear names describing purpose
- Include doc comments with examples
- Return Result or bool (not panic)
- Use async/await consistently

**Macros SHOULD**:
- Be simple (expand to 1-3 lines)
- Have clear error messages
- Document usage in doc comment
- Work only in async test contexts

**Configuration SHOULD**:
- Use const values where possible
- Document why each value was chosen
- Be easy to customize per-test
- Have sensible defaults

---

**END OF SHOULD DO DOCUMENTATION**
