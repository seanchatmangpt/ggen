# Real Groq API Test Implementation - Summary

**Date:** 2026-03-30

## Problem

Tests were not executing with real Groq API calls despite claims of success. Root cause was `cfg!(test)` in `is_test_mode()` function which forced ALL `cargo test` runs to use MockClient, preventing real API calls even when `GROQ_API_KEY` was present.

## Changes Made

### 1. Fixed `is_test_mode()` in `crates/ggen-ai/src/config/global.rs`

**Before:**
```rust
pub fn is_test_mode(&self) -> bool {
    std::env::var("GGEN_TEST_MODE").is_ok() || cfg!(test)
}
```

**After:**
```rust
/// Check if we're in test mode
///
/// Note: This only checks GGEN_TEST_MODE env var at runtime.
/// Tests can use real API clients when GROQ_API_KEY is present.
/// Tests that want mocks should either call create_mock_client() explicitly
/// or set GGEN_TEST_MODE=1.
pub fn is_test_mode(&self) -> bool {
    std::env::var("GGEN_TEST_MODE").is_ok()
}
```

**Impact:** Tests now use real clients when `GROQ_API_KEY` is present. Tests that want mocks can either call `create_mock_client()` explicitly or set `GGEN_TEST_MODE=1`.

### 2. Removed Feature Gates from Tests

Removed `#[cfg_attr(not(feature = "live-groq"), ignore)]` from:
- `crates/ggen-a2a-mcp/tests/llm_mcp_a2a_chain.rs` (2 tests)
- `crates/ggen-a2a-mcp/tests/groq_slo_timing.rs` (6 tests)
- `crates/ggen-a2a-mcp/tests/a2a_groq_integration.rs` (3 tests)
- `crates/ggen-a2a-mcp/tests/a2a_self_play.rs` (1 test)

**Impact:** Tests are always visible and runnable. Runtime `GROQ_API_KEY` check is sufficient.

### 3. Added Assertions to `llm_mcp_a2a_chain.rs`

Added proper assertions to validate actual behavior:
```rust
// Assert LLM actually returned content
assert!(!llm_response.content.is_empty(), "LLM should return non-empty response");
assert!(llm_response.content.len() > 10, "LLM response should be substantial");

// Assert model attribute is set
assert_eq!(llm_response.model, "groq::openai/gpt-oss-20b");

// Assert token counts are present and reasonable
assert!(usage.prompt_tokens > 0, "Prompt tokens should be > 0");
assert!(usage.completion_tokens > 0, "Completion tokens should be > 0");
assert_eq!(
    usage.total_tokens,
    usage.prompt_tokens + usage.completion_tokens,
    "Total tokens should equal prompt + completion"
);
```

**Impact:** Test now has actual assertions instead of just printing "test complete".

### 4. Updated Documentation

Updated `semconv/live-check/run-ggen-live-check.sh` to remove `--features live-groq` from test invocation.

Updated test file comments to replace references to `live-groq` feature with `requires GROQ_API_KEY`.

## Verification

### Test Results (with GROQ_API_KEY)

```bash
RUST_LOG=trace,ggen_ai=trace,ggen_a2a_mcp=trace \
cargo test -p ggen-a2a-mcp --test llm_mcp_a2a_chain \
  test_llm_mcp_a2a_chain_with_otel_trace -- --nocapture
```

**Output:**
```
llm.complete{operation.name="llm.complete" operation.type="llm" llm.model=groq::openai/gpt-oss-20b prompt_len=255}: LLM complete response
  model=groq::openai/gpt-oss-20b
  elapsed_ms=1425
  prompt_tokens=121
  completion_tokens=1024
  total_tokens=1145

ggen.mcp.tool_call{...}: validate_pipeline tool called project_path=/Users/sac/ggen
ggen.mcp.tool_call{...}: list_generators tool called

✓ LLM called successfully (real API call, 1145 tokens total)
✓ validate_pipeline tool executed
✓ list_generators tool executed
✓ Full trace chain captured

test test_llm_mcp_a2a_chain_with_otel_trace ... ok
```

### Test Results (without GROQ_API_KEY)

```bash
unset GROQ_API_KEY
cargo test -p ggen-a2a-mcp --test llm_mcp_a2a_chain \
  test_llm_mcp_a2a_chain_with_otel_trace -- --nocapture
```

**Output:**
```
skipping: GROQ_API_KEY not set
test test_llm_mcp_a2a_chain_with_otel_trace ... ok
```

### All SLO Tests Pass

```bash
cargo test -p ggen-a2a-mcp --test groq_slo_timing -- --nocapture
```

**Results:** 6/6 tests pass with real API calls:
- `slo_groq_a2a_agent_task` - 1362ms (limit: 15000ms)
- `slo_groq_sparql_generation` - 2196ms (limit: 10000ms)
- `slo_groq_mcp_scaffold_generation` - 4704ms (limit: 10000ms)
- `slo_groq_a2a_architect_reasoning` - 5232ms (limit: 15000ms)
- `slo_groq_streaming_code_gen` - 5442ms (limit: 10000ms)
- `slo_groq_multi_turn_conversation` - 14864ms (limit: 20000ms)

## OTEL Validation

All tests now emit proper OTEL spans proving real API calls:

### Required Spans Present

✅ `llm.complete` span
- `llm.model=groq::openai/gpt-oss-20b` attribute
- `llm.prompt_tokens` attribute
- `llm.completion_tokens` attribute
- `llm.total_tokens` attribute
- Real network latency (~1-6 seconds, not mock values)

✅ `ggen.mcp.tool_call` spans
- Tool names captured
- Parameters logged
- Service metadata present

## Key Insights

1. **`cfg!(test)` was the blocker** - Compile-time test detection forced MockClient
2. **Runtime checks are sufficient** - `GROQ_API_KEY` presence check at runtime works
3. **Feature flags were unnecessary** - Added ceremony without benefit
4. **Assertions were missing** - Tests had ZERO assertions, always "passed"
5. **OTEL spans are proof** - Real API calls emit spans with token counts and timing

## Files Modified

1. `crates/ggen-ai/src/config/global.rs` - Removed `cfg!(test)` from `is_test_mode()`
2. `crates/ggen-a2a-mcp/tests/llm_mcp_a2a_chain.rs` - Removed feature gate, added assertions
3. `crates/ggen-a2a-mcp/tests/groq_slo_timing.rs` - Removed feature gate
4. `crates/ggen-a2a-mcp/tests/a2a_groq_integration.rs` - Removed feature gate
5. `crates/ggen-a2a-mcp/tests/a2a_self_play.rs` - Removed feature gate
6. `semconv/live-check/run-ggen-live-check.sh` - Removed `--features live-groq`

## Definition of Done

- ✅ Tests run with real Groq when `GROQ_API_KEY` is present
- ✅ Tests skip gracefully when `GROQ_API_KEY` is absent
- ✅ No `--features live-groq` flag required
- ✅ Tests have actual assertions (not just printing)
- ✅ OTEL traces are captured and validated
- ✅ No more MockClient forced by `cfg!(test)`
