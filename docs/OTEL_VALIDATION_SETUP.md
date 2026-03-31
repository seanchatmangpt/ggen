# OTEL Validation Configuration Update

**Date:** 2026-03-30
**Summary:** Updated CLAUDE.md and .claude configuration to require OTEL span/trace validation for LLM/external service features

## Files Updated

### 1. `/Users/sac/ggen/CLAUDE.md`
- Added `OTEL Validation` to Agent Coordination Rules table
- Updated Definition of Done to include OTEL trace verification
- Added comprehensive OpenTelemetry Validation section with:
  - Required spans by feature (LLM, MCP, Pipeline)
  - Verification commands
  - OTEL validation checklist
  - Interpretation guide for valid/invalid output

### 2. `/Users/sac/ggen/.claude/rules/otel-validation.md` (NEW)
- Complete OTEL validation rule documentation
- Mandatory spans and attributes for each feature type
- Step-by-step verification process
- Common mistakes and how to avoid them
- Definition of Done with OTEL requirements
- Examples of valid vs invalid OTEL output

### 3. `/Users/sac/ggen/.claude/rules/README.md`
- Added `otel-validation.md` to directory structure
- Added OTEL Validation to Quick Reference section
- Added OTEL reminder to Critical Reminders
- Updated Definition of Done to include OTEL verification commands

### 4. `/Users/sac/.claude/projects/-Users-sac-ggen/memory/otel-validation-requirement.md` (NEW)
- Memory file documenting the OTEL validation requirement
- Why OTEL validation is necessary
- How to apply OTEL validation step-by-step
- Required spans and attributes reference table
- Valid vs invalid OTEL output examples

### 5. `/Users/sac/.claude/projects/-Users-sac-ggen/memory/MEMORY.md`
- Added `otel-validation-requirement.md` to Active Memory Files index
- Added OTEL Validation section to Quick Reference

## Key Changes

### New Rule: OTEL Validation Required

For any feature involving LLM calls or external services, you MUST verify OpenTelemetry spans/traces exist. Tests passing is NOT sufficient.

**Required OTEL Spans for LLM Integration:**
- `llm.complete` - Synchronous LLM completion
- `llm.complete_stream` - Streaming LLM completion

**Required Attributes:**
- `llm.model` - Model identifier (e.g., `groq::openai/gpt-oss-20b`)
- `llm.prompt_tokens` - Input token count
- `llm.completion_tokens` - Output token count
- `llm.total_tokens` - Total tokens used

### Updated Definition of Done

**Before:**
```bash
cargo make check && cargo make lint && cargo make test && cargo make slo-check
```

**After (for LLM/external service features):**
```bash
cargo make check && cargo make lint && cargo make test && cargo make slo-check

# PLUS OTEL verification:
RUST_LOG=trace,ggen_ai=trace cargo test <test_name> 2>&1 | grep -E "llm\.|mcp\."
# ✅ Required spans exist
# ✅ Required attributes populated
```

## Verification Commands

### For LLM Integration
```bash
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt
grep -E "llm\.complete|llm\.model|llm\.total_tokens" otel_output.txt
```

### For MCP Tools
```bash
RUST_LOG=trace ggen mcp call validate_pipeline 2>&1 | grep -E "mcp\.tool\."
```

### For Pipeline Stages
```bash
RUST_LOG=trace ggen sync --ontology example.ttl 2>&1 | grep -E "pipeline\.(load|extract|generate|validate|emit)"
```

## Impact

### Positive
- Ensures real API calls are made (not mocked)
- Provides observable proof of external service integration
- Catches integration issues that unit tests miss
- Enables production debugging with OTEL traces

### Enforcement
- Claims without OTEL evidence will be rejected
- Tests passing is not sufficient for LLM/external service features
- Must include OTEL span output in completion messages

## Examples

### ✅ Valid Completion Message
```
LLM integration verified with OTEL traces:
- llm.complete span found
- llm.model=groq::openai/gpt-oss-20b
- llm.total_tokens=770
- elapsed_ms=2341

All tests pass + OTEL validation complete.
```

### ❌ Invalid Completion Message
```
All tests pass. LLM integration complete.
```

**Missing:** OTEL span evidence. This claim will be rejected.

## Next Steps

When implementing LLM/external service features:

1. Write tests (Chicago TDD)
2. Implement feature
3. Run tests: `cargo make test`
4. **NEW:** Verify OTEL spans: `RUST_LOG=trace cargo test` + grep for spans
5. Only claim complete if both tests pass AND OTEL spans exist

## References

- Complete rules: `/Users/sac/ggen/.claude/rules/otel-validation.md`
- Project docs: `/Users/sac/ggen/CLAUDE.md` (OpenTelemetry Validation section)
- Memory: `/Users/sac/.claude/projects/-Users-sac-ggen/memory/otel-validation-requirement.md`
