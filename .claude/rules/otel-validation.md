# 🔍 OpenTelemetry (OTEL) Validation Rules

## The Golden Rule

**Tests passing is NOT sufficient. For LLM/external service features, you MUST verify OTEL spans/traces exist.**

If you claim an LLM integration is working, you must show the OTEL spans that prove the API was actually called.

## When OTEL Validation is Required

OTEL validation is MANDATORY for any feature involving:

- **LLM calls** (Groq, OpenAI, Anthropic, etc.)
- **MCP tool execution**
- **External API calls** (REST, GraphQL, RPC)
- **Database operations** (PostgreSQL, Redis, etc.)
- **Pipeline stages** (μ₁-μ₅ code generation)
- **Quality gate validation**

## Required OTEL Spans by Feature

### LLM Integration

**Required Spans:**
- `llm.complete` - Synchronous LLM completion
- `llm.complete_stream` - Streaming LLM completion

**Required Attributes:**
- `llm.model` - Model identifier (e.g., `groq::openai/gpt-oss-20b`)
- `llm.prompt_tokens` - Input token count
- `llm.completion_tokens` - Output token count
- `llm.total_tokens` - Total tokens used
- `operation.name` - Operation name (e.g., `llm.complete`)
- `operation.type` - Operation type (e.g., `llm`)

**Verification:**
```bash
RUST_LOG=trace,ggen_ai=trace cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | grep -E "(llm\.complete|llm\.model|llm\.total_tokens)"
```

### MCP Tools

**Required Spans:**
- `mcp.tool.call` - Tool invocation
- `mcp.tool.response` - Tool response

**Required Attributes:**
- `mcp.tool.name` - Tool name (e.g., `validate_pipeline`)
- `mcp.tool.duration_ms` - Tool execution time
- `mcp.tool.result` - Success/failure status

### Pipeline Stages

**Required Spans:**
- `pipeline.load` - μ₁: Load RDF ontology
- `pipeline.extract` - μ₂: Extract skill definitions
- `pipeline.generate` - μ₃: Generate code (LLM-assisted)
- `pipeline.validate` - μ₄: Quality gate validation
- `pipeline.emit` - μ₅: Write generated files

**Required Attributes:**
- `pipeline.stage` - Stage identifier
- `pipeline.duration_ms` - Stage execution time
- `pipeline.files_generated` - Number of files (μ₅)

## How to Verify OTEL Spans

### Step 1: Enable Trace Logging

```bash
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace,genai=trace
```

### Step 2: Run Tests with Output Capture

```bash
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt
```

### Step 3: Check for Required Spans

```bash
# Check for LLM completion spans
grep -E "llm\.complete" otel_output.txt

# Check for model name
grep -E "llm\.model.*groq" otel_output.txt

# Check for token counts
grep -E "llm\.(prompt_tokens|completion_tokens|total_tokens)" otel_output.txt
```

### Step 4: Validate Attributes

```bash
# Verify all required attributes are present
grep -E "llm\.model=.*groq" otel_output.txt && \
grep -E "llm\.total_tokens=[1-9]" otel_output.txt && \
echo "✅ OTEL validation passed" || echo "❌ OTEL validation failed"
```

## Interpretation of Results

### ✅ Valid OTEL Output

```
INFO ggen_ai::client: llm.complete request
  llm.model=groq::openai/gpt-oss-20b
  prompt_len=1234
  llm.complete response
  prompt_tokens=450
  completion_tokens=320
  total_tokens=770
  elapsed_ms=2341
```

**Conclusion:** Real LLM API call was made. Feature is working.

### ❌ Missing OTEL Output

```
Test passed.
No OTEL spans found in logs.
```

**Conclusion:** Tests are mocked or not actually calling the external service. Feature is **NOT** complete.

### ⚠️ Incomplete OTEL Output

```
INFO llm.complete request
  llm.model=groq::openai/gpt-oss-20b
```

**Conclusion:** Call initiated but no response/attributes logged. Investigation needed.

## Common Mistakes

### Mistake 1: "Tests pass, so it's working"

**Wrong:** Tests may use mocks or synthetic data.
**Right:** Verify OTEL spans show real API calls.

### Mistake 2: "I saw a log message, so OTEL is working"

**Wrong:** Log messages ≠ OTEL spans. Spans have structured attributes.
**Right:** Look for `span.enter()` / `span.exit()` with attributes.

### Mistake 3: "The code compiles, so the integration is complete"

**Wrong:** Compilation ≠ runtime behavior.
**Right:** Verify OTEL spans at runtime.

## Definition of Done with OTEL

A feature involving external services is **ONLY** complete when:

1. ✅ All tests pass
2. ✅ OTEL spans exist for the operation
3. ✅ Required attributes are populated
4. ✅ Token counts/timing are reasonable (not mock values)
5. ✅ Error spans appear if operation fails

**If any of these are missing, the feature is NOT done.**

## Examples

### Example 1: LLM Integration

**Claim:** "LLM integration is working"

**Evidence Required:**
```bash
$ RUST_LOG=trace,ggen_ai=trace cargo test -p ggen-cli-lib --test llm_e2e_test 2>&1 | grep llm
INFO llm.complete request
  llm.model=groq::openai/gpt-oss-20b
INFO llm.complete response
  prompt_tokens=450
  completion_tokens=320
  total_tokens=770
  elapsed_ms=2341
```

**Without this evidence, the claim is rejected.**

### Example 2: MCP Tool

**Claim:** "validate_pipeline MCP tool works"

**Evidence Required:**
```bash
$ ggen mcp call validate_pipeline 2>&1 | grep mcp.tool
INFO mcp.tool.call
  mcp.tool.name=validate_pipeline
INFO mcp.tool.response
  mcp.tool.duration_ms=123
  mcp.tool.result=pass
```

**Without this evidence, the tool is NOT working.**

## Enforcement

If you implement an LLM/external service feature:

1. **Before** claiming it works, run OTEL validation
2. **Include** the OTEL span output in your completion message
3. **Explain** what each span/attribute proves

**Claims without OTEL evidence will be rejected.**

---

**Remember:** Tests can lie. Mocks can deceive. OTEL spans don't lie. Verify everything.
