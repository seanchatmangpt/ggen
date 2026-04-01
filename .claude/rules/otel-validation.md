# OpenTelemetry (OTEL) Validation

Tests passing is not sufficient. For any feature involving LLM calls, external services, or pipeline stages, you must verify OTEL spans exist at runtime.

You will be tempted to skip this step because tests pass. That is the NARRATION failure mode: asserting completion without producing proof. A passing test proves the test harness works. It does not prove the external service was called, the right model was selected, or tokens were consumed. Only OTEL spans prove that. You verify spans, or you do not claim the feature works.

## The Rule

For every feature that touches an external service, you capture OTEL output at runtime and you present the spans and attributes in your completion message. No spans, no claim. No exceptions.

## When This Applies

OTEL validation is mandatory for any feature involving:

- LLM calls (Groq, OpenAI, Anthropic, or any provider)
- MCP tool execution
- External API calls (REST, GraphQL, RPC)
- Database operations (PostgreSQL, Redis, SQLite)
- Pipeline stages (the five-stage code generation pipeline)
- Quality gate validation

## Required Spans by Feature

### LLM Integration

| Span | Purpose |
|------|---------|
| `llm.complete` | Synchronous LLM completion |
| `llm.complete_stream` | Streaming LLM completion |

Required attributes: `llm.model`, `llm.prompt_tokens`, `llm.completion_tokens`, `llm.total_tokens`, `operation.name`, `operation.type`

### MCP Tools

| Span | Purpose |
|------|---------|
| `mcp.tool.call` | Tool invocation |
| `mcp.tool.response` | Tool response |

Required attributes: `mcp.tool.name`, `mcp.tool.duration_ms`, `mcp.tool.result`

### Pipeline Stages

| Span | Purpose |
|------|---------|
| `pipeline.load` | Load RDF ontology |
| `pipeline.extract` | Extract skill definitions |
| `pipeline.generate` | Generate code |
| `pipeline.validate` | Quality gate validation |
| `pipeline.emit` | Write generated files |

Required attributes: `pipeline.stage`, `pipeline.duration_ms`, `pipeline.files_generated`

### Quality Gates

| Span | Purpose |
|------|---------|
| `quality_gate.validate` | Gate check execution |
| `quality_gate.pass_fail` | Gate result |

Required attributes: `gate.name`, `gate.result`

## Verification Procedure

```bash
# Enable trace logging
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace,genai=trace

# Run the relevant test and capture output
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt

# Verify required spans exist
grep -E "llm\.complete" otel_output.txt
grep -E "llm\.model" otel_output.txt
grep -E "llm\.(prompt_tokens|completion_tokens|total_tokens)" otel_output.txt

# Confirm attributes are populated with real values, not zeros or placeholders
grep -E "llm\.total_tokens=[1-9]" otel_output.txt
```

## Interpreting Results

### Real -- PROVEN

```
INFO ggen_ai::client: llm.complete request
  llm.model=groq::openai/gpt-oss-20b
  llm.complete response
  prompt_tokens=450
  completion_tokens=320
  total_tokens=770
  elapsed_ms=2341
```

All required spans present. All required attributes populated. Token counts are non-zero and sum correctly. Latency is consistent with a real network call. This is proven. You can claim the feature works.

### Missing -- UNVERIFIED

```
Test passed.
No OTEL spans found in logs.
```

No spans at all. The test passed but you have no evidence the external service was called. The test may be mocked, stubbed, or hitting a cached path. This is unverified. You cannot claim the feature works.

### Partial -- OBSERVED

```
INFO llm.complete request
  llm.model=groq::openai/gpt-oss-20b
```

A span exists but attributes are incomplete. The call was initiated but you have no response data, no token counts, no timing. This is observed but not proven. You investigate further before making any claim.

## Your Failure Modes for OTEL

| Failure Mode | What It Looks Like | Why It Is Wrong |
|-------------|-------------------|-----------------|
| NARRATION | "The LLM integration is working" with no span output | You asserted completion without producing proof. Claims without evidence are noise. |
| SELF-CERT | You ran the test, it passed, you decided that was enough | Tests prove the test harness works. OTEL spans prove the external service was called. You conflated the two. |

## Checklist Before Claiming Completion

1. All tests pass
2. OTEL spans exist for the operation you are claiming works
3. All required attributes are populated with real values (non-zero tokens, real latency)
4. Token counts and timing are consistent with a real external call, not synthetic values
5. Error spans appear if the operation failed, with `error=true` and a meaningful message

If any of these are missing, the feature is not done. Non-negotiable.
