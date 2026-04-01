---
auto_load: false
category: quality
priority: critical
version: 6.0.1
---

# OTEL Validation

Tests passing is not sufficient. For features involving LLM calls, MCP tools, external APIs, or pipeline stages, you verify OpenTelemetry spans exist and contain required attributes. If spans are missing, the feature is not done.

## The Failure Modes

**NARRATION** is writing "the LLM integration works" without capturing a span. You described what you expect. You did not observe it.

**SELF-CERT** is writing "I verified the spans exist" without running the verification command. You certified your own work without mechanical proof. Run the command. Show the output.

## When OTEL Validation Is Required

Mandatory for any feature involving:
- LLM calls (Groq, OpenAI, Anthropic)
- MCP tool execution
- External API calls (REST, GraphQL, RPC)
- Database operations (PostgreSQL, Redis)
- Pipeline stages (mu_1 through mu_5)
- Quality gate validation

## Evidence Hierarchy Applied

| Evidence | Meaning | Action |
|----------|---------|--------|
| Real spans captured with attributes | PROVEN | Feature verified |
| Spans exist but attributes incomplete | OBSERVED | Investigate missing attributes |
| No spans found | UNVERIFIED | Feature not done. Do not claim completion. |

## Required Spans by Feature

**LLM Integration:** `llm.complete`, `llm.complete_stream` with `llm.model`, `llm.prompt_tokens`, `llm.completion_tokens`, `llm.total_tokens`.

**MCP Tools:** `mcp.tool.call`, `mcp.tool.response` with `mcp.tool.name`, `mcp.tool.duration_ms`, `mcp.tool.result`.

**Pipeline Stages:** `pipeline.load`, `pipeline.extract`, `pipeline.generate`, `pipeline.validate`, `pipeline.emit` with `pipeline.stage`, `pipeline.duration_ms`.

## Verification Procedure

```bash
# 1. Enable trace logging
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace,genai=trace

# 2. Run tests with output capture
cargo test -p <crate> -- <test_name> --nocapture 2>&1 | tee /tmp/otel_output.txt

# 3. Verify required spans
grep -E "llm\.complete" /tmp/otel_output.txt
grep -E "llm\.model" /tmp/otel_output.txt
grep -E "llm\.total_tokens=[1-9]" /tmp/otel_output.txt
```

## Interpretation

Spans with attributes and reasonable timing (seconds, not milliseconds for network calls) means a real API call was made. No spans means tests are mocked or the feature is not wired. Spans without attributes means the instrumentation is incomplete.

## Definition of Done

A feature involving external services is complete only when:
1. All tests pass
2. OTEL spans exist for the operation
3. Required attributes are populated
4. Token counts and timing are reasonable (not mock values)
5. Error spans appear if the operation fails

Missing any layer means the claim is unverified.
