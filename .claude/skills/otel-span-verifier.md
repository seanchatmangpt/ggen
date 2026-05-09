---
name: OpenTelemetry Span Verifier
description: Verify OTEL spans exist for LLM calls, MCP tools, and pipeline stages. Mandatory after any external service integration. Tests passing is not sufficient.
paths: ["crates/ggen-ai/**", "crates/ggen-a2a-mcp/**", "crates/ggen-cli/**"]
---

# Skill: otel-span-verifier

## Doctrine

**Tests passing is not proof. OTEL spans are proof.**

A test can pass while mocking the LLM call, stubbing the MCP tool, or skipping the external service entirely. Passing tests prove the test harness works. OTEL spans prove the external service was called.

## When to Trigger

- After implementing any LLM integration (Groq, OpenAI, Anthropic, etc.)
- After adding MCP tool execution
- After implementing pipeline stages (μ₁-μ₅)
- After external API call integration
- User claims LLM/external service feature is "complete" — verify first

## Verification Procedure

### Step 1: Enable Trace Logging

```bash
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace,genai=trace
```

### Step 2: Run Relevant Test

```bash
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt
```

### Step 3: Grep for Required Spans

```bash
# LLM integration
grep -E "llm\.complete|llm\.complete_stream" otel_output.txt
grep -E "llm\.model" otel_output.txt
grep -E "llm\.(prompt_tokens|completion_tokens|total_tokens)" otel_output.txt

# MCP tools
grep -E "mcp\.tool\.call|mcp\.tool\.response" otel_output.txt
grep -E "mcp\.tool\.name" otel_output.txt

# Pipeline stages
grep -E "pipeline\.(load|extract|generate|validate|emit)" otel_output.txt
grep -E "pipeline\.stage|pipeline\.duration_ms" otel_output.txt
```

### Step 4: Verify Attributes

Real evidence has:
- ✅ Non-zero token counts (not mocked)
- ✅ Real network latency (2-3s for LLM calls, not instant)
- ✅ All required attributes populated
- ✅ Correct model names (e.g., `groq::openai/gpt-oss-20b`)

## Required Spans by Feature

| Feature | Required Spans | Attributes |
|---------|---|---|
| **LLM** | `llm.complete`, `llm.complete_stream` | `llm.model`, `llm.prompt_tokens`, `llm.completion_tokens`, `llm.total_tokens` |
| **MCP Tools** | `mcp.tool.call`, `mcp.tool.response` | `mcp.tool.name`, `mcp.tool.duration_ms` |
| **Pipeline** | `pipeline.load`, `pipeline.extract`, `pipeline.generate`, `pipeline.validate`, `pipeline.emit` | `pipeline.stage`, `pipeline.duration_ms` |
| **Quality Gates** | `quality_gate.validate`, `quality_gate.pass_fail` | `gate.name`, `gate.result` |

## Failure Modes

| Observation | Conclusion |
|---|---|
| Spans exist with non-zero tokens and 2-3s latency | ✅ PROVEN — Real LLM call |
| No spans in output | ❌ UNVERIFIED — Test may be mocked |
| Spans with zero tokens or <100ms latency | ⚠️ OBSERVED — Check if synthetic response |
| Span without `llm.model` attribute | ⚠️ PARTIAL — Call initiated, response data missing |

## Before Claiming Completion

1. ✅ All tests pass
2. ✅ OTEL spans exist for the operation
3. ✅ All required attributes populated
4. ✅ Token counts and latency realistic
5. ✅ Error spans appear if operation failed

No spans = feature not done. Non-negotiable.
