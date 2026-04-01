# ggen-ai — Crate Audit

**Path:** `crates/ggen-ai/`
**Lines:** 17 modules, ~1,600 test markers
**Role:** Multi-provider LLM integration, MCP tools, AI agents, DSPy framework

---

## STUBS

| File:Line | Function | Returns | Should Do |
|-----------|----------|---------|-----------|
| `mcp/traits.rs:338` | `execute_tool()` | `{"status": "placeholder", "message": "Tool execution not yet implemented"}` | Dispatch through `ToolRegistry` to actual tool implementations |
| `agents/core/graph_evolution.rs:358` | `get_ai_client()` | `Err("AI client not implemented")` | Initialize and return LLM client for graph analysis |
| `agents/core/regeneration.rs:459` | `check_artifact_cache()` | `Ok(None)` | Check cache freshness for artifacts |
| `agents/core/regeneration.rs:465` | `validate_dependencies()` | `Ok(true)` | Validate all dependencies are up to date |

---

## DEAD CODE

| File:Line | Item | Status |
|-----------|------|--------|
| `governance/dashboard/metrics.rs:114` | Config field | `#[allow(dead_code)]` — "kept for future configuration updates" |
| `ultrathink/core.rs:20` | Config field | `#[allow(dead_code)]` — "kept for future configuration updates" |
| `ultrathink/core.rs:31` | Channel fields | `#[allow(dead_code)]` — "kept for future agent communication features" |
| `microframework/orchestrator.rs:22` | `AgentOrchestrator.config` | `#[allow(dead_code)]` — stored but unused |
| `microframework/pipeline.rs:94` | `Pipeline.results` | `#[allow(dead_code)]` — never populated or read |
| `dspy/testing/dummy_lm.rs:155` | Field | `#[allow(dead_code)]` |
| `dspy/optimizers/copro.rs:471` | Field | `#[allow(dead_code)]` |
| `providers/adapter.rs:40` | `MockClient` | Test double with predefined responses — used in tests only |

---

## ARCHITECTURE ISSUES

### OTEL Spans — No SDK Dependency

OTEL spans are emitted via `tracing` crate (`info_span!`), not the OpenTelemetry SDK. Attributes are recorded as span fields (`llm.model`, `llm.prompt_tokens`). Works with `tracing-opentelemetry` bridge but requires external configuration to export.

### Tool Calling — Not Wired to GenAiClient

`ToolRegistry::export_openapi()` generates function definitions, but `GenAiClient::create_chat_options()` does NOT attach them to `ChatOptions`. Callers must manually retrieve tools. Streaming explicitly returns empty chunks for `ToolCallChunk` events.

### Cycle Break with ggen-core

Both crates have comments about removed mutual dependencies. `ggen-ai` is described as "pure LLM integration layer without code-generation dependencies."

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **FIX** | MCP `execute_tool()`: dispatch through registry | P1 |
| **FIX** | AI client in graph evolution agent | P1 |
| **FIX** | Artifact cache and dependency validation stubs | P2 |
| **FIX** | Wire tool definitions into `create_chat_options()` | P2 |
| **DELETE** | `ultrathink/core.rs` unused channel/config fields | P3 |
| **DELETE** | `microframework/` dead `results` and `config` fields | P3 |
| **REFACTOR** | Consider whether `MockClient` violates Chicago TDD mandate | P2 |
