# OpenTelemetry (OTEL) Validation

Tests passing isn't sufficient — LLM/external-service/pipeline features need verified OTEL
spans before claiming they work. No spans, no claim.

| Feature | Spans | Attributes |
|---|---|---|
| LLM | `llm.complete{,_stream}` | `llm.model`, `llm.{prompt,completion,total}_tokens` |
| MCP | `mcp.tool.{call,response}` | `mcp.tool.{name,duration_ms,result}` |
| Pipeline | `pipeline.{load,extract,generate,validate,emit}` | `pipeline.{stage,duration_ms}` |
| Quality gates | `quality_gate.{validate,pass_fail}` | `gate.{name,result}` |

Verify: `RUST_LOG=trace cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt`,
grep for spans+`llm.total_tokens=[1-9]`.

**Proven**: spans+attributes present, tokens non-zero, real latency. **Unverified**: no
spans — may be mocked/cached. **Observed**: span exists, attributes incomplete → investigate.

Failure modes: NARRATION (asserting with no span output), SELF-CERT (test passed ≠ service was
called). Checklist: tests pass, spans exist, real attributes, error spans on failure.
