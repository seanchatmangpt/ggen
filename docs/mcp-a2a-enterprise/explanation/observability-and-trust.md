# Observability and trust in MCP + A2A delivery

In enterprise settings, "it passed" is not enough. Teams need to prove:
- what operation ran,
- with what inputs,
- through which tool surface,
- with what result and timing.

## Why observability is central

When multiple agents coordinate delivery, failures often happen at boundaries:
- handoff payload mismatch,
- stale assumptions about tool parameters,
- silent retries masking logic flaws.

Structured traces make these visible and auditable.

## Practical trust model

- MCP tools provide explicit operation contracts.
- Quality gates provide deterministic acceptance boundaries.
- A2A orchestration provides ownership and sequencing.
- Telemetry links all three into a coherent execution story.

## Recommended evidence for enterprise review

- Tool invocation trace (name, duration, outcome).
- Gate outcomes tied to project path and revision.
- Error spans with actionable context.
- Merge-time summary showing pass/fail lineage.

## Cross references

- [`docs/mcp-a2a-best-practices-summary.md`](../mcp-a2a-best-practices-summary.md)
- [`docs/multi-agent-a2a-best-practices.md`](../multi-agent-a2a-best-practices.md)
- [`crates/ggen-a2a-mcp/src/ggen_server.rs`](../../crates/ggen-a2a-mcp/src/ggen_server.rs)
