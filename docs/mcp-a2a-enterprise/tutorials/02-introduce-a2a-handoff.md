# Tutorial: Introduce an A2A handoff over MCP execution

## Goal

Learn how two agent roles collaborate while still using one execution surface (MCP + ggen core).

## What you will learn

- How to split responsibilities between agents.
- How to define a minimal handoff contract.
- How to keep generation and validation centralized.

## Prerequisites

- Completion of [`01-first-end-to-end-with-mcp.md`](01-first-end-to-end-with-mcp.md).
- A basic A2A orchestration setup (conceptual or implementation-specific).

## Roles in this tutorial

- Agent A: Ontology Curator
  - owns ontology and query correctness.
- Agent B: Implementation Curator
  - owns templates, generation output intent, and merge readiness.

## Handoff contract (minimum)

Agent A passes:
- ontology location,
- query set location,
- validation status summary,
- assumptions/constraints.

Agent B passes back:
- generated artifact summary,
- template/rendering validation status,
- unresolved issues list.

## Workflow steps

1. Agent A validates ontology/query quality through MCP tools.
2. Agent A sends handoff payload to Agent B.
3. Agent B runs generation/sync through MCP tools.
4. Agent B runs quality-gate validation and publishes result.
5. Both agents produce a final merge recommendation.

## Success criteria

- No agent performs private, non-MCP generation logic.
- Validation and generation outcomes are reproducible.
- Handoff payloads are explicit and reviewable.

## Where to go next

- Task-focused operations:
  - [`../how-to/coordinate-multi-agent-with-a2a.md`](../how-to/coordinate-multi-agent-with-a2a.md)
  - [`../how-to/use-mcp-for-ci-gates.md`](../how-to/use-mcp-for-ci-gates.md)
