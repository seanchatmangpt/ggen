# How-to: coordinate multi-agent delivery with A2A

## Problem

Multiple teams or agents must collaborate on ontology, templates, and validation without process ambiguity.

## Solution

Use A2A for role orchestration and handoffs, while keeping execution on MCP tools.

## Steps

1. Define role boundaries:
   - ontology owner,
   - template owner,
   - gate owner.
2. Define handoff schema:
   - required inputs,
   - status flags,
   - unresolved issues.
3. Require each role to call the same MCP operations for execution.
4. Aggregate outcomes into a final release/merge decision.

## Handoff minimum fields

- `artifact_scope`
- `project_path`
- `validation_status`
- `assumptions`
- `blocking_issues`

## Failure containment pattern

- If upstream validation fails, downstream generation is not attempted.
- If generation succeeds but gate checks fail, publish partial status and block merge.

## Related

- [`../tutorials/02-introduce-a2a-handoff.md`](../tutorials/02-introduce-a2a-handoff.md)
- [`../explanation/jtbds-ggen-mcp-a2a.md`](../explanation/jtbds-ggen-mcp-a2a.md)
- [`../reference/glossary.md`](../reference/glossary.md)
