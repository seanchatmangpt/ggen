# How-to: run quality gates before merge

## Problem

You need a consistent pre-merge quality decision for ontology-driven projects.

## Solution

Use pipeline gate validation as a hard checkpoint before merge.

## Steps

1. Ensure the project root has a valid `ggen.toml`.
2. Run the project-level validation path (MCP `validate_pipeline` or equivalent workflow).
3. Capture pass/fail details and recovery actions.
4. Block merge on any failing gate.

## What to check

- Manifest schema validity
- Ontology dependencies
- SPARQL validity
- Template validity
- File permissions/output path readiness
- Rule consistency

Reference implementation concepts:
- [`crates/ggen-core/src/poka_yoke/quality_gates.rs`](../../crates/ggen-core/src/poka_yoke/quality_gates.rs)

## Common failure patterns

- Missing referenced ontology files
- Template path mismatches
- Query syntax errors
- Rule references to non-existent assets

## Related

- [`use-mcp-for-ci-gates.md`](use-mcp-for-ci-gates.md)
- [`../reference/mcp-tool-intents.md`](../reference/mcp-tool-intents.md)
