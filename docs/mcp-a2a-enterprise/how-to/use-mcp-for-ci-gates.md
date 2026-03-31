# How-to: use MCP for CI gate enforcement

## Problem

You want CI to enforce the same generation and validation behavior used by humans and agents.

## Solution

Use MCP-exposed operations as the canonical CI execution interface for validation and generation checks.

## Steps

1. Select required CI stages:
   - project validation,
   - generation/sync verification,
   - optional cycle detection checks.
2. Execute MCP tool calls in CI through your chosen client runtime.
3. Persist outcomes (pass/fail, diagnostics, timing) for audit.
4. Fail the pipeline when mandatory checks fail.

## Design rules

- Keep CI operations aligned with local workflow semantics.
- Do not add a second implementation path for validation/generation.
- Keep required inputs explicit (`project_path`, ontology/query/template paths).

## Typical CI checks

- Validate pipeline for every PR touching ontology/templates/rules.
- Re-run generation on trusted branches to detect drift.
- Capture structured errors to speed triage.

## Related

- [`run-quality-gates-before-merge.md`](run-quality-gates-before-merge.md)
- [`../explanation/observability-and-trust.md`](../explanation/observability-and-trust.md)
