# How-to: resolve ontology import cycles safely

## Problem

Ontology imports can form cycles that block deterministic processing and validation.

## Solution

Use cycle detection and fix strategies with preview-first discipline.

## Steps

1. Detect cycle risk during validation or dedicated cycle checks.
2. Run cycle remediation in preview (`dry_run`) mode first.
3. Select a strategy:
   - remove import,
   - merge files,
   - create interface.
4. Apply fix and re-run validation gates.
5. Record strategy choice and rationale in PR notes.

Implementation reference:
- [`crates/ggen-core/src/graph/cycle_fixer.rs`](../../crates/ggen-core/src/graph/cycle_fixer.rs)

## Strategy guidance

- `remove_import`: quickest for obvious accidental loops.
- `merge_files`: useful for tightly coupled ontology fragments.
- `create_interface`: preferred when shared definitions should be explicit.

## Safety rules

- Never skip post-fix gate validation.
- Keep a rollback path for ontology changes.
- Do not combine cycle remediation with unrelated feature edits.

## Related

- [`run-quality-gates-before-merge.md`](run-quality-gates-before-merge.md)
- [`../reference/mcp-tool-intents.md`](../reference/mcp-tool-intents.md)
