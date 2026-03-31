# Tutorial: First end-to-end flow with MCP

## Goal

Run a full learning-path flow using MCP semantics:
1. discover examples,
2. inspect one example,
3. validate project quality gates,
4. run generation/sync operations.

## What you will learn

- How MCP acts as the execution surface for ggen jobs.
- How validation and generation compose into one repeatable workflow.
- What "success" looks like at each stage.

## Prerequisites

- Local repo checkout with examples present.
- Access to an MCP client capable of calling ggen MCP tools.
- Familiarity with `ggen.toml`, ontology `.ttl`, and template files.

## Step 1: Discover available starting points

Call the tool equivalent of `list_examples`.

Expected outcome:
- You receive a bounded list of example candidates with names and categories.

## Step 2: Inspect one example deeply

Call the tool equivalent of `get_example` for a selected item.

Expected outcome:
- You can view its `ggen.toml`, ontology snippet, and template inventory.

## Step 3: Validate the project gates before generation

Call the tool equivalent of `validate_pipeline` with the example project path.

Expected outcome:
- Gate pass/fail result with clear diagnostics.
- If failed, stop and resolve before proceeding.

## Step 4: Execute generation/sync

Call `generate` or `sync` with explicit project/ontology paths.

Expected outcome:
- Deterministic artifact output,
- execution metadata and result summary,
- no hidden side-path execution.

## Step 5: Confirm completion conditions

You are done when:
- all required gates pass,
- expected artifacts are produced,
- operation path is traceable through the MCP surface.

## Next tutorial

Continue with [`02-introduce-a2a-handoff.md`](02-introduce-a2a-handoff.md) to add multi-agent handoffs without changing the core execution surface.
