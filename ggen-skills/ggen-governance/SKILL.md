---
name: ggen-governance
description: Manages the manufacturing constitution. Use to define, update, or validate manufacturing intent and proof gate constraints.
---

# ggen-governance

Manages the semantic constraints and validation logic (proof gates) that govern the ggen pipeline.

## Manufacturing Intent

Define objectives and success criteria for projection runs by configuring the `ManufacturingIntent`.

## Proof Gate Orchestration

The validator enforces the 8 canonical proof gates. If a projection run fails a gate:
1. Examine the receipt output.
2. Address the specific gate violation (e.g., soundess violation, syntax error).
3. Re-run projection to regenerate the receipt.

## Configuration

Use `ggen governance init` to generate a base `governance.ttl` schema and `ManufacturingIntent` configuration file.
