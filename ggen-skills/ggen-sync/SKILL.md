---
name: ggen-sync
description: Orchestrates the ggen synchronization pipeline. Use to trigger the μ-projection, validate governance gates, and manage receipt chains for manufacturing artifacts.
---

# ggen-sync

Orchestrates the 5-stage μ-pipeline to project knowledge graphs into typed code.

## Workflow

1. **Trigger Projection**: Run `ggen sync` to initiate the projection.
2. **Review Governance**: Analyze the proof gate results in the output.
3. **Verify Receipt**: Use `ggen receipt verify` to check the cryptographic chain.

## Usage

Use `ggen sync` with appropriate flags:

- `--audit`: Enable full audit logging.
- `--force`: Force overwrite existing generated code.
- `--stage <STAGE>`: Run specific μ-pipeline stages.

See `ggen sync --help` for details on flags and pipeline stages.
