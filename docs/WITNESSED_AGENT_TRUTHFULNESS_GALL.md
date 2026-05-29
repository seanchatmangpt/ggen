# Witnessed Agent Truthfulness GALL Protocol

The Witnessed Agent Truthfulness GALL Protocol enforces transcript-bearing, sabotage-tested, clean-room verified execution for all agent work.

## Protocol Architecture

The protocol uses a dual-ring validation model:
1. **T0-T10 Checks**: Validate individual codebase parameters (compilation, unit tests, integration tests, feature flags, forbidden commands, receipts, and OCEL logs).
2. **W0-W9 Checks**: Validate process-level artifacts (worktree inventory, command execution transcripts, script adequacy, clean-room rebuilds, cross-artifact links, causal sufficiency, and contradiction checks).

The final promotion state is determined by the external witness adjudication receipt:
```
witnessed_truthfulness.external_adjudication.json
```
which binds all evaluation states and signs them with a cryptographic BLAKE3 receipt.
