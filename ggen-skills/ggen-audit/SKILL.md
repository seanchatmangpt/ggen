---
name: ggen-audit
description: Verifies cryptographic provenance and artifact integrity. Use to audit receipts, chain verification, and compliance of generated artifacts.
---

# ggen-audit

Tools for verifying the integrity and history of generated code.

## Verification Workflow

1. **Verify Receipt**: Use `ggen audit verify <receipt.json>` to validate SHA-256 and digital signatures.
2. **Trace Lineage**: Use `ggen audit lineage` to inspect the chain of custody for a generated file back to its source ontology input.
3. **Analyze Conformance**: Audit the proof gate results against the project's manufacturing intent.

## Commands

- `ggen audit verify`: Check cryptographic integrity.
- `ggen audit lineage`: Inspect dependency and proof chain.
- `ggen audit conformance`: Compare artifacts against security/quality constraints.
