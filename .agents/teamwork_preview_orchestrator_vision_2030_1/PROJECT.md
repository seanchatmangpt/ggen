# Project: ggen Reimagined Vision 2030 Audit

## Architecture
- Genesis-bearing interchangeable parts kernel.
- ggen membrane, packaging, adapter surface, and projection layer.
- Witness binaries and verification scripts.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Exploration & Audit | Audit existing codebase and identify remaining gaps in code, tests, and documentation. | none | DONE |
| 2 | Code/Test Gap Implementation | Fill any identified gaps in Genesis core, ggen membrane, or doctests. | M1 | IN_PROGRESS |
| 3 | Verification & Witnessing | Run the external witness binaries and scripts (W0-W9/T0-T10) under clean and sabotage conditions. | M2 | PLANNED |

## Interface Contracts
### Genesis Kernel ↔ ggen Membrane
- Genesis: provides core trait representations (O*, μ, RelationPage, Pair2, Construct8, Receipt, Replay, Refusal).
- ggen: wraps them and projects to standard formats.
