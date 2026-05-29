# Project Plan — capability-map (cpmp)

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Codebase & Test Audit | Run tests, audit files, verify Open Ontologies toolchain | None | PLANNED |
| 2 | Enterprise Documentation | Write the required enterprise docs under docs/enterprise/ | M1 | PLANNED |
| 3 | CLI Command Complete | Implement / finish all CLI nouns & commands | M1 | PLANNED |
| 4 | Enterprise Module Stubs | Create stubs for the 12 cpmp-enterprise modules | M1 | PLANNED |
| 5 | Scan Pipeline Gates | Implement/enforce the 8 scan refusal gates and Open Ontologies checks | M3, M4 | PLANNED |
| 6 | Verification & Adjudication | Run full validation, compile cleanly, pass tests | M2, M5 | PLANNED |

## Verification Strategy
- **Static checks**: Clean compile, deny warnings, clippy.
- **Unit & Integration tests**: `cargo test --all`.
- **Integrity Gates**: All scan gate checks will be validated against positive and negative inputs.
- **Documentation verification**: Ensure all required 11 documents are in `docs/enterprise/` with no private namespace laundering.
