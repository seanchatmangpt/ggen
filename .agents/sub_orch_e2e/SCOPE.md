# Scope: E2E Testing Track

## Architecture
- The E2E Testing Track builds opaque-box integration tests in `crates/ggen-projection/tests/` and `crates/tower-lsp-max/tests/` or a custom test runner, without referencing internal structures of the implementation where possible (only public interfaces/binary outputs).
- Publishes `TEST_READY.md` when completed.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Setup Test Layout | Set up test folders and check compiler configurations | None | DONE |
| 2 | Tier 1 Tests | Feature coverage (>=5 cases per feature) | M1 | DONE |
| 3 | Tier 2 Tests | Boundary and corner cases (>=5 cases per feature) | M2 | DONE |
| 4 | Tier 3 Tests | Cross-feature combination tests | M3 | DONE |
| 5 | Tier 4 Tests | Real-world application scenarios | M4 | DONE |
| 6 | Publish test ready | Create TEST_READY.md at project root | M5 | DONE |

## Interface Contracts
- None (independent test execution)
