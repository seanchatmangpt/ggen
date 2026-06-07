# Scope: Milestone M2 — Core Models & Defect Resolution

## Architecture
- `ggen-projection` structures: `PackDescriptor`, `PackPlan`, `ProjectionMap`, `CustomizationMap`, `ReceiptIndex`.
- Sync logic in `ggen-projection`.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Core Projection Models | Ensure the models `PackDescriptor`, `PackPlan`, `ProjectionMap`, `CustomizationMap`, `ReceiptIndex` are robust and correctly implemented. | None | IN_PROGRESS |
| 2 | Defect Resolution | Fix the following critical defects:
1. Concurrency race condition in bidirectional symbol lookup (`SymbolTable::insert_custom` in `crates/genesis-construct8/src/models.rs`).
2. Broken git commit chain history in receipt storage (`LockchainStorage::append_to_git` in `crates/genesis-lockchain/src/storage.rs`).
3. Silent mapping range overwrites on file collision (`ProjectionMap::add_mapping` in `crates/ggen-projection/src/mapping.rs`).
4. Non-deterministic indexes caused by random UUIDs (`ReceiptIndex::add_receipt` in `crates/ggen-projection/src/receipt.rs`).
5. Pre-existing workspace compile blocker in `ggen-core` test targets due to any missing `packs` fields in `GgenManifest` initializations. | M1 | IN_PROGRESS |

## Interface Contracts
- See PROJECT.md.
