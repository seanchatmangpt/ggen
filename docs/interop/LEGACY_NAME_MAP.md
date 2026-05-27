# Legacy Name Map
## Old names for current concepts — keep the alias, document the migration.

---

| Legacy name | Current name | Location | Status | Migration path |
|---|---|---|---|---|
| `Construct8Packet` | `Construct8` | `genesis-construct8/src/models.rs` → `genesis-core-v2/src/primitives.rs` | LEGACY_NAME | `pub type Construct8Packet = Construct8;` in genesis-construct8 |
| `SymbolTable` | `SymbolPage` | `genesis-construct8/src/models.rs` → `ggen-membrane/src/lib.rs` | LEGACY_NAME | Keep alias, add doc |
| `knhk-construct8` | `genesis-core` | Crate name | LEGACY_NAME | Add `[package] name = "genesis-core"` path as alias |
| `knhk_construct8` (crate name) | `genesis_core` | Cargo import path | LEGACY_NAME | Feature gate: `#[cfg(feature = "knhk-compat")]` |
| `genesis_core::primitives::Refusal` | `revelation::PlagueRecord` | `genesis-core-v2` | LEGACY_NAME | `PlagueRecord::from_refusal()` is the bridge |
| `ggen-yawl` | (removed from workspace) | `Cargo.toml` references | AMBIGUOUS | Classify before acting |
| `emission_determinism` | `verify_determinism` | `ggen-core/src/pipeline_engine/passes/emission.rs` | LEGACY_NAME | Keep both, add `pub use` |
| `RefusalCode` | `RefusalReason` | `ggen-core/src/genesis.rs` vs `genesis-core-v2/src/primitives.rs` | LEGACY_NAME | Alias in ggen-core |
| `parts_foundry` | `ggen-membrane` | `ggen-core/src/parts_foundry.rs` | LEGACY_NAME | Preserve, add compatibility re-export |
| `wasm4pm` | `ggen-graph/src/ocel/` | Docs only | DOC_ONLY | Connect docs to existing OCEL projection code |
