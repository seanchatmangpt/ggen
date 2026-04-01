# ggen-testing — Crate Audit

**Path:** `crates/ggen-testing/`
**Lines:** 276 lines + submodules
**Role:** Chicago TDD test harness (TestHarness, StateVerifier, assertions, property testing, snapshots)

---

## STATUS: DEAD INFRASTRUCTURE

**Zero consumers.** No other workspace crate depends on `ggen-testing`. The only references are within its own source files and self-tests.

### What it provides:

| Module | Types |
|--------|-------|
| `lib.rs` | `TestHarness`, `StateVerifier<T>` |
| `assertions.rs` | 16 custom assertion functions |
| `fixtures.rs` | `TempFsFixture`, `InMemoryStoreFixture`, `EventLogFixture`, `TestDataBuilder` |
| `property.rs` | `PropertyTest<S>`, `StrategyBuilder` (wraps proptest) |
| `snapshot.rs` | `SnapshotTest`, `SnapshotManager` (wraps insta) |

### Why it exists:

The CLAUDE.md mandates "Chicago TDD ONLY" with `ggen-testing` as the test harness. But no tests actually use it. All 12,514 test markers across the workspace use standard `#[test]`, `assert!`, and direct assertions.

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **DECIDE** | Either adopt across workspace tests or remove from workspace | P3 |
| **NOTE** | If removed, update CLAUDE.md testing section to remove references | P3 |
