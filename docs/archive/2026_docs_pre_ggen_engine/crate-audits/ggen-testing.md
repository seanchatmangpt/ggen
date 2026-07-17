<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen-testing — Crate Audit](#ggen-testing--crate-audit)
  - [STATUS: DEAD INFRASTRUCTURE](#status-dead-infrastructure)
    - [What it provides:](#what-it-provides)
    - [Why it exists:](#why-it-exists)
  - [FIX / DELETE / REFACTOR](#fix--delete--refactor)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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
