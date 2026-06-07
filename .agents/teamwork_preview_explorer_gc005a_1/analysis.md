# Analysis Report: GC005/GC006 Test Investigation

## 1. Summary of Findings
- **`check_gall_conformance` / `INCONCLUSIVE` Support**:
  - The working directory of the sealed authority `wasm4pm` (`/Users/sac/wasm4pm/crates/wasm4pm-algos/src/gall.rs`) already has uncommitted changes that add the `Inconclusive { reason: String }` variant to the `GallVerdict` enum.
  - The `check_gall_conformance` function has been updated to accept `ocel: OCEL` (by value) and returns `GallVerdict`. It yields `Inconclusive` when `ocel.events.is_empty()`.
  - The `gc005-wasm4pm-adapter` (in both `tower-lsp-max` and `ggen` workspaces) has corresponding uncommitted changes that match these signatures and map the inconclusive verdict to the `WASM4PM-VERDICT-INCONCLUSIVE` diagnostic code.
- **Workspace Tests Status**:
  - **`ggen` Workspace**:
    - Build blocked by a **duplicate key syntax error** in the untracked file `/Users/sac/ggen/crates/gc005-wasm4pm-adapter/Cargo.toml` (lines 11-14 specify `ocel-core` and `wasm4pm-algos` twice). This prevents any cargo commands (including tests) from running in the `ggen` workspace.
  - **`tower-lsp-max` Workspace**:
    - `gc005-wasm4pm-adapter/tests/dogfood_gc005.rs` fails compilation due to missing `tower_lsp` dev-dependency.
    - `gc005-wasm4pm-adapter/tests/dogfood_gc006.rs` compiles but fails at runtime with `NotFound` because `current_dir` resolves to the workspace root instead of the crate directory, producing incorrect parent relative paths.
    - `wasm4pm-lsp/tests/dogfood_gc005.rs` compiles successfully because the `wasm4pm-lsp` package includes correct dependencies (e.g., `tower-lsp`).
  - **`dogfood_gc006` Test Failure**:
    - The `dogfood_gc006` test enforces clean git status in `/Users/sac/wasm4pm` and `/Users/sac/wasm4pm-compat`.
    - Both repositories are dirty (uncommitted changes in `Cargo.lock`, `Cargo.toml`, `gall.rs`, and untracked `.gc-sealed-baseline` baseline manifests), causing the tests to panic and fail.

## 2. Detailed Observations & Build/Test Outputs

### 2.1. duplicate key error in ggen workspace Cargo.toml
Running `cargo test` in `/Users/sac/ggen` produces:
```
error: duplicate key
  --> crates/gc005-wasm4pm-adapter/Cargo.toml:13:1
   |
13 | ocel-core = { path = "../../../wasm4pm/crates/ocel-core" }
   | ^^^^^^^^^
error: failed to load manifest for workspace member `/Users/sac/ggen/crates/gc005-wasm4pm-adapter`
referenced by workspace at `/Users/sac/ggen/Cargo.toml`
```
Inspecting `/Users/sac/ggen/crates/gc005-wasm4pm-adapter/Cargo.toml`:
```toml
11: wasm4pm-algos = { path = "../../../wasm4pm/crates/wasm4pm-algos" }
12: ocel-core = { path = "../../../wasm4pm/crates/ocel-core" }
13: ocel-core = { path = "../../../wasm4pm/crates/ocel-core" }
14: wasm4pm-algos = { path = "../../../wasm4pm/crates/wasm4pm-algos" }
```

### 2.2. tower-lsp-max Crate Compilation status
- Running `cargo test -p gc005-wasm4pm-adapter --test dogfood_gc005` fails:
  ```
  error[E0433]: cannot find module or crate `tower_lsp` in this scope
   --> crates/gc005-wasm4pm-adapter/tests/dogfood_gc005.rs:7:5
    |
  7 | use tower_lsp::lsp_types::Url;
    |     ^^^^^^^^^ use of unresolved module or unlinked crate `tower_lsp`
  ```
  This is due to missing `tower-lsp` dev-dependency in the adapter's Cargo.toml.
- Running `cargo test -p wasm4pm-lsp --test dogfood_gc005` compiles and passes successfully:
  ```
  running 1 test
  test test_gc005_wasm4pm_lsp_observation ... ok

  test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.73s
  ```

### 2.3. Path Resolution Failure in tower-lsp-max dogfood_gc006.rs
Running `cargo test -p gc005-wasm4pm-adapter --test dogfood_gc006` produces:
```
running 1 test
test test_gc006_authority_surface_lock ... FAILED

failures:

---- test_gc006_authority_surface_lock stdout ----

thread 'test_gc006_authority_surface_lock' (1685652) panicked at crates/gc005-wasm4pm-adapter/tests/dogfood_gc006.rs:34:105:
called `Result::unwrap()` on an `Err` value: Os { code: 2, kind: NotFound, message: "No such file or directory" }
```
This is because:
```rust
let current_dir = std::env::current_dir().unwrap(); // resolves to /Users/sac/tower-lsp-max
let tower_lsp_max_root = current_dir.parent().unwrap().parent().unwrap().to_path_buf(); // resolves to /Users
```
Then it tries to read `/Users/crates/wasm4pm-lsp/src/main.rs`, which does not exist.

### 2.4. Dirty Sealed Repository Status
Running `git status --porcelain` in `/Users/sac/wasm4pm` outputs:
```
 M Cargo.lock
 M crates/wasm4pm-algos/Cargo.toml
 M crates/wasm4pm-algos/src/gall.rs
?? .gc-sealed-baseline
```
Running `git status --porcelain` in `/Users/sac/wasm4pm-compat` outputs:
```
?? .gc-sealed-baseline
```
Any modified (`M `) or untracked (`?? `) files trigger a panic in `dogfood_gc006.rs`'s sterility check.

## 3. Recommendations for Implementation

1. **Fix duplicate keys in `ggen/crates/gc005-wasm4pm-adapter/Cargo.toml`**:
   - Clean up the double definitions of `ocel-core` and `wasm4pm-algos`.
2. **Correct the Path Derivation in `tower-lsp-max/crates/gc005-wasm4pm-adapter/tests/dogfood_gc006.rs`**:
   - Instead of hardcoding `parent().unwrap().parent().unwrap()`, traverse upwards to locate the workspace root (just like in the `ggen` version of the test).
3. **Handle missing dev-dependencies in `gc005-wasm4pm-adapter/Cargo.toml`**:
   - Add `tower-lsp = "0.20"` as a dev-dependency in the adapter's Cargo.toml to allow `dogfood_gc005.rs` to compile.
4. **Sterility Baseline (GC006) Remediations**:
   - The uncommitted modifications in `/Users/sac/wasm4pm` should be committed to baseline state or stashed if they violate read-only status.
   - The untracked baseline manifests `.gc-sealed-baseline` should be handled appropriately (such as being explicitly excluded from the `git status` check in `dogfood_gc006.rs` or permitted within the baseline verification logic). The check in `dogfood_gc006.rs` must be updated to parse and assert against `.gc-sealed-baseline` instead of failing blindly on any untracked or modified files.
