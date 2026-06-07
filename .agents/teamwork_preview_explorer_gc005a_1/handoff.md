# Handoff Report: GC005/GC006 Test Investigation

## 1. Observation
- In `/Users/sac/ggen/crates/gc005-wasm4pm-adapter/Cargo.toml`, lines 11-14 specify:
  ```toml
  11: wasm4pm-algos = { path = "../../../wasm4pm/crates/wasm4pm-algos" }
  12: ocel-core = { path = "../../../wasm4pm/crates/ocel-core" }
  13: ocel-core = { path = "../../../wasm4pm/crates/ocel-core" }
  14: wasm4pm-algos = { path = "../../../wasm4pm/crates/wasm4pm-algos" }
  ```
  This causes cargo commands to fail with:
  ```
  error: duplicate key
    --> crates/gc005-wasm4pm-adapter/Cargo.toml:13:1
     |
  13 | ocel-core = { path = "../../../wasm4pm/crates/ocel-core" }
     | ^^^^^^^^^
  ```
- In `/Users/sac/tower-lsp-max/crates/gc005-wasm4pm-adapter/Cargo.toml`, there are no dev-dependencies. As a result, compiling `crates/gc005-wasm4pm-adapter/tests/dogfood_gc005.rs` fails with E0433 (`cannot find module or crate tower_lsp`). However, running the identical test target under `wasm4pm-lsp` package (`cargo test -p wasm4pm-lsp --test dogfood_gc005`) compiles and passes successfully:
  ```
  running 1 test
  test test_gc005_wasm4pm_lsp_observation ... ok

  test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.73s
  ```
- Running the compiled test binary `./target/debug/deps/dogfood_gc006-133fcf7213360636` in `/Users/sac/tower-lsp-max` panics:
  ```
  thread 'test_gc006_authority_surface_lock' (1685652) panicked at crates/gc005-wasm4pm-adapter/tests/dogfood_gc006.rs:34:105:
  called `Result::unwrap()` on an `Err` value: Os { code: 2, kind: NotFound, message: "No such file or directory" }
  ```
- Running `git status --porcelain` in `/Users/sac/wasm4pm` yields:
  ```
   M Cargo.lock
   M crates/wasm4pm-algos/Cargo.toml
   M crates/wasm4pm-algos/src/gall.rs
  ?? .gc-sealed-baseline
  ```
  And in `/Users/sac/wasm4pm-compat` yields:
  ```
  ?? .gc-sealed-baseline
  ```
- Viewing `/Users/sac/wasm4pm/crates/wasm4pm-algos/src/gall.rs` showed:
  ```rust
  pub enum GallVerdict {
      Fit { fitness: f64 },
      Deviation { fitness: f64, missing: Vec<String> },
      Blocked { reason: String },
      Inconclusive { reason: String },
  }
  ```
  And in the adapter `/Users/sac/tower-lsp-max/crates/gc005-wasm4pm-adapter/src/lib.rs`:
  ```rust
  GallVerdict::Inconclusive { reason } => {
      ("WARNING", "WASM4PM-VERDICT-INCONCLUSIVE", format!("Conformance Verdict: INCONCLUSIVE ({})", reason))
  }
  ```

## 2. Logic Chain
1. The duplicate keys in `ggen` adapter's Cargo.toml break the whole cargo workspace in `ggen` (Observation 1). Thus, tests cannot run or build there.
2. In `tower-lsp-max` adapter's Cargo.toml, missing `tower_lsp` as a dev-dependency prevents compilation of `dogfood_gc005.rs` (Observation 2).
3. The `dogfood_gc006.rs` path resolution assumes the current directory is `crates/gc005-wasm4pm-adapter`, but since cargo runs from the workspace root (`/Users/sac/tower-lsp-max`), `parent().parent()` resolves to `/Users`, producing the `NotFound` error (Observation 3).
4. The uncommitted modifications in `wasm4pm` and untracked `.gc-sealed-baseline` in both workspaces violate the clean working tree assumption of the `dogfood_gc006` test (Observation 4).
5. Both `check_gall_conformance` and `gc005-wasm4pm-adapter` already support the `Inconclusive` / `INCONCLUSIVE` variant in their working copies (Observation 5).

## 3. Caveats
- No changes were made to source files or project manifests as we are running under a read-only investigation constraint.
- The build issue on the `ggen` side was not resolved due to the same read-only constraint.

## 4. Conclusion
The workspace tests for GC005 and GC006 are currently failing:
- `ggen` is completely broken at compile-time due to duplicate Cargo.toml keys.
- `tower-lsp-max` fails due to incorrect test directory path assumptions (`dogfood_gc006`) and missing dev-dependencies (`dogfood_gc005`).
- The `INCONCLUSIVE` requirement is already implemented in the code on disk but has not been committed/baselined, which causes the clean repository sterility checks to fail.

## 5. Verification Method
1. To verify the duplicate key issue: Run `cargo check` in `/Users/sac/ggen`. It will fail with a duplicate key error.
2. To verify the `tower-lsp-max` compile error: Run `cargo test -p gc005-wasm4pm-adapter --test dogfood_gc005` in `/Users/sac/tower-lsp-max`.
3. To verify the git cleanliness failures: Run `cargo test -p ggen-projection --test dogfood_gc006` in `/Users/sac/ggen` (after fixing the duplicate key block).
