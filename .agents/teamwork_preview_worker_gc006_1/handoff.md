# Handoff Report: Authority Surface Lock (GC006)

## 1. Observation
- **Original request templates**: `crates/ggen-pack-proofs/templates/dogfood_gc006.rs.tmpl` contained hardcoded `/Users/sac` paths to refer to the workspace roots of `ggen` and sibling repository `tower-lsp-max`.
- **Target test**: `crates/ggen-projection/tests/dogfood_gc006.rs` was generated with these hardcoded paths and was untracked.
- **Cargo test registration**: `crates/ggen-projection/Cargo.toml` did not list `dogfood_gc006` in its `[[test]]` targets.
- **Test execution**:
  - Run command `cargo test -p ggen-projection --test dogfood_gc006` completed successfully:
    ```
    running 1 test
    test test_gc006_authority_surface_lock ... ok
    ```
  - Verification check of the read-only repositories completed successfully: git porcelain checks on both `/Users/sac/wasm4pm` and `/Users/sac/wasm4pm-compat` returned empty stdout, proving they are clean and unmodified.

## 2. Logic Chain
- **Step 1 (Path Resolution)**: To eliminate hardcoded paths, we implemented dynamic parent-directory traversal in `dogfood_gc006.rs.tmpl`. It starts at `std::env::current_dir()` and climbs up until `ggen.toml` is found, identifying `ggen_root`. Sibling directory `tower-lsp-max` is located by looking at `ggen_root.parent()`.
- **Step 2 (Projection)**: By running `cargo run -p ggen-projection --bin sync_target` with the manifest parameter `manifest_gc006.toml`, the template is projected into `crates/ggen-projection/tests/dogfood_gc006.rs`.
- **Step 3 (Integration)**: Adding the test target declaration to `crates/ggen-projection/Cargo.toml` registers `dogfood_gc006` as a standard cargo test in the workspace.
- **Step 4 (Validation)**: Executing `cargo test -p ggen-projection --test dogfood_gc006` verifies that the dynamically-located source files are verified:
  - No shadow `wasm4pm` crates exist under `ggen/crates/`.
  - `wasm4pm-lsp` calls the neutral adapter `gc005-wasm4pm-adapter` and does not fake its own conformance loop.
  - The neutral adapter calls the sealed `wasm4pm-algos` check_gall_conformance authority instead of emitting fake FIT results.
  - The workspaces `~/wasm4pm` and `~/wasm4pm-compat` are verified as read-only and clean.

## 3. Caveats
- We assumed that `tower-lsp-max` and `ggen` are siblings located in the same parent directory on the runner host (which is true for the test environment).
- We assumed git CLI is installed and configured on the path for executing repository status checks (which is verified to be true on mac zsh).

## 4. Conclusion
The hardcoded `/Users/sac` paths have been successfully removed and replaced with dynamic workspace path resolution. The projected test runs and passes with zero warnings, enforcing the architectural integrity constraints of checkpoint GC006.

## 5. Verification Method
1. Verify projection logic by running:
   ```bash
   cargo run -p ggen-projection --bin sync_target -- --workspace /Users/sac/ggen --target /Users/sac/ggen/crates/ggen-projection/tests --staging-dir /Users/sac/ggen/crates/ggen-projection/tests/staging --receipt-sink /Users/sac/ggen/crates/ggen-projection/tests/receipts --pack-roots /Users/sac/ggen/crates/ggen-pack-proofs --manifest /Users/sac/ggen/crates/ggen-pack-proofs/manifest_gc006.toml
   ```
2. Verify that the test compiles and passes successfully:
   ```bash
   cargo test -p ggen-projection --test dogfood_gc006
   ```
3. Run the workspace-wide test checks:
   ```bash
   cargo make check
   cargo test -p ggen -- --test-threads 1
   ```
