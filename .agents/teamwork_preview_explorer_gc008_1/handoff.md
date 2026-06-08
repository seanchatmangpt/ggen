# Handoff Report — GC008 Verification Target Exploration

This report details the read-only exploration of `crates/ggen-pack-clap-noun-verb`.

## 1. Observation
1. In `crates/ggen-pack-clap-noun-verb/Cargo.toml`, we observed the following configuration:
   ```toml
   [package]
   name = "clap-noun-verb-pack-lsp"
   version = "26.6.6"
   edition = "2021"
   publish = false

   [dependencies]
   tower-lsp = "0.20.0"
   serde_json = "1.0"
   tokio = { version = "1.0", features = ["rt-multi-thread", "macros", "io-std"] }
   ```
   No `[dev-dependencies]` section exists in this file.

2. Running `cargo test -p clap-noun-verb-pack-lsp` inside `/Users/sac/ggen` produced the following verbatim compilation errors:
   ```
   error[E0432]: unresolved import `walkdir`
    --> crates/ggen-pack-clap-noun-verb/tests/dogfood_clap_command_route.rs:3:5
     |
   3 | use walkdir::WalkDir;
     |     ^^^^^^^ use of unresolved module or unlinked crate `walkdir`

   error[E0432]: unresolved import `walkdir`
    --> crates/ggen-pack-clap-noun-verb/tests/dogfood_no_lsp_mutation.rs:3:5
     |
   3 | use walkdir::WalkDir;
     |     ^^^^^^^ use of unresolved module or unlinked crate `walkdir`

   error[E0432]: unresolved import `tempfile`
    --> crates/ggen-pack-clap-noun-verb/tests/f8_equation_enforcement.rs:2:5
     |
   2 | use tempfile::TempDir;
     |     ^^^^^^^^ use of unresolved module or unlinked crate `tempfile`

   error[E0432]: unresolved import `ggen_projection`
    --> crates/ggen-pack-clap-noun-verb/tests/f8_equation_enforcement.rs:4:5
     |
   4 | use ggen_projection::{
     |     ^^^^^^^^^^^^^^^ use of unresolved module or unlinked crate `ggen_projection`
   ```

3. In `crates/ggen-pack-clap-noun-verb/tests/dogfood_clap_command_route.rs` at line 8:
   ```rust
   let lsp_path = PathBuf::from("/Users/sac/wasm4pm/crates/wasm4pm-lsp/src");
   ```

4. In `crates/ggen-pack-clap-noun-verb/tests/dogfood_gc005.rs` at lines 21-26:
   ```rust
   let mut bin = std::env::current_exe().unwrap();
   bin.pop();
   if bin.ends_with("deps") {
       bin.pop();
   }
   let bin = bin.join("wasm4pm-lsp");
   ```

5. In `crates/ggen-pack-clap-noun-verb/tests/dogfood_no_lsp_mutation.rs` at line 8:
   ```rust
   let lsp_path = PathBuf::from("/Users/sac/wasm4pm/crates/wasm4pm-lsp/src");
   ```

6. In `crates/ggen-pack-clap-noun-verb/tests/projection-map.json`, the template paths point to a different workspace package `ggen-pack-proofs`:
   ```json
   "template_path": "/Users/sac/ggen/crates/ggen-pack-proofs/templates/dogfood_gc008_b_c.rs.tmpl"
   ```

---

## 2. Logic Chain
1. **Unresolved Imports**: The test files (`dogfood_clap_command_route.rs`, `dogfood_no_lsp_mutation.rs`, `f8_equation_enforcement.rs`) import `walkdir`, `tempfile`, and `ggen_projection`.
2. **Missing Configuration**: Because `Cargo.toml` has no `[dev-dependencies]` declaring these crates, Cargo cannot locate them when building the test targets.
3. **External Dependencies**: The tests scan `/Users/sac/wasm4pm/crates/wasm4pm-lsp/src` and execute the `wasm4pm-lsp` binary. Therefore, the sibling workspace `wasm4pm` must be present and compiled for the tests to run successfully.
4. **Conclusion**: To build and verify these tests within `ggen`, the crate's `Cargo.toml` needs `[dev-dependencies]`, and the sibling workspace must be populated and built.

---

## 3. Caveats
- We assumed `/Users/sac/wasm4pm` is always present on systems executing these tests. If it is absent, tests like `dogfood_clap_command_route` and `dogfood_no_lsp_mutation` will gracefully pass (due to `if lsp_path.exists()` guards), but `dogfood_gc005` will panic due to the missing `wasm4pm-lsp` binary.

---

## 4. Conclusion
The validation server is structured as a standard LSP server implementing single-command execution and basic Rust CLI checks. However, the test harness is currently broken at compile-time due to a missing `[dev-dependencies]` section in the package's local `Cargo.toml`. Adding `walkdir`, `tempfile`, and `ggen-projection` to `[dev-dependencies]` is required to make the tests compile.

---

## 5. Verification Method
To verify the compilation issue and its fix:
1. Run `cargo test -p clap-noun-verb-pack-lsp` inside `/Users/sac/ggen`. Note the compilation errors.
2. Apply the proposed `[dev-dependencies]` patch to `crates/ggen-pack-clap-noun-verb/Cargo.toml`.
3. Re-run `cargo test -p clap-noun-verb-pack-lsp` and verify it compiles and runs.
