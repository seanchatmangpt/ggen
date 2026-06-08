## 2026-06-07T01:12:52Z

You are teamwork_preview_worker. Your working directory is /Users/sac/ggen/.agents/teamwork_preview_worker_gc005a_1/.
Your task:
1. Fix any duplicate keys or syntax errors in `/Users/sac/ggen/crates/gc005-wasm4pm-adapter/Cargo.toml` and `/Users/sac/tower-lsp-max/crates/gc005-wasm4pm-adapter/Cargo.toml`.
2. Add `tower-lsp = "0.20"` as a dev-dependency to `/Users/sac/tower-lsp-max/crates/gc005-wasm4pm-adapter/Cargo.toml` (and `/Users/sac/ggen/crates/gc005-wasm4pm-adapter/Cargo.toml` if needed) to resolve compilation of the test `dogfood_gc005`.
3. Correct the path resolution in `/Users/sac/tower-lsp-max/crates/gc005-wasm4pm-adapter/tests/dogfood_gc006.rs` to search upwards for the workspace root instead of hardcoding `.parent().unwrap().parent().unwrap()`.
4. Create baseline manifest files `.gc-sealed-baseline` in the roots of `/Users/sac/wasm4pm` and `/Users/sac/wasm4pm-compat`.
   - The baseline manifest must record in JSON format:
     - `tracked_status`: expected tracked git status changes (e.g. for `wasm4pm`, the modified files: `Cargo.lock`, `crates/wasm4pm-algos/Cargo.toml`, `crates/wasm4pm-algos/src/gall.rs`; for `wasm4pm-compat`, empty `{}`).
     - `ignored_inventory`: list of expected ignored files/directories (like `target/`, `node_modules/`, `.DS_Store`, `.gc-sealed-baseline`).
     - `allowed_ignored_directories`: allowed ignored directories (e.g. `target`, `node_modules`).
     - `forbidden_generated_paths`: list of patterns that must not appear in ignored/untracked output (e.g., `gc005`, `gc006`).
     - `digest`: a cryptographic digest (SHA-256) computed over the serialized JSON representation of all other fields (excluding the digest field itself, e.g. with digest set to empty string or omitted during hashing).
   - Also, append `.gc-sealed-baseline` to `.git/info/exclude` in both `/Users/sac/wasm4pm` and `/Users/sac/wasm4pm-compat` to ignore it locally from standard git status.
5. Update the integration tests `dogfood_gc006.rs` in both `/Users/sac/ggen/crates/ggen-projection/tests/` and `/Users/sac/tower-lsp-max/crates/gc005-wasm4pm-adapter/tests/` to:
   - Read and parse `.gc-sealed-baseline` manifests from the target workspaces `/Users/sac/wasm4pm` and `/Users/sac/wasm4pm-compat`.
   - Verify the cryptographic digest in the manifest.
   - Run `git status --porcelain --ignored` and parse the output, validating each line against the baseline:
     - Assert there are zero new tracked changes (only the baselined ones are allowed).
     - Assert there are zero new untracked non-baselined files (no untracked files are allowed at all, except any listed in the manifest).
     - Assert there are zero new ignored files outside the baselined inventory.
     - Assert no writes have occurred from `ggen` or `tower-lsp-max` into the sealed repositories.
6. Verify that `cargo test` runs and passes successfully for:
   - `cargo test -p ggen-projection --test dogfood_gc005`
   - `cargo test -p ggen-projection --test dogfood_gc006`
   - `cargo test -p gc005-wasm4pm-adapter --test dogfood_gc005`
   - `cargo test -p gc005-wasm4pm-adapter --test dogfood_gc006`
   - `cargo test -p wasm4pm-lsp --test dogfood_gc005`
7. Write your report to `/Users/sac/ggen/.agents/teamwork_preview_worker_gc005a_1/handoff.md` summarizing files edited, command outputs, and pass/fail test status.
