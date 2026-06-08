# Detailed Changes Made

## Crates modified

### `ggen-pack-proofs`
- Modified `crates/ggen-pack-proofs/templates/dogfood_gc006.rs.tmpl`:
  - Removed all hardcoded `/Users/sac` prefix values.
  - Introduced dynamic workspace root detection by starting from `std::env::current_dir()` and traversing parent directories until `ggen.toml` is found.
  - Derived the sibling directory `tower-lsp-max` relative to the workspace parent directory, so that the path is resolved dynamically on any host environment.
  - Replaced hardcoded checks for forbidden shadow crates in the workspace to run dynamically from the resolved `ggen_root`.
  - Added git conformance check for read-only workspaces (`wasm4pm` and `wasm4pm-compat`) to ensure they remain 100% clean (no untracked or modified files).

### `ggen-projection`
- Modified `crates/ggen-projection/Cargo.toml`:
  - Registered the integration test `dogfood_gc006` located at `tests/dogfood_gc006.rs`.
- Projected `crates/ggen-projection/tests/dogfood_gc006.rs` by running the `sync_target` projection engine:
  - Invoked with `--manifest crates/ggen-pack-proofs/manifest_gc006.toml` to correctly populating Tera context fields `checkpoint` and `proofs_gc006`.
  - The rendered test compiles with 0 warnings/errors and passes cleanly.

## Build and Verification
- Ran `cargo test -p ggen-projection --test dogfood_gc006` and verified compilation and successful execution.
- Ran `cargo make check` and `cargo test -p ggen -- --test-threads 1` to verify workspace-wide compliance.
