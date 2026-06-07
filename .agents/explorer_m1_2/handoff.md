# Handoff Report - explorer_m1_2

## 1. Observation

During the investigation, the following files and configurations were inspected:

### A. Root Workspace Configuration (`/Users/sac/ggen/Cargo.toml`)
- The workspace members do not list `crates/ggen-projection`, `crates/genesis-construct8`, or `crates/genesis-lockchain` under `[workspace]`:
  ```toml
  [workspace]
  members = [
    "crates/ggen-a2a-mcp",
    "crates/ggen-config",
    "crates/ggen-marketplace",
    "crates/ggen-core",
    "crates/ggen-cli",
    "crates/ggen-graph",
    "crates/ggen-lsp",
    "crates/ggen-lsp-mcp",
    "crates/ggen-lsp-a2a",
    # Genesis kernel crates (pure Rust, IO-free)
    "crates/genesis-types-v2",
    "crates/genesis-schema-v2",
    "crates/genesis-core-v2",
    # DORMANT: no_std variant for wasm32-unknown-unknown targets
    "crates/genesis-core",
    "crates/cpmp",
    "crates/stpnt",
    # ARCHIVED out of the v26.5.29 build boundary (SETTLEMENT-CHECK-1)...
  ]
  ```
- The `[workspace.dependencies]` section does not contain declarations for `knhk-construct8`, `genesis-lockchain`, or `rio_turtle`.

### B. Target Package Configuration (`/Users/sac/ggen/crates/ggen-projection/Cargo.toml`)
- The target package `ggen-projection` (version `1.0.0`) inherits the following dependencies from the workspace:
  ```toml
  [dependencies]
  serde = { workspace = true, features = ["derive"] }
  serde_json = { workspace = true }
  chrono = { workspace = true, features = ["serde"] }
  thiserror = { workspace = true }
  anyhow = { workspace = true }
  blake3 = { workspace = true }
  knhk-construct8 = { workspace = true }

  [dev-dependencies]
  tempfile = { workspace = true }
  ```
- Running check directly on it fails with:
  ```
  error: failed to parse manifest at `/Users/sac/ggen/crates/ggen-projection/Cargo.toml`
  Caused by:
    error inheriting `knhk-construct8` from workspace root manifest's `workspace.dependencies.knhk-construct8`
  Caused by:
    `dependency.knhk-construct8` was not found in `workspace.dependencies`
  ```

### C. Upstream Dependency `knhk-construct8` Configuration (`/Users/sac/ggen/crates/genesis-construct8/Cargo.toml`)
- The dependency `knhk-construct8` inherits its dependencies as follows:
  ```toml
  [dependencies]
  blake3 = { workspace = true }
  serde = { workspace = true, features = ["derive"] }
  serde_json = { workspace = true }
  chrono = { workspace = true, features = ["serde"] }
  dashmap = { workspace = true }
  thiserror = { workspace = true }
  tracing = { workspace = true }
  anyhow = { workspace = true }
  clap = { workspace = true }
  rio_turtle = { workspace = true }
  rio_api = "0.8"
  hex = { workspace = true }
  genesis-lockchain = { workspace = true }
  ```
- Running check directly on it fails with:
  ```
  error: failed to parse manifest at `/Users/sac/ggen/crates/genesis-construct8/Cargo.toml`
  Caused by:
    error inheriting `genesis-lockchain` from workspace root manifest's `workspace.dependencies.genesis-lockchain`
  Caused by:
    `dependency.genesis-lockchain` was not found in `workspace.dependencies`
  ```

### D. Transitive Upstream Dependency `genesis-lockchain` Configuration (`/Users/sac/ggen/crates/genesis-lockchain/Cargo.toml`)
- The dependency `genesis-lockchain` contains the following dependencies (none of which are missing from the workspace root manifest):
  ```toml
  [dependencies]
  blake3 = { workspace = true }
  bincode = "1.3"
  serde = { version = "1.0", features = ["derive"] }
  sled = "0.34"
  thiserror = "2.0"
  sha2 = "0.10"
  git2 = "0.20"
  hex = "0.4"
  ```
- Running `cargo check --manifest-path crates/genesis-lockchain/Cargo.toml` compiles successfully:
  ```
      Checking git2 v0.20.4
      Checking genesis-lockchain v1.0.0 (/Users/sac/ggen/crates/genesis-lockchain)
      Finished `dev` profile [unoptimized + debuginfo] target(s) in 41.63s
  ```

### E. Workspace Lockfile (`/Users/sac/ggen/Cargo.lock`)
- The lockfile lists `rio_turtle` version as:
  ```toml
  [[package]]
  name = "rio_turtle"
  version = "0.8.6"
  ```

### F. Workspace Documentation (`/Users/sac/ggen/CLAUDE.md`)
- Architecture documentation explicitly documents these crates as dormant:
  ```
  `genesis-construct8`, `genesis-lockchain`, `genesis-wasm-shell`, `ggen-membrane`, `ggen-projection`. These directories exist but are excluded from `Cargo.toml` `members`; treat as non-compiled reference material until activated.
  ```

---

## 2. Logic Chain

1. **Workspace Member Inclusion**: To compile a package as part of the workspace, its path must be added to the `workspace.members` array in the root `Cargo.toml`. Therefore, `crates/ggen-projection` must be added.
2. **First-Party Dependency Resolution**: `ggen-projection` relies on `knhk-construct8` via `{ workspace = true }`, and `knhk-construct8` relies on `genesis-lockchain` via `{ workspace = true }`. To make these first-party local dependencies buildable under the workspace boundary:
   - Both `crates/genesis-construct8` and `crates/genesis-lockchain` should be added to `workspace.members`.
   - Their path definitions must be specified in the `[workspace.dependencies]` section of the root `Cargo.toml`.
3. **Third-Party Dependency Resolution**: `knhk-construct8` relies on `rio_turtle` via `{ workspace = true }`. `rio_turtle` is a third-party dependency from crates.io. In order for cargo to resolve this, `rio_turtle` must be defined in the root `[workspace.dependencies]`.
4. **Lockfile Alignment**: The exact version of `rio_turtle` already resolved in the workspace's `Cargo.lock` is `0.8.6`. Thus, the workspace dependency entry should specify this version.

---

## 3. Caveats

- **Active features**: Some of the activated dependencies or dev-dependencies might have other feature flags which are currently unused, but may be needed during full integration testing.
- **Other Dormant Crates**: Crates like `genesis-wasm-shell` and `ggen-membrane` remain dormant. They do not need to be activated to check `ggen-projection`, but their activation would follow a similar pattern if desired in the future.
- **Lockchain Database**: `genesis-lockchain` relies on `sled` database which creates a local file lock during testing or execution. This must be handled cleanly in tests to prevent locking conflicts.

---

## 4. Conclusion

To successfully activate `ggen-projection` in the workspace without build errors, the following changes to `/Users/sac/ggen/Cargo.toml` are required:

### A. Add Crate Paths to `workspace.members`
Add the following members to the `members` array in `Cargo.toml`:
```toml
  "crates/genesis-lockchain",
  "crates/genesis-construct8",
  "crates/ggen-projection",
```

### B. Add Dependency Specifications to `[workspace.dependencies]`
Add the following entries under `[workspace.dependencies]` in the root `Cargo.toml`:
```toml
# KNHK Genesis local components
genesis-lockchain = { path = "crates/genesis-lockchain", version = "1.0.0" }
knhk-construct8 = { path = "crates/genesis-construct8", version = "1.0.0" }
ggen-projection = { path = "crates/ggen-projection", version = "1.0.0" }

# Third-party RDF parser
rio_turtle = "0.8.6"
```

---

## 5. Verification Method

To independently verify the suggested changes after they are applied:

1. **Check compilation of each individual crate**:
   - Run `cargo check -p genesis-lockchain` (expected output: compiles successfully)
   - Run `cargo check -p knhk-construct8` (expected output: compiles successfully)
   - Run `cargo check -p ggen-projection` (expected output: compiles successfully)
2. **Check the entire workspace**:
   - Run `cargo check --workspace --all-targets` (expected output: no compiler errors across any workspace members)
3. **Run the tests for the newly activated crate**:
   - Run `cargo test -p ggen-projection` (expected output: all tests pass, including `test_ocel2_projection`, `test_nquads_projection`, `test_prov_projection`, `test_dcat_projection`, and `test_shacl_refusal_projection`)
4. **Invalidation condition**:
   - If `cargo check -p ggen-projection` fails with a missing workspace dependency error, verify that `[workspace.dependencies]` contains the exact keys/versions listed in the Conclusion.
