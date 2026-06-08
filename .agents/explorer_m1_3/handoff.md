# Handoff Report: Workspace Activation of `ggen-projection`

## 1. Observation
I directly observed the following configurations and paths:

1. **Root Workspace Definition (`/Users/sac/ggen/Cargo.toml`)**:
   Under `[workspace]`, members are listed from lines 33 to 58. `crates/ggen-projection` is currently absent from this list.
   ```toml
   32: [workspace]
   33: members = [
   34:   "crates/ggen-a2a-mcp",
   ...
   50:   "crates/stpnt",
   51:   # ARCHIVED out of the v26.5.29 build boundary...
   58:   ]
   ```
   Under `[workspace.dependencies]`, the following targeted dormant crates/dependencies are missing:
   - `ggen-projection`
   - `knhk-construct8`
   - `genesis-lockchain`
   - `rio_turtle`

2. **Target Crate Configuration (`/Users/sac/ggen/crates/ggen-projection/Cargo.toml`)**:
   The package name is declared as `ggen-projection` at line 2. It has the following dependencies using the workspace inheritance pattern (`workspace = true`):
   ```toml
   8: [dependencies]
   9: serde = { workspace = true, features = ["derive"] }
   10: serde_json = { workspace = true }
   11: chrono = { workspace = true, features = ["serde"] }
   12: thiserror = { workspace = true }
   13: anyhow = { workspace = true }
   14: blake3 = { workspace = true }
   15: knhk-construct8 = { workspace = true }
   ```

3. **Sub-Dependency Crate Configuration (`/Users/sac/ggen/crates/genesis-construct8/Cargo.toml`)**:
   The package name is declared as `knhk-construct8` at line 2. It lists its dependencies as:
   ```toml
   12: [dependencies]
   13: blake3 = { workspace = true }
   14: serde = { workspace = true, features = ["derive"] }
   15: serde_json = { workspace = true }
   16: chrono = { workspace = true, features = ["serde"] }
   17: dashmap = { workspace = true }
   18: thiserror = { workspace = true }
   19: tracing = { workspace = true }
   20: anyhow = { workspace = true }
   21: clap = { workspace = true }
   22: rio_turtle = { workspace = true }
   23: rio_api = "0.8"
   24: hex = { workspace = true }
   25: genesis-lockchain = { workspace = true }
   ```

4. **Sub-Dependency Crate Configuration (`/Users/sac/ggen/crates/genesis-lockchain/Cargo.toml`)**:
   The package name is declared as `genesis-lockchain` at line 2. It inherits the following workspace dependencies:
   ```toml
   14: [dependencies]
   15: blake3 = { workspace = true }
   ...
   30: [dev-dependencies]
   31: chicago-tdd-tools = { workspace = true }
   ```

5. **Lockfile Resolutions (`/Users/sac/ggen/Cargo.lock`)**:
   - `rio_turtle` is already resolved at version `0.8.6` (lines 6111–6119).
   - `genesis-lockchain` is resolved at version `1.0.0` (lines 2473–2486).

---

## 2. Logic Chain
To activate the target crate `ggen-projection` inside the workspace:

1. **Workspace Inclusion**: Cargo requires any active package directory to be declared in the `[workspace].members` array of the root `Cargo.toml`. Hence, `"crates/ggen-projection"` must be added.
2. **Transit Dependency Chain Resolution**:
   - `ggen-projection` inherits its dependency on `knhk-construct8` from the workspace (`knhk-construct8 = { workspace = true }`).
   - Thus, `knhk-construct8` must be defined in the root `[workspace.dependencies]` table, and its directory `"crates/genesis-construct8"` should be added to `[workspace].members` so that it is compiled/tested.
   - `knhk-construct8` inherits its dependency on `genesis-lockchain` and `rio_turtle` from the workspace.
   - Consequently, `genesis-lockchain` and `rio_turtle` must be defined in the root `[workspace.dependencies]` table, and `"crates/genesis-lockchain"` should be added to `[workspace].members`.
3. **Version Alignment**:
   - `rio_turtle` version `0.8.6` must be added to `[workspace.dependencies]` since it is a third-party dependency.
   - Local path-based declarations are needed for `ggen-projection`, `knhk-construct8`, and `genesis-lockchain` under `[workspace.dependencies]` to correctly map them as workspace-internal crates.

---

## 3. Caveats
- I did not test building/running other dormant packages like `ggen-membrane` and `genesis-wasm-shell` as they are not transitive dependencies of `ggen-projection`. However, `ggen-membrane` depends on `knhk-construct8`, so once these adjustments are made, it will also be trivial to activate `ggen-membrane`.
- I assumed the versions `1.0.0` defined in the dormant crates' `Cargo.toml` files are the desired target versions for this milestone since they match the lockfile and the package manifests.

---

## 4. Conclusion
To successfully activate `ggen-projection` in the cargo workspace, the root `/Users/sac/ggen/Cargo.toml` must be updated with the following:
1. Add to `[workspace].members`:
   - `"crates/genesis-lockchain"`
   - `"crates/genesis-construct8"`
   - `"crates/ggen-projection"`
2. Add to `[workspace.dependencies]`:
   - `rio_turtle = "0.8.6"`
   - `genesis-lockchain = { path = "crates/genesis-lockchain", version = "1.0.0" }`
   - `knhk-construct8 = { path = "crates/genesis-construct8", version = "1.0.0" }`
   - `ggen-projection = { path = "crates/ggen-projection", version = "1.0.0" }`

A complete patch representing these modifications has been written to:
`/Users/sac/ggen/.agents/explorer_m1_3/workspace_activation.patch`

---

## 5. Verification Method
To independently verify the activation and configuration changes:
1. Apply the patch file `/Users/sac/ggen/.agents/explorer_m1_3/workspace_activation.patch` to the root `Cargo.toml`.
2. Run the cargo build command to verify that all three packages compile correctly without dependency errors:
   ```bash
   cargo check -p ggen-projection
   ```
3. Run the cargo test command to verify unit and integration tests run successfully:
   ```bash
   cargo test -p ggen-projection
   ```
4. **Invalidation condition**: Any change to target package names or structural moves of `crates/genesis-construct8` or `crates/genesis-lockchain` will invalidate the paths specified in this patch.
