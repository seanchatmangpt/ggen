# Handoff Report - Workspace Activation of ggen-projection

## 1. Observation

Direct observations of workspace configuration, crate definitions, and dependency structures:

### A. Workspace Cargo Configuration (`/Users/sac/ggen/Cargo.toml`)
- The workspace `members` list does not include `crates/ggen-projection`, `crates/genesis-construct8`, or `crates/genesis-lockchain`:
  ```toml
  32: [workspace]
  33: members = [
  34:   "crates/ggen-a2a-mcp",
  35:   "crates/ggen-config",
  36:   "crates/ggen-marketplace",
  37:   "crates/ggen-core",
  38:   "crates/ggen-cli",
  39:   "crates/ggen-graph",
  40:   "crates/ggen-lsp",
  41:   "crates/ggen-lsp-mcp",
  42:   "crates/ggen-lsp-a2a",
  43:   # Genesis kernel crates (pure Rust, IO-free)
  44:   "crates/genesis-types-v2",
  45:   "crates/genesis-schema-v2",
  46:   "crates/genesis-core-v2",
  47:   # DORMANT: no_std variant for wasm32-unknown-unknown targets
  48:   "crates/genesis-core",
  49:   "crates/cpmp",
  50:   "crates/stpnt",
  51:   # ARCHIVED out of the v26.5.29 build boundary ...
  58:   ]
  ```
- The `[workspace.dependencies]` block lacks entries for `ggen-projection`, `knhk-construct8`, `genesis-lockchain`, and `rio_turtle`.

### B. Target Crate Configuration (`/Users/sac/ggen/crates/ggen-projection/Cargo.toml`)
- The target crate name is `ggen-projection` (version `1.0.0`):
  ```toml
  1: [package]
  2: name = "ggen-projection"
  3: version = "1.0.0"
  ```
- It specifies a local dependency on `knhk-construct8` using `workspace = true`:
  ```toml
  15: knhk-construct8 = { workspace = true }
  ```

### C. Transit Crate 1: `knhk-construct8` (`/Users/sac/ggen/crates/genesis-construct8/Cargo.toml`)
- The package name is `knhk-construct8` (version `1.0.0`):
  ```toml
  1: [package]
  2: name = "knhk-construct8"
  3: version = "1.0.0"
  ```
- It defines dependencies on `rio_turtle` and `genesis-lockchain` using `workspace = true`:
  ```toml
  22: rio_turtle = { workspace = true }
  ...
  25: genesis-lockchain = { workspace = true }
  ```

### D. Transit Crate 2: `genesis-lockchain` (`/Users/sac/ggen/crates/genesis-lockchain/Cargo.toml`)
- The package name is `genesis-lockchain` (version `1.0.0`):
  ```toml
  1: [package]
  2: name = "genesis-lockchain"
  3: version = "1.0.0"
  ```
- It does not define any `workspace = true` dependencies that are missing from the workspace root (it relies on `blake3` and `chicago-tdd-tools` which are already in `[workspace.dependencies]`).

### E. Root `Cargo.lock` (`/Users/sac/ggen/Cargo.lock`)
- The resolved version of `rio_turtle` in the repository lockfile is `0.8.6`:
  ```toml
  6111: name = "rio_turtle"
  6112: version = "0.8.6"
  ```

---

## 2. Logic Chain

1. **Goal**: Activate the `ggen-projection` crate in the workspace.
2. **Step 1**: To compile `ggen-projection` in the workspace, we must add `"crates/ggen-projection"` to the `members` list in `/Users/sac/ggen/Cargo.toml`.
3. **Step 2**: Since `ggen-projection` references `knhk-construct8 = { workspace = true }`, the workspace Cargo metadata needs to declare the package `knhk-construct8` in the `[workspace.dependencies]` block.
4. **Step 3**: `knhk-construct8` is located at `crates/genesis-construct8`. For it to compile in the workspace, `"crates/genesis-construct8"` should be added to the `members` list.
5. **Step 4**: Since `knhk-construct8` references `genesis-lockchain = { workspace = true }`, the workspace must declare `genesis-lockchain` in its `[workspace.dependencies]`.
6. **Step 5**: `genesis-lockchain` is located at `crates/genesis-lockchain`. For it to compile, `"crates/genesis-lockchain"` must be added to the workspace `members` list.
7. **Step 6**: `knhk-construct8` also references `rio_turtle = { workspace = true }`. Thus, the workspace must declare `rio_turtle` in `[workspace.dependencies]`. Using the version from the repository `Cargo.lock` (`0.8.6`) ensures build consistency.

---

## 3. Caveats

- **Other Dormant Crates**: The dormant crates `ggen-membrane` and `genesis-wasm-shell` are also excluded from workspace membership. However, since `ggen-projection` does not reference them, they do not need to be activated to compile `ggen-projection`.
- **Local Crate Dependencies**: The crate `genesis-lockchain` and `knhk-construct8` use some external dependencies directly (e.g. `bincode = "1.3"` and `rio_api = "0.8"`). These do not use `workspace = true`, so they do not strictly need to be moved to the workspace root, although future alignment might be recommended.

---

## 4. Conclusion

To successfully activate `ggen-projection`, the following modifications are required in `/Users/sac/ggen/Cargo.toml`:

### A. Add Members to `[workspace]`
Insert the paths of the three crates into the `members` block:

```toml
[workspace]
members = [
  ...
  "crates/genesis-lockchain",
  "crates/genesis-construct8",
  "crates/ggen-projection",
]
```

### B. Add Dependencies to `[workspace.dependencies]`
Add the following declarations:

```toml
# External dependencies
rio_turtle = "0.8.6"

# Workspace-local crates
genesis-lockchain = { path = "crates/genesis-lockchain", version = "1.0.0" }
knhk-construct8 = { path = "crates/genesis-construct8", version = "1.0.0" }
ggen-projection = { path = "crates/ggen-projection", version = "1.0.0" }
```

---

## 5. Verification Method

Once the changes are made, verify by running the following commands from the workspace root (`/Users/sac/ggen`):

1. **Verify Metadata & Dependency Resolution**:
   ```bash
   cargo metadata --format-version 1 > /dev/null
   ```
   *Expected result*: Exits with code 0 (no syntax/resolution errors).

2. **Verify Compilation of Activated Crates**:
   ```bash
   cargo check -p genesis-lockchain
   cargo check -p knhk-construct8
   cargo check -p ggen-projection
   ```
   *Expected result*: All check compilations finish successfully.

3. **Verify Tests**:
   ```bash
   cargo test -p ggen-projection
   cargo test -p knhk-construct8
   cargo test -p genesis-lockchain
   ```
   *Expected result*: All tests pass or run successfully.
