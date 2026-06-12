# Handoff Report - Cargo.toml version survey

This report surveys all `Cargo.toml` files in the root and under `crates/` to identify the packages and dependencies referencing version "26.5.29" (or other versions) that must be upgraded to "26.6.9".

## 1. Observation

Direct observations of version definitions and dependency references in `Cargo.toml` files:

### Root Manifest (`/Users/sac/ggen/Cargo.toml`)
* **Workspace Package Version** (Line 2):
  ```toml
  version = "26.5.29"
  ```
* **Workspace Dependency Definitions**:
  * Line 78: `clap-noun-verb = "26.5.19"`
  * Line 79: `clap-noun-verb-macros = "26.5.19"`
  * Line 115: `ggen-core = { path = "crates/ggen-core", version = "26.5.29" }`
  * Line 116: `ggen-cli-lib = { path = "crates/ggen-cli", version = "26.5.29" }`
  * Line 132: `# ggen-yawl = { path = "crates/ggen-yawl", version = "26.5.4" }` (dormant/commented out)

### Genesis Core Manifest (`/Users/sac/ggen/crates/genesis-core/Cargo.toml`)
* **Package Version** (Line 3):
  ```toml
  version = "26.5.29"
  ```

### ggen CLI Manifest (`/Users/sac/ggen/crates/ggen-cli/Cargo.toml`)
* **Package Version Inheritance** (Lines 90-91):
  ```toml
  [package.version]
  workspace = true
  ```
* **Local Dependency Definitions**:
  * Line 55: `ggen-a2a-mcp = { version = "26.5.29", path = "../ggen-a2a-mcp" }`
  * Line 58: `ggen-lsp = { version = "26.5.29", path = "../ggen-lsp", optional = true }`
  * Line 59: `ggen-lsp-mcp = { version = "26.5.29", path = "../ggen-lsp-mcp", optional = true }`

### ggen Core Manifest (`/Users/sac/ggen/crates/ggen-core/Cargo.toml`)
* **Package Version Inheritance** (Lines 8-9):
  ```toml
  [package.version]
  workspace = true
  ```
* **Local Dependency Definitions**:
  * Line 121 (under `[dependencies.ggen-config]`): `version = "26.5.29"`
  * Line 125 (under `[dependencies.ggen-a2a-mcp]`): `version = "26.5.29"`
  * Line 129 (under `[dependencies.ggen-marketplace]`): `version = "26.5.29"`

### ggen LSP Manifest (`/Users/sac/ggen/crates/ggen-lsp/Cargo.toml`)
* **Package Version** (Line 3):
  ```toml
  version = "26.5.29"
  ```

### ggen LSP A2A Manifest (`/Users/sac/ggen/crates/ggen-lsp-a2a/Cargo.toml`)
* **Package Version** (Line 3):
  ```toml
  version = "26.5.29"
  ```
* **Local Dependency Definitions**:
  * Line 11: `ggen-lsp-mcp = { version = "26.5.29", path = "../ggen-lsp-mcp" }`
  * Line 12: `ggen-a2a-mcp = { version = "26.5.29", path = "../ggen-a2a-mcp" }`
  * Line 15: `ggen-lsp = { version = "26.5.29", path = "../ggen-lsp" }`

### ggen LSP MCP Manifest (`/Users/sac/ggen/crates/ggen-lsp-mcp/Cargo.toml`)
* **Package Version** (Line 3):
  ```toml
  version = "26.5.29"
  ```
* **Local Dependency Definitions**:
  * Line 11: `ggen-lsp = { version = "26.5.29", path = "../ggen-lsp" }`

### ggen Marketplace Manifest (`/Users/sac/ggen/crates/ggen-marketplace/Cargo.toml`)
* **Package Version Inheritance** (Lines 8-9):
  ```toml
  [package.version]
  workspace = true
  ```
* **Local Dependency Definitions**:
  * Line 61 (under `[dependencies.ggen-config]`): `version = "26.5.29"`

### ggen Core Examples Manifest (`/Users/sac/ggen/crates/ggen-core/examples/Cargo.toml`)
* **Workspace Package Version** (Line 16):
  ```toml
  version = "26.5.4"
  ```

---

## 2. Logic Chain

1. **Root Workspace Upgrades**: Since the root workspace package version (`version = "26.5.29"` in `/Users/sac/ggen/Cargo.toml` Line 2) is upgraded to `"26.6.9"`, all workspace crates inheriting their version via `[package.version] workspace = true` or `version.workspace = true` will automatically be upgraded. These inheriting crates are:
   - `ggen` (root package)
   - `ggen-cli-lib` (`crates/ggen-cli`)
   - `ggen-config` (`crates/ggen-config`)
   - `ggen-marketplace` (`crates/ggen-marketplace`)
   - `ggen-core` (`crates/ggen-core`)
   - `ggen-graph` (`crates/ggen-graph`)
   - `ggen-a2a-mcp` (`crates/ggen-a2a-mcp`)
   - `genesis-types` (`crates/genesis-types-v2`)
   - `genesis-schema` (`crates/genesis-schema-v2`)
   - `genesis-core-v2` (`crates/genesis-core-v2`)
2. **Explicit Package Version Upgrades**: Crates that explicitly define their package version as `"26.5.29"` instead of inheriting it must be upgraded manually in their respective `Cargo.toml` files:
   - `genesis-core` (`crates/genesis-core`) - Line 3
   - `ggen-lsp` (`crates/ggen-lsp`) - Line 3
   - `ggen-lsp-mcp` (`crates/ggen-lsp-mcp`) - Line 3
   - `ggen-lsp-a2a` (`crates/ggen-lsp-a2a`) - Line 3
3. **Workspace Dependency Upgrades**: Workspace-wide dependency declarations in `/Users/sac/ggen/Cargo.toml` that reference version `"26.5.29"` must be updated to `"26.6.9"`:
   - `ggen-core` (Line 115)
   - `ggen-cli-lib` (Line 116)
4. **Local Dependency Upgrades**: Specific packages define local dependencies with version constraints that must be updated to `"26.6.9"`:
   - `ggen-cli` (`crates/ggen-cli/Cargo.toml` Lines 55, 58, 59): `ggen-a2a-mcp`, `ggen-lsp`, `ggen-lsp-mcp`
   - `ggen-core` (`crates/ggen-core/Cargo.toml` Lines 121, 125, 129): `ggen-config`, `ggen-a2a-mcp`, `ggen-marketplace`
   - `ggen-lsp-a2a` (`crates/ggen-lsp-a2a/Cargo.toml` Lines 11, 12, 15): `ggen-lsp-mcp`, `ggen-a2a-mcp`, `ggen-lsp`
   - `ggen-lsp-mcp` (`crates/ggen-lsp-mcp/Cargo.toml` Line 11): `ggen-lsp`
   - `ggen-marketplace` (`crates/ggen-marketplace/Cargo.toml` Line 61): `ggen-config`
5. **Other Versions**:
   - The commented out dependency `ggen-yawl` in `Cargo.toml` (Line 132) references version `"26.5.4"`.
   - The sub-workspace in `crates/ggen-core/examples/` has `version = "26.5.4"` at `crates/ggen-core/examples/Cargo.toml` Line 16, which is inherited by all of its nested examples (`advanced-cli-tool`, `perf-library`, `async-web-service`, `wasm-crypto`, `embedded-iot`). If these also need to be aligned with the workspace version, they should be upgraded.

---

## 3. Caveats

* **Other 26.5 versions**: `clap-noun-verb` and `clap-noun-verb-macros` are declared at version `"26.5.19"` in the root workspace `Cargo.toml`. They represent external/published dependency versions, and thus were excluded from the upgrade list as they do not match the unified release version `"26.5.29"`.
* **Example/Archived Crates**: The archived example `examples/7-agent-validation` was excluded from active workspace member upgrades because it is commented out and excluded in `Cargo.toml`.
* **No Code Modifications**: Per constraints, no code modifications were made. The verification only involved finding and viewing file contents.

---

## 4. Conclusion

To transition from unified release version `"26.5.29"` to `"26.6.9"`, modifications must be made across **8 Cargo.toml files**:
1. `/Users/sac/ggen/Cargo.toml` (Root workspace version and dependency specifications)
2. `/Users/sac/ggen/crates/genesis-core/Cargo.toml` (Explicit package version)
3. `/Users/sac/ggen/crates/ggen-cli/Cargo.toml` (Explicit dependency constraints)
4. `/Users/sac/ggen/crates/ggen-core/Cargo.toml` (Explicit dependency constraints)
5. `/Users/sac/ggen/crates/ggen-lsp/Cargo.toml` (Explicit package version)
6. `/Users/sac/ggen/crates/ggen-lsp-a2a/Cargo.toml` (Explicit package version and dependencies)
7. `/Users/sac/ggen/crates/ggen-lsp-mcp/Cargo.toml` (Explicit package version and dependencies)
8. `/Users/sac/ggen/crates/ggen-marketplace/Cargo.toml` (Explicit dependency constraints)

Optionally, `/Users/sac/ggen/crates/ggen-core/examples/Cargo.toml` can be upgraded from `"26.5.4"` if version alignment is desired for examples.

---

## 5. Verification Method

* Run a grep check command to verify no references to version `"26.5.29"` remain after the implementer completes the task:
  ```bash
  grep -rn "26.5.29" --include="Cargo.toml" /Users/sac/ggen
  ```
  Expected output: No matches found.
* Verify the new version is uniformly set by running:
  ```bash
  grep -rn "26.6.9" --include="Cargo.toml" /Users/sac/ggen
  ```
  Expected output: Matches in all corresponding package and dependency lines detailed in Section 1.
