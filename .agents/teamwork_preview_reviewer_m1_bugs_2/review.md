# Milestone 1 Review Report

## Review Summary

**Verdict**: REQUEST_CHANGES

## Findings

### Critical Finding 1: INTEGRITY VIOLATION - Facade Implementation of Dependency Graph

- **What**: The method `build_dependency_graph` does not implement actual dependency tracking; it returns empty lists of dependency edges for all packages.
- **Where**: `crates/ggen-marketplace/src/marketplace/install.rs` (Lines 1172-1188)
- **Why**: This is a facade implementation that looks like it builds a dependency graph for topological sorting but implements no real logic. Comments explicitly state: *"In a real implementation, we would populate edges from package metadata... For now, we return the initialized graph."* This violates the integrity rule against dummy or facade implementations.
- **Suggestion**: Implement actual dependency edge extraction from package metadata/releases and construct the dependency graph to support true topological sorting and parallel installation.

### Critical Finding 2: INTEGRITY VIOLATION - Facade Checksum Verification & Incompatibility

- **What**: There is a fundamental incompatibility and design facade between the checksum calculation in `generate_registry_index.py` and the checksum verification in `install.rs`.
  1. The Python script generates a checksum by walking and hashing the package directory's uncompressed files.
  2. The installer `verify_pack_digest` verifies the checksum of the raw downloaded compressed archive bytes (e.g. the ZIP archive).
- **Where**:
  - `marketplace/scripts/generate_registry_index.py` (Lines 25-55)
  - `crates/ggen-marketplace/src/marketplace/install.rs` (Lines 726-746)
- **Why**: Because the script hashes the directory and the installer hashes the zip file bytes, these checksums will never match. Any real installation of a package from the registry will always fail with a digest mismatch error. Additionally, the download URL is hardcoded in the script to the entire repository zip, ignoring any custom package `download_url`.
- **Suggestion**: Ensure both script and installer use a consistent hashing strategy (e.g., hashing the packaged zip file itself) and make sure the python script packages the directory to calculate the actual archive checksum.

### Major Finding 3: TOML Syntax Errors in Packages

- **What**: Six package manifests (`package.toml`) contain syntax errors, preventing them from being parsed or indexed.
- **Where**:
  - `marketplace/packages/customer-loyalty-rewards/package.toml` (Line 60)
  - `marketplace/packages/iso-20022-payments/package.toml` (Line 36)
  - `marketplace/packages/kyc-aml-compliance/package.toml` (Line 26)
  - `marketplace/packages/order-management-system/package.toml` (Line 60)
  - `marketplace/packages/trading-platform/package.toml` (Line 26)
  - `marketplace/packages/rest-api-template/package.toml` (Line 35)
- **Why**:
  - Five packages use invalid Markdown list syntax (`- "value"`) under `[features]`.
  - `rest-api-template` has an inline table spread across multiple lines (`frameworks = { ... }`), which is invalid in TOML.
- **Suggestion**: Reformat `[features]` lists into standard TOML arrays (`features = [ "...", "..." ]`) and make the inline table in `rest-api-template` single-line or use a standard TOML table.

### Major Finding 4: Inability to Resolve Semver Requirements

- **What**: The dependency resolver parses dependency version requirements (e.g. `^1.0.0` or `>=1.2.0`) directly as concrete `PackageVersion` strings, which fail version validation or repository lookup.
- **Where**: `crates/ggen-marketplace/src/marketplace/install.rs` (Lines 151-152, 1025-1026)
- **Why**: `PackageVersion` requires strict `MAJOR.MINOR.PATCH` syntax. Any operator like `^` or `>=` causes validation to fail. Even if validation is bypassed, `get_package_version` looks for a version literally named after the requirement string (e.g. `^1.0.0`) in the registry and fails.
- **Suggestion**: Integrate a proper semver matching resolver (e.g., using the `semver` crate's `VersionReq`) to match package releases from the registry against the requirements.

### Major Finding 5: Missing Fallback and Crash on `tomli` in `generate_registry_index.py`

- **What**: The script crashes on Python < 3.11 when `tomli` is not installed, even though the standard `toml` package is present.
- **Where**: `marketplace/scripts/generate_registry_index.py` (Lines 15-23)
- **Why**: The script only tries to import `tomllib` and `tomli`. In the system's Python 3.9 environment, `toml` package is installed but the script does not try importing it, causing a crash.
- **Suggestion**: Add a fallback import for `toml`:
  ```python
  try:
      import tomllib
  except ImportError:
      try:
          import tomli as tomllib
      except ImportError:
          try:
              import toml as tomllib
          except ImportError:
              ...
  ```

### Minor Finding 6: Silent Failure on Unsupported Archive Formats

- **What**: If the downloaded pack archive is in an unsupported format (neither `tar.gz` nor `ZIP`), `extract_pack` silently succeeds without raising an error.
- **Where**: `crates/ggen-marketplace/src/marketplace/install.rs` (Lines 797-802)
- **Why**: The format detection checks if it's `tar.gz` or `ZIP`. If it's neither, it does nothing and promotes an empty directory.
- **Suggestion**: Raise an `Error::InstallationFailed` if the detected format is unsupported.

### Minor Finding 7: High Memory Pressure in Cache Verification

- **What**: `verify_digest` reads the entire `pack.dat` archive file into memory at once.
- **Where**: `crates/ggen-marketplace/src/marketplace/cache.rs` (Line 469)
- **Why**: Large packages (up to the 2GB cache limit) will consume huge amounts of memory and potentially trigger OOM.
- **Suggestion**: Stream the file contents into the SHA-256 hasher using a buffered reader.

---

## Verified Claims

- **Crate Compilation and Tests** → verified via `cargo test -p ggen-marketplace` → **PASS**
- **LRU Cache Eviction Logic** → verified via checking size and count pressure checks → **PASS** (eviction count/size checks are logically sound, though double storage uses twice the cache size)
- **Zip Slip Vulnerability Protection** → verified via unit tests checking path traversal checks → **PASS** (the code successfully intercepts parent directories and path escapes)

---

## Coverage Gaps

- **Installer Verification in Test Suite** — risk level: **HIGH** — recommendation: **Investigate/Implement**. The test suite completely lacks tests for `Installer::install_pack`, `Installer::install`, and `Installer::batch_install` with actual or simulated registry data. This is why the archive checksum mismatch bug went undetected.

---

## Unverified Items

- **Regulated Security Profile Enforcement** — reason not verified: the profiles are checked in static unit tests, but real integration with registry downloads was not tested because no live download test exists.
