# Milestone 1 Bug Fixes & Refinements

## 1. LRU Cache Eviction Bug
- **Location**: `crates/ggen-marketplace/src/marketplace/cache.rs:insert`
- **Change**: If the pack we are inserting/updating is already in the cache, it is now removed from the cache metadata and size registry before capacity check and LRU eviction.
- **Rationale**: Prevents a package from evicting itself if the cache is full and the package is the oldest accessed entry.

## 2. Save Metadata on Get
- **Location**: `crates/ggen-marketplace/src/marketplace/cache.rs:get`
- **Change**: Added `self.save_metadata()` invocation when metadata persistence is enabled to ensure access counts and last-accessed times are recorded to disk immediately.
- **Rationale**: Avoids loss of access/usage statistics when the system is shut down or queried.

## 3. Strict Dependency Cycle Detection
- **Location**: `crates/ggen-marketplace/src/marketplace/install.rs` (in `resolve_dependencies` and `batch_resolve_dependencies`)
- **Change**: Replaced BFS/queue traversal with post-order iterative DFS using a DFS stack state enum (`DfsState::Enter` / `DfsState::Exit`). If a circular dependency is encountered, `Error::DependencyCycle` is returned.
- **Rationale**: Standard BFS/queue traversal could not properly identify dependency cycles and would silently skip packages, leading to incomplete or invalid installations.

## 4. Deterministic Walk in Registry Indexer
- **Location**: `marketplace/scripts/generate_registry_index.py`
- **Change**: Filtered and sorted directories in-place during the walk using `dirs[:] = sorted([d for d in dirs if not d.startswith('.')])`.
- **Rationale**: Ensures the directory traversal order is alphabetical and deterministic across different filesystems, generating identical hashes.

## 5. Symlink Traversal in Tar Extraction
- **Location**: `crates/ggen-marketplace/src/marketplace/install.rs:extract_tar_gz`
- **Change**: Added a check using `matches!(entry_type, tar::EntryType::Symlink | tar::EntryType::Link)`. If found, a path traversal error is returned.
- **Rationale**: Prevents malicious packs containing symlinks or hardlinks from writing files outside the extraction directory.

## 6. Cache Verification Bypass
- **Location**: `crates/ggen-marketplace/src/marketplace/cache.rs:verify_digest`
- **Change**: When `pack.dat` is present, it now verifies its digest, opens the archive (supporting ZIP and TAR.GZ), and compares all file entries against the extracted files on disk.
- **Rationale**: Detects if an attacker tampered with any extracted files under `pack.cache_path`. Updated the existing unit tests to supply valid empty ZIP archives.

## 7. TOML Syntax Errors
- **Location**: Manifests under `marketplace/packages/`
  - `customer-loyalty-rewards/package.toml`
  - `iso-20022-payments/package.toml`
  - `kyc-aml-compliance/package.toml`
  - `order-management-system/package.toml`
  - `trading-platform/package.toml`
  - `rest-api-template/package.toml`
- **Change**: Converted invalid bulleted lists (`- "feature"`) to valid TOML arrays, and converted multiline inline tables to single-line tables.
- **Rationale**: Fixes syntax errors that prevented these package manifests from being successfully parsed by `generate_registry_index.py`.

## 8. Dependency SemVer Range Parsing Bug
- **Location**: `crates/ggen-marketplace/src/marketplace/install.rs` (in `resolve_dependencies` and `batch_resolve_dependencies`)
- **Change**: Replaced parsing of version requirements directly as concrete `PackageVersion` with a matching mechanism using `semver::VersionReq`. It queries the repository to select the latest matching version with a fallback to direct parsing for mock test cases.
- **Rationale**: Allows packages to specify version requirements like `^1.0.0` or `>=2.0.0` as dependencies.
