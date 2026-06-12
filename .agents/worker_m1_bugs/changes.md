# Milestone 1: Changes Report

This document records the modifications made to resolve the critical bugs and vulnerabilities identified in Milestone 1.

## 1. Cache Verification Logic Fix

- **Files modified**: `crates/ggen-marketplace/src/marketplace/cache.rs` and `crates/ggen-marketplace/src/marketplace/install.rs`
- **Rationale**:
  - In the installer, the archive download digest was saved as `pack.digest`. However, on cache hits, the cache manager parsed/hashed the extracted directory files instead of the compressed archive, resulting in mismatching digests and persistent cache misses.
  - To align the verification with installer checksums, the installer now writes the compressed archive to `pack.dat` inside the cache directory.
  - The cache verification function `verify_digest` checks if `pack.dat` exists. If so, it hashes `pack.dat` directly to verify against `pack.digest` (O(1) direct verification).
  - If `pack.dat` does not exist (for backward compatibility), it falls back to a deterministic, lexicographically sorted walk of files in the directory to verify the digest.

## 2. Insecure Archive Extraction (Zip Slip Prevention) & Atomic Installer

- **Files modified**: `crates/ggen-marketplace/src/marketplace/install.rs`
- **Rationale**:
  - Unchecked archive extraction can write files to arbitrary locations outside the destination folder (Zip Slip vulnerability).
  - We refactored `extract_zip` and `extract_tar_gz` to inspect all entry paths:
    - Verifying that no path components contain `..` (ParentDir).
    - Verifying that the target file path starts with the base destination path.
  - Furthermore, to ensure atomic installations, `extract_pack` now extracts to a temp directory under the same parent folder (using `tempfile::Builder`). Upon successful extraction, verification, and writing of `pack.dat`, the temp directory is atomically renamed (`fs::rename`) to the destination directory. If any error occurs, the temp directory is cleaned up, and no partial extraction is left in the final location.

## 3. Nested Dotted Key Lookup Bug in Registry Indexer

- **Files modified**: `marketplace/scripts/generate_registry_index.py`
- **Rationale**:
  - In TOML, `[package.metadata]` is parsed as nested dictionaries (`data["package"]["metadata"]`), not flat dotted strings (`data["package.metadata"]`).
  - Corrected flat key retrievals `data.get("package.metadata", {})` (and for tags/keywords) to retrieve nested keys under the parent `package` dictionary:
    ```python
    package = data.get("package", {})
    package_tags = package.get("tags", {})
    package_keywords = package.get("keywords", {})
    package_metadata = package.get("metadata", {})
    ```
  - This successfully enabled setting `production_ready = true` for compliant packages, verified by inspecting the generated `index.json` containing compliant packages marked `true`.

## 4. Tests Added

- **Path-traversal / Zip Slip Prevention Tests**:
  - `test_zip_slip_prevention_zip`: Attempts to extract a Zip containing a path traversal component and asserts it returns a traversal error.
  - `test_zip_slip_prevention_tar`: Attempts to extract a Tar archive containing a path traversal component (by manually editing raw header bytes to bypass tar's builder validation) and asserts it returns a traversal error.
- **Cache Verification Tests**:
  - `test_cache_verification_with_pack_dat`: Verifies that a valid `pack.dat` matches `verify_digest`, and a tampered one fails.
  - `test_cache_verification_fallback`: Verifies that if `pack.dat` is missing, `verify_digest` falls back to sorting files in the directory and hashing them.
