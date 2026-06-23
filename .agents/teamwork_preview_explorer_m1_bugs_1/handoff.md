# Handoff Report: Milestone 1 Critical Bugs Analysis

## 1. Observation

Direct code analysis and execution results revealed the following:

### A. Cache Verification Failure
* **Files**: `crates/ggen-marketplace/src/marketplace/cache.rs` and `crates/ggen-marketplace/src/marketplace/install.rs`.
* **Code Details**:
  * In `cache.rs` (lines 458-494):
    ```rust
    pub fn verify_digest(&self, pack: &CachedPack) -> Result<bool> {
        use sha2::{Digest, Sha256};
        ...
        // Walk the pack directory and hash all files
        if pack.cache_path.exists() {
            for entry in walkdir::WalkDir::new(&pack.cache_path)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                if entry.file_type().is_file() {
                    if let Ok(contents) = fs::read(entry.path()) {
                        hasher.update(&contents);
                    } else {
                        verified = false;
                        break;
                    }
                }
            }
        }
        let calculated_digest = hex::encode(hasher.finalize());
        let matches = calculated_digest == pack.digest;
        ...
    ```
  * In `install.rs` (line 447):
    ```rust
    let digest = ChecksumCalculator::calculate(&pack_data);
    ```
  * `pack.digest` stores the SHA-256 of the raw compressed archive (`pack_data`), whereas `verify_digest` computes the digest of the uncompressed directory files.

### B. Zip Slip and Non-Atomic Extraction
* **File**: `crates/ggen-marketplace/src/marketplace/install.rs`.
* **Code Details**:
  * In `extract_pack` (lines 766-799), extraction is done directly into `cache_path`:
    ```rust
    let cache_path = self.persistent_cache_path(package_id, version);
    fs::create_dir_all(&cache_path)...
    if is_tar_gz(data) {
        self.extract_tar_gz(data, &cache_path)?;
    } else if is_zip(data) {
        self.extract_zip(data, &cache_path)?;
    }
    ```
  * Helper functions `extract_tar_gz` and `extract_zip` (lines 805-840) unpack files using `tar::Archive::unpack` and `zip::ZipArchive::extract` directly without validating entry paths for traversal components (`..`) or checking if they resolve outside the base cache directory.

### C. Dotted Key TOML Lookup
* **File**: `marketplace/scripts/generate_registry_index.py`.
* **Code Details**:
  * In `parse_package_toml` (lines 72-75):
    ```python
    package = data.get("package", {})
    package_tags = data.get("package.tags", {})
    package_keywords = data.get("package.keywords", {})
    package_metadata = data.get("package.metadata", {})
    ```
  * TOML dotted table headings (like `[package.metadata]`) are parsed into nested dictionaries `data["package"]["metadata"]`. The script performs flat lookups, resulting in `package_metadata` evaluating to `{}` and setting `"production_ready"` (line 117) to `False` for all packages in the registry `index.json`.

---

## 2. Logic Chain

1. **Cache Verification Failure**:
   * The installer saves the compressed archive hash (`ChecksumCalculator::calculate(&pack_data)`) as the package digest.
   * `verify_digest` walks the uncompressed directory and hashes the files sequentially.
   * A sequential hash of uncompressed files can never match the hash of a compressed archive byte stream.
   * *Conclusion*: Cache verification fails on every call, purging and re-downloading packages on each run.

2. **Insecure & Non-Atomic Extraction**:
   * Archives are unpacked directly to their target directory using helper library extract calls.
   * Entries containing `..` or absolute paths will traverse outside the target directory, leading to a Zip Slip vulnerability.
   * An interrupted extraction leaves a corrupted directory on disk, which is treated as a successful installation.
   * *Conclusion*: Path traversal must be blocked by validating entries, and extraction must happen in a temporary directory before an atomic rename.

3. **Dotted Key Registry Index Bug**:
   * Dotted keys are represented as nested dictionaries in standard parsed TOML.
   * Flat lookups like `data.get("package.metadata")` return `{}`.
   * *Conclusion*: Lookups must be corrected to retrieve nested tables from within the `package` dictionary.

---

## 3. Caveats

* Code was analyzed statically, and no source code modifications were written to the workspace root.
* Tests were verified to compile and run successfully using the cargo test suite on the `ggen-marketplace` crate.
* It is assumed the `tar` and `zip` crate APIs allow iterating over entries (which is verified by `ggen-marketplace`'s dependencies).

---

## 4. Conclusion

1. **Cache Verification Perf Bug**: Pack caching fails due to comparing compressed vs. uncompressed file hashes.
2. **Security Vulnerability**: Zip Slip directory traversal is possible during extraction.
3. **Atomic Installer Bug**: Failure during extraction leaves a corrupt, non-atomic installation.
4. **Registry Dotted Key Bug**: Dotted TOML table keys are read incorrectly, defaulting `"production_ready"` to `false` for all packages.

---

## 5. Verification Method

* Run cargo tests: `cargo test --package ggen-marketplace` (successfully passes 223 tests).
* Run python registry generation: `python3.11 marketplace/scripts/generate_registry_index.py` (successfully runs but triggers warnings for nested/corrupt TOML files).
* Inspect the recommendations and sketches in `analysis.md`.
