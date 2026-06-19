# Analysis and Recommendation Report: Milestone 1 Bugs & Vulnerabilities

This report presents a detailed analysis and concrete fix recommendations for resolving critical bugs and vulnerabilities identified in Milestone 1 of the `ggen` marketplace refactoring.

---

## 1. Cache Verification Logic Mismatch

### Problem Analysis
* **Affected Files**: 
  * `crates/ggen-marketplace/src/marketplace/cache.rs` (specifically `PackCache::verify_digest`)
  * `crates/ggen-marketplace/src/marketplace/install.rs` (specifically `Installer::install_pack` and `Installer::extract_pack`)
* **Underlying Bug**:
  * In `Installer::install_pack`, when downloading a package archive (`pack_data`), the installer computes the digest using `ChecksumCalculator::calculate(&pack_data)`. This digest represents the SHA-256 hash of the **compressed raw archive bytes** (either a `.tar.gz` or `.zip` file). This digest is saved in the `CachedPack` cache entry.
  * In `PackCache::verify_digest`, the cache manager verifies the cached pack entry by walking the cache directory (`pack.cache_path`), reading all individual extracted files, and hashing their contents sequentially.
* **Consequence**:
  * Comparing a directory-walk hash of uncompressed file contents to the SHA-256 hash of the original compressed archive byte stream will always result in a mismatch.
  * Cache verification fails on every request, resulting in a cache miss. The installer repeatedly purges the cache directory and re-downloads packages, rendering the caching system useless.
  * The file-walking order of `walkdir::WalkDir` is filesystem-dependent and non-deterministic, adding further instability to the computed digest.

### Recommended Fix Strategy
To resolve this mismatch, the cache verification should directly match the installer checksum generation:
1. **Save Compressed Archive**: Modify `Installer::extract_pack` to always write the compressed `pack_data` archive (as `pack.dat` or `pack.archive`) into the cache directory during extraction.
2. **Direct Hash Verification**: Modify `PackCache::verify_digest` to check for the existence of the archive file in the cache directory, read its contents, and calculate its SHA-256 hash. This hash will perfectly and deterministically match `pack.digest`.
3. **Fallback Walk**: If the archive file is missing, implement a fallback deterministic file-walk (sorting files by path first) for backward compatibility and test compatibility.

#### Proposed Code Sketch
* In `crates/ggen-marketplace/src/marketplace/install.rs`:
  ```rust
  // Inside extract_pack:
  let archive_path = cache_path.join("pack.dat");
  fs::write(&archive_path, data).map_err(|e| Error::InstallationFailed {
      reason: format!("Failed to write pack archive: {}", e),
  })?;
  ```

* In `crates/ggen-marketplace/src/marketplace/cache.rs`:
  ```rust
  pub fn verify_digest(&self, pack: &CachedPack) -> Result<bool> {
      use sha2::{Digest, Sha256};
      if !pack.cache_path.exists() {
          return Ok(false);
      }
      let archive_path = pack.cache_path.join("pack.dat");
      if archive_path.exists() {
          let contents = fs::read(&archive_path).map_err(Error::IoError)?;
          let calculated_digest = hex::encode(Sha256::digest(&contents));
          return Ok(calculated_digest == pack.digest);
      }
      
      // Fallback: Deterministic sorted file walk
      let mut hasher = Sha256::new();
      let mut entries: Vec<_> = walkdir::WalkDir::new(&pack.cache_path)
          .into_iter()
          .filter_map(|e| e.ok())
          .filter(|e| e.file_type().is_file())
          .collect();
      entries.sort_by_key(|e| e.path().to_path_buf());
      for entry in entries {
          let contents = fs::read(entry.path()).map_err(Error::IoError)?;
          hasher.update(&contents);
      }
      Ok(hex::encode(hasher.finalize()) == pack.digest)
  }
  ```

---

## 2. Insecure Archive Extraction (Zip Slip) & Non-Atomic Installer

### Problem Analysis
* **Affected Files**:
  * `crates/ggen-marketplace/src/marketplace/install.rs` (specifically `extract_tar_gz`, `extract_zip`, and `extract_pack`)
* **Underlying Bug**:
  * **Zip Slip Vulnerability**: The extractor helper functions `extract_tar_gz` and `extract_zip` extract files directly into the destination path using standard library unpacking methods (`archive.unpack(dest)` and `archive.extract(dest)`). These methods do not perform validation on path components, which leaves the system vulnerable to directory traversal attacks where malicious archives write files outside the target directory.
  * **Non-Atomic Extraction**: Archives are unpacked directly into the target cache path. If the extraction fails or is aborted halfway, the cache directory is left in a corrupted/incomplete state, but subsequent cache lookups may incorrectly treat it as a successful installation.

### Recommended Fix Strategy
1. **Path Traversal Prevention**:
   * For both zip and tar.gz extractions, iterate through all entry paths manually.
   * Verify that no path components represent parent directory traversal (`..` via `Component::ParentDir`) and that entry paths are not absolute.
   * Clean/resolve the joined path and verify that the target file path starts with the base destination path.
2. **Atomic Temp Extraction & Rename**:
   * Extract archives into a unique temporary directory created in the same parent directory (to ensure it resides on the same filesystem mount for atomic renaming), e.g. using `tempfile` or a UUID-suffix (e.g. `cache_path.with_extension("tmp-uuid")`).
   * Once extraction succeeds, clean up any existing target directory at `cache_path` and atomically rename/promote the temporary directory to `cache_path` via `fs::rename`.
   * Ensure the temporary directory is cleaned up in case of failure.

#### Proposed Code Sketch
* Refactoring `extract_zip`:
  ```rust
  fn extract_zip(&self, data: &[u8], dest: &Path) -> Result<()> {
      use zip::ZipArchive;
      let cursor = std::io::Cursor::new(data);
      let mut archive = ZipArchive::new(cursor).map_err(|e| Error::InstallationFailed {
          reason: format!("Failed to open ZIP archive: {}", e),
      })?;

      for i in 0..archive.len() {
          let mut file = archive.by_index(i).map_err(|e| Error::InstallationFailed {
              reason: format!("Failed to read ZIP entry: {}", e),
          })?;

          let outpath = match file.enclosed_name() {
              Some(p) => dest.join(p),
              None => return Err(Error::InstallationFailed {
                  reason: "Path traversal detected in ZIP".to_string(),
              }),
          };

          if !outpath.starts_with(dest) || Path::new(file.name()).components().any(|c| matches!(c, std::path::Component::ParentDir)) {
              return Err(Error::InstallationFailed {
                  reason: format!("Path traversal attempt detected in ZIP: {:?}", file.name()),
              });
          }

          if file.name().ends_with('/') {
              fs::create_dir_all(&outpath).map_err(|e| Error::InstallationFailed {
                  reason: format!("Failed to create directory: {}", e),
              })?;
          } else {
              if let Some(p) = outpath.parent() {
                  if !p.exists() {
                      fs::create_dir_all(p).map_err(|e| Error::InstallationFailed {
                          reason: format!("Failed to create parent directory: {}", e),
                      })?;
                  }
              }
              let mut outfile = fs::File::create(&outpath).map_err(|e| Error::InstallationFailed {
                  reason: format!("Failed to create file: {}", e),
              })?;
              std::io::copy(&mut file, &mut outfile).map_err(|e| Error::InstallationFailed {
                  reason: format!("Failed to copy file: {}", e),
              })?;
          }
      }
      Ok(())
  }
  ```

* Refactoring `extract_tar_gz`:
  ```rust
  fn extract_tar_gz(&self, data: &[u8], dest: &Path) -> Result<()> {
      use tar::Archive;
      let decoder = GzDecoder::new(data);
      let mut archive = Archive::new(decoder);

      for entry_result in archive.entries().map_err(|e| Error::InstallationFailed {
          reason: format!("Failed to read tar entries: {}", e),
      })? {
          let mut entry = entry_result.map_err(|e| Error::InstallationFailed {
              reason: format!("Failed to read entry: {}", e),
          })?;
          let path = entry.path().map_err(|e| Error::InstallationFailed {
              reason: format!("Failed to get path: {}", e),
          })?;

          let out_path = dest.join(&path);
          if !out_path.starts_with(dest) || path.components().any(|c| matches!(c, std::path::Component::ParentDir)) {
              return Err(Error::InstallationFailed {
                  reason: format!("Path traversal attempt detected in tar.gz: {:?}", path),
              });
          }
          entry.unpack_in(dest).map_err(|e| Error::InstallationFailed {
              reason: format!("Failed to unpack tar entry: {}", e),
          })?;
      }
      Ok(())
  }
  ```

---

## 3. Nested Dotted Key Lookup Bug in Registry Indexer

### Problem Analysis
* **Affected File**: `marketplace/scripts/generate_registry_index.py` (lines 72-75)
* **Underlying Bug**:
  * Python's standard `tomllib` (or `tomli`) parses TOML dotted tables (like `[package.metadata]` or `[package.tags]`) into nested dictionaries inside the parsed output dict, e.g. `data["package"]["metadata"]` and `data["package"]["tags"]`.
  * The script attempts to access these tables using flat dotted strings as keys directly on the root dictionary `data`:
    ```python
    package = data.get("package", {})
    package_tags = data.get("package.tags", {})
    package_keywords = data.get("package.keywords", {})
    package_metadata = data.get("package.metadata", {})
    ```
* **Consequence**:
  * Root lookups like `data.get("package.metadata")` return `{}`.
  * This causes the lookup for `"production_ready"` in `package_metadata` to default to `False` for all packages. As a result, all packages are marked as not production-ready (`"production_ready": false`) in the generated `index.json`, ignoring their actual TOML manifest configurations.
  * Extraction of tags and keywords is also broken when defined under dotted headers, falling back to empty lists or failing to parse correctly.

### Recommended Fix Strategy
* Update the lookup to retrieve nested tables from the `package` dictionary:
  ```python
  package = data.get("package", {})
  package_tags = package.get("tags") or data.get("package.tags") or {}
  package_keywords = package.get("keywords") or data.get("package.keywords") or {}
  package_metadata = package.get("metadata") or data.get("package.metadata") or {}
  ```
* This ensures full support for both nested structures (e.g. `package.metadata` nested inside `package`) and flat root dotted keys, if any exist.
