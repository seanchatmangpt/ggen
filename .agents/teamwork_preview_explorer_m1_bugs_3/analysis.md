# Milestone 1: Critical Bugs & Vulnerabilities Analysis Report

This report presents the findings and recommended fix strategies for Milestone 1 (Resolve Critical Bugs & Vulnerabilities) of the `ggen` marketplace refactoring.

---

## 1. Cache Verification Mismatch

### Location
* **Files**: 
  * `crates/ggen-marketplace/src/marketplace/cache.rs`
  * `crates/ggen-marketplace/src/marketplace/install.rs`
* **Affected Functions**: 
  * `PackCache::verify_digest` (in `cache.rs`, lines 458-494)
  * `Installer::install_pack` (in `install.rs`, lines 374-470)
  * `Installer::extract_pack` (in `install.rs`, lines 766-799)

### Issue Analysis
1. During installation (`Installer::install_pack`), a package archive is downloaded (`pack_data`), and its checksum is verified against `release.checksum`.
2. The installer then saves a metadata record (`CachedPack`) with `digest` set to `ChecksumCalculator::calculate(&pack_data)`. This digest represents the SHA-256 hash of the **compressed archive bytes**.
3. However, when checking the cache (`PackCache::verify_digest`), the cache walks the directory where the files were extracted (`pack.cache_path`), reads all individual files, and hashes their contents sequentially.
4. Comparing the sequential hash of the extracted contents to the SHA-256 hash of the original compressed archive guarantees a mismatch.
5. Consequently, `verify_digest` always fails in real installations, forcing the installer to delete and re-download/re-extract the package on every request.

### Recommended Fix Strategy
* **Step 1**: Update `Installer::extract_pack` in `crates/ggen-marketplace/src/marketplace/install.rs` to write the downloaded raw compressed bytes (`data`) into a specific archive file (e.g. `pack.archive`) inside the cache directory.
* **Step 2**: Update `PackCache::verify_digest` in `crates/ggen-marketplace/src/marketplace/cache.rs` to look for `pack.archive`, load it, compute its hash, and compare it against `pack.digest`.

#### Proposed Code Changes:
**In `crates/ggen-marketplace/src/marketplace/install.rs`:**
```rust
    fn extract_pack(
        &self, data: &[u8], package_id: &PackageId, version: &PackageVersion,
    ) -> Result<PathBuf> {
        let start = std::time::Instant::now();
        debug!("Extracting pack {}@{}", package_id, version);

        // Create cache directory for this pack
        let cache_path = self.persistent_cache_path(package_id, version);

        // ... [temp/atomic extraction changes detailed below] ...

        // Write the raw archive file for verify_digest to reference
        fs::write(cache_path.join("pack.archive"), data).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to write archive copy to cache: {}", e),
        })?;

        Ok(cache_path)
    }
```

**In `crates/ggen-marketplace/src/marketplace/cache.rs`:**
```rust
    pub fn verify_digest(&self, pack: &CachedPack) -> Result<bool> {
        let archive_path = pack.cache_path.join("pack.archive");
        if !archive_path.exists() {
            return Ok(false);
        }

        let contents = fs::read(&archive_path).map_err(Error::IoError)?;
        let calculated_digest = ggen_config::hash_data(&contents);

        Ok(calculated_digest == pack.digest)
    }
```

---

## 2. Path-Traversal (Zip Slip) & Atomic Extraction

### Location
* **File**: `crates/ggen-marketplace/src/marketplace/install.rs`
* **Affected Functions**: 
  * `Installer::extract_pack` (lines 766-799)
  * `Installer::extract_tar_gz` (lines 800-818)
  * `Installer::extract_zip` (lines 820-840)

### Issue Analysis
1. **Zip Slip**: The functions `extract_tar_gz` and `extract_zip` use `archive.unpack(dest)` and `archive.extract(dest)` respectively. These methods do not guard against malicious entries that contain absolute paths or path traversal components (e.g. `../../etc/passwd`), potentially overwriting files outside the target cache directory.
2. **Atomic Extraction**: Files are extracted directly into the destination cache directory. If extraction fails midway, the cache directory remains in a corrupted/incomplete state, but might still be considered partially installed.

### Recommended Fix Strategy
* **Zip Slip Prevention**:
  * For tar archives: Iterate through the entries using `archive.entries()`. For each entry, retrieve its path, resolve it against the target directory, verify it starts with the target directory, and check that no path components are `ParentDir` (`..`). Only unpack safe entries.
  * For ZIP archives: Iterate through the entries, check `file.enclosed_name()` (which is the library's built-in Zip Slip safeguard), and perform an explicit checks verifying that the resolved path starts with the destination path and does not contain `..` components.
* **Atomic Extraction/Rename**:
  * In `extract_pack`, create a sibling temporary directory `cache_path.with_extension(format!("tmp-{}", uuid::Uuid::new_v4()))`.
  * Extract the archive to this temporary directory.
  * If extraction is successful, delete any pre-existing directory at `cache_path` and atomically rename/move the temporary directory to `cache_path` using `fs::rename`.
  * Clean up the temporary directory on failure.

#### Proposed Code Changes:
**In `crates/ggen-marketplace/src/marketplace/install.rs`:**
```rust
    fn extract_tar_gz(&self, data: &[u8], dest: &Path) -> Result<()> {
        use tar::Archive;

        let decoder = GzDecoder::new(data);
        let mut archive = Archive::new(decoder);

        for entry_result in archive.entries().map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to read tar entries: {}", e),
        })? {
            let mut entry = entry_result.map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to read tar entry: {}", e),
            })?;

            let path = entry.path().map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to get tar entry path: {}", e),
            })?;

            let out_path = dest.join(&path);
            if !out_path.starts_with(dest) || path.components().any(|c| matches!(c, std::path::Component::ParentDir)) {
                return Err(Error::InstallationFailed {
                    reason: format!("Path traversal attempt detected in tar.gz: {:?}", path),
                });
            }

            entry.unpack_in(dest).map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to unpack tar entry {:?}: {}", path, e),
            })?;
        }

        Ok(())
    }

    fn extract_zip(&self, data: &[u8], dest: &Path) -> Result<()> {
        use zip::ZipArchive;

        let cursor = std::io::Cursor::new(data);
        let mut archive = ZipArchive::new(cursor).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to open ZIP archive: {}", e),
        })?;

        for i in 0..archive.len() {
            let mut file = archive.by_index(i).map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to read ZIP entry at index {}: {}", i, e),
            })?;

            let outpath = match file.enclosed_name() {
                Some(p) => dest.join(p),
                None => {
                    return Err(Error::InstallationFailed {
                        reason: "Path traversal attempt detected in ZIP".to_string(),
                    });
                }
            };

            if !outpath.starts_with(dest) || Path::new(file.name()).components().any(|c| matches!(c, std::path::Component::ParentDir)) {
                return Err(Error::InstallationFailed {
                    reason: format!("Path traversal attempt detected in ZIP: {:?}", file.name()),
                });
            }

            if file.name().ends_with('/') {
                fs::create_dir_all(&outpath).map_err(|e| Error::InstallationFailed {
                    reason: format!("Failed to create directory {:?}: {}", outpath, e),
                })?;
            } else {
                if let Some(p) = outpath.parent() {
                    if !p.exists() {
                        fs::create_dir_all(p).map_err(|e| Error::InstallationFailed {
                            reason: format!("Failed to create parent directory {:?}: {}", p, e),
                        })?;
                    }
                }
                let mut outfile = fs::File::create(&outpath).map_err(|e| Error::InstallationFailed {
                    reason: format!("Failed to create file {:?}: {}", outpath, e),
                })?;
                std::io::copy(&mut file, &mut outfile).map_err(|e| Error::InstallationFailed {
                    reason: format!("Failed to copy file contents for {:?}: {}", outpath, e),
                })?;
            }
        }

        Ok(())
    }

    fn extract_pack(
        &self, data: &[u8], package_id: &PackageId, version: &PackageVersion,
    ) -> Result<PathBuf> {
        let start = std::time::Instant::now();
        debug!("Extracting pack {}@{}", package_id, version);

        let cache_path = self.persistent_cache_path(package_id, version);
        let parent_dir = cache_path.parent().ok_or_else(|| Error::InstallationFailed {
            reason: "Invalid cache path: no parent directory".to_string(),
        })?;
        fs::create_dir_all(parent_dir).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to create cache parent directory: {}", e),
        })?;

        // Sibling temporary directory for atomic extraction
        let temp_dest = cache_path.with_extension(format!("tmp-{}", uuid::Uuid::new_v4()));
        fs::create_dir_all(&temp_dest).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to create temp extraction directory: {}", e),
        })?;

        // Extract to temporary directory
        let extract_res = if is_tar_gz(data) {
            self.extract_tar_gz(data, &temp_dest)
        } else if is_zip(data) {
            self.extract_zip(data, &temp_dest)
        } else {
            let output_path = temp_dest.join("pack.dat");
            fs::write(&output_path, data).map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to write pack data: {}", e),
            })
        };

        if let Err(e) = extract_res {
            let _ = fs::remove_dir_all(&temp_dest);
            return Err(e);
        }

        // Save raw archive for cache verification
        if let Err(e) = fs::write(temp_dest.join("pack.archive"), data) {
            let _ = fs::remove_dir_all(&temp_dest);
            return Err(Error::InstallationFailed {
                reason: format!("Failed to write archive copy to cache: {}", e),
            });
        }

        // Atomic rename / promotion to target cache directory
        if cache_path.exists() {
            fs::remove_dir_all(&cache_path).map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to remove existing cache directory: {}", e),
            })?;
        }

        fs::rename(&temp_dest, &cache_path).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to atomically rename temp directory to destination: {}", e),
        })?;

        let duration = start.elapsed();
        span::Span::current().record("duration_ms", duration.as_millis());
        debug!("Extracted pack to: {:?}", cache_path);

        Ok(cache_path)
    }
```

---

## 3. Python Nested Dotted Key Lookup Bug

### Location
* **File**: `marketplace/scripts/generate_registry_index.py`
* **Affected Lines**: 72-75 in `parse_package_toml`

### Issue Analysis
1. Dotted keys in a TOML header (like `[package.metadata]` or `[package.tags]`) represent nested dictionaries in Python's `tomllib` (or `tomli`), e.g., `{"package": {"metadata": {...}}}`.
2. The script currently tries to access these fields using flat dotted keys directly on the loaded dict:
   ```python
   package = data.get("package", {})
   package_tags = data.get("package.tags", {})
   package_keywords = data.get("package.keywords", {})
   package_metadata = data.get("package.metadata", {})
   ```
3. Since the keys `"package.tags"`, `"package.keywords"`, and `"package.metadata"` do not exist at the root level of `data`, all these calls return `{}`.
4. This causes the lookup for `"production_ready"` in `package_metadata` to default to `False`, rendering all packages as not production-ready in the generated `index.json`. It also breaks tags and keywords extraction when they are defined under dotted headers.

### Recommended Fix Strategy
* Update the lookup to correctly check the nested keys from within `package` first, falling back to the flat root keys (in case of different configurations/formats).

#### Proposed Code Changes:
**In `marketplace/scripts/generate_registry_index.py`:**
```python
    package = data.get("package", {})
    package_tags = package.get("tags") or data.get("package.tags") or {}
    package_keywords = package.get("keywords") or data.get("package.keywords") or {}
    package_metadata = package.get("metadata") or data.get("package.metadata") or {}
```
