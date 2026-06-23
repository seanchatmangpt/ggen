# Milestone 1: Resolve Critical Bugs & Vulnerabilities Analysis

This report details the analysis and concrete recommendation strategies for resolving the critical bugs and security vulnerabilities identified in Milestone 1 of the `ggen` Pack and Marketplace refactoring.

---

## 1. Cache Verification Logic Mismatch

### Problem Analysis
- **Location**: `crates/ggen-marketplace/src/marketplace/cache.rs` (lines 458-494) and `crates/ggen-marketplace/src/marketplace/install.rs` (lines 446-456).
- **Behavior**: 
  - During installation in `install_pack`, the installer downloads compressed pack archive bytes (`pack_data`), calculates its SHA-256 digest (`ChecksumCalculator::calculate(&pack_data)`), and stores this digest in the `CachedPack` cache entry.
  - When checking the cache on subsequent requests, the cache manager invokes `verify_digest`. This function walks the uncompressed directory, hashes each individual file, and compares the resulting cumulative hash against the stored archive hash.
- **Consequence**:
  - The hash of uncompressed files in a directory will never match the hash of the compressed archive file.
  - Cache verification fails on every single request, resulting in a cache miss. The installer purges the cache directory and re-downloads the package every time, rendering the cache completely useless and degrading performance.
  - The file walking order from `walkdir::WalkDir` is non-deterministic, introducing further instability.

### Recommended Fix Strategy
To resolve this mismatch, we propose a dual-layer verification strategy that:
1. Stores the raw compressed archive within the cache directory (as `pack.dat`) during installation.
2. Updates `verify_digest` to verify `pack.dat` if it exists.
3. Falls back to a deterministic (sorted) file-walk digest of the directory if `pack.dat` does not exist (for backward compatibility and test suite compatibility).

#### Proposed Code Changes

##### In `crates/ggen-marketplace/src/marketplace/install.rs`
Modify `extract_pack` to always write the compressed `pack_data` archive to `pack.dat` in the cache directory prior to extraction:

```rust
fn extract_pack(
    &self, data: &[u8], package_id: &PackageId, version: &PackageVersion,
) -> Result<PathBuf> {
    let start = std::time::Instant::now();
    debug!("Extracting pack {}@{}", package_id, version);

    // Create cache directory for this pack
    let cache_path = self.persistent_cache_path(package_id, version);

    fs::create_dir_all(&cache_path).map_err(|e| Error::InstallationFailed {
        reason: format!("Failed to create cache directory: {}", e),
    })?;

    // Always save the compressed archive to pack.dat for cache verification
    let archive_path = cache_path.join("pack.dat");
    fs::write(&archive_path, data).map_err(|e| Error::InstallationFailed {
        reason: format!("Failed to write pack archive file: {}", e),
    })?;

    // Detect format and extract
    if is_tar_gz(data) {
        self.extract_tar_gz(data, &cache_path)?;
    } else if is_zip(data) {
        self.extract_zip(data, &cache_path)?;
    }

    let duration = start.elapsed();
    span::Span::current().record("duration_ms", duration.as_millis());

    debug!("Extracted pack to: {:?}", cache_path);

    Ok(cache_path)
}
```

##### In `crates/ggen-marketplace/src/marketplace/cache.rs`
Update `verify_digest` to check for `pack.dat` first:

```rust
pub fn verify_digest(&self, pack: &CachedPack) -> Result<bool> {
    use sha2::{Digest, Sha256};

    if !pack.cache_path.exists() {
        return Ok(false);
    }

    let archive_path = pack.cache_path.join("pack.dat");
    if archive_path.exists() {
        // High-performance direct archive hash matching the installer checksum
        let mut hasher = Sha256::new();
        match fs::read(&archive_path) {
            Ok(contents) => {
                hasher.update(&contents);
                let calculated_digest = hex::encode(hasher.finalize());
                let matches = calculated_digest == pack.digest;
                if !matches {
                    warn!(
                        "Archive digest mismatch for {}: expected {}, got {}",
                        pack.cache_key(),
                        pack.digest,
                        calculated_digest
                    );
                }
                return Ok(matches);
            }
            Err(e) => {
                warn!("Failed to read cached archive file: {}", e);
                return Ok(false);
            }
        }
    }

    // Fallback: Deterministic sorted file-walk for backward compatibility and test compatibility
    let mut hasher = Sha256::new();
    let mut verified = true;

    let mut entries: Vec<_> = walkdir::WalkDir::new(&pack.cache_path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .collect();

    // Sort entries by path to ensure deterministic hashing order
    entries.sort_by_key(|e| e.path().to_path_buf());

    for entry in entries {
        if let Ok(contents) = fs::read(entry.path()) {
            hasher.update(&contents);
        } else {
            verified = false;
            break;
        }
    }

    let calculated_digest = hex::encode(hasher.finalize());
    let matches = calculated_digest == pack.digest;

    if !matches {
        warn!(
            "Directory digest mismatch for {}: expected {}, got {}",
            pack.cache_key(),
            pack.digest,
            calculated_digest
        );
    }

    Ok(verified && matches)
}
```

---

## 2. Insecure Archive Extraction (Zip Slip) & Non-Atomic Installer

### Problem Analysis
- **Location**: `crates/ggen-marketplace/src/marketplace/install.rs` (lines 805-840).
- **Behavior**:
  - `extract_tar_gz` and `extract_zip` unpack files directly to the destination cache path using third-party library extract functions without validating if the files contain path traversal elements (e.g. `..`).
  - If the extraction process is interrupted, the directory is left in a partially extracted state, leading to subsequent build failures or stale cache loads.
- **Consequence**:
  - Exposes the installer to **Zip Slip** directory traversal attacks. A malicious package could write files outside the target directory and overwrite critical system or user files.
  - The lack of atomicity means partial extractions will be cached and treated as successful installations.

### Recommended Fix Strategy
1. **Zip Slip Prevention**: Iterate through all entries in the archive individually. Clean the paths and check for any path-traversal components (`..` or absolute paths pointing outside of the target base directory).
2. **Atomic Extraction**: Unpack the archive into a temporary directory under the same parent directory. Only upon successful extraction and verification should the temporary directory be atomically renamed to the final destination.

#### Proposed Code Changes

##### In `crates/ggen-marketplace/src/marketplace/install.rs`

Modify `extract_pack` to utilize a temporary directory and atomic rename:

```rust
fn extract_pack(
    &self, data: &[u8], package_id: &PackageId, version: &PackageVersion,
) -> Result<PathBuf> {
    let start = std::time::Instant::now();
    debug!("Extracting pack {}@{}", package_id, version);

    // Create cache directory for this pack
    let cache_path = self.persistent_cache_path(package_id, version);
    let parent_dir = cache_path.parent().ok_or_else(|| Error::InstallationFailed {
        reason: "Cache path has no parent directory".to_string(),
    })?;

    fs::create_dir_all(parent_dir).map_err(|e| Error::InstallationFailed {
        reason: format!("Failed to create parent cache directory: {}", e),
    })?;

    // Create a temporary directory in the same parent directory to ensure atomic rename capability
    let temp_dir = tempfile::Builder::new()
        .prefix("ggen-install-")
        .tempdir_in(parent_dir)
        .map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to create temporary directory: {}", e),
        })?;
    let temp_path = temp_dir.path();

    // Write the compressed archive to pack.dat for cache verification inside the temp path
    let archive_path = temp_path.join("pack.dat");
    fs::write(&archive_path, data).map_err(|e| Error::InstallationFailed {
        reason: format!("Failed to write pack archive file: {}", e),
    })?;

    // Detect format and extract to temp path
    if is_tar_gz(data) {
        self.extract_tar_gz(data, temp_path)?;
    } else if is_zip(data) {
        self.extract_zip(data, temp_path)?;
    }

    // Atomic promotion: rename temp_path to cache_path on successful extraction
    let temp_dir_path = temp_dir.into_path(); // Consume TempDir to prevent cleanup
    if cache_path.exists() {
        fs::remove_dir_all(&cache_path).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to remove existing cache directory: {}", e),
        })?;
    }

    if let Err(e) = fs::rename(&temp_dir_path, &cache_path) {
        let _ = fs::remove_dir_all(&temp_dir_path); // Cleanup temp files
        return Err(Error::InstallationFailed {
            reason: format!("Failed to atomically rename cache directory: {}", e),
        });
    }

    let duration = start.elapsed();
    span::Span::current().record("duration_ms", duration.as_millis());

    debug!("Extracted pack to: {:?}", cache_path);

    Ok(cache_path)
}
```

Refactor `extract_zip` with path-traversal (Zip Slip) validation:

```rust
fn extract_zip(&self, data: &[u8], dest: &Path) -> Result<()> {
    use zip::ZipArchive;

    let cursor = std::io::Cursor::new(data);
    let mut archive = ZipArchive::new(cursor).map_err(|e| Error::InstallationFailed {
        reason: format!("Failed to open ZIP archive: {}", e),
    })?;

    for i in 0..archive.len() {
        let mut file = archive.by_index(i).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to read zip entry: {}", e),
        })?;

        let outpath = match file.enclosed_name() {
            Some(path) => path.to_owned(),
            None => {
                return Err(Error::InstallationFailed {
                    reason: format!(
                        "Invalid zip entry name (possible Zip Slip attack): {}",
                        file.name()
                    ),
                });
            }
        };

        // Ensure no parent directories are traversed
        if outpath.components().any(|c| matches!(c, std::path::Component::ParentDir)) {
            return Err(Error::InstallationFailed {
                reason: format!("Path traversal detected in zip entry: {:?}", outpath),
            });
        }

        let dest_file_path = dest.join(&outpath);
        // Ensure path resolves inside the target base directory
        if !dest_file_path.starts_with(dest) {
            return Err(Error::InstallationFailed {
                reason: format!("Path traversal resolved outside target: {:?}", outpath),
            });
        }

        if file.name().ends_with('/') {
            fs::create_dir_all(&dest_file_path).map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to create directory: {}", e),
            })?;
        } else {
            if let Some(parent) = dest_file_path.parent() {
                if !parent.exists() {
                    fs::create_dir_all(parent).map_err(|e| Error::InstallationFailed {
                        reason: format!("Failed to create parent directory: {}", e),
                    })?;
                }
            }
            let mut outfile = fs::File::create(&dest_file_path).map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to create file: {}", e),
            })?;
            std::io::copy(&mut file, &mut outfile).map_err(|e| Error::InstallationFailed {
                reason: format!("Failed to copy file contents: {}", e),
            })?;
        }

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            if let Some(mode) = file.unix_mode() {
                let _ = fs::set_permissions(&dest_file_path, fs::Permissions::from_mode(mode));
            }
        }
    }

    Ok(())
}
```

Refactor `extract_tar_gz` with path-traversal (Zip Slip) validation:

```rust
fn extract_tar_gz(&self, data: &[u8], dest: &Path) -> Result<()> {
    use tar::Archive;

    let decoder = GzDecoder::new(data);
    let mut archive = Archive::new(decoder);

    let entries = archive.entries().map_err(|e| Error::InstallationFailed {
        reason: format!("Failed to read tar entries: {}", e),
    })?;

    for entry_result in entries {
        let mut entry = entry_result.map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to read tar entry: {}", e),
        })?;

        let path = entry.path().map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to get tar entry path: {}", e),
        })?.to_path_buf();

        // Ensure no parent directories are traversed
        if path.components().any(|c| matches!(c, std::path::Component::ParentDir)) {
            return Err(Error::InstallationFailed {
                reason: format!("Path traversal detected in tar entry: {:?}", path),
            });
        }

        let dest_file_path = dest.join(&path);
        // Ensure path resolves inside the target base directory
        if !dest_file_path.starts_with(dest) {
            return Err(Error::InstallationFailed {
                reason: format!("Path traversal resolved outside target: {:?}", path),
            });
        }

        entry.unpack(&dest_file_path).map_err(|e| Error::InstallationFailed {
            reason: format!("Failed to unpack tar entry: {}", e),
        })?;
    }

    Ok(())
}
```

---

## 3. Nested Dotted Key Lookup Bug in Registry Indexer

### Problem Analysis
- **Location**: `marketplace/scripts/generate_registry_index.py` (lines 72-75).
- **Behavior**:
  - The script uses Python's standard `tomllib` (or `tomli`) to load the parsed TOML manifest.
  - The script reads the parsed dictionary using flat dotted keys:
    ```python
    package = data.get("package", {})
    package_tags = data.get("package.tags", {})
    package_keywords = data.get("package.keywords", {})
    package_metadata = data.get("package.metadata", {})
    ```
- **Consequence**:
  - Dotted tables in TOML, such as `[package.metadata]`, are parsed as nested dictionary structures (e.g. `data["package"]["metadata"]`), not flat keys.
  - Therefore, `data.get("package.metadata", {})` always returns `{}`.
  - This results in all packages having `production_ready = false` in the generated `index.json`, ignoring whatever value was actually specified in their manifests.

### Recommended Fix Strategy
Change the flat dictionary lookups to nested dictionary lookups under the `package` dictionary:
```python
    package = data.get("package", {})
    package_tags = package.get("tags", {})
    package_keywords = package.get("keywords", {})
    package_metadata = package.get("metadata", {})
```

#### Proposed Code Changes

##### In `marketplace/scripts/generate_registry_index.py`
Replace lines 72-75:

```python
    package = data.get("package", {})
    package_tags = package.get("tags", {})
    package_keywords = package.get("keywords", {})
    package_metadata = package.get("metadata", {})
```

This ensures that the keys `tags`, `keywords`, and `metadata` are correctly fetched from the parent `package` dictionary rather than the root directory context, resolving the dotted table lookup bug.
