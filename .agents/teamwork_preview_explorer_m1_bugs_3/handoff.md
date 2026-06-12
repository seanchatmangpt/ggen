# Handoff Report — explorer_3

This handoff report summarizes the findings of Milestone 1 (Resolve Critical Bugs & Vulnerabilities) codebase investigation.

---

## 1. Observation

### Observation 1: Cache Verification Digest Mismatch
* **File**: `crates/ggen-marketplace/src/marketplace/cache.rs`
* **Code Reference (Lines 458-493)**:
  ```rust
      pub fn verify_digest(&self, pack: &CachedPack) -> Result<bool> {
          use sha2::{Digest, Sha256};

          let mut hasher = Sha256::new();
          let mut verified = true;

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
  ```
* **File**: `crates/ggen-marketplace/src/marketplace/install.rs`
* **Code Reference (Lines 446-456)**:
  ```rust
          // Calculate final digest
          let digest = ChecksumCalculator::calculate(&pack_data);

          // Create cached pack entry
          let cached_pack = CachedPack::new(
              package_id.clone(),
              version.clone(),
              digest,
              pack_data.len() as u64,
              cache_path,
          );
  ```

### Observation 2: Unsafe Archive Extraction and Non-Atomic Promotion
* **File**: `crates/ggen-marketplace/src/marketplace/install.rs`
* **Code Reference (Lines 805-840)**:
  ```rust
      fn extract_tar_gz(&self, data: &[u8], dest: &Path) -> Result<()> {
          use tar::Archive;

          let decoder = GzDecoder::new(data);
          let mut archive = Archive::new(decoder);

          archive
              .unpack(dest)
              ...
      }

      fn extract_zip(&self, data: &[u8], dest: &Path) -> Result<()> {
          use zip::ZipArchive;

          let cursor = std::io::Cursor::new(data);
          let mut archive = ZipArchive::new(cursor)...

          archive
              .extract(dest)
              ...
      }
  ```
* **Code Reference (Lines 766-799)**:
  ```rust
      fn extract_pack(
          &self, data: &[u8], package_id: &PackageId, version: &PackageVersion,
      ) -> Result<PathBuf> {
          // ...
          let cache_path = self.persistent_cache_path(package_id, version);

          fs::create_dir_all(&cache_path)...
          
          if is_tar_gz(data) {
              self.extract_tar_gz(data, &cache_path)?;
          } else if is_zip(data) {
              self.extract_zip(data, &cache_path)?;
          }
          // ...
      }
  ```

### Observation 3: Python Flat/Dotted Key TOML Lookup
* **File**: `marketplace/scripts/generate_registry_index.py`
* **Code Reference (Lines 72-75)**:
  ```python
      package = data.get("package", {})
      package_tags = data.get("package.tags", {})
      package_keywords = data.get("package.keywords", {})
      package_metadata = data.get("package.metadata", {})
  ```

---

## 2. Logic Chain

### Logic Chain 1: Cache Verification Mismatch
1. The installer saves `CachedPack::digest` by computing the hash of the downloaded compressed archive (`pack_data`) using `ChecksumCalculator::calculate(&pack_data)` (Observation 1).
2. During cache hits, `verify_digest` reads the directory contents of `pack.cache_path` and computes a sequential SHA-256 of the *extracted* files (Observation 1).
3. The SHA-256 of extracted directory files will never match the SHA-256 of the compressed archive `.tar.gz`/`.zip` bytes.
4. Hence, cache verification always fails in practice, purging the cache on every installation request.

### Logic Chain 2: Unsafe Archive Extraction and Non-Atomic Promotion
1. Both `extract_tar_gz` and `extract_zip` unpack files directly to the target destination `dest` (which is `cache_path`) using the library's basic unpack/extract calls (Observation 2).
2. These basic calls do not validate if files inside the archive have names with relative parents (e.g. `../../filename`) or absolute paths, creating a Zip Slip path traversal vulnerability.
3. Because extraction writes directly to `cache_path` (Observation 2), any error mid-extraction leaves a corrupted set of files in the cache.

### Logic Chain 3: Python Dotted Key Lookup
1. Python `tomllib`/`tomli` parses dotted TOML headers like `[package.metadata]` into nested dictionaries (`{"package": {"metadata": {...}}}`) instead of flat dotted-key dictionaries (`{"package.metadata": {...}}`).
2. The lookup uses `data.get("package.metadata", {})` (Observation 3) which tries to read the flat key `package.metadata` at the root, returning an empty dict `{}`.
3. As a result, properties like `production_ready` under `[package.metadata]` default to `False`.

---

## 3. Caveats

* **Archive Preservation**: The proposed fix strategy involves saving the raw archive (`pack.archive`) in the cache directory. This increases disk utilization slightly per package version. However, this is standard package manager behavior and necessary for reliable validation of compressed downloads.
* **Zip Slip Check Performance**: Verifying every path component inside the archive adds negligible CPU overhead, but is essential for safety.

---

## 4. Conclusion

* **Bug 1 Solution**: Save the raw archive as `pack.archive` under the cache directory during extraction, and update `verify_digest` to hash and verify that file instead of walking the directory.
* **Bug 2 Solution**: Refactor tar/ZIP extraction to iterate over entry lists manually, validate paths for Zip Slip (`starts_with` and check components for `ParentDir`), extract to a temporary sibling directory, and use `fs::rename` to atomically replace the final `cache_path`.
* **Bug 3 Solution**: Retrieve metadata, tags, and keywords in Python by querying nested subkeys first, falling back to flat dotted keys, i.e., `package.get("metadata") or data.get("package.metadata")`.

---

## 5. Verification Method

### Test Commands
* Verify `cargo check` and `cargo test` pass after any future implementation by running:
  ```bash
  cargo test -p ggen-marketplace
  cargo test --test pack_cache_test --features integration
  ```
* Verify Python registry index generator runs successfully:
  ```bash
  python3 marketplace/scripts/generate_registry_index.py
  ```

### Inspecting Files
* Ensure `marketplace/registry/index.json` compiles with `"production_ready": true` for packages that define it.
* Inspect `crates/ggen-marketplace/src/marketplace/cache.rs` and `install.rs` to verify that `pack.archive` is written/read correctly, and that temporary directories are renamed atomically.
