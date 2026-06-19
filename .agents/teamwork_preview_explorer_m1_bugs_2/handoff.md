# Handoff Report — explorer_2

## 1. Observation

We directly observed the following code sections and behaviors:

1. **Cache Verification Bug**:
   - In `crates/ggen-marketplace/src/marketplace/cache.rs` (lines 458-494):
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
   - In `crates/ggen-marketplace/src/marketplace/install.rs` (lines 446-456):
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

2. **Insecure Archive Extraction (Zip Slip) & Non-Atomic Installer**:
   - In `crates/ggen-marketplace/src/marketplace/install.rs` (lines 805-840):
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

3. **Dotted Key Registry Indexer Lookup**:
   - In `marketplace/scripts/generate_registry_index.py` (lines 72-75):
     ```python
     package = data.get("package", {})
     package_tags = data.get("package.tags", {})
     package_keywords = data.get("package.keywords", {})
     package_metadata = data.get("package.metadata", {})
     ```

---

## 2. Logic Chain

1. **Cache Verification Bug**:
   - **Step 1**: The installer calculates `digest` over the raw compressed pack data bytes (`pack_data`) using `ChecksumCalculator::calculate(&pack_data)` (Observation 1.2).
   - **Step 2**: The cache verifier `verify_digest` walks the uncompressed directory `pack.cache_path` and computes a hash over the files recursively (Observation 1.1).
   - **Step 3**: Because the hash of individual extracted files does not match the hash of the compressed archive, comparing them (`calculated_digest == pack.digest`) will always return `false`.
   - **Step 4**: Consequently, the installer purges the cache and re-downloads the pack on every run, destroying caching benefits.

2. **Insecure Archive Extraction (Zip Slip) & Non-Atomic Installer**:
   - **Step 1**: `extract_tar_gz` and `extract_zip` invoke `unpack` and `extract` directly into the final `dest` path (Observation 2).
   - **Step 2**: If the archive is maliciously crafted with relative path components (e.g. `../../etc/passwd`), the extraction library extracts them relative to `dest`, writing files outside the destination cache directory (Zip Slip vulnerability).
   - **Step 3**: In addition, if extraction fails mid-way, the target cache path contains partial, corrupted extractions.

3. **Dotted Key Registry Indexer Lookup**:
   - **Step 1**: Dotted tables in TOML like `[package.metadata]` are parsed by Python's `tomllib` as nested dictionaries under `"package"`, i.e., `data["package"]["metadata"]`.
   - **Step 2**: The script retrieves them using `data.get("package.metadata", {})` (Observation 3).
   - **Step 3**: Because `"package.metadata"` does not exist as a top-level key in the root dictionary, it returns `{}`.
   - **Step 4**: As a result, `production_ready` is evaluated to `False` for all packages in the generated `index.json`.

---

## 3. Caveats

- No caveats. The codebase structure is straightforward, and the bugs have been isolated to their specific files and lines without external side-effects.

---

## 4. Conclusion

- The cache verification bug in `cache.rs` can be fixed by writing the compressed archive to `pack.dat` in the cache directory, and verifying its digest. A fallback sorted-walk ensures compatibility with unit tests.
- The Zip Slip vulnerability and non-atomicity in `install.rs` are solved by using a temporary directory for extraction, performing individual entry path checking (against `..` and confirming `starts_with(dest)`), and performing an atomic rename.
- The dotted key TOML lookup bug in `generate_registry_index.py` is solved by doing nested dictionary lookups (e.g., `package.get("metadata", {})`).

---

## 5. Verification Method

To independently verify the fixes:
1. **Cache Verification**: Run `cargo test -p ggen-marketplace` to ensure existing and modified tests pass. Run integration tests in `crates/ggen-cli/tests/pack_cache_test.rs` to verify that `verify_digest` behaves correctly.
2. **Zip Slip / Atomic Rename**: Compile the project with `cargo build --all-targets` and run the test suite to verify no regressions in installation or extraction logic.
3. **Indexer**: Run `python3 marketplace/scripts/generate_registry_index.py` and inspect the generated `marketplace/registry/index.json` to confirm `production_ready` is set correctly for packages defining it.
