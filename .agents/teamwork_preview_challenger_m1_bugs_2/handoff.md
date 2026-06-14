# Handoff Report — Milestone 1 Verification

## 1. Observation

We executed cargo test inside the `ggen-marketplace` package:
- **Command**: `cargo test -p ggen-marketplace`
- **Output**:
```
failures:
    marketplace::install::tests::test_cache_verification_tamper_extracted_files
    marketplace::install::tests::test_zip_slip_symlink_traversal_tar

test result: FAILED. 227 passed; 2 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.05s
```

The specific failures and error messages:
1. For symlink traversal in TAR extraction:
   ```
   thread 'marketplace::install::tests::test_zip_slip_symlink_traversal_tar' (8184435) panicked at crates/ggen-marketplace/src/marketplace/install.rs:2060:13:
   VULNERABILITY: Zip Slip symlink traversal allowed writing outside extraction directory!
   ```
2. For cache verification after tampering:
   ```
   thread 'marketplace::install::tests::test_cache_verification_tamper_extracted_files' (8184421) panicked at crates/ggen-marketplace/src/marketplace/install.rs:2106:9:
   VULNERABILITY: Cache verification succeeded even though extracted files were tampered with!
   ```

We observed the following code in `crates/ggen-marketplace/src/marketplace/install.rs` lines 858-868:
```rust
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
```

We also observed the following code in `crates/ggen-marketplace/src/marketplace/cache.rs` lines 465-474:
```rust
        let archive_path = pack.cache_path.join("pack.dat");
        if archive_path.exists() {
            // High-performance direct archive hash matching the installer checksum
            let mut hasher = Sha256::new();
            match fs::read(&archive_path) {
                Ok(contents) => {
                    hasher.update(&contents);
                    let calculated_digest = hex::encode(hasher.finalize());
                    let matches = calculated_digest == pack.digest;
```

---

## 2. Logic Chain

1. **Path Traversal Vulnerability**:
   - `entry.unpack(&dest_file_path)` unpacked a symlink entry to `dest_file_path` (inside the target directory) pointing to an external directory (Observation 1).
   - A subsequent entry `symlink_dir/target.txt` had its path checked as `dest/symlink_dir/target.txt`. Syntactically, this starts with `dest` and does not contain `..`, so it bypassed the path prefix check (Observation 1).
   - When unpacking `dest/symlink_dir/target.txt`, the OS followed the symlink `symlink_dir` and wrote the file outside the extraction directory.
   - Therefore, checking syntactical path prefixes is insufficient when symlinks are allowed.

2. **Cache Verification Vulnerability**:
   - `verify_digest` checks if `pack.dat` exists. If it does, it calculates and compares the hash of `pack.dat` against the expected digest, returning early (Observation 2).
   - Modifications to the extracted files are ignored by this check, because only `pack.dat` is read and hashed.
   - Therefore, an attacker can modify the actual template/ontology files used by the application, and the cache verification will still report the cache as valid/unmodified.

---

## 3. Caveats

- We assumed that ZIP files cannot contain symlinks because the `zip` crate extractor code in `install.rs` does not support symlink creation. We did not write a test for ZIP symlinks since they are not supported.
- We did not implement the fixes, in accordance with the rule to not modify implementation code.

---

## 4. Conclusion

The Milestone 1 fixes for Zip Slip protection and cache verification have critical security gaps:
- **Tar Slip**: The TAR extractor allows symlink creation, allowing files to be written outside the target directory.
- **Cache Verification**: The cache verification can be bypassed by modifying the extracted files on disk while leaving `pack.dat` intact.

---

## 5. Verification Method

To independently verify:
1. Run `cargo test -p ggen-marketplace` to execute all tests (including our added tests `test_zip_slip_symlink_traversal_tar` and `test_cache_verification_tamper_extracted_files`).
2. Verify that these two tests fail, confirming both vulnerabilities are active.
