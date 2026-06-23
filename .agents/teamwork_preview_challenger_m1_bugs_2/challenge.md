# Challenge Report — Milestone 1 Fixes Verification

## Challenge Summary

**Overall risk assessment**: CRITICAL

We have empirically verified the Milestone 1 fixes in `ggen-marketplace` by writing and running adversarial tests. Our stress testing revealed two critical vulnerabilities in the Zip Slip protection and cache verification mechanisms.

---

## Challenges

### [Critical] Challenge 1: Tar Slip Symlink Traversal

- **Assumption challenged**: The assumption that verifying `!dest_file_path.starts_with(dest)` and checking for `ParentDir` (`..`) components in tar entry names is sufficient to prevent writing outside the extraction directory.
- **Attack scenario**: A malicious `.tar.gz` package contains:
  1. A symlink entry named `symlink_dir` pointing to an arbitrary directory outside the target cache (e.g. `/etc/` or `/Users/sac/ggen/`).
  2. A subsequent file entry named `symlink_dir/target.txt` containing malicious data.
  
  During extraction, `entry.unpack` creates the symlink `symlink_dir` pointing to the external directory. When processing the next file entry, the path is checked as `dest/symlink_dir/target.txt`. This path starts with `dest` (syntactically) and contains no `..` components. However, when `entry.unpack` opens the path, the operating system resolves the symlink `symlink_dir`, traversing to the outside directory and writing `target.txt` outside the destination.
- **Blast radius**: Arbitrary file write/overwrite on the user's filesystem during pack installation.
- **Mitigation**: 
  - Ensure that the TAR extractor blocks the creation of symlinks entirely, or canonicalizes the target of any symlink to verify it lies inside `dest` before unpacking.
  - Alternatively, reject symlinks entirely for package contents if they are not required, or run a secondary path verification step that canonicalizes paths before writing.

### [High] Challenge 2: Cache Tampering Verification Bypass

- **Assumption challenged**: The assumption that verifying the hash of the raw `pack.dat` archive file in the cache directory is sufficient to guarantee the integrity of the cached pack files.
- **Attack scenario**: When a pack is cached, the raw compressed archive is saved as `pack.dat` inside the cache directory, alongside the extracted files.
  `PackCache::verify_digest` checks if `pack.dat` exists. If it does, it calculates the hash of `pack.dat` only and compares it to the registered digest.
  If an attacker or local process modifies/tampers with the *extracted* files in the cache (which are what is actually loaded/used by the CLI/application) but leaves `pack.dat` unchanged, `verify_digest` still returns `true`. The tampered files are used without detection.
- **Blast radius**: Code execution or configuration hijacking via tampered cached templates or package ontologies.
- **Mitigation**: 
  - The cache verification should always verify the integrity of the *extracted* files (e.g., via a deterministic sorted file-walk hashing all files in the cache directory), rather than relying only on `pack.dat`.
  - Alternatively, if `pack.dat` is used to skip the file walk for performance, there must be some additional check or the extraction must be re-run from `pack.dat` every time if integrity is critical.

---

## Stress Test Results

### 1. TAR Symlink Traversal (Tar Slip)
- **Scenario**: Extract a `.tar.gz` archive containing a symlink pointing to an outside directory (`/outside`) and a file entry inside that symlink (`symlink_dir/target.txt`).
- **Expected behavior**: Extraction fails with a path traversal error, or the file is not written outside the target directory.
- **Actual behavior**: The file `target.txt` was successfully written outside the extraction directory, and the installer returned `Ok(())`.
- **Result**: **FAIL** (Vulnerability Confirmed)

### 2. Cache Tampering Verification
- **Scenario**: Write `pack.dat` with correct archive bytes, write an extracted file `extracted_file.txt` with original contents, verify the digest, then modify `extracted_file.txt` and verify the digest again.
- **Expected behavior**: Verification fails after `extracted_file.txt` is tampered with.
- **Actual behavior**: Verification succeeded because only `pack.dat` was verified.
- **Result**: **FAIL** (Vulnerability Confirmed)

---

## Unchallenged Areas

- **ZIP Symlink Traversal**: Not challenged because the ZIP extractor (`extract_zip` in `install.rs`) does not support/unpack symlinks (it only creates directories and regular files), so it is not vulnerable to this type of attack.
