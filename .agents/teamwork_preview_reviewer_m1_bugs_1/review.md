# Review Report - Milestone 1 Bugs & Vulnerabilities

**Date**: 2026-06-12
**Reviewer**: reviewer_1 (Teamwork Reviewer & Critic)

---

# PART 1: Quality Review

## Review Summary

**Verdict**: REQUEST_CHANGES

We recommend rejecting the current implementation of Milestone 1 due to critical correctness bugs in the caching logic (leading to cache corruption and file deletion), incomplete/missing cycle detection in dependency resolution, and non-deterministic behavior in the Python registry indexer.

---

## Findings

### [Critical] Finding 1: Cache Corruption / File Deletion during LRU Eviction

- **What**: When updating/inserting a package that is already in the cache, the cache evicts the pack's own directories from the disk if the cache is at capacity and the pack is the Least Recently Used (LRU) one.
- **Where**: `crates/ggen-marketplace/src/marketplace/cache.rs`, `insert` (lines 182–226) and `evict_if_needed` (lines 233–284).
- **Why**: 
  1. The cache capacity check is performed *before* inserting the new entry:
     ```rust
     if new_size > self.config.max_size_bytes || packs.len() >= self.config.max_packs {
         drop(packs);
         self.evict_if_needed(pack.size_bytes)?;
     }
     ```
  2. If the cache is full, `evict_if_needed` is called. The target pack (which we are updating) is still in the `packs` HashMap under its key.
  3. If this pack happens to be the LRU entry, it will sort to the front of `packs_vec` in `evict_if_needed`.
  4. The code removes the pack from the map and calls `fs::remove_dir_all(&pack.cache_path)`.
  5. Back in `insert`, it proceeds to run `packs.insert(cache_key, pack)`.
  6. The in-memory map now claims the pack is cached, but its directory on disk has been deleted, leaving the cache in a corrupted state.
- **Suggestion**: Ensure that the entry being updated is excluded from eviction candidates, or remove the existing entry from the map and delete its files *before* checking and running `evict_if_needed` for the new size.

### [Major] Finding 2: Inefficient and Non-Persistent Cache Metadata Updates on Read (`get`)

- **What**: Cache metadata is not persisted to disk when updating access statistics on read hits.
- **Where**: `crates/ggen-marketplace/src/marketplace/cache.rs`, `get` (lines 155–172).
- **Why**: In `get`, when a cache hit occurs, `pack.record_access()` is called, updating `last_accessed` and `access_count`. However, unlike `insert` and `remove`, `self.save_metadata()` is never called in `get`. Upon application restart, all access updates since the last write are lost, causing inaccurate LRU eviction.
- **Suggestion**: Call `self.save_metadata()` in `get` when persistent config is enabled.

### [Major] Finding 3: Dead/Useless Fallback Code in `verify_digest`

- **What**: The fallback directory-hashing mechanism in `verify_digest` is guaranteed to fail in production.
- **Where**: `crates/ggen-marketplace/src/marketplace/cache.rs`, lines 491–525.
- **Why**: The stored `CachedPack::digest` is the hash of the downloaded compressed archive (`pack.dat`). If `pack.dat` is missing and fallback is used, it hashes the extracted directory contents. Hashing the raw files in a directory will never produce the same digest as hashing the compressed zip/tar.gz file, leading to constant validation failures and re-downloads.
- **Suggestion**: Re-architect fallback hashing to either hash a manifest/canonical metadata file or remove the fallback path entirely if `pack.dat` is the single source of truth.

### [Major] Finding 4: Incomplete / Missing Cycle Detection in Dependency Resolution

- **What**: The dependency resolution code does not perform cycle detection as documented.
- **Where**: `crates/ggen-marketplace/src/marketplace/install.rs`, `resolve_dependencies` (lines 132–177) and `batch_resolve_dependencies` (lines 994–1047).
- **Why**: The implementation uses `visited.contains(&id)` to skip processing of dependencies. If a cycle exists, it silently ignores the back-edge and returns a broken installation sequence without throwing `Error::DependencyResolutionFailed` as described in its API contract.
- **Suggestion**: Implement cycle detection using a recursive/DFS track or a Kahn's topological sort that detects back-edges/unsortable states and returns a `DependencyResolutionFailed` error.

### [Major] Finding 5: Non-Deterministic Checksum Generation in `generate_registry_index.py`

- **What**: The SHA256 checksum generation for package directories is non-deterministic.
- **Where**: `marketplace/scripts/generate_registry_index.py`, `calculate_sha256_checksum` (lines 25–55).
- **Why**: The script calls `os.walk` but does not sort the subdirectory names (`dirs`). Because directory traversal in `os.walk` relies on filesystem directory list order, the walk order will vary between filesystems, operating systems, and installations, leading to different checksums for identical packages.
- **Suggestion**: Sort `dirs` in-place inside `os.walk` (e.g., `dirs.sort()`).

### [Minor] Finding 6: Silent File Read Failures in Indexer

- **What**: The indexer silently ignores file read failures.
- **Where**: `marketplace/scripts/generate_registry_index.py`, `calculate_sha256_checksum` (lines 48–53).
- **Why**: In the event of a read failure (e.g., due to permissions or lock issues), the script silently skips the file and calculates an incomplete/incorrect directory hash, masking potential issues.
- **Suggestion**: Log a warning or error, or throw an exception if a file cannot be read.

### [Major] Finding 7: Invalid TOML Syntax in Package Manifests

- **What**: Multiple packages fail to parse because their `package.toml` files contain invalid Markdown-style list syntax.
- **Where**: For example, `marketplace/packages/customer-loyalty-rewards/package.toml` and `marketplace/packages/iso-20022-payments/package.toml`.
- **Why**: Bulleted lists (`- "..."`) are used directly under `[features]`, which is invalid TOML. This causes the python parser to reject the package, excluding it from the final registry index.
- **Suggestion**: Rewrite feature lists as standard TOML arrays: `features = [ "..." ]`.

---

## Verified Claims

- `cargo test -p ggen-marketplace` passes → Verified via `cargo test -p ggen-marketplace` → **PASS** (228 tests passed, 0 failed).
- Dotted key lookup (`package.metadata`) → Verified by running `python3.11 marketplace/scripts/generate_registry_index.py` → **PASS** (Script executes successfully and indexes 71 packages).

---

## Coverage Gaps

- **Installation Execution Pipeline**: The test suite contains no tests calling `install_pack`, `batch_install`, or `install`.
  - *Risk Level*: **HIGH**
  - *Recommendation*: Add integration tests that test download (using a local mock server or file URLs), signature validation, and archive extraction to prevent regressions.

---

## Unverified Items

- Remote download functionality — cannot be verified in CODE_ONLY network mode due to lack of external internet access.

---
---

# PART 2: Adversarial Review

## Challenge Summary

**Overall Risk Assessment**: HIGH

The primary risk vectors reside in the lack of end-to-end integration tests for signature validation and registry class validation, combined with a race condition / path traversal vulnerability during archive extraction.

---

## Challenges

### [High] Challenge 1: `pack.dat` Path Collision / Overwrite during Extraction

- **Assumption Challenged**: The temporary directory `temp_path` is safe for storing both `pack.dat` and the extracted archive contents.
- **Attack Scenario**: 
  1. The installer writes `pack.dat` containing the downloaded archive to `temp_path`.
  2. The installer extracts the archive files into `temp_path`.
  3. If the archive contains a malicious file named `pack.dat`, the extraction step will overwrite the authentic `pack.dat`.
  4. The installer promotes `temp_path` to `cache_path`.
  5. Subsequent checks (e.g., `verify_digest`) will read the tampered `pack.dat` instead of the original archive, causing validation failure or potential verification bypass depending on signature validations.
- **Blast Radius**: Integrity checks bypass or denial of service (corrupted cache entries).
- **Mitigation**: Extract archive contents to a subdirectory within `temp_path` or write `pack.dat` outside the target extraction folder.

### [Medium] Challenge 2: Cycle Dependency Loop Hole

- **Assumption Challenged**: Package dependency trees do not contain circular references.
- **Attack Scenario**: An attacker publishes two packages with mutually recursive dependencies (`A -> B -> A`). The dependency resolver resolves this cycle silently, leading to undefined installation order where `A` is installed before its dependency `B` has finished installing, breaking post-install scripts or runtime configurations.
- **Blast Radius**: Runtime errors and broken package installations.
- **Mitigation**: Enforce a strict directed acyclic graph (DAG) check during resolution.

---

## Stress Test Results

- **LRU Update on Replace Scenario**:
  - *Input*: Update metadata of LRU cached pack when cache count = `max_packs`.
  - *Expected Behavior*: Stored files are kept; metadata is updated.
  - *Actual Behavior*: The LRU pack's files are deleted from disk by `evict_if_needed` before the new metadata is inserted.
  - *Result*: **FAIL** (Cache Corruption).

---

## Unchallenged Areas

- **Cryptographic Library Core Security**: We assume the underlying `sha2` and `ed25519` libraries are secure and correct.
