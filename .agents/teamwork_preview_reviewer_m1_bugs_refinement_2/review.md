# Milestone 1 Bugs Refinement Review

## Review Summary

**Verdict**: **APPROVE**

---

## Quality Review Findings

### [Minor] Finding 1: Untracked Files in Cache Directory

- **What**: Extra files on disk under cache path are not detected by archive-based verification.
- **Where**: `crates/ggen-marketplace/src/marketplace/cache.rs:verify_digest`
- **Why**: When `pack.dat` is present, `verify_digest` validates only the files inside the archive against the disk. It does not check if there are extra untracked files in the cache directory. An attacker who writes an untracked file (e.g., a script `backdoor.sh`) inside `pack.cache_path` will not trigger a verification failure.
- **Suggestion**: If `pack.dat` is used, compare the total number of files on disk (excluding `pack.dat` and `cache_metadata.json`) with the number of file entries in the archive.

---

## Verified Claims

- **LRU Cache Eviction Bug Fix** &rarr; verified via code inspection and `cargo test -p ggen-marketplace` &rarr; **PASS**
  - *Analysis*: Removing the pack from `packs` and subtracting its size from `current_size` before evaluating capacity prevents self-eviction when inserting an update for an existing pack.
- **Save Metadata on Get** &rarr; verified via code inspection and `cargo test -p ggen-marketplace` &rarr; **PASS**
  - *Analysis*: Added `self.save_metadata()` immediately on cache hit when metadata persistence is enabled, ensuring metadata state is saved dynamically.
- **Strict Dependency Cycle Detection** &rarr; verified via DFS tracing and dependency cycle tests &rarr; **PASS**
  - *Analysis*: Replaced BFS with post-order iterative DFS using `DfsState` stack. Accurately detects cycles and correctly orders dependencies.
- **Symlink Traversal & Zip Slip Prevention** &rarr; verified via zip/tar tests and validation &rarr; **PASS**
  - *Analysis*: Explicit checks for symlinks and hardlinks in tar, and `enclosed_name()` plus `Component::ParentDir` validation in zip/tar ensure zero directory traversal vulnerabilities.
- **Deterministic Walk in Registry Indexer** &rarr; verified via script execution and output diff &rarr; **PASS**
  - *Analysis*: Filtering and sorting directories in-place during the walk ensures alphabetical and deterministic ordering.
- **Manifest TOML Syntax Errors** &rarr; verified via registry generation using python3.11 &rarr; **PASS**
  - *Analysis*: Syntactically invalid bulleted lists in features converted to valid TOML array structures.
- **Dependency SemVer Range Parsing** &rarr; verified via test suite execution &rarr; **PASS**
  - *Analysis*: Cleanly handles range syntax like `^1.0.0` or `>=2.0.0` with fallback parsing logic.

---

## Coverage Gaps

- None. Downstream dependency updates across all tools are verified. Risk level: Low.

---

## Unverified Items

- None. All modified aspects were successfully verified and tested.

---

## Adversarial Challenge Report

**Overall risk assessment**: **LOW**

### Challenges

#### [Medium] Challenge 1: Extra Files Injected in Cache Directory

- **Assumption challenged**: The cache directory only contains files extracted from the archive.
- **Attack scenario**: An attacker places an untracked backdoor script inside `pack.cache_path`. If another part of the system dynamically loads or executes files in the package directory without filtering by the manifest, the backdoor runs.
- **Blast radius**: Local code execution if package files are executed dynamically.
- **Mitigation**: Count files on disk and ensure no extra files exist beyond the archive contents.

#### [Low] Challenge 2: Version Requirement Sorting

- **Assumption challenged**: The first matching version in `package.versions` is the latest.
- **Attack scenario**: If the repository returns `package.versions` unsorted, the matching algorithm could return an older matching version.
- **Blast radius**: Installs an older package version than desired.
- **Mitigation**: The codebase documentation states `package.versions` is always sorted newest first, and tests enforce this. Thus the risk is low.

### Stress Test Results

- **ZIP slip attack with relative parent directories** &rarr; installation fails with Path Traversal error &rarr; **PASS**
- **Tar symlink Zip slip attack** &rarr; installation fails with Path Traversal error &rarr; **PASS**
- **Circular dependency** &rarr; DFS resolves cycle and throws DependencyCycle error &rarr; **PASS**

### Unchallenged Areas

- None.
