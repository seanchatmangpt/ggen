## 2026-06-12T03:07:29Z

You are a teamwork_preview_worker. Your working directory is /Users/sac/ggen/.agents/worker_m1_bugs_refinement/. Your identity is worker_2. Read the PROJECT.md at /Users/sac/ggen/.agents/teamwork_preview_orchestrator_marketplace_audit_1_gen2/PROJECT.md and the user requirements in /Users/sac/ggen/.agents/ORIGINAL_REQUEST.md.

Your objective is to fix the remaining bugs and vulnerabilities identified during the Milestone 1 verification:
1. **LRU Cache Eviction Bug**: In `crates/ggen-marketplace/src/marketplace/cache.rs:insert`, if the cache is full, we run LRU eviction. But if the pack we are inserting/updating is already in the cache, it might get evicted because it is the oldest. Fix it by excluding/removing the target pack before checking capacity and running LRU eviction.
2. **Save Metadata on Get**: In `cache.rs:get`, add `self.save_metadata()` to write the updated access count and last accessed time to disk.
3. **Strict dependency cycle detection**: In `crates/ggen-marketplace/src/marketplace/install.rs`, if a circular dependency is detected (visited contains id), return an error (`Error::DependencyCycle` or similar) instead of silently skipping.
4. **Deterministic Walk in Registry Indexer**: In `marketplace/scripts/generate_registry_index.py`, sort directories/files during walking.
5. **Symlink Traversal in Tar Extraction**: In `install.rs:extract_tar_gz`, check if the entry type is a symlink or hardlink. Return a path traversal error if `entry.header().entry_type().is_symlink()` or `is_link()` is true, since marketplace packs should not contain symlinks.
6. **Cache Verification Bypass**: In `cache.rs:verify_digest`, if `pack.dat` exists:
   - Verify that its digest matches `pack.digest`.
   - Also open `pack.dat` (as ZIP or TAR.GZ) and verify that all file entries in the archive exist on disk under `pack.cache_path` and their contents match the archive entries exactly.
7. **TOML Syntax Errors**: Fix package manifests under `marketplace/packages/` that contain syntax errors (such as bullet lists violating TOML syntax in `customer-loyalty-rewards/package.toml`, `iso-20022-payments/package.toml`, `kyc-aml-compliance/package.toml`, `order-management-system/package.toml`, `trading-platform/package.toml`, `rest-api-template/package.toml`).
8. **Dependency SemVer Range Parsing Bug**: If there are version parsing failures when handling operators like `^` or `>=`, fix them.

After applying the changes, run `cargo test -p ggen-marketplace` to verify that all tests pass, including the newly added tests:
- `test_zip_slip_symlink_traversal_tar`
- `test_cache_verification_tamper_extracted_files`

Document your changes and verification logs in `changes.md` and `handoff.md` in your working directory.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
