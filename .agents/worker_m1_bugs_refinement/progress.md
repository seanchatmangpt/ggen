# Progress Log

Last visited: 2026-06-11T20:13:30-07:00

## Milestone 1 Refinement Tasks
- [x] 1. LRU Cache Eviction Bug (exclude target pack before capacity check in `cache.rs:insert`)
- [x] 2. Save Metadata on Get (call `self.save_metadata()` in `cache.rs:get`)
- [x] 3. Strict dependency cycle detection (return error in `install.rs` if cycle detected)
- [x] 4. Deterministic Walk in Registry Indexer (sort directories/files in `generate_registry_index.py`)
- [x] 5. Symlink Traversal in Tar Extraction (check matches on EntryType::Symlink or Link in `install.rs:extract_tar_gz`)
- [x] 6. Cache Verification Bypass (verify digest and extract/compare pack.dat file contents in `cache.rs:verify_digest`)
- [x] 7. TOML Syntax Errors (fixed package manifests under `marketplace/packages/`)
- [x] 8. Dependency SemVer Range Parsing Bug (resolved version requirement matching in `install.rs`)
- [/] 9. Run cargo test and confirm all pass including new tests (running in background)
