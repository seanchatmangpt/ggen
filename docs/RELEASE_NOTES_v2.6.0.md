# Release Notes v2.6.0

**Release Date:** 2025-11-12  
**Type:** Minor Release

## Overview

This release focuses on code quality improvements, bug fixes, and workflow enhancements. Key highlights include Kaizen improvements, Chicago TDD Tools integration, and resolution of Andon signals.

## What's New

### Code Quality Improvements (Kaizen)

- **Magic Numbers Extraction**: Extracted all magic numbers in marketplace search to named constants
  - Created `scoring` module with 9 relevance scoring constants
  - Created `defaults` module with 3 configuration constants
  - Improved code readability and maintainability
  - Made scoring weights easier to tune and understand

### Documentation Consolidation

- **Sparse Priming Representation**: Applied SPR technique to consolidate documentation
  - Reduced documentation size by 90%+ while preserving critical information
  - Consolidated 12+ large documentation files
  - Made documentation more LLM-friendly and easier to scan

### Chicago TDD Tools Integration

- Enhanced `.cursorrules` with CTT best practices
- Added timeout SLAs for all CLI commands
- Improved error handling patterns

## Bug Fixes

### Clippy Linting Errors (24 fixes)

- Fixed all clippy warnings and errors across the codebase
- Improved API ergonomics (`&PathBuf` → `&Path`)
- Standard compliance (`FromStr` trait implementation)
- Performance improvements (`double_ended_iterator_last` → `next_back()`)
- Better error handling (`manual_inspect` → `inspect_err`)

### Error Handling Improvements

- Fixed silent failure in version parsing
- Added descriptive error messages for invalid inputs
- Improved NaN handling in relevance score comparison

### Git Hooks & Workflows

- Fixed timeout issues with pre-push hook
- Resolved Cargo lock contention in workflows
- Added `check-pre-push` task with 30s timeout
- Improved reliability of pre-push validation

## Technical Details

### Version Updates

- Main workspace: `2.5.1` → `2.6.0`
- ggen-domain: `3.0.0` → `3.1.0` (separate version scheme)
- All workspace members updated consistently

### Files Changed

- `crates/ggen-domain/src/marketplace/search.rs` - Magic number extraction
- `Makefile.toml` - Workspace task configuration
- `.cursorrules` - Chicago TDD Tools integration
- Multiple files - Clippy linting fixes

## Upgrade Instructions

No breaking changes in this release. Upgrade with:

```bash
cargo update
```

Or if installing from source:

```bash
git pull
cargo build --release
```

## Known Issues

None at this time.

## Contributors

- Sean Chatman

## Full Changelog

See [CHANGELOG.md](../CHANGELOG.md) for complete list of changes.

