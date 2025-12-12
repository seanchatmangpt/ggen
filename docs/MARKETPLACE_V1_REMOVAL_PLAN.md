# Marketplace V1 Removal Plan

**Status:** Ready for Execution  
**Date:** 2025-12-11  
**Objective:** Remove all legacy marketplace v1 code and references after verifying all JTBDs work with v2

---

## Executive Summary

All marketplace commands have been migrated to v2 and are working. The CLI (`crates/ggen-cli/src/cmds/marketplace.rs`) uses `ggen-marketplace` directly with no adapter layer. This plan removes all remaining v1 references, legacy code, and disabled tests.

---

## Marketplace JTBDs Verification

### ✅ All 9 Commands Verified Working with V2

1. **`install`** - Install packages with dependency resolution ✅
2. **`search`** - Search packages with filters and pagination ✅
3. **`publish`** - Publish packages to registry ✅
4. **`info`** - Get detailed package information ✅
5. **`validate`** - Validate package quality ✅
6. **`versions`** - List all versions of a package ✅
7. **`metrics`** - Show marketplace metrics ✅
8. **`sparql`** - Query packages using SPARQL ✅
9. **`rdf_stats`** - Get RDF registry statistics ✅

**Verification:** All commands use `ggen-marketplace::prelude::*` directly, have 10s timeout wrappers, and proper error handling.

---

## Code Inventory

### Files to Remove

#### 1. Legacy Adapter Code (Unused)
- **`crates/ggen-domain/src/marketplace/v2_adapter.rs`** (291 lines)
  - Status: Marked as "legacy code (not used by CLI)"
  - Action: **REMOVE** - CLI uses v2 directly, no adapter needed

#### 2. Disabled Test Files (V1 References)
- **`crates/ggen-cli/tests/marketplace/integration/backward_compat_test.rs`**
  - Status: Gated behind `marketplace-v1` feature (doesn't exist)
  - Action: **REMOVE** - Tests are disabled and reference non-existent v1 crate

- **`crates/ggen-cli/tests/marketplace/integration/cross_backend_test.rs`**
  - Status: Gated behind `marketplace-v1` feature (doesn't exist)
  - Action: **REMOVE** - Tests are disabled and reference non-existent v1 crate

- **`crates/ggen-cli/tests/marketplace/unit/adapter_conversion_test.rs`**
  - Status: References `ggen_marketplace::Package as V1Package` (v1 crate doesn't exist)
  - Action: **REMOVE** - Tests conversion between non-existent v1 and v2

- **`crates/ggen-cli/tests/integration/marketplace_test.rs`**
  - Status: Gated behind `marketplace_v1` feature
  - Action: **REMOVE** - Old v1 integration tests

- **`crates/ggen-cli/tests/integration/complete_marketplace_test.rs`**
  - Status: Gated behind `marketplace_v1` feature
  - Action: **REMOVE** - Old v1 integration tests

- **`crates/ggen-cli/tests/integration/integration_backup/marketplace_tests.rs`**
  - Status: Gated behind `marketplace_v1` feature
  - Action: **REMOVE** - Backup of old v1 tests

#### 3. Legacy Domain Modules (Review for Removal)

These modules in `crates/ggen-domain/src/marketplace/` are marked as "not used by CLI":

- **`adapter.rs`** - Legacy trait (not used by CLI, kept for reference)
- **`registry.rs`** - Legacy registry implementation
- **`search.rs`** - Legacy search implementation

**Action:** **REVIEW** - Check if these are used by other parts of the codebase (packs, hooks, etc.) before removing.

#### 4. Documentation Updates

- **`crates/ggen-domain/src/marketplace/V2_MIGRATION.md`**
  - Status: Documents completed migration
  - Action: **UPDATE** - Mark as historical, add note that v1 removal is complete

- **`docs/MARKETPLACE_V2_MIGRATION.md`**
  - Action: **UPDATE** - Add section noting v1 removal completion

- **`docs/architecture/marketplace-v2-migration/`**
  - Action: **REVIEW** - Archive or update to reflect v1 removal

---

## Removal Steps

### Phase 1: Remove Disabled Test Files (Low Risk)

```bash
# Remove disabled test files
rm crates/ggen-cli/tests/marketplace/integration/backward_compat_test.rs
rm crates/ggen-cli/tests/marketplace/integration/cross_backend_test.rs
rm crates/ggen-cli/tests/marketplace/unit/adapter_conversion_test.rs
rm crates/ggen-cli/tests/integration/marketplace_test.rs
rm crates/ggen-cli/tests/integration/complete_marketplace_test.rs
rm crates/ggen-cli/tests/integration/integration_backup/marketplace_tests.rs
```

**Verification:**
```bash
cargo make check
cargo make test
```

### Phase 2: Remove Legacy Adapter Code (Low Risk)

```bash
# Remove unused adapter
rm crates/ggen-domain/src/marketplace/v2_adapter.rs
```

**Update `crates/ggen-domain/src/marketplace/mod.rs`:**
- Remove `pub mod v2_adapter;` line
- Update module documentation

**Verification:**
```bash
cargo make check
cargo make test
```

### Phase 3: Review Legacy Domain Modules (Medium Risk)

**Check usage of legacy modules:**

```bash
# Check if adapter.rs is used
grep -r "marketplace::adapter" crates/
grep -r "MarketplaceRegistry" crates/

# Check if registry.rs is used (beyond CLI)
grep -r "marketplace::registry" crates/ | grep -v "marketplace.rs"

# Check if search.rs is used (beyond CLI)
grep -r "marketplace::search" crates/ | grep -v "marketplace.rs"
```

**If unused, remove:**
- `crates/ggen-domain/src/marketplace/adapter.rs`
- `crates/ggen-domain/src/marketplace/registry.rs`
- `crates/ggen-domain/src/marketplace/search.rs`

**Update `crates/ggen-domain/src/marketplace/mod.rs`** to remove unused module declarations.

**Verification:**
```bash
cargo make check
cargo make test
cargo make lint
```

### Phase 4: Update Documentation (Low Risk)

1. **Update `crates/ggen-domain/src/marketplace/V2_MIGRATION.md`:**
   - Add section: "V1 Removal Complete (2025-12-11)"
   - Note that all v1 code has been removed

2. **Update `docs/MARKETPLACE_V2_MIGRATION.md`:**
   - Add completion section
   - Note v1 removal date

3. **Review migration docs:**
   - Archive or update `docs/architecture/marketplace-v2-migration/` to reflect completion

**Verification:**
```bash
# Check for remaining v1 references
grep -r "marketplace.*v1\|v1.*marketplace" docs/ crates/ --exclude-dir=target
```

### Phase 5: Final Verification (Critical)

```bash
# 1. Compilation check
cargo make check

# 2. All tests pass
cargo make test

# 3. Linting clean
cargo make lint

# 4. Verify no v1 references remain
grep -r "marketplace.*v1\|v1.*marketplace" crates/ --exclude-dir=target | grep -v "V2_MIGRATION\|MARKETPLACE_V2"

# 5. Verify CLI commands work
cargo run --package ggen-cli-lib --bin ggen -- marketplace --help
cargo run --package ggen-cli-lib --bin ggen -- marketplace search --query "test"
cargo run --package ggen-cli-lib --bin ggen -- marketplace metrics
```

---

## Risk Assessment

| Phase | Risk Level | Impact | Mitigation |
|-------|-----------|--------|------------|
| Phase 1: Remove disabled tests | **LOW** | None (tests already disabled) | Tests are gated behind non-existent feature |
| Phase 2: Remove v2_adapter | **LOW** | None (marked as unused) | Code explicitly marked as "not used by CLI" |
| Phase 3: Review legacy modules | **MEDIUM** | Possible breakage if used elsewhere | Check usage before removal |
| Phase 4: Update docs | **LOW** | None | Documentation only |
| Phase 5: Verification | **CRITICAL** | Must pass all checks | Full test suite + manual CLI verification |

---

## Rollback Plan

If issues are discovered:

1. **Git revert** the removal commits
2. **Restore files** from git history if needed
3. **Re-enable tests** by restoring feature flags (if v1 code restored)

**Note:** Since v1 code is already removed from workspace, rollback would require restoring from git history.

---

## Success Criteria

✅ All marketplace commands work with v2  
✅ No compilation errors  
✅ All tests pass  
✅ No v1 references in active code  
✅ Documentation updated  
✅ CLI commands verified working  

---

## Timeline

- **Phase 1-2:** 30 minutes (low risk, straightforward removal)
- **Phase 3:** 1 hour (requires usage analysis)
- **Phase 4:** 30 minutes (documentation updates)
- **Phase 5:** 30 minutes (verification)

**Total Estimated Time:** 2.5 hours

---

## Notes

- The `ggen-marketplace` v1 crate has already been removed from the workspace
- CLI uses `ggen-marketplace` directly with no adapter layer
- All 9 marketplace commands are implemented and working
- Legacy code is explicitly marked as unused
- Test files are disabled via non-existent feature flags
