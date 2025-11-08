# URGENT FIX PLAN - Template API Breaking Change

**Date**: 2025-11-07
**Severity**: 🔴 **CRITICAL - BLOCKING v2.5.0 RELEASE**
**Estimated Time**: 4-8 hours

---

## Problem Statement

**Breaking Change**: The `Frontmatter` struct no longer has a `vars` field, breaking 10+ test files and potentially production code.

**Impact**:
- ❌ **10+ test files** fail to compile
- ❌ **0 tests can run** (blocks entire test suite)
- ❌ **v2.5.0 release blocked** (not production ready)
- ⚠️ **Existing users** may have projects broken if they use `vars`

**Affected Files**:
1. `crates/ggen-core/tests/template_comprehensive_test.rs` (10 errors)
2. `examples/rdf_template_integration.rs` (5 errors)
3. Any production code using `template.front.vars.*`

---

## Root Cause Analysis

### Old API (Expected by Tests)

```rust
pub struct Frontmatter {
    pub to: Option<String>,
    pub from: Option<String>,
    pub force: bool,
    // ... other fields
    pub vars: HashMap<String, serde_json::Value>, // ❌ MISSING!
}

// Usage in tests:
template.front.vars.get("name").and_then(|v| v.as_str())
```

### Current API (What Exists Now)

```rust
pub struct Frontmatter {
    pub to: Option<String>,
    pub from: Option<String>,
    pub force: bool,
    pub unless_exists: bool,
    pub inject: Option<Inject>,
    pub skip_if: Option<String>,
    pub eof: Option<String>,
    pub sh: Option<String>,
    pub message: Option<String>,
    pub add_package: Option<String>,
    pub add_dev_package: Option<String>,
    // ... MORE FIELDS (20+ total)
    // ❌ NO 'vars' FIELD!
}
```

### Why the Change Happened

**Hypothesis**: The `vars` field was refactored into a different structure or removed in favor of:
1. Template body variables (Tera context)
2. Frontmatter-level configuration only
3. Separate variable storage mechanism

**Need to Investigate**:
- When was `vars` removed?
- What replaced it?
- Is there migration path?

---

## Fix Options

### Option 1: Restore `vars` Field (RECOMMENDED)

**Pros**:
- ✅ Minimal code changes
- ✅ Backward compatible
- ✅ Quick fix (2-4 hours)
- ✅ Preserves test expectations

**Cons**:
- ⚠️ May conflict with new architecture
- ⚠️ May not be the "correct" long-term solution

**Implementation**:

```rust
// File: crates/ggen-core/src/template.rs
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Frontmatter {
    // ... existing fields

    /// Template variables (for backward compatibility and test support)
    #[serde(default)]
    pub vars: HashMap<String, serde_json::Value>,
}
```

**Testing**:
```bash
# Verify all tests compile
cargo test --no-run --workspace

# Run all tests
cargo test --workspace
```

**Timeline**: 2-4 hours

---

### Option 2: Update All Tests to New API

**Pros**:
- ✅ Aligns with new architecture
- ✅ Future-proof solution

**Cons**:
- ❌ More work (8-12 hours)
- ❌ Requires understanding new variable system
- ❌ May break user code if they use `vars`

**Implementation**:

**Step 1**: Understand new variable system
```rust
// How do variables work now?
// Is it in Context? In Template body? Elsewhere?
```

**Step 2**: Update all 10+ test files
```rust
// OLD:
assert_eq!(
    template.front.vars.get("name").and_then(|v| v.as_str()),
    Some("test")
);

// NEW: (depends on new API)
// Option A: Variables in context only
let context = template.render_context()?;
assert_eq!(context.get("name"), Some("test"));

// Option B: Variables in separate field
assert_eq!(template.variables.get("name"), Some("test"));

// Option C: Variables in body only (parsed from YAML)
let vars = template.extract_vars_from_body()?;
assert_eq!(vars.get("name"), Some("test"));
```

**Step 3**: Update examples
```rust
// Update: examples/rdf_template_integration.rs
```

**Timeline**: 8-12 hours

---

### Option 3: Hybrid Approach (BEST LONG-TERM)

**Strategy**: Restore `vars` field for v2.5.0, plan migration for v3.0.0

**Phase 1 (v2.5.0 - Now)**:
1. Restore `vars` field to `Frontmatter`
2. Add deprecation warning
3. Document migration path
4. Release v2.5.0 with tests passing

**Phase 2 (v2.6.0 - Next Sprint)**:
1. Implement new variable system
2. Add migration tool
3. Update documentation

**Phase 3 (v3.0.0 - Future)**:
1. Remove deprecated `vars` field
2. Breaking change properly communicated
3. Migration guide published

**Implementation**:

```rust
// File: crates/ggen-core/src/template.rs
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Frontmatter {
    // ... existing fields

    /// Template variables
    ///
    /// ⚠️ DEPRECATED: This field will be removed in v3.0.0
    /// Use the new variable system instead (TODO: link to docs)
    #[deprecated(since = "2.6.0", note = "Use new variable system")]
    #[serde(default)]
    pub vars: HashMap<String, serde_json::Value>,
}
```

**Timeline**:
- Phase 1: 2-4 hours
- Phase 2: 1-2 days
- Phase 3: 1 day

---

## Recommended Fix

**DECISION**: Use **Option 3 - Hybrid Approach**

**Rationale**:
1. ✅ **Unblock v2.5.0 immediately** (restore `vars`)
2. ✅ **Maintain backward compatibility**
3. ✅ **Provide migration path** for v3.0.0
4. ✅ **Low risk** of breaking user code
5. ✅ **Fast fix** (2-4 hours)

---

## Implementation Steps

### Step 1: Restore `vars` Field (30 min)

```bash
# Edit file
vim crates/ggen-core/src/template.rs

# Add field
pub struct Frontmatter {
    // ... existing fields

    #[serde(default)]
    pub vars: HashMap<String, serde_json::Value>,
}
```

### Step 2: Verify Compilation (15 min)

```bash
# Check tests compile
cargo test --no-run --workspace

# Expected output: All tests compile successfully
```

### Step 3: Run All Tests (30 min)

```bash
# Run full test suite
cargo test --workspace -- --test-threads=1

# Expected: 570+ tests pass
```

### Step 4: Run Specific Broken Tests (15 min)

```bash
# Test template_comprehensive_test
cargo test --package ggen-core --test template_comprehensive_test

# Test rdf_template_integration
cargo test --example rdf_template_integration

# Expected: All pass
```

### Step 5: Verify Chicago TDD Tests (15 min)

```bash
# Test ontology_driven_e2e
cargo test --test chicago_tdd

# Expected: 40 Chicago TDD tests pass
```

### Step 6: Run Marketplace Tests (15 min)

```bash
# Test marketplace integration
cargo test --package ggen-cli --test complete_marketplace_test
cargo test --package ggen-marketplace

# Expected: 70+ marketplace tests pass
```

### Step 7: Add Deprecation Warning (30 min)

```rust
// Add deprecation notice
#[deprecated(
    since = "2.6.0",
    note = "Template vars field will be removed in v3.0.0. See migration guide: https://docs.ggen.io/migration/v3"
)]
pub vars: HashMap<String, serde_json::Value>,
```

### Step 8: Update Changelog (15 min)

```markdown
## [2.5.0] - 2025-11-07

### Fixed
- Restored `Frontmatter.vars` field for backward compatibility
- Fixed compilation of 10+ test files

### Deprecated
- `Frontmatter.vars` field (will be removed in v3.0.0)
  - See migration guide: https://docs.ggen.io/migration/v3
```

### Step 9: Commit and Push (15 min)

```bash
# Commit fix
git add crates/ggen-core/src/template.rs
git commit -m "fix: restore Frontmatter.vars field for backward compatibility

- Fixes #<issue-number>
- Restores template variable support
- Adds deprecation warning for v3.0.0 migration
- Unblocks v2.5.0 release

Breaking change fix:
- 10+ test files now compile and pass
- Maintains backward compatibility with existing code
- Provides migration path for future versions"

# Push to branch
git push origin fix/restore-frontmatter-vars
```

---

## Verification Checklist

**Before Release**:

- [ ] `cargo test --no-run --workspace` compiles successfully
- [ ] `cargo test --workspace` runs all tests
- [ ] All Chicago TDD tests pass (40 tests)
- [ ] All marketplace tests pass (70+ tests)
- [ ] Template comprehensive tests pass (10+ tests)
- [ ] RDF integration example compiles and runs
- [ ] Deprecation warning added
- [ ] Changelog updated
- [ ] Migration guide started (for v3.0.0)
- [ ] PR created and reviewed
- [ ] CI/CD passes (once workflow added)

**Timeline**:
- Total estimated time: **4-6 hours**
- Buffer for testing: **2 hours**
- **Total**: 6-8 hours (1 day)

---

## Post-Fix Actions

### After v2.5.0 Release

1. **Add CI/CD Workflow** (2 hours)
   - Create `.github/workflows/test.yml`
   - Run tests on every PR
   - Prevent future breaking changes

2. **Add Regression Test** (1 hour)
   - Test nested tokio runtime fix
   - Ensure no future panics

3. **Plan v2.6.0 Migration** (1 week)
   - Design new variable system
   - Create migration tool
   - Update documentation

4. **Communicate Breaking Change** (1 day)
   - Blog post about v3.0.0 changes
   - Update migration guide
   - Notify users via GitHub discussions

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| **Restoring `vars` breaks new features** | Low | High | Test thoroughly; review recent commits |
| **Users depend on removed `vars`** | Medium | High | Deprecation warning + migration guide |
| **New variable system not ready** | Low | Medium | Hybrid approach allows time to implement |
| **Tests still fail after fix** | Low | Critical | Run full test suite before release |

---

## Rollback Plan

**If fix causes issues**:

1. **Revert commit**:
   ```bash
   git revert <commit-hash>
   git push origin master
   ```

2. **Use Option 2** instead (update all tests)
3. **Delay v2.5.0 release** by 1 week
4. **Notify users** of delay

---

## Success Criteria

**Fix is successful when**:

1. ✅ All 570+ tests compile
2. ✅ All 570+ tests pass
3. ✅ Chicago TDD tests pass (40 tests)
4. ✅ Marketplace tests pass (70+ tests)
5. ✅ Template tests pass (10+ tests)
6. ✅ No new compilation errors
7. ✅ No new test failures
8. ✅ CI/CD workflow added and passing

**Ready for v2.5.0 release**: YES

---

**Action Owner**: Core Development Team
**Deadline**: End of day (2025-11-07)
**Status**: 🔴 **URGENT - IN PROGRESS**
