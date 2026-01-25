<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Security Remediation Plan - ggen-ontology-core](#security-remediation-plan---ggen-ontology-core)
  - [Fixing Unwrap/Expect Poka-Yoke Violations](#fixing-unwrapexpect-poka-yoke-violations)
  - [Overview](#overview)
  - [Affected Files](#affected-files)
    - [1. entity_mapper.rs (15 violations)](#1-entity_mapperrs-15-violations)
    - [2. triple_store.rs (23 violations)](#2-triple_storers-23-violations)
    - [3. validators.rs (15 violations)](#3-validatorsrs-15-violations)
  - [Implementation Steps](#implementation-steps)
    - [Step 1: Fix entity_mapper.rs (15 minutes)](#step-1-fix-entity_mapperrs-15-minutes)
    - [Step 2: Fix triple_store.rs (45 minutes)](#step-2-fix-triple_storers-45-minutes)
    - [Step 3: Fix validators.rs (30 minutes)](#step-3-fix-validatorsrs-30-minutes)
  - [Testing & Verification](#testing--verification)
    - [Test Execution Order](#test-execution-order)
    - [Verification Checklist](#verification-checklist)
  - [Risk Mitigation](#risk-mitigation)
    - [Risk 1: Breaking Existing Tests](#risk-1-breaking-existing-tests)
    - [Risk 2: Changing Default Behavior](#risk-2-changing-default-behavior)
    - [Risk 3: Test Coverage Loss](#risk-3-test-coverage-loss)
  - [Success Criteria](#success-criteria)
  - [Rollback Plan](#rollback-plan)
  - [Timeline](#timeline)
  - [Files to Modify](#files-to-modify)
  - [Approval Process](#approval-process)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Security Remediation Plan - ggen-ontology-core
## Fixing Unwrap/Expect Poka-Yoke Violations

**Priority**: CRITICAL
**Estimated Time**: 2-3 hours
**Target**: Zero unwrap/expect in production code

---

## Overview

The ggen-ontology-core crate violates the project's strict Poka-Yoke error handling standards with 53 `unwrap()` and `expect()` calls that can cause runtime panics.

**Project Standard** (from CLAUDE.md):
```toml
[workspace.lints.clippy]
expect_used = "deny"
unwrap_used = "deny"
panic = "deny"
```

---

## Affected Files

### 1. entity_mapper.rs (15 violations)

**Location 1 & 2: Lines 101 & 171**
```rust
// BEFORE (VIOLATES)
matches.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
```

**Analysis**: `partial_cmp()` returns `Option<Ordering>`. For floats, it can return `None` for NaN comparisons. The code uses `unwrap_or()` but the clippy rule still flags the unwrap.

**SOLUTION A: Use proper error handling**
```rust
// AFTER (CORRECT)
matches.sort_by(|a, b| {
    match (b.score.partial_cmp(&a.score), a.score.partial_cmp(&b.score)) {
        (Some(ord), _) => ord,
        (None, Some(ord)) => ord,
        (None, None) => std::cmp::Ordering::Equal,
    }
});
```

**SOLUTION B: Simplified (recommended)**
```rust
// SIMPLEST (BEST)
matches.sort_by(|a, b| b.score.partial_cmp(&a.score)
    .unwrap_or(std::cmp::Ordering::Equal));
```

**Issue with Solution B**: Clippy still flags `unwrap_or()` as indirect unwrap. Better approach:

**SOLUTION C: No unwrap (RECOMMENDED)**
```rust
// CLEANEST - No unwrap at all
matches.sort_by(|a, b| {
    b.score.partial_cmp(&a.score)
        .unwrap_or(std::cmp::Ordering::Equal)
});
```

Actually, the issue is that `unwrap_or()` is technically calling unwrap. Let's use a different approach:

**SOLUTION D: Pattern match**
```rust
// BEST APPROACH
matches.sort_by(|a, b| {
    if a.score.is_nan() || b.score.is_nan() {
        std::cmp::Ordering::Equal
    } else {
        b.score.partial_cmp(&a.score).unwrap()
    }
});
```

Actually the cleanest is:

**SOLUTION E: Float-safe comparison**
```rust
// CORRECT AND CLEAN
matches.sort(|a, b| {
    // For floats with potential NaN, use this pattern
    if b.score > a.score { std::cmp::Ordering::Greater }
    else if b.score < a.score { std::cmp::Ordering::Less }
    else { std::cmp::Ordering::Equal }
});
```

This avoids partial_cmp entirely.

---

**Test Cases** (Lines 43-104):
```rust
let matches = EntityMapper::match_policy("Privacy Policy").unwrap();
let matches = EntityMapper::match_policy("Security Policy").unwrap();
let result1 = EntityMapper::match_data_classification("Confidential").unwrap();
let result2 = EntityMapper::match_data_classification("Confidential").unwrap();
let matches = EntityMapper::match_service_level(99.99).unwrap();
let matches = EntityMapper::match_service_level(95.0).unwrap();
let matches = EntityMapper::match_security_control("MFA").unwrap();
let matches = EntityMapper::match_compute_service("VM").unwrap();
let matches = EntityMapper::match_compute_service("Kubernetes").unwrap();
```

**Note**: Test code unwraps are typically acceptable. These can be left as-is since test panics are expected failure behavior. However, strict linting will still flag them.

**Remediation**: Convert to `expect()` with descriptive message:
```rust
let matches = EntityMapper::match_policy("Privacy Policy")
    .expect("test: match_policy should succeed");
```

Or use `assert!()`:
```rust
let matches = match EntityMapper::match_policy("Privacy Policy") {
    Ok(m) => m,
    Err(e) => panic!("test failure: {}", e),
};
```

---

### 2. triple_store.rs (23 violations)

**Location: Line 41 - Default Impl**
```rust
// BEFORE (VIOLATES)
impl Default for TripleStore {
    fn default() -> Self {
        Self::new().expect("Failed to create default TripleStore")
    }
}
```

**Issue**: `TripleStore::new()` returns `Result<Self>`. Using `expect()` violates error handling standards.

**SOLUTION**:
```rust
// OPTION A: Remove Default impl if not needed
// (Check if Default is actually used)

// OPTION B: Return Result-based initialization
impl TripleStore {
    fn new() -> Result<Self> { ... }  // Already exists

    // Remove the problematic Default impl
    // And require explicit error handling
}

// OPTION C: Use a factory method instead
impl TripleStore {
    pub fn new_default() -> Self {
        Self::new().expect("Failed to create default TripleStore")
    }
}
```

**Recommended**: Remove Default impl since TripleStore::new() already exists and returns Result.

**Lines 327-370**: Test file operations
```rust
let store = TripleStore::new().unwrap();
assert!(store.is_empty().unwrap());
file.write_all(turtle_content.as_bytes()).unwrap();
file.flush().unwrap();
store.load_turtle(file.path()).unwrap();
```

**Issue**: Test code unwraps are flagged by strict linting.

**Remediation Options**:

**Option 1: Use `?` operator** (requires test to return Result)
```rust
#[test]
fn test_load_turtle() -> Result<()> {
    let mut file = NamedTempFile::new()?;
    let turtle_content = r#"@prefix ex: <http://example.com/> ."#;
    file.write_all(turtle_content.as_bytes())?;
    file.flush()?;

    let store = TripleStore::new()?;
    store.load_turtle(file.path())?;
    assert!(store.is_empty()?);

    Ok(())
}
```

**Option 2: Use assert_ok!()** (from chicago-tdd-tools)
```rust
#[test]
fn test_load_turtle() {
    let mut file = NamedTempFile::new().unwrap();
    let store = TripleStore::new().unwrap();
    assert_ok!(store.load_turtle(file.path()));
    assert_ok!(store.is_empty());
}
```

**Option 3: Descriptive panic** (acceptable for tests)
```rust
#[test]
fn test_load_turtle() {
    let store = TripleStore::new()
        .expect("test setup failed: could not create TripleStore");
}
```

**Recommended**: Use Option 1 (`?` operator with `Result<()>` return) for best practices.

---

### 3. validators.rs (15 violations)

**Lines 141-207**: Test file operations
```rust
let mut file = NamedTempFile::new().unwrap();
let content = r#"..."#;
file.write_all(content.as_bytes()).unwrap();
file.flush().unwrap();
let report = validate_turtle(file.path()).unwrap();
let report = validate_rdf_xml(file.path()).unwrap();
let result = validate_sparql_query(query).unwrap();
```

**Issue**: Same as triple_store - test unwraps flagged by strict linting.

**Remediation**: Convert to `Result<()>` tests with `?` operator:
```rust
#[test]
fn test_validate_valid_turtle() -> Result<()> {
    let mut file = NamedTempFile::new()?;
    let content = r#"..."#;
    file.write_all(content.as_bytes())?;
    file.flush()?;

    let report = validate_turtle(file.path())?;
    assert!(report.is_valid);

    Ok(())
}
```

---

## Implementation Steps

### Step 1: Fix entity_mapper.rs (15 minutes)

**File**: `crates/ggen-ontology-core/src/entity_mapper.rs`

**Changes**:

1. **Lines 101 & 171**: Replace float sorting with non-NaN comparison
```rust
// Replace:
matches.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));

// With:
matches.sort_by(|a, b| {
    if b.score > a.score {
        std::cmp::Ordering::Greater
    } else if b.score < a.score {
        std::cmp::Ordering::Less
    } else {
        std::cmp::Ordering::Equal
    }
});
```

2. **Test functions (lines 43-104)**: Keep as-is or convert to descriptive panic:
```rust
let matches = EntityMapper::match_policy("Privacy Policy")
    .expect("Unit test: match_policy should handle standard input");
```

---

### Step 2: Fix triple_store.rs (45 minutes)

**File**: `crates/ggen-ontology-core/src/triple_store.rs`

**Changes**:

1. **Line 41**: Remove Default impl (already have ::new())
```rust
// REMOVE THIS ENTIRE IMPL:
impl Default for TripleStore {
    fn default() -> Self {
        Self::new().expect("Failed to create default TripleStore")
    }
}

// Users must call TripleStore::new()? explicitly
```

2. **Convert test functions to Result<()>**:
```rust
#[test]
fn test_triple_store_empty() -> Result<()> {
    let store = TripleStore::new()?;
    assert!(store.is_empty()?);
    Ok(())
}

#[test]
fn test_triple_count() -> Result<()> {
    let store = TripleStore::new()?;
    let count = store.triple_count()?;
    assert_eq!(count, 0);
    Ok(())
}

#[test]
fn test_load_turtle() -> Result<()> {
    let mut file = NamedTempFile::new()?;
    let turtle_content = r#"..."#;
    file.write_all(turtle_content.as_bytes())?;
    file.flush()?;

    let store = TripleStore::new()?;
    store.load_turtle(file.path())?;
    assert!(store.is_empty()?);
    Ok(())
}
```

---

### Step 3: Fix validators.rs (30 minutes)

**File**: `crates/ggen-ontology-core/src/validators.rs`

**Changes**:

Convert all test functions from `fn test()` to `fn test() -> Result<()>`:

```rust
#[test]
fn test_validate_valid_turtle() -> Result<()> {
    let mut file = NamedTempFile::new()?;
    let content = r#"
@prefix ex: <http://example.com/> .
ex:subject ex:predicate ex:object .
"#;
    file.write_all(content.as_bytes())?;
    file.flush()?;

    let report = validate_turtle(file.path())?;
    assert!(report.is_valid);
    Ok(())
}

#[test]
fn test_validate_invalid_turtle() -> Result<()> {
    let mut file = NamedTempFile::new()?;
    let content = "this is not valid turtle !!!";
    file.write_all(content.as_bytes())?;
    file.flush()?;

    let report = validate_turtle(file.path())?;
    assert!(!report.is_valid);
    assert!(!report.errors.is_empty());
    Ok(())
}

#[test]
fn test_validate_valid_sparql_query() -> Result<()> {
    let query = "SELECT ?s WHERE { ?s ?p ?o }";
    validate_sparql_query(query)?;
    Ok(())
}

#[test]
fn test_validate_invalid_sparql_query() -> Result<()> {
    let query = "SELECT ? WHERE { invalid syntax }";
    let result = validate_sparql_query(query);
    assert!(result.is_err());
    Ok(())
}
```

---

## Testing & Verification

### Test Execution Order

1. **Compile check** (before any changes)
```bash
cargo make check -p ggen-ontology-core
```

2. **Unit tests** (verify changes don't break functionality)
```bash
cargo test -p ggen-ontology-core --lib
```

3. **Integration tests** (verify with security tests)
```bash
cargo test -p ggen-ontology-core --test ontology_integration
cargo test -p ggen-ontology-core --test security_
```

4. **Linting** (verify Poka-Yoke compliance)
```bash
cargo make lint -p ggen-ontology-core
```

5. **Full test suite**
```bash
cargo make test -p ggen-ontology-core
```

### Verification Checklist

After each fix:
- [ ] Code compiles without errors
- [ ] Related tests pass
- [ ] No clippy warnings
- [ ] Security tests pass (30/30)

---

## Risk Mitigation

### Risk 1: Breaking Existing Tests
**Mitigation**: Convert tests to `Result<()>` return type - Rust will help catch incompatibilities

### Risk 2: Changing Default Behavior
**Mitigation**: Verify no code calls `TripleStore::default()` before removing impl

### Risk 3: Test Coverage Loss
**Mitigation**: Result<()> tests provide same coverage with better error propagation

---

## Success Criteria

✅ **APPROVAL CHECKLIST**:

- [ ] All 53 unwrap/expect calls replaced
- [ ] `cargo make check -p ggen-ontology-core` passes
- [ ] `cargo make test -p ggen-ontology-core` passes
- [ ] `cargo make lint -p ggen-ontology-core` has no unwrap/expect warnings
- [ ] All 30 security tests pass
- [ ] Code review approval

---

## Rollback Plan

If issues arise:

1. **Review specific failures**:
```bash
cargo test -p ggen-ontology-core -- --nocapture --test-threads=1
```

2. **Revert changes**:
```bash
git checkout crates/ggen-ontology-core/src/
```

3. **Investigate root cause** with previous audit findings

4. **Re-apply fixes** with adjusted approach

---

## Timeline

```
Phase 1: entity_mapper.rs fixes        (15 min)
Phase 2: triple_store.rs fixes         (45 min)
Phase 3: validators.rs fixes           (30 min)
Phase 4: Test & verify all changes     (30 min)
─────────────────────────────────────────────
Total Estimated Time:                  ~2 hours
```

---

## Files to Modify

1. `crates/ggen-ontology-core/src/entity_mapper.rs` - 2 fixes + optional test cleanup
2. `crates/ggen-ontology-core/src/triple_store.rs` - 1 + 22 fixes
3. `crates/ggen-ontology-core/src/validators.rs` - 15 test fixes

**No new files needed** - all fixes are in-place modifications.

---

## Approval Process

1. ✓ Security audit identifies issues
2. **→ Implement remediation** (you are here)
3. ✓ Run full test suite
4. ✓ Code review
5. ✓ Final approval

---

**Remediation Date**: 2026-01-19
**Status**: READY TO IMPLEMENT
**Expected Completion**: 2-3 hours from start
**Next Step**: Begin with Step 1 (entity_mapper.rs fixes)
