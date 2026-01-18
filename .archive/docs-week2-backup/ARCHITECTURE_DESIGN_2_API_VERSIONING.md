<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Week 2 Architecture Design 2: API Versioning System](#week-2-architecture-design-2-api-versioning-system)
  - [Executive Summary](#executive-summary)
  - [Current State Analysis](#current-state-analysis)
    - [Existing Issues](#existing-issues)
    - [Impact Assessment](#impact-assessment)
  - [Proposed Architecture](#proposed-architecture)
    - [1. Semantic Versioning Scheme](#1-semantic-versioning-scheme)
    - [2. Deprecation Attribute Pattern](#2-deprecation-attribute-pattern)
    - [3. API Evolution Patterns](#3-api-evolution-patterns)
      - [Pattern 1: Field Addition (MINOR version bump)](#pattern-1-field-addition-minor-version-bump)
      - [Pattern 2: Field Removal (MAJOR version bump + deprecation)](#pattern-2-field-removal-major-version-bump--deprecation)
      - [Pattern 3: Method Signature Change (MAJOR bump + new method)](#pattern-3-method-signature-change-major-bump--new-method)
    - [4. `cargo semver-checks` Integration](#4-cargo-semver-checks-integration)
  - [Migration Documentation Pattern](#migration-documentation-pattern)
    - [CHANGELOG.md Structure](#changelogmd-structure)
      - [`process_package` signature change](#process_package-signature-change)
    - [Added](#added)
    - [Deprecated](#deprecated)
  - [&#91;3.4.0&#93; - 2025-11-25](#340---2025-11-25)
    - [Deprecated](#deprecated-1)
    - [Added](#added-1)
    - [Phase 2: CI Integration (Week 2, Day 3)](#phase-2-ci-integration-week-2-day-3)
    - [Phase 3: Test Migration (Week 2, Days 4-5)](#phase-3-test-migration-week-2-days-4-5)
    - [Phase 4: Documentation (Week 2, Day 5)](#phase-4-documentation-week-2-day-5)
  - [Success Criteria](#success-criteria)
    - [Functional Requirements](#functional-requirements)
    - [Non-Functional Requirements](#non-functional-requirements)
    - [Quality Gates](#quality-gates)
  - [Benefits Analysis](#benefits-analysis)
    - [Defect Prevention](#defect-prevention)
    - [Time Savings](#time-savings)
  - [Risk Analysis](#risk-analysis)
  - [ADR: API Versioning Strategy](#adr-api-versioning-strategy)
  - [Effort Estimates](#effort-estimates)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Week 2 Architecture Design 2: API Versioning System

**Design Phase:** Reduce Waste - API Breaking Change Prevention
**Priority:** HIGH (prevents test failures from API changes)
**Effort Estimate:** 2 hours design + 3 hours implementation
**Target SLO:** 0 undocumented breaking changes, <30s semver validation

---

## Executive Summary

**Problem:** API changes break tests. No versioning system. Current: 158 compiler errors, 27+ test failures from API mismatches.

**Solution:** Semantic versioning with deprecation attributes + `cargo semver-checks` CI integration to detect breaking changes before merge.

**Benefits:**
- ✅ Zero undocumented breaking changes
- ✅ Graceful API evolution with deprecation warnings
- ✅ Automated breaking change detection in CI
- ✅ Clear migration paths for API consumers

---

## Current State Analysis

### Existing Issues

**Compiler Errors from API Changes:**
```rust
error[E0609]: no field `manifest` on type `ggen_marketplace::Package`
   --> crates/ggen-marketplace/tests/integration/marketplace_lifecycle_test.rs:356:24
    |
356 |     assert_eq!(top_pkg.manifest.dependencies.len(), 1);
    |                        ^^^^^^^^ unknown field
    |
    = note: available fields are: `metadata`, `latest_version`, `versions`, `releases`
```

**Root Cause:** Package struct changed without:
1. Deprecation warnings
2. Version bump
3. Migration documentation
4. Test updates

### Impact Assessment

**Current State (Week 1 baseline):**
- 158 compiler errors across 6 crates
- 27+ test failures from API mismatches
- No deprecation strategy
- No semver enforcement
- Breaking changes merged without detection

**Waste Generated:**
- **Time:** ~4 hours debugging type mismatches
- **Defects:** Silent API breakages in production
- **Rework:** Repeated fixes for same API issues

---

## Proposed Architecture

### 1. Semantic Versioning Scheme

**Version Format:** `MAJOR.MINOR.PATCH`

- **MAJOR:** Breaking changes (incompatible API)
- **MINOR:** Backward-compatible features
- **PATCH:** Backward-compatible bug fixes

**Example:**
```
3.3.0 → 3.4.0  (add new fields to Package - backward compatible)
3.3.0 → 4.0.0  (remove Package.manifest - breaking change)
```

### 2. Deprecation Attribute Pattern

**Before (breaking change without warning):**
```rust
// v3.3.0
pub struct Package {
    pub manifest: PackageManifest,  // Removed in v3.4.0
}

// v3.4.0 - BREAKING CHANGE, no warning
pub struct Package {
    pub metadata: PackageMetadata,  // New field, different type
}
```

**After (graceful deprecation):**
```rust
// v3.3.0
pub struct Package {
    pub manifest: PackageManifest,
}

// v3.4.0 - Add new field, deprecate old
pub struct Package {
    #[deprecated(since = "3.4.0", note = "Use `metadata` instead. `manifest` will be removed in 4.0.0")]
    pub manifest: PackageManifest,

    /// New unified metadata structure (replaces manifest)
    pub metadata: PackageMetadata,
}

impl Package {
    /// Helper method for migration
    #[deprecated(since = "3.4.0", note = "Access metadata directly")]
    pub fn manifest(&self) -> &PackageManifest {
        &self.manifest
    }
}

// v4.0.0 - Remove deprecated field (MAJOR bump)
pub struct Package {
    pub metadata: PackageMetadata,
}
```

### 3. API Evolution Patterns

#### Pattern 1: Field Addition (MINOR version bump)

```rust
// v3.3.0
pub struct Package {
    pub name: String,
    pub version: String,
}

// v3.4.0 - Add optional field (non-breaking)
pub struct Package {
    pub name: String,
    pub version: String,
    /// New field: optional for backward compatibility
    pub license: Option<String>,  // ✅ Non-breaking
}
```

#### Pattern 2: Field Removal (MAJOR version bump + deprecation)

```rust
// v3.3.0
pub struct Package {
    pub manifest: PackageManifest,
}

// v3.4.0 - Deprecate field (MINOR bump)
pub struct Package {
    #[deprecated(since = "3.4.0", note = "Use metadata. Removed in 4.0.0")]
    pub manifest: PackageManifest,
    pub metadata: PackageMetadata,
}

// v4.0.0 - Remove field (MAJOR bump)
pub struct Package {
    pub metadata: PackageMetadata,
}
```

#### Pattern 3: Method Signature Change (MAJOR bump + new method)

```rust
// v3.3.0
pub fn process_package(pkg: &Package) -> Result<Output, Error> {
    // ...
}

// v3.4.0 - Add new method, deprecate old (MINOR bump)
#[deprecated(since = "3.4.0", note = "Use process_package_v2 instead")]
pub fn process_package(pkg: &Package) -> Result<Output, Error> {
    process_package_v2(pkg, ProcessOptions::default())
}

pub fn process_package_v2(
    pkg: &Package,
    options: ProcessOptions,
) -> Result<Output, Error> {
    // New implementation
}

// v4.0.0 - Remove old method, rename new (MAJOR bump)
pub fn process_package(
    pkg: &Package,
    options: ProcessOptions,
) -> Result<Output, Error> {
    // Renamed from process_package_v2
}
```

### 4. `cargo semver-checks` Integration

**Installation:**
```bash
cargo install cargo-semver-checks --locked
```

**Usage in CI:**
```yaml
# .github/workflows/quality-gates.yml
semver-check:
  name: Semver Compatibility Check
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
      with:
        fetch-depth: 0  # Need full history for comparison

    - name: Setup Rust
      uses: dtolnay/rust-toolchain@stable

    - name: Install cargo-semver-checks
      run: cargo install cargo-semver-checks --locked

    - name: Check for breaking changes
      run: |
        # Compare current branch against master
        cargo semver-checks check-release --baseline-version 3.3.0

    - name: Fail on breaking changes in MINOR/PATCH versions
      run: |
        # Only allow breaking changes if MAJOR version bumped
        CURRENT_VERSION=$(grep '^version = ' Cargo.toml | head -1 | sed 's/.*"\(.*\)".*/\1/')
        BASELINE_VERSION="3.3.0"

        # Extract major versions
        CURRENT_MAJOR=$(echo $CURRENT_VERSION | cut -d. -f1)
        BASELINE_MAJOR=$(echo $BASELINE_VERSION | cut -d. -f1)

        if [ "$CURRENT_MAJOR" == "$BASELINE_MAJOR" ]; then
          # Same major version - breaking changes not allowed
          if cargo semver-checks check-release --baseline-version $BASELINE_VERSION 2>&1 | grep -q "breaking"; then
            echo "❌ Breaking changes detected in MINOR/PATCH version"
            echo "   Bump MAJOR version or use deprecation pattern"
            exit 1
          fi
        else
          echo "✅ MAJOR version bump detected - breaking changes allowed"
        fi
```

**Makefile.toml Integration:**
```toml
[tasks.semver-check]
description = "Check for breaking changes against baseline version"
workspace = false
command = "cargo"
args = ["semver-checks", "check-release", "--baseline-version", "3.3.0"]

[tasks.release-validate-breaking-changes]
description = "Detect breaking changes requiring documentation (FMEA: prevents undocumented breaking changes, RPN 240)"
workspace = false
dependencies = ["semver-check"]
script = '''
#!/bin/bash
# Verify breaking changes documented in CHANGELOG.md
root_version=$(grep '^version = ' Cargo.toml | head -1 | sed 's/.*version = "\(.*\)"/\1/' | tr -d '"')

if cargo semver-checks check-release --baseline-version 3.3.0 2>&1 | grep -q "breaking"; then
  echo "⚠️  Breaking changes detected - verifying CHANGELOG.md..."

  if ! grep -q "## \[$root_version\].*BREAKING" CHANGELOG.md; then
    echo "❌ ERROR: Breaking changes not documented in CHANGELOG.md"
    echo "   Add '## [$root_version] - BREAKING CHANGES' section with migration guide"
    exit 1
  fi

  echo "✅ Breaking changes documented in CHANGELOG.md"
fi
'''

[tasks.pre-commit]
dependencies = [
    "timeout-check",
    "fmt",
    "lint",
    "semver-check",  # NEW: Catch breaking changes before commit
    "test",
    "test-doc",
]
```

---

## Migration Documentation Pattern

### CHANGELOG.md Structure

```markdown
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [4.0.0] - 2025-11-30 - BREAKING CHANGES

### Breaking Changes

#### `Package` struct field removal
- **Removed:** `Package.manifest: PackageManifest`
- **Replaced with:** `Package.metadata: PackageMetadata`
- **Migration:**
  ```rust
  // Before (v3.x)
  let deps = package.manifest.dependencies;

  // After (v4.0)
  let deps = package.metadata.dependencies;
  ```

#### `process_package` signature change
- **Old:** `fn process_package(pkg: &Package) -> Result<Output>`
- **New:** `fn process_package(pkg: &Package, options: ProcessOptions) -> Result<Output>`
- **Migration:**
  ```rust
  // Before (v3.x)
  let output = process_package(&pkg)?;

  // After (v4.0)
  let output = process_package(&pkg, ProcessOptions::default())?;
  ```

### Added
- `PackageMetadata` unified structure (replaces `PackageManifest`)
- `ProcessOptions` for configurable package processing

### Deprecated
- None (deprecated APIs removed in this major version)

## [3.4.0] - 2025-11-25

### Deprecated
- `Package.manifest` - Use `Package.metadata` instead (removed in 4.0.0)
- `process_package(pkg)` - Use `process_package_v2(pkg, options)` instead

### Added
- `Package.metadata` field (replaces `manifest`)
- `process_package_v2` with configurable options
```

---

## Rollout Strategy

### Phase 1: Add Deprecation Attributes (Week 2, Days 1-2)

**Files to modify:**
```
crates/ggen-marketplace/src/lib.rs
crates/ggen-marketplace/src/package.rs
crates/ggen-domain/src/marketplace/*.rs
```

**Actions:**
1. Identify all changed APIs (use git diff + semver-checks)
2. Add `#[deprecated]` attributes with migration notes
3. Add new APIs alongside old (parallel implementation)
4. Update CHANGELOG.md with deprecation notices

**Validation:**
```bash
cargo make lint  # Should show deprecation warnings
cargo make test  # Should still pass (backward compatibility)
```

### Phase 2: CI Integration (Week 2, Day 3)

**Files to modify:**
```
.github/workflows/quality-gates.yml
Makefile.toml
```

**Actions:**
1. Add `semver-check` task to Makefile.toml
2. Add `semver-check` job to quality-gates.yml
3. Add `pre-commit` dependency on `semver-check`

**Validation:**
```bash
cargo make pre-commit  # Should detect breaking changes
gh workflow run quality-gates.yml  # Should pass with warnings
```

### Phase 3: Test Migration (Week 2, Days 4-5)

**Files to modify:**
```
crates/ggen-marketplace/tests/integration/*.rs
crates/ggen-marketplace/tests/unit/*.rs
```

**Actions:**
1. Update tests to use new APIs
2. Suppress deprecation warnings in tests (temporary)
3. Add migration tests (both old and new APIs work)

**Validation:**
```bash
cargo make test  # All tests pass with new APIs
cargo make test-doc  # Doctests use new APIs
```

### Phase 4: Documentation (Week 2, Day 5)

**Files to modify:**
```
CHANGELOG.md
docs/MIGRATION_GUIDE.md
docs/API_VERSIONING.md
```

**Actions:**
1. Document all breaking changes in CHANGELOG.md
2. Create migration guide with before/after examples
3. Document API versioning policy

---

## Success Criteria

### Functional Requirements

- ✅ All changed APIs have `#[deprecated]` attributes
- ✅ CHANGELOG.md documents all breaking changes
- ✅ Migration guide provides clear before/after examples
- ✅ `cargo semver-checks` detects breaking changes in CI

### Non-Functional Requirements

- ✅ CI pipeline blocks PRs with undocumented breaking changes
- ✅ Semver check completes in <30s
- ✅ Deprecation warnings visible in builds
- ✅ 100% test coverage for migration paths

### Quality Gates

- ✅ `cargo make semver-check` - No undocumented breaking changes
- ✅ `cargo make lint` - Shows deprecation warnings
- ✅ `cargo make test` - All tests pass with new APIs
- ✅ `cargo make release-validate` - Includes semver validation

---

## Benefits Analysis

### Defect Prevention

| Defect Type | Before | After | Improvement |
|-------------|--------|-------|-------------|
| Silent API breakages | 27+ per release | 0 | **100% reduction** |
| Undocumented migrations | 100% | 0% | **100% reduction** |
| Test failures from API changes | 158 errors | 0 errors | **100% reduction** |

### Time Savings

| Activity | Before | After | Savings |
|----------|--------|-------|---------|
| Debugging type mismatches | 4h per release | 0h | **4h** |
| Writing migration docs | 0h (never done) | 1h (automated) | **3h saved** |
| Fixing broken tests | 2h per release | 0h | **2h** |
| **Total** | **6h** | **1h** | **5h per release** |

---

## Risk Analysis

| Risk | Mitigation | RPN (Before) | RPN (After) |
|------|------------|--------------|-------------|
| Breaking changes merged without detection | CI semver-checks block PRs | 504 | 50 |
| Tests break from API changes | Deprecation warnings + parallel APIs | 360 | 36 |
| No migration documentation | CHANGELOG.md validation in CI | 288 | 29 |
| Consumers upgrade blindly | Semantic versioning + clear deprecation | 240 | 24 |

---

## ADR: API Versioning Strategy

**Status:** Proposed
**Context:** 158 compiler errors from undocumented API changes
**Decision:** Implement semantic versioning + cargo semver-checks CI integration

**Rationale:**
1. **Prevention**: Catch breaking changes before merge
2. **Communication**: Clear deprecation warnings guide migrations
3. **Automation**: semver-checks enforces policy in CI
4. **Standards**: Industry-standard semantic versioning

**Consequences:**
- **Positive**: Zero undocumented breaking changes, clear migration paths
- **Negative**: Slightly longer CI time (+30s for semver-checks)
- **Neutral**: Requires discipline in version bumps

**Alternatives Considered:**
1. **Manual review** - Error-prone, doesn't scale
2. **No versioning** - Chaotic, unpredictable breakages
3. **Strict backward compatibility** - Limits evolution, accumulates technical debt

---

## Effort Estimates

| Task | Hours | Confidence |
|------|-------|-----------|
| Add deprecation attributes | 2h | High |
| CI integration (semver-checks) | 1h | High |
| Test migration | 2h | Medium |
| Documentation (CHANGELOG, migration guide) | 2h | High |
| **Total** | **7h** | **High** |

---

## Next Steps

1. **Approval**: Team review of versioning policy
2. **Prototyping**: semver-checks spike on one crate (1 hour)
3. **Implementation**: Phase 1-4 rollout
4. **Validation**: Run full CI with semver-checks enabled
5. **Documentation**: Publish API versioning policy

---

**Architecture Owner:** System Architect
**Design Date:** 2025-11-20
**Review Status:** Pending Team Approval
**Target Completion:** Week 2, Day 5
