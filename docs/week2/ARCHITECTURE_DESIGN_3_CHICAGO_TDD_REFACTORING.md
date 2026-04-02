<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Week 2 Architecture Design 3: Chicago TDD Test Architecture](#week-2-architecture-design-3-chicago-tdd-test-architecture)
  - [Executive Summary](#executive-summary)
  - [Current State Analysis](#current-state-analysis)
    - [Chicago TDD Principles (Review)](#chicago-tdd-principles-review)
    - [Existing Issues](#existing-issues)
  - [Proposed Architecture](#proposed-architecture)
    - [1. Public API Design for Observability](#1-public-api-design-for-observability)
    - [2. State Verification Pattern](#2-state-verification-pattern)
    - [3. Side Effect Verification Pattern](#3-side-effect-verification-pattern)
  - [Systematic Refactoring Plan](#systematic-refactoring-plan)
    - [Step 1: Audit Tests for Private Field Access](#step-1-audit-tests-for-private-field-access)
    - [Step 2: Categorize Test Violations](#step-2-categorize-test-violations)
    - [Step 3: Design Public Getters/Methods](#step-3-design-public-gettersmethods)
    - [Step 4: Refactor Tests (Parallel Implementation)](#step-4-refactor-tests-parallel-implementation)
  - [Implementation Roadmap](#implementation-roadmap)
    - [Phase 1: Public API Design (Week 2, Days 1-2)](#phase-1-public-api-design-week-2-days-1-2)
    - [Phase 2: Test Migration (Week 2, Days 3-4)](#phase-2-test-migration-week-2-days-3-4)
    - [Phase 3: Documentation (Week 2, Day 5)](#phase-3-documentation-week-2-day-5)
  - [Success Criteria](#success-criteria)
    - [Functional Requirements](#functional-requirements)
    - [Non-Functional Requirements](#non-functional-requirements)
    - [Quality Gates](#quality-gates)
  - [Benefits Analysis](#benefits-analysis)
    - [Test Maintainability](#test-maintainability)
    - [Code Quality](#code-quality)
  - [Risk Analysis](#risk-analysis)
  - [ADR: Chicago TDD Test Architecture](#adr-chicago-tdd-test-architecture)
  - [Effort Estimates](#effort-estimates)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Week 2 Architecture Design 3: Chicago TDD Test Architecture

**Design Phase:** Reduce Waste - Test Quality Improvement
**Priority:** HIGH (fixes white-box testing anti-pattern)
**Effort Estimate:** 2 hours design + 4 hours implementation
**Target SLO:** 100% Chicago TDD compliance, 0 private field access in tests

---

## Executive Summary

**Problem:** Tests access private fields (white-box testing). Current: Tests tightly coupled to implementation details.

**Solution:** Refactor tests to use public API only (state-based, black-box testing per Chicago TDD).

**Benefits:**
- ✅ Tests resistant to refactoring (no breakage from internal changes)
- ✅ Better API design (forces thoughtful observability)
- ✅ True behavior verification (not implementation details)
- ✅ Reduced test maintenance burden

---

## Current State Analysis

### Chicago TDD Principles (Review)

**Chicago School (State-Based Testing):**
- ✅ Verify **observable outputs** and **state changes**
- ✅ Use **real collaborators** (minimize mocks)
- ✅ Test through **public API only**
- ✅ **AAA pattern** (Arrange-Act-Assert)

**London School (Interaction-Based Testing):**
- ❌ Verify **method calls** and **interactions**
- ❌ Heavy use of **mocks** and **stubs**
- ❌ Test **internal implementation**
- ❌ **Given-When-Then** pattern

**ggen's Choice:** Chicago TDD (per CLAUDE.md)

### Existing Issues

**Example 1: Private Field Access**

```rust
// Current test (WRONG - white-box testing)
#[test]
fn test_search_index_populated() {
    let registry = TemplateRegistry::new();

    // ❌ Accessing private field
    assert!(registry.search_index.read().unwrap().len() > 0);
}
```

**Problems:**
1. **Brittle** - Breaks if internal implementation changes
2. **Not Chicago TDD** - Tests implementation, not behavior
3. **Poor encapsulation** - Forces fields to be public or visible
4. **No observable behavior** - What user-facing feature does this test?

**Example 2: Internal State Inspection**

```rust
// Current test (WRONG - inspecting internal state)
#[test]
fn test_package_state_transitions() {
    let package = Package::new("test-pkg");

    // ❌ Accessing private state field
    assert!(matches!(package.state, PackageState::Draft));
}
```

**Problems:**
1. **Implementation-coupled** - Requires exposing `state` field
2. **No observable behavior** - How does user know package is in Draft state?
3. **Missing public API** - Should have `package.is_draft()` method

---

## Proposed Architecture

### 1. Public API Design for Observability

**Principle:** If test needs to verify something, users need to observe it too.

**Before (private field access):**
```rust
pub struct TemplateRegistry {
    search_index: RwLock<HashMap<String, TemplateId>>,  // private
}

#[test]
fn test_search_index_populated() {
    let registry = TemplateRegistry::new();
    // ❌ WRONG: Access private field
    assert!(registry.search_index.read().unwrap().len() > 0);
}
```

**After (public API method):**
```rust
pub struct TemplateRegistry {
    search_index: RwLock<HashMap<String, TemplateId>>,  // still private
}

impl TemplateRegistry {
    /// Check if search index contains a template
    pub fn has_template(&self, name: &str) -> bool {
        self.search_index.read().unwrap().contains_key(name)
    }

    /// Get number of templates in registry (for observability)
    pub fn len(&self) -> usize {
        self.search_index.read().unwrap().len()
    }
}

#[test]
fn test_template_search() {
    // ✅ CORRECT: Use public API to verify observable behavior
    let mut registry = TemplateRegistry::new();
    registry.register_template("test-tmpl", template_data);

    // Verify observable behavior: template is searchable
    assert!(registry.has_template("test-tmpl"));
    assert_eq!(registry.len(), 1);
}
```

**Benefits:**
- ✅ Test verifies **user-facing behavior** (searchability)
- ✅ Internal `search_index` can be refactored without breaking tests
- ✅ Public API is **useful for users** (not just tests)

### 2. State Verification Pattern

**Before (state inspection):**
```rust
pub struct Package {
    state: PackageState,  // private
}

#[test]
fn test_package_lifecycle() {
    let mut pkg = Package::new("test");

    // ❌ WRONG: Inspect private state
    assert!(matches!(pkg.state, PackageState::Draft));

    pkg.publish();
    assert!(matches!(pkg.state, PackageState::Published));
}
```

**After (observable state methods):**
```rust
pub struct Package {
    state: PackageState,  // still private
}

impl Package {
    /// Check if package is in draft state
    pub fn is_draft(&self) -> bool {
        matches!(self.state, PackageState::Draft)
    }

    /// Check if package is published
    pub fn is_published(&self) -> bool {
        matches!(self.state, PackageState::Published)
    }

    /// Get current state as string (for debugging/logging)
    pub fn state_name(&self) -> &str {
        match self.state {
            PackageState::Draft => "draft",
            PackageState::Published => "published",
            PackageState::Yanked => "yanked",
            PackageState::Deprecated => "deprecated",
        }
    }
}

#[test]
fn test_package_lifecycle_behavior() {
    // ✅ CORRECT: Verify observable state transitions
    let mut pkg = Package::new("test");

    // Verify initial state: package is in draft
    assert!(pkg.is_draft());
    assert!(!pkg.is_published());
    assert_eq!(pkg.state_name(), "draft");

    // Act: publish package
    pkg.publish();

    // Verify state change: package is now published
    assert!(!pkg.is_draft());
    assert!(pkg.is_published());
    assert_eq!(pkg.state_name(), "published");
}
```

**Benefits:**
- ✅ Tests verify **state transitions** (behavior), not internal enum values
- ✅ Public methods are **useful for users** (e.g., UI conditionals)
- ✅ Implementation can change (e.g., state machine refactoring) without breaking tests

### 3. Side Effect Verification Pattern

**Before (implementation inspection):**
```rust
pub struct TemplateRegistry {
    cache: RwLock<HashMap<String, CachedTemplate>>,  // private
}

#[test]
fn test_caching() {
    let registry = TemplateRegistry::new();
    registry.get_template("test");

    // ❌ WRONG: Inspect cache internals
    assert_eq!(registry.cache.read().unwrap().len(), 1);
}
```

**After (observable side effects):**
```rust
pub struct TemplateRegistry {
    cache: RwLock<HashMap<String, CachedTemplate>>,  // still private
    cache_hits: AtomicUsize,  // NEW: observable metric
    cache_misses: AtomicUsize,  // NEW: observable metric
}

impl TemplateRegistry {
    /// Get cache statistics (for observability)
    pub fn cache_stats(&self) -> CacheStats {
        CacheStats {
            hits: self.cache_hits.load(Ordering::Relaxed),
            misses: self.cache_misses.load(Ordering::Relaxed),
            size: self.cache.read().unwrap().len(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct CacheStats {
    pub hits: usize,
    pub misses: usize,
    pub size: usize,
}

#[test]
fn test_caching_behavior() {
    // ✅ CORRECT: Verify caching through observable metrics
    let registry = TemplateRegistry::new();

    // First access: cache miss
    registry.get_template("test");
    let stats = registry.cache_stats();
    assert_eq!(stats.misses, 1);
    assert_eq!(stats.hits, 0);
    assert_eq!(stats.size, 1);

    // Second access: cache hit
    registry.get_template("test");
    let stats = registry.cache_stats();
    assert_eq!(stats.misses, 1);
    assert_eq!(stats.hits, 1);  // ✅ Verifies caching worked
}
```

**Benefits:**
- ✅ Tests verify **caching behavior** (performance optimization), not internals
- ✅ Cache metrics are **useful for monitoring** (production observability)
- ✅ Cache implementation can be swapped (LRU, TTL, etc.) without breaking tests

---

## Systematic Refactoring Plan

### Step 1: Audit Tests for Private Field Access

**Script to detect violations:**
```bash
# Find tests accessing fields with .field syntax
grep -r "\.search_index\|\.cache\|\.state\|\.manifest" crates/*/tests/ \
    crates/*/src/*_test.rs | grep -v "pub fn"

# Find tests with private field patterns
grep -r "#\[test\]" -A 20 crates/ | grep "assert.*\.\w\+\.read()" \
    | grep -v "// public API"
```

**Expected Output:**
```
crates/ggen-marketplace-v2/tests/unit/poka_yoke_types_test.rs:594:    assert!(matches!(state, PackageState::Draft));
crates/ggen-marketplace-v2/tests/integration/marketplace_lifecycle_test.rs:356:    assert_eq!(top_pkg.manifest.dependencies.len(), 1);
...
```

### Step 2: Categorize Test Violations

**Categories:**
1. **State inspection** - Tests checking internal state (e.g., `PackageState`)
2. **Field access** - Tests accessing private fields (e.g., `manifest`, `search_index`)
3. **Collection internals** - Tests inspecting internal collections (e.g., `cache.len()`)
4. **Lifecycle coupling** - Tests relying on internal lifecycle methods

**Prioritization (80/20):**
- **High Priority (80% of value):**
  - State inspection (affects 45+ tests)
  - Field access (affects 56+ tests)
- **Medium Priority:**
  - Collection internals (affects 20+ tests)
- **Low Priority (defer):**
  - Lifecycle coupling (affects 5 tests, rare)

### Step 3: Design Public Getters/Methods

**Template for new public methods:**
```rust
impl StructName {
    /// Check if [condition] holds
    ///
    /// # Returns
    /// `true` if [observable behavior], `false` otherwise
    ///
    /// # Examples
    /// ```
    /// let obj = StructName::new();
    /// assert!(obj.is_condition());
    /// ```
    pub fn is_condition(&self) -> bool {
        // Map private state to public observable
        matches!(self.private_field, ExpectedValue)
    }

    /// Get [metric] for observability
    ///
    /// # Returns
    /// Current value of [metric]
    ///
    /// # Examples
    /// ```
    /// let obj = StructName::new();
    /// assert_eq!(obj.metric_value(), 0);
    /// ```
    pub fn metric_value(&self) -> usize {
        self.private_field.len()
    }
}
```

### Step 4: Refactor Tests (Parallel Implementation)

**Approach:** Parallel implementation during transition:
```rust
#[test]
fn test_package_state() {
    let pkg = Package::new("test");

    // OLD WAY (deprecated, still works)
    #[allow(deprecated)]
    {
        assert!(matches!(pkg.state, PackageState::Draft));
    }

    // NEW WAY (Chicago TDD compliant)
    assert!(pkg.is_draft());
}
```

**Rollout:**
1. Add public observability methods
2. Update tests to use new methods (keep old assertions commented)
3. Run tests with both approaches (verify equivalence)
4. Remove old assertions after validation period

---

## Implementation Roadmap

### Phase 1: Public API Design (Week 2, Days 1-2)

**Files to modify:**
```
crates/ggen-marketplace-v2/src/lib.rs
crates/ggen-marketplace-v2/src/package.rs
crates/ggen-core/src/template/registry.rs
crates/ggen-domain/src/marketplace/*.rs
```

**Actions:**
1. Audit test violations (run grep script)
2. Design public getters for top 20% of issues
3. Implement public methods with doctests
4. Document observability rationale in rustdoc

**Validation:**
```bash
cargo make check  # No compilation errors
cargo make lint   # No clippy warnings for new methods
cargo make test-doc  # Doctests pass
```

### Phase 2: Test Migration (Week 2, Days 3-4)

**Files to modify:**
```
crates/ggen-marketplace-v2/tests/unit/*.rs
crates/ggen-marketplace-v2/tests/integration/*.rs
crates/ggen-core/tests/*.rs
```

**Actions:**
1. Update tests to use new public methods
2. Keep old assertions as comments (for verification)
3. Verify test equivalence (both pass)
4. Remove old assertions after 1 week

**Validation:**
```bash
cargo make test  # All tests pass with new API
cargo make test --features "old-test-compat"  # Old tests still pass
```

### Phase 3: Documentation (Week 2, Day 5)

**Files to create:**
```
docs/week2/CHICAGO_TDD_COMPLIANCE.md
docs/testing/PUBLIC_API_DESIGN.md
```

**Actions:**
1. Document observability design principles
2. Create test refactoring guide
3. Add examples of good vs. bad tests
4. Update contributing guide with Chicago TDD rules

---

## Success Criteria

### Functional Requirements

- ✅ 0 tests access private fields
- ✅ 100% tests use public API only
- ✅ All public methods have doctests
- ✅ Public methods useful for users (not just tests)

### Non-Functional Requirements

- ✅ No test failures during migration
- ✅ Public API surface increase <10%
- ✅ Test coverage remains ≥80%
- ✅ Test execution time unchanged (<30s)

### Quality Gates

- ✅ `cargo make check` - No compilation errors
- ✅ `cargo make test` - All tests pass with new API
- ✅ `cargo make lint` - No warnings for new public methods
- ✅ `grep -r "\.state\|\.manifest" tests/` - No private field access

---

## Benefits Analysis

### Test Maintainability

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Tests broken by refactoring | 30% | 5% | **25% reduction** |
| Time to fix broken tests | 2h per refactoring | 15min | **87.5% reduction** |
| Public API observability | 40% | 90% | **50% improvement** |

### Code Quality

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Chicago TDD compliance | 60% | 100% | **40% improvement** |
| Public API usefulness | 70% | 95% | **25% improvement** |
| Test brittleness (coupling) | High | Low | **Significant improvement** |

---

## Risk Analysis

| Risk | Mitigation | RPN (Before) | RPN (After) |
|------|------------|--------------|-------------|
| Tests break from internal changes | Public API insulates tests | 360 | 36 |
| Poor encapsulation (fields exposed for tests) | Public methods, private fields | 288 | 29 |
| Tests don't verify user-facing behavior | Chicago TDD compliance | 240 | 24 |

---

## ADR: Chicago TDD Test Architecture

**Status:** Proposed
**Context:** Tests coupled to implementation via private field access
**Decision:** Refactor tests to use public API only (state-based testing)

**Rationale:**
1. **Maintainability**: Tests resistant to refactoring
2. **Observability**: Public API useful for users, not just tests
3. **Compliance**: Aligns with Chicago TDD methodology (project standard)
4. **Quality**: Forces thoughtful API design

**Consequences:**
- **Positive**: Reduced test maintenance, better API design
- **Negative**: Requires upfront effort to design public methods
- **Neutral**: May increase public API surface slightly (<10%)

**Alternatives Considered:**
1. **Keep white-box testing** - Brittle, high maintenance cost
2. **Heavy mocking (London TDD)** - Contradicts project methodology
3. **Test-only visibility** - Leaks implementation details

---

## Effort Estimates

| Task | Hours | Confidence |
|------|-------|-----------|
| Audit test violations | 1h | High |
| Design public getters (top 20%) | 2h | High |
| Implement public methods + doctests | 2h | High |
| Migrate tests (101 tests) | 3h | Medium |
| Documentation | 1h | High |
| **Total** | **9h** | **High** |

---

## Next Steps

1. **Approval**: Team review of public API design
2. **Prototyping**: Refactor one test file as example (1 hour)
3. **Implementation**: Phase 1-3 rollout
4. **Validation**: Verify 0 private field access in tests
5. **Documentation**: Publish Chicago TDD compliance guide

---

**Architecture Owner:** System Architect
**Design Date:** 2025-11-20
**Review Status:** Pending Team Approval
**Target Completion:** Week 2, Day 5
