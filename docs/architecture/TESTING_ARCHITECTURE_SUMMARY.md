# Testing Architecture Summary - Quick Reference

**For**: Other Hive Mind Agents (Coder, Tester, Reviewer)
**Purpose**: Quick reference for implementing tests based on architectural standards
**Full Architecture**: See `TESTING_ARCHITECTURE.md`

---

## Quick Stats - Current Gap Analysis

| Metric | Current | Target | Action Needed |
|--------|---------|--------|---------------|
| **ggen-core** | 38 tests / 76 src | 91 tests | Generate 53 test files |
| **ggen-domain** | 14 tests / 52 src | 62 tests | Generate 48 test files |
| **ggen-cli** | Mixed | Complete | Add missing integration tests |
| **Chicago TDD** | ~20% | 100% | Migrate all tests |
| **Coverage** | ~49% | 90% | Add 500+ tests |

**Total Gap**: ~101 missing test files + 500+ individual tests

---

## Test File Structure (MUST FOLLOW)

```
crates/[crate]/
├── tests/
│   ├── unit/
│   │   └── [module]_tests.rs          # One per src module
│   ├── integration/
│   │   └── [feature]_integration.rs   # Full workflows
│   ├── property/
│   │   └── [module]_properties.rs     # Property-based tests
│   ├── security/
│   │   └── [concern]_security.rs      # Security tests
│   └── common/
│       └── mod.rs                      # Test config helpers
└── benches/
    └── [module]_benchmarks.rs          # Performance benchmarks
```

---

## Mandatory Test Patterns (Chicago TDD)

### ✅ CORRECT - Use These

```rust
use chicago_tdd_tools::prelude::*;

#[path = "../common/mod.rs"]
mod test_config;
use test_config::{integration_timeout, unit_timeout};

// Synchronous test
test!(test_function_with_valid_input_succeeds, {
    // Arrange
    let input = create_valid_input();

    // Act
    let result = function(input);

    // Assert
    assert_ok!(result);
});

// Async test (auto-timeout from config)
async_test!(test_async_function_completes, async {
    // Arrange
    let input = create_valid_input();

    // Act
    let result = async_function(input).await.unwrap();

    // Assert
    assert!(!result.is_empty());
});
```

### ❌ WRONG - Don't Use These

```rust
// ❌ Don't use standard Rust test attributes
#[test]
fn old_test() { }

#[tokio::test]
async fn old_async_test() { }

// ❌ Don't hardcode timeouts
async_test_with_timeout!(test_name, 30, async { });

// ❌ Don't return Result<()>
async fn test() -> Result<()> { Ok(()) }
```

---

## Test Coverage Requirements

| Crate | Min Coverage | Test Types Required |
|-------|--------------|---------------------|
| **ggen-domain** | 95% | Unit + Integration + Property |
| **ggen-core** | 90% | Unit + Integration + Security |
| **ggen-cli** | 85% | Unit + Integration + E2E |
| **ggen-marketplace** | 85% | Unit + Integration |
| **ggen-utils** | 80% | Unit + Property |

---

## Quick Commands

### Generate Test Skeleton

```bash
# Generate complete test skeleton for a module
./scripts/generate-test-skeleton.sh <module> <crate>

# Example
./scripts/generate-test-skeleton.sh cache ggen-core
```

**Generates**:
- Unit tests: `tests/unit/cache_tests.rs`
- Integration tests: `tests/integration/cache_integration.rs`
- Property tests: `tests/property/cache_properties.rs`
- Test checklist: `tests/MODULE_TEST_CHECKLIST_cache.md`

### Validate Coverage

```bash
# Validate specific crate
./scripts/validate-test-coverage.sh ggen-core

# Validate all crates (CI)
./scripts/validate-crate-coverage.sh

# Run tests with coverage
cargo tarpaulin --package ggen-core --out Html
```

---

## Test Naming Convention

**Pattern**: `test_[action]_[condition]_[expected_outcome]`

### Examples

```rust
test!(test_install_package_with_valid_name_succeeds, { });
test!(test_search_with_empty_query_returns_all_packages, { });
test!(test_parse_config_with_invalid_syntax_fails, { });
async_test!(test_fetch_data_from_api_completes, async { });
```

---

## Per-Module Test Requirements

### Minimum Tests Per Module

**Every module MUST have**:

1. **Unit Tests** (10 minimum):
   - Constructor/creation tests
   - Basic operation tests
   - Error handling tests
   - Edge case tests
   - Async operation tests (if applicable)

2. **Integration Tests** (5 minimum):
   - Full workflow tests
   - Cross-module interaction tests
   - API contract validation
   - Concurrent operation tests (if applicable)

3. **Property Tests** (3 minimum):
   - Round-trip consistency
   - Idempotency
   - Invariant validation

---

## Test Documentation Template

```rust
/// Tests that [action] with [condition] [expected outcome].
///
/// # Test Scenario
/// - Arrange: [Setup description]
/// - Act: [Action description]
/// - Assert: [Verification description]
///
/// # Chicago TDD
/// - Uses real [object type] (not mocked)
/// - Verifies final state ([what state])
/// - Uses config-based timeout
test!(test_name, {
    // Implementation
});
```

---

## Priority Modules to Test (80/20)

### High Priority (ggen-domain) - 95% target

**Critical 20% that provides 80% value**:

1. **marketplace/install.rs** - Package installation logic
2. **marketplace/search.rs** - Search functionality
3. **template/generate.rs** - Template generation
4. **ai/analyze.rs** - AI code analysis
5. **graph/query.rs** - SPARQL query execution

**Generate tests**:
```bash
./scripts/generate-test-skeleton.sh install ggen-domain
./scripts/generate-test-skeleton.sh search ggen-domain
./scripts/generate-test-skeleton.sh generate ggen-domain
./scripts/generate-test-skeleton.sh analyze ggen-domain
./scripts/generate-test-skeleton.sh query ggen-domain
```

### Medium Priority (ggen-core) - 90% target

**Core infrastructure**:

1. **cache** - Caching logic
2. **lifecycle** - Lifecycle management
3. **templates** - Template processing
4. **rdf** - RDF operations
5. **generator** - Code generation

**Generate tests**:
```bash
./scripts/generate-test-skeleton.sh cache ggen-core
./scripts/generate-test-skeleton.sh lifecycle ggen-core
./scripts/generate-test-skeleton.sh templates ggen-core
./scripts/generate-test-skeleton.sh rdf ggen-core
./scripts/generate-test-skeleton.sh generator ggen-core
```

---

## Common Test Utilities

### Test Config Helpers

```rust
#[path = "../common/mod.rs"]
mod test_config;
use test_config::{
    integration_timeout,    // Duration for integration tests
    unit_timeout,           // Duration for unit tests
    property_test_cases,    // Number of property test cases
    http_connection_timeout, // HTTP client timeout
    container_wait_timeout,  // Container startup timeout
};
```

### Test Data Management

**✅ DO**:
```
tests/fixtures/sample_package.toml
tests/data/test_graph.ttl
benches/data/large_template.tera
```

**❌ DON'T**:
```
test_package.toml  (in root)
sample.ttl         (in root)
fixture.json       (in root)
```

---

## CI/CD Integration

### Pre-Commit Validation

```bash
# Auto-runs before commit
- Fast unit tests
- Coverage check (>85%)
- Chicago TDD compliance check
- Hardcoded timeout detection
```

**Blocks commit if**:
- Unit tests fail
- Coverage <85%
- Non-Chicago TDD tests found
- Test data in root folder

### GitHub Actions

**On PR/Push**:
- Full test suite
- Coverage validation (per-crate thresholds)
- Mutation testing (nightly)
- Performance regression detection

---

## Migration Checklist

**When creating new tests**:

- [ ] Use `test!()` or `async_test!()` macros
- [ ] Follow AAA pattern with explicit comments
- [ ] Use real objects (minimal mocking)
- [ ] Use `integration_timeout()` from config
- [ ] Test file in correct directory (`tests/unit/`, `tests/integration/`)
- [ ] Update module checklist
- [ ] Run coverage: `cargo tarpaulin --package <crate>`
- [ ] Verify tests pass: `cargo test --package <crate>`

**When migrating existing tests**:

- [ ] Add `use chicago_tdd_tools::prelude::*;`
- [ ] Convert `#[test]` → `test!()`
- [ ] Convert `#[tokio::test]` → `async_test!()`
- [ ] Remove `-> Result<()>` return type
- [ ] Replace `?` with `.unwrap()` or `assert_ok!()`
- [ ] Remove `Ok(())` at end
- [ ] Add AAA comments
- [ ] Verify tests still pass

---

## Example Test Files

### Unit Test Example

```rust
//! Unit tests for cache module

use chicago_tdd_tools::prelude::*;

#[path = "../common/mod.rs"]
mod test_config;
use test_config::unit_timeout;

use ggen_core::cache::*;

test!(test_cache_creation_with_valid_path_succeeds, {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let cache_path = temp_dir.path();

    // Act
    let cache = CacheManager::new(cache_path);

    // Assert
    assert_ok!(cache);
});

async_test!(test_cache_get_returns_cached_value, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::new(temp_dir.path()).unwrap();
    cache.set("key", "value").await.unwrap();

    // Act
    let result = cache.get("key").await.unwrap();

    // Assert
    assert_eq!(result, Some("value"));
});
```

### Integration Test Example

```rust
//! Integration tests for marketplace install

use chicago_tdd_tools::prelude::*;

#[path = "../common/mod.rs"]
mod test_config;
use test_config::integration_timeout;

use ggen_domain::marketplace::install::*;

async_test!(test_install_package_full_workflow_succeeds, async {
    // Arrange: Set up isolated test environment
    let temp_dir = TempDir::new().unwrap();
    let registry = create_test_registry(&temp_dir).unwrap();
    let options = InstallOptions::new("test-package")
        .with_target(temp_dir.path().to_path_buf());

    // Act: Execute full installation workflow
    let result = install_package(&options, &registry).await.unwrap();

    // Assert: Verify package is installed
    assert!(result.is_success());
    assert!(temp_dir.path().join("test-package").exists());
});
```

---

## Quick Reference Cards

### For Coders

**When writing new code**:
1. Generate test skeleton: `./scripts/generate-test-skeleton.sh <module> <crate>`
2. Implement tests using Chicago TDD patterns
3. Run tests: `cargo test --package <crate>`
4. Check coverage: `cargo tarpaulin --package <crate>`
5. Update module checklist

### For Testers

**When adding tests**:
1. Follow test file structure (unit, integration, property)
2. Use Chicago TDD macros (`test!`, `async_test!`)
3. Follow AAA pattern
4. Use real objects
5. Verify coverage meets threshold

### For Reviewers

**When reviewing PRs**:
1. Check test file locations (correct directory structure)
2. Verify Chicago TDD compliance (macros, AAA, real objects)
3. Validate coverage: `./scripts/validate-crate-coverage.sh`
4. Check module checklist updated
5. Ensure no test data in root folder

---

## Automated Scripts Available

| Script | Purpose | Usage |
|--------|---------|-------|
| **generate-test-skeleton.sh** | Generate test files | `./scripts/generate-test-skeleton.sh <module> <crate>` |
| **validate-test-coverage.sh** | Validate module coverage | `./scripts/validate-test-coverage.sh <crate>` |
| **validate-crate-coverage.sh** | Validate per-crate thresholds | `./scripts/validate-crate-coverage.sh` |

---

## Success Metrics (8-Week Plan)

| Week | ggen-domain | ggen-core | Overall | Status |
|------|-------------|-----------|---------|--------|
| 0 | 27% | 50% | 49% | 🔴 Current |
| 2 | 40% | 55% | 57% | 🟡 Foundation |
| 4 | 70% | 65% | 72% | 🟡 Critical Path |
| 6 | 85% | 75% | 82% | 🟢 Core Complete |
| 8 | 95% | 90% | 90% | 🟢 Target |

---

**Architecture Complete**: ✅
**Implementation Status**: ⏳ Phase 1 (Foundation)
**Next Actions**: Execute automated test generation for priority modules

**Full Details**: See `docs/architecture/TESTING_ARCHITECTURE.md`
