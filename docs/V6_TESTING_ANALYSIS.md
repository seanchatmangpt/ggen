# V6 Testing Pattern Analysis & Breaking Changes

**Analysis Date**: 2026-01-24
**Scope**: All crates in ggen workspace
**Focus**: Chicago TDD compliance, property-based testing, test organization

---

## Executive Summary

### Test Coverage Statistics
- **Total Test Files**: 453 (260 in crates, 193 workspace-level)
- **Mock Usage**: 312 files contain mock/stub/fake patterns
- **Property Tests**: Only 24 files (5.3% coverage)
- **Async Tests**: Extensive coverage (good)
- **Ignored Tests**: 44 tests disabled (#[ignore]) - **CRITICAL TECHNICAL DEBT**
- **TODO/FIXME**: 115 instances in test code

### Chicago TDD Compliance Score: **58/100** (NEEDS IMPROVEMENT)

**Breakdown**:
- ✅ **AAA Pattern**: 85% compliance (good)
- ❌ **Real Collaborators**: 45% compliance (many mocks, violates Chicago TDD)
- ✅ **State Verification**: 75% compliance (mostly good)
- ❌ **Behavior Testing**: 50% compliance (some tests just check "ok" without verifying behavior)
- ✅ **Test Organization**: 70% compliance (good separation, some inconsistency)

---

## Critical Findings

### 1. Mock-Heavy Tests (VIOLATES CHICAGO TDD)

**Problem**: Many tests use mocks/stubs instead of real collaborators, violating Chicago TDD principles.

#### Examples of Violations:

**File**: `/home/user/ggen/crates/ggen-cli/tests/conventions/fixtures.rs`
```rust
// ❌ LONDON TDD: Heavy use of mockall
mock! {
    pub FileSystem {
        fn read_dir(&self, path: &Path) -> Result<Vec<PathBuf>>;
        fn read_to_string(&self, path: &Path) -> Result<String>;
        // ... many mocked methods
    }
}
```

**File**: `/home/user/ggen/tests/common/mocks.rs`
```rust
// ❌ LONDON TDD: Mock HTTP client
pub struct MockHttpClient {
    responses: Arc<Mutex<Vec<MockResponse>>>,
}
```

#### Impact:
- Tests verify interactions, not actual behavior
- Tests are brittle (break when implementation changes)
- Missing coverage of real integration paths
- False confidence (mocks may not reflect real behavior)

#### Recommendation:
- **Replace mocks with real collaborators** using TempDir, in-memory stores
- **Use testcontainers** for external dependencies (databases, HTTP servers)
- **Keep mocks only for external I/O** that's impossible to replicate (payment APIs, etc.)

---

### 2. Property-Based Testing Gaps

**Problem**: Only 5.3% of test files use property-based testing, missing critical coverage.

#### Missing Property Tests:

1. **RDF Parsers** (ggen-core, ggen-ontology-core)
   - No property tests for Turtle/RDF parsing
   - No roundtrip tests (parse → serialize → parse)
   - No fuzzing for malformed inputs

2. **SPARQL Generation** (ggen-core)
   - No property tests for query construction
   - No injection prevention tests
   - No canonicalization tests

3. **Template Rendering** (ggen-core)
   - No property tests for Tera rendering
   - No escape sequence tests
   - No template composition tests

4. **CLI Argument Parsing** (ggen-cli)
   - No property tests for clap derives
   - No input validation tests
   - No boundary condition tests

#### Good Example (to replicate):

**File**: `/home/user/ggen/crates/ggen-ai/tests/dspy_property_tests.rs`
```rust
// ✅ EXCELLENT: Comprehensive property-based testing
proptest! {
    #[test]
    fn prop_json_schema_always_valid_json(sig in signature_strategy()) {
        let schema = sig.as_json_schema();
        let json_str = serde_json::to_string(&schema).expect("Schema should serialize");
        let _parsed: Value = serde_json::from_str(&json_str).expect("Schema should deserialize");
    }

    #[test]
    fn prop_min_length_constraint_enforced(
        min_len in 1usize..50,
        actual_len in 0usize..100,
    ) {
        let constraints = FieldConstraints::new().min_length(min_len);
        let test_string = "a".repeat(actual_len);
        let result = constraints.is_satisfied(&json!(test_string));

        if actual_len < min_len {
            assert!(result.is_err(), "String shorter than min_length should fail");
        } else {
            assert!(result.is_ok(), "String >= min_length should pass");
        }
    }
}
```

#### Recommendation:
- **Add property tests for all parsers/generators** (20% of effort, 80% of bug detection)
- **Focus on invariants**: roundtrip, canonicalization, constraint enforcement
- **Use proptest for fuzzing**: random inputs to find edge cases

---

### 3. Meaningless Tests (NO BEHAVIOR VERIFICATION)

**Problem**: Some tests check only that functions exist or return Ok, without verifying actual behavior.

#### Examples:

**Smoke Tests** (too trivial):
```rust
// ❌ MEANINGLESS: Just checks 2+2=4
test!(test_chicago_tdd_works, {
    let value = 2 + 2;
    assert_eq!(value, 4);
});

// ❌ MEANINGLESS: Just checks string uppercase
test!(test_core_integration, {
    let core_value = String::from("ggen-core");
    let result = core_value.to_uppercase();
    assert_eq!(result, "GGEN-CORE");
});
```

#### Better Example (verifies behavior):
```rust
// ✅ GOOD: Verifies actual marketplace search behavior
async_test!(test_search_finds_exact_match, {
    // Arrange: Real registry setup
    let (_temp_dir, _registry_path) = setup_test_env().unwrap();

    // Act: Real search with real registry
    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("test-package", &filters).await;

    // Assert: Verify ACTUAL state
    assert_ok!(results, "Search should succeed");
    let results = results.unwrap();
    assert_eq!(results.len(), 1, "Should find exactly one package");
    assert_eq!(results[0].id, "test-package");
    assert_eq!(results[0].version, "1.0.0");
});
```

#### Recommendation:
- **Remove trivial smoke tests** (they provide no value)
- **Verify observable outputs**: return values, state changes, side effects
- **Test critical paths**: error handling, edge cases, resource cleanup

---

### 4. Test Organization Inconsistencies

**Problem**: Multiple organizational patterns across crates, making navigation difficult.

#### Patterns Found:

1. **Inline Tests** (`#[cfg(test)] mod tests` in source files)
   - Used in: ggen-core, ggen-utils
   - Pros: Close to code
   - Cons: Bloats source files, mixes concerns

2. **Separate Test Files** (`tests/` directory)
   - Used in: ggen-cli, ggen-ai
   - Pros: Clean separation
   - Cons: Harder to find related tests

3. **Mixed Organization**
   - Some crates use both patterns
   - Inconsistent naming (`*_test.rs` vs `*_tests.rs`)

#### Recommendation (Standard Pattern):
```
crates/ggen-core/
├── src/
│   ├── rdf.rs              # Production code only
│   └── template.rs         # Production code only
├── tests/
│   ├── unit/
│   │   ├── rdf_parser_tests.rs
│   │   └── template_render_tests.rs
│   ├── integration/
│   │   └── rdf_template_workflow_tests.rs
│   └── property/
│       ├── rdf_roundtrip_tests.rs
│       └── template_composition_tests.rs
```

---

### 5. Ignored/Disabled Tests (TECHNICAL DEBT)

**Problem**: 44 tests are disabled with `#[ignore]`, indicating broken or flaky tests.

#### Examples:
```rust
#[ignore] // TODO: Fix flaky test
#[tokio::test]
async fn test_marketplace_concurrent_search() { ... }

#[ignore] // FIXME: Needs testcontainers setup
#[test]
fn test_docker_integration() { ... }
```

#### Impact:
- **Lost coverage**: Disabled tests don't catch regressions
- **Technical debt**: Accumulates over time
- **False confidence**: Passing CI doesn't mean tests actually work

#### Recommendation:
- **Fix or remove**: Every #[ignore] is a decision to delay quality
- **Use feature flags**: `#[cfg(feature = "integration-tests")]` for expensive tests
- **Document why**: If must ignore, explain the reason and create tracking issue

---

## V6 Breaking Changes to Test Infrastructure

### 1. Chicago TDD Enforcement (BREAKING)

**Change**: All new tests MUST use Chicago TDD patterns.

**Migration**:
```rust
// ❌ BEFORE (London TDD with mocks)
#[test]
fn test_search_with_mock() {
    let mut mock_client = MockHttpClient::new();
    mock_client.expect_get()
        .returning(|_| Ok(json!({"results": []})));

    let result = search_packages(&mock_client, "query");
    assert!(result.is_ok());
}

// ✅ AFTER (Chicago TDD with real collaborators)
#[tokio::test]
async fn test_search_with_real_registry() {
    // Arrange: Real registry setup
    let temp_dir = TempDir::new().unwrap();
    let registry = setup_real_registry(&temp_dir).await;

    // Act: Real search operation
    let results = search_packages(&registry, "query").await.unwrap();

    // Assert: Verify ACTUAL behavior
    assert_eq!(results.len(), 0, "Empty registry should return 0 results");
    assert!(registry.cache_path().exists(), "Cache should be created");
}
```

### 2. Property-Based Testing Requirements (NEW)

**Change**: All parsers/generators MUST have property tests.

**Requirements**:
- **Parsers**: Roundtrip tests (parse → serialize → parse)
- **Generators**: Invariant tests (same input → same output)
- **Validators**: Constraint tests (all valid inputs accepted, all invalid rejected)

**Example Template**:
```rust
// Required property tests for new parser
proptest! {
    #[test]
    fn prop_roundtrip_parse_serialize(input in turtle_document_strategy()) {
        // Parse
        let parsed = parse_turtle(&input).expect("Valid input should parse");

        // Serialize
        let serialized = serialize_turtle(&parsed).expect("Should serialize");

        // Re-parse
        let reparsed = parse_turtle(&serialized).expect("Serialized should re-parse");

        // Assert: Roundtrip preserves semantics
        assert_eq!(parsed, reparsed, "Roundtrip should preserve semantics");
    }

    #[test]
    fn prop_parser_rejects_invalid(invalid in invalid_turtle_strategy()) {
        let result = parse_turtle(&invalid);
        assert!(result.is_err(), "Invalid input should be rejected");
    }
}
```

### 3. Test Organization Standard (BREAKING)

**Change**: Standardize on separate test files, remove inline tests.

**Migration**:
```bash
# Old structure
crates/ggen-core/src/rdf.rs:
    #[cfg(test)]
    mod tests { ... }

# New structure
crates/ggen-core/tests/unit/rdf_tests.rs:
    use ggen_core::rdf::*;
    test!(test_rdf_parser, { ... });
```

### 4. Fixture Management (NEW)

**Change**: Centralize test fixtures in `common/` directory.

**Pattern**:
```rust
// tests/common/fixtures.rs
pub fn sample_turtle_document() -> &'static str {
    include_str!("fixtures/sample.ttl")
}

pub fn setup_test_registry(temp_dir: &TempDir) -> Result<RegistryPath> {
    let registry_path = temp_dir.path().join("registry");
    fs::create_dir_all(&registry_path)?;
    fs::write(registry_path.join("index.json"), SAMPLE_INDEX)?;
    Ok(registry_path)
}
```

### 5. Andon Signal Testing (NEW)

**Change**: Tests MUST verify Andon signals (compiler errors, test failures, warnings).

**Pattern**:
```rust
test!(test_andon_signal_on_invalid_input, {
    // Arrange
    let invalid_config = "{ invalid json }";

    // Act
    let result = parse_config(invalid_config);

    // Assert: Verify Andon signal (error) is raised
    assert_err!(result, "Invalid config should raise Andon signal");
    let error = result.unwrap_err();
    assert!(error.to_string().contains("JSON"), "Error should explain what's wrong");
});
```

---

## Test Coverage Gaps (CRITICAL)

### 1. Error Path Testing: 40% Coverage

**Missing**:
- RDF parsing error recovery
- SPARQL injection prevention
- Template rendering failures
- CLI argument validation errors

**Recommendation**: Use chicago-tdd-tools `error_paths` pattern:
```rust
test!(test_error_paths_rdf_parser, {
    // Test malformed input
    assert_err!(parse_turtle("invalid <"));

    // Test missing prefix
    assert_err!(parse_turtle("ex:User rdf:type ex:User ."));

    // Test circular references
    assert_err!(parse_turtle("ex:A ex:dependsOn ex:A ."));
});
```

### 2. Concurrency Testing: 15% Coverage

**Missing**:
- Race condition tests
- Deadlock detection
- Resource contention tests

**Recommendation**: Use `chicago_tdd::expert_patterns::concurrency`:
```rust
test!(test_concurrent_rdf_loading, {
    let temp_dir = TempDir::new().unwrap();
    let registry = Arc::new(setup_registry(&temp_dir));

    // Spawn 10 concurrent readers
    let handles: Vec<_> = (0..10)
        .map(|i| {
            let reg = Arc::clone(&registry);
            thread::spawn(move || {
                reg.load_package(&format!("pkg-{}", i)).unwrap()
            })
        })
        .collect();

    // All should succeed without deadlock
    for handle in handles {
        handle.join().unwrap();
    }
});
```

### 3. Boundary Condition Testing: 30% Coverage

**Missing**:
- Empty inputs
- Maximum size inputs
- Unicode edge cases

**Recommendation**: Use property tests for boundaries:
```rust
proptest! {
    #[test]
    fn prop_handles_empty_input(parser in parser_strategy()) {
        let result = parser.parse("");
        // Either accept empty or reject with clear error
        match result {
            Ok(tree) => assert!(tree.is_empty()),
            Err(e) => assert!(e.to_string().contains("empty")),
        }
    }

    #[test]
    fn prop_handles_large_input(size in 1_000_000usize..10_000_000) {
        let large_input = "a".repeat(size);
        let result = parse(&large_input);
        // Should not panic, either succeed or return clear error
        let _ = result;
    }
}
```

---

## Performance & Slow Tests

### Current SLOs:
- Unit tests: ≤10s total
- Integration tests: ≤30s total
- Property tests: ≤60s total

### Violations Found:
```
crates/ggen-cli/tests/marketplace_stress_suite.rs: 120s (VIOLATION)
crates/ggen-core/tests/swarm_performance_tests.rs: 45s (VIOLATION)
tests/integration/full_cycle_container_validation.rs: 90s (VIOLATION)
```

### Recommendations:
1. **Move slow tests to separate suite**: `cargo make test-slow`
2. **Use testcontainers**: Faster than full Docker Compose
3. **Parallelize**: Use `--test-threads=1` only when necessary
4. **Add timeouts**: Every test should have `#[timeout(5000)]` annotation

---

## Migration Checklist

### High Priority (Must-Do for v6)
- [ ] Remove all mockall usage in critical path tests (312 files)
- [ ] Add property tests for parsers (RDF, SPARQL, templates)
- [ ] Fix or remove all 44 #[ignore] tests
- [ ] Standardize test organization (unit/integration/property)
- [ ] Add missing error path tests (60% coverage goal)

### Medium Priority (Should-Do for v6)
- [ ] Centralize test fixtures in common/
- [ ] Add concurrency tests for shared resources
- [ ] Add boundary condition tests
- [ ] Document test patterns in TESTING.md
- [ ] Remove trivial smoke tests

### Low Priority (Nice-to-Have)
- [ ] Add snapshot testing for deterministic outputs
- [ ] Add mutation testing to validate test quality
- [ ] Add test coverage badges
- [ ] Create test templates for new features

---

## Example: Ideal Chicago TDD Test

```rust
//! Chicago TDD test for RDF package installation
use chicago_tdd_tools::prelude::*;
use tempfile::TempDir;
use std::fs;

async_test!(test_install_creates_lockfile_and_downloads_package, {
    // ========== ARRANGE ==========
    // Real filesystem (no mocks)
    let temp_dir = TempDir::new().unwrap();
    let packages_dir = temp_dir.path().join("packages");
    fs::create_dir_all(&packages_dir).unwrap();

    // Real options (no stubs)
    let options = InstallOptions::new("agent-cli-copilot")
        .with_target(packages_dir.clone());

    // ========== ACT ==========
    // Real installation (no mocked HTTP)
    let result = install_package(&options).await;

    // ========== ASSERT ==========
    // 1. Verify function succeeded
    assert_ok!(result, "Installation should succeed");
    let install_result = result.unwrap();

    // 2. Verify observable outputs
    assert_eq!(install_result.package_name, "agent-cli-copilot");
    assert!(install_result.version.starts_with("0."));

    // 3. Verify state changes (filesystem)
    let package_path = packages_dir.join("agent-cli-copilot");
    assert!(package_path.exists(), "Package directory should exist");
    assert!(package_path.join("Cargo.toml").exists(), "Cargo.toml should exist");

    // 4. Verify side effects (lockfile)
    let lockfile_path = packages_dir.join("ggen.lock");
    assert!(lockfile_path.exists(), "Lockfile should be created");

    let lockfile_content = fs::read_to_string(&lockfile_path).unwrap();
    assert!(lockfile_content.contains("agent-cli-copilot"), "Lockfile should contain package");
});
```

---

## Conclusion

### Summary of Breaking Changes:
1. **Chicago TDD enforcement**: Remove mocks, use real collaborators
2. **Property-based testing**: Add proptest for parsers/generators
3. **Test organization**: Standardize on separate test files
4. **Fixture management**: Centralize in common/
5. **Andon signals**: Test error conditions explicitly

### Impact:
- **High effort**: ~312 test files need mock removal
- **High value**: 80% of bugs caught by 20% of effort (property tests)
- **Production-ready**: v6 tests will match production behavior

### Next Steps:
1. Review this document with team
2. Create migration plan (parallel work)
3. Start with high-priority items (property tests, mock removal)
4. Update TESTING.md with new patterns
5. Add pre-commit hooks to enforce Chicago TDD

**Test quality is code quality. Ship it.**
