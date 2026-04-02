<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [V6 Testing Breaking Changes](#v6-testing-breaking-changes)
  - [Breaking Changes Summary](#breaking-changes-summary)
  - [&#91;BC-1&#93; Mock Removal (CRITICAL)](#bc-1-mock-removal-critical)
    - [What Changed](#what-changed)
    - [Breaking Change](#breaking-change)
    - [Migration Required](#migration-required)
    - [Files Affected](#files-affected)
    - [Detection Script](#detection-script)
    - [Verification](#verification)
  - [&#91;BC-2&#93; Property-Based Testing Required (CRITICAL)](#bc-2-property-based-testing-required-critical)
    - [What Changed](#what-changed-1)
    - [Breaking Change](#breaking-change-1)
    - [Migration Required](#migration-required-1)
    - [Components Requiring Property Tests](#components-requiring-property-tests)
    - [Detection Script](#detection-script-1)
    - [Verification](#verification-1)
  - [&#91;BC-3&#93; Test Organization Restructure (MEDIUM)](#bc-3-test-organization-restructure-medium)
    - [What Changed](#what-changed-2)
    - [Breaking Change](#breaking-change-2)
    - [Migration Required](#migration-required-2)
    - [New Directory Structure](#new-directory-structure)
    - [Migration Script](#migration-script)
    - [Verification](#verification-2)
  - [&#91;BC-4&#93; Fixture Centralization (MEDIUM)](#bc-4-fixture-centralization-medium)
    - [What Changed](#what-changed-3)
    - [Breaking Change](#breaking-change-3)
    - [Migration Required](#migration-required-3)
    - [Fixture Types to Centralize](#fixture-types-to-centralize)
    - [Detection Script](#detection-script-2)
    - [Verification](#verification-3)
  - [&#91;BC-5&#93; Andon Signal Testing (LOW)](#bc-5-andon-signal-testing-low)
    - [What Changed](#what-changed-4)
    - [Breaking Change](#breaking-change-4)
    - [Migration Required](#migration-required-4)
    - [Error Paths to Test](#error-paths-to-test)
    - [Detection Script](#detection-script-3)
    - [Verification](#verification-4)
  - [Migration Checklist](#migration-checklist)
    - [Phase 1: Critical Changes (Week 1-2)](#phase-1-critical-changes-week-1-2)
    - [Phase 2: Organization (Week 3)](#phase-2-organization-week-3)
    - [Phase 3: Quality (Week 4)](#phase-3-quality-week-4)
    - [Verification Gates](#verification-gates)
  - [Rollback Plan](#rollback-plan)
  - [Communication Plan](#communication-plan)
    - [Week 1 (Preparation)](#week-1-preparation)
    - [Week 2-3 (Migration)](#week-2-3-migration)
    - [Week 4 (Verification)](#week-4-verification)
  - [Support & Questions](#support--questions)
    - [Common Questions](#common-questions)
    - [Getting Help](#getting-help)
  - [Timeline](#timeline)
  - [Success Metrics](#success-metrics)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# V6 Testing Breaking Changes

**Version**: 6.0.0
**Date**: 2026-01-24
**Impact**: HIGH (312 test files affected)
**Migration Time**: 60-90 hours

---

## Breaking Changes Summary

| Change | Files Affected | Priority | Effort | Impact |
|--------|---------------|----------|--------|--------|
| [BC-1] Mock Removal | 312 | HIGH | 40h | ⚠️ Test failures |
| [BC-2] Property Tests Required | 24 → 100+ | HIGH | 30h | ✅ Better coverage |
| [BC-3] Test Organization | 453 | MEDIUM | 15h | ℹ️ Restructure |
| [BC-4] Fixture Centralization | ~50 | MEDIUM | 10h | ℹ️ Cleanup |
| [BC-5] Andon Signal Testing | ALL | LOW | 5h | ✅ Error handling |

**Total Affected**: 453 test files
**Total Effort**: 60-90 hours

---

## [BC-1] Mock Removal (CRITICAL)

### What Changed
- **Before**: Tests used `mockall` and hand-written mocks
- **After**: Tests use real collaborators with TempDir/testcontainers

### Breaking Change
```rust
// ❌ BREAKING: This will no longer compile
use mockall::mock;

mock! {
    pub HttpClient {
        fn get(&self, url: &str) -> Result<String>;
    }
}

#[test]
fn test_with_mock() {
    let mut mock = MockHttpClient::new();
    mock.expect_get().returning(|_| Ok("response".to_string()));
    // ... test code
}
```

### Migration Required
```rust
// ✅ FIXED: Use real collaborators
use chicago_tdd_tools::prelude::*;
use testcontainers::{clients::Cli, GenericImage};

async_test!(test_with_real_server, {
    // Real HTTP server
    let docker = Cli::default();
    let container = docker.run(GenericImage::new("kennethreitz/httpbin", "latest"));
    let port = container.get_host_port_ipv4(80);

    // Real HTTP client
    let client = reqwest::Client::new();
    let response = client.get(format!("http://localhost:{}/get", port))
        .send()
        .await
        .unwrap();

    // Real assertions
    assert_eq!(response.status(), 200);
});
```

### Files Affected
```
crates/ggen-cli/tests/conventions/fixtures.rs
tests/common/mocks.rs
crates/ggen-core/tests/unit/mock_impls.rs
crates/ggen-cli/tests/marketplace/unit/mod.rs
... (308 more files)
```

### Detection Script
```bash
# Find all files with mocks
rg "mockall|Mock\{|mock!" crates/*/tests tests/ -l
```

### Verification
```bash
# After migration, this should pass
cargo make test-lint-chicago-tdd
```

---

## [BC-2] Property-Based Testing Required (CRITICAL)

### What Changed
- **Before**: Property tests optional (5.3% coverage)
- **After**: Property tests REQUIRED for parsers/generators

### Breaking Change
```rust
// ❌ BREAKING: This is no longer sufficient for parsers
#[test]
fn test_parse_simple_turtle() {
    let turtle = "@prefix ex: <http://example.org/> .\nex:Subject ex:predicate ex:Object .";
    let result = parse_turtle(turtle);
    assert!(result.is_ok());
}
```

### Migration Required
```rust
// ✅ FIXED: Add property tests
use proptest::prelude::*;

proptest! {
    // Property 1: Never panics
    #[test]
    fn prop_parser_never_panics(input in "\\PC*") {
        let _ = parse_turtle(&input);
    }

    // Property 2: Roundtrip preserves semantics
    #[test]
    fn prop_roundtrip(doc in valid_turtle_strategy()) {
        let parsed = parse_turtle(&doc).unwrap();
        let serialized = serialize_turtle(&parsed).unwrap();
        let reparsed = parse_turtle(&serialized).unwrap();
        assert_eq!(parsed, reparsed);
    }

    // Property 3: Deterministic
    #[test]
    fn prop_deterministic(doc in valid_turtle_strategy()) {
        let result1 = parse_turtle(&doc);
        let result2 = parse_turtle(&doc);
        assert_eq!(result1, result2);
    }
}
```

### Components Requiring Property Tests
1. **RDF Parsers** (ggen-core, ggen-ontology-core)
   - Turtle parsing
   - RDF/XML parsing
   - N-Triples parsing

2. **SPARQL Generators** (ggen-core)
   - Query construction
   - Injection prevention
   - Canonicalization

3. **Template Rendering** (ggen-core)
   - Tera template rendering
   - Variable substitution
   - Escape sequences

4. **CLI Argument Parsing** (ggen-cli)
   - Clap derives
   - Input validation
   - Constraint checking

### Detection Script
```bash
# Find components missing property tests
cargo make test-analyze-property-gaps
```

### Verification
```bash
# After migration, coverage should be >80%
cargo make test-property-coverage
```

---

## [BC-3] Test Organization Restructure (MEDIUM)

### What Changed
- **Before**: Inline tests (`#[cfg(test)] mod tests`) mixed with production code
- **After**: Separate test files in `tests/` directory

### Breaking Change
```rust
// ❌ BREAKING: Inline tests no longer allowed in src/
// src/rdf.rs
pub fn parse_turtle(input: &str) -> Result<Graph> {
    // ... implementation
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_turtle() {
        // ... test code
    }
}
```

### Migration Required
```rust
// ✅ FIXED: Move to separate test file
// tests/unit/rdf_parser_tests.rs
use ggen_core::rdf::parse_turtle;
use chicago_tdd_tools::prelude::*;

test!(test_parse_turtle, {
    // ... test code
});
```

### New Directory Structure
```
crates/ggen-core/
├── src/
│   └── rdf.rs              # Production code only (no #[cfg(test)])
└── tests/
    ├── common/
    │   ├── mod.rs
    │   └── fixtures.rs
    ├── unit/
    │   ├── mod.rs
    │   └── rdf_parser_tests.rs
    ├── integration/
    │   ├── mod.rs
    │   └── rdf_workflow_tests.rs
    └── property/
        ├── mod.rs
        └── rdf_roundtrip_tests.rs
```

### Migration Script
```bash
# Run for each crate
./scripts/migrate-test-structure.sh ggen-core
./scripts/migrate-test-structure.sh ggen-cli
# ... etc
```

### Verification
```bash
# No inline tests should remain
! rg "#\[cfg\(test\)\]" crates/*/src --quiet
```

---

## [BC-4] Fixture Centralization (MEDIUM)

### What Changed
- **Before**: Fixtures duplicated across test files
- **After**: Fixtures centralized in `tests/common/fixtures.rs`

### Breaking Change
```rust
// ❌ BREAKING: Duplicated setup in every test file
// test1.rs
fn setup_test_dir() -> TempDir {
    let temp = TempDir::new().unwrap();
    fs::create_dir_all(temp.path().join("packages")).unwrap();
    temp
}

// test2.rs (duplicate!)
fn setup_test_dir() -> TempDir {
    let temp = TempDir::new().unwrap();
    fs::create_dir_all(temp.path().join("packages")).unwrap();
    temp
}
```

### Migration Required
```rust
// ✅ FIXED: Centralized fixture
// tests/common/fixtures.rs
pub struct TestEnv {
    _temp_dir: TempDir,
    pub root: PathBuf,
    pub packages_dir: PathBuf,
}

impl TestEnv {
    pub fn new() -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let root = temp_dir.path().to_path_buf();
        let packages_dir = root.join("packages");
        fs::create_dir_all(&packages_dir).expect("Failed to create packages dir");

        Self { _temp_dir: temp_dir, root, packages_dir }
    }
}

// test1.rs
use common::fixtures::TestEnv;

test!(test_with_fixture, {
    let env = TestEnv::new();
    // ... use env.packages_dir
});
```

### Fixture Types to Centralize
1. **Filesystem Setup**: TempDir, directory creation
2. **Sample Data**: Turtle documents, JSON configs
3. **Mock Servers**: testcontainers setup
4. **Test Registries**: Package metadata

### Detection Script
```bash
# Find duplicated setup functions
rg "fn setup.*TempDir" crates/*/tests tests/ -c | awk -F: '$2 > 1 {print}'
```

### Verification
```bash
# All tests should use centralized fixtures
rg "use common::fixtures" crates/*/tests tests/ | wc -l
# Should be > 100
```

---

## [BC-5] Andon Signal Testing (LOW)

### What Changed
- **Before**: Error paths often untested
- **After**: All error conditions MUST be tested

### Breaking Change
```rust
// ❌ BREAKING: Only happy path tested
#[test]
fn test_parse_config() {
    let config = r#"{"name": "test"}"#;
    let result = parse_config(config);
    assert!(result.is_ok());
}
```

### Migration Required
```rust
// ✅ FIXED: Test error paths (Andon signals)
test!(test_parse_config_happy_path, {
    // Arrange
    let config = r#"{"name": "test"}"#;

    // Act
    let result = parse_config(config);

    // Assert
    assert_ok!(result);
});

test!(test_parse_config_invalid_json_raises_andon_signal, {
    // Arrange
    let invalid_config = "{ invalid json }";

    // Act
    let result = parse_config(invalid_config);

    // Assert: Andon signal (error) raised
    assert_err!(result, "Invalid JSON should raise Andon signal");
    let error = result.unwrap_err();
    assert!(error.to_string().contains("JSON"), "Error should explain issue");
});

test!(test_parse_config_missing_required_field_raises_andon_signal, {
    // Arrange
    let incomplete_config = r#"{}"#;

    // Act
    let result = parse_config(incomplete_config);

    // Assert: Andon signal (error) raised
    assert_err!(result, "Missing required field should raise Andon signal");
});
```

### Error Paths to Test
1. **Invalid Input**: Malformed JSON, invalid Turtle, bad CLI args
2. **Missing Resources**: File not found, missing dependencies
3. **Constraint Violations**: Out-of-bounds values, type mismatches
4. **Resource Exhaustion**: OOM, disk full, timeout

### Detection Script
```bash
# Find tests missing error path coverage
for file in crates/*/tests/**/*.rs; do
    happy=$(rg "assert_ok!" "$file" -c 2>/dev/null || echo 0)
    sad=$(rg "assert_err!" "$file" -c 2>/dev/null || echo 0)

    if [ "$happy" -gt 0 ] && [ "$sad" -eq 0 ]; then
        echo "❌ $file: $happy happy paths, 0 error paths"
    fi
done
```

### Verification
```bash
# Every test file should have error path tests
for file in crates/*/tests/**/*.rs; do
    if ! rg "assert_err!" "$file" --quiet; then
        echo "⚠️  $file missing error path tests"
    fi
done
```

---

## Migration Checklist

### Phase 1: Critical Changes (Week 1-2)
- [ ] [BC-1] Remove mockall from 312 test files
  - [ ] ggen-core (80 files)
  - [ ] ggen-cli (120 files)
  - [ ] ggen-ai (30 files)
  - [ ] Other crates (82 files)
- [ ] [BC-2] Add property tests for parsers
  - [ ] RDF parsers (Turtle, RDF/XML, N-Triples)
  - [ ] SPARQL generators
  - [ ] Template renderers
  - [ ] CLI argument parsers

### Phase 2: Organization (Week 3)
- [ ] [BC-3] Restructure test organization
  - [ ] Create `tests/{unit,integration,property}` directories
  - [ ] Move inline tests to separate files
  - [ ] Remove `#[cfg(test)]` from src/
- [ ] [BC-4] Centralize fixtures
  - [ ] Create `tests/common/fixtures.rs`
  - [ ] Migrate duplicated setup functions
  - [ ] Add fixture documentation

### Phase 3: Quality (Week 4)
- [ ] [BC-5] Add error path testing
  - [ ] Test all parse errors
  - [ ] Test all validation errors
  - [ ] Test all resource errors
- [ ] Fix 44 ignored tests
- [ ] Remove 115 TODO/FIXME from tests

### Verification Gates
- [ ] `cargo make test-lint-chicago-tdd` passes (no mocks)
- [ ] `cargo make test-property-coverage` shows >80%
- [ ] `cargo make test` passes in <30s
- [ ] No `#[ignore]` tests remain
- [ ] No `#[cfg(test)]` in src/ files

---

## Rollback Plan

If migration causes issues, rollback is possible:

```bash
# Rollback to v5.x test patterns
git checkout v5.1.0 -- crates/*/tests tests/

# Restore mockall dependencies
git checkout v5.1.0 -- Cargo.toml crates/*/Cargo.toml

# Verify rollback
cargo make test
```

**Warning**: v6 features will not work with v5 tests. Only rollback if migration blocked.

---

## Communication Plan

### Week 1 (Preparation)
- [ ] Share V6_TESTING_ANALYSIS.md with team
- [ ] Review breaking changes in team meeting
- [ ] Assign migration tasks to engineers

### Week 2-3 (Migration)
- [ ] Daily standup on migration progress
- [ ] Code reviews for migrated tests
- [ ] Update TESTING.md documentation

### Week 4 (Verification)
- [ ] Run full test suite
- [ ] Performance benchmarks
- [ ] Security audit
- [ ] Go/No-Go decision

---

## Support & Questions

### Common Questions

**Q: Why remove mocks?**
A: Mocks test interactions, not behavior. Chicago TDD tests real behavior, finds real bugs.

**Q: Why property tests?**
A: 20% of effort (property tests on parsers) catches 80% of bugs (edge cases, unicode, malformed input).

**Q: Can I keep some mocks?**
A: Only for external I/O that's impossible to replicate (payment APIs, third-party services).

**Q: What if tests are too slow?**
A: Use testcontainers (faster than Docker Compose), parallel execution, or move to slow test suite.

### Getting Help

- **Slack**: #ggen-testing
- **GitHub**: [Issues with `testing` label](https://github.com/seanchatmangpt/ggen/labels/testing)
- **Docs**: `/docs/TESTING.md`

---

## Timeline

```
Week 1-2: Critical Changes (BC-1, BC-2)
  ├── Remove mocks (40h)
  └── Add property tests (30h)

Week 3: Organization (BC-3, BC-4)
  ├── Restructure tests (15h)
  └── Centralize fixtures (10h)

Week 4: Quality & Verification (BC-5)
  ├── Add error path tests (5h)
  ├── Fix ignored tests
  └── Final verification

Total: 60-90 hours
```

---

## Success Metrics

After migration:
- ✅ 0 mock usage (down from 312)
- ✅ 80%+ property test coverage (up from 5.3%)
- ✅ 0 ignored tests (down from 44)
- ✅ 0 TODO/FIXME in tests (down from 115)
- ✅ <30s full test suite (currently 45s+)
- ✅ 90%+ Chicago TDD compliance (up from 58%)

**Ship it.**
