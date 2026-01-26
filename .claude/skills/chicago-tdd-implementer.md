# Skill: chicago-tdd-implementer

## Purpose
Implement comprehensive tests using Arrange-Act-Assert (AAA) pattern verifying observable behavior and state changes

## Triggers (WHEN)
- `#[test]` annotation context
- `Arrange` / `Act` / `Assert` pattern needed
- Test coverage discussion
- Observable state verification
- Behavior validation
- Chicago School TDD mentioned
- 95%+ coverage target discussion

## Don't Trigger (WHEN NOT)
- Mock object discussion
- Internal implementation details
- Unit testing only (without behavior focus)

## Responsibilities

### Test Implementation
- Write unit tests using AAA pattern
- Verify observable state changes
- Create integration tests
- Implement edge case testing
- Achieve 95%+ code coverage target

### Test Organization
- Create test modules: `tests/common/mod.rs`
- Set up test fixtures (temp directories, ontologies)
- Implement test helpers
- Create reusable test macros

### Coverage Areas
- Task 1.1: Audit trail tests (audit.json creation, structure, hashing)
- Task 1.2: Force flag tests (protected files, non-protected files)
- Task 1.3: Test scaffold (fixtures, helpers, macros)
- Task 2.1: Merge mode tests (markers, preservation, injection)
- Task 2.2: Watch mode tests (change detection, debouncing, queuing)
- Task 2.3: Conditional execution tests (ASK evaluation, rule skipping)
- Task 4.1-4.2: Validation tests (SHACL, SPARQL, severity)

## Test Pattern (AAA)

```rust
#[test]
fn test_audit_trail_created_with_flag() {
    // Arrange: Set up test environment
    let temp_dir = TempDir::new().unwrap();
    let manifest = load_fixture("minimal.toml");
    
    // Act: Execute sync with --audit flag
    let result = execute_sync(&manifest, &["--audit"]).unwrap();
    
    // Assert: Verify observable state changed
    assert!(temp_dir.path().join("output/audit.json").exists());
    assert_eq!(result.files_written, 2);
    let audit = parse_audit_json(&temp_dir);
    assert!(!audit.rules_executed.is_empty());
}
```

## Chicago School Principles
✅ **State-based testing**: Verify real object state changes
✅ **Observable behavior**: Test outputs, file writes, returned values
✅ **No mocks**: Use real collaborators (TempDir, actual files)
✅ **Fail fast**: Tests should panic on assertion failure

## Test Categories

### Unit Tests
- `crates/ggen-core/tests/audit_trail_tests.rs`
- `crates/ggen-core/tests/force_flag_tests.rs`
- `crates/ggen-core/tests/merge_mode_tests.rs`
- `crates/ggen-core/tests/watch_mode_tests.rs`
- `crates/ggen-core/tests/conditional_execution_tests.rs`
- `crates/ggen-core/tests/validation_tests.rs`

### Integration Tests
- `tests/integration/sync_e2e_tests.rs`
- `tests/integration/cli_flag_combinations.rs`

## Coverage Target
- **Minimum**: 95% for `crates/ggen-core/src/codegen/`
- **Verification**: `cargo make` with coverage reporting

## Dependencies
- `tempfile` - Temp directory management
- `assert_cmd` - CLI testing
- `serde_json` - JSON parsing

## Related Skills
- `sync-executor` - Code being tested
- `rust-executor` - Running tests
- `architecture-validator` - Test organization

## Files to Create
- `crates/ggen-core/tests/common/mod.rs`
- `crates/ggen-core/tests/fixtures/` (ontology, toml, templates)
- `crates/ggen-core/tests/audit_trail_tests.rs`
- `crates/ggen-core/tests/force_flag_tests.rs`
- `crates/ggen-core/tests/merge_mode_tests.rs`
- `crates/ggen-core/tests/watch_mode_tests.rs`
- `crates/ggen-core/tests/conditional_execution_tests.rs`
- `crates/ggen-core/tests/validation_tests.rs`

## Test Execution
```bash
# Run all tests
cargo make test

# Run specific test file
cargo make test -- tests/audit_trail_tests.rs

# Run with coverage
cargo make test-coverage

# Check coverage
cargo make coverage-report
```

## Quality Standards
- ✅ 100% of test names describe behavior (not implementation)
- ✅ No test interdependencies (each test independent)
- ✅ Fixtures are deterministic and reproducible
- ✅ Tests run in <10s total

## Constitution Alignment
- **Chicago School TDD**: Observable state verification
- **Type Safety**: Result<T,E> in test setups
- **Deterministic**: Reproducible fixtures, no randomness
- **Coverage**: 95%+ enforces comprehensive behavior validation
