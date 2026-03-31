# London TDD Test Execution Guide

## Quick Start

```bash
# Run all London TDD tests
cargo test --test london_tdd_main

# Run specific test module
cargo test --test london_tdd_main -- doctor_test

# Run with output
cargo test --test london_tdd_main -- --nocapture

# Run single test
cargo test --test london_tdd_main -- test_doctor_checks_all_prerequisites
```

## Test Performance

All tests are designed to execute in <100ms each:

```bash
# Run tests with timing
cargo test --test london_tdd_main -- --nocapture --test-threads=1

# Expected output:
# test doctor_test::test_doctor_checks_all_prerequisites ... ok (12ms)
# test help_me_test::test_help_me_detects_newcomer_level ... ok (5ms)
# test quickstart_test::test_quickstart_complete_flow ... ok (8ms)
```

## Coverage by README Section

### User-Friendly Features (§User-Friendly Features)
- ✅ `doctor_test.rs` - Environment health checks
- ✅ `help_me_test.rs` - Progressive help system
- ✅ `quickstart_test.rs` - 2-minute quickstart
- ✅ `enhanced_errors_test.rs` - Error messages with suggestions

### Marketplace (§Marketplace)
- ✅ `search_test.rs` - Natural language search
- ✅ `install_test.rs` - Package installation

### AI Generation (§AI-Powered Generation)
- ✅ `template_gen_test.rs` - AI template generation
- ✅ `project_gen_test.rs` - AI project scaffolding

### Template Engine (§Template Example)
- ✅ `rendering_test.rs` - Tera template rendering
- ✅ `rdf_sparql_test.rs` - RDF/SPARQL integration

### Observability (§Performance SLOs)
- ✅ `trace_validator.rs` - OpenTelemetry instrumentation

## Test Structure

Each test follows London TDD principles:

```rust
#[test]
fn test_feature_behaves_correctly() {
    let start = std::time::Instant::now();

    // Arrange: Set up mocks
    let mut mock_dependency = MockDependency::new();
    mock_dependency
        .expect_method()
        .with(eq("input"))
        .times(1)
        .returning(|_| Ok("output"));

    // Act: Execute behavior
    let result = system_under_test(&mock_dependency);

    // Assert: Verify interactions and outcomes
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "expected");

    // Performance: Verify <100ms
    assert!(start.elapsed().as_millis() < 100);
}
```

## Continuous Integration

Add to `.github/workflows/tests.yml`:

```yaml
- name: Run London TDD Tests
  run: cargo test --test london_tdd_main
  timeout-minutes: 1  # All tests complete in <1 minute
```

## Debugging Failed Tests

```bash
# Run single test with full output
cargo test --test london_tdd_main -- test_name --nocapture --exact

# Run with backtrace
RUST_BACKTRACE=1 cargo test --test london_tdd_main -- test_name

# Run single-threaded for debugging
cargo test --test london_tdd_main -- --test-threads=1
```

## Mock Verification

All mocks verify:
- Method called correct number of times
- Method called with correct arguments
- Method returns expected results
- No unexpected calls

## Adding New Tests

1. Create test file in appropriate directory:
   ```
   tests/london_tdd/
   ├── cli_commands/     # CLI feature tests
   ├── marketplace/      # Marketplace tests
   ├── ai_generation/    # AI feature tests
   ├── template_engine/  # Template tests
   └── otel_validation/  # Observability tests
   ```

2. Use mocks from `lib.rs`:
   ```rust
   use crate::lib::*;
   ```

3. Follow London TDD pattern:
   - Mock all external dependencies
   - Test behavior, not implementation
   - Verify interactions
   - <100ms execution

4. Add module to `london_tdd_main.rs`:
   ```rust
   #[path = "london_tdd/your_module/your_test.rs"]
   mod your_test;
   ```

## Test Coverage Report

```bash
# Generate coverage report (requires tarpaulin)
cargo tarpaulin --test london_tdd_main --out Html

# Open report
open tarpaulin-report.html
```

## Performance Benchmarking

```bash
# Run with timing for all tests
cargo test --test london_tdd_main -- --nocapture | grep -E "test.*ok"

# Expected:
# - Average test time: <50ms
# - Max test time: <100ms
# - Total suite time: <5 seconds
```

## Success Criteria

✅ All tests pass
✅ Average test time <50ms
✅ No external dependencies (all mocked)
✅ 100% reproducible results
✅ No test flakiness
✅ Clear failure messages
