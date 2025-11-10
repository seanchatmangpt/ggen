# Testing - Close Gaps and Verify

Guide to testing that helps identify gaps, verify functionality, and ensure features work end-to-end.

## Quick Reference

### Commands
```bash
# Run all tests
cargo make test

# Unit tests
cargo make test-unit

# Integration tests
cargo make test-integration

# E2E tests
cargo test --test e2e_v2_validation

# Property tests
cargo make test-proptest

# Single-threaded (deterministic)
cargo make test-single-threaded

# Production readiness (includes all tests)
cargo make production-readiness
```

## What to Test

### User Journeys
Verify complete workflows from start to finish:
- New user installs and creates first project
- User generates code from templates
- User uses marketplace packages
- Generated code compiles and runs
- Multi-step workflows complete successfully

### Layer Interactions
Verify components work together:
- CLI layer parses arguments correctly
- Domain layer executes business logic
- Core layer performs actual work
- Errors propagate correctly through layers
- State changes are observable

### Workflows
Verify multi-command sequences:
- Template generation → lifecycle execution
- Marketplace search → package installation → code generation
- RDF loading → SPARQL query → template rendering
- Error recovery and rollback scenarios

## How to Identify Gaps

### Missing Tests
Check for:
- Public APIs without tests
- Error paths not tested
- Edge cases not covered
- Integration points not verified
- User journeys not tested end-to-end

### Flaky Tests
Identify:
- Tests that pass sometimes, fail other times
- Tests dependent on timing or order
- Tests with race conditions
- Tests that depend on external state

### Untested Paths
Find:
- Code paths never executed in tests
- Error handling not exercised
- Failure scenarios not covered
- Boundary conditions not tested

## How to Verify

### Outputs Compile
- Generated code compiles successfully
- Generated projects build without errors
- Dependencies resolve correctly
- No syntax errors in generated code

### Deterministic Outputs
- Same inputs produce identical outputs
- No timestamps or random values in generated code
- Snapshot tests pass consistently
- Tests use fixed seeds

### Errors Work Correctly
- Error messages are clear and actionable
- Errors propagate through layers correctly
- Error types are preserved
- Error handling doesn't swallow errors

### Performance Meets SLOs
- Build times meet targets
- Runtime performance acceptable
- Memory usage within limits
- No performance regressions

## Gaps to Close Checklist

### Test Coverage
- [ ] All public APIs have tests
- [ ] All error paths tested
- [ ] All edge cases covered
- [ ] Integration points verified
- [ ] User journeys tested end-to-end

### Test Quality
- [ ] No flaky tests
- [ ] Tests are independent
- [ ] Tests verify observable outputs
- [ ] Tests are deterministic
- [ ] Tests run in reasonable time

### Test Organization
- [ ] Unit tests colocated with code
- [ ] Integration tests in dedicated directory
- [ ] E2E tests cover critical paths
- [ ] Test helpers are reusable
- [ ] Test data is isolated

### Verification
- [ ] Generated code compiles
- [ ] Outputs are deterministic
- [ ] Errors propagate correctly
- [ ] Performance meets SLOs
- [ ] No test interdependencies

## Common Gaps to Fix

### Gap: Missing E2E Tests
**Problem**: No tests for complete user workflows
**Fix**: Add E2E tests for critical user journeys
**Verify**: Run `cargo test --test e2e_v2_validation`

### Gap: Flaky Tests
**Problem**: Tests fail intermittently
**Fix**: Use fixed seeds, mock external dependencies, ensure test isolation
**Verify**: Tests pass consistently on multiple runs

### Gap: Untested Error Paths
**Problem**: Error handling not tested
**Fix**: Add tests for all error cases
**Verify**: Error paths execute in tests

### Gap: Non-Deterministic Outputs
**Problem**: Generated code differs between runs
**Fix**: Remove timestamps, use fixed seeds, ensure deterministic processing
**Verify**: Same inputs produce identical outputs

### Gap: Missing Integration Tests
**Problem**: Components tested in isolation but not together
**Fix**: Add integration tests for layer interactions
**Verify**: Components work together correctly

## Testing Principles

### Test Real Code
- Use actual implementations, not mocks
- Test complete workflows, not isolated functions
- Verify actual outputs and side effects
- Only mock external APIs

### Verify Behavior
- Test what code does, not how it does it
- Verify observable outputs and state changes
- Test user-visible behavior
- Avoid testing implementation details

### Close Gaps
- Identify missing test coverage
- Fix flaky tests
- Add tests for untested paths
- Verify error handling works

## Commands for Gap Analysis

```bash
# Check test coverage
cargo make test-coverage

# Run tests multiple times to find flaky tests
for i in {1..10}; do cargo make test; done

# Run single-threaded to identify race conditions
cargo make test-single-threaded

# Run specific test suite
cargo test --test e2e_v2_validation
cargo test --test integration
```

