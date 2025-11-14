# Module Test Checklist Template

**Purpose**: Use this template to track test coverage for each module in ggen.

**Usage**:
1. Copy this template to `crates/[crate]/tests/MODULE_TEST_CHECKLIST_[module].md`
2. Fill in module-specific details
3. Update checklist as tests are added
4. Review monthly and update coverage metrics

---

# Module Test Checklist: [MODULE_NAME]

**Module**: `crates/[crate]/src/[module]`
**Owner**: [Team/Person]
**Created**: [Date]
**Last Updated**: [Date]
**Status**: 🔴 Red / 🟡 Yellow / 🟢 Green

---

## Executive Summary

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| **Overall Coverage** | X% | 85% | 🔴 / 🟡 / 🟢 |
| **Line Coverage** | X% | 85% | 🔴 / 🟡 / 🟢 |
| **Branch Coverage** | X% | 75% | 🔴 / 🟡 / 🟢 |
| **Integration Coverage** | X% | 75% | 🔴 / 🟡 / 🟢 |
| **Mutation Score** | X% | 80% | 🔴 / 🟡 / 🟢 |

**Status Colors**:
- 🔴 **Red**: <70% (Critical - blocks release)
- 🟡 **Yellow**: 70-85% (Needs improvement)
- 🟢 **Green**: ≥85% (Meets standard)

---

## Test Inventory

### Unit Tests

**File**: `crates/[crate]/tests/unit/[module]_tests.rs`

**Status**: ❌ Missing / ⏳ In Progress / ✅ Complete

**Test Cases**:

- [ ] `test_[module]_creation_with_valid_input_succeeds`
  - **Purpose**: Verify module can be created with valid input
  - **Coverage**: Constructor/builder pattern
  - **Chicago TDD**: ✅ Real objects, state verification

- [ ] `test_[module]_operation_with_invalid_input_fails`
  - **Purpose**: Verify error handling for invalid input
  - **Coverage**: Input validation
  - **Chicago TDD**: ✅ Real error cases

- [ ] `test_[module]_async_operation_completes_successfully`
  - **Purpose**: Verify async operations complete
  - **Coverage**: Async methods
  - **Chicago TDD**: ✅ Config-based timeout

- [ ] `test_[module]_edge_case_[description]`
  - **Purpose**: [Describe edge case]
  - **Coverage**: Edge case handling
  - **Chicago TDD**: ✅ Real edge case data

- [ ] `test_[module]_error_propagation_is_correct`
  - **Purpose**: Verify errors propagate correctly
  - **Coverage**: Error handling chain
  - **Chicago TDD**: ✅ Real error scenarios

**Total Unit Tests**: X / 10 minimum

---

### Integration Tests

**File**: `crates/[crate]/tests/integration/[module]_integration.rs`

**Status**: ❌ Missing / ⏳ In Progress / ✅ Complete

**Test Cases**:

- [ ] `test_[module]_full_workflow_succeeds`
  - **Purpose**: Verify complete workflow end-to-end
  - **Coverage**: Full integration path
  - **Chicago TDD**: ✅ Real workflow, no mocks

- [ ] `test_[module]_cross_module_interaction_with_[other_module]`
  - **Purpose**: Verify integration with dependent modules
  - **Coverage**: Module boundaries
  - **Chicago TDD**: ✅ Real module instances

- [ ] `test_[module]_api_contract_validation`
  - **Purpose**: Verify public API contract
  - **Coverage**: Public interface
  - **Chicago TDD**: ✅ Real API calls

- [ ] `test_[module]_concurrent_operations_are_safe`
  - **Purpose**: Verify thread safety
  - **Coverage**: Concurrency handling
  - **Chicago TDD**: ✅ Real concurrent execution

**Total Integration Tests**: X / 5 minimum

---

### Property Tests

**File**: `crates/[crate]/tests/property/[module]_properties.rs`

**Status**: ❌ Missing / ⏳ In Progress / ✅ Complete

**Test Cases**:

- [ ] `property_[module]_round_trip_consistency`
  - **Purpose**: Verify serialize/deserialize round-trip
  - **Coverage**: Data transformation
  - **Property**: `∀x: deserialize(serialize(x)) = x`

- [ ] `property_[module]_operation_is_idempotent`
  - **Purpose**: Verify idempotency
  - **Coverage**: State modification
  - **Property**: `∀x: f(f(x)) = f(x)`

- [ ] `property_[module]_invariant_holds`
  - **Purpose**: Verify module invariants
  - **Coverage**: Internal consistency
  - **Property**: `∀x: invariant(x) = true`

**Total Property Tests**: X / 3 minimum

---

### Security Tests

**File**: `crates/[crate]/tests/security/[module]_security.rs`

**Status**: ❌ Missing / ⏳ In Progress / ✅ Complete

**Test Cases**:

- [ ] `test_[module]_input_validation_prevents_injection`
  - **Purpose**: Verify injection prevention
  - **Coverage**: Input sanitization
  - **Chicago TDD**: ✅ Real attack vectors

- [ ] `test_[module]_handles_malformed_input_safely`
  - **Purpose**: Verify graceful degradation
  - **Coverage**: Error handling
  - **Chicago TDD**: ✅ Real malformed data

- [ ] `test_[module]_authentication_is_enforced`
  - **Purpose**: Verify auth requirements
  - **Coverage**: Security constraints
  - **Chicago TDD**: ✅ Real auth scenarios

**Total Security Tests**: X / 3 minimum

---

### Performance Tests

**File**: `benches/[module]_benchmarks.rs`

**Status**: ❌ Missing / ⏳ In Progress / ✅ Complete

**Benchmarks**:

- [ ] `bench_[module]_hot_path_operation`
  - **Purpose**: Measure hot path performance
  - **Baseline**: [X] ms/op
  - **SLA**: <[Y] ms/op

- [ ] `bench_[module]_memory_usage`
  - **Purpose**: Measure memory allocation
  - **Baseline**: [X] MB
  - **SLA**: <[Y] MB

**Total Benchmarks**: X / 2 minimum

---

## Chicago TDD Compliance

### ✅ Required Standards

- [ ] **All tests use Chicago TDD macros**
  - `test!()` for synchronous tests
  - `async_test!()` for async tests
  - No `#[test]` or `#[tokio::test]`

- [ ] **AAA pattern enforced**
  - Explicit `// Arrange` comments
  - Explicit `// Act` comments
  - Explicit `// Assert` comments

- [ ] **Real objects used**
  - Minimal mocking (only for external services)
  - Real dependencies wherever possible
  - State-based verification

- [ ] **Config-based timeouts**
  - Use `integration_timeout()` from `test_config`
  - No hardcoded timeout values
  - Use `async_test!()` (auto-timeout) over `async_test_with_timeout!()`

- [ ] **No test data in root folder**
  - All test data in `tests/fixtures/`
  - All sample data in `tests/data/`
  - No `.json`, `.toml`, `.ttl` in project root

---

## Code Coverage Details

### Covered Lines

**Total Lines**: [X]
**Covered Lines**: [Y]
**Coverage**: [Y/X * 100]%

### Uncovered Areas

1. **[Function/Module]**: Lines [X-Y]
   - **Reason**: [Why uncovered]
   - **Action**: [Plan to cover]

2. **[Function/Module]**: Lines [X-Y]
   - **Reason**: [Why uncovered]
   - **Action**: [Plan to cover]

### Missing Tests

**Critical Gaps** (must fix before release):

1. **[Test Name]**
   - **Description**: [What needs testing]
   - **Priority**: 🔴 High / 🟡 Medium / 🟢 Low
   - **Estimated Effort**: [Hours/Days]

2. **[Test Name]**
   - **Description**: [What needs testing]
   - **Priority**: 🔴 High / 🟡 Medium / 🟢 Low
   - **Estimated Effort**: [Hours/Days]

---

## Test Quality Metrics

### Test Execution

| Metric | Value | Target | Status |
|--------|-------|--------|--------|
| **Total Tests** | X | ≥20 | 🔴 / 🟡 / 🟢 |
| **Avg Test Time** | Xms | <50ms | 🔴 / 🟡 / 🟢 |
| **Max Test Time** | Xms | <500ms | 🔴 / 🟡 / 🟢 |
| **Flaky Tests** | X | 0 | 🔴 / 🟡 / 🟢 |

### Test Reliability

- **Last 30 Runs**: X/30 passed (X% reliability)
- **Flaky Tests Identified**: [List any flaky tests]
- **Action Items**: [Plans to fix flaky tests]

---

## Dependencies and Blockers

### Test Dependencies

**Required Modules**:
- `[module-1]`: [Why needed]
- `[module-2]`: [Why needed]

**External Services**:
- `[service-1]`: [Why needed, how mocked]
- `[service-2]`: [Why needed, how mocked]

### Current Blockers

1. **[Blocker Description]**
   - **Impact**: [How it blocks testing]
   - **Owner**: [Who is responsible]
   - **ETA**: [When will it be resolved]

2. **[Blocker Description]**
   - **Impact**: [How it blocks testing]
   - **Owner**: [Who is responsible]
   - **ETA**: [When will it be resolved]

---

## Action Items

### Immediate (This Week)

- [ ] **[Action 1]**
  - **Owner**: [Person]
  - **Due**: [Date]

- [ ] **[Action 2]**
  - **Owner**: [Person]
  - **Due**: [Date]

### Short-Term (This Month)

- [ ] **[Action 1]**
  - **Owner**: [Person]
  - **Due**: [Date]

- [ ] **[Action 2]**
  - **Owner**: [Person]
  - **Due**: [Date]

### Long-Term (This Quarter)

- [ ] **[Action 1]**
  - **Owner**: [Person]
  - **Due**: [Date]

---

## Review History

| Date | Reviewer | Coverage | Status | Notes |
|------|----------|----------|--------|-------|
| [Date] | [Name] | X% | 🔴 / 🟡 / 🟢 | [Review notes] |
| [Date] | [Name] | X% | 🔴 / 🟡 / 🟢 | [Review notes] |

---

## References

- **Source Code**: `crates/[crate]/src/[module]`
- **Test Code**: `crates/[crate]/tests/unit/[module]_tests.rs`
- **Documentation**: `docs/modules/[module].md`
- **Architecture**: `docs/architecture/TESTING_ARCHITECTURE.md`

---

## Notes

[Add any additional notes, context, or special considerations for this module]

---

**Checklist Status**: ⏳ In Progress
**Next Review**: [Date]
**Owner**: [Team/Person]
