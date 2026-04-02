<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Andon + Gemba Walk Playbook](#andon--gemba-walk-playbook)
  - [Overview](#overview)
  - [Quick Start](#quick-start)
    - [Run Andon Monitoring](#run-andon-monitoring)
    - [Run Gemba Walk Inspection](#run-gemba-walk-inspection)
  - [Alert Levels](#alert-levels)
    - [🚨 Red Alert (Critical)](#-red-alert-critical)
    - [⚠️ Yellow Alert (Warning)](#-yellow-alert-warning)
    - [✅ Green (All Clear)](#-green-all-clear)
  - [Gemba Walk Checklist](#gemba-walk-checklist)
    - [1. Real Implementation Usage](#1-real-implementation-usage)
    - [2. Assertion Clarity](#2-assertion-clarity)
    - [3. Bug Detection](#3-bug-detection)
    - [4. Setup/Teardown Clarity](#4-setupteardown-clarity)
    - [5. Debug-ability](#5-debug-ability)
    - [6. Performance](#6-performance)
    - [7. Isolation](#7-isolation)
    - [8. Reproducibility](#8-reproducibility)
  - [Andon Triggers](#andon-triggers)
    - [Automated Detection](#automated-detection)
    - [Manual Triggers](#manual-triggers)
  - [Remediation Playbooks](#remediation-playbooks)
    - [Compilation Failure](#compilation-failure)
    - [Test Timeout](#test-timeout)
    - [Flaky Test](#flaky-test)
    - [Memory Leak](#memory-leak)
  - [Continuous Monitoring](#continuous-monitoring)
    - [CI Integration](#ci-integration)
    - [Local Development](#local-development)
  - [Metrics & Thresholds](#metrics--thresholds)
  - [Best Practices](#best-practices)
    - [Writing Andon-Friendly Tests](#writing-andon-friendly-tests)
    - [Conducting Gemba Walks](#conducting-gemba-walks)
    - [Responding to Alerts](#responding-to-alerts)
  - [Tools & Utilities](#tools--utilities)
    - [Rust Test Utilities](#rust-test-utilities)
    - [Gemba Walk Inspector](#gemba-walk-inspector)
  - [References](#references)
  - [Memory Location](#memory-location)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Andon + Gemba Walk Playbook

## Overview

This playbook implements Lean Manufacturing principles for test quality:

- **Andon**: Immediate visual/loud alerts on test failures (stop the line)
- **Gemba Walk**: Inspect tests where they actually run (go see reality)

## Quick Start

### Run Andon Monitoring

```bash
# Full monitoring suite
./scripts/andon_monitor.sh

# With flaky test detection (slower)
CHECK_FLAKY=true ./scripts/andon_monitor.sh

# With memory leak detection (requires valgrind)
CHECK_MEMORY=true ./scripts/andon_monitor.sh
```

### Run Gemba Walk Inspection

```bash
# Inspect integration tests
./scripts/gemba_walk.sh tests/integration

# With actual test runs observed
OBSERVE_RUN=true ./scripts/gemba_walk.sh tests/integration

# With code interviews
INTERVIEW_CODE=true ./scripts/gemba_walk.sh tests/integration
```

## Alert Levels

### 🚨 Red Alert (Critical)

**Stop immediately and fix:**

1. **Compilation failures**
   - Syntax errors
   - Missing dependencies
   - Type errors

2. **Test timeouts (> 30s)**
   - Infinite loops
   - Deadlocks
   - Hanging operations

3. **High failure rate (> 5%)**
   - Multiple tests failing
   - Systemic issues
   - Breaking changes

4. **Memory leaks**
   - Unclosed resources
   - Missing Drop implementations
   - Circular references

### ⚠️ Yellow Alert (Warning)

**Monitor and investigate:**

1. **Flaky tests**
   - Intermittent failures
   - Race conditions
   - Non-deterministic behavior

2. **Moderate failure rate (2-5%)**
   - Some tests failing
   - Isolated issues
   - Edge case failures

3. **Performance degradation**
   - Tests slowing down
   - Resource consumption increasing

### ✅ Green (All Clear)

- All tests passing
- Performance within thresholds
- No flaky tests detected
- Clean compilation

## Gemba Walk Checklist

### 1. Real Implementation Usage

**Check:** Are tests using real code or mocks?

```bash
# Count mock usage
grep -c "mock\|Mock\|stub" test_file.rs
```

**Good:** < 5 mock usages per file
**Bad:** Heavy mocking hiding real behavior

**Remediation:**
- Replace mocks with real implementations
- Use test fixtures instead of stubs
- Mock only at system boundaries

### 2. Assertion Clarity

**Check:** Do assertions provide clear failure messages?

```bash
# Check assertion types
grep "assert!" test_file.rs
grep "assert_eq!" test_file.rs
```

**Good:** More `assert_eq!` than `assert!`
**Bad:** Many bare `assert!()` calls

**Remediation:**
```rust
// ❌ Bad
assert!(result.is_ok());

// ✅ Good
assert_eq!(result, Ok(expected_value), "Failed to process input");
```

### 3. Bug Detection

**Check:** Do tests verify behavior or just syntax?

**Good signs:**
- Edge case testing
- Error handling coverage
- Boundary condition checks
- Integration scenarios

**Bad signs:**
- Only happy path tests
- Trivial assertions
- Tautological tests

**Remediation:**
- Add error case tests
- Test boundary conditions
- Verify actual behavior, not implementation

### 4. Setup/Teardown Clarity

**Check:** Is test setup clear and explicit?

**Good:**
```rust
fn setup() -> TestContext {
    TestContext::new()
        .with_database()
        .with_cache()
}
```

**Bad:**
- Hidden global state
- Unclear initialization
- Missing cleanup

### 5. Debug-ability

**Check:** Can failures be debugged quickly?

**Good:**
```rust
assert_eq!(
    actual, expected,
    "User creation failed: expected {expected:?}, got {actual:?}"
);
```

**Bad:**
- Generic error messages
- No context in failures
- Hard to reproduce

### 6. Performance

**Check:** Do tests run quickly?

**Threshold:** < 30s total test time

**Remediation:**
- Parallelize tests
- Reduce setup overhead
- Mock expensive operations (at boundaries only)

### 7. Isolation

**Check:** Do tests run independently?

**Red flags:**
- `static mut` variables
- Shared global state
- Test order dependencies

**Remediation:**
- Use test fixtures
- Reset state between tests
- Avoid shared mutable state

### 8. Reproducibility

**Check:** Do tests pass consistently?

**Test:**
```bash
# Run test 3 times
for i in {1..3}; do cargo test test_name; done
```

**Good:** 3/3 passes or 0/3 passes
**Bad:** Flaky (1/3 or 2/3 passes)

## Andon Triggers

### Automated Detection

```yaml
# .github/workflows/andon_ci.yml
- Red Alert: Compilation failure
- Red Alert: Test timeout (> 120s in CI)
- Red Alert: Failure rate > 5%
- Yellow Alert: Flaky tests detected
- Yellow Alert: Clippy warnings > 10
```

### Manual Triggers

```bash
# Trigger red alert manually
./scripts/andon_monitor.sh || echo "Red alert triggered"

# Check specific test
cargo test test_name --quiet || andon_red_alert "test_name failed"
```

## Remediation Playbooks

### Compilation Failure

1. Review error messages
2. Check recent changes
3. Verify dependencies in Cargo.toml
4. Run `cargo check` incrementally
5. Fix syntax/type errors
6. Re-run build

### Test Timeout

1. Identify hanging test
2. Check for infinite loops
3. Review async operations
4. Add timeout to operations
5. Profile with `cargo flamegraph`
6. Optimize or split test

### Flaky Test

1. Run test 10 times locally
2. Look for race conditions:
   - Shared mutable state
   - Unordered async operations
   - Timing dependencies
3. Add synchronization:
   - Mutex/RwLock
   - Barriers
   - Channels
4. Make assertions deterministic
5. Verify fix with repeated runs

### Memory Leak

1. Run with valgrind:
   ```bash
   valgrind --leak-check=full cargo test
   ```
2. Identify leaked resources
3. Check Drop implementations
4. Look for circular Rc/Arc
5. Fix resource cleanup
6. Re-run valgrind

## Continuous Monitoring

### CI Integration

The `.github/workflows/andon_ci.yml` runs on every push:

- ✅ Compilation check (Red alert on failure)
- ✅ Test execution (Red alert on timeout)
- ✅ Failure rate monitoring (Red/Yellow thresholds)
- ✅ Flaky test detection (Yellow alert)
- ✅ Health dashboard generation

### Local Development

```bash
# Run before commit
./scripts/andon_monitor.sh

# Run on file changes (requires cargo-watch)
cargo watch -x "run --bin andon_monitor"
```

## Metrics & Thresholds

| Metric | Green | Yellow | Red |
|--------|-------|--------|-----|
| Failure Rate | 0% | 2-5% | > 5% |
| Flaky Rate | 0% | 1-2% | > 2% |
| Test Time | < 10s | 10-30s | > 30s |
| Compilation | Pass | Warnings | Errors |
| Memory Leaks | 0 | - | Any |

## Best Practices

### Writing Andon-Friendly Tests

1. **Fast feedback**: Tests should fail quickly
2. **Clear messages**: Assertion messages explain what failed
3. **Isolated**: No shared state between tests
4. **Deterministic**: Same input = same output
5. **Maintainable**: Easy to understand and update

### Conducting Gemba Walks

1. **Go see**: Run tests in their actual environment
2. **Observe**: Watch tests fail in real-time
3. **Ask why**: Understand test intent from code
4. **Measure**: Time actual execution
5. **Improve**: Refactor based on observations

### Responding to Alerts

**Red Alert Response:**
1. Stop current work
2. Investigate immediately
3. Fix root cause (not symptoms)
4. Verify fix with tests
5. Document learnings

**Yellow Alert Response:**
1. Create tracking issue
2. Investigate when available
3. Monitor for escalation
4. Fix during normal work
5. Review patterns

## Tools & Utilities

### Rust Test Utilities

```rust
// In tests/lean_quality/andon_system.rs
use ggen::lean_quality::andon_system::*;

let mut dashboard = TestHealthDashboard::new();
dashboard.record_test("test1", true);
dashboard.status(); // Returns AndonSignal
```

### Gemba Walk Inspector

```rust
// In tests/lean_quality/gemba_walk.rs
use ggen::lean_quality::gemba_walk::*;

let checklist = GembaWalkInspector::inspect(
    PathBuf::from("tests/my_test.rs")
);
checklist.print_report();
```

## References

- Lean Manufacturing: Andon cord concept
- Gemba Walk: Management by wandering around
- Test-Driven Development: Red-Green-Refactor
- Continuous Integration: Fast feedback loops

## Memory Location

Store all learnings: `swarm/lean/andon_gemba_walk_system`

---

**Remember:** Andon stops the line. Gemba goes to see. Both prevent defects from escaping.
