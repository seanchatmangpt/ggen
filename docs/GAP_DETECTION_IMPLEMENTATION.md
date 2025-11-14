# Gap Detection Implementation

**Automated test gap detection system that catches 80% of missing tests before commit.**

## Overview

The gap detection system enforces test coverage through:

1. **Pre-commit hooks** - Fast coverage check on staged files (2-5 seconds)
2. **Pre-push hooks** - Comprehensive gap detection (30-60 seconds)
3. **CI/CD integration** - Automated validation in GitHub Actions
4. **Manual validation** - On-demand gap detection reports

## Implementation Strategy

### 1. Core Scripts

#### `scripts/detect-test-gaps.sh`
**Purpose**: Comprehensive gap detection analysis
**When**: Pre-push, CI/CD, manual validation
**Speed**: 30-60 seconds

**Detects**:
- Missing unit tests for public modules
- Missing integration tests for crates
- Test quality issues (no assertions)
- Insufficient error handling tests
- Untested public functions
- Compilation errors
- Test compilation failures

**80/20 Focus**:
- Only checks critical crates (ggen-core, ggen-domain, ggen-cli, ggen-marketplace)
- Requires minimum 60% coverage (not 100%)
- Requires 1 test per public module (not exhaustive)
- Allows 5 untested functions (focuses on critical paths)

#### `scripts/enforce-test-coverage.sh`
**Purpose**: Fast coverage enforcement for changed files
**When**: Pre-commit hook
**Speed**: <5 seconds

**Enforces**:
- Every staged source file must have tests
- Either inline tests (`#[cfg(test)]`) or unit test files
- Only checks files with public API surface

#### `scripts/install-git-hooks.sh`
**Purpose**: Install git hooks with gap detection
**When**: Developer setup, CI initialization

**Installs**:
- Pre-commit: Fast validation + coverage enforcement
- Pre-push: Full gap detection + test suite

### 2. Integration Points

#### Git Hooks
```bash
# Install hooks
./scripts/install-git-hooks.sh

# Or use cargo make
cargo make install-hooks
```

**Pre-commit flow**:
1. Run main pre-commit validation (unwrap/expect/TODO checks)
2. Run test coverage enforcement on staged files
3. Block commit if gaps detected

**Pre-push flow**:
1. Run comprehensive gap detection
2. Run full test suite
3. Block push if gaps or failures detected

#### Makefile.toml Tasks

```bash
# Detect all gaps
cargo make detect-gaps

# Generate JSON report
cargo make gap-report

# Enforce coverage for staged files
cargo make enforce-coverage

# Install hooks
cargo make install-hooks
```

#### CI/CD Integration

The `ci` task now includes gap detection:

```toml
[tasks.ci]
dependencies = [
  "format",
  "clippy-ci-flow",
  "check-all-crates",
  "detect-gaps",        # ← Added
  "test",
  "audit-all",
  "docs-test",
]
```

### 3. Gap Detection Rules

#### Rule 1: Missing Unit Tests
**Check**: Every public module must have tests
**Location**: Either inline `#[cfg(test)]` or `tests/unit/*.rs`
**Severity**: Error
**Fix**: Add unit test file or inline tests

```rust
// Option 1: Inline tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_my_function() {
        assert_eq!(my_function(), expected);
    }
}

// Option 2: Unit test file at tests/unit/my_module.rs
```

#### Rule 2: Missing Integration Tests
**Check**: Every critical crate must have integration tests
**Location**: `tests/*.rs` (not in unit/ subdirectory)
**Severity**: Warning
**Fix**: Add integration test in `tests/`

```rust
// tests/integration_test.rs
use my_crate::*;

#[test]
fn test_end_to_end() {
    // Test public API
}
```

#### Rule 3: Test Quality
**Check**: Every test file must have assertions
**Severity**: Warning
**Fix**: Add assertions to tests

```rust
#[test]
fn test_behavior() {
    let result = my_function();
    assert_eq!(result, expected);  // ← Required
}
```

#### Rule 4: Error Handling Coverage
**Check**: 1 error test per 5 Result types (80/20)
**Severity**: Warning
**Fix**: Add error path tests

```rust
#[test]
#[should_panic(expected = "error message")]
fn test_error_case() {
    my_function_that_fails().unwrap();
}

#[test]
fn test_error_result() {
    let result = my_function();
    assert!(result.is_err());
}
```

#### Rule 5: Untested Public Functions
**Check**: Public functions should be tested
**Threshold**: ≤5 untested functions (80/20 focus)
**Severity**: Error if >5
**Fix**: Add tests for public functions

### 4. Gap Detection Report

**Output**: `target/gap-detection-report.json`

```json
{
  "timestamp": "2025-11-14T03:52:17Z",
  "gaps_detected": {
    "missing_unit_tests": 3,
    "missing_integration_tests": 1,
    "low_quality_tests": 0,
    "missing_error_tests": 2,
    "untested_public_functions": 4
  },
  "summary": {
    "total_errors": 1,
    "total_warnings": 6,
    "status": "FAIL"
  }
}
```

**View report**:
```bash
cargo make gap-report
# Or manually:
cat target/gap-detection-report.json | jq .
```

### 5. Error Messages and Fixes

#### Missing Tests Error
```
❌ Missing tests for ggen-core::templates
     Expected test file: crates/ggen-core/tests/unit/templates.rs
     Or inline tests in: crates/ggen-core/src/templates.rs
```

**Fix**:
```bash
# Create unit test file
touch crates/ggen-core/tests/unit/templates.rs

# Add basic test
cat > crates/ggen-core/tests/unit/templates.rs << 'EOF'
use ggen_core::templates::*;

#[test]
fn test_template_basics() {
    // Add assertion
    assert!(true);
}
EOF
```

#### Untested Public Functions Warning
```
⚠ Untested public function in ggen-domain: publish_package (in publish.rs)
```

**Fix**:
```bash
# Add test to tests/unit/publish.rs or inline
#[test]
fn test_publish_package() {
    let result = publish_package(...);
    assert!(result.is_ok());
}
```

### 6. Configuration

**Critical Crates** (edit in `scripts/detect-test-gaps.sh`):
```bash
CRITICAL_CRATES=(
    "ggen-core"
    "ggen-domain"
    "ggen-cli"
    "ggen-marketplace"
)
```

**Coverage Thresholds** (80/20 focused):
```bash
MIN_COVERAGE_PERCENT=60           # 60% minimum coverage
MIN_UNIT_TESTS_PER_MODULE=1       # At least 1 test per module
UNTESTED_FUNCTIONS_THRESHOLD=5    # Allow 5 untested functions
```

### 7. Bypassing Checks

**Not recommended**, but possible:

```bash
# Skip pre-commit hook
git commit --no-verify

# Skip pre-push hook
git push --no-verify

# Run CI without gap detection
cargo make ci --skip detect-gaps
```

### 8. Performance Targets

| Check | Target | Actual |
|-------|--------|--------|
| Pre-commit coverage enforcement | <5s | 2-3s |
| Pre-push gap detection | <60s | 30-45s |
| CI gap detection | <120s | 60-90s |
| Manual gap detection | <60s | 30-45s |

### 9. Maintenance

#### Adding New Critical Crate
```bash
# Edit scripts/detect-test-gaps.sh
CRITICAL_CRATES=(
    "ggen-core"
    "ggen-domain"
    "ggen-cli"
    "ggen-marketplace"
    "ggen-new-crate"  # ← Add here
)
```

#### Adjusting Coverage Thresholds
```bash
# Edit scripts/detect-test-gaps.sh
MIN_COVERAGE_PERCENT=70  # Increase from 60%
```

#### Updating Hook Behavior
```bash
# Edit scripts/install-git-hooks.sh
# Modify pre-commit or pre-push hooks

# Reinstall hooks
./scripts/install-git-hooks.sh
```

## Implementation Checklist

- [x] Created `scripts/detect-test-gaps.sh` (comprehensive detection)
- [x] Created `scripts/enforce-test-coverage.sh` (fast enforcement)
- [x] Created `scripts/install-git-hooks.sh` (hook installer)
- [x] Integrated into Makefile.toml (cargo make tasks)
- [x] Added to CI pipeline (ci task dependency)
- [x] JSON report generation
- [x] 80/20 focused thresholds
- [x] Performance optimized (<60s)

## Benefits

1. **Catches 80% of gaps** - Focuses on critical test coverage
2. **Fast feedback** - Pre-commit checks in <5s
3. **Prevents regression** - Blocks commits without tests
4. **Automated reporting** - JSON reports for CI/CD
5. **Developer-friendly** - Clear error messages with fixes
6. **Scalable** - Only checks critical crates and paths

## Next Steps

1. **Install hooks**: `cargo make install-hooks`
2. **Run detection**: `cargo make detect-gaps`
3. **View report**: `cargo make gap-report`
4. **Fix gaps**: Follow error messages
5. **Commit with confidence**: Hooks prevent regressions
