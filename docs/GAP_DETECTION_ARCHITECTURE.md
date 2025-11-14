# Gap Detection Architecture

**Design document for automated test gap detection system.**

## System Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Developer Workflow                        │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
        ┌────────────────────────────────────────┐
        │         git add / git commit           │
        └────────────────────────────────────────┘
                            │
                            ▼
        ┌────────────────────────────────────────┐
        │      Pre-Commit Hook (Fast Check)      │
        │  - Unwrap/expect/TODO validation       │
        │  - Test coverage enforcement           │
        │  - Clippy + format check               │
        │  Target: <5 seconds                    │
        └────────────────────────────────────────┘
                            │
                    ┌───────┴────────┐
                    │   Pass?        │
                    └───────┬────────┘
                   No       │       Yes
                    │       │        │
                    ▼       │        ▼
            [Block Commit]  │  [Allow Commit]
                            │
                            ▼
        ┌────────────────────────────────────────┐
        │            git push                    │
        └────────────────────────────────────────┘
                            │
                            ▼
        ┌────────────────────────────────────────┐
        │   Pre-Push Hook (Comprehensive Check) │
        │  - Full gap detection (7 rules)       │
        │  - Complete test suite                 │
        │  Target: 30-60 seconds                 │
        └────────────────────────────────────────┘
                            │
                    ┌───────┴────────┐
                    │   Pass?        │
                    └───────┬────────┘
                   No       │       Yes
                    │       │        │
                    ▼       │        ▼
            [Block Push]    │   [Allow Push]
                            │
                            ▼
        ┌────────────────────────────────────────┐
        │         GitHub Actions CI              │
        │  - cargo make ci                       │
        │  - Includes detect-gaps task           │
        │  Target: 60-90 seconds                 │
        └────────────────────────────────────────┘
```

## Component Architecture

### 1. Script Layer

#### `scripts/detect-test-gaps.sh`
**Purpose**: Comprehensive gap detection
**Type**: Bash script
**Dependencies**: cargo, grep, find

**Algorithm**:
```bash
for each critical_crate in [ggen-core, ggen-domain, ggen-cli, ggen-marketplace]:
    # Gap 1: Missing unit tests
    for each src_file in crate/src/**/*.rs:
        if has_public_items(src_file):
            if not has_inline_tests(src_file):
                if not has_unit_test_file(src_file):
                    report_missing_test(src_file)

    # Gap 2: Missing integration tests
    if not exists(crate/tests/*.rs):
        report_missing_integration(crate)

    # Gap 3: Test quality
    for each test_file in crate/tests/**/*.rs:
        if has_test_functions(test_file):
            if not has_assertions(test_file):
                report_low_quality(test_file)

    # Gap 4: Error handling
    result_count = count_result_types(crate/src)
    error_test_count = count_error_tests(crate/tests)
    if error_test_count < (result_count / 5):
        report_insufficient_error_tests(crate)

    # Gap 5: Untested functions
    for each public_function in crate/src:
        if not tested_in(public_function, crate/tests):
            report_untested_function(public_function)

    # Gap 6-7: Compilation checks
    run_cargo_check()
    run_cargo_test_no_run()

generate_json_report()
```

**Output**: JSON report at `target/gap-detection-report.json`

#### `scripts/enforce-test-coverage.sh`
**Purpose**: Fast coverage enforcement for staged files
**Type**: Bash script
**Dependencies**: git

**Algorithm**:
```bash
staged_files = git_diff_cached(filter: "*.rs", exclude: "tests/")

for each file in staged_files:
    if not in_crate(file):
        continue

    if has_public_items(file):
        if not has_inline_tests(file):
            if not has_unit_test_file(file):
                report_missing_coverage(file)
                exit(1)

exit(0)
```

**Output**: Error messages to stderr, exit code 0/1

#### `scripts/install-git-hooks.sh`
**Purpose**: Install git hooks with gap detection
**Type**: Bash script
**Dependencies**: git, chmod

**Algorithm**:
```bash
# Install pre-commit hook
cat > .git/hooks/pre-commit << 'EOF'
    run_main_validation()
    run_test_coverage_enforcement()
EOF

# Install pre-push hook
cat > .git/hooks/pre-push << 'EOF'
    run_gap_detection()
    run_full_test_suite()
EOF

chmod +x .git/hooks/*
```

### 2. Integration Layer

#### Makefile.toml Tasks

```toml
[tasks.detect-gaps]
# Runs scripts/detect-test-gaps.sh
# Called by: CI pipeline, manual validation
# Duration: 30-60s

[tasks.enforce-coverage]
# Runs scripts/enforce-test-coverage.sh
# Called by: Pre-commit hook
# Duration: <5s

[tasks.install-hooks]
# Runs scripts/install-git-hooks.sh
# Called by: Developer setup
# Duration: <1s

[tasks.gap-report]
# Displays JSON report
# Called by: Manual validation
# Duration: <1s

[tasks.ci]
dependencies = [
  "format",
  "clippy-ci-flow",
  "check-all-crates",
  "detect-gaps",  # ← Gap detection integrated
  "test",
  "audit-all",
  "docs-test",
]
```

#### Git Hook Integration

```bash
# Pre-commit flow
.git/hooks/pre-commit
├── Run git_hook_pre_commit (Rust binary)
│   ├── Check unwrap/expect/TODO
│   ├── Check formatting
│   └── Check clippy
└── Run scripts/enforce-test-coverage.sh
    └── Check test coverage for staged files

# Pre-push flow
.git/hooks/pre-push
├── Run scripts/detect-test-gaps.sh
│   └── 7 gap detection rules
└── Run cargo test --workspace
    └── Full test suite
```

### 3. Data Layer

#### Gap Detection Report Schema

```json
{
  "timestamp": "ISO 8601",
  "gaps_detected": {
    "missing_unit_tests": integer,
    "missing_integration_tests": integer,
    "low_quality_tests": integer,
    "missing_error_tests": integer,
    "untested_public_functions": integer
  },
  "summary": {
    "total_errors": integer,
    "total_warnings": integer,
    "status": "PASS" | "FAIL"
  }
}
```

**Location**: `target/gap-detection-report.json`
**Format**: JSON
**Consumers**: CI systems, developers, monitoring

## Detection Rules Engine

### Rule 1: Missing Unit Tests
**Trigger**: Public module without tests
**Severity**: Error
**Algorithm**:
1. Find all `*.rs` files in `src/` (exclude test files)
2. Check for `pub fn/struct/enum/trait/mod`
3. Check for inline `#[cfg(test)]` module
4. Check for unit test file in `tests/unit/`
5. If none found, report error

### Rule 2: Missing Integration Tests
**Trigger**: Crate without integration tests
**Severity**: Warning
**Algorithm**:
1. For each critical crate
2. Check for `tests/*.rs` (exclude `tests/unit/`)
3. If count < 1, report warning

### Rule 3: Test Quality
**Trigger**: Test file without assertions
**Severity**: Warning
**Algorithm**:
1. For each test file
2. Count `#[test]` functions
3. Count assertions (assert!, assert_eq!, etc.)
4. If tests > 0 && assertions == 0, report warning

### Rule 4: Error Handling Coverage
**Trigger**: Insufficient error path tests
**Severity**: Warning
**Algorithm**:
1. Count `Result<` types in `src/`
2. Count error tests (`#[should_panic]`, `Err(`)
3. If error_tests < (result_count / 5), report warning

### Rule 5: Untested Public Functions
**Trigger**: Public function not called in tests
**Severity**: Error (if >5)
**Algorithm**:
1. Extract public function names from `src/`
2. Search for function name in `tests/`
3. Count untested functions
4. If count > 5, report error

### Rule 6-7: Compilation Checks
**Trigger**: Compilation errors
**Severity**: Error
**Algorithm**:
1. Run `cargo check --workspace --all-targets`
2. Run `cargo test --workspace --no-run`
3. If either fails, report error

## Performance Optimization

### Fast Path (Pre-Commit)
- **Target**: <5 seconds
- **Strategy**: Only check staged files
- **Optimizations**:
  - Skip non-Rust files early
  - Skip test files (no coverage needed)
  - Per-package clippy (not workspace)
  - Parallel checks where possible

### Comprehensive Path (Pre-Push)
- **Target**: 30-60 seconds
- **Strategy**: Check all critical crates
- **Optimizations**:
  - Focus on 4 critical crates
  - 80/20 thresholds (not exhaustive)
  - Parallel crate processing
  - Cached cargo check results

### CI Path
- **Target**: 60-90 seconds
- **Strategy**: Full validation
- **Optimizations**:
  - Cargo incremental builds
  - Cached dependencies
  - Parallel test execution

## Error Handling

### Script Failures
```bash
set -euo pipefail  # Fail fast on errors

# Graceful degradation
if ! command -v jq &> /dev/null; then
    warn "jq not found, skipping JSON report"
fi
```

### Git Hook Failures
```bash
# Pre-commit: Block commit
exit 1

# Pre-push: Block push
exit 1

# Manual override
git commit --no-verify
```

### CI Failures
```yaml
# GitHub Actions
- run: cargo make ci
  # Includes gap detection
  # Fails PR if gaps detected
```

## Extensibility

### Adding New Detection Rule

```bash
# Edit scripts/detect-test-gaps.sh
log "Checking for [new rule]..."
NEW_RULE_COUNT=0

for crate in "${CRITICAL_CRATES[@]}"; do
    # Detection logic
    if [condition]; then
        warn "Found issue in $crate"
        ((NEW_RULE_COUNT++))
    fi
done

if [ $NEW_RULE_COUNT -gt 0 ]; then
    error "Found $NEW_RULE_COUNT issues"
else
    success "Check passed"
fi

# Add to JSON report
cat > "$REPORT_FILE" << EOF
{
  "gaps_detected": {
    ...
    "new_rule_violations": $NEW_RULE_COUNT
  }
}
EOF
```

### Adding New Critical Crate

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

### Custom Thresholds

```bash
# Edit scripts/detect-test-gaps.sh
MIN_COVERAGE_PERCENT=70  # Increase from 60%
MIN_UNIT_TESTS_PER_MODULE=2  # Increase from 1
UNTESTED_FUNCTIONS_THRESHOLD=3  # Decrease from 5
```

## Monitoring and Metrics

### Key Metrics
- **Gap Detection Rate**: Gaps found per 100 commits
- **False Positive Rate**: Incorrectly flagged files
- **Performance**: Hook execution time
- **Coverage Trend**: Coverage % over time

### Tracking
```json
{
  "timestamp": "2025-11-14T03:52:17Z",
  "metrics": {
    "gaps_detected": 10,
    "commits_checked": 100,
    "detection_rate": "10%",
    "avg_pre_commit_time": "3.2s",
    "avg_pre_push_time": "42s"
  }
}
```

## Security Considerations

1. **Script Injection**: All scripts use `set -euo pipefail`
2. **Path Traversal**: Scripts validate file paths
3. **Secrets Exposure**: No secrets in scripts or reports
4. **Resource Limits**: Timeouts prevent infinite loops

## Future Enhancements

1. **Coverage Percentage**: Integrate with cargo-tarpaulin
2. **Historical Trends**: Track gaps over time
3. **Auto-Fix**: Generate test stubs automatically
4. **IDE Integration**: VS Code extension
5. **Custom Rules**: Per-project configuration
