<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Performance Budget Enforcement Research](#performance-budget-enforcement-research)
  - [Executive Summary](#executive-summary)
  - [1. Budget Classification](#1-budget-classification)
    - [Problem](#problem)
    - [Solution: Multi-Layer Classification](#solution-multi-layer-classification)
      - [**Primary Method: Test Location Convention** (80% coverage)](#primary-method-test-location-convention-80-coverage)
      - [**Secondary Method: Custom Attributes** (15% coverage)](#secondary-method-custom-attributes-15-coverage)
      - [**Tertiary Method: Naming Convention** (5% coverage)](#tertiary-method-naming-convention-5-coverage)
    - [Decision Matrix](#decision-matrix)
  - [2. Real-Time Monitoring](#2-real-time-monitoring)
    - [Problem](#problem-1)
    - [Solution: cargo-nextest + JSON Output](#solution-cargo-nextest--json-output)
      - [**Per-Test Timing Capture**](#per-test-timing-capture)
      - [**Real-Time Aggregation Script**](#real-time-aggregation-script)
      - [**Dashboard Integration** (Kaizen metrics)](#dashboard-integration-kaizen-metrics)
  - [3. Budget Violation Handling](#3-budget-violation-handling)
    - [Problem](#problem-2)
    - [Solution: Tiered Enforcement Strategy](#solution-tiered-enforcement-strategy)
      - [**Tier 1: Development (Warnings)**](#tier-1-development-warnings)
      - [**Tier 2: CI (Strict Enforcement)**](#tier-2-ci-strict-enforcement)
      - [**Tier 3: Pre-Commit (Incremental Enforcement)**](#tier-3-pre-commit-incremental-enforcement)
    - [Decision: Progressive Enforcement](#decision-progressive-enforcement)
  - [4. Timeout Enforcement](#4-timeout-enforcement)
    - [Problem](#problem-3)
    - [Solution: cargo-nextest Native Timeouts](#solution-cargo-nextest-native-timeouts)
      - [**Configuration**](#configuration)
      - [**Timeout Behavior**](#timeout-behavior)
      - [**Comparison: cargo test vs nextest**](#comparison-cargo-test-vs-nextest)
  - [5. CI/CD Integration](#5-cicd-integration)
    - [Problem](#problem-4)
    - [Solution: Nextest + GitHub Actions Integration](#solution-nextest--github-actions-integration)
      - [**GitHub Actions Workflow**](#github-actions-workflow)
      - [**Makefile.toml Integration**](#makefiletoml-integration)
  - [6. Progressive Enforcement](#6-progressive-enforcement)
    - [Problem](#problem-5)
    - [Solution: 3-Phase Rollout Strategy](#solution-3-phase-rollout-strategy)
      - [**Phase 1: Measurement Baseline (Week 1)**](#phase-1-measurement-baseline-week-1)
      - [**Phase 2: Warning Enforcement (Weeks 2-4)**](#phase-2-warning-enforcement-weeks-2-4)
      - [**Phase 3: Strict Enforcement (Week 5+)**](#phase-3-strict-enforcement-week-5)
    - [Legacy Test Handling](#legacy-test-handling)
  - [7. Alternatives Considered](#7-alternatives-considered)
    - [Alternative 1: `cargo test` + Timeout Wrapper](#alternative-1-cargo-test--timeout-wrapper)
    - [Alternative 2: Custom Test Harness](#alternative-2-custom-test-harness)
    - [Alternative 3: `build.rs` Pre-Processing](#alternative-3-buildrs-pre-processing)
    - [Alternative 4: External Test Runner (e.g., pytest-timeout equivalent)](#alternative-4-external-test-runner-eg-pytest-timeout-equivalent)
  - [8. Implementation Plan](#8-implementation-plan)
    - [Week 1: Setup Infrastructure](#week-1-setup-infrastructure)
    - [Week 2-4: Warning Phase](#week-2-4-warning-phase)
    - [Week 5: Strict Enforcement](#week-5-strict-enforcement)
    - [Week 6: Validation](#week-6-validation)
  - [9. Success Metrics](#9-success-metrics)
    - [Primary Metrics (SC-003, SC-004)](#primary-metrics-sc-003-sc-004)
    - [Secondary Metrics](#secondary-metrics)
  - [10. Risk Mitigation](#10-risk-mitigation)
    - [Risk 1: False Positives (Slow CI Runners)](#risk-1-false-positives-slow-ci-runners)
    - [Risk 2: Legitimate Slow Tests (e.g., Cryptography)](#risk-2-legitimate-slow-tests-eg-cryptography)
    - [Risk 3: Developer Pushback](#risk-3-developer-pushback)
  - [11. References](#11-references)
  - [Appendix A: Nextest Installation](#appendix-a-nextest-installation)
  - [Appendix B: Budget Violation Examples](#appendix-b-budget-violation-examples)
    - [Example 1: Unit Test Exceeding Budget](#example-1-unit-test-exceeding-budget)
    - [Example 2: Integration Test Within Budget](#example-2-integration-test-within-budget)
  - [Appendix C: Migration Checklist](#appendix-c-migration-checklist)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Performance Budget Enforcement Research

**Date**: 2025-12-11
**Context**: Need strict hard limits for test execution (unit: ≤1s, integration: ≤10s)
**Goal**: Enforce budgets during test selection and execution (single slow test >1s violates budget, not averages)

---

## Executive Summary

**Decision**: Use cargo-nextest with per-test timeout enforcement + custom classification via attributes/naming conventions
**Rationale**: Native Rust integration, per-test timeouts, JSON output, mature ecosystem (0.9.114), works with existing infrastructure
**Alternatives Considered**: cargo test --test-threads + timeout wrapper, custom harness, build.rs pre-processing

---

## 1. Budget Classification

### Problem
How to classify tests as **unit** (in-memory, no I/O) vs **integration** (file system, network) and automate classification?

### Solution: Multi-Layer Classification

#### **Primary Method: Test Location Convention** (80% coverage)
```
tests/
├── unit/                    # ≤1s budget (in-memory, no I/O)
│   ├── template_critical_tests.rs
│   └── marketplace_critical_tests.rs
├── integration/             # ≤10s budget (filesystem, Docker, network)
│   ├── clap/
│   ├── config/
│   └── graph/
└── e2e/                     # ≤30s budget (full workflows)
    └── complete_user_journey.rs
```

**Automation**: Directory-based rules in `.cargo/nextest.toml`
```toml
[[profile.default.overrides]]
filter = 'test(/^tests::unit::/)'
slow-timeout = { period = "1s", terminate-after = 2 }

[[profile.default.overrides]]
filter = 'test(/^tests::integration::/)'
slow-timeout = { period = "10s", terminate-after = 2 }
```

#### **Secondary Method: Custom Attributes** (15% coverage)
For crate-level tests (src/lib.rs, examples/) where directory structure doesn't apply:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    /// Unit test: lock-free graph cache lookup
    /// Budget: ≤1s (in-memory only)
    #[test]
    #[cfg_attr(not(feature = "integration"), ignore)]
    fn test_cache_hit_performance() {
        // ... test body
    }
}
```

**Automation**: Custom test attribute macro
```rust
// In chicago-tdd-tools crate
#[proc_macro_attribute]
pub fn unit_budget(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Inject timeout enforcement metadata at compile time
}
```

#### **Tertiary Method: Naming Convention** (5% coverage)
Fallback for legacy tests not yet migrated to directories:

```rust
#[test]
fn unit_lockfile_upsert() { ... }  // ≤1s (prefix: unit_)

#[test]
fn integration_docker_startup() { ... }  // ≤10s (prefix: integration_)
```

**Automation**: Regex filter in nextest profile
```toml
[[profile.default.overrides]]
filter = 'test(~^unit_)'
slow-timeout = { period = "1s" }
```

### Decision Matrix

| Method | Coverage | Automation | Maintenance |
|--------|----------|------------|-------------|
| Directory | 80% | High (nextest config) | Low (mkdir) |
| Attributes | 15% | Medium (proc macro) | Medium (manual tagging) |
| Naming | 5% | Medium (regex filter) | High (rename refactor) |

**Chosen**: Directory-first (80%), attributes for edge cases (15%), naming as migration path (5%)

---

## 2. Real-Time Monitoring

### Problem
Track per-test execution time during parallel runs, aggregate by category (unit/integration)

### Solution: cargo-nextest + JSON Output

#### **Per-Test Timing Capture**
```bash
cargo nextest run --profile ci --message-format json-pretty > test-results.json
```

**JSON Output Format** (nextest 0.9+):
```json
{
  "type": "test-finished",
  "test": {
    "name": "tests::unit::lockfile_upsert",
    "status": "pass"
  },
  "execution_time": {
    "secs": 0,
    "nanos": 45_234_000  // 0.045s (under 1s budget ✅)
  }
}
```

#### **Real-Time Aggregation Script**
```bash
#!/bin/bash
# scripts/test-budget-monitor.sh

cargo nextest run --profile ci --message-format json | \
  jq -c 'select(.type == "test-finished")' | \
  while read -r event; do
    test_name=$(echo "$event" | jq -r '.test.name')
    duration_ms=$(echo "$event" | jq '.execution_time | (.secs * 1000) + (.nanos / 1_000_000)')

    # Classify by directory/prefix
    if [[ "$test_name" =~ ^tests::unit:: ]]; then
      budget_ms=1000
      category="unit"
    elif [[ "$test_name" =~ ^tests::integration:: ]]; then
      budget_ms=10000
      category="integration"
    fi

    # Check violation
    if (( $(echo "$duration_ms > $budget_ms" | bc -l) )); then
      echo "❌ BUDGET VIOLATION: $test_name ($category: ${duration_ms}ms > ${budget_ms}ms)" >&2
      VIOLATIONS=$((VIOLATIONS + 1))
    fi
  done

exit $VIOLATIONS
```

#### **Dashboard Integration** (Kaizen metrics)
```json
{
  "metrics": {
    "test_budgets": {
      "unit_violations": 3,
      "integration_violations": 0,
      "total_violations": 3,
      "violation_rate": "2.1%",  // 3 / 142 tests
      "slowest_unit_test": {
        "name": "tests::unit::template_validation",
        "duration_ms": 1234,
        "budget_ms": 1000,
        "overage_pct": 23.4
      }
    }
  },
  "andon_signals": {
    "high": [
      {
        "message": "Unit test budget violation: tests::unit::template_validation (1234ms > 1000ms)",
        "category": "performance",
        "action": "Optimize or reclassify as integration test"
      }
    ]
  }
}
```

**Integration**: Add to `scripts/metrics/collect_metrics.sh`

---

## 3. Budget Violation Handling

### Problem
What to do when test exceeds budget: fail build, warn, auto-exclude from optimized suite?

### Solution: Tiered Enforcement Strategy

#### **Tier 1: Development (Warnings)**
```toml
# .cargo/nextest.toml
[profile.dev]
slow-timeout = { period = "60s" }  # Liberal timeout for debugging

[[profile.dev.overrides]]
filter = 'test(/^tests::unit::/)'
slow-timeout = { period = "5s", terminate-after = 1, grace-period = "2s" }

# Behavior: Warn on stdout, don't fail build
```

**Output**:
```
⚠️  Slow test detected: tests::unit::lockfile_upsert (1.2s > 1.0s budget)
   Consider: optimizing algorithm, reducing I/O, or reclassifying as integration
```

#### **Tier 2: CI (Strict Enforcement)**
```toml
[profile.ci]
fail-fast = false  # Run all tests to collect violations
slow-timeout = { period = "1s", terminate-after = 1 }

[[profile.ci.overrides]]
filter = 'test(/^tests::unit::/)'
slow-timeout = { period = "1s", terminate-after = 1 }

[[profile.ci.overrides]]
filter = 'test(/^tests::integration::/)'
slow-timeout = { period = "10s", terminate-after = 1 }
```

**Makefile.toml integration**:
```toml
[tasks.test-budget-check]
description = "Enforce test performance budgets (CI gate)"
command = "cargo"
args = ["nextest", "run", "--profile", "ci"]
env = { NEXTEST_FAILURE_OUTPUT = "final" }

[tasks.ci-gate]
dependencies = ["test-budget-check", "lint", "audit-all"]
```

**Behavior**: CI fails if ANY test exceeds budget

#### **Tier 3: Pre-Commit (Incremental Enforcement)**
Only check modified tests:
```bash
#!/bin/bash
# .git/hooks/pre-commit

# Get modified test files
MODIFIED_TESTS=$(git diff --cached --name-only --diff-filter=ACMRTUXB | grep '_test\.rs$\|_tests\.rs$\|^tests/')

if [ -n "$MODIFIED_TESTS" ]; then
  # Build filter expression for nextest
  FILTER_EXPR=$(echo "$MODIFIED_TESTS" | sed 's|tests/\(.*\)\.rs|test(/^\1::/)|' | paste -sd '|')

  cargo nextest run --profile ci --filter-expr "$FILTER_EXPR" || {
    echo "❌ Modified tests exceed performance budgets"
    echo "Run: cargo nextest run --profile dev --filter-expr '$FILTER_EXPR'"
    exit 1
  }
fi
```

### Decision: Progressive Enforcement

| Environment | Enforcement | Action on Violation |
|-------------|-------------|---------------------|
| **Dev** | Warnings | Log to stdout, continue |
| **Pre-Commit** | Incremental | Fail only on modified tests |
| **CI** | Strict | Fail build, block merge |
| **Release** | Strict + SLO | Fail + generate performance regression report |

**Rationale**: Balances developer velocity (warnings in dev) with quality gates (strict in CI)

---

## 4. Timeout Enforcement

### Problem
How to kill runaway tests, prevent hanging builds

### Solution: cargo-nextest Native Timeouts

#### **Configuration**
```toml
# .cargo/nextest.toml
[profile.default]
# Global per-test timeout
slow-timeout = { period = "60s", terminate-after = 2, grace-period = "5s" }

# Max time for entire test run
max-fail = 10  # Stop after 10 failures (prevents 1000+ timeout failures)

[[profile.default.overrides]]
filter = 'test(/^tests::unit::/)'
slow-timeout = { period = "1s", terminate-after = 1, grace-period = "100ms" }

[[profile.default.overrides]]
filter = 'test(/^tests::integration::/)'
slow-timeout = { period = "10s", terminate-after = 1, grace-period = "1s" }

[[profile.default.overrides]]
filter = 'test(/^tests::e2e::/)'
slow-timeout = { period = "30s", terminate-after = 2, grace-period = "5s" }
```

#### **Timeout Behavior**
```
Test: tests::unit::infinite_loop

  [00:00:01.000] ⚠️  Slow test warning (exceeded 1s budget)
  [00:00:01.100] 🛑 Terminating test (grace period expired)
  [00:00:01.200] ❌ Test killed (SIGTERM sent)

Result: TIMEOUT (exceeded 1s budget by 200ms)
```

#### **Comparison: cargo test vs nextest**

| Feature | `cargo test` | `cargo nextest` |
|---------|--------------|-----------------|
| Per-test timeout | ❌ (global only via wrapper) | ✅ Native |
| Grace period | ❌ | ✅ |
| Parallel execution | ✅ (limited) | ✅ (optimized) |
| JSON output | ❌ | ✅ |
| Test retries | ❌ | ✅ |
| Flaky test detection | ❌ | ✅ |
| Filter DSL | Basic | Advanced (regex, paths, metadata) |

**Decision**: Use cargo-nextest for timeout enforcement (native support, better UX)

---

## 5. CI/CD Integration

### Problem
Enforce budgets in GitHub Actions, provide clear feedback on violations

### Solution: Nextest + GitHub Actions Integration

#### **GitHub Actions Workflow**
```yaml
# .github/workflows/test-budgets.yml
name: Test Performance Budgets

on:
  pull_request:
    paths:
      - 'crates/**/*.rs'
      - 'tests/**/*.rs'
      - '.cargo/nextest.toml'

jobs:
  budget-enforcement:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Rust toolchain
        uses: dtolnay/rust-toolchain@stable

      - name: Install cargo-nextest
        uses: taiki-e/install-action@v2
        with:
          tool: nextest

      - name: Run tests with budget enforcement
        run: cargo nextest run --profile ci --message-format json > test-results.json
        continue-on-error: true

      - name: Analyze budget violations
        run: |
          VIOLATIONS=$(jq -r '
            select(.type == "test-finished") |
            select(.test.status == "timeout" or
                   (.execution_time.secs > 1 and .test.name | test("^tests::unit::")) or
                   (.execution_time.secs > 10 and .test.name | test("^tests::integration::"))) |
            "\(.test.name): \(.execution_time.secs).\(.execution_time.nanos / 1000000 | floor)s"
          ' test-results.json)

          if [ -n "$VIOLATIONS" ]; then
            echo "❌ Performance budget violations detected:"
            echo "$VIOLATIONS"
            exit 1
          fi

      - name: Upload test results
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: test-budget-results
          path: test-results.json

      - name: Comment on PR (violations)
        if: failure() && github.event_name == 'pull_request'
        uses: actions/github-script@v7
        with:
          script: |
            const fs = require('fs');
            const results = JSON.parse(fs.readFileSync('test-results.json', 'utf8'));

            const violations = results
              .filter(e => e.type === 'test-finished' && e.test.status === 'timeout')
              .map(e => `- \`${e.test.name}\`: ${e.execution_time.secs}s (budget exceeded)`);

            if (violations.length > 0) {
              github.rest.issues.createComment({
                issue_number: context.issue.number,
                owner: context.repo.owner,
                repo: context.repo.repo,
                body: `## ⚠️ Test Performance Budget Violations\n\n${violations.join('\n')}\n\n**Action Required**: Optimize tests or reclassify to appropriate category.`
              });
            }
```

#### **Makefile.toml Integration**
```toml
[tasks.test-budget-ci]
description = "Run tests with strict budget enforcement (CI only)"
workspace = false
command = "cargo"
args = ["nextest", "run", "--profile", "ci", "--message-format", "json"]

[tasks.test-budget-report]
description = "Generate budget violation report"
workspace = false
script_runner = "@shell"
script = '''
#!/bin/bash
set -euo pipefail

cargo nextest run --profile ci --message-format json > target/test-budgets.json

echo "📊 Test Performance Budget Report"
echo "=================================="
echo ""

UNIT_VIOLATIONS=$(jq '[select(.type == "test-finished") | select(.test.name | test("^tests::unit::")) | select(.execution_time.secs >= 1)] | length' target/test-budgets.json)
INTEGRATION_VIOLATIONS=$(jq '[select(.type == "test-finished") | select(.test.name | test("^tests::integration::")) | select(.execution_time.secs >= 10)] | length' target/test-budgets.json)

echo "Unit test violations (≥1s): $UNIT_VIOLATIONS"
echo "Integration test violations (≥10s): $INTEGRATION_VIOLATIONS"

if [ "$UNIT_VIOLATIONS" -gt 0 ] || [ "$INTEGRATION_VIOLATIONS" -gt 0 ]; then
  echo ""
  echo "❌ Budget violations detected!"
  exit 1
fi

echo "✅ All tests within budget"
'''

[tasks.ci-gate]
dependencies = ["test-budget-ci", "lint", "audit-all"]
```

---

## 6. Progressive Enforcement

### Problem
Should budgets be warnings initially, errors after grace period? How to handle legacy slow tests?

### Solution: 3-Phase Rollout Strategy

#### **Phase 1: Measurement Baseline (Week 1)**
```toml
# .cargo/nextest.toml
[profile.baseline]
slow-timeout = { period = "60s" }  # No enforcement, just measure

# Run with: cargo nextest run --profile baseline --message-format json
```

**Goal**: Establish current state (how many tests violate budgets today?)

**Output Example**:
```
Baseline Report (2025-12-11):
- Total tests: 1,247
- Unit tests: 856
  - Within 1s budget: 723 (84.5%)
  - Violations: 133 (15.5%)
- Integration tests: 391
  - Within 10s budget: 378 (96.7%)
  - Violations: 13 (3.3%)
```

#### **Phase 2: Warning Enforcement (Weeks 2-4)**
```toml
[profile.warning]
slow-timeout = { period = "60s" }

[[profile.warning.overrides]]
filter = 'test(/^tests::unit::/)'
slow-timeout = { period = "1s", terminate-after = 10 }  # Warn 10x before killing

[[profile.warning.overrides]]
filter = 'test(/^tests::integration::/)'
slow-timeout = { period = "10s", terminate-after = 5 }  # Warn 5x before killing
```

**Goal**: Flag violations without breaking builds, give teams time to optimize

**CI Behavior**:
```yaml
- name: Test with warnings
  run: cargo nextest run --profile warning
  continue-on-error: true  # Don't fail PR, just warn

- name: Report violations
  run: |
    # Parse violations, post as PR comment
    # Track in metrics dashboard
```

#### **Phase 3: Strict Enforcement (Week 5+)**
```toml
[profile.strict]
slow-timeout = { period = "60s" }

[[profile.strict.overrides]]
filter = 'test(/^tests::unit::/)'
slow-timeout = { period = "1s", terminate-after = 1 }  # Kill immediately

[[profile.strict.overrides]]
filter = 'test(/^tests::integration::/)'
slow-timeout = { period = "10s", terminate-after = 1 }
```

**Goal**: Zero tolerance for budget violations

**CI Behavior**:
```yaml
- name: Enforce budgets
  run: cargo nextest run --profile strict
  # Fails build on any violation
```

### Legacy Test Handling

**Option A: Gradual Migration**
```rust
// Mark legacy slow tests for migration
#[test]
#[ignore]  // Temporarily ignored during strict enforcement
fn legacy_slow_test() {
    // TODO: Optimize or reclassify as integration test
}
```

**Option B: Automatic Reclassification**
```bash
#!/bin/bash
# scripts/reclassify-slow-tests.sh

# Find all unit tests exceeding 1s budget
SLOW_UNIT_TESTS=$(jq -r '
  select(.type == "test-finished") |
  select(.test.name | test("^tests::unit::")) |
  select(.execution_time.secs >= 1) |
  .test.name
' target/test-budgets.json)

# Move to integration/ directory
for test in $SLOW_UNIT_TESTS; do
  FILE=$(find tests/unit -name "*${test##*::}.rs")
  [ -f "$FILE" ] && git mv "$FILE" tests/integration/
done
```

**Option C: Budget Exceptions**
```toml
# .cargo/nextest.toml
[[profile.strict.overrides]]
filter = 'test(=tests::unit::legacy_rdf_parser)'  # Exact match
slow-timeout = { period = "5s" }  # Temporary exception, tracked in Jira ticket
```

**Decision**: Use Option A (gradual migration) + Option C (tracked exceptions)
- **Why**: Option B (automatic) risks miscategorizing tests that need optimization, not reclassification

---

## 7. Alternatives Considered

### Alternative 1: `cargo test` + Timeout Wrapper
```bash
timeout 1s cargo test --lib  # Unit tests
timeout 10s cargo test --test  # Integration tests
```

**Pros**:
- No new dependencies
- Works with existing infrastructure

**Cons**:
- ❌ Kills entire test run, not individual tests
- ❌ No per-test metrics
- ❌ Poor UX (no JSON output, hard to parse)
- ❌ Doesn't work with parallel test execution

**Verdict**: ❌ Rejected (too coarse-grained)

---

### Alternative 2: Custom Test Harness
```rust
// tests/custom_harness.rs
#[cfg(test)]
mod custom_harness {
    use std::time::{Duration, Instant};

    pub fn run_with_budget<F>(budget: Duration, test: F)
    where F: Fn() + std::panic::UnwindSafe {
        let start = Instant::now();
        let result = std::panic::catch_unwind(test);
        let elapsed = start.elapsed();

        if elapsed > budget {
            panic!("Test exceeded budget: {:?} > {:?}", elapsed, budget);
        }

        result.unwrap();
    }
}

#[test]
fn test_with_budget() {
    custom_harness::run_with_budget(Duration::from_secs(1), || {
        // Test body
    });
}
```

**Pros**:
- Full control over timeout logic
- Can add custom metrics

**Cons**:
- ❌ Requires rewriting ALL tests
- ❌ Doesn't handle parallel execution well
- ❌ No termination of runaway tests (only post-check)
- ❌ High maintenance burden

**Verdict**: ❌ Rejected (too invasive, high maintenance)

---

### Alternative 3: `build.rs` Pre-Processing
```rust
// build.rs
fn main() {
    // Parse all test files
    // Inject timeout macros based on directory/naming
    // Generate nextest config automatically
}
```

**Pros**:
- Zero boilerplate in test files
- Automatic classification

**Cons**:
- ❌ Complex build logic
- ❌ Fragile (breaks on Rust syntax changes)
- ❌ Hard to debug (generated code)

**Verdict**: ❌ Rejected (over-engineering)

---

### Alternative 4: External Test Runner (e.g., pytest-timeout equivalent)
```toml
[dev-dependencies]
test-timeout = "0.1"  # Hypothetical crate
```

**Pros**:
- Declarative timeout configuration

**Cons**:
- ❌ No mature Rust equivalent exists (as of 2025-12)
- ❌ Would need to build from scratch
- ❌ Duplicates work that nextest already does

**Verdict**: ❌ Rejected (reinventing the wheel)

---

## 8. Implementation Plan

### Week 1: Setup Infrastructure
1. Install cargo-nextest: `cargo install cargo-nextest`
2. Create `.cargo/nextest.toml` with baseline profile
3. Run baseline measurement: `cargo nextest run --profile baseline --message-format json > target/baseline.json`
4. Generate baseline report: `scripts/test-budget-baseline.sh`

### Week 2-4: Warning Phase
1. Switch CI to warning profile
2. Add budget violation dashboard to Kaizen metrics
3. Create tracking issues for top 20 violators
4. Start optimization work (parallel to feature development)

### Week 5: Strict Enforcement
1. Switch CI to strict profile
2. Update pre-commit hook to enforce budgets
3. Document budget exceptions in ADR
4. Monitor defect escape rate reduction (SC-004 metric)

### Week 6: Validation
1. Measure SLO violation reduction (target: 60% reduction per SC-003)
2. Generate performance regression report
3. Retrospective: What worked, what didn't

---

## 9. Success Metrics

### Primary Metrics (SC-003, SC-004)
- **SLO Violation Reduction**: ≥60% reduction in timeout-related build failures
  - Baseline (Week 0): 2,354 violations
  - Target (Week 6): ≤942 violations
- **Defect Escape Rate Reduction**: ≥50% reduction in performance regressions reaching production
  - Baseline (Week 0): 12 performance defects/quarter
  - Target (Week 6): ≤6 performance defects/quarter

### Secondary Metrics
- **Test Execution Time**: ≤10% increase in total test suite time (parallel execution compensates for stricter timeouts)
- **Developer Velocity**: No measurable decrease in feature delivery (warnings don't block development)
- **CI Stability**: ≥95% green builds after strict enforcement (no flaky timeouts)

---

## 10. Risk Mitigation

### Risk 1: False Positives (Slow CI Runners)
**Mitigation**: Use nextest's `retries` feature
```toml
[profile.ci]
retries = 2  # Retry timeouts once before failing
```

### Risk 2: Legitimate Slow Tests (e.g., Cryptography)
**Mitigation**: Document exceptions in `.cargo/nextest.toml` + tracking ticket
```toml
[[profile.strict.overrides]]
filter = 'test(=tests::unit::pqc_key_generation)'  # Post-quantum crypto
slow-timeout = { period = "5s" }  # Exception: Kyber-1024 keygen takes 3.2s
# Tracked in: https://github.com/seanchatmangpt/ggen/issues/1234
```

### Risk 3: Developer Pushback
**Mitigation**: Gradual rollout (warnings first), provide optimization guides
- Create `docs/OPTIMIZING_TESTS.md` with common patterns
- Offer opt-in optimization sessions (pair programming)

---

## 11. References

- [cargo-nextest Documentation](https://nexte.st/docs/configuration/timeouts/)
- [Rust Testing Best Practices](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Chicago TDD Testing Philosophy](https://github.com/seanchatmangpt/ggen/blob/master/docs/CHICAGO_TDD.md)
- [ggen Constitution - Principle IV: cargo make Protocol](https://github.com/seanchatmangpt/ggen/blob/master/docs/CONSTITUTION.md#principle-iv-cargo-make-protocol)

---

## Appendix A: Nextest Installation

```bash
# Install via cargo
cargo install cargo-nextest --locked

# Verify installation
cargo nextest --version

# Create default config
mkdir -p .cargo
cat > .cargo/nextest.toml <<'EOF'
[profile.default]
retries = 0
slow-timeout = { period = "60s" }
EOF
```

---

## Appendix B: Budget Violation Examples

### Example 1: Unit Test Exceeding Budget
```
Test: tests::unit::template_validation
Budget: 1s
Actual: 1.234s
Violation: +234ms (23.4% over budget)

Root Cause: Synchronous file I/O in unit test
Fix: Mock filesystem OR reclassify as integration test
```

### Example 2: Integration Test Within Budget
```
Test: tests::integration::docker_startup
Budget: 10s
Actual: 8.456s
Status: ✅ PASS (15.4% under budget)
```

---

## Appendix C: Migration Checklist

- [ ] Install cargo-nextest
- [ ] Create `.cargo/nextest.toml`
- [ ] Run baseline measurement
- [ ] Organize tests into `unit/`, `integration/`, `e2e/` directories
- [ ] Create `scripts/test-budget-monitor.sh`
- [ ] Update Makefile.toml with `test-budget-ci` target
- [ ] Add GitHub Actions workflow
- [ ] Create Kaizen metrics dashboard widget
- [ ] Document exceptions in ADR
- [ ] Run retrospective after 6 weeks
