<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Andon + Gemba Walk Quick Start Guide](#andon--gemba-walk-quick-start-guide)
  - [🚀 5-Minute Setup](#-5-minute-setup)
    - [Run the Demo](#run-the-demo)
    - [Run Verification](#run-verification)
  - [📊 Local Development](#-local-development)
    - [Monitor Tests (Andon)](#monitor-tests-andon)
    - [Inspect Test Quality (Gemba Walk)](#inspect-test-quality-gemba-walk)
  - [🔧 Programmatic Usage](#-programmatic-usage)
    - [Rust Code](#rust-code)
    - [CI/CD Integration](#cicd-integration)
  - [📋 Common Scenarios](#-common-scenarios)
    - [Scenario 1: Before Committing](#scenario-1-before-committing)
    - [Scenario 2: Test Suite Refactoring](#scenario-2-test-suite-refactoring)
    - [Scenario 3: Investigating Flaky Tests](#scenario-3-investigating-flaky-tests)
    - [Scenario 4: CI Failure](#scenario-4-ci-failure)
  - [🎯 Key Metrics](#-key-metrics)
    - [Andon Thresholds](#andon-thresholds)
    - [Gemba Weights](#gemba-weights)
  - [🔍 Troubleshooting](#-troubleshooting)
    - ["No tests found"](#no-tests-found)
    - [Scripts not executable](#scripts-not-executable)
    - [Valgrind not available](#valgrind-not-available)
  - [📚 Learn More](#-learn-more)
  - [✅ Success Checklist](#-success-checklist)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Andon + Gemba Walk Quick Start Guide

## 🚀 5-Minute Setup

### Run the Demo

```bash
# See Andon alerts and Gemba Walk in action
cargo run --example andon_gemba_demo
```

**Output:**
- ✅ Green scenario (all tests passing)
- 🚨 Red scenario (high failure rate)
- ⚠️ Yellow scenario (flaky tests)
- 🚶 Gemba Walk checklist example

### Run Verification

```bash
# Verify complete installation
./scripts/verify_andon_gemba.sh
```

**Validates:**
- All source files present
- Demo application works
- Scripts are executable
- CI workflow configured
- Documentation complete

## 📊 Local Development

### Monitor Tests (Andon)

```bash
# Quick check (compilation + failure rate)
./scripts/andon_monitor.sh

# Full monitoring (includes flaky detection)
CHECK_FLAKY=true ./scripts/andon_monitor.sh

# Complete suite (includes memory leaks via valgrind)
CHECK_MEMORY=true ./scripts/andon_monitor.sh
```

**Alerts:**
- 🚨 **Red Alert**: Stops pipeline, fix immediately
  - Compilation failures
  - Test timeouts (> 30s)
  - High failure rate (> 5%)
  - Memory leaks

- ⚠️ **Yellow Alert**: Monitor and investigate
  - Flaky tests
  - Moderate failure rate (2-5%)
  - Performance warnings

- ✅ **Green**: All tests passing

### Inspect Test Quality (Gemba Walk)

```bash
# Basic inspection
./scripts/gemba_walk.sh tests/integration

# Observe actual test runs
OBSERVE_RUN=true ./scripts/gemba_walk.sh tests/integration

# Interview code (extract test intent)
INTERVIEW_CODE=true ./scripts/gemba_walk.sh tests/integration
```

**8-Point Checklist:**
1. Real implementations (not mocked)
2. Clear failure messages
3. Bug detection (not just syntax)
4. Setup/teardown clarity
5. Debug-ability
6. Performance (< 30s)
7. Test isolation
8. Reproducibility

**Score Interpretation:**
- 90-100%: Excellent
- 75-89%: Good
- 60-74%: Fair
- < 60%: Poor (refactor needed)

## 🔧 Programmatic Usage

### Rust Code

```rust
use std::collections::HashMap;

// Import the modules (when available in library)
// For now, copy from tests/integration/lean_quality_tests.rs

// Andon Dashboard
let mut dashboard = TestHealthDashboard::new();
dashboard.record_test("my_test", true);
dashboard.record_test("flaky_test", false);

let status = dashboard.status();
match status.severity {
    Severity::Red => eprintln!("🚨 Critical failure!"),
    Severity::Yellow => eprintln!("⚠️ Warning"),
    Severity::Green => println!("✅ All good"),
}

// Gemba Walk
let mut checklist = GembaWalkChecklist::new(
    PathBuf::from("tests/my_test.rs")
);
checklist.add_check("Mock usage", true, "Minimal", 15.0);
checklist.add_check("Assertions", false, "Needs improvement", 15.0);
checklist.calculate_score();

println!("Quality Score: {:.1}%", checklist.score);
```

### CI/CD Integration

The GitHub Actions workflow runs automatically on push/PR:

```yaml
# .github/workflows/andon_ci.yml
on: [push, pull_request]

jobs:
  andon-red-alert:    # Critical failures
  andon-yellow-alert: # Warnings
  andon-dashboard:    # Health report
```

**Workflow:**
1. Push code → GitHub Actions triggers
2. Red Alert job checks compilation, timeouts, failure rate
3. Yellow Alert job checks flaky tests, code quality
4. Dashboard job generates health report
5. PR blocked if Red Alert fails

## 📋 Common Scenarios

### Scenario 1: Before Committing

```bash
# Quick local check
./scripts/andon_monitor.sh

# If Green ✅ → commit
# If Yellow ⚠️ → investigate, may commit
# If Red 🚨 → fix before committing
```

### Scenario 2: Test Suite Refactoring

```bash
# 1. Baseline quality
./scripts/gemba_walk.sh tests/integration > baseline_report.txt

# 2. Refactor tests
# ... make changes ...

# 3. Compare quality
./scripts/gemba_walk.sh tests/integration > new_report.txt
diff baseline_report.txt new_report.txt

# 4. Verify improvement
# Score should increase by 10-20%
```

### Scenario 3: Investigating Flaky Tests

```bash
# Run flaky detection
CHECK_FLAKY=true ./scripts/andon_monitor.sh

# Output shows:
# ⚠️ YELLOW ALERT: Flaky test detected: test_name (2/3 passes)

# Fix the test, re-run
CHECK_FLAKY=true ./scripts/andon_monitor.sh

# Should show:
# ✅ No flaky tests detected
```

### Scenario 4: CI Failure

**Red Alert in CI:**

1. Check workflow logs for category:
   - Compilation → syntax errors
   - Timeout → infinite loops, deadlocks
   - Failure rate → multiple tests failing

2. Get remediation from alert:
   ```
   REMEDIATION:
   1. Check for infinite loops
   2. Review async operations
   3. Increase timeout if justified
   ```

3. Fix locally:
   ```bash
   ./scripts/andon_monitor.sh  # Reproduce
   # Fix issue
   ./scripts/andon_monitor.sh  # Verify
   ```

4. Push fix → CI re-runs → Should be Green ✅

## 🎯 Key Metrics

### Andon Thresholds

| Metric | Green | Yellow | Red |
|--------|-------|--------|-----|
| Failure Rate | 0% | 2-5% | > 5% |
| Flaky Rate | 0% | 1-2% | > 2% |
| Test Time | < 10s | 10-30s | > 30s |
| Compilation | Pass | Warnings | Errors |

### Gemba Weights

| Check | Weight | Why Important |
|-------|--------|---------------|
| Bug Detection | 20.0 | Highest value - tests must catch bugs |
| Real Implementations | 15.0 | Avoid over-mocking |
| Clear Assertions | 15.0 | Enable debugging |
| Debug-ability | 15.0 | Fast feedback |
| Setup/Teardown | 10.0 | Test clarity |
| Performance | 10.0 | Fast CI |
| Isolation | 10.0 | No side effects |
| Reproducibility | 5.0 | Consistency |

## 🔍 Troubleshooting

### "No tests found"

**Problem:** `cargo test --test lean_quality_tests` fails

**Solution:** Tests are in `tests/integration/lean_quality_tests.rs` but not registered in Cargo.toml yet. They compile and work, just run:

```bash
cargo build --tests  # Compiles all tests including lean_quality
```

Or add to `Cargo.toml`:

```toml
[[test]]
name = "lean_quality_tests"
path = "tests/integration/lean_quality_tests.rs"
```

### Scripts not executable

**Problem:** `Permission denied` when running scripts

**Solution:**
```bash
chmod +x scripts/andon_monitor.sh scripts/gemba_walk.sh
```

### Valgrind not available

**Problem:** `valgrind: command not found`

**Solution:** Memory leak checking is optional:
```bash
# Skip memory check
./scripts/andon_monitor.sh  # Default: no memory check

# Or install valgrind
brew install valgrind  # macOS
sudo apt install valgrind  # Linux
```

## 📚 Learn More

- **Full Playbook:** `docs/lean_quality/ANDON_GEMBA_PLAYBOOK.md`
- **Implementation Details:** `docs/lean_quality/IMPLEMENTATION_SUMMARY.md`
- **Source Code:**
  - Andon: `tests/lean_quality/andon_system.rs`
  - Gemba: `tests/lean_quality/gemba_walk.rs`
  - Integration: `tests/integration/lean_quality_tests.rs`
  - Demo: `examples/andon_gemba_demo.rs`

## ✅ Success Checklist

- [ ] Run demo: `cargo run --example andon_gemba_demo`
- [ ] Run verification: `./scripts/verify_andon_gemba.sh`
- [ ] Monitor tests: `./scripts/andon_monitor.sh`
- [ ] Inspect quality: `./scripts/gemba_walk.sh tests/integration`
- [ ] Review playbook: `docs/lean_quality/ANDON_GEMBA_PLAYBOOK.md`

**You're ready to use Andon + Gemba Walk!** 🎉
