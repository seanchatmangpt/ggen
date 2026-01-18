<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Week 4: Getting Started Guide](#week-4-getting-started-guide)
  - [Prerequisites](#prerequisites)
  - [Initial Setup (One-Time)](#initial-setup-one-time)
    - [1. Verify Week 3 Baseline](#1-verify-week-3-baseline)
    - [2. Create Week 4 Structure](#2-create-week-4-structure)
    - [3. Make Scripts Executable](#3-make-scripts-executable)
  - [Daily Workflow](#daily-workflow)
    - [Every Morning (Days 1-5)](#every-morning-days-1-5)
    - [Every Evening (Optional)](#every-evening-optional)
  - [What to Expect Each Day](#what-to-expect-each-day)
    - [Day 1: Baseline Establishment](#day-1-baseline-establishment)
    - [Day 2: CLI Test Coverage](#day-2-cli-test-coverage)
    - [Day 3: Utils & Ontology Tests](#day-3-utils--ontology-tests)
    - [Day 4: Marketplace & Integration](#day-4-marketplace--integration)
    - [Day 5: Final Validation](#day-5-final-validation)
  - [Understanding the Metrics](#understanding-the-metrics)
    - [Health Score (81% → 85%)](#health-score-81-%E2%86%92-85)
    - [Coverage (50% → 65%)](#coverage-50-%E2%86%92-65)
    - [Tests (464 → 560+)](#tests-464-%E2%86%92-560)
  - [Interpreting Dashboard Output](#interpreting-dashboard-output)
    - [Progress Bars](#progress-bars)
    - [Status Icons](#status-icons)
    - [Trend Arrows](#trend-arrows)
  - [Troubleshooting](#troubleshooting)
    - [Daily Tracker Fails](#daily-tracker-fails)
    - [Dashboard Shows Wrong Data](#dashboard-shows-wrong-data)
    - [Coverage Not Increasing](#coverage-not-increasing)
  - [Common Questions](#common-questions)
    - [Q: How often should I run the tracker?](#q-how-often-should-i-run-the-tracker)
    - [Q: What if I'm behind schedule?](#q-what-if-im-behind-schedule)
    - [Q: Can I run tracker multiple times per day?](#q-can-i-run-tracker-multiple-times-per-day)
    - [Q: What if health score regresses?](#q-what-if-health-score-regresses)
    - [Q: How do I know if I'm on track?](#q-how-do-i-know-if-im-on-track)
  - [Success Indicators](#success-indicators)
    - [Daily Success](#daily-success)
    - [Week 4 Success (Final)](#week-4-success-final)
  - [Next Steps After Week 4](#next-steps-after-week-4)
  - [Quick Reference](#quick-reference)
    - [Essential Commands](#essential-commands)
    - [File Locations](#file-locations)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Week 4: Getting Started Guide

**Quick start guide for Week 4 metrics tracking and health score validation.**

---

## Prerequisites

Before starting Week 4:
- Week 3 completed (baseline: 81% health, 50% coverage)
- All Week 3 tests passing (464+ tests)
- Compilation successful (0 errors)
- Scripts directory structure exists

---

## Initial Setup (One-Time)

### 1. Verify Week 3 Baseline

```bash
# Check current health score
./scripts/health_dashboard.sh

# Verify test count
cargo test --lib -- --list | grep -c "test "

# Should show: 464+ tests
```

### 2. Create Week 4 Structure

```bash
# Already created by setup
mkdir -p docs/metrics/week4/{daily,reports}
mkdir -p scripts/week4
```

### 3. Make Scripts Executable

```bash
# Already done during setup
chmod +x scripts/week4/daily_tracker.sh
chmod +x scripts/week4/week4_dashboard.sh
```

---

## Daily Workflow

### Every Morning (Days 1-5)

```bash
# 1. Run daily metrics tracker
./scripts/week4/daily_tracker.sh

# Expected output:
#   ═══════════════════════════════════════════════════════
#      Week 4 Daily Tracker - Day N/5 - YYYY-MM-DD
#      Health Score Validation: 81% → 85% Target
#   ═══════════════════════════════════════════════════════
#
#   [1/7] Checking compilation status...
#     ✓ Compilation SUCCESS - 0 warnings
#
#   [2/7] Running test suite...
#     ✓ All tests passing: 464 tests
#
#   [3/7] Calculating coverage...
#     ✓ Estimated coverage: 50%
#
#   [4/7] Analyzing code quality...
#     ✓ Code quality: 96%
#
#   [5/7] Checking security status...
#     ⏳ Security score: 82% (baseline)
#
#   [6/7] Checking performance status...
#     ⏳ Performance score: 88% (baseline)
#
#   [7/7] Calculating health score...
#     ⏳ Health score: 81.0% (+0.0%, gap: 4.0%)
#
#   ═══════════════════════════════════════════════════════
#                DAY N/5 SUMMARY
#   ═══════════════════════════════════════════════════════
#
#     Health Score:  81.0%  ✓ On Track
#     Coverage:      50%    ⏳ Behind
#     Tests:         464    ✓ All Passing

# 2. Review daily report
cat docs/metrics/week4/daily/day*_$(date +%Y-%m-%d).md

# 3. View interactive dashboard
./scripts/week4/week4_dashboard.sh
```

### Every Evening (Optional)

```bash
# Re-run tracker if tests were added during the day
./scripts/week4/daily_tracker.sh

# Compare morning vs evening metrics
diff docs/metrics/week4/daily/day*_$(date +%Y-%m-%d)*.md
```

---

## What to Expect Each Day

### Day 1: Baseline Establishment

**Morning:**
- Run daily_tracker.sh for baseline
- Health: 81% (expected)
- Coverage: 50% (expected)
- Tests: 464 (expected)

**Activities:**
- Fix any compilation issues
- Verify Week 3 tests still pass
- Review baseline metrics

**Evening:**
- No major changes expected
- Health: 81% (holding steady)

### Day 2: CLI Test Coverage

**Morning:**
- Health: 81% (from Day 1)
- Coverage: 50%
- Tests: 464

**Activities:**
- Add 25 CLI tests
- Fix 1 security issue
- Re-run tracker after tests added

**Evening:**
- Health: 81.5% (expected)
- Coverage: 52%
- Tests: 489

### Day 3: Utils & Ontology Tests

**Morning:**
- Health: 81.5% (from Day 2)
- Coverage: 52%
- Tests: 489

**Activities:**
- Add 30 utils tests
- Add 25 ontology tests
- Validate performance metrics

**Evening:**
- Health: 82% (expected)
- Coverage: 54%
- Tests: 519

### Day 4: Marketplace & Integration

**Morning:**
- Health: 82% (from Day 3)
- Coverage: 54%
- Tests: 519

**Activities:**
- Add 30 marketplace tests
- Fix 2-3 security issues
- Improve architecture (+5%)

**Evening:**
- Health: 84% (expected)
- Coverage: 58%
- Tests: 549

### Day 5: Final Validation

**Morning:**
- Health: 84% (from Day 4)
- Coverage: 58%
- Tests: 549

**Activities:**
- Add final 11+ tests
- Fix remaining security issues
- Achieve A+ performance

**Evening:**
- Health: 85% ✅ TARGET ACHIEVED
- Coverage: 65% ✅ TARGET ACHIEVED
- Tests: 560+ ✅ TARGET ACHIEVED

---

## Understanding the Metrics

### Health Score (81% → 85%)

**What it means:**
Overall codebase quality combining 6 weighted dimensions.

**How it's calculated:**
```
Health = (Compilation × 0.30) + (Testing × 0.25) + (Quality × 0.15) +
         (Security × 0.15) + (Performance × 0.10) + (Architecture × 0.05)
```

**How to improve:**
- Add tests (+3.75% max contribution)
- Fix security issues (+0.45% max)
- Optimize performance (+0.4% max)
- Refactor architecture (+0.25% max)

### Coverage (50% → 65%)

**What it means:**
Percentage of source code executed by tests.

**How it's calculated:**
```
Coverage ≈ (test_lines / source_lines) × 1.4, capped at 100%
```

**How to improve:**
- Add unit tests for uncovered modules
- Add integration tests for workflows
- Focus on critical 20% functionality (80/20 rule)

### Tests (464 → 560+)

**What it means:**
Total number of passing tests.

**How to improve:**
- Add 25 tests (Day 2 - CLI)
- Add 55 tests (Day 3 - Utils/Ontology)
- Add 85 tests (Day 4 - Marketplace)
- Add 100+ tests (Day 5 - Complete)

---

## Interpreting Dashboard Output

### Progress Bars

```
[████████████████░░░░░░░░░░░░]
 ^              ^
 Current        Gap to 100%
```

- **Green (█)**: Progress toward goal
- **Cyan (░)**: Remaining to 100%
- Length: 40 characters (scaled)

### Status Icons

- **✓ (Green)**: On track or achieved
- **⏳ (Yellow)**: In progress, slightly behind
- **✗ (Red)**: Critical blocker or regression

### Trend Arrows

- **↗ (Green)**: Improving
- **→ (Yellow)**: Stable
- **↘ (Red)**: Declining (regression)

---

## Troubleshooting

### Daily Tracker Fails

**Error:** "Compilation FAILED"
```bash
# Fix: Check compilation errors
cargo check --package ggen-core --lib

# Fix errors, then re-run tracker
./scripts/week4/daily_tracker.sh
```

**Error:** "Tests FAILING"
```bash
# Fix: Run tests to see failures
cargo test --lib

# Fix failing tests, then re-run tracker
./scripts/week4/daily_tracker.sh
```

**Error:** "No reports found"
```bash
# This is normal on first run
# Tracker will create first report automatically
```

### Dashboard Shows Wrong Data

```bash
# Fix: Re-run daily tracker to refresh data
./scripts/week4/daily_tracker.sh

# Then view dashboard again
./scripts/week4/week4_dashboard.sh
```

### Coverage Not Increasing

```bash
# Check if tests are actually testing code
cargo tarpaulin -p ggen-core -o Html --timeout 300

# Open coverage report
open tarpaulin-report.html

# Focus on uncovered critical paths
```

---

## Common Questions

### Q: How often should I run the tracker?

**A:** Minimum once per day (morning). Optionally after adding tests (evening).

### Q: What if I'm behind schedule?

**A:** Daily tracker will show "Behind Schedule" warning. Prioritize test coverage for critical modules (graph/core.rs, ontology/*.rs).

### Q: Can I run tracker multiple times per day?

**A:** Yes! Each run overwrites the daily report with latest metrics. Evening runs show progress since morning.

### Q: What if health score regresses?

**A:** Tracker exits with code 1 and shows "REGRESSION" alert. Fix immediately:
1. Check for test failures
2. Check for compilation issues
3. Review code quality changes (new TODO/FIXME)

### Q: How do I know if I'm on track?

**A:** Dashboard shows status icons:
- ✓ = On track
- ⏳ = Slightly behind but manageable
- ✗ = Critical blocker requiring immediate action

---

## Success Indicators

### Daily Success

Each day should show:
- [ ] Compilation: 100% (✓)
- [ ] Tests: 100% pass rate (✓)
- [ ] Health: ≥ Day N target (✓ or ⏳)
- [ ] Coverage: ≥ Day N target (✓ or ⏳)

### Week 4 Success (Final)

By end of Day 5:
- [ ] Health: ≥85% (✅)
- [ ] Coverage: ≥65% (✅)
- [ ] Tests: ≥560 (✅)
- [ ] All dimensions improved or maintained
- [ ] 5 daily reports complete

---

## Next Steps After Week 4

Once Week 4 targets achieved:

1. **Generate Final Report**
   ```bash
   # Consolidate all 5 daily reports
   cat docs/metrics/week4/daily/*.md > docs/metrics/week4/reports/WEEK4_FINAL_REPORT.md
   ```

2. **Review Lessons Learned**
   - What worked well?
   - What blockers occurred?
   - How to improve Week 5?

3. **Plan Week 5**
   - Performance optimization (88% → 92%)
   - Security hardening (82% → 85%)
   - Architecture refinement (60% → 65%)

---

## Quick Reference

### Essential Commands

```bash
# Daily workflow
./scripts/week4/daily_tracker.sh        # Run metrics
./scripts/week4/week4_dashboard.sh      # View dashboard
cat docs/metrics/week4/daily/day*.md    # Read report

# Manual verification
cargo check --package ggen-core --lib   # Compilation
cargo test --lib                        # Tests
cargo test --lib -- --list | wc -l      # Count tests

# Coverage analysis
cargo tarpaulin -p ggen-core -o Html    # HTML coverage
```

### File Locations

```bash
# Daily reports
docs/metrics/week4/daily/dayN_YYYY-MM-DD.md

# Tracking plan
docs/metrics/week4/WEEK4_TRACKING_PLAN.md

# Scripts
scripts/week4/daily_tracker.sh
scripts/week4/week4_dashboard.sh
```

---

**Document Owner:** Code Quality Analyzer
**Created:** 2025-11-18
**Week:** 4 (Getting Started)
**For:** New users starting Week 4 metrics tracking
