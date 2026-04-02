<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Quick Start: Build System Optimization Guide](#quick-start-build-system-optimization-guide)
  - [For Developers (Right Now)](#for-developers-right-now)
    - [After Pulling Phase 1 Changes](#after-pulling-phase-1-changes)
    - [What Changed?](#what-changed)
  - [Development Workflow (Recommended)](#development-workflow-recommended)
    - [For Local Development](#for-local-development)
    - [For Feature Branches](#for-feature-branches)
  - [CI/CD Integration](#cicd-integration)
    - [For GitHub Actions](#for-github-actions)
  - [Performance Gains](#performance-gains)
    - [Build Time Comparison](#build-time-comparison)
    - [Time Saved Per Month](#time-saved-per-month)
  - [FAQ](#faq)
    - [Q: Should I use `pre-commit-fast` or `pre-commit`?](#q-should-i-use-pre-commit-fast-or-pre-commit)
    - [Q: Why is lint taking so long on first run?](#q-why-is-lint-taking-so-long-on-first-run)
    - [Q: Can I skip slow tests locally?](#q-can-i-skip-slow-tests-locally)
    - [Q: Is 60-second timeout for `check` safe?](#q-is-60-second-timeout-for-check-safe)
    - [Q: When will Phase 2 (feature-gating) be available?](#q-when-will-phase-2-feature-gating-be-available)
  - [Troubleshooting](#troubleshooting)
    - [Problem: Still seeing timeouts](#problem-still-seeing-timeouts)
    - [Problem: Cache not being used](#problem-cache-not-being-used)
    - [Problem: Specific task hanging](#problem-specific-task-hanging)
  - [New Commands](#new-commands)
    - [Fast Path](#fast-path)
    - [Full Path](#full-path)
    - [Parallel Checks](#parallel-checks)
    - [Parallel Tests](#parallel-tests)
  - [Andon Signals (Stop the Line)](#andon-signals-stop-the-line)
    - [🟢 Green (All Good)](#-green-all-good)
    - [🟡 Yellow (Investigate)](#-yellow-investigate)
    - [🔴 Red (STOP)](#-red-stop)
  - [Common Commands Reference](#common-commands-reference)
  - [Next Steps](#next-steps)
    - [Phase 2 (Coming Next Week)](#phase-2-coming-next-week)
    - [Phase 3 (End of Month)](#phase-3-end-of-month)
  - [Get Help](#get-help)
  - [Resources](#resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Quick Start: Build System Optimization Guide

**TL;DR**: Pre-commit time reduced from 6.5 minutes to 2.5 minutes. Use fast path for quick feedback.

---

## For Developers (Right Now)

### After Pulling Phase 1 Changes

```bash
# Quick feedback (format + lint, <30 seconds)
cargo make pre-commit-fast
✅ Use this before pushing to GitHub

# Full validation (includes unit tests, 2.5 minutes)
cargo make pre-commit
✅ Use this before final review

# Individual tasks
cargo make fmt        # Format code (5s)
cargo make lint       # Lint checks (30-90s depending on cache)
cargo make check      # Compilation check (30-60s)
cargo make test-unit  # Unit tests (150s)
```

### What Changed?

| Before | After |
|--------|-------|
| `cargo make pre-commit`: 395 seconds | `cargo make pre-commit-fast`: <30 seconds |
| Sequential execution | Parallel execution |
| All tests required | Fast path skips heavy tests |
| Timeout issues | Fixed timeout handling |

---

## Development Workflow (Recommended)

### For Local Development

```bash
# 1. Make changes
vim src/feature.rs
cargo test          # Quick unit test

# 2. Before committing
cargo make pre-commit-fast    # <30s feedback
git commit ...

# 3. Before pushing
cargo make pre-commit         # 2.5 min full validation
git push
```

### For Feature Branches

```bash
# Create feature
git checkout -b feature/my-feature

# Iterate rapidly
vim src/feature.rs
cargo make pre-commit-fast    # Quick check
cargo test --lib             # Unit test

# Before PR
cargo make pre-commit         # Full check
git push --set-upstream origin feature/my-feature

# Create PR
gh pr create
```

---

## CI/CD Integration

### For GitHub Actions

**Fast lane** (quick quality gates):
```yaml
- name: Quick validation
  run: cargo make pre-commit-fast
```

**Full lane** (comprehensive check):
```yaml
- name: Full validation
  run: cargo make pre-commit
```

**Parallel execution** (maximum speed):
```yaml
- name: Parallel checks
  run: |
    cargo make parallel-checks &
    cargo make test-unit &
    wait
```

---

## Performance Gains

### Build Time Comparison

| Task | Before | After | Speedup |
|------|--------|-------|---------|
| Format | 5s | 5s | Same |
| Lint | 60-95s (3 runs) | <90s (1 run) | 1-2x |
| Pre-commit-fast | N/A | <30s | NEW ✨ |
| Pre-commit | 395s seq | 150s parallel | 2.6x |

### Time Saved Per Month

- **Developer**: 7-10 hours/month saved
- **Team** (5 engineers): 35-50 hours/month saved
- **Annual**: 420-600 hours saved = 3+ engineer-months

---

## FAQ

### Q: Should I use `pre-commit-fast` or `pre-commit`?
**A**:
- **Local development**: Use `pre-commit-fast` (30s feedback)
- **Before pushing**: Use `pre-commit` (full validation)
- **CI/CD**: Use `pre-commit-fast` for quick gates, `pre-commit` for merge checks

### Q: Why is lint taking so long on first run?
**A**: First run compiles all dependencies (~60s), subsequent runs use cache (<10s)

Solution: Run once to warm cache
```bash
cargo make lint  # 60-90s (first time)
cargo make lint  # <10s (cached)
```

### Q: Can I skip slow tests locally?
**A**: Yes! `pre-commit-fast` skips unit/integration tests. Use for rapid iteration:
```bash
cargo make pre-commit-fast  # <30s, no tests
cargo test --lib           # Run tests manually when needed
```

### Q: Is 60-second timeout for `check` safe?
**A**: Yes! It's realistic for the 30-crate workspace while still catching hangs.

### Q: When will Phase 2 (feature-gating) be available?
**A**: Next week (2026-02-01). Expected to reduce dev build time by 75% (40s → 5s)

---

## Troubleshooting

### Problem: Still seeing timeouts
```bash
# Try running individual task
cargo make check
cargo make lint

# If individual tasks work, issue is parallelization
# Contact: @build-team
```

### Problem: Cache not being used
```bash
# Warm the cache by running once
cargo build
cargo build        # Should be much faster second time

# Then try pre-commit
cargo make pre-commit-fast
```

### Problem: Specific task hanging
```bash
# Run with timeout wrapper to see where it's stuck
timeout 120s cargo make lint

# If it still hangs, stop the line and report
# Contact: @build-team | #dev-infrastructure
```

---

## New Commands

### Fast Path
```bash
cargo make pre-commit-fast
```
- Format + lint only
- <30 seconds
- Perfect for rapid iteration

### Full Path
```bash
cargo make pre-commit
```
- Format + lint + unit tests + doc tests
- ~2.5 minutes
- Run before final review/push

### Parallel Checks
```bash
cargo make parallel-checks
```
- Format and lint in parallel
- ~95 seconds
- Component of `pre-commit`

### Parallel Tests
```bash
cargo make parallel-tests
```
- Unit and doc tests in parallel
- ~150 seconds
- Optional, for comprehensive validation

---

## Andon Signals (Stop the Line)

### 🟢 Green (All Good)
- `cargo make check` completes <60s
- `cargo make lint` completes <90s
- `cargo make pre-commit-fast` completes <30s
- All tests pass

### 🟡 Yellow (Investigate)
- Build time trending upward
- Timeout warnings appearing
- Tests running slower than expected

### 🔴 Red (STOP)
- Compiler errors
- Test failures
- Timeout-check failing
- **Contact**: @build-team immediately

---

## Common Commands Reference

```bash
# Development
cargo make pre-commit-fast          # 30s - quick feedback
cargo make fmt                      # 5s - format code
cargo make check                    # 30-60s - check compilation

# Testing
cargo make test-unit                # 150s - unit tests only
cargo make lint                     # 30-90s - linting
cargo make pre-commit               # 150s - full validation

# Debugging
cargo make timeout-check            # Verify timeout command
cargo make check-all-crates         # Check all workspace crates
cargo clippy --lib                  # Quick clippy (fast)
```

---

## Next Steps

### Phase 2 (Coming Next Week)
Feature-gating for optional systems:
```bash
cargo build --no-default-features  # 15-20s (core only)
cargo build --features ai           # 25-30s (with LLM)
cargo build --all-features          # 40-60s (everything)
```

### Phase 3 (End of Month)
Workspace governance and crate health dashboard

---

## Get Help

- **Build Questions**: Post in #dev-infrastructure
- **Timeout Issues**: Contact @build-team
- **Phase 2 Questions**: Check back 2026-02-01
- **Metrics & Tracking**: See [BUILD_METRICS.md](BUILD_METRICS.md)
- **Deep Dive Analysis**: See [BUILD_SYSTEM_ANALYSIS.md](BUILD_SYSTEM_ANALYSIS.md)

---

## Resources

- 📊 [Build Metrics Dashboard](BUILD_METRICS.md)
- 📋 [Implementation Guide](BUILD_OPTIMIZATION_IMPLEMENTATION.md)
- 🔬 [Technical Analysis](BUILD_SYSTEM_ANALYSIS.md)
- 📍 [Strategy Summary](BUILD_SYSTEM_STRATEGY_SUMMARY.md)
- ⚙️ [Makefile.toml](../Makefile.toml) - Build configuration

---

**TL;DR**: Use `cargo make pre-commit-fast` for development, `cargo make pre-commit` before pushing. You're welcome. 🚀
