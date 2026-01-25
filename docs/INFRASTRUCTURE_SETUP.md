<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Infrastructure Setup Guide](#infrastructure-setup-guide)
  - [Systems Overview](#systems-overview)
    - [1. Template Discovery System (build.rs)](#1-template-discovery-system-buildrs)
    - [2. API Versioning Helpers](#2-api-versioning-helpers)
    - [3. Pre-Commit Hooks](#3-pre-commit-hooks)
    - [4. Metrics Collection System](#4-metrics-collection-system)
    - [5. Chicago TDD Test Templates](#5-chicago-tdd-test-templates)
  - [CI/CD Integration](#cicd-integration)
    - [GitHub Actions Workflow](#github-actions-workflow)
    - [Daily Metrics Collection](#daily-metrics-collection)
  - [Makefile.toml Additions](#makefiletoml-additions)
  - [Maintenance](#maintenance)
    - [Adding New Templates](#adding-new-templates)
    - [Updating Metrics](#updating-metrics)
    - [Customizing Pre-Commit Hooks](#customizing-pre-commit-hooks)
  - [Troubleshooting](#troubleshooting)
    - ["timeout command not found"](#timeout-command-not-found)
    - [Build.rs fails to discover templates](#buildrs-fails-to-discover-templates)
    - [Pre-commit hook not running](#pre-commit-hook-not-running)
  - [Best Practices](#best-practices)
  - [Support](#support)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Infrastructure Setup Guide

This document describes the automated infrastructure systems that enable continuous quality improvement.

## Systems Overview

### 1. Template Discovery System (build.rs)

**Purpose**: Automatically discovers and validates all templates at compile time.

**Location**: `/build.rs`

**How it works**:
1. Scans `templates/clap-noun-verb-360/` for `.tmpl` files
2. Validates template syntax (balanced braces, non-empty)
3. Generates Rust code for compile-time template access
4. Creates `TEMPLATES` static map with all templates

**Usage**:
```rust
use ggen::templates::{get_template, list_templates, TEMPLATE_COUNT};

// Get a specific template
if let Some(content) = get_template("noun-user-command") {
    println!("Template: {}", content);
}

// List all templates
for name in list_templates() {
    println!("Available: {}", name);
}

// Get total count
println!("Total templates: {}", TEMPLATE_COUNT);
```

**Benefits**:
- Zero runtime discovery overhead
- Compile-time validation
- Type-safe template access
- Automatic updates when templates change

### 2. API Versioning Helpers

**Purpose**: Manage API deprecations and breaking changes with compile-time warnings.

**Location**: `/crates/ggen-utils/src/versioning.rs`

**Macros**:

```rust
use ggen_utils::{deprecated_since, experimental, breaking_change};

// Mark deprecated APIs
#[deprecated_since!("1.0.0", "Use new_api() instead")]
pub fn old_api() -> Result<String, Error> { ... }

// Mark experimental features
#[experimental!("1.0.0", "This API may change")]
pub fn experimental_feature() -> Result<(), Error> { ... }

// Document breaking changes
#[breaking_change!("2.0.0", "Signature changed from fn(i32) to fn(i64)")]
pub fn updated_api(value: i64) -> Result<i64, Error> { ... }
```

**Version Compatibility**:
```rust
use ggen_utils::versioning::VersionChecker;

let compatible = VersionChecker::is_compatible("1.0.0", "1.2.0")?;
assert!(compatible); // true - same major version
```

**Benefits**:
- Clear migration paths
- Compile-time deprecation warnings
- SemVer compatibility checking
- Documentation generation

### 3. Pre-Commit Hooks

**Purpose**: Prevent defects from entering the codebase through automated validation.

**Location**: `/scripts/pre-commit-hook.sh`

**Installation**:
```bash
# Install hooks (run once)
./scripts/install-hooks.sh

# Or manually:
ln -s ../../scripts/pre-commit-hook.sh .git/hooks/pre-commit
```

**Validation Checks** (with timeouts):
1. ✅ Compilation check (`cargo make check`) - 10s timeout
2. ✅ Format check (`cargo make fmt`) - 5s timeout
3. ✅ Linting (`cargo make lint`) - 10s timeout
4. ✅ Unit tests (`cargo make test-unit`) - 15s timeout
5. ✅ Security audit (`cargo audit`) - 10s timeout
6. ✅ Debug print detection - Instant

**Andon Signal Integration**:
- **CRITICAL signals** (errors, test failures): Block commit immediately
- **HIGH signals** (warnings, lint issues): Block commit with fix suggestions
- Color-coded output: Red (✗) for failures, Green (✓) for success

**Skip temporarily** (use sparingly):
```bash
git commit --no-verify
```

**Benefits**:
- Stop defects at source (DfLSS principle)
- Fast feedback (<60s total)
- Consistent quality standards
- Prevents "broken window" syndrome

### 4. Metrics Collection System

**Purpose**: Track project health metrics over time for data-driven improvements.

**Location**: `/crates/ggen-utils/src/bin/collect-metrics.rs`

**Metrics Collected**:
- Build time (clean build)
- Test pass rate
- Total/passed tests
- Compiler errors/warnings
- Clippy warnings
- Code/test lines of code
- Code coverage (optional)

**Usage**:
```bash
# Collect metrics (run daily in CI)
cargo run --bin collect-metrics

# Metrics saved to: .metrics/YYYY-MM-DD.json
```

**Dashboard**:
```bash
# Generate HTML dashboard
./scripts/generate-dashboard.sh

# Open in browser
open .metrics/dashboard.html
```

**Benefits**:
- Track improvement trends
- Identify regressions early
- Data-driven decision making
- Visual progress monitoring

### 5. Chicago TDD Test Templates

**Purpose**: Standardize test structure using Chicago-style TDD principles.

**Location**: `/tests/templates/chicago_tdd_template.rs`

**Template Structure**:
```rust
#[test]
fn when_condition_should_observable_result() {
    // Arrange: Set up initial state (real collaborators)
    let system = RealSystem::new();

    // Act: Execute behavior
    let result = system.do_something();

    // Assert: Verify observable state change
    assert_eq!(result.status(), Status::Success);
    assert!(system.state_changed());
}
```

**Key Principles**:
- ✅ State-based testing (verify outputs, not implementation)
- ✅ Real collaborators (minimize mocks)
- ✅ Behavior verification (what code does, not how)
- ✅ AAA pattern (Arrange-Act-Assert)
- ❌ No mock-heavy tests (London style)
- ❌ No meaningless tests (just checking functions exist)

**Benefits**:
- Consistent test structure
- Focus on behavior, not implementation
- Real integration testing
- Better refactoring support

## CI/CD Integration

### GitHub Actions Workflow

Create `.github/workflows/ci.yml`:

```yaml
name: CI

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Install cargo-make
        run: cargo install cargo-make

      - name: Run validation
        run: cargo make ci

      - name: Collect metrics
        run: cargo run --bin collect-metrics

      - name: Upload metrics
        uses: actions/upload-artifact@v3
        with:
          name: metrics
          path: .metrics/*.json
```

### Daily Metrics Collection

Add to `.github/workflows/metrics.yml`:

```yaml
name: Daily Metrics

on:
  schedule:
    - cron: '0 0 * * *'  # Run daily at midnight

jobs:
  metrics:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Collect metrics
        run: cargo run --bin collect-metrics
      - name: Commit metrics
        run: |
          git config user.name "GitHub Actions"
          git config user.email "actions@github.com"
          git add .metrics/
          git commit -m "chore: daily metrics collection" || true
          git push
```

## Makefile.toml Additions

Add these tasks to `Makefile.toml`:

```toml
[tasks.ci-gate]
description = "CI gate validation (runs all checks)"
dependencies = [
    "timeout-check",
    "check",
    "fmt-check",
    "lint",
    "test-unit",
    "audit"
]

[tasks.fmt-check]
description = "Check formatting without modifying files"
command = "cargo"
args = ["fmt", "--", "--check"]

[tasks.collect-metrics]
description = "Collect project metrics"
command = "cargo"
args = ["run", "--bin", "collect-metrics"]

[tasks.install-hooks]
description = "Install Git hooks"
script = ["./scripts/install-hooks.sh"]
```

## Maintenance

### Adding New Templates

1. Add `.tmpl` file to `templates/clap-noun-verb-360/`
2. Run `cargo build` (build.rs auto-discovers)
3. Access via `get_template("your-template-name")`

### Updating Metrics

1. Modify `collect-metrics.rs` to add new metrics
2. Update `Metrics` struct with new fields
3. Update dashboard generation if needed

### Customizing Pre-Commit Hooks

Edit `scripts/pre-commit-hook.sh`:
- Add/remove validation checks
- Adjust timeout durations
- Change failure behavior

## Troubleshooting

### "timeout command not found"

```bash
# macOS
brew install coreutils

# Linux
apt-get install coreutils
```

### Build.rs fails to discover templates

```bash
# Verify templates directory exists
ls -la templates/clap-noun-verb-360/

# Force rebuild
cargo clean && cargo build
```

### Pre-commit hook not running

```bash
# Verify hook is installed
ls -la .git/hooks/pre-commit

# Reinstall
./scripts/install-hooks.sh

# Verify executable
chmod +x .git/hooks/pre-commit
```

## Best Practices

1. **Run metrics daily**: Track trends, not just point-in-time
2. **Review dashboard weekly**: Identify patterns and regressions
3. **Keep hooks fast**: Total time should be <60s
4. **Update templates**: Keep examples current with best practices
5. **Version carefully**: Use deprecation helpers for API changes

## Support

For issues or questions:
- Check existing metrics in `.metrics/` directory
- Review pre-commit hook output for specific failures
- See template validation errors in build output
- Consult versioning docs for API changes

---

**Remember**: These systems are designed to prevent defects and waste from entering the codebase (DfLSS principle). They should fail fast, fail loudly, and fail helpfully.
