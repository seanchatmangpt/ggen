# Build System Guide

## cargo make Tasks and Targets

ggen uses **Makefile.toml** with `cargo-make` for deterministic builds with timeout SLAs.

### Quick Feedback Loop (Fast Iteration)

```bash
# 5s timeout - Fast syntax/type check
cargo make check

# 10s timeout - Unit tests only
cargo make test-unit

# Run single test
cargo make test test_name

# Clippy linting with timeout
cargo make lint
```

### Full Validation Pipeline

```bash
# All tests (unit + integration) - 10s unit + 30s integration
cargo make test

# Pre-commit: format + lint + unit tests
cargo make pre-commit

# Complete CI pipeline
cargo make ci

# Release validation (comprehensive checks)
cargo make release-validate
```

### Timeout SLAs (CRITICAL)

Every target has a timeout wrapper:

```toml
[tasks.check]
command = "timeout"
args = ["5s", "cargo", "check"]
description = "Quick syntax/type check (5s SLA)"

[tasks.build]
command = "timeout"
args = ["10s", "cargo", "build"]
description = "Debug build (10s SLA)"

[tasks.test-unit]
command = "timeout"
args = ["10s", "cargo", "test", "--lib"]
description = "Unit tests (10s SLA)"

[tasks.test]
dependencies = ["test-unit"]
command = "timeout"
args = ["30s", "cargo", "test", "--test"]
description = "All tests (10s unit + 30s integration)"

[tasks.release]
command = "timeout"
args = ["30s", "cargo", "build", "--release"]
description = "Release build (30s SLA)"
```

### Performance Profiling

```bash
# Run benchmarks
cargo make bench

# Check performance SLOs
cargo make slo-check

# Profile with perf
cargo make profile

# Memory analysis
cargo make profile-memory
```

### Security & Quality

```bash
# Dependency vulnerability check
cargo make audit

# Template validation
cargo make validate-templates

# RDF ontology validation
cargo make validate-rdf

# Security scanning
cargo make security-scan
```

### Development Utilities

```bash
# Live reload watch mode
cargo make watch

# Debug mode (verbose output)
cargo make debug

# Generate completions (bash, zsh, fish)
cargo make completions

# Verify timeout command exists
cargo make timeout-check
```

## Makefile.toml Structure

```toml
[tasks]

# Task definitions with timeout wrappers
[tasks.check]
command = "timeout"
args = ["5s", "cargo", "check", "--all-features"]
description = "Quick compilation check (5s SLA)"

[tasks.test-unit]
condition = { environment_contains = { "CARGO_MAKE_SKIP_SLOW_TESTS" = "false" } }
command = "timeout"
args = ["10s", "cargo", "test", "--lib", "--all-features", "--quiet"]

# Task dependencies
[tasks.pre-commit]
dependencies = ["fmt", "lint", "test-unit"]
description = "Run before committing (format + lint + unit tests)"

# Workspace-wide tasks
[tasks.test-all-crates]
script = '''
for crate in crates/*; do
    echo "Testing $crate..."
    timeout 15s cargo test -p $(basename $crate) || exit 1
done
'''

# Conditional tasks
[tasks.ci]
condition = { files_exist = ["Makefile.toml"] }
dependencies = ["check", "lint", "test", "audit"]
description = "Full CI pipeline"
```

## Pre-Commit & Pre-Push Hooks

Hooks validate code before commits:

```bash
# Validate timeout command exists
cargo make timeout-check

# Format code
cargo make fmt

# Lint with clippy
cargo make lint

# Run unit tests
cargo make test-unit

# All together
cargo make pre-commit
```

### Hook Configuration

Hooks are defined in `.git/hooks/`:

```bash
#!/bin/bash
# .git/hooks/pre-commit

set -e

echo "Running pre-commit checks..."

# Quick format + lint + unit tests
cargo make pre-commit || {
    echo "Pre-commit checks failed!"
    exit 1
}

echo "Pre-commit checks passed!"
```

## CI/CD Validation Gates

```bash
# Pre-push validation
cargo make ci

# Pre-release validation
cargo make release-validate
```

Gates check:
- ✅ No compiler errors
- ✅ No clippy warnings
- ✅ All tests passing
- ✅ No security vulnerabilities
- ✅ Performance SLOs met
- ✅ Code coverage ≥80%

## Andon Signals (Stop-the-Line Quality)

**Visual problem indicators** that require immediate action:

### CRITICAL (RED) Signals - STOP THE LINE

- **Compiler errors** (`error[E...]`)
  - Action: STOP - Fix immediately
  - Verification: `cargo make check`

- **Test failures** (`test ... FAILED`)
  - Action: STOP - Debug and fix
  - Verification: `cargo make test`

### HIGH (YELLOW) Signals - Should Stop

- **Compiler warnings** (`warning:`)
  - Action: Fix warnings before proceeding
  - Verification: `cargo make check` output

- **Clippy errors** (clippy warnings)
  - Action: Fix linting issues
  - Verification: `cargo make lint`

### Andon Workflow

```
┌─────────────────┐
│  Run command    │
└────────┬────────┘
         │
         ▼
    ┌─────────────────────────┐
    │  Signal Detected?       │
    │  (error/warning/fail)   │
    └────────┬────────────────┘
    │        │
    │ YES    │ NO
    ▼        │
┌──────────────┐       │
│ STOP THE LINE│       │
│ (Red light)  │       │
└──────────────┘       │
    │                  │
    │ ROOT CAUSE?      │
    │ 5 WHYS          │
    │                  │
    ▼                  │
┌──────────────┐       │
│ FIX ISSUE    │       │
│ (address root│       │
│  cause, not  │       │
│  symptom)    │       │
└──────┬───────┘       │
       │               │
       ▼               │
┌──────────────┐       │
│ RE-RUN CHECK │       │
│ Verify signal│       │
│ cleared      │       │
└──────┬───────┘       │
       │               │
       └───────┬───────┘
               ▼
         ┌──────────────┐
         │ PROCEED      │
         │ (Green light)│
         └──────────────┘
```

## SLO Verification

ggen has strict performance Service Level Objectives:

```bash
# Verify all SLOs
cargo make slo-check

# Individual SLO checks
cargo make slo-check-compile  # First build ≤ 15s
cargo make slo-check-incremental  # Incremental ≤ 2s
cargo make slo-check-rdf  # RDF processing ≤ 5s for 1k+ triples
cargo make slo-check-memory  # Generation memory ≤ 100MB
cargo make slo-check-cli  # CLI scaffolding ≤ 3s end-to-end
```

### Performance Targets

- **First compile:** ≤ 15s
- **Incremental rebuild:** ≤ 2s
- **RDF processing:** ≤ 5s for 1000+ triples
- **Memory overhead:** ≤ 100MB for typical operations
- **CLI command:** ≤ 3s end-to-end scaffolding
- **Template generation:** ≤ 500ms per file
- **SPARQL query:** ≤ 100ms (with caching)

## Workspace Management

```bash
# Build specific crate
cargo make build -p ggen-core

# Test specific crate
cargo make test -p ggen-cli

# Test all crates in parallel
cargo make test-all-crates

# Clean workspace
cargo make clean

# Check dependencies
cargo tree
cargo audit
```

## Profiling & Debugging

```bash
# Verbose output during build
cargo make debug

# Profile with flamegraph
cargo make flamegraph

# Memory profiling
cargo make profile-memory

# Check compilation time
cargo make compile-time-profile
```

## Critical Rules

1. **NEVER USE DIRECT CARGO** - Always use `cargo make`
2. **ALWAYS USE TIMEOUT WRAPPERS** - Prevent freezing
3. **STOP ON SIGNALS** - Don't ignore compiler errors/warnings
4. **FIX ROOT CAUSE** - Not just symptoms
5. **VERIFY SIGNALS CLEARED** - Before proceeding
6. **RESPECT SLOs** - Performance targets are non-negotiable
7. **BATCH OPERATIONS** - Use `cargo make pre-commit` not individual steps

---

## Example Makefile.toml

```toml
[env]
CARGO_MAKE_SKIP_SLOW_TESTS = { value = "false" }

[tasks.default]
alias = "check"

[tasks.check]
command = "timeout"
args = ["5s", "cargo", "check", "--all-features"]
description = "Quick syntax check (5s SLA)"

[tasks.fmt]
command = "cargo"
args = ["fmt", "--all"]
description = "Format code"

[tasks.lint]
command = "timeout"
args = ["10s", "cargo", "clippy", "--all-features", "--all-targets"]
description = "Clippy linting (10s SLA)"

[tasks.test-unit]
command = "timeout"
args = ["10s", "cargo", "test", "--lib"]
description = "Unit tests (10s SLA)"

[tasks.test]
dependencies = ["test-unit"]
command = "timeout"
args = ["30s", "cargo", "test", "--test"]
description = "All tests (unit + integration)"

[tasks.pre-commit]
dependencies = ["fmt", "lint", "test-unit"]
description = "Pre-commit checks"

[tasks.ci]
dependencies = ["check", "lint", "test", "audit"]
description = "Complete CI pipeline"

[tasks.bench]
command = "timeout"
args = ["60s", "cargo", "bench"]
description = "Run benchmarks (60s SLA)"

[tasks.audit]
command = "cargo"
args = ["audit"]
description = "Security vulnerability check"

[tasks.release]
dependencies = ["ci"]
command = "timeout"
args = ["30s", "cargo", "build", "--release"]
description = "Release build with CI gates"

[tasks.slo-check]
script = '''
echo "Checking SLOs..."
# Add specific SLO checks here
'''
```
