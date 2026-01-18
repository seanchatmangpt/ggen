<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Andon Signal Validation Framework - Quick Start](#andon-signal-validation-framework---quick-start)
  - [🚀 Get Started in 5 Minutes](#-get-started-in-5-minutes)
    - [Prerequisites](#prerequisites)
    - [1. Run Validation Locally](#1-run-validation-locally)
    - [2. Test with Act (Local GitHub Actions)](#2-test-with-act-local-github-actions)
    - [3. Pre-Commit Integration](#3-pre-commit-integration)
    - [4. CI/CD Integration](#4-cicd-integration)
  - [Understanding Andon Signals](#understanding-andon-signals)
  - [Three-Layer Validation](#three-layer-validation)
    - [Layer 1: Compile-Time (RED)](#layer-1-compile-time-red)
    - [Layer 2: Test-Time (YELLOW)](#layer-2-test-time-yellow)
    - [Layer 3: Runtime (GREEN)](#layer-3-runtime-green)
  - [Common Commands](#common-commands)
  - [Troubleshooting](#troubleshooting)
    - [Validation Fails](#validation-fails)
    - [Act Not Working](#act-not-working)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Andon Signal Validation Framework - Quick Start

## 🚀 Get Started in 5 Minutes

### Prerequisites

Before using the framework, ensure you have:

1. **Rust and Cargo** (installed via rustup)
   ```bash
   rustc --version
   cargo --version
   ```

2. **cargo-make** (task runner)
   ```bash
   cargo install cargo-make
   ```

3. **Docker** (for act integration - optional but recommended)
   ```bash
   docker ps
   ```

4. **act** (for local GitHub Actions testing - optional)
   ```bash
   # macOS
   brew install act
   
   # Linux
   curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
   ```

**Verify Prerequisites**:
```bash
cargo make act-status  # Checks act and Docker
```

### 1. Run Validation Locally

```bash
# Generate validation report
cargo make validation-report

# Monitor validation status
cargo make monitor-validation
```

### 2. Test with Act (Local GitHub Actions)

```bash
# Check act is installed
cargo make act-status

# Test validation workflow locally
cargo make act-validation

# Test specific layer
cargo make act-validation JOB=compile-time
```

### 3. Pre-Commit Integration

The framework runs automatically on pre-commit:

```bash
# Manual pre-commit check
cargo make pre-commit

# Or commit normally (hooks run automatically)
git commit -m "Your message"
```

### 4. CI/CD Integration

The framework runs automatically on:
- **Push to master/main**: Full validation
- **Pull Requests**: Full validation
- **Manual Trigger**: `workflow_dispatch`

View results in GitHub Actions: `.github/workflows/andon-validation.yml`

---

## Understanding Andon Signals

| Signal | Meaning | Action |
|--------|---------|--------|
| 🔴 **RED** | Critical failure | **Stop the line** - Fix immediately |
| 🟡 **YELLOW** | Warning/partial failure | Investigate - May proceed with caution |
| 🟢 **GREEN** | All validation passed | Proceed to next stage |

---

## Three-Layer Validation

### Layer 1: Compile-Time (RED)
- `cargo make check` - Compilation
- `cargo make lint` - Linting
- **Purpose**: Catch issues before code runs

### Layer 2: Test-Time (YELLOW)
- `cargo make test-unit` - Unit tests
- `cargo make test-clnrm` - Integration tests
- **Purpose**: Verify behavior, not just execution

### Layer 3: Runtime (GREEN)
- `cargo make verify-cli` - CLI verification
- **Purpose**: Verify CLI commands actually work

---

## Common Commands

```bash
# Validation
cargo make verify-cli              # Verify CLI commands
cargo make validation-report        # Generate report
cargo make monitor-validation      # Monitor and alert

# Testing with Act
cargo make act-validation           # Test validation workflow
cargo make act-status              # Check act installation

# Pre-commit
cargo make pre-commit               # Run all validation
```

---

## Troubleshooting

### Validation Fails

1. **Check which layer failed**:
   ```bash
   cargo make validation-report
   ```

2. **Fix the failing layer**:
   - Layer 1 (RED): `cargo make check` or `cargo make lint`
   - Layer 2 (YELLOW): `cargo make test-unit` or `cargo make test-clnrm`
   - Layer 3 (GREEN): `cargo make verify-cli`

3. **Re-run validation**:
   ```bash
   cargo make validation-report
   cargo make monitor-validation
   ```

### Act Not Working

1. **Check installation**:
   ```bash
   cargo make act-status
   ```

2. **Install act**:
   ```bash
   # macOS
   brew install act
   
   # Linux
   curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
   ```

3. **Check Docker**:
   ```bash
   docker ps
   ```

---

## Next Steps

- Read full documentation: `docs/innovation/ANDON_VALIDATION_FRAMEWORK.md`
- View implementation details: `docs/innovation/PHASE*_IMPLEMENTATION.md`
- Check status: `docs/innovation/STATUS.md`

---

**Framework Version**: v1.0.0
**Status**: ✅ Complete and Ready

