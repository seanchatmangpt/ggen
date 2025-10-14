# Cargo Publish Validation

This document describes the cleanroom-based cargo publish validator and how to use it for pre-publish validation.

## Overview

The `validate-crate` script provides fast, comprehensive validation that mimics `cargo publish` checks without actually publishing. It runs in **under 10 seconds** using parallel execution and cleanroom hermetic environments.

## Quick Start

```bash
# Run validation on current project
./bin/validate-crate

# Verbose mode with detailed output
./bin/validate-crate -v

# Custom timeout (default: 10s)
./bin/validate-crate -t 30

# Generate report in specific location
./bin/validate-crate -o /tmp/validation.json
```

## What It Validates

### 1. Cargo.toml Requirements ✅

Checks for required publishing fields:
- `name` - Package name
- `version` - Version number
- `edition` - Rust edition
- `authors` - Package authors
- `description` - Package description
- `license` - License identifier

**Example Cargo.toml:**
```toml
[package]
name = "my-crate"
version = "0.1.0"
edition = "2021"
authors = ["Your Name <you@example.com>"]
description = "A helpful description"
license = "MIT"
```

### 2. Compilation Checks ⚡ (Parallel)

**Fast checks run in parallel:**

| Check | Purpose | Time |
|-------|---------|------|
| `cargo check` | Verify compilation | ~3-5s |
| `cargo fmt --check` | Code formatting | ~1s |
| `cargo clippy` | Lints and warnings | ~3-5s |

**Total parallel time:** ~5-7s (vs ~15-20s sequential)

### 3. Test Execution 🧪

Runs all tests in isolated cleanroom environment:
```bash
cargo test --all
```

- Uses cleanroom for hermetic isolation
- Falls back to standard cargo test if cleanroom unavailable
- Captures full test output

### 4. Documentation Requirements 📚

| File | Required | Purpose |
|------|----------|---------|
| README.md | Recommended | Project documentation |
| LICENSE | Recommended | License text |
| Cargo.toml metadata | Required | Package metadata |

### 5. Common Issues Detection ⚠️

Automatically checks for:

- **Uncommitted changes** in git
- **Untracked files** in `src/`
- **`.expect()` usage** in production code
- **`.unwrap()` usage** in production code
- **Missing documentation**
- **Invalid metadata**

## Validation Report

After validation, a JSON report is generated:

```json
{
  "timestamp": "2025-10-13T19:30:00Z",
  "project_root": "/path/to/project",
  "duration_seconds": 8,
  "status": "passed",
  "checks": {
    "passed": 8,
    "failed": 0,
    "warnings": 2
  },
  "ready_to_publish": true,
  "validations": {
    "cargo_toml": "passed",
    "cargo_check": "passed",
    "cargo_fmt": "passed",
    "cargo_clippy": "passed",
    "cargo_test": "passed",
    "readme": "passed",
    "license": "passed",
    "metadata": "passed"
  }
}
```

## Exit Codes

| Code | Status | Meaning |
|------|--------|---------|
| 0 | ✅ Success | Ready to publish |
| 1 | ❌ Failed | Validation failed |
| 2 | ⚠️ Error | Error during validation |

## Performance

The validator achieves fast execution through:

### Parallel Execution Strategy

```
Time →
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Sequential (slow):
├── cargo check     ████████ 5s
├── cargo fmt       ██ 1s
└── cargo clippy    ████████ 5s
Total: 11s

Parallel (fast):
├── cargo check     ████████
├── cargo fmt       ██
└── cargo clippy    ████████
Total: 5s (fastest of 3)
```

### Optimization Techniques

1. **Parallel Check Execution**
   - Runs `check`, `fmt`, `clippy` concurrently
   - Uses `&` background jobs in bash
   - Waits for all with timeout

2. **Early Exit Strategy**
   - Stops on critical failures
   - Skips remaining checks if timeout exceeded
   - Preserves partial results

3. **Cargo Cache Reuse**
   - Leverages cargo's build cache
   - Avoids unnecessary recompilation
   - Shares dependencies across checks

4. **Cleanroom Container Singleton**
   - Reuses containers when possible
   - 10-50x faster than creating new containers
   - Hermetic isolation maintained

## Integration Examples

### GitHub Actions

```yaml
name: Pre-Publish Validation
on:
  pull_request:
    branches: [main]
  push:
    branches: [main]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          components: rustfmt, clippy

      - name: Run validator
        run: |
          chmod +x bin/validate-crate
          ./bin/validate-crate -v

      - name: Upload validation report
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: validation-report
          path: validation-report.json

      - name: Check ready to publish
        run: |
          if jq -e '.ready_to_publish == true' validation-report.json; then
            echo "✅ Ready to publish!"
          else
            echo "❌ Not ready to publish"
            exit 1
          fi
```

### Pre-Commit Hook

```bash
# .git/hooks/pre-commit
#!/bin/bash
set -e

echo "Running pre-publish validation..."
./bin/validate-crate -t 15

if [ $? -eq 0 ]; then
    echo "✅ Validation passed"
else
    echo "❌ Validation failed - fix issues before committing"
    exit 1
fi
```

### Makefile Integration

```makefile
.PHONY: validate
validate:
	@echo "Running cargo publish validation..."
	@./bin/validate-crate -v

.PHONY: validate-fast
validate-fast:
	@echo "Running fast validation (5s timeout)..."
	@./bin/validate-crate -t 5

.PHONY: validate-thorough
validate-thorough:
	@echo "Running thorough validation (30s timeout)..."
	@./bin/validate-crate -v -t 30
```

## Troubleshooting

### Validation Timeout

**Problem:** Validation exceeds time limit

**Solutions:**
```bash
# Increase timeout
./bin/validate-crate -t 30

# Check what's slow
./bin/validate-crate -v | grep "Running"

# Skip tests for faster check
cargo check && cargo fmt --check && cargo clippy
```

### Clippy Warnings Fail Build

**Problem:** Clippy finds warnings treated as errors

**Solutions:**
```bash
# Run clippy separately to see all issues
cargo clippy --all-features --all-targets

# Fix issues automatically where possible
cargo clippy --fix --allow-dirty --allow-staged

# View specific warnings
cargo clippy -- -W clippy::unwrap_used
```

### Test Failures

**Problem:** Tests fail during validation

**Solutions:**
```bash
# Run tests with full output
cargo test --all -- --nocapture

# Run specific failing test
cargo test <test_name> -- --nocapture

# Check for environment issues
cargo test --all -- --test-threads=1
```

### Missing Dependencies

**Problem:** `cargo check` fails with missing dependencies

**Solutions:**
```bash
# Update dependencies
cargo update

# Check for conflicting versions
cargo tree

# Clean and rebuild
cargo clean && cargo check
```

## Best Practices

### 1. Run Before Publishing

```bash
# Always validate before publishing
./bin/validate-crate -v

# If validation passes, publish
cargo publish --dry-run
cargo publish
```

### 2. Fix Issues Early

```bash
# Run validation frequently during development
./bin/validate-crate

# Fix formatting
cargo fmt

# Fix clippy warnings
cargo clippy --fix
```

### 3. Automate in CI/CD

- Add validation to CI/CD pipeline
- Block merges on validation failures
- Generate reports for review

### 4. Monitor Performance

```bash
# Time validation execution
time ./bin/validate-crate

# Check if under target time
./bin/validate-crate -t 10 && echo "Fast enough!"
```

## Comparison with Cargo Publish

| Feature | `cargo publish --dry-run` | `validate-crate` |
|---------|---------------------------|------------------|
| Speed | Slow (~30-60s) | Fast (<10s) |
| Parallel | No | Yes |
| Hermetic | No | Yes (cleanroom) |
| Custom checks | No | Yes |
| JSON report | No | Yes |
| CI/CD friendly | Limited | Excellent |

## Future Enhancements

Planned improvements:

1. **Cleanroom Swarm Integration**
   - Distributed test execution
   - Parallel test suite runs
   - Resource usage optimization

2. **Custom Validation Rules**
   - Project-specific checks
   - Plugin system
   - Configurable rules

3. **Advanced Reporting**
   - HTML reports
   - Trend analysis
   - Performance metrics

4. **Integration**
   - IDE plugins
   - Git hooks
   - Release automation

## Related Tools

- **`cargo publish`** - Official publishing command
- **`cargo package`** - Create package tarball
- **`cargo-release`** - Release automation
- **`cargo-audit`** - Security auditing
- **`cargo-outdated`** - Dependency checking

## See Also

- [Cargo.toml Manifest Format](https://doc.rust-lang.org/cargo/reference/manifest.html)
- [Publishing on crates.io](https://doc.rust-lang.org/cargo/reference/publishing.html)
- [Cleanroom Documentation](../README.md)
- [Validator Script](../bin/validate-crate)
