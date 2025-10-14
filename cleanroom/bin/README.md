# Cleanroom Validation Scripts

This directory contains scripts for validating Rust crates using cleanroom hermetic environments.

## `validate-crate` - Cargo Publish Validator

A fast, comprehensive validator that mimics `cargo publish` checks without actually publishing.

### Features

- **Fast Execution**: Completes in <10 seconds using parallel execution
- **Comprehensive Checks**: All standard cargo publish validations
- **Hermetic Testing**: Uses cleanroom for isolated test execution
- **JSON Reporting**: Generates detailed validation reports
- **Production-Ready**: Checks for common issues (`.expect()`, `.unwrap()`)

### Usage

```bash
# Basic validation (current directory)
./bin/validate-crate

# Verbose mode with custom timeout
./bin/validate-crate -v -t 30

# Custom output location
./bin/validate-crate -o /tmp/validation.json

# Run from specific project
PROJECT_ROOT=/path/to/project ./bin/validate-crate
```

### Options

| Option | Description | Default |
|--------|-------------|---------|
| `-v, --verbose` | Enable verbose output | Off |
| `-t, --timeout SECONDS` | Maximum validation time | 10 |
| `-o, --output FILE` | Output report file | `validation-report.json` |
| `-h, --help` | Show help message | - |

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `PROJECT_ROOT` | Project directory to validate | Script parent directory |
| `VALIDATION_REPORT` | Output report path | `validation-report.json` |
| `MAX_TIME_SECONDS` | Timeout in seconds | 10 |
| `VERBOSE` | Enable verbose output (0/1) | 0 |

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Ready to publish |
| 1 | Validation failed |
| 2 | Error during validation |

### Validation Checks

The validator performs the following checks:

#### 1. Cargo.toml Validation
- Checks for required fields: `name`, `version`, `edition`, `authors`, `description`, `license`
- Validates metadata structure

#### 2. Parallel Compilation Checks (Fast)
- `cargo check --all-features` - Verify compilation
- `cargo fmt --check` - Check code formatting
- `cargo clippy -- -D warnings` - Run lints

#### 3. Test Execution (Cleanroom)
- `cargo test --all` - Run all tests in isolated environment
- Uses cleanroom swarm orchestration when available

#### 4. Documentation Checks
- README existence
- LICENSE file verification
- Package metadata validation

#### 5. Common Issues
- Uncommitted changes detection
- Untracked files in `src/`
- `.expect()` usage in production code
- `.unwrap()` usage in production code

### Validation Report Format

The validator generates a JSON report with the following structure:

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

### Integration with CI/CD

#### GitHub Actions

```yaml
name: Validate Crate
on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - name: Run validation
        run: |
          chmod +x bin/validate-crate
          ./bin/validate-crate -v
      - name: Upload report
        uses: actions/upload-artifact@v3
        with:
          name: validation-report
          path: validation-report.json
```

#### GitLab CI

```yaml
validate:
  stage: test
  script:
    - chmod +x bin/validate-crate
    - ./bin/validate-crate -v
  artifacts:
    reports:
      junit: validation-report.json
```

### Performance Optimization

The validator achieves <10s execution through:

1. **Parallel Execution**: Runs `cargo check`, `cargo fmt`, and `cargo clippy` concurrently
2. **Early Exit**: Stops on critical failures
3. **Cached Dependencies**: Reuses cargo cache
4. **Cleanroom Reuse**: Leverages container singleton pattern (10-50x faster)

### Development

#### Testing the Validator

```bash
# Test on cleanroom project itself
cd /Users/sac/ggen/cleanroom
./bin/validate-crate -v

# Test with timeout
./bin/validate-crate -t 30

# Generate report in specific location
./bin/validate-crate -o /tmp/cleanroom-validation.json
```

#### Adding Custom Checks

To add custom validation checks, modify the `main()` function:

```bash
# Add custom check function
check_custom_requirement() {
    log_info "Checking custom requirement..."

    # Your check logic here
    if [ condition ]; then
        log_success "Custom check passed"
        return 0
    else
        log_error "Custom check failed"
        return 1
    fi
}

# Call in main()
check_custom_requirement || exit 1
```

### Troubleshooting

#### Validation Times Out

```bash
# Increase timeout
./bin/validate-crate -t 30
```

#### Clippy Warnings

```bash
# Run clippy separately to see all warnings
cargo clippy --all-features --all-targets
```

#### Test Failures

```bash
# Run tests with full output
cargo test --all -- --nocapture
```

### Related Tools

- `cargo publish --dry-run` - Official cargo publish check (slower)
- `cargo package` - Package crate for publishing
- `cargo-release` - Full release workflow automation

### License

This script is part of the cleanroom project and follows the same MIT license.
