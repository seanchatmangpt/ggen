# Quick Test Commands Reference

## Essential Commands

### Run All Tests (Excluding Marketplace)
```bash
cargo test --workspace --exclude ggen-marketplace
```

### Run Tests with Output
```bash
cargo test --workspace --exclude ggen-marketplace -- --nocapture
```

### Run Specific Test
```bash
cargo test test_name --package package_name
```

## Module-Specific Tests

### ggen-core Tests
```bash
# All tests
cargo test -p ggen-core

# Unit tests only
cargo test -p ggen-core --test marketplace_tests_main unit::

# Integration tests only
cargo test -p ggen-core --test marketplace_tests_main integration::

# Property tests
cargo test -p ggen-core --features proptest --test marketplace_tests_main property::

# Security tests
cargo test -p ggen-core --test marketplace_tests_main security::
```

### CLI Tests
```bash
# All CLI tests
cargo test -p ggen-cli-lib

# Integration tests
cargo test -p ggen-cli-lib --test integration_tests

# Cleanroom tests
cargo test -p ggen-cli-lib --test cleanroom_production
```

### AI Tests
```bash
# All AI tests
cargo test -p ggen-ai

# Specific AI features
cargo test -p ggen-ai -- ai_generate
```

## Special Test Types

### BDD Tests (Cucumber)
```bash
cargo test --test bdd
```

### End-to-End Tests
```bash
# Production marketplace
cargo test --test e2e_production_marketplace

# PQC infrastructure
cargo test --test e2e_pqc_infrastructure

# GitHub integration
cargo test --test e2e_github_integration

# Ultra deploy
cargo test --test ultra_deploy_test
```

### Cleanroom Tests
```bash
cargo test --test cli_integration_cleanroom
```

### Lifecycle Tests
```bash
cargo test lifecycle --package ggen-core
```

## Performance & Benchmarks

### Run Benchmarks
```bash
# All benchmarks
cargo bench --workspace --exclude ggen-marketplace

# Specific benchmark
cargo bench --bench marketplace_benchmarks
cargo bench --bench lifecycle_benchmarks
```

### Profile Performance
```bash
cargo build --release
cargo flamegraph --test test_name
```

## Coverage & Analysis

### Test Coverage
```bash
# Install tarpaulin
cargo install cargo-tarpaulin

# Generate coverage report
cargo tarpaulin --workspace --exclude ggen-marketplace \
  --out Html --output-dir coverage
```

### Security Audit
```bash
cargo audit
```

### Dependency Check
```bash
cargo tree --duplicates
```

## CI Simulation

### Run Full CI Workflow
```bash
# Format check
cargo fmt --all -- --check

# Clippy
cargo clippy --all-targets --all-features -- -D warnings

# Test
cargo test --workspace --exclude ggen-marketplace

# Build
cargo build --release
```

## Debugging Tests

### Run with Debug Output
```bash
RUST_LOG=debug cargo test --test test_name -- --nocapture
```

### Run Single Test
```bash
cargo test test_function_name -- --exact --nocapture
```

### Show Test Output Always
```bash
cargo test -- --show-output
```

## Troubleshooting

### Clean and Rebuild
```bash
cargo clean
cargo build --workspace
cargo test --workspace --exclude ggen-marketplace
```

### Check Compilation Only
```bash
cargo check --workspace --all-features
```

### Test with Specific Features
```bash
cargo test --workspace --exclude ggen-marketplace --all-features
```

## Quick Validation

### Pre-commit Check
```bash
cargo fmt --all
cargo clippy --all-targets --all-features
cargo test --workspace --exclude ggen-marketplace
```

### Pre-release Check
```bash
cargo clean
cargo build --release
cargo test --workspace --exclude ggen-marketplace --release
cargo bench --workspace --exclude ggen-marketplace
cargo audit
```

## Marketplace Tests (When Fixed)

```bash
# Compile with all features
cargo check -p ggen-marketplace --all-features

# Run tests
cargo test -p ggen-marketplace --all-features

# Run specific test categories
cargo test -p ggen-marketplace --all-features unit::
cargo test -p ggen-marketplace --all-features integration::
cargo test -p ggen-marketplace --all-features property::
cargo test -p ggen-marketplace --all-features security::
```

---

**Quick Start:**
```bash
# Most common workflow
cargo test --workspace --exclude ggen-marketplace && cargo clippy
```

**Before Release:**
```bash
# Full validation
cargo clean && \
cargo build --release && \
cargo test --workspace --exclude ggen-marketplace --release && \
cargo bench --workspace --exclude ggen-marketplace && \
cargo audit
```
