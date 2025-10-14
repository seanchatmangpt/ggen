# Tests Directory

This directory contains the comprehensive test suite for the ggen project.

## Ultra-Deploy Test Suite

### Quick Start

```bash
# Build ggen in release mode (required for performance tests)
cargo build --release

# Run all ultra-deploy tests
cargo test --test ultra_deploy_test

# Run with detailed output
cargo test --test ultra_deploy_test -- --nocapture --test-threads=1

# Run specific test
cargo test --test ultra_deploy_test test_ultra_deploy_cli_under_60s -- --nocapture

# Use Makefile
make -f ../Makefile.ultra-deploy test-all

# Use test runner script
../scripts/run-ultra-deploy-tests.sh
```

## Test Files

### ultra_deploy_test.rs
Comprehensive test suite for the ultra-fast ggen → cleanroom → deploy workflow.

**Tests**:
- `test_ultra_deploy_cli_under_60s` - Complete CLI workflow (<60s)
- `test_ggen_cleanroom_integration` - Hermetic environment validation
- `test_output_correctness` - Project structure validation
- `test_performance_benchmark` - Detailed performance analysis
- `test_stage_performance_breakdown` - Individual stage measurement
- `test_workflow_reliability` - Consistency testing
- `test_template_matrix` - Multiple template validation
- `test_sequential_workflow_performance` - Sequential execution
- `test_fake_publish_validation` - Package creation
- `test_error_handling` - Error recovery

**Performance Targets**:
- Code Generation: <5s
- Validation: <10s
- Test Execution: <20s
- Build: <25s
- Fake Publish: <5s
- **Total: <60s**

## Documentation

See `/docs/testing/` for comprehensive documentation:

- `ultra-deploy-testing-guide.md` - Complete testing guide
- `ULTRA_DEPLOY_TEST_README.md` - Quick reference
- `TEST_SUMMARY.md` - Summary of test suite
- `ULTRA_DEPLOY_DELIVERABLES.md` - Complete deliverables list

## CI/CD

Tests run automatically via GitHub Actions:
- **File**: `.github/workflows/ultra-deploy-test.yml`
- **Platforms**: Ubuntu, macOS
- **Rust**: Stable, Nightly
- **Triggers**: Push, PR, Nightly schedule

## Test Reports

Test reports are saved to:
- `target/test-reports/ultra-deploy-test-*.md` - Full reports
- `target/test-reports/*.txt` - Individual test outputs

## Usage Examples

### Quick Validation
```bash
# Single test with timing
cargo test --test ultra_deploy_test test_ultra_deploy_cli_under_60s -- --nocapture
```

### Performance Analysis
```bash
# Detailed benchmark
cargo test --test ultra_deploy_test test_performance_benchmark -- --nocapture
```

### Full Suite
```bash
# All tests with detailed output
cargo test --test ultra_deploy_test -- --nocapture --test-threads=1 | tee test-results.txt
```

### Reliability Check
```bash
# Multiple iterations
for i in {1..5}; do
    echo "Iteration $i"
    cargo test --test ultra_deploy_test test_workflow_reliability -- --nocapture
done
```

## Troubleshooting

### Docker not available
```bash
# Check Docker status
docker info

# On macOS
open -a Docker

# On Linux
sudo systemctl start docker
```

### ggen binary not found
```bash
# Build release binary
cargo build --release

# Verify location
ls -la target/release/ggen
```

### Tests timing out
```bash
# Increase timeout
cargo test --test ultra_deploy_test -- --timeout 300

# Check system resources
docker stats
top
```

## Contributing

When adding tests:
1. Follow existing patterns
2. Add timing assertions for performance tests
3. Use `#[serial]` for tests that can't run in parallel
4. Document test purpose and success criteria
5. Update this README

## Support

- **Documentation**: `/docs/testing/`
- **Issues**: GitHub Issues
- **CI/CD Results**: GitHub Actions tab
