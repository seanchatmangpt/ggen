# FactoryPaaS Test Suite - Quick Start Guide

## Installation

```bash
# Clone repository
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen

# Install dependencies
cargo build
```

## Running Tests

### Quick Test (2-5 seconds)

```bash
# Run integration and property tests
cargo test --test factory_paas_runner
```

### Full Test Suite with Load Tests (20-30 seconds)

```bash
# Run all tests including performance tests
cargo test --test factory_paas_runner -- --ignored --test-threads=1 --nocapture
```

### Specific Test Categories

```bash
# Integration tests only
cargo test --test factory_paas_runner integration_tests::

# Property tests only
cargo test --test factory_paas_runner property_tests::

# Load tests only
cargo test --test factory_paas_runner load_tests:: -- --ignored --nocapture
```

### Run Specific Test

```bash
# Run single test by name
cargo test --test factory_paas_runner test_affiliate_link_routing_end_to_end -- --exact

# Run all tests matching pattern
cargo test --test factory_paas_runner click_tracking
```

## Understanding Test Output

### Successful Test Run

```
test integration_tests::test_affiliate_link_routing_end_to_end ... ok
test integration_tests::test_click_tracking_generates_valid_receipt ... ok
test integration_tests::test_revenue_attribution_accurate_commission ... ok

test result: ok. 22 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### Load Test Output

```
✓ Load Test: 10,000 route resolutions completed in 847ms (11,804 req/sec)
✓ Load Test: 1,000 concurrent clicks tracked in 1.2s (833 clicks/sec)
✓ Load Test: 1,000 content items published in 890ms (1,123 items/sec)
```

## Test Structure

### Integration Tests (22 tests)

Test real workflows with actual state changes:

- `test_affiliate_link_routing_end_to_end` - Route resolution
- `test_click_tracking_generates_valid_receipt` - Click receipt generation
- `test_subscription_creation_webhook` - Webhook handling
- `test_content_draft_to_published_workflow` - Publishing pipeline
- `test_revenue_attribution_accurate_commission` - Commission calculation
- `test_full_workflow_affiliate_click_to_revenue` - End-to-end integration

### Property Tests (15 tests)

Verify invariants hold across thousands of generated inputs:

- `prop_route_resolution_is_deterministic` - Same input → same output
- `prop_click_receipts_always_verify` - Cryptographic correctness
- `prop_commission_calculation_accurate` - Math correctness
- `prop_content_validation_rules` - Validation always works
- `prop_quota_enforced_at_limit` - Quotas never exceeded

### Load Tests (10 tests, run with `--ignored`)

Measure performance under load:

- `load_test_10k_route_resolutions_per_second` - Throughput test
- `load_test_1k_concurrent_click_tracking` - Concurrency test
- `load_test_batch_content_publishing` - Bulk operations
- `load_test_end_to_end_workflow` - Full system under load

## Debugging Failed Tests

### Compilation Errors

```bash
# Check for compilation errors
cargo check --package ggen-saas

# View full error messages
cargo check --package ggen-saas --verbose
```

### Test Failures

```bash
# Run with verbose output
cargo test --test factory_paas_runner -- --nocapture

# Run single failing test
cargo test --test factory_paas_runner <test_name> -- --exact --nocapture
```

### Performance Issues

```bash
# Profile load tests
cargo test --test factory_paas_runner load_tests:: -- --ignored --nocapture

# Check individual latencies
cargo test --test factory_paas_runner load_test_route_resolution_latency -- --ignored --nocapture
```

## Test Development Workflow

### 1. Write Failing Test (RED)

```rust
#[tokio::test]
async fn test_new_feature() {
    // ARRANGE: Set up test data
    let ctx = TestContext::new();

    // ACT: Execute feature
    let result = ctx.do_something().await;

    // ASSERT: Verify outcome
    assert!(result.is_ok(), "Feature should work");
}
```

### 2. Implement Minimal Code (GREEN)

```rust
// Add minimal implementation to pass test
pub async fn do_something(&self) -> Result<(), Error> {
    // Simplest implementation that passes
    Ok(())
}
```

### 3. Refactor (REFACTOR)

```rust
// Improve implementation while keeping test green
pub async fn do_something(&self) -> Result<(), Error> {
    // Better implementation
    self.validate()?;
    self.execute()?;
    Ok(())
}
```

### 4. Run Tests

```bash
# Quick feedback
cargo test --test factory_paas_runner test_new_feature -- --exact

# Full suite
cargo make test
```

## Common Issues

### Issue: Tests timeout

**Solution**: Run with fewer threads

```bash
cargo test --test factory_paas_runner -- --test-threads=1
```

### Issue: Load tests fail

**Solution**: Load tests are ignored by default, run explicitly

```bash
cargo test --test factory_paas_runner load_tests:: -- --ignored
```

### Issue: Property tests are slow

**Solution**: Property tests generate 1000+ cases, this is expected

```bash
# Reduce test cases for faster feedback
PROPTEST_CASES=100 cargo test --test factory_paas_runner property_tests::
```

### Issue: Database connection errors

**Solution**: Ensure Docker is running for testcontainers

```bash
# Check Docker status
docker ps

# Start Docker if needed
systemctl start docker
```

## Performance Benchmarks

Expected performance on modern hardware (2023+ CPU):

| Operation | Throughput | Latency (p99) |
|-----------|------------|---------------|
| Route resolution | 10k+ req/sec | < 10ms |
| Click tracking | 1k+ concurrent | < 50ms |
| Content publishing | 1k+ items/sec | < 100ms |
| Revenue events | 10k+ events/sec | < 10ms |
| Webhook processing | 1k+ hooks/sec | < 50ms |

## CI/CD Integration

### GitHub Actions

```yaml
name: FactoryPaaS Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - name: Run tests
        run: cargo test --test factory_paas_runner
      - name: Run load tests
        run: cargo test --test factory_paas_runner -- --ignored --test-threads=1
```

### Pre-commit Hook

```bash
# .git/hooks/pre-commit
#!/bin/bash
cargo test --test factory_paas_runner
if [ $? -ne 0 ]; then
  echo "Tests failed. Commit aborted."
  exit 1
fi
```

## Next Steps

1. **Read the architecture**: See `README.md` for domain model overview
2. **Explore tests**: Browse `integration_tests.rs` for examples
3. **Add new tests**: Follow Chicago TDD pattern
4. **Run benchmarks**: Profile with `cargo bench`
5. **Deploy to production**: All tests must pass

## Support

- **Documentation**: See `README.md` and `IMPLEMENTATION_SUMMARY.md`
- **Issues**: GitHub Issues for bug reports
- **Discussions**: GitHub Discussions for questions
- **Contributing**: See `CONTRIBUTING.md` for guidelines

---

Happy testing! 🚀
