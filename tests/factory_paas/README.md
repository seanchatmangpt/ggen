# FactoryPaaS Comprehensive Test Suite

## Overview

This directory contains a comprehensive Chicago TDD test suite for FactoryPaaS, an affiliate marketing and content publishing platform.

## Test Organization

### Integration Tests (`integration_tests.rs`)

End-to-end workflow tests using real collaborators (no mocks):

1. **Affiliate Link Routing** - Route resolution, active/inactive routes, duplicate handling
2. **Click Tracking** - Receipt generation, chain integrity, cryptographic verification
3. **SaaS Subscription Webhooks** - Creation, payment processing, status updates
4. **Content Publishing Pipeline** - Draft→Published workflow, scheduling, validation
5. **Revenue Attribution** - Commission calculations, affiliate attribution, accuracy verification
6. **Subscription Quota Enforcement** - Quota limits, upgrades, tier management
7. **End-to-End Integration** - Complete workflow from click to revenue

### Property Tests (`property_tests.rs`)

Property-based tests using `proptest` to verify invariants:

1. **Route Resolution Determinism** - Same input → same output
2. **Click Receipt Verification** - Cryptographic correctness
3. **Commission Calculation Accuracy** - Mathematical correctness across all tiers
4. **Content Validation Rules** - Title/body requirements, status transitions
5. **Subscription Quota Enforcement** - Quota limits hold across all tiers
6. **Thread Safety** - Deterministic behavior under concurrency

### Load Tests (`load_tests.rs`)

Performance and scalability tests (run with `--ignored` flag):

1. **10k Route Resolutions/Second** - High-throughput routing
2. **1k Concurrent Click Tracking** - Parallel click processing
3. **Batch Content Publishing** - Mass content operations
4. **Revenue Attribution Throughput** - High-volume event recording
5. **Subscription Management** - Concurrent subscription operations
6. **Webhook Processing** - High-volume webhook handling
7. **End-to-End Load Test** - Complete workflow under load

## Running Tests

### Quick Test (Fast)

```bash
# Run integration and property tests only
cargo make test-unit

# Or specifically for FactoryPaaS
cargo test --test factory_paas_runner
```

### Full Test Suite (Slow)

```bash
# Run all tests including load tests
cargo test --test factory_paas_runner -- --ignored --test-threads=1

# Run specific test category
cargo test --test factory_paas_runner integration_tests::
cargo test --test factory_paas_runner property_tests::
cargo test --test factory_paas_runner load_tests::
```

### Load Tests Only

```bash
# Run only load tests (requires --ignored flag)
cargo test --test factory_paas_runner load_tests:: -- --ignored --nocapture
```

## Test Principles (Chicago TDD)

All tests follow Chicago TDD principles:

1. **State-Based Testing** - Verify outputs and state changes, not implementation details
2. **Real Collaborators** - Use real objects, minimize mocks (only mock external I/O)
3. **AAA Pattern** - Arrange/Act/Assert structure for clarity
4. **Behavior Verification** - Test what the code does, not how it does it
5. **Observable Outputs** - Assert on actual effects on system state

## Test Coverage

### Critical Paths Covered

✅ Affiliate link routing end-to-end
✅ Click tracking with receipt generation
✅ SaaS subscription webhooks
✅ Content publishing pipeline
✅ Revenue attribution calculations
✅ Subscription quota enforcement
✅ End-to-end workflow integration

### Property Invariants Verified

✅ Route resolution is deterministic
✅ Click receipts are cryptographically verifiable
✅ Commission calculations are accurate
✅ Content validation rules hold
✅ Subscription quotas are enforced
✅ Thread-safe operations

### Performance Targets

✅ 10k requests/sec to `/r/{route_slug}`
✅ 1k concurrent click tracking
✅ Batch content publishing (1k items)
✅ 10k revenue events/sec
✅ 1k webhooks/sec

## Domain Models

Located in `crates/ggen-saas/src/factory_paas/`:

- `mod.rs` - Core types (RouteSlug, ClickReceipt, SubscriptionTier)
- `affiliate.rs` - Route resolution and management
- `click_tracking.rs` - Click events and receipt generation
- `content.rs` - Content publishing pipeline
- `revenue.rs` - Revenue attribution and commission calculations
- `subscription.rs` - Subscription management and webhooks

## Test Context

The `TestContext` struct provides shared test infrastructure:

```rust
pub struct TestContext {
    pub route_resolver: Arc<RwLock<affiliate::RouteResolver>>,
    pub click_tracker: Arc<RwLock<click_tracking::ClickTracker>>,
    pub content_pipeline: Arc<RwLock<content::ContentPipeline>>,
    pub revenue_attribution: Arc<RwLock<revenue::RevenueAttribution>>,
    pub subscription_manager: Arc<RwLock<subscription::SubscriptionManager>>,
}
```

All tests use real implementations (no mocks) for maximum confidence.

## Test Database

Integration tests use `testcontainers` for PostgreSQL:

```rust
let db = TestDatabase::new().await?;
// Automatically cleaned up when dropped
```

## Performance Benchmarks

Run benchmarks with:

```bash
cargo bench --bench factory_paas_benchmarks
```

## Continuous Integration

Tests are run in CI with:

```bash
# Fast tests (no load tests)
cargo make test

# Full suite (including load tests)
cargo make ci
```

## Future Enhancements

- [ ] Add mutation testing with `cargo-mutants`
- [ ] Add snapshot testing for receipts
- [ ] Add fuzzing tests for route slugs
- [ ] Add chaos engineering tests
- [ ] Add distributed tracing integration

## Related Documentation

- [Chicago TDD Guide](../../docs/testing/chicago-tdd.md)
- [Property Testing Guide](../../docs/testing/property-testing.md)
- [Load Testing Guide](../../docs/testing/load-testing.md)
- [FactoryPaaS Architecture](../../docs/architecture/factory-paas.md)
