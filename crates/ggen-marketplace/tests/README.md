# ggen-marketplace Testing Guide

## 🎯 Philosophy: 80/20 Testing Strategy

This test suite follows the **80/20 principle**: focus on the **20% of tests that provide 80% of confidence**.

**Key Principles:**
- ✅ **Test behavior, not implementation**
- ✅ **Test critical paths, not every edge case**
- ✅ **Test real scenarios, not mocked components**
- ✅ **Test properties and invariants, not specific values**
- ❌ **Don't test trivial code (getters, setters)**
- ❌ **Don't test third-party libraries**
- ❌ **Don't test internal implementation details**

**📖 See:** [`80_20_TESTING_STRATEGY.md`](./80_20_TESTING_STRATEGY.md) for complete philosophy.

---

## 📁 Test Structure

```
tests/
├── README.md                          # This file
├── 80_20_TESTING_STRATEGY.md         # Testing philosophy
├── common/
│   └── mod.rs                         # Shared test helpers
├── integration_critical_paths.rs     # 15 high-value integration tests
├── property_based_invariants.rs      # 9 mathematical invariants
└── error_scenarios.rs                # 20+ error handling tests
```

---

## 🚀 Running Tests

### Run All Tests
```bash
cargo test --package ggen-marketplace
```

### Run Specific Test File
```bash
cargo test --package ggen-marketplace --test integration_critical_paths
cargo test --package ggen-marketplace --test property_based_invariants
cargo test --package ggen-marketplace --test error_scenarios
```

### Run Specific Test
```bash
cargo test --package ggen-marketplace test_critical_journey_publish_search_retrieve
```

### Run with Output
```bash
cargo test --package ggen-marketplace -- --nocapture
```

### Run in Release Mode (faster)
```bash
cargo test --package ggen-marketplace --release
```

---

## 📋 Test Categories

### 1. Critical Path Integration Tests (15 tests)
**File:** `integration_critical_paths.rs`
**Purpose:** Test the 20% of functionality that provides 80% of user value
**Coverage:**
- ✅ Core user journeys (publish → search → retrieve)
- ✅ Version management (multiple versions, latest version)
- ✅ Content-addressable storage (integrity, deduplication)
- ✅ Offline-first operations (persistence, no network)
- ✅ Concurrent access (thread safety)
- ✅ Error handling (not found, duplicates, validation)
- ✅ Delete operations (versions, content)

**Example:**
```rust
#[tokio::test]
async fn test_critical_journey_publish_search_retrieve() -> Result<()> {
    let (registry, _temp) = setup_local_registry().await;

    // Publish
    let pkg = create_test_package("web-framework", "1.0.0")?;
    registry.publish(pkg.clone()).await?;

    // Search
    let results = registry.search(&Query::new("web")).await?;
    assert!(results.len() >= 1);

    // Retrieve
    let retrieved = registry.get_package(&pkg.id).await?;
    assert_eq!(retrieved.id, pkg.id);

    Ok(())
}
```

### 2. Property-Based Invariant Tests (9 tests)
**File:** `property_based_invariants.rs`
**Purpose:** Verify mathematical laws that MUST hold for all inputs
**Coverage:**
- ✅ Content addressability: `store(x) == store(x)` for all x
- ✅ Hash uniqueness: `store(x) != store(y)` for x != y
- ✅ Roundtrip fidelity: `retrieve(store(x)) == x` for all x
- ✅ Idempotency: `delete(delete(x)) == delete(x)`
- ✅ Version ordering: versions sorted newest-first
- ✅ Search consistency: `search(query) ⊆ all_packages`
- ✅ Metadata accuracy: size matches actual content
- ✅ Hash verification: hashes are cryptographically verifiable
- ✅ Uniqueness constraints: package ID + version is unique

**Example:**
```rust
#[tokio::test]
async fn property_same_content_same_id() {
    // Property: store(x) == store(x) for all x
    let store = MemoryStore::new();

    let test_cases = vec![
        b"Hello, World!".as_slice(),
        b"".as_slice(),
        &vec![0u8; 1024 * 1024],
    ];

    for content in test_cases {
        let id1 = store.store(content).await.expect("store failed");
        let id2 = store.store(content).await.expect("store failed");
        assert_eq!(id1.hash, id2.hash, "Content must be deterministic");
    }
}
```

### 3. Error Scenario Tests (20+ tests)
**File:** `error_scenarios.rs`
**Purpose:** Verify graceful failure handling, not catastrophic crashes
**Coverage:**
- ✅ Network failures (invalid URLs, timeouts)
- ✅ Filesystem errors (permissions, read-only)
- ✅ Corrupted data (invalid JSON, malformed data)
- ✅ Resource exhaustion (100MB content)
- ✅ Concurrent modifications (race conditions)
- ✅ Invalid package data (missing required fields)
- ✅ Query edge cases (empty, special characters)
- ✅ Version edge cases (0.0.0)
- ✅ Content edge cases (empty, binary)

**Example:**
```rust
#[tokio::test]
async fn error_nonexistent_registry_url() {
    let registry = CentralizedRegistry::new("https://nonexistent-domain.com")
        .expect("registry creation failed");

    let result = registry.search(&Query::new("test")).await;
    assert!(result.is_err(), "Should fail with network error");
}
```

---

## 🛠️ Test Helpers

### Common Module
**File:** `common/mod.rs`

Import in your tests:
```rust
mod common;
use common::*;
```

### Available Helpers

#### Registry Helpers
```rust
// Create local registry with temp storage
let (registry, _temp) = setup_local_registry().await;

// Create centralized registry (for testing error paths)
let registry = setup_centralized_registry("https://example.com");
```

#### Storage Helpers
```rust
// Create filesystem store with temp storage
let (store, _temp) = setup_filesystem_store().await;

// Create in-memory store (fast)
let store = setup_memory_store();
```

#### Package Builders
```rust
// Create test package with defaults
let pkg = create_test_package("my-package", "1.0.0")?;

// Create custom package
let pkg = create_custom_package(
    "my-package",
    "1.0.0",
    "Custom Title",
    "Custom description",
    "Apache-2.0",
)?;

// Create multiple packages
let packages = create_test_packages(&[
    ("web-framework", "1.0.0"),
    ("cli-tool", "2.0.0"),
    ("database", "1.5.0"),
])?;
```

#### Content Helpers
```rust
// Generate test content
let content = generate_test_content(1024); // 1KB
let content = generate_patterned_content(1024); // Detectable pattern
```

#### Assertion Helpers
```rust
// Assert packages are equal
assert_package_eq(&actual, &expected, "context message");

// Assert package has specific values
assert_package_has(&package, "expected-name", "1.0.0", "context");
```

#### Async Helpers
```rust
// Publish multiple packages
publish_packages(&registry, &packages).await?;

// Store multiple contents
let ids = store_contents(&store, &[b"content1", b"content2"]).await?;

// Measure execution time
let elapsed = measure_async(|| async {
    registry.search(&Query::new("test")).await
}).await;
```

#### Test Fixtures
```rust
// Get standard test packages
let fixtures = TestFixtures::new()?;
let pkg = fixtures.get("web-framework").expect("not found");
```

---

## ✍️ Writing New Tests

### Template for Integration Test

```rust
use ggen_marketplace::prelude::*;

mod common;
use common::*;

#[tokio::test]
async fn test_my_feature() -> Result<()> {
    // Arrange: Setup test environment
    let (registry, _temp) = setup_local_registry().await;
    let pkg = create_test_package("example", "1.0.0")?;

    // Act: Perform the operation
    registry.publish(pkg).await?;

    // Assert: Verify the outcome
    let result = registry.get_package(&PackageId::new("test", "example")).await?;
    assert_eq!(result.version, Version::new(1, 0, 0));

    Ok(())
}
```

### Template for Property-Based Test

```rust
#[tokio::test]
async fn property_my_invariant() {
    // Property: describe the law that must hold
    // Example: store(x) + store(y) = store(y) + store(x) (commutativity)

    let store = MemoryStore::new();

    let test_cases = vec![
        (b"content1".as_slice(), b"content2".as_slice()),
        (b"x".as_slice(), b"y".as_slice()),
    ];

    for (content1, content2) in test_cases {
        // Test the property holds for all inputs
        // ...
    }
}
```

### Template for Error Scenario Test

```rust
#[tokio::test]
async fn error_my_failure_case() {
    // Arrange: Setup conditions that cause failure
    let store = MemoryStore::new();

    // Act: Attempt the operation that should fail
    let result = store.retrieve(&ContentId::new("invalid", HashAlgorithm::Sha256)).await;

    // Assert: Verify graceful failure
    assert!(result.is_err(), "Should fail gracefully, not panic");

    // Optionally: Check error message
    if let Err(e) = result {
        assert!(e.to_string().contains("not found"));
    }
}
```

---

## 📊 Coverage Guidelines

| Category | Target Coverage | Why |
|----------|----------------|-----|
| **Critical paths** | 100% | Must work in production |
| **Public APIs** | 90% | User-facing contracts |
| **Error handling** | 80% | Common failure modes |
| **Internal utils** | 50% | Lower risk |
| **Generated code** | 0% | Not our code |

**Note:** Coverage percentage is less important than covering critical scenarios.

---

## ⚡ Performance Expectations

**Target:** All tests should complete in **< 2 seconds** total.

**Individual test guidelines:**
- Integration tests: < 100ms each
- Property-based tests: < 50ms each
- Error scenario tests: < 100ms each

**Measuring performance:**
```rust
use common::measure_async;

let elapsed = measure_async(|| async {
    registry.search(&Query::new("test")).await
}).await;

assert!(elapsed < Duration::from_millis(100), "Too slow: {:?}", elapsed);
```

---

## 🚨 Best Practices

### ✅ DO

1. **Use `?` for error handling**
   ```rust
   #[tokio::test]
   async fn test_example() -> Result<()> {
       let result = some_operation().await?;
       Ok(())
   }
   ```

2. **Use temporary directories**
   ```rust
   let (registry, _temp) = setup_local_registry().await;
   // _temp is dropped automatically, cleanup happens
   ```

3. **Test real scenarios**
   ```rust
   // ✅ Good: Test actual user workflow
   registry.publish(pkg).await?;
   let found = registry.search(&Query::new("package")).await?;
   ```

4. **Use descriptive test names**
   ```rust
   // ✅ Good
   #[tokio::test]
   async fn test_search_returns_packages_matching_query()

   // ❌ Bad
   #[tokio::test]
   async fn test1()
   ```

5. **Keep tests isolated**
   ```rust
   // Each test gets its own temp directory
   let (registry, _temp) = setup_local_registry().await;
   ```

### ❌ DON'T

1. **Don't use `.unwrap()` or `.expect()` in test logic**
   ```rust
   // ❌ Bad: Hides error context
   let result = operation().await.unwrap();

   // ✅ Good: Provides error context
   let result = operation().await?;
   ```

2. **Don't test implementation details**
   ```rust
   // ❌ Bad: Tests internal structure
   assert_eq!(registry.cache.len(), 5);

   // ✅ Good: Tests behavior
   let results = registry.search(&Query::new("")).await?;
   assert_eq!(results.len(), 5);
   ```

3. **Don't depend on test execution order**
   ```rust
   // ❌ Bad: Tests depend on each other

   // ✅ Good: Each test is independent
   ```

4. **Don't use sleeps or timers**
   ```rust
   // ❌ Bad: Flaky and slow
   tokio::time::sleep(Duration::from_secs(1)).await;

   // ✅ Good: Synchronous checks
   assert!(store.exists(&id).await?);
   ```

5. **Don't test obvious code**
   ```rust
   // ❌ Bad: Trivial getter
   #[test]
   fn test_package_get_name() {
       let pkg = Package::new(...);
       assert_eq!(pkg.name, "expected");
   }

   // ✅ Good: Meaningful behavior
   #[tokio::test]
   async fn test_package_can_be_published_and_retrieved()
   ```

---

## 🐛 Debugging Failed Tests

### View Test Output
```bash
cargo test --package ggen-marketplace -- --nocapture
```

### Run Single Test with Backtrace
```bash
RUST_BACKTRACE=1 cargo test --package ggen-marketplace test_name
```

### Run Tests in Single Thread (for debugging)
```bash
cargo test --package ggen-marketplace -- --test-threads=1
```

---

## 📚 Further Reading

- **80/20 Testing Strategy:** [`80_20_TESTING_STRATEGY.md`](./80_20_TESTING_STRATEGY.md)
- **Rust Testing Guide:** https://doc.rust-lang.org/book/ch11-00-testing.html
- **Property-Based Testing:** https://proptest-rs.github.io/proptest/intro.html
- **Integration Testing:** https://doc.rust-lang.org/book/ch11-03-test-organization.html

---

**Remember:** Focus on the 20% of tests that provide 80% of confidence. Quality over quantity!
