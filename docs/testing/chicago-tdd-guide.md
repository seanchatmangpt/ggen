# Chicago TDD Tools Integration Guide

**Version**: 1.1.0  
**Last Updated**: 2025-01-15  
**Migration Status**: In Progress (see [Migration Status](#migration-status) section)

## Overview

This guide explains how to use `chicago-tdd-tools` v1.1.0 in the ggen codebase. Chicago TDD (Classicist/Detroit School) emphasizes real objects, state verification, and minimal mocking.

## Quick Start

### 1. Add Dependency

The dependency is already configured in workspace `Cargo.toml`:

```toml
[workspace.dependencies]
chicago-tdd-tools = "1.1.0"
```

In your crate's `Cargo.toml`:

```toml
[dev-dependencies]
chicago-tdd-tools = { workspace = true }
```

### 2. Import Macros

Add to your test file:

```rust
use chicago_tdd_tools::prelude::*;
```

### 3. Write Tests

Use Chicago TDD macros instead of standard Rust test attributes:

```rust
// Synchronous test
test!(test_name, {
    // Arrange
    let input = 5;
    
    // Act
    let result = input * 2;
    
    // Assert
    assert_eq!(result, 10);
});

// Async test (uses config timeout automatically)
async_test!(test_async_name, async {
    // Arrange
    let client = create_client().await.unwrap();
    
    // Act
    let result = client.fetch_data().await.unwrap();
    
    // Assert
    assert!(!result.is_empty());
});

// Async test with custom timeout (only if you need a timeout different from config)
async_test_with_timeout!(test_slow_operation, 60, async {
    // Use this when a specific test needs more/less time than the config default
    // Example: Very slow integration test that needs 60s instead of default 30s
    let result = very_slow_operation().await.unwrap();
    assert!(result.is_ready());
});

// Test with fixture (for setup/teardown)
fixture_test!(test_with_fixture, fixture, async {
    let counter = fixture.test_counter();
    assert!(counter >= 0);
});
```

## Migration Pattern

### Before (Standard Rust)

```rust
#[tokio::test]
async fn test_install_package() -> Result<()> {
    // Arrange
    let env = TestEnv::new();
    
    // Act
    let result = install_package(&options).await?;
    
    // Assert
    assert!(result.is_ok());
    Ok(())
}
```

### After (Chicago TDD)

```rust
use chicago_tdd_tools::prelude::*;

async_test!(test_install_package, async {
    // Arrange
    let env = TestEnv::new();
    
    // Act
    let result = install_package(&options).await.unwrap();
    
    // Assert
    assert_ok!(result);
});
```

## Key Changes

1. **Remove `#[tokio::test]` / `#[test]`** → Use `async_test!()` / `test!()`
2. **Remove `-> Result<()>` return type** → Use `.unwrap()` or `assert_ok!()`
3. **Remove `Ok(())` at end** → Macro handles return
4. **Add AAA comments** → Explicit Arrange-Act-Assert sections

## Assertion Macros

Chicago TDD tools provide enhanced assertion macros:

```rust
assert_ok!(result);                    // Assert Result is Ok
assert_err!(result);                   // Assert Result is Err
assert_in_range!(value, 0, 100);       // Assert value in range
```

## When to Use Each Macro

### `test!()` - Synchronous Tests
- Pure functions
- Data transformations
- File I/O operations
- Synchronous API calls

```rust
test!(test_parse_config, {
    let config = parse_config("key=value").unwrap();
    assert_eq!(config.get("key"), Some("value"));
});
```

### `async_test!()` - Async Tests (Recommended)
- Network operations
- Database queries
- Async file operations
- Tokio-based operations
- **Automatically uses `integration_timeout_seconds` from config**

```rust
async_test!(test_fetch_data, async {
    let data = fetch_from_api().await.unwrap();
    assert!(!data.is_empty());
});
```

### `async_test_with_timeout!()` - Async Tests with Custom Timeout
- Tests that need a timeout different from config
- Very slow operations that need more time
- Fast operations that should fail quickly
- **Use sparingly** - prefer `async_test!()` with config when possible

```rust
// Example: Test that legitimately needs 60s (more than default 30s)
async_test_with_timeout!(test_slow_docker_build, 60, async {
    let result = build_docker_image().await.unwrap();
    assert!(result.is_ready());
});

// Example: Test that should fail fast (less than default)
async_test_with_timeout!(test_quick_timeout, 5, async {
    // This test should complete quickly or fail fast
    let result = quick_check().await.unwrap();
    assert!(result.is_valid());
});
```

**Guidance**: 
- **Default choice**: Use `async_test!()` - it automatically respects config
- **Custom timeout**: Only use `async_test_with_timeout!()` when you have a specific reason
- **Config override**: To change timeout for all tests, edit `chicago-tdd-tools.toml` instead of hardcoding values

### `fixture_test!()` - Tests with Setup/Teardown
- Tests requiring database setup
- Tests with temporary files
- Tests with external services
- Complex test environments

```rust
fixture_test!(test_with_database, fixture, async {
    let db = fixture.database();
    let result = db.query("SELECT 1").await.unwrap();
    assert_eq!(result.len(), 1);
});
```

## Best Practices

### 1. Always Use AAA Pattern

```rust
async_test!(test_example, async {
    // Arrange: Set up test data and environment
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("test.txt");
    
    // Act: Execute the code under test
    fs::write(&file, "content").unwrap();
    
    // Assert: Verify the outcome
    assert!(file.exists());
    assert_eq!(fs::read_to_string(&file).unwrap(), "content");
});
```

### 2. Use Real Objects

✅ **DO**: Use real implementations
```rust
async_test!(test_search, async {
    let registry = create_real_registry().unwrap();
    let results = search_packages("rust", &registry).await.unwrap();
    assert!(!results.is_empty());
});
```

❌ **DON'T**: Mock everything
```rust
// Avoid excessive mocking - use real objects when possible
```

### 3. Verify State, Not Behavior

✅ **DO**: Check final state
```rust
async_test!(test_install, async {
    install_package("my-package").await.unwrap();
    assert!(package_dir.exists());  // Verify state
});
```

❌ **DON'T**: Verify method calls
```rust
// Avoid: mock.expect_install().times(1);
```

### 4. Handle Errors Explicitly

```rust
async_test!(test_with_error_handling, async {
    // Option 1: Use assert_ok! for expected success
    let result = operation().await;
    assert_ok!(result);
    
    // Option 2: Use assert_err! for expected failures
    let result = invalid_operation().await;
    assert_err!(result);
    
    // Option 3: Use unwrap() when error is unexpected
    let result = operation().await.unwrap();
    // Continue with assertions
});
```

## Examples from ggen Codebase

### Marketplace Installation Test

```rust
use chicago_tdd_tools::prelude::*;

async_test!(test_install_package_from_github, {
    // Arrange: Set up test environment
    let env = TestEnv::new();
    let options = InstallOptions::new("agent-cli-copilot")
        .with_target(env.packages_path().clone())
        .dry_run();

    // Act: Attempt installation
    let result = install_package(&options).await;

    // Assert: Verify dry-run completes successfully
    assert_ok!(result, "Dry-run installation should succeed");
    let install_result = result.unwrap();
    assert_eq!(install_result.package_name, "agent-cli-copilot");
});
```

### Search Integration Test

```rust
async_test!(test_search_finds_exact_match, async {
    // Arrange
    let _temp = create_test_registry().unwrap();

    // Act: Real search with real registry
    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("test-package", &filters).await;

    // Assert: Chicago TDD - Verify ACTUAL state
    assert_ok!(results, "Search should succeed");
    let results = results.unwrap();
    assert_eq!(results.len(), 1, "Should find exactly one package");
    assert_eq!(results[0].id, "test-package");
});
```

## Migration Checklist

When migrating existing tests:

- [ ] Add `use chicago_tdd_tools::prelude::*;`
- [ ] Convert `#[tokio::test]` → `async_test!()` or `async_test_with_timeout!(..., 30, ...)`
- [ ] Convert `#[test]` → `test!()`
- [ ] Remove `-> Result<()>` return type
- [ ] Replace `?` with `.unwrap()` or `assert_ok!()`
- [ ] Remove `Ok(())` at end
- [ ] Add explicit AAA comments (Arrange, Act, Assert)
- [ ] Verify tests still pass

## Migration Status

**Last Updated**: 2025-01-15

### Completed Migrations ✅

- **Integration Tests** (`tests/integration/*.rs`)
  - ✅ `test_cli_generator_workspace.rs` (2 tests)
  - ✅ `otel_validation_tests.rs` (11 async tests with `async_test_with_timeout!`)
  - ✅ `test_rdf.rs`, `test_manifest.rs`, `test_gen.rs`, `test_determinism.rs` (already migrated)

- **Placeholder Implementations**
  - ✅ `crates/ggen-core/src/lifecycle/validation.rs` - `ReadinessValidator::validate()` implemented

### In Progress 🚧

- **Core Crate Tests** (`crates/ggen-core/tests/**/*.rs`) - ~50+ test files
- **Domain Crate Tests** (`crates/ggen-domain/**/*.rs`) - ~30+ test files

### Remaining Work 📋

- **CLI Crate Tests** (`crates/ggen-cli/tests/**/*.rs`) - ~15+ test files
- **Marketplace Crate Tests** (`crates/ggen-marketplace/tests/**/*.rs`) - ~10+ test files
- **Node Crate Tests** (`crates/ggen-node/tests/**/*.rs`) - ~5 test files
- **Root-Level Tests** (`tests/**/*.rs`) - ~100+ test files
- **Template Tests** (`tests/integration/template_tests/*.rs`) - ~25+ tests

### Checking Migration Status

To check which tests still need migration:

```bash
# Find tests using old attributes
grep -r "#\[test\]\|#\[tokio::test\]" tests/ crates/*/tests/

# Find tests with Result<()> return types
grep -r "-> Result<()>" tests/ crates/*/tests/

# Find tests missing CTT imports
grep -L "chicago_tdd_tools::prelude" tests/**/*.rs crates/*/tests/**/*.rs
```

### Migration Priority (80/20)

Following 80/20 thinking, prioritize:
1. **High Impact**: Integration tests, core crate tests (most frequently run)
2. **High Value**: Domain crate tests (critical business logic)
3. **Medium Priority**: CLI, marketplace, node crate tests
4. **Lower Priority**: Root-level tests, template tests

## Configuration

### Using chicago-tdd-tools.toml

The `chicago-tdd-tools.toml` configuration file allows you to customize test timeouts, property test cases, testcontainers settings, and more without modifying test code.

#### Configuration File Location

Place `chicago-tdd-tools.toml` in your project root (same directory as `Cargo.toml`):

```toml
[test]
unit_timeout_seconds = 1
integration_timeout_seconds = 30

[property]
default_test_cases = 100

[testcontainers]
container_wait_timeout_seconds = 5
http_connection_timeout_seconds = 2

[performance]
hot_path_tick_budget = 8

[guards]
max_run_len = 8
max_batch_size = 1000
```

#### Automatic Configuration

The `test!()` and `async_test!()` macros automatically use timeout values from the configuration file. You don't need to do anything special - just ensure `chicago-tdd-tools.toml` exists in your project root.

**Important**: Use `async_test!()` for integration tests - it automatically uses `integration_timeout_seconds` from config. Only use `async_test_with_timeout!(test_name, custom_timeout, ...)` if you need a timeout different from the config value. Avoid hardcoding timeout values that duplicate config defaults.

#### Manual Timeout Configuration

For manual timeout calls (e.g., HTTP client timeouts, command timeouts), use the `test_config` helper module:

```rust
#[path = "../common/mod.rs"]
mod test_config;
use test_config::{integration_timeout, http_connection_timeout, unit_timeout};

// HTTP client timeout
let client = reqwest::Client::builder()
    .timeout(http_connection_timeout())
    .build()?;

// Command timeout
Command::cargo_bin("ggen")?
    .arg("search")
    .timeout(integration_timeout())
    .assert();

// Unit test timeout (for fast operations)
let result = some_fast_operation();
tokio::time::timeout(unit_timeout(), result).await?;
```

#### Available Configuration Functions

The `test_config` module provides these functions:

**Test Timeouts:**
- `unit_timeout()` - Duration for unit tests (default: 1s)
- `integration_timeout()` - Duration for integration tests (default: 30s)

**Property Testing:**
- `property_test_cases()` - Number of test cases for proptest (default: 100)

**Testcontainers:**
- `container_wait_timeout()` - Container readiness wait timeout (default: 5s)
- `http_connection_timeout()` - HTTP client connection timeout (default: 2s)
- `default_http_port()` - Default HTTP port (default: 80)
- `default_https_port()` - Default HTTPS port (default: 443)
- `default_http_alt_port()` - Alternate HTTP port (default: 8080)
- `concurrent_containers_count()` - Concurrent containers for stress tests (default: 5)
- `concurrent_commands_count()` - Concurrent commands for stress tests (default: 10)
- `multi_container_count()` - Multi-container test count (default: 3)
- `commands_per_container()` - Commands per container (default: 5)

**Performance:**
- `hot_path_tick_budget()` - Hot path tick budget (default: 8)

**Guards:**
- `max_run_len()` - Maximum run length (default: 8)
- `max_batch_size()` - Maximum batch size (default: 1000)

#### Example: Property-Based Testing

```rust
#[path = "../common/mod.rs"]
mod test_config;
use test_config::property_test_cases;
use proptest::prelude::*;

proptest! {
    #![proptest_config(ProptestConfig::with_cases(property_test_cases()))]
    #[test]
    fn test_parsing_property(input in "[a-z]+") {
        // Test implementation
    }
}
```

#### Example: Testcontainers

```rust
#[path = "../common/mod.rs"]
mod test_config;
use test_config::{container_wait_timeout, http_connection_timeout};

#[tokio::test]
async fn test_with_container() {
    let docker = testcontainers::Cli::default();
    let image = RunnableImage::from(GenericImage::new("nginx", "latest"));
    let container = docker.run(image);
    
    // Use config-based timeout for container readiness
    let timeout = container_wait_timeout();
    // ... wait for container with timeout
}
```

#### Overriding Configuration

To override configuration values:

1. Edit `chicago-tdd-tools.toml` in your project root
2. Change the desired values
3. Tests will automatically use the new values

Example: Increase integration test timeout for slow CI environments:

```toml
[test]
integration_timeout_seconds = 60  # Increased from default 30s
```

#### Default Values

If `chicago-tdd-tools.toml` is missing or values are invalid, the `test_config` module uses sensible defaults matching the configuration file comments.

## Resources

- [chicago-tdd-tools crates.io](https://crates.io/crates/chicago-tdd-tools)
- [Chicago TDD Principles](https://github.com/seanchatmangpt/knhk)
- [ggen Testing Strategy](../testing/README.md)

## Troubleshooting

### "cannot find macro `test!`"
- Ensure you have `use chicago_tdd_tools::prelude::*;` at the top
- Verify `chicago-tdd-tools` is in `[dev-dependencies]`

### "proc-macro errors"
- Ensure you're using `cargo make test` (not `cargo test` directly)
- The build system handles proc-macro compilation correctly

### "Tests fail after migration"
- Check that you replaced `?` with `.unwrap()` correctly
- Verify AAA pattern is maintained
- Ensure assertions match the original test intent

## Summary

Chicago TDD tools provide:
- ✅ Enforced AAA pattern
- ✅ Better error messages
- ✅ Consistent test structure
- ✅ Real object emphasis
- ✅ State-based verification

Use these macros for all new tests and migrate existing tests following the patterns above.

