# Chicago TDD Tools Integration Guide

**Version**: 1.1.0  
**Last Updated**: 2025-01-15

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

// Async test
async_test!(test_async_name, async {
    // Arrange
    let client = create_client().await.unwrap();
    
    // Act
    let result = client.fetch_data().await.unwrap();
    
    // Assert
    assert!(!result.is_empty());
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

### `async_test!()` - Async Tests
- Network operations
- Database queries
- Async file operations
- Tokio-based operations

```rust
async_test!(test_fetch_data, async {
    let data = fetch_from_api().await.unwrap();
    assert!(!data.is_empty());
});
```

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
- [ ] Convert `#[tokio::test]` → `async_test!()`
- [ ] Convert `#[test]` → `test!()`
- [ ] Remove `-> Result<()>` return type
- [ ] Replace `?` with `.unwrap()` or `assert_ok!()`
- [ ] Remove `Ok(())` at end
- [ ] Add explicit AAA comments
- [ ] Verify tests still pass

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

