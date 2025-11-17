# Ggen Expert Testing Patterns - Implementation Guide

## Quick Start: Where to Add Tests

### Error Path Tests
Location: `/home/user/ggen/tests/expert_testing_patterns/error_paths/`

Key patterns to test:
- File not found scenarios
- Invalid input (UTF-8, lengths, formats)
- Permission errors
- JSON parse failures

### Boundary Condition Tests  
Location: `/home/user/ggen/tests/expert_testing_patterns/boundary_conditions/`

Key patterns to test:
- Empty collections (graphs, caches)
- At-capacity conditions
- Off-by-one lengths/counts
- Minimal/maximal valid values

### Resource Management Tests
Location: `/home/user/ggen/tests/expert_testing_patterns/resource_management/`

Key patterns to test:
- File handle cleanup
- Async operation cancellation
- Temporary file cleanup
- Mutex/lock cleanup

### Concurrency Tests
Location: `/home/user/ggen/tests/expert_testing_patterns/concurrency/`

Key patterns to test:
- Multi-threaded access
- Race conditions
- Panic handling in threads
- Atomic operations

## Test Template Examples

### Error Path Test Template
```rust
#[test]
fn test_error_path_description() {
    // Arrange: Set up conditions that cause error
    let invalid_input = create_invalid_input();
    
    // Act: Call function expected to error
    let result = function_under_test(invalid_input);
    
    // Assert: Verify error details
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("expected error message"));
}
```

### Boundary Condition Test Template
```rust
#[test]
fn test_boundary_at_capacity() {
    // Arrange: Fill to exactly boundary value
    let mut collection = create_collection();
    fill_to_capacity(&mut collection, 1000);
    
    // Act: Add one more at boundary
    let result = collection.insert(1001st_item);
    
    // Assert: Verify boundary behavior
    assert!(result.is_ok());  // Or error, depending on spec
    assert_eq!(collection.len(), expected_size);
}
```

### Concurrency Test Template
```rust
#[tokio::test]
async fn test_concurrent_access() {
    // Arrange
    let shared = Arc::new(Graph::new().unwrap());
    let mut handles = vec![];
    
    // Act: Spawn multiple concurrent tasks
    for i in 0..10 {
        let graph = Arc::clone(&shared);
        let handle = tokio::spawn(async move {
            graph.query("SELECT ...").unwrap()
        });
        handles.push(handle);
    }
    
    // Assert: All complete without panic
    for handle in handles {
        assert!(handle.await.is_ok());
    }
}
```

## Using Chicago TDD Framework

All tests should use the chicago-tdd-tools framework for consistency:

```rust
use chicago_tdd_tools::prelude::*;

test!(test_name, {
    // Arrange
    let input = setup();
    
    // Act
    let result = function_under_test(input);
    
    // Assert
    assert!(result.is_ok());
});
```

## Key Testing Utilities Already Available

From Cargo.toml:
- `chicago-tdd-tools` - Test framework macros
- `proptest` - Property-based testing
- `tempfile` - Temporary file/directory management
- `tokio::test` - Async testing
- `assert_cmd` / `predicates` - CLI testing
- `insta` - Snapshot testing
- `serial_test` - Serial test execution

## Running Tests

```bash
# Run all tests
cargo test

# Run specific test category
cargo test test_graph_

# Run with output
cargo test -- --nocapture

# Run concurrency tests (use cargo-nextest)
cargo nextest run

# Coverage
cargo tarpaulin --out Html

# Specific feature tests
cargo test --features chicago_tdd_main
```

## Common Test Patterns in Ggen

### Graph Testing
```rust
let graph = Graph::new().unwrap();
graph.insert_turtle(r#"
    @prefix ex: <http://example.org/> .
    ex:alice a ex:Person .
"#).unwrap();
let results = graph.query("SELECT ?s WHERE { ?s a ?type }").unwrap();
assert!(!results.is_empty());
```

### Domain Logic Testing
```rust
let options = GenerateFileOptions::new(template_path, output_path)
    .with_var("name", "test")
    .force();
let result = generate_file(&options)?;
assert!(output_path.exists());
```

### Error Testing
```rust
use ggen_utils::error::Result;

fn returns_error() -> Result<()> {
    Err(ggen_utils::error::Error::new("Test error"))
}

#[test]
fn test_error_handling() {
    assert!(returns_error().is_err());
}
```

## Next Steps

1. Create test module structure:
   ```
   tests/expert_testing_patterns/
   ├── error_paths.rs
   ├── boundaries.rs
   ├── resources.rs
   └── concurrency.rs
   ```

2. Implement 5-7 tests from Phase 1 (highest ROI)

3. Integrate into CI/CD:
   - Add `cargo test` to GitHub Actions
   - Add coverage tracking
   - Add performance benchmarks

4. Document test patterns in wiki

5. Create shared test utilities module if needed
