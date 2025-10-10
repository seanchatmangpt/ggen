# GGen MCP Test Suite

Comprehensive test infrastructure for the ggen-mcp crate following Rust best practices.

## Structure

```
tests/
├── common/                 # Shared test utilities
│   ├── mod.rs             # Test helpers and builders
│   └── fixtures.rs        # Mock data and fixtures
├── fixtures/              # Static test data files
│   └── sample_template.json
├── integration_server.rs  # Server integration tests
└── README.md             # This file

benches/
└── server_benchmarks.rs   # Performance benchmarks
```

## Running Tests

### All Tests
```bash
cargo test --all-features
```

### Unit Tests Only
```bash
cargo test --lib
```

### Integration Tests Only
```bash
cargo test --test '*'
```

### Specific Test
```bash
cargo test test_server_initialization
```

### With Output
```bash
cargo test -- --nocapture
```

### Stress Tests (Slow)
```bash
cargo test --features stress-test -- --ignored
```

## Running Benchmarks

### All Benchmarks
```bash
cargo bench
```

### Specific Benchmark
```bash
cargo bench server_creation
```

### With Baseline Comparison
```bash
cargo bench -- --save-baseline main
# Make changes...
cargo bench -- --baseline main
```

## Test Helpers

### Creating Test Servers
```rust
use common::create_test_server;

let server = create_test_server();
```

### Working with Temporary Workspaces
```rust
use common::create_temp_workspace;

let workspace = create_temp_workspace();
let path = workspace.path();
// Workspace auto-cleaned on drop
```

### Using Fixtures
```rust
use common::fixtures;

let template = fixtures::rust_api_template();
let tool_request = fixtures::tool_calls::generate_template_request("my-template");
```

### Assertions
```rust
use common::{assert_json_success, assert_json_error};

assert_json_success(&response);
assert_json_error(&error_response, Some(-32600));
```

## CI/CD Integration

Tests are optimized for CI environments:

- Fast compilation with `opt-level = 1` in test profile
- Parallel execution with `--jobs` flag
- No-fail-fast mode for comprehensive reports: `cargo test --no-fail-fast`
- Environment variable support for CI-specific config

### GitHub Actions Example
```yaml
- name: Run tests
  run: |
    cargo test --all-features --no-fail-fast
    cargo test --features stress-test -- --ignored --test-threads=1
```

## Performance Testing

Benchmark categories:

1. **Server Creation**: Measures initialization overhead
2. **Template Parsing**: JSON parsing performance
3. **Template Sizes**: Scaling with file count
4. **Concurrent Requests**: Multi-threaded performance
5. **Memory Allocation**: Memory footprint analysis

## Best Practices

1. **Isolation**: Each test uses fresh temp directories
2. **Cleanup**: TempDir auto-cleanup prevents pollution
3. **Fixtures**: Reusable test data in `common/fixtures.rs`
4. **Naming**: Tests follow `test_*` convention
5. **Ignore Slow**: Use `#[ignore]` for expensive tests
6. **Async**: Use `#[tokio::test]` for async operations
7. **Logging**: Call `setup_test_logging()` for debug output

## Adding New Tests

### Integration Test
```rust
// tests/integration_new_feature.rs
mod common;

use common::*;

#[test]
fn test_new_feature() {
    setup_test_logging();
    let server = create_test_server();
    // Test logic
}
```

### Benchmark
```rust
// benches/new_benchmark.rs
use criterion::{criterion_group, criterion_main, Criterion};

fn benchmark_feature(c: &mut Criterion) {
    c.bench_function("feature_name", |b| {
        b.iter(|| {
            // Benchmark code
        });
    });
}

criterion_group!(benches, benchmark_feature);
criterion_main!(benches);
```

## Troubleshooting

### Tests Hanging
- Check for deadlocks in async code
- Verify timeout settings
- Use `--test-threads=1` to isolate

### Flaky Tests
- Ensure proper isolation
- Check for race conditions
- Use deterministic fixtures

### Memory Issues
- Run with `--release` for stress tests
- Monitor with `cargo test -- --show-output`
- Use `valgrind` for leak detection

## Resources

- [Rust Testing Guide](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Criterion.rs Documentation](https://bheisler.github.io/criterion.rs/book/)
- [Tokio Testing](https://tokio.rs/tokio/topics/testing)
