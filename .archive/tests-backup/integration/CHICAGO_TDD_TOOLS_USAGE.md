# Chicago TDD Tools - Testcontainers Usage Guide

## Current API (What We're Using)

The tests are using the `chicago_tdd_tools::testcontainers` API which provides:

### Core Types

```rust
use chicago_tdd_tools::testcontainers::{
    ContainerClient,      // Client for managing containers
    GenericContainer,     // Generic container type
    TestcontainersResult, // Result type for operations
    exec::SUCCESS_EXIT_CODE, // Success exit code constant
};
```

### Basic Usage Pattern

```rust
#[test]
#[ignore] // Long-running integration test
fn test_example() {
    // 1. Require Docker
    require_docker();
    
    // 2. Create container client
    let client = ContainerClient::new();
    
    // 3. Create container
    let container = GenericContainer::with_command(
        client.client(),
        "rust",                    // image
        "1.83-slim-bookworm",      // tag
        "sleep",                    // command
        &["infinity"],              // args
        None,                       // env vars (optional)
    )?;
    
    // 4. Execute commands
    let result = container.exec(
        "sh",
        &["-c", "echo 'Hello from container'"],
    )?;
    
    // 5. Check results
    assert_eq!(result.exit_code, SUCCESS_EXIT_CODE);
    assert!(result.stdout.contains("Hello"));
    
    // 6. Container automatically cleaned up on drop
}
```

## Available Helper Functions

### Container Creation

```rust
// Generic container with custom command
let container = GenericContainer::with_command(
    client.client(),
    "node",
    "20-bullseye",
    "sleep",
    &["infinity"],
    None,
)?;

// Container with environment variables
let container = GenericContainer::with_command(
    client.client(),
    "postgres",
    "16-alpine",
    "postgres",
    &[],
    Some(vec![
        ("POSTGRES_DB", "testdb"),
        ("POSTGRES_USER", "testuser"),
        ("POSTGRES_PASSWORD", "testpass"),
    ]),
)?;
```

### Command Execution

```rust
// Execute shell command
let result = container.exec(
    "sh",
    &["-c", "ls -la /workspace"],
)?;

// Execute binary directly
let result = container.exec(
    "cargo",
    &["--version"],
)?;

// Check exit code
if result.exit_code == SUCCESS_EXIT_CODE {
    println!("Command succeeded: {}", result.stdout);
} else {
    eprintln!("Command failed: {}", result.stderr);
}
```

### Docker Verification

```rust
use tests::common::require_docker;

#[test]
fn test_requires_docker() {
    require_docker(); // Panics if Docker not available
    // Test code here...
}
```

## Higher-Level Helpers (Future Enhancement)

The documentation mentions fixture-based helpers like:

```rust
// These may be available in future versions or different modules
fixture_test!(test_with_postgres, fixture, {
    let container = fixture.postgres_container()?;
    let conn_string = container.connection_string();
    let result = container.execute_query("SELECT 1").await?;
});
```

**Current Status**: These higher-level helpers are not yet available in the current `chicago_tdd_tools::testcontainers` API. The tests use the lower-level API which provides full control.

## Examples in Codebase

### Example 1: Basic Container Setup
```rust
// From marketplace_nextjs_ontology_e2e.rs
fn setup_container_environment(client: &ContainerClient) -> TestcontainersResult<GenericContainer> {
    GenericContainer::with_command(
        client.client(),
        NODE_IMAGE,
        NODE_TAG,
        "sleep",
        &["infinity"],
        None,
    )
}
```

### Example 2: Building from Source
```rust
// From marketplace_nextjs_ontology_e2e.rs
fn build_ggen_from_source_in_container(container: &GenericContainer) -> TestcontainersResult<()> {
    // Clone repository
    container.exec("git", &["clone", GGEN_REPO, WORKSPACE_DIR])?;
    
    // Install dependencies
    container.exec("sh", &["-c", "apt-get update && apt-get install -y git build-essential"])?;
    
    // Build
    let build_result = container.exec(
        "sh",
        &["-c", &format!("cd {} && cargo build --release", WORKSPACE_DIR)],
    )?;
    
    assert_eq!(build_result.exit_code, SUCCESS_EXIT_CODE);
    Ok(())
}
```

### Example 3: Multi-Container Test
```rust
// From full_cycle_container_validation.rs
fn run_build_container(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = GenericContainer::with_command(
        client.client(),
        RUST_IMAGE,
        RUST_TAG,
        "sleep",
        &["infinity"],
        None,
    )?;
    
    // Multiple operations in same container
    container.exec("apt-get", &["update"])?;
    container.exec("git", &["clone", GGEN_REPO, "/workspace"])?;
    container.exec("cargo", &["build", "--release"])?;
    
    Ok(())
}
```

## Best Practices

1. **Always check Docker first**:
   ```rust
   require_docker();
   ```

2. **Use descriptive container names** (via image/tag):
   ```rust
   const RUST_IMAGE: &str = "rust";
   const RUST_TAG: &str = "1.83-slim-bookworm";
   ```

3. **Check exit codes**:
   ```rust
   assert_eq!(result.exit_code, SUCCESS_EXIT_CODE);
   ```

4. **Handle errors gracefully**:
   ```rust
   let result = container.exec("command", &[])?;
   if result.exit_code != SUCCESS_EXIT_CODE {
       return Err(TestcontainersError::CommandExecutionFailed(
           format!("Command failed: {}", result.stderr)
       ));
   }
   ```

5. **Mark long-running tests**:
   ```rust
   #[test]
   #[ignore] // Long-running integration test
   fn test_integration() { ... }
   ```

6. **Run with --ignored flag**:
   ```bash
   cargo test --test test_name -- --ignored
   ```

## Comparison: Current API vs. Future Fixture API

| Feature | Current API | Future Fixture API (Documented) |
|---------|-------------|--------------------------------|
| Container Creation | `GenericContainer::with_command()` | `fixture.postgres_container()` |
| Connection String | Manual format string | `container.connection_string()` |
| Query Execution | Manual `exec()` calls | `container.execute_query()` |
| Cleanup | Automatic on drop | Automatic on fixture drop |
| Control | Full control | Higher-level abstraction |

## Summary

✅ **Current Status**: Tests are using the correct `chicago_tdd_tools::testcontainers` API  
✅ **API Available**: `ContainerClient`, `GenericContainer`, `TestcontainersResult`  
✅ **All Tests Working**: Integration tests compile and use the API correctly  
⚠️ **Future Enhancement**: Higher-level fixture helpers may be added in future versions

The current API provides full control and is production-ready. The fixture-based helpers mentioned in documentation would be a convenience layer on top of this API.

