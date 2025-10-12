# Service Fixtures

Cleanroom provides ready-to-use service fixtures for common testing scenarios.

## Overview

Service fixtures manage the lifecycle of external dependencies like databases and caches, providing:

- **Automatic provisioning**: Start services in containers
- **Health checking**: Wait for services to be ready
- **Connection info**: Provide connection details to tests
- **Cleanup**: Automatic teardown and resource cleanup

## PostgreSQL Fixture

Ready-to-use PostgreSQL database for testing:

```rust
use cleanroom::services::Postgres;

#[tokio::test]
async fn test_with_database() {
    let mut postgres = Postgres::new().unwrap();
    postgres.start().unwrap();

    // Get connection information
    let conn_info = postgres.connection_info().unwrap();
    println!("Database ready at {}:{}", conn_info.host, conn_info.port);

    // Use database in tests
    let config = DatabaseConfig {
        host: conn_info.host,
        port: conn_info.port,
        database: conn_info.params.get("database").unwrap().clone(),
        username: conn_info.params.get("username").unwrap().clone(),
        password: conn_info.params.get("password").unwrap().clone(),
    };

    let pool = PgPool::connect(&config.connection_string()).await.unwrap();

    // Run your database tests...
    sqlx::query("CREATE TABLE test (id SERIAL PRIMARY KEY, name TEXT)")
        .execute(&pool)
        .await
        .unwrap();

    // Cleanup happens automatically when postgres goes out of scope
}
```

### Configuration Options

```rust
impl Postgres {
    pub fn new() -> Result<Self>
    pub fn with_config(
        port: u16,
        database: String,
        username: String,
        password: String,
    ) -> Result<Self>
}
```

### Connection Information

```rust
pub struct ConnectionInfo {
    pub host: String,
    pub port: u16,
    pub params: HashMap<String, String>, // database, username, password
}
```

## Redis Fixture

Redis cache for testing caching logic:

```rust
use cleanroom::services::Redis;

#[tokio::test]
async fn test_with_cache() {
    let mut redis = Redis::with_config(6379, Some("testpass".to_string())).unwrap();
    redis.start().unwrap();

    let conn_info = redis.connection_info().unwrap();

    let client = redis::Client::open(format!(
        "redis://:{}@{}:{}",
        conn_info.params.get("password").unwrap_or(&"".to_string()),
        conn_info.host,
        conn_info.port
    )).unwrap();

    let mut conn = client.get_connection().unwrap();

    // Test Redis operations
    redis::cmd("SET").arg("test_key").arg("test_value")
        .execute(&mut conn);

    let result: String = redis::cmd("GET").arg("test_key")
        .query(&mut conn).unwrap();

    assert_eq!(result, "test_value");
}
```

### Authentication

Redis fixtures support optional password authentication:
```rust
// Without password
let redis = Redis::new().unwrap();

// With password
let redis = Redis::with_config(6379, Some("mypass".to_string())).unwrap();
```

## Service Lifecycle

### RAII Cleanup

Services implement `Drop` for automatic cleanup:

```rust
{
    let mut postgres = Postgres::new().unwrap();
    postgres.start().unwrap();

    // Use the service...

    // postgres.stop() called automatically when dropped
} // <- Cleanup happens here
```

### Manual Lifecycle Management

```rust
let mut postgres = Postgres::new().unwrap();

postgres.start().unwrap();
// ... use service ...

postgres.stop().unwrap(); // Explicit cleanup
```

## Health Checking

Services include built-in health checks:

```rust
impl Service for Postgres {
    fn health_check(&self) -> Result<bool> {
        // Check if container is running and PostgreSQL is accepting connections
    }
}
```

Health checks are used internally for startup coordination but can also be used in tests.

## Error Handling

Service errors provide detailed information:

```rust
use cleanroom::error::{ServiceError, CleanroomError};

match postgres.start() {
    Err(CleanroomError::Service(ServiceError::HealthCheckFailed(msg))) => {
        eprintln!("Service health check failed: {}", msg);
    }
    Err(CleanroomError::Service(ServiceError::StartupTimeout(msg))) => {
        eprintln!("Service startup timed out: {}", msg);
    }
    _ => {}
}
```

## Integration with Scenarios

Services integrate with the Scenario DSL:

```rust
use cleanroom::{scenario, services::{Postgres, Redis}};

let mut postgres = Postgres::new().unwrap();
let mut redis = Redis::new().unwrap();

let result = scenario("integration_test")
    .services(vec![postgres, redis])
    .step("setup", ["./setup.sh"])
    .step("test", ["./test.sh"])
    .step("verify", ["./verify.sh"])
    .run()
    .unwrap();
```

Services are started before scenario execution and stopped afterward.

## Custom Services

Implement the `Service` trait for custom fixtures:

```rust
use cleanroom::services::{Service, ConnectionInfo};
use cleanroom::error::{Result, ServiceError};

pub struct CustomService {
    container_name: String,
    port: u16,
    started: bool,
}

impl Service for CustomService {
    fn name(&self) -> &str {
        "custom"
    }

    fn health_check(&self) -> Result<bool> {
        // Implement health checking logic
        Ok(true)
    }

    fn connection_info(&self) -> Result<ConnectionInfo> {
        // Return connection details
    }

    fn start(&mut self) -> Result<()> {
        // Start the service
        self.started = true;
        Ok(())
    }

    fn stop(&mut self) -> Result<()> {
        // Stop the service
        self.started = false;
        Ok(())
    }
}
```

## Docker Images

Services use official, minimal images:

- **PostgreSQL**: `postgres:15-alpine` (lightweight, secure)
- **Redis**: `redis:7-alpine` (latest stable, minimal)

Images are pulled automatically on first use and cached for subsequent runs.

## Network Isolation

Service fixtures create isolated networks:

- Each service gets its own container network
- Services can communicate with each other
- Services are isolated from the host network
- Test containers can access services via port mapping

## Resource Management

### Container Resource Limits

Services run with resource constraints:
- **Memory**: 256MB default
- **CPU**: 0.5 cores default
- **Storage**: Ephemeral (tmpfs where possible)

### Cleanup Guarantees

Services ensure cleanup even if tests panic:
- Containers are explicitly stopped and removed
- Networks are cleaned up
- Temporary files are deleted
- Resource leaks are prevented

## Performance Considerations

### Startup Time

Service startup adds overhead:
- PostgreSQL: ~10-15 seconds
- Redis: ~5-10 seconds

Consider this in test timing.

### Parallel Execution

Services support parallel test execution:
- Each test gets its own service instance
- Container names are unique per test
- No port conflicts between parallel tests

### Caching

Service images are cached locally:
- First run pulls images
- Subsequent runs use cached images
- Network conditions don't affect repeat runs

## Troubleshooting

### Container Issues

```bash
# Check running containers
docker ps -a

# Check container logs
docker logs <container_name>

# Clean up stopped containers
docker container prune

# Remove unused images
docker image prune
```

### Port Conflicts

Services use random container names but fixed ports. If ports conflict:

```rust
let postgres = Postgres::with_config(
    5433, // Custom port
    "testdb".to_string(),
    "testuser".to_string(),
    "testpass".to_string(),
).unwrap();
```

### Network Issues

If services fail to start:
1. Check Docker/Podman daemon status
2. Verify network connectivity
3. Check available disk space
4. Review container logs

## Best Practices

### Test Design

1. **Use fixtures for external dependencies**: Avoid mocking when possible
2. **Test realistic scenarios**: Use real databases for integration tests
3. **Clean test data**: Ensure each test starts with clean state
4. **Resource cleanup**: Rely on RAII for automatic cleanup

### Performance

1. **Parallel testing**: Run tests in parallel when possible
2. **Connection pooling**: Use connection pools for database tests
3. **Minimal data**: Keep test datasets small
4. **Service reuse**: Share service instances across related tests

### Security

1. **Isolated networks**: Services run in isolated networks
2. **Resource limits**: Prevent resource exhaustion
3. **Image updates**: Keep service images updated
4. **Access control**: Limit service access to test containers only
