# Cleanroom Documentation

This directory contains comprehensive documentation for the Cleanroom Testing Framework, including architecture diagrams, implementation guides, and best practices.

## Documentation Structure

### Core Architecture
- **[Architecture Overview](architecture-overview.md)** - High-level system architecture and component relationships
- **[Container Lifecycle](container-lifecycle.md)** - Container management, lifecycle states, and optimization strategies
- **[Test Execution Flow](test-execution-flow.md)** - Test execution pipeline, scenario orchestration, and deterministic execution
- **[Security Architecture](security-architecture.md)** - Security boundaries, policy enforcement, and compliance measures
- **[Performance Monitoring](performance-monitoring.md)** - Performance metrics, SLOs, and optimization strategies

### Error System
- **[Error Architecture](error-architecture.md)** - Error hierarchy, types, and handling patterns
- **[Error Flow Diagrams](error-flow-diagrams.md)** - Error propagation, recovery, and escalation flows
- **[Error Examples](error-examples.md)** - Comprehensive error creation, handling, and testing examples

## Quick Start

### Basic Usage
```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

#[tokio::test]
async fn test_my_application() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    
    let result = environment.execute_test("my_test", || {
        Ok("test_passed")
    }).await.unwrap();
}
```

### Advanced Usage
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig, PostgresContainer, RedisContainer,
    SecurityPolicy, PerformanceMonitoringConfig, ResourceLimits,
};

#[tokio::test]
async fn test_advanced_cleanroom() {
    let mut config = CleanroomConfig::default();
    
    // Configure security policy
    config.security_policy.enable_network_isolation = true;
    config.security_policy.enable_filesystem_isolation = true;
    config.security_policy.enable_process_isolation = true;
    
    // Configure performance monitoring
    config.performance_monitoring.enable_monitoring = true;
    config.performance_monitoring.metrics_interval = Duration::from_secs(5);
    
    // Configure resource limits
    config.resource_limits.max_cpu_usage_percent = 80.0;
    config.resource_limits.max_memory_usage_bytes = 1024 * 1024 * 1024;
    
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    
    // Get or create PostgreSQL container (singleton pattern)
    let postgres = environment.get_or_create_container("postgres", || {
        PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
    }).await.unwrap();
    
    // Wait for container to be ready
    postgres.wait_for_ready().await.unwrap();
    
    // Execute test with proper lifecycle management
    let result = environment.execute_test("database_test", || {
        Ok("test_passed")
    }).await.unwrap();
}
```

## Architecture Overview

The Cleanroom Testing Framework is designed as a comprehensive, production-ready testing environment using testcontainers with core team best practices.

### Key Features
- **Singleton Containers**: Start containers once per test suite for performance
- **Resource Monitoring**: Track CPU, memory, disk, and network usage
- **Security Isolation**: Network, filesystem, and process isolation
- **Deterministic Execution**: Fixed seeds for reproducible tests
- **Coverage Tracking**: Track test coverage and execution paths
- **Snapshot Testing**: Capture and compare test outputs
- **Tracing & Observability**: Detailed tracing and metrics collection
- **Error Handling**: Comprehensive error handling and recovery
- **Performance Monitoring**: Real-time performance monitoring and alerting

### Core Components
- **CleanroomEnvironment**: Central orchestrator for the testing environment
- **ContainerRegistry**: Singleton container management with caching
- **ServiceManager**: Database and cache service orchestration
- **TestcontainerBackend**: Container execution backend
- **SecurityPolicy**: Security boundaries and policy enforcement
- **PerformanceMonitoring**: Real-time performance tracking
- **DeterminismEngine**: Deterministic execution with fixed seeds

## Security Features

### Network Isolation
- Isolated network environments
- Configurable port access
- Network traffic monitoring
- Firewall rules enforcement

### Filesystem Isolation
- Secure filesystem boundaries
- Read-only filesystem support
- Path access restrictions
- File operation monitoring

### Process Isolation
- Isolated process execution
- Process resource limits
- Command execution restrictions
- Process monitoring

### Data Protection
- Automatic sensitive data redaction
- Configurable redaction patterns
- Data encryption support
- Data sanitization

## Performance Features

### SLOs (Service Level Objectives)
- First build ≤ 15s
- Incremental ≤ 2s
- RDF processing ≤ 5s for 1k+ triples
- Generation memory ≤ 100MB
- CLI scaffolding ≤ 3s end-to-end
- 100% reproducible outputs

### Performance Monitoring
- Real-time performance monitoring
- Resource usage tracking
- Performance threshold alerts
- Performance optimization recommendations

### Resource Management
- CPU usage monitoring and limits
- Memory usage tracking and limits
- Disk usage monitoring and limits
- Network bandwidth tracking and limits

## Best Practices

### 1. Use Singleton Containers
```rust
// Good: Use singleton pattern
let postgres = environment.get_or_create_container("postgres", || {
    PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
}).await.unwrap();

// Avoid: Creating new containers for each test
let postgres = PostgresContainer::new(&docker_client, "testdb", "testuser", "testpass").unwrap();
```

### 2. Configure Resource Limits
```rust
// Good: Set appropriate resource limits
config.resource_limits.max_cpu_usage_percent = 80.0;
config.resource_limits.max_memory_usage_bytes = 1024 * 1024 * 1024;
config.resource_limits.max_disk_usage_bytes = 10 * 1024 * 1024 * 1024;
```

### 3. Enable Security Features
```rust
// Good: Enable security features
config.security_policy.enable_network_isolation = true;
config.security_policy.enable_filesystem_isolation = true;
config.security_policy.enable_process_isolation = true;
config.security_policy.enable_data_redaction = true;
```

### 4. Use Deterministic Execution
```rust
// Good: Use deterministic execution for reproducible tests
config.enable_deterministic_execution = true;
config.deterministic_seed = Some(42);
```

### 5. Monitor Performance
```rust
// Good: Enable performance monitoring
config.performance_monitoring.enable_monitoring = true;
config.performance_monitoring.metrics_interval = Duration::from_secs(5);
```

### 6. Handle Errors Properly
```rust
// Good: Handle errors with specific error types
match result {
    Ok(value) => println!("Success: {:?}", value),
    Err(CleanroomError::ContainerError(msg)) => {
        eprintln!("Container error: {}", msg);
        // Handle container-specific error
    },
    Err(CleanroomError::ResourceLimitExceeded(msg)) => {
        eprintln!("Resource limit exceeded: {}", msg);
        // Handle resource limit error
    },
    Err(e) => {
        eprintln!("Unexpected error: {}", e);
        // Handle unexpected error
    },
}
```

### 7. Use RAII for Cleanup
```rust
// Good: Use RAII guard for automatic cleanup
let _guard = CleanroomGuard::new(environment_arc.clone());
```

## Configuration

### Environment Variables
- `CLEANROOM_ENABLE_SINGLETON_CONTAINERS`: Enable singleton container pattern
- `CLEANROOM_CONTAINER_STARTUP_TIMEOUT`: Container startup timeout in seconds
- `CLEANROOM_TEST_EXECUTION_TIMEOUT`: Test execution timeout in seconds
- `CLEANROOM_ENABLE_DETERMINISTIC_EXECUTION`: Enable deterministic execution
- `CLEANROOM_DETERMINISTIC_SEED`: Fixed seed for deterministic runs
- `CLEANROOM_ENABLE_COVERAGE_TRACKING`: Enable coverage tracking
- `CLEANROOM_ENABLE_SNAPSHOT_TESTING`: Enable snapshot testing
- `CLEANROOM_ENABLE_TRACING`: Enable tracing and observability

### Configuration Files
Configuration can be loaded from TOML files:
```toml
[cleanroom]
enable_singleton_containers = true
container_startup_timeout = 30
test_execution_timeout = 300
enable_deterministic_execution = true
deterministic_seed = 42
enable_coverage_tracking = true
enable_snapshot_testing = true
enable_tracing = true

[cleanroom.resource_limits]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 1073741824
max_disk_usage_bytes = 10737418240
max_network_bandwidth_bytes_per_sec = 104857600
max_container_count = 10
max_test_execution_time = 300
enable_resource_monitoring = true
resource_cleanup_timeout = 60

[cleanroom.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [5432, 6379, 8080]
enable_data_redaction = true
redaction_patterns = [
    "password\\s*=\\s*[^\\s]+",
    "token\\s*=\\s*[^\\s]+"
]
enable_audit_logging = true
security_level = "Medium"

[cleanroom.performance_monitoring]
enable_monitoring = true
metrics_interval = 5
enable_profiling = false
enable_memory_tracking = true

[cleanroom.performance_monitoring.thresholds]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 1073741824
max_test_execution_time = 300
max_container_startup_time = 30
```

## Testing

### Running Tests
```bash
# Run all tests
cargo test

# Run integration tests
cargo test --test integration_tests

# Run container tests
cargo test --test container_tests

# Run with Docker (requires Docker daemon)
cargo test --test integration_tests -- --ignored
cargo test --test container_tests -- --ignored
```

### Test Structure
```
cleanroom/
├── src/
│   ├── cleanroom.rs          # Core cleanroom environment
│   ├── containers.rs         # Container implementations
│   ├── error.rs             # Error types
│   ├── policy.rs            # Policy enforcement
│   ├── determinism.rs       # Deterministic execution
│   ├── coverage.rs          # Coverage tracking
│   ├── snapshots.rs         # Snapshot testing
│   ├── tracing.rs           # Tracing and observability
│   ├── limits.rs            # Resource limits
│   ├── redaction.rs         # Data redaction
│   └── report.rs            # Test reporting
├── tests/
│   ├── integration_tests.rs # Integration tests
│   └── container_tests.rs   # Container tests
└── README.md               # This file
```

## Troubleshooting

### Common Issues

#### Docker Not Running
```bash
# Error: Docker daemon is not running
# Solution: Start Docker daemon
sudo systemctl start docker
```

#### Port Conflicts
```bash
# Error: Port already in use
# Solution: Check for conflicting services
netstat -tulpn | grep :5432
netstat -tulpn | grep :6379
```

#### Container Startup Failures
```bash
# Error: Container failed to start
# Solution: Check Docker logs
docker logs <container_id>
```

#### Test Timeouts
```bash
# Error: Test timeout
# Solution: Increase timeout or check container health
export TEST_TIMEOUT=600
```

### Debug Mode

#### Enable Verbose Logging
```bash
RUST_LOG=debug cargo test --test integration_tests
```

#### Container Inspection
```bash
# List running containers
docker ps

# Inspect container logs
docker logs <container_id>

# Execute commands in container
docker exec -it <container_id> /bin/bash
```

#### Network Debugging
```bash
# Check container networking
docker network ls
docker network inspect <network_id>

# Test connectivity
docker exec <container_id> ping <host>
```

## Contributing

### Development Setup
1. Clone the repository
2. Install dependencies: `cargo build`
3. Run tests: `cargo test`
4. Run integration tests: `cargo test --test integration_tests -- --ignored`

### Code Style
- Follow Rust conventions
- Use `cargo fmt` for formatting
- Use `cargo clippy` for linting
- Write comprehensive tests
- Document public APIs

### Pull Request Process
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- [testcontainers-rs](https://github.com/testcontainers/testcontainers-rs) for container testing inspiration
- [Docker](https://www.docker.com/) for containerization platform
- [Rust](https://www.rust-lang.org/) for the programming language
- [Tokio](https://tokio.rs/) for async runtime
