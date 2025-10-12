# API Reference

This directory contains comprehensive API reference documentation for the Cleanroom Testing Framework.

## Documentation Structure

### Core API Reference
- **[Module Reference](module-reference.md)** - Complete module documentation
- **[Type System](type-system.md)** - Type definitions and relationships
- **[Error Catalog](error-catalog.md)** - Comprehensive error reference
- **[Configuration Reference](configuration-reference.md)** - Configuration options

### Feature-Specific Documentation
- **[Container Management](container-management.md)** - Container lifecycle APIs
- **[Security Policies](security-policies.md)** - Security policy APIs
- **[Performance Monitoring](performance-monitoring.md)** - Performance monitoring APIs
- **[Scenario Execution](scenario-execution.md)** - Scenario execution APIs

### Integration Documentation
- **[Backend APIs](backend-apis.md)** - Backend abstraction APIs
- **[Service Integration](service-integration.md)** - Service integration APIs
- **[Testing Utilities](testing-utilities.md)** - Testing utility APIs

## Quick Reference

### Core Types

```rust
// Main environment type
pub struct CleanroomEnvironment {
    pub session_id: Uuid,
    // ... other fields
}

// Configuration type
pub struct CleanroomConfig {
    pub backend: BackendType,
    pub security: SecurityPolicy,
    pub performance: PerformanceMonitoringConfig,
    pub resources: ResourceLimits,
    // ... other fields
}

// Policy type
pub struct Policy {
    pub security: SecurityPolicy,
    pub resources: ResourcePolicy,
    pub execution: ExecutionPolicy,
    pub compliance: CompliancePolicy,
}

// Result types
pub struct RunResult {
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
    pub duration: Duration,
    pub metrics: Option<ExecutionMetrics>,
}

pub struct StepResult {
    pub name: String,
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
    pub duration: Duration,
    pub success: bool,
}
```

### Core Functions

```rust
// Main execution functions
pub fn run<I, S>(args: I) -> Result<RunResult>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>;

pub fn run_with_policy<I, S>(args: I, policy: &Policy) -> Result<RunResult>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>;

// Environment creation
impl CleanroomEnvironment {
    pub async fn new(config: CleanroomConfig) -> Result<Self>;
    pub async fn execute_test(&self, command: &str) -> Result<RunResult>;
    pub async fn execute_scenario(&self, scenario: &Scenario) -> Result<ScenarioResult>;
}
```

### Error Types

```rust
// Main error type
#[derive(Debug, thiserror::Error)]
pub enum CleanroomError {
    #[error("Container error: {0}")]
    Container(#[from] ContainerError),
    
    #[error("Backend error: {0}")]
    Backend(#[from] BackendError),
    
    #[error("Policy error: {0}")]
    Policy(#[from] PolicyError),
    
    #[error("Configuration error: {0}")]
    Configuration(#[from] ConfigurationError),
    
    #[error("Execution error: {0}")]
    Execution(#[from] ExecutionError),
}

// Specific error types
#[derive(Debug, thiserror::Error)]
pub enum ContainerError {
    #[error("Container startup failed: {0}")]
    StartupFailed(String),
    
    #[error("Container execution failed: {0}")]
    ExecutionFailed(String),
    
    #[error("Container cleanup failed: {0}")]
    CleanupFailed(String),
}
```

## Usage Examples

### Basic Usage

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create configuration
    let config = CleanroomConfig::default();
    
    // Create environment
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Execute test
    let result = environment.execute_test("echo 'Hello World'").await?;
    
    // Check result
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.stdout.trim(), "Hello World");
    
    Ok(())
}
```

### Advanced Usage with Policies

```rust
use cleanroom::{
    CleanroomEnvironment, 
    CleanroomConfig, 
    Policy, 
    SecurityPolicy, 
    ResourcePolicy
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create security policy
    let security = SecurityPolicy {
        enable_network_isolation: true,
        enable_filesystem_isolation: true,
        enable_process_isolation: true,
        ..Default::default()
    };
    
    // Create resource policy
    let resources = ResourcePolicy {
        max_memory_mb: 512,
        max_cpu_percent: 50,
        max_disk_mb: 1024,
        ..Default::default()
    };
    
    // Create policy
    let policy = Policy {
        security,
        resources,
        ..Default::default()
    };
    
    // Create configuration
    let config = CleanroomConfig {
        policy: Some(policy),
        ..Default::default()
    };
    
    // Create environment
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Execute test with policy
    let result = environment.execute_test("python3 --version").await?;
    
    println!("Python version: {}", result.stdout);
    
    Ok(())
}
```

### Scenario Execution

```rust
use cleanroom::{
    CleanroomEnvironment, 
    CleanroomConfig, 
    Scenario, 
    Step
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create configuration
    let config = CleanroomConfig::default();
    
    // Create environment
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Create scenario
    let scenario = Scenario::new("python_test")
        .add_step(Step::new("install_deps", "pip install requests"))
        .add_step(Step::new("run_test", "python test.py"))
        .add_step(Step::new("cleanup", "pip uninstall requests -y"));
    
    // Execute scenario
    let result = environment.execute_scenario(&scenario).await?;
    
    // Check results
    for step in result.steps {
        println!("Step {}: {}", step.name, if step.success { "PASSED" } else { "FAILED" });
    }
    
    Ok(())
}
```

## Error Handling

### Error Types and Handling

```rust
use cleanroom::{CleanroomError, CleanroomEnvironment, CleanroomConfig};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    match environment.execute_test("invalid_command").await {
        Ok(result) => {
            println!("Command executed successfully");
            println!("Exit code: {}", result.exit_code);
            println!("Output: {}", result.stdout);
        }
        Err(CleanroomError::Container(err)) => {
            eprintln!("Container error: {}", err);
        }
        Err(CleanroomError::Backend(err)) => {
            eprintln!("Backend error: {}", err);
        }
        Err(CleanroomError::Policy(err)) => {
            eprintln!("Policy error: {}", err);
        }
        Err(CleanroomError::Configuration(err)) => {
            eprintln!("Configuration error: {}", err);
        }
        Err(CleanroomError::Execution(err)) => {
            eprintln!("Execution error: {}", err);
        }
        Err(err) => {
            eprintln!("Unexpected error: {}", err);
        }
    }
    
    Ok(())
}
```

### Custom Error Handling

```rust
use cleanroom::{CleanroomError, CleanroomEnvironment, CleanroomConfig};

async fn execute_with_retry(
    environment: &CleanroomEnvironment,
    command: &str,
    max_retries: usize,
) -> Result<RunResult, CleanroomError> {
    let mut last_error = None;
    
    for attempt in 1..=max_retries {
        match environment.execute_test(command).await {
            Ok(result) => return Ok(result),
            Err(err) => {
                last_error = Some(err);
                if attempt < max_retries {
                    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
                }
            }
        }
    }
    
    Err(last_error.unwrap())
}
```

## Configuration

### Configuration Options

```rust
use cleanroom::{
    CleanroomConfig, 
    BackendType, 
    SecurityPolicy, 
    PerformanceMonitoringConfig, 
    ResourceLimits
};

// Basic configuration
let config = CleanroomConfig {
    backend: BackendType::Docker,
    container_pool_size: 10,
    prewarm_containers: true,
    enable_debug: false,
    ..Default::default()
};

// Security configuration
let security = SecurityPolicy {
    enable_network_isolation: true,
    enable_filesystem_isolation: true,
    enable_process_isolation: true,
    allowed_ports: vec![80, 443],
    blocked_commands: vec!["rm", "format"],
    ..Default::default()
};

// Performance monitoring
let performance = PerformanceMonitoringConfig {
    enable_cpu_monitoring: true,
    enable_memory_monitoring: true,
    enable_disk_monitoring: true,
    enable_network_monitoring: true,
    sampling_interval: Duration::from_secs(1),
    ..Default::default()
};

// Resource limits
let resources = ResourceLimits {
    max_memory_mb: 1024,
    max_cpu_percent: 80,
    max_disk_mb: 2048,
    max_network_mb: 100,
    max_execution_time: Duration::from_secs(300),
    ..Default::default()
};
```

### Environment Variables

```bash
# Backend configuration
export CLEANROOM_BACKEND=docker
export CLEANROOM_DOCKER_HOST=unix:///var/run/docker.sock

# Security configuration
export CLEANROOM_ENABLE_NETWORK_ISOLATION=true
export CLEANROOM_ENABLE_FILESYSTEM_ISOLATION=true

# Performance configuration
export CLEANROOM_ENABLE_PERFORMANCE_MONITORING=true
export CLEANROOM_SAMPLING_INTERVAL=1000

# Resource limits
export CLEANROOM_MAX_MEMORY_MB=1024
export CLEANROOM_MAX_CPU_PERCENT=80
export CLEANROOM_MAX_DISK_MB=2048

# Debug configuration
export CLEANROOM_ENABLE_DEBUG=false
export CLEANROOM_LOG_LEVEL=info
```

## Best Practices

### 1. Resource Management

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

// Use RAII for automatic cleanup
struct TestEnvironment {
    environment: CleanroomEnvironment,
}

impl TestEnvironment {
    async fn new() -> Result<Self, CleanroomError> {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await?;
        Ok(Self { environment })
    }
}

impl Drop for TestEnvironment {
    fn drop(&mut self) {
        // Environment automatically cleans up resources
    }
}
```

### 2. Error Propagation

```rust
use cleanroom::{CleanroomError, CleanroomEnvironment, CleanroomConfig};

async fn execute_test_suite(
    environment: &CleanroomEnvironment,
    tests: Vec<&str>,
) -> Result<Vec<RunResult>, CleanroomError> {
    let mut results = Vec::new();
    
    for test in tests {
        let result = environment.execute_test(test).await?;
        results.push(result);
    }
    
    Ok(results)
}
```

### 3. Async Best Practices

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig};
use tokio::task;

async fn execute_parallel_tests(
    environment: &CleanroomEnvironment,
    tests: Vec<&str>,
) -> Result<Vec<RunResult>, CleanroomError> {
    let handles: Vec<_> = tests
        .into_iter()
        .map(|test| {
            let env = environment.clone();
            task::spawn(async move {
                env.execute_test(test).await
            })
        })
        .collect();
    
    let mut results = Vec::new();
    for handle in handles {
        let result = handle.await??;
        results.push(result);
    }
    
    Ok(results)
}
```

## Performance Considerations

### 1. Container Pooling

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

// Use container pooling for better performance
let config = CleanroomConfig {
    container_pool_size: 20,
    prewarm_containers: true,
    container_reuse: true,
    ..Default::default()
};
```

### 2. Batch Operations

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

// Execute multiple tests in batch
let tests = vec![
    "echo 'Test 1'",
    "echo 'Test 2'",
    "echo 'Test 3'",
];

let results = environment.execute_batch_tests(tests).await?;
```

### 3. Resource Monitoring

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig, PerformanceMonitoringConfig};

// Enable performance monitoring
let config = CleanroomConfig {
    performance: PerformanceMonitoringConfig {
        enable_cpu_monitoring: true,
        enable_memory_monitoring: true,
        enable_disk_monitoring: true,
        sampling_interval: Duration::from_millis(100),
        ..Default::default()
    },
    ..Default::default()
};
```

## Security Considerations

### 1. Network Isolation

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig, SecurityPolicy};

// Enable network isolation
let config = CleanroomConfig {
    security: SecurityPolicy {
        enable_network_isolation: true,
        allowed_ports: vec![80, 443],
        blocked_hosts: vec!["malicious.com".to_string()],
        ..Default::default()
    },
    ..Default::default()
};
```

### 2. Filesystem Isolation

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig, SecurityPolicy};

// Enable filesystem isolation
let config = CleanroomConfig {
    security: SecurityPolicy {
        enable_filesystem_isolation: true,
        read_only_filesystem: true,
        allowed_paths: vec!["/tmp".to_string()],
        blocked_paths: vec!["/etc".to_string()],
        ..Default::default()
    },
    ..Default::default()
};
```

### 3. Process Isolation

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig, SecurityPolicy};

// Enable process isolation
let config = CleanroomConfig {
    security: SecurityPolicy {
        enable_process_isolation: true,
        blocked_commands: vec!["rm", "format", "shutdown"],
        allowed_commands: vec!["echo", "cat", "ls"],
        ..Default::default()
    },
    ..Default::default()
};
```

## Troubleshooting

### Common Issues

#### Container Startup Failures
```rust
use cleanroom::{CleanroomError, CleanroomEnvironment, CleanroomConfig};

match environment.execute_test("command").await {
    Err(CleanroomError::Container(err)) => {
        match err {
            ContainerError::StartupFailed(msg) => {
                eprintln!("Container startup failed: {}", msg);
                // Check Docker daemon status
                // Verify container image availability
                // Check resource availability
            }
            _ => {}
        }
    }
    _ => {}
}
```

#### Backend Errors
```rust
use cleanroom::{CleanroomError, CleanroomEnvironment, CleanroomConfig};

match environment.execute_test("command").await {
    Err(CleanroomError::Backend(err)) => {
        match err {
            BackendError::ConnectionFailed(msg) => {
                eprintln!("Backend connection failed: {}", msg);
                // Check Docker daemon status
                // Verify network connectivity
                // Check authentication
            }
            _ => {}
        }
    }
    _ => {}
}
```

#### Policy Violations
```rust
use cleanroom::{CleanroomError, CleanroomEnvironment, CleanroomConfig};

match environment.execute_test("command").await {
    Err(CleanroomError::Policy(err)) => {
        match err {
            PolicyError::ResourceLimitExceeded(limit) => {
                eprintln!("Resource limit exceeded: {}", limit);
                // Increase resource limits
                // Optimize resource usage
                // Check for resource leaks
            }
            _ => {}
        }
    }
    _ => {}
}
```

## Resources

### Documentation Links
- **Module Reference**: [module-reference.md](module-reference.md)
- **Type System**: [type-system.md](type-system.md)
- **Error Catalog**: [error-catalog.md](error-catalog.md)
- **Configuration Reference**: [configuration-reference.md](configuration-reference.md)

### External Resources
- **Rust Documentation**: [doc.rust-lang.org](https://doc.rust-lang.org/)
- **Tokio Documentation**: [tokio.rs](https://tokio.rs/)
- **Docker Documentation**: [docs.docker.com](https://docs.docker.com/)
- **testcontainers-rs**: [github.com/testcontainers/testcontainers-rs](https://github.com/testcontainers/testcontainers-rs)

### Community
- **GitHub Issues**: [github.com/sac/ggen/issues](https://github.com/sac/ggen/issues)
- **GitHub Discussions**: [github.com/sac/ggen/discussions](https://github.com/sac/ggen/discussions)
- **Discord**: [discord.gg/cleanroom](https://discord.gg/cleanroom)

## License

The Cleanroom Testing Framework is licensed under the MIT License. See the [LICENSE](../../LICENSE) file for details.
