# Getting Started Tutorial

This 15-minute tutorial will get you up and running with the Cleanroom Testing Framework quickly.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Installation](#installation)
3. [Your First Test](#your-first-test)
4. [Basic Commands](#basic-commands)
5. [Scenario Testing](#scenario-testing)
6. [Container Testing](#container-testing)
7. [Configuration](#configuration)
8. [Next Steps](#next-steps)

## Prerequisites

Before you begin, ensure you have:

- **Rust 1.70+** installed ([rustup.rs](https://rustup.rs/))
- **Docker** installed and running ([docker.com](https://www.docker.com/))
- **Git** for cloning the repository
- **5-10 minutes** to complete this tutorial

### Verify Prerequisites

```bash
# Check Rust installation
rustc --version
cargo --version

# Check Docker installation
docker --version
docker ps

# Check Git installation
git --version
```

## Installation

### Option 1: From Source (Recommended)

```bash
# Clone the repository
git clone https://github.com/sac/ggen.git
cd ggen/cleanroom

# Build the project
cargo build --release

# Install globally (optional)
sudo cp target/release/cleanroom /usr/local/bin/
```

### Option 2: From Crates.io (When Available)

```bash
# Install from crates.io
cargo install cleanroom
```

### Option 3: Using Docker

```bash
# Pull the Docker image
docker pull cleanroom:latest

# Run a test
docker run --rm cleanroom:latest cleanroom --version
```

## Your First Test

Let's create your first test using Cleanroom:

### 1. Create a Test Project

```bash
# Create a new Rust project
cargo new my-cleanroom-tests
cd my-cleanroom-tests

# Add Cleanroom as a dependency
cargo add cleanroom
```

### 2. Write Your First Test

Create `src/main.rs`:

```rust
use cleanroom::{run, scenario, Policy};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Welcome to Cleanroom Testing Framework!");
    
    // Test 1: Simple command execution
    println!("\n=== Test 1: Simple Command ===");
    let result = run(["echo", "Hello, Cleanroom!"])?;
    println!("Exit code: {}", result.exit_code);
    println!("Output: {}", result.stdout);
    println!("Success: {}", result.success());
    
    // Test 2: Command with arguments
    println!("\n=== Test 2: Command with Arguments ===");
    let result = run(["ls", "-la", "."])?;
    println!("Exit code: {}", result.exit_code);
    println!("Output length: {} characters", result.stdout.len());
    
    // Test 3: Failed command
    println!("\n=== Test 3: Failed Command ===");
    let result = run(["false"])?;
    println!("Exit code: {}", result.exit_code);
    println!("Success: {}", result.success());
    
    Ok(())
}
```

### 3. Run Your First Test

```bash
# Run the test
cargo run

# Expected output:
# Welcome to Cleanroom Testing Framework!
# 
# === Test 1: Simple Command ===
# Exit code: 0
# Output: Hello, Cleanroom!
# Success: true
# 
# === Test 2: Command with Arguments ===
# Exit code: 0
# Output length: 1234 characters
# 
# === Test 3: Failed Command ===
# Exit code: 1
# Success: false
```

## Basic Commands

### Command Execution

```rust
use cleanroom::run;

// Basic command
let result = run(["echo", "hello"])?;

// Command with multiple arguments
let result = run(["ls", "-la", "/tmp"])?;

// Command that might fail
let result = run(["grep", "pattern", "file.txt"])?;
if !result.success() {
    println!("Command failed: {}", result.stderr);
}
```

### Working with Results

```rust
use cleanroom::run;

let result = run(["echo", "Hello, World!"])?;

// Check success
if result.success() {
    println!("Command succeeded!");
} else {
    println!("Command failed with exit code: {}", result.exit_code);
}

// Access output
println!("Standard output: {}", result.stdout);
println!("Standard error: {}", result.stderr);
println!("Duration: {}ms", result.duration_ms);

// Check for specific content
if result.stdout.contains("Hello") {
    println!("Found 'Hello' in output!");
}
```

### Error Handling

```rust
use cleanroom::{run, Error};

match run(["nonexistent-command"]) {
    Ok(result) => {
        if result.success() {
            println!("Command succeeded: {}", result.stdout);
        } else {
            println!("Command failed: {}", result.stderr);
        }
    }
    Err(Error::ValidationError(msg)) => {
        println!("Validation error: {}", msg);
    }
    Err(Error::ExecutionError(msg)) => {
        println!("Execution error: {}", msg);
    }
    Err(e) => {
        println!("Other error: {}", e);
    }
}
```

## Scenario Testing

Scenarios allow you to define multi-step workflows:

### Basic Scenario

```rust
use cleanroom::scenario;

let result = scenario("basic_test")
    .step("create_file", ["touch", "test.txt"])
    .step("write_content", ["echo", "Hello, World!", ">", "test.txt"])
    .step("read_content", ["cat", "test.txt"])
    .step("cleanup", ["rm", "test.txt"])
    .run()?;

println!("Scenario completed in {}ms", result.duration_ms);
println!("Steps executed: {}", result.steps.len());
```

### Scenario with Assertions

```rust
use cleanroom::{scenario, Assert};

let result = scenario("assertion_test")
    .step("create_file", ["echo", "test content", ">", "test.txt"])
    .step("verify_content", ["cat", "test.txt"])
    .run()?;

// Assert on the result
result
    .assert_success()
    .assert_stdout_contains("test content");

println!("All assertions passed!");
```

### Scenario with Error Handling

```rust
use cleanroom::scenario;

let result = scenario("error_handling_test")
    .step("create_file", ["touch", "test.txt"])
    .step("write_content", ["echo", "Hello", ">", "test.txt"])
    .step("read_content", ["cat", "test.txt"])
    .step("cleanup", ["rm", "test.txt"])
    .run()?;

// Check individual step results
for step in &result.steps {
    println!("Step '{}': {}", step.name, if step.success { "PASSED" } else { "FAILED" });
    if !step.success {
        println!("  Error: {}", step.stderr);
    }
}
```

## Container Testing

Cleanroom excels at containerized testing:

### Basic Container Test

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create cleanroom environment
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Execute test in container
    let result = environment.execute_test("container_test", || {
        // Your test logic here
        println!("Running in container!");
        Ok("test_passed")
    }).await?;
    
    println!("Test result: {}", result);
    Ok(())
}
```

### Database Testing

```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig, PostgresContainer
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Get PostgreSQL container
    let postgres = environment.get_or_create_container("postgres", || {
        PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
    }).await?;
    
    // Wait for container to be ready
    postgres.wait_for_ready().await?;
    
    // Execute database test
    let result = environment.execute_test("database_test", || {
        // Database operations here
        println!("Database is ready!");
        Ok("database_test_passed")
    }).await?;
    
    println!("Database test result: {}", result);
    Ok(())
}
```

### Redis Testing

```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig, RedisContainer
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Get Redis container
    let redis = environment.get_or_create_container("redis", || {
        RedisContainer::new(&environment.docker_client, None)
    }).await?;
    
    // Wait for container to be ready
    redis.wait_for_ready().await?;
    
    // Execute Redis test
    let result = environment.execute_test("redis_test", || {
        // Redis operations here
        println!("Redis is ready!");
        Ok("redis_test_passed")
    }).await?;
    
    println!("Redis test result: {}", result);
    Ok(())
}
```

## Configuration

### Basic Configuration

Create `cleanroom.toml`:

```toml
[cleanroom]
enable_singleton_containers = true
container_startup_timeout = 120
test_execution_timeout = 300
enable_deterministic_execution = true
deterministic_seed = 42

[cleanroom.resource_limits]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 8589934592  # 8GB
max_disk_usage_bytes = 107374182400  # 100GB
max_container_count = 10

[cleanroom.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [5432, 6379, 8080]
enable_data_redaction = true
```

### Using Configuration

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig};
use std::fs;

// Load configuration from file
let config_content = fs::read_to_string("cleanroom.toml")?;
let config: CleanroomConfig = toml::from_str(&config_content)?;

// Create environment with configuration
let environment = CleanroomEnvironment::new(config).await?;
```

### Environment Variables

```bash
# Set environment variables
export CLEANROOM_CONFIG=./cleanroom.toml
export CLEANROOM_ENABLE_SINGLETON_CONTAINERS=true
export CLEANROOM_CONTAINER_STARTUP_TIMEOUT=120
export CLEANROOM_TEST_EXECUTION_TIMEOUT=300

# Run your tests
cargo run
```

## Next Steps

Congratulations! You've completed the Cleanroom Getting Started Tutorial. Here's what you can do next:

### 1. Explore Advanced Features

- **Security Policies**: Learn about network isolation and data redaction
- **Performance Monitoring**: Set up metrics and monitoring
- **Custom Backends**: Create your own execution backends
- **Plugin Development**: Build plugins and extensions

### 2. Integration Guides

- **[CI/CD Integration](ci-cd-integration.md)**: Integrate with your CI/CD pipeline
- **[Docker Integration](docker-integration.md)**: Use with Docker containers
- **[Kubernetes Integration](kubernetes-integration.md)**: Deploy on Kubernetes

### 3. Best Practices

- **[Best Practices Guide](best-practices.md)**: Production-ready patterns
- **[Performance Best Practices](performance-best-practices.md)**: Optimize performance
- **[Security Best Practices](security-best-practices.md)**: Security considerations

### 4. Advanced Topics

- **[Custom Backends](custom-backends.md)**: Extend functionality
- **[Plugin Development](plugin-development.md)**: Build extensions
- **[Advanced Configuration](advanced-configuration.md)**: Advanced options

### 5. Community and Support

- **GitHub Issues**: Report bugs and request features
- **Discussions**: Ask questions and get help
- **Discord**: Join the community chat
- **Stack Overflow**: Tag questions with `cleanroom`

## Common Issues and Solutions

### Issue: Docker Not Running

**Error**: `Docker daemon is not running`

**Solution**:
```bash
# Start Docker daemon
sudo systemctl start docker
sudo systemctl enable docker

# Verify Docker is running
docker ps
```

### Issue: Permission Denied

**Error**: `Permission denied (publickey)`

**Solution**:
```bash
# Add user to docker group
sudo usermod -aG docker $USER

# Log out and back in, or run:
newgrp docker
```

### Issue: Container Startup Timeout

**Error**: `Container startup timeout`

**Solution**:
```toml
# Increase timeout in cleanroom.toml
[cleanroom]
container_startup_timeout = 300  # 5 minutes
```

### Issue: Memory Issues

**Error**: `Out of memory`

**Solution**:
```toml
# Increase memory limits in cleanroom.toml
[cleanroom.resource_limits]
max_memory_usage_bytes = 17179869184  # 16GB
```

## Tutorial Complete!

You've successfully completed the Cleanroom Getting Started Tutorial! You now know how to:

- ✅ Install and set up Cleanroom
- ✅ Run basic commands and tests
- ✅ Create multi-step scenarios
- ✅ Use containerized testing
- ✅ Configure Cleanroom for your needs

### What's Next?

1. **Try the examples**: Run through the provided examples
2. **Read the guides**: Explore the integration and best practice guides
3. **Join the community**: Get help and share your experiences
4. **Contribute**: Help improve Cleanroom for everyone

### Resources

- **Documentation**: [docs.cleanroom.dev](https://docs.cleanroom.dev)
- **Examples**: [github.com/sac/ggen/examples](https://github.com/sac/ggen/examples)
- **Community**: [discord.gg/cleanroom](https://discord.gg/cleanroom)
- **Support**: [github.com/sac/ggen/issues](https://github.com/sac/ggen/issues)

Happy testing with Cleanroom! 🚀
