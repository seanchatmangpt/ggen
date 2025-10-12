# Integration Guides

This directory contains comprehensive guides for integrating and using the Cleanroom Testing Framework in various environments and scenarios.

## Documentation Structure

### Getting Started
- **[Getting Started Tutorial](getting-started-tutorial.md)** - 15-minute tutorial for new users
- **[Quick Start Guide](quick-start-guide.md)** - Fast setup and first test
- **[Installation Guide](installation-guide.md)** - Detailed installation instructions

### Migration Guides
- **[Migration from Other Frameworks](migration-guide.md)** - Migrating from other testing frameworks
- **[Version Upgrade Guide](upgrade-guide.md)** - Upgrading between Cleanroom versions
- **[Configuration Migration](config-migration.md)** - Migrating configuration files

### Integration Patterns
- **[CI/CD Integration](ci-cd-integration.md)** - Integrating with CI/CD pipelines
- **[Docker Integration](docker-integration.md)** - Using with Docker containers
- **[Kubernetes Integration](kubernetes-integration.md)** - Deploying on Kubernetes
- **[Cloud Integration](cloud-integration.md)** - Using with cloud providers

### Best Practices
- **[Best Practices Guide](best-practices.md)** - Production-ready patterns and recommendations
- **[Performance Best Practices](performance-best-practices.md)** - Optimizing performance
- **[Security Best Practices](security-best-practices.md)** - Security considerations
- **[Testing Strategy Guide](testing-strategy.md)** - Comprehensive testing strategies

### Advanced Topics
- **[Custom Backends](custom-backends.md)** - Creating custom execution backends
- **[Plugin Development](plugin-development.md)** - Developing plugins and extensions
- **[Advanced Configuration](advanced-configuration.md)** - Advanced configuration options
- **[Troubleshooting Guide](troubleshooting.md)** - Common issues and solutions

## Quick Navigation

### For New Users
1. Start with [Getting Started Tutorial](getting-started-tutorial.md)
2. Follow [Quick Start Guide](quick-start-guide.md)
3. Read [Best Practices Guide](best-practices.md)

### For Existing Users
1. Check [Migration Guide](migration-guide.md) for framework migrations
2. Review [Upgrade Guide](upgrade-guide.md) for version upgrades
3. Explore [Advanced Configuration](advanced-configuration.md)

### For DevOps/Infrastructure
1. See [CI/CD Integration](ci-cd-integration.md) for pipeline setup
2. Review [Docker Integration](docker-integration.md) for containerization
3. Check [Kubernetes Integration](kubernetes-integration.md) for orchestration

### For Developers
1. Read [Custom Backends](custom-backends.md) for extending functionality
2. Explore [Plugin Development](plugin-development.md) for extensions
3. Review [Testing Strategy Guide](testing-strategy.md) for comprehensive testing

## Common Use Cases

### Basic Testing
```rust
use cleanroom::{run, scenario, Policy};

// Simple command execution
let result = run(["echo", "hello world"])?;
assert!(result.success());

// Multi-step scenario
let result = scenario("test")
    .step("echo", ["echo", "hello"])
    .step("cat", ["cat", "/dev/null"])
    .run()?;
```

### Advanced Testing
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
    
    // Configure performance monitoring
    config.performance_monitoring.enable_monitoring = true;
    
    let environment = CleanroomEnvironment::new(config).await.unwrap();
    
    // Get or create PostgreSQL container
    let postgres = environment.get_or_create_container("postgres", || {
        PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
    }).await.unwrap();
    
    // Execute test
    let result = environment.execute_test("database_test", || {
        Ok("test_passed")
    }).await.unwrap();
}
```

### CI/CD Integration
```yaml
# GitHub Actions example
name: Cleanroom Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - name: Install Cleanroom
        run: cargo install cleanroom
      - name: Run Tests
        run: cleanroom test
```

### Docker Integration
```dockerfile
FROM rust:1.70-slim as builder
WORKDIR /app
COPY . .
RUN cargo build --release

FROM debian:bullseye-slim
RUN apt-get update && apt-get install -y docker.io
COPY --from=builder /app/target/release/cleanroom /usr/local/bin/
CMD ["cleanroom", "server"]
```

## Getting Help

### Documentation
- **README.md**: Project overview and quick start
- **API Documentation**: Complete API reference
- **Examples**: Working code examples
- **Architecture**: System architecture and design

### Community Support
- **GitHub Issues**: Bug reports and feature requests
- **Discussions**: Questions and community support
- **Discord**: Real-time community chat
- **Stack Overflow**: Tag questions with `cleanroom`

### Professional Support
- **Enterprise Support**: Dedicated support for enterprise users
- **Training**: Custom training sessions
- **Consulting**: Architecture and implementation consulting
- **Custom Development**: Custom features and integrations

## Contributing

We welcome contributions to the Cleanroom Testing Framework! Please see our [Contributing Guide](../development/CONTRIBUTING.md) for details on:

- **Code Contributions**: Bug fixes, features, and improvements
- **Documentation**: Improving guides and examples
- **Testing**: Adding tests and improving coverage
- **Community**: Helping other users and contributors

## License

The Cleanroom Testing Framework is licensed under the MIT License. See the [LICENSE](../../LICENSE) file for details.

## Acknowledgments

- [testcontainers-rs](https://github.com/testcontainers/testcontainers-rs) for container testing inspiration
- [Docker](https://www.docker.com/) for containerization platform
- [Rust](https://www.rust-lang.org/) for the programming language
- [Tokio](https://tokio.rs/) for async runtime
