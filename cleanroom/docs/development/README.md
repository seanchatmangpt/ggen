# Development Documentation

This directory contains documentation for developers working on the Cleanroom Testing Framework.

## Documentation Structure

### Core Development
- **[Development Setup](development-setup.md)** - Setting up the development environment
- **[Development Checklist](development-checklist.md)** - Pre-commit and pre-release checklist
- **[Release Process](release-process.md)** - How to create releases
- **[Benchmarking Guide](benchmarking-guide.md)** - Performance benchmarking and testing

### Architecture and Design
- **[Architecture Overview](../architecture-overview.md)** - High-level system architecture
- **[Container Lifecycle](../container-lifecycle.md)** - Container management lifecycle
- **[Error Architecture](../error-architecture.md)** - Error handling system design
- **[Security Architecture](../security-architecture.md)** - Security system design
- **[Performance Monitoring](../performance-monitoring.md)** - Performance monitoring system

### Testing and Quality
- **[Test Execution Flow](../test-execution-flow.md)** - How tests are executed
- **[Error Examples](../error-examples.md)** - Common error scenarios
- **[Error Flow Diagrams](../error-flow-diagrams.md)** - Error handling flow diagrams

## Quick Start for Developers

### 1. Prerequisites

Ensure you have the following installed:
- **Rust 1.85+** ([rustup.rs](https://rustup.rs/))
- **Docker** ([docker.com](https://www.docker.com/))
- **Git** for version control
- **VS Code** or **IntelliJ IDEA** with Rust plugin

### 2. Setup Development Environment

```bash
# Clone the repository
git clone https://github.com/sac/ggen.git
cd ggen/cleanroom

# Install development dependencies
cargo install cargo-fmt cargo-clippy cargo-doc
cargo install cargo-tarpaulin cargo-audit

# Build the project
cargo build

# Run tests
cargo test
```

### 3. Development Workflow

```bash
# Create feature branch
git checkout -b feature/your-feature-name

# Make changes and test
cargo test
cargo clippy
cargo fmt

# Commit changes
git add .
git commit -m "Add your feature"

# Push and create PR
git push origin feature/your-feature-name
```

## Development Guidelines

### Code Style

- Follow Rust conventions (`cargo fmt`)
- Use clippy for linting (`cargo clippy`)
- Write comprehensive tests
- Document public APIs
- Use meaningful commit messages

### Testing

- Write unit tests for new functionality
- Add integration tests for complex features
- Ensure > 90% test coverage
- Test error conditions and edge cases

### Documentation

- Update README for significant changes
- Add examples for new features
- Create ADRs for architectural decisions
- Keep API documentation current

## Architecture Overview

### Core Components

1. **CleanroomEnvironment**: Main orchestrator
2. **Backend**: Execution backend abstraction
3. **Containers**: Container lifecycle management
4. **Policy**: Security and resource policies
5. **Scenario**: Test scenario execution
6. **Error**: Comprehensive error handling

### Key Design Principles

- **Deterministic**: Reproducible test results
- **Isolated**: Complete test isolation
- **Performant**: Optimized for speed
- **Secure**: Built-in security features
- **Extensible**: Plugin architecture

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for detailed contribution guidelines.

### Types of Contributions

- **Bug Fixes**: Fix existing issues
- **Features**: Add new functionality
- **Documentation**: Improve documentation
- **Tests**: Add or improve tests
- **Performance**: Optimize performance
- **Security**: Enhance security

### Getting Help

- **GitHub Issues**: Report bugs and request features
- **GitHub Discussions**: Ask questions and discuss ideas
- **Discord**: Real-time community chat
- **Email**: Contact maintainers directly

## Development Tools

### Recommended Tools

```bash
# Code quality
cargo install cargo-fmt cargo-clippy cargo-doc

# Testing and coverage
cargo install cargo-tarpaulin cargo-audit

# Development utilities
cargo install cargo-expand cargo-tree cargo-outdated
```

### IDE Configuration

#### VS Code
- Rust Analyzer extension
- CodeLLDB for debugging
- Docker extension
- GitLens for Git integration

#### IntelliJ IDEA
- Rust plugin
- Docker integration
- Git integration
- Database tools

## Performance Considerations

### Optimization Targets

- **Test Execution**: < 5 seconds per test
- **Container Startup**: < 30 seconds
- **Memory Usage**: < 1GB per test
- **CPU Usage**: < 80% during tests

### Profiling Tools

```bash
# Install profiling tools
cargo install cargo-flamegraph cargo-profdata

# Profile performance
cargo flamegraph --bin cleanroom

# Analyze performance
cargo profdata --help
```

## Security Considerations

### Security Features

- **Network Isolation**: Complete network isolation
- **Filesystem Isolation**: Isolated filesystem access
- **Process Isolation**: Isolated process execution
- **Data Redaction**: Automatic sensitive data removal
- **Audit Logging**: Comprehensive audit trails

### Security Testing

```bash
# Run security audit
cargo audit

# Check for vulnerabilities
cargo audit --deny warnings

# Test security policies
cargo test --test security_tests
```

## Release Management

### Version Strategy

- **Semantic Versioning**: MAJOR.MINOR.PATCH
- **Release Branches**: Stable release branches
- **Hotfixes**: Critical bug fixes
- **Feature Flags**: Gradual feature rollout

### Release Process

1. **Prepare Release**: Update version and changelog
2. **Test Release**: Run full test suite
3. **Create Tag**: Tag the release
4. **Publish**: Publish to crates.io
5. **Announce**: Announce the release

## Monitoring and Observability

### Metrics Collection

- **Performance Metrics**: CPU, memory, disk usage
- **Test Metrics**: Execution time, success rate
- **Container Metrics**: Startup time, resource usage
- **Error Metrics**: Error rates, types

### Logging

- **Structured Logging**: JSON-formatted logs
- **Log Levels**: DEBUG, INFO, WARN, ERROR
- **Log Aggregation**: Centralized log collection
- **Log Analysis**: Automated log analysis

## Troubleshooting

### Common Issues

#### Build Issues
```bash
# Clean build artifacts
cargo clean

# Update dependencies
cargo update

# Check Rust version
rustc --version
```

#### Test Issues
```bash
# Run tests with output
cargo test -- --nocapture

# Run specific test
cargo test test_name

# Run tests in single thread
cargo test -- --test-threads=1
```

#### Docker Issues
```bash
# Check Docker status
docker --version
docker ps

# Restart Docker
sudo systemctl restart docker
```

### Getting Help

1. **Check Documentation**: Read relevant docs
2. **Search Issues**: Look for similar issues
3. **Ask Community**: Use GitHub Discussions
4. **Contact Maintainers**: For urgent issues

## Resources

### Documentation

- **API Reference**: [docs.rs/cleanroom](https://docs.rs/cleanroom)
- **Examples**: [github.com/sac/ggen/examples](https://github.com/sac/ggen/examples)
- **Architecture**: [docs/architecture-overview.md](../architecture-overview.md)

### Community

- **GitHub**: [github.com/sac/ggen](https://github.com/sac/ggen)
- **Discord**: [discord.gg/cleanroom](https://discord.gg/cleanroom)
- **Stack Overflow**: Tag questions with `cleanroom`

### Tools

- **Rust**: [rust-lang.org](https://rust-lang.org)
- **Docker**: [docker.com](https://www.docker.com)
- **Tokio**: [tokio.rs](https://tokio.rs)

## License

The Cleanroom Testing Framework is licensed under the MIT License. See the [LICENSE](../../LICENSE) file for details.

## Acknowledgments

- [testcontainers-rs](https://github.com/testcontainers/testcontainers-rs) for inspiration
- [Docker](https://www.docker.com/) for containerization
- [Rust](https://www.rust-lang.org/) for the programming language
- [Tokio](https://tokio.rs/) for async runtime
- All contributors who help make Cleanroom better
