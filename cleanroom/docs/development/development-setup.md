# Development Setup

This guide provides step-by-step instructions for setting up a development environment for the Cleanroom Testing Framework.

## Prerequisites

### Required Software

- **Rust 1.70+**: [rustup.rs](https://rustup.rs/)
- **Docker**: [docker.com](https://www.docker.com/)
- **Git**: Version control
- **VS Code** or **IntelliJ IDEA**: IDE with Rust support

### System Requirements

- **OS**: Linux, macOS, or Windows with WSL2
- **RAM**: 8GB minimum, 16GB recommended
- **Disk**: 10GB free space
- **CPU**: 4 cores minimum, 8 cores recommended

## Installation

### 1. Install Rust

```bash
# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Verify installation
rustc --version
cargo --version
```

### 2. Install Docker

#### Linux (Ubuntu/Debian)
```bash
# Install Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Add user to docker group
sudo usermod -aG docker $USER

# Start Docker service
sudo systemctl start docker
sudo systemctl enable docker
```

#### macOS
```bash
# Install Docker Desktop
brew install --cask docker

# Start Docker Desktop
open /Applications/Docker.app
```

#### Windows
1. Download Docker Desktop from [docker.com](https://www.docker.com/)
2. Install and start Docker Desktop
3. Enable WSL2 integration

### 3. Install Development Tools

```bash
# Install Rust development tools
cargo install cargo-fmt cargo-clippy cargo-doc
cargo install cargo-tarpaulin cargo-audit
cargo install cargo-expand cargo-tree

# Install Git tools
cargo install git-cliff

# Verify installations
cargo fmt --version
cargo clippy --version
cargo doc --version
```

## Project Setup

### 1. Clone Repository

```bash
# Clone the repository
git clone https://github.com/sac/ggen.git
cd ggen/cleanroom

# Verify project structure
ls -la
```

### 2. Build Project

```bash
# Build the project
cargo build

# Build with all features
cargo build --all-features

# Build release version
cargo build --release
```

### 3. Run Tests

```bash
# Run all tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run specific test
cargo test test_name

# Run integration tests
cargo test --test integration_tests
```

## IDE Configuration

### VS Code Setup

#### Install Extensions
- Rust Analyzer
- CodeLLDB
- Docker
- GitLens
- Better TOML

#### Configure Settings
```json
{
    "rust-analyzer.checkOnSave.command": "clippy",
    "rust-analyzer.checkOnSave.extraArgs": ["--", "-W", "clippy::all"],
    "rust-analyzer.cargo.features": "all",
    "rust-analyzer.procMacro.enable": true,
    "rust-analyzer.completion.autoimport.enable": true,
    "rust-analyzer.diagnostics.enable": true
}
```

### IntelliJ IDEA Setup

#### Install Plugin
1. Open IntelliJ IDEA
2. Go to Settings > Plugins
3. Search for "Rust"
4. Install the Rust plugin
5. Restart IntelliJ IDEA

#### Configure Rust Toolchain
1. Go to Settings > Languages & Frameworks > Rust
2. Set Rust toolchain to "stable"
3. Enable clippy integration
4. Configure cargo features

## Development Workflow

### 1. Create Feature Branch

```bash
# Update main branch
git checkout main
git pull origin main

# Create feature branch
git checkout -b feature/your-feature-name

# Push branch to remote
git push -u origin feature/your-feature-name
```

### 2. Development Cycle

```bash
# Make changes to code
# ...

# Format code
cargo fmt

# Run clippy
cargo clippy --all-targets --all-features

# Run tests
cargo test

# Build documentation
cargo doc --no-deps --open

# Commit changes
git add .
git commit -m "Add your feature"

# Push changes
git push origin feature/your-feature-name
```

### 3. Pre-commit Checks

```bash
#!/bin/sh
# .git/hooks/pre-commit

echo "Running pre-commit checks..."

# Format check
echo "Checking code formatting..."
cargo fmt --all -- --check

# Clippy check
echo "Running clippy..."
cargo clippy --all-targets --all-features -- -D warnings

# Test check
echo "Running tests..."
cargo test

# Security audit
echo "Running security audit..."
cargo audit

echo "All checks passed!"
```

## Testing Setup

### 1. Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function() {
        // Test implementation
        assert_eq!(function(), expected_result);
    }
}
```

### 2. Integration Tests

```rust
// tests/integration_test.rs
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

#[tokio::test]
async fn test_integration() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Test implementation
}
```

### 3. Test Coverage

```bash
# Install coverage tool
cargo install cargo-tarpaulin

# Run coverage
cargo tarpaulin --out Html

# View coverage report
open tarpaulin-report.html
```

## Debugging Setup

### 1. VS Code Debugging

#### Launch Configuration
```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "lldb",
            "request": "launch",
            "name": "Debug Cleanroom",
            "cargo": {
                "args": ["build", "--bin=cleanroom"],
                "filter": {
                    "name": "cleanroom",
                    "kind": "bin"
                }
            },
            "args": [],
            "cwd": "${workspaceFolder}"
        }
    ]
}
```

### 2. IntelliJ IDEA Debugging

1. Set breakpoints in code
2. Right-click on test or main function
3. Select "Debug 'test_name'"
4. Use debugger controls

## Performance Profiling

### 1. Install Profiling Tools

```bash
# Install profiling tools
cargo install cargo-flamegraph cargo-profdata

# Install system tools (Linux)
sudo apt install linux-tools-common linux-tools-generic
```

### 2. Profile Performance

```bash
# Generate flamegraph
cargo flamegraph --bin cleanroom

# Profile with perf
cargo profdata --help
```

## Docker Development

### 1. Development Container

```dockerfile
# Dockerfile.dev
FROM rust:1.70-slim

# Install development dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    docker.io \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Build project
RUN cargo build

# Default command
CMD ["cargo", "test"]
```

### 2. Docker Compose for Development

```yaml
# docker-compose.dev.yml
version: '3.8'

services:
  cleanroom-dev:
    build:
      context: .
      dockerfile: Dockerfile.dev
    volumes:
      - .:/app
      - cargo-cache:/usr/local/cargo/registry
    environment:
      - RUST_LOG=debug
    command: cargo test

volumes:
  cargo-cache:
```

## Troubleshooting

### Common Issues

#### Rust Installation Issues
```bash
# Update Rust
rustup update

# Check toolchain
rustup show

# Reinstall Rust
rustup self uninstall
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

#### Docker Issues
```bash
# Check Docker status
docker --version
docker ps

# Restart Docker
sudo systemctl restart docker

# Check Docker daemon
sudo systemctl status docker
```

#### Build Issues
```bash
# Clean build artifacts
cargo clean

# Update dependencies
cargo update

# Check for conflicts
cargo tree
```

#### Test Issues
```bash
# Run tests with verbose output
cargo test -- --nocapture

# Run tests in single thread
cargo test -- --test-threads=1

# Run specific test
cargo test test_name
```

### Getting Help

1. **Check Documentation**: Read relevant docs
2. **Search Issues**: Look for similar issues on GitHub
3. **Ask Community**: Use GitHub Discussions
4. **Contact Maintainers**: For urgent issues

## Environment Variables

### Development Variables

```bash
# Rust configuration
export RUST_LOG=debug
export RUST_BACKTRACE=1

# Cleanroom configuration
export CLEANROOM_CONFIG=./cleanroom.dev.toml
export CLEANROOM_ENABLE_DEBUG=true

# Docker configuration
export DOCKER_BUILDKIT=1
export COMPOSE_DOCKER_CLI_BUILD=1
```

### VS Code Environment

```json
{
    "terminal.integrated.env": {
        "RUST_LOG": "debug",
        "RUST_BACKTRACE": "1"
    }
}
```

## Performance Optimization

### 1. Build Optimization

```toml
# Cargo.toml
[profile.dev]
opt-level = 1
debug = true
overflow-checks = true

[profile.release]
opt-level = 3
lto = true
codegen-units = 1
panic = "abort"
```

### 2. Test Optimization

```bash
# Run tests in parallel
cargo test --jobs 4

# Use release build for tests
cargo test --release

# Skip expensive tests
cargo test -- --skip expensive_test
```

## Security Considerations

### 1. Security Audit

```bash
# Install security tools
cargo install cargo-audit

# Run security audit
cargo audit

# Check for vulnerabilities
cargo audit --deny warnings
```

### 2. Dependency Management

```bash
# Check outdated dependencies
cargo outdated

# Update dependencies
cargo update

# Check dependency tree
cargo tree
```

## Summary

### Setup Checklist

- [ ] Rust 1.70+ installed
- [ ] Docker installed and running
- [ ] Development tools installed
- [ ] Repository cloned
- [ ] Project builds successfully
- [ ] Tests pass
- [ ] IDE configured
- [ ] Pre-commit hooks set up

### Next Steps

1. **Read Documentation**: Familiarize yourself with the codebase
2. **Run Examples**: Try the example programs
3. **Write Tests**: Add tests for new functionality
4. **Contribute**: Submit your first pull request

### Resources

- **Rust Book**: [doc.rust-lang.org/book](https://doc.rust-lang.org/book/)
- **Cargo Book**: [doc.rust-lang.org/cargo](https://doc.rust-lang.org/cargo/)
- **Docker Docs**: [docs.docker.com](https://docs.docker.com/)
- **VS Code Rust**: [marketplace.visualstudio.com/items?itemName=rust-lang.rust-analyzer](https://marketplace.visualstudio.com/items?itemName=rust-lang.rust-analyzer)

Your development environment is now ready! Start contributing to Cleanroom.
