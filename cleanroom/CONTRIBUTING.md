# Contributing to Cleanroom

Thank you for your interest in contributing to the Cleanroom Testing Framework! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [Getting Started](#getting-started)
3. [Development Setup](#development-setup)
4. [Contributing Process](#contributing-process)
5. [Code Style](#code-style)
6. [Testing](#testing)
7. [Documentation](#documentation)
8. [Release Process](#release-process)
9. [Community Guidelines](#community-guidelines)

## Code of Conduct

This project follows the [Rust Code of Conduct](https://www.rust-lang.org/policies/code-of-conduct). By participating, you agree to uphold this code.

### Our Pledge

We are committed to making participation in our project a harassment-free experience for everyone, regardless of age, body size, disability, ethnicity, gender identity and expression, level of experience, education, socio-economic status, nationality, personal appearance, race, religion, or sexual identity and orientation.

### Expected Behavior

- Use welcoming and inclusive language
- Be respectful of differing viewpoints and experiences
- Gracefully accept constructive criticism
- Focus on what is best for the community
- Show empathy towards other community members

### Unacceptable Behavior

- The use of sexualized language or imagery
- Trolling, insulting/derogatory comments, and personal or political attacks
- Public or private harassment
- Publishing others' private information without explicit permission
- Other conduct which could reasonably be considered inappropriate in a professional setting

## Getting Started

### Prerequisites

Before contributing, ensure you have:

- **Rust 1.70+** installed ([rustup.rs](https://rustup.rs/))
- **Docker** installed and running ([docker.com](https://www.docker.com/))
- **Git** for version control
- **A GitHub account** for submitting contributions

### Fork and Clone

1. Fork the repository on GitHub
2. Clone your fork locally:
   ```bash
   git clone https://github.com/YOUR_USERNAME/ggen.git
   cd ggen/cleanroom
   ```
3. Add the upstream repository:
   ```bash
   git remote add upstream https://github.com/sac/ggen.git
   ```

### Verify Installation

```bash
# Check Rust installation
rustc --version
cargo --version

# Check Docker installation
docker --version
docker ps

# Build the project
cargo build

# Run tests
cargo test
```

## Development Setup

### 1. Environment Setup

#### Install Dependencies

```bash
# Install Rust toolchain
rustup update

# Install development dependencies
cargo install cargo-fmt cargo-clippy cargo-doc

# Install testing tools
cargo install cargo-tarpaulin  # For coverage
cargo install cargo-audit      # For security auditing
```

#### Configure Git

```bash
# Set up Git user information
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"

# Set up Git hooks (optional)
cp .git/hooks/pre-commit.sample .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
```

### 2. IDE Setup

#### VS Code

Install the following extensions:
- Rust Analyzer
- CodeLLDB
- Docker
- GitLens

#### IntelliJ IDEA / CLion

Install the Rust plugin and configure:
- Rust toolchain
- Docker integration
- Git integration

### 3. Project Structure

```
cleanroom/
├── src/                    # Source code
│   ├── lib.rs             # Library entry point
│   ├── cleanroom.rs       # Core cleanroom functionality
│   ├── backend/           # Backend implementations
│   ├── containers.rs      # Container management
│   ├── policy.rs          # Security policies
│   ├── scenario.rs        # Test scenarios
│   ├── error.rs           # Error handling
│   └── ...
├── tests/                 # Integration tests
├── examples/              # Example programs
├── docs/                  # Documentation
├── Cargo.toml             # Project configuration
└── README.md              # Project overview
```

## Contributing Process

### 1. Issue Tracking

#### Before Starting Work

1. **Check existing issues**: Look for similar issues or feature requests
2. **Create an issue**: If none exists, create a new issue describing your contribution
3. **Wait for approval**: Wait for maintainer feedback before starting work
4. **Assign yourself**: Assign the issue to yourself

#### Issue Types

- **Bug**: Something isn't working
- **Enhancement**: New feature or improvement
- **Documentation**: Documentation improvements
- **Performance**: Performance improvements
- **Security**: Security-related changes

### 2. Branch Strategy

#### Branch Naming

Use descriptive branch names:
- `feature/description` - New features
- `fix/description` - Bug fixes
- `docs/description` - Documentation changes
- `refactor/description` - Code refactoring
- `perf/description` - Performance improvements

#### Branch Creation

```bash
# Update main branch
git checkout main
git pull upstream main

# Create feature branch
git checkout -b feature/your-feature-name

# Push branch to your fork
git push -u origin feature/your-feature-name
```

### 3. Development Workflow

#### Make Changes

1. **Write code**: Implement your changes
2. **Write tests**: Add tests for new functionality
3. **Update documentation**: Update relevant documentation
4. **Run checks**: Ensure all checks pass

#### Code Quality Checks

```bash
# Format code
cargo fmt

# Run clippy
cargo clippy --all-targets --all-features

# Run tests
cargo test

# Check documentation
cargo doc --no-deps

# Run security audit
cargo audit
```

### 4. Pull Request Process

#### Create Pull Request

1. **Push changes**: Push your branch to your fork
2. **Create PR**: Create a pull request on GitHub
3. **Fill template**: Complete the pull request template
4. **Link issues**: Link related issues using `Fixes #123` or `Closes #123`

#### Pull Request Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Testing
- [ ] Tests pass locally
- [ ] New tests added for new functionality
- [ ] All existing tests pass

## Checklist
- [ ] Code follows style guidelines
- [ ] Self-review completed
- [ ] Documentation updated
- [ ] No breaking changes (or clearly documented)
```

#### Review Process

1. **Automated checks**: Wait for CI/CD checks to pass
2. **Code review**: Address reviewer feedback
3. **Testing**: Ensure all tests pass
4. **Approval**: Wait for maintainer approval
5. **Merge**: Maintainer will merge the PR

## Code Style

### 1. Rust Style Guidelines

#### Formatting

Use `cargo fmt` to format code according to Rust standards:

```bash
cargo fmt
```

#### Naming Conventions

- **Functions**: `snake_case`
- **Variables**: `snake_case`
- **Types**: `PascalCase`
- **Constants**: `SCREAMING_SNAKE_CASE`
- **Modules**: `snake_case`

#### Documentation

Use Rust doc comments for public APIs:

```rust
/// Creates a new CleanroomEnvironment with the specified configuration.
///
/// # Arguments
///
/// * `config` - The configuration for the environment
///
/// # Returns
///
/// * `Result<CleanroomEnvironment>` - The created environment or an error
///
/// # Examples
///
/// ```
/// use cleanroom::{CleanroomEnvironment, CleanroomConfig};
///
/// let config = CleanroomConfig::default();
/// let environment = CleanroomEnvironment::new(config).await?;
/// ```
pub async fn new(config: CleanroomConfig) -> Result<CleanroomEnvironment> {
    // Implementation
}
```

### 2. Code Organization

#### Module Structure

```rust
// lib.rs
pub mod cleanroom;
pub mod backend;
pub mod containers;
pub mod policy;
pub mod scenario;
pub mod error;

// Re-export main types
pub use cleanroom::CleanroomEnvironment;
pub use policy::Policy;
pub use scenario::Scenario;
pub use error::CleanroomError;
```

#### Error Handling

Use the `Result` type for error handling:

```rust
use cleanroom::Result;

pub fn validate_config(config: &Config) -> Result<()> {
    if config.timeout < 0 {
        return Err(CleanroomError::validation_error("Timeout must be positive"));
    }
    Ok(())
}
```

### 3. Performance Guidelines

#### Memory Management

- Use `Arc` for shared ownership
- Use `RwLock` for read-heavy workloads
- Avoid unnecessary allocations
- Use object pools for frequent allocations

#### Async Programming

- Use `tokio` for async runtime
- Prefer `async/await` over callbacks
- Use `tokio::spawn` for concurrent tasks
- Handle errors properly in async code

## Testing

### 1. Test Structure

#### Unit Tests

Place unit tests in the same file as the code:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function() {
        // Test implementation
    }
}
```

#### Integration Tests

Place integration tests in the `tests/` directory:

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

### 2. Test Guidelines

#### Test Naming

Use descriptive test names:
- `test_user_creation_success`
- `test_user_creation_duplicate_email`
- `test_user_creation_invalid_email`

#### Test Organization

```rust
#[cfg(test)]
mod tests {
    use super::*;

    mod user_creation_tests {
        use super::*;

        #[test]
        fn test_success() {
            // Test successful case
        }

        #[test]
        fn test_duplicate_email() {
            // Test duplicate email case
        }
    }

    mod user_validation_tests {
        use super::*;

        #[test]
        fn test_valid_email() {
            // Test valid email
        }

        #[test]
        fn test_invalid_email() {
            // Test invalid email
        }
    }
}
```

### 3. Test Coverage

#### Coverage Requirements

- **Unit tests**: > 90% coverage
- **Integration tests**: > 80% coverage
- **Critical paths**: 100% coverage

#### Coverage Measurement

```bash
# Install coverage tool
cargo install cargo-tarpaulin

# Run coverage
cargo tarpaulin --out Html

# View coverage report
open tarpaulin-report.html
```

## Documentation

### 1. Code Documentation

#### API Documentation

Document all public APIs:

```rust
/// A container for running tests in isolated environments.
///
/// This struct provides methods for managing containers and executing tests
/// in a controlled, isolated environment.
///
/// # Examples
///
/// ```
/// use cleanroom::{CleanroomEnvironment, CleanroomConfig};
///
/// let config = CleanroomConfig::default();
/// let environment = CleanroomEnvironment::new(config).await?;
/// ```
pub struct CleanroomEnvironment {
    // Fields
}
```

#### Inline Documentation

Add comments for complex logic:

```rust
// Calculate the optimal number of worker threads based on CPU cores
// and system load to maximize performance while avoiding resource contention
let worker_threads = std::cmp::min(
    num_cpus::get(),
    max_worker_threads
);
```

### 2. User Documentation

#### README Updates

Update the README for significant changes:
- New features
- Configuration changes
- Breaking changes
- Performance improvements

#### Example Updates

Update examples for new functionality:

```rust
// examples/new_feature.rs
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Demonstrate new feature
    println!("New feature works!");
    
    Ok(())
}
```

### 3. Architecture Documentation

#### ADR Updates

Create Architecture Decision Records (ADRs) for significant decisions:
- New features
- Architecture changes
- Technology choices
- Performance optimizations

## Release Process

### 1. Version Management

#### Semantic Versioning

Follow [Semantic Versioning](https://semver.org/):
- **MAJOR**: Breaking changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

#### Version Bumping

```bash
# Update version in Cargo.toml
# Update CHANGELOG.md
# Update documentation
# Create release tag
git tag -a v1.0.0 -m "Release version 1.0.0"
git push origin v1.0.0
```

### 2. Release Checklist

#### Pre-Release

- [ ] All tests pass
- [ ] Documentation updated
- [ ] CHANGELOG.md updated
- [ ] Version bumped
- [ ] Security audit passed
- [ ] Performance benchmarks updated

#### Release

- [ ] Create release tag
- [ ] Push to crates.io
- [ ] Update GitHub releases
- [ ] Announce release
- [ ] Update documentation

### 3. Changelog Management

#### CHANGELOG.md Format

```markdown
# Changelog

All notable changes to this project will be documented in this file.

## [1.0.0] - 2024-01-15

### Added
- Initial release
- Core cleanroom functionality
- Container management
- Security policies

### Changed
- N/A

### Fixed
- N/A

### Removed
- N/A
```

## Community Guidelines

### 1. Communication

#### GitHub Issues

- Use clear, descriptive titles
- Provide context and examples
- Include relevant code snippets
- Use labels appropriately

#### Pull Requests

- Keep PRs focused and small
- Provide clear descriptions
- Include tests and documentation
- Respond to feedback promptly

#### Discussions

- Use GitHub Discussions for questions
- Be respectful and helpful
- Search before asking
- Provide context and examples

### 2. Getting Help

#### Resources

- **Documentation**: Check the docs first
- **Examples**: Look at example code
- **Issues**: Search existing issues
- **Discussions**: Ask in GitHub Discussions
- **Discord**: Join our Discord server

#### Asking Questions

When asking questions, include:
- What you're trying to do
- What you've tried
- What error messages you're seeing
- Your environment details
- Relevant code snippets

### 3. Recognition

#### Contributors

We recognize contributors in several ways:
- **Contributors list**: Listed in README.md
- **Release notes**: Mentioned in release notes
- **GitHub**: Listed as contributors
- **Community**: Recognized in community channels

#### Types of Contributions

We welcome all types of contributions:
- **Code**: Bug fixes, features, improvements
- **Documentation**: Guides, examples, API docs
- **Testing**: Test cases, test improvements
- **Community**: Helping others, answering questions
- **Design**: UI/UX improvements, architecture

## Development Tools

### 1. Recommended Tools

#### Code Quality

```bash
# Install development tools
cargo install cargo-fmt cargo-clippy cargo-doc
cargo install cargo-tarpaulin cargo-audit
cargo install cargo-expand cargo-tree
```

#### Git Tools

```bash
# Install Git tools
cargo install git-cliff  # For changelog generation
cargo install git-oxide   # For Git operations
```

### 2. IDE Configuration

#### VS Code Settings

```json
{
    "rust-analyzer.checkOnSave.command": "clippy",
    "rust-analyzer.checkOnSave.extraArgs": ["--", "-W", "clippy::all"],
    "rust-analyzer.cargo.features": "all",
    "rust-analyzer.procMacro.enable": true
}
```

#### IntelliJ IDEA Settings

- Enable Rust plugin
- Configure Rust toolchain
- Enable clippy integration
- Configure Docker integration

### 3. Pre-commit Hooks

#### Git Hooks

```bash
#!/bin/sh
# .git/hooks/pre-commit

# Run cargo fmt
cargo fmt --all -- --check

# Run clippy
cargo clippy --all-targets --all-features -- -D warnings

# Run tests
cargo test

# Run security audit
cargo audit
```

## Troubleshooting

### 1. Common Issues

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

### 2. Getting Help

#### Before Asking

1. **Check documentation**: Read the relevant docs
2. **Search issues**: Look for similar issues
3. **Try examples**: Run the example code
4. **Check environment**: Verify your setup

#### When Asking

1. **Be specific**: Describe the exact problem
2. **Provide context**: Include relevant code
3. **Include errors**: Paste error messages
4. **Share environment**: Include system details

## Summary

### Key Points

1. **Follow the process**: Use the established workflow
2. **Write good code**: Follow style guidelines
3. **Test thoroughly**: Add tests for new functionality
4. **Document changes**: Update relevant documentation
5. **Be respectful**: Follow the code of conduct
6. **Ask for help**: Use community resources

### Quick Start

1. Fork the repository
2. Clone your fork
3. Create a feature branch
4. Make your changes
5. Run tests and checks
6. Submit a pull request

### Resources

- **Repository**: [github.com/sac/ggen](https://github.com/sac/ggen)
- **Documentation**: [docs.cleanroom.dev](https://docs.cleanroom.dev)
- **Discord**: [discord.gg/cleanroom](https://discord.gg/cleanroom)
- **Issues**: [github.com/sac/ggen/issues](https://github.com/sac/ggen/issues)

Thank you for contributing to Cleanroom! Your contributions help make the project better for everyone.
