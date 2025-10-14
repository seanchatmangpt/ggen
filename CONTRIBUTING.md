# Contributing to ggen

Thank you for your interest in contributing to ggen! This guide will help you get started.

## Quick Start for Contributors

```bash
# 1. Fork and clone the repository
git clone https://github.com/YOUR_USERNAME/ggen.git
cd ggen

# 2. Set up development environment
cargo make quick  # Runs format + test

# 3. Make your changes
# ... edit files ...

# 4. Test your changes
cargo make dev    # Format, lint, and test

# 5. Submit a pull request
git add .
git commit -m "Description of changes"
git push origin your-branch-name
```

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Workflow](#development-workflow)
- [Finding Good First Issues](#finding-good-first-issues)
- [Pull Request Process](#pull-request-process)
- [Code Style](#code-style)
- [Testing](#testing)
- [Documentation](#documentation)
- [Getting Help](#getting-help)

## Code of Conduct

Be respectful, inclusive, and considerate. We're all here to build something great together.

## Getting Started

### Prerequisites

- **Rust 1.70+**: `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
- **cargo-make**: `cargo install cargo-make` (optional but recommended)
- **Git**: For version control

### Development Setup

```bash
# Clone your fork
git clone https://github.com/YOUR_USERNAME/ggen.git
cd ggen

# Add upstream remote
git remote add upstream https://github.com/seanchatmangpt/ggen.git

# Build the project
cargo make build-release

# Run tests
cargo make test

# Verify everything works
ggen doctor  # Once implemented
```

## Development Workflow

### Before You Start

1. **Check existing issues**: See if someone is already working on it
2. **Create an issue**: Discuss your idea before implementing
3. **Get feedback**: Wait for maintainer approval on larger changes

### Making Changes

```bash
# Create a feature branch
git checkout -b feature/your-feature-name

# Make your changes
# ...

# Format code
cargo make fmt

# Run linter
cargo make lint

# Run tests
cargo make test

# Run full CI check
cargo make ci
```

### Commit Messages

Follow the [Conventional Commits](https://www.conventionalcommits.org/) format:

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `test`: Adding or updating tests
- `refactor`: Code refactoring
- `perf`: Performance improvements
- `chore`: Maintenance tasks

**Examples:**
```
feat(cli): add ggen doctor command for health checks

Implements a new doctor command that checks:
- Rust/Cargo versions
- Git installation
- Optional tools (Ollama, Docker)

Closes #123
```

```
fix(ai): correct SPARQL query generation for complex graphs

The AI was generating invalid SPARQL when handling nested queries.
Added proper escaping and validation.

Fixes #456
```

## Finding Good First Issues

Look for issues tagged with:
- **`good first issue`**: Perfect for newcomers
- **`help wanted`**: We need help with this
- **`documentation`**: Documentation improvements

**New to Rust?** Start with:
- Documentation improvements
- Test coverage improvements
- Example code additions

**Experienced?** Try:
- Performance optimizations
- New AI providers
- Advanced features

## Pull Request Process

### Before Submitting

- ✅ Code formatted: `cargo make fmt`
- ✅ Linting passes: `cargo make lint`
- ✅ All tests pass: `cargo make test`
- ✅ New tests added for new features
- ✅ Documentation updated
- ✅ CHANGELOG.md updated (if applicable)

### Submitting a PR

1. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

2. **Create Pull Request on GitHub**:
   - Use a clear, descriptive title
   - Reference related issues
   - Describe what changed and why
   - Add screenshots for UI changes

3. **PR Template** (fill this in):
   ```markdown
   ## Description
   Brief description of changes

   ## Type of Change
   - [ ] Bug fix
   - [ ] New feature
   - [ ] Breaking change
   - [ ] Documentation update

   ## Related Issues
   Closes #XXX

   ## Testing
   How did you test this?

   ## Checklist
   - [ ] Code formatted (cargo make fmt)
   - [ ] Linting passes (cargo make lint)
   - [ ] Tests pass (cargo make test)
   - [ ] Documentation updated
   ```

### Review Process

- **Response Time**: We aim to respond within 48 hours
- **Feedback**: Address all review comments
- **Approval**: Requires at least 1 maintainer approval
- **Merge**: Maintainers will merge once approved

## Code Style

### Rust Style

Follow the [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/):

```rust
// ✅ Good: Clear, descriptive names
pub fn generate_template_from_description(desc: &str) -> Result<Template> {
    // ...
}

// ❌ Bad: Unclear abbreviations
pub fn gen_tmpl(d: &str) -> Result<Tmpl> {
    // ...
}

// ✅ Good: Proper error handling
pub fn read_config() -> Result<Config> {
    let path = config_path()?;
    let content = fs::read_to_string(path)
        .map_err(|e| anyhow::anyhow!("Failed to read config: {}", e))?;
    Ok(toml::from_str(&content)?)
}

// ❌ Bad: Using .expect() or .unwrap()
pub fn read_config() -> Result<Config> {
    let path = config_path().expect("config path");  // Don't do this!
    // ...
}
```

### File Organization

```
ggen/
├── cli/               # CLI commands
│   └── src/cmds/      # Individual command implementations
├── ggen-core/         # Core generation logic
├── ggen-ai/           # AI integration
├── cleanroom/         # Testing framework
├── utils/             # Shared utilities
└── tests/             # Integration tests
```

### Module Organization

```rust
// Module order:
// 1. Imports (std, external, internal)
use std::path::Path;
use anyhow::Result;
use crate::template::Template;

// 2. Type definitions
pub struct Generator { /* ... */ }

// 3. Public API
impl Generator {
    pub fn new() -> Self { /* ... */ }
    pub fn generate(&self) -> Result<String> { /* ... */ }
}

// 4. Private helpers
impl Generator {
    fn validate(&self) -> Result<()> { /* ... */ }
}

// 5. Tests
#[cfg(test)]
mod tests { /* ... */ }
```

## Testing

### Running Tests

```bash
# All tests
cargo make test

# Specific test
cargo test test_name

# With logging
RUST_LOG=debug cargo test

# Integration tests only
cargo test --test integration_tests

# Cleanroom tests
cargo test --test cli_integration_cleanroom
```

### Writing Tests

**Unit Tests:**
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_template_parsing() {
        let input = r#"---
to: "src/main.rs"
---
fn main() {}
"#;
        let template = Template::parse(input).unwrap();
        assert_eq!(template.target(), "src/main.rs");
    }
}
```

**Integration Tests:**
```rust
// tests/cli_integration_test.rs
#[test]
fn test_gen_command() {
    let output = Command::new("ggen")
        .arg("gen")
        .arg("template.tmpl")
        .output()
        .expect("failed to execute");

    assert!(output.status.success());
}
```

### Test Coverage

We aim for **>85% coverage** on critical paths:
- Core generation logic
- Template parsing
- AI integration
- CLI commands

## Documentation

### Code Documentation

```rust
/// Generates code from a template with the given variables.
///
/// # Arguments
///
/// * `template` - The template to render
/// * `vars` - Variables to substitute
///
/// # Returns
///
/// The rendered template as a String
///
/// # Errors
///
/// Returns an error if template rendering fails or variables are missing
///
/// # Examples
///
/// ```
/// use ggen_core::Generator;
///
/// let generator = Generator::new();
/// let result = generator.generate(template, vars)?;
/// ```
pub fn generate(&self, template: &Template, vars: &HashMap<String, String>) -> Result<String> {
    // ...
}
```

### README and Guides

- Keep README.md concise and focused on quick start
- Put detailed guides in `docs/`
- Include examples in `examples/`
- Update CHANGELOG.md for user-facing changes

### Documentation Standards

- All public APIs must have doc comments
- Examples should compile and run
- Update docs when changing APIs
- Add inline comments for complex logic

## Getting Help

### Stuck? Here's how to get help:

1. **Check Documentation**:
   - [Main README](README.md)
   - [Full Docs](https://seanchatmangpt.github.io/ggen/)
   - [Architecture Overview](CLAUDE.md)

2. **Search Issues**:
   - [Open Issues](https://github.com/seanchatmangpt/ggen/issues)
   - [Closed Issues](https://github.com/seanchatmangpt/ggen/issues?q=is%3Aissue+is%3Aclosed)

3. **Ask Questions**:
   - [Create a Discussion](https://github.com/seanchatmangpt/ggen/discussions)
   - [Join Discord](https://discord.gg/ggen) (coming soon)

4. **Report Bugs**:
   - [Create an Issue](https://github.com/seanchatmangpt/ggen/issues/new)
   - Include: ggen version, OS, error message, steps to reproduce

## Project-Specific Guidelines

### Working with AI Integration

```rust
// ✅ Good: Handle API failures gracefully
match ai_client.generate(prompt).await {
    Ok(response) => Ok(response),
    Err(e) if e.is_rate_limit() => {
        warn!("Rate limited, retrying...");
        tokio::time::sleep(Duration::from_secs(5)).await;
        ai_client.generate(prompt).await
    }
    Err(e) => Err(e),
}

// ❌ Bad: Panic on API failures
let response = ai_client.generate(prompt).await.expect("API call failed");
```

### Working with Templates

- Templates use Tera syntax
- Test templates with edge cases
- Validate YAML frontmatter
- Handle missing variables gracefully

### Working with RDF/SPARQL

- Validate RDF graphs before processing
- Cache SPARQL query results
- Handle malformed SPARQL gracefully
- Test with various RDF formats (Turtle, N-Triples, JSON-LD)

## Recognition

Contributors are automatically added to:
- [README Contributors Section](README.md#contributors) (via All Contributors Bot)
- [CONTRIBUTORS.md](CONTRIBUTORS.md) (manual updates)
- Git commit history

**Notable contributions** may receive:
- Shout-outs in release notes
- Special recognition in community channels
- Swag (coming soon!)

## Release Process

**For maintainers only:**

```bash
# 1. Update version in Cargo.toml
# 2. Update CHANGELOG.md
# 3. Create release commit
git commit -am "chore: release v1.x.x"

# 4. Tag release
git tag -a v1.x.x -m "Release v1.x.x"

# 5. Push
git push origin master --tags

# 6. Publish to crates.io
cargo publish
```

## Questions?

If you have questions not covered here:
- **General questions**: [Start a Discussion](https://github.com/seanchatmangpt/ggen/discussions)
- **Bug reports**: [Create an Issue](https://github.com/seanchatmangpt/ggen/issues/new)
- **Security issues**: Email security@ggen.dev
- **Direct contact**: Open an issue or discussion first

## Thank You!

Every contribution helps make ggen better. Whether you're fixing a typo, adding a feature, or helping with documentation - thank you for being part of the community! 🙏

---

**Ready to contribute?**

1. Fork the repo
2. Find a [good first issue](https://github.com/seanchatmangpt/ggen/labels/good%20first%20issue)
3. Make your changes
4. Submit a PR

**Happy coding!** 🚀
