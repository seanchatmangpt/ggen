# Contributing to ggen

Thank you for your interest in contributing to ggen! This guide will help you get started.

## Quick Start for Contributors

```bash
git clone https://github.com/YOUR_USERNAME/ggen.git && cd ggen
cargo make quick              # Format + test
cargo make dev                # Format + lint + test
git commit -m "Description of changes"
git push origin your-branch-name
```

## Code of Conduct

Be respectful, inclusive, and considerate. We're all here to build something great together.

## Getting Started

**Prerequisites**: Rust 1.70+ (`curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`), cargo-make (`cargo install cargo-make`), Git

**Development Setup**: Clone your fork → Add upstream remote (`git remote add upstream https://github.com/seanchatmangpt/ggen.git`) → Build (`cargo make build-release`) → Test (`cargo make test`) → Verify (`ggen doctor`)

## Development Workflow

**Before You Start**: Check existing issues → Create an issue → Get feedback (wait for maintainer approval on larger changes)

**Making Changes**: Create feature branch (`git checkout -b feature/your-feature-name`) → Make changes → Format (`cargo make fmt`) → Lint (`cargo make lint`) → Test (`cargo make test`) → Full CI (`cargo make ci`)

**Commit Messages**: Follow [Conventional Commits](https://www.conventionalcommits.org/) format: `<type>(<scope>): <description>`. Types: `feat` (new feature), `fix` (bug fix), `docs` (documentation), `test` (tests), `refactor` (refactoring), `perf` (performance), `chore` (maintenance). Examples: `feat(cli): add ggen doctor command`, `fix(ai): correct SPARQL query generation`

## Finding Good First Issues

**Tags**: `good first issue` (perfect for newcomers), `help wanted` (we need help), `documentation` (documentation improvements)

**New to Rust?**: Start with documentation improvements, test coverage improvements, example code additions

**Experienced?**: Try performance optimizations, new AI providers, advanced features

## Pull Request Process

**Before Submitting**: Code formatted (`cargo make fmt`), linting passes (`cargo make lint`), all tests pass (`cargo make test`), new tests added for new features, documentation updated, CHANGELOG.md updated (if applicable), files in correct locations (no test/template/data files in root)

**Submitting a PR**: Push to your fork → Create Pull Request on GitHub (clear title, reference issues, describe changes, add screenshots for UI changes) → Fill PR template (description, type of change, related issues, testing, checklist)

**Review Process**: Response time (aim for 48 hours), feedback (address all review comments), approval (requires at least 1 maintainer approval), merge (maintainers will merge once approved)

## Code Style

**Rust Style**: Follow [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/). Use clear, descriptive names. Proper error handling (Result types, no `.expect()` or `.unwrap()` in production paths)

**File Organization**: `cli/` (CLI commands, `src/cmds/` for implementations), `ggen-core/` (core generation logic), `ggen-ai/` (AI integration), `cleanroom/` (testing framework), `utils/` (shared utilities), `tests/` (integration tests)

**Root Directory File Placement Rules**: **CRITICAL** - Files must be in correct directories. Root should only contain essential project files. **MUST be in root**: `Cargo.toml`, `Makefile.toml`, `README.md`, `CHANGELOG.md`, `CONTRIBUTING.md`, `LICENSE`, `SECURITY.md`, `deny.toml`, `rustfmt.toml`, `.clog.toml`, `VERSION`. **MUST NOT be in root**: Test files (`.rs`) → `tests/`, Template files (`.tmpl`) → `templates/`, Data files (`.ttl`, `.rdf`) → `tests/data/` or `examples/`, Scripts (`.sh`) → `scripts/`, Temporary files → Delete immediately, Redundant Makefiles → Use only `Makefile.toml`, Outdated completion reports → `docs/archive/`

**Module Organization**: Imports (std, external, internal) → Type definitions → Public API → Private helpers → Tests

## Testing

**Running Tests**: All tests (`cargo make test`), specific test (`cargo test test_name`), with logging (`RUST_LOG=debug cargo test`), integration tests only (`cargo test --test integration_tests`), cleanroom tests (`cargo test --test cli_integration_cleanroom`)

**Writing Tests**: Unit tests (colocated with code, use `#[cfg(test)] mod tests`), integration tests (in `tests/`, use `Command::new("ggen")`)

**Test Coverage**: Aim for **>85% coverage** on critical paths (core generation logic, template parsing, AI integration, CLI commands)

## Documentation

**Code Documentation**: All public APIs must have doc comments. Use `///` for documentation. Include arguments, returns, errors, examples. Examples should compile and run.

**Module Documentation**: All public modules must have module-level documentation using `//!` comments at the top of the file. Top-level crates (`lib.rs`) should use `#` heading format. Module documentation should include:
- Brief description of the module's purpose
- Key features/capabilities (bullet list)
- Usage examples where appropriate (using `rust,no_run` for non-executable examples)
- Architecture notes for complex modules

**Module Documentation Format**:
```rust
//! Brief description of module purpose
//!
//! This module provides [key functionality]. It handles [main responsibilities]
//! and integrates with [related systems].
//!
//! ## Features
//!
//! - **Feature 1**: Description
//! - **Feature 2**: Description
//!
//! ## Examples
//!
//! ```rust,no_run
//! use crate::module::Function;
//! // Example code
//! ```
```

**Verification**: Run `cargo make docs-check` to verify documentation compiles without errors. This is automatically run in pre-commit hooks. See [Documentation Standards](docs/DOCUMENTATION_STANDARDS.md) for complete guidelines.

**README and Guides**: Keep README.md concise and focused on quick start. Put detailed guides in `docs/`. Include examples in `examples/`. Update CHANGELOG.md for user-facing changes.

**Documentation Standards**: All public APIs must have doc comments. Examples should compile and run. Update docs when changing APIs. Add inline comments for complex logic. See [Documentation Standards](docs/DOCUMENTATION_STANDARDS.md) for detailed formatting guidelines.

## Bug Reporting

Thank you for helping us improve ggen! This guide explains how to report bugs effectively and responsibly.

### Before You Report

Please check the following before opening a bug report:

1. **Check the [Issues](https://github.com/seanchatmangpt/ggen/issues)** - Your bug may already be reported
2. **Check the [Discussions](https://github.com/seanchatmangpt/ggen/discussions)** - It might be a known limitation
3. **Update to the latest version** - The bug may be already fixed
4. **Check the documentation** - It might not be a bug but expected behavior

### Security Vulnerabilities

**Do not** open a public issue for security vulnerabilities. Instead:

1. Email: `sean@chatmangpt.com` with subject line `[SECURITY] ggen vulnerability`
2. Include:
   - A clear description of the vulnerability
   - Steps to reproduce (if possible)
   - The affected versions
   - Any suggested fixes

We aim to respond to security reports within 48 hours.

### How to Report a Bug

#### 1. Use a Clear, Descriptive Title

Good: `Watch service crashes when RDF file contains non-UTF8 characters`
Bad: `Watch mode broken`

#### 2. Provide Your Environment

```
- ggen version: 3.0.0
- Operating System: Ubuntu 22.04 LTS
- Rust version: 1.75.0
- Installed features: all
```

#### 3. Describe the Bug

Write a clear description including:
- What you expected to happen
- What actually happened
- When the issue started (if known)

#### 4. Provide Minimal Reproducible Example

```rust
// Minimal code that reproduces the bug
use ggen::GeneratorConfig;

fn main() {
    let config = GeneratorConfig::default();
    // Steps to reproduce bug...
}
```

#### 5. Include Relevant Logs

Attach error messages, stack traces, or logs:

```
thread 'main' panicked at 'Failed to parse ontology: expected RDF/XML',
src/core/ontology.rs:42:5
```

Enable debug logging:

```bash
RUST_LOG=debug ggen generate --template templates/
```

#### 6. Screenshots (if applicable)

For UI-related issues, include screenshots showing the problem.

### Bug Report Template

```markdown
## Environment

- ggen version:
- OS:
- Rust version:

## Description

Brief description of the bug.

## Steps to Reproduce

1.
2.
3.

## Expected Behavior

What should happen.

## Actual Behavior

What actually happens.

## Minimal Example

```rust
// Code that reproduces the issue
```

## Logs/Error Messages

```
Paste error messages here
```

## Additional Context

Any other context about the problem.
```

### Bug Severity Levels

- **Critical**: System crash, data loss, security vulnerability
- **High**: Core functionality broken, affects most users
- **Medium**: Feature doesn't work as expected, workaround exists
- **Low**: Minor issue, cosmetic bug, affects edge cases

### What Happens Next

1. We'll acknowledge your report within 48 hours
2. We'll assign a severity level
3. We'll work on a fix (timeline depends on severity)
4. You'll be notified when fixed in a release

### Contributing a Fix

Found a fix for the bug? We'd love your contribution!

1. Fork the repository
2. Create a feature branch: `git checkout -b fix/issue-description`
3. Add tests for the bug
4. Submit a pull request with a clear description

See [Pull Request Process](#pull-request-process) section above for more details.

## Getting Help

**Stuck?**: Check documentation ([Main README](README.md), [Full Docs](https://seanchatmangpt.github.io/ggen/), [Architecture Overview](docs/CLAUDE.md)) → Search issues ([Open Issues](https://github.com/seanchatmangpt/ggen/issues), [Closed Issues](https://github.com/seanchatmangpt/ggen/issues?q=is%3Aissue+is%3Aclosed)) → Ask questions ([Create a Discussion](https://github.com/seanchatmangpt/ggen/discussions)) → Report bugs (see [Bug Reporting](#bug-reporting) section above)

## Project-Specific Guidelines

**Working with AI Integration**: Handle API failures gracefully (use `match` with error handling, retry on rate limits). Don't panic on API failures (no `.expect()` or `.unwrap()`)

**Working with Templates**: Templates use Tera syntax. Test templates with edge cases. Validate YAML frontmatter. Handle missing variables gracefully.

**Working with RDF/SPARQL**: Validate RDF graphs before processing. Cache SPARQL query results. Handle malformed SPARQL gracefully. Test with various RDF formats (Turtle, N-Triples, JSON-LD).

## Recognition

Contributors are automatically added to: [README Contributors Section](README.md#contributors) (via All Contributors Bot), [CONTRIBUTORS.md](CONTRIBUTORS.md) (manual updates), Git commit history.

**Notable contributions** may receive: Shout-outs in release notes, special recognition in community channels, swag (coming soon!)

## Release Process

**For maintainers only**: 

1. **FMEA-Based Validation**: Run `cargo make release-validate` to execute all automated release checks (git state, version consistency, artifacts, build, security, CHANGELOG, breaking changes, docs sync). See [FMEA Analysis](./docs/FMEA_RELEASE_V2.6.0.md) for details.

2. **Release Steps**: Update version in Cargo.toml → Update CHANGELOG.md → Run `cargo make release-validate` → Create release commit (`git commit -am "chore: release v2.x.x"`) → Tag release (`git tag -a v2.x.x -m "Release v2.x.x"`) → Push (`git push origin master --tags`) → Publish to crates.io (`cargo publish`)

**Quick Reference**: See `docs/releases/RELEASE_v2.6.0_CHECKLIST.md` for complete release checklist with FMEA-based validation.

## Questions?

**General questions**: [Start a Discussion](https://github.com/seanchatmangpt/ggen/discussions)

**Bug reports**: [Create an Issue](https://github.com/seanchatmangpt/ggen/issues/new)

**Security issues**: Email security@ggen.dev

**Direct contact**: Open an issue or discussion first

## Thank You!

Every contribution helps make ggen better. Whether you're fixing a typo, adding a feature, or helping with documentation - thank you for being part of the community! 🙏

---

**Ready to contribute?**

1. Fork the repo
2. Find a [good first issue](https://github.com/seanchatmangpt/ggen/labels/good%20first%20issue)
3. Make your changes
4. Submit a PR

**Happy coding!** 🚀
