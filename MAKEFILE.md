# Cargo Make Tasks for Ggen

This document describes the comprehensive `Makefile.toml` that standardizes all development, testing, and deployment workflows for the ggen project.

## Installation

First, install `cargo-make` if you haven't already:

```bash
cargo install cargo-make
```

## Quick Start

```bash
# Run the default development workflow
cargo make

# Or use the short alias
cargo make d
```

## Core Development Tasks

### Basic Commands
```bash
cargo make check          # Check code without building
cargo make build          # Build in debug mode
cargo make build-release  # Build in release mode
cargo make clean          # Clean build artifacts
cargo make fmt            # Format code
cargo make lint           # Run clippy with strict settings
```

### Development Workflow
```bash
cargo make dev            # Format, lint, and test
cargo make pre-commit     # Pre-commit checks
cargo make pre-push       # Comprehensive pre-push checks
```

## Testing Suite

### Unit and Integration Tests
```bash
cargo make test           # Run all tests
cargo make test-unit      # Unit tests only
cargo make test-integration # Integration tests only
cargo make test-cli       # CLI-specific tests
cargo make test-core      # Core module tests
```

### Specialized Testing
```bash
cargo make test-single-threaded  # Deterministic single-threaded tests
cargo make test-release          # Tests in release mode
cargo make test-coverage         # Generate HTML coverage report
cargo make test-coverage-xml     # Generate XML coverage report
```

### Determinism and Validation
```bash
cargo make deterministic        # Run tests with fixed seeds
cargo make validate-outputs     # Validate deterministic outputs
cargo make slo-check           # Check SLO compliance
```

## Code Quality and Linting

```bash
cargo make lint           # Strict clippy checks
cargo make lint-allow     # Allow warnings
cargo make audit          # Security audit
cargo make outdated       # Check outdated dependencies
cargo make deny           # License and dependency checks
```

## Documentation

### Rust API Documentation
```bash
cargo make doc            # Generate and open docs
cargo make doc-private    # Include private items
cargo make doc-check      # Check docs without generating
```

### mdBook User Documentation
```bash
cargo make docs-build     # Build mdbook documentation
cargo make docs-serve     # Serve docs locally at http://localhost:3000
cargo make docs-watch     # Watch and rebuild on changes
cargo make docs-clean     # Clean built documentation
cargo make docs-test      # Build and test documentation
cargo make docs-validate  # Validate documentation structure
cargo make docs-deploy    # Build, validate, and prepare for deployment
```

**Note:** mdbook tasks run at workspace level only. Requires `mdbook` to be installed:
```bash
cargo install mdbook --no-default-features --features search
```

### GitHub Pages Diagnostics & Management

**Comprehensive validation and status checking:**
```bash
cargo make gh-pages-setup-check  # Validate complete GitHub Pages setup
cargo make gh-pages-status       # Check Pages configuration via GitHub API
cargo make gh-workflow-status    # View workflow run history and status
```

**Deployment management:**
```bash
cargo make gh-pages-trigger      # Manually trigger Pages deployment workflow
cargo make gh-pages-logs         # View logs from latest deployment
cargo make gh-pages-compare      # Compare local build with deployed version
```

**Requirements:**
- `gh-pages-status`: Works with/without GitHub CLI (limited info without)
- `gh-workflow-status`, `gh-pages-trigger`, `gh-pages-logs`: Require GitHub CLI
- `gh-pages-compare`: Requires `wget` and deployed site to be live

**Install GitHub CLI:**
```bash
brew install gh
gh auth login
```

## Performance and Benchmarking

```bash
cargo make bench          # Run benchmarks
cargo make profile        # Profile the application
cargo make flamegraph     # Generate flamegraph
```

## Build and Release

```bash
cargo make release        # Full release with all checks
cargo make release-check  # Check if ready for release
cargo make package        # Create distribution packages
```

## Cross-Compilation

```bash
cargo make cross-build           # Build for all platforms
cargo make cross-macos-aarch64   # macOS ARM64
cargo make cross-macos-x86_64    # macOS x86_64
cargo make cross-linux-aarch64   # Linux ARM64
cargo make cross-linux-x86_64    # Linux x86_64
```

## Template and RDF Validation

```bash
cargo make validate-templates    # Validate all templates
cargo make validate-rdf         # Validate RDF graphs and SHACL
cargo make test-templates       # Test template generation
```

## Marketplace and Registry

```bash
cargo make marketplace-demo     # Run marketplace demo
cargo make registry-test        # Test registry functionality
```

## Development Tools

```bash
cargo make watch           # Watch and run tests on changes
cargo make watch-build     # Watch and build on changes
cargo make debug           # Run with debug logging
```

## Completions

```bash
cargo make completions     # Generate all shell completions
cargo make completions-bash # Bash completions
cargo make completions-zsh  # Zsh completions
cargo make completions-fish # Fish completions
```

## Utilities

```bash
cargo make install         # Install ggen binary
cargo make uninstall       # Uninstall ggen binary
cargo make update          # Update dependencies
cargo make tree            # Show dependency tree
cargo make size            # Show binary size
cargo make version         # Show version info
```

## Maintenance

```bash
cargo make clean-all       # Clean all artifacts and caches
cargo make fix             # Auto-fix common issues
cargo make maintenance     # Run maintenance tasks
```

## Short Aliases

For faster development, use these short aliases:

```bash
cargo make c      # check
cargo make t      # test
cargo make f      # fmt
cargo make l      # lint-allow
cargo make b      # build
cargo make r      # build-release
cargo make d      # dev
cargo make q      # quick (fmt + test-unit)
cargo make full   # ci
cargo make prep   # pre-commit
cargo make push   # pre-push
cargo make pkg    # package
cargo make cov    # test-coverage
cargo make det    # deterministic
cargo make val    # validate-outputs
cargo make slo    # slo-check
cargo make tmpl   # validate-templates
cargo make rdf    # validate-rdf
cargo make demo   # marketplace-demo
cargo make reg    # registry-test
cargo make w      # watch
cargo make wb     # watch-build
cargo make comp   # completions
cargo make cleanup # clean-all
cargo make fixup  # fix
cargo make maint  # maintenance
```

## Environment Variables

The Makefile uses several environment variables:

- `RUST_BACKTRACE=1` - Enable backtraces
- `RUST_LOG=info` - Set log level
- `RUST_TEST_THREADS=1` - Single-threaded tests for determinism
- `RNG_SEED=42` - Fixed seed for deterministic tests

## CI/CD Integration

For CI/CD pipelines, use these tasks:

```bash
# GitHub Actions / GitLab CI
cargo make ci              # Full CI workflow

# Pre-commit hooks
cargo make pre-commit      # Quick checks

# Pre-push hooks  
cargo make pre-push        # Comprehensive checks

# Release pipeline
cargo make release-check   # Validate release readiness
cargo make package         # Create packages
```

## SLO Compliance

The Makefile includes tasks to check SLO compliance:

- **Build Time**: ≤15s first build, ≤2s incremental
- **Memory Usage**: ≤100MB for generation
- **Determinism**: 100% reproducible outputs

```bash
cargo make slo-check       # Check all SLOs
cargo make slo-build-time  # Check build times
cargo make slo-memory      # Check memory usage
```

## Workspace Management

```bash
cargo make workspace-check # Check workspace configuration
cargo make workspace-deps  # Show workspace dependencies
```

## Examples

### Daily Development
```bash
# Start your day
cargo make d              # Run development workflow

# Make changes, then
cargo make q              # Quick check (fmt + test)

# Before committing
cargo make prep           # Pre-commit checks

# Before pushing
cargo make push           # Pre-push checks
```

### Release Process
```bash
# Check release readiness
cargo make release-check

# Create release
cargo make release

# Package for distribution
cargo make pkg
```

### Debugging
```bash
# Run with debug logging
cargo make debug

# Profile performance
cargo make profile

# Generate flamegraph
cargo make flamegraph
```

### Testing
```bash
# Run all tests
cargo make t

# Run with coverage
cargo make cov

# Test determinism
cargo make det

# Validate outputs
cargo make val
```

## Integration with IDEs

### VS Code
Add to your `.vscode/tasks.json`:
```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Cargo Make: Dev",
            "type": "shell",
            "command": "cargo",
            "args": ["make", "dev"],
            "group": "build",
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "shared"
            }
        }
    ]
}
```

### IntelliJ/CLion
Create a run configuration:
- Program: `cargo`
- Arguments: `make dev`
- Working directory: Project root

## Troubleshooting

### Common Issues

1. **Task not found**: Make sure you're using `cargo-make` version 0.30.0 or later
2. **Permission denied**: Some tasks may require additional permissions
3. **Missing dependencies**: Install required tools like `cross`, `tarpaulin`, etc.
4. **docs-build fails in workspace**: Make sure you run `cargo make docs-build` from the repository root, not from workspace members (utils, cli, ggen-core)
5. **mdbook not found**: Install mdbook with `cargo install mdbook --no-default-features --features search`

### GitHub Pages Deployment

The documentation is automatically deployed to GitHub Pages when changes are pushed to the `master` branch. The workflow:

1. Builds the documentation with mdbook
2. Validates the HTML output
3. Uploads to GitHub Pages
4. Deploys to https://seanchatmangpt.github.io/ggen/

To test the deployment process locally:
```bash
cargo make docs-deploy
```

### Getting Help

```bash
cargo make help           # Show all available tasks
cargo make --list-all-steps # List all tasks with descriptions
```

## Contributing

When adding new tasks:

1. Follow the existing naming conventions
2. Add appropriate descriptions
3. Include dependencies if needed
4. Add short aliases for common tasks
5. Update this documentation

## Performance Notes

- Use `cargo make q` for quick checks during development
- Use `cargo make d` for comprehensive development workflow
- Use `cargo make full` for CI/CD pipelines
- Use `cargo make det` for deterministic testing
