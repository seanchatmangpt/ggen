# Developer Onboarding Guide - ggen v2.6.0
**For**: New contributors and developers
**Goal**: Get from "git clone" to "first PR merged" in < 30 minutes
**Version**: 2.6.0 (Updated 2025-11-13)

---

## Quick Start (5 Minutes)

### Step 1: Clone and Setup (2 minutes)

```bash
# Clone the repository
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen

# Verify Rust is installed (requires 1.70+)
rustc --version  # Should show 1.70.0 or higher

# If Rust not installed:
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env

# Install cargo-make (build tool)
cargo install cargo-make
```

### Step 2: Build and Validate (3 minutes)

```bash
# Quick build check (should complete in < 30s)
cargo make check

# Build release binary (optional, takes 2-3 minutes)
cargo make build-release

# Verify ggen works
./target/debug/ggen --version
# Expected output: ggen 2.6.0

# See available commands
./target/debug/ggen --help
```

**Expected Output**:
```
Commands:
  project      Watch for changes and auto-regenerate
  template     Generate CLI project from RDF/TTL file
  marketplace  Publish a package to the marketplace
  ai           Analyze code with AI insights
  utils        Manage environment variables
  graph        Visualize graph structure
  hook         Monitor hook events
  help         Print this message or the help of the given subcommand(s)
```

### Step 3: Run Development Workflow (< 1 minute)

```bash
# Format + lint + test (development workflow)
cargo make dev

# Or run individual steps:
cargo make fmt       # Format code
cargo make lint      # Run clippy
cargo make test      # Run tests
```

**Success Criteria**: All commands complete without errors.

---

## Understanding the Project (10 Minutes)

### What is ggen?

ggen is a **knowledge graph-driven code generator** where RDF ontologies are the single source of truth. Change the ontology → code automatically updates across all languages.

**Key Insight**: ggen treats code as a *projection* of knowledge graphs, not templates.

### Architecture Overview

```
ggen/
├── crates/
│   ├── ggen-cli/          # Command-line interface (32 commands)
│   ├── ggen-core/         # Core generation logic (template engine, RDF processing)
│   ├── ggen-ai/           # AI integration (GPT-4o, Claude, Ollama)
│   ├── ggen-domain/       # Domain layer (marketplace, hooks, utils)
│   ├── ggen-marketplace/  # Template marketplace (local backend)
│   └── ggen-utils/        # Shared utilities
├── tests/                 # Integration tests (Chicago TDD, E2E)
├── scripts/               # Build and validation scripts
├── docs/                  # Documentation (FMEA, validation reports)
└── Makefile.toml         # Build tasks (cargo-make)
```

### Key Technologies

- **Language**: Rust 1.90.0
- **RDF**: Oxigraph (in-memory triple store)
- **SPARQL**: 1.1 query language for RDF
- **Templates**: Tera (Jinja2-like syntax)
- **AI**: OpenAI/Anthropic/Ollama APIs
- **Testing**: Chicago TDD (real systems, no mocks)

### Important Files

| File | Purpose |
|------|---------|
| `Makefile.toml` | Build tasks (format, lint, test, release) |
| `CONTRIBUTING.md` | Contribution guidelines and standards |
| `Cargo.toml` | Workspace dependencies and versions |
| `VERSION` | Current version (single source of truth) |
| `CHANGELOG.md` | Release history and changes |
| `docs/FMEA_RELEASE_V2.6.0.md` | Release validation framework |

---

## Development Workflow (15 Minutes)

### Making Your First Change

**Example**: Add a new utility command to `ggen utils`

#### Step 1: Create a Branch

```bash
git checkout -b feature/add-environment-info
```

#### Step 2: Make Changes

Edit `crates/ggen-cli/src/cmds/utils.rs`:

```rust
// Add a new subcommand to UtilsCommand enum
#[derive(Debug, Subcommand)]
pub enum UtilsCommand {
    // ... existing commands ...

    /// Show environment information
    EnvInfo,
}

// Add handler in execute() function
pub fn execute(&self, runtime: &Runtime) -> Result<()> {
    match self {
        // ... existing handlers ...

        UtilsCommand::EnvInfo => {
            println!("Rust version: {}", std::env!("CARGO_PKG_RUST_VERSION"));
            println!("ggen version: {}", env!("CARGO_PKG_VERSION"));
            println!("OS: {}", std::env::consts::OS);
            Ok(())
        }
    }
}
```

#### Step 3: Test Locally

```bash
# Build and test
cargo make dev

# Try the new command
./target/debug/ggen utils env-info
```

**Expected Output**:
```
Rust version: 1.90.0
ggen version: 2.6.0
OS: macos
```

#### Step 4: Write Tests

Create test in `crates/ggen-cli/src/cmds/utils.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env_info_command() {
        let cmd = UtilsCommand::EnvInfo;
        let runtime = Runtime::new();
        let result = cmd.execute(&runtime);

        assert!(result.is_ok(), "EnvInfo command should succeed");
    }
}
```

Run tests:
```bash
cargo test --package ggen-cli -- utils::tests::test_env_info_command
```

#### Step 5: Commit and Push

```bash
# Format and lint
cargo make fmt
cargo make lint

# Commit with conventional commit message
git add crates/ggen-cli/src/cmds/utils.rs
git commit -m "feat(cli): add utils env-info command

Shows Rust version, ggen version, and OS information.

Closes #123"

# Push to your fork
git push origin feature/add-environment-info
```

#### Step 6: Create Pull Request

1. Go to GitHub: https://github.com/seanchatmangpt/ggen/compare
2. Select your branch
3. Fill in PR template:
   - **Title**: `feat(cli): add utils env-info command`
   - **Description**: Describe what the command does and why it's useful
   - **Testing**: How you tested the change
   - **Related Issues**: Link to issue #123

### Common Development Tasks

#### Running Tests

```bash
# All tests (may take 5+ minutes)
cargo make test

# Unit tests only (faster)
cargo make test-unit

# Integration tests
cargo test --test integration_tests

# Specific test
cargo test test_template_rendering

# With logging
RUST_LOG=debug cargo test test_name
```

#### Checking Code Quality

```bash
# Format code (auto-fix)
cargo make fmt

# Lint code (show warnings)
cargo make lint

# Check compilation (fast)
cargo make check

# Full CI validation
cargo make ci
```

#### Working with Git Hooks

**Pre-commit hooks** run automatically on `git commit`:
- Code formatting (cargo fmt)
- Linting (clippy)
- ⚠️ **Note**: Currently blocks TODO/FUTURE comments (see Known Issues below)

**Pre-push hooks** run automatically on `git push`:
- All tests must pass
- Security audit
- ⚠️ **Note**: Currently blocks unwrap() in production code (30s timeout)

**Bypass hooks** (not recommended):
```bash
git commit --no-verify -m "message"
git push --no-verify
```

#### Debugging

```bash
# Run with debug output
RUST_LOG=debug cargo run --bin ggen -- --help

# Debug specific module
RUST_LOG=ggen_core=trace cargo test

# Debug with backtrace
RUST_BACKTRACE=1 cargo test test_name

# Debug with lldb (macOS) or gdb (Linux)
lldb ./target/debug/ggen
(lldb) run --help
```

---

## Common Workflows

### Adding a New CLI Command

**Steps**:
1. Add subcommand to enum in `crates/ggen-cli/src/cmds/<module>.rs`
2. Implement handler in `execute()` function
3. Add tests in `#[cfg(test)] mod tests`
4. Update help documentation (automatic via clap)
5. Run `cargo make dev` to validate

**Example**: See "Making Your First Change" above

### Adding a New Template

**Steps**:
1. Create template directory: `marketplace/templates/<template-name>/`
2. Add template files with Tera syntax: `{{variable}}`
3. Add metadata: `marketplace/templates/<template-name>/ggen.yaml`
4. Test generation: `ggen template gen test-output --template <template-name>`

**Example**:
```bash
# Create template
mkdir -p marketplace/templates/my-template
cat > marketplace/templates/my-template/main.rs.tera << 'EOF'
fn main() {
    println!("Hello, {{name}}!");
}
EOF

cat > marketplace/templates/my-template/ggen.yaml << 'EOF'
name: my-template
version: 1.0.0
description: Simple Rust hello world
variables:
  name:
    type: string
    default: World
EOF

# Test template
ggen template gen test-output --template my-template
```

### Adding RDF/SPARQL Features

**Steps**:
1. Add RDF processing logic to `crates/ggen-core/src/graph/`
2. Add SPARQL queries to `crates/ggen-core/src/sparql/`
3. Add integration tests with real Oxigraph store
4. Validate with Chicago TDD approach (no mocks)

**Example**:
```rust
use oxigraph::store::Store;
use oxigraph::model::*;

#[test]
fn test_sparql_query() {
    // Create in-memory store
    let store = Store::new().unwrap();

    // Add triples
    let subject = NamedNode::new("http://example.com/Product1").unwrap();
    let predicate = NamedNode::new("http://schema.org/name").unwrap();
    let object = Literal::new_simple_literal("Widget");
    store.insert(&Quad::new(subject, predicate, object, None)).unwrap();

    // Query
    let query = "SELECT ?name WHERE { ?s <http://schema.org/name> ?name }";
    let results = store.query(query).unwrap();

    // Validate
    assert_eq!(results.len(), 1);
}
```

### Adding AI Features

**Steps**:
1. Add AI provider logic to `crates/ggen-ai/src/providers/`
2. Implement `AIProvider` trait
3. Add configuration to `crates/ggen-ai/src/config.rs`
4. Add CLI command to `crates/ggen-cli/src/cmds/ai.rs`

**Example**: See `crates/ggen-ai/src/providers/openai.rs` for reference

---

## Troubleshooting

### Build Issues

#### "Package not found" errors

```bash
# Update Cargo index
cargo update

# Clean and rebuild
cargo clean
cargo make build-release
```

#### Slow compilation

```bash
# Use sccache (caching compiler)
cargo install sccache
export RUSTC_WRAPPER=sccache

# Or use mold linker (faster linking on Linux)
sudo apt install mold
export RUSTFLAGS="-C link-arg=-fuse-ld=mold"
```

### Test Issues

#### Tests timeout

```bash
# Run with longer timeout
cargo test -- --test-threads=1

# Or run only unit tests
cargo make test-unit
```

#### Tests fail with "lock contention"

```bash
# Pre-push hook has 30s timeout to handle this
# If still timing out, run tests directly:
cargo test --no-fail-fast
```

### Git Hook Issues

#### Pre-commit hook rejects TODO comments

**Workaround** (temporary):
```bash
git commit --no-verify -m "message"
```

**Proper Fix**: See [Known Issues](#known-issues) below

#### Pre-push hook rejects unwrap()

**Check if unwrap() is in test code**:
```bash
# Test code unwrap() is usually OK
grep -r "\.unwrap()" crates/*/src/ | grep -v "/tests/"
```

**Fix**: Replace unwrap() with proper error handling in production code

### Runtime Issues

#### "CLI execution failed: Argument parsing failed"

**Cause**: Command not found or wrong syntax

**Fix**: Check `ggen --help` for correct syntax

**Example**:
```bash
# Wrong (doctor command doesn't exist yet)
ggen doctor

# Right (use utils for diagnostics)
ggen utils env
```

---

## Known Issues

### Issue 1: Missing `ggen doctor` Command

**Status**: ⚠️ Documented but not implemented

**Impact**: Developers follow README → command fails

**Workaround**: Use `cargo make check` to validate environment

**Expected Fix**: v2.6.1 or documentation update

### Issue 2: Git Hooks Too Strict

**Status**: ⚠️ Blocks legitimate development work

**Impact**: Cannot commit with TODO/FUTURE comments

**Workaround**: Use `--no-verify` flag (not recommended)

**Expected Fix**: Relax hook rules in v2.6.1

### Issue 3: Unit Tests Timeout

**Status**: ⚠️ Tests take > 60s (compilation-heavy)

**Impact**: Slow development workflow

**Workaround**: Run specific tests instead of full suite

**Expected Fix**: Split unit vs integration tests in v2.6.1

---

## Getting Help

### Documentation

- **Main README**: [README.md](../README.md) - Overview and quick start
- **Contributing Guide**: [CONTRIBUTING.md](../CONTRIBUTING.md) - Detailed contribution guidelines
- **Architecture**: [CLAUDE.md](../CLAUDE.md) - SPARC development environment
- **Testing Guide**: [docs/TESTING_AND_QUALITY_ASSURANCE.md](TESTING_AND_QUALITY_ASSURANCE.md)
- **Release Process**: [docs/FMEA_RELEASE_V2.6.0.md](FMEA_RELEASE_V2.6.0.md)

### Community

- **GitHub Issues**: [github.com/seanchatmangpt/ggen/issues](https://github.com/seanchatmangpt/ggen/issues)
- **Discussions**: [github.com/seanchatmangpt/ggen/discussions](https://github.com/seanchatmangpt/ggen/discussions)
- **Pull Requests**: [github.com/seanchatmangpt/ggen/pulls](https://github.com/seanchatmangpt/ggen/pulls)

### Asking Questions

**Good Question Format**:
```
**Environment**:
- OS: macOS 14.0
- Rust: 1.90.0
- ggen: 2.6.0

**What I'm trying to do**:
Add a new AI provider for Anthropic Claude

**What I've tried**:
1. Created new file in crates/ggen-ai/src/providers/anthropic.rs
2. Implemented AIProvider trait
3. Added to providers/mod.rs

**Error**:
```
error[E0277]: the trait bound `AnthropicProvider: AIProvider` is not satisfied
```

**Question**:
How do I properly implement async methods for the AIProvider trait?
```

---

## Next Steps

### After Your First PR Merges

1. **Explore Advanced Features**
   - Add RDF/SPARQL integration
   - Implement AI features
   - Create new templates

2. **Improve Quality**
   - Add test coverage
   - Write documentation
   - Fix technical debt

3. **Join the Community**
   - Help other contributors
   - Review pull requests
   - Propose new features

### Becoming a Regular Contributor

**Benefits**:
- Recognition in CONTRIBUTORS.md
- Shout-outs in release notes
- Deep understanding of knowledge graph-driven development

**Expectations**:
- Follow code style guidelines
- Write tests for new features
- Maintain backward compatibility
- Help review other PRs

---

## Appendix: Useful Commands

### Cargo Make Tasks

```bash
# Development
cargo make dev              # Format + lint + test
cargo make quick            # Format + test (skip lint)
cargo make watch            # Auto-run on file changes

# Building
cargo make build            # Debug build
cargo make build-release    # Release build
cargo make check            # Fast compilation check

# Testing
cargo make test             # All tests
cargo make test-unit        # Unit tests only
cargo make test-integration # Integration tests only
cargo make test-e2e         # End-to-end tests

# Quality
cargo make fmt              # Format code
cargo make lint             # Clippy linting
cargo make audit            # Security audit
cargo make ci               # Full CI pipeline

# Release
cargo make release-validate # Run all release checks
cargo make release          # Create release
```

### Git Workflows

```bash
# Start new feature
git checkout -b feature/my-feature
git add .
git commit -m "feat: description"
git push origin feature/my-feature

# Update from master
git checkout master
git pull upstream master
git checkout feature/my-feature
git rebase master

# Fix merge conflicts
git add <resolved-files>
git rebase --continue
git push origin feature/my-feature --force-with-lease
```

### Debugging Commands

```bash
# Trace execution
RUST_LOG=trace cargo run --bin ggen -- <command>

# Debug specific module
RUST_LOG=ggen_core::graph=debug cargo test

# Run with debugger
lldb ./target/debug/ggen
(lldb) run <command>
(lldb) bt  # backtrace
```

---

**Onboarding Guide Version**: 2.6.0
**Last Updated**: 2025-11-13
**Feedback**: Create an issue or discussion on GitHub

**Welcome to ggen development!** 🎉
