# Contributing to ggen

Thank you for your interest in contributing to ggen! This guide will help you get started and ensure your contributions align with the project's standards and practices.

## Table of Contents

- [Quick Start](#quick-start)
- [Development Setup](#development-setup)
- [Development Workflow](#development-workflow)
- [Testing Requirements](#testing-requirements)
- [Code Style Guidelines](#code-style-guidelines)
- [OpenTelemetry Validation](#opentelemetry-validation)
- [Pull Request Process](#pull-request-process)
- [Commit Conventions](#commit-conventions)
- [Quality Gates](#quality-gates)
- [Getting Help](#getting-help)

## Quick Start

```bash
# 1. Fork and clone the repository
git clone https://github.com/YOUR_USERNAME/ggen.git
cd ggen

# 2. Install Rust toolchain (1.91.1+)
rustup install 1.91.1
rustup default 1.91.1

# 3. Install cargo-make
cargo install cargo-make

# 4. Verify development environment
cargo make check      # Compilation check
cargo make test       # Run tests
cargo make lint       # Check style
```

## Development Setup

### Prerequisites

- **Rust**: Version 1.91.1 or later ([install](https://rustup.rs/))
- **Cargo make**: `cargo install cargo-make`
- **timeout command**: Required for all operations
  - macOS: `brew install coreutils`
  - Ubuntu/Debian: `sudo apt-get install coreutils`

### Environment Setup

```bash
# Install cargo-make if not already installed
cargo install cargo-make

# Verify installation
cargo make --version

# Clone your fork and add upstream remote
git clone https://github.com/YOUR_USERNAME/ggen.git
cd ggen
git remote add upstream https://github.com/seanchatmangpt/ggen.git
```

### Project Structure

```
ggen/
├── crates/           # 30+ workspace crates
│   ├── ggen-cli/     # Main CLI binary
│   ├── ggen-core/    # Core pipeline logic
│   ├── ggen-a2a-mcp/ # MCP server integration
│   └── ...
├── .specify/         # RDF specifications (source of truth)
├── docs/             # Documentation
├── tests/            # Integration tests
├── Makefile.toml     # Cargo-make tasks
└── CLAUDE.md         # Project constitution
```

## Development Workflow

ggen follows a strict **4-step development workflow**:

### 1. Create RDF Specification (if applicable)

If your feature requires new domain concepts:

```bash
# Create specification directory
mkdir -p .specify/specs/NNN-feature

# Edit Turtle ontology (SOURCE OF TRUTH)
vim .specify/specs/NNN-feature/feature.ttl

# Validate RDF syntax
ggen validate .specify/specs/NNN-feature/feature.ttl

# Generate documentation (optional)
cargo make speckit-render
```

**CRITICAL**: RDF `.ttl` files are the source of truth. Never edit generated `.md` files.

### 2. Chicago TDD (Test-Driven Development)

This project uses **Chicago TDD exclusively**. See [Testing Requirements](#testing-requirements) for details.

```bash
# Write failing test (RED)
vim crates/ggen-core/tests/feature_test.rs

# Verify test fails
cargo make test-unit

# Implement feature (GREEN)
vim crates/ggen-core/src/feature.rs

# Verify test passes
cargo make test-unit

# Refactor while maintaining GREEN
cargo make pre-commit
```

### 3. Generate from Ontology (if applicable)

```bash
# Preview changes
ggen sync --dry_run true

# Full sync with cryptographic audit
ggen sync --audit true
```

### 4. Commit with Evidence

```bash
# Run all quality gates
cargo make pre-commit

# Commit with receipts
git commit -m "feat(NNN): Implement feature

[Receipt] cargo make pre-commit: ✓ 3/3 gates
[Receipt] cargo make test: ✓ 347/347 tests"
```

## Testing Requirements

### Chicago TDD (MANDATORY)

This project uses **Chicago TDD exclusively**. London TDD patterns (mocks, test doubles, behavior verification) are **FORBIDDEN**.

#### What is Chicago TDD?

- **State-based verification**: Assert on actual results, not mock interactions
- **Real collaborators**: Use real databases, filesystems, HTTP clients, LLM APIs
- **Real execution**: Tests make real API calls, real I/O, real concurrent operations
- **Empirical observation**: Verify observable system behavior

#### Forbidden Patterns

**❌ DO NOT USE:**
- `mockall` crate or any mocking framework
- Test doubles (`InMemoryStorage`, `FakeDatabase`, etc.)
- Behavior verification (`.expect_x().times(1)`)
- Dependency injection for testability

**✅ USE INSTEAD:**
- Real HTTP clients (`reqwest::Client`)
- Real databases (`SqlitePool :memory:`, PostgreSQL via testcontainers)
- Real filesystem (`tempfile::TempDir` for real I/O)
- Real LLM API calls (with OTEL verification)

#### Example: Chicago TDD

```rust
// ACCEPTABLE: Real HTTP client
let client = reqwest::Client::new();
let response = client.get("https://example.com").await?;
assert_eq!(response.status(), 200);

// ACCEPTABLE: Real database
let pool = SqlitePool::connect(":memory:").await?;
sqlx::query("INSERT INTO users (name) VALUES (?)")
    .bind("Alice")
    .execute(&pool)
    .await?;

// ACCEPTABLE: Real filesystem
let temp_dir = TempDir::new()?;
std::fs::write(temp_dir.path().join("test.txt"), "content")?;
assert!(temp_dir.path().join("test.txt").exists());
```

#### Test Coverage Requirements

- **Minimum coverage**: 80%+
- **Public APIs**: 100% tested
- **Error paths**: All error cases must be tested
- **Edge cases**: Boundary conditions, concurrency, resource cleanup

#### Test Commands

```bash
cargo make test-unit     # Fast unit tests (<16s)
cargo make test          # Full test suite (<30s)
cargo make test-mutation # Mutation testing (≥60% score)
```

## Code Style Guidelines

### Rust Best Practices

Follow the **Elite Rust Mindset**:

- **Type-first design**: Encode invariants in types, use compiler as design tool
- **Zero-cost abstractions**: Prefer generics over trait objects
- **Performance**: References > owned, stack > heap, minimize allocations
- **Memory safety**: Ownership explicit, lifetimes prevent use-after-free
- **API design**: Type-safe, ergonomic, `Result<T,E>` not panics

### Formatting and Linting

```bash
# Format code
cargo make fmt

# Run clippy (strict settings)
cargo make lint

# Full pre-commit check
cargo make pre-commit
```

### Code Organization Rules

- **NEVER save files to root**: Use appropriate subdirectories
- **RDF is truth**: Edit `.specify/*.ttl`, never generated `.md`
- **No unwrap()**: Zero `unwrap()/expect()` in production code
- **Result<T,E> required**: All fallible operations return Result

### Absolute Rules

| Rule | Requirement |
|------|-------------|
| **Concurrent Operations** | ALL operations MUST be parallel in ONE message |
| **No Root Files** | NEVER save files to root - use subdirectories |
| **Cargo Make Only** | ALWAYS `cargo make` - NEVER direct cargo commands |
| **Andon Protocol** | STOP THE LINE when signals appear - fix before proceeding |

### File Organization

**CRITICAL** - Files must be in correct directories:

**MUST be in root**: `Cargo.toml`, `Makefile.toml`, `README.md`, `CHANGELOG.md`, `CONTRIBUTING.md`, `LICENSE`, `SECURITY.md`, `CLAUDE.md`

**MUST NOT be in root**:
- Test files (`.rs`) → `tests/`
- Template files (`.tera`) → `templates/`
- Data files (`.ttl`, `.rdf`) → `.specify/` or `examples/`
- Scripts (`.sh`) → `scripts/`
- Temporary files → Delete immediately

## OpenTelemetry Validation

### When is OTEL Validation Required?

**MANDATORY** for any feature involving:
- LLM calls (Groq, OpenAI, Anthropic, etc.)
- MCP tool execution
- External API calls (REST, GraphQL, RPC)
- Database operations (PostgreSQL, Redis, etc.)
- Pipeline stages (μ₁-μ₅ code generation)

### Required OTEL Spans

#### LLM Integration

**Required Spans:**
- `llm.complete` - Synchronous LLM completion
- `llm.complete_stream` - Streaming LLM completion

**Required Attributes:**
- `llm.model` - Model identifier (e.g., `groq::openai/gpt-oss-20b`)
- `llm.prompt_tokens` - Input token count
- `llm.completion_tokens` - Output token count
- `llm.total_tokens` - Total tokens used

#### MCP Tools

**Required Spans:**
- `mcp.tool.call` - Tool invocation
- `mcp.tool.response` - Tool response

**Required Attributes:**
- `mcp.tool.name` - Tool name
- `mcp.tool.duration_ms` - Execution time
- `mcp.tool.result` - Success/failure status

### How to Verify OTEL Spans

```bash
# 1. Enable trace logging
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace

# 2. Run tests with output capture
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt

# 3. Check for required spans
grep -E "llm\.complete|llm\.model|llm\.total_tokens" otel_output.txt
```

### Golden Rule

**Tests passing is NOT sufficient.** For LLM/external service features, you MUST verify OTEL spans/traces exist.

**Without OTEL evidence, the feature is NOT complete.**

## Pull Request Process

### Before Creating a PR

1. **Run all quality gates**:
   ```bash
   cargo make pre-commit
   ```

2. **Verify OTEL spans** (if applicable):
   ```bash
   RUST_LOG=trace,ggen_ai=trace cargo test <test_name> 2>&1 | grep -E "llm\.|mcp\."
   ```

3. **Update documentation** (if needed):
   - Update README.md for user-facing changes
   - Update relevant docs in `docs/`
   - Add examples for new features

### Creating a PR

1. **Branch naming**: Use `feat/*`, `fix/*`, `refactor/*` prefixes
   ```bash
   git checkout -b feat/your-feature-name
   ```

2. **Commit messages**: Follow [Commit Conventions](#commit-conventions)

3. **PR title**: Use conventional commit format
   ```
   feat: Add support for XYZ
   fix: Resolve ABC issue
   refactor: Improve DEF performance
   ```

4. **PR description template**:
   ```markdown
   ## Summary
   Brief description of changes (1-2 sentences)

   ## Changes
   - Change 1
   - Change 2
   - Change 3

   ## Testing
   - [ ] Unit tests pass
   - [ ] Integration tests pass
   - [ ] OTEL spans verified (if applicable)

   ## Documentation
   - [ ] README updated (if needed)
   - [ ] Examples added (if needed)

   ## Quality Gates
   [Receipt] cargo make pre-commit: ✓ 3/3 gates
   [Receipt] cargo make test: ✓ 347/347 tests
   ```

### PR Review Process

1. **Automated checks**: CI will run quality gates
2. **Code review**: Maintainers will review your changes
3. **OTEL validation**: For LLM/external service features, include OTEL span output
4. **Address feedback**: Make requested changes
5. **Approval and merge**: Once approved, PR will be merged

## Commit Conventions

### Conventional Commits

Follow [Conventional Commits](https://www.conventionalcommits.org/) format:

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

### Type Values

- `feat`: New feature
- `fix`: Bug fix
- `refactor`: Code refactoring (no functional change)
- `test`: Adding or updating tests
- `docs`: Documentation changes
- `chore`: Maintenance tasks
- `perf`: Performance improvements
- `ci`: CI/CD changes

### Scope Values

Use crate name or feature area:
- `cli`: CLI changes
- `core`: Core pipeline changes
- `a2a-mcp`: MCP server changes
- `ai`: LLM integration changes
- `tps`: Test and production system changes

### Examples

```
feat(cli): Add wizard command for interactive project setup

Implement new ggen wizard command with profile selection and
non-interactive mode support.

[Receipt] cargo make pre-commit: ✓ 3/3 gates
[Receipt] cargo make test: ✓ 347/347 tests
```

```
fix(core): Resolve SPARQL query parsing issue with UTF-8 characters

Fix bug where SPARQL queries with non-ASCII characters would fail
to parse correctly. Updated Oxigraph integration to handle UTF-8
properly.

Fixes #123
```

```
refactor(a2a): Improve task state machine performance

Optimize state transition validation to reduce overhead in
high-throughput scenarios. Performance improved by ~40%.

[Receipt] cargo make bench: 40% improvement
```

## Quality Gates

### Pre-Commit Checklist

Before committing or creating a PR, run:

```bash
cargo make pre-commit
```

This runs:
1. **check**: Compilation check (<5s)
2. **lint**: Clippy + rustfmt (<60s)
3. **test-unit**: Fast unit tests (<16s)

### Full Validation

For complete validation:

```bash
# All tests
cargo make test           # Full test suite (<30s)

# Mutation testing
cargo make test-mutation  # ≥60% score required

# Performance SLOs
cargo make slo-check      # Verify performance targets

# Security audit
cargo make audit          # Check for vulnerabilities
```

### Andon Signals (Stop the Line)

| Level | Pattern | Action |
|-------|---------|--------|
| 🔴 **CRITICAL** | `error[E...]` | HALT - Compiler errors |
| 🔴 **CRITICAL** | `test ... FAILED` | HALT - Test failures |
| 🟡 **HIGH** | `warning:` | STOP before release |
| 🟡 **HIGH** | Clippy errors | STOP before release |
| 🟢 **GREEN** | All checks pass | Proceed |

**When you see a signal:**
1. STOP work immediately
2. Investigate root cause (5 Whys)
3. Fix root cause, not symptom
4. Re-run checks until cleared

### Definition of Done

A feature is **ONLY** complete when:

- ✅ All tests pass (unit + integration)
- ✅ OTEL spans exist (for LLM/external services)
- ✅ Code compiles without warnings
- ✅ Clippy passes with strict settings
- ✅ Code formatted with rustfmt
- ✅ Coverage ≥80%
- ✅ Performance SLOs met
- ✅ Documentation updated

## Getting Help

### Resources

- **Documentation**: [README.md](README.md) - Project overview and quick start
- **Project Rules**: [CLAUDE.md](CLAUDE.md) - Constitutional rules and standards
- **Architecture**: [docs/](docs/) - Detailed architecture documentation
- **GitHub Issues**: [Report bugs or request features](https://github.com/seanchatmangpt/ggen/issues)
- **Discussions**: [Ask questions and discuss ideas](https://github.com/seanchatmangpt/ggen/discussions)

### Asking Questions

1. **Search first**: Check existing issues and discussions
2. **Be specific**: Include code examples, error messages, and context
3. **Provide evidence**: For bugs, include OTEL spans, test output, or reproduction steps
4. **Use discussions**: For questions, use GitHub Discussions (not issues)

### Reporting Bugs

When reporting bugs, include:

- **Rust version**: `rustc --version`
- **ggen version**: `ggen --version`
- **Reproduction steps**: Minimal example to reproduce the issue
- **Expected behavior**: What you expected to happen
- **Actual behavior**: What actually happened (with error messages)
- **OTEL output**: If applicable, include OTEL span output

### Feature Requests

When requesting features:

- **Use case**: Describe the problem you're trying to solve
- **Proposed solution**: How you envision the feature working
- **Alternatives**: Other approaches you've considered
- **Context**: Why this feature is important to you

### Finding Good First Issues

Look for issues labeled:
- `good first issue` - Perfect for newcomers
- `help wanted` - We need help with these
- `documentation` - Documentation improvements

**New to Rust?** Start with:
- Documentation improvements
- Test coverage improvements
- Example code additions

**Experienced?** Try:
- Performance optimizations
- New AI providers
- Advanced features

## Code of Conduct

- Be respectful and inclusive
- Provide constructive feedback
- Focus on what is best for the community
- Show empathy towards other community members

## License

By contributing to ggen, you agree that your contributions will be licensed under the same license as the project (Apache 2.0 OR MIT).

## Recognition

Contributors are automatically added to:
- [README Contributors Section](README.md#contributors) (via All Contributors Bot)
- [CONTRIBUTORS.md](CONTRIBUTORS.md) (manual updates)
- Git commit history

**Notable contributions** may receive:
- Shout-outs in release notes
- Special recognition in community channels

---

**Thank you for contributing to ggen!** 🚀

For more details on project philosophy and advanced workflows, see [CLAUDE.md](CLAUDE.md).

**Ready to contribute?**

1. Fork the repo
2. Find a [good first issue](https://github.com/seanchatmangpt/ggen/labels/good%20first%20issue)
3. Make your changes
4. Submit a PR

**Happy coding!** 🎉
