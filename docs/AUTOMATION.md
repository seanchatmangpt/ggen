# Automation & CI/CD

**Quick Reference**: Validation and automation workflows
**Last Updated**: 2025-12-11

---

## Quick Validation Commands

```bash
# Format + lint + tests (< 60s)
cargo make pre-commit

# Documentation validation (< 30s)
./scripts/validate-docs/validate-all.sh

# Full validation suite (< 5 min)
./scripts/run-validation-suite.sh

# SLO verification (< 10s)
cargo make slo-check
```

---

## Documentation Validation Pipeline

### validate-all.sh Master Script

**Purpose**: Comprehensive documentation quality checks

**What It Validates**:
```bash
./scripts/validate-docs/validate-all.sh

# Runs 8 validation scripts:
1. check-no-typescript.sh     # ✅ No TypeScript usage
2. check-broken-links.sh      # ✅ All internal links valid
3. validate-tutorial-*.sh     # ✅ Tutorial examples execute
4. validate-howto-*.sh        # ✅ How-to guides work
5. validate-reference-*.sh    # ✅ Reference docs accurate
6. validate-examples.sh       # ✅ Example projects build
7. validate-case-study.sh     # ✅ Diataxis case study valid
8. check-frontmatter.sh       # ✅ YAML frontmatter correct
```

**Output Format**:
```
=== Documentation Validation Suite ===

[TypeScript Detection]
✓ No TypeScript code blocks found
✓ No TypeScript interfaces/types
✓ All examples use JavaScript + JSDoc

[Link Validation]
✓ 156 internal links validated
✓ 0 broken links found

[Tutorial Validation]
✓ 01-quick-start.md: All commands executed successfully
✓ 02-first-template.md: Template generation working
✓ 03-rdf-basics.md: RDF operations verified

[Case Study Validation]
✓ Diataxis case study builds successfully
✓ All 12 documentation files valid

=== Validation Complete ===
Duration: 28.3s
Status: ALL PASSED ✅
```

---

### Individual Validation Scripts

#### TypeScript Detection (check-no-typescript.sh)

**Purpose**: Enforce JavaScript + JSDoc standard

**Patterns Detected**:
```bash
# Fails on:
- ```typescript code blocks
- interface Name { ... }
- type Alias = ...
- import type { ... }
- function foo(): Type { ... }

# Passes on:
- ```javascript code blocks
- /** @typedef {Object} Name ... */
- /** @type {Type} */
```

**How to Fix**:
```bash
# Run check
./scripts/validate-docs/check-no-typescript.sh

# Auto-fix (converts TypeScript → JSDoc)
./scripts/validate-docs/convert-typescript-to-jsdoc.sh
```

---

#### Broken Link Checker (check-broken-links.sh)

**Purpose**: Ensure all internal documentation links are valid

**What It Checks**:
- `[text](path.md)` style links
- Relative paths resolve correctly
- Referenced files exist
- Anchor links valid (#section-name)

**Example Output**:
```
Checking: docs/tutorials/01-quick-start.md
  ✓ Link to ../reference/cli/commands.md - OK
  ✓ Link to ../how-to/generation/use-templates.md - OK
  ✗ Link to ../missing-file.md - BROKEN

Checking: docs/how-to/configuration/ggen-toml-reference.md
  ✓ Link to ./common-toml-configs.md - OK
  ✓ Link to ../../reference/configuration/lifecycle-hooks.md - OK

Summary: 142 valid, 1 broken
```

**How to Fix**:
```bash
# Run check
./scripts/validate-docs/check-broken-links.sh

# Fix broken links by:
1. Updating path in documentation
2. Creating missing file
3. Removing invalid link
```

---

#### Tutorial Validation (validate-tutorial-*.sh)

**Purpose**: Ensure tutorial examples actually work

**How It Works**:
1. Extracts code blocks from tutorial markdown
2. Creates temporary directory
3. Executes commands in order
4. Verifies expected output
5. Cleans up temp files

**Example**:
```bash
# Validate quick-start tutorial
./scripts/validate-docs/validate-tutorial-01-quick-start.sh

# Steps executed:
1. ggen --version
2. ggen graph load --file schema.ttl
3. ggen graph query --sparql "..."
4. ggen generate --template class.rs.tera
5. Verify output files exist and compile
```

---

## Pre-Commit Validation (Git Hooks)

### What Runs on Every Commit

**Hook**: `.git/hooks/pre-commit` (auto-installed via `cargo make install-hooks`)

**Validations** (< 5s):
```bash
1. cargo fmt --check      # Format check
2. cargo clippy           # Lint check
3. cargo check            # Compilation check
```

**If Any Fail**: Commit is blocked until fixed

**How to Install**:
```bash
# One-time setup
cargo make install-hooks

# Verify installation
ls -la .git/hooks/pre-commit
```

**NEVER** bypass with `--no-verify` (defeats purpose)

---

### What Runs on Every Push

**Hook**: `.git/hooks/pre-push`

**Validations** (< 60s):
```bash
1. cargo make check         # Compilation
2. cargo make lint          # Clippy with all lints
3. cargo make format-check  # Format validation
4. cargo make test-unit     # Unit tests
5. cargo audit              # Security vulnerabilities
```

**If Any Fail**: Push is blocked until fixed

---

## CI/CD Pipeline (GitHub Actions)

### Pull Request Validation

**Workflow**: `.github/workflows/ci.yml`

**Runs On**: Every pull request, every push to main

**Jobs**:
```yaml
jobs:
  lint:
    - cargo fmt --check
    - cargo clippy -- -D warnings

  test:
    - cargo make test-unit
    - cargo make test-integration
    - cargo make coverage

  docs:
    - ./scripts/validate-docs/validate-all.sh
    - mdbook build docs/book

  build:
    - cargo make build
    - cargo make slo-check
```

**Duration**: ~5 minutes

**Status Checks**: All must pass before merge

---

### Documentation Deployment

**Workflow**: `.github/workflows/deploy-docs.yml`

**Runs On**: Push to main branch

**Steps**:
```yaml
1. Validate all documentation
2. Build mdBook (docs/book)
3. Deploy to GitHub Pages
```

**URL**: `https://seanchatmangpt.github.io/ggen/`

---

### Release Automation

**Workflow**: `.github/workflows/release.yml`

**Triggered By**: Git tag (e.g., `v4.0.0`)

**Steps**:
```yaml
1. Run full validation suite
2. Build release binaries (Linux, macOS, Windows)
3. Run SLO checks on release binaries
4. Create GitHub release with artifacts
5. Publish to crates.io
```

**Artifacts**:
- `ggen-v4.0.0-x86_64-linux.tar.gz`
- `ggen-v4.0.0-x86_64-darwin.tar.gz`
- `ggen-v4.0.0-x86_64-windows.zip`

---

## Lifecycle Hooks (ggen.toml)

### Pre-Generation Validation

**Configuration** (ggen.toml):
```toml
[lifecycle.phases.pre_generate]
scripts = [
    "scripts/validate-docs/validate-all.sh"
]
```

**When It Runs**: Before every `ggen generate` command

**Purpose**: Ensure documentation is valid before generating code from it

**Example**:
```bash
$ ggen generate --template docs.tera

[Lifecycle] Running pre-generation hooks...
[Lifecycle] Executing: scripts/validate-docs/validate-all.sh
✓ Documentation validation passed

[Generation] Rendering template docs.tera...
✓ Generated output.rs
```

---

### Post-Generation Formatting

**Configuration** (ggen.toml):
```toml
[lifecycle.phases.post_generate]
scripts = [
    "scripts/format-docs.sh"
]
```

**When It Runs**: After every `ggen generate` command

**Purpose**: Automatically format generated code

**Example**:
```bash
$ ggen generate --template class.rs.tera

[Generation] Rendering template class.rs.tera...
✓ Generated User.rs

[Lifecycle] Running post-generation hooks...
[Lifecycle] Executing: scripts/format-docs.sh
✓ Formatted User.rs with rustfmt
```

---

## Performance Monitoring

### SLO Verification (Service Level Objectives)

**Command**: `cargo make slo-check`

**What It Checks**:
```bash
# Build Performance
- First build ≤ 15s
- Incremental ≤ 2s
- cargo check ≤ 5s

# Runtime Performance
- RDF processing ≤ 5s (1k+ triples)
- Template rendering < 1ms
- CLI startup ≤ 50ms

# Current Results:
✓ First build: 12.3s (82% of SLO)
✓ Incremental: 0.8s (40% of SLO)
✓ cargo check: 4.1s (82% of SLO)
✓ RDF processing: 3.2s (64% of SLO)
✓ Template rendering: 0.6ms
✓ CLI startup: 38ms (76% of SLO)

Status: ALL SLOs MET ✅
```

**See**: `docs/PERFORMANCE.md` for detailed benchmarks

---

### Continuous Benchmarking

**Workflow**: `.github/workflows/benchmark.yml`

**Runs On**: Every push to main

**Tracks**:
- Build times (first, incremental, check)
- RDF query performance
- Template rendering speed
- CLI startup time
- Memory usage

**Storage**: Results stored in `docs/benchmark-results/`

**Visualization**: `scripts/benchmark-visualize.sh` generates charts

---

## Environment-Specific Automation

### Development Environment

**Configuration** (ggen.toml):
```toml
[env.development]
"ai.model" = "claude-3-haiku-20240307"  # Faster, cheaper
"logging.level" = "debug"               # Verbose logging
"performance.max_workers" = 4           # Don't overwhelm laptop
```

**Automation**:
- Fast feedback (< 2s check times)
- Verbose debugging
- Local AI provider (Ollama)

---

### CI Environment

**Configuration** (ggen.toml):
```toml
[env.ci]
"ai.provider" = "ollama"        # No API costs
"ai.model" = "llama2"
"ai.base_url" = "http://localhost:11434"
"logging.level" = "warn"        # Less noise
"logging.format" = "json"       # Structured logs
```

**Automation**:
- All validation runs automatically
- Parallel test execution
- Coverage reporting
- Status checks block merge

---

### Production Environment

**Configuration** (ggen.toml):
```toml
[env.production]
"ai.temperature" = 0.3          # Deterministic
"logging.level" = "warn"
"logging.format" = "json"
"performance.max_workers" = 16  # Max parallelism
"sparql.cache_ttl" = 86400      # 24 hour cache
```

**Automation**:
- Stricter validation
- Performance benchmarking
- Security scanning
- Release artifacts

---

## Infrastructure Scripts

### Development Setup

**Script**: `scripts/dev-setup.sh`

**What It Does**:
```bash
1. Check Rust installation (rustc, cargo)
2. Install cargo-make
3. Install cargo-audit
4. Install cargo-tarpaulin (coverage)
5. Install git hooks
6. Create .ggen/ directory structure
7. Download example ontologies
8. Run initial validation
```

**Usage**:
```bash
# One-time setup for new contributors
./scripts/dev-setup.sh

# Output:
✓ Rust 1.74.0 installed
✓ cargo-make installed
✓ Git hooks installed
✓ Development environment ready
```

---

### Clean Script

**Script**: `scripts/clean.sh`

**What It Cleans**:
```bash
1. cargo clean
2. Remove target/ directory
3. Remove .ggen/cache/
4. Remove .ggen/rdf-store/
5. Remove generated files
6. Remove test artifacts
```

**Usage**:
```bash
# Clean everything
./scripts/clean.sh

# Clean only cache
./scripts/clean.sh --cache-only
```

---

### Validation Suite Runner

**Script**: `scripts/run-validation-suite.sh`

**What It Runs**:
```bash
1. Format check
2. Lint check
3. Type check
4. Unit tests
5. Integration tests
6. Documentation validation
7. Security audit
8. SLO verification
9. Coverage report generation
```

**Usage**:
```bash
# Run full suite (5-10 minutes)
./scripts/run-validation-suite.sh

# Run with coverage
./scripts/run-validation-suite.sh --coverage

# Output:
=== Validation Suite ===
✓ Format check (1.2s)
✓ Lint check (3.4s)
✓ Type check (4.1s)
✓ Unit tests (18.7s)
✓ Integration tests (42.3s)
✓ Documentation (28.1s)
✓ Security audit (2.1s)
✓ SLO checks (5.3s)
✓ Coverage: 82.4%

Duration: 105.2s
Status: ALL PASSED ✅
```

---

## Key Takeaways

**Focus on these automated workflows**:

1. ✅ **Pre-commit hooks**: Fast feedback (< 5s)
2. ✅ **Documentation validation**: Comprehensive checks
3. ✅ **CI pipeline**: All validation on every PR
4. ✅ **Lifecycle hooks**: Validate before/after generation
5. ✅ **SLO monitoring**: Continuous performance tracking
6. ✅ **Environment configs**: Optimized per environment

---

## Detailed Automation Documentation

This is a **quick reference**. For detailed documentation, see:

- **Validation Guide**: `docs/contributing/VALIDATION.md`
  - Complete validation script reference
  - Debugging validation failures
  - Writing custom validators

- **CI/CD Documentation**: `docs/architecture/ci-cd.md`
  - GitHub Actions workflow details
  - Release process
  - Deployment automation

- **Lifecycle Hooks**: `docs/reference/configuration/lifecycle-hooks.md`
  - Hook configuration reference
  - Writing custom hooks
  - Hook execution order

---

**Next Steps**:
- Setting up development? → `scripts/dev-setup.sh`
- Running validation? → `./scripts/validate-docs/validate-all.sh`
- Contributing? → `docs/contributing/GETTING_STARTED.md`
