# Lifecycle Team Workflow Guide

**Daily driver guide for ggen core team developers.**

This guide covers the practical, day-to-day use of the lifecycle system. Optimized for maintainability, team collaboration, and reproducibility.

---

## Table of Contents

- [Daily Development Workflow](#daily-development-workflow)
- [Best Practices from the Codebase](#best-practices-from-the-codebase)
- [Team Collaboration Patterns](#team-collaboration-patterns)
- [Real Examples from the Codebase](#real-examples-from-the-codebase)
- [Debugging and Troubleshooting](#debugging-and-troubleshooting)
- [CI/CD Integration](#cicd-integration)

---

## Daily Development Workflow

### 1. Starting a New Feature

```bash
# 1. Check current project state
ggen lifecycle list              # See available phases
ggen state show                  # Check what's been run
ggen state history               # Review execution history

# 2. Initialize feature branch
git checkout -b feature/my-feature

# 3. Run setup phase to ensure dependencies
ggen setup                       # Cached if already run

# 4. Start development
ggen dev                         # Starts dev server with hot reload
```

**State Tracking**: Every phase execution is recorded in `.ggen/state.json`:

```json
{
  "last_phase": "dev",
  "phase_history": [
    {
      "phase": "setup",
      "started_ms": 1736539200000,
      "duration_ms": 1500,
      "success": true
    }
  ],
  "cache_keys": [
    {
      "phase": "setup",
      "key": "a1b2c3d4..."  // SHA256 hash of inputs
    }
  ]
}
```

### 2. Running Local Development with Hooks

**Key Pattern**: Hooks run automatically before/after phases:

```toml
# make.toml
[hooks]
before_dev = ["check:ports", "ensure:db"]
after_dev = ["cleanup:temp"]

before_build = ["test", "lint"]
after_build = ["analyze:bundle"]

before_deploy = ["test:e2e", "check:migrations"]
after_deploy = ["smoke:test", "notify:slack"]
```

**Development Loop**:

```bash
# Start dev server (runs before_dev hooks first)
ggen dev

# In another terminal, make changes and test
# Hooks ensure consistent environment

# Build for testing (runs test + lint first)
ggen build

# If build fails, hooks prevent bad builds
# State preserves what succeeded/failed
```

**Pro Tip**: Use `--dry-run` to see what will execute:

```bash
ggen build --dry-run
# Output:
# Will execute hooks: test, lint
# Then execute: build
```

### 3. Testing with Lifecycle Pipelines

**Pipeline Pattern**: Chain multiple phases in order:

```bash
# Run complete test pipeline
ggen pipeline init setup test

# Or run individual phases
ggen test                    # Run tests
ggen test:unit               # If defined in scripts
ggen test --filter e2e       # Filter tests
```

**Test Phase Configuration**:

```toml
[lifecycle.test]
description = "Run test suite"
commands = [
    "cargo test --workspace",
    "cargo test --doc",
]
parallel = false  # Run sequentially

[lifecycle."test:unit"]
description = "Unit tests only"
command = "cargo test --lib"

[lifecycle."test:integration"]
description = "Integration tests"
command = "cargo test --test '*'"
```

### 4. Debugging Phase Failures

**When a phase fails, check state**:

```bash
# View last execution
ggen state show

# Check specific phase history
ggen state history build

# View cache status
ggen state cache
```

**State shows exactly what happened**:

```json
{
  "phase_history": [
    {
      "phase": "build",
      "started_ms": 1736539200000,
      "duration_ms": 5000,
      "success": false  // ← Failed!
    }
  ]
}
```

**Debugging Steps**:

1. **Force rerun** (ignore cache):
   ```bash
   ggen build --force
   ```

2. **Check hook execution**:
   ```bash
   # Hooks run before build might be failing
   ggen test  # Run before_build hook manually
   ggen lint  # Run other before_build hook
   ```

3. **Run with verbose output**:
   ```bash
   RUST_LOG=debug ggen build
   ```

4. **Reset state if corrupted**:
   ```bash
   ggen state reset
   ggen state reset --phase build  # Reset specific phase
   ```

### 5. Committing with CI Integration

**Pre-commit workflow**:

```toml
[hooks]
before_commit = ["lint", "test:unit"]
before_push = ["test:all", "build"]
```

**Git hooks integration** (optional):

```bash
# .git/hooks/pre-commit
#!/bin/bash
ggen lint && ggen test:unit
```

**CI Pipeline**:

```yaml
# .github/workflows/ci.yml
name: CI
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable

      # Use ggen lifecycle for CI
      - name: Setup
        run: ggen setup

      - name: Lint
        run: ggen lint

      - name: Test
        run: ggen test

      - name: Build
        run: ggen build
```

---

## Best Practices from the Codebase

### 1. Structure make.toml for Maintainability

**✅ Good: Modular, composable phases**

```toml
# Clear phase separation
[lifecycle.setup]
description = "Install dependencies"
command = "cargo fetch"
cache_key = ["Cargo.lock"]

[lifecycle.build]
description = "Build release binary"
command = "cargo build --release"
outputs = ["target/release/ggen"]
cache = true

[lifecycle.test]
description = "Run all tests"
commands = [
    "cargo test --workspace",
    "cargo test --doc",
]

# Hooks compose phases
[hooks]
before_build = ["test", "lint"]
```

**❌ Bad: Monolithic, unclear phases**

```toml
# Don't do this
[lifecycle.do-everything]
commands = [
    "cargo test",
    "cargo build",
    "cargo clippy",
    "cargo fmt",
    "echo done",
]
```

**Why?**
- Hard to run individual steps
- Can't leverage caching effectively
- Difficult to debug failures
- Poor hook integration

### 2. Hook Patterns that Scale

**Pattern 1: Validation Hooks**

```toml
[hooks]
before_all = ["validate:env"]  # Runs before every phase

[lifecycle."validate:env"]
description = "Validate environment"
command = "ggen check:env"
```

**Pattern 2: Cleanup Hooks**

```toml
[hooks]
after_all = ["cleanup:temp"]

[lifecycle."cleanup:temp"]
description = "Remove temporary files"
command = "rm -rf /tmp/ggen-*"
```

**Pattern 3: Conditional Hooks**

```toml
[hooks]
on_error = "rollback:last"
on_success = "notify:success"

[lifecycle."rollback:last"]
description = "Rollback on failure"
command = "ggen state revert"
```

**Best Practice: Keep hooks lightweight**

```toml
# ✅ Good: Fast validation
before_dev = ["check:ports"]

# ❌ Bad: Slow, blocks development
before_dev = ["test", "build", "deploy"]
```

### 3. State Management for Reproducibility

**Track Generated Files**:

```toml
[state]
file = ".ggen/state.json"
track_generated = true
track_migrations = true

# Files to track
[state.generated]
src = ["src/generated/*.rs"]
templates = ["templates/output/*"]
```

**State Benefits**:

1. **Reproducibility**: Know exactly what was run and when
2. **Debugging**: Trace issues back to specific executions
3. **Caching**: Skip expensive operations if inputs unchanged
4. **Rollback**: Revert to previous states

**Caching Strategy**:

```toml
[lifecycle.build]
command = "cargo build --release"
outputs = ["target/release/"]
cache = true
cache_key = ["Cargo.toml", "Cargo.lock", "src/**/*.rs"]

# Cache only valid if these files unchanged
force_rerun_if_changed = [
    "Cargo.toml",
    "build.rs",
]
```

**Implementation** (from codebase):

```rust
// ggen-core/src/lifecycle/cache.rs
pub fn cache_key(
    phase: &str,
    commands: &[String],
    env: &[(String, String)],
    inputs: &[String],
) -> String {
    use sha2::{Digest, Sha256};

    let mut hasher = Sha256::new();
    hasher.update(phase.as_bytes());

    for cmd in commands {
        hasher.update(cmd.as_bytes());
    }

    for (k, v) in env {
        hasher.update(k.as_bytes());
        hasher.update(v.as_bytes());
    }

    for input in inputs {
        if let Ok(content) = std::fs::read(input) {
            hasher.update(&content);
        }
    }

    format!("{:x}", hasher.finalize())
}
```

### 4. Cache Strategies that Work

**Strategy 1: Content-Based Caching**

```toml
[lifecycle.build]
cache = true
# Automatically includes command + env in cache key
outputs = ["target/release/ggen"]
```

**How it works**:
1. Hash phase name + commands + env vars
2. Store hash in `.ggen/state.json`
3. On next run, compare hashes
4. Skip if unchanged

**Strategy 2: File-Based Invalidation**

```toml
[lifecycle.setup]
cache_key = ["Cargo.lock", "package.json"]
# Re-run if these files changed
```

**Strategy 3: Output Verification**

```toml
[lifecycle.build]
outputs = ["target/release/ggen"]
# Skip if output exists and cache valid
skip_if_exists = ["target/release/ggen"]
```

**Cache Location**:

```
.ggen/
├── state.json           # State + cache keys
└── cache/
    ├── build/
    │   └── a1b2c3...    # Cache marker files
    └── test/
        └── d4e5f6...
```

### 5. Error Handling Patterns

**Pattern 1: Early Validation**

```toml
[hooks]
before_all = ["validate:deps"]

[lifecycle."validate:deps"]
description = "Check all dependencies available"
command = "ggen check:deps"
```

**Pattern 2: Graceful Degradation**

```toml
[lifecycle.dev]
command = "cargo watch -x run"
# If fails, try fallback
on_error = "cargo run"
```

**Pattern 3: Cleanup on Failure**

```toml
[hooks]
on_error = "cleanup:partial"

[lifecycle."cleanup:partial"]
description = "Clean up partial builds"
command = "cargo clean"
```

**Implementation** (from codebase):

```rust
// ggen-core/src/lifecycle/exec.rs
pub fn run_phase(ctx: &Context, phase_name: &str) -> Result<()> {
    let phase = ctx.make.lifecycle.get(phase_name)
        .ok_or_else(|| anyhow::anyhow!("Phase '{}' not found", phase_name))?;

    // Run before hooks
    run_before_hooks(ctx, phase_name)?;

    // Execute phase
    let started = current_time_ms();
    let timer = Instant::now();

    for cmd in &cmds {
        execute_command(cmd, ctx.root, &ctx.env)?;
    }

    let duration = timer.elapsed().as_millis();

    // Update state
    let mut state = load_state(ctx.state_path);
    state.record_run(phase_name.to_string(), started, duration, true);
    save_state(ctx.state_path, &state)?;

    // Run after hooks
    run_after_hooks(ctx, phase_name)?;

    Ok(())
}
```

---

## Team Collaboration Patterns

### 1. Shared make.toml Conventions

**Convention 1: Standard Phase Names**

Every project uses the same core phases:

```toml
[lifecycle.init]    # First-time setup
[lifecycle.setup]   # Install dependencies
[lifecycle.dev]     # Development mode
[lifecycle.build]   # Production build
[lifecycle.test]    # Run tests
[lifecycle.lint]    # Code quality
[lifecycle.format]  # Code formatting
[lifecycle.deploy]  # Deployment
[lifecycle.clean]   # Cleanup
```

**Why?** Developers can work across projects with same muscle memory.

**Convention 2: Descriptive Phase Names**

```toml
# ✅ Good
[lifecycle."generate:api"]
[lifecycle."test:e2e"]
[lifecycle."deploy:staging"]

# ❌ Bad
[lifecycle.gen1]
[lifecycle.t]
[lifecycle.d]
```

**Convention 3: Environment Awareness**

```toml
[env.development]
API_URL = "http://localhost:4000"

[env.staging]
API_URL = "https://staging-api.example.com"

[env.production]
API_URL = "https://api.example.com"
```

### 2. Workspace Patterns for Monorepos

**Pattern: Explicit Workspace Configuration**

```toml
[project]
name = "my-monorepo"
type = "monorepo"

[workspace.frontend]
path = "apps/web"
framework = "react"
runtime = "node:20"

[workspace.backend]
path = "apps/api"
framework = "axum"
runtime = "rust:1.75"

[workspace.shared]
path = "packages/types"
runtime = "node:20"
```

**Pattern: Workspace-Specific Phases**

```toml
[lifecycle.install]
description = "Install all workspace dependencies"
parallel = true

[lifecycle.install.workspaces.frontend]
command = "cd apps/web && pnpm install"

[lifecycle.install.workspaces.backend]
command = "cd apps/api && cargo fetch"

[lifecycle.install.workspaces.shared]
command = "cd packages/types && pnpm install"
```

**Pattern: Dependency Order**

```toml
[lifecycle.build]
description = "Build all workspaces in correct order"
command = "ggen sequential:build --order shared,backend,frontend"
```

### 3. Environment Management Across Team

**Pattern: Local Environment Files**

```bash
# .env.local (gitignored, per-developer)
DATABASE_URL=postgres://localhost/myapp_dev
API_KEY=dev_key_123

# .env.example (committed, template)
DATABASE_URL=postgres://localhost/myapp_dev
API_KEY=your_key_here
```

**Pattern: Environment Detection**

```toml
[env]
detect = "auto"  # From NODE_ENV, RAILS_ENV, etc.

[env.development]
phases.dev.command = "cargo run"
phases.dev.watch = true

[env.production]
phases.build.command = "cargo build --release"
phases.build.optimize = true
```

**Pattern: Team Defaults**

```toml
# make.toml (committed)
[env.development]
API_URL = "http://localhost:4000"
LOG_LEVEL = "debug"

# ggen.toml (per-developer, optional)
[project.env]
development.API_URL = "http://localhost:5000"  # Override
```

### 4. CI/CD Integration Patterns

**Pattern: Single Source of Truth**

```yaml
# .github/workflows/ci.yml
jobs:
  build:
    steps:
      # Instead of duplicating commands:
      - run: cargo test
      - run: cargo build
      - run: cargo clippy

      # Use make.toml as source of truth:
      - run: ggen pipeline setup test build
```

**Pattern: Environment-Specific Workflows**

```yaml
# .github/workflows/deploy-staging.yml
jobs:
  deploy:
    steps:
      - run: ggen deploy --env staging

# .github/workflows/deploy-production.yml
jobs:
  deploy:
    steps:
      - run: ggen deploy --env production
```

**Pattern: Cache Integration**

```yaml
jobs:
  build:
    steps:
      - uses: actions/cache@v3
        with:
          path: |
            .ggen/cache
            target/
          key: ${{ runner.os }}-ggen-${{ hashFiles('Cargo.lock') }}

      - run: ggen build  # Uses cache if valid
```

### 5. Code Review Checklist for Lifecycle Changes

**When reviewing make.toml changes, check:**

- [ ] **Phase names** follow conventions
- [ ] **Commands** are idempotent (safe to run multiple times)
- [ ] **Hooks** are necessary and lightweight
- [ ] **Cache keys** include all relevant inputs
- [ ] **Outputs** are tracked for cleanup
- [ ] **Environment vars** don't contain secrets
- [ ] **Dependencies** between phases are correct
- [ ] **Workspaces** configuration is accurate
- [ ] **State tracking** is enabled where needed
- [ ] **Error handling** hooks are defined

**Example Review Comments**:

```toml
# ❌ Needs improvement
[lifecycle.deploy]
command = "docker push myapp:latest"  # Missing before_deploy checks

# ✅ Better
[lifecycle.deploy]
command = "docker push myapp:latest"

[hooks]
before_deploy = ["test:e2e", "check:migrations", "backup:db"]
after_deploy = ["smoke:test", "notify:slack"]
```

---

## Real Examples from the Codebase

### Example 1: Simple Node.js Project

```toml
# examples/make.toml
[project]
name = "example-app"
type = "webapp"
version = "1.0.0"

[lifecycle.init]
description = "Initialize project structure"
commands = [
    "mkdir -p src tests docs",
    "echo 'console.log(\"Hello, ggen!\");' > src/index.js",
]

[lifecycle.setup]
description = "Install dependencies"
command = "npm install"

[lifecycle.dev]
description = "Start development server"
command = "npm run dev"
watch = true
port = 3000

[lifecycle.build]
description = "Build for production"
command = "npm run build"
outputs = ["dist/"]
cache = true

[lifecycle.test]
description = "Run test suite"
command = "npm test"

[hooks]
before_build = ["test", "lint"]
```

### Example 2: Rust Project with Workspaces

```toml
# ggen workspace (real example)
[project]
name = "ggen"
type = "rust-workspace"
version = "1.2.0"

[workspace.core]
path = "ggen-core"
language = "rust"

[workspace.cli]
path = "cli"
language = "rust"

[workspace.ai]
path = "ggen-ai"
language = "rust"

[lifecycle.setup]
description = "Install dependencies"
command = "cargo fetch"
cache_key = ["Cargo.lock"]

[lifecycle.build]
description = "Build all workspace crates"
command = "cargo build --workspace --release"
outputs = ["target/release/ggen"]
cache = true

[lifecycle.test]
description = "Run all tests"
commands = [
    "cargo test --workspace",
    "cargo test --doc",
]

[lifecycle.lint]
description = "Lint code"
commands = [
    "cargo clippy --workspace -- -D warnings",
    "cargo fmt --check",
]

[hooks]
before_build = ["test", "lint"]
before_deploy = ["test", "build"]
```

### Example 3: Integration Test Pattern

```rust
// ggen-core/src/lifecycle/integration_test.rs
#[test]
fn test_run_single_phase() {
    let fixture = LifecycleTestFixture::new();
    let ctx = fixture.create_context();

    // Run init phase
    let result = run_phase(&ctx, "init");
    assert!(result.is_ok());

    // Verify state was updated
    let state = fixture.load_state();
    assert_eq!(state.last_phase, Some("init".to_string()));
    assert_eq!(state.phase_history.len(), 1);

    let record = &state.phase_history[0];
    assert_eq!(record.phase, "init");
    assert!(record.success);
    assert!(record.duration_ms > 0);

    // Verify files were created
    assert!(fixture.path().join("src/test.txt").exists());
}
```

**Key Testing Patterns**:

1. **Fixture-based setup**: `LifecycleTestFixture` provides isolated test environment
2. **State verification**: Check state after each operation
3. **File verification**: Ensure outputs are created
4. **Timing checks**: Verify phases execute in reasonable time

---

## Debugging and Troubleshooting

### Common Issues and Solutions

#### Issue 1: Phase Not Found

```bash
$ ggen build
Error: Phase 'build' not found
```

**Solution**:

```bash
# Check available phases
ggen lifecycle list

# If build not defined, add to make.toml:
[lifecycle.build]
command = "cargo build --release"
```

#### Issue 2: Hook Fails

```bash
$ ggen build
▶️  Running phase: test  (before_build hook)
Error: Command failed: cargo test
```

**Solution**:

```bash
# Run hook manually to debug
ggen test

# Or skip hooks for testing
ggen build --no-hooks  # If flag supported

# Or temporarily remove from make.toml:
[hooks]
# before_build = ["test"]  # Comment out
```

#### Issue 3: Cache Not Invalidating

```bash
$ ggen build
⚡ Using cached result for 'build'
# But you made changes!
```

**Solution**:

```bash
# Force rebuild
ggen build --force

# Or update cache_key in make.toml:
[lifecycle.build]
cache_key = [
    "Cargo.toml",
    "Cargo.lock",
    "src/**/*.rs",  # Add missing inputs
]
```

#### Issue 4: State Corruption

```bash
$ ggen dev
Error: Failed to load state: invalid JSON
```

**Solution**:

```bash
# Reset state
ggen state reset

# Or manually remove corrupted file
rm .ggen/state.json

# Then re-run setup
ggen setup
```

#### Issue 5: Workspace Not Found

```bash
$ ggen build
Error: Workspace 'frontend' path 'apps/web' does not exist
```

**Solution**:

```bash
# Check workspace paths in make.toml
[workspace.frontend]
path = "apps/web"  # Verify this exists

# Or create missing workspace
mkdir -p apps/web
```

### Debug Logging

```bash
# Enable debug output
RUST_LOG=debug ggen build

# Specific module
RUST_LOG=ggen_core::lifecycle=trace ggen build

# Save log to file
RUST_LOG=debug ggen build 2>&1 | tee debug.log
```

### State Inspection

```bash
# View state file directly
cat .ggen/state.json | jq .

# Check last run
cat .ggen/state.json | jq '.last_phase'

# View phase history
cat .ggen/state.json | jq '.phase_history'

# Check cache keys
cat .ggen/state.json | jq '.cache_keys'
```

---

## CI/CD Integration

### GitHub Actions Complete Example

```yaml
name: CI/CD

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

env:
  RUST_VERSION: "1.75.0"

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - uses: dtolnay/rust-toolchain@stable
        with:
          toolchain: ${{ env.RUST_VERSION }}

      # Cache cargo dependencies
      - uses: actions/cache@v3
        with:
          path: |
            ~/.cargo/registry
            ~/.cargo/git
            target/
            .ggen/cache/
          key: ${{ runner.os }}-cargo-${{ hashFiles('Cargo.lock') }}

      # Use ggen lifecycle
      - name: Setup
        run: cargo run --bin ggen -- setup

      - name: Lint
        run: cargo run --bin ggen -- lint

      - name: Test
        run: cargo run --bin ggen -- test

      - name: Build
        run: cargo run --bin ggen -- build

      # Upload state for debugging
      - name: Upload state
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: ggen-state
          path: .ggen/state.json

  deploy:
    needs: test
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Deploy to production
        run: cargo run --bin ggen -- deploy --env production
        env:
          DEPLOY_TOKEN: ${{ secrets.DEPLOY_TOKEN }}
```

### GitLab CI Complete Example

```yaml
stages:
  - setup
  - test
  - build
  - deploy

variables:
  RUST_VERSION: "1.75.0"

cache:
  paths:
    - .ggen/cache/
    - target/

setup:
  stage: setup
  script:
    - cargo run --bin ggen -- setup
  artifacts:
    paths:
      - .ggen/state.json

test:
  stage: test
  script:
    - cargo run --bin ggen -- test
  artifacts:
    when: on_failure
    paths:
      - .ggen/state.json

build:
  stage: build
  script:
    - cargo run --bin ggen -- build
  artifacts:
    paths:
      - target/release/ggen
      - .ggen/state.json

deploy_staging:
  stage: deploy
  script:
    - cargo run --bin ggen -- deploy --env staging
  only:
    - develop

deploy_production:
  stage: deploy
  script:
    - cargo run --bin ggen -- deploy --env production
  only:
    - main
  when: manual
```

---

## Quick Reference Commands

```bash
# List phases
ggen lifecycle list
ggen lifecycle:list

# Run phase
ggen run init
ggen init              # Shorthand

# Run pipeline
ggen pipeline init setup dev
ggen run init setup dev

# Show state
ggen state show
ggen state history
ggen state cache

# Reset state
ggen state reset
ggen state reset --phase build

# Force rerun
ggen build --force
ggen build --no-cache

# Dry run
ggen deploy --dry-run

# Environment
ggen build --env production
ggen dev --env staging
```

---

## Summary: Core Principles

1. **State-aware**: Every execution tracked in `.ggen/state.json`
2. **Cacheable**: Skip expensive operations when possible
3. **Composable**: Phases compose via hooks
4. **Reproducible**: Same inputs = same outputs
5. **Debuggable**: State history enables time-travel debugging
6. **Team-friendly**: Consistent conventions across projects

**Remember**: The lifecycle system is your project's operating system. Master these patterns and your team will ship faster, more reliably, and with fewer surprises.

---

## Next Steps

- Read [LIFECYCLE_SYSTEM_DESIGN.md](LIFECYCLE_SYSTEM_DESIGN.md) for the vision
- See [LIFECYCLE_BEST_PRACTICES.md](LIFECYCLE_BEST_PRACTICES.md) for Rust patterns
- Check [LIFECYCLE_README.md](LIFECYCLE_README.md) for end-user guide
- Use [LIFECYCLE_QUICK_REFERENCE.md](LIFECYCLE_QUICK_REFERENCE.md) for command lookup

---

**Last Updated**: 2025-10-11
**Version**: 1.2.0
**Status**: Living Document (update as patterns emerge)
