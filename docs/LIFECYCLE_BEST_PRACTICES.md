# ggen Lifecycle System: Best Practices

## Core Principles (80/20 Rule)

This guide focuses on the 20% of patterns that deliver 80% of value. These practices are extracted from real-world usage of the ggen lifecycle system.

## 1. Essential make.toml Patterns

### Start Simple, Grow Gradually

```toml
# Minimum viable make.toml
[project]
name = "my-app"
type = "webapp"
version = "1.0.0"

[lifecycle.dev]
command = "npm run dev"

[lifecycle.build]
command = "npm run build"
```

**Why**: Don't over-engineer. Start with basic commands and add complexity only when needed.

### Use Commands Arrays for Multi-Step Operations

```toml
[lifecycle.init]
description = "Initialize project structure"
commands = [
    "mkdir -p src tests docs config",
    "echo 'console.log(\"Hello\");' > src/index.js",
    "echo '{}' > config/default.json"
]
```

**Why**: Commands run sequentially, failing fast on errors. Better than shell scripts for visibility.

### Leverage Hooks for Quality Gates

```toml
[hooks]
# Run tests and linting before every build
before_build = ["test", "lint"]

# Deploy only after successful build
before_deploy = ["build"]

# Notify team after deployment
after_deploy = ["notify"]

[lifecycle.test]
command = "npm test"

[lifecycle.lint]
command = "npm run lint"

[lifecycle.notify]
command = "echo '✅ Deployment complete!'"
```

**Why**: Hooks enforce best practices automatically. No manual checks needed.

## 2. State Management Patterns

### Track Build Outputs

```toml
[lifecycle.build]
description = "Build for production"
command = "npm run build"
outputs = ["dist/", ".output/"]
cache = true
```

**Why**: ggen tracks outputs in `.ggen/state.json` for cache invalidation and cleanup.

### Use State for Idempotency

```bash
# First run - executes fully
ggen run setup

# Second run - checks state, may skip if nothing changed
ggen run setup
```

**State file location**: `.ggen/state.json`

**Why**: Prevents redundant work. Critical for CI/CD performance.

### Clean Generated Files Safely

```bash
# Remove tracked build artifacts
ggen clean

# View what will be cleaned (dry run)
ggen state files
```

**Why**: State tracking makes cleanup safe - only removes what ggen created.

## 3. Common Workflows

### Full Development Cycle

```bash
# One-time setup
ggen run init
ggen run setup

# Daily development
ggen run dev          # Start dev server

# Before committing
ggen run test
ggen run lint

# Production build
ggen run build --env production

# Deployment
ggen run deploy --env production
```

### Pipeline Execution

```bash
# Run multiple phases in sequence
ggen pipeline test lint build deploy

# With environment override
ggen pipeline build deploy --env staging
```

**Why**: Pipelines ensure phases run in correct order with proper dependencies.

## 4. Real-World Examples

### Node.js Web Application

```toml
[project]
name = "web-app"
type = "webapp"
version = "1.0.0"

[lifecycle.init]
commands = [
    "npm init -y",
    "mkdir -p src tests",
    "echo 'export {}' > src/index.ts"
]

[lifecycle.setup]
command = "npm install"

[lifecycle.dev]
description = "Start development server with hot reload"
command = "npm run dev"
watch = true
port = 3000

[lifecycle.build]
description = "Build optimized production bundle"
commands = [
    "npm run build",
    "npm run analyze"
]
outputs = ["dist/"]
cache = true

[lifecycle.test]
command = "npm test -- --coverage"

[lifecycle.lint]
command = "npm run lint -- --fix"

[lifecycle.deploy]
commands = [
    "npm run build",
    "./scripts/deploy.sh"
]

[hooks]
before_build = ["test", "lint"]
before_deploy = ["build"]
```

### Rust CLI Tool

```toml
[project]
name = "rust-cli"
type = "cli"
version = "0.1.0"

[lifecycle.init]
command = "cargo init --name rust-cli"

[lifecycle.setup]
command = "cargo fetch"

[lifecycle.dev]
command = "cargo watch -x run"
watch = true

[lifecycle.build]
commands = [
    "cargo build --release",
    "strip target/release/rust-cli"
]
outputs = ["target/release/"]
cache = true

[lifecycle.test]
command = "cargo test --all-features"

[lifecycle.lint]
commands = [
    "cargo clippy -- -D warnings",
    "cargo fmt --check"
]

[hooks]
before_build = ["test", "lint"]
```

### Monorepo with Multiple Services

```toml
[project]
name = "monorepo"
type = "monorepo"
version = "1.0.0"

[workspace.frontend]
path = "apps/web"
framework = "nuxt"
runtime = "node"
package_manager = "pnpm"

[workspace.backend]
path = "apps/api"
framework = "rust"
runtime = "cargo"

[workspace.mobile]
path = "apps/mobile"
framework = "react-native"
runtime = "node"

[lifecycle.dev]
description = "Start all services"
command = "pnpm run dev"
workspaces = ["frontend", "backend"]
parallel = true

[lifecycle.build]
description = "Build all services in order"
command = "pnpm run build"
workspaces = ["backend", "frontend", "mobile"]
parallel = false  # Build in sequence

[lifecycle.test]
command = "pnpm test"
workspaces = ["frontend", "backend", "mobile"]
parallel = true
```

## 5. Common Pitfalls and Solutions

### ❌ Problem: Commands Fail Without Clear Error

```toml
[lifecycle.build]
command = "build.sh"  # No output, hard to debug
```

✅ **Solution**: Use verbose commands with echo

```toml
[lifecycle.build]
commands = [
    "echo '🔨 Starting build...'",
    "./build.sh",
    "echo '✅ Build complete!'"
]
```

### ❌ Problem: Forgetting to Run Tests Before Deploy

```toml
[lifecycle.deploy]
command = "./deploy.sh"
```

✅ **Solution**: Use hooks for automatic quality gates

```toml
[hooks]
before_deploy = ["test", "lint", "build"]

[lifecycle.deploy]
command = "./deploy.sh"
```

### ❌ Problem: Build Cache Never Invalidates

```toml
[lifecycle.build]
command = "npm run build"
cache = true  # Too aggressive
```

✅ **Solution**: Track outputs for smart invalidation

```toml
[lifecycle.build]
command = "npm run build"
outputs = ["dist/"]
cache = true  # Invalidates when dist/ changes
```

### ❌ Problem: Development Server Blocks Other Commands

```toml
[lifecycle.dev]
command = "npm run dev"  # Runs forever
```

✅ **Solution**: Mark as watch mode

```toml
[lifecycle.dev]
command = "npm run dev"
watch = true  # ggen handles gracefully
port = 3000   # Documents exposed port
```

## 6. Integration with ggen Templates

### Generate Component with Lifecycle

```yaml
---
to: "src/components/{{name}}.tsx"
---
import React from 'react';

export const {{name}}: React.FC = () => {
  return <div>{{name}}</div>;
};
```

### Custom Generate Phase

```toml
[lifecycle."generate:component"]
description = "Generate React component"
command = "ggen gen component {{name}}"
```

Usage:
```bash
ggen run generate:component --name Button
```

## 7. Environment Management

### Basic Environment Support

```toml
[lifecycle.dev]
command = "npm run dev"

[lifecycle.build]
command = "npm run build"
```

Usage:
```bash
# Development (default)
ggen run dev

# Production
ggen run build --env production
```

### Environment-Specific Behavior

Environment name is passed as `GGEN_ENV` variable:

```toml
[lifecycle.deploy]
command = "deploy.sh $GGEN_ENV"
```

```bash
ggen run deploy --env staging  # GGEN_ENV=staging
ggen run deploy --env production  # GGEN_ENV=production
```

## 8. Performance Tips

### Cache Expensive Operations

```toml
[lifecycle.setup]
description = "Install dependencies (cached)"
command = "npm install"
cache = true
outputs = ["node_modules/"]
```

### Parallel Execution in Monorepos

```toml
[lifecycle.test]
command = "pnpm test"
workspaces = ["app1", "app2", "app3"]
parallel = true  # Run tests in parallel
```

### Use Outputs for Smart Rebuilds

```toml
[lifecycle.build]
command = "webpack --mode production"
outputs = ["dist/bundle.js", "dist/styles.css"]
cache = true
```

**Why**: ggen only rebuilds if source files are newer than outputs.

## 9. Quick Debugging

### View Phase Details

```bash
# See what a phase will do
ggen lifecycle show build

# Output:
# 📦 Phase: build
# Description: Build for production
# Commands:
#   $ npm run build
# Cache: true
# Outputs: dist/
```

### Check State

```bash
# View lifecycle history
ggen state history

# See last run phase
ggen lifecycle list
# Output:
# 🔄 Last executed: build
```

### Dry Run

```bash
# See what would execute without running
ggen run deploy --dry-run
```

## 10. Quick Reference Patterns

### Minimal Viable make.toml

```toml
[project]
name = "my-app"
type = "webapp"

[lifecycle.dev]
command = "npm run dev"

[lifecycle.build]
command = "npm run build"

[lifecycle.test]
command = "npm test"
```

### Standard Web App

```toml
[project]
name = "web-app"
type = "webapp"
version = "1.0.0"

[lifecycle.init]
commands = ["mkdir -p src tests", "npm init -y"]

[lifecycle.setup]
command = "npm install"

[lifecycle.dev]
command = "npm run dev"
watch = true
port = 3000

[lifecycle.build]
commands = ["npm run build"]
outputs = ["dist/"]
cache = true

[lifecycle.test]
command = "npm test"

[hooks]
before_build = ["test"]
before_deploy = ["build"]
```

### With Quality Gates

```toml
[project]
name = "quality-app"
type = "webapp"

[lifecycle.dev]
command = "npm run dev"

[lifecycle.test]
command = "npm test"

[lifecycle.lint]
command = "npm run lint"

[lifecycle.format]
command = "npm run format"

[lifecycle.build]
command = "npm run build"
outputs = ["dist/"]

[lifecycle.deploy]
command = "./deploy.sh"

[hooks]
before_build = ["test", "lint"]
before_deploy = ["build"]
after_deploy = ["notify"]

[lifecycle.notify]
command = "echo '✅ Deployed successfully!'"
```

## Key Takeaways

1. **Start simple** - Add complexity only when needed
2. **Use hooks** - Enforce quality gates automatically
3. **Track outputs** - Enable smart caching and cleanup
4. **Pipelines over scripts** - Better visibility and control
5. **Document with descriptions** - Self-documenting workflows
6. **Test before deploy** - Use before_deploy hooks
7. **Cache expensive ops** - setup, build, test phases
8. **Environment awareness** - Use --env flag for different deployments

## Next Steps

- Read [LIFECYCLE_QUICK_REFERENCE.md](./LIFECYCLE_QUICK_REFERENCE.md) for command syntax
- See [LIFECYCLE_SYSTEM_DESIGN.md](./LIFECYCLE_SYSTEM_DESIGN.md) for architecture details
- Check [examples/make.toml](../examples/make.toml) for a working example

---

## 9. London School TDD Best Practices

### What is London School TDD?

London School TDD (also called "mockist" or "outside-in") is a test-driven development methodology that emphasizes:

1. **Outside-In Development** - Start with high-level acceptance tests
2. **Interaction Testing** - Verify object collaborations, not state
3. **Mock All Dependencies** - Use test doubles for all collaborators
4. **Design Through Testing** - Discover interfaces naturally through test needs

### Why London School for Lifecycle?

The lifecycle system is **perfect for London School TDD** because:

- ✅ Many external dependencies (filesystem, shell, state)
- ✅ Clear collaboration boundaries (executor, repository, hooks)
- ✅ Need for fast, isolated tests
- ✅ Complex interaction flows (hooks, observers, state)

### Quick Start: mockall Integration

```toml
# Cargo.toml - Already added
[dev-dependencies]
mockall = "0.13"
```

### Pattern 1: Extract Trait Interfaces

**Before (Hard to Test)**:
```rust
// Direct dependencies - cannot mock
pub fn run_phase(ctx: &Context, phase_name: &str) -> Result<()> {
    // Direct shell execution
    let status = Command::new("sh").arg("-c").arg(cmd).status()?;

    // Direct filesystem
    let mut state = load_state(ctx.state_path);
    save_state(ctx.state_path, &state)?;
}
```

**After (Testable with Mocks)**:
```rust
// Dependency injection with traits
pub struct PhaseExecutor {
    cmd_executor: Box<dyn CommandExecutor>,
    state_repo: Box<dyn StateRepository>,
}

#[automock]
pub trait CommandExecutor: Send + Sync {
    fn execute(&self, spec: &CommandSpec) -> Result<CommandOutput>;
}
```

### Pattern 2: Outside-In Acceptance Tests

```rust
#[test]
fn acceptance_run_phase_with_hooks() {
    // GIVEN: Mocked dependencies
    let mut mock_cmd = MockCommandExecutor::new();
    let mut mock_state = MockStateRepository::new();
    let mut mock_hooks = MockHookRegistry::new();

    // EXPECT: Observer notified
    mock_observer.expect_on_phase_start()
        .with(eq("build"))
        .times(1);

    // EXPECT: Before hooks executed
    mock_hooks.expect_execute_before()
        .with(eq("build"))
        .times(1)
        .returning(|_| Ok(()));

    // EXPECT: Command executed
    mock_cmd.expect_execute()
        .withf(|spec| spec.command.contains("build"))
        .times(1)
        .returning(|_| Ok(CommandOutput::success()));

    // EXPECT: State saved
    mock_state.expect_save()
        .times(1)
        .returning(|_| Ok(()));

    // WHEN: Run phase
    let executor = PhaseExecutor::new(
        Box::new(mock_cmd),
        Box::new(mock_state),
        Box::new(mock_hooks),
    );

    // THEN: Success
    assert!(executor.run_phase("build").is_ok());
}
```

### Pattern 3: Interaction Verification

```rust
#[test]
fn test_hooks_execute_in_correct_order() {
    use mockall::Sequence;

    let mut seq = Sequence::new();
    let mut mock = MockHookRegistry::new();

    // Verify order
    mock.expect_execute_before()
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_| Ok(()));

    mock.expect_execute_after()
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_| Ok(()));

    // Sequence verified automatically
}
```

### Pattern 4: Test Builders for Ergonomics

```rust
pub struct PhaseExecutorBuilder {
    cmd_executor: Option<Box<dyn CommandExecutor>>,
    state_repo: Option<Box<dyn StateRepository>>,
}

impl PhaseExecutorBuilder {
    pub fn with_successful_command(mut self) -> Self {
        let mut mock = MockCommandExecutor::new();
        mock.expect_execute()
            .returning(|_| Ok(CommandOutput::success()));
        self.cmd_executor = Some(Box::new(mock));
        self
    }

    pub fn with_empty_state(mut self) -> Self {
        let mut mock = MockStateRepository::new();
        mock.expect_load()
            .returning(|| Ok(LifecycleState::default()));
        mock.expect_save()
            .returning(|_| Ok(()));
        self.state_repo = Some(Box::new(mock));
        self
    }

    pub fn build(self) -> PhaseExecutor {
        PhaseExecutor::new(
            self.cmd_executor.unwrap(),
            self.state_repo.unwrap(),
        )
    }
}

// Usage in tests:
#[test]
fn test_with_builder() {
    let executor = PhaseExecutorBuilder::new()
        .with_successful_command()
        .with_empty_state()
        .build();

    assert!(executor.run_phase("build").is_ok());
}
```

### Red-Green-Refactor Workflow

**RED: Write Failing Test**
```rust
#[test]
fn test_hook_recursion_prevention() {
    let mut mock = MockHookRegistry::new();

    mock.expect_execute_before()
        .with(eq("build"))
        .returning(|_| Err(Error::HookRecursion));

    // This should fail - not implemented yet
    let registry = HookRegistry::new(mock);
    let result = registry.execute_before("build");

    assert!(matches!(result, Err(Error::HookRecursion)));
}
```

**GREEN: Minimal Implementation**
```rust
pub struct HookRegistry {
    executing: RefCell<HashSet<String>>,
}

impl HookRegistry {
    pub fn execute_before(&self, phase: &str) -> Result<()> {
        if self.executing.borrow().contains(phase) {
            return Err(Error::HookRecursion);
        }
        self.executing.borrow_mut().insert(phase.into());
        // ... execute hook
        self.executing.borrow_mut().remove(phase);
        Ok(())
    }
}
```

**REFACTOR: Improve Design**
```rust
struct HookGuard<'a> {
    registry: &'a HookRegistry,
    phase: String,
}

impl Drop for HookGuard<'_> {
    fn drop(&mut self) {
        self.registry.executing.borrow_mut().remove(&self.phase);
    }
}

// Now cleanup is automatic via RAII
```

### mockall Cheat Sheet

```rust
use mockall::prelude::*;

// Define mockable trait
#[automock]
trait MyTrait {
    fn method(&self, arg: &str) -> Result<i32>;
}

// Create and configure mock
let mut mock = MockMyTrait::new();
mock.expect_method()
    .with(eq("test"))           // Exact arg match
    .times(1)                    // Called exactly once
    .returning(|_| Ok(42));     // Return value

// Use in test
assert_eq!(mock.method("test"), Ok(42));
```

### Common Mock Patterns

**Verify Call Count**:
```rust
mock.expect_method()
    .times(1)           // Exactly once
    .times(2..5)        // Between 2 and 5
    .times(mockall::predicate::ge(1))  // At least once
```

**Verify Arguments**:
```rust
mock.expect_method()
    .with(eq("exact"))                      // Exact match
    .withf(|arg| arg.len() > 5)            // Custom predicate
    .with(predicate::function(|x: &str| x.starts_with("pre")))
```

**Return Values**:
```rust
mock.expect_method()
    .returning(|| Ok(42))           // Constant
    .returning(|arg| Ok(arg * 2))  // Computed
```

### Best Practices

✅ **DO**:
- Mock external dependencies (filesystem, network, shell)
- Use mocks to discover interfaces
- Test interactions, not implementation
- Use builders for test setup
- Verify execution order when it matters

❌ **DON'T**:
- Mock simple value objects
- Over-specify implementation details
- Test private methods directly
- Use mocks for everything (unit test pure functions normally)

### Complete Example

See `/Users/sac/ggen/ggen-core/tests/london_tdd_examples.rs` for:
- 7 complete test examples
- Mock infrastructure setup
- Builder patterns
- Interaction verification
- Error handling tests

### Further Reading

- **[London School TDD Guide](./LONDON_SCHOOL_TDD_LIFECYCLE.md)** - Complete methodology guide
- **mockall documentation** - https://docs.rs/mockall/
- **Test examples** - `/ggen-core/tests/london_tdd_examples.rs`

---

## 10. Summary: Core Team Workflow

### Daily Development Cycle

1. **Write test first** (London School TDD)
   ```bash
   # Create test with mocks
   cargo test test_new_feature -- --nocapture
   # Should FAIL (RED)
   ```

2. **Implement minimal code**
   ```bash
   # Write just enough to pass
   cargo test test_new_feature
   # Should PASS (GREEN)
   ```

3. **Refactor with confidence**
   ```bash
   # Improve design
   cargo test
   # All tests still PASS
   ```

4. **Commit with tests**
   ```bash
   git add .
   git commit -m "feat: add feature with tests"
   ```

### Code Review Checklist

- ✅ Tests written first (TDD)
- ✅ Interactions verified with mocks
- ✅ No unwraps in production code
- ✅ Error types with context
- ✅ Documentation updated
- ✅ All tests pass
- ✅ No clippy warnings

### Performance Benchmarks

```bash
# Run benchmarks
cargo bench

# Profile with flamegraph
cargo install flamegraph
cargo flamegraph --bin ggen -- lifecycle run build
```

### Release Checklist

- ✅ All tests pass (>80% coverage)
- ✅ No clippy warnings
- ✅ Documentation complete
- ✅ CHANGELOG.md updated
- ✅ Version bumped in Cargo.toml
- ✅ Tagged release in git

---

**Document Version**: 1.1
**Last Updated**: 2025-01-11
**Includes**: London School TDD patterns

**For Questions**: See `/docs/LONDON_SCHOOL_TDD_LIFECYCLE.md` for complete TDD guide
