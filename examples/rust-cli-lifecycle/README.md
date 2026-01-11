# TaskMgr - Example Rust CLI with ggen Lifecycle

A comprehensive example demonstrating how to build a **noun-verb CLI** in Rust using the **ggen universal lifecycle system**.

## 🎯 What This Example Demonstrates

### Complete Lifecycle Integration
- ✅ **ALL 15 standard lifecycle phases** (init, setup, dev, build, test, lint, format, docs, install, bench, clean, release, etc.)
- ✅ **Custom phases** (generate:command, check:security, coverage:generate)
- ✅ **Hooks system** (before/after hooks, error handling)
- ✅ **Environment management** (development, staging, production)
- ✅ **State tracking** (`.ggen/state.json` for reproducibility)
- ✅ **Deterministic caching** (content-addressed cache keys)
- ✅ **Workspace support** (ready for monorepo scaling)

### Noun-Verb CLI Architecture
- ✅ **Clap-based** command structure
- ✅ **Modular design** (commands organized by domain)
- ✅ **Colored output** for better UX
- ✅ **Async support** with tokio
- ✅ **Comprehensive error handling** with anyhow

## 📋 Project Structure

```
rust-cli-lifecycle/
├── make.toml              # ← Universal lifecycle configuration (THE KEY FILE)
├── Cargo.toml             # Rust package manifest
├── README.md              # This file
├── src/
│   ├── main.rs            # CLI entry point
│   └── commands/          # Noun-verb command modules
│       ├── mod.rs
│       ├── task.rs        # Task management commands
│       ├── project.rs     # Project management commands
│       ├── status.rs      # Status reporting commands
│       └── config.rs      # Configuration commands
├── tests/                 # Integration tests
├── docs/                  # Documentation
└── .ggen/                 # ggen lifecycle state
    ├── state.json         # Execution history
    └── cache/             # Deterministic cache
```

## 🚀 Quick Start

### 1. Initialize Project

```bash
cd examples/rust-cli-lifecycle
ggen lifecycle run init
```

**What it does**:
- Creates Rust project structure
- Generates README
- Sets up directory structure
- Records initial state in `.ggen/state.json`

### 2. Setup Dependencies

```bash
ggen lifecycle run setup
```

**What it does**:
- Installs all cargo dependencies (clap, serde, tokio, etc.)
- Configures development environment
- Auto-triggers `scaffold` phase via after-hook
- Caches `Cargo.toml` for future builds

### 3. Development Mode

```bash
ggen lifecycle run dev
```

**What it does**:
- Starts `cargo watch` for hot reload
- Runs CLI with `--help` flag
- Shows real-time compilation feedback

### 4. Build & Test

```bash
ggen lifecycle pipeline build test
```

**What it does**:
- **Before build**: Runs tests and linting (via hooks)
- **Build**: Creates optimized release binary
- **After build**: Generates documentation (via hooks)
- **Test**: Runs unit, integration, and doc tests in parallel

### 5. Full Release

```bash
ggen lifecycle run release
```

**What it does**:
- **Before release**: Runs build, test, and bench (via hooks)
- **Release**: Creates git tag and stripped binary
- **After release**: Auto-installs globally (via hooks)

## 📖 Using the CLI

Once built, you can use the noun-verb commands:

```bash
# Task management
taskmgr task create "Implement feature X" --priority high
taskmgr task list
taskmgr task complete 42
taskmgr task delete 1

# Project management
taskmgr project create my-project
taskmgr project list
taskmgr project show my-project

# Status and reporting
taskmgr status
taskmgr status --detailed

# Configuration
taskmgr config show
taskmgr config set default_priority high
```

## 🔧 Lifecycle Commands

### Standard Commands

```bash
# List all available phases
ggen lifecycle list

# Show details of a specific phase
ggen lifecycle show build

# Run a single phase
ggen lifecycle run build

# Run multiple phases in sequence (pipeline)
ggen lifecycle pipeline format lint test build

# Run with specific environment
ggen lifecycle run build --env production
```

### Phase Dependencies (Hooks)

The `make.toml` defines automatic phase dependencies:

```toml
[hooks]
before_build = ["test", "lint"]  # Always test & lint before building
after_build = ["docs"]           # Generate docs after successful build
before_release = ["build", "test", "bench"]  # Full validation
after_release = ["install"]      # Auto-install after release
```

**Example**: Running `ggen lifecycle run build` actually executes:
1. `format` (before test hook)
2. `test` (before build hook)
3. `lint` (before build hook)
4. `build` (the requested phase)
5. `docs` (after build hook)

## 📊 State Tracking

Every phase execution is tracked in `.ggen/state.json`:

```json
{
  "last_phase": "build",
  "phase_history": [
    {
      "phase": "build",
      "started_ms": 1704902400000,
      "duration_ms": 45000,
      "success": true
    }
  ],
  "generated": [...],
  "cache_keys": [...]
}
```

View state with:
```bash
cat .ggen/state.json | jq
ggen lifecycle history  # (if implemented)
```

## 🎯 80/20 Lessons from This Example

### 20% of Features That Deliver 80% Value

1. **Hooks System** → Automatic quality gates
2. **State Tracking** → Reproducible builds
3. **Cache Keys** → Fast incremental builds
4. **Noun-Verb Structure** → Scalable CLI
5. **Environment Management** → One config, all envs

### What Makes This Better Than Traditional Approaches

**Traditional**:
```bash
# Manual steps, easy to forget
cargo fmt
cargo clippy
cargo test
cargo build --release
cargo doc
# Did I run all checks? 🤔
```

**With ggen Lifecycle**:
```bash
# Single command, guaranteed complete
ggen lifecycle run build
# All checks run automatically via hooks ✅
```

## 🔄 Extending the Example

### Add a New Command

1. Create new module: `src/commands/report.rs`
2. Add to `Commands` enum in `main.rs`
3. Implement command logic
4. **No changes to make.toml needed** - lifecycle works automatically!

### Add Custom Phase

```toml
[lifecycle."generate:command"]
description = "Generate new command from template"
command = "ggen gen command {{name}}"
```

Usage:
```bash
ggen lifecycle run generate:command --name report
```

### Add Environment-Specific Behavior

```toml
[env.production]
RUST_LOG = "warn"
commands.build = "cargo build --release --locked"
commands.test = "cargo test --release -- --test-threads=1"
```

Usage:
```bash
ggen lifecycle run build --env production
```

## 📚 Key Files to Study

### 1. `make.toml` (THE MOST IMPORTANT)
**The single source of truth** for your project lifecycle. Study this file to understand:
- How phases are defined
- How hooks create automation
- How environments change behavior
- How caching works

### 2. `src/main.rs`
Shows the noun-verb CLI pattern with clap:
- Clear command hierarchy
- Type-safe arguments
- Async support
- Error handling

### 3. `src/commands/*.rs`
Demonstrates modular command organization:
- Each noun gets its own module
- Verbs are functions in that module
- Shared utilities in common module

## 🧪 Testing

Run tests at different levels:

```bash
# Unit tests only
cargo test --lib

# Integration tests only
cargo test --test '*'

# Doc tests only
cargo test --doc

# All tests (via lifecycle)
ggen lifecycle run test
```

## 📦 Distribution

Build and install globally:

```bash
# Build release binary
ggen lifecycle run build

# Install globally
ggen lifecycle run install

# Now use from anywhere
taskmgr --help
```

## 🎓 What You'll Learn

After studying this example, you'll understand:

1. **Universal Lifecycle** - How one `make.toml` manages entire project
2. **Hook Automation** - How before/after hooks create guardrails
3. **State Management** - How `.ggen/state.json` enables reproducibility
4. **Caching Strategy** - How content-addressed keys speed up builds
5. **Environment Handling** - How one config works in dev/staging/prod
6. **Noun-Verb Architecture** - How to scale CLI to dozens of commands
7. **Error Handling** - How anyhow provides ergonomic error propagation
8. **Async Patterns** - How tokio enables concurrent operations

## 🚀 Next Steps

1. **Study `make.toml`** - This is your lifecycle blueprint
2. **Run each phase** - See what happens at each step
3. **Check `.ggen/state.json`** - Understand state tracking
4. **Modify hooks** - Add your own automation
5. **Add commands** - Extend the noun-verb structure
6. **Deploy** - Use lifecycle for CI/CD

## 🔗 Related Documentation

- [Lifecycle System Design](../../docs/LIFECYCLE_SYSTEM_DESIGN.md)
- [Lifecycle Best Practices](../../docs/LIFECYCLE_BEST_PRACTICES.md)
- [Quick Reference Guide](../../docs/LIFECYCLE_QUICK_REFERENCE.md)
- [Team Workflow Guide](../../docs/LIFECYCLE_TEAM_WORKFLOW.md)

## 📝 License

MIT - This is an example project for educational purposes.

---

**Built with ggen lifecycle system** - The universal framework standard for 2027.
