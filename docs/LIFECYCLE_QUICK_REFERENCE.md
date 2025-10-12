# ggen Lifecycle: Quick Reference

One-page reference for ggen lifecycle commands and make.toml configuration.

---

## CLI Commands

### List & Inspect

```bash
# List all available phases
ggen lifecycle list

# Show details of a specific phase
ggen lifecycle show <phase>
ggen lifecycle show build

# Show state and history
ggen state
ggen state history
ggen state files
```

### Run Phases

```bash
# Run single phase
ggen run <phase>
ggen run dev
ggen run build
ggen run test

# Run with environment
ggen run build --env production
ggen run deploy --env staging

# Run multiple phases (pipeline)
ggen pipeline <phase1> <phase2> <phase3>
ggen pipeline test lint build deploy

# Pipeline with environment
ggen pipeline build deploy --env production
```

### State Management

```bash
# View current state
ggen state
ggen state show

# View generated files
ggen state files

# View phase history
ggen state history

# Reset state
ggen state reset

# Clean build artifacts
ggen clean
```

---

## make.toml Configuration

### Project Metadata

```toml
[project]
name = "my-app"              # Required: project name
type = "webapp"              # Optional: webapp, cli, library, monorepo
version = "1.0.0"            # Optional: semantic version
description = "My app"       # Optional: description
```

### Lifecycle Phases

#### Single Command

```toml
[lifecycle.dev]
description = "Start dev server"
command = "npm run dev"
watch = true                 # Mark as watch mode (long-running)
port = 3000                  # Document exposed port
```

#### Multiple Commands

```toml
[lifecycle.build]
description = "Build for production"
commands = [
    "echo '🔨 Building...'",
    "npm run build",
    "echo '✅ Done!'"
]
outputs = ["dist/"]          # Track outputs for caching
cache = true                 # Enable caching
```

#### Custom Phases

```toml
[lifecycle."generate:component"]
description = "Generate component"
command = "ggen gen component {{name}}"

[lifecycle.lint]
command = "npm run lint"
```

### Hooks

```toml
[hooks]
# Global hooks (run for all phases)
before_all = ["validate"]
after_all = ["cleanup"]

# Phase-specific hooks
before_init = ["check-deps"]
after_init = ["setup-git"]

before_setup = []
after_setup = []

before_build = ["test", "lint"]    # Quality gates
after_build = ["analyze"]

before_test = []
after_test = ["coverage"]

before_deploy = ["build"]          # Ensure build before deploy
after_deploy = ["notify", "smoke-test"]

# Error handling
on_error = "rollback"
on_success = "tag-release"
```

### Workspace (Monorepo)

```toml
[workspace.frontend]
path = "apps/web"
framework = "nuxt"
runtime = "node"
package_manager = "pnpm"

[workspace.backend]
path = "apps/api"
framework = "rust"
runtime = "cargo"

[lifecycle.dev]
command = "pnpm dev"
workspaces = ["frontend", "backend"]
parallel = true              # Run in parallel

[lifecycle.build]
command = "pnpm build"
workspaces = ["backend", "frontend"]
parallel = false             # Run sequentially
```

---

## Common Workflows

### Initialize New Project

```bash
# Create project structure
ggen run init

# Install dependencies
ggen run setup
```

### Development Workflow

```bash
# Start dev server
ggen run dev

# Run tests
ggen run test

# Lint code
ggen run lint
```

### Build & Deploy

```bash
# Build for production
ggen run build --env production

# Deploy (with hooks)
ggen run deploy --env production

# Full pipeline
ggen pipeline test lint build deploy --env production
```

### Generate Code

```bash
# Custom generate phase
ggen run generate:component --name Button
ggen run generate:page --path /about
```

---

## make.toml Templates

### Minimal

```toml
[project]
name = "my-app"
type = "webapp"

[lifecycle.dev]
command = "npm run dev"

[lifecycle.build]
command = "npm run build"
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

[lifecycle.lint]
command = "npm run lint"

[lifecycle.deploy]
command = "./deploy.sh"

[hooks]
before_build = ["test", "lint"]
before_deploy = ["build"]
```

### Rust Project

```toml
[project]
name = "rust-cli"
type = "cli"
version = "0.1.0"

[lifecycle.init]
command = "cargo init"

[lifecycle.setup]
command = "cargo fetch"

[lifecycle.dev]
command = "cargo watch -x run"
watch = true

[lifecycle.build]
commands = ["cargo build --release"]
outputs = ["target/release/"]
cache = true

[lifecycle.test]
command = "cargo test"

[lifecycle.lint]
commands = ["cargo clippy", "cargo fmt --check"]

[hooks]
before_build = ["test", "lint"]
```

### Monorepo

```toml
[project]
name = "monorepo"
type = "monorepo"

[workspace.web]
path = "apps/web"
framework = "react"

[workspace.api]
path = "apps/api"
framework = "express"

[lifecycle.dev]
command = "pnpm dev"
workspaces = ["web", "api"]
parallel = true

[lifecycle.build]
command = "pnpm build"
workspaces = ["api", "web"]  # Order matters
parallel = false

[lifecycle.test]
command = "pnpm test"
parallel = true

[hooks]
before_deploy = ["build"]
```

---

## Phase Options Reference

### Common Options

| Option | Type | Description | Example |
|--------|------|-------------|---------|
| `description` | string | Phase description | `"Build for production"` |
| `command` | string | Single command | `"npm run build"` |
| `commands` | array | Multiple commands | `["lint", "build"]` |
| `watch` | bool | Watch mode (long-running) | `true` |
| `port` | number | Exposed port | `3000` |
| `outputs` | array | Output files/dirs | `["dist/"]` |
| `cache` | bool | Enable caching | `true` |
| `workspaces` | array | Target workspaces | `["web", "api"]` |
| `parallel` | bool | Parallel execution | `true` |

---

## Environment Variables

ggen sets these variables during execution:

| Variable | Description | Example |
|----------|-------------|---------|
| `GGEN_ENV` | Environment name | `production`, `staging`, `development` |
| `GGEN_PHASE` | Current phase | `build`, `test`, `deploy` |

Usage in commands:
```toml
[lifecycle.deploy]
command = "deploy.sh $GGEN_ENV"
```

```bash
ggen run deploy --env production  # GGEN_ENV=production
```

---

## State File (.ggen/state.json)

```json
{
  "project": {
    "name": "my-app",
    "created_at": "2025-01-01T00:00:00Z"
  },
  "lifecycle": {
    "phases_run": ["init", "setup", "build"],
    "last_phase": "build",
    "phase_history": [
      {
        "phase": "build",
        "timestamp": "2025-01-10T12:00:00Z",
        "duration_ms": 45000,
        "success": true,
        "cache_key": "abc123..."
      }
    ]
  },
  "generated": {
    "files": [],
    "count": 0
  }
}
```

---

## Troubleshooting

### Phase Not Found

```bash
❌ Phase 'buildx' not found
```

**Solution**: Check available phases
```bash
ggen lifecycle list
```

### Command Failed

```bash
❌ Command failed: npm run build
```

**Solution**: Check phase details and run command directly
```bash
ggen lifecycle show build
npm run build  # Run directly to see error
```

### Cache Not Invalidating

```toml
[lifecycle.build]
command = "npm run build"
cache = true
```

**Solution**: Add outputs to track
```toml
[lifecycle.build]
command = "npm run build"
outputs = ["dist/"]  # Now invalidates when dist/ changes
cache = true
```

### Hooks Not Running

```toml
[hooks]
before_build = "test"  # Wrong: string instead of array
```

**Solution**: Use array syntax
```toml
[hooks]
before_build = ["test"]  # Correct: array
```

### State Reset Needed

```bash
# Phase history is wrong or corrupted
ggen state reset

# Or remove state file
rm .ggen/state.json
```

---

## Quick Tips

1. **Start simple** - Minimal make.toml, add features as needed
2. **Use descriptions** - Document what each phase does
3. **Add hooks** - Enforce quality gates (test before build)
4. **Track outputs** - Enable smart caching
5. **Test locally** - Run phases before CI/CD
6. **Use pipelines** - `ggen pipeline test lint build` instead of scripts
7. **Check state** - `ggen state` shows what's been run
8. **Clean often** - `ggen clean` removes tracked artifacts

---

## Related Documentation

- [LIFECYCLE_BEST_PRACTICES.md](./LIFECYCLE_BEST_PRACTICES.md) - Patterns and real examples
- [LIFECYCLE_SYSTEM_DESIGN.md](./LIFECYCLE_SYSTEM_DESIGN.md) - Architecture and design
- [examples/make.toml](../examples/make.toml) - Working example

---

## Command Cheat Sheet

```bash
# List
ggen lifecycle list              # List phases
ggen lifecycle show build        # Show phase details

# Run
ggen run dev                     # Run phase
ggen run build --env production  # With environment
ggen pipeline test build deploy  # Pipeline

# State
ggen state                       # Show state
ggen state history              # Show history
ggen state files                # Show generated files
ggen clean                      # Clean artifacts

# Help
ggen lifecycle --help           # CLI help
ggen run --help                 # Run command help
```
