# ggen Lifecycle System: Universal Framework Standard

## Vision

Transform ggen from a template generator into **the universal lifecycle standard for all software frameworks** - think of it as "package.json + Makefile + docker-compose + framework.toml" unified into one semantic, composable system.

## The Problem: Framework Fragmentation

Every framework has its own lifecycle:
- **Nuxt**: `nuxi init`, `nuxi dev`, `nuxi build`, `nuxi generate`
- **Next.js**: `next dev`, `next build`, `next start`
- **Rails**: `rails new`, `rails server`, `rails console`, `rails db:migrate`
- **Django**: `django-admin startproject`, `python manage.py runserver`, `python manage.py migrate`
- **Rust**: `cargo new`, `cargo build`, `cargo test`, `cargo run`
- **Go**: `go mod init`, `go build`, `go test`, `go run`

**Current Reality**: Developers must learn different commands, configuration formats, and mental models for each framework.

**ggen Solution**: One universal lifecycle system that adapts to any framework through semantic templates.

---

## Core Concepts

### 1. Lifecycle Phases (Universal Across Frameworks)

```toml
[lifecycle]
# Standard phases - every framework must implement these
phases = [
    "init",        # First-time project creation
    "setup",       # Install dependencies, configure environment
    "dev",         # Development mode (hot reload, debug)
    "build",       # Production build
    "test",        # Run tests
    "lint",        # Code quality checks
    "format",      # Code formatting
    "deploy",      # Deployment
    "clean",       # Cleanup artifacts
    "docs",        # Generate documentation
    "migrate",     # Data/schema migrations
    "seed",        # Seed data
    "console",     # Interactive REPL
    "upgrade",     # Upgrade dependencies/framework
]

# Custom phases (framework-specific)
custom_phases = ["generate:component", "generate:page", "analyze:bundle"]
```

### 2. Hooks System (Before/After Any Phase)

```toml
[hooks]
# Global hooks (run for all phases)
before_all = "ggen validate:env"
after_all = "ggen notify:complete"

# Phase-specific hooks
before_init = ["ggen check:dependencies", "ggen backup:config"]
after_init = ["ggen setup:git", "ggen setup:ide"]

before_dev = "ggen check:ports"
after_dev = "ggen cleanup:temp"

before_build = ["ggen test:unit", "ggen lint"]
after_build = ["ggen analyze:bundle", "ggen optimize:assets"]

before_deploy = ["ggen test:e2e", "ggen check:migrations"]
after_deploy = ["ggen smoke:test", "ggen notify:slack"]

# Conditional hooks
on_error = "ggen rollback:deploy"
on_success = "ggen tag:release"
```

### 3. State Management (Track What's Been Done)

```toml
[state]
# Track lifecycle state
file = ".ggen/state.json"  # { "last_phase": "build", "migrations": ["001", "002"], ... }

# Idempotency - skip if already done
skip_if_exists = [
    "node_modules",
    ".next/cache",
    "dist",
]

# Re-run triggers
force_rerun_if_changed = [
    "package.json",
    "ggen.toml",
    "tsconfig.json",
]

# Cleanup tracking
track_generated_files = true  # Auto-cleanup on ggen clean
```

### 4. Environment Detection & Adaptation

```toml
[env]
# Detect environment and adapt behavior
detect = "auto"  # or "development", "staging", "production"

[env.development]
phases.dev.command = "nuxi dev --host 0.0.0.0"
phases.dev.watch = true
phases.build.skip = true  # Skip production build in dev

[env.staging]
phases.build.command = "nuxi build"
phases.deploy.target = "staging.example.com"

[env.production]
phases.build.command = "nuxi build --analyze"
phases.build.optimize = true
phases.deploy.target = "example.com"
phases.deploy.strategy = "blue-green"
```

---

## Template Frontmatter Enhancement

### Current Frontmatter (Hygen-style)

```yaml
---
to: src/components/{{name}}.vue
---
<template>
  <div>{{ name }}</div>
</template>
```

### Enhanced Frontmatter with Lifecycle

```yaml
---
# File generation
to: "src/components/{{name}}.vue"

# Lifecycle integration
lifecycle:
  phase: "generate:component"

  # Hooks for this specific template
  before:
    - "ggen validate:name '{{name}}'"
    - "ggen check:duplicate src/components/{{name}}.vue"

  after:
    - "ggen add:export src/components/index.ts {{name}}"
    - "ggen test:generate tests/{{name}}.spec.ts"
    - "ggen format src/components/{{name}}.vue"

  # State tracking
  track: true
  category: "components"

  # Dependencies - run these phases first
  requires: ["init", "setup"]

  # Triggers - run these after
  triggers: ["build", "test"]

# RDF metadata for discoverability
rdf_inline:
  - "@prefix ex: <http://example.org/> ."
  - "ex:{{name}} a ex:Component ;"
  - "  ex:framework 'nuxt' ;"
  - "  ex:version '4.0' ;"
  - "  ex:author '{{author}}' ."
---
<template>
  <div class="{{name | kebabCase}}">
    {{ name }}
  </div>
</template>
```

---

## make.toml Integration

### Project-Level make.toml

```toml
# make.toml - The Makefile of the future
[project]
name = "my-nuxt-app"
framework = "nuxt"
version = "4.0"

[lifecycle.init]
description = "Initialize new Nuxt 4 project"
commands = [
    "nuxi init {{project_name}}",
    "cd {{project_name}}",
    "ggen init nuxt-4",
]
first_run_only = true

[lifecycle.setup]
description = "Install dependencies and configure"
commands = [
    "pnpm install",
    "ggen setup:env",
    "ggen setup:vscode",
]
cache_key = "package.json|pnpm-lock.yaml"

[lifecycle.dev]
description = "Start development server"
command = "nuxi dev"
watch = true
port = 3000
env = { NODE_ENV = "development" }

[lifecycle.build]
description = "Build for production"
commands = [
    "nuxi build",
    "ggen optimize:bundle",
]
outputs = [".output"]
cache = true

[lifecycle.test]
description = "Run tests"
commands = [
    "vitest run",
    "playwright test",
]
parallel = true

[lifecycle.deploy]
description = "Deploy to production"
commands = [
    "ggen pre-deploy:checks",
    "nuxi build",
    "ggen deploy:vercel",
]
requires_approval = true
rollback_command = "ggen rollback:deploy"

# Custom phases
[lifecycle."generate:component"]
description = "Generate Vue component"
command = "ggen gen component {{name}}"
template = "templates/component.vue.tmpl"

[lifecycle."generate:page"]
description = "Generate Nuxt page"
command = "ggen gen page {{path}}"
template = "templates/page.vue.tmpl"

# Hooks
[hooks]
before_deploy = ["test", "lint", "build"]
after_deploy = ["smoke-test", "notify"]

# Dependencies
[deps]
node = ">=20.0.0"
pnpm = ">=8.0.0"
nuxt = "^4.0.0"

# Scripts (like package.json scripts but better)
[scripts]
# These are shortcuts that can compose lifecycle phases
start = "ggen dev"
release = "ggen build && ggen test && ggen deploy"
clean-all = "ggen clean && rm -rf node_modules .nuxt"
```

---

## ggen.toml Project Configuration

### Enhanced Project Configuration

```toml
# ggen.toml - Per-project configuration
[project]
name = "my-app"
framework = "nuxt"
framework_version = "4.0"
ggen_version = "1.2.0"

[project.paths]
templates = "templates"
generated = "src"
state = ".ggen"
cache = ".ggen/cache"

[project.lifecycle]
# Override default lifecycle commands
dev.command = "nuxi dev --host 0.0.0.0 --port 3001"
build.outputs = [".output", "dist"]
test.parallel = true
test.coverage = true

[project.hooks]
# Project-specific hooks
before_commit = ["lint", "test:unit"]
before_push = ["test:all", "build"]

[project.state]
# Track what's been generated
track_generated = true
migrations = ["001_init", "002_add_auth"]
last_phase = "build"
last_run = "2025-01-10T12:00:00Z"

[project.env]
# Environment variables
development.API_URL = "http://localhost:4000"
staging.API_URL = "https://staging-api.example.com"
production.API_URL = "https://api.example.com"

[project.templates]
# Template discovery and organization
scan_dirs = ["templates", ".ggen/templates"]
auto_import = true

[project.rdf]
# Semantic metadata
base = "http://example.org/project/"
prefixes = { ex = "http://example.org/", schema = "http://schema.org/" }
inline = [
    "ex:MyApp a schema:SoftwareApplication ;",
    "  schema:name '{{project.name}}' ;",
    "  schema:version '{{project.version}}' .",
]
```

---

## Universal Framework Adaptation

### How ggen Adapts to Any Framework

```toml
# templates/frameworks/nuxt-4.toml
[framework]
name = "nuxt"
version = "4.0"
type = "vue-meta-framework"

[framework.lifecycle.init]
command = "nuxi init {{name}}"
template = "templates/nuxt-4/init.tmpl"

[framework.lifecycle.dev]
command = "nuxi dev"
port = 3000
hot_reload = true

[framework.lifecycle.build]
command = "nuxi build"
outputs = [".output"]

[framework.generators]
# Framework-specific generators
component = "templates/nuxt-4/component.vue.tmpl"
page = "templates/nuxt-4/page.vue.tmpl"
layout = "templates/nuxt-4/layout.vue.tmpl"
composable = "templates/nuxt-4/composable.ts.tmpl"
middleware = "templates/nuxt-4/middleware.ts.tmpl"
plugin = "templates/nuxt-4/plugin.ts.tmpl"
```

### Multi-Framework Project Support

```toml
# ggen.toml for full-stack project
[project]
name = "my-fullstack-app"
type = "monorepo"

[project.workspaces]
frontend = { path = "apps/web", framework = "nuxt", version = "4.0" }
backend = { path = "apps/api", framework = "rust", version = "axum" }
mobile = { path = "apps/mobile", framework = "react-native", version = "0.73" }

[lifecycle.dev]
# Run all dev servers in parallel
command = "ggen parallel:dev"
workspaces = ["frontend", "backend"]

[lifecycle.build]
# Build in dependency order
command = "ggen sequential:build"
order = ["backend", "frontend", "mobile"]

[lifecycle.deploy]
# Deploy with coordination
strategy = "blue-green"
workspaces = ["backend", "frontend"]
health_checks = true
```

---

## State Management & Idempotency

### .ggen/state.json

```json
{
  "project": {
    "name": "my-app",
    "created_at": "2025-01-01T00:00:00Z",
    "last_modified": "2025-01-10T12:00:00Z"
  },
  "lifecycle": {
    "phases_run": ["init", "setup", "dev", "build"],
    "last_phase": "build",
    "last_phase_at": "2025-01-10T11:30:00Z",
    "phase_history": [
      {
        "phase": "init",
        "timestamp": "2025-01-01T00:00:00Z",
        "duration_ms": 15000,
        "success": true
      },
      {
        "phase": "build",
        "timestamp": "2025-01-10T11:30:00Z",
        "duration_ms": 45000,
        "success": true,
        "outputs": [".output/", "dist/"]
      }
    ]
  },
  "generated": {
    "files": [
      {
        "path": "src/components/Button.vue",
        "template": "templates/component.vue.tmpl",
        "generated_at": "2025-01-05T10:00:00Z",
        "vars": { "name": "Button" },
        "hash": "abc123..."
      }
    ],
    "count": 42
  },
  "migrations": {
    "applied": ["001_init", "002_add_auth", "003_add_components"],
    "pending": ["004_add_api"]
  },
  "env": "development",
  "cache": {
    "package.json": "hash123...",
    "ggen.toml": "hash456..."
  }
}
```

---

## CLI Commands

### Lifecycle Commands

```bash
# List available phases
ggen lifecycle list
ggen lifecycle:list

# Run a phase
ggen run init
ggen run:init
ggen init  # shorthand

# Run multiple phases
ggen run init setup dev
ggen pipeline init setup dev

# Run with environment
ggen run build --env production
ggen run:build --env staging

# Force re-run (ignore cache)
ggen run build --force
ggen run build --no-cache

# Dry run (show what would execute)
ggen run deploy --dry-run

# Show phase details
ggen lifecycle info build
ggen lifecycle show build

# Show hooks
ggen hooks list
ggen hooks show before_deploy
```

### State Commands

```bash
# Show current state
ggen state
ggen state show

# Show generated files
ggen state files
ggen generated

# Show lifecycle history
ggen state history
ggen lifecycle history

# Reset state
ggen state reset
ggen state reset --phase build

# Clean generated files
ggen clean
ggen clean --all
ggen clean --phase build
```

### Framework Commands

```bash
# Detect framework
ggen framework detect

# Show framework lifecycle
ggen framework show nuxt

# List supported frameworks
ggen framework list

# Add framework support
ggen framework add svelte-5

# Update framework definitions
ggen framework update
```

---

## Why This Matters

### 1. **Developer Experience**
- **One mental model** for all frameworks
- **Consistent commands** across projects
- **Discoverability** through `ggen lifecycle list`
- **Self-documenting** via make.toml

### 2. **Framework Adoption**
- **Lower barrier to entry** - same commands for new framework
- **Transfer knowledge** between frameworks
- **Polyglot teams** use same tooling

### 3. **Automation & CI/CD**
- **Standard interface** for build pipelines
- **Framework-agnostic** CI configuration
- **Reproducible builds** through state tracking
- **Rollback capability** built-in

### 4. **Semantic Understanding**
- **RDF metadata** makes projects queryable
- **Dependency graphs** between components
- **Impact analysis** for changes
- **Architecture documentation** generated automatically

---

## Implementation Plan

### Phase 1: Core Lifecycle System (1.2.0)
- [ ] Define lifecycle phases schema
- [ ] Implement hooks system
- [ ] Add state tracking (.ggen/state.json)
- [ ] Create make.toml parser
- [ ] Build lifecycle runner

### Phase 2: Framework Integration (1.3.0)
- [ ] Add Nuxt 4 lifecycle
- [ ] Add Next.js 15 lifecycle
- [ ] Add Rust/Cargo lifecycle
- [ ] Add Python/Poetry lifecycle
- [ ] Framework detection

### Phase 3: Advanced Features (1.4.0)
- [ ] Monorepo support
- [ ] Multi-environment management
- [ ] Cache optimization
- [ ] Parallel execution
- [ ] Rollback system

### Phase 4: Ecosystem (1.5.0)
- [ ] Template marketplace with lifecycle
- [ ] Community framework definitions
- [ ] IDE integrations
- [ ] GitHub Actions integration
- [ ] Visual lifecycle designer

---

## Example: Nuxt 4 Project Lifecycle

```bash
# 1. Initialize new Nuxt 4 project
ggen init my-app --framework nuxt --version 4.0

# 2. Setup (installs deps, configures IDE)
ggen setup

# 3. Generate components
ggen generate:component Button
ggen gen:page about
ggen gen:composable useAuth

# 4. Development
ggen dev  # Runs hooks, starts server, watches files

# 5. Testing
ggen test  # Runs unit + e2e

# 6. Build
ggen build  # Runs pre-build hooks, builds, analyzes

# 7. Deploy
ggen deploy --env production  # Checks, builds, deploys, verifies

# 8. Rollback if needed
ggen rollback  # Uses state to revert

# 9. Clean up
ggen clean  # Removes build artifacts, tracked by state
```

---

## Conclusion

This lifecycle system transforms ggen from a template tool into **the universal standard for software project management**. It's:

- **Framework-agnostic** - works with any framework
- **Semantic** - understands what it's doing through RDF
- **State-aware** - tracks history, enables rollbacks
- **Composable** - phases, hooks, and templates work together
- **Developer-friendly** - consistent, discoverable, self-documenting

Just as `package.json` became the standard for JavaScript projects, `make.toml` + `ggen.toml` can become the standard for **all software projects**, regardless of language or framework.

---

## Additional Resources

### For Implementation

- **[Lifecycle Best Practices](LIFECYCLE_BEST_PRACTICES.md)** - Rust implementation patterns, design patterns, and code guidelines
- **[Lifecycle Team Workflow](LIFECYCLE_TEAM_WORKFLOW.md)** - Daily development workflows and team collaboration
- **[Lifecycle Code Review](LIFECYCLE_CODE_REVIEW.md)** - Comprehensive code review analysis
- **[Lifecycle Performance Analysis](LIFECYCLE_PERFORMANCE_ANALYSIS.md)** - Performance optimization guide

### For Usage

- **[Quick Reference Guide](LIFECYCLE_QUICK_REFERENCE.md)** - Commands and common patterns cheat sheet
- **[Lifecycle README](LIFECYCLE_README.md)** - Getting started overview

### Related Concepts

- **[Graph-Driven Development](GRAPH_DRIVEN_NUXT_GENERATION.md)** - Template integration patterns
- **[Marketplace Guide](MARKETPLACE_HOWTO_CLI_PROJECT.md)** - Package system and distribution

---

## Document Reading Order

**For Core Team Members:**
1. Read this document (SYSTEM_DESIGN.md) first - understand the vision
2. Then read [BEST_PRACTICES.md](LIFECYCLE_BEST_PRACTICES.md) - learn Rust patterns
3. Then read [TEAM_WORKFLOW.md](LIFECYCLE_TEAM_WORKFLOW.md) - apply to daily work
4. Keep [QUICK_REFERENCE.md](LIFECYCLE_QUICK_REFERENCE.md) handy - command lookup

**For End Users:**
1. Start with [LIFECYCLE_README.md](LIFECYCLE_README.md) - overview
2. Use [QUICK_REFERENCE.md](LIFECYCLE_QUICK_REFERENCE.md) - commands
3. Optional: Read [SYSTEM_DESIGN.md](LIFECYCLE_SYSTEM_DESIGN.md) - understand philosophy
