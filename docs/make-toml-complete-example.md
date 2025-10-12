# Complete make.toml Example: The Rust Equivalent of package.json

## Vision

`make.toml` becomes to **all software projects** what `package.json` is to JavaScript projects - a universal project descriptor that any tool can understand.

## Real-World Example: Nuxt 4 + Rust API

### Project Structure

```
my-app/
├── make.toml              # ← The Universal Standard (like package.json)
├── ggen.toml              # ← Project config (like .npmrc or tsconfig.json)
├── .ggen/
│   ├── state.json         # ← Execution state (like .next/cache)
│   └── cache/
├── apps/
│   ├── web/               # Nuxt 4 frontend
│   │   ├── nuxt.config.ts
│   │   ├── components/
│   │   ├── composables/
│   │   └── pages/
│   └── api/               # Rust + Axum backend
│       ├── Cargo.toml
│       ├── src/
│       └── tests/
├── packages/
│   └── types/             # Shared TypeScript types
└── templates/
    ├── api-route.rs.tmpl
    ├── component.vue.tmpl
    └── composable.ts.tmpl
```

---

## make.toml - The Complete Example

```toml
# ============================================================================
# make.toml - Universal Project Descriptor
# ============================================================================
# This file is the "Rust equivalent of package.json" - a universal standard
# for describing any software project, regardless of language or framework.
# ============================================================================

[project]
name = "my-fullstack-app"
version = "1.0.0"
description = "Nuxt 4 frontend with Rust backend"
license = "MIT"
authors = ["Your Name <you@example.com>"]
repository = "https://github.com/you/my-app"

# ============================================================================
# Workspaces - Multi-language monorepo support
# ============================================================================
# Think: Cargo workspaces + npm workspaces + lerna, but universal

[workspace.frontend]
path = "apps/web"
runtime = "node:20.10.0"          # Specify exact Node version
package_manager = "pnpm"          # npm, yarn, pnpm, bun
framework = "nuxt"
framework_version = "4.0.0"
language = "typescript"

[workspace.backend]
path = "apps/api"
runtime = "rust:1.75.0"           # Specify exact Rust version
package_manager = "cargo"
framework = "axum"
framework_version = "0.7.0"
language = "rust"

[workspace.shared]
path = "packages/types"
runtime = "node:20.10.0"
language = "typescript"
description = "Shared types between frontend and backend"

# ============================================================================
# Dependencies - Universal dependency specification
# ============================================================================
# Unlike package.json (JS only) or Cargo.toml (Rust only),
# make.toml can specify dependencies for ANY language

[deps]
# System dependencies
node = ">=20.0.0"
pnpm = ">=8.0.0"
rust = ">=1.75.0"
cargo = ">=1.75.0"

# Optional dependencies
docker = { version = ">=24.0.0", optional = true }
kubectl = { version = ">=1.28.0", optional = true }

# ============================================================================
# Lifecycle Phases - Universal commands that work everywhere
# ============================================================================
# These are like npm scripts, but work across ALL languages and frameworks

[lifecycle.init]
description = "Initialize new project from scratch"
commands = [
    "ggen init:workspace frontend --template nuxt-4",
    "ggen init:workspace backend --template rust-axum",
    "ggen init:workspace shared --template ts-types",
    "ggen link:workspaces",
]
first_run_only = true

[lifecycle.setup]
description = "Install dependencies for all workspaces"
commands = [
    "cd apps/web && pnpm install",
    "cd apps/api && cargo fetch",
    "cd packages/types && pnpm install",
]
cache_key = [
    "apps/web/pnpm-lock.yaml",
    "apps/api/Cargo.lock",
    "packages/types/pnpm-lock.yaml",
]
parallel = true

[lifecycle.dev]
description = "Start development servers for all workspaces"
command = "ggen parallel:dev --workspaces frontend,backend"
watch = true
env_sync = true  # Share env vars between workspaces

[lifecycle.dev.workspaces.frontend]
command = "cd apps/web && nuxi dev"
port = 3000
watch = ["apps/web/**/*.vue", "apps/web/**/*.ts"]

[lifecycle.dev.workspaces.backend]
command = "cd apps/api && cargo watch -x run"
port = 4000
watch = ["apps/api/src/**/*.rs"]

[lifecycle.build]
description = "Build all workspaces in dependency order"
command = "ggen sequential:build --order shared,backend,frontend"

[lifecycle.build.workspaces.shared]
command = "cd packages/types && pnpm build"
outputs = ["packages/types/dist"]

[lifecycle.build.workspaces.backend]
command = "cd apps/api && cargo build --release"
outputs = ["apps/api/target/release/api"]

[lifecycle.build.workspaces.frontend]
command = "cd apps/web && nuxi build"
outputs = ["apps/web/.output"]

[lifecycle.test]
description = "Run tests for all workspaces"
parallel = true

[lifecycle.test.workspaces.frontend]
command = "cd apps/web && vitest run"

[lifecycle.test.workspaces.backend]
command = "cd apps/api && cargo test"

[lifecycle.lint]
description = "Lint all workspaces"
parallel = true

[lifecycle.lint.workspaces.frontend]
command = "cd apps/web && pnpm exec eslint ."

[lifecycle.lint.workspaces.backend]
command = "cd apps/api && cargo clippy -- -D warnings"

[lifecycle.format]
description = "Format all workspaces"
parallel = true

[lifecycle.format.workspaces.frontend]
command = "cd apps/web && pnpm exec prettier --write ."

[lifecycle.format.workspaces.backend]
command = "cd apps/api && cargo fmt"

[lifecycle.deploy]
description = "Deploy to production"
commands = [
    "ggen build --env production",
    "ggen deploy:backend",
    "ggen deploy:frontend",
]
requires_approval = true
env = { NODE_ENV = "production", RUST_LOG = "info" }

# ============================================================================
# Custom Phases - Project-specific commands
# ============================================================================

[lifecycle."generate:api"]
description = "Generate API endpoint in Rust + corresponding Nuxt composable"
commands = [
    "ggen gen api {{name}} --workspace backend",
    "ggen gen composable use{{name}} --workspace frontend",
    "ggen gen types {{name}}Types --workspace shared",
    "ggen sync:types",
]

[lifecycle."generate:component"]
description = "Generate Vue component with test"
command = "ggen gen component {{name}} --workspace frontend"

[lifecycle."sync:types"]
description = "Sync TypeScript types from Rust definitions"
commands = [
    "cd apps/api && cargo build --features typescript-export",
    "cp apps/api/target/types/*.ts packages/types/src/generated/",
]

# ============================================================================
# Hooks - Before/After any phase
# ============================================================================
# These are like git hooks, but for your build lifecycle

[hooks]
# Global hooks (run for all phases)
before_all = "ggen validate:env"
after_all = "ggen notify:complete"

# Phase-specific hooks
before_init = [
    "ggen check:system-deps",
    "ggen backup:config",
]

after_init = [
    "ggen setup:git",
    "ggen setup:vscode",
    "ggen setup:env",
]

before_dev = [
    "ggen check:ports 3000,4000",
    "ggen ensure:db-running",
]

after_dev = [
    "ggen cleanup:temp",
]

before_build = [
    "ggen test:unit",
    "ggen lint",
    "ggen sync:types",
]

after_build = [
    "ggen analyze:bundle",
    "ggen optimize:assets",
    "ggen generate:docs",
]

before_deploy = [
    "ggen test:e2e",
    "ggen check:migrations",
    "ggen backup:db",
]

after_deploy = [
    "ggen smoke:test",
    "ggen notify:slack",
    "ggen tag:release {{version}}",
]

# Error handling
on_error = "ggen rollback:last"
on_success = "ggen celebrate"

# ============================================================================
# Hooks for Custom Phases
# ============================================================================

[hooks."generate:api"]
before = [
    "ggen validate:api-name {{name}}",
    "ggen check:duplicate apps/api/src/routes/{{name}}.rs",
]
after = [
    "ggen format apps/api/src/routes/{{name}}.rs",
    "ggen format apps/web/composables/use{{name}}.ts",
    "ggen test:generate apps/api/tests/{{name}}_test.rs",
]

# ============================================================================
# Environment Configuration
# ============================================================================

[env.development]
API_URL = "http://localhost:4000"
DATABASE_URL = "postgres://localhost/myapp_dev"
RUST_LOG = "debug"
NODE_ENV = "development"

[env.staging]
API_URL = "https://staging-api.example.com"
DATABASE_URL = "postgres://staging-db.example.com/myapp"
RUST_LOG = "info"
NODE_ENV = "production"

[env.production]
API_URL = "https://api.example.com"
DATABASE_URL = "postgres://prod-db.example.com/myapp"
RUST_LOG = "warn"
NODE_ENV = "production"

# ============================================================================
# State Management
# ============================================================================

[state]
file = ".ggen/state.json"
track_generated = true
track_migrations = true

# Cache configuration
cache_dir = ".ggen/cache"
cache_ttl = "7d"  # Cache valid for 7 days

# Idempotency
skip_if_exists = [
    "node_modules",
    "apps/api/target",
    "apps/web/.nuxt",
]

# Force re-run if these files change
force_rerun_if_changed = [
    "make.toml",
    "ggen.toml",
    "apps/web/package.json",
    "apps/api/Cargo.toml",
]

# ============================================================================
# Scripts - Shortcuts that compose lifecycle phases
# ============================================================================
# Think: npm scripts, but universal and composable

[scripts]
# Development shortcuts
start = "ggen dev"
dev = "ggen dev"

# Build shortcuts
build = "ggen build"
build:prod = "ggen build --env production"

# Test shortcuts
test = "ggen test"
test:unit = "ggen test --filter unit"
test:e2e = "ggen test --filter e2e"
test:watch = "ggen test --watch"

# Deployment shortcuts
deploy = "ggen deploy --env production"
deploy:staging = "ggen deploy --env staging"

# Maintenance
clean = "ggen clean"
clean:all = "ggen clean && rm -rf node_modules apps/api/target"
reset = "ggen clean:all && ggen setup"

# Type safety
types:sync = "ggen sync:types"
types:check = "ggen check:types"

# Release
release = "ggen test && ggen build && ggen deploy"
release:major = "ggen version:bump major && ggen release"
release:minor = "ggen version:bump minor && ggen release"
release:patch = "ggen version:bump patch && ggen release"

# ============================================================================
# CI/CD Configuration
# ============================================================================

[ci]
provider = "github-actions"
on = ["push", "pull_request"]

[ci.stages]
lint = { phase = "lint", required = true }
test = { phase = "test", required = true }
build = { phase = "build", required = true }
deploy = { phase = "deploy", required = false, branches = ["main"] }

# ============================================================================
# Metadata - RDF semantic data (optional but powerful)
# ============================================================================

[rdf]
base = "http://example.org/my-app/"

[rdf.prefixes]
app = "http://example.org/my-app/"
schema = "http://schema.org/"
api = "http://example.org/my-app/api/"

[rdf.triples]
inline = [
    "app:MyApp a schema:SoftwareApplication ;",
    "  schema:name 'My Fullstack App' ;",
    "  schema:version '1.0.0' ;",
    "  schema:programmingLanguage 'Rust', 'TypeScript' ;",
    "  app:hasWorkspace app:Frontend, app:Backend .",
    "",
    "app:Frontend a app:Workspace ;",
    "  schema:name 'web' ;",
    "  app:framework 'nuxt' ;",
    "  app:runtime 'node:20' .",
    "",
    "app:Backend a app:Workspace ;",
    "  schema:name 'api' ;",
    "  app:framework 'axum' ;",
    "  app:runtime 'rust:1.75' .",
]
```

---

## Templates Stay Simple

### Component Template (Simple!)

```yaml
---
to: "apps/web/components/{{name}}.vue"
sh_before: "ggen hooks:before generate:component {{name}}"
sh_after: "ggen hooks:after generate:component {{name}}"
---
<template>
  <div class="{{name | kebabCase}}">
    <h1>{{ title }}</h1>
    <slot />
  </div>
</template>

<script setup lang="ts">
interface Props {
  title: string
}

defineProps<Props>()
</script>

<style scoped>
.{{name | kebabCase}} {
  padding: 1rem;
}
</style>
```

### Rust API Route Template (Simple!)

```yaml
---
to: "apps/api/src/routes/{{name}}.rs"
sh_before: "ggen hooks:before generate:api {{name}}"
sh_after: "cargo fmt {{output}}"
---
use axum::{
    extract::{Path, State},
    http::StatusCode,
    Json,
};
use serde::{Deserialize, Serialize};

use crate::AppState;

#[derive(Debug, Serialize, Deserialize)]
pub struct {{name}}Response {
    pub id: String,
    pub data: String,
}

pub async fn get_{{name | snake_case}}(
    State(state): State<AppState>,
    Path(id): Path<String>,
) -> Result<Json<{{name}}Response>, StatusCode> {
    // Implementation
    Ok(Json({{name}}Response {
        id,
        data: "Hello from Rust!".to_string(),
    }))
}
```

---

## Developer Workflows

### Initialize Project

```bash
ggen init my-app --template fullstack-rust
cd my-app
ggen setup
```

### Development

```bash
# Start both frontend and backend
ggen dev

# Or start individually
ggen dev --workspace frontend
ggen dev --workspace backend
```

### Generate Code Across Languages

```bash
# Generate API endpoint + composable + types in one command
ggen generate:api users

# This creates:
# - apps/api/src/routes/users.rs (Rust)
# - apps/web/composables/useUsers.ts (TypeScript)
# - packages/types/src/users.ts (TypeScript types from Rust)
```

### Build

```bash
# Build everything in correct order
ggen build

# Build for production
ggen build --env production
```

### Deploy

```bash
# Deploy to staging
ggen deploy --env staging

# Deploy to production (requires approval)
ggen deploy --env production
```

---

## Why This Changes Everything

### 1. **Cross-Language Projects Made Easy**

```bash
# Before ggen:
cd frontend && npm run dev &
cd backend && cargo run &
# Hope they're in sync...

# With ggen:
ggen dev
# Both start, types synced, hot reload works
```

### 2. **Type Safety Across Languages**

```rust
// Rust backend (apps/api/src/models.rs)
#[derive(Serialize, TypeScript)]
pub struct User {
    pub id: Uuid,
    pub email: String,
    pub created_at: DateTime<Utc>,
}
```

```bash
ggen sync:types
```

```typescript
// Generated TypeScript (packages/types/src/generated/user.ts)
export interface User {
  id: string;
  email: string;
  created_at: Date;
}
```

### 3. **One Command, All Languages**

```bash
ggen test   # Runs Rust tests + Vitest + Playwright
ggen lint   # Runs Clippy + ESLint
ggen format # Runs cargo fmt + Prettier
ggen build  # Builds Rust binary + Nuxt bundle
```

### 4. **Framework Agnostic**

Same `make.toml` structure works for:
- Nuxt + Rust
- Next.js + Go
- Rails + Vue
- Django + React
- Any combination!

---

## The Path to Universal Adoption

### Phase 1: ggen Community (Now)
- Ship lifecycle system
- Prove it works with Nuxt + Rust
- Build template marketplace

### Phase 2: Framework Communities (3-6 months)
- Nuxt team evaluates
- Community templates proliferate
- Conference talks, blog posts

### Phase 3: Mainstream (6-12 months)
- IDE support (VS Code, IntelliJ)
- CI/CD providers support make.toml
- Major frameworks consider adoption

### Phase 4: Standard (12-24 months)
- Frameworks ship with make.toml by default
- Teaching materials reference it
- Expected like package.json or Cargo.toml

---

## Conclusion

`make.toml` is not just another config file. It's **the universal project descriptor** that:

- Works with **any language** (Rust, JavaScript, Python, Go, etc.)
- Works with **any framework** (Nuxt, Next, Rails, Django, etc.)
- Provides **one mental model** for all projects
- Enables **cross-language type safety**
- Makes **monorepos natural**
- Is **AI-understandable** through RDF metadata

Just as `package.json` standardized JavaScript projects, `make.toml` can standardize **all software projects**.

The future is cross-language, and `make.toml` is the interface.
