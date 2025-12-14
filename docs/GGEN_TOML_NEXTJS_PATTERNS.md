<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen.toml Configuration Patterns from Next.js](#ggentoml-configuration-patterns-from-nextjs)
  - [Overview](#overview)
  - [Side-by-Side Pattern Comparison](#side-by-side-pattern-comparison)
    - [1. Environment-Specific Configuration](#1-environment-specific-configuration)
    - [2. Build/Generation Phase Hooks](#2-buildgeneration-phase-hooks)
    - [3. Workspace Dependencies (Monorepo Pattern)](#3-workspace-dependencies-monorepo-pattern)
    - [4. Performance Optimization Configuration](#4-performance-optimization-configuration)
    - [5. Module Configuration Sections](#5-module-configuration-sections)
  - [Configuration Philosophy Comparison](#configuration-philosophy-comparison)
    - [Next.js Configuration Philosophy](#nextjs-configuration-philosophy)
    - [ggen.toml Configuration Philosophy](#ggentoml-configuration-philosophy)
  - [Translation Table: Next.js ↔ ggen.toml](#translation-table-nextjs--ggentoml)
  - [Unified Command Pattern (Package.json Scripts)](#unified-command-pattern-packagejson-scripts)
  - [Key Takeaways](#key-takeaways)
    - [What ggen.toml Adopted from Next.js](#what-ggentoml-adopted-from-nextjs)
    - [What ggen.toml Adapted for Rust](#what-ggentoml-adapted-for-rust)
    - [What ggen.toml Skipped (Not Applicable)](#what-ggentoml-skipped-not-applicable)
  - [Benefits of This Approach](#benefits-of-this-approach)
    - [For JavaScript Developers Coming to Rust](#for-javascript-developers-coming-to-rust)
    - [For Rust Developers](#for-rust-developers)
    - [For ggen Users](#for-ggen-users)
  - [Future Enhancements (Optional)](#future-enhancements-optional)
    - [Could Add (80/20 high value):](#could-add-8020-high-value)
    - [Could Skip (Low value, high complexity):](#could-skip-low-value-high-complexity)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen.toml Configuration Patterns from Next.js

**Status**: Documentation of configuration architecture
**Goal**: Show how ggen.toml adopts proven patterns from Next.js ecosystem

---

## Overview

`ggen.toml` follows modern configuration patterns proven in Next.js and JavaScript ecosystem, adapted for Rust CLI tooling. This document maps ggen.toml sections to their Next.js equivalents.

---

## Side-by-Side Pattern Comparison

### 1. Environment-Specific Configuration

**Next.js Pattern** (`next.config.ts` + env files):
```typescript
// next.config.ts
const nextConfig: NextConfig = {
  output: process.env.NODE_ENV === 'production' ? 'standalone' : undefined,
  eslint: {
    ignoreDuringBuilds: process.env.CI === 'true',
  },
};

// .env.development
NEXT_PUBLIC_API_URL=http://localhost:3000

// .env.production
NEXT_PUBLIC_API_URL=https://api.example.com
```

**ggen.toml Equivalent**:
```toml
[ai]
provider = "anthropic"
model = "claude-3-opus-20240229"
temperature = 0.5

# Development environment - faster, cheaper models
[env.development]
"ai.model" = "claude-3-haiku-20240307"  # Like Next.js dev optimizations
"ai.temperature" = 0.9
"logging.level" = "debug"
"performance.max_workers" = 4

# CI environment - local, no API costs
[env.ci]
"ai.provider" = "ollama"  # Like Next.js CI optimizations
"ai.model" = "llama2"
"ai.base_url" = "http://localhost:11434"
"logging.level" = "warn"

# Production environment - deterministic, optimized
[env.production]
"ai.temperature" = 0.3  # Like Next.js production mode
"logging.level" = "warn"
"performance.max_workers" = 16
```

**Key Insight**: ggen uses TOML sections for environment overrides instead of separate .env files, but achieves the same goal: environment-specific configuration without code changes.

---

### 2. Build/Generation Phase Hooks

**Next.js Pattern** (`next.config.ts`):
```typescript
const nextConfig: NextConfig = {
  // Pre-build validation
  eslint: {
    ignoreDuringBuilds: false,  // Run linting before build
  },
  typescript: {
    ignoreBuildErrors: false,  // Type-check before build
  },

  // Build-time configuration
  webpack: (config, { isServer }) => {
    // Custom webpack transformations during build
    config.resolve.alias = { ...config.resolve.alias, canvas: false };
    return config;
  },

  // Post-build optimization
  output: "standalone",  // Docker-ready output after build
};
```

**ggen.toml Equivalent**:
```toml
[lifecycle]
enabled = true
config_file = ".ggen/lifecycle.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

# Pre-generation: validate before generating
[lifecycle.phases.pre_generate]
scripts = [
    "scripts/validate-docs/validate-all.sh"  # Like Next.js pre-build checks
]

# Post-generation: format and optimize after generating
[lifecycle.phases.post_generate]
scripts = [
    "scripts/format-docs.sh"  # Like Next.js post-build optimizations
]
```

**Key Insight**: ggen's lifecycle hooks parallel Next.js build phases (pre-build validation, post-build optimization), but for code generation instead of compilation.

---

### 3. Workspace Dependencies (Monorepo Pattern)

**Next.js Pattern** (`apps/web/package.json`):
```json
{
  "name": "@astro/web",
  "dependencies": {
    "@astro/db-schema": "workspace:*",      // Workspace package
    "@astro/hooks": "workspace:*",          // Workspace package
    "@astro/services": "workspace:*",       // Workspace package
    "next": "15.5.7",                       // External package
    "react": "19.2.1"                       // External package
  }
}
```

**Next.js Config Transpilation**:
```typescript
const nextConfig: NextConfig = {
  transpilePackages: [
    "@astro/db",          // Must transpile workspace packages
    "@astro/hooks",
    "@astro/services",
  ],
};
```

**ggen Equivalent** (Rust workspace pattern):
```toml
# Cargo.toml (root)
[workspace]
members = [
  "crates/ggen-utils",       # Like workspace:* packages
  "crates/ggen-cli",
  "crates/ggen-domain",
  "crates/ggen-core",
]

# crates/ggen-cli/Cargo.toml
[dependencies]
ggen-utils = { path = "../ggen-utils" }    # Like workspace:*
ggen-domain = { path = "../ggen-domain" }  # Like workspace:*
serde = "1.0"                              # Like external npm package
```

**Key Insight**: Both use workspace references for internal packages and explicit versions for external dependencies. Next.js requires explicit transpilation; Rust handles this automatically via Cargo.

---

### 4. Performance Optimization Configuration

**Next.js Pattern**:
```typescript
const nextConfig: NextConfig = {
  // Performance: standalone output for Docker
  output: "standalone",
  outputFileTracingRoot: path.join(__dirname, "../../"),

  // Performance: Turbopack optimizations
  turbopack: {
    resolveAlias: { canvas: "./empty-module.js" },
  },

  // Performance: external packages (don't bundle server-side)
  serverExternalPackages: [
    "@opentelemetry/api",
    "@opentelemetry/sdk-trace-base",
  ],
};
```

**ggen.toml Equivalent**:
```toml
[performance]
parallel_generation = true     # Like Next.js parallel compilation
max_workers = 8                # Like Next.js worker threads
cache_templates = true         # Like Next.js build cache
incremental_build = true       # Like Next.js incremental builds

# Production optimizations
[env.production]
"performance.max_workers" = 16              # Scale up in production
"sparql.cache_ttl" = 86400                  # 24 hour cache (like Next.js ISR)
```

**Key Insight**: Both frameworks optimize for production with caching, parallelization, and incremental processing.

---

### 5. Module Configuration Sections

**Next.js Pattern** (single file, multiple concerns):
```typescript
const nextConfig: NextConfig = {
  // Output configuration
  output: "standalone",

  // Build tool configuration
  eslint: { ignoreDuringBuilds: false },
  typescript: { ignoreBuildErrors: false },

  // Bundler configuration
  webpack: (config) => { ... },
  turbopack: { resolveAlias: { ... } },

  // External packages
  serverExternalPackages: [...],
  transpilePackages: [...],
};
```

**ggen.toml Equivalent** (single file, multiple sections):
```toml
# Project metadata
[project]
name = "ggen"
version = "4.0.0"

# AI provider configuration
[ai]
provider = "anthropic"
model = "claude-3-opus-20240229"

# Template system configuration
[templates]
directory = "templates"
output_directory = "generated"
idempotent = true

# RDF/SPARQL configuration
[rdf]
base_uri = "https://ggen.dev/"
default_format = "turtle"

[sparql]
timeout = 60
cache_enabled = true

# Lifecycle hooks
[lifecycle]
enabled = true

# Performance tuning
[performance]
parallel_generation = true
max_workers = 8

# Logging
[logging]
level = "info"
format = "pretty"

# Security
[security]
allowed_domains = [...]
validate_ssl = true
```

**Key Insight**: Both use structured configuration with named sections for different concerns, keeping related settings together.

---

## Configuration Philosophy Comparison

### Next.js Configuration Philosophy

1. **Single Source of Truth**: One `next.config.ts` file
2. **Environment Overrides**: `.env.*` files for environment-specific values
3. **Type Safety**: TypeScript configuration with `NextConfig` type
4. **Composability**: Export function for dynamic config
5. **Convention over Configuration**: Sensible defaults, explicit overrides

### ggen.toml Configuration Philosophy

1. **Single Source of Truth**: One `ggen.toml` file
2. **Environment Overrides**: `[env.*]` sections within same file
3. **Schema Validation**: TOML format with strict parsing
4. **Composability**: Section overrides merge with base config
5. **Convention over Configuration**: Sensible defaults, explicit overrides

**Alignment**: Both philosophies prioritize simplicity, type safety, and environment-specific configuration without code duplication.

---

## Translation Table: Next.js ↔ ggen.toml

| Next.js Concept | Next.js Implementation | ggen.toml Equivalent | Notes |
|-----------------|------------------------|----------------------|-------|
| Environment config | `.env.development`, `.env.production` | `[env.development]`, `[env.production]` | ggen uses TOML sections |
| Build hooks | `webpack`, `turbopack` callbacks | `[lifecycle.phases.*]` | ggen uses shell scripts |
| Workspace packages | `workspace:*` in package.json | `path = "../crate"` in Cargo.toml | Rust native feature |
| Transpilation list | `transpilePackages: [...]` | Automatic via Cargo workspace | Rust handles automatically |
| Output mode | `output: "standalone"` | N/A (CLI binary) | Different output targets |
| Parallel builds | Turbopack/webpack workers | `performance.max_workers` | Both use parallelization |
| Caching | Next.js build cache | `cache_templates`, `sparql.cache_enabled` | Both cache expensive operations |
| Incremental builds | Next.js incremental mode | `incremental_build = true` | Both support incremental generation |
| External packages | `serverExternalPackages` | Cargo dependency resolution | Different packaging systems |
| Linting pre-build | `eslint.ignoreDuringBuilds` | `lifecycle.phases.pre_generate` | ggen more flexible (any script) |
| Type checking | `typescript.ignoreBuildErrors` | Cargo type checking (automatic) | Rust enforces at compile time |

---

## Unified Command Pattern (Package.json Scripts)

**Astro Root** (`package.json`):
```json
{
  "scripts": {
    "dev": "pnpm -F @astro/web dev",
    "build": "pnpm -r build",
    "test": "vitest --exclude 'tests/integration/**'",
    "lint": "pnpm -r lint",
    "validate": "pnpm lint && pnpm format:check && pnpm typecheck && pnpm test:run"
  }
}
```

**ggen Could Adopt** (if package.json added):
```json
{
  "name": "ggen-workspace",
  "private": true,
  "scripts": {
    "dev": "cargo run --package ggen-cli-lib --bin ggen --",
    "build": "cargo make build",
    "test": "cargo make test",
    "lint": "cargo make lint",
    "validate": "cargo make pre-commit"
  }
}
```

**Why Consider This**: Provides unified interface for developers familiar with npm scripts, while delegating to Cargo/cargo-make underneath.

---

## Key Takeaways

### What ggen.toml Adopted from Next.js

✅ **Environment-specific configuration** - Different settings for dev/ci/prod
✅ **Lifecycle hooks** - Pre/post generation phases (like build phases)
✅ **Performance tuning** - Parallel execution, caching, incremental builds
✅ **Modular sections** - Related settings grouped together
✅ **Security configuration** - Allowed domains, SSL validation
✅ **Convention over configuration** - Sensible defaults with explicit overrides

### What ggen.toml Adapted for Rust

🔄 **TOML format** - Rust ecosystem standard (instead of TypeScript)
🔄 **Shell script hooks** - More flexible than JavaScript callbacks
🔄 **Cargo workspace** - Native Rust monorepo (instead of pnpm workspaces)
🔄 **Compile-time type safety** - Rust compiler enforces correctness

### What ggen.toml Skipped (Not Applicable)

❌ **Bundler configuration** - ggen is a CLI tool, not a web app
❌ **Transpilation lists** - Cargo handles this automatically
❌ **Client/server split** - ggen generates code, doesn't serve it
❌ **Hydration config** - No SSR/CSR concerns

---

## Benefits of This Approach

### For JavaScript Developers Coming to Rust

- ✅ Familiar configuration patterns (environment overrides, lifecycle hooks)
- ✅ Similar mental model to Next.js config
- ✅ Easy to understand section-based organization

### For Rust Developers

- ✅ Idiomatic Rust (TOML, Cargo workspace, compile-time safety)
- ✅ No JavaScript dependency for configuration
- ✅ Type-safe deserialization via serde

### For ggen Users

- ✅ One file to configure everything
- ✅ Environment-specific overrides without duplication
- ✅ Lifecycle hooks for validation/formatting
- ✅ Performance tuning via simple settings

---

## Future Enhancements (Optional)

### Could Add (80/20 high value):

1. **Root package.json** - Unified command interface (see above)
2. **Config validation script** - Pre-flight check for ggen.toml
3. **Environment templates** - Starter configs for common scenarios

### Could Skip (Low value, high complexity):

- ❌ TypeScript config file (TOML is idiomatic Rust)
- ❌ JavaScript callbacks (shell scripts more flexible)
- ❌ Hot module reloading (not applicable to CLI tool)

---

## Conclusion

**ggen.toml successfully adopts Next.js configuration patterns** while staying idiomatic to the Rust ecosystem:

- **Environment configuration** → `[env.*]` sections
- **Build phases** → `[lifecycle.phases.*]`
- **Performance tuning** → `[performance]` section
- **Modular organization** → Named TOML sections

This gives developers familiar with Next.js an intuitive configuration experience, while Rust developers get idiomatic TOML and Cargo integration.

**Next Step**: Document these patterns in root-level docs (ARCHITECTURE.md, BEST_PRACTICES.md, etc.)
