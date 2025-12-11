# Astro Next.js Monorepo → ggen Rust Workspace Mapping

**Purpose**: Translate proven Next.js monorepo patterns to Rust workspace
**Reference**: ~/dis/astro (Next.js 15 + pnpm workspaces)
**Target**: ggen (Rust + Cargo workspace)

---

## 1. Workspace Structure Comparison

### Astro (Next.js Monorepo)
```
astro/
├── package.json                    # Root workspace config
├── pnpm-workspace.yaml             # Workspace definition
├── apps/
│   └── web/                        # Next.js app
│       ├── package.json
│       ├── app/                    # Next.js app router
│       └── lib/
├── packages/                       # Shared packages
│   ├── db-schema/
│   ├── hooks/
│   ├── providers/
│   ├── services/
│   ├── state-machines/
│   ├── stores/
│   ├── tanstack-db/
│   ├── telemetry/
│   ├── ui-components/
│   └── ui-primitives/
├── infrastructure/
│   ├── docker/
│   ├── postgres/
│   └── scripts/
├── docs/
├── specs/
└── tests/
```

### ggen (Rust Workspace) - Current
```
ggen/
├── Cargo.toml                      # Root workspace config
├── crates/                         # Workspace members
│   ├── ggen-cli/
│   ├── ggen-domain/
│   ├── ggen-core/
│   ├── ggen-ai/
│   ├── ggen-config/
│   ├── ggen-marketplace-v2/
│   ├── ggen-utils/
│   └── ...
├── examples/                       # Example projects
├── templates/                      # Code gen templates
├── docs/
├── scripts/
└── tests/
```

### ggen (Proposed) - With Astro Patterns
```
ggen/
├── Cargo.toml                      # Root workspace config
├── package.json                    # NEW: Unified script interface
├── crates/                         # Workspace members
│   └── ...
├── infrastructure/                 # NEW: From astro pattern
│   ├── docker/
│   ├── scripts/
│   │   ├── dev-setup.sh
│   │   ├── validate-all.sh
│   │   ├── benchmark.sh
│   │   └── clean.sh
│   └── ci/
├── docs/
│   ├── ARCHITECTURE.md             # NEW: Root-level docs
│   ├── AUTOMATION.md
│   ├── BEST_PRACTICES.md
│   ├── PRD.md
│   ├── thesis/                     # NEW: Research docs
│   │   ├── ontology-driven-development.md
│   │   ├── deterministic-generation.md
│   │   └── rdf-as-universal-schema.md
│   └── ...
├── specs/                          # NEW: Feature specifications
│   ├── 001-rdf-sparql-engine/
│   ├── 002-template-system/
│   └── ...
├── examples/
├── templates/
└── tests/
```

---

## 2. Package.json Scripts Mapping

### Astro Root package.json Scripts
```json
{
  "scripts": {
    "dev": "pnpm -F @astro/web dev",
    "dev:full": "concurrently --names \"DB,WEB\" -c \"blue,green\" \"./infrastructure/scripts/start.sh\" \"pnpm dev\"",
    "db:start": "./infrastructure/scripts/start.sh",
    "db:stop": "./infrastructure/scripts/stop.sh",
    "db:reset": "./infrastructure/scripts/reset.sh",
    "build": "pnpm -r build",
    "test": "vitest --exclude 'tests/integration/**'",
    "test:unit": "vitest --exclude 'tests/integration/**'",
    "test:integration": "vitest tests/integration --testTimeout 5000",
    "test:all": "vitest",
    "test:run": "vitest run --exclude 'tests/integration/**'",
    "test:coverage": "vitest run --coverage",
    "lint": "pnpm -r lint",
    "lint:fix": "pnpm -r lint:fix",
    "format": "prettier --write \"**/*.{js,jsx,ts,tsx,json,css,md}\"",
    "format:check": "prettier --check \"**/*.{js,jsx,ts,tsx,json,css,md}\"",
    "typecheck": "pnpm -r typecheck",
    "validate": "pnpm lint && pnpm format:check && pnpm typecheck && pnpm test:run",
    "validate:all": "./scripts/validate-all.sh",
    "setup": "./scripts/dev-setup.sh",
    "clean": "./scripts/clean.sh",
    "prepare": "husky"
  }
}
```

### ggen package.json (Proposed) - Rust Equivalents
```json
{
  "name": "ggen-workspace",
  "version": "4.0.0",
  "private": true,
  "description": "RDF-based code generation toolkit - Unified script interface",
  "scripts": {
    "dev": "cargo run --package ggen-cli-lib --bin ggen --",
    "dev:watch": "cargo watch -x 'run --package ggen-cli-lib --bin ggen'",
    "build": "cargo make build",
    "build:release": "cargo make build-release",
    "test": "cargo make test",
    "test:unit": "cargo make test-unit",
    "test:integration": "cargo make test-integration",
    "test:all": "cargo make test",
    "test:coverage": "cargo make coverage",
    "test:watch": "cargo make test-watch",
    "lint": "cargo make lint",
    "lint:fix": "cargo make lint-fix",
    "format": "cargo make format",
    "format:check": "cargo make format-check",
    "typecheck": "cargo make check",
    "validate": "cargo make pre-commit",
    "validate:all": "./infrastructure/scripts/validate-all.sh",
    "validate:docs": "./scripts/validate-docs/validate-all.sh",
    "benchmark": "./infrastructure/scripts/benchmark.sh",
    "slo:check": "cargo make slo-check",
    "setup": "./infrastructure/scripts/dev-setup.sh",
    "clean": "./infrastructure/scripts/clean.sh",
    "docs:serve": "mdbook serve docs/book",
    "docs:build": "mdbook build docs/book",
    "prepare": "husky install || true"
  },
  "engines": {
    "rust": ">=1.74.0",
    "cargo": ">=1.74.0",
    "node": ">=18.0.0"
  },
  "devDependencies": {
    "husky": "^9.1.0",
    "concurrently": "^9.0.0"
  }
}
```

---

## 3. Workspace Package Pattern Translation

### Astro Package Pattern (packages/services/)
```json
{
  "name": "@astro/services",
  "version": "1.0.0",
  "private": true,
  "type": "module",
  "main": "./src/index.js",
  "exports": {
    ".": "./src/index.js",
    "./AssetService": "./src/AssetService.js",
    "./JobService": "./src/JobService.js",
    "./TransformService": "./src/TransformService.js"
  },
  "scripts": {
    "test": "vitest",
    "lint": "eslint"
  },
  "dependencies": {
    "@astro/telemetry": "workspace:*",
    "@opentelemetry/api": "^1.9.0"
  }
}
```

### ggen Crate Pattern (crates/ggen-domain/Cargo.toml)
```toml
[package]
name = "ggen-domain"
version = "4.0.0"
edition = "2021"
authors = ["Sean Chatman <sean@ggen.ai>"]
license = "MIT"

[lib]
name = "ggen_domain"
path = "src/lib.rs"

[dependencies]
# Workspace dependencies
ggen-utils = { path = "../ggen-utils", version = "4.0.0" }
ggen-core = { path = "../ggen-core", version = "4.0.0" }

# External dependencies
tokio = { version = "1.47", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
```

**Key Insight**: Astro's `workspace:*` ≈ Rust's `path = "../crate-name"`

---

## 4. Unified Commands Pattern

### Astro Pattern
```bash
# Root delegates to workspaces
pnpm dev              # → pnpm -F @astro/web dev
pnpm test             # → vitest (runs all workspace tests)
pnpm lint             # → pnpm -r lint (recursive)
pnpm validate         # → Multi-step validation
```

### ggen Pattern (Proposed)
```bash
# Root delegates to cargo-make
npm run dev           # → cargo run
npm run test          # → cargo make test
npm run lint          # → cargo make lint
npm run validate      # → cargo make pre-commit
npm run setup         # → ./infrastructure/scripts/dev-setup.sh
```

**Why npm scripts for Rust?**
1. ✅ Familiar interface for Node developers
2. ✅ CI/CD can use same commands
3. ✅ Husky git hooks work seamlessly
4. ✅ Consistent with astro pattern

---

## 5. Infrastructure Scripts Pattern

### Astro Scripts
```
infrastructure/scripts/
├── start.sh          # Start services (DB, Electric)
├── stop.sh           # Stop all services
├── reset.sh          # Reset database
├── dev-setup.sh      # Development environment setup
├── clean.sh          # Clean build artifacts
└── validate-all.sh   # Comprehensive validation
```

### ggen Scripts (Proposed)
```
infrastructure/scripts/
├── dev-setup.sh       # Install Rust, cargo-make, tools
├── validate-all.sh    # Run all validation (tests + docs + lint)
├── benchmark.sh       # Performance benchmarking
├── clean.sh           # Clean target/, .ggen/, caches
├── slo-check.sh       # Verify SLO targets
└── publish.sh         # Publish to crates.io workflow
```

---

## 6. Documentation Structure (80/20 from Astro)

### Astro Has
- ✅ docs/ (60+ docs)
- ✅ specs/ (31 specs with detailed structure)
- ✅ docs/thesis/ (research-level docs)
- ✅ Root-level summary docs (ARCHITECTURE.md, etc.)
- ✅ docs/diataxis/ (4 quadrants)

### ggen Should Add
```
docs/
├── ARCHITECTURE.md               # NEW: System overview
├── AUTOMATION.md                 # NEW: CI/CD guide
├── BEST_PRACTICES.md             # NEW: Dev standards
├── PRD.md                        # NEW: Product requirements
├── TESTING_STRATEGY.md           # NEW: Test philosophy
├── PERFORMANCE.md                # NEW: Benchmarks & SLOs
│
├── thesis/                       # NEW: Research docs
│   ├── ontology-driven-development.md
│   ├── deterministic-generation.md
│   ├── rdf-as-universal-schema.md
│   └── ai-assisted-codegen.md
│
├── tutorials/                    # EXISTING
├── how-to/                       # EXISTING
├── reference/                    # EXISTING
├── explanations/                 # EXISTING
└── examples/                     # EXISTING
```

---

## 7. Specs Directory Pattern (From Astro)

### Astro Spec Structure
```
specs/001-complete-migration/
├── spec.md                       # Full specification
├── plan.md                       # Implementation plan
├── tasks.md                      # Task breakdown
├── research.md                   # Alternatives considered
├── data-model.md                 # Data structures
├── quickstart.md                 # Quick reference
├── analysis.md                   # Analysis reports
├── clarification-report.md       # Clarifications log
├── checklists/
│   └── requirements.md           # Validation checklist
└── contracts/
    └── api-contract.md           # API contracts
```

### ggen Spec Structure (Simplified)
```
specs/001-rdf-sparql-engine/
├── spec.md                       # REQUIRED
├── plan.md                       # REQUIRED
├── research.md                   # For design-heavy features
├── data-model.md                 # For data-centric features
├── quickstart.md                 # For user-facing features
└── checklists/
    └── requirements.md           # For critical features
```

**80/20 Rule**: Include spec.md + plan.md minimum, add others as needed

---

## 8. Key Patterns to Adopt from Astro

### 1. Root-Level Summary Docs
**Pattern**: ARCHITECTURE.md, AUTOMATION.md at docs/ root
**Why**: Quick executive summary without diving into subdirs
**Apply to ggen**: Create 6 root-level docs

### 2. Unified Script Interface
**Pattern**: package.json scripts delegate to underlying tools
**Why**: Familiar commands across ecosystems
**Apply to ggen**: npm run * → cargo make *

### 3. Infrastructure Directory
**Pattern**: infrastructure/{docker,scripts,ci}
**Why**: Centralize automation and deployment
**Apply to ggen**: Move scripts/ → infrastructure/scripts/

### 4. Specs Directory
**Pattern**: Numbered specs (001-xxx/) with standard structure
**Why**: Track feature evolution with clear ownership
**Apply to ggen**: Create specs for 6 core features

### 5. Thesis Directory
**Pattern**: docs/thesis/ for research-level documentation
**Why**: Document design philosophy and trade-offs
**Apply to ggen**: 4 core thesis docs

### 6. Workspace Scripts Pattern
**Pattern**: Root scripts invoke workspace members recursively
**Why**: Single command tests/lints entire workspace
**Apply to ggen**: Cargo workspace already does this, mirror in package.json

### 7. Database Scripts
**Pattern**: start.sh, stop.sh, reset.sh for services
**Why**: Consistent dev environment setup
**Apply to ggen**: dev-setup.sh, clean.sh, benchmark.sh

---

## 9. Translation Table (Next.js ↔ Rust)

| Astro (Next.js) | ggen (Rust) | Notes |
|-----------------|-------------|-------|
| `pnpm dev` | `cargo run` | Start dev server |
| `pnpm build` | `cargo build` | Compile |
| `pnpm test` | `cargo test` | Run tests |
| `vitest` | `cargo test` | Unit testing framework |
| `eslint` | `cargo clippy` | Linting |
| `prettier` | `cargo fmt` | Code formatting |
| `typescript` | Rust compiler | Type checking |
| `pnpm -r` | Cargo workspace | Recursive workspace ops |
| `workspace:*` | `path = "../crate"` | Workspace dependencies |
| `package.json` exports | `lib.rs` pub items | Public API |
| `husky` | `husky` | Git hooks (works with both!) |
| `concurrently` | Bash `&` or `cargo make` | Parallel execution |

---

## 10. Implementation Priority (80/20)

### Phase 1: Core Infrastructure (Week 1)
```bash
# 1. Create package.json with unified scripts
cat > package.json << 'EOF'
{
  "name": "ggen-workspace",
  "scripts": {
    "dev": "cargo run --package ggen-cli-lib --bin ggen --",
    "test": "cargo make test",
    "validate": "cargo make pre-commit",
    "setup": "./infrastructure/scripts/dev-setup.sh"
  }
}
EOF

# 2. Create infrastructure directory
mkdir -p infrastructure/{docker,scripts,ci}
mv scripts/validate-docs infrastructure/scripts/
touch infrastructure/scripts/{dev-setup.sh,clean.sh,benchmark.sh}

# 3. Create specs directory
mkdir -p specs/{001-rdf-sparql-engine,002-template-system,003-ai-integration}

# 4. Create root-level docs
touch docs/{ARCHITECTURE.md,AUTOMATION.md,BEST_PRACTICES.md,PRD.md}
```

### Phase 2: Documentation Enrichment (Week 2)
```bash
# 5. Create thesis directory
mkdir -p docs/thesis
touch docs/thesis/{ontology-driven-development.md,deterministic-generation.md,rdf-as-universal-schema.md}

# 6. Populate specs
# Each spec gets: spec.md, plan.md, research.md
```

### Phase 3: Polish & Integration (Week 3)
```bash
# 7. Add remaining docs
touch docs/{TESTING_STRATEGY.md,PERFORMANCE.md}

# 8. Set up husky hooks
npm install husky
npx husky init
```

---

## 11. Example: Complete Spec Template

```markdown
# specs/001-rdf-sparql-engine/spec.md

**Feature**: RDF/SPARQL Processing Engine
**Status**: Implemented ✅
**Owner**: Core Team

## Executive Summary
[2-3 paragraph overview of feature]

## User Scenarios & Testing
[User stories with acceptance criteria]

## Technical Design
### Architecture
[Component diagram]

### Data Model
[Rust structs, RDF ontology]

### API Surface
```rust
pub struct RdfEngine {
    store: Oxigraph,
}

impl RdfEngine {
    pub fn new() -> Result<Self, Error>;
    pub fn load_rdf(&mut self, data: &str) -> Result<(), Error>;
    pub fn query(&self, sparql: &str) -> Result<QueryResults, Error>;
}
```

## Implementation Plan
[Phases, tasks, dependencies]

## Testing Strategy
[Unit, integration, performance tests]

## Success Metrics
[SLOs, performance targets]
```

---

## 12. Benefits of This Mapping

### For Contributors
- ✅ Familiar npm scripts (even in Rust project)
- ✅ Clear feature specifications in specs/
- ✅ Root-level docs for quick reference
- ✅ Thesis docs explain design philosophy

### For Users
- ✅ Consistent commands across projects
- ✅ Comprehensive documentation
- ✅ Clear examples and quickstarts
- ✅ Production-ready patterns

### For Maintainers
- ✅ Specs track feature evolution
- ✅ Infrastructure scripts standardize automation
- ✅ Root docs provide executive summary
- ✅ Validation scripts prevent regressions

---

## 13. What NOT to Copy from Astro

### ❌ Skip These Patterns
1. **TanStack/React Query** - Not applicable to Rust CLI
2. **Next.js App Router** - Rust isn't a web framework
3. **Vercel deployment** - Rust deploys to crates.io
4. **React components** - No UI in CLI tool
5. **Database migrations** - ggen doesn't have a DB

### ✅ Keep These Rust-Specific
1. **Cargo.toml** - Better than package.json for Rust
2. **cargo-make** - Better than npm scripts for complex builds
3. **Result<T,E>** - Better than try/catch
4. **Ownership** - Rust's unique strength
5. **Zero-cost abstractions** - Performance matters

---

## Next Steps

1. **Review this mapping** - Does it align with your vision?
2. **Approve Phase 1** - package.json + infrastructure + specs
3. **I'll implement** - Create all Phase 1 content
4. **Validate** - Test new structure with validation scripts

**Timeline**: ~6 hours for complete Phase 1 implementation
**Risk**: Minimal (purely additive, no breaking changes)

---

**Key Insight**: Astro uses Next.js patterns for a web app monorepo. ggen translates these patterns to a Rust CLI workspace, keeping what works (unified scripts, specs, thesis docs) and skipping what doesn't (React, Next.js specifics).
