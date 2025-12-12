# ggen

**Ontology-Driven Code Generation Framework**

[![Tests](https://img.shields.io/badge/tests-1168%20passing-success)]() [![Rust](https://img.shields.io/badge/rust-1.74%2B-orange)]() [![License](https://img.shields.io/badge/license-MIT-blue)]() [![Version](https://img.shields.io/badge/version-4.0.0-blue)]()

> Define your domain once in RDF. Generate type-safe code everywhere.

Transform RDF/OWL ontologies into polyglot code using SPARQL queries and Tera templates. Generate consistent, type-safe code across Rust, TypeScript, Python, JavaScript, Go, and more—all from a single source of truth.

**v4.0.0** | **Deterministic** | **Type-Safe** | **Production-Ready** | **1,168 Tests Passing**

---

## What is ggen?

ggen treats software artifacts as **projections of knowledge graphs**. Unlike traditional code generators that use string templates, ggen:

1. **Models domains in RDF/OWL** — Standard, queryable ontologies
2. **Queries with SPARQL** — Extract patterns like SQL for graphs
3. **Projects to any language** — Type-safe Rust, TypeScript, Python, etc.
4. **Guarantees reproducibility** — Same input → same output, always

**The Result**: One ontology → Many languages → Perfect consistency

```
Schema.org Person Ontology (RDF)
         ↓ SPARQL Query
    ┌────┴────┬────────┬────────┐
    ↓         ↓        ↓        ↓
  Rust    TypeScript Python   Go
  struct  interface  class    struct
  + Serde + Zod      + Pydantic + JSON tags
```

**Why this matters**:
- No more type drift between frontend/backend
- Single domain model, infinite projections
- Ontology changes propagate everywhere automatically
- Standards-based (RDF, SPARQL, OWL)

---

## 🗺️ Documentation Navigator

**This README is Reference material (information-oriented lookup). Choose your path based on your goal:**

### 📖 Learn by Doing
**Goal**: Get hands-on experience
**Path**: [Getting Started Guide](docs/getting-started/README.md) → [10-Minute Tutorial](docs/getting-started/quick-start.md)
**Time**: 30-60 minutes
**Outcome**: Working code generation from RDF ontologies

### 🛠️ Solve a Specific Problem
**Goal**: Complete a task right now
**Path**: [How-to Guides](docs/how-to/)
**Time**: 5-15 minutes per guide
**Popular tasks**:
- [Generate TypeScript + Zod from Schema.org](docs/how-to/generation/generate-javascript-zod.md)
- [Query RDF with SPARQL](docs/how-to/generation/query-rdf-sparql.md)
- [Create Custom Templates](docs/how-to/templates/create-custom-template.md)

### 💡 Understand the Concepts
**Goal**: Learn the "why" and "how"
**Path**: [Explanations](docs/explanations/)
**Time**: 15-30 minutes per topic
**Core concepts**:
- [Ontology-Driven Development](docs/explanations/fundamentals/ontology-driven-development.md)
- [RDF for Programmers](docs/explanations/fundamentals/rdf-for-programmers.md)
- [SPARQL for Code Generation](docs/explanations/concepts/sparql-for-code-generation.md)

### 📋 Look Up Technical Details
**Goal**: Find specific information
**You're here!** Browse sections below:
- [Installation](#installation) — Platform-specific installation
- [CLI Reference](#cli-reference) — All commands with examples
- [Architecture](#architecture) — System design and components
- [Configuration](#configuration) — TOML config files
- [Performance](#performance) — Benchmarks and SLOs

---

## Installation

### Prerequisites
- **Rust 1.74+** (for building from source or Cargo install)
- **macOS/Linux/Windows** (full platform support)

### Method 1: Homebrew (macOS/Linux)
```bash
brew install seanchatmangpt/ggen/ggen
ggen --version  # Verify: ggen 4.0.0
```

### Method 2: Cargo (Cross-platform)
```bash
cargo install ggen-cli-lib
ggen --version  # Verify: ggen 4.0.0
```

### Method 3: From Source
```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo install --path crates/ggen-cli --bin ggen
ggen --version  # Verify: ggen 4.0.0
```

### Quick Verification (30 seconds)

Confirm ggen works on your system:

```bash
# 1. List built-in templates (22 available)
ggen template list

# 2. Load an RDF ontology
ggen graph load --file examples/basic-ontology/person.ttl

# 3. Query with SPARQL
ggen graph query --sparql_query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
```

✅ **All commands succeeded?** You're ready to go!
📖 **Next step**: [10-Minute Tutorial](docs/getting-started/quick-start.md)

---

## CLI Reference

### Template Management

```bash
# List all 22 built-in templates
ggen template list

# Show template details (variables, metadata)
ggen template show --template rust.tmpl

# Validate template syntax
ggen template lint --template my-template.tmpl

# Create new template
ggen template new --name my-template
```

### RDF Graph Operations

```bash
# Load ontology (Turtle, RDF/XML, N-Triples)
ggen graph load --file ontology.ttl

# Execute SPARQL query
ggen graph query --sparql_query "
  PREFIX schema: <https://schema.org/>
  SELECT ?name WHERE { ?person schema:name ?name }
"

# Export graph to file
ggen graph export --input graph.ttl --output output.ttl --format turtle

# Visualize ontology structure
ggen graph visualize --input graph.ttl
```

### Ontology Processing

```bash
# Extract schema from RDF/OWL file
ggen ontology extract --ontology_file schema.ttl

# Generate code from ontology
ggen ontology generate \
  --schema schema.json \
  --language typescript \
  --output src/types/

# Validate ontology quality
ggen ontology validate --schema_file schema.ttl

# Initialize new ontology project
ggen ontology init --project_name my-ontology
```

### Project Scaffolding

```bash
# Initialize project with preset
ggen project init \
  --preset clap-noun-verb \
  --name my-project \
  --path .

# Generate from template with variables
ggen project gen \
  --template_ref pack:rust-service \
  --vars service=auth,port=8080

# Watch for changes (auto-regenerate)
ggen project watch --path ./src --debounce 500

# Generate project plan (preview)
ggen project plan \
  --template_ref service.tmpl \
  --vars service=auth \
  --format json

# Apply generation plan
ggen project apply --plan_file plan.json
```

### AI-Powered Generation

```bash
# Interactive chat
ggen ai chat --message "Explain RDF triples"

# Generate code with AI
ggen ai generate \
  --prompt "Create a REST API for products" \
  --model gpt-4

# Analyze existing code
ggen ai analyze --file src/main.rs
```

### Marketplace

```bash
# Search template packages
ggen marketplace search --query "rust microservice"

# Install package
ggen marketplace install --package_id rust-api-template

# Publish your package
ggen marketplace publish

# View package info
ggen marketplace info --package_id rust-api-template
```

**Full command reference**: [Complete CLI Documentation](docs/reference/commands/complete-cli-reference.md)

---

## Architecture

### System Overview

```
┌─────────────────────────────────────────────────────────┐
│                    ggen Architecture                     │
├─────────────────────────────────────────────────────────┤
│                                                          │
│  User Input (CLI)                                        │
│       ↓                                                  │
│  ┌──────────────┐      ┌──────────────┐                │
│  │  ggen-cli    │ ───→ │ ggen-domain  │ (Pure Logic)   │
│  └──────────────┘      └──────────────┘                │
│       ↓                       ↓                          │
│  ┌──────────────────────────────────────┐              │
│  │         ggen-core (Engine)           │              │
│  ├──────────────────────────────────────┤              │
│  │  • RDF Triplestore (Oxigraph)        │              │
│  │  • SPARQL Query Engine               │              │
│  │  • Template Renderer (Tera)          │              │
│  │  • Code Generation Pipeline          │              │
│  └──────────────────────────────────────┘              │
│       ↓                                                  │
│  Generated Code (Rust, TypeScript, Python, ...)         │
│                                                          │
└─────────────────────────────────────────────────────────┘
```

### Workspace Crates (14)

**Core Engine**
- `ggen-core` — RDF engine, SPARQL queries, template system (heart of ggen)
- `ggen-domain` — Pure business logic (no I/O, framework-agnostic)

**Interface Layer**
- `ggen-cli` — Command-line interface (clap-noun-verb auto-discovery)
- `ggen-node` — Node.js FFI bindings (JavaScript/TypeScript integration)

**Feature Modules**
- `ggen-ai` — AI integration (genai multi-provider wrapper)
- `ggen-marketplace` — Package registry (RDF metadata, post-quantum signatures)

**Configuration & Validation**
- `ggen-config` — Configuration parser (ggen.toml, gpack.toml)
- `ggen-config-clap` — CLI config integration
- `ggen-cli-validation` — Input validation and safety checks

**Development Tools**
- `ggen-utils` — Shared utilities (error handling, logging, git hooks)
- `ggen-macros` — Procedural macros (compile-time code generation)
- `ggen-dod` — Definition of Done (observability, MAPE-K governance)
- `ggen-test-audit` — Test quality auditing (coverage, quality metrics)
- `ggen-test-opt` — Test optimization (parallel execution, performance)

**Assets**
- `templates/` — 22 built-in templates (Rust, TypeScript, Python, etc.)
- `examples/` — 48 working examples across all features
- `tests/` — 1,168 integration tests (100% passing)
- `docs/` — Diataxis-structured documentation

### Technology Stack

| Layer | Technology | Purpose |
|-------|-----------|---------|
| **Language** | Rust 1.74+ (edition 2021) | Type safety, zero-cost abstractions, memory safety |
| **RDF Store** | Oxigraph 0.5 | SPARQL 1.1 compliant triplestore |
| **Templates** | Tera 1.20 | Jinja2/Liquid-like syntax with RDF integration |
| **CLI** | clap-noun-verb 5.3.2 | Plugin-style command auto-discovery |
| **AI** | genai 0.4+ | Multi-provider LLM support (OpenAI, Anthropic, Ollama) |
| **Async** | tokio 1.47+ | High-performance async runtime |
| **Crypto** | ML-DSA | Post-quantum package signatures |
| **Observability** | OpenTelemetry | Distributed tracing, metrics, spans |

### Design Philosophy

| Principle | Implementation |
|-----------|----------------|
| **Type-First Thinking** | Express invariants in types, validate at compile-time |
| **Zero-Cost Abstractions** | Generics over trait objects, monomorphization |
| **Deterministic Outputs** | Same inputs → identical outputs (cryptographic hashing) |
| **Memory Safety** | Rust ownership, no GC pauses, no data races |
| **Chicago TDD** | State-based testing with real collaborators (no mocks) |
| **SPARC Methodology** | Specification → Pseudocode → Architecture → Refinement → Completion |
| **DfLSS Quality** | Design for Lean Six Sigma (prevent defects, not detect) |

📐 **Detailed architecture**: [Architecture Documentation](docs/architecture/)

---

## Configuration

### Project Config: `ggen.toml`

Used for project-wide settings (placed at repository root):

```toml
[project]
name = "my-project"
version = "1.0.0"
description = "E-commerce domain model"

[generation]
templates_dir = "templates/"
output_dir = "generated/"

[rdf]
ontology_files = ["schema/product.ttl", "schema/order.ttl"]
default_format = "turtle"  # turtle | rdf-xml | n-triples

[marketplace]
registry_url = "https://marketplace.ggen.io"
```

📖 **Full reference**: [ggen.toml Documentation](docs/reference/configuration/ggen-toml-reference.md)

### Package Config: `gpack.toml`

Used for distributable template packages:

```toml
[package]
name = "rust-microservice-template"
version = "2.1.0"
author = "Your Name <you@example.com>"
description = "Production Rust microservice with OpenTelemetry"
license = "MIT"

[templates]
templates = [
  "service.tmpl",
  "dockerfile.tmpl",
  "ci.tmpl"
]

[dependencies]
requires = ["ggen >= 4.0.0"]
```

📖 **Full reference**: [gpack.toml Documentation](docs/reference/configuration/gpack-toml-reference.md)

---

## Performance

### Benchmarks (v4.0.0)

All metrics measured on M1 MacBook Pro, averaged over 1000 runs:

| Metric | SLO Target | Actual | Performance |
|--------|-----------|--------|-------------|
| **Template Parsing** | <5ms | **115 ns** | ✅ 43,480× under budget |
| **Incremental Build** | <5s | **0.79s** | ✅ 6.3× faster than target |
| **Cold Startup** | <50ms | **2.0ms** | ✅ 25× faster than target |
| **Binary Size** | <5MB | **2.8 MB** | ✅ 44% smaller than limit |
| **RDF Triple Processing** | <10µs | **<1µs** | ✅ 10× faster than target |
| **SPARQL Complex Query** | <50ms | **<10ms** | ✅ 5× faster than target |
| **First Build (clean)** | <15s | **~12s** | ✅ Within target |
| **Memory (1K triples)** | <100MB | **~45MB** | ✅ 55% under budget |

### Quality Metrics

- ✅ **Tests**: 1,168 passing | 0 failing | 10 ignored (optional)
- ✅ **Coverage**: 80%+ on critical paths (ggen-core, ggen-domain)
- ✅ **Linting**: Zero clippy warnings (pedantic + nursery enabled)
- ✅ **Security**: Zero cargo-audit vulnerabilities

📊 **Detailed metrics**: [PERFORMANCE.md](PERFORMANCE.md)

---

## Features

### Production-Ready Capabilities

| Feature | Technology | Status |
|---------|-----------|--------|
| **RDF/OWL Processing** | Oxigraph triplestore | ✅ Production |
| **SPARQL 1.1 Queries** | Full spec compliance | ✅ Production |
| **Template Engine** | Tera with 22+ templates | ✅ Production |
| **Polyglot Generation** | Rust, TS, Python, Go, JS | ✅ Production |
| **CLI Auto-Discovery** | clap-noun-verb pattern | ✅ Production |
| **AI Integration** | OpenAI, Anthropic, Ollama | ✅ Production |
| **Package Marketplace** | RDF metadata + ML-DSA signatures | ✅ Production |
| **Deterministic Builds** | Lockfiles + SHA-256 hashing | ✅ Production |
| **Graph Visualization** | GraphViz DOT output | ✅ Production |
| **Delta Projections** | Incremental updates | ✅ Production |

### Built-in Templates (22)

**Starter Templates**
- `hello.tmpl` — Minimal example
- `rust.tmpl` — Rust project scaffold
- `python.tmpl` — Python project scaffold

**Production Templates**
- `rust-service-with-placeholders.tmpl` — Microservice with OTEL
- `database-with-migrations.tmpl` — Schema + migrations
- `safe-error-handling.tmpl` — Result<T, E> patterns

**Domain Templates**
- `ai-ontology.tmpl` — E-commerce domain example
- `ai-sparql.tmpl` — Complex SPARQL queries

**Quality Templates**
- `production-readiness-demo.tmpl` — Deployment checklist

**View all**: Run `ggen template list`

---

## Use Cases

### What ggen Excels At

**Type-Safe Multi-Language Development**
```
Input:  Schema.org Product ontology (RDF)
Output: • Rust struct with Serde
        • TypeScript interface with Zod
        • Python Pydantic model
        • Go struct with JSON tags
        • GraphQL schema
```

**Specific Applications**:

1. **Full-Stack Type Safety**
   Generate matching types for frontend (TypeScript) and backend (Rust) from one ontology

2. **API-First Development**
   Define API contracts in RDF/OWL, generate OpenAPI specs + server stubs + client SDKs

3. **Database Schema Management**
   Ontology → SQL migrations + ORM models + repository patterns

4. **CLI Tool Scaffolding**
   Generate clap-noun-verb CLIs with auto-discovery and completions

5. **Microservices Architecture**
   Consistent service scaffolds (Rust, Docker, CI/CD, OTEL) from templates

6. **Knowledge Graph → Code**
   Query existing knowledge graphs (DBpedia, Wikidata) to extract code patterns

### Target Users

| Role | Use Case |
|------|----------|
| **Backend Developers** | Generate type-safe APIs from domain ontologies |
| **Full-Stack Teams** | Eliminate type drift between frontend/backend |
| **DevOps Engineers** | Scaffold microservices with consistent structure |
| **Data Engineers** | Transform RDF knowledge graphs into queryable APIs |
| **AI/ML Engineers** | Integrate LLMs into code generation workflows |
| **OSS Maintainers** | Distribute template packages via marketplace |
| **Enterprise Architects** | Implement ontology-driven system architecture |

---

## Examples

Browse `examples/` directory (48 working projects):

| Category | Count | Examples |
|----------|-------|----------|
| **Basic** | 8 | `basic-template-generation/`, `simple-project/` |
| **Advanced** | 12 | `advanced-rust-api-8020/`, `advanced-sparql-graph/` |
| **AI Integration** | 6 | `ai-code-generation/`, `ai-microservice/` |
| **CLI Tools** | 7 | `clap-noun-verb-demo/`, `cli-advanced/` |
| **Lifecycle** | 4 | `lifecycle-complete/`, `advanced-lifecycle-demo/` |
| **Ontologies** | 5 | `knowledge-graph-builder/`, `fastapi-from-rdf/` |
| **SPARQL** | 3 | `sparql-engine/`, `advanced-sparql-graph/` |
| **Full-Stack** | 3 | `comprehensive-rust-showcase/`, `microservices-architecture/` |

Each example includes:
- ✅ Working code (tested in CI)
- ✅ README with explanation
- ✅ Sample ontology/templates
- ✅ Expected output

🔍 **Browse examples**: [examples/](examples/)

---

## Development

### For Contributors

**Essential Reading** (in order):
1. [CONTRIBUTING.md](CONTRIBUTING.md) — Workflow, PR guidelines, code of conduct
2. [CLAUDE.md](CLAUDE.md) — Development constitution (SPARC + Chicago TDD + DfLSS)
3. [TESTING.md](TESTING.md) — Testing philosophy and requirements

### Quick Reference

```bash
# Development cycle
cargo make check          # Compile (<2s) ← Start here
cargo make test-unit      # Unit tests (~16s)
cargo make lint           # Clippy (strict mode)
cargo make test           # All 1,168 tests (~32s)

# Before commit
cargo make pre-commit     # Format + lint + test

# CI simulation
cargo make ci             # Full pipeline
```

### Spec-First Workflow (Required)

ggen uses [GitHub Spec Kit](https://github.com/github/spec-kit) for all features:

```bash
# One-time setup
uv tool install specify-cli --from git+https://github.com/github/spec-kit.git

# For each feature
/speckit.specify "Add SHACL validation for ontologies"
/speckit.plan     # Technical architecture
/speckit.tasks    # Actionable breakdown
/speckit.implement # Implementation with evidence

# All specs: .specify/specs/NNN-feature-name/
# Evidence:  .specify/specs/NNN-feature/evidence/
```

**Branch naming**: `NNN-feature-name` (e.g., `042-shacl-validation`)
**Evidence required**: Tests + benchmarks + OTEL spans
**Constitution**: [.specify/memory/constitution.md](.specify/memory/constitution.md)

### Development Principles

1. ✅ **ALWAYS** use `cargo make` (never direct `cargo` commands)
2. ✅ Chicago TDD: State-based tests with real collaborators (no mocks)
3. ✅ No `unwrap()`/`expect()` in production code (use `Result<T, E>`)
4. ✅ Type-first: Express invariants in types, not runtime checks
5. ✅ Stop on errors: Andon signal (RED/YELLOW/GREEN status)

---

## Links

- 📦 **Crates.io**: https://crates.io/crates/ggen-cli-lib
- 🐙 **GitHub**: https://github.com/seanchatmangpt/ggen
- 🐛 **Issues**: https://github.com/seanchatmangpt/ggen/issues
- 💬 **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- 📚 **Documentation**: [docs/](docs/)

---

## License

MIT License — see [LICENSE](LICENSE) for details

---

## Why ggen?

**Traditional code generators**: String templates + search-replace = fragile, inconsistent, hard to maintain

**ggen philosophy**:
- ✅ **Ontologies** define domain truth (not brittle templates)
- ✅ **SPARQL** extracts patterns (not regex/string manipulation)
- ✅ **Type systems** enforce correctness (compile-time, not runtime)
- ✅ **Determinism** guarantees reproducibility (cryptographic hashing)
- ✅ **Standards-based** interoperates with existing ontologies (Schema.org, FOAF, Dublin Core)

**The paradigm shift**:
```
Old Way: Code → Templates → More Code (copy/paste hell)
New Way: Ontology → SPARQL → Projections (single source of truth)
```

**Result**: Code generation that feels like compilation, not macro expansion.

📖 **Learn more**: [Explanations](docs/explanations/)

---

<div align="center">

**Built with Rust 🦀 | Powered by RDF 🔗 | Tested with 1,168 passing tests ✅**

_Deterministic. Ontology-driven. Production-ready._

**ggen v4.0.0** — Define once. Generate everywhere.

</div>
