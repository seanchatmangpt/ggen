# ggen

**Ontology-Driven Code Generation Framework**

[![Tests](https://img.shields.io/badge/tests-1168%20passing-success)]()
[![Coverage](https://img.shields.io/badge/coverage-80%25%2B-brightgreen)]()
[![Rust](https://img.shields.io/badge/rust-1.74%2B-orange)]()
[![License](https://img.shields.io/badge/license-MIT-blue)]()
[![Version](https://img.shields.io/badge/version-4.0.0-blue)]()
[![Security](https://img.shields.io/badge/vulnerabilities-0-success)]()
[![Performance](https://img.shields.io/badge/startup-2ms-brightgreen)]()

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

## ⚡ Quick Win (60 Seconds)

See the power of ontology-driven generation with one command:

**Input**: Schema.org Product ontology
```turtle
@prefix schema: <https://schema.org/> .

schema:Product a rdfs:Class ;
    rdfs:label "Product" ;
    schema:property schema:name, schema:price, schema:description .

schema:name a rdf:Property ;
    rdfs:range schema:Text .

schema:price a rdf:Property ;
    rdfs:range schema:Number .
```

**Command**:
```bash
ggen ontology generate \
  --schema product.ttl \
  --language typescript \
  --template zod
```

**Output**: Type-safe TypeScript + Zod validation
```typescript
import { z } from 'zod';

// Generated from Schema.org Product ontology
export const ProductSchema = z.object({
  name: z.string(),
  price: z.number(),
  description: z.string().optional(),
});

export type Product = z.infer<typeof ProductSchema>;

// Auto-generated validation
export function validateProduct(data: unknown): Product {
  return ProductSchema.parse(data);
}
```

**The magic**: Change the ontology once → regenerate → all languages update automatically. Zero manual sync.

---

## 🆚 Comparison

### ggen vs. Traditional Approaches

| Aspect | Manual Coding | String Templates | **ggen (Ontology-Driven)** |
|--------|---------------|------------------|----------------------------|
| **Type Safety** | ❌ Manual sync required | ❌ No type checking | ✅ Guaranteed by ontology |
| **Cross-Language** | ❌ Rewrite for each language | ⚠️ Templates per language | ✅ One ontology, any language |
| **Consistency** | ❌ Drift over time | ⚠️ Partial (template only) | ✅ Mathematically guaranteed |
| **Maintainability** | ❌ High (N × M problem) | ⚠️ Medium (M templates) | ✅ Low (1 ontology) |
| **Standards-Based** | ❌ Custom schemas | ❌ Proprietary | ✅ RDF, SPARQL, OWL |
| **Queryable** | ❌ No | ❌ No | ✅ SPARQL queries |
| **Reproducible** | ❌ No | ⚠️ Maybe | ✅ Cryptographically verified |
| **Learning Curve** | Low | Medium | Medium-High |
| **Best For** | One-off scripts | Simple scaffolding | Production systems |

**The N × M Problem**: With manual coding, you need N types × M languages = exponential work. With ggen: 1 ontology → infinite projections.

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

## 🌐 Community & Ecosystem

### Related Projects

**Official Ecosystem**:
- **ggen-templates** — Curated community template collection
- **ggen-vscode** — VS Code extension (syntax highlighting, snippets)
- **ggen-playground** — Browser-based playground for experimentation

**Integrations**:
- **Schema.org** — Use official schemas directly
- **FOAF** — Friend-of-a-Friend ontology support
- **Dublin Core** — Metadata standards integration
- **Oxigraph** — High-performance RDF triplestore
- **Tera** — Jinja2/Liquid-like template engine

**Community Tools** (via marketplace):
- **graphql-to-rdf** — Convert GraphQL schemas to RDF
- **openapi-to-rdf** — OpenAPI → RDF ontology converter
- **rdf-visualizer** — Interactive ontology browser
- **ggen-watch** — Advanced file watcher with smart regeneration

### Getting Help

| Channel | Purpose | Response Time |
|---------|---------|---------------|
| 📚 [Documentation](docs/) | Self-service guides | Immediate |
| 💬 [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions) | Q&A, ideas, show & tell | <24 hours |
| 🐛 [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues) | Bug reports, feature requests | <48 hours |
| 📖 [Examples](examples/) | Working code samples | Immediate |

**Before asking**:
1. Check the [FAQ](#-faq) below
2. Search [existing discussions](https://github.com/seanchatmangpt/ggen/discussions)
3. Try the [10-minute tutorial](docs/getting-started/quick-start.md)

### Contributing

We welcome contributions! Here's how to get involved:

**Code Contributions**:
- 🐛 Fix bugs (check [good first issue](https://github.com/seanchatmangpt/ggen/labels/good%20first%20issue))
- ✨ Add features (discuss in [Discussions](https://github.com/seanchatmangpt/ggen/discussions) first)
- 📝 Improve docs (especially examples!)
- 🧪 Write tests (increase coverage)

**Non-Code Contributions**:
- 🎨 Create templates (publish to marketplace)
- 📚 Write tutorials or blog posts
- 🗣️ Give talks or presentations
- 🌍 Translate documentation

**Quick start**: See [CONTRIBUTING.md](CONTRIBUTING.md) for full guidelines.

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

## ❓ FAQ

<details>
<summary><strong>How is ggen different from Yeoman, Plop, or Hygen?</strong></summary>

**Traditional generators** (Yeoman, Plop, Hygen) use string templates with variable substitution. They can't:
- Understand relationships between types
- Query your domain model
- Guarantee consistency across languages
- Leverage existing ontologies (Schema.org, FOAF)

**ggen** treats your domain as a knowledge graph. You can query it with SPARQL, traverse relationships, and generate code that respects your domain's semantics.

**Example**: Traditional tools can't automatically generate matching validation rules for a "Person must have at least one email" constraint. ggen extracts this from OWL restrictions.
</details>

<details>
<summary><strong>Do I need to learn RDF/OWL/SPARQL?</strong></summary>

**Short answer**: No, to get started. Yes, to unlock full power.

**Getting started** (no RDF knowledge needed):
- Use `ggen template list` to see built-in templates
- Generate from existing Schema.org ontologies
- Scaffold projects with presets

**Intermediate** (basic RDF, ~2 hours learning):
- Create your own domain ontologies in Turtle format
- Use simple SPARQL queries (like SQL)
- Customize templates

**Advanced** (deep knowledge):
- OWL restrictions for validation rules
- Complex SPARQL queries
- Custom template functions

📚 **Resources**: [RDF for Programmers](docs/explanations/fundamentals/rdf-for-programmers.md)
</details>

<details>
<summary><strong>Can ggen integrate with my existing codebase?</strong></summary>

Yes! ggen supports several integration patterns:

1. **Standalone generation**: Generate into `generated/` directory, import as library
2. **Partial file generation**: Use markers like `// BEGIN GENERATED` / `// END GENERATED`
3. **Delta updates**: Regenerate only changed parts (incremental)
4. **Watch mode**: Auto-regenerate on ontology changes

**Best practice**: Treat generated code as read-only. Put custom logic in separate files that import generated code.
</details>

<details>
<summary><strong>What languages can ggen generate?</strong></summary>

**Built-in templates**: Rust, TypeScript, Python, JavaScript, Go

**Easy to add**: Any language via Tera templates. Community templates available for:
- Java
- C#
- GraphQL schemas
- OpenAPI specs
- SQL migrations
- Kubernetes manifests

**Template marketplace**: Browse community templates with `ggen marketplace search`
</details>

<details>
<summary><strong>How fast is code generation?</strong></summary>

**Blazing fast** (see [Performance](#performance)):
- Template parsing: **115 nanoseconds**
- Simple generation: **<100ms**
- Complex ontology (1000+ classes): **<2 seconds**
- Incremental updates: **<100ms**

**Why so fast?** Rust, zero-copy parsing, HNSW indexing for SPARQL, template caching.
</details>

<details>
<summary><strong>Is ggen production-ready?</strong></summary>

**Yes.** ggen v4.0.0 is production-ready:
- ✅ **1,168 passing tests** (0 failures)
- ✅ **80%+ code coverage** on critical paths
- ✅ **Zero clippy warnings** (pedantic mode)
- ✅ **Zero security vulnerabilities** (cargo-audit)
- ✅ **Deterministic builds** (lockfiles + SHA-256)
- ✅ **Semantic versioning** commitment
- ✅ **Used in production** by early adopters

**Stability**: Core APIs stable. Marketplace and AI features actively evolving.
</details>

<details>
<summary><strong>How do I migrate from [other tool]?</strong></summary>

**From manual coding**:
1. Model your types in RDF/OWL (start simple, use Schema.org types)
2. Generate code with `ggen ontology generate`
3. Replace manual types incrementally

**From GraphQL codegen**:
1. Convert GraphQL schema to RDF (tool: `graphql-to-rdf`)
2. Use ggen to generate types + resolvers
3. Bonus: Generate other languages from same ontology

**From Prisma/TypeORM**:
1. Model database in RDF ontology
2. Generate both schema migrations + ORM types
3. Keep database + application in sync

📖 **Migration guides**: [docs/how-to/migration/](docs/how-to/migration/)
</details>

<details>
<summary><strong>Can I contribute templates to the marketplace?</strong></summary>

**Absolutely!** We welcome community templates.

**Process**:
1. Create your template package (`gpack.toml`)
2. Test thoroughly (`ggen template lint`)
3. Publish: `ggen marketplace publish`
4. (Optional) Submit to curated list via PR

**Popular needs**:
- Industry-specific ontologies (healthcare, finance, IoT)
- Framework-specific generators (Django, Rails, Spring)
- Infrastructure templates (Terraform, CloudFormation)

💡 **Guide**: [Creating Marketplace Packages](docs/how-to/templates/create-marketplace-package.md)
</details>

<details>
<summary><strong>What's the learning curve like?</strong></summary>

**Timeline** (for developers comfortable with types):

- **Day 1**: Install, run quick start, understand basic flow (2 hours)
- **Week 1**: Create first ontology, generate TypeScript + Rust (5 hours)
- **Month 1**: Proficient with RDF, SPARQL, custom templates (20 hours)
- **Month 3**: Advanced patterns, marketplace publishing (40 hours)

**Compared to**:
- Easier than: Learning GraphQL from scratch
- Similar to: Learning Terraform or Kubernetes basics
- Harder than: Using Yeoman or Plop

**Payoff**: Scales exponentially. One ontology → infinite languages.
</details>

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
