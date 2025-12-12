# ggen - Ontology-Driven Code Generation Framework

**v4.0.0** | **Deterministic** | **Type-Safe** | **Production-Ready**

[![Tests](https://img.shields.io/badge/tests-1168%20passing-success)]() [![Rust](https://img.shields.io/badge/rust-1.74%2B-orange)]() [![License](https://img.shields.io/badge/license-MIT-blue)]()

Language-agnostic code generation framework that transforms RDF/OWL ontologies into polyglot code. Uses SPARQL queries and Tera templates to generate type-safe code across Rust, TypeScript, Python, JavaScript, Go, and more.

---

## 🗺️ I want to...

**This is a Reference document. Choose your path:**

### Learn by Doing
**Goal**: Get started building with ggen
**Go to**: [Getting Started Guide](docs/getting-started/README.md)
**Time**: 30-60 minutes
**You'll build**: Working code generation from RDF ontologies

### Solve a Specific Problem
**Goal**: Accomplish a specific task
**Go to**: [How-to Guides](docs/how-to/)
**Time**: 5-15 minutes per guide
**Examples**:
- [Generate JavaScript + Zod from Schema.org](docs/how-to/generation/generate-javascript-zod.md)
- [Query RDF Data with SPARQL](docs/how-to/generation/query-rdf-sparql.md)
- [Create Custom Templates](docs/how-to/templates/create-custom-template.md)

### Understand the Concepts
**Goal**: Learn how and why ggen works
**Go to**: [Explanations](docs/explanations/)
**Time**: 15-30 minutes per topic
**Topics**:
- [What is Ontology-Driven Development?](docs/explanations/fundamentals/ontology-driven-development.md)
- [RDF for Programmers](docs/explanations/fundamentals/rdf-for-programmers.md)
- [Why SPARQL for Code Generation?](docs/explanations/concepts/sparql-for-code-generation.md)

### Look Up Details
**Goal**: Find specific technical information
**You're here!** Continue reading below for:
- [Installation](#installation)
- [CLI Commands](#cli-commands)
- [Architecture](#architecture)
- [Configuration](#configuration)
- [Performance Metrics](#performance)

---

## Installation

### Homebrew (macOS/Linux)
```bash
brew install seanchatmangpt/ggen/ggen
```

### Cargo (any platform)
```bash
cargo install ggen-cli-lib
```

### From Source
```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo install --path crates/ggen-cli --bin ggen
```

### Verify Installation
```bash
ggen --version
# Output: ggen 4.0.0
```

---

## Quick Verification (30 seconds)

```bash
# List available templates
ggen template list

# Load RDF ontology
ggen graph load --file examples/basic-ontology/person.ttl

# Query with SPARQL
ggen graph query --sparql_query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
```

**For full tutorial**: [10-Minute Quick Start](docs/getting-started/quick-start.md)

---

## CLI Commands

### Template Commands
```bash
ggen template list                        # List all 22 built-in templates
ggen template show --template rust.tmpl   # Show template metadata and variables
ggen template lint --template my.tmpl     # Validate template syntax
ggen template new --name my-template      # Create new template
```

### Graph Commands (RDF/SPARQL)
```bash
ggen graph load --file ontology.ttl       # Load RDF data (Turtle, RDF/XML, N-Triples)
ggen graph query --sparql_query "..."     # Execute SPARQL queries
ggen graph export --input graph.ttl       # Export graph to file
ggen graph visualize --input graph.ttl    # Visualize ontology structure
```

### Ontology Commands
```bash
ggen ontology extract --ontology_file schema.ttl         # Extract ontology schema
ggen ontology generate --schema schema.json --language typescript  # Generate code
ggen ontology validate --schema_file schema.ttl          # Validate ontology quality
ggen ontology init --project_name my-ontology            # Initialize ontology project
```

### Project Commands
```bash
ggen project init --preset clap-noun-verb                # Scaffold project with conventions
ggen project gen --template_ref pack:tmpl --vars k=v     # Generate code from template
ggen project watch --path ./src --debounce 500           # Auto-regenerate on changes
ggen project plan --template_ref service.tmpl            # Generate project plan
ggen project apply --plan_file plan.json                 # Apply generation plan
```

### AI Commands
```bash
ggen ai chat --message "Explain Rust ownership"          # Interactive chat session
ggen ai generate --prompt "Create REST API"              # AI-powered code generation
ggen ai analyze --file src/main.rs                       # Code analysis
```

### Marketplace Commands
```bash
ggen marketplace search --query "rust microservice"      # Search for packages
ggen marketplace install --package_id my-package         # Install package
ggen marketplace publish                                 # Publish package
ggen marketplace info --package_id my-package            # Get package information
```

**Complete reference**: [Complete CLI Reference](docs/reference/commands/complete-cli-reference.md)

---

## Architecture

### Workspace Structure (14 Crates)

```
ggen/
├── crates/
│   ├── ggen-core/            # RDF engine, SPARQL queries, template system
│   ├── ggen-cli/             # CLI commands (clap-noun-verb auto-discovery)
│   ├── ggen-domain/          # Business logic (pure, no CLI dependencies)
│   ├── ggen-ai/              # AI integration (genai multi-provider wrapper)
│   ├── ggen-marketplace/     # Package management (RDF metadata, signatures)
│   ├── ggen-utils/           # Shared utilities (error handling, logging)
│   ├── ggen-config/          # Configuration management (ggen.toml parser)
│   ├── ggen-config-clap/     # CLI config integration
│   ├── ggen-cli-validation/  # Input validation and safety checks
│   ├── ggen-macros/          # Procedural macros (compile-time code generation)
│   ├── ggen-dod/             # Definition of Done (observability, governance)
│   ├── ggen-node/            # Node.js N-API bindings (JavaScript/TypeScript FFI)
│   ├── ggen-test-audit/      # Test quality auditing (coverage, quality metrics)
│   └── ggen-test-opt/        # Test optimization (parallel execution)
├── templates/                # 22 built-in templates
├── examples/                 # 48 working examples
├── tests/                    # 1,168+ integration tests
└── docs/                     # Comprehensive documentation (Diataxis framework)
```

### Technology Stack

| Component | Technology | Version |
|-----------|-----------|---------|
| **Core Language** | Rust | 1.74+ (edition 2021) |
| **RDF Store** | Oxigraph | 0.5 (SPARQL 1.1 compliant) |
| **Template Engine** | Tera | 1.20 (Jinja2/Liquid-like) |
| **CLI Framework** | clap-noun-verb | 5.3.2 (auto-discovery) |
| **AI Integration** | genai | 0.4+ (multi-provider) |
| **Async Runtime** | tokio | 1.47+ |
| **Cryptography** | ML-DSA | Post-quantum signatures |
| **Observability** | OpenTelemetry | Distributed tracing |

### Design Principles

- **Type-First Thinking**: Express invariants in types, not runtime checks
- **Zero-Cost Abstractions**: Generics over trait objects, compile-time over runtime
- **Deterministic Outputs**: Same ontology + same templates = identical code
- **Memory Safety**: Rust ownership system, no garbage collection
- **Chicago TDD**: State-based testing with real collaborators
- **SPARC Methodology**: Specification → Pseudocode → Architecture → Refinement → Completion
- **DfLSS Quality**: Design for Lean Six Sigma (prevent defects, not detect)

**For detailed architecture**: [Architecture Documentation](docs/architecture/)

---

## Configuration

### Project Configuration (ggen.toml)

```toml
[project]
name = "my-project"
version = "1.0.0"

[generation]
templates_dir = "templates/"
output_dir = "generated/"

[rdf]
ontology_files = ["schema.ttl"]
default_format = "turtle"

[marketplace]
registry_url = "https://marketplace.ggen.io"
```

**Reference**: [ggen.toml Reference](docs/reference/configuration/ggen-toml-reference.md)

### Package Configuration (gpack.toml)

```toml
[package]
name = "my-template-pack"
version = "1.0.0"
author = "Your Name <you@example.com>"

[templates]
templates = ["template1.tmpl", "template2.tmpl"]
```

**Reference**: [gpack.toml Reference](docs/reference/configuration/gpack-toml-reference.md)

---

## Performance

### Benchmarks (v4.0.0)

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Template Parsing | <5ms | 115 ns | ✅ 43,480x under SLO |
| Incremental Build | <5s | 0.79s | ✅ 84% under SLO |
| Cold Startup | <50ms | 2.0ms | ✅ 96% under SLO |
| Binary Size | <5MB | 2.8 MB | ✅ 44% under SLO |
| RDF Triple Processing | <10µs | <1µs | ✅ 10x under SLO |
| SPARQL Complex Query | <50ms | <10ms | ✅ 5x under SLO |
| First Build | <15s | ~12s | ✅ On target |
| Memory (1K triples) | <100MB | ~45MB | ✅ 55% under SLO |

### Test Coverage

- **Total Tests**: 1,168 passing
- **Failures**: 0
- **Ignored**: 10 (optional integrations)
- **Coverage**: 80%+ on critical paths

**Detailed metrics**: [PERFORMANCE.md](PERFORMANCE.md)

---

## Features

### Core Capabilities

| Feature | Description | Status |
|---------|-------------|--------|
| **RDF/OWL Processing** | Load, query, transform ontologies (Turtle, RDF/XML, N-Triples) | ✅ Production |
| **SPARQL 1.1** | Full query support via Oxigraph triplestore | ✅ Production |
| **Template Engine** | Tera-based with 22+ built-in templates | ✅ Production |
| **Polyglot Generation** | Rust, TypeScript, Python, JavaScript, Go, and more | ✅ Production |
| **CLI Auto-Discovery** | clap-noun-verb pattern for plugin-style commands | ✅ Production |
| **AI Integration** | Multi-provider (OpenAI, Anthropic, Ollama) code assistance | ✅ Production |
| **Semantic Marketplace** | RDF-based package registry with cryptographic signatures | ✅ Production |
| **Deterministic Lockfiles** | Reproducible builds with dependency pinning | ✅ Production |
| **Graph Visualization** | Ontology structure visualization | ✅ Production |
| **Delta Projections** | Incremental updates without full regeneration | ✅ Production |

### Built-in Templates (22)

- `hello.tmpl` - Basic hello world
- `rust.tmpl` - Rust project scaffolding
- `python.tmpl` - Python project scaffolding
- `ai-ontology.tmpl` - E-commerce ontology example
- `ai-sparql.tmpl` - SPARQL query templates
- `rust-service-with-placeholders.tmpl` - Production Rust service
- `database-with-migrations.tmpl` - Database schema + migrations
- `safe-error-handling.tmpl` - Error handling patterns
- `production-readiness-demo.tmpl` - Production checklist

**View all**: `ggen template list`

---

## Examples

### By Category (48 Total)

| Category | Examples | Description |
|----------|----------|-------------|
| **Basic** | `basic-template-generation/`, `simple-project/` | Starter examples |
| **Advanced** | `advanced-rust-api-8020/`, `advanced-sparql-graph/` | Production patterns |
| **AI** | `ai-code-generation/`, `ai-microservice/` | LLM integration |
| **CLI** | `clap-noun-verb-demo/`, `cli-advanced/` | Command-line tools |
| **Lifecycle** | `lifecycle-complete/`, `advanced-lifecycle-demo/` | Full project lifecycle |
| **Ontology** | `knowledge-graph-builder/`, `fastapi-from-rdf/` | RDF workflows |
| **SPARQL** | `sparql-engine/`, `advanced-sparql-graph/` | Query examples |
| **Full-Stack** | `comprehensive-rust-showcase/`, `microservices-architecture/` | End-to-end systems |

**Browse**: [examples/](examples/)

---

## Use Cases

### What ggen Excels At

- Generate type-safe TypeScript/Zod schemas from Schema.org ontologies
- Create Rust APIs with complete type definitions from domain ontologies
- Build Python Pydantic models from RDF/OWL schemas
- Scaffold full-stack projects (Rust CLI, Next.js, microservices) with conventions
- Query knowledge graphs with SPARQL to extract code patterns
- Automate repetitive coding tasks via template-driven generation
- Build CLI tools following clap-noun-verb patterns
- Create polyglot APIs from unified domain models
- Generate database schemas and migrations from ontologies
- Produce API documentation (OpenAPI, GraphQL) from RDF specifications

### Target Users

- Backend developers building type-safe APIs from domain models
- Full-stack developers needing consistent types across languages
- DevOps engineers scaffolding microservices and CLI tools
- Data engineers working with knowledge graphs and ontologies
- AI/ML engineers integrating LLMs into code generation workflows
- Open source maintainers creating template packages for distribution
- Enterprise architects implementing ontology-driven development

---

## Development

### For Contributors

**Required reading**:
1. [CONTRIBUTING.md](CONTRIBUTING.md) - Contribution guidelines
2. [CLAUDE.md](CLAUDE.md) - Development constitution (SPARC + Chicago TDD + DfLSS)
3. [TESTING.md](TESTING.md) - Testing strategy

### Quick Commands

```bash
# Fast compilation check (<2s)
cargo make check

# Unit tests only (~16s)
cargo make test-unit

# Full test suite (1,168 tests, ~32s)
cargo make test

# Clippy with strict rules
cargo make lint

# Pre-commit validation
cargo make pre-commit

# Full CI pipeline
cargo make ci
```

### Spec-First Workflow

ggen uses [GitHub Spec Kit](https://github.com/github/spec-kit) for feature development:

```bash
# Install Speckit
uv tool install specify-cli --from git+https://github.com/github/spec-kit.git

# For each feature
/speckit.specify "Feature description"
/speckit.plan
/speckit.tasks
/speckit.implement
```

**Branch naming**: `NNN-feature-name`
**Evidence required**: Tests, benchmarks, OTEL spans in `.specify/specs/NNN-feature/evidence/`

---

## Links

- **GitHub**: https://github.com/seanchatmangpt/ggen
- **Crates.io**: https://crates.io/crates/ggen-cli-lib
- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- **Documentation**: [docs/](docs/)

---

## License

MIT License - see [LICENSE](LICENSE)

---

## What is ggen?

ggen is a deterministic code generation framework that treats software artifacts as projections of knowledge graphs. Unlike traditional templating tools that use string manipulation, ggen:

1. **Defines domain models in RDF/OWL** (ontologies)
2. **Queries models with SPARQL** (like SQL for graphs)
3. **Projects to target languages** (Rust, TypeScript, Python, etc.)
4. **Guarantees reproducibility** (lockfiles, deterministic outputs)

**Result**: Define your domain once in RDF, generate type-safe code for any language, maintain consistency across your entire codebase.

**Why RDF?**: Standard, language-agnostic, queryable with SPARQL, extensible with OWL, interoperable with existing ontologies (Schema.org, FOAF, Dublin Core).

**Why deterministic?**: Same input → same output, always. Enables caching, reproducible builds, and confident regeneration.

**For deeper understanding**: [Explanations](docs/explanations/)

---

**Built with Rust | Powered by RDF | Tested with 1,168+ passing tests**

_Deterministic. Ontology-driven. Production-ready._
