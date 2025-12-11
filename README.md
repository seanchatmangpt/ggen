# ggen - RDF-Based Code Generation Toolkit

**Production-ready code generation from knowledge graphs | 1,168+ tests passing | Rust + RDF + SPARQL**

`ggen` is a deterministic code generator that transforms RDF ontologies into polyglot code. Define your domain once in RDF/OWL, generate consistently across languages.

---

## ⚡ Quick Start

### Installation

```bash
# Homebrew (macOS/Linux)
brew install seanchatmangpt/ggen/ggen

# Cargo (any platform)
cargo install ggen-cli-lib

# From source
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo install --path crates/ggen-cli

# Install Speckit for spec-driven development (recommended for contributors)
uv tool install specify-cli --from git+https://github.com/github/spec-kit.git
```

### Your First Generation

```bash
# List available templates
ggen template list

# Show template details
ggen template show --template hello.tmpl

# Load RDF data into graph
ggen graph load --file your-ontology.ttl

# Query with SPARQL
ggen graph query --sparql_query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

# Extract ontology schema
ggen ontology extract --ontology_file schema.ttl
```

### Spec-First Development (For Contributors)

ggen uses [GitHub Spec Kit](https://github.com/github/spec-kit) for structured feature development:

```bash
# 1. Define project principles (one-time setup)
/speckit.constitution

# 2. For each new feature, create a specification
/speckit.specify "Add RDF validation with SHACL constraints"

# 3. Create technical plan
/speckit.plan

# 4. Generate actionable tasks
/speckit.tasks

# 5. Implement with evidence
/speckit.implement

# All specs live in .specify/specs/NNN-feature-name/
# Constitution at .specify/memory/constitution.md
```

**Branch naming**: `NNN-feature-name` (e.g., `001-rdf-validation`)
**Evidence required**: Tests, benchmarks, OTEL spans in `.specify/specs/NNN-feature/evidence/`

See [.specify/memory/constitution.md](.specify/memory/constitution.md) for full development principles.

---

## 📖 Documentation

**New to ggen?** Start with our comprehensive getting-started guide:

👉 **[Getting Started Guide](docs/getting-started/README.md)** - Your complete onboarding resource

### Quick Links

- **[10-Minute Quick Start](docs/getting-started/quick-start.md)** - Generate JavaScript + Zod from RDF in 10 minutes
- **[Complete CLI Reference](docs/reference/commands/complete-cli-reference.md)** - All commands and options
- **[Configuration Reference](docs/reference/configuration/)** - TOML configuration files
  - [ggen.toml Project Configuration](docs/reference/configuration/ggen-toml-reference.md) - Complete project settings
  - [gpack.toml Package Format](docs/reference/configuration/gpack-toml-reference.md) - Template package metadata
  - [Common TOML Configurations](docs/how-to/configuration/common-toml-configs.md) - Example configurations
- **[How-to Guides](docs/how-to/)** - Solutions to common tasks
  - [Generate JavaScript + Zod from Schema.org](docs/how-to/generation/generate-javascript-zod.md)
  - [Query RDF Data with SPARQL](docs/how-to/generation/query-rdf-sparql.md)
- **[Explanations](docs/explanations/)** - Understand the concepts
  - [What is Ontology-Driven Development?](docs/explanations/fundamentals/ontology-driven-development.md)
  - [RDF for Programmers](docs/explanations/fundamentals/rdf-for-programmers.md)

### 📚 Meta-Level: Learn Diataxis

- **[Diataxis Case Study](docs/examples/diataxis-case-study/README.md)** - Learn documentation by example
  - Complete Next.js + shadcn + ElectricSQL example
  - All 4 Diataxis quadrants demonstrated
  - 4-hour learning path with exercises
  - Meta-lessons on writing each doc type

---

## 🎯 Core Features

### ✅ What Works (Verified by 1,168+ Passing Tests)

| Feature | Commands | Status |
|---------|----------|--------|
| **Template Management** | `template list`, `template show`, `template lint` | ✅ Production |
| **RDF Graph Operations** | `graph load`, `graph export`, `graph query`, `graph visualize` | ✅ Production |
| **Ontology Extraction** | `ontology extract`, `ontology validate`, `ontology generate` | ✅ Production |
| **Project Scaffolding** | `project init`, `project gen`, `project watch` | ✅ Production |
| **AI Integration** | `ai chat`, `ai generate`, `ai analyze` | ✅ Production |
| **Marketplace** | `marketplace improve`, Package management | ✅ Production |
| **SPARQL Queries** | Full SPARQL 1.1 support via Oxigraph | ✅ Production |
| **Template Rendering** | Tera templates with RDF integration | ✅ Production |

---

## 📚 Command Reference

### Template Commands

```bash
# List all available templates (22 templates included)
ggen template list

# Show template metadata and variables
ggen template show --template rust.tmpl

# Lint template for errors
ggen template lint --template my-template.tmpl

# Create new template
ggen template new --name my-template --output templates/

# Generate file tree from template
ggen template generate_tree --template my-template.tmpl
```

### Graph Commands (RDF/SPARQL)

```bash
# Load RDF data (supports Turtle, RDF/XML, N-Triples)
ggen graph load --file ontology.ttl

# Export graph to file
ggen graph export --output graph.ttl --format turtle

# Query with SPARQL
ggen graph query --sparql_query "
  PREFIX ex: <http://example.org/>
  SELECT ?product ?price
  WHERE {
    ?product a ex:Product .
    ?product ex:price ?price .
  }
"

# Visualize graph structure
ggen graph visualize --output graph.svg
```

### Ontology Commands

```bash
# Extract ontology schema from RDF/OWL file
ggen ontology extract --ontology_file schema.ttl --output schema.json

# Initialize ontology project with examples
ggen ontology init --name my-ontology --template schema.org

# Generate code from ontology
ggen ontology generate --schema schema.json --language typescript --output src/types/

# Validate ontology quality
ggen ontology validate --schema schema.json --strict
```

### Project Commands

```bash
# Initialize project with conventions
ggen project init --name my-project --preset clap-noun-verb

# Generate code from template with variables
ggen project gen --template_ref pack:template --vars key=value --dry_run

# Watch for changes and auto-regenerate
ggen project watch --path ./src --debounce 500

# Generate project plan
ggen project plan --template service.tmpl --var service=auth --format json

# Apply generation plan
ggen project apply plan.json --yes

# Create new project
ggen project new --name my-app --project_type rust-cli --output .
```

### AI Commands

```bash
# Interactive chat session
ggen ai chat "Explain Rust ownership" --interactive

# Generate code with AI
ggen ai generate "Create a REST API server" --model gpt-4

# Analyze code
ggen ai analyze "fn main() { println!(\"hello\"); }"

# Analyze from file
ggen ai analyze --file src/main.rs --model claude-3-sonnet
```

### Marketplace Commands

```bash
# Get improvement suggestions
ggen marketplace improve my-package

# Apply suggested improvements
ggen marketplace improve my-package --apply license-mit
```

### Utility Commands

```bash
# Manage environment variables
ggen utils env --list
ggen utils env --get API_KEY
ggen utils env --set API_KEY=value

# Track academic paper status
ggen paper track paper.rdf --venue neurips-2024

# Generate workflow reports
ggen workflow report --workflow-file wf.json --format html --output report.html

# Validate package packs
ggen packs validate --pack-id startup-essentials

# Monitor hook events
ggen hook monitor --event pre-commit
```

---

## 🏗️ Architecture

### Workspace Structure (12 Crates)

```
ggen/
├── crates/
│   ├── ggen-cli/            # CLI commands (noun-verb auto-discovery)
│   ├── ggen-domain/         # Business logic (pure, no CLI deps)
│   ├── ggen-core/           # RDF engine, template system
│   ├── ggen-ai/             # AI integration (genai wrapper)
│   ├── ggen-marketplace-v2/ # Package management
│   ├── ggen-utils/          # Shared utilities, error handling
│   ├── ggen-config/         # Configuration management
│   ├── ggen-config-clap/    # CLI config integration
│   ├── ggen-cli-validation/ # CLI validation rules
│   ├── ggen-macros/         # Procedural macros
│   ├── ggen-dod/            # Data-oriented design patterns
│   └── ggen-node/           # Node.js FFI bindings
├── examples/                # 48 working examples
├── templates/               # 22 built-in templates
└── tests/                   # 1,168+ integration tests
```

### Tech Stack

- **Core**: Rust 1.74+ (edition 2021)
- **RDF/SPARQL**: Oxigraph 0.5 (SPARQL 1.1 compliant)
- **Templates**: Tera (Liquid/Jinja2-like syntax)
- **CLI**: clap-noun-verb 5.3.2 (auto-discovery)
- **AI**: genai 0.4+ (multi-provider support)
- **Async**: tokio 1.47+ (async runtime)

---

## 🧪 Development

### Testing (Chicago TDD)

```bash
# Quick compile check (<2s)
cargo make check

# Unit tests only (~16s)
cargo make test-unit

# Full test suite (1,168+ tests)
cargo make test

# Watch mode
cargo make test-watch
```

**Test Results:**
- ✅ 1,168 tests passing
- ✅ 0 failures
- ✅ 10 ignored (optional integrations)
- ✅ 80%+ code coverage on critical paths

### Build System (cargo-make)

**Critical Rules:**
1. ✅ **ALWAYS** use `cargo make` (never direct `cargo`)
2. ✅ Fast feedback loops (<5s for check)
3. ✅ Chicago TDD (state-based, real collaborators)
4. ✅ No `.expect()` in production code (use `Result<T,E>`)
5. ✅ Comprehensive error handling with context

```bash
cargo make check      # Fast compilation check (1.95s)
cargo make test-unit  # Unit tests (15.82s)
cargo make test       # Full suite (31.60s)
cargo make lint       # Clippy with strict rules
cargo make pre-commit # All checks before commit
```

---

## 📖 Examples (48 Working Projects)

Browse `examples/` directory for complete, runnable examples:

- **Basic**: `basic-template-generation/`, `hello-world-cli/`
- **Advanced**: `advanced-rust-api-8020/`, `advanced-sparql-graph/`
- **AI**: `ai-code-generation/`, `ai-microservice/`, `ai-template-creation/`
- **CLI**: `clap-noun-verb-demo/`, `cli-advanced/`, `cli-subcommand/`
- **Lifecycle**: `lifecycle-demo/`, `lifecycle-hooks/`, `lifecycle-validation/`
- **Ontology**: `ontology-driven/`, `semantic-web/`, `knowledge-graph/`
- **SPARQL**: `sparql-queries/`, `rdf-generation/`, `graph-traversal/`

---

## 📦 Templates (22 Included)

| Template | Description | Type |
|----------|-------------|------|
| `hello.tmpl` | Basic hello world | Starter |
| `rust.tmpl` | Rust project | Language |
| `python.tmpl` | Python project | Language |
| `ai-ontology.tmpl` | E-commerce ontology | Ontology |
| `ai-sparql.tmpl` | SPARQL queries | Query |
| `rust-service-with-placeholders.tmpl` | Production Rust service | Production |
| `database-with-migrations.tmpl` | DB schema + migrations | Database |
| `safe-error-handling.tmpl` | Error patterns | Pattern |
| `production-readiness-demo.tmpl` | Production checklist | Production |

**View all**: `ggen template list`

---

## 🚀 Performance

**Verified by Benchmarks:**
- ✅ Template parsing: **115 nanoseconds** (43,480x under 5ms SLO)
- ✅ Build time: **0.79 seconds** (84% under 5s SLO)
- ✅ Startup time: **2.0 milliseconds** (96% under 50ms SLO)
- ✅ Binary size: **2.8 MB** (44% under 5MB SLO)
- ✅ RDF triple processing: **<1µs per triple**
- ✅ SPARQL query execution: **<10ms for complex queries**

---

## 🔗 Links

- **GitHub**: https://github.com/seanchatmangpt/ggen
- **Crates.io**: https://crates.io/crates/ggen-cli-lib
- **Homebrew**: `brew install seanchatmangpt/ggen/ggen`
- **Documentation**:
  - [Getting Started](docs/getting-started/README.md) - Complete onboarding guide
  - [Quick Start](docs/getting-started/quick-start.md) - 10-minute tutorial
  - [CLI Reference](docs/reference/commands/complete-cli-reference.md) - All commands
  - [How-to Guides](docs/how-to/) - Task-oriented guides
  - [Explanations](docs/explanations/) - Conceptual understanding
- **Examples**: [examples/](examples/) (48 projects)
- **Issues**: https://github.com/seanchatmangpt/ggen/issues

---

## 🤝 Contributing

1. Read [CONTRIBUTING.md](CONTRIBUTING.md)
2. Check [docs/agent/](docs/agent/) for architecture
3. Run tests: `cargo make test`
4. Submit PR with tests
5. All tests must pass

---

## 📄 License

MIT License - see [LICENSE](LICENSE)

---

## 🎯 Use Cases

**ggen excels at:**
- ✅ Generating type-safe code from RDF ontologies
- ✅ Building CLI tools with clap-noun-verb patterns
- ✅ Creating polyglot APIs from knowledge graphs
- ✅ Template-driven project scaffolding
- ✅ SPARQL-powered code generation
- ✅ AI-assisted development workflows
- ✅ Marketplace package management
- ✅ Research paper tracking and generation

**Real-world applications:**
- Generate TypeScript types from Schema.org
- Create Rust APIs from domain ontologies
- Build Python Pydantic models from RDF
- Scaffold full-stack projects with conventions
- Query knowledge graphs for code patterns
- Automate repetitive coding tasks

---

**Built with Rust | Powered by RDF | Tested at 1,168+ passing tests**

_Deterministic. Ontology-driven. Production-ready._
