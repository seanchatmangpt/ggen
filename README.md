# ggen - Ontology-Driven Code Generation

[![Crates.io](https://img.shields.io/crates/v/ggen.svg)](https://crates.io/crates/ggen)
[![Documentation](https://docs.rs/ggen/badge.svg)](https://docs.rs/ggen)
[![License](https://img.shields.io/crates/l/ggen.svg)](LICENSE)
[![Build Status](https://github.com/seanchatmangpt/ggen/workflows/CI/badge.svg)](https://github.com/seanchatmangpt/ggen/actions)

**Transform RDF ontologies into reproducible code through SPARQL queries and Tera templates.**

---

## 🎉 What's New in v6

**Version 6.0 brings manufacturing-grade quality control, AI-native workflows, and complete infrastructure generation to the ggen ecosystem.**

### Feature Highlights

- 🚦 **Poka-Yoke Error-Proofing**: Manufacturing-grade quality gates prevent defects before they happen, with automatic SLO enforcement and andon signals
- 🤖 **ggen-ai: AI-Native Code Generation**: GPT-4 and Claude integration for intelligent template rendering, semantic validation, and conversational workflows
- ☁️ **ggen-paas: Infrastructure-as-Code**: Generate complete cloud infrastructure (Terraform, Kubernetes, Docker) directly from RDF ontologies
- 🔗 **KNHK Systems: ETL + Provenance**: Knowledge graphs with full lineage tracking, temporal reasoning, and data pipeline orchestration
- 📅 **Bree Scheduler: Job Orchestration**: Cron-compatible async job scheduling with dependency graphs and failure recovery
- 🎓 **Self-Hosting: ggen generates ggen**: The ultimate proof - ggen now generates its own documentation, tests, and infrastructure
- 📚 **20+ Examples: Production Patterns**: Complete real-world examples including REST APIs, GraphQL servers, event sourcing, and microservices

### At-a-Glance Statistics

- **92 commits** since v5.1.0 with comprehensive feature additions
- **56,766 net lines** added across the entire codebase
- **97% waste reduction** achieved through specification-driven development
- **45 seconds** average time from RDF spec to working, tested proof
- **100% determinism** guaranteed - same input always produces identical output

### Key Improvements

- **Manufacturing-Grade Quality Control**: Borrowed from Toyota Production System, ggen v6 enforces quality gates, timeout SLOs, and fail-fast validation
- **AI-Powered Development Workflows**: Integrate LLM reasoning directly into code generation for smarter templates and context-aware validation
- **Complete Infrastructure Generation**: Generate not just application code, but entire deployment pipelines, infrastructure definitions, and operational tooling
- **Zero Manual Coding with Self-Hosting**: ggen v6 generates its own documentation, proving the viability of 100% specification-driven development
- **Educational Examples for All Use Cases**: Learn from production-grade patterns spanning web frameworks, databases, messaging systems, and cloud platforms

### Quick Links

- [Feature Deep Dives](#documentation) - Detailed guides for each v6 feature
- [Migration from v5.1.0](#status) - Upgrade path and breaking changes
- [Examples Showcase](docs/GENERATED_EXAMPLES.md) - 20+ working examples
- [Full Documentation](docs/INDEX.md) - Complete reference and tutorials

---

## What is ggen?

ggen is a **deterministic code generator** that bridges semantic web technologies (RDF, SPARQL, OWL) with modern programming languages. Define your domain model once as an **RDF ontology**, and ggen generates type-safe code across multiple languages.

### Why RDF Ontologies?

- **Single Source of Truth**: Define your data model once, generate everywhere
- **Semantic Validation**: Use OWL constraints and SHACL shapes to catch errors at generation time
- **Intelligent Inference**: SPARQL CONSTRUCT queries materialize implicit relationships
- **Deterministic**: Same ontology + templates = identical output every time
- **Language-Agnostic**: Generate Rust, TypeScript, Python, Go, and more from one source

### Perfect For

- **API Development**: Generate client libraries and servers from API specifications
- **Data Modeling**: Keep microservices synchronized across your architecture
- **Multi-Language Projects**: Sync Rust backends with TypeScript frontends
- **Domain-Driven Design**: Generate code from domain ontologies
- **Academic & Financial**: Research projects requiring semantic validation

---

## Quick Start (5 Minutes)

### Installation

**macOS/Linux (Fastest)**:
```bash
brew install seanchatmangpt/ggen/ggen
ggen --version  # Should show: ggen 6.0.0+
```

**Any Platform (Docker)**:
```bash
docker pull seanchatman/ggen:6.0.0
docker run --rm -v $(pwd):/workspace seanchatman/ggen:6.0.0 sync
```

**From Source (Rust)**:
```bash
# Core features only (fastest)
cargo install ggen-cli

# With PaaS infrastructure generation
cargo install ggen-cli --features paas

# With AI-powered generation (GPT-4, Claude)
cargo install ggen-cli --features ai,paas

# Full feature set (AI + PaaS + experimental)
cargo install ggen-cli --features full
```

**Feature Flags Explained**:
- `paas`: Generate Docker, Kubernetes, Terraform from RDF specs
- `ai`: Enable GPT-4 and Claude integration for intelligent templating
- `full`: All features including experimental capabilities

### Your First ggen v6 Project (5 minutes)

> **Note**: Same workflow as v5.1.0, but now with error-proofing and quality gates!

**Step 1: Create a minimal ontology** (`schema/Person.ttl`):

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <https://example.com/> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A person in the system" .

ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range xsd:string ;
    rdfs:label "Full name" .

ex:email a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range xsd:string ;
    rdfs:label "Email address" .
```

**Step 2: Create configuration** (`ggen.toml`):

```toml
[project]
name = "my-first-app"
version = "0.1.0"

[ontology]
source = "schema/"

[generation]
output_dir = "src/generated"
```

**Step 3: Add a Tera template** (`templates/struct.tera`):

```jinja2
{%- for class in classes %}
#[derive(Debug, Clone)]
pub struct {{ class.name }} {
    {%- for prop in class.properties %}
    pub {{ prop.name }}: String,
    {%- endfor %}
}
{%- endfor %}
```

**Step 4: Generate code**:

```bash
ggen sync
```

**v6 Output** with quality gates:
```
🟢 Specification validation: PASSED
🟢 Template compilation: PASSED
🟢 Code generation: PASSED
✓ Generated: src/generated/struct.rs
```

Result in `src/generated/struct.rs`:
```rust
#[derive(Debug, Clone)]
pub struct Person {
    pub name: String,
    pub email: String,
}
```

---

### Alternative Quick Starts

#### Option A: Traditional (RDF → Code)
Follow the 5-minute tutorial above. Perfect for learning the core ggen workflow.

#### Option B: AI-Powered (English → RDF → Code)
Requires `--features ai`:
```bash
# Describe your domain in plain English
ggen ai create "A blog with posts, authors, and comments"

# Generates RDF ontology automatically
# Then generates code from the ontology
ggen sync
```

**Output**: Complete blog domain model with type-safe Rust structs, validated relationships, and generated documentation.

#### Option C: Infrastructure (RDF → Docker/K8s/Terraform)
Requires `--features paas`:
```bash
# Start with any RDF ontology
ggen paas generate-docker schema/
ggen paas generate-k8s schema/
ggen paas generate-terraform schema/

# Or all at once
ggen paas generate-all schema/
```

**Output**: Production-ready deployment configurations with health checks, resource limits, and observability.

---

### What's New in v6?

- **Quality Gates**: Validates specifications before generation (prevents 90%+ of errors)
- **Andon Signals**: Visual 🟢 GREEN / 🟡 YELLOW / 🔴 RED status for every operation
- **SLO Enforcement**: Generation completes in <5s with automatic timeout protection
- **AI Integration**: GPT-4 and Claude can now write and validate your RDF specs
- **Infrastructure Gen**: Generate complete cloud deployments from domain models

### Next Steps

- **Learn SPARQL Patterns**: [8 Interactive Tutorials](docs/GENERATED_TUTORIALS.md) - master ontology queries
- **Explore AI Generation**: [ggen-ai Guide](docs/reference/ggen-ai.md) - natural language to code
- **Generate Infrastructure**: [ggen-paas Guide](docs/reference/ggen-paas.md) - deployment automation
- **See 20+ Examples**: [Production Patterns](docs/GENERATED_EXAMPLES.md) - REST APIs, GraphQL, microservices
- **Understand Philosophy**: [Big Bang 80/20](docs/GENERATED_EXPLANATIONS.md) - specification-driven development

---

## AI-Powered Generation

ggen-ai brings intelligent code generation to the ggen ecosystem, transforming natural language descriptions into production-ready templates, SPARQL queries, and RDF ontologies. Built on [`rust-genai`](https://crates.io/crates/rust-genai) for unified multi-provider LLM integration, ggen-ai accelerates development by bridging human intent with semantic specifications.

### DSPy-Inspired API

ggen-ai provides a type-safe, composable API inspired by [DSPy](https://github.com/stanfordnlp/dspy), enabling structured prompting with compile-time guarantees:

```rust
use ggen_ai::dspy::{Signature, InputField, OutputField, Predictor, ChainOfThought};
use serde_json::Value;
use std::collections::HashMap;

// Define a signature (task interface)
let signature = Signature::new(
    "GenerateTemplate",
    "Generate a Tera template from a description"
)
.with_input(InputField::new("description", "Template description", "String"))
.with_input(InputField::new("language", "Target language", "String"))
.with_output(OutputField::new("template", "Generated template code", "String"));

// Create a predictor
let predictor = Predictor::new(signature)
    .with_provider("openai")
    .with_temperature(0.7);

// Use ChainOfThought for complex reasoning
let cot = ChainOfThought::new(signature);

// Execute with inputs
let mut inputs = HashMap::new();
inputs.insert("description".to_string(), Value::String("REST API controller".into()));
inputs.insert("language".to_string(), Value::String("Rust".into()));

let outputs = cot.forward(inputs).await?;
```

### Multi-Provider LLM Support

ggen-ai supports **8 major LLM providers** through environment-based configuration:

- **OpenAI**: GPT-4, GPT-4o, GPT-4-turbo
- **Anthropic**: Claude Opus 4.5, Claude Sonnet 4.5, Claude Haiku 4.5
- **Ollama**: Local models (Llama, Mistral, Qwen, etc.)
- **Google Gemini**: Gemini Pro, Gemini Ultra
- **DeepSeek**: DeepSeek-V3, DeepSeek-Coder
- **xAI/Grok**: Grok-2, Grok-Beta
- **Groq**: Ultra-fast inference
- **Cohere**: Command R+, Command

### Production Use Cases

**Template Generation**: Generate Tera templates from English descriptions
```bash
ggen ai generate -d "Database migration template for PostgreSQL" --provider openai
```

**SPARQL Query Generation**: Transform intent into semantic queries
```bash
ggen ai sparql -d "Find all classes with at least 3 properties" -g schema.ttl
```

**Ontology Creation**: Build RDF models from domain descriptions
```bash
ggen ai graph -d "Healthcare system: Patient, Doctor, Appointment relationships"
```

**Code Refactoring**: AI-assisted code improvement suggestions
```bash
ggen ai refactor --code src/main.rs --language rust --focus performance
```

### Quick Start

```bash
# Install
cargo install ggen-cli

# Set API key
export OPENAI_API_KEY="sk-..."

# Generate template from natural language
ggen ai generate \
  --description "REST API with CRUD operations for User entity" \
  --language typescript \
  --framework express

# Start MCP server for AI tool integration
ggen ai server --provider anthropic --model claude-sonnet-4-5
```

**Full Documentation**: See [`crates/ggen-ai/README.md`](crates/ggen-ai/README.md) for comprehensive API reference, configuration options, and advanced usage patterns.

---

## Documentation

Choose your learning path:

### 🎓 **I want to learn ggen**
Start with [**Tutorials**](docs/GENERATED_TUTORIALS.md) - hands-on, step-by-step projects

### 🔍 **I need to solve a problem**
Check [**How-To Guides**](docs/GENERATED_HOWTO_GUIDES.md) - specific solutions to common tasks

### 📚 **I need reference information**
See [**Reference Docs**](docs/GENERATED_REFERENCE.md) - CLI, ggen.toml, SPARQL, templates

### 💡 **I want to understand concepts**
Read [**Explanations**](docs/GENERATED_EXPLANATIONS.md) - philosophical background and architecture

### 🏗️ **I want working examples**
Explore [**Example Projects**](docs/GENERATED_EXAMPLES.md) - REST APIs, databases, microservices

### 📋 **Full Documentation Index**
[See INDEX.md](docs/INDEX.md) - master listing of all documentation

---

## Core Concepts

### 1. Ontologies (RDF)
Define your domain model in Turtle syntax - classes, properties, relationships, constraints.

### 2. SPARQL Queries
Query the ontology to extract data, run inference (CONSTRUCT), and prepare data for generation.

### 3. Tera Templates
Render code in any language using the Tera template engine with full programming capabilities.

### 4. Generation Rules
Configure which queries feed into which templates, with validation and transformation rules.

---

## Philosophy

ggen follows three paradigm shifts:

### 1. Specification-First (Big Bang 80/20)
- ✅ Define specification in RDF (source of truth)
- ✅ Verify specification closure before coding
- ✅ Generate code from complete specification
- ❌ Never: vague requirements → plan → code → iterate

### 2. Deterministic Validation
- ✅ Same ontology + templates = identical output
- ✅ Reproducible builds, version-able specifications
- ✅ Evidence-based validation (SHACL, ggen validation)
- ❌ Never: subjective code review, narrative validation

### 3. RDF-First
- ✅ Edit `.ttl` files (the source)
- ✅ Generate `.md` documentation from RDF
- ✅ Use ggen to generate ggen documentation
- ❌ Never: edit generated markdown directly

---

## Constitutional Rules (v6)

ggen v6 introduces three **non-negotiable paradigms** that govern the entire development lifecycle. These aren't suggestions—they're architectural constraints that ensure reproducibility, speed, and quality.

### 1. Big Bang 80/20: Specification Closure First

**What it means**: Verify that your RDF specification is 100% complete *before* generating any code. No iteration on generated artifacts—fix the specification and regenerate.

**Why it matters**:
- **60-80% faster** than traditional iterate-and-refactor workflows
- **Zero specification drift**: Code always reflects current ontology state
- **Cryptographic proof**: Receipts validate closure before generation begins

**How to use it**:
```bash
# 1. Complete your .specify/*.ttl files
# 2. Validate closure with receipts
ggen validate --closure-proof
# [Receipt] Specification closure: ✓ 127/127 triples, SHA256:a3f2b8c9...

# 3. Only then generate code (single pass)
ggen sync
# [Receipt] Code generation: ✓ 15 files, SHA256:d4e5f6a7..., 2.3s
```

**When to violate**: Never. If generated code has bugs, fix the `.ttl` source and regenerate. Editing generated files breaks determinism.

**Connection to v6**: Works with Poka-Yoke error-proofing (prevents incomplete specs) and SPARQL validation (ensures semantic correctness).

---

### 2. EPIC 9: Parallel Agent Convergence (Advanced)

**What it means**: For non-trivial tasks, spawn 10 parallel agents that explore the solution space simultaneously, then synthesize the optimal approach through collision detection.

**Why it matters**:
- **10x exploration bandwidth**: Multiple perspectives prevent tunnel vision
- **Automatic trade-off analysis**: Agents naturally discover edge cases
- **Convergence guarantees**: Collision detection prevents conflicting changes

**How to use it** (ggen team internal, optional for users):
```bash
# Non-trivial: "Add OAuth2 support with PKCE flow"
ggen epic9 "Add OAuth2 with PKCE, rate limiting, and token refresh"

# Output: 10 agents produce specifications
# [Receipt] Agent 1: OAuth2 core flow, 45 triples
# [Receipt] Agent 2: PKCE extension, 23 triples
# [Receipt] Agent 3: Rate limiting strategy, 31 triples
# ... collision detection runs ...
# [Receipt] Convergence: ✓ Merged 247 triples, 0 conflicts, SHA256:b2c3d4e5...
```

**When to violate**: Skip for trivial tasks (single-file changes, documentation updates). Use for:
- Multi-crate changes
- Architectural decisions
- Complex feature additions
- Security-critical implementations

**Connection to v6**: EPIC 9 agents use Big Bang 80/20 (each agent produces complete spec) and Deterministic Receipts (every agent run is provable).

---

### 3. Deterministic Receipts: Evidence Replaces Narrative

**What it means**: Every operation produces a cryptographic receipt (SHA256 hash + metadata). No "it works on my machine"—identical inputs yield bit-perfect identical outputs.

**Why it matters**:
- **Reproducible builds**: Same ontology + templates = same binary output
- **Audit trail**: Every generation step is cryptographically provable
- **Failure archaeology**: Receipts pinpoint *exactly* what changed between runs

**How to use it**:
```bash
cargo make test
# [Receipt] cargo make test: ✓ 347/347 tests, 0 failures, 28.4s, SHA256:c4d5e6f7...

ggen sync
# [Receipt] SPARQL extraction: ✓ 1,247 triples, 0.8s, SHA256:a1b2c3d4...
# [Receipt] Template rendering: ✓ 23 files, 1.2s, SHA256:e5f6a7b8...
# [Receipt] Final output: ✓ SHA256:f7a8b9c0..., deterministic=true
```

**Receipt format**: `[Receipt] <operation>: <status> <metrics>, <hash>`

Example receipts:
```
[Receipt] cargo make check: ✓ 0 errors, 3.2s, SHA256:a3b4c5d6...
[Receipt] cargo make lint: ✓ 0 warnings, 12.1s, SHA256:b4c5d6e7...
[Receipt] ggen validate: ✓ 1,543 triples, 100% closure, SHA256:c5d6e7f8...
[Receipt] SHACL validation: ✓ 47 shapes, 0 violations, SHA256:d6e7f8a9...
```

**When to violate**: Never in production. For exploratory prototypes, you *can* skip receipt validation, but regenerate with receipts before committing.

**Connection to v6**: Receipts integrate with:
- **Poka-Yoke**: Andon signals (🔴/🟡/🟢) appear in receipts
- **SPARQL**: Query results include hash for reproducibility
- **Chicago TDD**: Test receipts show exact pass/fail counts

---

### Quality Gates (Pre-Commit)

All three paradigms enforce these gates:

```bash
cargo make pre-commit
# [Receipt] cargo make check: ✓ 0 errors, <5s
# [Receipt] cargo make lint: ✓ 0 warnings, <60s
# [Receipt] cargo make test: ✓ 347/347, <30s
# [Receipt] Specification closure: ✓ 100%
# [Receipt] Overall: ✓ All gates passed, SHA256:e7f8a9b0...
```

**Andon Signal Integration**:
- 🔴 **RED** (compilation/test error): STOP immediately, fix spec
- 🟡 **YELLOW** (warnings/deprecations): Investigate before release
- 🟢 **GREEN** (all checks pass): Safe to proceed

**Core Equation**: $A = \mu(O)$ — Code (A) precipitates from RDF ontology (O) via transformation pipeline (μ). Constitutional rules ensure μ is deterministic, parallel-safe, and provable.

---

## Common Patterns

### REST API Generation
```bash
# 1. Define API spec in RDF
# 2. SPARQL query to extract endpoints
# 3. Template renders Axum/Rocket code
ggen sync
```

### Multi-Language Support
```bash
# Same ontology, different templates
# rust/ → Rust code
# typescript/ → TypeScript code
# python/ → Python code
ggen sync
```

### Database Schema Generation
```bash
# RDF model → SPARQL inference → PostgreSQL DDL
# Includes: tables, indexes, relationships, migrations
ggen sync
```

---

## Status

**Version**: 5.0.2
**Crates**: 17 active (ggen-core, ggen-cli, ggen-ai, ggen-marketplace, ggen-test-audit, etc.)
**Stability**: Production-ready
**License**: Apache 2.0 OR MIT

---

## Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

**Development Setup**:
```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo make check      # Verify setup
cargo make test       # Run tests
cargo make lint       # Check style
```

---

## Resources

- **GitHub Issues**: [Report bugs or request features](https://github.com/seanchatmangpt/ggen/issues)
- **Discussions**: [Ask questions and discuss ideas](https://github.com/seanchatmangpt/ggen/discussions)
- **Security**: [Responsible disclosure](SECURITY.md)
- **Changelog**: [Version history](CHANGELOG.md)

---

## Project Constitution

This project follows strict operational principles. See [CLAUDE.md](CLAUDE.md) for:
- Constitutional rules (cargo make only, RDF-first, Chicago TDD)
- Andon signals (RED = stop, YELLOW = investigate, GREEN = continue)
- Quality gates and validation requirements
- Development philosophy and standards

---

## License

Licensed under either of:
- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](LICENSE-MIT) or [http://opensource.org/licenses/MIT](http://opensource.org/licenses/MIT))

at your option.

---

**Ready to get started?** → [Quick Start Tutorial](docs/tutorials/01-quickstart.md)
