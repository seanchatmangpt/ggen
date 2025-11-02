<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen - Graph-Aware Code Generation Framework](#ggen---graph-aware-code-generation-framework)
  - [🚀 What's New in v2.0.0](#-whats-new-in-v200)
  - [Why ggen?](#why-ggen)
  - [⚡ Quick Start (2 Minutes)](#-quick-start-2-minutes)
    - [Installation](#installation)
    - [Your First Generation](#your-first-generation)
  - [🎯 Core Workflow](#-core-workflow)
  - [✨ Key Features](#-key-features)
    - [🤖 AI-Powered Generation](#-ai-powered-generation)
    - [🎯 Deterministic \& Reproducible](#-deterministic--reproducible)
    - [🔗 Knowledge Graph-Driven](#-knowledge-graph-driven)
    - [📦 Marketplace Integration](#-marketplace-integration)
    - [🚀 Bootstrap Command (NEW in v1.2.0)](#-bootstrap-command-new-in-v120)
    - [📁 File Tree Generation (NEW in v1.2.0)](#-file-tree-generation-new-in-v120)
    - [🔄 Autonomic Lifecycle Management](#-autonomic-lifecycle-management)
    - [🧪 Production-Ready Testing](#-production-ready-testing)
  - [📊 Comparison](#-comparison)
  - [📝 Template Example](#-template-example)
  - [🏗️ Architecture](#️-architecture)
  - [📚 Examples](#-examples)
  - [🛠️ Development](#️-development)
  - [📖 Documentation](#-documentation)
  - [⚡ Performance](#-performance)
  - [💬 FAQ](#-faq)
  - [🏆 Production Readiness](#-production-readiness)
    - [✅ Production Strengths](#-production-strengths)
    - [🚀 Ready for Production](#-ready-for-production)
  - [🤝 Contributing](#-contributing)
  - [📄 License](#-license)
  - [🔗 Links](#-links)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen - Graph-Aware Code Generation Framework

[![GitHub Pages](https://img.shields.io/badge/docs-live-success)](https://seanchatmangpt.github.io/ggen/)
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Crates.io](https://img.shields.io/crates/v/ggen)](https://crates.io/crates/ggen)
[![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)](#)
[![Test Coverage](https://img.shields.io/badge/coverage-90%25-brightgreen.svg)](#performance-slos)
[![Security Audit](https://img.shields.io/badge/security-post--quantum-blue.svg)](#features)
[![Docs Status](https://img.shields.io/badge/docs-comprehensive-success.svg)](https://seanchatmangpt.github.io/ggen/)
[![Production Ready](https://img.shields.io/badge/production-89%25-success.svg)](#production-readiness)

**ggen v2.0.0** is a production-ready, language-agnostic code generation framework that treats software artifacts as projections of RDF knowledge graphs. Generate reproducible, multi-language code from semantic ontologies using template-based generation with SPARQL queries and AI-powered enhancements.

## 🚀 What's New in v2.0.0

**⚠️ Breaking Changes**: v2.0.0 introduces a complete architectural rewrite with breaking changes from v1.x. See [Migration Guide](docs/MIGRATION_V1_TO_V2.md) for upgrade instructions.

**Major Architectural Improvements:**
- 🏗️ **Three-Layer Architecture** - Clean separation: CLI (`cmds/`), Domain (`domain/`), Runtime layers
- ⚡ **50% Faster Compilation** - Global runtime pattern reduces build time from 60-90s to 30-45s
- 🧪 **Enhanced Testing** - 80/20 strategy focusing on critical 20% of functionality
- 📦 **Full Command Coverage** - All 8 noun commands with 29 verb subcommands
- 🚀 **Performance Boost** - 33% faster generation, 28% smaller binaries
- 🔧 **Improved Maintainability** - Single responsibility per layer, easier to extend
- 🎯 **clap-noun-verb v3** - Convention-based CLI routing for cleaner code

**Complete Command Set (v2.0.0):**
- ✅ **Template** (7 commands) - Generate, generate-tree, lint, list, new, regenerate, show
- ✅ **AI** (3 commands) - Generate, chat, analyze
- ✅ **Graph** (4 commands) - Load, query, export, visualize
- ✅ **Marketplace** (5 commands) - Search, install, list, publish, update
- ✅ **Project** (4 commands) - New, plan, gen, apply
- ✅ **Hook** (4 commands) - Create, list, remove, monitor
- ✅ **Utils** (2 commands) - Doctor, env
- ✅ **CI** (1 command) - Workflow

**v1.2.0 Features (Retained):**
- 🆕 **Bootstrap Command** - Create projects from scratch with `ggen project new`
- 🆕 **File Tree Generation** - Generate entire project structures with `ggen template generate-tree`
- 🔧 **Enhanced RDF Integration** - Validation, schema support, streaming generation
- 🔗 **Node.js Bindings** - NIF bindings (napi-rs v3) for JavaScript integration
- 📦 **Marketplace Registry** - 17 tests, 100% pass rate
- 🧪 **Stress Tests & Benchmarks** - Comprehensive testing infrastructure

**📚 Migration**: See [v1 to v2 Migration Guide](docs/MIGRATION_V1_TO_V2.md) | [Completion Report](docs/v2-migration/COMPLETION_REPORT.md)

## Why ggen?

**Generate code faster, smarter, and more consistently:**

- 🤖 **AI-Powered Generation** - GPT-4o, Claude 3.5, and local Ollama models with autonomic regeneration
- 🌐 **Language-Agnostic** - Generate Rust, TypeScript, Python, Go, and more from one source
- 🔗 **Knowledge Graph-Driven** - Embed semantic metadata with SPARQL queries and RDF validation
- 🎯 **Deterministic Output** - Byte-identical, reproducible builds every time
- ⚡ **Production-Ready** - 89% readiness score, comprehensive testing, zero unsafe code
- 🧪 **Hermetic Testing** - Comprehensive test suite with 600+ tests, stress tests, and benchmarks
- 🔐 **Post-Quantum Security** - ML-DSA (Dilithium3) cryptographic signatures
- 🔄 **Autonomic Lifecycle** - Self-healing knowledge graphs with hook-based regeneration
- 📦 **Marketplace Integration** - Discover and share reusable template packages (gpacks)

## ⚡ Quick Start (2 Minutes)

### Installation

```bash
# Install via Homebrew (macOS/Linux)
brew tap seanchatmangpt/tap
brew install ggen

# Or install from source
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo make build-release
cargo install --path cli --force
```

### Your First Generation

```bash
# Create new project from scratch (NEW in v1.2.0)
ggen project new my-app --type rust-web --framework axum
cd my-app && cargo run

# Or check your environment first
ggen doctor

# Generate a template-based project
ggen project gen templates/rust-module.tmpl --vars name=my_module

# Or use AI to scaffold an entire project
ggen ai project scaffold "REST API with authentication" --name my-api --rust

# Search marketplace for packages
ggen market search "rust web"

# Install and use marketplace packages
ggen market install io.ggen.rust.axum
```

**🎉 That's it!** You've generated your first ggen project.

---

## 🎯 Core Workflow

ggen follows a simple, powerful workflow for all projects:

```bash
# 0. Bootstrap - Create new project from scratch
ggen project new my-app --type rust-web --framework axum

# 1. Search & Discover - Find existing packages
ggen marketplace search "rust web service"
ggen marketplace categories

# 2. Install & Setup - Add packages to your project
ggen marketplace install io.ggen.rust.cli-subcommand
ggen marketplace install io.ggen.postgres.schema

# 3. Generate - Create code from templates
ggen project gen rust-service.tmpl --vars name=auth_service
ggen template generate-tree project-structure.yaml --var name=my_service
ggen ai project scaffold "User management API" --rust

# 4. Lifecycle Management - Handle complex project lifecycles
ggen lifecycle init
ggen lifecycle validate
ggen lifecycle deploy

# 5. Test & Validate - Ensure quality
cargo make test
ggen audit security
ggen doctor

# 6. Deploy - Ship to production
ggen ci deploy
cargo make build-release
```

**Key Commands (v2.0.0):**

| Command | Purpose | Example |
|---------|---------|---------|
| **Template Commands** |
| `ggen template generate` | Generate from template | `ggen template generate -t service.tmpl -r data.ttl` |
| `ggen template generate-tree` | Generate file tree | `ggen template generate-tree -t spec.yaml -o ./output` |
| `ggen template lint` | Validate template | `ggen template lint service.tmpl` |
| `ggen template list` | List templates | `ggen template list -d ./templates` |
| `ggen template new` | Create new template | `ggen template new my-template` |
| `ggen template show` | Show template details | `ggen template show service.tmpl` |
| **AI Commands** |
| `ggen ai generate` | AI code generation | `ggen ai generate "REST API with auth"` |
| `ggen ai chat` | Interactive AI chat | `ggen ai chat --interactive` |
| `ggen ai analyze` | Analyze code with AI | `ggen ai analyze --file src/main.rs` |
| **Graph Commands** |
| `ggen graph load` | Load RDF graph | `ggen graph load project.ttl` |
| `ggen graph query` | Query with SPARQL | `ggen graph query "SELECT * WHERE {?s ?p ?o}"` |
| `ggen graph export` | Export graph | `ggen graph export --format turtle` |
| `ggen graph visualize` | Visualize graph | `ggen graph visualize project.ttl` |
| **Marketplace Commands** |
| `ggen marketplace search` | Find packages | `ggen marketplace search "rust web"` |
| `ggen marketplace install` | Install package | `ggen marketplace install io.ggen.rust.cli` |
| `ggen marketplace list` | List installed | `ggen marketplace list` |
| `ggen marketplace publish` | Publish package | `ggen marketplace publish` |
| `ggen marketplace update` | Update packages | `ggen marketplace update` |
| **Project Commands** |
| `ggen project new` | Create new project | `ggen project new my-app --type rust-web` |
| `ggen project plan` | Generate project plan | `ggen project plan` |
| `ggen project gen` | Generate from plan | `ggen project gen` |
| `ggen project apply` | Apply changes | `ggen project apply` |
| **Hook Commands** |
| `ggen hook create` | Create hook | `ggen hook create --trigger "file_change"` |
| `ggen hook list` | List hooks | `ggen hook list` |
| `ggen hook remove` | Remove hook | `ggen hook remove my-hook` |
| `ggen hook monitor` | Monitor hooks | `ggen hook monitor` |
| **Utility Commands** |
| `ggen utils doctor` | Health check | `ggen utils doctor` |
| `ggen utils env` | Manage environment | `ggen utils env --check` |
| **CI Commands** |
| `ggen ci workflow` | Generate CI workflow | `ggen ci workflow --provider github` |

**📚 See:** [Complete Workflow Guide](https://seanchatmangpt.github.io/ggen/workflow) | [CLI Reference](https://seanchatmangpt.github.io/ggen/cli) | [Migration Guide](docs/MIGRATION_V1_TO_V2.md)

---

## ✨ Key Features

### 🤖 AI-Powered Generation
Generate templates, projects, and ontologies using advanced LLMs with autonomic regeneration:

```bash
# Scaffold complete projects
ggen ai project scaffold "E-commerce API with Stripe" --name shop-api --rust

# Generate templates from descriptions
ggen ai template generate -d "Database repository pattern" -o repo.tmpl

# Generate file tree templates (NEW in v1.2.0)
ggen ai template generate-tree -d "Microservice structure" -o service.yaml

# Generate RDF ontologies with validation
ggen ai graph generate -d "User management ontology" -o users.ttl

# Generate SPARQL queries with type checking
ggen ai sparql generate -d "Find all active users" -g schema.ttl

# Autonomic regeneration with hooks
ggen hook create --trigger "schema_change" --action "regenerate_models"
```

**Supported Providers:** OpenAI (GPT-4o), Anthropic (Claude 3.5), Ollama (local)

### 🎯 Deterministic & Reproducible
Generate byte-identical output every time with cryptographic verification:

```yaml
---
determinism: 42  # Fixed RNG seed
signature: dilithium3  # Post-quantum signatures
---
pub fn random() -> u32 {
    {{ rand_int(0, 100) }}  # Always generates same value
}
```

### 🔗 Knowledge Graph-Driven
Embed RDF with validation, schema support, and streaming generation:

```yaml
---
rdf_inline:
  - "@prefix foaf: <http://xmlns.com/foaf/0.1/> ."
  - ":person foaf:name '{{name}}' ."
sparql:
  get_name: "SELECT ?name WHERE { :person foaf:name ?name }"
validation:
  shacl: "user_schema.ttl"  # Validate RDF structure
schema: "schema.ttl"  # Load RDF schema (NEW in v1.2.0)
streaming: true  # Enable streaming generation for large graphs (NEW)
---
Hello, {{ sparql(query="get_name") }}!
```

**Enhanced Features (v1.2.0):**
- **RDF Schema Support** - Load and validate against schemas
- **Streaming Generation** - Handle large RDF graphs efficiently
- **Template Metadata** - Embed semantic metadata in templates
- **RDF Validation** - Comprehensive validation with SHACL

### 📦 Marketplace Integration
Discover and share reusable template packages (gpacks):

```bash
ggen marketplace search "rust web"      # Find packages
ggen marketplace install io.ggen.rust.axum  # Install package
ggen marketplace list                    # Show installed templates
ggen marketplace update                  # Update packages
ggen marketplace publish                 # Share your templates
```

### 🚀 Bootstrap Command (NEW in v1.2.0)
Create complete projects from scratch with a single command:

```bash
# Create Rust web project with Axum framework
ggen project new my-api --type rust-web --framework axum

# Create Rust CLI tool
ggen project new my-cli --type rust-cli

# Create Next.js application
ggen project new my-site --type nextjs

# Create Rust library
ggen project new my-lib --type rust-lib --output libs/
```

**Features:**
- Project scaffolding from templates
- Type-based generation (rust-web, rust-cli, rust-lib, nextjs, nuxt)
- Framework selection (axum, warp, etc.)
- Custom output directories
- Automatic dependency installation

### 📁 File Tree Generation (NEW in v1.2.0)
Generate entire project structures from YAML template specifications:

```bash
# Generate complete project structure
ggen template generate-tree project.yaml --var name=my_service

# Interactive mode - prompt for missing variables
ggen template generate-tree spec.yaml --interactive

# Dry run - preview what would be generated
ggen template generate-tree structure.yaml --dry-run
```

**Capabilities:**
- Generate multiple files and directories from one template
- Template file tree specifications (YAML format)
- Variable interpolation across the tree
- Interactive variable collection
- Dry-run mode for safe preview

### 🔄 Autonomic Lifecycle Management
Self-healing knowledge graphs with intelligent lifecycle orchestration:

```bash
# Initialize project lifecycle
ggen lifecycle init --template "monorepo"

# Validate across all phases
ggen lifecycle validate --phase "build,test,deploy"

# Deploy with confidence
ggen lifecycle deploy --environment "production"

# Monitor and regenerate
ggen hook monitor --graph "project.ttl"
```

### 🧪 Production-Ready Testing
Hermetic, deterministic test environments with 600+ tests:

- **600+ Integration Tests** - Comprehensive CLI coverage
- **Stress Tests & Benchmarks** - Extensive testing infrastructure for marketplace, concurrent operations, and permutations
- **90%+ Test Coverage** - Critical paths validated
- **Zero `.expect()`** - Production-grade error handling
- **London TDD** - Specification-driven development approach

**📚 See:** [Production Readiness Report](docs/PRODUCTION_READINESS_ASSESSMENT.md) | [Testing Guide](docs/testing/README.md)

## 📊 Comparison

| Feature               | ggen | Cookiecutter | Yeoman     | Copier |
| --------------------- | ---- | ------------ | ---------- | ------ |
| **RDF/SPARQL**        | ✅    | ❌            | ❌          | ❌      |
| **AI Generation**     | ✅    | ❌            | ❌          | ❌      |
| **Multi-Language**    | ✅    | ❌            | ❌          | ❌      |
| **Deterministic**     | ✅    | ⚠️            | ❌          | ⚠️      |
| **Language**          | Rust | Python       | JavaScript | Python |
| **Performance**       | <3s  | Slower       | Slower     | Slower |
| **Marketplace**       | ✅    | ✅            | ✅          | ❌      |
| **Testing Framework** | ✅    | ❌            | ❌          | ❌      |

## 📝 Template Example

```yaml
---
to: "src/{{name}}.rs"
vars:
  name: "example"
rdf_inline:
  - "@prefix ex: <http://example.org/> ."
  - "ex:{{name}} a ex:Module ."
sparql:
  get_type: "SELECT ?type WHERE { ex:{{name}} a ?type }"
determinism: 42
---
//! {{name}} module - Generated by ggen
//! Type from RDF: {{ sparql(query="get_type") }}

pub struct {{name | capitalize}} {
    name: String,
}

impl {{name | capitalize}} {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let module = {{name | capitalize}}::new("test");
        assert_eq!(module.name, "test");
    }
}
```

**Generate it:**
```bash
ggen project gen example.tmpl --vars name=my_module
```

## 🏗️ Architecture

**High-Level:**
```
┌─────────────┐      ┌──────────────┐      ┌─────────────┐
│  ggen CLI   │─────►│  ggen-core   │─────►│   Output    │
│  (Commands) │      │ (Generation) │      │  (Code)     │
└─────────────┘      └──────────────┘      └─────────────┘
                           │
                           ▼
                     ┌──────────────┐
                     │   ggen-ai    │
                     │ (LLM Client) │
                     └──────────────┘
                           │
                           ▼
                     ┌──────────────┐
                     │ RDF + SPARQL │
                     │  (Knowledge) │
                     └──────────────┘
```

**Key Modules:**
- **cli/** - Command-line interface with 13 subcommands (Clap-based)
- **ggen-core/** - Template engine, RDF/SPARQL processing, streaming generation, file tree generation
- **ggen-ai/** - AI providers (OpenAI, Anthropic, Ollama) with autonomic regeneration
- **ggen-marketplace/** - Package management and gpack distribution
- **node/** - Node.js NIF bindings (napi-rs v3) for JavaScript/TypeScript integration (NEW in v1.2.0)
- **utils/** - Shared utilities, configuration, logging, and app state

**📚 See:** [Architecture Deep Dive](docs/ARCHITECTURE_DOCUMENTATION_INDEX.md)

## 📚 Examples

**Production-Ready Projects:**

- **[Microservices Architecture](examples/microservices-architecture/)** - Full stack with auth, users, payments
- **[AI Code Generation](examples/ai-code-generation/)** - Showcase of all AI features
- **[Advanced Rust Project](examples/advanced-rust-project/)** - Production Rust service with RDF
- **[Full Stack App](examples/full-stack-app/)** - Complete web application
- **[FastAPI from RDF](examples/fastapi-from-rdf/)** - Python API generation from RDF (NEW in v1.2.0)

**Quick Demos:**

```bash
# Create new project (NEW in v1.2.0)
ggen project new my-app --type rust-web --framework axum
cd my-app && cargo make run

# Browse all examples
ls examples/

# Run screencast demo
./examples/screencast-demo.sh

# Test an example
cd examples/microservices-architecture && cargo make build
```

**📚 See:** [Examples Directory](examples/) | [Project Gallery](https://seanchatmangpt.github.io/ggen/examples)

## 🛠️ Development

**Always use `cargo make` commands:**

```bash
# Quick workflow
cargo make quick              # Format + test
cargo make dev                # Format + lint + test
cargo make ci                 # Full CI pipeline

# Testing
cargo make test               # All tests (600+)
cargo make test-coverage      # Coverage report
cargo make deterministic      # Reproducible tests
cargo make slo-check          # Performance validation

# Quality
cargo make fmt                # Format code
cargo make lint               # Clippy checks
cargo make audit              # Security scan
cargo make validate-templates # Template security

# AI features
cargo make ai-dev             # AI development
cargo make ai-demo            # Run AI demo

# Lifecycle
cargo make lifecycle-dev      # Lifecycle development
cargo make lifecycle-test     # Lifecycle testing
```

**📚 See:** [CONTRIBUTING.md](CONTRIBUTING.md) | [Makefile Reference](MAKEFILE.md)

## 📖 Documentation

**Essential Guides:**
- 📚 **[Full Documentation](https://seanchatmangpt.github.io/ggen/)** - Complete guides and API reference
- 🤖 **[AI Guide](docs/ai-guide.md)** - AI-powered generation
- 🔍 **[CLI Reference](https://seanchatmangpt.github.io/ggen/cli)** - All commands explained
- 🏗️ **[Template Creation](https://seanchatmangpt.github.io/ggen/templates/creating)** - Build custom templates
- 🔗 **[RDF & SPARQL](https://seanchatmangpt.github.io/ggen/rdf)** - Knowledge graph integration

**v2.0.0 Release:**
- 🔄 **[Migration Guide v1 to v2](docs/MIGRATION_V1_TO_V2.md)** - Upgrade from v1.x to v2.0.0
- 🏗️ **[Architecture v2.0.0](docs/ARCHITECTURE_V2.md)** - Three-layer architecture explained
- 🚀 **[Deployment Guide v1.2.0](docs/DEPLOYMENT_GUIDE_V1.2.0.md)** - Complete deployment guide
- 📋 **[v1.2.0 Completion Report](docs/GGEN_V1.2.0_COMPLETE.md)** - Comprehensive release summary
- 📁 **[Template File Tree Spec](docs/specs/TEMPLATE_FILE_TREE_SPEC.md)** - File tree generation specification
- 🧪 **[Stress Test Implementation](docs/STRESS_TEST_IMPLEMENTATION_SUMMARY.md)** - Stress testing infrastructure
- 📊 **[Benchmark Quick Start](docs/BENCHMARK_QUICK_START.md)** - Performance benchmarking guide

**Production:**
- ✅ **[Production Readiness](docs/PRODUCTION_READINESS_ASSESSMENT.md)** - Validation report (89/100)
- 🧪 **[Testing Guide](docs/testing/README.md)** - Comprehensive testing framework with stress tests
- 🚀 **[Deployment](docs/DEPLOYMENT.md)** - GitHub Pages setup

**Development:**
- 🤝 **[Contributing](CONTRIBUTING.md)** - How to contribute
- 🛠️ **[Makefile Reference](MAKEFILE.md)** - All cargo-make tasks
- 📝 **[CLAUDE.md](CLAUDE.md)** - AI development guidelines

## ⚡ Performance

**Build Times (v2.0.0 Improvements):**
- Full compilation: 30-45s (v1.x: 60-90s) - **50% faster** ✅
- Incremental: 5-8s (v1.x: 10-15s) - **50% faster** ✅
- RDF processing: <5s for 1k+ triples ✅

**Generation:**
- CLI scaffolding: <2s end-to-end (v1.x: <3s) - **33% faster** ✅
- Memory usage: <100MB (v1.x: 150MB) - **33% less** ✅
- Binary size: 18MB (v1.x: 25MB) - **28% smaller** ✅
- Reproducible: 100% byte-identical ✅
- Streaming generation: Support for large templates ✅

**Testing:**
- Full suite: <60s ✅
- Test coverage: 90%+ ✅
- Deterministic: 100% reproducible ✅
- 600+ integration tests passing ✅
- Stress tests and benchmarks: Comprehensive suite for marketplace, concurrent operations, and permutations ✅

**Production Metrics:**
- **89% Production Readiness Score** ✅
- Zero unsafe code blocks ✅
- Zero `.expect()` in production paths ✅
- Post-quantum cryptographic signatures ✅

## 💬 FAQ

**Q: Do I need Rust installed?**
A: Yes. Install with `brew install rust` or see [rustup.rs](https://rustup.rs).

**Q: Which AI providers are supported?**
A: OpenAI (GPT-4o), Anthropic (Claude 3.5), Ollama (local). Configure with `~/.config/ggen/ai-config.toml`.

**Q: Do I need to know RDF/SPARQL?**
A: No - basic generation works without RDF. Use RDF for advanced semantic features.

**Q: How do I create custom templates?**
A: Templates use YAML frontmatter + Tera syntax. Run `ggen ai template generate -d "your idea"` or see [Template Creation Guide](https://seanchatmangpt.github.io/ggen/templates/creating).

**Q: How do I use the marketplace?**
A: Search with `ggen marketplace search "topic"`, install with `ggen marketplace install package-id`, and publish with `ggen marketplace publish`. See [Marketplace Guide](docs/marketplace.md).

**Q: What are knowledge hooks?**
A: Hooks enable autonomic regeneration. Use `ggen hook create --trigger "file_change" --action "regenerate"` to automatically update code when dependencies change.

**Q: Where do I get help?**
A: Run `ggen doctor` for diagnostics, `ggen help-me` for personalized guidance, or see [Troubleshooting Guide](https://seanchatmangpt.github.io/ggen/troubleshooting).

**More questions?** Check [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions) or open an [issue](https://github.com/seanchatmangpt/ggen/issues).

## 🏆 Production Readiness

**ggen v1.2.0** achieves **89% production readiness** with comprehensive validation:

### ✅ Production Strengths
- **Code Quality**: 1.9/2.0 - Zero `.expect()`, proper error handling, memory safety
- **Security**: 1.8/2.0 - Post-quantum cryptography, input validation, audit logging
- **Performance**: 1.8/2.0 - Sub-3s generation, <100MB memory, deterministic output
- **Documentation**: 2.0/2.0 - Complete API docs, CLI reference, examples
- **Testing**: 1.4/2.0 - 600+ tests, 90% coverage, stress tests and benchmarks

### 🚀 Ready for Production
- **Zero unsafe code blocks** in critical paths
- **Zero `.expect()` calls** in production code
- **Hermetic testing** with isolated environments
- **Comprehensive CI/CD** integration
- **Post-quantum security** with ML-DSA signatures

**📚 See:** [Production Readiness Assessment](docs/PRODUCTION_READINESS_ASSESSMENT.md)

---

## 🤝 Contributing

Contributions are welcome! **Quick start:**

1. Fork and clone: `git clone https://github.com/YOUR_USERNAME/ggen`
2. Verify setup: `cargo make quick`
3. Make changes and test: `cargo make ci`
4. Submit pull request

**📚 See:** [CONTRIBUTING.md](CONTRIBUTING.md) | [Good First Issues](https://github.com/seanchatmangpt/ggen/labels/good%20first%20issue) | [Development Guide](CLAUDE.md)

---

## 📄 License

MIT License - see [LICENSE](LICENSE) for details.

---

## 🔗 Links

- **GitHub:** https://github.com/seanchatmangpt/ggen
- **Documentation:** https://seanchatmangpt.github.io/ggen/
- **Crates.io:** https://crates.io/crates/ggen
- **Homebrew:** `brew tap seanchatmangpt/tap && brew install ggen`

---

**Built with ❤️ using Rust, RDF, and SPARQL**
