<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen - Graph-Aware Code Generation Framework](#ggen---graph-aware-code-generation-framework)
  - [Features](#features)
  - [Quick Start](#quick-start)
    - [Installation](#installation)
    - [Basic Usage](#basic-usage)
  - [Template Example](#template-example)
  - [Architecture](#architecture)
  - [Key Capabilities](#key-capabilities)
    - [Deterministic Generation](#deterministic-generation)
    - [RDF + SPARQL Integration](#rdf--sparql-integration)
    - [Injection Modes](#injection-modes)
    - [GitHub Integration](#github-integration)
  - [Development](#development)
  - [Marketplace (gpacks)](#marketplace-gpacks)
  - [Documentation](#documentation)
  - [Performance SLOs](#performance-slos)
  - [Contributing](#contributing)
  - [License](#license)
  - [Repository](#repository)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen - Graph-Aware Code Generation Framework

[![GitHub Pages](https://img.shields.io/badge/docs-live-success)](https://seanchatmangpt.github.io/ggen/)
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Crates.io](https://img.shields.io/crates/v/ggen)](https://crates.io/crates/ggen)
[![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)](#)

**ggen** is a deterministic, language-agnostic code generation framework that treats software artifacts as projections of RDF knowledge graphs. Generate reproducible, multi-language code from a single semantic ontology using template-based generation with SPARQL queries and AI-powered enhancements.

## 🚀 **NEW: AI-Powered Generation v1.0.0**

- 🤖 **AI-Enhanced Templates** - Generate templates, SPARQL queries, and RDF graphs using advanced LLMs via rust-genai
- 🧠 **Intelligent Project Scaffolding** - Create entire project structures with AI assistance
- 🔍 **Natural Language Search** - Find templates and packages using conversational queries
- 📋 **Smart Frontmatter** - Generate and convert metadata using AI
- 🎯 **Latest Model Support** - GPT-4o, Claude 3.5, Qwen3-coder:30b, and more
- 🎪 **MCP Server** - Model Context Protocol server for AI tool integration
- 🔧 **Multi-Provider Support** - OpenAI, Anthropic, Ollama with unified configuration

📚 **[Full Documentation](https://seanchatmangpt.github.io/ggen/)**

## Features

- 🎯 **Deterministic Generation** - Byte-identical output with fixed seeds
- 🤖 **AI-Powered Generation** - Generate templates, SPARQL queries, and RDF graphs using advanced LLMs (GPT-4o, Claude 3.5, Qwen3-coder:30b)
- 🧠 **Intelligent Project Scaffolding** - Create entire multi-language projects with AI assistance
- 🔍 **Natural Language Search** - Find templates and packages using conversational queries
- 📋 **Smart Frontmatter** - Generate and convert YAML/JSON metadata using AI
- 🌐 **Language-Agnostic** - Generate code in any language from the same ontology
- 🔗 **RDF Knowledge Graphs** - Embed semantic metadata with SPARQL queries
- 📦 **Marketplace Integration** - Reusable template packages (gpacks) with versioning and AI discovery
- 🧪 **Template-Based** - YAML frontmatter with Tera templating engine
- 🔄 **Injection Support** - Modify existing files with idempotent updates
- 🚀 **GitHub Integration** - Built-in GitHub Pages and Actions API support
- 🔐 **Post-Quantum Security** (planned for v1.0.0) - ML-DSA (Dilithium3) signatures for quantum-resistant package integrity
- ⚡ **Performance SLOs** - Fast builds, low memory, reproducible outputs

## Quick Start

### Installation

**Homebrew (macOS/Linux):**
```bash
brew tap seanchatmangpt/tap
brew install ggen
```

**From Source:**
```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo make build-release
```

### Basic Usage

```bash
# Traditional template generation
ggen gen templates/rust-module.tmpl --vars name=my_module

# 🤖 AI-powered template generation
ggen ai generate -d "REST API module" -o api_module.rs

# 🧠 AI-powered SPARQL query generation
ggen ai sparql -d "Find all people" -g ontology.ttl -o query.sparql

# 📊 AI-powered RDF graph generation
ggen ai graph -d "Person ontology" -o person.ttl

# 🏗️ AI-powered project scaffolding
ggen ai project -d "Web service in Rust" -n myproject --rust

# 🔍 Natural language AI search
ggen ai search -d "I need a user authentication system"

# 📦 Smart frontmatter generation
ggen ai frontmatter -d "API controller" --json --yaml

# 🎪 Start MCP server for AI tools
ggen ai server --openai-key $OPENAI_API_KEY

# 📦 Search marketplace for templates
ggen search "rust cli"

# Add a template pack
ggen add io.ggen.rust.cli-subcommand

# List available templates
ggen list

# Check GitHub Pages status
ggen github pages-status
```

## Template Example

```yaml
---
to: "src/{{name}}.rs"
vars:
  name: "example"
  author: "ggen"
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:{{name}} a ex:Module ."
sparql:
  get_type: "SELECT ?type WHERE { ex:{{name}} a ?type }"
determinism: 42
---
//! {{name}} module
//! Generated by {{author}}

pub struct {{name | capitalize}} {
    // Module implementation
}

impl {{name | capitalize}} {
    pub fn new() -> Self {
        Self {}
    }
}
```

## Architecture

```
ggen/
├── cli/           # Clap CLI with subcommands
│   └── cmds/      # Individual command implementations
├── ggen-core/     # Core generation engine
│   ├── pipeline.rs   # Template rendering pipeline
│   ├── template.rs   # Frontmatter + body parsing
│   ├── graph.rs      # RDF graph with SPARQL caching
│   ├── generator.rs  # Generation orchestration
│   ├── registry.rs   # Marketplace client
│   └── github.rs     # GitHub API integration
├── ggen-ai/       # AI-powered generation capabilities
│   ├── client.rs     # Unified LLM client (rust-genai)
│   ├── generators/   # AI template, SPARQL, graph generators
│   ├── config/       # AI provider configuration (OpenAI, Anthropic, Ollama)
│   ├── mcp/         # Model Context Protocol server
│   └── security/     # Post-quantum cryptography
├── utils/         # Configuration, logging, errors
└── templates/     # Built-in templates
```

## Key Capabilities

### AI-Powered Generation
Generate templates, SPARQL queries, and RDF graphs using LLMs:

```bash
# Generate a template using AI (with rust-genai)
ggen ai generate -d "Database model" --provider openai --model gpt-4o

# Generate SPARQL queries from natural language
ggen ai sparql -d "Find all active users" -g schema.ttl --provider anthropic

# Generate RDF graphs from descriptions
ggen ai graph -d "E-commerce product ontology" -o products.ttl --provider ollama

# Generate complete project structures
ggen ai project -d "Web service with authentication" -n my-api --rust

# Start MCP server for AI tool integration
ggen ai server --provider openai --model gpt-4o
```

**Supported AI Providers:**
- **OpenAI** - GPT-4o, GPT-4o-mini (via rust-genai)
- **Anthropic** - Claude 3.5 Sonnet, Claude 3.5 Haiku (via rust-genai)
- **Ollama** - Qwen3-coder:30b, Llama 3, and more (local models)
- **MCP Server** - Model Context Protocol for AI assistant integration

### Deterministic Generation
Generate byte-identical output with fixed seeds:
```yaml
---
determinism: 42  # Fixed RNG seed
---
```

### RDF + SPARQL Integration
Embed semantic knowledge and query it:
```yaml
---
prefixes:
  foaf: "http://xmlns.com/foaf/0.1/"
rdf_inline:
  - "@prefix foaf: <http://xmlns.com/foaf/0.1/> . :person foaf:name \"{{name}}\" ."
sparql:
  get_name: "SELECT ?name WHERE { :person foaf:name ?name }"
---
Name from RDF: {{ sparql(query="get_name") }}
```

### Injection Modes
Modify existing files idempotently:
```yaml
---
to: "src/lib.rs"
inject:
  mode: "after"
  pattern: "pub mod"
  skip_if: "pub mod {{name}}"
---
pub mod {{name}};
```

### GitHub Integration
Built-in GitHub API commands:
```bash
# Check Pages deployment status
ggen github pages-status

# View workflow runs
ggen github workflow-status

# Trigger workflow
ggen github trigger-workflow
```

## Development

**CRITICAL:** Always use `cargo make` commands, never direct `cargo` commands.

```bash
# Quick development workflow
cargo make quick      # Format and test
cargo make dev        # Format, lint, test

# Testing
cargo make test                 # All tests
cargo make deterministic        # Fixed seeds + single-threaded
cargo make test-coverage        # Coverage report

# Code quality
cargo make fmt                  # Format
cargo make lint                 # Strict clippy
cargo make audit                # Security scan

# Build
cargo make build-release        # Release build
cargo make ci                   # Full CI workflow

# AI Development
cargo make ai-dev               # AI module development
cargo make ai-test              # AI tests only
cargo make ai-lint              # AI linting
cargo make ai-integration       # Test AI CLI commands
cargo make ai-demo              # Run AI demo
cargo make ai-models            # Test all AI providers
cargo make ai-models-local      # Test with local Ollama
cargo make validate-templates   # Validate AI templates
cargo make validate-rdf         # Validate RDF graphs
cargo make completions          # Generate shell completions

# GitHub/Pages
cargo make docs-build           # Build documentation
cargo make gh-pages-status      # Check Pages status
```

## Marketplace (gpacks)

Gpacks are versioned, reusable template packages:

```bash
# Search for gpacks
ggen search "rust"

# View categories
ggen categories

# Add an gpack
ggen add io.ggen.rust.cli-subcommand

# List installed gpacks
ggen packs

# Update gpacks
ggen update
```

## Documentation

- 📚 **[Full Documentation](https://seanchatmangpt.github.io/ggen/)** - Complete guides and API reference
🤖 **[AI Guide](docs/ai-guide.md)** - Comprehensive AI-powered generation guide
- 🚀 **[Deployment Guide](docs/DEPLOYMENT.md)** - GitHub Pages setup
- 🔧 **[GitHub API Integration](docs/GITHUB_API_RUST_INTEGRATION.md)** - Rust-based GitHub integration
- 📝 **[CLAUDE.md](CLAUDE.md)** - Development guidelines for Claude Code
- 🛠️ **[Makefile Reference](MAKEFILE.md)** - All cargo-make tasks

## Performance SLOs

- First build: ≤ 15s
- Incremental build: ≤ 2s
- RDF processing: ≤ 5s for 1k+ triples
- Generation memory: ≤ 100MB
- CLI scaffolding: ≤ 3s end-to-end
- 100% reproducible outputs

## Contributing

1. Follow the guidelines in [CLAUDE.md](CLAUDE.md)
2. Always use `cargo make` commands
3. Ensure `cargo make ci` passes before submitting
4. Add tests for new features
5. Update documentation

## License

MIT License - see [LICENSE](LICENSE) for details.

## Repository

**Source:** https://github.com/seanchatmangpt/ggen

**Homebrew Tap:**
```ruby
tap "seanchatmangpt/tap"
brew "ggen"
```

---

Built with ❤️ using Rust, RDF, and SPARQL
