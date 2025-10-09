<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [rgen v0.1.0](#rgen-v010)
  - [🚀 Installation](#-installation)
    - [Build from Source (Recommended)](#build-from-source-recommended)
  - [⚡ Quick Start](#-quick-start)
    - [1. Create a Template Directory](#1-create-a-template-directory)
    - [2. Create Your First Template](#2-create-your-first-template)
    - [3. List Available Templates](#3-list-available-templates)
    - [4. Generate Code (Coming Soon)](#4-generate-code-coming-soon)
  - [📁 Project Structure](#-project-structure)
  - [🧩 Template Structure](#-template-structure)
  - [💡 Available Commands](#-available-commands)
  - [🔍 Hazard Report Example](#-hazard-report-example)
  - [🎯 Template Features](#-template-features)
    - [Tera Template Engine](#tera-template-engine)
    - [RDF & SPARQL Integration](#rdf--sparql-integration)
    - [File Injection](#file-injection)
    - [Idempotency](#idempotency)
    - [Shell Hooks](#shell-hooks)
  - [🔁 Deterministic Generation](#-deterministic-generation)
  - [📦 Example Templates](#-example-templates)
    - [CLI Subcommand Template](#cli-subcommand-template)
    - [API Endpoint Template](#api-endpoint-template)
  - [🧰 Development Commands](#-development-commands)
  - [📚 Template Frontmatter Reference](#-template-frontmatter-reference)
    - [Core Fields](#core-fields)
    - [RDF & SPARQL](#rdf--sparql)
    - [Injection & Idempotency](#injection--idempotency)
    - [Shell & Determinism](#shell--determinism)
  - [🏗️ Architecture](#-architecture)
  - [🔧 Current Limitations (v0.1.0)](#-current-limitations-v010)
    - [Working ✅](#working-)
    - [In Development 🚧](#in-development-)
    - [Requires Backend ⚠️](#requires-backend-)
  - [🎓 Understanding rgen](#-understanding-rgen)
    - [Core Concepts](#core-concepts)
  - [🚧 Roadmap](#-roadmap)
    - [v0.2.0 (Planned)](#v020-planned)
    - [v0.3.0 (Planned)](#v030-planned)
    - [v1.0.0 (Future)](#v100-future)
  - [🤝 Contributing](#-contributing)
  - [📖 Documentation](#-documentation)
  - [🔒 License](#-license)
  - [🙏 Acknowledgments](#-acknowledgments)
  - [⚡ Quick Reference](#-quick-reference)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# rgen v0.1.0

**Language-agnostic code generator with RDF-based semantic templates.**

`rgen` is a deterministic code generation framework that treats software artifacts as projections of knowledge graphs. Define your intent once using RDF/SPARQL metadata, then generate code for any target language.

---

## 🚀 Installation

### Build from Source (Recommended)

```bash
# Clone the repository
git clone https://github.com/seanchatmangpt/rgen
cd rgen

# Build with cargo-make (required)
cargo make build

# Run rgen
./target/debug/rgen --version
# Output: rgen 0.1.0

# Optional: Install to system
cargo make install
```

**Requirements:**
- Rust 1.90.0 or later
- cargo-make 0.37.x or later (`cargo install cargo-make`)

---

## ⚡ Quick Start

### 1. Create a Template Directory

```bash
mkdir -p templates/hello
```

### 2. Create Your First Template

Create `templates/hello/greeting.tmpl`:

```yaml
---
to: output/{{ name }}.txt
vars:
  name: world
  greeting: Hello
---
{{ greeting }}, {{ name }}!
```

### 3. List Available Templates

```bash
rgen list
```

Output:
```
Available templates:
===================

📄 hello/greeting.tmpl
   Output: output/{{ name }}.txt
   Variables:
     greeting: Hello
     name: world

Total: 1 template(s)
```

### 4. Generate Code (Coming Soon)

**Note:** Template generation via CLI is currently under development. The template discovery and listing functionality is working.

---

## 📁 Project Structure

```
your-project/
├── templates/          # Your template files (.tmpl)
│   ├── cli/
│   │   └── command.tmpl
│   ├── api/
│   │   └── endpoint.tmpl
│   └── ...
├── output/            # Generated files (configurable)
├── graphs/            # Optional RDF knowledge graphs
│   └── *.ttl
└── rgen.toml          # Optional project configuration
```

---

## 🧩 Template Structure

Templates consist of YAML frontmatter and a body:

```yaml
---
# Output path (supports Tera template variables)
to: src/{{ module }}.rs

# Default variables
vars:
  module: example
  description: "Example module"

# Optional: RDF graphs to load
rdf:
  - "graphs/schema.ttl"

# Optional: Inline RDF (Turtle format)
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:{{ module }} a ex:Module ."

# Optional: SPARQL queries for variable extraction
sparql:
  get_modules: "SELECT ?module WHERE { ?module a ex:Module }"

# Optional: Namespace prefixes
prefixes:
  ex: "http://example.org/"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"

# Optional: Deterministic generation
determinism:
  seed: "{{ module }}"
  sort_order: ["module"]

# Optional: File handling
force: false           # Overwrite existing files
unless_exists: false   # Only create if doesn't exist

# Optional: Injection mode (modify existing files)
inject: false
append: false          # Append to end of file
prepend: false         # Prepend to start of file
before: "pattern"      # Insert before matching line
after: "pattern"       # Insert after matching line
at_line: 10           # Insert at specific line number

# Optional: Idempotency
skip_if: "pattern"     # Skip if pattern exists in target file
idempotent: false      # Skip if content already exists

# Optional: Shell hooks
sh_before: "echo 'Generating...'"
sh_after: "rustfmt {{ output_path }}"
---
// Template body using Tera syntax
/// {{ description }}
pub mod {{ module }} {
    // Generated code here
}
```

---

## 💡 Available Commands

| Command | Description | Status |
|---------|-------------|--------|
| `rgen list` | List available templates in `templates/` directory | ✅ Working |
| `rgen hazard` | Generate project hazard/health report | ✅ Working |
| `rgen packs` | List installed rpacks (marketplace packages) | ✅ Working |
| `rgen completion <shell>` | Generate shell completions (bash, zsh, fish) | ✅ Working |
| `rgen gen <template>` | Generate code from template | 🚧 In Development |
| `rgen show <template>` | Show template metadata | 🚧 Has Issues |
| `rgen lint <template>` | Validate template syntax | 🚧 Has Issues |
| `rgen search <query>` | Search marketplace (requires registry) | ⚠️ Requires Backend |
| `rgen add <rpack>` | Install marketplace package | ⚠️ Requires Backend |
| `rgen categories` | Browse marketplace categories | ⚠️ Requires Backend |
| `rgen update` | Update installed packages | ⚠️ Requires Backend |
| `rgen remove <rpack>` | Remove installed package | ⚠️ Requires Backend |
| `rgen graph` | Export merged RDF graph | 📝 Documented |

---

## 🔍 Hazard Report Example

The `hazard` command provides helpful project health checks:

```bash
rgen hazard
```

Output:
```
🔍 RGen Hazard Report
====================

⚠️  Found 3 potential hazard(s):

1. LOW - Templates directory 'examples' not found
   💡 Recommendation: Create a templates directory to organize your templates

2. MEDIUM - No RDF files found in project
   💡 Recommendation: Add RDF files to enable graph-based code generation

3. LOW - Configuration file 'rgen.toml' not found
   💡 Recommendation: Consider adding configuration for better project management
```

---

## 🎯 Template Features

### Tera Template Engine

rgen uses [Tera](https://keats.github.io/tera/) for template rendering:

```jinja2
{{ variable }}                    {# Basic variable #}
{{ name | title }}                {# Filters #}
{% if condition %}...{% endif %}  {# Conditionals #}
{% for item in list %}...{% endfor %}  {# Loops #}
```

### RDF & SPARQL Integration

Embed semantic knowledge graphs directly in templates:

```yaml
---
to: output/{{ entity }}.rs
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:Person a rdfs:Class ."
sparql:
  entities: |
    SELECT ?entity WHERE {
      ?entity a ex:Person
    }
---
// Code generated from RDF knowledge graph
```

### File Injection

Modify existing files instead of creating new ones:

```yaml
---
to: existing_file.rs
inject: true
append: true
---
// This content will be appended to existing_file.rs
```

Injection modes:
- `append`: Add to end of file
- `prepend`: Add to start of file
- `before: "pattern"`: Insert before line matching pattern
- `after: "pattern"`: Insert after line matching pattern
- `at_line: N`: Insert at specific line number

### Idempotency

Prevent duplicate insertions:

```yaml
---
to: config.toml
inject: true
skip_if: "my_feature"  # Skip if this pattern exists
idempotent: true       # Skip if content already exists
---
[my_feature]
enabled = true
```

### Shell Hooks

Run commands before/after generation:

```yaml
---
to: src/generated.rs
sh_before: "cargo fmt --check"
sh_after: "rustfmt {{ output_path }}"
---
// Generated Rust code
```

---

## 🔁 Deterministic Generation

rgen ensures byte-identical output for the same inputs:

```yaml
---
determinism:
  seed: "{{ module }}-{{ version }}"
  sort_order: ["module", "version"]
---
```

Same graph + seed = identical output every time.

---

## 📦 Example Templates

The repository includes example templates in `examples/`:

### CLI Subcommand Template

`examples/cli-subcommand/rust.tmpl`:
- Generates Clap-based CLI subcommand handlers
- Integrates with RDF graphs for metadata
- Uses SPARQL for command discovery
- Deterministic output with configurable seed

### API Endpoint Template

`examples/api-endpoint/rust.tmpl`:
- Generates Axum REST API endpoints
- RDF-driven route definitions
- SPARQL-based endpoint discovery
- Type-safe handler generation

---

## 🧰 Development Commands

All development tasks use `cargo make`:

```bash
# Quick development workflow
cargo make quick       # Format + unit tests
cargo make dev         # Format + lint + tests

# Building
cargo make build       # Debug build
cargo make build-release  # Optimized release build

# Testing
cargo make test        # All tests
cargo make test-unit   # Unit tests only
cargo make test-single-threaded  # Deterministic tests

# Code Quality
cargo make fmt         # Format code
cargo make lint        # Strict clippy checks
cargo make audit       # Security audit

# CI/CD
cargo make ci          # Full CI pipeline
cargo make pre-commit  # Pre-commit checks
cargo make pre-push    # Pre-push comprehensive checks

# Documentation
cargo make doc         # Generate and open docs

# Utilities
cargo make watch       # Watch for changes
cargo make clean       # Clean build artifacts
```

**Important:** Never use direct `cargo` commands. Always use `cargo make` for all workflows.

---

## 📚 Template Frontmatter Reference

### Core Fields

| Field | Type | Description |
|-------|------|-------------|
| `to` | String | Output file path (supports Tera variables) |
| `from` | String | Alternative source file (overrides body) |
| `vars` | Map | Default variables |
| `force` | Boolean | Overwrite existing files (default: false) |
| `unless_exists` | Boolean | Only create if file doesn't exist |

### RDF & SPARQL

| Field | Type | Description |
|-------|------|-------------|
| `rdf` | String/Array | RDF file paths to load |
| `rdf_inline` | String/Array | Inline RDF (Turtle format) |
| `sparql` | String/Map | SPARQL queries for variables |
| `prefixes` | Map | Namespace prefixes |
| `base` | String | Base IRI for relative URIs |
| `shape` | String/Array | SHACL shape files |

### Injection & Idempotency

| Field | Type | Description |
|-------|------|-------------|
| `inject` | Boolean | Enable injection mode |
| `append` | Boolean | Append to file |
| `prepend` | Boolean | Prepend to file |
| `before` | String | Insert before pattern |
| `after` | String | Insert after pattern |
| `at_line` | Number | Insert at line number |
| `skip_if` | String | Skip if pattern exists |
| `idempotent` | Boolean | Skip if content exists |
| `backup` | Boolean | Create backup before injection |

### Shell & Determinism

| Field | Type | Description |
|-------|------|-------------|
| `sh_before` | String | Shell command before generation |
| `sh_after` | String | Shell command after generation |
| `determinism` | Object | Seed and sort config |

---

## 🏗️ Architecture

rgen is organized as a Rust workspace:

```
rgen/
├── src/           # Binary entry point
├── cli/           # CLI argument parsing & subcommands
├── core/          # Generation engine
│   ├── pipeline   # Template rendering pipeline
│   ├── template   # Frontmatter parsing
│   ├── graph      # RDF/SPARQL integration
│   ├── generator  # High-level orchestration
│   └── registry   # Marketplace client (WIP)
├── utils/         # Shared utilities
│   ├── app_config # Configuration management
│   ├── logger     # Structured logging
│   └── error      # Error types
├── templates/     # Project-specific templates
└── examples/      # Example templates
```

**Key Components:**
- **Pipeline**: Orchestrates Tera rendering + RDF processing
- **Template**: Parses frontmatter + body, handles injection
- **Graph**: Thread-safe Oxigraph wrapper with SPARQL caching
- **Generator**: High-level API for template execution

---

## 🔧 Current Limitations (v0.1.0)

### Working ✅
- Template discovery and listing (`rgen list`)
- Project health checks (`rgen hazard`)
- Shell completion generation (`rgen completion`)
- Template parsing with frontmatter
- RDF graph loading and SPARQL queries
- Local template organization

### In Development 🚧
- Template generation via CLI (`rgen gen`)
- Template preview (`rgen show` - has rendering issues)
- Template validation (`rgen lint` - has rendering issues)
- Complete end-to-end generation workflow

### Requires Backend ⚠️
- Marketplace search (`rgen search`)
- Package installation (`rgen add`)
- Package updates (`rgen update`)
- Registry synchronization

**Note:** The marketplace features require a registry backend at:
`https://raw.githubusercontent.com/seanchatmangpt/rgen/master/registry/`

This is currently under development.

---

## 🎓 Understanding rgen

### Core Concepts

**Templates are Blueprints**
- YAML frontmatter defines metadata and behavior
- Tera template body defines output structure
- Variables can come from CLI, frontmatter, or SPARQL

**RDF Graphs are the Ledger**
- Single source of truth for domain knowledge
- Templates project graph data into code
- SPARQL extracts structured data from graphs

**Determinism is a Guarantee**
- Same inputs → identical bytes
- Configurable seed ensures reproducibility
- Sort order maintains consistency

**Injection Enables Evolution**
- Modify existing files without replacing them
- Idempotency prevents duplicate insertions
- Shell hooks enable post-processing

---

## 🚧 Roadmap

### v0.2.0 (Planned)
- ✅ Complete CLI generation workflow
- ✅ Fix `show` and `lint` commands
- ✅ Template variable resolution from CLI
- ✅ Dry-run mode testing
- ✅ Comprehensive error messages

### v0.3.0 (Planned)
- 🏪 Marketplace backend implementation
- 📦 Rpack creation and publishing tools
- 🔍 Working search and package management
- 🔄 Dependency resolution for rpacks

### v1.0.0 (Future)
- 🎯 Production-ready marketplace
- 🧪 Property-based testing for determinism
- 📊 Performance benchmarking suite
- 🌍 Multi-language template ecosystem

---

## 🤝 Contributing

rgen follows strict development practices:

1. **Always use `cargo make`** - Direct cargo commands are not allowed
2. **No `unwrap()` in libraries** - All errors must be handled
3. **Tests required** - Unit + integration for all features
4. **Documentation required** - Public APIs must be documented
5. **Determinism verified** - Use `cargo make deterministic`

See `CLAUDE.md` for complete development guidelines.

---

## 📖 Documentation

- [`CLAUDE.md`](CLAUDE.md) - Development guide for Claude Code
- [`docs/readme-validation.md`](docs/readme-validation.md) - Feature verification report
- [`docs/`](docs/) - Additional documentation
- [Tera Documentation](https://keats.github.io/tera/) - Template syntax reference

---

## 🔒 License

MIT © Sean Chatman

---

## 🙏 Acknowledgments

Built with:
- [Tera](https://github.com/Keats/tera) - Template engine
- [Oxigraph](https://github.com/oxigraph/oxigraph) - RDF/SPARQL engine
- [Clap](https://github.com/clap-rs/clap) - CLI framework
- [cargo-make](https://github.com/sagiegurari/cargo-make) - Build automation

---

## ⚡ Quick Reference

```bash
# Setup
git clone https://github.com/seanchatmangpt/rgen && cd rgen
cargo make build

# Create templates
mkdir -p templates/myapp
# Add .tmpl files to templates/

# Discover templates
rgen list

# Check project health
rgen hazard

# Generate completions
rgen completion bash > ~/.bash_completion.d/rgen

# Development
cargo make quick  # Fast iteration
cargo make ci     # Full validation
```

---

**Version:** 0.1.0
**Repository:** https://github.com/seanchatmangpt/rgen
**Status:** Early Development - Local Template Features Working
