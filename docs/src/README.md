# ggen Documentation

**Language-agnostic generator for reproducible code projections.**

`ggen` turns one ontology into CLI subcommands, APIs, schema files, and docs for any target language.

---

## 🧭 Purpose

Developers repeat the same scaffolding logic across stacks. `ggen` removes the language barrier.

You describe the **intent** (command, type, or system capability) once as a graph or RDF-like metadata block. ggen projects that intent into any target framework or language.

## 🚀 Quick Start

### Using marketplace gpacks (recommended)

```bash
# Search for CLI subcommand templates
ggen search rust cli

# Install a high-quality gpack
ggen add io.ggen.rust.cli-subcommand

# Generate using the installed gpack
ggen gen io.ggen.rust.cli-subcommand:rust.tmpl cmd=hello description="Print a greeting"
```

### Using local templates

```bash
ggen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

## 🏪 Marketplace

The ggen marketplace provides a curated ecosystem of reusable code generation packs (gpacks) served via GitHub Pages with automated validation and deployment.

**Registry API**: [registry/index.json](registry/index.json)

### Discover gpacks

```bash
# Search for templates by language and type
ggen search rust cli
ggen search python api
ggen search typescript react

# Browse popular categories
ggen categories

# Get detailed information about a specific gpack
ggen show io.ggen.rust.cli-subcommand
```

### Install and use

```bash
# Install the latest version
ggen add io.ggen.rust.cli-subcommand

# Install specific version
ggen add io.ggen.rust.cli-subcommand@0.1.0

# List installed gpacks
ggen packs

# Update to latest versions
ggen update

# Use installed gpack templates
ggen gen io.ggen.rust.cli-subcommand:rust.tmpl cmd=users
```

## 📚 Documentation Sections

### [Getting Started](guides/install.md)
Get started quickly with installation, basic usage, and template development.

### [Core Concepts](concepts/frontmatter.md)
Understand the fundamental ideas behind ggen: templates, RDF integration, projections, and determinism.

### [Reference](reference/cli.md)
Complete CLI reference, troubleshooting guides, and technical details.

### [Advanced](advanced/calculus.md)
Deep dive into mathematical foundations, developer experience features, and gpack development.

### [Examples](examples/cli-subcommand-multi.md)
Real-world usage examples and tutorials.

## 🔧 Installation

### Homebrew

```bash
brew tap seanchatmangpt/tap
brew install ggen
```

### Cargo

```bash
cargo install ggen
```

## 🔁 Determinism

ggen computes a manifest hash over:

```
graph data + shape + frontmatter + template + seed
```

The same graph + seed = byte-identical results.

## 📦 Extend

### Create local templates

Add your own generator:

```bash
mkdir -p templates/api/endpoint
cp templates/cli/subcommand/rust.tmpl templates/api/endpoint/rust.tmpl
```

Edit frontmatter and target path. ggen will detect and render automatically.

### Publish gpacks to marketplace

Share your templates with the community:

```bash
# Initialize new gpack
ggen pack init

# Lint and test your gpack
ggen pack lint
ggen pack test

# Publish to registry
ggen pack publish
```

## 🔒 License

MIT © ggen contributors

---

> **ggen** — one intent, many projections.
> Code is just a projection of knowledge.
