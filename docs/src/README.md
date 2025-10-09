# rgen Documentation

**Language-agnostic generator for reproducible code projections.**

`rgen` turns one ontology into CLI subcommands, APIs, schema files, and docs for any target language.

---

## 🧭 Purpose

Developers repeat the same scaffolding logic across stacks. `rgen` removes the language barrier.

You describe the **intent** (command, type, or system capability) once as a graph or RDF-like metadata block. rgen projects that intent into any target framework or language.

## 🚀 Quick Start

### Using marketplace rpacks (recommended)

```bash
# Search for CLI subcommand templates
rgen search rust cli

# Install a high-quality rpack
rgen add io.ggen.rust.cli-subcommand

# Generate using the installed rpack
rgen gen io.ggen.rust.cli-subcommand:rust.tmpl cmd=hello description="Print a greeting"
```

### Using local templates

```bash
rgen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

## 🏪 Marketplace

The rgen marketplace provides a curated ecosystem of reusable code generation packs (rpacks) served via GitHub Pages with automated validation and deployment.

**Registry API**: [registry/index.json](registry/index.json)

### Discover rpacks

```bash
# Search for templates by language and type
rgen search rust cli
rgen search python api
rgen search typescript react

# Browse popular categories
rgen categories

# Get detailed information about a specific rpack
rgen show io.ggen.rust.cli-subcommand
```

### Install and use

```bash
# Install the latest version
rgen add io.ggen.rust.cli-subcommand

# Install specific version
rgen add io.ggen.rust.cli-subcommand@0.1.0

# List installed rpacks
rgen packs

# Update to latest versions
rgen update

# Use installed rpack templates
rgen gen io.ggen.rust.cli-subcommand:rust.tmpl cmd=users
```

## 📚 Documentation Sections

### [Getting Started](guides/install.md)
Get started quickly with installation, basic usage, and template development.

### [Core Concepts](concepts/frontmatter.md)
Understand the fundamental ideas behind rgen: templates, RDF integration, projections, and determinism.

### [Reference](reference/cli.md)
Complete CLI reference, troubleshooting guides, and technical details.

### [Advanced](advanced/calculus.md)
Deep dive into mathematical foundations, developer experience features, and rpack development.

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
cargo install rgen
```

## 🔁 Determinism

rgen computes a manifest hash over:

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

Edit frontmatter and target path. rgen will detect and render automatically.

### Publish rpacks to marketplace

Share your templates with the community:

```bash
# Initialize new rpack
rgen pack init

# Lint and test your rpack
rgen pack lint
rgen pack test

# Publish to registry
rgen pack publish
```

## 🔒 License

MIT © rgen contributors

---

> **rgen** — one intent, many projections.
> Code is just a projection of knowledge.
