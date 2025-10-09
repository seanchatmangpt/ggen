<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [rgen](#rgen)
  - [🧭 Purpose](#-purpose)
  - [🚀 Install](#-install)
    - [Homebrew](#homebrew)
    - [Cargo](#cargo)
  - [⚙️ Quick start](#-quick-start)
    - [Using marketplace rpacks (recommended)](#using-marketplace-rpacks-recommended)
    - [Using local templates](#using-local-templates)
  - [🏪 Marketplace](#-marketplace)
    - [Discover rpacks](#discover-rpacks)
    - [Install and use](#install-and-use)
  - [🧩 Templates](#-templates)
    - [Example: `templates/cli/subcommand/rust.tmpl`](#example-templatesclisubcommandrusttmpl)
  - [💡 Commands](#-commands)
  - [🔁 Determinism](#-determinism)
  - [🧠 Example: Multi-language CLI generation](#-example-multi-language-cli-generation)
    - [Using marketplace rpacks](#using-marketplace-rpacks)
    - [Using local templates](#using-local-templates-1)
  - [🧰 Integrations](#-integrations)
  - [📦 Extend](#-extend)
    - [Create local templates](#create-local-templates)
    - [Publish rpacks to marketplace](#publish-rpacks-to-marketplace)
  - [🔒 License](#-license)
  - [📚 Documentation](#-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# rgen

**Language-agnostic generator for reproducible code projections.**
`rgen` turns one ontology into CLI subcommands, APIs, schema files, and docs for any target language.

---

## 🧭 Purpose

Developers repeat the same scaffolding logic across stacks. `rgen` removes the language barrier.
You describe the **intent** (command, type, or system capability) once as a graph or RDF-like metadata block.
rgen projects that intent into any target framework or language.

---

## 🚀 Install

### Homebrew

```bash
brew tap rgen-dev/tap
brew install rgen
rgen --version
```

### Cargo

```bash
cargo install rgen
```

---

## ⚙️ Quick start

**Goal:** generate a new CLI subcommand for any language.

### Using marketplace rpacks (recommended)
```bash
# Search for CLI subcommand templates
rgen search rust cli

# Install a high-quality rpack
rgen add io.rgen.rust.cli-subcommand

# Generate using the installed rpack
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello description="Print a greeting"
```

### Using local templates
```bash
rgen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

Output depends on your template set (Rust, Python, Bash, etc).
Each output is produced deterministically from the same RDF description.

---

## 🏪 Marketplace

The rgen marketplace provides a curated ecosystem of reusable code generation packs (rpacks). Discover, install, and use high-quality templates from the community.

### Discover rpacks

```bash
# Search for templates by language and type
rgen search rust cli
rgen search python api
rgen search typescript react

# Browse popular categories
rgen categories

# Get detailed information about a specific rpack
rgen show io.rgen.rust.cli-subcommand
```

### Install and use

```bash
# Install the latest version
rgen add io.rgen.rust.cli-subcommand

# Install specific version
rgen add io.rgen.rust.cli-subcommand@0.2.0

# List installed rpacks
rgen packs

# Update to latest versions
rgen update

# Use installed rpack templates
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=Users
```

Rpacks include templates, RDF schemas, SPARQL queries, and dependencies. They're versioned, tested, and maintained by the community.

---

## 🧩 Templates

```
templates/
  cli/
    subcommand/
      rust.tmpl
      python.tmpl
      bash.tmpl
```

Each `.tmpl` has a YAML frontmatter header that describes:

* `to:` — where to write the file
* `vars:` — default variables
* `rdf:` — optional RDF or JSON-LD defining the semantic object
* `sparql:` — queries to extract variables from graph data
* `determinism:` — optional seed for reproducibility

### Example: `templates/cli/subcommand/rust.tmpl`

```yaml
---
to: src/cmds/{{ slug }}.rs
vars: { cmd: hello, summary: "Print a greeting" }
rdf:
  inline:
    - mediaType: text/turtle
      text: |
        @prefix cli: <urn:rgen:cli#> .
        [] a cli:Command ;
           cli:slug "{{ cmd }}" ;
           cli:summary "{{ summary }}" .
sparql:
  vars:
    - name: slug
      query: |
        PREFIX cli: <urn:rgen:cli#>
        SELECT ?slug WHERE { ?c a cli:Command ; cli:slug ?slug } LIMIT 1
determinism: { seed: "{{ cmd }}" }
---
pub fn {{ slug }}(name: &str) {
    println!("hello {}", name);
}
```

Same RDF + seed → identical files every run.

---

## 💡 Commands

| Command                          | Description                                |
| -------------------------------- | ------------------------------------------ |
| **Marketplace**                  |                                            |
| `rgen search <query>`            | Search for rpacks in registry              |
| `rgen categories`                | Show popular categories and keywords       |
| `rgen add <rpack>`               | Install an rpack to the project            |
| `rgen remove <rpack>`            | Remove an rpack from the project           |
| `rgen packs`                     | List installed rpacks                      |
| `rgen update [rpack]`            | Update rpacks to latest versions           |
| **Generation**                   |                                            |
| `rgen gen <template>`            | Generate code from templates               |
| `rgen list`                      | List available template scopes and actions |
| `rgen show <template>`           | Show template and resolved context         |
| **Validation**                   |                                            |
| `rgen validate <template>`       | Validate RDF/SHACL graphs                  |
| `rgen lint <template>`           | Lint template with schema validation       |
| **Utilities**                    |                                            |
| `rgen graph export`              | Merge RDF sources into a single graph      |
| `rgen hazard`                    | Generate hazard report                     |
| `rgen completion <shell>`        | Generate shell completion scripts          |

---

## 🔁 Determinism

rgen computes a manifest hash over:

```
graph data + shape + frontmatter + template + seed
```

The same graph + seed = byte-identical results.

---

## 🧠 Example: Multi-language CLI generation

### Using marketplace rpacks

```bash
# Install multi-language CLI rpacks
rgen add io.rgen.rust.cli-subcommand
rgen add io.rgen.python.cli-subcommand
rgen add io.rgen.bash.cli-subcommand

# Generate for each language
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=status description="Show app status"
rgen gen io.rgen.python.cli-subcommand:cli/subcommand/python.tmpl name=status description="Show app status"
rgen gen io.rgen.bash.cli-subcommand:cli/subcommand/bash.tmpl name=status description="Show app status"
```

### Using local templates

```bash
rgen gen cli subcommand --vars cmd=status summary="Show app status"
```

Both approaches create:

```
src/cmds/status.rs
commands/status.py
commands/status.sh
```

All derived from one ontology.
No duplicated logic, no language bias.

---

## 🧰 Integrations

rgen doesn’t care about runtime:

* Works for **Rust**, **Python**, **Bash**, **Go**, **TypeScript**, etc.
* Graph-aware: uses RDF, JSON-LD, or YAML metadata.
* Deterministic output: same intent, same projection.

---

## 📦 Extend

### Create local templates

Add your own generator:

```bash
mkdir -p templates/api/endpoint
cp templates/cli/subcommand/rust.tmpl templates/api/endpoint/rust.tmpl
```

Edit frontmatter and target path.
rgen will detect and render automatically.

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

Rpacks support versioning, dependencies, and comprehensive testing. See the [marketplace documentation](docs/marketplace.md) for details.

---

## 🔒 License

MIT © rgen contributors

---

## 📚 Documentation

- [Installation Guide](docs/install.md) - Detailed installation instructions
- [Quick Start](docs/quickstart.md) - Get up and running quickly
- [Templates Guide](docs/templates.md) - Creating and using templates
- [Marketplace Guide](docs/marketplace.md) - Using and publishing rpacks
- [RDF & SPARQL](docs/rdf-shacl-sparql.md) - Semantic web integration
- [Determinism](docs/determinism.md) - Reproducible generation
- [CLI Reference](docs/cli.md) - Complete command reference

---

> **rgen** — one intent, many projections.
> Code is just a projection of knowledge.
