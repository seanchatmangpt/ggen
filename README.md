<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [rgen](#rgen)
  - [🧭 Purpose](#-purpose)
  - [🚀 Install](#-install)
    - [Homebrew](#homebrew)
    - [Cargo](#cargo)
  - [⚙️ Quick start](#-quick-start)
  - [🧩 Templates](#-templates)
    - [Example: `templates/cli/subcommand/rust.tmpl`](#example-templatesclisubcommandrusttmpl)
  - [💡 Commands](#-commands)
  - [🔁 Determinism](#-determinism)
  - [🧠 Example: Multi-language CLI generation](#-example-multi-language-cli-generation)
  - [🧰 Integrations](#-integrations)
  - [📦 Extend](#-extend)
  - [🔒 License](#-license)

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

```bash
rgen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

Output depends on your template set (Rust, Python, Bash, etc).
Each output is produced deterministically from the same RDF description.

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
| `rgen list`                      | List available template scopes and actions |
| `rgen show <scope> <action>`     | Show template and resolved context         |
| `rgen validate <scope> <action>` | Validate RDF/SHACL graphs                  |
| `rgen gen <scope> <action>`      | Render template(s) into outputs            |
| `rgen graph export`              | Merge RDF sources into a single graph      |

---

## 🔁 Determinism

rgen computes a manifest hash over:

```
graph data + shape + frontmatter + template + seed
```

The same graph + seed = byte-identical results.

---

## 🧠 Example: Multi-language CLI generation

Run:

```bash
rgen gen cli subcommand --vars cmd=status summary="Show app status"
```

Creates:

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

Add your own generator:

```bash
mkdir -p templates/api/endpoint
cp templates/cli/subcommand/rust.tmpl templates/api/endpoint/rust.tmpl
```

Edit frontmatter and target path.
rgen will detect and render automatically.

---

## 🔒 License

MIT © rgen contributors

---

> **rgen** — one intent, many projections.
> Code is just a projection of knowledge.
