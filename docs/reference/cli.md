<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [CLI Reference](#cli-reference)
  - [Global Options](#global-options)
  - [Commands Overview](#commands-overview)
  - [Marketplace Commands](#marketplace-commands)
    - [ggen marketplace search](#ggen-marketplace-search)
    - [ggen marketplace install](#ggen-marketplace-install)
    - [ggen marketplace list](#ggen-marketplace-list)
    - [ggen marketplace publish](#ggen-marketplace-publish)
  - [AI Commands](#ai-commands)
    - [ggen ai generate-ontology](#ggen-ai-generate-ontology)
    - [ggen ai chat](#ggen-ai-chat)
    - [ggen ai analyze](#ggen-ai-analyze)
  - [Template Commands](#template-commands)
    - [ggen template generate-rdf](#ggen-template-generate-rdf)
    - [ggen template list](#ggen-template-list)
    - [ggen template lint](#ggen-template-lint)
  - [Graph Commands](#graph-commands)
    - [ggen graph load](#ggen-graph-load)
    - [ggen graph query](#ggen-graph-query)
    - [ggen graph validate](#ggen-graph-validate)
    - [ggen graph export](#ggen-graph-export)
  - [Hook Commands](#hook-commands)
    - [ggen hook create](#ggen-hook-create)
    - [ggen hook list](#ggen-hook-list)
    - [ggen hook remove](#ggen-hook-remove)
    - [ggen hook monitor](#ggen-hook-monitor)
  - [Project Commands](#project-commands)
    - [ggen project new](#ggen-project-new)
    - [ggen project gen](#ggen-project-gen)
  - [Utils Commands](#utils-commands)
    - [ggen utils doctor](#ggen-utils-doctor)
    - [ggen utils env](#ggen-utils-env)
  - [Command Examples](#command-examples)
    - [Complete Workflow](#complete-workflow)
    - [Marketplace Workflow](#marketplace-workflow)
  - [Exit Codes](#exit-codes)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# CLI Reference

Complete reference for all `ggen` command-line interface commands.

## Global Options

All commands support these global flags:

```bash
--help, -h          Show help information
--version, -V       Show version information
--json              Output in JSON format
--verbose, -v       Enable verbose output
--quiet, -q         Suppress output
```

## Commands Overview

ggen provides seven main command categories:

- **marketplace** - Search, install, and publish templates
- **project** - Create and manage projects
- **ai** - AI-powered code generation and analysis
- **template** - Template management and generation
- **hook** - Git hooks and automation
- **graph** - RDF graph operations
- **utils** - System utilities and diagnostics

## Marketplace Commands

### ggen marketplace search

Search for packages in the marketplace.

**Usage:**
```bash
ggen marketplace search <QUERY> [OPTIONS]
```

**Arguments:**
- `<QUERY>` - Search query string

**Options:**
- `--limit <N>` - Maximum number of results (default: 10)
- `--category <CAT>` - Filter by category

**Examples:**
```bash
ggen marketplace search "rust microservice"
ggen marketplace search "api" --category backend --limit 20
```

### ggen marketplace install

Install a package from the marketplace.

**Usage:**
```bash
ggen marketplace install <PACKAGE_ID> [OPTIONS]
```

**Arguments:**
- `<PACKAGE_ID>` - Package identifier (e.g., `io.ggen.rust.microservice`)

**Options:**
- `--version <VERSION>` - Install specific version
- `--force` - Force reinstall

**Examples:**
```bash
ggen marketplace install io.ggen.rust.microservice
ggen marketplace install io.ggen.rust.microservice --version 1.2.0
```

### ggen marketplace list

List installed packages.

**Usage:**
```bash
ggen marketplace list [OPTIONS]
```

**Options:**
- `--json` - Output as JSON

**Examples:**
```bash
ggen marketplace list
ggen marketplace list --json
```

### ggen marketplace publish

Publish a package to the marketplace.

**Usage:**
```bash
ggen marketplace publish [OPTIONS]
```

**Options:**
- `--name <NAME>` - Package name
- `--version <VERSION>` - Package version
- `--template-dir <DIR>` - Template directory

**Examples:**
```bash
ggen marketplace publish --name my-template --version 1.0.0 --template-dir ./templates
```

## AI Commands

### ggen ai generate-ontology

Generate an RDF ontology using AI.

**Usage:**
```bash
ggen ai generate-ontology --prompt <PROMPT> --output <FILE> [OPTIONS]
```

**Arguments:**
- `--prompt <PROMPT>` - Natural language description of domain
- `--output <FILE>` - Output RDF file path

**Options:**
- `--provider <PROVIDER>` - AI provider (anthropic, openai, ollama)
- `--model <MODEL>` - Model name

**Examples:**
```bash
ggen ai generate-ontology --prompt "E-commerce: Product, Order, Customer" --output domain.ttl
```

### ggen ai chat

Interactive AI chat session.

**Usage:**
```bash
ggen ai chat [OPTIONS]
```

**Options:**
- `--interactive` - Interactive mode
- `--prompt <PROMPT>` - Single prompt
- `--input <FILE>` - Input file

**Examples:**
```bash
ggen ai chat --interactive
ggen ai chat --prompt "Improve this ontology" --input domain.ttl
```

### ggen ai analyze

Analyze codebase with AI.

**Usage:**
```bash
ggen ai analyze <PATH> [OPTIONS]
```

**Arguments:**
- `<PATH>` - Path to analyze

**Options:**
- `--focus <ASPECT>` - Focus area (domain-model, architecture, etc.)
- `--suggest-improvements` - Suggest improvements

**Examples:**
```bash
ggen ai analyze src/ --focus domain-model
```

## Template Commands

### ggen template generate-rdf

Generate code from RDF ontology using template.

**Usage:**
```bash
ggen template generate-rdf --ontology <FILE> --template <TEMPLATE> --output <DIR> [OPTIONS]
```

**Arguments:**
- `--ontology <FILE>` - RDF ontology file
- `--template <TEMPLATE>` - Template name or path
- `--output <DIR>` - Output directory

**Options:**
- `--vars <KEY=VALUE>` - Template variables
- `--dry-run` - Show what would be generated

**Examples:**
```bash
ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/
```

### ggen template list

List available templates.

**Usage:**
```bash
ggen template list [OPTIONS]
```

**Examples:**
```bash
ggen template list
```

### ggen template lint

Validate template syntax.

**Usage:**
```bash
ggen template lint <TEMPLATE> [OPTIONS]
```

**Arguments:**
- `<TEMPLATE>` - Template file or name

**Examples:**
```bash
ggen template lint templates/rust-models/models.rs.tmpl
```

## Graph Commands

### ggen graph load

Load RDF graph from file.

**Usage:**
```bash
ggen graph load <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - RDF file path

**Examples:**
```bash
ggen graph load domain.ttl
```

### ggen graph query

Execute SPARQL query against graph.

**Usage:**
```bash
ggen graph query <FILE> --sparql <QUERY> [OPTIONS]
```

**Arguments:**
- `<FILE>` - RDF file path
- `--sparql <QUERY>` - SPARQL query string

**Options:**
- `--file <FILE>` - SPARQL query file

**Examples:**
```bash
ggen graph query domain.ttl --sparql "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
```

### ggen graph validate

Validate RDF graph.

**Usage:**
```bash
ggen graph validate <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - RDF file path

**Options:**
- `--shacl <FILE>` - SHACL shapes file
- `--verbose` - Verbose output

**Examples:**
```bash
ggen graph validate domain.ttl
ggen graph validate domain.ttl --shacl shapes.ttl
```

### ggen graph export

Export graph to different format.

**Usage:**
```bash
ggen graph export <FILE> --format <FORMAT> [OPTIONS]
```

**Arguments:**
- `<FILE>` - RDF file path
- `--format <FORMAT>` - Output format (turtle, json-ld, n-triples)

**Examples:**
```bash
ggen graph export domain.ttl --format json-ld
```

## Hook Commands

### ggen hook create

Create a Git hook.

**Usage:**
```bash
ggen hook create <PHASE> --name <NAME> --command <CMD> [OPTIONS]
```

**Arguments:**
- `<PHASE>` - Lifecycle phase (pre-commit, post-merge, etc.)
- `--name <NAME>` - Hook name
- `--command <CMD>` - Command to execute

**Examples:**
```bash
ggen hook create pre-commit --name validate --command "ggen graph validate domain.ttl"
```

### ggen hook list

List registered hooks.

**Usage:**
```bash
ggen hook list [OPTIONS]
```

**Examples:**
```bash
ggen hook list
```

### ggen hook remove

Remove a hook.

**Usage:**
```bash
ggen hook remove <PHASE> <NAME>
```

**Arguments:**
- `<PHASE>` - Lifecycle phase
- `<NAME>` - Hook name

**Examples:**
```bash
ggen hook remove pre-commit validate
```

### ggen hook monitor

Monitor hook execution.

**Usage:**
```bash
ggen hook monitor [OPTIONS]
```

**Examples:**
```bash
ggen hook monitor
```

## Project Commands

### ggen project new

Create a new project.

**Usage:**
```bash
ggen project new <NAME> [OPTIONS]
```

**Arguments:**
- `<NAME>` - Project name

**Options:**
- `--type <TYPE>` - Project type
- `--framework <FRAMEWORK>` - Framework name

**Examples:**
```bash
ggen project new my-app --type rust-web --framework axum
```

### ggen project gen

Generate code from project plan.

**Usage:**
```bash
ggen project gen <NAME> [OPTIONS]
```

**Arguments:**
- `<NAME>` - Project name

**Options:**
- `--template <TEMPLATE>` - Template to use
- `--vars <KEY=VALUE>` - Variables

**Examples:**
```bash
ggen project gen my-service --template io.ggen.rust.microservice
```

## Utils Commands

### ggen utils doctor

System diagnostics and health check.

**Usage:**
```bash
ggen utils doctor [OPTIONS]
```

**Examples:**
```bash
ggen utils doctor
```

### ggen utils env

Environment variable management.

**Usage:**
```bash
ggen utils env [OPTIONS]
```

**Examples:**
```bash
ggen utils env
```

## Command Examples

### Complete Workflow

```bash
# 1. Generate ontology
ggen ai generate-ontology --prompt "Blog system" --output blog.ttl

# 2. Validate ontology
ggen graph validate blog.ttl

# 3. Generate Rust code
ggen template generate-rdf --ontology blog.ttl --template rust-models --output src/

# 4. Set up hook for auto-regeneration
ggen hook create post-merge --name regenerate --command "ggen template generate-rdf --ontology blog.ttl --template rust-models --output src/"
```

### Marketplace Workflow

```bash
# 1. Search for templates
ggen marketplace search "rust api"

# 2. Install template
ggen marketplace install io.ggen.rust.axum-api

# 3. Use template
ggen template generate-rdf --ontology domain.ttl --template io.ggen.rust.axum-api --output src/
```

## Exit Codes

- `0` - Success
- `1` - General error
- `2` - Invalid arguments
- `3` - File not found
- `4` - Validation error

## See Also

- [Getting Started Tutorial](../tutorials/getting-started.md)
- [Installation Guide](../how-to-guides/installation.md)
- [Troubleshooting Guide](../how-to-guides/troubleshoot.md)

