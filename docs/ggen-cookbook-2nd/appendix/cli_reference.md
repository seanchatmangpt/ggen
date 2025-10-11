<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGen CLI Reference](#ggen-cli-reference)
  - [Overview](#overview)
  - [Installation](#installation)
  - [Global Options](#global-options)
    - [`--help`, `-h`](#--help--h)
    - [`--version`, `-V`](#--version--v)
    - [`--verbose`, `-v`](#--verbose--v)
    - [`--quiet`, `-q`](#--quiet--q)
    - [`--color <WHEN>`](#--color-when)
  - [Commands](#commands)
    - [`ggen project`](#ggen-project)
      - [`ggen project gen`](#ggen-project-gen)
        - [`--output <PATH>`, `-o <PATH>`](#--output-path--o-path)
        - [`--context <FILE>`, `-c <FILE>`](#--context-file--c-file)
        - [`--var <KEY=VALUE>`, `-V <KEY=VALUE>`](#--var-keyvalue--v-keyvalue)
        - [`--dry-run`, `-n`](#--dry-run--n)
        - [`--force`, `-f`](#--force--f)
        - [`--freeze`, `-F`](#--freeze--f)
        - [`--seal`, `-S`](#--seal--s)
        - [`--parallel <N>`, `-j <N>`](#--parallel-n--j-n)
      - [`ggen project plan`](#ggen-project-plan)
        - [`--output <FILE>`, `-o <FILE>`](#--output-file--o-file)
        - [`--format <FORMAT>`, `-f <FORMAT>`](#--format-format--f-format)
        - [`--context <FILE>`, `-c <FILE>`](#--context-file--c-file-1)
        - [`--var <KEY=VALUE>`, `-V <KEY=VALUE>`](#--var-keyvalue--v-keyvalue-1)
      - [`ggen project apply`](#ggen-project-apply)
        - [`--dry-run`, `-n`](#--dry-run--n-1)
        - [`--force`, `-f`](#--force--f-1)
        - [`--parallel <N>`, `-j <N>`](#--parallel-n--j-n-1)
      - [`ggen project test`](#ggen-project-test)
        - [`--watch`, `-w`](#--watch--w)
        - [`--filter <PATTERN>`, `-F <PATTERN>`](#--filter-pattern--f-pattern)
        - [`--parallel <N>`, `-j <N>`](#--parallel-n--j-n-2)
        - [`--coverage`, `-c`](#--coverage--c)
    - [`ggen market`](#ggen-market)
      - [`ggen market search`](#ggen-market-search)
        - [`--tag <TAG>`, `-t <TAG>`](#--tag-tag--t-tag)
        - [`--author <NAME>`, `-a <NAME>`](#--author-name--a-name)
        - [`--min-stars <N>`](#--min-stars-n)
        - [`--limit <N>`, `-l <N>`](#--limit-n--l-n)
        - [`--sort <FIELD>`](#--sort-field)
        - [`--order <DIR>`](#--order-dir)
      - [`ggen market install`](#ggen-market-install)
        - [`--version <VERSION>`, `-v <VERSION>`](#--version-version--v-version)
        - [`--global`, `-g`](#--global--g)
        - [`--force`, `-f`](#--force--f-2)
        - [`--registry <URL>`, `-r <URL>`](#--registry-url--r-url)
      - [`ggen market publish`](#ggen-market-publish)
        - [`--registry <URL>`, `-r <URL>`](#--registry-url--r-url-1)
        - [`--access <LEVEL>`](#--access-level)
        - [`--tag <TAG>`, `-t <TAG>`](#--tag-tag--t-tag-1)
        - [`--dry-run`, `-n`](#--dry-run--n-2)
    - [`ggen template`](#ggen-template)
      - [`ggen template new`](#ggen-template-new)
        - [`--path <DIR>`, `-p <DIR>`](#--path-dir--p-dir)
        - [`--type <TYPE>`, `-t <TYPE>`](#--type-type--t-type)
        - [`--lang <LANG>`, `-l <LANG>`](#--lang-lang--l-lang)
        - [`--scaffold`, `-s`](#--scaffold--s)
      - [`ggen template validate`](#ggen-template-validate)
        - [`--strict`, `-s`](#--strict--s)
        - [`--schema <FILE>`](#--schema-file)
        - [`--fix`, `-f`](#--fix--f)
      - [`ggen template test`](#ggen-template-test)
        - [`--watch`, `-w`](#--watch--w-1)
        - [`--snapshot`, `-s`](#--snapshot--s)
        - [`--coverage`, `-c`](#--coverage--c-1)
    - [`ggen graph`](#ggen-graph)
      - [`ggen graph query`](#ggen-graph-query)
        - [`--format <FORMAT>`, `-f <FORMAT>`](#--format-format--f-format-1)
        - [`--limit <N>`, `-l <N>`](#--limit-n--l-n-1)
        - [`--output <FILE>`, `-o <FILE>`](#--output-file--o-file-1)
        - [`--graph <URI>`, `-g <URI>`](#--graph-uri--g-uri)
      - [`ggen graph load`](#ggen-graph-load)
        - [`--format <FORMAT>`, `-f <FORMAT>`](#--format-format--f-format-2)
        - [`--graph <URI>`, `-g <URI>`](#--graph-uri--g-uri-1)
        - [`--clear`, `-c`](#--clear--c)
        - [`--validate`, `-v`](#--validate--v)
      - [`ggen graph validate`](#ggen-graph-validate)
        - [`--graph <URI>`, `-g <URI>`](#--graph-uri--g-uri-2)
        - [`--format <FORMAT>`, `-f <FORMAT>`](#--format-format--f-format-3)
        - [`--output <FILE>`, `-o <FILE>`](#--output-file--o-file-2)
    - [`ggen init`](#ggen-init)
        - [`--template <TEMPLATE>`, `-t <TEMPLATE>`](#--template-template--t-template)
        - [`--interactive`, `-i`](#--interactive--i)
    - [`ggen config`](#ggen-config)
      - [`ggen config get <KEY>`](#ggen-config-get-key)
      - [`ggen config set <KEY> <VALUE>`](#ggen-config-set-key-value)
      - [`ggen config list`](#ggen-config-list)
      - [`ggen config reset`](#ggen-config-reset)
        - [`--global`, `-g`](#--global--g-1)
    - [`ggen audit`](#ggen-audit)
        - [`--severity <LEVEL>`](#--severity-level)
        - [`--format <FORMAT>`, `-f <FORMAT>`](#--format-format--f-format-4)
        - [`--output <FILE>`, `-o <FILE>`](#--output-file--o-file-3)
    - [`ggen ci`](#ggen-ci)
      - [`ggen ci check`](#ggen-ci-check)
      - [`ggen ci plan`](#ggen-ci-plan)
      - [`ggen ci apply`](#ggen-ci-apply)
    - [`ggen shell`](#ggen-shell)
        - [`--template <TEMPLATE>`, `-t <TEMPLATE>`](#--template-template--t-template-1)
        - [`--context <FILE>`, `-c <FILE>`](#--context-file--c-file-2)
  - [Environment Variables](#environment-variables)
    - [`GGEN_REGISTRY_URL`](#ggen_registry_url)
    - [`GGEN_LOG_LEVEL`](#ggen_log_level)
    - [`GGEN_CONFIG_DIR`](#ggen_config_dir)
    - [`GGEN_CACHE_DIR`](#ggen_cache_dir)
    - [`GGEN_NO_COLOR`](#ggen_no_color)
  - [Configuration Files](#configuration-files)
    - [Project Configuration: `ggen.toml`](#project-configuration-ggentoml)
    - [User Configuration: `~/.config/ggen/config.toml`](#user-configuration-configggenconfigtoml)
  - [Exit Codes](#exit-codes)
  - [Shell Completion](#shell-completion)
  - [Getting Help](#getting-help)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGen CLI Reference

## Overview

The `ggen` command-line interface provides tools for project scaffolding, template management, marketplace integration, and knowledge graph operations. This reference documents all commands, flags, and options.

---

## Installation

```bash
# Install from crates.io
cargo install ggen

# Build from source
git clone https://github.com/ggen-project/ggen
cd ggen
cargo make release
```

Verify installation:
```bash
ggen --version
```

---

## Global Options

These flags apply to all `ggen` commands:

### `--help`, `-h`
Display help information for any command.

```bash
ggen --help
ggen project gen --help
```

### `--version`, `-V`
Print the installed ggen version.

```bash
ggen --version
# Output: ggen 0.2.4
```

### `--verbose`, `-v`
Enable verbose logging output (can be repeated for more verbosity).

```bash
ggen -v project gen
ggen -vv project gen  # Extra verbose
```

### `--quiet`, `-q`
Suppress non-error output.

```bash
ggen -q project gen
```

### `--color <WHEN>`
Control colored output.

**Values:** `auto`, `always`, `never`

```bash
ggen --color=never project gen
```

---

## Commands

### `ggen project`

Project lifecycle management: planning, generation, application, and testing.

#### `ggen project gen`

Generate code from templates.

**Usage:**
```bash
ggen project gen [OPTIONS] <TEMPLATE>
```

**Arguments:**
- `<TEMPLATE>` - Template name or path

**Options:**

##### `--output <PATH>`, `-o <PATH>`
Output directory for generated files.

```bash
ggen project gen my-template -o ./src/generated
```

##### `--context <FILE>`, `-c <FILE>`
Context data file (JSON, YAML, or TOML).

```bash
ggen project gen my-template -c context.yaml
```

##### `--var <KEY=VALUE>`, `-V <KEY=VALUE>`
Set template variables inline (repeatable).

```bash
ggen project gen my-template -V name=MyApp -V version=1.0.0
```

##### `--dry-run`, `-n`
Preview generated output without writing files.

```bash
ggen project gen my-template --dry-run
```

##### `--force`, `-f`
Overwrite existing files without prompting.

```bash
ggen project gen my-template --force
```

##### `--freeze`, `-F`
Apply freeze blocks (default: true).

```bash
ggen project gen my-template --freeze=false
```

##### `--seal`, `-S`
Apply seal blocks (default: true).

```bash
ggen project gen my-template --seal=false
```

##### `--parallel <N>`, `-j <N>`
Number of parallel generation workers.

```bash
ggen project gen my-template -j 4
```

**Examples:**

```bash
# Basic generation
ggen project gen rust-api

# With context file
ggen project gen rust-api -c config.yaml -o ./src

# Inline variables
ggen project gen rust-api -V db=postgres -V port=8080

# Dry run to preview
ggen project gen rust-api --dry-run

# Force overwrite
ggen project gen rust-api -o ./src --force
```

---

#### `ggen project plan`

Create an execution plan for code generation.

**Usage:**
```bash
ggen project plan [OPTIONS] <TEMPLATE>
```

**Arguments:**
- `<TEMPLATE>` - Template name or path

**Options:**

##### `--output <FILE>`, `-o <FILE>`
Save plan to file (default: stdout).

```bash
ggen project plan my-template -o plan.json
```

##### `--format <FORMAT>`, `-f <FORMAT>`
Plan output format.

**Values:** `json`, `yaml`, `toml`, `text`

```bash
ggen project plan my-template -f yaml
```

##### `--context <FILE>`, `-c <FILE>`
Context data file.

```bash
ggen project plan my-template -c context.yaml
```

##### `--var <KEY=VALUE>`, `-V <KEY=VALUE>`
Set template variables inline.

```bash
ggen project plan my-template -V name=MyApp
```

**Examples:**

```bash
# Generate plan to stdout
ggen project plan rust-api -c config.yaml

# Save plan as JSON
ggen project plan rust-api -o plan.json -f json

# Preview plan with inline vars
ggen project plan rust-api -V db=postgres -V port=8080
```

---

#### `ggen project apply`

Apply a previously generated plan.

**Usage:**
```bash
ggen project apply [OPTIONS] <PLAN>
```

**Arguments:**
- `<PLAN>` - Plan file to apply

**Options:**

##### `--dry-run`, `-n`
Preview application without making changes.

```bash
ggen project apply plan.json --dry-run
```

##### `--force`, `-f`
Overwrite existing files without prompting.

```bash
ggen project apply plan.json --force
```

##### `--parallel <N>`, `-j <N>`
Number of parallel workers.

```bash
ggen project apply plan.json -j 4
```

**Examples:**

```bash
# Apply plan
ggen project apply plan.json

# Dry run to preview
ggen project apply plan.json --dry-run

# Force application
ggen project apply plan.json --force
```

---

#### `ggen project test`

Test generated code against expectations.

**Usage:**
```bash
ggen project test [OPTIONS] [TESTS]...
```

**Arguments:**
- `[TESTS]...` - Specific test files or directories (default: all tests)

**Options:**

##### `--watch`, `-w`
Watch for changes and re-run tests.

```bash
ggen project test --watch
```

##### `--filter <PATTERN>`, `-F <PATTERN>`
Run tests matching pattern.

```bash
ggen project test -F "api_*"
```

##### `--parallel <N>`, `-j <N>`
Number of parallel test workers.

```bash
ggen project test -j 4
```

##### `--coverage`, `-c`
Generate code coverage report.

```bash
ggen project test --coverage
```

**Examples:**

```bash
# Run all tests
ggen project test

# Run specific test file
ggen project test tests/api_test.ggen

# Filter by pattern
ggen project test -F "integration_*"

# Watch mode
ggen project test --watch
```

---

### `ggen market`

Marketplace operations: search, install, and publish templates.

#### `ggen market search`

Search the marketplace for templates.

**Usage:**
```bash
ggen market search [OPTIONS] <QUERY>
```

**Arguments:**
- `<QUERY>` - Search query string

**Options:**

##### `--tag <TAG>`, `-t <TAG>`
Filter by tag (repeatable).

```bash
ggen market search api -t rust -t postgres
```

##### `--author <NAME>`, `-a <NAME>`
Filter by author.

```bash
ggen market search api -a "@ggen-team"
```

##### `--min-stars <N>`
Minimum star count.

```bash
ggen market search api --min-stars 100
```

##### `--limit <N>`, `-l <N>`
Maximum results to return (default: 20).

```bash
ggen market search api -l 50
```

##### `--sort <FIELD>`
Sort results by field.

**Values:** `relevance`, `stars`, `downloads`, `updated`, `created`

```bash
ggen market search api --sort stars
```

##### `--order <DIR>`
Sort order.

**Values:** `asc`, `desc` (default: desc)

```bash
ggen market search api --sort downloads --order desc
```

**Examples:**

```bash
# Basic search
ggen market search "rust api"

# Filter by tags
ggen market search api -t rust -t graphql

# Sort by popularity
ggen market search web --sort stars -l 10

# Author-specific search
ggen market search -a "@ggen-team"
```

---

#### `ggen market install`

Install a template from the marketplace.

**Usage:**
```bash
ggen market install [OPTIONS] <PACKAGE>
```

**Arguments:**
- `<PACKAGE>` - Package name (format: `@namespace/template@version`)

**Options:**

##### `--version <VERSION>`, `-v <VERSION>`
Specific version to install (default: latest).

```bash
ggen market install @ggen/rust-api -v 1.2.0
```

##### `--global`, `-g`
Install globally (available to all projects).

```bash
ggen market install @ggen/rust-api --global
```

##### `--force`, `-f`
Force reinstall if already installed.

```bash
ggen market install @ggen/rust-api --force
```

##### `--registry <URL>`, `-r <URL>`
Use custom registry.

```bash
ggen market install @myorg/template -r https://my-registry.com
```

**Examples:**

```bash
# Install latest version
ggen market install @ggen/rust-api

# Install specific version
ggen market install @ggen/rust-api -v 1.2.0

# Global installation
ggen market install @ggen/base-template -g

# From custom registry
ggen market install @company/internal-api -r https://registry.company.com
```

---

#### `ggen market publish`

Publish a template to the marketplace.

**Usage:**
```bash
ggen market publish [OPTIONS] <TEMPLATE>
```

**Arguments:**
- `<TEMPLATE>` - Template directory to publish

**Options:**

##### `--registry <URL>`, `-r <URL>`
Registry to publish to (default: official registry).

```bash
ggen market publish ./my-template -r https://my-registry.com
```

##### `--access <LEVEL>`
Access level for package.

**Values:** `public`, `private`, `restricted`

```bash
ggen market publish ./my-template --access public
```

##### `--tag <TAG>`, `-t <TAG>`
Add tags to package (repeatable).

```bash
ggen market publish ./my-template -t rust -t api -t graphql
```

##### `--dry-run`, `-n`
Validate package without publishing.

```bash
ggen market publish ./my-template --dry-run
```

**Examples:**

```bash
# Publish to official registry
ggen market publish ./my-template

# Publish to custom registry
ggen market publish ./my-template -r https://company.registry.com

# Add tags
ggen market publish ./my-template -t rust -t microservice

# Dry run to validate
ggen market publish ./my-template --dry-run
```

---

### `ggen template`

Template authoring and validation.

#### `ggen template new`

Create a new template.

**Usage:**
```bash
ggen template new [OPTIONS] <NAME>
```

**Arguments:**
- `<NAME>` - Template name

**Options:**

##### `--path <DIR>`, `-p <DIR>`
Directory to create template in (default: current directory).

```bash
ggen template new my-template -p ./templates
```

##### `--type <TYPE>`, `-t <TYPE>`
Template type.

**Values:** `basic`, `project`, `component`, `utility`

```bash
ggen template new my-template -t project
```

##### `--lang <LANG>`, `-l <LANG>`
Target language for template.

**Values:** `rust`, `python`, `javascript`, `typescript`, `go`, `java`, etc.

```bash
ggen template new my-api -l rust
```

##### `--scaffold`, `-s`
Generate example files and tests.

```bash
ggen template new my-template --scaffold
```

**Examples:**

```bash
# Basic template
ggen template new my-template

# Project template with scaffolding
ggen template new rust-api -t project -l rust --scaffold

# Custom location
ggen template new util -p ./my-templates -t utility
```

---

#### `ggen template validate`

Validate template syntax and structure.

**Usage:**
```bash
ggen template validate [OPTIONS] <TEMPLATE>
```

**Arguments:**
- `<TEMPLATE>` - Template file or directory

**Options:**

##### `--strict`, `-s`
Enable strict validation mode.

```bash
ggen template validate my-template --strict
```

##### `--schema <FILE>`
Custom validation schema.

```bash
ggen template validate my-template --schema custom-schema.json
```

##### `--fix`, `-f`
Attempt to auto-fix validation errors.

```bash
ggen template validate my-template --fix
```

**Examples:**

```bash
# Validate template
ggen template validate ./templates/rust-api

# Strict mode
ggen template validate ./templates/rust-api --strict

# Auto-fix issues
ggen template validate ./templates/rust-api --fix
```

---

#### `ggen template test`

Run tests for a template.

**Usage:**
```bash
ggen template test [OPTIONS] <TEMPLATE>
```

**Arguments:**
- `<TEMPLATE>` - Template to test

**Options:**

##### `--watch`, `-w`
Watch for changes and re-run tests.

```bash
ggen template test my-template --watch
```

##### `--snapshot`, `-s`
Update test snapshots.

```bash
ggen template test my-template --snapshot
```

##### `--coverage`, `-c`
Generate coverage report.

```bash
ggen template test my-template --coverage
```

**Examples:**

```bash
# Run template tests
ggen template test ./templates/rust-api

# Update snapshots
ggen template test ./templates/rust-api --snapshot

# Watch mode with coverage
ggen template test ./templates/rust-api --watch --coverage
```

---

### `ggen graph`

Knowledge graph operations.

#### `ggen graph query`

Query the knowledge graph using SPARQL.

**Usage:**
```bash
ggen graph query [OPTIONS] <QUERY>
```

**Arguments:**
- `<QUERY>` - SPARQL query string or file path

**Options:**

##### `--format <FORMAT>`, `-f <FORMAT>`
Output format.

**Values:** `json`, `turtle`, `ntriples`, `xml`, `csv`, `table`

```bash
ggen graph query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" -f json
```

##### `--limit <N>`, `-l <N>`
Maximum results (default: 100).

```bash
ggen graph query query.sparql -l 1000
```

##### `--output <FILE>`, `-o <FILE>`
Save results to file.

```bash
ggen graph query query.sparql -o results.json -f json
```

##### `--graph <URI>`, `-g <URI>`
Named graph to query (default: default graph).

```bash
ggen graph query query.sparql -g http://example.org/graph1
```

**Examples:**

```bash
# Simple query
ggen graph query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Query from file
ggen graph query queries/find-entities.sparql

# Export as JSON
ggen graph query query.sparql -f json -o results.json

# Query named graph
ggen graph query query.sparql -g http://my-project.org/entities
```

---

#### `ggen graph load`

Load RDF data into the knowledge graph.

**Usage:**
```bash
ggen graph load [OPTIONS] <FILE>...
```

**Arguments:**
- `<FILE>...` - RDF files to load (Turtle, N-Triples, RDF/XML, JSON-LD)

**Options:**

##### `--format <FORMAT>`, `-f <FORMAT>`
RDF format (auto-detected if not specified).

**Values:** `turtle`, `ntriples`, `rdfxml`, `jsonld`

```bash
ggen graph load data.ttl -f turtle
```

##### `--graph <URI>`, `-g <URI>`
Named graph to load into.

```bash
ggen graph load entities.ttl -g http://my-project.org/entities
```

##### `--clear`, `-c`
Clear existing graph before loading.

```bash
ggen graph load data.ttl --clear
```

##### `--validate`, `-v`
Validate RDF before loading.

```bash
ggen graph load data.ttl --validate
```

**Examples:**

```bash
# Load Turtle file
ggen graph load entities.ttl

# Load multiple files
ggen graph load entities.ttl relationships.ttl

# Load into named graph
ggen graph load data.ttl -g http://example.org/mydata

# Clear and reload
ggen graph load data.ttl --clear

# Validate before loading
ggen graph load data.ttl --validate
```

---

#### `ggen graph validate`

Validate RDF graph against SHACL shapes or custom rules.

**Usage:**
```bash
ggen graph validate [OPTIONS] [SHAPES]
```

**Arguments:**
- `[SHAPES]` - SHACL shapes file (optional)

**Options:**

##### `--graph <URI>`, `-g <URI>`
Named graph to validate.

```bash
ggen graph validate shapes.ttl -g http://my-project.org/entities
```

##### `--format <FORMAT>`, `-f <FORMAT>`
Validation report format.

**Values:** `text`, `turtle`, `json`

```bash
ggen graph validate shapes.ttl -f json
```

##### `--output <FILE>`, `-o <FILE>`
Save validation report to file.

```bash
ggen graph validate shapes.ttl -o report.json -f json
```

**Examples:**

```bash
# Validate against SHACL shapes
ggen graph validate shapes.ttl

# Validate named graph
ggen graph validate shapes.ttl -g http://example.org/data

# Export report as JSON
ggen graph validate shapes.ttl -f json -o report.json
```

---

### `ggen init`

Initialize a new ggen project.

**Usage:**
```bash
ggen init [OPTIONS] [PATH]
```

**Arguments:**
- `[PATH]` - Project directory (default: current directory)

**Options:**

##### `--template <TEMPLATE>`, `-t <TEMPLATE>`
Initialize from template.

```bash
ggen init my-project -t @ggen/rust-api
```

##### `--interactive`, `-i`
Interactive setup wizard.

```bash
ggen init --interactive
```

**Examples:**

```bash
# Initialize in current directory
ggen init

# Create new project from template
ggen init my-project -t @ggen/rust-microservice

# Interactive setup
ggen init my-project --interactive
```

---

### `ggen config`

Manage ggen configuration.

**Usage:**
```bash
ggen config [OPTIONS] <SUBCOMMAND>
```

**Subcommands:**

#### `ggen config get <KEY>`
Get configuration value.

```bash
ggen config get registry.url
```

#### `ggen config set <KEY> <VALUE>`
Set configuration value.

```bash
ggen config set registry.url https://my-registry.com
```

#### `ggen config list`
List all configuration.

```bash
ggen config list
```

#### `ggen config reset`
Reset to default configuration.

```bash
ggen config reset
```

**Options:**

##### `--global`, `-g`
Operate on global configuration (default: project-local).

```bash
ggen config set registry.url https://my-registry.com --global
```

---

### `ggen audit`

Audit templates and generated code for security and compliance.

**Usage:**
```bash
ggen audit [OPTIONS] [PATH]
```

**Arguments:**
- `[PATH]` - Path to audit (default: current project)

**Options:**

##### `--severity <LEVEL>`
Minimum severity to report.

**Values:** `info`, `warning`, `error`, `critical`

```bash
ggen audit --severity error
```

##### `--format <FORMAT>`, `-f <FORMAT>`
Output format.

**Values:** `text`, `json`, `sarif`

```bash
ggen audit -f json -o audit-report.json
```

##### `--output <FILE>`, `-o <FILE>`
Save audit report to file.

```bash
ggen audit -o audit.txt
```

**Examples:**

```bash
# Audit current project
ggen audit

# Audit specific template
ggen audit ./templates/my-template

# Export as SARIF for CI/CD
ggen audit -f sarif -o results.sarif
```

---

### `ggen ci`

CI/CD integration helpers.

**Usage:**
```bash
ggen ci [OPTIONS] <SUBCOMMAND>
```

**Subcommands:**

#### `ggen ci check`
Check if generated code is up-to-date.

```bash
ggen ci check
```

Exit codes:
- `0` - Generated code is up-to-date
- `1` - Generated code is stale
- `2` - Error occurred

#### `ggen ci plan`
Generate execution plan for CI pipeline.

```bash
ggen ci plan -o ci-plan.json
```

#### `ggen ci apply`
Apply plan in CI environment.

```bash
ggen ci apply ci-plan.json
```

**Examples:**

```bash
# In CI pipeline
ggen ci check || (echo "Generated code is stale!" && exit 1)

# Generate and apply
ggen ci plan -o plan.json
ggen ci apply plan.json
```

---

### `ggen shell`

Interactive REPL for template development and testing.

**Usage:**
```bash
ggen shell [OPTIONS]
```

**Options:**

##### `--template <TEMPLATE>`, `-t <TEMPLATE>`
Load template into REPL context.

```bash
ggen shell -t ./templates/my-template
```

##### `--context <FILE>`, `-c <FILE>`
Load context data.

```bash
ggen shell -c context.yaml
```

**Examples:**

```bash
# Start interactive shell
ggen shell

# Load template and context
ggen shell -t my-template -c context.yaml
```

**REPL Commands:**
```
> .load <template>        # Load template
> .context <file>         # Load context
> .vars                   # Show variables
> .gen                    # Generate output
> .help                   # Show help
> .quit                   # Exit
```

---

## Environment Variables

### `GGEN_REGISTRY_URL`
Default marketplace registry URL.

```bash
export GGEN_REGISTRY_URL=https://my-registry.com
```

### `GGEN_LOG_LEVEL`
Logging level.

**Values:** `trace`, `debug`, `info`, `warn`, `error`

```bash
export GGEN_LOG_LEVEL=debug
```

### `GGEN_CONFIG_DIR`
Configuration directory (default: `~/.config/ggen`).

```bash
export GGEN_CONFIG_DIR=/custom/config/path
```

### `GGEN_CACHE_DIR`
Cache directory (default: platform-specific cache dir).

```bash
export GGEN_CACHE_DIR=/custom/cache/path
```

### `GGEN_NO_COLOR`
Disable colored output (any value).

```bash
export GGEN_NO_COLOR=1
```

---

## Configuration Files

### Project Configuration: `ggen.toml`

```toml
[project]
name = "my-project"
version = "1.0.0"

[generation]
parallel = 4
freeze = true
seal = true

[registry]
url = "https://registry.ggen.io"
auth_token_env = "GGEN_TOKEN"

[graph]
default_namespace = "http://my-project.org/"
```

### User Configuration: `~/.config/ggen/config.toml`

```toml
[user]
name = "Your Name"
email = "you@example.com"

[registry]
url = "https://registry.ggen.io"

[defaults]
parallel = 8
color = true
```

---

## Exit Codes

- `0` - Success
- `1` - General error
- `2` - Invalid usage
- `3` - Validation error
- `4` - Generation error
- `5` - Network error

---

## Shell Completion

Generate shell completion scripts:

```bash
# Bash
ggen completions bash > /etc/bash_completion.d/ggen

# Zsh
ggen completions zsh > /usr/local/share/zsh/site-functions/_ggen

# Fish
ggen completions fish > ~/.config/fish/completions/ggen.fish

# PowerShell
ggen completions powershell > ggen.ps1
```

---

## Getting Help

```bash
# General help
ggen --help

# Command-specific help
ggen project gen --help

# Show version
ggen --version

# Report issues
https://github.com/ggen-project/ggen/issues
```

---

**Related:**
- [Full Pattern Language](full_pattern_language.md) - Index of all 253 patterns
- [Glossary](glossary.md) - Terminology reference
