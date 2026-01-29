<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Command Reference](#command-reference)
  - [Core Commands](#core-commands)
    - [ggen sync](#ggen-sync)
    - [ggen init](#ggen-init)
    - [ggen validate](#ggen-validate)
    - [ggen inspect](#ggen-inspect)
    - [ggen add](#ggen-add)
    - [ggen publish](#ggen-publish)
  - [Project Management](#project-management)
    - [ggen info](#ggen-info)
  - [Environment Variables](#environment-variables)
    - [Logging](#logging)
    - [Performance](#performance)
    - [Behavior](#behavior)
  - [Exit Codes](#exit-codes)
  - [Configuration (ggen.toml)](#configuration-ggentoml)
    - [Project Section](#project-section)
    - [Generation Section](#generation-section)
    - [Output Section](#output-section)
    - [Features](#features)
  - [Shell Completion](#shell-completion)
    - [Bash](#bash)
    - [Zsh](#zsh)
    - [Fish](#fish)
  - [Helpful Flags (All Commands)](#helpful-flags-all-commands)
  - [Error Messages](#error-messages)
    - ["Specification not found"](#specification-not-found)
    - ["SPARQL query failed"](#sparql-query-failed)
    - ["Template variable undefined"](#template-variable-undefined)
    - ["Generation timeout"](#generation-timeout)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Command Reference

**Complete guide to all ggen commands.**

## Core Commands

### ggen sync

Generate code from ontology specifications.

```bash
ggen sync [OPTIONS]
```

**Options:**

| Flag | Type | Default | Description |
|------|------|---------|-------------|
| `--dry_run` | bool | false | Preview changes without writing files |
| `--force` | bool | false | Overwrite existing generated files |
| `--audit` | bool | false | Generate cryptographic audit trail |
| `--validate_only` | bool | false | Validate specifications without generation |
| `--watch` | bool | false | Watch for file changes and regenerate |
| `--cache` | bool | true | Use caching for faster regeneration |
| `--ai` | bool | false | Use AI for template rendering |

**Examples:**

```bash
# Standard generation
ggen sync

# Preview changes first
ggen sync --dry_run true

# Generate with audit trail (for CI/CD)
ggen sync --audit true

# Watch for changes during development
ggen sync --watch true

# Validate without generating
ggen sync --validate_only true

# Force overwrite with audit trail
ggen sync --force true --audit true
```

**Output:**

```
✓ Loading specifications from .specify/specs/
✓ Validating SHACL schemas
✓ Executing SPARQL queries
✓ Rendering 12 Tera templates
✓ Generated 5 files (2.3 KB)
✓ Audit trail: .ggen/receipts/1a2b3c4d.json

Receipt:
- Execution ID: 1a2b3c4d-5e6f-7g8h-9i0j
- Manifest: SHA-256 abc123...
- Ontology: SHA-256 def456...
- Files: 5 (2.3 KB total)
- Time: 1.245s
```

---

### ggen init

Initialize a new ggen project.

```bash
ggen init [OPTIONS]
```

**Options:**

| Flag | Type | Default | Description |
|------|------|---------|-------------|
| `--template` | string | "default" | Project template (default, rest-api, microservice) |
| `--name` | string | Current dir | Project name |

**Examples:**

```bash
# Initialize with default template
ggen init

# Initialize REST API project
ggen init --template rest-api

# Initialize with specific name
ggen init --name my-project
```

**Creates:**

```
.ggen/                  # Cache, receipts, audit logs
.specify/              # Specifications directory
  specs/               # Your ontologies
  templates/           # Project templates
ggen.toml             # Project configuration
README.md             # Generated documentation
```

---

### ggen validate

Validate ontologies against SHACL schemas.

```bash
ggen validate <PATH> [OPTIONS]
```

**Arguments:**

- `<PATH>` - Path to `.ttl` file or directory

**Options:**

| Flag | Type | Default | Description |
|------|------|---------|-------------|
| `--strict` | bool | false | Treat warnings as errors |
| `--quiet` | bool | false | Minimal output |

**Examples:**

```bash
# Validate specific file
ggen validate .specify/specs/001-users/users.ttl

# Validate entire specs directory
ggen validate .specify/specs/

# Strict validation (warnings = errors)
ggen validate .specify/specs/ --strict true
```

---

### ggen inspect

Inspect RDF triples in ontology.

```bash
ggen inspect <PATH> [OPTIONS]
```

**Options:**

| Flag | Type | Default | Description |
|------|------|---------|-------------|
| `--filter` | string | "" | Filter triples by subject/predicate/object |
| `--format` | string | "table" | Output format (table, json, turtle) |

**Examples:**

```bash
# List all triples
ggen inspect .specify/specs/001-users/users.ttl

# Filter by subject
ggen inspect .specify/specs/001-users/users.ttl --filter ":User"

# Output as JSON
ggen inspect .specify/specs/001-users/users.ttl --format json

# Output as Turtle
ggen inspect .specify/specs/001-users/users.ttl --format turtle
```

---

### ggen add

Add dependency to project.

```bash
ggen add <PACKAGE> [OPTIONS]
```

**Arguments:**

- `<PACKAGE>` - Package name from registry

**Options:**

| Flag | Type | Default | Description |
|------|------|---------|-------------|
| `--version` | string | "latest" | Specific version |
| `--registry` | url | crates.io | Registry URL |

**Examples:**

```bash
# Add latest version
ggen add user-api

# Add specific version
ggen add user-api --version 1.0.0

# Add from custom registry
ggen add user-api --registry https://registry.custom.com
```

---

### ggen publish

Publish ontology package to registry.

```bash
ggen publish [OPTIONS]
```

**Options:**

| Flag | Type | Default | Description |
|------|------|---------|-------------|
| `--registry` | url | crates.io | Registry URL |
| `--dry-run` | bool | false | Preview publish without uploading |

**Examples:**

```bash
# Publish to default registry
ggen publish

# Dry-run (preview)
ggen publish --dry-run true

# Publish to custom registry
ggen publish --registry https://registry.custom.com
```

---

## Project Management

### ggen info

Display project information.

```bash
ggen info
```

**Output:**

```
Project: My REST API
Version: 0.1.0
Location: /home/user/my-project
Specifications: .specify/specs/
Templates: templates/
Output: src/

Ontologies: 3
  - 001-users/users.ttl
  - 002-products/products.ttl
  - 003-orders/orders.ttl

Templates: 5
  - rust/models.rs.tera
  - rust/handlers.rs.tera
  - typescript/types.ts.tera
  - sql/migrations.sql.tera
  - docs/README.md.tera

Last Generation: 2 hours ago
Receipt: .ggen/receipts/latest.json
```

---

## Environment Variables

### Logging

```bash
# Control log level
export GGEN_LOG_LEVEL=debug
# Options: trace, debug, info, warn, error

# Output to file
export GGEN_LOG_FILE=.ggen/ggen.log

# JSON structured logging
export GGEN_LOG_FORMAT=json
```

### Performance

```bash
# Cache directory
export GGEN_CACHE_DIR=.ggen/cache

# Maximum cache size (bytes)
export GGEN_CACHE_MAX_SIZE=1073741824  # 1 GB

# Disable cache
export GGEN_CACHE_DISABLED=true
```

### Behavior

```bash
# Project home directory
export GGEN_HOME=.ggen

# Config file location
export GGEN_CONFIG=ggen.toml

# Timeout for generation (seconds)
export GGEN_TIMEOUT=60
```

---

## Exit Codes

| Code | Meaning |
|------|---------|
| `0` | Success |
| `1` | General error |
| `2` | Validation failed |
| `3` | Generation failed |
| `4` | SPARQL query failed |
| `5` | Template rendering failed |
| `64` | Configuration error |
| `65` | I/O error |
| `124` | Timeout |

---

## Configuration (ggen.toml)

### Project Section

```toml
[project]
name = "My API"
version = "0.1.0"
description = "REST API for users"
authors = ["Alice", "Bob"]
```

### Generation Section

```toml
[generation]
# Input specifications
specs_dir = ".specify/specs"

# Output directory
output_dir = "src"

# Templates to use
templates = [
    "templates/rust/models.rs.tera",
    "templates/rust/handlers.rs.tera",
]

# Validation gates
validate_before_generation = true
validate_after_generation = true

# Watch for changes
watch = false
watch_debounce = 1000  # milliseconds
```

### Output Section

```toml
[output]
# Auto-format generated code
format = true
format_command = "rustfmt"

# Generate documentation
docs = true

# Generate audit trail
audit = true

# Caching
cache = true
cache_ttl = 3600  # seconds
```

### Features

```toml
[features]
ai = false  # Enable AI template rendering
otel = false  # Enable OpenTelemetry
rdf_inference = true  # Enable OWL inference
```

---

## Shell Completion

### Bash

```bash
# Generate completion
ggen --generate-bash-completion >> ~/.bashrc

# Reload
source ~/.bashrc

# Test
ggen syn<TAB>  # Auto-completes to "ggen sync"
```

### Zsh

```bash
# Generate completion
ggen --generate-zsh-completion >> ~/.zshrc

# Reload
source ~/.zshrc
```

### Fish

```bash
# Generate completion
ggen --generate-fish-completion > ~/.config/fish/completions/ggen.fish

# Reload
source ~/.config/fish/completions/ggen.fish
```

---

## Helpful Flags (All Commands)

```bash
ggen [COMMAND] --help              # Show help for command
ggen --version                     # Show version
ggen --verbose                     # Verbose output
ggen --quiet                       # Minimal output
ggen --config <PATH>              # Use custom config file
```

---

## Error Messages

### "Specification not found"

**Cause**: `.specify/specs/*.ttl` not found

**Solution**:
```bash
ggen init  # Create default specs directory
```

### "SPARQL query failed"

**Cause**: Invalid RDF or SPARQL syntax

**Solution**:
```bash
ggen validate .specify/specs/  # Check for errors
GGEN_LOG_LEVEL=debug ggen sync  # See detailed error
```

### "Template variable undefined"

**Cause**: Template uses variable that doesn't exist

**Solution**:
```bash
ggen inspect .specify/specs/  # See available variables
# Update template to match variable names
```

### "Generation timeout"

**Cause**: Complex ontology or slow system

**Solution**:
```bash
# Increase timeout
export GGEN_TIMEOUT=120

# Or: Simplify ontology
# Split large .ttl files
```

---

**See also:**
- [Installation Guide](../installation/INSTALLATION.md)
- [Getting Started Tutorial](../tutorials/01-getting-started.md)
- [How-To Guides](../how-to/01-common-tasks.md)
