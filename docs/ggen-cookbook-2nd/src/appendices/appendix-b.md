<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Appendix B - CLI Reference](#appendix-b---cli-reference)
  - [Command Overview](#command-overview)
    - [Core Commands](#core-commands)
    - [Marketplace Commands](#marketplace-commands)
    - [AI Commands (v1.0.0+)](#ai-commands-v100)
    - [Development Commands](#development-commands)
  - [Detailed Command Reference](#detailed-command-reference)
    - [`ggen gen` - Generate Code](#ggen-gen---generate-code)
    - [`ggen validate` - Validate Templates](#ggen-validate---validate-templates)
    - [`ggen list` - List Templates](#ggen-list---list-templates)
    - [`ggen show` - Show Template Details](#ggen-show---show-template-details)
    - [`ggen search` - Search Marketplace](#ggen-search---search-marketplace)
    - [`ggen add` - Install Templates](#ggen-add---install-templates)
    - [`ggen remove` - Remove Templates](#ggen-remove---remove-templates)
    - [`ggen update` - Update Templates](#ggen-update---update-templates)
    - [`ggen packs` - List Installed Packs](#ggen-packs---list-installed-packs)
  - [AI Commands (v1.0.0+)](#ai-commands-v100-1)
    - [`ggen ai generate` - AI Template Generation](#ggen-ai-generate---ai-template-generation)
    - [`ggen ai sparql` - AI SPARQL Generation](#ggen-ai-sparql---ai-sparql-generation)
    - [`ggen ai graph` - AI RDF Graph Generation](#ggen-ai-graph---ai-rdf-graph-generation)
    - [`ggen ai validate` - AI Template Validation](#ggen-ai-validate---ai-template-validation)
  - [Development Commands](#development-commands-1)
    - [`ggen template` - Template Development](#ggen-template---template-development)
    - [`ggen pack` - Pack Management](#ggen-pack---pack-management)
  - [Configuration](#configuration)
    - [Global Configuration](#global-configuration)
    - [Project Configuration](#project-configuration)
  - [Environment Variables](#environment-variables)
  - [Exit Codes](#exit-codes)
  - [Troubleshooting](#troubleshooting)
    - [Common Issues](#common-issues)
      - [Template Not Found](#template-not-found)
      - [Generation Errors](#generation-errors)
      - [Performance Issues](#performance-issues)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Appendix B - CLI Reference

This appendix provides a comprehensive reference for all GGen CLI commands, their options, and usage patterns.

## Command Overview

GGen provides a rich set of commands organized into several categories:

### Core Commands
- `ggen gen` - Generate code from templates
- `ggen validate` - Validate templates and generated code
- `ggen list` - List available templates
- `ggen show` - Show template details

### Marketplace Commands
- `ggen search` - Search for templates
- `ggen add` - Install templates from marketplace
- `ggen remove` - Remove installed templates
- `ggen update` - Update installed templates
- `ggen packs` - List installed template packs

### AI Commands (v1.0.0+)
- `ggen ai generate` - Generate templates using AI
- `ggen ai sparql` - Generate SPARQL queries using AI
- `ggen ai graph` - Generate RDF graphs using AI
- `ggen ai validate` - Validate templates with AI assistance
- `ggen ai demo` - Run AI template demonstrations

### Development Commands
- `ggen template` - Template development tools
- `ggen pack` - Template pack management
- `ggen extension` - Extension management
- `ggen plugin` - Plugin management

## Detailed Command Reference

### `ggen gen` - Generate Code

Generate code from templates with variable substitution.

```bash
ggen gen <template-ref> [--vars KEY=VALUE ...] [OPTIONS]
```

**Arguments:**
- `<template-ref>` - Template reference (e.g., `template-name`, `pack-id:template-path`)

**Options:**
- `--vars KEY=VALUE` - Set template variables
- `--var-file FILE` - Load variables from file
- `--output DIR` - Output directory
- `--dry` - Dry run (preview without writing files)
- `--force` - Overwrite existing files
- `--format FORMAT` - Output format (rust, typescript, etc.)
- `--watch` - Watch for changes and regenerate

**Examples:**
```bash
# Generate from local template
ggen gen api-endpoint --vars service=user method=GET

# Generate from marketplace template
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl --vars cmd=hello

# Generate with variable file
ggen gen api-endpoint --var-file vars.json

# Dry run preview
ggen gen api-endpoint --vars service=user --dry
```

### `ggen validate` - Validate Templates

Validate template syntax and variable usage.

```bash
ggen validate <template-ref> [--vars KEY=VALUE ...] [OPTIONS]
```

**Options:**
- `--strict` - Enable strict validation
- `--vars KEY=VALUE` - Test with specific variables
- `--format` - Validation format (human, json, junit)

**Examples:**
```bash
# Basic validation
ggen validate api-endpoint.tmpl

# Validate with test variables
ggen validate api-endpoint.tmpl --vars service=user method=GET

# Strict validation
ggen validate api-endpoint.tmpl --strict
```

### `ggen list` - List Templates

List available templates from all sources.

```bash
ggen list [OPTIONS]
```

**Options:**
- `--source SOURCE` - Filter by source (local, marketplace)
- `--format FORMAT` - Output format (table, json, tree)
- `--filter PATTERN` - Filter by name pattern

**Examples:**
```bash
# List all templates
ggen list

# List only marketplace templates
ggen list --source marketplace

# Filter by pattern
ggen list --filter "*api*"
```

### `ggen show` - Show Template Details

Display detailed information about a template.

```bash
ggen show <template-ref> [OPTIONS]
```

**Options:**
- `--vars KEY=VALUE` - Show with example variables
- `--format FORMAT` - Output format (human, json)
- `--preview` - Show rendered preview

**Examples:**
```bash
# Show template details
ggen show api-endpoint

# Show with example variables
ggen show api-endpoint --vars service=user method=GET

# Show as JSON
ggen show api-endpoint --format json
```

### `ggen search` - Search Marketplace

Search for templates in the marketplace.

```bash
ggen search <query> [OPTIONS]
```

**Options:**
- `--category CATEGORY` - Filter by category
- `--language LANGUAGE` - Filter by language
- `--author AUTHOR` - Filter by author
- `--limit N` - Limit results

**Examples:**
```bash
# Search for Rust templates
ggen search rust

# Search for API templates
ggen search api

# Search with filters
ggen search web --language typescript --category framework
```

### `ggen add` - Install Templates

Install templates from the marketplace.

```bash
ggen add <gpack-id> [OPTIONS]
```

**Options:**
- `--version VERSION` - Specific version to install
- `--local` - Install to local project only
- `--global` - Install globally

**Examples:**
```bash
# Install latest version
ggen add io.ggen.rust.cli-subcommand

# Install specific version
ggen add io.ggen.rust.cli-subcommand@1.2.0

# Install to local project
ggen add io.ggen.rust.cli-subcommand --local
```

### `ggen remove` - Remove Templates

Remove installed templates.

```bash
ggen remove <gpack-id> [OPTIONS]
```

**Examples:**
```bash
# Remove template pack
ggen remove io.ggen.rust.cli-subcommand

# Remove with confirmation
ggen remove io.ggen.rust.cli-subcommand --force
```

### `ggen update` - Update Templates

Update installed templates to latest versions.

```bash
ggen update [gpack-id] [OPTIONS]
```

**Options:**
- `--all` - Update all installed templates
- `--check` - Check for updates without installing
- `--dry-run` - Preview updates

**Examples:**
```bash
# Update all templates
ggen update --all

# Update specific template
ggen update io.ggen.rust.cli-subcommand

# Check for updates
ggen update --check
```

### `ggen packs` - List Installed Packs

List all installed template packs.

```bash
ggen packs [OPTIONS]
```

**Options:**
- `--format FORMAT` - Output format (table, json)
- `--outdated` - Show only outdated packs

**Examples:**
```bash
# List all installed packs
ggen packs

# Show as JSON
ggen packs --format json

# Show only outdated packs
ggen packs --outdated
```

## AI Commands (v1.0.0+)

### `ggen ai generate` - AI Template Generation

Generate templates using AI from natural language descriptions.

```bash
ggen ai generate <description> [OPTIONS]
```

**Options:**
- `--language LANG` - Target programming language
- `--framework FRAMEWORK` - Target framework
- `--output FILE` - Output file path
- `--provider PROVIDER` - AI provider (ollama, openai, etc.)
- `--model MODEL` - Specific AI model
- `--temperature TEMP` - Creativity level (0.0-1.0)

**Examples:**
```bash
# Generate Rust API controller
ggen ai generate "REST API controller for user management" --language rust --framework axum

# Generate Python CLI tool
ggen ai generate "CLI tool for data processing" --language python --output data_tool.py

# Generate with specific model
ggen ai generate "React component" --language typescript --model gpt-4o
```

### `ggen ai sparql` - AI SPARQL Generation

Generate SPARQL queries from natural language intent.

```bash
ggen ai sparql <intent> [OPTIONS]
```

**Options:**
- `--graph FILE` - RDF graph file
- `--output FILE` - Output query file
- `--prefixes PREFIX=URI` - Define namespace prefixes

**Examples:**
```bash
# Generate user query
ggen ai sparql "Find all users with admin role" --graph data.ttl

# Generate property query
ggen ai sparql "Get all properties of a resource" --graph ontology.ttl --prefixes foaf=http://xmlns.com/foaf/0.1/
```

### `ggen ai graph` - AI RDF Graph Generation

Generate RDF ontologies from domain descriptions.

```bash
ggen ai graph <description> [OPTIONS]
```

**Options:**
- `--output FILE` - Output graph file
- `--format FORMAT` - Output format (turtle, jsonld, etc.)

**Examples:**
```bash
# Generate person ontology
ggen ai graph "Person management system with roles and permissions" --output person.ttl

# Generate e-commerce ontology
ggen ai graph "E-commerce product catalog" --output catalog.ttl --format jsonld
```

### `ggen ai validate` - AI Template Validation

Validate templates using AI assistance.

```bash
ggen ai validate <template> [OPTIONS]
```

**Options:**
- `--vars KEY=VALUE` - Test variables
- `--strict` - Enable strict validation
- `--threshold SCORE` - Minimum confidence score

**Examples:**
```bash
# Validate template
ggen ai validate api-endpoint.tmpl

# Validate with variables
ggen ai validate api-endpoint.tmpl --vars service=user method=GET

# Strict validation
ggen ai validate api-endpoint.tmpl --strict --threshold 0.8
```

## Development Commands

### `ggen template` - Template Development

Tools for template development and testing.

```bash
ggen template <subcommand> [OPTIONS]
```

**Subcommands:**
- `create` - Create new template
- `test` - Test template
- `lint` - Lint template
- `format` - Format template

**Examples:**
```bash
# Create new template
ggen template create api-endpoint

# Test template
ggen template test api-endpoint.tmpl

# Lint template
ggen template lint api-endpoint.tmpl
```

### `ggen pack` - Pack Management

Manage template packs for publishing.

```bash
ggen pack <subcommand> [OPTIONS]
```

**Subcommands:**
- `init` - Initialize new pack
- `build` - Build pack
- `test` - Test pack
- `publish` - Publish pack

**Examples:**
```bash
# Initialize new pack
ggen pack init

# Build pack
ggen pack build

# Publish pack
ggen pack publish
```

## Configuration

### Global Configuration

GGen uses a hierarchical configuration system:

```toml
# ~/.ggen/config.toml
[registry]
default = "https://registry.ggen.io"

[generation]
output_dir = "./generated"
dry_run = false

[ai]
default_provider = "ollama"
default_model = "qwen3-coder:30b"

[cache]
enabled = true
ttl = 3600
```

### Project Configuration

```toml
# .ggen.toml
[project]
name = "my-project"
version = "1.0.0"

[dependencies]
"io.ggen.rust.base" = "1.0"
"io.ggen.common.test-utils" = "2.0"

[overrides]
"*" = { author = "My Company" }
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `GGEN_CONFIG` | Path to config file | `~/.ggen/config.toml` |
| `GGEN_CACHE_DIR` | Cache directory | `~/.ggen/cache` |
| `GGEN_LOG_LEVEL` | Log level | `info` |
| `GGEN_REGISTRY_URL` | Registry URL | `https://registry.ggen.io` |
| `GGEN_AI_PROVIDER` | Default AI provider | `ollama` |
| `GGEN_AI_MODEL` | Default AI model | `qwen3-coder:30b` |

## Exit Codes

GGen uses standard exit codes:

- `0` - Success
- `1` - General error
- `2` - Template not found
- `3` - Invalid template
- `4` - Generation failed
- `5` - Validation failed
- `6` - Network error

## Troubleshooting

### Common Issues

#### Template Not Found
```bash
# Check if template is installed
ggen packs

# Search for template
ggen search <template-name>

# Install missing template
ggen add <gpack-id>
```

#### Generation Errors
```bash
# Enable debug logging
GGEN_LOG_LEVEL=debug ggen gen <template>

# Validate template first
ggen validate <template>

# Check variable requirements
ggen show <template>
```

#### Performance Issues
```bash
# Enable caching
ggen gen <template> --cache

# Check cache status
ggen cache status

# Clear cache if needed
ggen cache clear
```

This CLI reference covers all major commands and options available in GGen. For the most up-to-date information, use `ggen --help` or `ggen <command> --help`.
