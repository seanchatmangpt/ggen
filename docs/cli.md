<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [CLI](#cli)
  - [Marketplace Commands](#marketplace-commands)
    - [Search and Discovery](#search-and-discovery)
    - [Installation and Management](#installation-and-management)
    - [Gpack Publishing (for authors)](#gpack-publishing-for-authors)
  - [AI Commands](#ai-commands)
    - [AI Template Generation](#ai-template-generation)
    - [AI SPARQL Generation](#ai-sparql-generation)
    - [AI RDF Graph Generation](#ai-rdf-graph-generation)
    - [AI Project Scaffolding](#ai-project-scaffolding)
    - [AI Source File Analysis](#ai-source-file-analysis)
    - [AI Model Management](#ai-model-management)
    - [AI Template Validation](#ai-template-validation)
    - [AI MCP Server](#ai-mcp-server)
  - [Generation Commands](#generation-commands)
    - [Template Generation](#template-generation)
    - [Template Discovery](#template-discovery)
  - [Validation Commands](#validation-commands)
  - [Utility Commands](#utility-commands)
  - [Variable Precedence](#variable-precedence)
  - [Gpack Template Reference Syntax](#gpack-template-reference-syntax)
  - [Dry-Run Mode](#dry-run-mode)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# CLI

## Marketplace Commands

### Search and Discovery

```bash
# Search for gpacks by keywords
ggen search <query>

# Examples:
ggen search rust cli
ggen search python api
ggen search typescript react

# Browse popular categories
ggen categories

# Get detailed gpack information
ggen show <gpack-id>
```

### Installation and Management

```bash
# Install gpack (latest version)
ggen add <gpack-id>

# Install specific version
ggen add <gpack-id>@<version>

# Examples:
ggen add io.ggen.rust.cli-subcommand
ggen add io.ggen.rust.cli-subcommand@0.2.0

# List installed gpacks
ggen packs

# Update all gpacks to latest compatible versions
ggen update

# Update specific gpack
ggen update <gpack-id>

# Remove gpack
ggen remove <gpack-id>
```

### Gpack Publishing (for authors)

```bash
# Initialize new gpack
ggen pack init

# Lint gpack for publishing
ggen pack lint

# Run tests
ggen pack test

# Publish to registry
ggen pack publish
```

## AI Commands

**ggen-ai v1.0.0** provides intelligent code generation using advanced LLMs with multi-provider support (OpenAI, Anthropic, Ollama, Gemini, Groq, Cohere).

### AI Template Generation

```bash
# Generate templates from natural language descriptions
ggen ai generate <description> [--language LANG] [--framework FRAMEWORK] [--output FILE]

# Examples:
ggen ai generate "A Rust REST API controller for user management" --language rust --framework axum --output user_controller.tmpl
ggen ai generate "Python CLI tool for data processing" --language python --output data_tool.py.tmpl
ggen ai generate "React component for user profiles" --language typescript --framework react --output profile.tsx.tmpl
```

### AI SPARQL Generation

```bash
# Generate SPARQL queries from natural language intent
ggen ai sparql <intent> [--graph FILE] [--output FILE] [--prefixes PREFIX=URI]

# Examples:
ggen ai sparql "Find all users with admin role" --graph data.ttl --output admin_query.sparql
ggen ai sparql "Get all properties of a resource" --graph ontology.ttl --output properties.sparql
```

### AI RDF Graph Generation

```bash
# Generate RDF ontologies from domain descriptions
ggen ai graph <description> [--output FILE] [--format FORMAT]

# Examples:
ggen ai graph "Person management system with roles and permissions" --output person.ttl
ggen ai graph "E-commerce product catalog" --output catalog.ttl --format jsonld
```

### AI Project Scaffolding

```bash
# Generate complete project structures
ggen ai project <description> --name NAME --language LANG [--framework FRAMEWORK] [--output DIR] [--tests] [--docs] [--ci]

# Examples:
ggen ai project "E-commerce API with authentication" --name shop-api --language rust --framework axum --tests --docs --output generated-shop-api/
ggen ai project "Python web application" --name webapp --language python --framework fastapi --output webapp/
```

### AI Source File Analysis

```bash
# Generate templates from existing source files
ggen ai from-source <file> [--language LANG] [--output FILE] [--extract-variables] [--include-rdf]

# Examples:
ggen ai from-source src/main.rs --language rust --output main_template.tmpl
ggen ai from-source lib/utils.js --language javascript --extract-variables --output utils.tmpl
```

### AI Model Management

```bash
# List available AI models and providers
ggen ai models

# Output shows supported models across all providers
```

### AI Template Validation

```bash
# Validate templates with AI assistance
ggen ai validate <template> [--vars KEY=VALUE]

# Examples:
ggen ai validate templates/api.tmpl
ggen ai validate templates/cli.tmpl --vars name=hello
```

### AI MCP Server

```bash
# Start MCP server for AI tool integration
ggen ai server [--model MODEL] [--port PORT] [--host HOST]

# Examples:
ggen ai server --model qwen3-coder:30b
ggen ai server --model gpt-4o --port 8080
```

## Generation Commands

### Template Generation

```bash
# Generate from gpack template
ggen gen <gpack-id>:<template-path> [--vars k=v ...] [--dry]

# Generate from local template
ggen gen <scope> <action> [--vars k=v ...] [--dry]

# Examples:
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello
ggen gen cli subcommand --vars cmd=hello summary="Print greeting"
```

### Template Discovery

```bash
# List available templates (local + gpacks)
ggen list

# Show template details
ggen show <template-ref> [--vars k=v ...]

# Examples:
ggen show io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl
ggen show cli subcommand
```

## Validation Commands

```bash
# Validate template frontmatter
ggen validate <template-ref> [--vars k=v ...]

# Lint template with schema validation
ggen lint <template-ref>

# Examples:
ggen validate io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl
ggen lint cli subcommand
```

## Utility Commands

```bash
# Export RDF graph
ggen graph export <template-ref> --fmt ttl|jsonld

# Generate hazard report
ggen hazard

# Generate shell completion scripts
ggen completion bash|zsh|fish
```

## Variable Precedence

Variables are resolved in this order (later values override earlier):

1. **CLI arguments** (`--var key=value`)
2. **Environment variables** (from `.env` files)
3. **System environment** (`$HOME`, `$USER`, etc.)
4. **Gpack variables** (from gpack `ggen.toml`)
5. **Template frontmatter** (`vars:` section)
6. **SPARQL variables** (from queries)

## Gpack Template Reference Syntax

When using gpack templates, use the format:

```
<gpack-id>:<template-path>
```

Examples:
- `io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl`
- `io.ggen.python.api:api/endpoint/fastapi.tmpl`
- `io.ggen.typescript.react:components/button.tsx.tmpl`

## Dry-Run Mode

Preview template rendering without writing files:

```bash
ggen gen --template templates/api/endpoint/rust.tmpl --var name=User --dry
```

Dry-run behavior:
- RDF graphs are loaded (read-only)
- SPARQL queries execute normally
- Templates render completely
- Output shows what would be written
- No files are created or modified
- No shell commands execute (when implemented)
- No injections occur (when implemented)
