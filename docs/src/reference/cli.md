# CLI Reference

Complete reference for all `ggen` command-line interface commands.

## Table of Contents

- [Installation](#installation)
- [Global Options](#global-options)
- [Commands Overview](#commands-overview)
- [Marketplace Commands](#marketplace-commands)
- [Project Commands](#project-commands)
- [AI Commands](#ai-commands)
- [Template Commands](#template-commands)
- [Hook Commands](#hook-commands)
- [Graph Commands](#graph-commands)
- [Utils Commands](#utils-commands)

## Installation

```bash
# Install from cargo
cargo install ggen-cli

# Or build from source
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo build --release
```

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

---

## Marketplace Commands

Discover, install, and publish templates in the ggen marketplace.

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
# Search for React templates
ggen marketplace search "react"

# Search with category filter
ggen marketplace search "api" --category backend --limit 20

# Search for Rust templates
ggen marketplace search "rust" --limit 5
```

**Output:**
```json
{
  "packages": [
    {
      "name": "rust-api-template",
      "version": "1.0.0",
      "description": "REST API template for Rust",
      "author": "example",
      "downloads": 1500,
      "stars": 42
    }
  ],
  "total": 1
}
```

### ggen marketplace install

Install a package from the marketplace.

**Usage:**
```bash
ggen marketplace install <PACKAGE> [OPTIONS]
```

**Arguments:**
- `<PACKAGE>` - Package name to install

**Options:**
- `--version <VERSION>` - Specific version to install
- `--path <PATH>` - Installation directory

**Examples:**
```bash
# Install latest version
ggen marketplace install rust-api-template

# Install specific version
ggen marketplace install rust-api-template --version 1.2.0

# Install to custom directory
ggen marketplace install react-app --path ./my-templates
```

**Output:**
```json
{
  "package": "rust-api-template",
  "version": "1.0.0",
  "path": "~/.ggen/templates/rust-api-template",
  "dependencies": []
}
```

### ggen marketplace list

List installed packages.

**Usage:**
```bash
ggen marketplace list [OPTIONS]
```

**Options:**
- `--outdated` - Show only packages with available updates

**Examples:**
```bash
# List all installed packages
ggen marketplace list

# Show packages with updates available
ggen marketplace list --outdated
```

**Output:**
```json
{
  "packages": [
    {
      "name": "rust-api-template",
      "version": "1.0.0",
      "title": "Rust API Template",
      "description": "REST API template"
    }
  ],
  "total": 1
}
```

### ggen marketplace publish

Publish a template to the marketplace.

**Usage:**
```bash
ggen marketplace publish <PATH> [OPTIONS]
```

**Arguments:**
- `<PATH>` - Path to template directory

**Options:**
- `--name <NAME>` - Package name (defaults to directory name)
- `--version <VERSION>` - Version string (default: 0.1.0)
- `--dry-run` - Preview without publishing

**Examples:**
```bash
# Publish from current directory
ggen marketplace publish .

# Publish with specific name and version
ggen marketplace publish ./my-template --name custom-template --version 1.0.0

# Preview publication
ggen marketplace publish ./my-template --dry-run
```

**Output:**
```json
{
  "package": "custom-template",
  "version": "1.0.0"
}
```

---

## Project Commands

Create and manage projects using templates and code generation.

### ggen project new

Create a new project from scratch.

**Usage:**
```bash
ggen project new <NAME> [OPTIONS]
```

**Arguments:**
- `<NAME>` - Project name

**Options:**
- `--type <TYPE>` - Project type (rust-cli, rust-lib, nextjs, etc.)
- `--framework <FW>` - Framework selection
- `--path <PATH>` - Output directory

**Examples:**
```bash
# Create Rust CLI project
ggen project new my-cli --type rust-cli

# Create Next.js web project
ggen project new my-app --type nextjs --framework react

# Create in specific directory
ggen project new my-lib --type rust-lib --path ./projects
```

**Output:**
```json
{
  "project_name": "my-cli",
  "path": "./my-cli",
  "project_type": "rust-cli",
  "framework": null,
  "files_created": 15,
  "next_steps": "cd my-cli && cargo build"
}
```

### ggen project plan

Create a generation plan from templates.

**Usage:**
```bash
ggen project plan <PLAN_FILE> [OPTIONS]
```

**Arguments:**
- `<PLAN_FILE>` - Path to plan YAML/JSON file

**Options:**
- `--output <DIR>` - Output directory
- `--format <FMT>` - Output format (yaml, json)
- `--dry-run` - Preview without executing

**Examples:**
```bash
# Generate from YAML plan
ggen project plan project-plan.yaml

# Specify output directory
ggen project plan plan.yaml --output ./generated

# Preview plan execution
ggen project plan plan.yaml --dry-run
```

**Plan File Example:**
```yaml
templates:
  - name: rust-models
    variables:
      project_name: "my-api"
      models: ["User", "Post"]
  - name: rust-api
    variables:
      port: 8080
```

**Output:**
```json
{
  "plan_file": "project-plan.yaml",
  "output_path": "./generated",
  "format": "yaml",
  "tasks": ["Generate models", "Generate API"],
  "variables_count": 3,
  "operations_count": 10
}
```

### ggen project gen

Generate code from templates.

**Usage:**
```bash
ggen project gen <TEMPLATE> [OPTIONS]
```

**Arguments:**
- `<TEMPLATE>` - Template name or path

**Options:**
- `--output <DIR>` - Output directory (default: current)
- `--var <KEY=VALUE>` - Template variables (repeatable)
- `--dry-run` - Preview without creating files

**Examples:**
```bash
# Generate from template
ggen project gen rust-models --output ./src

# With template variables
ggen project gen rust-api --var project_name=my-api --var port=8080

# Preview generation
ggen project gen rust-models --var model=User --dry-run
```

**Output:**
```json
{
  "files_generated": 5,
  "files_created": 5,
  "output_dir": "./src",
  "operations": [
    {
      "operation_type": "create",
      "path": "./src/models/user.rs"
    }
  ],
  "dry_run": false
}
```

### ggen project apply

Apply a changeset to existing code.

**Usage:**
```bash
ggen project apply <CHANGESET> [OPTIONS]
```

**Arguments:**
- `<CHANGESET>` - Path to changeset file

**Options:**
- `--dry-run` - Preview changes without applying
- `--force` - Apply without confirmation

**Examples:**
```bash
# Apply changeset
ggen project apply changes.yaml

# Preview changes
ggen project apply changes.yaml --dry-run

# Force application
ggen project apply changes.yaml --force
```

**Output:**
```json
{
  "changes_applied": 5,
  "operations_count": 5,
  "files_modified": 3,
  "files_created": 2,
  "files_deleted": 0,
  "dry_run": false
}
```

### ggen project init

Initialize a new ggen project.

**Usage:**
```bash
ggen project init [NAME] [OPTIONS]
```

**Arguments:**
- `[NAME]` - Project name (default: current directory name)

**Options:**
- `--preset <PRESET>` - Project preset (minimal, standard, full)
- `--path <PATH>` - Project directory

**Examples:**
```bash
# Initialize in current directory
ggen project init

# Initialize with name
ggen project init my-project

# Use preset
ggen project init my-app --preset full
```

**Output:**
```json
{
  "project_name": "my-project",
  "project_path": "./my-project",
  "preset": "standard",
  "files_created": ["ggen.yaml", "README.md"],
  "directories_created": ["templates", "hooks"],
  "next_steps": ["Edit ggen.yaml", "Add templates"]
}
```

### ggen project generate

Generate files from configured templates.

**Usage:**
```bash
ggen project generate [OPTIONS]
```

**Options:**
- `--config <FILE>` - Configuration file (default: ggen.yaml)
- `--template <NAME>` - Generate specific template only
- `--output <DIR>` - Output directory

**Examples:**
```bash
# Generate all configured templates
ggen project generate

# Generate specific template
ggen project generate --template rust-models

# Use custom config
ggen project generate --config custom.yaml
```

**Output:**
```json
{
  "templates_processed": 3,
  "files_generated": 15,
  "bytes_written": "45.2 KB",
  "output_paths": ["./src/models", "./src/api"]
}
```

### ggen project watch

Watch for changes and regenerate automatically.

**Usage:**
```bash
ggen project watch [OPTIONS]
```

**Options:**
- `--path <PATH>` - Directory to watch (default: current)
- `--debounce <MS>` - Debounce time in milliseconds (default: 500)
- `--config <FILE>` - Configuration file

**Examples:**
```bash
# Watch current directory
ggen project watch

# Watch with custom debounce
ggen project watch --debounce 1000

# Watch specific directory
ggen project watch --path ./templates
```

**Output:**
```json
{
  "project_path": "./",
  "debounce_ms": 500,
  "status": "watching",
  "message": "Watching for changes..."
}
```

---

## AI Commands

AI-powered code generation and analysis using LLM models.

### ggen ai generate

Generate code with AI assistance.

**Usage:**
```bash
ggen ai generate <PROMPT> [OPTIONS]
```

**Arguments:**
- `<PROMPT>` - Generation prompt

**Options:**
- `--code <CODE>` - Existing code context
- `--model <MODEL>` - AI model (default: gpt-3.5-turbo)
- `--api-key <KEY>` - API key (or set OPENAI_API_KEY)
- `--suggestions` - Include improvement suggestions
- `--language <LANG>` - Programming language
- `--max-tokens <N>` - Maximum tokens (default: 2000)
- `--temperature <T>` - Temperature 0.0-2.0 (default: 0.7)

**Examples:**
```bash
# Basic generation
ggen ai generate "Create a Rust function that calculates fibonacci numbers"

# With existing code
ggen ai generate "Add error handling" --code "fn main() { ... }"

# Specific model and language
ggen ai generate "Generate REST API" --model gpt-4 --language rust

# With suggestions
ggen ai generate "Optimize this code" --code "..." --suggestions
```

**Output:**
```json
{
  "generated_code": "fn fibonacci(n: u64) -> u64 { ... }",
  "language": "rust",
  "tokens_used": 150,
  "model": "gpt-3.5-turbo",
  "finish_reason": "stop"
}
```

### ggen ai chat

Interactive chat session with AI.

**Usage:**
```bash
ggen ai chat [OPTIONS]
```

**Options:**
- `--message <MSG>` - Initial message
- `--model <MODEL>` - AI model
- `--api-key <KEY>` - API key
- `--session <ID>` - Resume session
- `--system <PROMPT>` - System prompt

**Examples:**
```bash
# Start interactive chat
ggen ai chat

# Chat with initial message
ggen ai chat --message "How do I implement async in Rust?"

# Resume previous session
ggen ai chat --session abc123

# Custom system prompt
ggen ai chat --system "You are a Rust expert"
```

**Output:**
```json
{
  "messages": [
    {
      "role": "user",
      "content": "How do I implement async?"
    },
    {
      "role": "assistant",
      "content": "To implement async in Rust..."
    }
  ],
  "session_id": "abc123",
  "model": "gpt-3.5-turbo",
  "tokens_used": 250
}
```

### ggen ai analyze

Analyze code with AI insights.

**Usage:**
```bash
ggen ai analyze <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - File to analyze

**Options:**
- `--focus <ASPECT>` - Analysis focus (performance, security, style)
- `--model <MODEL>` - AI model
- `--api-key <KEY>` - API key

**Examples:**
```bash
# Analyze file
ggen ai analyze src/main.rs

# Focus on performance
ggen ai analyze src/lib.rs --focus performance

# Security analysis
ggen ai analyze src/auth.rs --focus security
```

**Output:**
```json
{
  "file_path": "src/main.rs",
  "insights": [
    "Function complexity is high",
    "Consider using error handling"
  ],
  "suggestions": [
    "Extract helper functions",
    "Add Result type"
  ],
  "complexity_score": 7.5,
  "model": "gpt-3.5-turbo",
  "tokens_used": 300
}
```

---

## Template Commands

Manage and work with code generation templates.

### ggen template show

Show template metadata and details.

**Usage:**
```bash
ggen template show <TEMPLATE>
```

**Arguments:**
- `<TEMPLATE>` - Template name

**Examples:**
```bash
# Show template details
ggen template show rust-models

# Show installed template
ggen template show my-custom-template
```

**Output:**
```json
{
  "name": "rust-models",
  "path": "~/.ggen/templates/rust-models",
  "description": "Generate Rust data models",
  "output_path": "./src/models",
  "variables": ["model_name", "fields"],
  "rdf_sources": ["schema.ttl"],
  "sparql_queries_count": 3,
  "determinism_seed": 42
}
```

### ggen template new

Create a new template.

**Usage:**
```bash
ggen template new <NAME> [OPTIONS]
```

**Arguments:**
- `<NAME>` - Template name

**Options:**
- `--type <TYPE>` - Template type (tera, rdf, hybrid)
- `--path <PATH>` - Template directory

**Examples:**
```bash
# Create Tera template
ggen template new my-template --type tera

# Create RDF template
ggen template new rdf-template --type rdf

# Create in custom directory
ggen template new custom --path ./templates
```

**Output:**
```json
{
  "template_name": "my-template",
  "template_type": "tera",
  "path": "~/.ggen/templates/my-template"
}
```

### ggen template list

List available templates.

**Usage:**
```bash
ggen template list [OPTIONS]
```

**Options:**
- `--local` - Show only local templates
- `--installed` - Show only installed marketplace templates
- `--all` - Show all templates

**Examples:**
```bash
# List all templates
ggen template list

# List local templates only
ggen template list --local

# List installed from marketplace
ggen template list --installed
```

**Output:**
```json
{
  "templates": [
    {
      "name": "rust-models",
      "source": "marketplace",
      "description": "Generate Rust models",
      "path": "~/.ggen/templates/rust-models"
    }
  ],
  "total": 1,
  "directory": "~/.ggen/templates"
}
```

### ggen template lint

Validate template syntax and structure.

**Usage:**
```bash
ggen template lint <TEMPLATE> [OPTIONS]
```

**Arguments:**
- `<TEMPLATE>` - Template name or path

**Options:**
- `--strict` - Enable strict mode
- `--fix` - Auto-fix issues where possible

**Examples:**
```bash
# Lint template
ggen template lint my-template

# Strict validation
ggen template lint my-template --strict

# Auto-fix issues
ggen template lint my-template --fix
```

**Output:**
```json
{
  "has_errors": false,
  "has_warnings": true,
  "errors": [],
  "warnings": [
    {
      "line": 10,
      "message": "Variable 'unused_var' is declared but not used"
    }
  ]
}
```

### ggen template generate

Generate output from a template.

**Usage:**
```bash
ggen template generate <TEMPLATE> [OPTIONS]
```

**Arguments:**
- `<TEMPLATE>` - Template name or path

**Options:**
- `--output <PATH>` - Output file path
- `--var <KEY=VALUE>` - Template variables (repeatable)
- `--rdf <FILE>` - RDF data file
- `--query <SPARQL>` - SPARQL query

**Examples:**
```bash
# Generate from template
ggen template generate rust-models --output ./src/models.rs

# With variables
ggen template generate api-routes --var model=User --var version=v1

# With RDF data
ggen template generate rdf-template --rdf schema.ttl --output generated.rs
```

**Output:**
```json
{
  "output_path": "./src/models.rs",
  "files_created": 1,
  "bytes_written": 2048,
  "rdf_files_loaded": 1,
  "sparql_queries_executed": 3
}
```

### ggen template generate-tree

Generate directory structure from template.

**Usage:**
```bash
ggen template generate-tree <TEMPLATE> [OPTIONS]
```

**Arguments:**
- `<TEMPLATE>` - Template name

**Options:**
- `--output <DIR>` - Output directory
- `--var <KEY=VALUE>` - Template variables

**Examples:**
```bash
# Generate directory tree
ggen template generate-tree project-scaffold --output ./my-project

# With variables
ggen template generate-tree full-stack --var name=MyApp --output ./app
```

**Output:**
```json
{
  "output_directory": "./my-project"
}
```

### ggen template generate-rdf

Generate RDF-based templates.

**Usage:**
```bash
ggen template generate-rdf <RDF_FILE> [OPTIONS]
```

**Arguments:**
- `<RDF_FILE>` - RDF data file

**Options:**
- `--output <DIR>` - Output directory
- `--template <TEMPLATE>` - Template to use
- `--format <FMT>` - RDF format (turtle, rdf/xml, n-triples)

**Examples:**
```bash
# Generate from RDF
ggen template generate-rdf schema.ttl --output ./generated

# Specify template
ggen template generate-rdf data.rdf --template rust-models --output ./src
```

**Output:**
```json
{
  "output_dir": "./generated",
  "files_generated": 5,
  "project_name": "generated-project"
}
```

---

## Hook Commands

Manage Git hooks and file system automation.

### ggen hook create

Create a new hook.

**Usage:**
```bash
ggen hook create <EVENT> <SCRIPT> [OPTIONS]
```

**Arguments:**
- `<EVENT>` - Trigger event (pre-commit, post-commit, etc.)
- `<SCRIPT>` - Script path or command

**Options:**
- `--name <NAME>` - Hook name

**Examples:**
```bash
# Create pre-commit hook
ggen hook create pre-commit ./scripts/lint.sh

# Create post-commit hook
ggen hook create post-commit "cargo fmt" --name format-code

# Create with custom name
ggen hook create pre-push "./test.sh" --name run-tests
```

**Output:**
```json
{
  "hook_id": "abc123",
  "status": "Active"
}
```

### ggen hook list

List all hooks.

**Usage:**
```bash
ggen hook list [OPTIONS]
```

**Options:**
- `--filter <FILTER>` - Filter by event type
- `--verbose` - Show detailed information

**Examples:**
```bash
# List all hooks
ggen hook list

# Filter by event
ggen hook list --filter pre-commit

# Verbose output
ggen hook list --verbose
```

**Output:**
```json
{
  "hooks": [
    {
      "id": "abc123",
      "trigger": "pre-commit",
      "action": "./scripts/lint.sh",
      "created_at": "2024-01-15T10:30:00Z"
    }
  ],
  "total": 1
}
```

### ggen hook remove

Remove a hook.

**Usage:**
```bash
ggen hook remove <HOOK_ID>
```

**Arguments:**
- `<HOOK_ID>` - Hook ID to remove

**Examples:**
```bash
# Remove hook by ID
ggen hook remove abc123
```

**Output:**
```json
{
  "hook_id": "abc123",
  "status": "Removed"
}
```

### ggen hook monitor

Monitor and execute hooks.

**Usage:**
```bash
ggen hook monitor [OPTIONS]
```

**Options:**
- `--watch <PATH>` - Directory to watch
- `--event <EVENT>` - Specific event to monitor

**Examples:**
```bash
# Monitor all hooks
ggen hook monitor

# Watch specific directory
ggen hook monitor --watch ./src

# Monitor specific event
ggen hook monitor --event file-change
```

**Output:**
```json
{
  "active_hooks": 3,
  "watching": 1,
  "hooks": [
    {
      "id": "abc123",
      "trigger": "file-change",
      "action": "regenerate",
      "created_at": "2024-01-15T10:30:00Z"
    }
  ]
}
```

---

## Graph Commands

Work with RDF graphs and SPARQL queries.

### ggen graph load

Load RDF data into graph.

**Usage:**
```bash
ggen graph load <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - RDF file to load

**Options:**
- `--format <FMT>` - RDF format (turtle, rdf/xml, n-triples, n-quads)

**Examples:**
```bash
# Load Turtle file
ggen graph load schema.ttl

# Load RDF/XML
ggen graph load data.rdf --format rdf/xml

# Load N-Triples
ggen graph load triples.nt --format n-triples
```

**Output:**
```json
{
  "triples_loaded": 150,
  "total_triples": 150,
  "format": "turtle",
  "file_path": "schema.ttl"
}
```

### ggen graph query

Query graph with SPARQL.

**Usage:**
```bash
ggen graph query <SPARQL_QUERY> [OPTIONS]
```

**Arguments:**
- `<SPARQL_QUERY>` - SPARQL query string

**Options:**
- `--graph-file <FILE>` - Load graph from file first
- `--format <FMT>` - Output format (json, csv, xml)

**Examples:**
```bash
# Query loaded graph
ggen graph query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

# Query from file
ggen graph query "SELECT * WHERE { ?s a :Person }" --graph-file schema.ttl

# CSV output
ggen graph query "SELECT ?name ?age WHERE { ?p :name ?name ; :age ?age }" --format csv
```

**Output:**
```json
{
  "bindings": [
    {
      "s": "http://example.org/Person1",
      "p": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      "o": "http://example.org/Person"
    }
  ],
  "variables": ["s", "p", "o"],
  "result_count": 1
}
```

### ggen graph export

Export graph to file.

**Usage:**
```bash
ggen graph export <OUTPUT> [OPTIONS]
```

**Arguments:**
- `<OUTPUT>` - Output file path

**Options:**
- `--format <FMT>` - Export format (turtle, rdf/xml, n-triples, n-quads)
- `--compress` - Compress output

**Examples:**
```bash
# Export to Turtle
ggen graph export output.ttl --format turtle

# Export to RDF/XML
ggen graph export data.rdf --format rdf/xml

# Compressed export
ggen graph export graph.ttl.gz --compress
```

**Output:**
```json
{
  "output_path": "output.ttl",
  "format": "turtle",
  "triples_exported": 150,
  "file_size_bytes": 8192
}
```

### ggen graph visualize

Visualize graph structure.

**Usage:**
```bash
ggen graph visualize <OUTPUT> [OPTIONS]
```

**Arguments:**
- `<OUTPUT>` - Output file (SVG, PNG, DOT)

**Options:**
- `--format <FMT>` - Output format (svg, png, dot)
- `--layout <LAYOUT>` - Layout algorithm (dot, neato, circo)
- `--max-nodes <N>` - Maximum nodes to render

**Examples:**
```bash
# Generate SVG
ggen graph visualize graph.svg --format svg

# Generate PNG with layout
ggen graph visualize graph.png --format png --layout neato

# Limit nodes
ggen graph visualize large-graph.svg --max-nodes 100
```

**Output:**
```json
{
  "nodes_rendered": 50,
  "edges_rendered": 75,
  "output_path": "graph.svg",
  "format": "svg"
}
```

---

## Utils Commands

System utilities and diagnostics.

### ggen utils doctor

Run system diagnostics.

**Usage:**
```bash
ggen utils doctor [OPTIONS]
```

**Options:**
- `--all` - Run all checks
- `--fix` - Attempt to fix issues
- `--format <FMT>` - Output format (table, json, env)

**Examples:**
```bash
# Run diagnostics
ggen utils doctor

# Run all checks
ggen utils doctor --all

# Auto-fix issues
ggen utils doctor --fix

# JSON output
ggen utils doctor --format json
```

**Output:**
```json
{
  "checks_passed": 8,
  "checks_failed": 0,
  "warnings": 1,
  "results": [
    {
      "name": "Cargo Installation",
      "status": "Ok",
      "message": "cargo 1.70.0 found"
    },
    {
      "name": "Template Directory",
      "status": "Warning",
      "message": "No templates found"
    }
  ],
  "overall_status": "healthy"
}
```

### ggen utils env

Manage environment variables.

**Usage:**
```bash
ggen utils env [OPTIONS]
```

**Options:**
- `--list` - List all variables
- `--get <KEY>` - Get specific variable
- `--set <KEY=VALUE>` - Set variable
- `--system` - Use system environment

**Examples:**
```bash
# List all variables
ggen utils env --list

# Get specific variable
ggen utils env --get OPENAI_API_KEY

# Set variable
ggen utils env --set OPENAI_API_KEY=sk-...

# System environment
ggen utils env --list --system
```

**Output:**
```json
{
  "variables": {
    "GGEN_HOME": "~/.ggen",
    "GGEN_TEMPLATES": "~/.ggen/templates",
    "OPENAI_API_KEY": "sk-***"
  },
  "total": 3
}
```

### ggen utils completion

Generate shell completion scripts.

**Usage:**
```bash
ggen utils completion <SHELL>
```

**Arguments:**
- `<SHELL>` - Shell type (bash, zsh, fish, powershell)

**Examples:**
```bash
# Bash completion
ggen utils completion bash > ~/.local/share/bash-completion/completions/ggen

# Zsh completion
ggen utils completion zsh > ~/.zsh/completions/_ggen

# Fish completion
ggen utils completion fish > ~/.config/fish/completions/ggen.fish
```

**Setup:**
```bash
# Bash
echo 'source <(ggen utils completion bash)' >> ~/.bashrc

# Zsh
echo 'source <(ggen utils completion zsh)' >> ~/.zshrc

# Fish
ggen utils completion fish > ~/.config/fish/completions/ggen.fish
```

---

## Environment Variables

ggen respects these environment variables:

- `GGEN_HOME` - Home directory for ggen (default: `~/.ggen`)
- `GGEN_TEMPLATES` - Template directory (default: `$GGEN_HOME/templates`)
- `GGEN_CONFIG` - Default config file (default: `./ggen.yaml`)
- `OPENAI_API_KEY` - OpenAI API key for AI commands
- `ANTHROPIC_API_KEY` - Anthropic API key for AI commands
- `RUST_LOG` - Logging level (error, warn, info, debug, trace)

## Configuration File

Default configuration file: `ggen.yaml`

```yaml
# ggen configuration
version: "1.0"

# Template directories
templates:
  - ~/.ggen/templates
  - ./templates

# Default variables
variables:
  author: "Your Name"
  license: "MIT"

# AI configuration
ai:
  model: "gpt-3.5-turbo"
  max_tokens: 2000
  temperature: 0.7

# Hook configuration
hooks:
  pre-commit:
    - cargo fmt
    - cargo clippy
```

## Exit Codes

- `0` - Success
- `1` - General error
- `2` - Invalid arguments
- `3` - File not found
- `4` - Network error
- `5` - Permission denied

## Examples

### Complete Workflow

```bash
# 1. Initialize project
ggen project init my-app

# 2. Install templates
ggen marketplace search rust
ggen marketplace install rust-api-template

# 3. Generate code
ggen template generate rust-api-template --var name=MyAPI --output ./src

# 4. Add AI-generated code
ggen ai generate "Create user authentication" --language rust > ./src/auth.rs

# 5. Set up hooks
ggen hook create pre-commit "cargo fmt && cargo clippy"

# 6. Run diagnostics
ggen utils doctor --all
```

### Working with RDF

```bash
# Load RDF schema
ggen graph load schema.ttl

# Query data
ggen graph query "SELECT ?name WHERE { ?person :name ?name }" --format json

# Generate code from RDF
ggen template generate-rdf schema.ttl --template rust-models --output ./src/models

# Export modified graph
ggen graph export updated-schema.ttl
```

### Marketplace Publishing

```bash
# Create template
ggen template new my-awesome-template --type tera

# Edit template files
# ...

# Lint before publishing
ggen template lint my-awesome-template

# Publish to marketplace
ggen marketplace publish ~/.ggen/templates/my-awesome-template \
  --name awesome-template \
  --version 1.0.0
```

## See Also

- [Getting Started Guide](../getting-started.md)
- [Template Development](../guides/templates.md)
- [RDF and SPARQL](../guides/rdf.md)
- [AI Integration](../guides/ai.md)
- [API Reference](./api.md)
