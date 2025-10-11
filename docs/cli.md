<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [CLI](#cli)
  - [Advanced Commands](#advanced-commands)
    - [Swarm Intelligence](#swarm-intelligence)
    - [Knowledge Hooks](#knowledge-hooks)
    - [Autonomous Systems](#autonomous-systems)
    - [Ultrathink](#ultrathink)
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
    - [AI Demo](#ai-demo)
    - [AI Frontmatter Generation](#ai-frontmatter-generation)
    - [AI Model Management](#ai-model-management-1)
    - [AI Project Scaffolding](#ai-project-scaffolding-1)
    - [AI Source File Analysis](#ai-source-file-analysis-1)
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

## Core Team CLI Best Practices

**Command Structure Pattern** (Required for all subcommands):
```rust
use clap::Parser;
use ggen_utils::error::Result;

#[derive(Parser)]
#[command(name = "ai")]
#[command(about = "AI-powered template generation and analysis")]
pub struct AiCommand {
    #[command(subcommand)]
    pub command: AiSubcommands,
}

#[derive(Parser)]
pub enum AiSubcommands {
    #[command(about = "Generate templates using AI")]
    Generate {
        #[arg(short, long, help = "Description of what to generate")]
        description: String,

        #[arg(short, long, help = "Examples or requirements")]
        examples: Option<String>,

        #[arg(short = 'o', long, help = "Output file path")]
        output: Option<PathBuf>,
    },
}

impl AiCommand {
    pub async fn run(self) -> Result<()> {
        use ggen_utils::logger;

        let logger = slog_scope::logger();
        info!(logger, "Starting AI command"; "command" => "ai");

        match self.command {
            AiSubcommands::Generate { description, examples, output } => {
                self.generate_template(description, examples, output).await
            }
        }
    }

    async fn generate_template(
        &self,
        description: String,
        examples: Option<String>,
        output: Option<PathBuf>,
    ) -> Result<()> {
        // Implementation with proper error handling and logging
        Ok(())
    }
}
```

**Error Handling in CLI**:
```rust
// ✅ GOOD: Proper CLI error handling
pub async fn run(self) -> Result<()> {
    match self.execute().await {
        Ok(_) => Ok(()),
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    }
}

// ❌ BAD: Panic in CLI
pub fn bad_run() {
    if some_condition {
        panic!("Something went wrong"); // Don't panic in CLI
    }
}
```

## Advanced Commands

### Swarm Intelligence

Ultrathink swarm intelligence for autonomous development workflows.

```bash
# Initialize and start the ultrathink swarm
ggen ultrathink start

# Submit a task to the swarm
ggen ultrathink task --description "Analyze codebase for improvements"

# Show swarm status and metrics
ggen ultrathink status

# Configure swarm behavior
ggen ultrathink intelligence
```

### Knowledge Hooks

Autonomic graph regeneration hooks for continuous evolution.

```bash
# Install knowledge hooks for a project
ggen hook install --project my-project

# Run hook validation
ggen hook validate --path .ggen/hooks/

# Execute knowledge regeneration
ggen hook regenerate --trigger file-change
```

### Autonomous Systems

Autonomous graph evolution and regeneration capabilities.

```bash
# Evolve graph from natural language requirements
ggen autonomous evolve --description "Add user authentication"

# Regenerate all artifacts from current graph state
ggen autonomous regenerate

# Show current autonomous system status
ggen autonomous status
```

### Ultrathink

Advanced reasoning and problem-solving using swarm intelligence.

```bash
# Run ultrathink reasoning
ggen ultrathink reason --query "optimize database queries"

# Execute ultrathink task
ggen ultrathink task --description "implement user authentication"

# Configure ultrathink parameters
ggen ultrathink config --reasoning-depth 3 --creativity 0.8
```

## Governance Commands

AI-powered governance and safety workflows for autonomous systems.

### Autonomous System Management

### Governance Operations

```bash
# Approve a pending governance operation
ggen autonomous approve --operation-id abc123

# Rollback to a previous snapshot
ggen autonomous rollback --snapshot-id previous-version
```

## Marketplace Commands

### Search and Discovery

```bash
# Search for gpacks by keywords
ggen market search <query>

# Examples:
ggen market search rust cli
ggen market search python api
ggen market search typescript react

# Browse popular categories
ggen market categories

# Get detailed gpack information
ggen market show <gpack-id>
```

### Installation and Management

```bash
# Install gpack (latest version)
ggen market add <gpack-id>

# Install specific version
ggen market add <gpack-id>@<version>

# Examples:
ggen market add io.ggen.rust.cli-subcommand
ggen market add io.ggen.rust.cli-subcommand@1.0.0

# List installed gpacks
ggen market list

# Update all gpacks to latest compatible versions
ggen market update

# Update specific gpack
ggen market update <gpack-id>

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

### AI Demo

```bash
# Run the AI template demo
ggen ai demo

# Examples:
ggen ai demo --model qwen3-coder:30b
```

### AI Frontmatter Generation

```bash
# Generate frontmatter for templates using AI
ggen ai frontmatter <template-file> [--description DESC] [--model MODEL]

# Examples:
ggen ai frontmatter api_controller.tmpl --description "REST API controller for user management"
ggen ai frontmatter data_processor.py.tmpl --model gpt-4o
```

### AI Model Management

```bash
# List available AI models
ggen ai models [--provider PROVIDER]

# Examples:
ggen ai models
ggen ai models --provider ollama
ggen ai models --provider openai
```

### AI Project Scaffolding

```bash
# Generate complete template projects using AI
ggen ai project <description> [--language LANG] [--framework FRAMEWORK] [--output DIR]

# Examples:
ggen ai project "A full-stack web application" --language typescript --framework nextjs --output my-app
ggen ai project "A Rust CLI tool" --language rust --output cli-tool
```

### AI Source File Analysis

```bash
# Generate templates from existing source files
ggen ai from-source <source-file> [--output FILE] [--description DESC]

# Examples:
ggen ai from-source src/main.rs --output main_template.tmpl
ggen ai from-source components/UserProfile.tsx --description "React user profile component"
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
ggen project gen <gpack-id>:<template-path> [--vars k=v ...] [--dry]

# Generate from local template
ggen project gen <scope> <action> [--vars k=v ...] [--dry]

# Examples:
ggen project gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl --vars name=hello
ggen project gen cli subcommand --vars cmd=hello summary="Print greeting"
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
