<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [CLI](#cli)
  - [Marketplace Commands](#marketplace-commands)
    - [Search and Discovery](#search-and-discovery)
    - [Installation and Management](#installation-and-management)
    - [Gpack Publishing (for authors)](#gpack-publishing-for-authors)
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
