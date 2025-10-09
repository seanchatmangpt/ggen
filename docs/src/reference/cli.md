# CLI

## Marketplace Commands

### Search and Discovery

```bash
# Search for rpacks by keywords
rgen search <query>

# Examples:
rgen search rust cli
rgen search python api
rgen search typescript react

# Browse popular categories
rgen categories

# Get detailed rpack information
rgen show <rpack-id>
```

### Installation and Management

```bash
# Install rpack (latest version)
rgen add <rpack-id>

# Install specific version
rgen add <rpack-id>@<version>

# Examples:
rgen add io.ggen.rust.cli-subcommand
rgen add io.ggen.rust.cli-subcommand@0.2.0

# List installed rpacks
rgen packs

# Update all rpacks to latest compatible versions
rgen update

# Update specific rpack
rgen update <rpack-id>

# Remove rpack
rgen remove <rpack-id>
```

### Rpack Publishing (for authors)

```bash
# Initialize new rpack
rgen pack init

# Lint rpack for publishing
rgen pack lint

# Run tests
rgen pack test

# Publish to registry
rgen pack publish
```

## Generation Commands

### Template Generation

```bash
# Generate from rpack template
rgen gen <rpack-id>:<template-path> [--vars k=v ...] [--dry]

# Generate from local template
rgen gen <scope> <action> [--vars k=v ...] [--dry]

# Examples:
rgen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello
rgen gen cli subcommand --vars cmd=hello summary="Print greeting"
```

### Template Discovery

```bash
# List available templates (local + rpacks)
rgen list

# Show template details
rgen show <template-ref> [--vars k=v ...]

# Examples:
rgen show io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl
rgen show cli subcommand
```

## Validation Commands

```bash
# Validate template frontmatter
rgen validate <template-ref> [--vars k=v ...]

# Lint template with schema validation
rgen lint <template-ref>

# Examples:
rgen validate io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl
rgen lint cli subcommand
```

## Utility Commands

```bash
# Export RDF graph
rgen graph export <template-ref> --fmt ttl|jsonld

# Generate hazard report
rgen hazard

# Generate shell completion scripts
rgen completion bash|zsh|fish
```

## Variable Precedence

Variables are resolved in this order (later values override earlier):

1. **CLI arguments** (`--var key=value`)
2. **Environment variables** (from `.env` files)
3. **System environment** (`$HOME`, `$USER`, etc.)
4. **Rpack variables** (from rpack `ggen.toml`)
5. **Template frontmatter** (`vars:` section)
6. **SPARQL variables** (from queries)

## Rpack Template Reference Syntax

When using rpack templates, use the format:

```
<rpack-id>:<template-path>
```

Examples:
- `io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl`
- `io.ggen.python.api:api/endpoint/fastapi.tmpl`
- `io.ggen.typescript.react:components/button.tsx.tmpl`

## Dry-Run Mode

Preview template rendering without writing files:

```bash
rgen gen --template templates/api/endpoint/rust.tmpl --var name=User --dry
```

Dry-run behavior:
- RDF graphs are loaded (read-only)
- SPARQL queries execute normally
- Templates render completely
- Output shows what would be written
- No files are created or modified
- No shell commands execute (when implemented)
- No injections occur (when implemented)
