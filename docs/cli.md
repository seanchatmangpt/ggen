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
