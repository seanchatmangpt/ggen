# Rgen DX Features

This document covers all developer experience features in rgen, including both marketplace and local template workflows. These features focus on ergonomics, authoring workflows, error handling, and development productivity.

## CLI Ergonomics

### One Verb Philosophy

```bash
# Single command for everything
rgen gen <template> key=val ...

# No complex subcommand trees
# Just: rgen gen [template-ref] [options] [variables]
```

### Auto-Discovery

```bash
# Automatically finds project configuration
cd my-project/
rgen gen cli subcommand name=hello  # Finds ggen.toml automatically

# Discovers templates directory
# Loads project-specific RDF graphs
# Merges environment variables
```

### Variable Precedence

Variables are resolved in this order (later values override earlier):

1. **Environment variables** (from `.env` files)
2. **System environment** (`$HOME`, `$USER`, etc.)
3. **Project presets** (from `ggen.toml` [preset] section)
4. **Template frontmatter** (`vars:` section in template)
5. **CLI arguments** (`--var key=value`)

```bash
# .env file
author=John Doe

# ggen.toml
[preset]
vars = { license = "MIT" }

# template frontmatter
vars:
  author: "Jane Smith"  # Overridden by CLI
  feature: "basic"

# CLI call
rgen gen cli subcommand --var author="CLI Author" --var feature="advanced"
# Result: author="CLI Author", license="MIT", feature="advanced"
```

### Rich Dry Run

```bash
# Side-by-side diff view
rgen gen cli subcommand name=hello --dry

# Shows unified diff with context
# Displays target paths and variable summary
# No files written until you remove --dry
```

### Execution Tracing

```bash
# See everything that happens during generation
RGEN_TRACE=1 rgen gen cli subcommand name=hello

# Outputs:
# === RGEN TRACE ===
# Template path: templates/cli/subcommand/rust.tmpl
# Resolved frontmatter:
# {to: "src/cmds/{{name}}.rs", vars: {name: "hello"}, ...}
# SPARQL prolog:
# @prefix cli: <urn:rgen:cli#> . @base <http://example.org/> .
# Target output path: src/cmds/hello.rs
```

## Marketplace DX Features

### Rpack Development Workflow

```bash
# Initialize new rpack
rgen pack init

# Add templates and dependencies
mkdir -p templates/cli/subcommand
# Create template files...

# Test rpack locally
rgen pack test

# Lint for publishing
rgen pack lint

# Publish to registry
rgen pack publish
```

### Rpack Testing Best Practices

```bash
# Run golden tests
rgen pack test

# Test with different variables
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=test1
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=test2

# Verify deterministic output
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=test1
# Should produce identical output
```

### Rpack Versioning Strategies

```bash
# Semantic versioning for rpacks
# Major.Minor.Patch
# 1.0.0 -> 1.0.1 (patch: bug fixes)
# 1.0.0 -> 1.1.0 (minor: new features)
# 1.0.0 -> 2.0.0 (major: breaking changes)

# Update rpack version
# Edit ggen.toml:
# version = "0.2.1"

# Test before publishing
rgen pack test
rgen pack lint
```

## Authoring Loop

### Live Development Mode

```bash
# Watch mode for rapid iteration
rgen dev --watch templates/

# Automatically re-renders when:
# - Template files change
# - Frontmatter is modified
# - RDF graphs are updated
# - SPARQL queries change

# Outputs to temp directory with live diff
# Perfect for template development
```

### Rpack Development Mode

```bash
# Watch mode for rpack development
rgen pack dev --watch

# Automatically re-renders when:
# - Rpack templates change
# - Dependencies update
# - RDF graphs are modified
# - Tests need re-running

# Outputs to temp directory with live diff
# Perfect for rpack development
```

### Template Scaffolding

```bash
# Generate new template with sensible defaults
rgen new template cli/subcommand/typescript

# Creates:
# templates/cli/subcommand/typescript.tmpl
# With standard frontmatter structure
# Includes example RDF and SPARQL
# Ready for customization
```

### Template Documentation

```bash
# Get help for any template
rgen help cli/subcommand/rust.tmpl

# Shows:
# Template: cli/subcommand/rust.tmpl
# Description: Generate Rust CLI subcommand
# Required Variables:
#   name (string): Subcommand name
#   description (string): Help text
# Optional Variables:
#   author (string): Code author
# Examples:
#   rgen gen cli/subcommand/rust.tmpl name=status description="Show status"
# Dependencies:
#   RDF: graphs/cli.ttl
#   Queries: SELECT ?name ?description WHERE { ?cmd rdfs:label ?name }
```

### Manifest Preview

```bash
# See what would be generated without running templates
rgen plan cli subcommand

# Shows:
# Would generate:
#   src/cmds/hello.rs (from templates/cli/subcommand/rust.tmpl)
#   src/cmds/goodbye.rs (from templates/cli/subcommand/rust.tmpl)
#   commands/hello.py (from templates/cli/subcommand/python.tmpl)
#
# Variables applied:
#   name=hello, description="Say hello"
#   name=goodbye, description="Say goodbye"
```

## Error Handling

### Tera Template Errors

```bash
# File:line:col with 5-line snippet and highlighted token
Error in templates/api/endpoint/rust.tmpl:12:8
   |
10 | pub struct {{name|pascal}}Handler {
11 |     // TODO: Add fields
12 |     pub {{field_name}
   |           ^^^^^^^^
   |
Expected closing `}}` for variable `field_name`

Suggestion: Add `}}` after field_name
```

### Frontmatter Validation Errors

```bash
# Path.to.field with expected type and example
Error in templates/cli/subcommand/rust.tmpl frontmatter:
  .rdf[0] : Expected string, found array

  Expected format:
  rdf:
    - "graphs/cli.ttl"

  Got:
  rdf:
    - ["graphs/cli.ttl"]

Suggestion: Use string instead of array for single file
```

### SPARQL Query Errors

```bash
# Shows prepended prolog and failing variable binding
SPARQL Error in templates/api/endpoint/rust.tmpl:
Query:
  @prefix api: <urn:rgen:api#> .
  @base <http://example.org/> .
  SELECT ?name ?type WHERE {
    ?endpoint a api:Endpoint .
    ?endpoint api:name ?name .
    ?endpoint api:type ?type
  }

Variable binding failed for ?type:
  No value found for variable 'type' in graph

Suggestion: Check RDF data or query pattern
Available variables: ?name, ?endpoint
```

### Injection Errors

```bash
# Shows first non-matching context lines and regex used
Injection Error in src/main.rs:
Pattern 'fn main\(\) {' not found in file

Context (first 10 lines):
  1 | use std::env;
  2 |
  3 | fn main() {
  4 |     println!("Hello, world!");
  5 | }

Regex used: fn main\(\) \{

Suggestion: Check if pattern exists in target file
Try: --dry to preview injection before applying
```

## Hygen Parity

### Complete Frontmatter Support

All Hygen frontmatter keys supported 1:1:

```yaml
---
to: "src/{{type}}s/{{name}}.rs"           # Output path
from: "templates/base.rs"                  # Source template
force: true                               # Overwrite existing
unless_exists: true                       # Skip if exists
inject: true                              # Enable injection mode
before: "// Existing content"             # Inject before pattern
after: "fn main() {"                      # Inject after pattern
prepend: true                             # Prepend to file
append: true                              # Append to file
at_line: 10                               # Inject at line number
eof_last: true                            # Inject before EOF
skip_if: "// GENERATED"                   # Skip if pattern found
sh_before: "echo 'Generating...'"         # Pre-generation shell
sh_after: "cargo fmt"                     # Post-generation shell
---
```

### Regex-Based Injection

```bash
# Compiled once for performance
# Deterministic first-match behavior
# All injection modes use regex patterns

# Inject before existing function
before: "fn existing_function\(\) {"

# Inject after struct definition
after: "struct ExistingStruct \{[^}]*\}"

# Skip if already injected
skip_if: "// GENERATED CODE"
```

### Idempotency Guarantees

```bash
# Checked before any write operation
# Echo reason in dry-run mode

# If skip_if pattern found:
#   → Skip injection entirely
#   → Log: "Skipped injection: pattern found"

# If unless_exists and file exists:
#   → Skip generation entirely
#   → Log: "Skipped generation: file exists"
```

## Determinism & Previews

### Default Diff View

```bash
# Diff shown by default in --dry mode
rgen gen cli subcommand name=hello --dry

# Unified diff format:
# --- templates/cli/subcommand/rust.tmpl
# +++ would generate: src/cmds/hello.rs
# @@ -1,4 +1,4 @@
# -use utils::error::Result;
# +use utils::error::Result;
# +
# +#[derive(clap::Args, Debug)]
# +pub struct HelloArgs {
# +    /// Name to greet
# +    #[arg(short, long, default_value = "World")]
# +    pub name: String,
# +}
```

### Content Hashing

```bash
# Printed after successful write
rgen gen cli subcommand name=hello

# Output:
# Generated: src/cmds/hello.rs
# Content hash: sha256:a1b2c3d4e5f6...

# Same inputs → identical bytes
# Enables caching and change detection
```

### Stable Ordering

```bash
# --idempotency-key seeds stable ordering
rgen gen cli subcommand --idempotency-key "my-project"

# Multi-file generation produces consistent output order
# Same key → same file ordering across runs
```

## Graph (RDF) Integration

### Single Shared Graph

```rust
// One Graph instance per pipeline run
// Preloads project + template RDF once
// Cached query results for performance

let mut pipeline = Pipeline::new()?;
pipeline.load_rdf("graphs/project.ttl")?;
pipeline.load_rdf("graphs/cli.ttl")?;
```

### SPARQL Functions

```tera
// In templates:
{{ sparql(query="SELECT ?name WHERE { ?cmd rdfs:label ?name }") }}

// Named queries with parameters:
{{ sparql_named(name="command_by_name", var="name=hello") }}

// Results available as JSON in templates
{% for cmd in sparql_results %}
pub struct {{cmd.name}}Args;
{% endfor %}
```

### Automatic Prolog Building

```yaml
# Frontmatter automatically builds prolog:
prefixes:
  cli: "urn:rgen:cli#"
  ex: "http://example.org/"

base: "http://example.org/"

# Generates:
# @prefix cli: <urn:rgen:cli#> .
# @prefix ex: <http://example.org/> .
# @base <http://example.org/> .
```

## Template Helpers

### Text Transformation Filters

```tera
// All Inflector + Heck filters available:
{{ name | camel }}           // userName
{{ name | pascal }}          // UserName
{{ name | snake }}           // user_name
{{ name | kebab }}           // user-name
{{ name | shouty_snake }}    // USER_NAME
{{ name | plural }}          // users
{{ name | singular }}        // user
```

### Built-in Functions

```tera
// Local name from IRI
{{ local(iri="<http://example.org/User>") }}  // "User"

// Slug generation
{{ slug(text="Hello World!") }}              // "hello-world"

// Indentation control
{{ indent(text="line1\nline2", n=2) }}       // "  line1\n  line2"

// Newline insertion
{{ newline(n=3) }}                           // "\n\n\n"
```

## Safety & Guardrails

### Safe Write Root

```bash
# Safe write root = current directory
rgen gen cli subcommand name=hello

# Generates: ./src/cmds/hello.rs
# Cannot write outside project root

# Override with --unsafe-write (requires explicit opt-in)
rgen gen cli subcommand name=hello --unsafe-write /tmp/output
```

### Shell Hook Controls

```yaml
# Off by default for security
sh_before: "echo 'Generating...'"    # Not executed
sh_after: "cargo fmt"                # Not executed

# Enable with --allow-sh flag
rgen gen template --allow-sh

# Always preview in --dry mode
rgen gen template --dry --allow-sh  # Shows what shell commands would run
```

### Network Restrictions

```bash
# No network during render by default
# Prevents malicious template behavior

# Enable network for rpack fetching only
rgen add io.rgen.rust.cli-subcommand --net

# Network only for registry operations
# Templates cannot make HTTP requests
```

## Configuration & Discovery

### Project Configuration

```toml
# ggen.toml - single source of project config
[project]
name = "My CLI Tool"
version = "0.1.0"

[prefixes]
ex = "http://example.org/"

[rdf]
files = ["templates/**/graphs/*.ttl"]
inline = ["@prefix ex: <http://example.org/> . ex:Project a ex:Tool ."]

[preset]
vars = { author = "Team", license = "MIT" }
```

### Health Check

```bash
# Validate entire project setup
rgen doctor

# Checks:
# ✓ ggen.toml syntax
# ✓ Template frontmatter validity
# ✓ RDF graph well-formedness
# ✓ SPARQL query syntax
# ✓ File path resolution
# ✓ Rpack compatibility
```

### Path Resolution

```bash
# All paths resolved relative to ggen.toml location
# Printed in --trace mode for debugging

# Project structure:
# my-project/
#   ggen.toml
#   graphs/cli.ttl
#   templates/cli/subcommand/rust.tmpl

# Paths automatically resolved:
# graphs/cli.ttl → /path/to/my-project/graphs/cli.ttl
# templates/cli/subcommand/rust.tmpl → /path/to/my-project/templates/cli/subcommand/rust.tmpl
```

## Testing Infrastructure

### Golden Test System

```bash
# Run golden tests for specific template
rgen test cli/subcommand/rust.tmpl

# Test structure:
# tests/golden/cli/subcommand/rust.tmpl/
#   input.toml     # Variables for test
#   output.rs      # Expected output

# Update goldens after changes
rgen test cli/subcommand/rust.tmpl --update-goldens
```

### Test Organization

```toml
# tests/golden/cli/subcommand/rust.tmpl/input.toml
name = "hello"
description = "Print a greeting"
author = "Team"

# Generates and compares against:
# tests/golden/cli/subcommand/rust.tmpl/output.rs
```

## Pipeline Integration

### Builder Pattern

```rust
// Fluent API for pipeline configuration
let pipeline = Pipeline::builder()
    .with_rdf("graphs/project.ttl")
    .with_prefixes([("ex", "http://example.org/")])
    .with_templates_dir("custom-templates")
    .with_cache_strategy(CacheStrategy::Memory)
    .build()?;
```

### Single Render Call

```rust
// One method handles everything
let plan = pipeline.render_file(
    "templates/cli/subcommand/rust.tmpl",
    &variables,
    DryRun::No
)?;

// Apply or preview
plan.apply()?;           // Write files
plan.print_diff()?;      // Show diff
```

## Sensible Defaults

### Pre-filled Context

```tera
// Available in all templates:
{{ cwd }}              // Current working directory
{{ env.HOME }}         // User home directory
{{ git.branch }}       // Current git branch
{{ git.user }}         // Git user name
{{ now }}              // RFC3339 timestamp
```

### Flexible Output Control

```yaml
# to: can be null to skip file generation
to: null               # No file written

# from: overrides template body
from: "base-template.rs"  # Use different source

# Works with all injection modes
inject: true
before: "fn main() {"
```

## Error Recovery

### Graceful Degradation

```bash
# Missing optional RDF → continues with empty graph
# Invalid SPARQL query → shows helpful error
# Template syntax error → precise location + suggestion
# Path traversal attempt → clear security message
```

### Recovery Suggestions

```bash
# Every error includes actionable next steps
Error: Template 'missing.tmpl' not found
Suggestion: Available templates:
  - cli/subcommand/rust.tmpl
  - api/endpoint/typescript.tmpl
  - Run 'rgen list' to see all options
```

## Performance Optimizations

### Streaming & Caching

```bash
# Large RDF graphs processed incrementally
# Repeated queries cached automatically
# Template compilation cached per-run
# File I/O batched for efficiency
```

### Memory Efficiency

```bash
# Bounded caches prevent memory leaks
# Stream processing for large files
# Minimal allocations in hot paths
# LTO enabled in release builds
```

## Development Workflow

### Rapid Iteration Cycle

```bash
# 1. Edit template
# 2. Test with --dry
# 3. Check --trace output
# 4. Iterate quickly

rgen gen template --dry --trace
# → See exactly what happens
# → Fix issues immediately
# → No waiting for file writes
```

### Template Debugging

```bash
# Debug template logic step by step
RGEN_TRACE=1 rgen gen template

# See:
# - Frontmatter resolution
# - Variable precedence
# - SPARQL query execution
# - Template rendering
# - File path calculation
```

## Integration Benefits

### IDE Support

```bash
# Rich error messages work in IDEs
# Template syntax highlighting
# Variable name completion
# Live preview of generated code
# Source maps for debugging
```

### Tool Integration

```bash
# JSON output for CI/CD
rgen gen template --dry --json > plan.json

# Machine-readable error format
# Structured logging for dashboards
# Metrics collection hooks
```

## Best Practices

### Template Organization

```bash
templates/
  cli/
    subcommand/
      rust.tmpl
      python.tmpl
      bash.tmpl
  api/
    endpoint/
      rust.tmpl
      typescript.tmpl
  component/
    mod.rs.tmpl
```

### Variable Naming

```yaml
# Use descriptive variable names
vars:
  component_name: "UserService"
  api_version: "v1"
  author_email: "team@example.com"

# Avoid generic names like 'name', 'type'
# Use domain-specific names
```

### Error Prevention

```yaml
# Validate early with schemas
# Use RDF shapes for data validation
# Test templates with golden tests
# Use --dry before --allow-sh
```

This comprehensive DX system provides fast feedback, predictable outputs, clear error messages, and zero ceremony—exactly the developer experience lift that covers 80% of use cases while maintaining the power and flexibility needed for complex scenarios.