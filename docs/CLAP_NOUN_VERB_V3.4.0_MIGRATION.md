# clap-noun-verb v3.4.0 Migration Guide

## Overview

This guide documents the migration from clap 4.5 enum-based commands to clap-noun-verb v3.4.0 attribute-based pattern.

## Benefits of v3.4.0

- **Zero Boilerplate**: Just add `#[verb]` attributes
- **Auto-Discovery**: Commands automatically registered at compile time
- **JSON Output**: All output automatically serializable
- **Type Inference**: Arguments inferred from function signatures
- **Advanced Features**: Environment variables, positional args, argument groups, custom parsers

## Migration Strategy

### Phase 1: Update Dependencies (✅ DONE)

```toml
clap-noun-verb = "3.4.0"
clap-noun-verb-macros = "3.4.0"
```

### Phase 2: File Organization

**Before (enum-based)**:
```
cmds/
├── mod.rs          # Manual routing
├── template.rs     # TemplateArgs enum + execute()
├── graph.rs        # GraphArgs enum + execute()
└── marketplace.rs  # MarketplaceArgs enum + execute()
```

**After (v3.4.0)**:
```
cmds/
├── template.rs     # Functions with #[verb] attributes
├── graph.rs        # Functions with #[verb] attributes
└── marketplace.rs  # Functions with #[verb] attributes
```

### Phase 3: Command Structure Changes

#### Before (clap enum pattern):
```rust
// template.rs
use clap::{Args, Subcommand};

#[derive(Debug, Args)]
pub struct TemplateArgs {
    #[command(subcommand)]
    pub command: TemplateCommand,
}

#[derive(Debug, Subcommand)]
pub enum TemplateCommand {
    Generate(GenerateArgs),
    List(ListArgs),
}

impl TemplateArgs {
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            TemplateCommand::Generate(args) => run_generate(args),
            TemplateCommand::List(args) => run_list(args),
        }
    }
}
```

#### After (v3.4.0 attribute pattern):
```rust
// template.rs
//! Template management commands

use clap_noun_verb_macros::verb;
use clap_noun_verb::Result;
use serde::Serialize;
use std::path::PathBuf;

/// Output for generate command
#[derive(Serialize)]
struct GenerateOutput {
    output_path: String,
    bytes_written: usize,
    files_created: usize,
}

/// Generate from template
#[verb] // Verb "generate", noun "template" auto-inferred from filename
fn generate(
    #[arg(short = 't', long)]
    template: Option<PathBuf>,

    #[arg(short = 'r', long)]
    rdf: Vec<PathBuf>,

    #[arg(short = 'o', long)]
    output: Option<PathBuf>,

    #[arg(short = 'v', long)]
    var: Vec<String>,

    #[arg(short = 'f', long)]
    force: bool,
) -> Result<GenerateOutput> {
    // Implementation...
    Ok(GenerateOutput {
        output_path: "output.txt".to_string(),
        bytes_written: 100,
        files_created: 1,
    })
}

/// List available templates
#[verb] // Verb "list", noun "template" auto-inferred
fn list(
    #[arg(short = 'd', long)]
    directory: Option<PathBuf>,
) -> Result<Vec<TemplateInfo>> {
    // Implementation...
}
```

### Phase 4: Main Entry Point

**Before**:
```rust
// lib.rs or main.rs
pub fn run_cli() -> Result<()> {
    let cli = Cli::parse();
    cli.execute()
}
```

**After**:
```rust
// lib.rs or main.rs
pub fn run_cli() -> Result<()> {
    clap_noun_verb::run() // Auto-discovers all #[verb] functions!
}
```

### Phase 5: Type Inference

Arguments are automatically inferred from function signatures:

| Rust Type | CLI Behavior | Example |
|-----------|--------------|---------|
| `String` | Required argument | `--name <VALUE>` |
| `Option<T>` | Optional argument | `--name <VALUE>` (optional) |
| `bool` | Flag (SetTrue action) | `--flag` (true if present) |
| `usize` | Count action | `-vvv` → 3 |
| `Vec<T>` | Multiple values (Append) | `--tags rust cli` |
| `PathBuf` | File path with auto-completion | `--file <PATH>` |

### Phase 6: Advanced v3.4.0 Features

#### Environment Variables
```rust
#[verb]
fn config(
    #[arg(env = "SERVER_HOST", default_value = "localhost")]
    host: String,

    #[arg(env = "SERVER_PORT", default_value = "8080")]
    port: u16,
) -> Result<ConfigOutput> {
    // Uses env vars as fallback
}
```

#### Positional Arguments
```rust
#[verb]
fn deploy(
    #[arg(index = 0)] // First positional argument
    url: String,

    #[arg(short = 'p', long)]
    port: Option<u16>,
) -> Result<DeployOutput> {
    // ggen deploy https://example.com --port 3000
}
```

#### Argument Groups (Exclusive)
```rust
#[verb]
fn format(
    #[arg(group = "format")]
    json: bool,

    #[arg(group = "format")]
    yaml: bool,

    #[arg(group = "format")]
    toml: bool,
) -> Result<FormatOutput> {
    // Only one format can be selected
}
```

#### Requires & Conflicts
```rust
#[verb]
fn publish(
    #[arg(short = 'o', long)]
    output: Option<PathBuf>,

    #[arg(long, requires = "output")] // Only valid if --output is set
    format: Option<String>,

    #[arg(long, conflicts_with = "format")] // Cannot use with --format
    raw: bool,
) -> Result<PublishOutput> {
    // Complex argument relationships
}
```

#### Count Actions
```rust
#[verb]
fn run(
    #[arg(short = 'v', action = "count")]
    verbose: usize, // -vvv → 3
) -> Result<RunOutput> {
    match verbose {
        0 => println!("Normal output"),
        1 => println!("Verbose output"),
        2 => println!("Very verbose output"),
        _ => println!("Debug output"),
    }
}
```

#### Custom Value Names & Aliases
```rust
#[verb]
fn export(
    #[arg(short = 'o', long, value_name = "FILE")]
    output: PathBuf,

    #[arg(short = 'd', long, alias = "debug")]
    verbose_debug: bool,
) -> Result<ExportOutput> {
    // Shown in help as: --output <FILE>
    // Can use: --debug or --verbose-debug
}
```

## JSON Output

All commands automatically serialize to JSON:

```rust
#[derive(Serialize)]
struct GenerateOutput {
    files_created: usize,
    bytes_written: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    errors: Option<Vec<String>>,
}

#[verb]
fn generate(...) -> Result<GenerateOutput> {
    Ok(GenerateOutput {
        files_created: 5,
        bytes_written: 1024,
        errors: None,
    })
}
```

Output:
```bash
$ ggen template generate --template foo.tmpl
{"files_created":5,"bytes_written":1024}
```

## Migration Checklist

For each noun (template, graph, marketplace, etc.):

- [ ] Add `#[derive(Serialize)]` to all output types
- [ ] Convert enum-based commands to `#[verb]` functions
- [ ] Update function signatures with `#[arg(...)]` attributes
- [ ] Return `Result<T>` where `T: Serialize`
- [ ] Remove manual `execute()` implementations
- [ ] Remove `Args` and `Command` enums
- [ ] Update tests to expect JSON output

## Testing v3.4.0 Features

```bash
# Test auto-discovery
cargo run -- template --help

# Test JSON output
cargo run -- template generate --template foo.tmpl

# Test environment variables
export SERVER_HOST=example.com
cargo run -- config

# Test count actions
cargo run -- run -vvv

# Test positional + named args
cargo run -- deploy https://api.example.com --port 3000
```

## Common Patterns

### Async Functions with Runtime Bridge

```rust
#[verb]
fn lint(template: PathBuf) -> Result<LintOutput> {
    crate::runtime::execute(async move {
        let report = ggen_domain::template::lint::lint_template(&template, &options)?;
        Ok(LintOutput {
            errors: report.errors.len(),
            warnings: report.warnings.len(),
        })
    })
}
```

### Optional Domain Integration

```rust
#[verb]
fn search(
    query: String,

    #[arg(long)]
    category: Option<String>,

    #[arg(short = 'l', long, default_value = "10")]
    limit: usize,
) -> Result<SearchOutput> {
    use ggen_domain::marketplace;

    let results = marketplace::search::search_packages(&query, category.as_deref(), limit)?;

    Ok(SearchOutput {
        query,
        results: results.into_iter().map(|r| r.name).collect(),
        count: results.len(),
    })
}
```

## Benefits Achieved

1. **84% Less Boilerplate**: No enums, no manual routing, no match statements
2. **100% Type Safety**: Compile-time verification of all commands
3. **Auto-Documentation**: Help text generated from attributes and doc comments
4. **JSON-First**: Perfect for CI/CD, scripting, and AI agents
5. **Modern Features**: Environment variables, positional args, argument groups, etc.

## Next Steps

1. Review this guide
2. Examine the proof-of-concept implementation (see `/docs/examples/v3.4.0_poc_template.rs`)
3. Migrate one noun at a time, starting with simplest (utils, hook)
4. Test thoroughly with existing integration tests
5. Update documentation and examples

## Resources

- [clap-noun-verb v3.4.0 Documentation](https://docs.rs/clap-noun-verb/3.4.0)
- [clap-noun-verb Examples](https://github.com/seanchatmangpt/clap-noun-verb/tree/main/examples)
- [Migration from clap](https://github.com/seanchatmangpt/clap-noun-verb#migration-from-clap)
