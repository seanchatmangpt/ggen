<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Recipe: Scaffolding a CLI Command with Freeze Blocks](#recipe-scaffolding-a-cli-command-with-freeze-blocks)
  - [What You'll Build](#what-youll-build)
  - [Prerequisites](#prerequisites)
  - [The Problem](#the-problem)
  - [The Recipe](#the-recipe)
    - [Step 1: Create Command Definitions](#step-1-create-command-definitions)
    - [Step 2: Create Command Template with Freeze Blocks](#step-2-create-command-template-with-freeze-blocks)
    - [Step 3: Generate Initial Commands](#step-3-generate-initial-commands)
    - [Step 4: Customize Your Implementation](#step-4-customize-your-implementation)
    - [Step 5: Add a New Command and Regenerate](#step-5-add-a-new-command-and-regenerate)
    - [Step 6: Verify Freeze Blocks Preserved](#step-6-verify-freeze-blocks-preserved)
  - [What's Happening?](#whats-happening)
    - [The Freeze Block System](#the-freeze-block-system)
    - [Template Helpers](#template-helpers)
    - [The Graph Query (Auto-generated)](#the-graph-query-auto-generated)
  - [Complete Working Example](#complete-working-example)
    - [Project Structure](#project-structure)
    - [Main CLI Entry Point](#main-cli-entry-point)
    - [Test It](#test-it)
  - [Common Patterns](#common-patterns)
    - [1. Conditional Arguments](#1-conditional-arguments)
    - [2. Nested Subcommands](#2-nested-subcommands)
    - [3. Shared Validation Logic](#3-shared-validation-logic)
  - [Troubleshooting](#troubleshooting)
    - [Freeze Blocks Not Preserved](#freeze-blocks-not-preserved)
    - [New Command Not Appearing](#new-command-not-appearing)
    - [Compilation Errors After Generation](#compilation-errors-after-generation)
  - [Advanced: Template Helpers](#advanced-template-helpers)
  - [Next Steps](#next-steps)
  - [Related Patterns](#related-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Recipe: Scaffolding a CLI Command with Freeze Blocks

**Time:** 15 minutes
**Difficulty:** Intermediate
**Patterns:** Freeze blocks, idempotent generation, command scaffolding

## What You'll Build

A CLI command scaffolding system that generates new commands without overwriting your existing implementations. You'll create a Rust CLI command structure where the boilerplate regenerates but your custom logic stays frozen.

## Prerequisites

- Completed [Your First Template](./first_template.md)
- Rust project with `clap` for CLI parsing

## The Problem

You need to add new CLI commands frequently, but:
- Boilerplate (parsing, validation) should regenerate
- Custom business logic should never be overwritten
- Adding new commands shouldn't break existing ones

## The Recipe

### Step 1: Create Command Definitions

Create `data/cli_commands.ttl`:

```turtle
@prefix cli: <http://example.org/cli#> .

cli:serve
  a cli:Command ;
  cli:name "serve" ;
  cli:description "Start the development server" ;
  cli:has_arg [
    cli:arg_name "port" ;
    cli:arg_type "u16" ;
    cli:arg_default "3000" ;
    cli:arg_help "Port to bind to"
  ] ;
  cli:has_arg [
    cli:arg_name "host" ;
    cli:arg_type "String" ;
    cli:arg_default "\"localhost\".to_string()" ;
    cli:arg_help "Host address"
  ] .

cli:build
  a cli:Command ;
  cli:name "build" ;
  cli:description "Build the project" ;
  cli:has_arg [
    cli:arg_name "release" ;
    cli:arg_type "bool" ;
    cli:arg_default "false" ;
    cli:arg_help "Build in release mode"
  ] .

cli:deploy
  a cli:Command ;
  cli:name "deploy" ;
  cli:description "Deploy to production" ;
  cli:has_arg [
    cli:arg_name "environment" ;
    cli:arg_type "String" ;
    cli:arg_default "\"production\".to_string()" ;
    cli:arg_help "Target environment"
  ] ;
  cli:has_arg [
    cli:arg_name "dry_run" ;
    cli:arg_type "bool" ;
    cli:arg_default "false" ;
    cli:arg_help "Simulate deployment without applying changes"
  ] .
```

### Step 2: Create Command Template with Freeze Blocks

Create `templates/cli_command.tmpl`:

```handlebars
{{#each commands}}
// Auto-generated command: {{name}}
// Generated at: {{timestamp}}
// DO NOT EDIT the struct and argument parsing - regenerate instead

use clap::{Args, Subcommand};

#[derive(Args, Debug)]
pub struct {{capitalize name}}Args {
{{#each args}}
    /// {{arg_help}}
    #[arg(long, default_value_t = {{arg_default}})]
    pub {{arg_name}}: {{arg_type}},
{{/each}}
}

impl {{capitalize name}}Args {
    // <<<FREEZE_START:{{name}}_execute>>>
    /// Execute the {{name}} command
    /// CUSTOM IMPLEMENTATION: Edit this method freely
    pub fn execute(&self) -> anyhow::Result<()> {
        // TODO: Implement {{name}} logic
        println!("{{capitalize name}} command executed");
{{#each args}}
        println!("  {{arg_name}}: {:?}", self.{{arg_name}});
{{/each}}
        Ok(())
    }
    // <<<FREEZE_END:{{name}}_execute>>>

    // <<<FREEZE_START:{{name}}_validate>>>
    /// Validate command arguments
    /// CUSTOM IMPLEMENTATION: Add your validation logic here
    pub fn validate(&self) -> anyhow::Result<()> {
        // TODO: Add validation
        Ok(())
    }
    // <<<FREEZE_END:{{name}}_validate>>>
}

{{/each}}

// <<<FREEZE_START:command_enum>>>
// CUSTOM: Add additional command variants here
#[derive(Subcommand, Debug)]
pub enum Command {
{{#each commands}}
    /// {{description}}
    {{capitalize name}}({{capitalize name}}Args),
{{/each}}
}
// <<<FREEZE_END:command_enum>>>

impl Command {
    pub fn execute(&self) -> anyhow::Result<()> {
        match self {
{{#each commands}}
            Command::{{capitalize name}}(args) => {
                args.validate()?;
                args.execute()
            },
{{/each}}
        }
    }
}
```

### Step 3: Generate Initial Commands

```bash
ggen exec \
  --template templates/cli_command.tmpl \
  --data data/cli_commands.ttl \
  --output src/commands.rs \
  --freeze
```

### Step 4: Customize Your Implementation

Edit `src/commands.rs` and add custom logic inside the freeze blocks:

```rust
// Find the serve_execute freeze block and replace with:
// <<<FREEZE_START:serve_execute>>>
/// Execute the serve command
/// CUSTOM IMPLEMENTATION: Edit this method freely
pub fn execute(&self) -> anyhow::Result<()> {
    use std::net::TcpListener;

    println!("🚀 Starting server on {}:{}", self.host, self.port);

    let addr = format!("{}:{}", self.host, self.port);
    let listener = TcpListener::bind(&addr)
        .map_err(|e| anyhow::anyhow!("Failed to bind to {}: {}", addr, e))?;

    println!("✅ Server listening at http://{}", addr);
    println!("   Press Ctrl+C to stop");

    // Your custom server logic here
    for stream in listener.incoming() {
        match stream {
            Ok(_stream) => println!("New connection!"),
            Err(e) => eprintln!("Connection failed: {}", e),
        }
    }

    Ok(())
}
// <<<FREEZE_END:serve_execute>>>
```

### Step 5: Add a New Command and Regenerate

Add to `data/cli_commands.ttl`:

```turtle
cli:test
  a cli:Command ;
  cli:name "test" ;
  cli:description "Run tests" ;
  cli:has_arg [
    cli:arg_name "coverage" ;
    cli:arg_type "bool" ;
    cli:arg_default "false" ;
    cli:arg_help "Generate coverage report"
  ] ;
  cli:has_arg [
    cli:arg_name "filter" ;
    cli:arg_type "String" ;
    cli:arg_default "\"".to_string()" ;
    cli:arg_help "Test name filter"
  ] .
```

Regenerate:

```bash
ggen exec \
  --template templates/cli_command.tmpl \
  --data data/cli_commands.ttl \
  --output src/commands.rs \
  --freeze
```

### Step 6: Verify Freeze Blocks Preserved

Check `src/commands.rs`:

```rust
// Your custom serve implementation should be UNCHANGED
// New TestArgs struct should be ADDED
// All boilerplate should be REGENERATED
```

## What's Happening?

### The Freeze Block System

1. **First generation**: GGen creates default implementations inside freeze blocks
2. **You customize**: Edit code inside `<<<FREEZE_START>>>` ... `<<<FREEZE_END>>>`
3. **Regeneration**: GGen preserves freeze block contents, regenerates everything else
4. **New commands**: Added seamlessly without touching existing code

### Template Helpers

- `{{capitalize name}}`: Converts "serve" → "Serve" for struct names
- `{{timestamp}}`: Adds generation timestamp for tracking
- `{{#each args}}`: Iterates over nested argument structures

### The Graph Query (Auto-generated)

```sparql
SELECT ?name ?description
       (GROUP_CONCAT(?arg_name; separator=",") as ?arg_names)
       (GROUP_CONCAT(?arg_type; separator=",") as ?arg_types)
WHERE {
  ?command a cli:Command ;
           cli:name ?name ;
           cli:description ?description ;
           cli:has_arg ?arg .
  ?arg cli:arg_name ?arg_name ;
       cli:arg_type ?arg_type .
}
GROUP BY ?name ?description
```

## Complete Working Example

### Project Structure

```
my-cli/
├── data/
│   └── cli_commands.ttl
├── templates/
│   └── cli_command.tmpl
├── src/
│   ├── main.rs
│   └── commands.rs (generated)
└── Cargo.toml
```

### Main CLI Entry Point

Create `src/main.rs`:

```rust
mod commands;

use clap::Parser;
use commands::Command;

#[derive(Parser)]
#[command(name = "mycli")]
#[command(about = "A scaffolded CLI application", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    cli.command.execute()
}
```

### Test It

```bash
# Build
cargo build

# Run commands
cargo run -- serve --port 8080 --host 127.0.0.1
cargo run -- build --release
cargo run -- deploy --environment staging --dry-run
cargo run -- test --coverage --filter integration
```

## Common Patterns

### 1. Conditional Arguments

```turtle
cli:command_with_optional
  cli:has_arg [
    cli:arg_name "verbose" ;
    cli:arg_type "bool" ;
    cli:arg_short "v" ;
    cli:arg_long "verbose" ;
    cli:arg_help "Enable verbose output"
  ] .
```

Template:

```handlebars
{{#if arg_short}}
    #[arg(short = '{{arg_short}}', long)]
{{else}}
    #[arg(long)]
{{/if}}
    pub {{arg_name}}: {{arg_type}},
```

### 2. Nested Subcommands

```turtle
cli:database
  a cli:CommandGroup ;
  cli:name "database" ;
  cli:has_subcommand cli:db_migrate ;
  cli:has_subcommand cli:db_seed .

cli:db_migrate
  a cli:Command ;
  cli:parent "database" ;
  cli:name "migrate" .
```

### 3. Shared Validation Logic

Add to freeze block:

```rust
// <<<FREEZE_START:shared_validators>>>
fn validate_port(port: u16) -> anyhow::Result<()> {
    if port < 1024 {
        anyhow::bail!("Port must be >= 1024");
    }
    Ok(())
}
// <<<FREEZE_END:shared_validators>>>
```

## Troubleshooting

### Freeze Blocks Not Preserved

**Problem:** Regeneration overwrites your custom code

**Solution:** Ensure exact syntax:
```rust
// <<<FREEZE_START:unique_id>>>
// code here
// <<<FREEZE_END:unique_id>>>
```

- No spaces after `<<<`
- Same `unique_id` on both markers
- Markers must be on their own lines

### New Command Not Appearing

**Problem:** Added command to graph but doesn't show in output

**Solution:** Check your RDF syntax:
```bash
ggen validate --data data/cli_commands.ttl
```

Common issues:
- Missing `a cli:Command` type declaration
- Typo in predicate names (`cli:has_arg` vs `cli:hasArg`)
- Missing trailing `.` on statements

### Compilation Errors After Generation

**Problem:** Generated Rust code doesn't compile

**Solution:** Check argument types match Rust types exactly:
```turtle
cli:arg_type "u16"      # ✅ Correct
cli:arg_type "uint16"   # ❌ Wrong - not a Rust type
```

## Advanced: Template Helpers

Add to template for better output:

```handlebars
{{!-- Custom helper: snake_case to CamelCase --}}
{{#helper "capitalize"}}
  {{name}}
{{/helper}}

{{!-- Timestamp for tracking --}}
// Generated: {{timestamp "2006-01-02 15:04:05"}}

{{!-- Conditional sections --}}
{{#if has_validation}}
  args.validate()?;
{{/if}}
```

## Next Steps

- **Add middleware**: Generate request/response middleware with freeze blocks
- **API endpoints**: Apply same pattern to REST endpoints ([API Generator](./api_endpoint_generator.md))
- **Database models**: Scaffold ORM models with custom queries frozen

## Related Patterns

- [Freeze Blocks Deep Dive](../patterns/freeze_blocks.md)
- [Idempotent Generation](../patterns/idempotent_generation.md)
- [Command Pattern](../patterns/command_pattern.md)

---

**Success checkpoint:** You should be able to:
1. Generate CLI commands from graph data
2. Add custom logic inside freeze blocks
3. Regenerate without losing customizations
4. Add new commands seamlessly

If any step fails, review the freeze block syntax carefully—whitespace and marker format must be exact.
