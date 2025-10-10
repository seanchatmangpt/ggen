# CLI Commands Directory

This directory contains the implementation of all ggen CLI commands, organized using a **noun-verb** structure for better discoverability and logical grouping.

## Structure Overview

### Noun-Verb Pattern
Commands follow the pattern: `ggen <noun> <verb> [args]`

- **Nouns** represent the domain/object being operated on
- **Verbs** represent the action being performed
- This creates intuitive, discoverable commands like `ggen market search` instead of `ggen search`

### Directory Organization

```
cmds/
├── mod.rs                    # Main command dispatcher
├── project/                  # Project operations
│   ├── mod.rs               # ProjectCmd with verbs: gen, plan, apply, diff
│   ├── gen.rs               # Generate artifacts from templates
│   ├── plan.rs              # Create generation plans (dry-run)
│   ├── apply.rs             # Apply previously generated plans
│   └── diff.rs              # Show unified diff of changes
├── market/                  # Marketplace operations
│   ├── mod.rs               # MarketCmd with verbs: search, add, remove, list, update, show, categories
│   ├── search.rs            # Search for gpacks
│   ├── add.rs               # Add gpack to project
│   ├── remove.rs            # Remove gpack from project
│   ├── list.rs              # List installed gpacks
│   ├── update.rs            # Update gpacks
│   ├── show.rs              # Show gpack details
│   └── categories.rs         # Show popular categories
├── template/                # Template management
│   ├── mod.rs               # TemplateCmd with verbs: new, list, show, lint
│   ├── new.rs               # Create new template
│   ├── list.rs              # List available templates
│   ├── show.rs              # Show template details
│   └── lint.rs              # Lint template with validation
├── graph/                   # RDF graph operations
│   ├── mod.rs               # GraphCmd with verbs: query, load, export, validate, stats
│   ├── query.rs             # Execute SPARQL queries
│   ├── load.rs              # Load RDF data
│   ├── export.rs            # Export graph to file
│   ├── validate.rs          # Validate against SHACL
│   └── stats.rs             # Show graph statistics
├── ci/                      # CI/CD operations (planned)
│   ├── mod.rs               # CiCmd with verbs: pages, workflow, trigger
│   ├── pages.rs             # GitHub Pages operations
│   ├── workflow.rs          # GitHub Actions workflow operations
│   └── trigger.rs           # Trigger workflows
├── audit/                   # Security and quality audits (planned)
│   ├── mod.rs               # AuditCmd with verbs: hazard, security, performance
│   ├── hazard.rs            # Generate hazard reports
│   ├── security.rs          # Security audits
│   └── performance.rs       # Performance audits
├── shell/                   # Shell integration (planned)
│   ├── mod.rs               # ShellCmd with verbs: completion, init
│   ├── completion.rs        # Generate completion scripts
│   └── init.rs              # Initialize shell integration
└── [legacy files]           # Legacy flat commands (deprecated)
    ├── add.rs               # → market add
    ├── search.rs            # → market search
    ├── gen.rs               # → project gen
    ├── list.rs              # → template list
    ├── show.rs              # → template show
    ├── lint.rs              # → template lint
    ├── github.rs            # → ci pages/workflow/trigger
    ├── hazard.rs            # → audit hazard
    └── completion.rs        # → shell completion
```

## Migration Status

### ✅ Completed Migrations
- **Market**: All marketplace operations migrated to `market` noun
- **Template**: All template operations migrated to `template` noun  
- **Project**: Generation operations migrated to `project` noun
- **Graph**: All RDF operations migrated to `graph` noun

### 🔄 In Progress
- Legacy command deprecation warnings
- Help text updates pointing to new commands

### 📋 Planned Migrations
- **GitHub operations** → `ci` noun (pages, workflow, trigger)
- **Hazard reporting** → `audit` noun (hazard, security, performance)
- **Shell completion** → `shell` noun (completion, init)

## Implementation Patterns

### Command Structure
Each noun follows this pattern:

```rust
// mod.rs
use clap::{Args, Subcommand};
use ggen_utils::error::Result;

pub mod verb1;
pub mod verb2;

#[derive(Args, Debug)]
pub struct NounCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Description of what this verb does
    Verb1(verb1::Verb1Args),
    /// Description of what this verb does  
    Verb2(verb2::Verb2Args),
}

impl NounCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Verb1(args) => verb1::run(args).await,
            Verb::Verb2(args) => verb2::run(args).await,
        }
    }
}
```

### Argument Structure
Each verb follows this pattern:

```rust
// verb.rs
use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct VerbArgs {
    /// Description of the argument
    #[arg(short, long)]
    pub flag: Option<String>,
    
    /// Required positional argument
    pub required: String,
}

pub async fn run(args: &VerbArgs) -> Result<()> {
    // Implementation here
    Ok(())
}
```

## Best Practices

### Command Design
- Use clear, action-oriented verbs
- Group related functionality under logical nouns
- Provide helpful descriptions for all commands
- Include examples in help text where helpful

### Error Handling
- Use `ggen_utils::error::Result` for all operations
- Provide actionable error messages
- Include context about what went wrong and how to fix it

### Output Formatting
- Use colored output for better UX
- Provide both human-readable and machine-readable (JSON) formats
- Include helpful next steps in command output

### Testing
- Unit tests for argument parsing
- Integration tests for command execution
- Test both success and error cases

## Adding New Commands

1. **Determine the noun**: What domain does this command operate on?
2. **Choose the verb**: What action is being performed?
3. **Create the structure**: Follow the established patterns
4. **Add to dispatcher**: Update `mod.rs` to include the new command
5. **Write tests**: Ensure the command works correctly
6. **Update documentation**: Add to this README and help text

## Legacy Command Deprecation

Legacy flat commands are maintained for backward compatibility but marked as deprecated:

```rust
#[command(name = "search", about = "Search for gpacks (deprecated: use 'ggen market search')")]
Search(search::SearchArgs),
```

When deprecating:
1. Add deprecation notice to help text
2. Point users to the new noun-verb command
3. Maintain functionality during transition period
4. Remove in next major version

## Performance Considerations

- Commands should be fast and responsive
- Use async/await for I/O operations
- Cache expensive operations where appropriate
- Provide progress indicators for long-running operations

## Definition of Done

For every noun-verb command implementation, see the comprehensive [Definition of Done document](../../noun-verb-definition-of-done.plan.md) which covers:

- Code quality gates (`cargo make fmt`, `cargo make lint`, `cargo make audit`)
- Testing requirements (unit tests with ≥80% coverage, integration tests)
- Documentation standards (help text, module docs, README updates)
- Integration requirements (registration, error handling, output formatting)
- Migration guidelines for legacy commands
- Review standards and PR requirements

## Examples of Excellent Implementations

- **`market add`**: Clear Args structure, trait-based dependency injection, comprehensive tests
- **`project gen`**: London TDD pattern, multiple mock traits, documented edge cases
- **`template lint`**: Clean domain types, testable with mocks, proper error propagation

## Anti-Patterns to Avoid

### NEVER Do
- Direct cargo commands (use `cargo make`)
- `unwrap()` or `expect()` in library code
- `println!` in library code (use in CLI layer only)
- Hardcoded paths
- Swallowing errors
- Tests without assertions
- Placeholder implementations in main branch

### ALWAYS Do
- Use `cargo make` for all development
- Use `ggen_utils::error::Result` for errors
- Provide actionable error messages
- Write tests before merging
- Document breaking changes
- Follow established patterns

## References

- **Definition of Done**: `noun-verb-definition-of-done.plan.md`
- **Project SPR**: `.cursorrules`
- **Development Guide**: `CLAUDE.md`
- **Makefile Tasks**: `Makefile.toml`
- **Examples**: All files in `cli/src/cmds/{market,project,template,graph}/`
