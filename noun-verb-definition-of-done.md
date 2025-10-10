# Noun-Verb CLI Command System - Definition of Done

## Document Purpose

This document establishes the core team's definition of done for all noun-verb CLI command implementations in ggen. It ensures consistency, quality, and maintainability across the CLI surface.

## 1. Command Structure Standards

### 1.1 Directory Organization

**Requirement**: All noun-verb commands must follow the established directory pattern:

```
cli/src/cmds/<noun>/
├── mod.rs              # Noun definition with Verb enum
├── <verb1>.rs          # Individual verb implementation
├── <verb2>.rs          # Individual verb implementation
└── ...
```

**Example**: `cli/src/cmds/market/`, `cli/src/cmds/project/`, `cli/src/cmds/template/`, `cli/src/cmds/graph/`

### 1.2 Noun Module Pattern (mod.rs)

**Required structure**:

```rust
use clap::{Args, Subcommand};
use ggen_utils::error::Result;

// Declare verb modules
pub mod verb1;
pub mod verb2;

#[derive(Args, Debug)]
pub struct NounCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Clear, action-oriented description
    Verb1(verb1::Verb1Args),
    /// Clear, action-oriented description  
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

### 1.3 Verb Implementation Pattern

**Required structure**:

```rust
use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct VerbArgs {
    /// Required positional arguments
    pub required_arg: String,
    
    /// Optional flags with clear descriptions
    #[arg(short, long)]
    pub optional_flag: Option<String>,
}

/// Main entry point - delegates to ggen-core
pub async fn run(args: &VerbArgs) -> Result<()> {
    // Placeholder during development is acceptable
    // but must be replaced before merge to main
    Ok(())
}

/// Testable implementation with dependency injection (London TDD)
#[cfg_attr(test, allow(dead_code))]
pub async fn run_with_deps(
    args: &VerbArgs,
    dependency: &dyn Trait,
) -> Result<()> {
    // Implementation with injected dependencies
    Ok(())
}
```

## 2. Testing Requirements

### 2.1 Unit Tests (MANDATORY)

**Every verb implementation MUST include**:

1. **Argument parsing tests**
   ```rust
   #[test]
   fn test_parse_args_valid() { ... }
   
   #[test]
   fn test_parse_args_invalid() { ... }
   ```

2. **Business logic tests with mocks** (London TDD pattern)
   ```rust
   #[tokio::test]
   async fn test_verb_calls_dependency() {
       let mut mock = MockDependency::new();
       mock.expect_method()
           .with(eq("expected"))
           .times(1)
           .returning(|_| Ok(()));
       
       let args = VerbArgs { ... };
       let result = run_with_deps(&args, &mock).await;
       assert!(result.is_ok());
   }
   ```

3. **Error handling tests**
   ```rust
   #[tokio::test]
   async fn test_verb_handles_error() { ... }
   ```

4. **Edge case tests**
   ```rust
   #[test]
   fn test_edge_case_empty_input() { ... }
   
   #[test]
   fn test_edge_case_special_chars() { ... }
   ```

**Minimum coverage**: 80% line coverage per verb module

### 2.2 Integration Tests (MANDATORY)

**Location**: `cli/tests/integration_tests.rs`

**Every noun-verb command MUST have**:

1. **Help text test**
   ```rust
   #[test]
   fn test_noun_verb_help() {
       let output = Command::new("cargo")
           .args(&["run", "--", "noun", "verb", "--help"])
           .output()
           .expect("Failed to execute");
       assert!(output.status.success());
       assert!(output.stdout.contains("expected text"));
   }
   ```

2. **Successful execution test**
   ```rust
   #[test]
   fn test_noun_verb_success() { ... }
   ```

3. **Error case test**
   ```rust
   #[test]
   fn test_noun_verb_error() { ... }
   ```

### 2.3 Test Execution Standards

**MUST pass all of**:

```bash
cargo make test                 # All tests pass
cargo make test-single-threaded # Deterministic execution
cargo make deterministic        # Fixed seed tests (RNG_SEED=42)
```

**NEVER use direct `cargo test`** - always use `cargo make test`

## 3. Code Quality Gates

### 3.1 Formatting (MANDATORY)

```bash
cargo make fmt
```

**Gate**: All code must be rustfmt-compliant with workspace settings

### 3.2 Linting (MANDATORY)

```bash
cargo make lint
```

**Gates**:

- Zero clippy warnings (`-D warnings`)
- No `unwrap()` or `expect()` in library code
- All errors use `ggen_utils::error::Result`
- No `println!` in library code (CLI output layer only)

### 3.3 Security (MANDATORY)

```bash
cargo make audit
```

**Gate**: Zero known vulnerabilities in dependencies

## 4. Documentation Requirements

### 4.1 Help Text (MANDATORY)

**Every command/subcommand MUST have**:

1. Clear, concise description in `#[command(about = "...")]`
2. Documented arguments with `#[arg(help = "...")]`
3. Examples in extended help where beneficial

**Format**:

```rust
#[derive(Args, Debug)]
pub struct VerbArgs {
    /// Clear description of what this argument does.
    /// Use complete sentences. End with period.
    #[arg(short, long)]
    pub flag: Option<String>,
}
```

### 4.2 Module Documentation (MANDATORY)

**Every verb module MUST have**:

```rust
//! Brief description of what this verb does.
//!
//! # Examples
//!
//! ```bash
//! ggen noun verb --flag value
//! ```
//!
//! # Errors
//!
//! Returns error if...
```

### 4.3 README.md Updates (MANDATORY)

When adding new noun or verb:

1. Update `cli/src/cmds/README.md` with new command structure
2. Update main `README.md` with usage examples
3. Update `CLAUDE.md` if adding new patterns

## 5. Integration Standards

### 5.1 Registration in mod.rs (MANDATORY)

**Location**: `cli/src/cmds/mod.rs`

1. Add noun module declaration
2. Add to `Commands` enum with deprecation notice for legacy equivalent
3. Add to `run()` and `run_with_config()` match arms
4. Add deprecation notices to legacy commands

**Example**:

```rust
// New noun-verb structure
#[command(name = "noun", about = "Noun operations")]
Noun(noun::NounCmd),

// Legacy flat command (deprecated)
#[command(name = "old-cmd", about = "Old command (deprecated: use 'ggen noun verb')")]
OldCmd(old_cmd::OldCmdArgs),
```

### 5.2 Error Handling (MANDATORY)

**All errors MUST**:

1. Use `ggen_utils::error::Result<T>`
2. Provide actionable error messages
3. Include context about what failed and how to fix it

**Example**:

```rust
return Err(ggen_utils::error::Error::new_fmt(
    format_args!("Failed to parse template '{}': {}. Check YAML syntax.", path, err)
));
```

### 5.3 Output Formatting (MANDATORY)

**CLI output MUST**:

1. Use `colored` crate for semantic coloring
2. Provide both human-readable and JSON output where applicable
3. Include helpful next steps in output

**Example**:

```rust
println!("{} Successfully added gpack '{}'", "✅".green(), gpack_id);
println!("\n{}", "Next steps:".bold());
println!("  {} ggen project gen {}:template.tmpl", "Generate:".bold(), gpack_id);
```

## 6. Migration from Legacy Commands

### 6.1 During Migration (ACCEPTABLE)

1. Keep legacy command alongside new noun-verb command
2. Mark legacy as deprecated with pointer to new command
3. Maintain identical functionality in both versions
4. Add deprecation warnings to output

### 6.2 After Migration (MANDATORY)

1. Remove legacy command implementation file
2. Remove legacy command from `Commands` enum
3. Update all tests to use new noun-verb syntax
4. Update all documentation

### 6.3 Migration Checklist

- [ ] New noun-verb command implemented
- [ ] Unit tests cover ≥80% of new implementation
- [ ] Integration tests pass
- [ ] Help text complete and accurate
- [ ] Legacy command marked deprecated
- [ ] Documentation updated
- [ ] `cargo make ci` passes

## 7. Definition of Done Checklist

**A noun-verb command is DONE when**:

### Code Quality

- [ ] `cargo make fmt` - No formatting changes needed
- [ ] `cargo make lint` - Zero clippy warnings
- [ ] `cargo make audit` - No security vulnerabilities
- [ ] No `unwrap()` or `expect()` in library code
- [ ] All errors use `ggen_utils::error::Result`

### Testing

- [ ] Unit tests exist with ≥80% coverage
- [ ] All unit tests use mockall for dependency injection
- [ ] Integration tests exist for happy path and error cases
- [ ] `cargo make test` passes
- [ ] `cargo make deterministic` passes

### Documentation

- [ ] Help text complete with examples
- [ ] Module documentation exists
- [ ] `cli/src/cmds/README.md` updated
- [ ] Main `README.md` updated with usage

### Integration

- [ ] Registered in `cli/src/cmds/mod.rs`
- [ ] Legacy command marked deprecated (if applicable)
- [ ] Error messages are actionable
- [ ] Output formatting uses colored text appropriately

### CI/CD

- [ ] `cargo make ci` passes (runs fmt, lint, test, audit)
- [ ] GitHub Actions workflows pass
- [ ] No regression in other commands

### Performance

- [ ] Command execution < 3s for typical usage
- [ ] No memory leaks detected
- [ ] Meets SLO requirements: `cargo make slo-check` passes

## 8. Review Standards

### 8.1 Code Review Checklist

**Reviewer MUST verify**:

1. All DoD checklist items completed
2. Tests are meaningful (not just coverage padding)
3. Error messages are clear and actionable
4. Help text matches implementation
5. No hardcoded paths or secrets
6. Follows established patterns in similar commands

### 8.2 PR Requirements

**Every noun-verb PR MUST include**:

1. Title: "feat(cli): Add `ggen noun verb` command"
2. Description with rationale and examples
3. Test output showing all checks pass
4. Breaking changes noted (if any)
5. Migration guide (if deprecating legacy command)

## 9. Examples of Compliant Commands

### 9.1 Excellent Example: `market add`

**File**: `cli/src/cmds/market/add.rs`

- Clear Args structure with documentation
- Trait-based dependency injection for testability
- Comprehensive unit tests with mockall
- Error handling with context
- Helpful output with next steps

### 9.2 Excellent Example: `project gen`

**File**: `cli/src/cmds/project/gen.rs`

- London TDD pattern with multiple mock traits
- Comprehensive test coverage (parsing, dry-run, etc.)
- Clear separation of concerns
- Documented edge cases

### 9.3 Excellent Example: `template lint`

**File**: `cli/src/cmds/template/lint.rs`

- Clean domain types (LintReport, LintError, LintWarning)
- Testable with mocks
- Clear output formatting
- Proper error propagation

## 10. Anti-Patterns to Avoid

### 10.1 NEVER Do

- Direct cargo commands (use `cargo make`)
- `unwrap()` or `expect()` in library code
- `println!` in library code (use in CLI layer only)
- Hardcoded paths
- Swallowing errors
- Tests without assertions
- Placeholder implementations in main branch

### 10.2 ALWAYS Do

- Use `cargo make` for all development
- Use `ggen_utils::error::Result` for errors
- Provide actionable error messages
- Write tests before merging
- Document breaking changes
- Follow established patterns

## 11. Continuous Improvement

### 11.1 Pattern Updates

When a better pattern emerges:

1. Document in this file
2. Update examples
3. Apply to new commands immediately
4. Refactor existing commands opportunistically

### 11.2 Tooling Updates

When adding new quality gates:

1. Update `Makefile.toml`
2. Update CI workflows
3. Update this document
4. Announce to team

## 12. References

- **Project SPR**: `.cursorrules`
- **Development Guide**: `CLAUDE.md`
- **Makefile Tasks**: `Makefile.toml`
- **Architecture**: `cli/src/cmds/README.md`
- **Examples**: All files in `cli/src/cmds/{market,project,template,graph}/`
